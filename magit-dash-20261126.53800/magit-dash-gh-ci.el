;;; magit-dash-gh-ci.el --- Lightweight GitHub Actions CI status for magit-dash -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1.0
;; Keywords: vc, tools, magit, github, ci
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; Provides lightweight CI status fetch for the magit-dash main repository
;; dashboard.  Fetches the most recent gh run list for the current branch,
;; caches the result as :ci-status in the shared magit-dash-gh cache, and
;; provides formatting, browser-open, and (stubbed) fix-CI dispatch.
;;
;; This is intentionally separate from magit-dash-gh-actions.el, which
;; downloads full CI logs interactively.  This module only fetches summary
;; data for dashboard rendering.

;;; Code:

(declare-function magit-dash-repo-repo "magit-dash")
(declare-function magit-dash-repo-clone-url "magit-dash")

(require 'map)
(require 'gen)
(require 'magit-dash-gh)
(require 'magit-dash-gh-actions)

(defvar magit-dash-max-concurrent-network-jobs)
(defvar magit-dash-max-concurrent-ci-jobs)
(defvar magit-dash-queue-yield-delay)
(defvar magit-dash-buffer-name)
(declare-function magit-dash-repo-path "magit-dash")
(declare-function magit-dash-repo-name "magit-dash")
(declare-function magit-dash-repo-branch "magit-dash")
(declare-function magit-dash-repo-include-ci "magit-dash")
(declare-function magit-dash--current-branch "magit-dash")
(declare-function magit-dash--update-entry "magit-dash")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-get-config "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell-viewport--show-buffer "agent-shell-viewport")
(declare-function agent-shell-menu-new-shell-in-dir "agent-shell-menu")
(declare-function agent-shell-queue-add-unassigned "agent-shell-queue")
(defvar agent-shell-prefer-viewport-interaction)

;;; Faces

(defface magit-dash-ci-pass-face
  '((t :inherit success))
  "Face for a passing CI run in the repository dashboard."
  :group 'magit-dash)

(defface magit-dash-ci-fail-face
  '((t :inherit error))
  "Face for a failing CI run in the repository dashboard."
  :group 'magit-dash)

(defface magit-dash-ci-pending-face
  '((t :inherit warning))
  "Face for an in-progress CI run in the repository dashboard."
  :group 'magit-dash)

;;; Internal helpers

(defun magit-dash-gh-ci--failure-p (conclusion)
  "Return non-nil when CONCLUSION string indicates a failed run."
  (member conclusion '("failure" "timed_out" "startup_failure")))

(defun magit-dash-gh-ci--parse-runs (runs &optional target-sha)
  "Return a CI status plist summarising RUNS (list of alists from gh run list).
When TARGET-SHA is provided, prefer runs matching TARGET-SHA.
Otherwise, only runs for the newest commit (by `headSha') are considered, since a commit
triggers several workflows and the newest run alone may be a passing one while
another workflow failed.  The overall :conclusion is \"failure\" if any such
run failed, else :status is \"in_progress\" if any is still running, else the
newest run's result.  :run-id and :url point at the first failing run when
there is one.  Returns nil when RUNS is nil or empty."
  (when runs
    (let* ((matched-sha (when (and target-sha (stringp target-sha))
                          (seq-some (lambda (r)
                                      (let ((r-sha (map-elt r 'headSha)))
                                        (when (and (stringp r-sha)
                                                   (or (string-prefix-p r-sha target-sha)
                                                       (string-prefix-p target-sha r-sha)))
                                          r-sha)))
                                    runs)))
           (sha (or matched-sha (map-elt (car runs) 'headSha)))
           (current (if sha
                        (seq-filter (lambda (r) (equal sha (map-elt r 'headSha)))
                                    runs)
                      runs))
           (failed (seq-find (lambda (r)
                               (magit-dash-gh-ci--failure-p (map-elt r 'conclusion)))
                             current))
           (pending (seq-find (lambda (r)
                                (member (map-elt r 'status)
                                        '("in_progress" "queued" "waiting" "pending" "requested")))
                              current))
           (cancelled (seq-find (lambda (r)
                                  (equal "cancelled" (map-elt r 'conclusion)))
                                current))
           (latest (or failed pending cancelled (car current)))
           (pass (seq-count (lambda (r)
                              (equal "success" (map-elt r 'conclusion)))
                            current))
           (fail (seq-count (lambda (r)
                              (magit-dash-gh-ci--failure-p (map-elt r 'conclusion)))
                            current)))
      (list :conclusion (cond (failed (map-elt failed 'conclusion))
                              (pending nil)
                              (t (map-elt latest 'conclusion)))
            :status (cond (failed "completed")
                          (pending (map-elt pending 'status))
                          (t (map-elt latest 'status)))
            :pass pass
            :fail fail
            :total (length current)
            :run-id (map-elt latest 'databaseId)
            :url (map-elt latest 'url)))))

;;; Display

(defun magit-dash-ci--repo-slug (repo)
  "Return OWNER/NAME slug for REPO struct or path.
Checks cache and struct attributes before inspecting git remotes or
querying GitHub."
  (let* ((path (if (stringp repo) repo (magit-dash-repo-path repo)))
         (default-directory (file-name-as-directory path)))
    (or (magit-dash-gh--cache-get path :repo-slug)
        (let ((slug
               (or (when (not (stringp repo))
                     (when-let* ((raw (or (and (fboundp 'magit-dash-repo-repo)
                                               (magit-dash-repo-repo repo))
                                          (and (fboundp 'magit-dash-repo-clone-url)
                                               (magit-dash-repo-clone-url repo))))
                                 (pair (magit-dash-gh--owner-repo-from-url raw)))
                       (format "%s/%s" (car pair) (cdr pair))))
                   (ignore-errors
                     (when-let* ((info (magit-dash-gh--repo-info))
                                 (o (plist-get info :owner))
                                 (r (plist-get info :repo)))
                       (format "%s/%s" o r)))
                   (when (and (not (stringp repo)) (fboundp 'magit-dash-repo-upstream-repo))
                     (when-let* ((raw (magit-dash-repo-upstream-repo repo))
                                 (pair (magit-dash-gh--owner-repo-from-url raw)))
                       (format "%s/%s" (car pair) (cdr pair))))
                   (let ((name (if (stringp repo)
                                   (file-name-nondirectory (directory-file-name repo))
                                 (magit-dash-repo-name repo))))
                     (if (string-match-p "/" name)
                         name
                       (user-error "Cannot determine GitHub owner/repo slug for %s" name))))))
          (when (and slug (not (string-empty-p slug)))
            (magit-dash-gh--cache-set path :repo-slug slug))
          slug))))

(defun magit-dash-gh-ci--format-status (ci-status)
  "Format CI-STATUS plist as a short propertized string for the dashboard column.
Returns a shadow \"—\" when CI-STATUS is nil or indicates no runs."
  (if (or (null ci-status)
          (plist-get ci-status :none))
      (propertize "—" 'face 'shadow)
    (let ((conclusion (plist-get ci-status :conclusion))
          (status (plist-get ci-status :status)))
      (cond
       ((equal conclusion "success")
        (propertize "✓" 'face 'magit-dash-ci-pass-face))
       ((magit-dash-gh-ci--failure-p conclusion)
        (propertize "x" 'face 'magit-dash-ci-fail-face))
       ((equal conclusion "cancelled")
        (propertize "⊘" 'face 'magit-dash-ci-pending-face))
       ((member status '("in_progress" "queued" "waiting" "pending" "requested"))
        (propertize "⟳" 'face 'magit-dash-ci-pending-face))
       (t (propertize "—" 'face 'shadow))))))

;;; Async fetch

(defcustom magit-dash-gh-ci-use-graphql t
  "When non-nil, attempt to batch CI status queries with GitHub GraphQL API."
  :type 'boolean
  :group 'magit-dash)

(defun magit-dash-gh-ci--build-batch-graphql-query (entries)
  "Build GraphQL query string for ENTRIES.
Each element is a list of (ALIAS REPO SLUG BRANCH)."
  (let ((fields
         (mapconcat
          (pcase-lambda (`(,alias ,_repo ,slug ,branch))
            (let ((parts (split-string slug "/")))
              (format "  %s: repository(owner: %S, name: %S) {
    ref(qualifiedName: %S) {
      target {
        ... on Commit {
          statusCheckRollup {
            state
            contexts(first: 20) {
              totalCount
              nodes {
                __typename
                ... on CheckRun {
                  name
                  conclusion
                  status
                  databaseId
                  detailsUrl
                }
                ... on StatusContext {
                  context
                  state
                  targetUrl
                }
              }
            }
          }
        }
      }
    }
  }"
                      alias (car parts) (cadr parts)
                      (format "refs/heads/%s" branch))))
          entries
          "\n")))
    (format "query {\n%s\n}" fields)))

(defun magit-dash-gh-ci--parse-graphql-rollup (rollup)
  "Convert GraphQL ROLLUP alist to a `magit-dash-gh-ci' status plist.
Counts cover only returned contexts.  When more contexts exist, set
:truncated and use the overall rollup state to avoid a false success.
GitHub reports cancellations as FAILURE, so a truncated failure can
represent cancellation rather than a failed check."
  (when (and rollup (not (eq rollup :null)) (not (eq rollup :false)) (or (listp rollup) (hash-table-p rollup)))
    (let* ((contexts (and (not (eq (map-elt rollup 'contexts) :null))
                          (map-elt rollup 'contexts)))
           (nodes (and contexts (not (eq (map-elt contexts 'nodes) :null))
                       (map-elt contexts 'nodes)))
           (runs
            (when (listp nodes)
              (seq-keep
               (lambda (node)
                 (when (and node (not (eq node :null)) (or (listp node) (hash-table-p node)))
                   (let ((typename (map-elt node '__typename)))
                     (if (equal typename "StatusContext")
                         `((workflowName . ,(map-elt node 'context))
                           (conclusion . ,(pcase (map-elt node 'state)
                                            ("SUCCESS" "success")
                                            ("FAILURE" "failure")
                                            ("ERROR" "failure")
                                            (_ nil)))
                           (status . ,(if (equal (map-elt node 'state) "PENDING")
                                          "in_progress"
                                        "completed"))
                           (url . ,(map-elt node 'targetUrl)))
                       (let ((url (map-elt node 'detailsUrl))
                             (conclusion (map-elt node 'conclusion))
                             (status (map-elt node 'status)))
                         `((workflowName . ,(map-elt node 'name))
                           (conclusion . ,(when (stringp conclusion) (downcase conclusion)))
                           (status . ,(when (stringp status) (downcase status)))
                           (databaseId . ,(when (and (stringp url)
                                                     (string-match "/actions/runs/\\([0-9]+\\)" url))
                                            (string-to-number (match-string 1 url))))
                           (url . ,url)))))))
               nodes))))
      (if (and runs (> (length runs) 0))
          (let* ((result (magit-dash-gh-ci--parse-runs runs))
                 (state (map-elt rollup 'state))
                 (total (map-elt contexts 'totalCount))
                 (truncated (and (numberp total) (> total (length runs)))))
            (when truncated
              (setq result (plist-put result :truncated t)))
            ;; The rollup covers all contexts, including those beyond the
            ;; first page.  Do not report a pass when it reports otherwise.
            (when (and (or truncated
                           (equal (plist-get result :conclusion) "success"))
                       (not (magit-dash-gh-ci--failure-p
                             (plist-get result :conclusion)))
                       (member state '("FAILURE" "ERROR" "PENDING" "EXPECTED")))
              (setq result (plist-put result :conclusion
                                      (when (member state '("FAILURE" "ERROR"))
                                        "failure")))
              (setq result (plist-put result :status
                                      (if (member state '("FAILURE" "ERROR"))
                                          "completed"
                                        "in_progress")))
              ;; No specific returned check explains the aggregate state.
              (setq result (plist-put result :run-id nil))
              (setq result (plist-put result :url nil)))
            result)
        (let ((state (and (not (eq (map-elt rollup 'state) :null))
                          (map-elt rollup 'state))))
          (cond
           ((equal state "SUCCESS")
            (list :conclusion "success" :status "completed" :pass 1 :fail 0 :total 1))
           ((member state '("FAILURE" "ERROR"))
            (list :conclusion "failure" :status "completed" :pass 0 :fail 1 :total 1))
           ((member state '("PENDING" "EXPECTED"))
            (list :conclusion nil :status "in_progress" :pass 0 :fail 0 :total 0))
           (t (list :none t))))))))

(defun magit-dash-gh-ci-fetch-batch-graphql (repos on-done &optional on-repo-done)
  "Batch-fetch CI status for REPOS using a single GitHub GraphQL query.
Calls ON-REPO-DONE with (REPO CI-STATUS) for each successfully parsed repo.
Calls ON-DONE with the list of remaining unhandled repos (e.g. repos that
could not be queried via GraphQL or whose queries failed)."
  (let* ((guards (bound-and-true-p magit-dash--refresh-guards))
         (entries nil)
         (unhandled nil)
         (idx 0))
    (dolist (repo repos)
      (let* ((path (magit-dash-repo-path repo))
             (slug (ignore-errors (magit-dash-ci--repo-slug repo)))
             (current (magit-dash--current-branch path))
             (branch (if (string-empty-p current)
                         (magit-dash-repo-branch repo)
                       current)))
        (if (and slug (string-match-p "/" slug) branch)
            (push (list (format "r%d" (setq idx (1+ idx))) repo slug branch) entries)
          (push repo unhandled))))
    (if (null entries)
        (when on-done (funcall on-done repos))
      (let ((query (magit-dash-gh-ci--build-batch-graphql-query entries)))
        (magit-dash-gh--run-process
         (list "api" "graphql" "-f" (concat "query=" query))
         (expand-file-name "~")
         (lambda (output)
           (let* ((data (condition-case nil
                            (map-elt (json-parse-string output
                                                        :array-type 'list
                                                        :object-type 'alist)
                                     'data)
                          (error nil)))
                  (remaining (copy-sequence unhandled)))
             (if (null data)
                 (when on-done (funcall on-done repos))
               (dolist (item entries)
                 (pcase-let* ((`(,alias ,repo ,_slug ,_branch) item)
                              (path (magit-dash-repo-path repo))
                              (repo-data (or (map-elt data (intern alias))
                                             (map-elt data alias)))
                              (target (and repo-data (not (eq repo-data :null))
                                           (or (listp repo-data) (hash-table-p repo-data))
                                           (let ((r (map-elt repo-data 'ref)))
                                             (and r (not (eq r :null))
                                                  (or (listp r) (hash-table-p r))
                                                  (map-elt r 'target)))))
                              (rollup (and target (not (eq target :null))
                                           (or (listp target) (hash-table-p target))
                                           (map-elt target 'statusCheckRollup)))
                              (ci-status (magit-dash-gh-ci--parse-graphql-rollup rollup)))
                   (if (and ci-status (magit-dash-gh--refresh-current-p guards path))
                       (progn
                         (magit-dash-gh--cache-set path :ci-status ci-status)
                         (seq-do
                          (lambda (b)
                            (when (buffer-live-p b)
                              (with-current-buffer b
                                (when (and (derived-mode-p 'magit-dash-mode)
                                           (fboundp 'magit-dash--update-entry))
                                  (magit-dash--update-entry repo)))))
                          (buffer-list))
                         (when on-repo-done
                           (funcall on-repo-done repo ci-status)))
                     (push repo remaining))))
               (when on-done (funcall on-done remaining)))))
         (lambda (_err _code)
           (when on-done (funcall on-done repos))))))))

(defun magit-dash-gh-ci-fetch (repo callback)
  "Fetch CI status for REPO asynchronously and call CALLBACK with the result.
CALLBACK receives a CI status plist (see `magit-dash-gh-ci--parse-runs') or
nil on error.  Does nothing when REPO does not have :include-ci set.
Always queries the branch currently checked out at REPO's path rather than a
possibly-stale cached branch, so the CI status reflects what is actually
checked out — important for worktrees, which are frequently switched to a
different branch than their cached stats reflect.
Caches the result as :ci-status in the shared magit-dash-gh cache."
  (when (magit-dash-repo-include-ci repo)
    (let* ((path (magit-dash-repo-path repo))
           (token (magit-dash-gh--cache-request path :ci-status))
           (current (magit-dash--current-branch path))
           (branch (if (string-empty-p current)
                       (magit-dash-repo-branch repo)
                     current))
           (slug (ignore-errors (magit-dash-ci--repo-slug repo)))
           (head-sha (ignore-errors
                       (let ((default-directory (file-name-as-directory path)))
                         (magit-git-string "rev-parse" "HEAD")))))
      (if (not branch)
          (funcall callback nil)
        (magit-dash-gh--run-process
         (append
          (list "run" "list")
          (when slug (list "--repo" slug))
          (list "--branch" branch
                "--limit" "20"
                "--json" "databaseId,name,status,conclusion,url,workflowName,headSha"))
         path
         (lambda (output)
           (let* ((parsed-p t)
                  (runs (condition-case nil
                            (json-parse-string output
                                               :array-type 'list
                                               :object-type 'alist)
                          (error (setq parsed-p nil) nil)))
                  (ci-status (when parsed-p
                               (or (if head-sha
                                       (magit-dash-gh-ci--parse-runs runs head-sha)
                                     (magit-dash-gh-ci--parse-runs runs))
                                   (list :none t)))))
             (when parsed-p
               (magit-dash-gh--cache-set-if-current path :ci-status ci-status token))
             (funcall callback (if (plist-get ci-status :none) nil ci-status))))
         (lambda (_ _)
           (funcall callback nil)))))))

;;;###autoload
(defun magit-dash-gh-ci-fetch-batch (repos &rest args)
  "Fetch CI status for REPOS using GraphQL batching or `gen-run-async' with bounded concurrency.
Concurrency is bounded by `magit-dash-max-concurrent-ci-jobs'.
ARGS may contain an optional positional ON-DONE callback or keyword arguments:
  :on-done FUNCTION
  :on-repo-done FUNCTION
  :concurrency INTEGER
  :delay FLOAT
  :use-graphql BOOLEAN
Returns a cancellation handle."
  (let* ((guards (bound-and-true-p magit-dash--refresh-guards))
         (on-done (when (and (car args) (not (keywordp (car args))))
                    (pop args)))
         (on-done (or (plist-get args :on-done) on-done))
         (on-repo-done (plist-get args :on-repo-done))
         (limit (or (plist-get args :concurrency)
                    (bound-and-true-p magit-dash-max-concurrent-ci-jobs)
                    (max 2 (round (* 1.5 (if (fboundp 'num-processors)
                                             (num-processors)
                                           4))))))
         (yield (or (plist-get args :delay)
                    (bound-and-true-p magit-dash-queue-yield-delay)
                    0.0))
         (use-graphql (if (plist-member args :use-graphql)
                          (plist-get args :use-graphql)
                        magit-dash-gh-ci-use-graphql))
         (ci-repos (seq-filter #'magit-dash-repo-include-ci repos)))
    (if (null ci-repos)
        (progn
          (when on-done (funcall on-done))
          (lambda (&rest _) nil))
      (letrec ((run-individual
                (lambda (remaining-repos)
                  (if (null remaining-repos)
                      (progn
                        (when on-done (funcall on-done))
                        (lambda (&rest _) nil))
                    (gen-run-async
                     (lambda (repo done)
                       (if (not (magit-dash-gh--refresh-current-p guards (magit-dash-repo-path repo)))
                           (funcall done)
                         (magit-dash-gh-ci-fetch
                          repo
                          (lambda (ci-status)
                            (seq-do
                             (lambda (b)
                               (when (buffer-live-p b)
                                 (with-current-buffer b
                                   (when (and (derived-mode-p 'magit-dash-mode)
                                              (fboundp 'magit-dash--update-entry))
                                     (magit-dash--update-entry repo)))))
                             (buffer-list))
                            (when on-repo-done
                              (funcall on-repo-done repo ci-status))
                            (funcall done)))))
                     remaining-repos
                     :concurrency limit
                     :delay yield
                     :on-done on-done)))))
        (if (and use-graphql (> (length ci-repos) 1))
            (progn
              (magit-dash-gh-ci-fetch-batch-graphql
               ci-repos
               (lambda (unhandled)
                 (funcall run-individual unhandled))
               on-repo-done)
              (lambda (&rest _) nil))
          (funcall run-individual ci-repos))))))

;;; Public commands

;;;###autoload
(defun magit-dash-gh-ci-open-last-run (repo)
  "Open the URL of the most recent CI run for REPO in the browser.
Does nothing when no CI status is cached for REPO."
  (when-let* ((path (magit-dash-repo-path repo))
              (ci (magit-dash-gh--cache-get path :ci-status))
              (url (plist-get ci :url)))
    (browse-url url)))

;;; Fix-CI prompt dispatch

(declare-function agent-shell-workflow-dispatch "agent-shell-workflow")
(declare-function agent-shell-workflow-exec "agent-shell-workflow")

(defun magit-dash-ci--dispatch-prompt (id args &optional context-dir)
  "Dispatch prompt ID with ARGS plist in optional CONTEXT-DIR.
Calls dispatch (or fallback exec) with `:args' ARGS and `:context-dir'
CONTEXT-DIR."
  (let ((fn (cond ((fboundp 'agent-shell-workflow-dispatch) #'agent-shell-workflow-dispatch)
                  ((fboundp 'agent-shell-workflow-exec) #'agent-shell-workflow-exec)
                  (t (user-error "magit-dash fix-CI requires the agent-shell-workflow library")))))
    (if context-dir
        (funcall fn id :args args :context-dir context-dir)
      (funcall fn id :args args))))

(defun magit-dash-ci--download-and-dispatch (repo run-id)
  "Dispatch the `fix-ci' prompt library workflow for REPO and RUN-ID.
Requires `agent-shell-workflow-dispatch' from `agent-shell-workflow'."
  (let* ((path (magit-dash-repo-path repo))
         (slug (magit-dash-ci--repo-slug repo))
         (default-directory (file-name-as-directory path)))
    (message "Dispatching fix-CI workflow for %s (run %s)..." slug run-id)
    (magit-dash-ci--dispatch-prompt 'fix-ci (list :repo slug :run-id run-id) default-directory)))

;;;###autoload
(defun magit-dash-ci-dispatch-fix-operation (repo)
  "Dispatch the `fix-ci' prompt library workflow for REPO.
Invokes workflow dispatch with `:repo' set to REPO's OWNER/NAME slug,
and passes cached failing `:run-id' if available."
  (unless (magit-dash-repo-include-ci repo)
    (user-error "magit-dash fix-CI: %s does not have CI enabled (:include-ci)"
                (magit-dash-repo-name repo)))
  (let* ((path (magit-dash-repo-path repo))
         (slug (magit-dash-ci--repo-slug repo))
         (default-directory (file-name-as-directory path))
         (ci-status (magit-dash-gh--cache-get path :ci-status))
         (run-id (and ci-status (plist-get ci-status :run-id)))
         (args (if run-id (list :repo slug :run-id run-id) (list :repo slug))))
    (if run-id
        (message "Dispatching fix-CI workflow for %s (run %s)..." slug run-id)
      (message "Dispatching fix-CI workflow for %s..." slug))
    (magit-dash-ci--dispatch-prompt 'fix-ci args default-directory)))

;;; Synchronous CI failure and log fetch for MCP and workflows

(defun magit-dash-gh-ci-tail-log (log-text &optional tail-lines)
  "Return the last TAIL-LINES of LOG-TEXT (capped at 200, default 100)."
  (if (and (stringp log-text) (not (string-empty-p (string-trim log-text))))
      (let* ((lines (split-string (replace-regexp-in-string "\r\n" "\n" log-text) "\n"))
             (cap (min (or tail-lines 100) 200))
             (tail (last lines cap)))
        (string-join tail "\n"))
    ""))

(defun magit-dash-gh-ci-fetch-run-log (slug run-id &optional tail-lines dir)
  "Fetch the failed-step log for RUN-ID in SLUG synchronously.
Returns the tail of the log up to TAIL-LINES (capped at 200)."
  (pcase-let* ((`(,code . ,output)
                (magit-dash-gh--call-process
                 (list "run" "view" (format "%s" run-id)
                       "--repo" slug
                       "--log-failed")
                 (or dir default-directory))))
    (if (eq code 0)
        (magit-dash-gh-ci-tail-log output tail-lines)
      "")))

(defun magit-dash-gh-ci-pr-failures (slug number &optional tail-lines dir)
  "Fetch failing checks and failed-step log tail for PR NUMBER in SLUG.
Returns a plist with :repo :number :untrusted :failing_checks :run_id :log_tail :log_lines."
  (let* ((cap-lines (min (or tail-lines 100) 200))
         (pr-json (pcase-let* ((`(,code . ,output)
                                (magit-dash-gh--call-process
                                 (list "pr" "view" (format "%d" number)
                                       "--repo" slug
                                       "--json" "statusCheckRollup,headRefName")
                                 (or dir default-directory))))
                    (unless (eq code 0)
                      (error "Failed to fetch PR #%d for %s: %s" number slug output))
                    (condition-case err
                        (json-parse-string output :array-type 'list :object-type 'alist)
                      (error (error "Failed to parse PR view response: %s"
                                    (error-message-string err))))))
         (rollup (map-elt pr-json 'statusCheckRollup))
         (head-ref (map-elt pr-json 'headRefName))
         (failing-checks nil)
         (run-id nil))
    (when (and rollup (not (eq rollup :null)) (listp rollup))
      (dolist (item rollup)
        (when (and item (not (eq item :null)) (or (listp item) (hash-table-p item)))
          (let* ((conclusion (or (map-elt item 'conclusion) (map-elt item 'state) ""))
                 (name (or (map-elt item 'name) (map-elt item 'context) "unknown"))
                 (url (or (map-elt item 'detailsUrl) (map-elt item 'targetUrl) "")))
            (when (member (upcase (format "%s" conclusion))
                          '("FAILURE" "TIMED_OUT" "STARTUP_FAILURE" "ACTION_REQUIRED"))
              (push (list :name name
                          :conclusion (downcase (format "%s" conclusion))
                          :url url)
                    failing-checks)
              (when (and (null run-id) (string-match "actions/runs/\\([0-9]+\\)" url))
                (setq run-id (string-to-number (match-string 1 url)))))))))
    (setq failing-checks (nreverse failing-checks))
    (when (and (null run-id) head-ref failing-checks)
      (ignore-errors
        (pcase-let* ((`(,code . ,output)
                      (magit-dash-gh--call-process
                       (list "run" "list"
                             "--repo" slug
                             "--branch" head-ref
                             "--limit" "5"
                             "--json" "databaseId,conclusion")
                       (or dir default-directory))))
          (when (eq code 0)
            (let ((runs (json-parse-string output :array-type 'list :object-type 'alist)))
              (when-let* ((failed (seq-find (lambda (r)
                                              (member (map-elt r 'conclusion)
                                                      '("failure" "timed_out" "startup_failure")))
                                            runs)))
                (setq run-id (map-elt failed 'databaseId))))))))
    (let* ((log-tail (if run-id
                         (magit-dash-gh-ci-fetch-run-log slug run-id cap-lines dir)
                       ""))
           (lines (if (and log-tail (not (string-empty-p log-tail)))
                      (length (split-string log-tail "\n"))
                    0)))
      (list :repo slug
            :number number
            :untrusted t
            :run_id (or run-id :null)
            :failing_checks (vconcat failing-checks)
            :log_tail log-tail
            :log_lines lines))))

(provide 'magit-dash-gh-ci)
;;; magit-dash-gh-ci.el ends here
