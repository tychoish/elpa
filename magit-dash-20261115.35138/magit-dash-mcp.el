;;; magit-dash-mcp.el --- MCP tool integration for magit-dash -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1") (hitl "0.1"))
;; Keywords: vc, tools, magit, github, mcp
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; Provides `mcpkit' tools for higher-level `magit-dash' PR, worktree,
;; and branch pruning operations:
;;   - `manage_worktrees'
;;   - `create_worktree_for_pr'
;;   - `clean_merged_worktrees'
;;   - `prune_branches'
;;   - `magit_dash_pr_status'
;;   - `chain_plan' and `chain_resume' (PR chain merge plans, see `magit-dash-chain')
;;   - `stack_show' and `stack_sync' (local branch stacks, see `magit-dash-stack')

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'mcpkit)
(require 'magit-dash-gh)
(require 'magit-dash-gh-pr)
(require 'magit-dash-gh-pr-status)
(require 'magit-dash-gh-ci)
(require 'hitl-plan-mcp nil t)
(require 'magit-dash-worktree)
(require 'magit-dash-chain)
(require 'magit-dash-stack)

(declare-function magit-dash-repo-name "magit-dash")
(declare-function magit-dash-repo-path "magit-dash")
(declare-function magit-dash-repo-repo "magit-dash")
(declare-function magit-dash-repo-worktree "magit-dash")
(defvar magit-dash-repo-list)

;;;; Service definition

(defvar magit-dash-mcp-service
  (or (mcpkit-get-service 'magit-dash)
      (mcpkit-define-service 'magit-dash
        :description "Magit Dash Git Worktree, PR, and Branch Management Service"))
  "The magit-dash `mcpkit-service' instance.")

;;;###autoload
(defun magit-dash-mcp-start ()
  "Start and register the `magit-dash' MCP service with the Emacs MCP server."
  (interactive)
  (mcpkit-start-service 'magit-dash :on-collision 'namespace))

(defun magit-dash-mcp-call-tool (tool-name &optional args)
  "Invoke registered tool TOOL-NAME synchronously with ARGS plist."
  (let* ((svc (mcpkit-get-service 'magit-dash))
         (tools (and svc (mcpkit-service-tools svc)))
         (tool (and tools (gethash tool-name tools))))
    (unless tool
      (error "Unknown magit-dash-mcp tool: %s" tool-name))
    (let (result err-msg)
      (funcall (mcpkit-tool-handler tool)
               args
               (lambda (err res)
                 (if err
                     (setq err-msg err)
                   (setq result res))))
      (when err-msg
        (error "%s" err-msg))
      result)))

;;;; Repository and PR resolution helpers

(defun magit-dash-mcp--resolve-repo-dir (repo)
  "Resolve REPO (a local path, `owner/name' slug, or registered repo name) to an absolute path."
  (unless (and (stringp repo) (not (string-empty-p (string-trim repo))))
    (error "Repository argument `repo' is required"))
  (let* ((trimmed (string-trim repo))
         (expanded (expand-file-name trimmed))
         (local-dir
          (cond
           ((file-directory-p expanded)
            expanded)
           ((fboundp 'magit-dash-gh-pr-dashboard--find-local-path)
            (or (magit-dash-gh-pr-dashboard--find-local-path trimmed)
                (when-let* ((r (seq-find (lambda (it)
                                           (and (not (magit-dash-repo-worktree it))
                                                (or (equal (magit-dash-repo-name it) trimmed)
                                                    (equal (magit-dash-repo-repo it) trimmed))))
                                         (and (boundp 'magit-dash-repo-list) magit-dash-repo-list))))
                  (magit-dash-repo-path r))))
           (t nil))))
    (unless (and local-dir (file-directory-p local-dir))
      (error "Cannot resolve local repository directory for %s" repo))
    (if (fboundp 'magit-dash-worktree-main-dir)
        (magit-dash-worktree-main-dir local-dir)
      (expand-file-name local-dir))))

(defun magit-dash-mcp--resolve-repo-slug (repo repo-dir)
  "Resolve GitHub `owner/name' slug from REPO or REPO-DIR."
  (if (and (stringp repo)
           (string-match-p "\\`[^/]+/[^/]+\\'" (string-trim repo))
           (not (file-directory-p (expand-file-name repo))))
      (string-trim repo)
    (let ((default-directory repo-dir))
      (or (let ((info (ignore-errors (magit-dash-gh--repo-info))))
            (when (and (plist-get info :owner) (plist-get info :repo))
              (format "%s/%s" (plist-get info :owner) (plist-get info :repo))))
          (when-let* ((r (seq-find (lambda (it)
                                     (and (not (magit-dash-repo-worktree it))
                                          (equal (directory-file-name (expand-file-name (magit-dash-repo-path it)))
                                                 (directory-file-name (expand-file-name repo-dir)))))
                                   (and (boundp 'magit-dash-repo-list) magit-dash-repo-list))))
            (or (magit-dash-repo-repo r)
                (when (string-match-p "/" (magit-dash-repo-name r))
                  (magit-dash-repo-name r))))))))

;;;; Tool 1: manage_worktrees

(mcpkit-register-tool "manage_worktrees" 'magit-dash
  :description "List, create, delete, or review git worktrees for a repository."
  :input-schema
  '(:type "object"
    :properties (:repo (:type "string"
                        :description "Local repository path, owner/repo slug, or registered repo name.")
                 :action (:type "string"
                          :enum ["list" "create" "delete" "review"]
                          :description "Worktree action: list (default), create, delete, or review.")
                 :branch (:type "string"
                          :description "Branch name (required for action=create).")
                 :worktree_path (:type "string"
                                 :description "Target worktree path (optional for create, required for delete).")
                 :base_ref (:type "string"
                            :description "Base ref when creating a new branch worktree (optional)."))
    :required ["repo"])
  (let* ((repo (plist-get args :repo))
         (action (or (plist-get args :action) "list"))
         (repo-dir (magit-dash-mcp--resolve-repo-dir repo))
         (repo-slug (or (magit-dash-mcp--resolve-repo-slug repo repo-dir) repo)))
    (pcase action
      ("list"
       (let* ((all-wts (magit-dash-gh--git-worktrees repo-dir))
              (prs (ignore-errors (magit-dash-gh-pr--fetch-repo-prs repo-slug "all" repo-dir)))
              (def-branch (ignore-errors (let ((default-directory repo-dir))
                                           (magit-dash-gh--default-branch))))
              (git-merged (when def-branch
                            (magit-dash-gh--git-merged-branches def-branch repo-dir)))
              (items
               (seq-map
                (lambda (wt)
                  (let* ((wt-path (alist-get 'path wt))
                         (wt-branch (alist-get 'branch wt))
                         (wt-head (alist-get 'head wt))
                         (main-p (string= (directory-file-name (expand-file-name wt-path))
                                          (directory-file-name (expand-file-name repo-dir))))
                         (dirty (and (not main-p)
                                     (file-directory-p wt-path)
                                     (magit-dash-worktree-uncommitted-changes-p wt-path)))
                         (matched-pr (and prs (magit-dash-gh-pr--match-worktree-to-pr wt prs)))
                         (pr-num (and matched-pr (map-elt matched-pr 'number)))
                         (pr-state (and matched-pr (map-elt matched-pr 'state)))
                         (pr-merged (or (and matched-pr
                                             (magit-dash-gh-pr--pr-merged-p matched-pr repo-slug pr-num repo-dir))
                                        (and wt-branch git-merged
                                             (not (null (member wt-branch git-merged)))))))
                    (list :path wt-path
                          :branch (or wt-branch :null)
                          :head (or wt-head :null)
                          :is_main (if main-p t :false)
                          :dirty (if dirty t :false)
                          :pr_number (or pr-num :null)
                          :pr_state (or pr-state :null)
                          :pr_merged (if pr-merged t :false))))
                all-wts)))
         (list :repo_dir repo-dir
               :action "list"
               :worktrees (vconcat items))))
      ("create"
       (let* ((branch (plist-get args :branch))
              (_ (unless (and (stringp branch) (not (string-empty-p (string-trim branch))))
                   (error "Argument `branch' is required when action is \"create\"")))
              (wt-path (or (plist-get args :worktree_path)
                           (magit-dash-gh-pr--compute-worktree-path repo-dir branch)))
              (base-ref (plist-get args :base_ref))
              (created (magit-dash-worktree-create repo-dir wt-path branch base-ref)))
         (list :repo_dir repo-dir
               :action "create"
               :branch branch
               :worktree_path created)))
      ("delete"
       (let* ((wt-path (plist-get args :worktree_path))
              (_ (unless (and (stringp wt-path) (not (string-empty-p (string-trim wt-path))))
                   (error "Argument `worktree_path' is required when action is \"delete\""))))
         (magit-dash-gh-pr-delete-worktree wt-path repo-dir)
         (list :repo_dir repo-dir
               :action "delete"
               :worktree_path (expand-file-name wt-path)
               :deleted t)))
      ("review"
       (let ((res (magit-dash-gh-pr-review-worktrees repo-dir nil)))
         (list :repo_dir repo-dir
               :action "review"
               :renamed (or (plist-get res :renamed) 0)
               :deleted (or (plist-get res :deleted) 0)
               :matching (or (plist-get res :matching) 0)
               :skipped (or (plist-get res :skipped) 0))))
      (_
       (error "Unsupported manage_worktrees action: %s" action)))))

;;;; Tool 2: create_worktree_for_pr

(mcpkit-register-tool "create_worktree_for_pr" 'magit-dash
  :description "Create git worktree(s) for a specific PR, a list of PRs, or all open PRs in a repository."
  :input-schema
  '(:type "object"
    :properties (:repo (:type "string"
                        :description "Local repository path, owner/repo slug, or registered repo name.")
                 :pr_number (:type "integer"
                             :description "Single pull request number to create a worktree for.")
                 :pr_numbers (:type "array"
                              :items (:type "integer")
                              :description "Multiple pull request numbers to create worktrees for.")
                 :all_open (:type "boolean"
                            :description "When true (or when no PR numbers are given), create worktrees for all open PRs."))
    :required ["repo"])
  (let* ((repo (plist-get args :repo))
         (repo-dir (magit-dash-mcp--resolve-repo-dir repo))
         (repo-slug (or (magit-dash-mcp--resolve-repo-slug repo repo-dir) repo))
         (pr-number (plist-get args :pr_number))
         (pr-numbers (append (when pr-number (list pr-number))
                             (seq-into (plist-get args :pr_numbers) 'list)))
         (all-open (or (eq (plist-get args :all_open) t)
                       (null pr-numbers))))
    (if (not all-open)
        (let ((created-paths nil)
              (failed 0))
          (dolist (num (seq-uniq pr-numbers))
            (let* ((entry (list :number num :repo repo-slug :pr nil))
                   (wt-path (condition-case nil
                                (magit-dash-gh-pr--create-worktree-for-entry entry nil)
                              (error nil))))
              (if wt-path
                  (push (list :pr_number num :worktree_path wt-path) created-paths)
                (cl-incf failed))))
          (setq created-paths (nreverse created-paths))
          (list :repo_dir repo-dir
                :repo repo-slug
                :created (length created-paths)
                :failed failed
                :worktree_path (if (= (length created-paths) 1)
                                   (plist-get (car created-paths) :worktree_path)
                                 :null)
                :worktrees (vconcat created-paths)))
      ;; Create worktrees for all open PRs in repo-dir
      (magit-dash-gh-pr-review-worktrees repo-dir nil)
      (let* ((open-prs (magit-dash-gh-pr--fetch-repo-prs repo-slug "open" repo-dir))
             (wts (magit-dash-gh--git-worktrees repo-dir))
             (created 0)
             (existing 0)
             (failed 0)
             (worktree-results nil))
        (dolist (pr open-prs)
          (let* ((num (map-elt pr 'number))
                 (head (map-elt pr 'headRefName))
                 (expected-path (magit-dash-gh-pr--compute-worktree-path repo-dir (format "pr-%d" num)))
                 (matched-wt (seq-find
                              (lambda (wt)
                                (let ((wt-path (alist-get 'path wt)))
                                  (and wt-path
                                       (not (string= (directory-file-name (expand-file-name wt-path))
                                                     (directory-file-name (expand-file-name repo-dir))))
                                       (or (string= (directory-file-name (expand-file-name wt-path))
                                                    (directory-file-name (expand-file-name expected-path)))
                                           (equal (magit-dash-gh-pr--match-worktree-to-pr wt (list pr)) pr)))))
                              wts)))
            (if matched-wt
                (progn
                  (cl-incf existing)
                  (push (list :pr_number num
                              :worktree_path (alist-get 'path matched-wt)
                              :status "existing")
                        worktree-results))
              (let* ((entry (list :number num :repo repo-slug :pr pr))
                     (wt-path (condition-case nil
                                  (magit-dash-gh-pr--create-worktree-for-entry entry nil)
                                (error nil))))
                (if wt-path
                    (progn
                      (cl-incf created)
                      (push (list :pr_number num
                                  :worktree_path wt-path
                                  :status "created")
                            worktree-results))
                  (cl-incf failed))))))
        (list :repo_dir repo-dir
              :repo repo-slug
              :created created
              :existing existing
              :failed failed
              :worktrees (vconcat (nreverse worktree-results)))))))

;;;; Tool 3: clean_merged_worktrees

(mcpkit-register-tool "clean_merged_worktrees" 'magit-dash
  :description "Clean up local git worktrees whose associated pull requests have been merged."
  :input-schema
  '(:type "object"
    :properties (:repo (:type "string"
                        :description "Local repository path, owner/repo slug, or registered repo name.")
                 :pr_numbers (:type "array"
                              :items (:type "integer")
                              :description "Optional specific PR numbers to clean worktrees for."))
    :required ["repo"])
  (let* ((repo (plist-get args :repo))
         (repo-dir (magit-dash-mcp--resolve-repo-dir repo))
         (repo-slug (or (magit-dash-mcp--resolve-repo-slug repo repo-dir) repo))
         (pr-numbers (seq-into (plist-get args :pr_numbers) 'list)))
    (if pr-numbers
        (let ((deleted 0)
              (skipped 0))
          (dolist (num pr-numbers)
            (let* ((entry (list :number num :repo repo-slug :pr nil))
                   (res (magit-dash-gh-pr--clean-worktree-for-entry entry nil)))
              (cond
               ((eq res :deleted) (cl-incf deleted))
               ((eq res :skipped) (cl-incf skipped))
               ((listp res)
                (setq deleted (+ deleted (or (plist-get res :deleted) 0))
                      skipped (+ skipped (or (plist-get res :skipped) 0)))))))
          (list :repo_dir repo-dir
                :deleted deleted
                :skipped skipped))
      (let ((res (magit-dash-gh-pr-clean-merged-worktrees repo-dir nil)))
        (list :repo_dir repo-dir
              :deleted (or (plist-get res :deleted) 0)
              :skipped (or (plist-get res :skipped) 0))))))

;;;; Tool 4: prune_branches

(mcpkit-register-tool "prune_branches" 'magit-dash
  :description "Scan and prune local branches whose PRs are merged/closed or merged into the default branch, optionally cleaning merged worktrees first."
  :input-schema
  '(:type "object"
    :properties (:repo (:type "string"
                        :description "Local repository path, owner/repo slug, or registered repo name.")
                 :dry_run (:type "boolean"
                           :description "When true, only scan and return candidate branches without deleting them.")
                 :clean_worktrees_first (:type "boolean"
                                         :description "When true (default), clean merged worktrees before pruning branches."))
    :required ["repo"])
  (let* ((repo (plist-get args :repo))
         (repo-dir (magit-dash-mcp--resolve-repo-dir repo))
         (dry-run (eq (plist-get args :dry_run) t))
         (clean-wts (if (plist-member args :clean_worktrees_first)
                        (not (memq (plist-get args :clean_worktrees_first) '(nil :false :json-false)))
                      t))
         (wt-res (when (and clean-wts (not dry-run))
                   (magit-dash-gh-pr-clean-merged-worktrees repo-dir nil)))
         (default-directory repo-dir)
         (def-branch (ignore-errors (magit-dash-gh--default-branch)))
         (candidates (magit-dash-gh--prune-scan))
         (branch-names (seq-map #'car candidates))
         (del-res (when (and branch-names (not dry-run))
                    (magit-dash-gh--prune-delete-branches branch-names repo-dir nil))))
    (list :repo_dir repo-dir
          :default_branch (or def-branch "main")
          :dry_run (if dry-run t :false)
          :candidate_branches (vconcat branch-names)
          :deleted_count (if dry-run 0 (or (plist-get del-res :deleted) 0))
          :skipped_count (if dry-run 0 (or (plist-get del-res :skipped) 0))
          :worktrees_cleaned (if wt-res (or (plist-get wt-res :deleted) 0) 0))))

;;;; Tool 5: magit_dash_pr_status

(defun magit-dash-mcp--json-value (value)
  "Return VALUE converted for JSON: symbols to strings, nil to `:null'."
  (cond ((null value) :null)
        ((eq value t) t)
        ((keywordp value) (substring (symbol-name value) 1))
        ((symbolp value) (symbol-name value))
        (t value)))

(defun magit-dash-mcp--pr-json (pr)
  "Return PR (a plist with `:tracking') as a JSON-serializable plist."
  (let ((tr (plist-get pr :tracking)))
    (list :number (plist-get pr :number)
          :repo (magit-dash-mcp--json-value (plist-get pr :repo))
          :title (magit-dash-mcp--json-value (plist-get pr :title))
          :url (magit-dash-mcp--json-value (plist-get pr :url))
          :branch (magit-dash-mcp--json-value (plist-get pr :branch))
          :base (magit-dash-mcp--json-value (plist-get pr :base))
          :state (magit-dash-mcp--json-value (plist-get pr :state))
          :draft (if (plist-get pr :draft) t :false)
          :bucket (magit-dash-mcp--json-value (plist-get pr :bucket))
          :approvals (or (plist-get pr :approvals) 0)
          :changes_requested (or (plist-get pr :changes-requested) 0)
          :requested (vconcat (plist-get pr :requested))
          :ci (magit-dash-mcp--json-value (plist-get pr :ci))
          :failing (vconcat (mapcar (lambda (f) (if (consp f) (car f) f))
                                    (plist-get pr :failing)))
          :threads_open (or (plist-get pr :threads-open) 0)
          :threads_outdated (or (plist-get pr :threads-outdated) 0)
          :ticket (magit-dash-mcp--json-value (plist-get pr :ticket))
          :tracking
          (list :last_seen (magit-dash-mcp--json-value (plist-get tr :last-seen))
                :previous_bucket (magit-dash-mcp--json-value (plist-get tr :previous-bucket))
                :delta_type (magit-dash-mcp--json-value (plist-get tr :delta-type))
                :delta_reason (magit-dash-mcp--json-value (plist-get tr :delta-reason))
                :action_summary (magit-dash-mcp--json-value (plist-get tr :action-summary))
                :failing_checks (vconcat (plist-get tr :failing-checks))
                :threads_open (or (plist-get tr :threads-open) 0)
                :approvals (or (plist-get tr :approvals) 0)
                :stack_parent (magit-dash-mcp--json-value (plist-get tr :stack-parent))
                :ticket (magit-dash-mcp--json-value (plist-get tr :ticket))))))

(mcpkit-register-tool "magit_dash_pr_status" 'magit-dash
  :description "Return the author's open PRs classified into next-step buckets, with tracking deltas since the previous call. Without `repo', covers all repositories. Returns data only; writes no files. Read-only by default: the tracking cache is advanced (consuming deltas) only when `commit' is true."
  :input-schema
  '(:type "object"
    :properties (:repo (:type "string"
                        :description "Optional local repository path, owner/repo slug, or registered repo name. Omit for all repositories.")
                :commit (:type "boolean"
                         :description "When true, record this snapshot so the next call reports deltas against it. Default false: inspection does not mutate tracking state."))
    :required [])
  (let* ((repo (plist-get args :repo))
         (all (or (null repo) (and (stringp repo) (string-empty-p (string-trim repo)))))
         (repo-dir (and (not all) (magit-dash-mcp--resolve-repo-dir repo)))
         (repo-slug (and (not all)
                         (or (magit-dash-mcp--resolve-repo-slug repo repo-dir) repo)))
         (prs (if all
                  (magit-dash-gh-pr-status-collect-all)
                (magit-dash-gh-pr-status-collect repo-slug repo-dir)))
         (tracked (mapcar #'magit-dash-gh-pr-attach-tracking prs)))
    (when (eq (plist-get args :commit) t)
      (magit-dash-gh-pr-commit-tracking prs))
    (list :repo (or repo-slug :null)
          :count (length tracked)
          :fingerprint (magit-dash-gh-pr-tracking-fingerprint prs)
          :prs (vconcat (mapcar #'magit-dash-mcp--pr-json tracked)))))

(defmacro magit-dash-mcp--guard (&rest body)
  "Execute BODY, catching errors and returning (:error MESSAGE) plist."
  `(condition-case err
       (progn ,@body)
     (error (list :error (error-message-string err)))))

(mcpkit-register-tool "magit_dash_pr_threads" 'magit-dash
  :description "Fetch unresolved review threads for a pull request (author, path, body, outdated status). Text returned from GitHub is marked untrusted. Thread count is capped at `limit' (max 50, default 20) and comment text is bounded."
  :input-schema
  '(:type "object"
    :properties (:repo (:type "string"
                        :description "Repository path, owner/repo slug, or registered repo name.")
                :number (:type "integer"
                         :description "Pull request number.")
                :limit (:type "integer"
                        :description "Maximum number of unresolved threads to return (default 20, max 50).")
                :max_chars (:type "integer"
                            :description "Maximum characters per comment body (default 1000)."))
    :required ["repo" "number"])
  (magit-dash-mcp--guard
    (let* ((repo (plist-get args :repo))
           (number (plist-get args :number))
           (_ (unless (and repo (stringp repo) (not (string-empty-p (string-trim repo))))
                (error "Repository argument `repo' is required")))
           (_ (unless (and (numberp number) (> number 0))
                (error "PR `number' must be a positive integer")))
           (repo-dir (ignore-errors (magit-dash-mcp--resolve-repo-dir repo)))
           (slug (or (ignore-errors (magit-dash-mcp--resolve-repo-slug repo repo-dir))
                     (if (string-match-p "\\`[^/]+/[^/]+\\'" (string-trim repo))
                         (string-trim repo)
                       (error "Cannot resolve repository slug for %s" repo))))
           (limit (plist-get args :limit))
           (max-chars (plist-get args :max_chars))
           (threads (magit-dash-gh-pr-fetch-threads-graphql slug number limit max-chars repo-dir)))
      (list :repo slug
            :number number
            :untrusted t
            :count (length threads)
            :threads (vconcat threads)))))

(mcpkit-register-tool "magit_dash_pr_ci_failures" 'magit-dash
  :description "Fetch failing checks and a bounded failed-step log tail for a pull request. Text returned from GitHub is marked untrusted. Log tail is capped at `tail_lines' (max 200, default 100)."
  :input-schema
  '(:type "object"
    :properties (:repo (:type "string"
                        :description "Repository path, owner/repo slug, or registered repo name.")
                :number (:type "integer"
                         :description "Pull request number.")
                :tail_lines (:type "integer"
                             :description "Maximum lines of failed log tail (default 100, max 200)."))
    :required ["repo" "number"])
  (magit-dash-mcp--guard
    (let* ((repo (plist-get args :repo))
           (number (plist-get args :number))
           (_ (unless (and repo (stringp repo) (not (string-empty-p (string-trim repo))))
                (error "Repository argument `repo' is required")))
           (_ (unless (and (numberp number) (> number 0))
                (error "PR `number' must be a positive integer")))
           (repo-dir (ignore-errors (magit-dash-mcp--resolve-repo-dir repo)))
           (slug (or (ignore-errors (magit-dash-mcp--resolve-repo-slug repo repo-dir))
                     (if (string-match-p "\\`[^/]+/[^/]+\\'" (string-trim repo))
                         (string-trim repo)
                       (error "Cannot resolve repository slug for %s" repo))))
           (tail-lines (plist-get args :tail_lines)))
      (magit-dash-gh-ci-pr-failures slug number tail-lines repo-dir))))

;;;; Tools 6-7: chain_plan, chain_resume

(mcpkit-register-tool "chain_plan" 'magit-dash
  :description "Discover and validate a chain or tree of dependent PRs (PR base -> head edges) under a root PR or branch, render it, and create a hitl-plan (domain magit-dash-chain) that merges parent/base branches into each child with --no-ff merge commits in topological order. Never rebases. Nothing is executed and nothing is pushed; with land=true the plan also contains separate push/merge-PR steps that need plan approval. No plan is created when validation fails. Execute with hitl plan_execute (needs approval) and resume after a conflict with chain_resume."
  :input-schema
  '(:type "object"
    :properties (:repo (:type "string"
                        :description "Local repository path, owner/repo slug, or registered repo name.")
                :root (:type "string"
                       :description "Root of the chain: PR number or branch name.")
                :base (:type "string"
                       :description "Optional branch the chain lands on (default: the root PR's base).")
                :land (:type "boolean"
                       :description "Also add explicit push + gh pr merge steps (default false)."))
    :required ["repo" "root"])
  (let* ((repo (plist-get args :repo))
         (repo-dir (magit-dash-mcp--resolve-repo-dir repo))
         (slug (magit-dash-mcp--resolve-repo-slug repo repo-dir))
         (base (plist-get args :base)))
    (magit-dash-chain-plan-json repo-dir slug (plist-get args :root)
                                (and (stringp base) (not (string-empty-p base)) base)
                                (eq (plist-get args :land) t)
                                'mcp)))

(mcpkit-register-tool "chain_resume" 'magit-dash
  :description "Resume a chain plan after a halt (typically a merge conflict): retries failed/blocked steps, finishing an in-progress merge once its conflicts are resolved, then continues with the remaining steps. Subject to the same approval gating as plan_execute. Use hitl plan_status for read-only status."
  :input-schema
  '(:type "object"
    :properties (:plan_id (:type "string" :description "Chain plan id."))
    :required ["plan_id"])
  (magit-dash-chain-resume-json (plist-get args :plan_id)))

;;;; Tools 8-9: stack_show, stack_sync

(defun magit-dash-mcp--stack-args (args)
  "Return (REPO-DIR PREFIX BASE) from tool ARGS, signalling on bad input."
  (let ((prefix (plist-get args :prefix))
        (base (plist-get args :base)))
    (unless (and (stringp prefix) (not (string-empty-p (string-trim prefix))))
      (error "Argument `prefix' is required"))
    (list (magit-dash-mcp--resolve-repo-dir (plist-get args :repo))
          prefix
          (and (stringp base) (not (string-empty-p base)) base))))

(defun magit-dash-mcp--stack-discover (repo-dir prefix base)
  "Discover the stack of PREFIX in REPO-DIR on BASE, writing nothing."
  (let* ((slug (ignore-errors (magit-dash-mcp--resolve-repo-slug repo-dir repo-dir)))
         (prs (and slug (ignore-errors (magit-dash-chain-fetch-prs slug repo-dir)))))
    (magit-dash-stack-discover repo-dir prefix base prs t)))

(defun magit-dash-mcp--stack-tree (stack)
  "Render STACK as an ASCII tree string via `magit-dash-chain-render-ascii'.
Each bottom's subtree is rendered as a chain whose node :state carries the
commit count and drift; the base line is shown once."
  (let ((base (plist-get stack :base))
        (blocks nil))
    (dolist (bottom (magit-dash-stack--bottoms stack))
      (let* ((chain (magit-dash-stack-to-chain stack (plist-get bottom :branch)))
             (nodes (mapcar
                     (lambda (cn)
                       (let ((n (magit-dash-stack--node stack (plist-get cn :branch))))
                         (append cn
                                 (list :state
                                       (format "%s%s"
                                               (magit-dash-stack--commit-word
                                                (magit-dash-stack--own-count n))
                                               (if (plist-get n :drift)
                                                   (format ", behind %d" (plist-get n :drift))
                                                 ""))))))
                     (plist-get chain :nodes)))
             (text (magit-dash-chain-render-ascii (plist-put chain :nodes nodes))))
        (push (if blocks (cdr (split-string text "\n" t)) (split-string text "\n" t)) blocks)))
    (string-join (if blocks
                     (apply #'append (nreverse blocks))
                   (list (format "%s (base)" base)))
                 "\n")))

(defun magit-dash-mcp--stack-node-json (n)
  "Return the JSON-ready plist for stack node N."
  (let ((commits (plist-get n :commits)))
    (list :branch (plist-get n :branch)
          :parent (magit-dash-mcp--json-value (plist-get n :parent))
          :parent_source (magit-dash-mcp--json-value (plist-get n :parent-source))
          :tip (magit-dash-mcp--json-value (plist-get n :tip))
          :worktree (magit-dash-mcp--json-value (plist-get n :worktree))
          :drift (or (plist-get n :drift) 0)
          :ahead (or (plist-get n :ahead) 0)
          :commit_count (magit-dash-stack--own-count n)
          :merge_commits (length (seq-filter (lambda (c) (plist-get c :merge)) commits))
          :lines_added (apply #'+ (mapcar (lambda (c) (or (plist-get c :added) 0)) commits))
          :lines_removed (apply #'+ (mapcar (lambda (c) (or (plist-get c :removed) 0)) commits)))))

(mcpkit-register-tool "stack_show" 'magit-dash
  :description "Show a stack of dependent local branches matching a glob prefix (for example \"feature/*\"): each branch's parent, worktree, drift (commits of its parent it lacks), commit counts and line totals, plus an ASCII tree and dot and mermaid graphs. Read-only: writes no files and no git config."
  :input-schema
  '(:type "object"
    :properties (:repo (:type "string"
                        :description "Local repository path, owner/repo slug, or registered repo name.")
                :prefix (:type "string"
                         :description "Branch glob selecting the stack, e.g. feature/*.")
                :base (:type "string"
                       :description "Optional base branch (default main or master; origin/main equals main)."))
    :required ["repo" "prefix"])
  (cl-destructuring-bind (repo-dir prefix base) (magit-dash-mcp--stack-args args)
    (let ((stack (magit-dash-mcp--stack-discover repo-dir prefix base)))
      (list :base (plist-get stack :base)
            :prefix prefix
            :nodes (vconcat (mapcar #'magit-dash-mcp--stack-node-json (plist-get stack :nodes)))
            :problems (vconcat (plist-get stack :problems))
            :tree (magit-dash-mcp--stack-tree stack)
            :dot (magit-dash-stack-dot stack)
            :mermaid (magit-dash-stack-mermaid stack)))))

(mcpkit-register-tool "stack_sync" 'magit-dash
  :description "Create a chain plan (domain magit-dash-chain, origin mcp) that merges each parent branch of a local branch stack forward into its children with --no-ff merge commits. Nothing is executed and nothing is pushed or rebased; the plan needs user approval, then run it with hitl plan_execute and resume after a conflict with chain_resume. Returns the plan id and record file. No plan is created when validation fails."
  :input-schema
  '(:type "object"
    :properties (:repo (:type "string"
                        :description "Local repository path, owner/repo slug, or registered repo name.")
                :prefix (:type "string"
                         :description "Branch glob selecting the stack, e.g. feature/*.")
                :base (:type "string"
                       :description "Optional base branch (default main or master).")
                :root (:type "string"
                       :description "Optional bottom branch whose subtree to sync; required when the stack has several bottoms."))
    :required ["repo" "prefix"])
  (cl-destructuring-bind (repo-dir prefix base) (magit-dash-mcp--stack-args args)
    (let* ((root (plist-get args :root))
           (root (and (stringp root) (not (string-empty-p root)) root))
           (stack (magit-dash-mcp--stack-discover repo-dir prefix base))
           (chain (and (cdr (plist-get stack :nodes))
                       (magit-dash-chain-validate (magit-dash-stack-to-chain stack root))))
           (errors (if chain
                       (plist-get chain :errors)
                     (list "Nothing to sync: the stack has fewer than two branches")))
           (plan (and (null errors)
                      (magit-dash-chain-create-plan chain :origin 'mcp))))
      (list :valid (if errors :false t)
            :errors (vconcat errors)
            :warnings (vconcat (and chain (plist-get chain :warnings)))
            :problems (vconcat (plist-get stack :problems))
            :tree (magit-dash-mcp--stack-tree stack)
            :plan_id (if plan (hitl-plan-id plan) :null)
            :record_file (if plan (magit-dash-chain-record-file plan) :null)
            :steps (if plan
                       (vconcat (mapcar (lambda (s) (magit-dash-chain--step-label (hitl-plan-step-target s)))
                                        (hitl-plan-steps plan)))
                     [])))))

(provide 'magit-dash-mcp)

;;; magit-dash-mcp.el ends here
