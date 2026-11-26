;;; magit-dash-mcp.el --- MCP tool integration for magit-dash -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1
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
;;   - `branch_update', `branch_resolve' and `branch_verify' (bring one topic
;;     worktree up to date with its base, see `magit-dash-update')

;; `manage_worktrees' actions: list, create, delete, move, review.
;; Worktrees follow the `.wt' convention: `<repo>/.wt/<name>' (see
;; `magit-dash-pr-worktree-path-format'); PR worktrees are `.wt/pr-<n>'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'mcpkit)
(require 'magit-dash-gh)
(require 'magit-dash-pr)
(require 'magit-dash-pr-status)
(require 'magit-dash-gh-ci)
(require 'hitl-plan-mcp nil t)
(require 'magit-dash-worktree)
(require 'magit-dash-chain)
(require 'magit-dash-stack)
(require 'magit-dash-update)

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
           ((fboundp 'magit-dash-pr-dashboard--find-local-path)
            (or (magit-dash-pr-dashboard--find-local-path trimmed)
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

(defun magit-dash-mcp--same-dir-p (a b)
  "Return non-nil when directories A and B are the same, ignoring slashes."
  (string= (directory-file-name (expand-file-name a))
           (directory-file-name (expand-file-name b))))

(defun magit-dash-mcp--resolve-worktree-path (repo-dir path)
  "Resolve PATH to a worktree directory under REPO-DIR.
Absolute (or ~) paths are expanded as is; short names such as \"foo\"
or \"pr-42\" map to the configured layout (`<root>/.wt/<name>')."
  (if (or (file-name-absolute-p path) (string-prefix-p "~" path))
      (expand-file-name path)
    (magit-dash-pr--compute-worktree-path repo-dir path)))

(defun magit-dash-mcp--worktree-main-p (repo-dir path)
  "Return non-nil if PATH is the main checkout of REPO-DIR."
  (let ((norm (lambda (p) (directory-file-name (expand-file-name p))))
        (main (alist-get 'path (car (magit-dash-gh--git-worktrees repo-dir)))))
    (or (equal (funcall norm path) (funcall norm repo-dir))
        (and main (equal (funcall norm path) (funcall norm main))))))

(mcpkit-register-tool "manage_worktrees" 'magit-dash
  :description "List, create, delete, move, or review git worktrees for a repository.
Worktrees live under `<repo>/.wt/<name>' by convention (PR worktrees are
`.wt/pr-<n>').  create: new branch (required `branch'; `worktree_path'
defaults to .wt/<branch without prefix/>, `base_ref' to the default
branch).  delete: `worktree_path' may be a short name like \"pr-42\".
move: `worktree_path' (existing) to `new_path', `new_name' (.wt/<name>)
or `pr_number' (.wt/pr-<n>)."
  :input-schema
  '(:type "object"
    :properties (:repo (:type "string"
                        :description "Local repository path, owner/repo slug, or registered repo name.")
                 :action (:type "string"
                          :enum ["list" "create" "delete" "move" "review"]
                          :description "Worktree action: list (default), create, delete, move, or review.")
                 :branch (:type "string"
                          :description "Branch name (required for action=create).")
                 :worktree_path (:type "string"
                                 :description "Worktree path or short name under .wt (optional for create; required for delete and move).")
                 :new_path (:type "string"
                            :description "Destination path for action=move.")
                 :new_name (:type "string"
                            :description "Destination short name for action=move (becomes <root>/.wt/<name>).")
                 :pr_number (:type "integer"
                             :description "For action=move, move to <root>/.wt/pr-<n>.")
                 :base_ref (:type "string"
                            :description "Base ref when creating a new branch worktree (optional).")
                 :with_prs (:type "boolean"
                            :description "For action=list: look up PR number/state through GitHub (default true).  False skips the network round trip and leaves the PR fields null."))
    :required ["repo"])
  (let* ((repo (plist-get args :repo))
         (action (or (plist-get args :action) "list"))
         (repo-dir (magit-dash-mcp--resolve-repo-dir repo))
         (repo-slug (or (magit-dash-mcp--resolve-repo-slug repo repo-dir) repo)))
    (pcase action
      ("list"
       (let* ((all-wts (magit-dash-gh--git-worktrees repo-dir))
              (prs (and (or (not (plist-member args :with_prs))
                            (not (memq (plist-get args :with_prs) '(nil :false :json-false))))
                        (ignore-errors (magit-dash-pr--fetch-repo-prs repo-slug "all" repo-dir))))
              (dirty-alist (magit-dash-worktree-dirty-many
                            (delq nil
                                  (mapcar (lambda (wt)
                                            (let ((p (alist-get 'path wt)))
                                              (and p (not (magit-dash-mcp--same-dir-p p repo-dir)) p)))
                                          all-wts))))
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
                         (main-p (magit-dash-mcp--same-dir-p wt-path repo-dir))
                         (dirty (and (not main-p) (cdr (assoc wt-path dirty-alist))))
                         (matched-pr (and prs (magit-dash-pr--match-worktree-to-pr wt prs)))
                         (pr-num (and matched-pr (map-elt matched-pr 'number)))
                         (pr-state (and matched-pr (map-elt matched-pr 'state)))
                         (pr-merged (or (and matched-pr
                                             (magit-dash-pr--pr-merged-p matched-pr repo-slug pr-num repo-dir))
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
                           (magit-dash-pr--compute-worktree-path
                            repo-dir (magit-dash-worktree-short-name branch))))
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
         (setq wt-path (magit-dash-mcp--resolve-worktree-path repo-dir wt-path))
         (condition-case err
             (let ((magit-dash-worktree-unattended t))
               (magit-dash-pr-delete-worktree wt-path repo-dir)
               (list :repo_dir repo-dir
                     :action "delete"
                     :worktree_path wt-path
                     :deleted t))
           (user-error
            (list :repo_dir repo-dir
                  :action "delete"
                  :worktree_path wt-path
                  :deleted :false
                  :refused t
                  :reason (error-message-string err))))))
      ("move"
       (let* ((old (plist-get args :worktree_path))
              (_ (unless (and (stringp old) (not (string-empty-p (string-trim old))))
                   (error "Argument `worktree_path' is required when action is \"move\"")))
              (old (magit-dash-mcp--resolve-worktree-path repo-dir old))
              (new-path (plist-get args :new_path))
              (new-name (plist-get args :new_name))
              (pr-num (plist-get args :pr_number))
              (new (cond
                    ((and (stringp new-path) (not (string-empty-p new-path)))
                     (magit-dash-mcp--resolve-worktree-path repo-dir new-path))
                    ((and (stringp new-name) (not (string-empty-p new-name)))
                     (magit-dash-pr--compute-worktree-path repo-dir new-name))
                    (pr-num
                     (magit-dash-pr--compute-worktree-path
                      repo-dir (format "pr-%d" pr-num)))
                    (t (error "One of `new_path', `new_name' or `pr_number' is required when action is \"move\"")))))
         (when (magit-dash-mcp--worktree-main-p repo-dir old)
           (user-error "Refusing to move the main worktree: %s" old))
         (unless (file-directory-p old)
           (user-error "Worktree does not exist: %s" old))
         (when (file-exists-p new)
           (user-error "Destination worktree path already exists: %s" new))
         (let ((moved (magit-dash-pr--move-worktree repo-dir old new)))
           (unless moved
             (user-error "Failed to move worktree %s to %s" old new))
           (list :repo_dir repo-dir
                 :action "move"
                 :old_path old
                 :worktree_path moved))))
      ("review"
       (let ((res (magit-dash-pr-review-worktrees repo-dir nil)))
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
                   (existing-wt (or (magit-dash-pr--find-existing-worktree repo-dir num)
                                    (magit-dash-pr-dashboard--worktree-path entry))))
              (if (and existing-wt (file-directory-p existing-wt))
                  (push (list :pr_number num :worktree_path existing-wt :status "existing") created-paths)
                (let ((wt-path (condition-case nil
                                   (magit-dash-pr--create-worktree-for-entry entry nil)
                                 (error nil))))
                  (if wt-path
                      (push (list :pr_number num :worktree_path wt-path :status "created") created-paths)
                    (cl-incf failed))))))
          (setq created-paths (nreverse created-paths))
          (list :repo_dir repo-dir
                :repo repo-slug
                :created (seq-count (lambda (x) (equal (plist-get x :status) "created")) created-paths)
                :existing (seq-count (lambda (x) (equal (plist-get x :status) "existing")) created-paths)
                :failed failed
                :worktree_path (if (= (length created-paths) 1)
                                   (plist-get (car created-paths) :worktree_path)
                                 :null)
                :worktrees (vconcat created-paths)))
      ;; Create worktrees for all open PRs in repo-dir
      (magit-dash-pr-review-worktrees repo-dir nil)
      (let* ((open-prs (magit-dash-pr--fetch-repo-prs repo-slug "open" repo-dir))
             (wts (magit-dash-gh--git-worktrees repo-dir))
             (created 0)
             (existing 0)
             (failed 0)
             (worktree-results nil))
        (dolist (pr open-prs)
          (let* ((num (map-elt pr 'number))
                 (head (map-elt pr 'headRefName))
                 (expected-path (magit-dash-pr--compute-worktree-path repo-dir (format "pr-%d" num)))
                 (matched-wt (or (magit-dash-pr--find-existing-worktree repo-dir num head)
                                 (seq-find
                                  (lambda (wt)
                                    (let ((wt-path (alist-get 'path wt)))
                                      (and wt-path
                                           (not (string= (directory-file-name (expand-file-name wt-path))
                                                         (directory-file-name (expand-file-name repo-dir))))
                                           (or (string= (directory-file-name (expand-file-name wt-path))
                                                        (directory-file-name (expand-file-name expected-path)))
                                               (equal (magit-dash-pr--match-worktree-to-pr wt (list pr)) pr)))))
                                  wts))))
            (if matched-wt
                (let ((wt-path (if (consp matched-wt) (alist-get 'path matched-wt) matched-wt)))
                  (cl-incf existing)
                  (push (list :pr_number num
                              :worktree_path wt-path
                              :status "existing")
                        worktree-results))
              (let* ((entry (list :number num :repo repo-slug :pr pr))
                     (wt-path (condition-case nil
                                  (magit-dash-pr--create-worktree-for-entry entry nil)
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
  :description "Clean up local git worktrees whose associated pull requests have been merged or closed."
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
                   (res (magit-dash-pr--clean-worktree-for-entry entry nil)))
              (cond
               ((eq res :deleted) (cl-incf deleted))
               ((eq res :skipped) (cl-incf skipped))
               ((listp res)
                (setq deleted (+ deleted (or (plist-get res :deleted) 0))
                      skipped (+ skipped (or (plist-get res :skipped) 0)))))))
          (list :repo_dir repo-dir
                :deleted deleted
                :skipped skipped))
      (let ((res (magit-dash-pr-clean-merged-worktrees repo-dir nil)))
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
                   (magit-dash-pr-clean-merged-worktrees repo-dir nil)))
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
          :skipped_details (vconcat (and (not dry-run) (plist-get del-res :details)))
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
                  (magit-dash-pr-status-collect-all)
                (magit-dash-pr-status-collect repo-slug repo-dir)))
         (tracked (mapcar #'magit-dash-pr-attach-tracking prs)))
    (when (eq (plist-get args :commit) t)
      (magit-dash-pr-commit-tracking prs))
    (list :repo (or repo-slug :null)
          :count (length tracked)
          :fingerprint (magit-dash-pr-tracking-fingerprint prs)
          :prs (vconcat (mapcar #'magit-dash-mcp--pr-json tracked)))))

(defun magit-dash-mcp--pr-context (repo number)
  "Validate REPO and PR NUMBER; return (REPO-DIR . SLUG).
REPO-DIR is nil when REPO is only an owner/name slug with no local checkout."
  (unless (and repo (stringp repo) (not (string-empty-p (string-trim repo))))
    (error "Repository argument `repo' is required"))
  (unless (and (numberp number) (> number 0))
    (error "PR `number' must be a positive integer"))
  (let* ((repo-dir (ignore-errors (magit-dash-mcp--resolve-repo-dir repo)))
         (slug (or (ignore-errors (magit-dash-mcp--resolve-repo-slug repo repo-dir))
                   (if (string-match-p "\\`[^/]+/[^/]+\\'" (string-trim repo))
                       (string-trim repo)
                     (error "Cannot resolve repository slug for %s" repo)))))
    (cons repo-dir slug)))

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
           (ctx (magit-dash-mcp--pr-context repo number))
           (repo-dir (car ctx))
           (slug (cdr ctx))
           (limit (plist-get args :limit))
           (max-chars (plist-get args :max_chars))
           (threads (magit-dash-pr-fetch-threads-graphql slug number limit max-chars repo-dir)))
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
           (ctx (magit-dash-mcp--pr-context repo number))
           (repo-dir (car ctx))
           (slug (cdr ctx))
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

;;;; Tools 10-12: branch_update, branch_resolve, branch_verify

(defun magit-dash-mcp--worktree-dir (args &optional key)
  "Return the absolute worktree directory named by ARGS under KEY.
KEY defaults to :worktree.  An absolute (or ~) path is used as is; a short
name such as \"pr-42\" needs `repo' and maps to `<root>/.wt/<name>'."
  (let ((wt (plist-get args (or key :worktree)))
        (repo (plist-get args :repo)))
    (unless (and (stringp wt) (not (string-empty-p (string-trim wt))))
      (error "Argument `%s' is required" (substring (symbol-name (or key :worktree)) 1)))
    (let ((dir (if (or (file-name-absolute-p wt) (string-prefix-p "~" wt))
                   (expand-file-name wt)
                 (progn
                   (unless (and (stringp repo) (not (string-empty-p (string-trim repo))))
                     (error "A short worktree name needs `repo'"))
                   (magit-dash-mcp--resolve-worktree-path
                    (magit-dash-mcp--resolve-repo-dir repo) wt)))))
      (unless (file-directory-p dir)
        (error "Worktree does not exist: %s" dir))
      dir)))

(defun magit-dash-mcp--update-json (res)
  "Return the `branch_update' result plist RES as JSON-ready data."
  (if (plist-get res :error)
      res
    (let ((out nil))
      (cl-loop for (k v) on res by #'cddr
               do (setq out
                        (append out
                                (list k
                                      (cond
                                       ((eq k :conflicts)
                                        (vconcat
                                         (mapcar
                                          (lambda (c)
                                            (let ((cs (plist-get c :conflicts)))
                                              (append (list :file (plist-get c :file)
                                                            :kind (plist-get c :kind)
                                                            :hunks (plist-get c :hunks))
                                                      (and cs (list :conflicts (vconcat cs))))))
                                          v)))
                                       ((memq k '(:outcome)) (magit-dash-mcp--json-value v))
                                       ((memq k '(:rerere :merging :dirty))
                                        (if v t :false))
                                       (t (or v :null)))))))
      out)))

(mcpkit-register-tool "branch_update" 'magit-dash
  :description "Merge origin/<base> into a topic worktree in one call (never rebases, never pushes): fetches only the base branch, merges with --no-ff, and returns outcome merged | up-to-date | resumed | conflict. On conflict the worktree is left in its merge state and the conflicted files come back with kind, hunk count and (include_hunks) the ours/theirs text, ready for branch_resolve. Calling again after resolving finishes the merge. merge=false only reports ahead/behind. abort=true aborts the merge in progress. Pass `worktrees' to update several worktrees with one fetch per repository. Refuses detached HEAD, main/master and tracked changes. Push by hand only when the user asks."
  :input-schema
  '(:type "object"
    :properties (:worktree (:type "string"
                            :description "Worktree path, or short name such as pr-42 (then `repo' is required).")
                 :worktrees (:type "array" :items (:type "string")
                             :description "Several worktrees (paths or short names) instead of `worktree'.")
                 :repo (:type "string"
                        :description "Repository path, slug or name; only needed for short worktree names.")
                 :base (:type "string"
                        :description "Base branch to merge (default: the repository's default branch).")
                 :fetch (:type "boolean"
                         :description "Fetch the base branch from origin first (default true).")
                 :merge (:type "boolean"
                         :description "Perform the merge (default true); false only reports ahead/behind.")
                 :abort (:type "boolean"
                         :description "Abort the merge in progress instead of updating.")
                 :include_hunks (:type "boolean"
                                 :description "On conflict, include each hunk's ours/theirs text (default true).")
                 :max_chars (:type "integer"
                             :description "Total hunk text returned (default 6000); later hunks are marked truncated."))
    :required [])
  (let* ((many (plist-get args :worktrees))
         (dirs (if many
                   (mapcar (lambda (w) (magit-dash-mcp--worktree-dir (list :worktree w :repo (plist-get args :repo))))
                           (seq-into many 'list))
                 (list (magit-dash-mcp--worktree-dir args))))
         (flag (lambda (key default)
                 (if (plist-member args key)
                     (not (memq (plist-get args key) '(nil :false :json-false)))
                   default)))
         (base (let ((b (plist-get args :base))) (and (stringp b) (not (string-empty-p b)) b)))
         (opts (list :base base
                     :fetch (funcall flag :fetch t)
                     :merge (funcall flag :merge t)
                     :abort (funcall flag :abort nil)
                     :include-hunks (funcall flag :include_hunks t)
                     :max-chars (plist-get args :max_chars))))
    (if many
        (list :worktrees
              (vconcat (mapcar #'magit-dash-mcp--update-json
                               (apply #'magit-dash-update-branches dirs opts))))
      (let ((res (apply #'magit-dash-update-branch (car dirs) opts)))
        (magit-dash-mcp--update-json (append (list :worktree (car dirs)) res))))))

(mcpkit-register-tool "branch_resolve" 'magit-dash
  :description "Resolve conflict hunks of a worktree mid-merge in one call, for many files at once. Each file takes ours, theirs, both, both_theirs_first, base (diff3) or text (with `text'), for every hunk or per hunk by index (from branch_update). Only files git lists as unmerged are touched; edits are byte-exact (CRLF safe). Fully resolved files are staged in a single git add unless stage=false. Hunks you do not choose stay in place. Finish with branch_verify (commit=true)."
  :input-schema
  '(:type "object"
    :properties (:worktree (:type "string"
                            :description "Worktree path, or short name (then `repo' is required).")
                 :repo (:type "string" :description "Repository, for short worktree names.")
                 :files (:type "array"
                         :items (:type "object"
                                 :properties (:file (:type "string" :description "Conflicted path relative to the worktree.")
                                              :take (:type "string"
                                                     :enum ["ours" "theirs" "both" "both_theirs_first" "base" "text"]
                                                     :description "Choice for every hunk of the file.")
                                              :text (:type "string" :description "Replacement text when take is text.")
                                              :hunks (:type "array"
                                                      :items (:type "object"
                                                              :properties (:index (:type "integer")
                                                                           :take (:type "string"
                                                                                  :enum ["ours" "theirs" "both" "both_theirs_first" "base" "text"])
                                                                           :text (:type "string"))
                                                              :required ["index"])
                                                      :description "Per-hunk choices overriding `take'."))
                                 :required ["file"]))
                 :stage (:type "boolean" :description "Stage fully resolved files (default true)."))
    :required ["worktree" "files"])
  (let* ((dir (magit-dash-mcp--worktree-dir args))
         (specs (mapcar
                 (lambda (f)
                   (list :file (plist-get f :file)
                         :take (plist-get f :take)
                         :text (plist-get f :text)
                         :hunks (mapcar (lambda (h) (list :index (plist-get h :index)
                                                          :take (plist-get h :take)
                                                          :text (plist-get h :text)))
                                        (seq-into (plist-get f :hunks) 'list))))
                 (seq-into (plist-get args :files) 'list)))
         (stage (if (plist-member args :stage)
                    (not (memq (plist-get args :stage) '(nil :false :json-false)))
                  t))
         (res (magit-dash-update-resolve dir specs stage)))
    (list :worktree dir
          :files (vconcat (mapcar (lambda (r)
                                    (if (plist-get r :error)
                                        r
                                      (plist-put (copy-sequence r) :staged
                                                 (if (plist-get r :staged) t :false))))
                                  res)))))

(mcpkit-register-tool "branch_verify" 'magit-dash
  :description "Verify a worktree after a merge and optionally commit, in one call: fails on leftover conflict markers, then for Go modules vets only the packages the base changed (plus gofmt -l on changed files; tests=true also runs go test), then runs magit-dash-chain-verify-command when configured. With commit=true and no failures it stages tracked changes and commits: the merge in progress with git's own message, or tracked changes with `message'. Returns ok, failures, markers, needs_review (unmerged paths without markers, e.g. modify/delete: confirm before trusting the commit), packages and the new sha. Never pushes."
  :input-schema
  '(:type "object"
    :properties (:worktree (:type "string"
                            :description "Worktree path, or short name (then `repo' is required).")
                 :repo (:type "string" :description "Repository, for short worktree names.")
                 :tests (:type "boolean" :description "Also run go test on the touched packages (never -v).")
                 :commit (:type "boolean" :description "Commit when verification passes.")
                 :message (:type "string" :description "Commit message when there is no merge in progress.")
                 :base (:type "string" :description "Base branch, only for the fallback diff range."))
    :required ["worktree"])
  (let* ((dir (magit-dash-mcp--worktree-dir args))
         (flag (lambda (key) (eq (plist-get args key) t)))
         (res (magit-dash-update-verify
               dir :tests (funcall flag :tests) :commit (funcall flag :commit)
               :message (plist-get args :message) :base (plist-get args :base))))
    (list :worktree dir
          :ok (if (plist-get res :ok) t :false)
          :failures (vconcat (plist-get res :failures))
          :markers (vconcat (plist-get res :markers))
          :needs_review (vconcat (plist-get res :needs_review))
          :packages (vconcat (plist-get res :packages))
          :committed (if (plist-get res :committed) t :false)
          :sha (or (plist-get res :sha) :null))))

(provide 'magit-dash-mcp)

;;; magit-dash-mcp.el ends here
