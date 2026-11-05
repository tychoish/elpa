;;; magit-dash-mcp.el --- MCP tool integration for magit-dash -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1"))
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

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'mcpkit)
(require 'magit-dash-gh)
(require 'magit-dash-gh-pr)
(require 'magit-dash-worktree)

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

(provide 'magit-dash-mcp)

;;; magit-dash-mcp.el ends here
