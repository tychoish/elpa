;;; magit-dash-worktree.el --- Git worktree symlink and management extensions for magit-dash -*- lexical-binding: t -*-

;; Author: sam kleinman (tychoish)
;; Keywords: git, vc, tools, magit
;; URL: https://github.com/tychoish/magit-dash

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides automated worktree symlink enforcement and worktree management for `magit-dash'.
;;
;; Features:
;; 1. Global symlink targets shared across all worktrees (`magit-dash-worktree-global-symlinks').
;; 2. Repository-to-symlinks mapping alist (`magit-dash-worktree-repo-symlinks').
;; 3. Per-repository `:worktree-symlinks' field in `magit-dash-register'.
;;
;; When a new worktree is created or checked out, symlinks pointing from the main
;; repository checkout to the linked worktree are automatically established.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-git)
(require 'magit-process)
(require 'magit-worktree)
(require 'magit-dash-gh)
(require 'agent-shell-queue nil t)

(declare-function asq-worktree-create "agent-shell-queue")
(declare-function asq-worktree-dispatch-task "agent-shell-queue")
(declare-function asq-worktree-generate-name "agent-shell-queue")
(declare-function asq-worktree-git-repo-root "agent-shell-queue")
(declare-function annotated-completing-read "annotated-completing-read")
(declare-function magit-dash-gh-pr-dashboard-open-worktree "magit-dash-gh-pr")
(declare-function magit-dash-gh-pr-dashboard--current-repo-slug "magit-dash-gh-pr")
(declare-function magit-dash-gh-pr-create-all-worktrees "magit-dash-gh-pr")
(declare-function magit-dash-gh-pr-review-worktrees "magit-dash-gh-pr")
(declare-function magit-dash-gh-pr-clean-merged-worktrees "magit-dash-gh-pr")


;; Forward declarations for magit-dash core functions and variables
(declare-function magit-dash-repo-name "magit-dash")
(declare-function magit-dash-repo-path "magit-dash")
(declare-function magit-dash-repo-worktree "magit-dash")
(declare-function magit-dash-repo-worktree-symlinks "magit-dash")
(declare-function magit-dash--repo-at-point "magit-dash")
(declare-function magit-dash--resolve-repo-path "magit-dash")
(declare-function magit-dash-refresh "magit-dash")
(defvar magit-dash-repo-list)

;;; Customization

(defgroup magit-dash-worktree nil
  "Worktree symlink and management extensions for `magit-dash'."
  :group 'magit-dash
  :prefix "magit-dash-worktree-")

(defcustom magit-dash-worktree-global-symlinks '("node_modules" ".venv" ".env")
  "List of relative paths to symlink from the main worktree into linked worktrees.
These paths apply globally to all repositories unless overridden or extended."
  :type '(repeat string)
  :group 'magit-dash-worktree)

(defcustom magit-dash-worktree-repo-symlinks nil
  "Alist mapping repository names or paths to lists of relative paths to symlink.
Example:
  \\='((\"my-project\" . (\"dist\" \".env.local\"))
    (\"backend\"    . (\".venv\" \"target\")))"
  :type '(alist :key-type string :value-type (repeat string))
  :group 'magit-dash-worktree)

(defcustom magit-dash-worktree-auto-symlink t
  "When non-nil, automatically create symlinks in new worktrees.
Hooked via `magit-post-worktree-add-hook'."
  :type 'boolean
  :group 'magit-dash-worktree)

;;; Path resolution & helpers

(defun magit-dash-worktree-main-dir (&optional path)
  "Return the main repository checkout root directory for PATH.
If PATH is in a linked worktree, follows git's common directory back to
the main checkout.  If PATH is nil, uses `default-directory'."
  (let* ((dir (expand-file-name (or path default-directory)))
         (default-directory (file-name-as-directory dir))
         (common-dir (condition-case nil
                         (magit-git-string "rev-parse" "--path-format=absolute" "--git-common-dir")
                       (error nil))))
    (if (and common-dir (not (string-empty-p common-dir)))
        (let ((common-abs (expand-file-name common-dir)))
          (if (string-suffix-p "/.git" common-abs)
              (file-name-directory (directory-file-name common-abs))
            (file-name-directory (directory-file-name (or (magit-toplevel) dir)))))
      (or (magit-toplevel dir) dir))))

(defun magit-dash-worktree-linked-p (&optional path)
  "Return non-nil if PATH (default `default-directory') is a linked worktree.
Returns nil if PATH is the primary repository checkout or not a git repository."
  (let* ((dir (expand-file-name (or path default-directory)))
         (default-directory (file-name-as-directory dir))
         (top (magit-toplevel dir))
         (main (magit-dash-worktree-main-dir dir)))
    (and top main
         (not (equal (file-name-as-directory (expand-file-name top))
                     (file-name-as-directory (expand-file-name main)))))))

(defun magit-dash-worktree-symlinks-for (repo-or-path)
  "Return the combined, deduplicated list of symlink targets for REPO-OR-PATH.
Combines `magit-dash-worktree-global-symlinks', matching entries from
`magit-dash-worktree-repo-symlinks', and per-repo `:worktree-symlinks' settings."
  (let* ((path (cond
                ((stringp repo-or-path) (expand-file-name repo-or-path))
                ((and repo-or-path (fboundp 'magit-dash-repo-path))
                 (expand-file-name (magit-dash-repo-path repo-or-path)))
                (t default-directory)))
         (repo-name (cond
                     ((and repo-or-path (fboundp 'magit-dash-repo-name) (not (stringp repo-or-path)))
                      (magit-dash-repo-name repo-or-path))
                     ((stringp repo-or-path)
                      (if (bound-and-true-p magit-dash-repo-list)
                          (when-let* ((r (seq-find (lambda (r)
                                                     (or (equal (magit-dash-repo-name r) repo-or-path)
                                                         (equal (expand-file-name (magit-dash-repo-path r)) path)))
                                                   magit-dash-repo-list)))
                            (magit-dash-repo-name r))
                        (file-name-nondirectory (directory-file-name path))))
                     (t nil)))
         (repo-struct (if (and repo-or-path (not (stringp repo-or-path)))
                          repo-or-path
                        (when (bound-and-true-p magit-dash-repo-list)
                          (seq-find (lambda (r)
                                      (or (and repo-name (equal (magit-dash-repo-name r) repo-name))
                                          (equal (expand-file-name (magit-dash-repo-path r)) path)))
                                    magit-dash-repo-list))))
         (struct-links (when (and repo-struct (fboundp 'magit-dash-repo-worktree-symlinks))
                         (magit-dash-repo-worktree-symlinks repo-struct)))
         (map-links (let (found)
                      (dolist (entry magit-dash-worktree-repo-symlinks)
                        (let ((key (car entry))
                              (val (cdr entry)))
                          (when (or (and repo-name (equal key repo-name))
                                    (equal (expand-file-name key) path)
                                    (equal key (file-name-nondirectory (directory-file-name path))))
                            (setq found (append found (if (listp val) val (list val)))))))
                      found))
         (all (append magit-dash-worktree-global-symlinks map-links struct-links)))
    (delete-dups (seq-filter (lambda (s) (and (stringp s) (not (string-empty-p s)))) all))))

;;; Symlink execution

(defun magit-dash-worktree-symlink-worktree (worktree-path &optional main-path symlinks)
  "Create symlinks in WORKTREE-PATH pointing to entries in MAIN-PATH.
If MAIN-PATH is nil, resolves it via `magit-dash-worktree-main-dir'.
If SYMLINKS is nil, resolves via `magit-dash-worktree-symlinks-for'.
Returns an alist of ((TARGET . SOURCE) ...) for created or active symlinks."
  (let* ((wt (expand-file-name worktree-path))
         (main (expand-file-name (or main-path (magit-dash-worktree-main-dir wt))))
         (targets (or symlinks (magit-dash-worktree-symlinks-for (or main wt))))
         (created '()))
    (unless (equal (file-name-as-directory wt) (file-name-as-directory main))
      (dolist (rel targets)
        (let ((src (expand-file-name rel main))
              (dst (expand-file-name rel wt)))
          ;; Only link if the source file or directory exists in the main worktree
          (when (file-exists-p src)
            (let ((parent-dir (file-name-directory (directory-file-name dst))))
              (unless (file-directory-p parent-dir)
                (make-directory parent-dir t)))
            (cond
             ;; If destination is already a symlink pointing to source, record it
             ((and (file-symlink-p dst)
                   (equal (expand-file-name (file-symlink-p dst) (file-name-directory dst))
                          src))
              (push (cons rel src) created))
             ;; If destination does not exist (or is a stale symlink), create symlink
             ((or (not (file-exists-p dst)) (file-symlink-p dst))
              (when (file-symlink-p dst)
                (delete-file dst))
              (condition-case nil
                  (progn
                    (make-symbolic-link src dst t)
                    (push (cons rel src) created))
                (error nil))))))))
    (nreverse created)))

;;;###autoload
(defun magit-dash-worktree-sync-symlinks (&optional path)
  "Enforce worktree symlinks for the worktree at PATH.
Defaults to the repo at point or current buffer.  If invoked on a main
worktree, syncs symlinks for all linked worktrees of that repository."
  (interactive)
  (let* ((target-path (expand-file-name
                       (or path
                           (when (fboundp 'magit-dash--repo-at-point)
                             (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
                               (magit-dash-repo-path repo)))
                           default-directory)))
         (is-linked (magit-dash-worktree-linked-p target-path))
         (main-dir (magit-dash-worktree-main-dir target-path)))
    (if is-linked
        (let ((created (magit-dash-worktree-symlink-worktree target-path main-dir)))
          (message "magit-dash-worktree: created %d symlink(s) in %s"
                   (length created) (file-name-nondirectory (directory-file-name target-path)))
          created)
      ;; On main worktree: find all worktrees and sync each
      (let* ((default-directory (file-name-as-directory main-dir))
             (wt-lines (condition-case nil
                           (magit-git-lines "worktree" "list" "--porcelain")
                         (error nil)))
             (count 0))
        (when wt-lines
          (dolist (line wt-lines)
            (when (string-prefix-p "worktree " line)
              (let ((wt-dir (substring line 9)))
                (unless (equal (file-name-as-directory (expand-file-name wt-dir))
                               (file-name-as-directory main-dir))
                  (let ((created (magit-dash-worktree-symlink-worktree wt-dir main-dir)))
                    (setq count (+ count (length created)))))))))
        (message "magit-dash-worktree: synced %d symlink(s) across worktrees of %s"
                 count (file-name-nondirectory (directory-file-name main-dir)))))))

;;; Hook integration

(defun magit-dash-worktree-post-add-hook-function (path &optional _branch)
  "Hook function for `magit-post-worktree-add-hook' to enforce worktree symlinks.
Creates configured symlinks in PATH from the main repository checkout."
  (when magit-dash-worktree-auto-symlink
    (condition-case err
        (let ((created (magit-dash-worktree-symlink-worktree path)))
          (when created
            (message "magit-dash-worktree: linked %d target(s) into %s"
                     (length created) (file-name-nondirectory (directory-file-name path)))))
      (error
       (message "magit-dash-worktree: error creating symlinks in %s: %s"
                path (error-message-string err))))))

;; Automatically register on `magit-post-worktree-add-hook'

;;; Interactive worktree commands for dashboard & overview

;;;###autoload
(defun magit-dash-worktree-add ()
  "Add a new worktree for the repository at point via magit."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (when (magit-dash-repo-worktree repo)
      (user-error "Cannot add a worktree from a worktree entry"))
    (magit-dash-gh--with-repo-dir (magit-dash-repo-path repo)
      (call-interactively #'magit-worktree-checkout))
    (magit-dash-refresh)))

;;;###autoload
(defun magit-dash-worktree-uncommitted-changes-p (path)
  "Return non-nil if git worktree at PATH has uncommitted changes.
Checks for modified tracked files, staged changes, or untracked files
via `git status --porcelain'."
  (when (and path (file-directory-p path))
    (with-temp-buffer
      (let ((exit (call-process "git" nil (current-buffer) nil
                                "-C" (expand-file-name path)
                                "status" "--porcelain")))
        (if (zerop exit)
            (not (string-empty-p (string-trim (buffer-string))))
          t)))))

;;;###autoload
(defun magit-dash-worktree-delete ()
  "Delete the worktree at point via magit.
Signals `user-error' when the current row is not a worktree,
or when the worktree contains uncommitted changes."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (unless (magit-dash-repo-worktree repo)
      (user-error "Not a worktree row; use 'k' only on worktree entries"))
    (let ((path (magit-dash-repo-path repo)))
      (when (magit-dash-worktree-uncommitted-changes-p path)
        (user-error "Cannot delete worktree %s: contains uncommitted changes" path)))
    (magit-dash-gh--with-repo-dir (magit-dash-repo-path repo)
      (call-interactively #'magit-worktree-delete))
    (magit-dash-refresh)))

;;;###autoload
(defun magit-dash-worktree-dispatch (&optional dir)
  "Create an agent worktree under DIR and dispatch a task.
Prompts for task prompt, worktree short name, and target dispatch mode.
Establishes configured symlinks for the new worktree."
  (interactive)
  (let* ((repo-root (or (and (fboundp 'asq-worktree-git-repo-root)
                             (asq-worktree-git-repo-root dir))
                        (magit-dash-worktree-main-dir dir)
                        (user-error "Not in a git repository")))
         (prompt (read-string "Task prompt (empty for worktree only): "))
         (default-name (if (fboundp 'asq-worktree-generate-name)
                          (asq-worktree-generate-name prompt)
                        "worktree"))
         (name (read-string (format "Worktree short name (default %s): " default-name)
                            nil nil default-name))
         (name (if (string-empty-p name) default-name name))
         (choices '(("i: interactive agent-shell" . "Start an interactive agent-shell session")
                    ("q: asq queue" . "Enqueue prompt in directory task queue")
                    ("w: worktree only" . "Create git worktree without dispatching agent task")))
         (mode-choice (if (fboundp 'annotated-completing-read)
                          (annotated-completing-read
                           choices
                           :prompt "Dispatch target: "
                           :require-match t
                           :history 'magit-dash-worktree-dispatch-history)
                        (completing-read "Dispatch target: " '("interactive" "queue" "worktree") nil t)))
         (mode (cond
                ((string-prefix-p "i" mode-choice) "interactive")
                ((string-prefix-p "q" mode-choice) "queue")
                ((string-prefix-p "w" mode-choice) "worktree")
                (t mode-choice)))
         (wt-info (if (fboundp 'asq-worktree-create)
                      (asq-worktree-create :repo-root repo-root :name name :prompt prompt)
                    (let ((path (expand-file-name (concat ".wt/" name) repo-root)))
                      (make-directory (file-name-directory path) t)
                      (call-process "git" nil nil nil "-C" repo-root "worktree" "add" "-b" (concat "wt/" name) path "HEAD")
                      (list :repo-root repo-root :name name :branch (concat "wt/" name) :path path))))
         (wt-path (plist-get wt-info :path)))
    (when wt-path
      (magit-dash-worktree-symlink-worktree wt-path repo-root)
      (if (fboundp 'asq-worktree-dispatch-task)
          (asq-worktree-dispatch-task prompt :target-dir wt-path :mode mode :name name)
        wt-path))))


;;;###autoload
(defun magit-dash-worktree-from-pr (&optional pr-arg repo-dir)
  "Create and open a git worktree from a GitHub pull request.
The worktree directory is created using `magit-dash-gh-pr-worktree-path-format',
and the local branch mirrors the remote branch name.
Interactively, prompts for the PR number or selection in REPO-DIR (defaulting
to the current repository or repository at point)."
  (interactive)
  (require 'magit-dash-gh-pr)
  (let* ((dir (or repo-dir
                  (if (and (fboundp 'magit-dash--repo-at-point-p)
                           (magit-dash--repo-at-point-p))
                      (magit-dash-repo-path (magit-dash--repo-at-point))
                    (or (magit-toplevel) default-directory))))
         (slug (or (ignore-errors
                     (let ((default-directory dir))
                       (magit-dash-gh-pr-dashboard--current-repo-slug)))
                   (file-name-nondirectory (directory-file-name dir))))
         (pr-num
          (cond
           ((integerp pr-arg) pr-arg)
           ((and (stringp pr-arg) (string-match-p "\\`[0-9]+\\'" pr-arg))
            (string-to-number pr-arg))
           (t
            (let* ((candidates
                    (when (executable-find "gh")
                      (ignore-errors
                        (with-temp-buffer
                          (when (zerop (call-process "gh" nil (current-buffer) nil
                                                     "pr" "list" "-R" slug
                                                     "--json" "number,title,headRefName"
                                                     "--template"
                                                     "{{range .}}{{tablerow (printf \"#%v\" .number) .headRefName .title}}{{end}}"))
                            (split-string (string-trim (buffer-string)) "\n" t))))))
                   (choice (if candidates
                               (completing-read (format "PR in %s: " slug) candidates nil nil)
                             (read-string (format "PR number for %s: " slug)))))
              (if (string-match "#?\\([0-9]+\\)" choice)
                  (string-to-number (match-string 1 choice))
                (user-error "Invalid PR number: %s" choice)))))))
    (let ((entry (list :number pr-num :repo slug :pr nil)))
      (magit-dash-gh-pr-dashboard-open-worktree entry))))

;;;###autoload
(defalias 'magit-dash-worktree-checkout-pr #'magit-dash-worktree-from-pr)

;;;###autoload
(defun magit-dash-worktree-create-for-all-prs (&optional repo-dir)
  "Create worktrees for all open PRs in REPO-DIR.
Reviews existing worktrees first, prompting to rename any that do not match
the canonical PR format, and prompting to delete any for merged PRs that are clean.
Establishes configured symlinks for all created worktrees."
  (interactive)
  (require 'magit-dash-gh-pr)
  (magit-dash-gh-pr-create-all-worktrees repo-dir))

;;;###autoload
(defun magit-dash-worktree-review-prs (&optional repo-dir)
  "Review existing worktrees in REPO-DIR against GitHub pull requests.
Prompts to rename worktrees that do not match the canonical PR naming format,
and prompts to delete worktrees for merged PRs (refusing if uncommitted changes exist).
Leaves branches intact until branch pruning."
  (interactive)
  (require 'magit-dash-gh-pr)
  (magit-dash-gh-pr-review-worktrees repo-dir t))

;;;###autoload
(defun magit-dash-worktree-clean-merged-prs (&optional repo-dir)
  "Delete worktrees for merged PRs in REPO-DIR when clean.
Leaves local branches intact until branch pruning.
Refuses to delete any worktree with uncommitted changes."
  (interactive)
  (require 'magit-dash-gh-pr)
  (magit-dash-gh-pr-clean-merged-worktrees repo-dir t))

(provide 'magit-dash-worktree)
;;; magit-dash-worktree.el ends here
