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

;; Forward declarations for magit-dash core functions and variables
(declare-function magit-dash-repo-name "magit-dash")
(declare-function magit-dash-repo-path "magit-dash")
(declare-function magit-dash-repo-worktree "magit-dash")
(declare-function magit-dash-repo-worktree-symlinks "magit-dash")
(declare-function magit-dash--repo-at-point "magit-dash")
(declare-function magit-dash--resolve-repo-path "magit-dash")
(declare-function magit-dash-refresh "magit-dash")
(declare-function magit-dash-gh--with-repo-dir "magit-dash")
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
  "When non-nil, automatically create symlinks in new worktrees via `magit-post-worktree-add-hook'."
  :type 'boolean
  :group 'magit-dash-worktree)

;;; Path resolution & helpers

(defun magit-dash-worktree-main-dir (&optional path)
  "Return the main repository checkout root directory for PATH.
If PATH is in a linked worktree, follows git's common directory back to the main checkout.
If PATH is nil, uses `default-directory'."
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
  "Create symlinks in WORKTREE-PATH pointing to corresponding entries in MAIN-PATH.
If MAIN-PATH is nil, resolves it via `magit-dash-worktree-main-dir'.
If SYMLINKS is nil, resolves via `magit-dash-worktree-symlinks-for'.
Returns an alist of ((TARGET . SOURCE) ...) for successfully created or active symlinks."
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
  "Enforce worktree symlinks for the worktree at PATH (default repo at point or current buffer).
If invoked on a main worktree, syncs symlinks for all linked worktrees of that repository."
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
(add-hook 'magit-post-worktree-add-hook #'magit-dash-worktree-post-add-hook-function)

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
(defun magit-dash-worktree-delete ()
  "Delete the worktree at point via magit.
Signals `user-error' when the current row is not a worktree."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (unless (magit-dash-repo-worktree repo)
      (user-error "Not a worktree row; use 'k' only on worktree entries"))
    (magit-dash-gh--with-repo-dir (magit-dash-repo-path repo)
      (call-interactively #'magit-worktree-delete))
    (magit-dash-refresh)))

(provide 'magit-dash-worktree)
;;; magit-dash-worktree.el ends here
