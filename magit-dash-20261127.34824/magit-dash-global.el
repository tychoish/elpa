;;; magit-dash-global.el --- Lightweight global mode and settings for magit-dash -*- lexical-binding: t -*-

;; Author: sam kleinman
;; Maintainer: tychoish
;; Keywords: vc, tools, magit, github

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Provides `magit-dash-global-mode', configuring hooks, keybindings, and
;; variables for `magit-dash' without requiring the full package or its
;; dependencies at startup.  The dashboard and its subsystems load on demand
;; when invoked.

;;; Code:

(defvar savehist-additional-variables)
(defvar magit-dash-columns)
(defvar magit-dash-pr-columns)
(defvar magit-dash-pr-saved-filters)
(defvar which-key-mode)
(defvar which-key-replacement-alist)
(defvar magit-dash-which-key-abbreviate-commands 'strip
  "How to abbreviate magit-dash commands in which-key.
One of `strip', `md', or nil.")
(defvar magit-dash-buffer-name "*magit-dash-repos*")
(defvar magit-dash--inhibit-post-refresh-hook nil
  "Non-nil when `magit-dash--on-magit-post-refresh' is executing.")

(require 'seq)

(declare-function magit-toplevel "magit-git")
(declare-function magit-dash--all-repos "magit-dash")
(declare-function magit-dash-repo-path "magit-dash")
(declare-function magit-dash-refresh-repo "magit-dash")
(declare-function magit-dash-worktree-post-add-hook-function "magit-dash-worktree")
(declare-function magit-dash-open-other-window "magit-dash")
(declare-function magit-dash-dispatch "magit-dash-status")
(declare-function magit-dash-pr-dashboard--setup-code-review-keys "magit-dash-pr")

(defconst magit-dash--which-key-modes
  '(magit-dash-mode magit-dash-actionable-mode magit-dash-pr-dashboard-mode
    magit-dash-stack-mode magit-dash-gh-actions-log-mode)
  "Major modes whose which-key descriptions abbreviate package namespaces.")

(defun magit-dash--which-key-register-replacements ()
  "Register namespace abbreviations for every magit-dash major mode.
Trim PR dashboard and PR namespaces before the general package prefix.
Named prefix maps retain their which-key group marker.  Do nothing when
`magit-dash-which-key-abbreviate-commands' is nil or which-key is unloaded."
  (when (and magit-dash-which-key-abbreviate-commands
             (boundp 'which-key-replacement-alist))
    (let* ((prefix-repl
            (if (memq magit-dash-which-key-abbreviate-commands '(strip t))
                "\\1"
              "md-\\1"))
           (rules
            (seq-mapcat
             (lambda (prefix)
               (list
                (cons (cons nil (concat "\\`group:" prefix "\\(.*\\)"))
                      (cons nil (concat "group:" prefix-repl)))
                (cons (cons nil (concat "\\`" prefix "\\(.*\\)"))
                      (cons nil prefix-repl))))
             '("magit-dash-pr-dashboard-" "magit-dash-pr-" "magit-dash-"))))
      (seq-do
       (lambda (mode)
         (setf (alist-get mode which-key-replacement-alist) rules))
       magit-dash--which-key-modes))))

(defun magit-dash--on-magit-post-refresh ()
  "Refresh dashboard row for the current repository after a Magit operation."
  (unless magit-dash--inhibit-post-refresh-hook
    (let ((magit-dash--inhibit-post-refresh-hook t))
      (when-let* ((buf (get-buffer magit-dash-buffer-name))
                  ((buffer-live-p buf))
                  (top (or (and (fboundp 'magit-toplevel) (magit-toplevel))
                           default-directory))
                  (top-dir (file-name-as-directory (expand-file-name top)))
                  (repo (and (fboundp 'magit-dash--all-repos)
                             (seq-find (lambda (r)
                                         (let ((r-dir (file-name-as-directory
                                                       (expand-file-name (magit-dash-repo-path r)))))
                                           (or (string-equal r-dir top-dir)
                                               (ignore-errors (file-equal-p r-dir top-dir)))))
                                       (magit-dash--all-repos)))))
        (with-current-buffer buf
          (when (fboundp 'magit-dash-refresh-repo)
            (magit-dash-refresh-repo repo nil t)))))))

(defun magit-dash-global--setup-magit ()
  "Configure Magit keybindings and hooks for `magit-dash'."
  (when (boundp 'magit-mode-map)
    (keymap-set magit-mode-map "C-c C-d" #'magit-dash-open-other-window)
    (keymap-set magit-mode-map "." #'magit-dash-dispatch))
  (add-hook 'magit-post-refresh-hook #'magit-dash--on-magit-post-refresh)
  (add-hook 'magit-post-worktree-add-hook #'magit-dash-worktree-post-add-hook-function))

(defun magit-dash-global--teardown-magit ()
  "Remove Magit keybindings and hooks for `magit-dash'."
  (when (boundp 'magit-mode-map)
    (keymap-unset magit-mode-map "C-c C-d" t)
    (keymap-unset magit-mode-map "." t))
  (remove-hook 'magit-post-refresh-hook #'magit-dash--on-magit-post-refresh)
  (remove-hook 'magit-post-worktree-add-hook #'magit-dash-worktree-post-add-hook-function))

(defun magit-dash-global--on-after-load (file)
  "Update integrations when deferred packages load.
FILE is the loaded file passed by `after-load-functions'."
  (when (and (stringp file)
             (string-match-p "code-review" file)
             (fboundp 'magit-dash-pr-dashboard--setup-code-review-keys))
    (magit-dash-pr-dashboard--setup-code-review-keys)))

(defcustom magit-dash-global-managed-settings
  '((vc-follow-symlinks . t)
    (version-control . t)
    (code-review-worktree-resolver . magit-dash-review-worktree-resolver))
  "Alist of (SYMBOL . VALUE) settings managed globally by `magit-dash-global-mode'.
The `code-review-worktree-resolver' entry makes code-review reuse the
review worktrees magit-dash creates, falling back to its own when the
resolver finds none.  Remove the entry to keep code-review's own worktrees."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'magit-dash)

(defvar magit-dash-global--saved-settings nil
  "Settings saved before enabling `magit-dash-global-mode'.
Each entry is (SYMBOL . (BOUNDP . VALUE)).")

(defun magit-dash-global--apply-settings ()
  "Save previous values and apply `magit-dash-global-managed-settings'."
  (unless magit-dash-global--saved-settings
    (dolist (pair magit-dash-global-managed-settings)
      (let ((sym (car pair))
            (val (cdr pair)))
        (push (cons sym (if (boundp sym) (cons t (symbol-value sym)) (cons nil nil)))
              magit-dash-global--saved-settings)
        (set sym val)))))

(defun magit-dash-global--revert-settings ()
  "Restore variables to their state prior to `magit-dash-global-mode'."
  (dolist (entry magit-dash-global--saved-settings)
    (let ((sym (car entry))
          (status (cdr entry)))
      (if (car status)
          (set sym (cdr status))
        (makunbound sym))))
  (setq magit-dash-global--saved-settings nil))

;;;###autoload
(define-minor-mode magit-dash-global-mode
  "Global minor mode for `magit-dash' integrations.
Sets up keybindings, savehist variables, and hooks while leaving the main
`magit-dash' package and its dependencies to load on demand."
  :global t
  :group 'magit-dash
  (if magit-dash-global-mode
      (progn
        (magit-dash-global--apply-settings)
        (magit-dash-global--setup-magit)
        (add-hook 'magit-mode-hook #'magit-dash-global--setup-magit)
        (add-hook 'after-load-functions #'magit-dash-global--on-after-load)
        (add-hook 'which-key-mode-hook #'magit-dash--which-key-register-replacements)
        (magit-dash--which-key-register-replacements)
        (when (boundp 'savehist-additional-variables)
          (add-to-list 'savehist-additional-variables 'magit-dash-columns)
          (add-to-list 'savehist-additional-variables 'magit-dash-pr-columns)
          (add-to-list 'savehist-additional-variables 'magit-dash-pr-saved-filters)
          (add-to-list 'savehist-additional-variables 'magit-dash--ephemeral-tags)))
    (magit-dash-global--revert-settings)
    (remove-hook 'magit-mode-hook #'magit-dash-global--setup-magit)
    (remove-hook 'after-load-functions #'magit-dash-global--on-after-load)
    (magit-dash-global--teardown-magit)
    (remove-hook 'which-key-mode-hook #'magit-dash--which-key-register-replacements)))

(provide 'magit-dash-global)
;;; magit-dash-global.el ends here
