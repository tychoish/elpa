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
(defvar magit-dash-gh-pr-columns)
(defvar which-key-mode)
(defvar which-key-replacement-alist)
(defvar magit-dash-which-key-abbreviate-commands 'strip
  "How to abbreviate magit-dash commands in which-key.
One of `strip', `short', or nil.")
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

(defun magit-dash--which-key-register-replacements ()
  "Register command abbreviations in `which-key-replacement-alist'.
Applied for `magit-dash-mode'.  Does nothing if
`magit-dash-which-key-abbreviate-commands' is nil or
`which-key-replacement-alist' is not defined."
  (when (and (bound-and-true-p magit-dash-which-key-abbreviate-commands)
             (boundp 'which-key-replacement-alist))
    (let* ((prefix-repl (if (memq magit-dash-which-key-abbreviate-commands '(strip t))
                            "\\1"
                          "md-\\1"))
           (rules (list (cons (cons nil "\\`magit-dash-\\(.*\\)")
                              (cons nil prefix-repl)))))
      (setq which-key-replacement-alist
            (cons (cons 'magit-dash-mode rules)
                  (assq-delete-all 'magit-dash-mode which-key-replacement-alist))))))

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

;;;###autoload
(define-minor-mode magit-dash-global-mode
  "Global minor mode for `magit-dash' integrations.
Sets up keybindings, savehist variables, and hooks while leaving the main
`magit-dash' package and its dependencies to load on demand."
  :global t
  :group 'magit-dash
  (if magit-dash-global-mode
      (progn
        (with-eval-after-load 'magit
          (keymap-set magit-mode-map "C-c C-d" #'magit-dash-open-other-window)
          (keymap-set magit-mode-map "." #'magit-dash-dispatch)
          (add-hook 'magit-post-refresh-hook #'magit-dash--on-magit-post-refresh)
          (add-hook 'magit-post-worktree-add-hook #'magit-dash-worktree-post-add-hook-function))
        (add-hook 'which-key-mode-hook #'magit-dash--which-key-register-replacements)
        (when (boundp 'savehist-additional-variables)
          (add-to-list 'savehist-additional-variables 'magit-dash-columns)
          (add-to-list 'savehist-additional-variables 'magit-dash-gh-pr-columns)))
    (with-eval-after-load 'magit
      (keymap-unset magit-mode-map "C-c C-d" t)
      (keymap-unset magit-mode-map "." t)
      (remove-hook 'magit-post-refresh-hook #'magit-dash--on-magit-post-refresh)
      (remove-hook 'magit-post-worktree-add-hook #'magit-dash-worktree-post-add-hook-function))
    (remove-hook 'which-key-mode-hook #'magit-dash--which-key-register-replacements)))

(provide 'magit-dash-global)
;;; magit-dash-global.el ends here
