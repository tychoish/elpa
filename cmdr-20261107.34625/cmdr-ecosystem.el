;;; cmdr-ecosystem.el --- Optional ecosystem command bindings -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/cmdr.el

;;; Commentary:
;; Opt-in bindings for common Emacs packages.  Package commands remain
;; autoloaded; loading this library does not install bindings or modes.

;;; Code:

(require 'cmdr-keys)
(require 'cmdr-macros)
(require 'seq)

(make-read-extended-command-for-prefix "clipboard")
(make-read-extended-command-for-prefix "magit")
(make-read-extended-command-for-prefix "docker")
(make-read-extended-command-for-prefix "eat")
(make-read-extended-command-for-prefix "smerge")

;;;###autoload
(defun cmdr-ecosystem-install ()
  "Install generic ecosystem bindings in `cmdr-mode-map'."
  (seq-do (lambda (prefix)
            (cmdr-keys-prefix cmdr-mode-map (car prefix) (cdr prefix)))
          '(("C-x g" . "magit") ("C-x g m" . "smerge")
            ("C-x d" . "docker") ("C-c s" . "shell")
            ("C-c s e" . "shell-eat") ("C-c q" . "anzu")))
  (seq-do (lambda (binding)
            (cmdr-keys-set cmdr-mode-map (car binding) (cdr binding)))
          '(("C-x g s" . magit-status) ("C-x g f" . magit-branch)
            ("C-x g b" . magit-blame) ("C-x g l" . git-link)
            ("C-x g x" . execute-extended-magit-command)
            ("C-x g m n" . smerge-vc-next-conflict)
            ("C-x g m k" . smerge-kill-current)
            ("C-x g m s" . smerge-start-session)
            ("C-x g m t" . smerge-keep-current)
            ("C-x g m x" . execute-extended-smerge-command)
            ("C-x d d" . docker) ("C-x d c" . docker-containers)
            ("C-x d i" . docker-images) ("C-x d v" . docker-volumes)
            ("C-x d m" . docker-contexts) ("C-x d p" . docker-compose)
            ("C-x d x" . execute-extended-docker-command)
            ("C-c s m" . eshell) ("C-c s e e" . eat)
            ("C-c s e o" . eat-other-window) ("C-c s e p" . eat-project)
            ("C-c s e P" . eat-project-other-window)
            ("C-c s e m" . execute-extended-eat-command)
            ("C-c q r" . anzu-query-replace) ("C-c q e" . anzu-query-replace-regexp)
            ("C-c h i" . consult-info) ("C-c h m" . consult-man)
            ("C-c h a" . consult-eglot-symbols)
            ("C-x C-b r" . revbufs) ("C-x x a" . revbufs)
            ("C-x x c" . execute-extended-clipboard-command)
            ("C-c '" . popper-toggle) ("C-c \\" . popper-cycle)
            ("C-c C-'" . popper-kill-latest-popup)))
  (unless (keymap-lookup cmdr-mode-map "C-c C-d")
    (cmdr-bind-deferred-keymap cmdr-mode-map "C-c C-d" 'dape 'dape-global-map "dape"))
  (unless (keymap-lookup cmdr-mode-map "C-c p")
    (cmdr-bind-deferred-keymap cmdr-mode-map "C-c p" 'projectile 'projectile-command-map "projectile")))

(provide 'cmdr-ecosystem)
;;; cmdr-ecosystem.el ends here
