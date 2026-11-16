;;; cmdr-ecosystem.el --- Optional ecosystem command bindings -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/cmdr.el

;;; Commentary:
;; Bindings for common Emacs packages, one spec per package.  Each spec is
;; probed by an autoloaded command, so absent packages are skipped and nothing
;; is required.  Loading this library installs nothing.

;;; Code:

(require 'cmdr-spec)
(require 'cmdr-macros)

(make-read-extended-command-for-prefix "clipboard")
(make-read-extended-command-for-prefix "magit")
(make-read-extended-command-for-prefix "docker")
(make-read-extended-command-for-prefix "eat")
(make-read-extended-command-for-prefix "smerge")

(cmdr-register-spec
  :name clipboard
  :bindings (("C-x x c" . execute-extended-clipboard-command)))

(defvar magit-command-mode-map nil
  "Keymap for Magit extended commands beneath `/'.")

(autoload 'git-link "git-link" nil t)

(defun cmdr-git-link ()
  "Invoke `git-link' ensuring the generated URL is copied to the OS clipboard."
  (interactive)
  (let ((select-enable-clipboard t))
    (call-interactively #'git-link)))

(cmdr-register-spec
  :name magit
  :probe magit-status
  :prefixes (("C-x g" . "magit"))
  :bindings (("C-x g s" . magit-status)
             ("C-x g f" . magit-branch)
             ("C-x g b" . magit-blame)
             ("C-x g l" . cmdr-git-link)
             ("C-x g x" . execute-extended-magit-command))
  :keymap-bindings ((magit magit-mode-map
                     :prefixes (("/" "magit-command" magit-command-mode-map))
                     :bindings (("/ x" . (cons "magit-commands" 'execute-extended-magit-command))
                                ("/ m" . (cons "(s)merge-commands" 'execute-extended-smerge-command))))
                    (magit magit-status-mode-map
                     ("C-n" . next-line)
                     ("C-p" . previous-line))))

(cmdr-register-spec
  :name smerge
  :probe smerge-mode
  :prefixes (("C-x g m" . "smerge"))
  :bindings (("C-x g m n" . smerge-vc-next-conflict)
             ("C-x g m k" . smerge-kill-current)
             ("C-x g m r" . smerge-kill-and-vc-next-conflict)
             ("C-x g m s" . smerge-start-session)
             ("C-x g m t" . smerge-keep-current)
             ("C-x g m x" . execute-extended-smerge-command)))

(cmdr-register-spec
  :name docker
  :probe docker
  :prefixes (("C-x d" . "docker"))
  :bindings (("C-x d d" . docker)
             ("C-x d c" . docker-containers)
             ("C-x d i" . docker-images)
             ("C-x d v" . docker-volumes)
             ("C-x d m" . docker-contexts)
             ("C-x d p" . docker-compose)
             ("C-x d x" . execute-extended-docker-command))
  :transient-suffixes ((docker (-1 0) ("m" "emacs docker commands" execute-extended-docker-command))))

(cmdr-register-spec
  :name shell
  :prefixes (("C-c s" . "shell"))
  :bindings (("C-c s m" . eshell)))

(cmdr-register-spec
  :name eat
  :probe eat
  :prefixes (("C-c s" . "shell")
             ("C-c s e" . "shell-eat"))
  :bindings (("C-c s e e" . eat)
             ("C-c s e o" . eat-other-window)
             ("C-c s e p" . eat-project)
             ("C-c s e w" . eat-project-other-window)
             ("C-c s e m" . execute-extended-eat-command)))

(cmdr-register-spec
  :name anzu
  :probe anzu-query-replace
  :prefixes (("C-c q" . "anzu"))
  :bindings (("C-c q r" . anzu-query-replace)
             ("C-c q e" . anzu-query-replace-regexp)))

(cmdr-register-spec
  :name consult
  :probe consult-info
  :bindings (("C-c h i" . consult-info)
             ("C-c h m" . consult-man)))

(cmdr-register-spec
  :name consult-eglot
  :probe consult-eglot-symbols
  :bindings (("C-c h a" . consult-eglot-symbols)))

(cmdr-register-spec
  :name revbufs
  :probe revbufs
  :bindings (("C-x C-b r" . revbufs)
             ("C-x x a" . revbufs)))

(autoload 'popper-toggle "popper" nil t)
(autoload 'popper-cycle "popper" nil t)
(autoload 'popper-kill-latest-popup "popper" nil t)
(autoload 'popper-toggle-type "popper" nil t)

(cmdr-register-spec
  :name popper
  :probe popper-mode
  :bindings (("C-c '" . popper-toggle)
             ("C-c C-'" . popper-toggle)
             ("C-c \\" . popper-cycle)
             ("C-c M-'" . popper-kill-latest-popup)
             ("C-c C-M-'" . popper-toggle-type)))

(cmdr-register-spec
  :name dape
  :probe dape
  :deferred-keymaps (("C-c C-d" dape dape-global-map "dape")))

(cmdr-register-spec
  :name projectile
  :probe projectile-mode
  :deferred-keymaps (("C-c p" projectile projectile-command-map "projectile")))

(provide 'cmdr-ecosystem)
;;; cmdr-ecosystem.el ends here
