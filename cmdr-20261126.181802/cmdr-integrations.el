;;; cmdr-integrations.el --- Package and personal integration cmdr specs -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/cmdr.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Specs for mail, AI, network, notes, blogging, theme and tooling commands.
;; Every spec is probed by an autoloaded command, so absent packages are
;; skipped by `cmdr-spec-discover' and nothing here requires them.  Loading
;; this library installs nothing.

;;; Code:

(require 'cmdr-spec)
(require 'cmdr-macros)


(make-read-extended-command-for-prefix "denote")
(make-read-extended-command-for-prefix "elpaish")
(make-read-extended-command-for-prefix "agent-shell")
(make-read-extended-command-for-prefix "gptel")
(make-read-extended-command-for-prefix "gptel-set-backend")
(make-read-extended-command-for-prefix "ollama")
(make-read-extended-command-for-prefix "ollama-tailnet")
(make-read-extended-command-for-prefix "tailscale")
(make-read-extended-command-for-prefix "logx")
(make-read-extended-command-for-prefix "mcpkit")

(declare-function denote-dash-global-mode "denote-dash")
(declare-function magit-dash-global-mode "magit-dash-global")

(cmdr-register-spec
  :name denote
  :probe denote-dash
  :feature denote-dash
  :deferred-keymaps (("C-c d" denote-dash denote-dash-global-map "denote"))
  :activate-hook #'denote-dash-global-mode
  :keymap-bindings ((denote-dash denote-dash-global-map
                     ("x" . (cons "denote-commands" 'execute-extended-denote-command)))
                    (denote-dash denote-dash-org-map
                     ("r" . denote-convert-subtree)))
  :commands ((cmdr-spec-command :category denote
                                :command denote-dash
                                :description "denote list"
                                :transient-key "dv")
             (cmdr-spec-command :category denote
                                :command denote-dash-dispatch
                                :description "denote dispatch"
                                :transient-key "dd")
             (cmdr-spec-command :category denote
                                :command denote
                                :description "new note"
                                :transient-key "dn")
             (cmdr-spec-command :category denote
                                :command denote-journal-new-entry
                                :description "new journal entry"
                                :transient-key "dj")
             (cmdr-spec-command :category denote
                                :command denote-sequence-view-hierarchy
                                :description "denote sequence hierarchy"
                                :transient-key "ds")))

;; `magit-dash-open' loads first and enables `magit-dash-global-mode', which
;; leaves the main library to load on demand.  The `magit-dash' spec below
;; holds the core-library commands and fires once that library is loaded.
(cmdr-register-spec
  :name magit-dash-open
  :feature magit-dash-open
  :probe magit-dash-open
  :activate-hook #'magit-dash-global-mode
  :bindings (("C-c t d" . magit-dash-open)
             ("C-c t g" . magit-dash-open)
             ("C-x g d" . magit-dash-open)
             ("C-x g o" . magit-dash-open-repo)
             ("C-x g g" . magit-dash-gh-menu)
             ("C-x g w" . magit-dash-worktree-dispatch))
  :transient-suffixes ((magit-gh (-1) ["magit-dash" ("g" "magit-dash: prune/CI logs/PR comments/gh auth" magit-dash-gh-menu)]))
  :commands ((cmdr-spec-command :category magit-dash
                                :command magit-dash-open
                                :description "repo dashboard"
                                :transient-key "rd")
             (cmdr-spec-command :category magit-dash
                                :command magit-dash-open-repo
                                :description "magit open repo"
                                :transient-key "rr")))

(cmdr-register-spec
  :name magit-dash
  :doc "Core magit-dash commands; activates once the full library is loaded."
  :feature magit-dash
  :probe magit-dash
  :commands ((cmdr-spec-command :category magit-dash
                                :command magit-dash-sync-all
                                :description "sync all dashboard repos"
                                :transient-key "rs")
             (cmdr-spec-command :category magit-dash
                                :command magit-dash-sync-repo
                                :description "sync one dashboard repo"
                                :transient-key "ru")))

(cmdr-register-spec
  :name agent-shell
  :probe agent-shell
  :settings ((popper-reference-buffers . (agent-shell-queue-item-view-mode)))
  :prefixes (("C-c r" . "robot")
             ("C-c r s" . "agent-shell")
             ("C-c r s s" . "send"))
  :bindings (("C-c r p" . agent-shell-workflow-select)
             ("C-c r m" . agent-shell-menu-dispatch)
             ("C-c r s o" . agent-shell)
             ("C-c r s t" . agent-shell-toggle)
             ("C-c r s b" . agent-shell-switch-buffer)
             ("C-c r s n" . agent-shell-new-shell)
             ("C-c r s e" . agent-shell-new-temp-shell)
             ("C-c r s w" . agent-shell-new-worktree-shell)
             ("C-c r s p" . agent-shell-workflow-select)
             ("C-c r s m" . agent-shell-menu-dispatch)
             ("C-c r s a" . agent-shell-menu-select-action)
             ("C-c r s r" . agent-shell-menu-restart)
             ("C-c r s q" . agent-shell-queue-buffer-open)
             ("C-c r s /" . agent-shell-queue-capture)
             ("C-c r s x" . execute-extended-agent-shell-command)
             ("C-c r s s d" . agent-shell-send-dwim)
             ("C-c r s s r" . agent-shell-send-region)
             ("C-c r s s s" . agent-shell-send-region-to)
             ("C-c r s s f" . agent-shell-send-file)
             ("C-c r s s t" . agent-shell-send-file-to)
             ("C-c r s s b" . agent-shell-menu-send-buffer)
             ("C-c r s s p" . agent-shell-menu-send-file)
             ("C-c r s s y" . agent-shell-yank-dwim))
  :keymap-bindings ((agent-shell agent-shell-mode-map
                     ("C-c j" . (cons "robot-agent-shell" (cmdr-keys-prefix cmdr-mode-map "C-c r s" "agent-shell")))
                     ("C-c C-c" . agent-shell-submit)
                     ("C-c C-k" . agent-shell-interrupt)
                     ("C-c C-s" . agent-shell-prompt-steer)
                     ("C-c s" . agent-shell-prompt-steer)
                     ("C-c C-r" . agent-shell-menu-restart)
                     ("C-c r" . agent-shell-menu-restart))
                    (agent-shell-queue agent-shell-queue-mode-map
                     ("C-c j" . (cons "robot-agent-shell" (cmdr-keys-prefix cmdr-mode-map "C-c r s" "agent-shell")))))
  :commands ((cmdr-spec-command :category agent-shell
                                :command agent-shell-new-shell
                                :description "new agent shell"
                                :transient-key "an")
             (cmdr-spec-command :category agent-shell
                                :command agent-shell-menu-new-shell-in-dir
                                :description "new agent shell (dir)"
                                :transient-key "ad")
             (cmdr-spec-command :category agent-shell
                                :command agent-shell-switch-buffer
                                :transient-key "ab"
                                :description "switch agent shell buffer")
             (cmdr-spec-command :category agent-shell
                                :command agent-shell-queue-buffer-open
                                :description "open queue"
                                :transient-key "aq")
             (cmdr-spec-command :category agent-shell
                                :command agent-shell-workflow-select
                                :description "workflows"
                                :transient-key "ap")))

(cmdr-register-spec
  :name tailscale
  :probe tailscale-status
  :prefixes (("C-c r" . "robot")
             ("C-c r n" . "network"))
  :bindings (("C-c r n s" . tailscale-status)
             ("C-c r n c" . tailscale-connect)
             ("C-c r n d" . tailscale-disconnect)
             ("C-c r n y" . tailscale-copy-ip)
             ("C-c r n f" . tailscale-file-send)
             ("C-c r n x" . execute-extended-tailscale-command))
  :commands ((cmdr-spec-command :category network
                                :command tailscale-status
                                :description "tailscale status"
                                :transient-key "ns")
             (cmdr-spec-command :category network
                                :command tailscale-copy-ip
                                :description "copy node IP"
                                :transient-key "ny")
             (cmdr-spec-command :category network
                                :command tailscale-file-send
                                :description "send file via Taildrop"
                                :transient-key "nf")))

(cmdr-register-spec
  :name mail
  :probe "mu4e"
  :autoloads "mu4e-autoloads"
  :prefixes (("C-c m" . "mail"))
  :bindings (("C-c m m" . mu4e)
             ("C-c m d" . mu4e-search-maildir)
             ("C-c m b" . mu4e-search-bookmark)
             ("C-c m c" . mu4e-compose-new))
  :commands ((cmdr-spec-command :category mu4e
                                :command mu4e-compose-mail
                                :description "compose new email"
                                :transient-key "mc")
             (cmdr-spec-command :category mu4e
                                :command mu4e
                                :description "open mu4e dashboard"
                                :transient-key "mm")))

(cmdr-register-spec
  :name org
  :probe orgx-capture
  :settings ((popper-reference-buffers . (orgx-capture-shell-mode)))
  :bindings (("M-o" . orgx-capture-shell-toggle))
  :commands ((cmdr-spec-command :row 2
                                :category org-mode
                                :command orgx-capture
                                :description "org-capture with template"
                                :transient-key "oc")
             (cmdr-spec-command :row 2
                                :category org-mode
                                :command org-agenda
                                :description "org-agenda view dispatch"
                                :transient-key "oa")
             (cmdr-spec-command :row 2
                                :category org-mode
                                :command orgx-find-heading
                                :description "jump to org heading"
                                :transient-key "oh")))

(cmdr-register-spec
  :name mcpkit
  :probe mcpkit-menu
  :feature mcpkit-menu
  :prefixes (("C-c r" . "robot")
             ("C-c r c" . "mcpkit"))
  :bindings (("C-c r c c" . mcpkit-menu)
             ("C-c r c l" . mcpkit-list-services)
             ("C-c r c s" . mcpkit-start-service)
             ("C-c r c k" . mcpkit-stop-service)
             ("C-c r c i" . mcpkit-init-services)
             ("C-c r c r" . mcpkit-restart-all-services)
             ("C-c r c p" . mcpkit-show-endpoint)
             ("C-c r c t" . mcpkit-stats-view-logs)
             ("C-c r c x" . execute-extended-mcpkit-command))
  :transient-suffixes ((mcpkit-menu (-1 -1 0) ("x" "extended mcpkit commands" execute-extended-mcpkit-command))))

(cmdr-register-spec
  :name logx
  :probe logx-show-logs
  :feature logx-view
  :bindings (("C-c t l" . logx-show-logs))
  :keymap-bindings ((logx-view logx-view-mode-map
                     ("x" . (cons "logx-commands" 'execute-extended-logx-command)))))

(cmdr-register-spec
  :name sprite
  :probe sprite-list
  :settings ((popper-reference-buffers . (mcpkit-stats-mode logx-view-mode)))
  :commands ((cmdr-spec-command :row 2
                                :category sprite
                                :command sprite-list
                                :description "sprite dashboard"
                                :transient-key "sd")
             (cmdr-spec-command :row 2
                                :category sprite
                                :command sprite-create
                                :description "spawn sprite"
                                :transient-key "ss")
             (cmdr-spec-command :row 2
                                :category sprite
                                :command sprite-open-frame
                                :description "open frame in sprite"
                                :transient-key "sf")))

(cmdr-register-spec
  :name arch
  :probe arch-list
  :settings ((popper-reference-buffers . (arch-install-mode)))
  :commands ((cmdr-spec-command :row 2
                                :category arch
                                :command arch-find-package
                                :description "arch package info"
                                :transient-key "li")
             (cmdr-spec-command :row 2
                                :category arch
                                :command arch-list
                                :description "arch package list"
                                :transient-key "ll")))

(cmdr-register-spec
  :name elpaish
  :probe elpaish-install-upgrade-packages
  :settings ((popper-reference-buffers . (elpaish-build-mode elpaish-check-mode elpaish-status-mode)))
  :transient-suffixes ((elpaish-menu (-1 -1 0) ("x" "extended elpaish commands" execute-extended-elpaish-command)))
  :commands ((cmdr-spec-command :row 2
                                :category elpaish
                                :command elpaish-install-upgrade-packages
                                :description "upgrade configured packages"
                                :exclude-from-transient t)))

(cmdr-register-spec
  :name whitespace
  :probe toggle-local-whitespace-cleanup
  :settings ((whitespace-style . (face trailing tabs spaces lines newline missing-newline-at-eof empty space-mark tab-mark newline-mark)))
  :prefixes (("C-c t w" . "whitespace"))
  :bindings (("C-c t w c" . whitespace-cleanup)
             ("C-c t w t" . toggle-local-whitespace-cleanup)))

(cmdr-register-spec
  :name gptel
  :probe gptel
  :prefixes (("C-c r" . "robot")
             ("C-c r g" . "gptel"))
  :bindings (("C-c r g g" . gptel)
             ("C-c r g r" . gptel-rewrite)
             ("C-c r g a" . gptel-agent)
             ("C-c r g w" . gptel-aibo-summon)
             ("C-c r g b" . execute-extended-gptel-set-backend-command)
             ("C-c r g x" . execute-extended-gptel-command))
  :keymap-bindings ((gptel gptel-mode-map
                     ("C-c m" . gptel-menu))))

(cmdr-register-spec
  :name ollama
  :probe ollama-list-models
  :prefixes (("C-c r" . "robot")
             ("C-c r o" . "ollama"))
  :bindings (("C-c r o o" . ollama-transient-menu)
             ("C-c r o s" . ollama-status)
             ("C-c r o l" . ollama-list-models)
             ("C-c r o p" . ollama-pull-model)
             ("C-c r o d" . ollama-delete-model)
             ("C-c r o c" . ollama-copy-model)
             ("C-c r o i" . ollama-show-model)
             ("C-c r o k" . ollama-check-server)
             ("C-c r o x" . execute-extended-ollama-command)))

(cmdr-register-spec
  :name ollama-tailnet
  :probe ollama-tailnet-models
  :prefixes (("C-c r" . "robot")
             ("C-c r o" . "ollama")
             ("C-c r o t" . "tailnet"))
  :bindings (("C-c r o t s" . ollama-tailnet-status)
             ("C-c r o t d" . ollama-tailnet-discover-hosts)
             ("C-c r o t p" . ollama-tailnet-pull-model)
             ("C-c r o t r" . ollama-tailnet-service-restart)
             ("C-c r o t t" . ollama-tailnet-service-status)
             ("C-c r o t b" . ollama-tailnet-set-gptel-backend)
             ("C-c r o t l" . ollama-tailnet-models)
             ("C-c r o t f" . ollama-tailnet-search-model)
             ("C-c r o t x" . execute-extended-ollama-tailnet-command))
  :transient-suffixes ((ollama-transient-menu (-1 0) ("x" "emacs ollama commands" execute-extended-ollama-command))))

(cmdr-register-spec
  :name hud-modeline
  :probe hud-modeline-mode
  :prefixes (("C-c t" . "core")
             ("C-c t t" . "theme"))
  :bindings (("C-c t t i" . toggle-hud-modeline-icons)
             ("C-c t t s" . toggle-hud-modeline-show-buffer-size)))

(cmdr-register-spec
  :name daemons-dash
  :probe daemons-dash
  :prefixes (("C-c t" . "core"))
  :bindings (("C-c t s" . daemons-dash)))

(cmdr-register-spec
  :name eglox
  :probe eglox-describe-symbol-dwim
  :prefixes (("C-c h" . "docs")
             ("C-x C-b" . "buffer-control"))
  :bindings (("C-c h s" . eglox-describe-symbol-dwim)
             ("C-x C-b s" . projectile-save-project-buffers-picker)
             ("M-." . xref-find-definitions)))

(cmdr-register-spec
  :name tools
  :prefixes (("C-c t" . "core")
             ("C-c t t" . "theme")
             ("C-c h" . "docs"))
  :bindings (("C-c t t r" . disable-all-themes)
             ("C-c t k" . execute-extended-clipboard-command)
             ("C-c t e" . execute-extended-elpaish-command)
             ("C-c t j" . journalctl)
             ("C-c t c" . builder-compile)
             ("C-c h q" . kill-eldoc-and-help-buffers)))

(cmdr-register-spec
  :name emacs
  :doc "Emacs configuration and package maintenance commands."
  :commands ((cmdr-spec-command :row 2
                                :category emacs
                                :command builder-emacs-conf-recompile-all
                                :display-name "recompile-emacs-conf"
                                :description "recompile configuration"
                                :transient-key "er")
             (cmdr-spec-command :row 2
                                :category emacs
                                :command elpaish-install-upgrade-packages
                                :display-name "upgrade-emacs-packages"
                                :description "upgrade packages"
                                :transient-key "eu")))

(provide 'cmdr-integrations)
;;; cmdr-integrations.el ends here
