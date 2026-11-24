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
  :settings ((docker-terminal-backend . auto))
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
(autoload 'switch-to-popup-buffer "cmdr-ops" nil t)

(cmdr-register-spec
  :name popper
  :probe popper-mode
  :bindings (("C-c '" . popper-toggle)
             ("C-c C-'" . popper-toggle)
             ("C-c \\" . popper-cycle)
             ("C-c /" . switch-to-popup-buffer)
             ("C-c M-'" . popper-kill-latest-popup)
             ("C-c C-M-'" . popper-toggle-type))
  :settings ((popper-display-control . nil)
             (popper-echo-dispatch-keys . ("C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"))
             (popper-reference-buffers . (arch-install-mode
                                          orgx-capture-shell-mode
                                          mcpkit-stats-mode
                                          logx-view-mode
                                          elpaish-build-mode
                                          elpaish-check-mode
                                          elpaish-status-mode
                                          elpaish-install-progress-mode
                                          agent-shell-queue-item-view-mode
                                          ert-results-mode
                                          help-mode
                                          message-mode
                                          messages-buffer-mode
                                          backtrace-mode
                                          deadgrep-mode
                                          grep-mode
                                          occur-mode
                                          compilation-mode
                                          emacs-lisp-compilation-mode
                                          native-comp-limple-mode
                                          xref--xref-buffer-mode
                                          flymake-diagnostics-buffer-mode
                                          flymake-project-diagnostics-mode
                                          magit-process-mode
                                          profiler-report-mode
                                          Man-mode
                                          woman-mode
                                          jsonrpc-events-mode
                                          diff-mode
                                          denote-sequence-hierarchy-mode
                                          "\\*Warnings\\*"
                                          "\\*Shell Command Output\\*"
                                          "\\*Async Shell Command\\*"
                                          "\\*eldoc.*\\*"))))

(cmdr-register-spec
  :name dape
  :probe dape
  :deferred-keymaps (("C-c C-d" dape dape-global-map "dape")))

(cmdr-register-spec
  :name projectile
  :probe projectile-mode
  :deferred-keymaps (("C-c p" projectile projectile-command-map "projectile")))

(cmdr-register-spec
  :name ediff
  :probe ediff
  :settings ((ediff-window-setup-function . ediff-setup-windows-plain)
             (ediff-split-window-function . split-window-horizontally)))

(autoload 'mailx-select-account "mailx" nil t)
(autoload 'mailx-jump-to-tier "mailx-views" nil t)
(autoload 'mailx-bulk-stage-current-view "mailx-bulk" nil t)
(autoload 'mailx-capture-task "mailx-org" nil t)
(autoload 'mailx-generate-refile-rule "mailx" nil t)
(autoload 'mailx-sync-and-classify-async "mailx-org" nil t)
(autoload 'mailx-compose-reply "mailx" nil t)
(autoload 'cmdr-frame-record-home-frame "cmdr-frame")

(cmdr-register-spec
  :name mailx
  :probe mailx-select-account
  :prefixes (("C-c m" . "mail"))
  :bindings (("C-c m a" . mailx-select-account)
             ("C-c m v" . mailx-jump-to-tier)
             ("C-c m s" . mailx-bulk-stage-current-view)
             ("C-c o c" . mailx-capture-task)
             ("C-c m r" . mailx-generate-refile-rule)
             ("C-c m u" . mailx-sync-and-classify-async))
  :commands ((cmdr-spec-command :category mu4e
                                :command mailx-select-account
                                :display-name "mail-select-account"
                                :description "select mail account"
                                :transient-key "ma")
             (cmdr-spec-command :category mu4e
                                :command mailx-jump-to-tier
                                :display-name "mail-jump-to-tier"
                                :description "jump to attention tier"
                                :transient-key "mv")
             (cmdr-spec-command :category mu4e
                                :command mailx-bulk-stage-current-view
                                :display-name "mail-bulk-stage"
                                :description "stage bulk triage plan"
                                :transient-key "mb")
             (cmdr-spec-command :category mu4e
                                :command mailx-capture-task
                                :display-name "mail-capture-task"
                                :description "capture task from message"
                                :transient-key "mt")
             (cmdr-spec-command :category mu4e
                                :command mailx-generate-refile-rule
                                :display-name "mail-generate-refile-rule"
                                :description "generate refile rule"
                                :transient-key "mr")
             (cmdr-spec-command :category mu4e
                                :command mailx-sync-and-classify-async
                                :display-name "mail-sync-and-classify"
                                :description "sync and classify mail async"
                                :transient-key "mu"))
  :keymap-bindings
  ((message message-mode-map
    :bindings (("M-q" . ignore)))
   (mu4e-compose mu4e-compose-minor-mode-map
    :bindings (("R" . mailx-compose-reply)
               ("<remap> <mu4e-context-switch>" . mu4e-compose-context-switch)))
   (mu4e-headers mu4e-headers-mode-map
    :bindings (("C-r" . mailx-compose-reply)
               ("B" . mailx-bulk-stage-current-view)
               ("c" . mailx-capture-task)
               ("o" . mu4e-headers-mark-for-unread)
               ("u" . mu4e-headers-mark-for-unread)
               (";" . mu4e-mark-resolve-deferred-marks)))))

(autoload 'diff-review-find-file "diff-review" nil t)
(autoload 'diff-review-magit-mode "diff-review-magit" nil t)
(autoload 'magit-dash-review-select "magit-dash-review" nil t)
(autoload 'magit-dash-review-send-to-agent "magit-dash-review" nil t)
(autoload 'magit-dash-review-stage-hitl "magit-dash-review-hitl" nil t)
(autoload 'magit-dash-review-critique-hunk "magit-dash-review" nil t)
(autoload 'magit-dash-review-draft-test "magit-dash-review" nil t)
(autoload 'magit-dash-review-promote-draft "magit-dash-review" nil t)
(autoload 'magit-dash-review-submit "magit-dash-review" nil t)
(autoload 'magit-dash-review-sync "magit-dash-review" nil t)
(autoload 'magit-dash-review-open-backend "magit-dash-review" nil t)

(defun cmdr-diff-review--activate ()
  "Enable `diff-review-magit-mode' when `diff-review' activates."
  (cond
   ((locate-library "diff-review-magit")
    (diff-review-magit-mode 1))
   (t
    (message "cmdr: diff-review-magit not found; skipping diff-review-magit-mode"))))

(cmdr-register-spec
  :name diff-review
  :probe diff-review-find-file
  :activate-hook #'cmdr-diff-review--activate
  :bindings (("C-x g v" . magit-dash-review-select))
  :keymap-bindings
  ((diff-review diff-review-file-command-map
    :bindings (("e" . magit-dash-review-send-to-agent)
               ("h" . magit-dash-review-stage-hitl)
               ("k" . magit-dash-review-critique-hunk)
               ("T" . magit-dash-review-draft-test)
               ("a" . magit-dash-review-promote-draft)
               ("S" . magit-dash-review-submit)
               ("F" . magit-dash-review-sync)))
   (diff-review diff-review-mode-map
    :bindings (("e" . magit-dash-review-send-to-agent)
               ("h" . magit-dash-review-stage-hitl)
               ("S" . magit-dash-review-submit)
               ("F" . magit-dash-review-sync)
               ("B" . magit-dash-review-open-backend)))))

(declare-function grepx-mode "grepx")
(declare-function telegax-mode "telegax")
(declare-function builder-mode "builder-mode")

(cmdr-register-spec
  :name grepx
  :probe grepx-menu
  :feature grepx
  :deferred-keymaps (("C-c g" grepx grepx-command-map "grepx"))
  :activate-hook #'grepx-mode)

(cmdr-register-spec
  :name telega
  :probe telega
  :feature telegax
  :deferred-keymaps (("C-c v" telegax telega-prefix-map "telega")
                     ("C-c n" telegax telega-prefix-map "telega"))
  :activate-hook #'telegax-mode)

(cmdr-register-spec
  :name builder
  :probe builder-compile
  :feature builder-mode
  :activate-hook #'builder-mode
  :bindings (("C-c t c" . builder-compile)))

(declare-function orgx-fill-suppressed-p "orgx-fill")
(defvar afp-suppress-fill-pfunction-list)

(defun cmdr-fill--suppress-in-aggressive-fill ()
  "Make `aggressive-fill-paragraph' honor the `orgx-fill' no-fill policy."
  (require 'orgx-fill)
  (add-to-list 'afp-suppress-fill-pfunction-list #'orgx-fill-suppressed-p))

(cmdr-register-spec
  :name aggressive-fill
  :doc "Only fill comments in programming modes."
  :probe "aggressive-fill-paragraph"
  :settings ((afp-fill-comments-only-mode-list prog-mode)))

;; `orgx-fill-guard-mode' itself is enabled from the org and markdown mode
;; hooks (see `sprite-state-mode-hooks'); this only wires aggressive filling.
(cmdr-register-spec
  :name fill
  :doc "Keep aggressive filling out of headings, code blocks and drawers."
  :probe orgx-fill-guard-mode
  :feature aggressive-fill-paragraph
  :activate-hook #'cmdr-fill--suppress-in-aggressive-fill)

(provide 'cmdr-ecosystem)
;;; cmdr-ecosystem.el ends here
