;;; eglox.el --- Eglot extensions and stability enhancements -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (projectile "2.0") (gen "0.1.0") (annotated-completing-read "0.1"))
;; Keywords: languages, tools, lsp, eglot
;; URL: https://github.com/tychoish/eglox

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Eglox provides a multi-language programming environment combining Eglot
;; lifecycle management, IDE navigation and debugging, language defaults, and
;; independently usable project integration.  `eglox-global-mode' composes
;; these features; `eglox-project-mode' works without loading Eglot.
;;
;; Features:
;; - `eglox-mode': Buffer-local minor mode for Eglot-managed buffers, bundling
;;   save hooks, import organization, and buffer-local LSP keybindings under `C-c l'.
;; - `eglox-global-mode': Global minor mode managing connection throttling,
;;   server deduplication, dead server pruning, projectile cache warming, and
;;   global LSP keybindings under `C-c l'.
;; - `eglox-command-map', `eglox-mode-map', and `eglox-global-mode-map': Dedicated
;;   command prefix and minor mode maps for LSP commands.
;; - Rate-limited buffer activation to avoid connection storms.
;; - Dead and duplicate server pruning to prevent resource leaks and reconnect loops.
;; - Projectile cache warming integration on project switch.
;; - `eglox-ide-map': xref/imenu/extended-command navigation, bound at `C-c l x'.
;; - `eglox-describe-symbol-dwim': slime/eglot/describe-symbol contextual lookup.
;; - Guarded completion-at-point to prevent repeating jsonrpc errors.
;; - `eglox-dape-map': Dape debugging commands, bound at `C-c l d', with
;;   settings applied and restored by `eglox-global-mode'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'subr-x)

(autoload 'eglox-project-mode "eglox-project" nil t)
(autoload 'eglox-projectile-cache-warm-p "eglox-project")
(autoload 'eglox-projectile-warm-cache-process-queue "eglox-project")
(autoload 'eglox-projectile-warm-cache-enqueue "eglox-project")
(autoload 'eglox-projectile-warm-cache-for-buffer "eglox-project")
(autoload 'eglox-projectile-warm-cache-on-project-change "eglox-project")
(defvar eglox-project-mode)

(declare-function python-indent-shift-left "python")
(declare-function python-indent-shift-right "python")
(declare-function projectile-project-root "projectile")
(declare-function projectile-index-project-async "projectile")
(declare-function projectile-load-project-cache "projectile")
(declare-function eglot-tempel-mode "eglot-tempel")
(declare-function flycheck-eglot-mode "flycheck-eglot")
(declare-function slime-describe-symbol "slime")
(declare-function slime-connected-p "slime")
(declare-function consult-eglot-symbols "consult-eglot")
(declare-function imenu "imenu")
(declare-function xref-find-references "xref")
(declare-function xref-find-definitions "xref")
(declare-function xref-go-back "xref")
(declare-function xref-go-forward "xref")
(declare-function xref-find-definitions-other-window "xref")
(declare-function eglot-test-at-point "eglot-test-at-point")

(declare-function eglot "eglot")
(declare-function eglot-reconnect "eglot")
(declare-function eglot-shutdown "eglot")
(declare-function eglot-list-connections "eglot")
(declare-function eglot-forget-pending-continuations "eglot")
(declare-function eglot-rename "eglot")
(declare-function eglot-code-actions "eglot")
(declare-function eglot-code-action-quickfix "eglot")
(declare-function eglot-code-action-rewrite "eglot")
(declare-function eglot-code-action-organize-imports "eglot")
(declare-function eglot-format-buffer "eglot")
(declare-function eglot-signal-didChangeConfiguration "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot--current-server-or-lose "eglot")
(declare-function eglot-ensure "eglot")
(declare-function eglot-managed-p "eglot")
(declare-function eglot--lookup-mode "eglot")
(declare-function eglot--maybe-activate-editing-mode "eglot")
(declare-function eglot-alternatives "eglot")
(declare-function eglot--project "eglot")
(declare-function eglot--shutdown-requested "eglot")
(declare-function eglot--managed-mode-off "eglot")
(declare-function jsonrpc-shutdown "jsonrpc")
(declare-function jsonrpc--process "jsonrpc")
(declare-function eieio-oset "eieio-core")

(autoload 'dape "dape" nil t)
(autoload 'dape-breakpoint-toggle "dape" nil t)
(autoload 'dape-breakpoint-remove-all "dape" nil t)
(autoload 'dape-continue "dape" nil t)
(autoload 'dape-next "dape" nil t)
(autoload 'dape-step-in "dape" nil t)
(autoload 'dape-step-out "dape" nil t)
(autoload 'dape-pause "dape" nil t)
(autoload 'dape-restart "dape" nil t)
(autoload 'dape-quit "dape" nil t)
(autoload 'dape-repl "dape" nil t)
(autoload 'dape-evaluate-expression "dape" nil t)
(autoload 'dape-watch-dwim "dape" nil t)
(autoload 'dape-info "dape" nil t)
(autoload 'dape-memory "dape" nil t)
(declare-function dape-breakpoint-save "dape")

(defvar dape-key-prefix)
(defvar dape-buffer-window-arrangement)

(defvar projectile-projects-cache)
(defvar projectile-enable-caching)
(defvar projectile-project-changed-functions)
(defvar eglot-tempel-mode)
(defvar eglot--cached-server)
(defvar eglot-menu-string)
(defvar eglot-autoshutdown)
(defvar eglot-extend-to-xref)
(defvar eglot-report-progress)
(defvar eglot-stay-out-of)
(defvar eglot-server-programs)
(defvar eglot-workspace-configuration)
(defvar eglot-mode-map)
(defvar eglot--managed-mode)
(defvar eglot--servers-by-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prefix-filtered extended commands

(defun read-extended-command-for-eglot-prefix-p (command _)
  "Predicate for `read-extended-command-predicate' accepting only eglot commands.
COMMAND is a symbol; return non-nil when its name starts with \"eglot\"."
  (string-prefix-p "eglot" (symbol-name command)))

;;;###autoload
(defun execute-extended-eglot-command ()
  "Read extended command but filtered for only those beginning with prefix `eglot'."
  (interactive)
  (let ((read-extended-command-predicate #'read-extended-command-for-eglot-prefix-p))
    (with-suppressed-warnings ((interactive-only execute-extended-command))
      (execute-extended-command nil))))

(defun read-extended-command-for-xref-prefix-p (command _)
  "Predicate for `read-extended-command-predicate' accepting only xref commands.
COMMAND is a symbol; return non-nil when its name starts with \"xref\"."
  (string-prefix-p "xref" (symbol-name command)))

;;;###autoload
(defun execute-extended-xref-command ()
  "Read extended command but filtered for only those beginning with prefix `xref'."
  (interactive)
  (let ((read-extended-command-predicate #'read-extended-command-for-xref-prefix-p))
    (with-suppressed-warnings ((interactive-only execute-extended-command))
      (execute-extended-command nil))))

(defun read-extended-command-for-slime-prefix-p (command _)
  "Predicate for `read-extended-command-predicate' accepting only slime commands.
COMMAND is a symbol; return non-nil when its name starts with \"slime\"."
  (string-prefix-p "slime" (symbol-name command)))

;;;###autoload
(defun execute-extended-slime-command ()
  "Read extended command but filtered for only those beginning with prefix `slime'."
  (interactive)
  (let ((read-extended-command-predicate #'read-extended-command-for-slime-prefix-p))
    (with-suppressed-warnings ((interactive-only execute-extended-command))
      (execute-extended-command nil))))

;;;###autoload
(defun eglox-describe-symbol-dwim (prefix)
  "Look up symbol at point contextually.
With PREFIX arg, always use `describe-symbol'.
Otherwise: use `slime-describe-symbol' if slime is connected,
`consult-eglot-symbols' if in an eglot-managed buffer,
or `describe-symbol' as fallback."
  (interactive "P")
  (cond
   (prefix
    (call-interactively #'describe-symbol))
   ((and (fboundp 'slime-describe-symbol)
         (fboundp 'slime-connected-p)
         (slime-connected-p))
    (call-interactively #'slime-describe-symbol))
   ((and (fboundp 'eglot-current-server)
         (fboundp 'consult-eglot-symbols)
         (eglot-current-server))
    (consult-eglot-symbols))
   (t
    (call-interactively #'describe-symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymaps

(defvar-keymap eglox-ide-map
  :name "ide"
  :doc "IDE/language navigation commands, attached to `eglox-command-map' at \"x\"."
  "m" #'imenu
  "c" #'xref-find-references
  "d" #'xref-find-definitions
  "p" #'xref-go-back
  "n" #'xref-go-forward
  "o" #'xref-find-definitions-other-window
  "x" #'execute-extended-xref-command
  "s" #'execute-extended-slime-command)

(defvaralias 'eglox-server-map 'eglox-global-map)
(defvar-keymap eglox-global-map
  :name "eglot-server"
  :doc "Eglot global server management commands."
  "s" #'eglot
  "r" #'eglot-reconnect
  "k" #'eglot-shutdown
  "l" #'eglot-list-connections
  "c" #'execute-extended-eglot-command
  "g" #'eglot-forget-pending-continuations)

(defvar-keymap eglox-dape-map
  :name "dape"
  :doc "Dape debugging commands attached to `eglox-command-map' at \"d\"."
  "d" #'dape
  "b" #'dape-breakpoint-toggle
  "B" #'dape-breakpoint-remove-all
  "c" #'dape-continue
  "n" #'dape-next
  "s" #'dape-step-in
  "o" #'dape-step-out
  "p" #'dape-pause
  "r" #'dape-restart
  "q" #'dape-quit
  "R" #'dape-repl
  "e" #'dape-evaluate-expression
  "w" #'dape-watch-dwim
  "i" #'dape-info
  "m" #'dape-memory)

(defvar-keymap eglox-command-map
  :name "eglot"
  :doc "Eglot buffer and server commands prefix map."
  "a" #'eglot-code-actions
  "d" (cons "dape" eglox-dape-map)
  "e" #'execute-extended-eglot-command
  "f" #'eglot-format-buffer
  "i" #'eglox-code-action-inline
  "o" #'eglox-organize-imports-for-hook
  "q" #'eglot-code-action-quickfix
  "r" #'eglot-rename
  "s" #'consult-eglot-symbols
  "t" #'eglot-test-at-point
  "u" #'eglox-update-workspace
  "w" #'eglot-code-action-rewrite
  "x" (cons "ide" eglox-ide-map)
  "l" eglox-global-map)

(defvaralias 'eglox-minor-mode-map 'eglox-mode-map)
(defvar-keymap eglox-mode-map
  :doc "Keymap active when `eglox-mode' is enabled."
  "C-c l" (cons "eglot" eglox-command-map))

(defvar-keymap eglox-global-mode-map
  :doc "Keymap active when `eglox-global-mode' is enabled."
  "C-c l" (cons "eglot" eglox-command-map))

(defcustom eglox-auto-mode-alist
  '(("\\(?:^\\|/\\)[Mm]akefile\\'" . makefile-mode)
    ("\\.mk\\'" . makefile-mode)
    ("PKGBUILD\\'" . pkgbuild-mode)
    ("\\.proto\\'" . protobuf-mode)
    ("\\.ninja\\'" . ninja-mode)
    ("\\.tf\\'" . terraform-mode)
    ("\\.tfvars\\'" . terraform-mode)
    ("\\.tfvars\\.example\\'" . terraform-mode)
    ("\\.sh\\'" . bash-ts-mode)
    ("\\.bash\\'" . bash-ts-mode)
    ("\\.bashrc\\'" . bash-ts-mode)
    ("Dockerfile" . dockerfile-ts-mode)
    ("\\.ya?ml\\'" . yaml-mode)
    ("\\.go\\'" . go-ts-mode)
    ("/go\\.work\\'" . go-mod-ts-mode)
    ("/go\\.mod\\'" . go-mod-ts-mode)
    ("\\.rs\\'" . rustic-mode)
    ("/Cargo\\.lock\\'" . toml-ts-mode)
    ("\\.py\\'" . python-ts-mode)
    ("\\.html\\'" . web-mode)
    ("\\.htm\\'" . web-mode)
    ("\\.as[cp]x\\'" . web-mode)
    ("\\.djhtml\\'" . web-mode)
    ("\\.erb\\'" . web-mode)
    ("\\.jsp\\'" . web-mode)
    ("\\.mustache\\'" . web-mode)
    ("\\.phtml\\'" . web-mode)
    ("\\.tpl\\.php\\'" . web-mode)
    ("\\.ts\\'" . typescript-ts-mode)
    ("\\.js\\'" . js-ts-mode)
    ("\\.tsx\\'" . tsx-ts-mode)
    ("\\.css\\'" . css-ts-mode)
    ("\\.json\\'" . json-ts-mode)
    ("\\.toml\\'" . toml-ts-mode)
    ("CMakeLists.txt" . cmake-ts-mode)
    ("\\.lisp\\'" . lisp-mode)
    ("\\.h\\'" . c-or-c++-mode)
    ("\\.c\\'" . c-ts-mode)
    ("\\.cpp\\'" . c++-ts-mode)
    ("\\.cc\\'" . c++-ts-mode)
    ("\\.hh\\'" . c++-ts-mode)
    ("\\.cxx\\'" . c++-ts-mode)
    ("\\.java\\'" . java-ts-mode))
  "Alist of `(REGEXP . MODE)' associations merged into `auto-mode-alist'."
  :type '(alist :key-type regexp :value-type symbol)
  :group 'eglot)

;;;###autoload
(defun eglox-modes-merge-alist (current new-entries)
  "Merge NEW-ENTRIES into CURRENT alist, overriding existing keys."
  (let ((seen (make-hash-table :test #'equal)))
    (seq-filter (lambda (entry)
                  (unless (map-elt seen (car entry))
                    (setf (map-elt seen (car entry)) t)))
                (append new-entries current))))

;;;###autoload
(defun eglox-modes-initialize ()
  "Merge `eglox-auto-mode-alist' into `auto-mode-alist'."
  (setq auto-mode-alist
        (eglox-modes-merge-alist auto-mode-alist eglox-auto-mode-alist)))

(defcustom eglox-mode-hooks
  '((js-mode-hook . eglox-ensure)
    (js-ts-mode-hook . eglox-ensure)
    (typescript-ts-mode-hook . eglox-ensure)
    (go-ts-mode-hook . (eglox--go-setup eglox-ensure))
    (go-mod-ts-mode-hook . eglox-ensure)
    (yaml-mode-hook . (eglox--yaml-setup eglox-ensure))
    (rustic-mode-hook . (eglox--rustic-setup eglox-ensure))
    (c++-ts-mode-hook . eglox-ensure)
    (c-ts-mode-hook . eglox-ensure)
    (c-or-c++-mode-hook . eglox-ensure)
    (python-mode-hook . eglox-ensure)
    (python-ts-mode-hook . (eglox--python-setup eglox-ensure))
    (go-mode-hook . eglox-ensure)
    (go-mod-mode-hook . eglox-ensure)
    (bash-ts-mode-hook . eglox-ensure)
    (bash-mode-hook . eglox-ensure)
    (sh-mode-hook . eglox-ensure)
    (emacs-lisp-mode-hook . eglox--elisp-setup)
    (window-selection-change-functions . eglox-activate-prioritize-visible))
  "Alist mapping hook variables to a function or list of functions.
Each entry has the form `(HOOK . FUNCTION)' or `(HOOK . (FUNCTION ...))'.
Functions are registered when `eglox-global-mode' is enabled and removed
when it is disabled."
  :type '(alist :key-type symbol
                :value-type (choice function (repeat function)))
  :group 'eglot)

(defun eglox--hook-functions (spec)
  "Return a list of hook functions for SPEC."
  (if (functionp spec)
      (list spec)
    (ensure-list spec)))

(defun eglox--add-hooks (&optional hooks)
  "Add hook functions from HOOKS (default `eglox-mode-hooks')."
  (seq-do (lambda (entry)
            (let ((hook (if (consp entry) (car entry) entry))
                  (fns (if (consp entry)
                           (eglox--hook-functions (cdr entry))
                         (list #'eglox-ensure))))
              (seq-do (lambda (fn) (add-hook hook fn)) fns)))
          (or hooks eglox-mode-hooks)))

(defun eglox--remove-hooks (&optional hooks)
  "Remove hook functions from HOOKS (default `eglox-mode-hooks')."
  (seq-do (lambda (entry)
            (let ((hook (if (consp entry) (car entry) entry))
                  (fns (if (consp entry)
                           (eglox--hook-functions (cdr entry))
                         (list #'eglox-ensure))))
              (seq-do (lambda (fn) (remove-hook hook fn)) fns)))
          (or hooks eglox-mode-hooks)))

(defvar eglox-default-server-configuration
  `((:yaml :format
           (:enable t
            :singleQuote :json-false
            :bracketSpacing t
            :proseWrap "preserve"
            :printWidth 80)
           :validate t
           :hover t
           :completion t)
    (:gopls :gofumpt t
            :usePlaceholders :json-false
            :hoverKind "FullDocumentation"
            :linkTarget "pkg.go.dev"
            :analyses (:unreachable t
                       :unusedvariable t)
            :codelenses (:test t
                         :tidy t
                         :upgradeDependency t
                         :generate t)
            :hints (:parameterNames :json-false
                    :ignoredError t
                    :constantValues t
                    :compositeLiteralTypes :json-false
                    :compositeLiteralFields :json-false
                    :rangeVariableTypes :json-false
                    :functionTypeParameters :json-false))
    (:rust-analyzer :initializationOptions
                    (:server (:extraEnv (:RUSTUP_TOOLCHAIN ,(if (executable-find "rustup") "nightly" "stable")))
                     :rust (:analyzerTargetDir t)
                     :cargo (:buildScripts (:enable t :features "all"))
                     :procMacro (:enable :json-false :attributes (:enable t))
                     :check (:workspace t)))
    (:pylsp :plugins
            (:black (:enabled t)
             :jedi_completion (:enabled t
                               :fuzzy t
                               :include_params t)
             :ruff (:enabled t
                    :formatEnabled t
                    :lineLength 100
                    :format ["I"]
                    :extendSelect ["I"])
             :rope (:enabled t)
             :flake8 (:enabled :json-false)
             :pycodestyle (:enabled :json-false)
             :mccabe (:enabled :json-false)
             :autopep8 (:enabled :json-false)
             :pyflakes (:enabled :json-false))))
  "Default Eglot workspace configuration.")

(defcustom eglox-use-flycheck t
  "Whether eglox should configure Flycheck and disable Flymake in Eglot buffers.
When non-nil (the default), `eglot-stay-out-of' includes \='flymake and
`flycheck-eglot-mode' is activated for diagnostic reporting."
  :type 'boolean
  :group 'eglox)

(defvar eglox--eglot-initialized nil
  "Non-nil when Eglot settings and integrations have been applied.")

(defun eglox--init-eglot (&rest _args)
  "Initialize Eglot settings, server programs, and capabilities.
Rest argument _ARGS is ignored so this can be used on hooks or advices."
  (unless eglox--eglot-initialized
    (setq eglox--eglot-initialized t)
    (let ((gc-cons-threshold most-positive-fixnum))
      (require 'eglot))
    (setq eglot-menu-string "eg")
    (setq eglot-autoshutdown t)
    (setq eglot-extend-to-xref t)
    (setq eglot-report-progress nil)
    (if eglox-use-flycheck
        (add-to-list 'eglot-stay-out-of 'flymake)
      (setq eglot-stay-out-of (delq 'flymake eglot-stay-out-of)))
    (add-to-list 'eglot-stay-out-of 'company)
    (set-face-attribute 'eglot-highlight-symbol-face nil :underline nil :weight 'bold)
    (add-to-list 'eglot-server-programs
                 `((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode)
                   . ("gopls")))
    (add-to-list 'eglot-server-programs
                 `((c-mode c++-mode c-ts-mode c++-ts-mode c-or-c++-ts-mode c-or-c++-mode)
                   . ,(eglot-alternatives
                       `(("clangd" "-j=8" "--background-index" "--clang-tidy" "--cross-file-rename" "--completion-style=detailed")
                         ("clangd")
                         ("ccls")))))
    (setq-default eglot-workspace-configuration eglox-default-server-configuration)
    (keymap-set eglot-mode-map "C-c l" (cons "eglot" eglox-command-map))
    (cl-defmethod eglot-client-capabilities :around (_server)
      (let* ((caps (cl-call-next-method))
             (workspace (plist-get caps :workspace)))
        (plist-put workspace :didChangeWatchedFiles
                   (list :dynamicRegistration :json-false))
        caps))
    (eglox--install-eglot-integrations)))

;;;###autoload
(defun eglox-ensure ()
  "Initialize Eglot and ensure it manages the current buffer."
  (interactive)
  (eglox--init-eglot)
  (eglot-ensure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dape Integration

(defcustom eglox-configure-dape t
  "Whether eglox should configure Dape defaults and lifecycle hooks.
When non-nil, `eglox-global-mode' arranges debugger windows on the right,
saves breakpoints on exit, saves file buffers on session start, and
suppresses dape's default global prefix in favor of `eglox-command-map'."
  :type 'boolean
  :group 'eglox)

(defun eglox--save-all-buffers ()
  "Save all modified file-visiting buffers without confirmation."
  (save-some-buffers t))

(defun eglox--save-dape-breakpoints ()
  "Save Dape breakpoints on exit when Dape has been loaded.
Return nil without doing anything while Dape is not loaded, so exiting an
Emacs session that never debugged cannot signal an error."
  (when (featurep 'dape)
    (dape-breakpoint-save)))

(defun eglox--setup-dape ()
  "Install Dape hooks when `eglox-configure-dape' is non-nil."
  (when eglox-configure-dape
    (add-hook 'kill-emacs-hook #'eglox--save-dape-breakpoints)
    (add-hook 'dape-start-hook #'eglox--save-all-buffers)))

(defun eglox--teardown-dape ()
  "Remove Dape lifecycle hooks."
  (remove-hook 'kill-emacs-hook #'eglox--save-dape-breakpoints)
  (remove-hook 'dape-start-hook #'eglox--save-all-buffers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Managed Mode Hook & Completion Guard

(defun eglox-ensure-hook ()
  "Enable `eglot-tempel-mode' if inactive, and cycle `flycheck-eglot-mode'.
Avoids reconnect loops by only activating `eglot-tempel-mode' when disabled."
  (when (fboundp 'eglot-tempel-mode)
    (unless (bound-and-true-p eglot-tempel-mode)
      (eglot-tempel-mode 1)))
  (when eglox-use-flycheck
    (when (fboundp 'flycheck-eglot-mode)
      (flycheck-eglot-mode -1)
      (flycheck-eglot-mode 1))))

(defun ad:eglot-completion-at-point-guard-no-server (orig-fn)
  "Return nil if no Eglot server is connected instead of signaling error.
Prevents repeating jsonrpc errors when completion timers run after mode
teardown."
  (when (eglot-current-server)
    (funcall orig-fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save Hooks & Actions

(defun eglox-server-live-p ()
  "Return non-nil if the current buffer's Eglot server process is running."
  (when-let* ((server (eglot-current-server)))
    (process-live-p (jsonrpc--process server))))

;;;###autoload
(defun eglox-format-for-hook ()
  "Format the current buffer using Eglot if managed.
When the buffer's server has died, reconnect orphaned buffers instead."
  (interactive)
  (when (eglot-managed-p)
    (if (eglox-server-live-p)
        (eglot-format-buffer)
      (eglox-cleanup-stale-connections))))

;;;###autoload
(defun eglox-organize-imports-for-hook ()
  "Organize imports for the current buffer using Eglot if managed.
When the buffer's server has died, reconnect orphaned buffers instead."
  (interactive)
  (when (eglot-managed-p)
    (if (eglox-server-live-p)
        (with-demoted-errors "WARN (`eglox-organize-imports-for-hook'): %S"
          (eglot-code-action-organize-imports (point-min) (point-max)))
      (eglox-cleanup-stale-connections))))

;;;###autoload
(defun eglox-code-action-inline ()
  "Execute \"source.inline\" code action if managed."
  (interactive)
  (when (eglot-managed-p)
    (with-demoted-errors "WARN (`eglox-code-action-inline'): %S"
      (eglot-code-actions nil nil "source.inline" t))))

;;;###autoload
(defun eglox-update-workspace ()
  "Push updated workspace configuration to the active Eglot server."
  (interactive)
  (eglot-signal-didChangeConfiguration (eglot--current-server-or-lose)))

(defun eglox-before-save-hook ()
  "Add buffer-local format and import organization to `before-save-hook'."
  (add-hook 'before-save-hook #'eglox-format-for-hook nil t)
  (add-hook 'before-save-hook #'eglox-organize-imports-for-hook nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server Pruning & Connection Lifecycle

(defun eglox-prune-dead-servers ()
  "Remove non-live server processes from `eglot--servers-by-project'."
  (map-do
   (lambda (project servers)
     (let ((live (seq-filter (lambda (s) (process-live-p (jsonrpc--process s))) servers)))
       (unless (eq (length live) (length servers))
         (setf (map-elt eglot--servers-by-project project) live))))
   eglot--servers-by-project))

(defun eglox-prune-duplicate-servers (&optional _server)
  "Keep at most one server per project in `eglot--servers-by-project'.
Shuts down duplicate servers. Optional _SERVER argument is ignored for
compatibility with `eglot-connect-hook'."
  (map-do
   (lambda (project servers)
     (when (cdr servers)
       (setf (map-elt eglot--servers-by-project project) (list (car servers)))
       (let ((slot 'shutdown-requested))
         (seq-do (lambda (s)
                   (eieio-oset s slot t)
                   (ignore-errors (jsonrpc-shutdown s t)))
                 (cdr servers)))))
   eglot--servers-by-project))

(defun eglox-reconnect-orphaned-buffers ()
  "Re-run `eglot-ensure' on live file buffers whose cached server has died."
  (seq-do
   (lambda (buffer)
     (with-current-buffer buffer
       (when (and (bound-and-true-p eglot--managed-mode)
                  (bound-and-true-p eglot--cached-server)
                  (not (process-live-p (jsonrpc--process eglot--cached-server))))
         (setq eglot--cached-server nil)
         (ignore-errors (eglot--managed-mode-off))
         (when buffer-file-name
           (ignore-errors (eglot-ensure))))))
   (buffer-list)))

;;;###autoload
(defun eglox-ensure-unmanaged-buffers ()
  "Run `eglot-ensure' in file buffers of `eglox-mode-hooks' modes lacking Eglot.
Buffers are queued for throttled activation against an existing server, and
`eglot-ensure' covers projects with no server yet, so this does not start a
connection storm.  Return the number of buffers scheduled."
  (interactive)
  (let* ((modes (thread-last eglox-mode-hooks
                  (seq-filter (lambda (entry)
                                (or (symbolp entry)
                                    (memq 'eglox-ensure
                                          (eglox--hook-functions (cdr entry))))))
                  (seq-map (lambda (entry)
                             (let ((hook (if (consp entry) (car entry) entry)))
                               (intern (string-remove-suffix "-hook" (symbol-name hook))))))))
         (buffers (seq-filter
                   (lambda (buffer)
                     (with-current-buffer buffer
                       (and buffer-file-name
                            (apply #'derived-mode-p modes)
                            (not (bound-and-true-p eglot--managed-mode)))))
                   (buffer-list))))
    (seq-do (lambda (buffer)
              (with-current-buffer buffer
                (ignore-errors (eglot-ensure)))
              (eglox-activate-enqueue buffer))
            buffers)
    (when (called-interactively-p 'interactive)
      (message "eglox: scheduled eglot in %d buffer(s)" (length buffers)))
    (length buffers)))

;;;###autoload
(defun eglox-cleanup-stale-connections ()
  "Prune dead and duplicate servers, then reconnect orphaned buffers."
  (interactive)
  (eglox-prune-dead-servers)
  (eglox-prune-duplicate-servers)
  (eglox-reconnect-orphaned-buffers))

(defvar eglox-connecting-projects nil
  "List of project roots currently establishing an initial Eglot connection.")

(defun eglox-mark-connecting (server)
  "Add SERVER's project to `eglox-connecting-projects'."
  (let ((project (eglot--project server)))
    (unless (member project eglox-connecting-projects)
      (push project eglox-connecting-projects))))

(defun eglox-unmark-connecting (server)
  "Remove SERVER's project from `eglox-connecting-projects'."
  (setq eglox-connecting-projects
        (delete (eglot--project server) eglox-connecting-projects)))

(defun ad:eglot--connect-dedupe (orig-fn managed-modes project class contact language-ids)
  "Skip redundant `eglot--connect' calls for PROJECT.
Returns an existing live server if present, or nil if a connection handshake
is currently in flight for PROJECT."
  (cond
   ((let ((existing (car (gethash project eglot--servers-by-project))))
      (and existing (process-live-p (jsonrpc--process existing)) existing)))
   ((member project eglox-connecting-projects) nil)
   (t (funcall orig-fn managed-modes project class contact language-ids))))

(defun ad:eglot--on-shutdown-cleanup-stale (orig-fn server)
  "Clean up stale connections when SERVER exits unexpectedly."
  (let ((crashed (not (eglot--shutdown-requested server))))
    (eglox-unmark-connecting server)
    (ignore-errors (funcall orig-fn server))
    (when crashed
      (eglox-cleanup-stale-connections))))

(defvar eglox-cleanup-stale-connections-timer nil
  "Idle timer running `eglox-cleanup-stale-connections'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Throttled Buffer Activation

(defconst eglox-activate-burst-limit 3
  "Maximum buffers activated within `eglox-activate-burst-window' before queuing.")

(defconst eglox-activate-burst-window 2
  "Burst window in seconds for `eglox-activate-burst-limit'.")

(defconst eglox-activate-batch-delay 3
  "Delay in seconds between drains of `eglox-activate-queue'.")

(defvar eglox-activate-queue nil
  "Buffers queued for throttled `eglot--maybe-activate-editing-mode'.")

(defvar eglox-activate-recent nil
  "Timestamps of recent unthrottled activations (newest first).")

(defvar eglox-activate-timer nil
  "Timer draining `eglox-activate-queue', or nil when idle.")

(defconst eglox-activate-log-interval 60
  "Minimum seconds between queue size log messages from the drain timer.")

(defvar eglox-activate-last-log 0.0
  "Float time of the last `eglox-activate-queue' size log message.")

(defun eglox-activate-buffer-pending-p (buffer)
  "Return non-nil if BUFFER is live, non-empty, and not yet managed by Eglot."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (not (zerop (buffer-size)))
              (not (bound-and-true-p eglot--managed-mode))))))

(defun eglox-activate-prune-queue ()
  "Drop dead, empty, and already managed buffers from `eglox-activate-queue'.
Return the number of buffers removed."
  (let ((before (length eglox-activate-queue)))
    (setq eglox-activate-queue
          (seq-filter #'eglox-activate-buffer-pending-p eglox-activate-queue))
    (- before (length eglox-activate-queue))))

;;;###autoload
(defun eglox-activate-queue-size ()
  "Return the number of pending buffers in `eglox-activate-queue'.
Dead, empty, and managed buffers are pruned first."
  (eglox-activate-prune-queue)
  (length eglox-activate-queue))

;;;###autoload
(defun eglox-activate-queue-report ()
  "Report the activation queue size and drain timer state in the echo area."
  (interactive)
  (let ((pruned (eglox-activate-prune-queue)))
    (message "eglox: %d buffer(s) queued for activation (%d pruned), drain timer %s"
             (length eglox-activate-queue) pruned
             (if (timerp eglox-activate-timer) "active" "idle"))))

(defun eglox-activate-log-queue (pruned)
  "Log the size of `eglox-activate-queue' and PRUNED count.
Log at most once per interval."
  (when (>= (- (float-time) eglox-activate-last-log) eglox-activate-log-interval)
    (setq eglox-activate-last-log (float-time))
    (message "eglox: %d buffer(s) queued for activation (%d pruned)"
             (length eglox-activate-queue) pruned)))

(defun eglox-activate-process-queue ()
  "Prune, then drain up to `eglox-activate-burst-limit' queued buffers per tick.
Cancels `eglox-activate-timer' when the queue is empty."
  (let ((pruned (eglox-activate-prune-queue)))
    (if (null eglox-activate-queue)
        (when eglox-activate-timer
          (cancel-timer eglox-activate-timer)
          (setq eglox-activate-timer nil))
      (eglox-activate-log-queue pruned)
      (dotimes (_ eglox-activate-burst-limit)
        (when-let* ((buf (pop eglox-activate-queue)))
          (ignore-errors
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (eglot--maybe-activate-editing-mode)))))))))

;;;###autoload
(defun eglox-activate-flush-queue (&optional count)
  "Activate up to COUNT queued buffers now, bypassing the burst throttle.
COUNT defaults to 25; interactively, use a numeric prefix argument to change it.
Return the number of buffers left in the queue."
  (interactive "P")
  (eglox-activate-prune-queue)
  (dotimes (_ (if count (prefix-numeric-value count) 25))
    (when-let* ((buf (pop eglox-activate-queue)))
      (ignore-errors
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (let ((eglox-activate-recent nil))
              (eglot--maybe-activate-editing-mode)))))))
  (when (called-interactively-p 'interactive)
    (message "eglox: %d buffer(s) still queued" (length eglox-activate-queue)))
  (length eglox-activate-queue))

(defun eglox-activate-enqueue (buffer)
  "Add BUFFER to `eglox-activate-queue' if pending.
Start the drain timer if needed."
  (eglox-activate-prune-queue)
  (when (and (eglox-activate-buffer-pending-p buffer)
             (not (memq buffer eglox-activate-queue)))
    (setq eglox-activate-queue
          (append eglox-activate-queue (list buffer))))
  (when (and eglox-activate-queue (not eglox-activate-timer))
    (setq eglox-activate-timer
          (run-with-timer eglox-activate-batch-delay
                          eglox-activate-batch-delay
                          #'eglox-activate-process-queue))))

(defun ad:eglot--maybe-activate-editing-mode-throttle (orig-fn)
  "Rate-limit buffer activation to prevent connection storms.
Buffers without a file or configured server bypass throttling.
Surplus buffers are queued in `eglox-activate-queue'."
  (setq eglox-activate-recent
        (seq-filter (lambda (ts) (< (float-time (time-since ts))
                                    eglox-activate-burst-window))
                    eglox-activate-recent))
  (cond
   ((not (and buffer-file-name (cdr (eglot--lookup-mode major-mode))))
    (funcall orig-fn))
   ((< (length eglox-activate-recent) eglox-activate-burst-limit)
    (push (current-time) eglox-activate-recent)
    (funcall orig-fn))
   (t
    (eglox-activate-enqueue (current-buffer)))))

(defun eglox-activate-prioritize-visible (&optional window-or-frame)
  "Immediately activate WINDOW-OR-FRAME's buffer if pending in the queue."
  (let* ((win (cond
               ((windowp window-or-frame) window-or-frame)
               ((framep window-or-frame) (frame-selected-window window-or-frame))
               (t (selected-window))))
         (buf (and (window-live-p win) (window-buffer win))))
    (when (and buf (memq buf eglox-activate-queue))
      (setq eglox-activate-queue (delq buf eglox-activate-queue))
      (with-current-buffer buf
        (eglot--maybe-activate-editing-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor Modes (Local & Global)

;;;###autoload
(define-minor-mode eglox-mode
  "Buffer-local minor mode for Eglot enhancements in managed buffers."
  :lighter " eglox"
  :keymap eglox-mode-map
  (if eglox-mode
      (progn
        (eglox-ensure-hook)
        (eglox-before-save-hook))
    (remove-hook 'before-save-hook #'eglox-format-for-hook t)
    (remove-hook 'before-save-hook #'eglox-organize-imports-for-hook t)))

(defun eglox--managed-mode-sync ()
  "Synchronize `eglox-mode' with `eglot--managed-mode'."
  (eglox-mode (if (bound-and-true-p eglot--managed-mode) 1 -1)))

(defvar eglox-global-mode)

(defun eglox--install-eglot-integrations ()
  "Install advices, hooks, and background timers for Eglot.
Only called when `eglot' is loaded and `eglox-global-mode' is enabled."
  (when eglox-global-mode
    (advice-add 'eglot-completion-at-point :around #'ad:eglot-completion-at-point-guard-no-server)
    (advice-add 'eglot--connect :around #'ad:eglot--connect-dedupe)
    (advice-add 'eglot--on-shutdown :around #'ad:eglot--on-shutdown-cleanup-stale)
    (advice-add 'eglot--maybe-activate-editing-mode :around #'ad:eglot--maybe-activate-editing-mode-throttle)
    (add-hook 'eglot-managed-mode-hook #'eglox--managed-mode-sync)
    (add-hook 'eglot-connect-hook #'eglox-prune-duplicate-servers)
    (add-hook 'eglot-server-initialized-hook #'eglox-mark-connecting)
    (add-hook 'eglot-connect-hook #'eglox-unmark-connecting)
    (unless (timerp eglox-cleanup-stale-connections-timer)
      (setq eglox-cleanup-stale-connections-timer
            (run-with-idle-timer 300 t #'eglox-cleanup-stale-connections)))))

(defun eglox--uninstall-eglot-integrations ()
  "Remove advices, hooks, and background timers for Eglot."
  (advice-remove 'eglot-completion-at-point #'ad:eglot-completion-at-point-guard-no-server)
  (advice-remove 'eglot--connect #'ad:eglot--connect-dedupe)
  (advice-remove 'eglot--on-shutdown #'ad:eglot--on-shutdown-cleanup-stale)
  (advice-remove 'eglot--maybe-activate-editing-mode #'ad:eglot--maybe-activate-editing-mode-throttle)
  (remove-hook 'eglot-managed-mode-hook #'eglox--managed-mode-sync)
  (remove-hook 'eglot-connect-hook #'eglox-prune-duplicate-servers)
  (remove-hook 'eglot-server-initialized-hook #'eglox-mark-connecting)
  (remove-hook 'eglot-connect-hook #'eglox-unmark-connecting)
  (when (timerp eglox-cleanup-stale-connections-timer)
    (cancel-timer eglox-cleanup-stale-connections-timer)
    (setq eglox-cleanup-stale-connections-timer nil)))

(defcustom eglox-settings
  (append (when (executable-find "rustup")
            '((rustic-rustfmt-args . "+nightly")))
          '((dape-key-prefix . nil)
            (dape-buffer-window-arrangement . right)
            (show-paren-delay . 0.25)
            (comment-auto-fill-only-comments . t)
            (makefile-electric-keys . t)
            (rust-mode-treesitter-derive . t)
            (rustic-lsp-client . eglot)
            (rustic-format-trigger . on-save)
            (rustic-cargo-clippy-trigger-fix . on-compile)
            (rustic-use-rust-save-some-buffers . t)
            (rustic-clippy-arguments . "--all --all-features -- --deny warnings")
            (terraform-format-on-save . t)
            (terraform-indent-level . 2)
            (web-mode-markup-indent-offset . 2)
            (web-mode-code-indent-offset . 2)
            (web-mode-css-indent-offset . 2)
            (js-indent-level . 2)
            (web-mode-enable-auto-pairing . t)
            (web-mode-enable-auto-expanding . t)
            (web-mode-enable-css-colorization . t)))
  "Alist of `(SYMBOL . VALUE)' settings managed by `eglox-global-mode'."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'eglot)

(defvar eglox--saved-settings nil
  "Saved prior settings recorded by `eglox--apply-settings'.")

(defun eglox--apply-settings ()
  "Apply `eglox-settings' and record prior default values."
  (unless eglox--saved-settings
    (seq-do (lambda (entry)
              (let ((symbol (car entry))
                    (value (cdr entry)))
                (unless (and (not eglox-configure-dape)
                             (memq symbol '(dape-key-prefix dape-buffer-window-arrangement)))
                  (push (cons symbol
                              (cons (default-boundp symbol)
                                    (and (default-boundp symbol)
                                         (default-value symbol))))
                        eglox--saved-settings)
                  (set-default symbol value))))
            eglox-settings)))

(defun eglox--restore-settings ()
  "Restore settings saved by `eglox--apply-settings'."
  (seq-do (lambda (entry)
            (let ((symbol (car entry))
                  (bound (cadr entry))
                  (value (cddr entry)))
              (cond
               (bound
                (set-default symbol value))
               ((get symbol 'standard-value)
                (set-default symbol (eval (car (get symbol 'standard-value)) t)))
               (t
                (makunbound symbol)))))
          eglox--saved-settings)
  (setq eglox--saved-settings nil))

(defcustom eglox-go-fill-column 100
  "Fill column for Go buffers."
  :type 'integer :group 'eglot)
(defcustom eglox-python-fill-column 100
  "Fill column for Python buffers."
  :type 'integer :group 'eglot)
(defcustom eglox-python-tab-width 4
  "Tab width for Python buffers."
  :type 'integer :group 'eglot)
(defcustom eglox-yaml-tab-width 2
  "Tab width for YAML buffers."
  :type 'integer :group 'eglot)
(defcustom eglox-enable-yaml-pro t
  "When non-nil, enable yaml-pro when available."
  :type 'boolean :group 'eglot)
(defcustom eglox-enable-builder-elisp t
  "When non-nil, enable Builder integration when available."
  :type 'boolean :group 'eglot)

(defvar eglox--python-keys nil)
(defvar python-base-mode-map)

(defun eglox--configure-python ()
  "Install Python bindings while the global mode is active."
  (when (and eglox-global-mode (boundp 'python-base-mode-map)
             (not eglox--python-keys))
    (setq eglox--python-keys
          (list (lookup-key python-base-mode-map (kbd "M-<left>"))
                (lookup-key python-base-mode-map (kbd "M-<right>"))))
    (keymap-set python-base-mode-map "M-<left>" #'python-indent-shift-left)
    (keymap-set python-base-mode-map "M-<right>" #'python-indent-shift-right)
    (font-lock-add-keywords 'python-ts-mode '(("\t" 0 'trailing-whitespace prepend)))))

(with-eval-after-load 'python (eglox--configure-python))

(defun eglox--go-setup ()
  "Set defaults for Go buffers."
  (setq-local fill-column eglox-go-fill-column)
  (setq-local compilation-error-screen-columns nil))

(defun eglox--python-setup ()
  "Set defaults for Python buffers."
  (setq-local fill-column eglox-python-fill-column)
  (setq-local tab-width eglox-python-tab-width))

(defun eglox--yaml-setup ()
  "Set defaults for YAML buffers."
  (setq-local tab-width eglox-yaml-tab-width)
  (when (and eglox-enable-yaml-pro (fboundp 'yaml-pro-mode))
    (yaml-pro-mode 1)))

(defun eglox--rustic-setup ()
  "Avoid save prompts in file-visiting Rustic buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))

(defun eglox--elisp-setup ()
  "Enable Builder integration when available."
  (when (and eglox-enable-builder-elisp (fboundp 'builder-elisp-mode))
    (builder-elisp-mode 1)))

(defcustom eglox-enable-project-mode t
  "When non-nil, enable the independent project experience with Eglox."
  :type 'boolean :group 'eglot)
(defvar eglox--owns-project-mode nil)
(defvar eglox--global-active nil)

(defun eglox--enable-global ()
  "Enable global Eglot extensions, mode hooks, and projectile integration."
  (when (and eglox-enable-project-mode (not eglox-project-mode))
    (eglox-project-mode 1)
    (setq eglox--owns-project-mode t))
  (eglox--apply-settings)
  (eglox-modes-initialize)
  (eglox--configure-python)
  (eglox--add-hooks)
  (eglox--setup-dape)
  (when eglox--eglot-initialized
    (eglox--install-eglot-integrations)))

(defun eglox--disable-global ()
  "Disable global Eglot extensions, mode hooks, timers, and advices."
  (when eglox--owns-project-mode
    (eglox-project-mode -1)
    (setq eglox--owns-project-mode nil))
  (eglox--restore-settings)
  (when eglox--python-keys
    (when (eq (lookup-key python-base-mode-map (kbd "M-<left>")) #'python-indent-shift-left)
      (keymap-set python-base-mode-map "M-<left>" (car eglox--python-keys)))
    (when (eq (lookup-key python-base-mode-map (kbd "M-<right>")) #'python-indent-shift-right)
      (keymap-set python-base-mode-map "M-<right>" (cadr eglox--python-keys)))
    (font-lock-remove-keywords 'python-ts-mode '(("\t" 0 'trailing-whitespace prepend)))
    (setq eglox--python-keys nil))
  (eglox--remove-hooks)
  (eglox--uninstall-eglot-integrations)
  (eglox--teardown-dape)
  (when (timerp eglox-activate-timer)
    (cancel-timer eglox-activate-timer)
    (setq eglox-activate-timer nil)))

;;;###autoload
(define-minor-mode eglox-global-mode
  "Global minor mode bundling Eglot enhancements and stability protections."
  :global t
  :keymap eglox-global-mode-map
  :group 'eglot
  (cond
   ((and eglox-global-mode (not eglox--global-active))
    (setq eglox--global-active t)
    (condition-case err
        (eglox--enable-global)
      (error
       (eglox-global-mode -1)
       (signal (car err) (cdr err)))))
   ((and (not eglox-global-mode) eglox--global-active)
    (eglox--disable-global)
    (setq eglox--global-active nil))))

(provide 'eglox)
;;; eglox.el ends here
