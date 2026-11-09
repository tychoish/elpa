;;; minaid.el --- Unified minad completion and selection stack -*- lexical-binding: t; -*-

;; Author: Tycho Garen
;; URL: https://github.com/tychoish/minaid
;; Keywords: convenience, completion, matching, tools
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (vertico "1.0") (marginalia "1.0") (orderless "1.0") (corfu "1.0") (cape "1.0") (tempel "1.0") (consult "1.0") (embark "1.0"))

;;; Commentary:
;;
;; Consolidated configuration and unified lifecycle management for the "minad"
;; completion ecosystem:
;;   - Vertico: vertical minibuffer completion UI with multiform views
;;   - Marginalia: rich annotations for minibuffer completion candidates
;;   - Orderless: space-separated component completion style
;;   - Corfu: in-buffer popup completion overlay with history, quick jump, and indexing
;;   - Cape: completion-at-point function extensions and composition wrappers
;;   - Consult: practical search and navigation commands (lazily loaded)
;;   - Embark: contextual minibuffer and in-buffer actions (lazily loaded)
;;   - Grepx: preferred search engine for live and persistent grep/ripgrep operations
;;
;; Architectural principles:
;; 1. Unified lifecycle: `minaid-mode' coordinates activation, buffer hooks,
;;    and teardown across all component packages in a single switch.
;; 2. Eager UI core: Vertico, Marginalia, Orderless, Corfu, Corfu-Quick, and Cape
;;    are hard-required for instant minibuffer and in-buffer completion.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'subr-x)
(require 'vertico)
(require 'vertico-multiform)
(require 'marginalia)
(require 'orderless)
(require 'corfu)
(require 'cape)
(require 'tempel)

;; Autoload on-demand grepx commands
(autoload 'grepx-live "grepx" nil t)
(autoload 'grepx-live-pwd "grepx" nil t)
(autoload 'grepx-project "grepx" nil t)
(autoload 'grepx-directory "grepx" nil t)
(autoload 'grepx-menu "grepx" nil t)

;; Forward declarations for functions and variables from lazily loaded packages
(declare-function consult-find "consult")
(declare-function consult-info "consult")
(declare-function consult-man "consult")
(declare-function consult-eglot-symbols "consult-eglot")
(declare-function consult-complex-command "consult")
(declare-function consult-completion-in-region "consult")
(declare-function consult-line "consult")
(declare-function consult-line-multi "consult")
(declare-function consult-history "consult")
(declare-function consult-isearch-history "consult")
(declare-function consult-flyspell "consult-flyspell")
(declare-function consult-flycheck "consult-flycheck")
(declare-function nerd-icons-corfu-formatter "nerd-icons-corfu")
(declare-function anzu-mode "anzu")
(declare-function nerd-icons-xref-mode "nerd-icons-xref")
(defvar nerd-icons-completion-mode)
(defvar nerd-icons-completion-mode-hook)
(defvar nerd-icons-xref-mode)
(declare-function eglot-completion-at-point "eglot")
(declare-function which-key-show-keymap "which-key")
(declare-function which-key--popup-showing-p "which-key")
(declare-function corfu-history-mode "corfu-history")
(declare-function corfu-indexed-mode "corfu-indexed")
(declare-function corfu-popupinfo-mode "corfu-popupinfo")
(declare-function corfu-popupinfo-toggle "corfu-popupinfo")
(declare-function corfu-quick-jump "corfu-quick")
(declare-function corfu-quick-complete "corfu-quick")
(declare-function corfu-quick-insert "corfu-quick")
(defvar corfu-popupinfo-delay)
(defvar corfu-indexed-start)
(declare-function project-current "project")
(declare-function project-root "project")
(declare-function projectile-project-root "projectile")
(declare-function annotated-completing-read-directory "annotated-completing-read")

(defvar embark-prompter)
(defvar embark-quit-after-action)
(defvar consult-preview-key)
(defvar consult-narrow-key)
(defvar consult-async-split-style)
(defvar consult-async-min-input)
(defvar consult-async-input-debounce)
(defvar consult-async-input-throttle)
(defvar consult-async-refresh-delay)
(defvar consult-project-function)
(defvar register-preview-delay)
(defvar xref-show-xrefs-function)
(defvar xref-show-definitions-function)
(defvar completion-preview-sort-function)
(defvar corfu-auto-prefix)
(defvar corfu-auto-delay)
(defvar eglot--managed-mode)

(cl-defmethod nerd-icons-completion-get-icon :around (candidate (_category (eql buffer)))
  "Skip icon lookup when CANDIDATE names a killed buffer."
  (if (buffer-live-p (get-buffer candidate))
      (cl-call-next-method)
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization

(defgroup minaid nil
  "Consolidated completion and selection suite."
  :group 'convenience
  :group 'completion
  :prefix "minaid-")

(defcustom minaid-corfu-text-auto-prefix 2
  "Number of characters before Corfu popup triggers in text modes."
  :type 'integer
  :group 'minaid)

(defcustom minaid-corfu-prog-auto-prefix 3
  "Number of characters before Corfu popup triggers in programming modes."
  :type 'integer
  :group 'minaid)

(defcustom minaid-corfu-auto-delay 0.15
  "Idle delay in seconds before Corfu popup appears."
  :type 'number
  :group 'minaid)

;;;###autoload
(defcustom minaid-tempel-directory
  (expand-file-name "templates" user-emacs-directory)
  "Directory of Tempel templates.
Used for `tempel-path' while `minaid-mode' is enabled and by
`minaid-open-custom-template-file'."
  :type 'directory
  :group 'minaid)

(defcustom minaid-vertico-count 25
  "Maximum candidate count shown in Vertico minibuffer."
  :type 'integer
  :group 'minaid)

(defcustom minaid-enable-recursive-minibuffers t
  "Whether `minaid-mode' enables recursive minibuffers."
  :type 'boolean
  :group 'minaid)

(defcustom minaid-minibuffer-prompt-properties
  '(read-only t cursor-intangible t face minibuffer-prompt)
  "Minibuffer prompt properties applied while `minaid-mode' is active."
  :type '(repeat sexp)
  :group 'minaid)

(defcustom minaid-text-mode-ispell-word-completion nil
  "Value of `text-mode-ispell-word-completion' applied in `minaid-mode'."
  :type 'boolean
  :group 'minaid)

(defcustom minaid-completion-cycle-threshold 2
  "Number of candidates below which TAB cycles completion."
  :type '(choice (const :tag "Never cycle" nil)
                 (const :tag "Always cycle" t)
                 (integer :tag "Cycle if candidates <= N"))
  :group 'minaid)

(defcustom minaid-completion-ignore-case t
  "Whether minibuffer completion ignores case in `minaid-mode'."
  :type 'boolean
  :group 'minaid)

(defcustom minaid-read-buffer-completion-ignore-case t
  "Whether `read-buffer' completion ignores case in `minaid-mode'."
  :type 'boolean
  :group 'minaid)

(defcustom minaid-read-file-name-completion-ignore-case t
  "Whether `read-file-name' completion ignores case in `minaid-mode'."
  :type 'boolean
  :group 'minaid)

(defcustom minaid-read-extended-command-predicate
  #'command-completion-default-include-p
  "Predicate to filter commands in `execute-extended-command'."
  :type '(choice (const :tag "All commands" nil)
                 (function :tag "Predicate function"))
  :group 'minaid)

(defcustom minaid-reject-empty-kill-ring t
  "Whether `minaid-mode' filters empty strings out of `kill-ring'.
Uses `kill-transform-function' to reject empty additions."
  :type 'boolean
  :group 'minaid)

(defcustom minaid-dabbrev-ignored-buffer-regexps '("\\` ")
  "List of regexps matching names of buffers that Dabbrev should ignore."
  :type '(repeat regexp)
  :group 'minaid)

(defcustom minaid-dabbrev-ignored-buffer-modes
  '(authinfo-mode doc-view-mode pdf-view-mode tags-table-mode)
  "List of major modes that Dabbrev should ignore."
  :type '(repeat symbol)
  :group 'minaid)

;;;###autoload
(defcustom minaid-abbrev-directory
  (expand-file-name "abbrev" user-emacs-directory)
  "Directory of abbrev definition files loaded by `minaid-load-abbrev-files'."
  :type 'directory
  :group 'minaid)

(defvaralias 'minaid-map 'minaid-completion-map
  "Alias for `minaid-completion-map', container for completion-at-point shortcuts.")

(defvar-keymap minaid-completion-map
  :doc "Completion and template commands under C-c .")

(keymap-set minaid-completion-map "TAB" #'completion-at-point)
(keymap-set minaid-completion-map "." #'completion-at-point)
(keymap-set minaid-completion-map "p" #'completion-at-point)
(keymap-set minaid-completion-map "/" #'dabbrev-expand)
(keymap-set minaid-completion-map "c" #'dabbrev-completion)
(keymap-set minaid-completion-map "i" #'tempel-insert)
(keymap-set minaid-completion-map "t" #'tempel-insert)
(keymap-set minaid-completion-map "s" #'tempel-complete)
(keymap-set minaid-completion-map "x" #'tempel-expand)
(keymap-set minaid-completion-map "]" #'tempel-next)
(keymap-set minaid-completion-map "[" #'tempel-previous)
(keymap-set minaid-completion-map "RET" #'tempel-done)
(keymap-set minaid-completion-map "v" #'minaid-open-custom-template-file)
(keymap-set minaid-completion-map "d" #'cape-dabbrev)
(keymap-set minaid-completion-map "h" #'cape-history)
(keymap-set minaid-completion-map "f" #'cape-file)
(keymap-set minaid-completion-map "n" #'cape-elisp-symbol)
(keymap-set minaid-completion-map "b" #'cape-elisp-block)
(keymap-set minaid-completion-map "a" #'cape-abbrev)
(keymap-set minaid-completion-map "l" #'cape-line)
(keymap-set minaid-completion-map "w" #'cape-dict)
(keymap-set minaid-completion-map "k" #'cape-keyword)
(keymap-set minaid-completion-map ":" #'cape-emoji)
(keymap-set minaid-completion-map "e" #'cape-emoji)
(keymap-set minaid-completion-map "m" #'cape-sgml)
(keymap-set minaid-completion-map "u" #'cape-rfc1345)
(keymap-set minaid-completion-map ";" #'minaid-corfu-at-point)
(keymap-set minaid-completion-map "\\" #'cape-tex)

(defvar-keymap minaid-consult-search-map
  :doc "Search and grep commands under C-c C-; s (preferring grepx)")

(keymap-set minaid-consult-search-map "l" 'consult-line)
(keymap-set minaid-consult-search-map "m" 'consult-line-multi)
(keymap-set minaid-consult-search-map "k" 'consult-keep-lines)
(keymap-set minaid-consult-search-map "f" 'consult-focus-lines)
(keymap-set minaid-consult-search-map "r" #'grepx-live)
(keymap-set minaid-consult-search-map "g" #'grepx-project)
(keymap-set minaid-consult-search-map "d" #'grepx-directory)
(keymap-set minaid-consult-search-map "s" #'grepx-live-pwd)
(keymap-set minaid-consult-search-map "?" #'grepx-menu)

(defvar-keymap minaid-consult-map
  :doc "Consult and Grepx search commands under C-c C-;")

(keymap-set minaid-consult-map "s" (cons "search" minaid-consult-search-map))
(keymap-set minaid-consult-map "t" 'tempel-insert)
(keymap-set minaid-consult-map "h" 'consult-history)
(keymap-set minaid-consult-map "d" #'grepx-live-pwd)
(keymap-set minaid-consult-map "r" #'grepx-live)
(keymap-set minaid-consult-map "g" #'grepx-project)
(keymap-set minaid-consult-map "m" #'grepx-menu)
(keymap-set minaid-consult-map "k" 'consult-kmacro)
(keymap-set minaid-consult-map "f" #'consult-flyspell)
(keymap-set minaid-consult-map "c" #'consult-flycheck)

(defvaralias 'minaid-mode-map 'minaid-global-map
  "Alias for `minaid-global-map' matching conventional minor-mode keymap name.")

(defvar-keymap minaid-global-map
  :doc "Global keybindings managed by `minaid-mode'.")

(keymap-set minaid-global-map "C-c ." (cons "completion" minaid-completion-map))
(keymap-set minaid-global-map "C-c C-;" (cons "consult" minaid-consult-map))
(keymap-set minaid-global-map "M-/" #'dabbrev-completion)
(keymap-set minaid-global-map "C-M-/" #'dabbrev-expand)
(keymap-set minaid-global-map "C-<tab>" #'completion-at-point)
(keymap-set minaid-global-map "C-." 'embark-act)
(keymap-set minaid-global-map "C-c H" 'embark-bindings)
(keymap-set minaid-global-map "M-;" 'embark-dwim)
(keymap-set minaid-global-map "<remap> <Info-search>" 'consult-info)
(keymap-set minaid-global-map "C-x :" 'consult-complex-command)
(keymap-set minaid-global-map "C-x b" 'consult-buffer)
(keymap-set minaid-global-map "C-x 4 b" 'consult-buffer-other-window)
(keymap-set minaid-global-map "C-x 5 b" 'consult-buffer-other-frame)
(keymap-set minaid-global-map "C-x t b" 'consult-buffer-other-tab)
(keymap-set minaid-global-map "C-x r b" 'consult-bookmark)
(keymap-set minaid-global-map "C-x p b" 'consult-project-buffer)
(keymap-set minaid-global-map "C-x C-r" 'consult-recent-file)
(keymap-set minaid-global-map "C-c C-x r r" 'consult-register)
(keymap-set minaid-global-map "M-'" 'consult-register-store)
(keymap-set minaid-global-map "M-#" 'consult-register-load)
(keymap-set minaid-global-map "C-M-#" 'consult-register)
(keymap-set minaid-global-map "M-y" 'consult-yank-from-kill-ring)
(keymap-set minaid-global-map "C-M-I" 'consult-imenu-multi)
(keymap-set minaid-global-map "M-g e" 'consult-compile-error)
(keymap-set minaid-global-map "M-g g" 'consult-goto-line)
(keymap-set minaid-global-map "M-g M-g" 'consult-goto-line)
(keymap-set minaid-global-map "M-g m" 'consult-mark)
(keymap-set minaid-global-map "M-g k" 'consult-global-mark)
(keymap-set minaid-global-map "M-g i" 'consult-imenu)
(keymap-set minaid-global-map "M-g s" 'consult-line)
(keymap-set minaid-global-map "M-g r" #'grepx-live)
(keymap-set minaid-global-map "M-i" 'consult-imenu)
(keymap-set minaid-global-map "M-s r" #'grepx-live)
(keymap-set minaid-global-map "M-s g" #'grepx-project)
(keymap-set minaid-global-map "M-s d" #'grepx-directory)
(keymap-set minaid-global-map "M-s f" #'minaid-consult-find-at-point)
(keymap-set minaid-global-map "M-s c" 'consult-locate)
(keymap-set minaid-global-map "M-s e" 'consult-isearch-history)
(keymap-set minaid-global-map "M-s l" 'consult-line)
(keymap-set minaid-global-map "M-s L" 'consult-line-multi)
(keymap-set minaid-global-map "M-s m" #'grepx-menu)

;; Isearch integration (built-in map)
(keymap-set isearch-mode-map "M-e" #'consult-isearch-history)
(keymap-set isearch-mode-map "M-s e" #'consult-isearch-history)
(keymap-set isearch-mode-map "M-s l" #'consult-line)
(keymap-set isearch-mode-map "M-s L" #'consult-line-multi)

;; Minibuffer integration (built-in map), installed by `minaid-mode' only.
(defconst minaid-minibuffer-bindings
  '(("M-s" . consult-history)
    ("M-r" . consult-history)
    ("C-c a" . marginalia-cycle)
    ("C-g" . minibuffer-keyboard-quit)
    ("C-l" . backward-kill-word))
  "Alist of key and command installed in `minibuffer-local-map' by `minaid-mode'.")

(defvar minaid--saved-minibuffer-bindings nil
  "Alist of key to prior `minibuffer-local-map' definition (or `none').
Non-nil only while `minaid-mode' owns the minibuffer bindings.")

;; State tracking for clean mode reversals
(defvar minaid--saved-completion-styles nil
  "Saved `completion-styles' prior to enabling `minaid-mode'.")

(defvar minaid--saved-category-overrides nil
  "Saved `completion-category-overrides' prior to enabling `minaid-mode'.")

(defvar minaid--saved-xref-show-xrefs-function nil
  "Saved `xref-show-xrefs-function' prior to enabling `minaid-mode'.")

(defvar minaid--saved-xref-show-definitions-function nil
  "Saved `xref-show-definitions-function' prior to enabling `minaid-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cape Capf Infrastructure & Composition

(defun minaid-cape-capf-wrapper (wrapper inner)
  "Wrap INNER Capf using Cape WRAPPER function without defining synthetic symbols."
  (funcall wrapper inner))

(defun minaid--word-capfs ()
  "Return available word completion backends wrapped for comments and strings."
  (when (and (boundp 'cape-dict-file) (file-exists-p cape-dict-file))
    (list (cape-wrap-inside-comment #'cape-dict)
          (cape-wrap-inside-string #'cape-dict))))

(defun minaid-prog-mode-capf-setup ()
  "Configure buffer-local `completion-at-point-functions' for programming modes."
  (setq-local completion-at-point-functions
              (thread-last
                (list #'tempel-complete
                      #'cape-file
                      #'cape-rfc1345
                      #'cape-emoji
                      #'cape-dabbrev
                      (and (boundp 'cape-dict-file) (file-exists-p cape-dict-file) #'cape-dict))
                (seq-filter #'identity))))

(defun minaid-text-mode-capf-setup ()
  "Configure buffer-local `completion-at-point-functions' for text modes."
  (setq-local completion-at-point-functions
              (thread-last
                (list #'tempel-complete
                      #'cape-rfc1345
                      #'cape-emoji
                      #'cape-file
                      #'cape-dabbrev
                      (and (boundp 'cape-dict-file) (file-exists-p cape-dict-file) #'cape-dict))
                (seq-filter #'identity))))

(defun minaid-elisp-capf-setup ()
  "Configure buffer-local `completion-at-point-functions' for Emacs Lisp mode."
  (setq-local completion-at-point-functions
              (thread-last
                (list #'cape-elisp-symbol
                      (cape-wrap-inside-code #'cape-elisp-block)
                      #'tempel-complete
                      #'cape-dabbrev
                      (cape-wrap-inside-code #'cape-keyword)
                      (minaid--word-capfs)
                      #'cape-emoji)
                (flatten-tree)
                (seq-filter #'identity)
                (seq-uniq))))

(defun minaid--tempel-legend-update ()
  "Display Tempel field bindings until the active template ends."
  (if (and (bound-and-true-p minaid-mode)
           (bound-and-true-p tempel--active)
           (bound-and-true-p which-key-mode)
           (fboundp 'which-key-show-keymap))
      (unless (and (fboundp 'which-key--popup-showing-p)
                   (which-key--popup-showing-p))
        (which-key-show-keymap 'tempel-map t))
    (remove-hook 'post-command-hook #'minaid--tempel-legend-update t)))

(defun minaid--tempel-legend-start (&rest _args)
  "Start showing the Tempel field legend after template insertion."
  (when (and (bound-and-true-p minaid-mode)
             (bound-and-true-p tempel--active)
             (bound-and-true-p which-key-mode)
             (fboundp 'which-key-show-keymap))
    (add-hook 'post-command-hook #'minaid--tempel-legend-update nil t)
    (minaid--tempel-legend-update)))

(defun minaid-eglot-capf-setup ()
  "Configure Capf for Eglot-managed buffers."
  (setq-local completion-category-defaults nil)
  (setq-local completion-at-point-functions
              (thread-last
                (list (cape-capf-super #'tempel-complete
                                       (cape-capf-buster #'eglot-completion-at-point))
                      (minaid--word-capfs)
                      #'tempel-complete
                      #'cape-dabbrev
                      #'cape-emoji
                      #'cape-file)
                (flatten-tree)
                (seq-filter #'identity)
                (seq-uniq))))

;;;###autoload
(defun minaid-setup-capfs ()
  "Configure local `completion-at-point-functions' for the current buffer."
  (interactive)
  (cond
   ((bound-and-true-p eglot--managed-mode) (minaid-eglot-capf-setup))
   ((derived-mode-p 'emacs-lisp-mode) (minaid-elisp-capf-setup))
   ((derived-mode-p 'prog-mode) (minaid-prog-mode-capf-setup))
   ((derived-mode-p 'text-mode 'telega-chat-mode) (minaid-text-mode-capf-setup))
   ((not (buffer-file-name)) (user-error "Buffer '%s' is not visiting a file" (buffer-name)))
   (t (message "No Capf configuration for %s [%s]: %s"
               (buffer-name) major-mode completion-at-point-functions))))

;;;###autoload
(defun minaid-setup-all-capfs ()
  "Set local `completion-at-point-functions' for all open file-visiting buffers."
  (interactive)
  (thread-last (buffer-list)
               (seq-filter #'buffer-file-name)
               (seq-map (lambda (buf) (with-current-buffer buf (minaid-setup-capfs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Corfu In-Buffer Completion

(defun minaid-corfu-text-mode-setup ()
  "Enable `corfu-mode' in text buffers with appropriate prefix length."
  (corfu-mode 1)
  (setq-local corfu-auto-prefix minaid-corfu-text-auto-prefix))

(defun minaid-corfu-prog-mode-setup ()
  "Enable `corfu-mode' in programming and shell buffers."
  (corfu-mode 1)
  (setq-local corfu-auto-prefix minaid-corfu-prog-auto-prefix))

(defun minaid-corfu-agent-shell-setup ()
  "Configure Corfu for `agent-shell-mode' buffers.
Ensures prompt typing is not hijacked by ambient auto-completion while
preserving explicit `@` file, `/` command, and TAB completion."
  (corfu-mode 1)
  (setq-local corfu-auto nil)
  (setq-local corfu-quit-no-match t)
  (setq-local corfu-quit-at-boundary t))

;;;###autoload
(defun minaid-corfu-at-point ()
  "Run `completion-at-point' forcing Corfu in-region completion UI."
  (interactive)
  (let ((completion-in-region-function #'corfu--in-region))
    (completion-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Consult Custom Commands & Project Helpers

;;;###autoload
(defun minaid-consult-at-point ()
  "Run `completion-at-point' forcing Consult in-region completion UI."
  (interactive)
  (let ((completion-in-region-function #'consult-completion-in-region))
    (completion-at-point)))

;;;###autoload
(defun minaid-open-custom-template-file ()
  "Open the user Tempel templates file."
  (interactive)
  (find-file minaid-tempel-directory))

(defun minaid--project-root (&optional may-prompt)
  "Return the project root directory.
If MAY-PROMPT is non-nil and no project is found, prompt using ACR.
Prefers Projectile root, falling back to `project.el' and `default-directory'."
  (or (when (fboundp 'projectile-project-root)
        (projectile-project-root))
      (when-let* ((pr (and (fboundp 'project-current) (project-current))))
        (if (fboundp 'project-root)
            (project-root pr)
          (with-no-warnings (car (project-roots pr)))))
      (if (and may-prompt (fboundp 'annotated-completing-read-directory))
          (annotated-completing-read-directory :prompt "Project directory: ")
        default-directory)))

;;;###autoload
(defun minaid-consult-find-at-point (&optional dir initial)
  "Run `consult-find' in DIR, defaulting INITIAL input to symbol/filename at point."
  (interactive)
  (let ((init (or initial
                  (thing-at-point 'existing-filename)
                  (thing-at-point 'filename)
                  "./")))
    (consult-find dir init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration & Keymap Wiring Helpers

(defun minaid--configure-vertico ()
  "Configure Vertico variables and multiform rules."
  (setq vertico-resize t)
  (setq vertico-count minaid-vertico-count)
  (setq vertico-cycle t)
  (add-to-list 'vertico-multiform-commands '(yank (vertico-sort-function . nil) (vertico-sort-override-function . nil)))
  (add-to-list 'vertico-multiform-commands '(yank-from-kill-ring (vertico-sort-function . nil) (vertico-sort-override-function . nil)))
  (add-to-list 'vertico-multiform-commands '(consult-yank-from-kill-ring (vertico-sort-function . nil) (vertico-sort-override-function . nil)))
  (add-to-list 'vertico-multiform-commands '(consult-yank-pop (vertico-sort-function . nil) (vertico-sort-override-function . nil)))
  (add-to-list 'vertico-multiform-commands '("\\`execute-extended-command" (vertico-flat-annotate . t) (marginalia-annotators (command marginalia-annotate-command marginalia-annotate-binding))))
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

(defun minaid--configure-orderless ()
  "Configure Orderless completion styles and category overrides."
  (setq minaid--saved-completion-styles (or minaid--saved-completion-styles completion-styles 'empty))
  (setq minaid--saved-category-overrides (or minaid--saved-category-overrides completion-category-overrides 'empty))
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles orderless basic partial-completion))
                                        (consult-grep (styles basic))
                                        (buffer (styles orderless basic))
                                        (command (styles orderless basic))
                                        (symbol (styles orderless basic))))
  (setq completion-preview-sort-function nil)
  (setq orderless-matching-styles '(orderless-literal orderless-prefixes orderless-initialism orderless-regexp)))

(defun minaid--configure-corfu ()
  "Configure Corfu variables and sub-modes."
  (setq corfu-cycle t)
  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match t)
  (setq corfu-preview-current t)
  (setq corfu-preselect nil)
  (setq corfu-on-exact-match nil)
  (setq corfu-auto t)
  (setq corfu-auto-delay minaid-corfu-auto-delay)
  (setq corfu-auto-prefix minaid-corfu-prog-auto-prefix)
  (setq corfu-popupinfo-delay 0.125)
  (setq corfu-indexed-start 1)
  (setq global-corfu-minibuffer nil)
  (add-hook 'corfu-mode-hook #'corfu-history-mode)
  (add-hook 'corfu-mode-hook #'corfu-indexed-mode)
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
  (when (fboundp 'nerd-icons-corfu-formatter)
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
  (keymap-set corfu-map "C-<tab>" #'corfu-quick-complete)
  (keymap-set corfu-map "<tab>" #'corfu-complete)
  (keymap-set corfu-map "<return>" #'corfu-insert)
  (keymap-set corfu-map "C-<return>" #'corfu-quick-insert)
  (keymap-set corfu-map "M-SPC" #'corfu-insert-separator)
  (keymap-set corfu-map "C-SPC" #'corfu-insert-separator)
  (keymap-set corfu-map "<backtab>" #'corfu-insert-separator)
  (keymap-set corfu-map "C-j" #'corfu-quick-jump)
  (keymap-set corfu-map "M-d" #'corfu-popupinfo-toggle))

(defun minaid--configure-embark ()
  "Configure Embark prompter and buffer display rules."
  (setq embark-prompter 'embark-keymap-prompter)
  (setq embark-quit-after-action '((kill-buffer . nil)))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(defun minaid--configure-consult ()
  "Configure Consult variables, xref hooks, and deferred customizations."
  (setq register-preview-delay 0.1)
  (setq minaid--saved-xref-show-xrefs-function
        (or minaid--saved-xref-show-xrefs-function
            (and (boundp 'xref-show-xrefs-function) xref-show-xrefs-function)))
  (setq minaid--saved-xref-show-definitions-function
        (or minaid--saved-xref-show-definitions-function
            (and (boundp 'xref-show-definitions-function) xref-show-definitions-function)))
  (setq xref-show-xrefs-function 'consult-xref)
  (setq xref-show-definitions-function 'consult-xref)
  (setq consult-narrow-key "C-+")
  (setq consult-async-split-style 'semicolon)
  (setq consult-async-min-input 2)
  (setq consult-async-input-debounce 0.05)
  (setq consult-async-input-throttle 0.125)
  (setq consult-async-refresh-delay 0.05)
  (setq consult-project-function #'minaid--project-root)
  (setq consult-preview-key 'any)
  (advice-add 'register-preview :override 'consult-register-window))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode Lifecycle (Enablement / Teardown)

(defvar minaid--saved-minibuffer-settings nil
  "Alist of (SYMBOL . (BOUNDP . VALUE)) saved before enabling `minaid-mode'.")

(defvar dabbrev-ignored-buffer-regexps)
(defvar dabbrev-ignored-buffer-modes)

(defvar minaid--saved-dabbrev-settings nil
  "Saved dabbrev ignore lists before enabling `minaid-mode'.")

(defun minaid--configure-dabbrev ()
  "Configure Dabbrev buffer and mode ignore filters."
  (require 'dabbrev)
  (unless minaid--saved-dabbrev-settings
    (setq minaid--saved-dabbrev-settings
          (list (copy-sequence dabbrev-ignored-buffer-regexps)
                (copy-sequence dabbrev-ignored-buffer-modes)))
    (dolist (re minaid-dabbrev-ignored-buffer-regexps)
      (add-to-list 'dabbrev-ignored-buffer-regexps re))
    (dolist (mode minaid-dabbrev-ignored-buffer-modes)
      (add-to-list 'dabbrev-ignored-buffer-modes mode))))

(defun minaid--restore-dabbrev ()
  "Restore Dabbrev buffer and mode ignore filters."
  (when minaid--saved-dabbrev-settings
    (setq dabbrev-ignored-buffer-regexps (nth 0 minaid--saved-dabbrev-settings)
          dabbrev-ignored-buffer-modes (nth 1 minaid--saved-dabbrev-settings))
    (setq minaid--saved-dabbrev-settings nil)))

(defvar minaid-abbrev-files-cache (make-hash-table :test #'equal)
  "Cache mapping file paths to modification times to avoid redundant loading.")

(defun minaid-should-read-abbrev-file-p (path)
  "Return non-nil if abbrev file at PATH needs to be (re-)loaded."
  (when-let* ((attrs (file-attributes path))
              (mtime (file-attribute-modification-time attrs)))
    (or (not (map-contains-key minaid-abbrev-files-cache path))
        (time-less-p (map-elt minaid-abbrev-files-cache path) mtime))))

;;;###autoload
(defun minaid-load-abbrev-files (&optional directory)
  "Load all abbrev definitions from DIRECTORY (`minaid-abbrev-directory').
Only loads files whose modification time is newer than cache."
  (interactive)
  (let ((dir (or directory minaid-abbrev-directory)))
    (when (and dir (file-directory-p dir))
      (thread-last
        (directory-files dir t "\\.el\\(?:\\.gz\\)?\\'" :nosort)
        (seq-filter #'minaid-should-read-abbrev-file-p)
        (seq-map (lambda (path)
                   (let ((quietly t))
                     (read-abbrev-file path quietly)
                     path)))
        (mapc (lambda (it)
                (when-let* ((attrs (file-attributes it)))
                  (setf (map-elt minaid-abbrev-files-cache it)
                        (file-attribute-modification-time attrs)))))))))

(defun minaid--kill-transform-reject-empty (string)
  "Transform STRING for `kill-transform-function', rejecting empty strings.
Returns STRING when non-empty, or nil to discard it from the kill ring."
  (unless (string-empty-p string)
    string))

(defun minaid--configure-minibuffer ()
  "Configure standard Emacs minibuffer, completion, and kill-ring settings."
  (unless minaid--saved-minibuffer-settings
    (let ((settings `((enable-recursive-minibuffers . ,minaid-enable-recursive-minibuffers)
                      (minibuffer-prompt-properties . ,minaid-minibuffer-prompt-properties)
                      (text-mode-ispell-word-completion . ,minaid-text-mode-ispell-word-completion)
                      (completion-cycle-threshold . ,minaid-completion-cycle-threshold)
                      (completion-ignore-case . ,minaid-completion-ignore-case)
                      (read-buffer-completion-ignore-case . ,minaid-read-buffer-completion-ignore-case)
                      (read-file-name-completion-ignore-case . ,minaid-read-file-name-completion-ignore-case)
                      (read-extended-command-predicate . ,minaid-read-extended-command-predicate))))
      (dolist (pair settings)
        (let ((sym (car pair))
              (val (cdr pair)))
          (push (cons sym (if (boundp sym) (cons t (symbol-value sym)) (cons nil nil)))
                minaid--saved-minibuffer-settings)
          (set sym val))))
    (when minaid-reject-empty-kill-ring
      (push (cons 'kill-transform-function
                  (if (boundp 'kill-transform-function)
                      (cons t kill-transform-function)
                    (cons nil nil)))
            minaid--saved-minibuffer-settings)
      (let ((orig (and (boundp 'kill-transform-function) kill-transform-function)))
        (setq kill-transform-function
              (if orig
                  (lambda (str)
                    (when-let* ((s (funcall orig str)))
                      (minaid--kill-transform-reject-empty s)))
                #'minaid--kill-transform-reject-empty))))))

(defun minaid--restore-minibuffer ()
  "Restore standard Emacs minibuffer, completion, and kill-ring settings."
  (dolist (entry minaid--saved-minibuffer-settings)
    (let ((sym (car entry))
          (status (cdr entry)))
      (if (car status)
          (set sym (cdr status))
        (makunbound sym))))
  (setq minaid--saved-minibuffer-settings nil))

(defun minaid--install-minibuffer-bindings ()
  "Bind `minaid-minibuffer-bindings', saving prior definitions once."
  (unless minaid--saved-minibuffer-bindings
    (setq minaid--saved-minibuffer-bindings
          (seq-map (lambda (binding)
                     (cons (car binding)
                           (or (keymap-lookup minibuffer-local-map (car binding)) 'none)))
                   minaid-minibuffer-bindings))
    (seq-do (lambda (binding)
              (keymap-set minibuffer-local-map (car binding) (cdr binding)))
            minaid-minibuffer-bindings)))

(defun minaid--restore-minibuffer-bindings ()
  "Restore the `minibuffer-local-map' definitions saved on enable."
  (seq-do (lambda (saved)
            (if (eq (cdr saved) 'none)
                (keymap-unset minibuffer-local-map (car saved) t)
              (keymap-set minibuffer-local-map (car saved) (cdr saved))))
          minaid--saved-minibuffer-bindings)
  (setq minaid--saved-minibuffer-bindings nil))

(defvar minaid--xref-icons-prior nil
  "Previous xref icon mode state, wrapped in a list when managed.")

(defvar minaid--xref-icons-last nil
  "Last xref icon mode state applied by Minaid.")

(defun minaid--configure-anzu-state ()
  "Keep Anzu's search caches separate between buffers."
  (seq-do #'make-variable-buffer-local
          '(anzu--total-matched anzu--current-position anzu--state
            anzu--cached-count anzu--cached-positions anzu--last-command
            anzu--last-isearch-string anzu--last-search-state anzu--overflow-p)))

(when (bound-and-true-p minaid-mode)
  (minaid--configure-anzu-state))

(defun minaid--isearch-enable-anzu ()
  "Enable Anzu for the current search when it is available."
  (if (fboundp 'anzu-mode)
      (anzu-mode 1)
    (message "Minaid: Anzu is not installed; search match counts are unavailable")))

(defun minaid--sync-nerd-icons-xref ()
  "Synchronize xref icons with `nerd-icons-completion-mode'."
  (when (and (boundp 'nerd-icons-completion-mode)
             (fboundp 'nerd-icons-xref-mode))
    (unless minaid--xref-icons-prior
      (setq minaid--xref-icons-prior (list (bound-and-true-p nerd-icons-xref-mode))))
    (setq minaid--xref-icons-last (and nerd-icons-completion-mode t))
    (nerd-icons-xref-mode (if minaid--xref-icons-last 1 -1))))

(defun minaid--restore-nerd-icons-xref ()
  "Restore xref icon state unless it was independently changed."
  (when (and minaid--xref-icons-prior
             (fboundp 'nerd-icons-xref-mode)
             (eq (and (bound-and-true-p nerd-icons-xref-mode) t)
                 minaid--xref-icons-last))
    (nerd-icons-xref-mode (if (car minaid--xref-icons-prior) 1 -1)))
  (setq minaid--xref-icons-prior nil)
  (setq minaid--xref-icons-last nil))

(defun minaid--enable ()
  "Perform enablement of all minad completion components."
  (minaid--install-minibuffer-bindings)
  (minaid--configure-minibuffer)
  (minaid--configure-dabbrev)
  (minaid--configure-vertico)
  (minaid--configure-orderless)
  (minaid--configure-corfu)
  (minaid--configure-embark)
  (minaid--configure-consult)

  ;; Activate Vertico, Marginalia, & Corfu
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (marginalia-mode 1)
  (global-corfu-mode 1)

  ;; Search and xref integrations
  (minaid--configure-anzu-state)
  (add-hook 'isearch-mode-hook #'minaid--isearch-enable-anzu)
  (add-hook 'nerd-icons-completion-mode-hook #'minaid--sync-nerd-icons-xref)
  (minaid--sync-nerd-icons-xref)

  ;; Capf major mode hooks
  (advice-add 'tempel--insert :after #'minaid--tempel-legend-start)
  (add-hook 'eglot-managed-mode-hook #'minaid-eglot-capf-setup)
  (add-hook 'emacs-lisp-mode-hook #'minaid-elisp-capf-setup)
  (add-hook 'telega-chat-mode-hook #'minaid-text-mode-capf-setup)
  (add-hook 'text-mode-hook #'minaid-text-mode-capf-setup)
  (add-hook 'prog-mode-hook #'minaid-prog-mode-capf-setup)

  ;; Corfu buffer hooks (add-hook works even before modes load)
  (add-hook 'text-mode-hook #'minaid-corfu-text-mode-setup)
  (add-hook 'prog-mode-hook #'minaid-corfu-prog-mode-setup)
  (add-hook 'comint-mode-hook #'minaid-corfu-prog-mode-setup)
  (add-hook 'eat-mode-hook #'minaid-corfu-prog-mode-setup)
  (add-hook 'eshell-mode-hook #'minaid-corfu-prog-mode-setup)
  (add-hook 'shell-mode-hook #'minaid-corfu-prog-mode-setup)
  (add-hook 'agent-shell-mode-hook #'minaid-corfu-agent-shell-setup)
  (add-hook 'agent-shell-queue-capture-mode-hook #'minaid-corfu-text-mode-setup)

  (setq tempel-path minaid-tempel-directory)

  ;; Abbrev definitions loader
  (add-hook 'abbrev-mode-hook #'minaid-load-abbrev-files)
  (when (file-directory-p minaid-abbrev-directory)
    (minaid-load-abbrev-files))

  ;; Marginalia command category overrides
  (add-to-list 'marginalia-command-categories '(consult-completion-in-region . imenu)))

(defun minaid--disable ()
  "Tear down all minad completion components and restore original state."
  (minaid--restore-minibuffer-bindings)
  (minaid--restore-minibuffer)
  (minaid--restore-dabbrev)
  (remove-hook 'abbrev-mode-hook #'minaid-load-abbrev-files)

  (remove-hook 'isearch-mode-hook #'minaid--isearch-enable-anzu)
  (remove-hook 'nerd-icons-completion-mode-hook #'minaid--sync-nerd-icons-xref)
  (minaid--restore-nerd-icons-xref)

  ;; Remove Consult advice
  (advice-remove 'register-preview 'consult-register-window)

  ;; Deactivate Vertico, Marginalia, & Corfu
  (vertico-mode -1)
  (vertico-multiform-mode -1)
  (marginalia-mode -1)
  (global-corfu-mode -1)

  ;; Restore completion styles
  (when minaid--saved-completion-styles
    (setq completion-styles (if (eq minaid--saved-completion-styles 'empty) nil minaid--saved-completion-styles))
    (setq minaid--saved-completion-styles nil))
  (when minaid--saved-category-overrides
    (setq completion-category-overrides (if (eq minaid--saved-category-overrides 'empty) nil minaid--saved-category-overrides))
    (setq minaid--saved-category-overrides nil))

  ;; Restore Xref functions
  (when (boundp 'xref-show-xrefs-function)
    (setq xref-show-xrefs-function minaid--saved-xref-show-xrefs-function)
    (setq minaid--saved-xref-show-xrefs-function nil))
  (when (boundp 'xref-show-definitions-function)
    (setq xref-show-definitions-function minaid--saved-xref-show-definitions-function)
    (setq minaid--saved-xref-show-definitions-function nil))

  ;; Remove Capf hooks
  (advice-remove 'tempel--insert #'minaid--tempel-legend-start)
  (remove-hook 'eglot-managed-mode-hook #'minaid-eglot-capf-setup)
  (remove-hook 'emacs-lisp-mode-hook #'minaid-elisp-capf-setup)
  (remove-hook 'telega-chat-mode-hook #'minaid-text-mode-capf-setup)
  (remove-hook 'text-mode-hook #'minaid-text-mode-capf-setup)
  (remove-hook 'prog-mode-hook #'minaid-prog-mode-capf-setup)

  ;; Remove Corfu sub-mode hooks
  (remove-hook 'corfu-mode-hook #'corfu-history-mode)
  (remove-hook 'corfu-mode-hook #'corfu-indexed-mode)
  (remove-hook 'corfu-mode-hook #'corfu-popupinfo-mode)

  ;; Remove Corfu buffer hooks
  (remove-hook 'text-mode-hook #'minaid-corfu-text-mode-setup)
  (remove-hook 'prog-mode-hook #'minaid-corfu-prog-mode-setup)
  (remove-hook 'comint-mode-hook #'minaid-corfu-prog-mode-setup)
  (remove-hook 'eat-mode-hook #'minaid-corfu-prog-mode-setup)
  (remove-hook 'eshell-mode-hook #'minaid-corfu-prog-mode-setup)
  (remove-hook 'shell-mode-hook #'minaid-corfu-prog-mode-setup)
  (remove-hook 'agent-shell-mode-hook #'minaid-corfu-agent-shell-setup)
  (remove-hook 'agent-shell-queue-capture-mode-hook #'minaid-corfu-text-mode-setup)
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (bound-and-true-p corfu-mode)
          (corfu-mode -1))))))

;;;###autoload
(define-minor-mode minaid-mode
  "Global minor mode coordinating the consolidated Minad completion suite.
Activates and manages Vertico, Marginalia, Orderless, Corfu, Cape, Consult,
and Embark integrations."
  :global t
  :keymap minaid-global-map
  :group 'minaid
  :lighter " minaid"
  (if minaid-mode
      (minaid--enable)
    (minaid--disable)))

(provide 'minaid)
;;; minaid.el ends here
