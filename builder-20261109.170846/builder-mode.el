;;; builder-mode.el --- Global minor mode and compilation-mode integration -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, compilation

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Provides `builder-mode', a global minor mode that manages compilation
;; buffer error syntax, ANSI colorization, finish notifications, and
;; keyboard shortcuts.

;;; Code:

(require 'compile)
(require 'ansi-color)
(require 'seq)

(autoload 'cov-mode "cov" nil t)
(autoload 'builder-go-coverage-and-convert "builder-tasks" nil t)

(declare-function alert "alert" (message &rest args))
(declare-function builder--read-command "builder-core" (command))
(declare-function builder-candidate-command "builder-core" (candidate))
(declare-function builder-compile-project "builder-core" ())
(declare-function builder-change-directory "builder-core" ())
(declare-function builder-elisp-mode "builder-elisp" (&optional arg))
(defvar builder-mode)
(defvar go-ts-mode-map)
(defvar cov-coverage-mode)
(defvar cov-lcov-file-name)
(defvar cov-coverage-file-paths)
(defvar byte-compile-warnings)
(defvar checkdoc-force-docstrings-flag)
(defvar checkdoc-spellcheck-documentation-flag)
(defvar builder-elisp-byte-compile-warnings)
(defvar builder-elisp-checkdoc-force-docstrings-flag)
(defvar builder-elisp-checkdoc-spellcheck-documentation-flag)

(defgroup builder-mode nil
  "Compilation enhancements and global minor mode."
  :group 'builder
  :prefix "builder-")

(defcustom builder-mode-enable-colorization t
  "Whether `builder-mode' applies ANSI colorization to compilation buffers."
  :type 'boolean
  :group 'builder-mode)

(defcustom builder-mode-enable-finish-alerts t
  "Whether `builder-mode' fires background alert notifications on finish."
  :type 'boolean
  :group 'builder-mode)

(defcustom builder-mode-override-read-command t
  "Whether `builder-mode' advises `compilation-read-command' to use builder selection."
  :type 'boolean
  :group 'builder-mode)

(defcustom builder-mode-enable-language-modes t
  "Whether `builder-mode' enables language-specific builder modes."
  :type 'boolean
  :group 'builder-mode)

(defcustom builder-coverage-modes '(go-ts-mode)
  "Major modes in which `builder-mode' enables `cov-mode'.
Set to nil to disable automatic activation.  Changes take effect when
`builder-mode' is next enabled."
  :type '(repeat symbol)
  :group 'builder-mode)

(defcustom builder-coverage-display 'both
  "Value of `cov-coverage-mode' while `builder-mode' is enabled."
  :type '(choice (const :tag "Both (when supported by cov)" both) (const :tag "Coverage" t)
                 (const :tag "Frequency" nil))
  :group 'builder-mode)

(defcustom builder-coverage-lcov-file "coverage/lcov.info"
  "Value of `cov-lcov-file-name' while `builder-mode' is enabled."
  :type '(choice (const nil) string)
  :group 'builder-mode)

(defcustom builder-coverage-search-paths '("cover" "coverage" ".")
  "Value of `cov-coverage-file-paths' while `builder-mode' is enabled."
  :type '(repeat (choice string function))
  :group 'builder-mode)

(defvar builder--coverage-hooks nil
  "Hooks installed by the current `builder-mode' activation.")

(defun builder--configure-cov ()
  "Save and apply Builder's coverage settings after cov loads."
  (when (and (bound-and-true-p builder-mode)
             (not (assq 'cov-coverage-mode builder-mode--saved-settings)))
    (seq-do (lambda (pair)
              (push (cons (car pair) (cons t (default-value (car pair))))
                    builder-mode--saved-settings)
              (set-default (car pair) (cdr pair)))
            `((cov-coverage-mode . ,builder-coverage-display)
              (cov-lcov-file-name . ,builder-coverage-lcov-file)
              (cov-coverage-file-paths . ,builder-coverage-search-paths)))))

(with-eval-after-load 'cov
  (builder--configure-cov))

(defvar builder--go-coverage-key-saved nil
  "Prior Go coverage key binding, wrapped in a list when installed.")

(defun builder--install-go-coverage-key ()
  "Install Builder's Go coverage key while preserving its previous binding."
  (unless builder--go-coverage-key-saved
    (condition-case err
        (let ((binding (keymap-lookup go-ts-mode-map "C-c C-t c")))
          (keymap-set go-ts-mode-map "C-c C-t c" #'builder-go-coverage-and-convert)
          (setq builder--go-coverage-key-saved
                (list (if (numberp binding) nil binding))))
      (error (message "Builder: Go coverage key skipped: %s"
                      (error-message-string err))))))

(defun builder--remove-go-coverage-key ()
  "Restore the Go coverage key if it still names Builder's command."
  (when builder--go-coverage-key-saved
    (when (eq (keymap-lookup go-ts-mode-map "C-c C-t c")
              #'builder-go-coverage-and-convert)
      (if (car builder--go-coverage-key-saved)
          (keymap-set go-ts-mode-map "C-c C-t c" (car builder--go-coverage-key-saved))
        (keymap-unset go-ts-mode-map "C-c C-t c")))
    (setq builder--go-coverage-key-saved nil)))

(with-eval-after-load 'go-ts-mode
  (when builder-mode
    (builder--install-go-coverage-key)))

(defun builder--enable-cov ()
  "Enable coverage in the current buffer when cov is installed."
  (if (locate-library "cov")
      (cov-mode 1)
    (message "Builder: cov-mode skipped because cov is not installed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; error syntax registration

;;;###autoload
(defun builder-compile-add-error-syntax (name regexp file line &optional col level)
  "Register a new compilation error syntax pattern.
NAME is a symbol naming the rule.
REGEXP is the pattern matching the error line.
FILE is the subexpression matching file path.
LINE is the subexpression matching line number.
COL and LEVEL are optional subexpressions."
  (setq compilation-error-regexp-alist-alist
        (assq-delete-all name compilation-error-regexp-alist-alist))
  (push (list name regexp file line col level) compilation-error-regexp-alist-alist)
  (add-to-list 'compilation-error-regexp-alist name))

(defalias 'compile-add-error-syntax #'builder-compile-add-error-syntax)

(defconst builder-default-error-syntax
  '((go-test . ("^[[:space:]]*\([[:alnum:]_./-]+\.go\):\([0-9]+\):" 1 2))
    (go-panic . ("^[[:space:]]*\([[:alnum:]_./-]+\.go\):\([0-9]+\):" 1 2))
    (rust-pretty-logfile . ("^\s+ at \(.*\):\([0-9]+\)" 1 2)))
  "Default error regexps registered by `builder-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation buffer enhancements

;;;###autoload
(defun builder-colorize-compilation-buffer ()
  "Apply ANSI color codes to incoming compilation output."
  (when (and (boundp 'compilation-filter-start)
             compilation-filter-start
             (<= compilation-filter-start (point)))
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(defalias 'colorize-compilation-buffer #'builder-colorize-compilation-buffer)

;;;###autoload
(defun builder-alert-after-finish-in-background (buf str)
  "Send an alert notification when BUF finishes compilation in the background.
STR is the termination status string."
  (when (and (fboundp 'alert)
             (buffer-live-p buf)
             (or (not (get-buffer-window buf 'visible))
                 (not (frame-focus-state))))
    (alert str :buffer buf :title (buffer-name buf))))

(defalias 'alert-after-finish-in-background #'builder-alert-after-finish-in-background)

;;;###autoload
(defun builder-compilation-read-command (command)
  "Advise `compilation-read-command' to prompt via builder candidate selection.
COMMAND is the default compile command string."
  (require 'builder-core)
  (let* ((results (builder--read-command command))
         (candidate (car results))
         (cmd (builder-candidate-command candidate)))
    (read-from-minibuffer "edit command => " cmd)))

;;;###autoload
(defun builder-compile ()
  "Run interactive compile operation via `builder-compile-project'."
  (interactive)
  (require 'builder-core)
  (builder-compile-project))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minor mode definition

(defvar builder-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `builder-mode'.")

(defvar builder-mode--managed-settings
  '((compilation-scroll-output . t)
    (compilation-max-output-line-length . nil))
  "Alist of (SYMBOL . VALUE) settings managed globally by `builder-mode'.")

(defvar builder-mode--saved-settings nil
  "Alist of (SYMBOL . (BOUNDP . VALUE)) saved before enabling `builder-mode'.")

(defun builder-mode--apply-settings ()
  "Save previous values and apply `builder-mode--managed-settings'."
  (setq builder-mode--saved-settings nil)
  (dolist (pair builder-mode--managed-settings)
    (let ((sym (car pair))
          (val (cdr pair)))
      (push (cons sym (if (boundp sym) (cons t (symbol-value sym)) (cons nil nil)))
            builder-mode--saved-settings)
      (set sym val)))
  (when (boundp 'builder-elisp-byte-compile-warnings)
    (push (cons 'byte-compile-warnings
                (if (boundp 'byte-compile-warnings)
                    (cons t (default-value 'byte-compile-warnings))
                  (cons nil nil)))
          builder-mode--saved-settings)
    (setq-default byte-compile-warnings builder-elisp-byte-compile-warnings))
  (when (boundp 'builder-elisp-checkdoc-force-docstrings-flag)
    (push (cons 'checkdoc-force-docstrings-flag
                (if (boundp 'checkdoc-force-docstrings-flag)
                    (cons t (default-value 'checkdoc-force-docstrings-flag))
                  (cons nil nil)))
          builder-mode--saved-settings)
    (setq-default checkdoc-force-docstrings-flag builder-elisp-checkdoc-force-docstrings-flag))
  (when (boundp 'builder-elisp-checkdoc-spellcheck-documentation-flag)
    (push (cons 'checkdoc-spellcheck-documentation-flag
                (if (boundp 'checkdoc-spellcheck-documentation-flag)
                    (cons t (default-value 'checkdoc-spellcheck-documentation-flag))
                  (cons nil nil)))
          builder-mode--saved-settings)
    (setq-default checkdoc-spellcheck-documentation-flag builder-elisp-checkdoc-spellcheck-documentation-flag)))

(defun builder-mode--revert-settings ()
  "Restore variables to their state prior to `builder-mode'."
  (dolist (entry builder-mode--saved-settings)
    (let ((sym (car entry))
          (status (cdr entry)))
      (if (car status)
          (set-default sym (cdr status))
        (makunbound sym))))
  (setq builder-mode--saved-settings nil))

;;;###autoload
(define-minor-mode builder-mode
  "Global minor mode providing compilation enhancements and builder keybindings."
  :global t
  :group 'builder
  :lighter " Builder"
  :keymap builder-mode-map
  (if builder-mode
      (progn
        ;; 1. Register error regexps
        (pcase-dolist (`(,name . ,spec) builder-default-error-syntax)
          (apply #'builder-compile-add-error-syntax name spec))

        ;; 2. Colorization filter hook
        (when builder-mode-enable-colorization
          (add-hook 'compilation-filter-hook #'builder-colorize-compilation-buffer))

        ;; 3. Finish notification hook
        (when builder-mode-enable-finish-alerts
          (add-hook 'compilation-finish-functions #'builder-alert-after-finish-in-background))

        ;; 4. Output buffer scrolling and line length
        (builder-mode--apply-settings)
        (when (featurep 'cov)
          (builder--configure-cov))

        ;; 5. Keybindings in compilation-mode-map
        (when (boundp 'compilation-mode-map)
          (keymap-set compilation-mode-map "d" #'builder-change-directory)
          (keymap-set compilation-mode-map "C" #'compile))

        ;; 6. Advice on compilation-read-command
        (when builder-mode-override-read-command
          (advice-add 'compilation-read-command :override #'builder-compilation-read-command))

        ;; 7. Coverage hooks and Go command
        (when (featurep 'go-ts-mode)
          (builder--install-go-coverage-key))
        (setq builder--coverage-hooks
              (seq-map (lambda (mode) (intern (format "%s-hook" mode)))
                       builder-coverage-modes))
        (seq-do (lambda (hook) (add-hook hook #'builder--enable-cov))
                builder--coverage-hooks)

        ;; 8. Language mode hooks
        (when builder-mode-enable-language-modes
          (add-hook 'emacs-lisp-mode-hook #'builder-elisp-mode)))

    ;; Disabling mode
    (when (boundp 'go-ts-mode-map)
      (builder--remove-go-coverage-key))
    (seq-do (lambda (hook) (remove-hook hook #'builder--enable-cov))
            builder--coverage-hooks)
    (setq builder--coverage-hooks nil)
    (when builder-mode-enable-colorization
      (remove-hook 'compilation-filter-hook #'builder-colorize-compilation-buffer))
    (when builder-mode-enable-finish-alerts
      (remove-hook 'compilation-finish-functions #'builder-alert-after-finish-in-background))
    (when builder-mode-override-read-command
      (advice-remove 'compilation-read-command #'builder-compilation-read-command))
    (when builder-mode-enable-language-modes
      (remove-hook 'emacs-lisp-mode-hook #'builder-elisp-mode))
    (builder-mode--revert-settings)))

(provide 'builder-mode)
;;; builder-mode.el ends here
