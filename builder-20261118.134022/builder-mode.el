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
(require 'savehist)
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
(declare-function builder-elisp-enable-ert-advice "builder-elisp" ())
(declare-function builder-elisp-disable-ert-advice "builder-elisp" ())
(defvar builder-mode)
(defvar builder-elisp-ert-prevent-focus)
(defvar go-ts-mode-map)
(defvar go-ts-mode-hook)
(defvar cov-coverage-mode)
(defvar cov-lcov-file-name)
(defvar cov-coverage-file-paths)

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

(defun builder--enable-cov ()
  "Configure coverage variables buffer-locally and enable `cov-mode'."
  (if (locate-library "cov")
      (progn
        (setq-local cov-coverage-mode builder-coverage-display)
        (setq-local cov-lcov-file-name builder-coverage-lcov-file)
        (setq-local cov-coverage-file-paths builder-coverage-search-paths)
        (cov-mode 1))
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
  (seq-do (lambda (pair)
            (let ((sym (car pair))
                  (val (cdr pair)))
              (push (cons sym (if (boundp sym) (cons t (symbol-value sym)) (cons nil nil)))
                    builder-mode--saved-settings)
              (set sym val)))
          builder-mode--managed-settings))

(defun builder-mode--revert-settings ()
  "Restore variables to their state prior to `builder-mode'."
  (seq-do (lambda (entry)
            (let ((sym (car entry))
                  (status (cdr entry)))
              (if (car status)
                  (set-default sym (cdr status))
                (makunbound sym))))
          builder-mode--saved-settings)
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
        (add-to-list 'savehist-additional-variables 'compile-history)
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

        ;; 5. Keybindings in compilation-mode-map
        (when (boundp 'compilation-mode-map)
          (keymap-set compilation-mode-map "d" #'builder-change-directory)
          (keymap-set compilation-mode-map "C" #'compile))

        ;; 6. Advice on compilation-read-command
        (when builder-mode-override-read-command
          (advice-add 'compilation-read-command :override #'builder-compilation-read-command))

        ;; 7. Coverage hooks and Go command
        (add-hook 'go-ts-mode-hook #'builder--install-go-coverage-key)
        (when (featurep 'go-ts-mode)
          (builder--install-go-coverage-key))
        (setq builder--coverage-hooks
              (seq-map (lambda (mode) (intern (format "%s-hook" mode)))
                       builder-coverage-modes))
        (seq-do (lambda (hook) (add-hook hook #'builder--enable-cov))
                builder--coverage-hooks)

        ;; 8. Language mode hooks and ERT advice
        (when builder-mode-enable-language-modes
          (add-hook 'emacs-lisp-mode-hook #'builder-elisp-mode)
          (when (bound-and-true-p builder-elisp-ert-prevent-focus)
            (builder-elisp-enable-ert-advice))))

    ;; Disabling mode
    (remove-hook 'go-ts-mode-hook #'builder--install-go-coverage-key)
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
      (remove-hook 'emacs-lisp-mode-hook #'builder-elisp-mode)
      (unless (seq-some (lambda (buf)
                          (buffer-local-value 'builder-elisp-mode buf))
                        (buffer-list))
        (builder-elisp-disable-ert-advice)))
    (builder-mode--revert-settings)))

(provide 'builder-mode)
;;; builder-mode.el ends here
