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
(require 'builder-core)

(declare-function alert "alert" (message &rest args))

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
  (let* ((results (builder--read-command command))
         (candidate (car results))
         (cmd (builder-candidate-command candidate)))
    (read-from-minibuffer "edit command => " cmd)))

;;;###autoload
(defun builder-compile ()
  "Run interactive compile operation via `builder-compile-project'."
  (interactive)
  (builder-compile-project))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minor mode definition

(defvar builder-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `builder-mode'.")

(defvar builder--saved-scroll-output nil)
(defvar builder--saved-max-output-line-length nil)

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
        (setq builder--saved-scroll-output compilation-scroll-output
              builder--saved-max-output-line-length compilation-max-output-line-length)
        (setq compilation-scroll-output t
              compilation-max-output-line-length nil)

        ;; 5. Keybindings in compilation-mode-map
        (when (boundp 'compilation-mode-map)
          (keymap-set compilation-mode-map "d" #'builder-change-directory)
          (keymap-set compilation-mode-map "C" #'compile))

        ;; 6. Advice on compilation-read-command
        (when builder-mode-override-read-command
          (advice-add 'compilation-read-command :override #'builder-compilation-read-command)))

    ;; Disabling mode
    (when builder-mode-enable-colorization
      (remove-hook 'compilation-filter-hook #'builder-colorize-compilation-buffer))
    (when builder-mode-enable-finish-alerts
      (remove-hook 'compilation-finish-functions #'builder-alert-after-finish-in-background))
    (when builder-mode-override-read-command
      (advice-remove 'compilation-read-command #'builder-compilation-read-command))
    (setq compilation-scroll-output builder--saved-scroll-output
          compilation-max-output-line-length builder--saved-max-output-line-length)))

(provide 'builder-mode)
;;; builder-mode.el ends here
