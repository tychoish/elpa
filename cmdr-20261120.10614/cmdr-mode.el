;;; cmdr-mode.el --- Global minor mode attaching the cmdr key map -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/cmdr.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Attaches and detaches `cmdr-mode-map' and coordinates global minor-mode integrations.

;;; Code:

(require 'cmdr-keys)
(require 'which-key)

(autoload 'cmdr-set-frame-font-interactive "cmdr-ops" nil t)

(defvar which-key-mode-hook)

;; Design: a standard `define-minor-mode' with :keymap.  This registers
;; `cmdr-mode-map' in `minor-mode-map-alist', so it takes precedence over the
;; global map only while the mode is on; disabling merely deactivates the map
;; and the prior (global or major-mode) bindings show through.  The map itself
;; is never copied, so re-enabling sees any changes made while disabled.

(defcustom cmdr-which-key-integration t
  "When non-nil, enable and configure `which-key' side-window bottom display.
This configuration is attached to `which-key-mode-hook'."
  :type 'boolean
  :group 'convenience)

(defcustom cmdr-mode-managed-settings
  '((which-key-idle-delay . 0.25)
    (which-key-idle-secondary-delay . 0.125)
    (which-key-lighter . ""))
  "Alist of (SYMBOL . VALUE) settings managed globally by `cmdr-mode'."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'convenience)

(defvar cmdr-mode--saved-settings nil
  "Alist of (SYMBOL . (BOUNDP . VALUE)) saved before enabling `cmdr-mode'.")

(defun cmdr-mode--apply-settings ()
  "Save previous values and apply `cmdr-mode-managed-settings'."
  (setq cmdr-mode--saved-settings nil)
  (seq-do (lambda (pair)
            (let ((sym (car pair))
                  (val (cdr pair)))
              (push (cons sym (if (boundp sym) (cons t (symbol-value sym)) (cons nil nil)))
                    cmdr-mode--saved-settings)
              (set sym val)))
          cmdr-mode-managed-settings))

(defun cmdr-mode--revert-settings ()
  "Restore variables to their state prior to `cmdr-mode'."
  (seq-do (lambda (entry)
            (let ((sym (car entry))
                  (status (cdr entry)))
              (if (car status)
                  (set-default sym (cdr status))
                (makunbound sym))))
          cmdr-mode--saved-settings)
  (setq cmdr-mode--saved-settings nil))

(defun cmdr-mode--setup-which-key ()
  "Configure `which-key' to display in bottom side window."
  (when (fboundp 'which-key-setup-side-window-bottom)
    (which-key-setup-side-window-bottom)))

;;;###autoload
(define-minor-mode cmdr-mode
  "Attach `cmdr-mode-map' as a global minor-mode map.
When enabled, also configure `which-key-mode-hook' to use
side-window-bottom display and use the cmdr interactive font prompt."
  :global t
  :group 'convenience
  :keymap cmdr-mode-map
  (if cmdr-mode
      (progn
        (advice-add 'set-frame-font :around #'cmdr-set-frame-font-interactive)
        (cmdr-mode--apply-settings)
        (when cmdr-which-key-integration
          (add-hook 'which-key-mode-hook #'cmdr-mode--setup-which-key)
          (which-key-mode 1)
          (when (bound-and-true-p which-key-mode)
            (cmdr-mode--setup-which-key))))
    (advice-remove 'set-frame-font #'cmdr-set-frame-font-interactive)
    (remove-hook 'which-key-mode-hook #'cmdr-mode--setup-which-key)
    (when (and (boundp 'which-key-mode-hook)
               (null which-key-mode-hook)
               (not (featurep 'which-key)))
      (makunbound 'which-key-mode-hook))
    (cmdr-mode--revert-settings)))

;;;###autoload
(defalias 'cmdr-global-minor-mode #'cmdr-mode)

(provide 'cmdr-mode)
;;; cmdr-mode.el ends here
