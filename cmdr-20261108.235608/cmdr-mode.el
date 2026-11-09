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

(defvar which-key-mode-hook)

;; Design: a standard `define-minor-mode' with :keymap.  This registers
;; `cmdr-mode-map' in `minor-mode-map-alist', so it takes precedence over the
;; global map only while the mode is on; disabling merely deactivates the map
;; and the prior (global or major-mode) bindings show through.  The map itself
;; is never copied, so re-enabling sees any changes made while disabled.

(defcustom cmdr-which-key-integration t
  "When non-nil, configure `which-key' side-window bottom display.
This configuration is attached to `which-key-mode-hook'."
  :type 'boolean
  :group 'convenience)

(defun cmdr-mode--setup-which-key ()
  "Configure `which-key' to display in bottom side window."
  (when (fboundp 'which-key-setup-side-window-bottom)
    (which-key-setup-side-window-bottom)))

;;;###autoload
(define-minor-mode cmdr-mode
  "Attach `cmdr-mode-map' as a global minor-mode map.
When enabled, also configure `which-key-mode-hook' to use
side-window-bottom display."
  :global t
  :group 'convenience
  :keymap cmdr-mode-map
  (if cmdr-mode
      (when cmdr-which-key-integration
        (add-hook 'which-key-mode-hook #'cmdr-mode--setup-which-key)
        (when (bound-and-true-p which-key-mode)
          (cmdr-mode--setup-which-key)))
    (remove-hook 'which-key-mode-hook #'cmdr-mode--setup-which-key)
    (when (and (boundp 'which-key-mode-hook)
               (null which-key-mode-hook)
               (not (featurep 'which-key)))
      (makunbound 'which-key-mode-hook))))

;;;###autoload
(defalias 'cmdr-global-minor-mode #'cmdr-mode)

(provide 'cmdr-mode)
;;; cmdr-mode.el ends here
