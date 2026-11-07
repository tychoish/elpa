;;; cmdr-mode.el --- Global minor mode attaching the cmdr key map -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/cmdr.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Attaches and detaches `cmdr-mode-map' only.

;;; Code:

(require 'cmdr-keys)

;; Design: a standard `define-minor-mode' with :keymap.  This registers
;; `cmdr-mode-map' in `minor-mode-map-alist', so it takes precedence over the
;; global map only while the mode is on; disabling merely deactivates the map
;; and the prior (global or major-mode) bindings show through.  The map itself
;; is never copied, so re-enabling sees any changes made while disabled.

;;;###autoload
(define-minor-mode cmdr-mode
  "Attach `cmdr-mode-map' as a global minor-mode map.
Does nothing else: no display, completion, hooks, advice or frame policy."
  :global t
  :group 'convenience
  :keymap cmdr-mode-map)

(provide 'cmdr-mode)
;;; cmdr-mode.el ends here
