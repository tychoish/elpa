;;; agent-shell-notifications-alert.el --- alert.el provider for agent-shell-notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026 sam Kleinman

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; alert.el provider for agent-shell-notifications.

;;; Code:

(require 'alert)

(defvar agent-shell-notifications-send-function)
(defvar agent-shell-notifications-close-function)
(defvar agent-shell-notifications-transform-function)
(defvar agent-shell-notifications-transform-timeout-function)

(defun agent-shell-notifications-alert-send (plist)
  "Send agent-shell notification PLIST through `alert'.
The originating shell buffer name is used as the notification subject
\(:title\) and the message is placed in the body.  The plist's
`:timeout' is bound to `alert-fade-time' when positive so the configured
`agent-shell-notifications-timeout' controls how long notifications stay
visible for alert styles that honor it."
  (let* ((title (plist-get plist :title))
	 (body (plist-get plist :body))
	 (icon (plist-get plist :app-icon))
	 (buf-name (plist-get plist :shell-buffer-name))
	 (agent-name (plist-get plist :agent-name))
	 (timeout (plist-get plist :timeout))
	 ;; alert-fade-type is a dynamic variable that's captured
	 (alert-fade-time (if (and (numberp timeout) (> timeout 0))
			      timeout
			    alert-fade-time))
	 (effective-title (or title
			      (if (and agent-name (not (string-empty-p agent-name)))
				  (format "%s: Ready" agent-name)
				"agent-shell")))
	 (has-buf (and buf-name (not (string-empty-p buf-name))))
	 (subject (if has-buf buf-name (or effective-title "agent-shell")))
	 (message-body (if has-buf
			   (cond
			    ((and effective-title body (not (string-empty-p body)))
			     (format "%s: %s" effective-title body))
			    ((and body (not (string-empty-p body)))
			     body)
			    (t
			     (or effective-title "")))
			 (or body ""))))
    (alert message-body
	   :title subject
	   :icon icon
	   :category 'agent-shell
	   :severity 'normal))
  nil)

(defun agent-shell-notifications-alert-close (_id)
  "No-op close: `alert' styles dismiss themselves." nil)

;;;###autoload
(defun agent-shell-notifications-alert-setup ()
  "Configure `agent-shell-notifications' to use `alert.el'."
  (setq agent-shell-notifications-send-function #'agent-shell-notifications-alert-send)
  (setq agent-shell-notifications-close-function #'agent-shell-notifications-alert-close)
  (setq agent-shell-notifications-transform-function #'identity)
  (setq agent-shell-notifications-transform-timeout-function #'identity))

(agent-shell-notifications-alert-setup)

(provide 'agent-shell-notifications-alert)

;;; agent-shell-notifications-alert.el ends here
