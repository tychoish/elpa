;;; agent-shell-notifications-knockknock.el --- knockknock provider for agent-shell-notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Zachary Hanham
;; Copyright (C) 2026 Alvaro Ramirez

;; Package-Requires: ((emacs "29.1") (agent-shell-notifications "0.1") (knockknock "0.3"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; knockknock provider for agent-shell-notifications.
;;
;; NOTE: This backend is experimental.  It is included primarily as a working
;; example of how to implement a custom backend.
;;
;; Based on agent-shell-knockknock by Alvaro Ramirez.
;; https://github.com/xenodium/agent-shell-knockknock

;;; Code:

(require 'knockknock)

(defvar agent-shell-notifications--knockknock-ret-timer nil
  "Timer controlling the transient RET binding for knockknock.")

(defun agent-shell-notifications--knockknock-install-ret-binding (on-action)
  "Install a transient RET binding that calls ON-ACTION.
The binding auto-removes after `knockknock-default-duration' seconds."
  (when agent-shell-notifications--knockknock-ret-timer
    (cancel-timer agent-shell-notifications--knockknock-ret-timer))
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
                (lambda ()
                  (interactive)
                  (when agent-shell-notifications--knockknock-ret-timer
                    (cancel-timer agent-shell-notifications--knockknock-ret-timer)
                    (setq agent-shell-notifications--knockknock-ret-timer nil))
                  (funcall on-action nil nil)))
    (setq agent-shell-notifications--knockknock-ret-timer
          (run-with-timer knockknock-default-duration nil
                          (lambda ()
                            (setq agent-shell-notifications--knockknock-ret-timer nil))))
    (set-transient-map map (lambda () agent-shell-notifications--knockknock-ret-timer))))

(defun agent-shell-notifications--transform-knockknock (plist)
  "Transform a standard notification PLIST for knockknock.
Renames :body to :message, :app-icon to :icon-file, maps
:timeout (seconds) to :duration (seconds), and drops :actions."
  (list :title (plist-get plist :title)
        :message (plist-get plist :body)
        :icon-file (plist-get plist :app-icon)
        :duration (let ((secs (plist-get plist :timeout)))
                    (if (zerop secs) knockknock-default-duration secs))
        :on-action (plist-get plist :on-action)))

(defun agent-shell-notifications--send-knockknock (plist)
  "Send a notification described by PLIST via knockknock."
  (knockknock-notify
   :title (plist-get plist :title)
   :message (plist-get plist :message)
   :icon-file (plist-get plist :icon-file)
   :duration (plist-get plist :duration))
  (when-let ((on-action (plist-get plist :on-action)))
    (agent-shell-notifications--knockknock-install-ret-binding on-action))
  t)

(defun agent-shell-notifications--close-knockknock (_id)
  "Close the current knockknock notification."
  (knockknock-close))

(setq agent-shell-notifications-transform-timeout-function #'identity)
(setq agent-shell-notifications-transform-function
      #'agent-shell-notifications--transform-knockknock)
(setq agent-shell-notifications-send-function
      #'agent-shell-notifications--send-knockknock)
(setq agent-shell-notifications-close-function
      #'agent-shell-notifications--close-knockknock)

(provide 'agent-shell-notifications-knockknock)

;;; agent-shell-notifications-knockknock.el ends here
