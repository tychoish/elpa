;;; agent-shell-notifications-libnotify.el --- libnotify provider for agent-shell-notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Zachary Hanham

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; libnotify provider for agent-shell-notifications.

;;; Code:

(require 'notifications)

(defun agent-shell-notifications--send-libnotify (plist)
  "Send a notification described by PLIST via `notifications-notify'."
  (apply #'notifications-notify plist))

(defun agent-shell-notifications--close-libnotify (id)
  "Close the notification with ID via `notifications-close-notification'."
  (notifications-close-notification id))

(setq agent-shell-notifications-transform-timeout-function
      (lambda (secs) (* 1000 secs)))
(setq agent-shell-notifications-transform-function #'identity)
(setq agent-shell-notifications-send-function
      #'agent-shell-notifications--send-libnotify)
(setq agent-shell-notifications-close-function
      #'agent-shell-notifications--close-libnotify)

(provide 'agent-shell-notifications-libnotify)

;;; agent-shell-notifications-libnotify.el ends here
