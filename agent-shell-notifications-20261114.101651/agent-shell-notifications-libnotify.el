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
  (let* ((title (plist-get plist :title))
         (body (plist-get plist :body))
         (buf-name (plist-get plist :shell-buffer-name))
         (formatted-title (if (and buf-name (not (string-empty-p buf-name)))
                              (format "%s <%s>" (or title "agent-shell") buf-name)
                            (or title "agent-shell")))
         (final-plist (plist-put (copy-sequence plist) :title formatted-title)))
    (when (null body)
      (setq final-plist (plist-put final-plist :body (or title ""))))
    (apply #'notifications-notify final-plist)))

(defun agent-shell-notifications--close-libnotify (id)
  "Close the notification with ID via `notifications-close-notification'."
  (notifications-close-notification id))

;;;###autoload
(defun agent-shell-notifications-libnotify-setup ()
  "Configure `agent-shell-notifications' to use libnotify."
  (setq agent-shell-notifications-transform-timeout-function
        (lambda (secs) (* 1000 secs)))
  (setq agent-shell-notifications-transform-function #'identity)
  (setq agent-shell-notifications-send-function
        #'agent-shell-notifications--send-libnotify)
  (setq agent-shell-notifications-close-function
        #'agent-shell-notifications--close-libnotify))

(agent-shell-notifications-libnotify-setup)

(provide 'agent-shell-notifications-libnotify)

;;; agent-shell-notifications-libnotify.el ends here
