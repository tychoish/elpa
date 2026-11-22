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

(defvar agent-shell-notifications-transform-timeout-function)
(defvar agent-shell-notifications-transform-function)
(defvar agent-shell-notifications-send-function)
(defvar agent-shell-notifications-close-function)

(defun agent-shell-notifications--send-libnotify (plist)
  "Send a notification described by PLIST via `notifications-notify'.
The shell buffer name is used as the notification subject (:title) and the
message is placed in the body.  `:app-name' identifies agent-shell."
  (let* ((title (plist-get plist :title))
         (body (plist-get plist :body))
         (buf-name (plist-get plist :shell-buffer-name))
         (agent-name (plist-get plist :agent-name))
         (effective-title (or title
                              (when (and agent-name (not (string-empty-p agent-name)))
                                (format "%s: Ready" agent-name))))
         (app-name (or (plist-get plist :app-name)
                       (if (and agent-name (not (string-empty-p agent-name)))
                           (format "agent-shell (%s)" agent-name)
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
                         (or body "")))
         (final-plist (copy-sequence plist)))
    (setq final-plist (plist-put final-plist :app-name app-name))
    (setq final-plist (plist-put final-plist :title subject))
    (setq final-plist (plist-put final-plist :body message-body))
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
