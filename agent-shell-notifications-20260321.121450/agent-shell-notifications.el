;;; agent-shell-notifications.el --- Libnotify notifications for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Zachary Hanham
;; Copyright (C) 2026 Alvaro Ramirez

;; Author: Zachary Hanham
;; URL: https://github.com/zackattackz/agent-shell-notifications
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.50"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Display desktop notifications via pluggable backends for agent-shell events.
;;
;; Based on agent-shell-knockknock by Alvaro Ramirez.
;; https://github.com/xenodium/agent-shell-knockknock

;;; Code:

(require 'agent-shell)
(require 'agent-shell-viewport)
(require 'map)

(defgroup agent-shell-notifications nil
  "Notifications for agent-shell."
  :group 'agent-shell)

(defcustom agent-shell-notifications-provider 'agent-shell-notifications-libnotify
  "Notification provider to load.
A symbol naming the feature to require, which must set
`agent-shell-notifications-send-function' and
`agent-shell-notifications-close-function'.
Set to nil to configure those functions manually.

After changing this value, call `agent-shell-notifications-set-provider'
with the new symbol to load the provider and validate the result."
  :type '(choice (const :tag "libnotify (notifications.el)" agent-shell-notifications-libnotify)
                 (const :tag "knockknock (optional dependency)" agent-shell-notifications-knockknock)
                 (const :tag "None (configure manually)" nil)
                 (symbol :tag "Custom provider"))
  :group 'agent-shell-notifications)

(defcustom agent-shell-notifications-timeout 0
  "Timeout in seconds for notifications.
When a number, use the same timeout for all event types.
When an alist of (TYPE . SECONDS) pairs, use a per-type timeout.
When a function, call it with the event TYPE symbol and EVENT;
it should return the timeout in seconds.

A value of 0 means never expire.  A value of -1 means use the backend default.

Possible event types: `permission-request', `turn-complete'."
  :type '(choice (number :tag "Seconds for all types")
                 (alist :tag "Per-type timeouts"
                        :key-type symbol :value-type number)
                 (function :tag "Custom function"))
  :group 'agent-shell-notifications)

(defcustom agent-shell-notifications-shell-visible-function
  #'agent-shell-notifications--shell-visible-p-default
  "Function to determine if a shell buffer is currently visible.
The function is called with one argument, SHELL-BUFFER, and should
return non-nil if the buffer is visible."
  :type 'function
  :group 'agent-shell-notifications)

(defcustom agent-shell-notifications-switch-to-shell-function
  #'agent-shell-notifications--switch-to-shell-default
  "Function called to switch to a shell buffer on notification action.
The function is called with one argument, SHELL-BUFFER."
  :type 'function
  :group 'agent-shell-notifications)

(defcustom agent-shell-notifications-immediate-notifications t
  "Control which event types trigger immediate notifications when the shell is not visible.
When t, immediately notify for all event types when the shell is not visible.
When nil, disable all immediate notifications.
When a list of symbols (e.g. \\='(permission-request)), immediately notify only
for those event types when the shell is not visible.
When a function, call it with the event TYPE symbol and EVENT;
immediately notify if it returns non-nil.

Possible event types: `permission-request', `turn-complete'."
  :type '(choice (const :tag "All" t)
                 (const :tag "None" nil)
                 (repeat :tag "Event types" symbol)
                 (function :tag "Predicate function"))
  :group 'agent-shell-notifications)

(defcustom agent-shell-notifications-idle-notifications t
  "Control which event types trigger idle notifications.
When non-nil, notify even when the shell is visible after
`agent-shell-notifications-idle-timeout' seconds without a response.
When t, notify for all event types.
When nil, disable all idle notifications.
When a list of symbols (e.g. \\='(permission-request)), notify only
for those event types.
When a function, call it with the event TYPE symbol and EVENT;
notify if it returns non-nil.

Possible event types: `permission-request', `turn-complete'."
  :type '(choice (const :tag "All" t)
                 (const :tag "None" nil)
                 (repeat :tag "Event types" symbol)
                 (function :tag "Predicate function"))
  :group 'agent-shell-notifications)

(defcustom agent-shell-notifications-idle-timeout 10
  "Seconds to wait before notifying when shell is visible.
Only used when `agent-shell-notifications-idle-notifications' is non-nil.
When a number, use the same timeout for all event types.
When an alist of (TYPE . SECONDS) pairs, use a per-type timeout.
When a function, call it with the event TYPE symbol and EVENT;
it should return the timeout in seconds.

Possible event types: `permission-request', `turn-complete'."
  :type '(choice (number :tag "Seconds for all types")
                 (alist :tag "Per-type timeouts"
                        :key-type symbol :value-type number)
                 (function :tag "Custom function"))
  :group 'agent-shell-notifications)

(defcustom agent-shell-notifications-format-function
  #'agent-shell-notifications--format-default
  "Function to build notification content for an agent-shell event.
Called with two arguments: TYPE (a symbol such as
\\='permission-request or \\='turn-complete) and EVENT.
Should return a plist with at least :title; :body is optional.

Possible event types: `permission-request', `turn-complete'."
  :type 'function
  :group 'agent-shell-notifications)

(defcustom agent-shell-notifications-transform-timeout-function #'identity
  "Function to transform the timeout value before placing it in the notification plist.
Called with a single argument, the timeout in seconds as resolved by
`agent-shell-notifications--resolve-timeout'.
Should return the timeout value in whatever unit the backend expects.
The default `identity' passes the value through unchanged (seconds).
Backends that require a different unit should set this; for example, the
libnotify backend sets it to a function that converts seconds to milliseconds."
  :type 'function
  :group 'agent-shell-notifications)

(defcustom agent-shell-notifications-transform-function #'identity
  "Function to transform the notification plist before sending.
Called with a single argument, the plist produced by
`agent-shell-notifications--make-notification-plist'.
Should return the plist to pass to `agent-shell-notifications-send-function'.
The default `identity' passes the plist through unchanged.
Providers may set this to rekey or reformat the plist for their backend."
  :type 'function
  :group 'agent-shell-notifications)

(defun agent-shell-notifications--inhibit-cancelled (type event)
  "Return non-nil if TYPE is `turn-complete' and the turn was user-cancelled."
  (and (eq type 'turn-complete)
       (equal (map-nested-elt event '(:data :stop-reason)) "cancelled")))

(defcustom agent-shell-notifications-inhibit-functions
  (list #'agent-shell-notifications--inhibit-cancelled)
  "Abnormal hook run to decide whether to suppress a notification.
Each function is called with two arguments: TYPE (a symbol such as
\\='permission-request or \\='turn-complete) and EVENT.
If any function returns non-nil, the notification is suppressed and
no further functions are called.

The default suppresses `turn-complete' notifications when the turn
was cancelled by the user.

Note: prefer `agent-shell-notifications-immediate-notifications' and
`agent-shell-notifications-idle-notifications' for filtering by event
type — both accept a predicate function.  Reserve this hook for
advanced filtering.

Possible event types: `permission-request', `turn-complete'."
  :type 'hook
  :group 'agent-shell-notifications)

(defcustom agent-shell-notifications-send-function nil
  "Function to send a notification plist.
Called with a single argument, the plist returned by
`agent-shell-notifications-transform-function' (which may differ
from the standard plist depending on the provider).
Should return a notification ID if one is available, or nil.
Set by the provider loaded via `agent-shell-notifications-provider'."
  :type '(choice (const :tag "Set by provider" nil)
                 (function :tag "Custom function"))
  :group 'agent-shell-notifications)

(defcustom agent-shell-notifications-close-function nil
  "Function to close a notification by its ID.
Called with a single argument, the notification ID previously
returned by `agent-shell-notifications-send-function'.
Set by the provider loaded via `agent-shell-notifications-provider'."
  :type '(choice (const :tag "Set by provider" nil)
                 (function :tag "Custom function"))
  :group 'agent-shell-notifications)

(define-error 'agent-shell-notifications-unsubscribe-error
  "One or more errors occurred while unsubscribing from agent-shell events")

(defvar agent-shell-notifications--subscriptions nil
  "Alist mapping shell buffers to their subscription tokens.")

(defvar agent-shell-notifications--timers nil
  "Alist mapping shell buffers to pending idle notification timers.
Each value is an alist of (KEY . TIMER), where KEY is the :request-id
for permission-request events or `turn-complete' for turn-complete events.")

(defvar agent-shell-notifications--notification-ids nil
  "Alist mapping shell buffers to active notification IDs.
Each value is an alist of (KEY . ID), where KEY is the :request-id
for permission-request events or `turn-complete' for turn-complete events.")

(defun agent-shell-notifications--shell-visible-p-default (shell-buffer)
  "Return non-nil if SHELL-BUFFER or its viewport is visible in a focused frame.
Uses `frame-focus-state' to check whether the containing frame has input focus.
Returns nil (triggering a notification) when focus state is unknown."
  (when-let ((win (or (get-buffer-window shell-buffer t)
                      (when-let ((viewport (agent-shell-viewport--buffer
                                            :shell-buffer shell-buffer
                                            :existing-only t)))
                        (get-buffer-window viewport t)))))
    (eq t (frame-focus-state (window-frame win)))))

(defun agent-shell-notifications--shell-visible-p (shell-buffer)
  "Return non-nil if SHELL-BUFFER is visible per `agent-shell-notifications-shell-visible-function'."
  (funcall agent-shell-notifications-shell-visible-function shell-buffer))

(defun agent-shell-notifications--icon-file (shell-buffer)
  "Get the agent icon file path for SHELL-BUFFER."
  (when-let* ((state (buffer-local-value 'agent-shell--state shell-buffer))
              (icon-name (map-nested-elt state '(:agent-config :icon-name))))
    (agent-shell--fetch-agent-icon icon-name)))

(defun agent-shell-notifications--strip-kind-prefix (text kind)
  "Strip KIND prefix from TEXT to avoid redundancy.
For example, \"Edit README.org\" with kind \"edit\" becomes \"README.org\"."
  (if (and kind text
           (string-match-p (concat "\\`" (regexp-quote kind) " ")
                           (downcase text)))
      (string-trim (substring text (length kind)))
    text))

(defun agent-shell-notifications--format-permission-message (tool-call)
  "Format a user-friendly message from TOOL-CALL."
  (let ((stripped (agent-shell-notifications--strip-kind-prefix
                   (map-elt tool-call :title)
                   (map-elt tool-call :kind))))
    (pcase (map-elt tool-call :kind)
      ((or "read" "edit" "write" "delete" "move")
       (agent-shell--shorten-paths stripped))
      ((or "execute" "search" "fetch")
       (let ((first-line (car (split-string stripped "\n"))))
         (if (> (length first-line) 50)
             (concat (substring first-line 0 47) "...")
           first-line)))
      (_ stripped))))

(defun agent-shell-notifications--switch-to-shell-default (shell-buffer)
  "Switch to SHELL-BUFFER or its viewport.
If the buffer is already visible in a window on any visible frame, raise
that frame and select the window.  Otherwise fall back to switching the
current window's buffer."
  (when (buffer-live-p shell-buffer)
    (let* ((target (or (agent-shell-viewport--buffer
                        :shell-buffer shell-buffer
                        :existing-only t)
                       shell-buffer))
           (win (get-buffer-window target t)))
      (if win
          (progn
            (select-frame-set-input-focus (window-frame win))
            (select-window win))
        (progn
          (select-frame-set-input-focus (selected-frame))
          (switch-to-buffer target))))))

(defun agent-shell-notifications--switch-to-shell (shell-buffer)
  "Switch to SHELL-BUFFER using `agent-shell-notifications-switch-to-shell-function'."
  (funcall agent-shell-notifications-switch-to-shell-function shell-buffer))

(defun agent-shell-notifications--format-default (type event)
  "Default implementation of `agent-shell-notifications-format-function'.
Returns a plist with :title and optionally :body for TYPE and EVENT."
  (pcase type
    ('permission-request
     (list :title (concat "Agent-shell: "
                          (capitalize (or (map-nested-elt event '(:data :tool-call :kind)) "")))
           :body (agent-shell-notifications--format-permission-message
                  (map-nested-elt event '(:data :tool-call)))))
    ('turn-complete
     (list :title (if (equal (map-nested-elt event '(:data :stop-reason)) "end_turn")
                      "Agent-shell: Ready"
                    "Agent-shell: Stopped")))))

(defun agent-shell-notifications--should-notify-p (setting type event)
  "Return non-nil if SETTING allows notification for event TYPE.
SETTING may be t (all), nil (none), a list of type symbols, or a
predicate function taking TYPE and EVENT."
  (cond
   ((eq setting t) t)
   ((null setting) nil)
   ((functionp setting) (funcall setting type event))
   ((listp setting) (memq type setting))))

(defun agent-shell-notifications--resolve-timeout (setting type event)
  "Resolve SETTING to a timeout value for event TYPE.
SETTING may be a number (used for all types), an alist of
\(TYPE . NUMBER) pairs, or a function taking TYPE and EVENT.
When SETTING is an alist and TYPE is not present, the `default'
key is used.  If neither TYPE nor `default' is found, logs a
message and falls back to 0."
  (cond
   ((numberp setting) setting)
   ((functionp setting) (funcall setting type event))
   ((listp setting)
    (if-let ((entry (assq type setting)))
        (cdr entry)
      (if-let ((fallback (assq 'default setting)))
          (cdr fallback)
        (message "agent-shell-notifications: no timeout configured for `%s'; add (%s . VALUE) or (default . VALUE) to your timeout alist" type type)
        0)))))

(defun agent-shell-notifications--timer-key (type event)
  "Return the alist key for a timer associated with TYPE and EVENT."
  (pcase type
    ('permission-request (map-nested-elt event '(:data :request-id)))
    (_ type)))

(defun agent-shell-notifications--cancel-timer (shell-buffer key)
  "Cancel the pending idle notification timer for SHELL-BUFFER with KEY."
  (when-let* ((buffer-timers (map-elt agent-shell-notifications--timers shell-buffer))
              (entry (assoc key buffer-timers)))
    (cancel-timer (cdr entry))
    (let ((remaining (assoc-delete-all key buffer-timers)))
      (if remaining
          (setf (map-elt agent-shell-notifications--timers shell-buffer) remaining)
        (setq agent-shell-notifications--timers
              (map-delete agent-shell-notifications--timers shell-buffer))))))

(defun agent-shell-notifications--cancel-all-timers (shell-buffer)
  "Cancel all pending idle notification timers for SHELL-BUFFER."
  (dolist (entry (map-elt agent-shell-notifications--timers shell-buffer))
    (agent-shell-notifications--cancel-timer shell-buffer (car entry))))

(defun agent-shell-notifications--add-timer (shell-buffer key timer)
  "Register TIMER under KEY for SHELL-BUFFER, replacing any existing entry."
  (let ((buffer-timers (map-elt agent-shell-notifications--timers shell-buffer)))
    (setf (map-elt agent-shell-notifications--timers shell-buffer)
          (cons (cons key timer)
                (assoc-delete-all key buffer-timers)))))

(defun agent-shell-notifications--add-notification-id (shell-buffer key id)
  "Register notification ID under KEY for SHELL-BUFFER, replacing any existing entry."
  (let ((buffer-ids (map-elt agent-shell-notifications--notification-ids shell-buffer)))
    (setf (map-elt agent-shell-notifications--notification-ids shell-buffer)
          (cons (cons key id)
                (assoc-delete-all key buffer-ids)))))

(defun agent-shell-notifications--make-notification-plist (type shell-buffer event)
  "Build the full notification plist for TYPE on SHELL-BUFFER with EVENT.
Merges the content from `agent-shell-notifications-format-function' with
meta properties: :app-icon, :timeout, :actions, and :on-action."
  (append (funcall agent-shell-notifications-format-function type event)
          (list :app-icon (agent-shell-notifications--icon-file shell-buffer)
                :timeout (funcall agent-shell-notifications-transform-timeout-function
                                 (agent-shell-notifications--resolve-timeout
                                  agent-shell-notifications-timeout type event))
                :actions '("default" "Switch to shell")
                :on-action (lambda (_id _key)
                             (agent-shell-notifications--switch-to-shell shell-buffer)))))

(defun agent-shell-notifications--notify-and-track (type shell-buffer event key)
  "Send notification for TYPE on SHELL-BUFFER with EVENT and track the returned ID under KEY."
  (when-let ((id (with-demoted-errors "agent-shell-notifications: %s"
                   (funcall agent-shell-notifications-send-function
                            (funcall agent-shell-notifications-transform-function
                                     (agent-shell-notifications--make-notification-plist
                                      type shell-buffer event))))))
    (agent-shell-notifications--add-notification-id shell-buffer key id)))

(defun agent-shell-notifications--close-notification (shell-buffer key)
  "Close the active notification for KEY in SHELL-BUFFER."
  (when-let* ((buffer-ids (map-elt agent-shell-notifications--notification-ids shell-buffer))
              (entry (assoc key buffer-ids)))
    (with-demoted-errors "agent-shell-notifications: %s"
      (funcall agent-shell-notifications-close-function (cdr entry)))
    (let ((remaining (assoc-delete-all key buffer-ids)))
      (if remaining
          (setf (map-elt agent-shell-notifications--notification-ids shell-buffer) remaining)
        (setq agent-shell-notifications--notification-ids
              (map-delete agent-shell-notifications--notification-ids shell-buffer))))))

(defun agent-shell-notifications--close-all-notifications (shell-buffer)
  "Close all active notifications for SHELL-BUFFER."
  (dolist (entry (map-elt agent-shell-notifications--notification-ids shell-buffer))
    (agent-shell-notifications--close-notification shell-buffer (car entry))))

(defun agent-shell-notifications--schedule-or-notify (type shell-buffer event)
  "Notify for TYPE event on SHELL-BUFFER immediately or after idle timeout.
If shell is not visible, notify immediately.  If shell is visible and
`agent-shell-notifications-idle-notifications' is non-nil, schedule
notification after `agent-shell-notifications-idle-timeout' seconds."
  (unless (run-hook-with-args-until-success
           'agent-shell-notifications-inhibit-functions type event)
  (let ((key (agent-shell-notifications--timer-key type event)))
    (if (agent-shell-notifications--shell-visible-p shell-buffer)
        (when (agent-shell-notifications--should-notify-p
               agent-shell-notifications-idle-notifications type event)
          (agent-shell-notifications--add-timer
           shell-buffer key
           (run-with-timer (agent-shell-notifications--resolve-timeout
                            agent-shell-notifications-idle-timeout type event) nil
                           (lambda ()
                             (agent-shell-notifications--cancel-timer shell-buffer key)
                             (agent-shell-notifications--notify-and-track
                              type shell-buffer event key)))))
      (when (agent-shell-notifications--should-notify-p
             agent-shell-notifications-immediate-notifications type event)
        (agent-shell-notifications--notify-and-track type shell-buffer event key))))))


(defun agent-shell-notifications--on-permission-request (event)
  "Handle a permission-request EVENT with a libnotify notification."
  (with-demoted-errors "agent-shell-notifications: %s"
    (agent-shell-notifications--schedule-or-notify
     'permission-request (current-buffer) event)))

(defun agent-shell-notifications--on-permission-response (event)
  "Handle a permission-response EVENT by cancelling its timer and notification."
  (with-demoted-errors "agent-shell-notifications: %s"
    (let ((shell-buffer (current-buffer))
          (key (map-nested-elt event '(:data :request-id))))
      (agent-shell-notifications--cancel-timer shell-buffer key)
      (agent-shell-notifications--close-notification shell-buffer key))))

(defun agent-shell-notifications--on-turn-complete (event)
  "Handle a turn-complete EVENT with a libnotify notification."
  (with-demoted-errors "agent-shell-notifications: %s"
    (let ((shell-buffer (current-buffer)))
      (agent-shell-notifications--cancel-all-timers shell-buffer)
      (agent-shell-notifications--close-all-notifications shell-buffer)
      (agent-shell-notifications--schedule-or-notify
       'turn-complete shell-buffer event))))

(defun agent-shell-notifications--on-send-command (&rest args)
  "Cancel timers and close notifications when a command is sent."
  (with-demoted-errors "agent-shell-notifications: %s"
    (when-let ((shell-buffer (plist-get args :shell-buffer)))
      (agent-shell-notifications--cancel-all-timers shell-buffer)
      (agent-shell-notifications--close-all-notifications shell-buffer))))

(defun agent-shell-notifications--on-viewport-activity ()
  "Cancel timers and close notifications on viewport reply or send."
  (when-let ((shell-buffer (agent-shell-viewport--shell-buffer)))
    (agent-shell-notifications--cancel-and-close shell-buffer)))

(defun agent-shell-notifications--cancel-and-close (shell-buffer)
  "Cancel all timers and close all notifications for SHELL-BUFFER."
  (agent-shell-notifications--cancel-all-timers shell-buffer)
  (agent-shell-notifications--close-all-notifications shell-buffer))

(defun agent-shell-notifications--on-window-activity (window)
  "Cancel timers and close notifications when the shell buffer's WINDOW is selected or deselected."
  (let* ((buf (window-buffer window))
         (shell-buf (or (agent-shell-viewport--shell-buffer buf) buf)))
    (agent-shell-notifications--cancel-and-close shell-buf)))

(defun agent-shell-notifications--on-self-insert ()
  "Cancel timers and close notifications when a character is inserted in the shell buffer."
  (agent-shell-notifications--cancel-and-close (current-buffer)))

(defun agent-shell-notifications--on-buffer-kill ()
  "Clean up timers and notifications when a shell buffer is killed."
  (agent-shell-notifications-unsubscribe (current-buffer)))

(defun agent-shell-notifications-subscribe (&optional shell-buffer)
  "Subscribe to agent-shell events in SHELL-BUFFER.
If SHELL-BUFFER is nil, use the current buffer."
  (let ((buf (or shell-buffer (current-buffer))))
    (when (map-elt agent-shell-notifications--subscriptions buf)
      (agent-shell-notifications-unsubscribe buf))
    (condition-case err
        (progn
          (push (agent-shell-subscribe-to
                 :shell-buffer buf
                 :event 'permission-request
                 :on-event #'agent-shell-notifications--on-permission-request)
                (map-elt agent-shell-notifications--subscriptions buf))
          (push (agent-shell-subscribe-to
                 :shell-buffer buf
                 :event 'permission-response
                 :on-event #'agent-shell-notifications--on-permission-response)
                (map-elt agent-shell-notifications--subscriptions buf))
          (push (agent-shell-subscribe-to
                 :shell-buffer buf
                 :event 'turn-complete
                 :on-event #'agent-shell-notifications--on-turn-complete)
                (map-elt agent-shell-notifications--subscriptions buf)))
      (error
       (agent-shell-notifications-unsubscribe buf)
       (signal (car err) (cdr err))))))

(defun agent-shell-notifications-unsubscribe (&optional shell-buffer)
  "Unsubscribe from agent-shell events in SHELL-BUFFER.
If SHELL-BUFFER is nil, use the current buffer."
  (let ((buf (or shell-buffer (current-buffer)))
        errors)
    (agent-shell-notifications--cancel-all-timers buf)
    (agent-shell-notifications--close-all-notifications buf)
    (while (map-elt agent-shell-notifications--subscriptions buf)
      (let* ((tokens (map-elt agent-shell-notifications--subscriptions buf))
             (token (car tokens))
             (remaining (cdr tokens)))
        (condition-case err
            (agent-shell-unsubscribe :subscription token)
          (error (push err errors)))
        (if remaining
            (setf (map-elt agent-shell-notifications--subscriptions buf) remaining)
          (setq agent-shell-notifications--subscriptions
                (map-delete agent-shell-notifications--subscriptions buf)))))
    (when errors
      (signal 'agent-shell-notifications-unsubscribe-error (nreverse errors)))))

;;;###autoload
(define-minor-mode agent-shell-notifications-mode
  "Toggle libnotify notifications for the current agent-shell buffer."
  :lighter " libnotify"
  (if agent-shell-notifications-mode
      (progn
        (add-hook 'kill-buffer-hook #'agent-shell-notifications--on-buffer-kill nil t)
        (add-hook 'window-selection-change-functions #'agent-shell-notifications--on-window-activity nil t)
        (add-hook 'post-self-insert-hook #'agent-shell-notifications--on-self-insert nil t)
        (agent-shell-notifications-subscribe (current-buffer))
        (unless (advice-member-p #'agent-shell-notifications--on-send-command
                                 #'agent-shell--send-command)
          (advice-add #'agent-shell--send-command :before
                      #'agent-shell-notifications--on-send-command)))
    (agent-shell-notifications-unsubscribe (current-buffer))
    (remove-hook 'kill-buffer-hook #'agent-shell-notifications--on-buffer-kill t)
    (remove-hook 'window-selection-change-functions #'agent-shell-notifications--on-window-activity t)
    (remove-hook 'post-self-insert-hook #'agent-shell-notifications--on-self-insert t)
    (when (null agent-shell-notifications--subscriptions)
      (advice-remove #'agent-shell--send-command
                     #'agent-shell-notifications--on-send-command))))

;;;###autoload
(define-minor-mode agent-shell-notifications-viewport-edit-mode
  "Toggle notifications hooks for an agent-shell viewport edit buffer.
Add to `agent-shell-viewport-edit-mode-hook' to cancel idle timers and
close notifications when the viewport edit window is selected."
  :lighter nil
  (if agent-shell-notifications-viewport-edit-mode
      (add-hook 'window-selection-change-functions
                #'agent-shell-notifications--on-window-activity nil t)
    (remove-hook 'window-selection-change-functions
                 #'agent-shell-notifications--on-window-activity t)))

;;;###autoload
(define-minor-mode agent-shell-notifications-viewport-view-mode
  "Toggle notifications hooks for an agent-shell viewport view buffer.
Add to `agent-shell-viewport-view-mode-hook' to cancel idle timers and
close notifications when the viewport view window is selected or a reply is sent."
  :lighter nil
  (if agent-shell-notifications-viewport-view-mode
      (progn
        (add-hook 'window-selection-change-functions
                  #'agent-shell-notifications--on-window-activity nil t)
        (unless (advice-member-p #'agent-shell-notifications--on-viewport-activity
                                 #'agent-shell-viewport-reply)
          (advice-add #'agent-shell-viewport-reply :before
                      #'agent-shell-notifications--on-viewport-activity)))
    (remove-hook 'window-selection-change-functions
                 #'agent-shell-notifications--on-window-activity t)
    (unless (seq-some (lambda (buf)
                        (buffer-local-value
                         'agent-shell-notifications-viewport-view-mode buf))
                      (buffer-list))
      (advice-remove #'agent-shell-viewport-reply
                     #'agent-shell-notifications--on-viewport-activity))))

;;;###autoload
(defun agent-shell-notifications-set-provider (provider)
  "Load PROVIDER and validate that it sets the required functions.
PROVIDER is a symbol naming the feature to require.  The feature
must set `agent-shell-notifications-send-function' and
`agent-shell-notifications-close-function'.  When PROVIDER is nil,
skips loading and validation (the functions must be set manually).
Interactively, reads a provider symbol from the minibuffer."
  (interactive "SProvider symbol: ")
  (when provider
    (require provider))
  (unless agent-shell-notifications-send-function
    (error "agent-shell-notifications: `agent-shell-notifications-send-function' is nil; \
the provider must set it"))
  (unless agent-shell-notifications-close-function
    (error "agent-shell-notifications: `agent-shell-notifications-close-function' is nil; \
the provider must set it")))

(agent-shell-notifications-set-provider agent-shell-notifications-provider)

(provide 'agent-shell-notifications)

;;; agent-shell-notifications.el ends here
