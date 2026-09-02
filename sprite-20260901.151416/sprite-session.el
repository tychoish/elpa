;;; sprite-session.el --- Generic session lifecycle hooks -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/tychoish/sprite
;; Keywords: tools, daemon, processes
;;; Commentary:
;; Idle timer and systemd-logind sleep hooks that application packages can
;; register against.  Decouples trigger mechanisms (Emacs idleness, system
;; sleep) from application-level responses.
;;
;; Usage: call `sprite-session-start-idle-timer' and/or
;; `sprite-session-start-logind-watch' from your package's setup function,
;; add functions to `sprite-session-idle-hook' and/or
;; `sprite-session-before-sleep-hook', then stop the timers in teardown.

;;; Code:

(require 'dbus nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs idle hook

(defcustom sprite-session-idle-timeout 3600
  "Seconds of Emacs idle before `sprite-session-idle-hook' fires."
  :type 'integer
  :group 'sprite)

(defvar sprite-session-idle-hook nil
  "Hook run when Emacs has been idle for `sprite-session-idle-timeout' seconds.
Each function is called with no arguments.")

(defvar sprite-session--idle-timer nil
  "Timer that runs `sprite-session-idle-hook' after extended Emacs idle.")

(defun sprite-session-start-idle-timer ()
  "Start a repeating idle timer that runs `sprite-session-idle-hook'.
Cancels any existing timer first."
  (sprite-session-stop-idle-timer)
  (setq sprite-session--idle-timer
        (run-with-idle-timer sprite-session-idle-timeout t
                             #'run-hooks 'sprite-session-idle-hook)))

(defun sprite-session-stop-idle-timer ()
  "Cancel the idle timer."
  (when sprite-session--idle-timer
    (cancel-timer sprite-session--idle-timer)
    (setq sprite-session--idle-timer nil)))

(defun sprite-session-sync-idle-timer ()
  "Start or stop the idle timer to match `sprite-session-idle-hook' membership.
Starts the timer when the hook is non-nil; stops it when empty.
Always logs: sprite-session: idle timer <running|stopped> (<N> registered ops)"
  (if sprite-session-idle-hook
      (unless sprite-session--idle-timer
        (sprite-session-start-idle-timer))
    (when sprite-session--idle-timer
      (sprite-session-stop-idle-timer)))
  (let ((inhibit-message t))
    (message "sprite-session: idle timer %s (%d registered ops)"
             (if sprite-session--idle-timer "running" "stopped")
             (length sprite-session-idle-hook))))

;;;###autoload
(defun sprite-session-add-on-idle (fn)
  "Add FN to `sprite-session-idle-hook' and start the timer if needed.
Logs the registration and delegates to `sprite-session-sync-idle-timer'."
  (let ((inhibit-message t))
    (message "sprite-session: registered idle op: %s" (symbol-name fn)))
  (add-hook 'sprite-session-idle-hook fn)
  (sprite-session-sync-idle-timer))

;;;###autoload
(defun sprite-session-remove-on-idle (fn)
  "Remove FN from `sprite-session-idle-hook' and stop the timer if the hook is empty.
Logs the deregistration and delegates to `sprite-session-sync-idle-timer'."
  (let ((inhibit-message t))
    (message "sprite-session: deregistered idle op: %s" (symbol-name fn)))
  (remove-hook 'sprite-session-idle-hook fn)
  (sprite-session-sync-idle-timer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; systemd-logind hook

(defvar sprite-session-before-sleep-hook nil
  "Hook run before the system suspends or hibernates.
Each function is called with no arguments.  Only fires on Linux systems
with DBus support and systemd-logind.")

(defvar sprite-session-after-sleep-hook nil
  "Hook run when the system resumes from suspend or hibernate.
Each function is called with no arguments.  Only fires on Linux systems
with DBus support and systemd-logind.")

(defvar sprite-session--logind-signal nil
  "DBus registration object for the logind PrepareForSleep signal.")

(defvar sprite-session--dbus-signals nil
  "List of additional DBus registration objects.")

(defun sprite-session--logind-available-p ()
  "Return non-nil when systemd-logind DBus integration is usable."
  (and (fboundp 'dbus-register-signal)
       (eq system-type 'gnu/linux)))

(defun sprite-session--on-prepare-for-sleep (going-to-sleep)
  "Dispatch sleep/wake hooks based on GOING-TO-SLEEP.
GOING-TO-SLEEP is t when the system is suspending, nil on resume."
  (if going-to-sleep
      (run-hooks 'sprite-session-before-sleep-hook)
    (run-hooks 'sprite-session-after-sleep-hook)))

(defun sprite-session-start-logind-watch ()
  "Register DBus signals to run sleep/wake/lock/screensaver hooks.
Cancels any existing registrations first to prevent duplicates."
  (when (sprite-session--logind-available-p)
    (sprite-session-stop-logind-watch)
    (setq sprite-session--logind-signal
          (dbus-register-signal
           :system
           "org.freedesktop.login1"
           "/org/freedesktop/login1"
           "org.freedesktop.login1.Manager"
           "PrepareForSleep"
           #'sprite-session--on-prepare-for-sleep))
    (setq sprite-session--dbus-signals
          (list
           ;; 1. systemd-logind Session Lock
           (dbus-register-signal
            :system
            "org.freedesktop.login1"
            nil
            "org.freedesktop.login1.Session"
            "Lock"
            (lambda ()
              (message "sprite-session: received systemd-logind Lock signal")
              (run-hooks 'sprite-session-before-sleep-hook)))
           ;; 2. systemd-logind Session Unlock
           (dbus-register-signal
            :system
            "org.freedesktop.login1"
            nil
            "org.freedesktop.login1.Session"
            "Unlock"
            (lambda ()
              (message "sprite-session: received systemd-logind Unlock signal")
              (run-hooks 'sprite-session-after-sleep-hook)))
           ;; 3. freedesktop ScreenSaver ActiveChanged (Lock/Unlock)
           (dbus-register-signal
            :session
            nil
            nil
            "org.freedesktop.ScreenSaver"
            "ActiveChanged"
            (lambda (active)
              (message "sprite-session: received ScreenSaver ActiveChanged signal: %s" active)
              (if active
                  (run-hooks 'sprite-session-before-sleep-hook)
                (run-hooks 'sprite-session-after-sleep-hook))))))))

(defun sprite-session-stop-logind-watch ()
  "Unregister the logind PrepareForSleep DBus signal and other signals."
  (when sprite-session--logind-signal
    (dbus-ignore-errors
      (dbus-unregister-object sprite-session--logind-signal))
    (setq sprite-session--logind-signal nil))
  (when sprite-session--dbus-signals
    (dbus-ignore-errors
      (mapc #'dbus-unregister-object sprite-session--dbus-signals))
    (setq sprite-session--dbus-signals nil)))

(provide 'sprite-session)
;;; sprite-session.el ends here
