;;; sprite-system.el --- Generic session lifecycle hooks -*- lexical-binding: t; -*-

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
;; Usage: call `sprite-system-start-idle-timer' and/or
;; `sprite-system-start-logind-watch' from your package's setup function,
;; add functions to `sprite-system-idle-hook' and/or
;; `sprite-system-before-sleep-hook', then stop the timers in teardown.

;;; Code:

(require 'dbus nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs idle hook

(defcustom sprite-system-idle-timeout 3600
  "Seconds of Emacs idle before `sprite-system-idle-hook' fires."
  :type 'integer
  :group 'sprite)

(defvar sprite-system-idle-hook nil
  "Hook run when Emacs has been idle for `sprite-system-idle-timeout' seconds.
Each function is called with no arguments.")

(defvar sprite-system--idle-timer nil
  "Timer that runs `sprite-system-idle-hook' after extended Emacs idle.")

(defun sprite-system-start-idle-timer ()
  "Start a repeating idle timer that runs `sprite-system-idle-hook'.
Cancels any existing timer first."
  (sprite-system-stop-idle-timer)
  (setq sprite-system--idle-timer
        (run-with-idle-timer sprite-system-idle-timeout t
                             #'run-hooks 'sprite-system-idle-hook)))

(defun sprite-system-stop-idle-timer ()
  "Cancel the idle timer."
  (when sprite-system--idle-timer
    (cancel-timer sprite-system--idle-timer)
    (setq sprite-system--idle-timer nil)))

(defun sprite-system-sync-idle-timer ()
  "Start or stop the idle timer to match `sprite-system-idle-hook' membership.
Starts the timer when the hook is non-nil; stops it when empty.
Always logs: sprite-system: idle timer <running|stopped> (<N> registered ops)"
  (if sprite-system-idle-hook
      (unless sprite-system--idle-timer
        (sprite-system-start-idle-timer))
    (when sprite-system--idle-timer
      (sprite-system-stop-idle-timer)))
  (let ((inhibit-message t))
    (message "sprite-system: idle timer %s (%d registered ops)"
             (if sprite-system--idle-timer "running" "stopped")
             (length sprite-system-idle-hook))))

;;;###autoload
(defun sprite-system-add-on-idle (fn)
  "Add FN to `sprite-system-idle-hook' and start the timer if needed.
Logs the registration and delegates to `sprite-system-sync-idle-timer'."
  (let ((inhibit-message t))
    (message "sprite-system: registered idle op: %s" (symbol-name fn)))
  (add-hook 'sprite-system-idle-hook fn)
  (sprite-system-sync-idle-timer))

;;;###autoload
(defun sprite-system-remove-on-idle (fn)
  "Remove FN from `sprite-system-idle-hook' and maybe stop timer.
Logs deregistration and delegates to `sprite-system-sync-idle-timer'."
  (let ((inhibit-message t))
    (message "sprite-system: deregistered idle op: %s" (symbol-name fn)))
  (remove-hook 'sprite-system-idle-hook fn)
  (sprite-system-sync-idle-timer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; systemd-logind hook

(defvar sprite-system-before-sleep-hook nil
  "Hook run before the system suspends or hibernates.
Each function is called with no arguments.  Only fires on Linux systems
with DBus support and systemd-logind.")

(defvar sprite-system-after-sleep-hook nil
  "Hook run when the system resumes from suspend or hibernate.
Each function is called with no arguments.  Only fires on Linux systems
with DBus support and systemd-logind.")

(defvar sprite-system--logind-signal nil
  "DBus registration object for the logind PrepareForSleep signal.")

(defvar sprite-system--dbus-signals nil
  "List of additional DBus registration objects.")

(defun sprite-system--logind-available-p ()
  "Return non-nil when systemd-logind DBus integration is usable."
  (and (fboundp 'dbus-register-signal)
       (eq system-type 'gnu/linux)))

(defun sprite-system--on-prepare-for-sleep (going-to-sleep)
  "Dispatch sleep/wake hooks based on GOING-TO-SLEEP.
GOING-TO-SLEEP is t when the system is suspending, nil on resume."
  (if going-to-sleep
      (run-hooks 'sprite-system-before-sleep-hook)
    (run-hooks 'sprite-system-after-sleep-hook)))

(defun sprite-system-start-logind-watch ()
  "Register DBus signals to run sleep/wake/lock/screensaver hooks.
Cancels any existing registrations first to prevent duplicates."
  (when (sprite-system--logind-available-p)
    (sprite-system-stop-logind-watch)
    (setq sprite-system--logind-signal
          (dbus-register-signal
           :system
           "org.freedesktop.login1"
           "/org/freedesktop/login1"
           "org.freedesktop.login1.Manager"
           "PrepareForSleep"
           #'sprite-system--on-prepare-for-sleep))
    (setq sprite-system--dbus-signals
          (list
           ;; 1. systemd-logind Session Lock
           (dbus-register-signal
            :system
            "org.freedesktop.login1"
            nil
            "org.freedesktop.login1.Session"
            "Lock"
            (lambda ()
              (message "sprite-system: received systemd-logind Lock signal")
              (run-hooks 'sprite-system-before-sleep-hook)))
           ;; 2. systemd-logind Session Unlock
           (dbus-register-signal
            :system
            "org.freedesktop.login1"
            nil
            "org.freedesktop.login1.Session"
            "Unlock"
            (lambda ()
              (message "sprite-system: received systemd-logind Unlock signal")
              (run-hooks 'sprite-system-after-sleep-hook)))
           ;; 3. freedesktop ScreenSaver ActiveChanged (Lock/Unlock)
           (dbus-register-signal
            :session
            nil
            nil
            "org.freedesktop.ScreenSaver"
            "ActiveChanged"
            (lambda (active)
              (message "sprite-system: received ScreenSaver ActiveChanged signal: %s" active)
              (if active
                  (run-hooks 'sprite-system-before-sleep-hook)
                (run-hooks 'sprite-system-after-sleep-hook))))))))

(defun sprite-system-stop-logind-watch ()
  "Unregister the logind PrepareForSleep DBus signal and other signals."
  (when sprite-system--logind-signal
    (dbus-ignore-errors
      (dbus-unregister-object sprite-system--logind-signal))
    (setq sprite-system--logind-signal nil))
  (when sprite-system--dbus-signals
    (dbus-ignore-errors
      (mapc #'dbus-unregister-object sprite-system--dbus-signals))
    (setq sprite-system--dbus-signals nil)))

(provide 'sprite-system)
;;; sprite-system.el ends here
