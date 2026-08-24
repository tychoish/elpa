;;; magit-dash-timer.el --- Scheduled auto-sync for magit-dash repos -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides `magit-dash-register-sync-timer' for scheduling periodic
;; auto-sync of registered repositories.  Guards prevent syncing when
;; the network is unavailable, when Emacs has not been idle long enough,
;; or when the global minimum sync interval has not elapsed.

;;; Code:

(require 'magit-dash)
(require 'map)
(require 'seq)

;;;; Configuration

(defvar magit-dash-sync-min-interval 300
  "Minimum seconds that must elapse between timer-triggered sync attempts.
Applied globally across all registered timers.")

(defvar magit-dash-network-check-function #'magit-dash--network-available-p
  "Function called with no arguments to test network availability.
Return non-nil when syncing is safe, nil to skip.
Override to implement custom connectivity detection.")

;;;; State

(defvar magit-dash--sync-timers (make-hash-table :test #'equal)
  "Active sync timers keyed by registration name.
Values are timer objects from `run-with-timer' or `run-with-idle-timer'.")

(defvar magit-dash--last-sync-attempt 0.0
  "Float-time of the most recent timer-triggered sync attempt.
Initialised to 0.0 so the first firing always passes the interval guard.")

;;;; Guards

(defun magit-dash--network-available-p ()
  "Return non-nil when at least one non-loopback network interface is configured."
  (seq-some (lambda (iface)
              (not (string-prefix-p "lo" (car iface))))
            (network-interface-list)))

(defun magit-dash--idle-for-p (seconds)
  "Return non-nil when Emacs has been idle for at least SECONDS."
  (when-let* ((idle (current-idle-time)))
    (>= (float-time idle) seconds)))

(defun magit-dash--global-interval-elapsed-p ()
  "Return non-nil when `magit-dash-sync-min-interval' seconds have elapsed
since `magit-dash--last-sync-attempt'."
  (>= (- (float-time) magit-dash--last-sync-attempt)
      magit-dash-sync-min-interval))

;;;; Internal sync runner

(defun magit-dash--timer-sync (repos last-edit-threshold)
  "Attempt a timer-triggered auto-sync for REPOS, subject to guard conditions.
REPOS is a list of repo name strings; nil means all repos with auto-sync steps.
LAST-EDIT-THRESHOLD when non-nil skips sync unless Emacs has been idle
for at least that many seconds.

Skips silently when:
  - `magit-dash-network-check-function' returns nil;
  - `magit-dash--global-interval-elapsed-p' returns nil;
  - LAST-EDIT-THRESHOLD is set and `magit-dash--idle-for-p' returns nil;
  - no candidate repos have any auto-sync steps configured."
  (when (and (funcall magit-dash-network-check-function)
             (magit-dash--global-interval-elapsed-p)
             (or (null last-edit-threshold)
                 (magit-dash--idle-for-p last-edit-threshold)))
    (setq magit-dash--last-sync-attempt (float-time))
    (let* ((magit-dash-sync-trigger 'timer)
           (candidates (if repos
                           (seq-filter (lambda (r)
                                         (member (magit-dash-repo-name r) repos))
                                       magit-dash-repo-list)
                         magit-dash-repo-list))
           (targets (seq-filter #'magit-dash--auto-sync-steps candidates)))
      (when targets
        (magit-dash--batch-run targets #'magit-dash--auto-sync-async "timer-sync")))))

;;;; Public API

(cl-defun magit-dash-register-sync-timer
    (&key name repos (kind 'interval) (interval 300) (idle-delay 120)
          (last-edit-threshold nil))
  "Register a recurring auto-sync timer identified by NAME.

Registering a second timer with the same NAME cancels the first.

REPOS is a list of repo name strings to sync; nil syncs all repos
that have at least one auto-sync step configured.

KIND controls scheduling:
  `interval'  — fires every INTERVAL seconds (default 300).
  `idle'      — fires after IDLE-DELAY seconds of continuous idleness
                (default 120).

LAST-EDIT-THRESHOLD (seconds) — with KIND `interval', skip the sync
unless Emacs has also been idle for at least this many seconds.
Has no effect with KIND `idle', where IDLE-DELAY already serves
that purpose.

Every firing is subject to the global guards:
  - `magit-dash-network-check-function' must return non-nil;
  - `magit-dash-sync-min-interval' seconds must have elapsed since the
    last timer-triggered sync attempt;
  - repos with no auto-sync steps are silently skipped."
  (magit-dash-cancel-sync-timer name)
  (let ((fn (lambda ()
              (magit-dash--timer-sync repos last-edit-threshold))))
    (setf (map-elt magit-dash--sync-timers name)
          (cond
           ((eq kind 'idle)
            (run-with-idle-timer idle-delay t fn))
           (t
            (run-with-timer interval interval fn))))))

(defun magit-dash-cancel-sync-timer (name)
  "Cancel the sync timer registered under NAME, if any."
  (when-let* ((timer (map-elt magit-dash--sync-timers name)))
    (cancel-timer timer)
    (map-delete magit-dash--sync-timers name)))

(defun magit-dash-cancel-all-sync-timers ()
  "Cancel and remove all registered auto-sync timers."
  (interactive)
  (seq-do #'magit-dash-cancel-sync-timer
          (map-keys magit-dash--sync-timers)))

(provide 'magit-dash-timer)
;;; magit-dash-timer.el ends here
