;;; agent-shell-queue-dispatcher.el --- Adaptive train dispatcher -*- lexical-binding: t; -*-

;;; Commentary:
;; Single adaptive one-shot wakeup timer, bounded non-reentrant dispatch pass,
;; round-robin fair train selection, canonical resource mutation leases,
;; durable dispatch-token lookup, and pluggable effect handlers for durable
;; trains in `agent-shell-queue'.
;;
;; Load-time pure: starts no timers or background work when required.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-queue-train-contract)
(require 'agent-shell-queue-train)

;;; Configuration

(defcustom agent-shell-queue-dispatcher-max-concurrent 4
  "Maximum simultaneous claimed/running mutating attempts globally."
  :type 'integer
  :group 'agent-shell-queue)

(defcustom agent-shell-queue-dispatcher-max-per-pass 8
  "Maximum task claims performed in a single bounded dispatch pass."
  :type 'integer
  :group 'agent-shell-queue)

(defcustom agent-shell-queue-dispatcher-reconcile-interval 60
  "Maximum seconds between reconciliation wakeups while active work exists."
  :type 'integer
  :group 'agent-shell-queue)

;;; Runtime State

(defvar agent-shell-queue-dispatcher--timer nil
  "Single one-shot wakeup timer owned by the coordinator.")

(defvar agent-shell-queue-dispatcher--in-pass nil
  "Non-nil while a dispatch pass is currently executing (prevents reentrancy).")

(defvar agent-shell-queue-dispatcher--wakeup-coalesced nil
  "Non-nil when a wakeup was requested during an active dispatch pass.")

(defvar agent-shell-queue-dispatcher--rr-cursor 0
  "Round-robin offset across eligible active trains.")

(defvar agent-shell-queue-dispatcher--resource-leases (make-hash-table :test #'equal)
  "Map of canonical resource path to (:train-id :task-id :attempt-id :mode).")

(defvar agent-shell-queue-dispatcher--effect-handlers (make-hash-table :test #'eq)
  "Map of effect kind symbol to handler function (lambda (effect train) ...).")

;;; Pluggable Effect Handlers

(defun agent-shell-queue-dispatcher-register-effect-handler (kind handler)
  "Register HANDLER function for declared effect KIND symbol."
  (unless (memq kind agent-shell-queue-train-effect-kinds)
    (user-error "Unknown effect kind: %S" kind))
  (setf (map-elt agent-shell-queue-dispatcher--effect-handlers kind) handler))

(defun agent-shell-queue-dispatcher-effect-handler (kind)
  "Return registered effect handler for KIND, or nil."
  (map-elt agent-shell-queue-dispatcher--effect-handlers kind))

;;; Durable Dispatch Token Index

(defun agent-shell-queue-register-dispatch (dispatch-token item-record)
  "Record ITEM-RECORD under DISPATCH-TOKEN for restart reconciliation."
  (let ((existing (map-elt agent-shell-queue-train--dispatch-index dispatch-token)))
    (unless (member item-record existing)
      (setf (map-elt agent-shell-queue-train--dispatch-index dispatch-token)
            (cons item-record existing))))
  item-record)

(defun agent-shell-queue-find-dispatch (dispatch-token)
  "Look up DISPATCH-TOKEN in durable dispatch index.
Returns plist with `:status' set to `not-found', `found' (with `:item'),
or `ambiguous' (with `:items')."
  (let ((matches (map-elt agent-shell-queue-train--dispatch-index dispatch-token)))
    (pcase (length matches)
      (0 (list :status 'not-found :token dispatch-token))
      (1 (list :status 'found :token dispatch-token :item (car matches)))
      (_ (list :status 'ambiguous :token dispatch-token :items matches)))))

;;; Canonical Resource Leases

(defun agent-shell-queue-dispatcher-acquire-lease (resource train-id task-id attempt-id
                                                            &optional read-only)
  "Acquire a resource lease on RESOURCE for ATTEMPT-ID.
When READ-ONLY is non-nil, succeeds without taking an exclusive mutation lease.
Returns non-nil if lease was granted."
  (if read-only
      t
    (let* ((canon (if (and (stringp resource) (file-name-absolute-p resource))
                      (directory-file-name (expand-file-name resource))
                    resource))
           (holder (map-elt agent-shell-queue-dispatcher--resource-leases canon)))
      (when (or (null holder) (equal (plist-get holder :attempt-id) attempt-id))
        (setf (map-elt agent-shell-queue-dispatcher--resource-leases canon)
              (list :train-id train-id :task-id task-id :attempt-id attempt-id :mode 'mutating))
        t))))

(defun agent-shell-queue-dispatcher-release-lease (resource attempt-id)
  "Release mutation lease on RESOURCE if held by ATTEMPT-ID."
  (let* ((canon (if (and (stringp resource) (file-name-absolute-p resource))
                    (directory-file-name (expand-file-name resource))
                  resource))
         (holder (map-elt agent-shell-queue-dispatcher--resource-leases canon)))
    (when (and holder (equal (plist-get holder :attempt-id) attempt-id))
      (map-delete agent-shell-queue-dispatcher--resource-leases canon)
      t)))

(defun agent-shell-queue-dispatcher--active-lease-count ()
  "Return number of currently held exclusive mutation leases."
  (hash-table-count agent-shell-queue-dispatcher--resource-leases))

;;; Timer Management (Single Adaptive One-Shot Wakeup Owner)

(defun agent-shell-queue-dispatcher-stop ()
  "Cancel the coordinator wakeup timer if active."
  (when (timerp agent-shell-queue-dispatcher--timer)
    (cancel-timer agent-shell-queue-dispatcher--timer))
  (setq agent-shell-queue-dispatcher--timer nil))

(defun agent-shell-queue-dispatcher--next-wakeup-delay (now)
  "Compute next adaptive wakeup delay in seconds from NOW, or nil if idle."
  (let ((active (agent-shell-queue-train-list 'active))
        (nearest nil))
    (when active
      (setq nearest agent-shell-queue-dispatcher-reconcile-interval)
      (dolist (train active)
        (when-let* ((dl (plist-get train :deadline)))
          (let ((rem (max 0.1 (- dl now))))
            (setq nearest (min nearest rem))))
        (dolist (sub (plist-get train :subscriptions))
          (when-let* ((due (plist-get sub :next-reconcile-at)))
            (let ((rem (max 0.1 (- due now))))
              (setq nearest (min nearest rem))))))
      nearest)))

(defun agent-shell-queue-dispatcher-schedule-wakeup (&optional delay)
  "Schedule the single adaptive one-shot wakeup timer after DELAY seconds.
If DELAY is nil, computes the nearest due delay (stopping timer when idle)."
  (agent-shell-queue-dispatcher-stop)
  (let ((d (or delay (agent-shell-queue-dispatcher--next-wakeup-delay (float-time)))))
    (when d
      (setq agent-shell-queue-dispatcher--timer
            (run-with-timer d nil #'agent-shell-queue-dispatcher--timer-fire)))))

(defun agent-shell-queue-dispatcher-request-wakeup ()
  "Request an asynchronous dispatch pass, coalescing reentrant requests."
  (if agent-shell-queue-dispatcher--in-pass
      (setq agent-shell-queue-dispatcher--wakeup-coalesced t)
    (agent-shell-queue-dispatcher-schedule-wakeup 0)))

(defun agent-shell-queue-dispatcher--timer-fire ()
  "Internal timer callback executing one bounded dispatch pass."
  (setq agent-shell-queue-dispatcher--timer nil)
  (agent-shell-queue-dispatcher-run-pass))

;;; Bounded Fair Dispatch Pass

(defun agent-shell-queue-dispatcher--eligible-ready-task (train)
  "Return the first `ready' task in TRAIN, or nil."
  (seq-find (lambda (tk)
              (eq (agent-shell-queue-train--sym (plist-get tk :state)) 'ready))
            (plist-get train :tasks)))

(defun agent-shell-queue-dispatcher--rotate (list offset)
  "Rotate LIST left by OFFSET modulo (length LIST)."
  (if (or (null list) (zerop (length list)))
      nil
    (let ((n (mod offset (length list))))
      (append (nthcdr n list) (seq-take list n)))))

(cl-defun agent-shell-queue-dispatcher-run-pass (&key now max-claims state-dir
                                                      (reschedule t))
  "Execute one bounded, non-reentrant fair dispatch pass at clock NOW.
Returns summary plist (:claimed INT :effects LIST :expired INT :coalesced BOOL)."
  (if agent-shell-queue-dispatcher--in-pass
      (progn
        (setq agent-shell-queue-dispatcher--wakeup-coalesced t)
        (list :claimed 0 :effects nil :expired 0 :coalesced t))
    (let ((agent-shell-queue-dispatcher--in-pass t)
          (ts (or now (float-time)))
          (cap (or max-claims agent-shell-queue-dispatcher-max-per-pass))
          (claimed 0)
          (expired 0)
          (emitted nil))
      (setq agent-shell-queue-dispatcher--wakeup-coalesced nil)
      (unwind-protect
          (let* ((active (agent-shell-queue-train-list 'active))
                 (ordered (agent-shell-queue-dispatcher--rotate
                           active agent-shell-queue-dispatcher--rr-cursor)))
            (dolist (train ordered)
              (let ((tid (plist-get train :id))
                    (dl (plist-get train :deadline)))
                (cond
                 ;; Expire trains past absolute deadline
                 ((and dl (> ts dl))
                  (let* ((ev (list :event-id (format "expire-%s-%d" tid (floor ts))
                                   :type 'resume
                                   :generation (plist-get train :generation)))
                         (step (agent-shell-queue-train-step train ev ts))
                         (new-tr (plist-get step :train)))
                    (setf (map-elt agent-shell-queue-train--store tid) new-tr)
                    (when state-dir (agent-shell-queue-train-save-record new-tr state-dir))
                    (cl-incf expired)))
                 ;; Claim at most one ready task per train per round-robin turn
                 ((and (< claimed cap)
                       (< (agent-shell-queue-dispatcher--active-lease-count)
                          agent-shell-queue-dispatcher-max-concurrent))
                  (when-let* ((task (agent-shell-queue-dispatcher--eligible-ready-task train)))
                    (let* ((task-id (plist-get task :id))
                           (read-only (plist-get task :read-only))
                           (resource (plist-get train :resource))
                           (att-id (agent-shell-queue-train--generate-id "att"))
                           (dtok (format "dt:%s:%s:%s" tid task-id att-id)))
                      (when (agent-shell-queue-dispatcher-acquire-lease
                             resource tid task-id att-id read-only)
                        (let* ((ev (list :event-id (format "claim-%s" dtok)
                                         :type 'claim-task
                                         :generation (plist-get train :generation)
                                         :payload (list :task-id task-id
                                                        :attempt-id att-id
                                                        :dispatch-token dtok
                                                        :repair-p (plist-get task :repair-p))))
                               (step (agent-shell-queue-train-step train ev ts))
                               (new-tr (plist-get step :train)))
                          (if (eq (plist-get step :outcome) 'accepted)
                              (progn
                                (setf (map-elt agent-shell-queue-train--store tid) new-tr)
                                (when state-dir
                                  (agent-shell-queue-train-save-record new-tr state-dir))
                                (agent-shell-queue-register-dispatch
                                 dtok (list :train-id tid :task-id task-id :attempt-id att-id))
                                (dolist (eff (plist-get step :effects))
                                  (push eff emitted)
                                  (when-let* ((fn (agent-shell-queue-dispatcher-effect-handler
                                                   (plist-get eff :kind))))
                                    (funcall fn eff new-tr)))
                                (cl-incf claimed))
                            (agent-shell-queue-dispatcher-release-lease resource att-id))))))))))
            (when active
              (setq agent-shell-queue-dispatcher--rr-cursor
                    (mod (1+ agent-shell-queue-dispatcher--rr-cursor) (length active)))))
        (when reschedule
          (if agent-shell-queue-dispatcher--wakeup-coalesced
              (agent-shell-queue-dispatcher-schedule-wakeup 0)
            (agent-shell-queue-dispatcher-schedule-wakeup))))
      (list :claimed claimed :effects (nreverse emitted) :expired expired
            :coalesced agent-shell-queue-dispatcher--wakeup-coalesced))))

(provide 'agent-shell-queue-dispatcher)
;;; agent-shell-queue-dispatcher.el ends here
