;;; agent-shell-queue-train.el --- Durable staged trains and event inbox -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure state machine, revisioned lifecycle commands, durable event inbox,
;; atomic JSON persistence with last-valid snapshot retention, and exclusive
;; owner locking for durable work trains in `agent-shell-queue'.
;;
;; This module has zero dependencies on workflow, HITL, GitHub, or mail
;; packages and starts no timers or agent work when loaded.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-queue-train-contract)

(declare-function sprite-state-path "sprite" (&rest parts))

;;; Configuration and In-Memory Coordinator State

(defvar agent-shell-queue-train-state-dir nil
  "Optional override directory for durable train persistence.
When nil, resolved lazily via `sprite-state-path' or `user-emacs-directory'.")

(defvar agent-shell-queue-train--store (make-hash-table :test #'equal)
  "In-memory map of train ID string to train aggregate plist.")

(defvar agent-shell-queue-train--inbox-index (make-hash-table :test #'equal)
  "In-memory map of event ID string to (:digest STR :receipt PLIST :event PLIST).")

(defvar agent-shell-queue-train--inbox-seq 0
  "Monotonic sequence counter for committed inbox events.")

(defvar agent-shell-queue-train--dispatch-index (make-hash-table :test #'equal)
  "In-memory map of dispatch-token string to list of matching item records.")

(defvar agent-shell-queue-train--circuit-breaker-paused nil
  "Snapshot list of train IDs paused by the last circuit-breaker batch pause.")

(defun agent-shell-queue-train-state-directory ()
  "Return the resolved state directory path for durable trains."
  (or agent-shell-queue-train-state-dir
      (if (fboundp 'sprite-state-path)
          (sprite-state-path "asq-trains")
        (expand-file-name "asq-trains" user-emacs-directory))))

(defun agent-shell-queue-train--digest (obj)
  "Return a deterministic SHA-256 hex digest for plain-data OBJ."
  (secure-hash 'sha256 (agent-shell-queue-train-to-json-string obj)))

(defun agent-shell-queue-train--generate-id (prefix)
  "Generate a unique opaque identifier starting with PREFIX."
  (format "%s-%d-%06x" prefix (floor (float-time)) (random #xffffff)))

(defun agent-shell-queue-train--copy-plist (plist)
  "Return a shallow copy of PLIST."
  (copy-sequence plist))

(defun agent-shell-queue-train--sym (val)
  "Normalize string or symbol VAL to a symbol."
  (cond ((symbolp val) val)
        ((stringp val) (intern val))
        (t nil)))

;;; Task Graph & Budget Helpers

(defun agent-shell-queue-train--task-adjacency (tasks)
  "Build adjacency alist (TASK-ID . DEPENDS-ON) from TASKS list."
  (mapcar (lambda (task)
            (cons (plist-get task :id) (plist-get task :depends-on)))
          tasks))

(defun agent-shell-queue-train--find-task (tasks task-id)
  "Return task plist with :id equal to TASK-ID from TASKS."
  (seq-find (lambda (task) (equal (plist-get task :id) task-id)) tasks))

(defun agent-shell-queue-train--replace-task (tasks task-id new-task)
  "Return new list of TASKS with TASK-ID replaced by NEW-TASK."
  (mapcar (lambda (task)
            (if (equal (plist-get task :id) task-id) new-task task))
          tasks))

(defun agent-shell-queue-train--recompute-readiness (tasks)
  "Promote `pending' tasks whose dependencies are all `succeeded' to `ready'."
  (let ((succeeded (make-hash-table :test #'equal)))
    (dolist (task tasks)
      (when (eq (agent-shell-queue-train--sym (plist-get task :state)) 'succeeded)
        (setf (map-elt succeeded (plist-get task :id)) t)))
    (mapcar
     (lambda (task)
       (if (and (eq (agent-shell-queue-train--sym (plist-get task :state)) 'pending)
                (seq-every-p (lambda (dep) (map-elt succeeded dep))
                             (plist-get task :depends-on)))
           (plist-put (copy-sequence task) :state 'ready)
         task))
     tasks)))

(defun agent-shell-queue-train--all-terminal-succeeded-p (tasks)
  "Return non-nil when TASKS is non-empty and every task is `succeeded'."
  (and tasks
       (seq-every-p (lambda (task)
                      (eq (agent-shell-queue-train--sym (plist-get task :state)) 'succeeded))
                    tasks)))

(defun agent-shell-queue-train--usage-get (train key)
  "Return integer usage counter KEY from TRAIN's :usage plist."
  (or (plist-get (plist-get train :usage) key) 0))

(defun agent-shell-queue-train--usage-inc (train key)
  "Return new :usage plist for TRAIN with KEY incremented by 1."
  (let ((usage (copy-sequence (or (plist-get train :usage) '()))))
    (plist-put usage key (1+ (or (plist-get usage key) 0)))))

(defun agent-shell-queue-train--budget-exhausted-p (train usage-key limit-key)
  "Return non-nil if TRAIN has reached or exceeded LIMIT-KEY for USAGE-KEY."
  (let ((limit (plist-get (plist-get train :limits) limit-key))
        (used (agent-shell-queue-train--usage-get train usage-key)))
    (and limit (>= used limit))))

;;; Pure State Transition Engine

(defun agent-shell-queue-train--commit-event (train event-id digest now)
  "Return updated TRAIN recording consumed EVENT-ID with DIGEST at NOW."
  (let* ((consumed (copy-sequence (plist-get train :consumed-events)))
         (entry (list :event-id event-id :digest digest :at now))
         (updated (plist-put (copy-sequence train) :consumed-events
                             (cons entry consumed))))
    (plist-put updated :revision (1+ (or (plist-get train :revision) 0)))))

(defun agent-shell-queue-train--check-consumed (train event-id digest)
  "Check if EVENT-ID was already consumed in TRAIN.
Returns nil (new), `duplicate' (same digest), or `conflict' (different digest)."
  (when-let* ((prior (seq-find (lambda (e) (equal (plist-get e :event-id) event-id))
                               (plist-get train :consumed-events))))
    (if (equal (plist-get prior :digest) digest)
        'duplicate
      'conflict)))

(defun agent-shell-queue-train-step (train event now)
  "Pure transition function applying EVENT to TRAIN at clock NOW.
Returns (:train NEW-TRAIN :effects EFFECTS :outcome OUTCOME :reason REASON)."
  (let* ((event-id (plist-get event :event-id))
         (ev-type (agent-shell-queue-train--sym (plist-get event :type)))
         (ev-gen (plist-get event :generation))
         (train-gen (or (plist-get train :generation) 1))
         (state (agent-shell-queue-train--sym (plist-get train :state)))
         (deadline (plist-get train :deadline))
         (payload (plist-get event :payload))
         (digest (agent-shell-queue-train--digest (or payload '(:empty t))))
         (prior (and event-id (agent-shell-queue-train--check-consumed train event-id digest))))
    (cond
     ((eq prior 'duplicate)
      (list :train train :effects nil :outcome 'duplicate :reason "already-consumed"))
     ((eq prior 'conflict)
      (list :train train :effects nil :outcome 'conflict :reason "conflicting-event-payload"))
     ((and ev-gen (/= ev-gen train-gen))
      (list :train train :effects nil :outcome 'conflict :reason "stale-generation"))
     ((memq state agent-shell-queue-train-terminal-states)
      (list :train train :effects nil :outcome 'conflict
            :reason (format "train-terminal:%s" state)))
     ((and deadline (> now deadline) (not (eq ev-type 'cancel)))
      (let* ((t1 (agent-shell-queue-train--commit-event train (or event-id "deadline") digest now))
             (t2 (plist-put t1 :state 'expired))
             (t3 (plist-put t2 :expired-at now)))
        (list :train t3 :effects nil :outcome 'accepted :reason "deadline-expired")))
     (t
      (agent-shell-queue-train--apply-event train ev-type event-id digest payload now)))))

(defun agent-shell-queue-train--apply-event (train ev-type event-id digest payload now)
  "Apply validated EV-TYPE with PAYLOAD to TRAIN at NOW."
  (let ((state (agent-shell-queue-train--sym (plist-get train :state)))
        (tasks (plist-get train :tasks)))
    (pcase ev-type
      ('activate
       (if (not (eq state 'staged))
           (list :train train :effects nil :outcome 'conflict :reason "not-staged")
         (let* ((t1 (agent-shell-queue-train--commit-event train event-id digest now))
                (t2 (plist-put t1 :state 'active))
                (t3 (plist-put t2 :activated-at now))
                (t4 (plist-put t3 :tasks (agent-shell-queue-train--recompute-readiness tasks))))
           (list :train t4 :effects nil :outcome 'accepted :reason "activated"))))
      ('pause
       (if (not (eq state 'active))
           (list :train train :effects nil :outcome 'conflict :reason "not-active")
         (let* ((t1 (agent-shell-queue-train--commit-event train event-id digest now))
                (t2 (plist-put t1 :state 'paused))
                (t3 (plist-put t2 :paused-at now)))
           (list :train t3 :effects nil :outcome 'accepted :reason "paused"))))
      ('resume
       (if (not (eq state 'paused))
           (list :train train :effects nil :outcome 'conflict :reason "not-paused")
         (let* ((t1 (agent-shell-queue-train--commit-event train event-id digest now))
                (t2 (plist-put t1 :state 'active))
                (t3 (plist-put t2 :tasks (agent-shell-queue-train--recompute-readiness tasks))))
           (list :train t3 :effects nil :outcome 'accepted :reason "resumed"))))
      ('cancel
       (let* ((interrupt (or (agent-shell-queue-train--sym (plist-get payload :interrupt))
                             'after-turn))
              (t1 (agent-shell-queue-train--commit-event train event-id digest now))
              (t2 (plist-put t1 :state 'cancelled))
              (t3 (plist-put t2 :generation (1+ (or (plist-get train :generation) 1))))
              (t4 (plist-put t3 :cancel-policy interrupt))
              (t5 (plist-put t4 :cancelled-at now))
              (active-effects
               (when (eq interrupt 'interrupt)
                 (list (list :kind 'apply-authorized-operation
                             :action 'interrupt-owned-attempts
                             :train-id (plist-get train :id))))))
         (list :train t5 :effects active-effects :outcome 'accepted :reason "cancelled")))
      ('claim-task
       (let* ((task-id (plist-get payload :task-id))
              (task (agent-shell-queue-train--find-task tasks task-id))
              (is-repair (plist-get payload :repair-p)))
         (cond
          ((not (eq state 'active))
           (list :train train :effects nil :outcome 'conflict :reason "train-not-active"))
          ((null task)
           (list :train train :effects nil :outcome 'invalid :reason "unknown-task"))
          ((not (eq (agent-shell-queue-train--sym (plist-get task :state)) 'ready))
           (list :train train :effects nil :outcome 'conflict :reason "task-not-ready"))
          ((agent-shell-queue-train--budget-exhausted-p train :turns :max-turns)
           (let* ((t1 (agent-shell-queue-train--commit-event train event-id digest now))
                  (t2 (plist-put t1 :state 'paused))
                  (t3 (plist-put t2 :pause-reason "max-turns-exhausted")))
             (list :train t3 :effects nil :outcome 'conflict :reason "max-turns-exhausted")))
          ((and is-repair
                (agent-shell-queue-train--budget-exhausted-p train :repairs :max-repairs))
           (let* ((t1 (agent-shell-queue-train--commit-event train event-id digest now))
                  (t2 (plist-put t1 :state 'paused))
                  (t3 (plist-put t2 :pause-reason "max-repairs-exhausted")))
             (list :train t3 :effects nil :outcome 'conflict :reason "max-repairs-exhausted")))
          (t
           (let* ((attempt-id (or (plist-get payload :attempt-id)
                                  (agent-shell-queue-train--generate-id "att")))
                  (dispatch-token (or (plist-get payload :dispatch-token)
                                      (format "dt:%s:%s:%s"
                                              (plist-get train :id) task-id attempt-id)))
                  (exec-token (or (plist-get payload :execution-token)
                                  (agent-shell-queue-train--generate-id "etok")))
                  (new-task (thread-first (copy-sequence task)
                              (plist-put :state 'claimed)
                              (plist-put :current-attempt attempt-id)
                              (plist-put :dispatch-token dispatch-token)
                              (plist-put :execution-token exec-token)))
                  (attempt-rec (list :attempt-id attempt-id
                                     :task-id task-id
                                     :dispatch-token dispatch-token
                                     :execution-token exec-token
                                     :generation (or (plist-get train :generation) 1)
                                     :claimed-at now
                                     :status 'claimed
                                     :partial-results nil
                                     :final-result nil))
                  (usage (agent-shell-queue-train--usage-inc train :turns))
                  (usage (if is-repair
                             (let ((u (copy-sequence usage)))
                               (plist-put u :repairs (1+ (or (plist-get u :repairs) 0))))
                           usage))
                  (effect (list :kind 'dispatch
                                :train-id (plist-get train :id)
                                :task-id task-id
                                :attempt-id attempt-id
                                :dispatch-token dispatch-token
                                :execution-token exec-token
                                :resource (plist-get train :resource)))
                  (t1 (agent-shell-queue-train--commit-event train event-id digest now))
                  (t2 (plist-put t1 :usage usage))
                  (t3 (plist-put t2 :tasks (agent-shell-queue-train--replace-task tasks task-id new-task)))
                  (t4 (plist-put t3 :attempts (cons attempt-rec (plist-get train :attempts))))
                  (t5 (plist-put t4 :pending-effects (cons effect (plist-get train :pending-effects)))))
             (list :train t5 :effects (list effect) :outcome 'accepted :reason "claimed"))))))
      ('task-waiting
       (let* ((task-id (plist-get payload :task-id))
              (task (agent-shell-queue-train--find-task tasks task-id))
              (sub (plist-get payload :subscription)))
         (if (null task)
             (list :train train :effects nil :outcome 'invalid :reason "unknown-task")
           (let* ((new-task (plist-put (copy-sequence task) :state 'waiting))
                  (t1 (agent-shell-queue-train--commit-event train event-id digest now))
                  (t2 (plist-put t1 :tasks (agent-shell-queue-train--replace-task tasks task-id new-task)))
                  (t3 (if sub
                          (plist-put t2 :subscriptions (cons sub (plist-get train :subscriptions)))
                        t2)))
             (list :train t3 :effects nil :outcome 'accepted :reason "waiting")))))
      ('task-result
       (agent-shell-queue-train--apply-task-result train event-id digest payload now))
      ('task-completed
       (let* ((task-id (plist-get payload :task-id))
              (task (agent-shell-queue-train--find-task tasks task-id)))
         (if (null task)
             (list :train train :effects nil :outcome 'invalid :reason "unknown-task")
           (let* ((new-task (plist-put (copy-sequence task) :state 'succeeded))
                  (new-tasks (agent-shell-queue-train--recompute-readiness
                              (agent-shell-queue-train--replace-task tasks task-id new-task)))
                  (all-done (agent-shell-queue-train--all-terminal-succeeded-p new-tasks))
                  (t1 (agent-shell-queue-train--commit-event train event-id digest now))
                  (t2 (plist-put t1 :tasks new-tasks))
                  (t3 (if all-done
                          (plist-put (plist-put t2 :state 'completed) :completed-at now)
                        t2)))
             (list :train t3 :effects nil :outcome 'accepted
                   :reason (if all-done "train-completed" "task-completed"))))))
      ('task-failed
       (let* ((task-id (plist-get payload :task-id))
              (task (agent-shell-queue-train--find-task tasks task-id))
              (retryable (plist-get payload :retryable-p)))
         (if (null task)
             (list :train train :effects nil :outcome 'invalid :reason "unknown-task")
           (let* ((exhausted (agent-shell-queue-train--budget-exhausted-p
                              train :retries :max-retries))
                  (next-state (if (and retryable (not exhausted)) 'ready 'blocked))
                  (usage (if (and retryable (not exhausted))
                             (agent-shell-queue-train--usage-inc train :retries)
                           (plist-get train :usage)))
                  (new-task (plist-put (copy-sequence task) :state next-state))
                  (t1 (agent-shell-queue-train--commit-event train event-id digest now))
                  (t2 (plist-put t1 :usage usage))
                  (t3 (plist-put t2 :tasks (agent-shell-queue-train--replace-task tasks task-id new-task))))
             (list :train t3 :effects nil :outcome 'accepted
                   :reason (if (eq next-state 'ready) "retry-scheduled" "task-blocked"))))))
      ('observation
       (if (agent-shell-queue-train--budget-exhausted-p train :observations :max-observations)
           (let* ((t1 (agent-shell-queue-train--commit-event train event-id digest now))
                  (t2 (plist-put t1 :state 'paused))
                  (t3 (plist-put t2 :pause-reason "max-observations-exhausted")))
             (list :train t3 :effects nil :outcome 'conflict :reason "max-observations-exhausted"))
         (let* ((t1 (agent-shell-queue-train--commit-event train event-id digest now))
                (t2 (plist-put t1 :usage (agent-shell-queue-train--usage-inc train :observations)))
                (t3 (plist-put t2 :last-observation payload)))
           (list :train t3 :effects nil :outcome 'accepted :reason "observation-recorded"))))
      (_
       (list :train train :effects nil :outcome 'invalid
             :reason (format "unknown-event-type:%S" ev-type))))))

(defun agent-shell-queue-train--apply-task-result (train event-id digest payload now)
  "Record partial or final attempt result PAYLOAD on TRAIN at NOW.
Enforces at most one final result per attempt; conflicting finals block."
  (let* ((task-id (plist-get payload :task-id))
         (attempt-id (plist-get payload :attempt-id))
         (token (plist-get payload :execution-token))
         (final-p (plist-get payload :final))
         (attempts (plist-get train :attempts))
         (att (seq-find (lambda (a) (equal (plist-get a :attempt-id) attempt-id)) attempts)))
    (cond
     ((null att)
      (list :train train :effects nil :outcome 'invalid :reason "unknown-attempt"))
     ((not (equal (plist-get att :execution-token) token))
      (list :train train :effects nil :outcome 'forbidden :reason "invalid-execution-token"))
     ((and final-p (plist-get att :final-result))
      (let* ((existing (plist-get att :final-result))
             (same-p (equal (plist-get existing :idempotency-key)
                            (plist-get payload :idempotency-key))))
        (if same-p
            (list :train train :effects nil :outcome 'duplicate :reason "duplicate-final-result")
          ;; Conflicting final result blocks the task visibly
          (let* ((tasks (plist-get train :tasks))
                 (task (agent-shell-queue-train--find-task tasks task-id))
                 (new-task (when task
                             (plist-put (plist-put (copy-sequence task) :state 'blocked)
                                        :block-reason "conflicting-final-result")))
                 (t1 (agent-shell-queue-train--commit-event train event-id digest now))
                 (t2 (if new-task
                         (plist-put t1 :tasks (agent-shell-queue-train--replace-task tasks task-id new-task))
                       t1)))
            (list :train t2 :effects nil :outcome 'conflict :reason "conflicting-final-result")))))
     (t
      (let* ((new-att (if final-p
                          (plist-put (copy-sequence att) :final-result payload)
                        (plist-put (copy-sequence att) :partial-results
                                   (cons payload (plist-get att :partial-results)))))
             (new-attempts (mapcar (lambda (a)
                                     (if (equal (plist-get a :attempt-id) attempt-id) new-att a))
                                   attempts))
             (t1 (agent-shell-queue-train--commit-event train event-id digest now))
             (t2 (plist-put t1 :attempts new-attempts)))
        (list :train t2 :effects nil :outcome 'accepted
              :reason (if final-p "final-result-recorded" "partial-result-recorded")))))))

;;; Owner Lock and Atomic Persistence

(defun agent-shell-queue-train--pid-alive-p (pid)
  "Return non-nil when local OS process PID is alive."
  (and (integerp pid)
       (> pid 0)
       (or (= pid (emacs-pid))
           (and (file-exists-p (format "/proc/%d" pid)) t)
           (= 0 (call-process "kill" nil nil nil "-0" (number-to-string pid))))))

(defun agent-shell-queue-train-acquire-lock (&optional state-dir)
  "Acquire exclusive coordinator owner lock in STATE-DIR.
Signals `user-error' if another live process owns the lock.  Reclaims stale
locks whose owner PID is no longer alive.  Returns lock directory path."
  (let* ((dir (or state-dir (agent-shell-queue-train-state-directory)))
         (lock-dir (expand-file-name "owner.lock" dir))
         (owner-file (expand-file-name "owner.json" lock-dir)))
    (make-directory dir t)
    (when (file-directory-p lock-dir)
      (let* ((existing (when (file-regular-p owner-file)
                         (ignore-errors
                           (with-temp-buffer
                             (insert-file-contents owner-file)
                             (agent-shell-queue-train-from-json-string (buffer-string))))))
             (pid (plist-get existing :pid))
             (host (plist-get existing :host)))
        (if (and pid (equal host (system-name))
                 (not (= pid (emacs-pid)))
                 (agent-shell-queue-train--pid-alive-p pid))
            (user-error "Train state directory %s is locked by live PID %d" dir pid)
          (ignore-errors (delete-directory lock-dir t)))))
    (make-directory lock-dir t)
    (with-temp-file owner-file
      (insert (agent-shell-queue-train-to-json-string
               (list :pid (emacs-pid)
                     :host (system-name)
                     :acquired-at (floor (float-time))))))
    lock-dir))

(defun agent-shell-queue-train-release-lock (&optional state-dir)
  "Release coordinator owner lock in STATE-DIR if owned by current process."
  (let* ((dir (or state-dir (agent-shell-queue-train-state-directory)))
         (lock-dir (expand-file-name "owner.lock" dir))
         (owner-file (expand-file-name "owner.json" lock-dir)))
    (when (file-directory-p lock-dir)
      (let* ((existing (when (file-regular-p owner-file)
                         (ignore-errors
                           (with-temp-buffer
                             (insert-file-contents owner-file)
                             (agent-shell-queue-train-from-json-string (buffer-string))))))
             (pid (plist-get existing :pid)))
        (when (or (null pid) (= pid (emacs-pid)))
          (delete-directory lock-dir t))))))

(defun agent-shell-queue-train--train-file (train-id &optional state-dir)
  "Return canonical JSON persistence file path for TRAIN-ID in STATE-DIR."
  (expand-file-name (format "train-%s.json" train-id)
                    (expand-file-name "trains"
                                      (or state-dir (agent-shell-queue-train-state-directory)))))

(defun agent-shell-queue-train-save-record (train &optional state-dir)
  "Atomically persist TRAIN record to STATE-DIR, retaining last valid backup."
  (let* ((id (plist-get train :id))
         (path (agent-shell-queue-train--train-file id state-dir))
         (dir (file-name-directory path))
         (tmp (concat path ".tmp"))
         (bak (concat path ".bak"))
         (json-str (agent-shell-queue-train-to-json-string train)))
    (make-directory dir t)
    (when (file-regular-p path)
      (copy-file path bak t))
    (with-temp-file tmp
      (insert json-str))
    (rename-file tmp path t)
    path))

(defun agent-shell-queue-train-load-record (train-id &optional state-dir)
  "Load TRAIN-ID record from STATE-DIR, validating schema version.
Signals an error on corrupt JSON or newer unsupported schema without
overwriting the file.  Returns (:train PLIST :recovered-from-backup BOOL)."
  (let* ((path (agent-shell-queue-train--train-file train-id state-dir))
         (bak (concat path ".bak")))
    (cl-labels
        ((read-one (file)
           (with-temp-buffer
             (insert-file-contents file)
             (let* ((rec (agent-shell-queue-train-from-json-string (buffer-string)))
                    (sv (agent-shell-queue-train-validate-schema-version
                         (plist-get rec :schema-version))))
               (unless (plist-get sv :ok)
                 (error "Invalid train schema in %s: %s"
                        file (string-join (plist-get sv :errors) "; ")))
               rec))))
      (condition-case primary-err
          (list :train (read-one path) :recovered-from-backup nil)
        (error
         (if (and (file-regular-p bak)
                  (not (string-match-p "Unsupported newer schema"
                                       (error-message-string primary-err))))
             (list :train (read-one bak) :recovered-from-backup t)
           (signal (car primary-err) (cdr primary-err))))))))

;;; Durable Event Inbox

(defun agent-shell-queue-publish-event (event &optional state-dir)
  "Persist incoming EVENT to durable inbox before returning acceptance receipt.
Returns outcome plist with `:outcome' (`accepted', `duplicate', `conflict',
or `invalid'), `:event-id', and `:seq'."
  (let ((check (agent-shell-queue-train-validate-event event)))
    (if (not (plist-get check :ok))
        (agent-shell-queue-train-make-outcome
         'invalid :errors (plist-get check :errors))
      (let* ((event-id (plist-get event :event-id))
             (digest (agent-shell-queue-train--digest (or (plist-get event :payload) '(:empty t))))
             (existing (map-elt agent-shell-queue-train--inbox-index event-id)))
        (cond
         ((and existing (equal (plist-get existing :digest) digest))
          (plist-put (copy-sequence (plist-get existing :receipt)) :outcome 'duplicate))
         (existing
          (agent-shell-queue-train-make-outcome
           'conflict :event-id event-id :reason "conflicting-inbox-event"))
         (t
          (let* ((seq-num (cl-incf agent-shell-queue-train--inbox-seq))
                 (dir (expand-file-name "inbox"
                                        (or state-dir (agent-shell-queue-train-state-directory))))
                 (file (expand-file-name (format "%08d-%s.json" seq-num event-id) dir))
                 (tmp (concat file ".tmp"))
                 (stored (plist-put (copy-sequence event) :seq seq-num))
                 (receipt (agent-shell-queue-train-make-outcome
                           'accepted :event-id event-id :seq seq-num :digest digest)))
            (make-directory dir t)
            (with-temp-file tmp
              (insert (agent-shell-queue-train-to-json-string stored)))
            (rename-file tmp file t)
            (setf (map-elt agent-shell-queue-train--inbox-index event-id)
                  (list :digest digest :receipt receipt :event stored :file file))
            receipt)))))))

;;; Public Revisioned Train Commands

(cl-defun agent-shell-queue-train-create (&key id definition resource authorization
                                               limits tasks deadline now state-dir)
  "Create a new durable train in `staged' state.
Validates LIMITS, AUTHORIZATION, and TASKS DAG.  Never starts work on creation."
  (let ((lim-check (agent-shell-queue-train-validate-limits limits))
        (auth-check (agent-shell-queue-train-validate-authorization authorization)))
    (unless (plist-get lim-check :ok)
      (user-error "Invalid train limits: %s" (string-join (plist-get lim-check :errors) "; ")))
    (unless (plist-get auth-check :ok)
      (user-error "Invalid train authorization: %s" (string-join (plist-get auth-check :errors) "; ")))
    (when (agent-shell-queue-train-dag-cycle-p (agent-shell-queue-train--task-adjacency tasks))
      (user-error "Train task graph contains a cycle"))
    (let* ((ts (or now (float-time)))
           (train-id (or id (agent-shell-queue-train--generate-id "train")))
           (norm-tasks (mapcar (lambda (tk)
                                 (if (plist-get tk :state)
                                     tk
                                   (plist-put (copy-sequence tk) :state 'pending)))
                               tasks))
           (record (list :schema-version agent-shell-queue-train-schema-version
                         :id train-id
                         :revision 1
                         :generation 1
                         :state 'staged
                         :definition definition
                         :resource resource
                         :authorization authorization
                         :limits limits
                         :usage '(:turns 0 :retries 0 :repairs 0 :observations 0)
                         :deadline (or deadline (+ ts (plist-get limits :max-duration)))
                         :created-at ts
                         :tasks norm-tasks
                         :attempts nil
                         :subscriptions nil
                         :pending-effects nil
                         :consumed-events nil
                         :proposed-patches nil
                         :reviews nil)))
      (setf (map-elt agent-shell-queue-train--store train-id) record)
      (when state-dir
        (agent-shell-queue-train-save-record record state-dir))
      (agent-shell-queue-train-make-outcome
       'accepted :id train-id :state 'staged :revision 1))))

(defun agent-shell-queue-train-status (train-id)
  "Return plain-data record for TRAIN-ID without side effects, or nil."
  (when-let* ((rec (map-elt agent-shell-queue-train--store train-id)))
    (copy-sequence rec)))

(defun agent-shell-queue-train-list (&optional state-filter)
  "Return list of plain-data train records, optionally filtered by STATE-FILTER."
  (let (out)
    (map-do (lambda (_id rec)
              (when (or (null state-filter)
                        (eq (agent-shell-queue-train--sym (plist-get rec :state))
                            (agent-shell-queue-train--sym state-filter)))
                (push (copy-sequence rec) out)))
            agent-shell-queue-train--store)
    (nreverse out)))

(defun agent-shell-queue-train--mutate (train-id expected-revision event-type payload now state-dir)
  "Apply revision-checked transition EVENT-TYPE to TRAIN-ID."
  (let ((train (map-elt agent-shell-queue-train--store train-id)))
    (cond
     ((null train)
      (agent-shell-queue-train-make-outcome 'invalid :id train-id :reason "train-not-found"))
     ((and expected-revision (/= expected-revision (plist-get train :revision)))
      (agent-shell-queue-train-make-outcome
       'conflict :id train-id :reason "stale-revision"
       :current-revision (plist-get train :revision)))
     (t
      (let* ((ts (or now (float-time)))
             (ev (list :schema-version agent-shell-queue-train-schema-version
                       :event-id (agent-shell-queue-train--generate-id (symbol-name event-type))
                       :source "coordinator"
                       :type event-type
                       :train-id train-id
                       :generation (plist-get train :generation)
                       :observed-at ts
                       :received-at ts
                       :payload payload))
             (res (agent-shell-queue-train-step train ev ts))
             (new-train (plist-get res :train))
             (outcome (plist-get res :outcome)))
        (when (eq outcome 'accepted)
          (setf (map-elt agent-shell-queue-train--store train-id) new-train)
          (when state-dir
            (agent-shell-queue-train-save-record new-train state-dir)))
        (agent-shell-queue-train-make-outcome
         outcome :id train-id
         :state (plist-get new-train :state)
         :revision (plist-get new-train :revision)
         :effects (plist-get res :effects)
         :reason (plist-get res :reason)))))))

(cl-defun agent-shell-queue-train-activate (train-id &key expected-revision now state-dir)
  "Activate staged TRAIN-ID."
  (agent-shell-queue-train--mutate train-id expected-revision 'activate nil now state-dir))

(cl-defun agent-shell-queue-train-pause (train-id &key expected-revision now state-dir)
  "Pause active TRAIN-ID."
  (agent-shell-queue-train--mutate train-id expected-revision 'pause nil now state-dir))

(cl-defun agent-shell-queue-train-resume (train-id &key expected-revision now state-dir)
  "Resume paused TRAIN-ID."
  (agent-shell-queue-train--mutate train-id expected-revision 'resume nil now state-dir))

(cl-defun agent-shell-queue-train-cancel (train-id &key expected-revision (interrupt 'after-turn)
                                                   now state-dir)
  "Cancel TRAIN-ID with explicit INTERRUPT policy (`after-turn' or `interrupt')."
  (agent-shell-queue-train--mutate
   train-id expected-revision 'cancel (list :interrupt interrupt) now state-dir))

(cl-defun agent-shell-queue-train-add-tasks (train-id patch &key expected-revision
                                                      authorized-p state-dir)
  "Validate and apply or stage graph PATCH on TRAIN-ID.
When AUTHORIZED-P is nil, stages PATCH under `:proposed-patches' for human
review without mutating `:tasks'."
  (let ((train (map-elt agent-shell-queue-train--store train-id)))
    (cond
     ((null train)
      (agent-shell-queue-train-make-outcome 'invalid :id train-id :reason "train-not-found"))
     ((and expected-revision (/= expected-revision (plist-get train :revision)))
      (agent-shell-queue-train-make-outcome
       'conflict :id train-id :reason "stale-revision"
       :current-revision (plist-get train :revision)))
     (t
      (let* ((adj (agent-shell-queue-train--task-adjacency (plist-get train :tasks)))
             (check (agent-shell-queue-train-validate-graph-patch patch adj)))
        (if (not (plist-get check :ok))
            (agent-shell-queue-train-make-outcome
             'invalid :id train-id :errors (plist-get check :errors))
          (let ((new-rev (1+ (plist-get train :revision))))
            (if (not authorized-p)
                (let* ((t1 (plist-put (copy-sequence train) :revision new-rev))
                       (t2 (plist-put t1 :proposed-patches
                                      (cons patch (plist-get train :proposed-patches)))))
                  (setf (map-elt agent-shell-queue-train--store train-id) t2)
                  (when state-dir (agent-shell-queue-train-save-record t2 state-dir))
                  (agent-shell-queue-train-make-outcome
                   'accepted :id train-id :revision new-rev :staged-proposal t))
              (let* ((added (mapcar (lambda (tk)
                                      (if (plist-get tk :state)
                                          tk
                                        (plist-put (copy-sequence tk) :state 'pending)))
                                    (plist-get patch :added-tasks)))
                     (merged (append (plist-get train :tasks) added))
                     (final-tasks (if (eq (agent-shell-queue-train--sym (plist-get train :state))
                                          'active)
                                      (agent-shell-queue-train--recompute-readiness merged)
                                    merged))
                     (t1 (plist-put (copy-sequence train) :revision new-rev))
                     (t2 (plist-put t1 :tasks final-tasks)))
                (setf (map-elt agent-shell-queue-train--store train-id) t2)
                (when state-dir (agent-shell-queue-train-save-record t2 state-dir))
                (agent-shell-queue-train-make-outcome
                 'accepted :id train-id :revision new-rev :applied t))))))))))

;;; Circuit Breaker (Scoped Batch Pause/Resume)

(defun agent-shell-queue-train-circuit-breaker-pause (&optional train-ids now state-dir)
  "Batch-pause currently active trains in TRAIN-IDS (or all active trains).
Records the exact set paused so `agent-shell-queue-train-circuit-breaker-resume'
never activates trains that were already staged or paused."
  (let* ((targets (or train-ids
                      (mapcar (lambda (r) (plist-get r :id))
                              (agent-shell-queue-train-list 'active))))
         (paused nil))
    (dolist (id targets)
      (let ((rec (map-elt agent-shell-queue-train--store id)))
        (when (and rec (eq (agent-shell-queue-train--sym (plist-get rec :state)) 'active))
          (let ((res (agent-shell-queue-train-pause id :now now :state-dir state-dir)))
            (when (eq (plist-get res :outcome) 'accepted)
              (push id paused))))))
    (setq agent-shell-queue-train--circuit-breaker-paused (nreverse paused))
    (list :paused agent-shell-queue-train--circuit-breaker-paused)))

(defun agent-shell-queue-train-circuit-breaker-resume (&optional now state-dir)
  "Resume only the exact set of trains paused by the last circuit-breaker."
  (let ((resumed nil))
    (dolist (id agent-shell-queue-train--circuit-breaker-paused)
      (let ((res (agent-shell-queue-train-resume id :now now :state-dir state-dir)))
        (when (eq (plist-get res :outcome) 'accepted)
          (push id resumed))))
    (setq agent-shell-queue-train--circuit-breaker-paused nil)
    (list :resumed (nreverse resumed))))

;;; Retention Compaction

(defun agent-shell-queue-train-compact-history (train max-consumed)
  "Compact TRAIN's `:consumed-events' to at most MAX-CONSUMED recent entries.
Preserves `:pending-effects', `:reviews', and recent deduplication tombstones."
  (let ((consumed (plist-get train :consumed-events)))
    (if (<= (length consumed) max-consumed)
        train
      (plist-put (copy-sequence train)
                 :consumed-events (seq-take consumed max-consumed)))))

(provide 'agent-shell-queue-train)
;;; agent-shell-queue-train.el ends here
