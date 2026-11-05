;;; orgx-bulk-asq.el --- Agent-shell-queue backend for orgx-bulk agenda batches -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, convenience

;;; Commentary:
;; Attaches `org-agenda-mode' buffers as target buffers in `agent-shell-queue'
;; (ASQ).
;;
;; Registers the `orgx-bulk' item type when this module is loaded.
;; Each queued item freezes its batch plan at enqueue
;; time, locks marks in the target agenda buffer until queued bulk
;; items finish, enforces `:queueable' and `orgx-bulk-asq-max-items',
;; and dispatches through `orgx-bulk-execute'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org-agenda)
(require 'agent-shell-queue-core)
(require 'orgx-bulk)
(require 'orgx-bulk-agenda)
(require 'orgx-bulk-ops)

(defcustom orgx-bulk-asq-max-items 5
  "Maximum number of active or running `orgx-bulk' items per agenda buffer."
  :type 'integer
  :group 'orgx-bulk)

(declare-function agent-shell-queue-complete-item "agent-shell-queue-core")
(declare-function agent-shell-queue--complete-item "agent-shell-queue-core")
(declare-function agent-shell-queue--enqueue-args "agent-shell-queue-core")
(declare-function agent-shell-queue--send-next-for-buffer "agent-shell-queue-core")
(declare-function agent-shell-queue--ensure-loaded "agent-shell-queue-core")
(declare-function agent-shell-queue-store-items "agent-shell-queue-core")
(declare-function agent-shell-queue-item-kind "agent-shell-queue-core")
(declare-function agent-shell-queue-item-status "agent-shell-queue-core")
(declare-function agent-shell-queue-item-args "agent-shell-queue-core")
(declare-function agent-shell-queue-item-delay-after "agent-shell-queue-core")

(defvar agent-shell-queue--store nil)

;;; Queue Inspection Helpers

(defun orgx-bulk-asq--bucket-items (buf-name)
  "Return the list of queue items in BUF-NAME's bucket, or nil."
  (when (bound-and-true-p agent-shell-queue--store)
    (when (fboundp 'agent-shell-queue-store-items)
      (cdr (assoc buf-name
                  (funcall (symbol-function 'agent-shell-queue-store-items)
                           agent-shell-queue--store))))))

(defun orgx-bulk-asq--pending-count (buf-name)
  "Return the number of active or running `orgx-bulk' items for BUF-NAME."
  (seq-count
   (lambda (item)
     (and (fboundp 'agent-shell-queue-item-kind)
          (eq (funcall (symbol-function 'agent-shell-queue-item-kind) item)
              'orgx-bulk)
          (memq (funcall (symbol-function 'agent-shell-queue-item-status) item)
                '(active running))))
   (orgx-bulk-asq--bucket-items buf-name)))

(defun orgx-bulk-asq--validate-plan-queueable (plan)
  "Signal `user-error' if PLAN has non-`:queueable' ops or `each' notes."
  (when (eq (orgx-bulk-plan-log-notes plan) 'each)
    (user-error "Cannot enqueue plan with `each' log-notes mode"))
  (dolist (op (orgx-bulk-plan-ops plan))
    (let* ((name (orgx-bulk-op-name op))
           (spec (orgx-bulk-get-op name)))
      (unless spec
        (user-error "Unknown operation `%s'" name))
      (unless (orgx-bulk-op-spec-queueable spec)
        (user-error "Operation `%s' is not queueable" name)))))

;;; Dispatch and Completion

(defun orgx-bulk-asq--parse-item-args (raw-args)
  "Parse RAW-ARGS string or plist from a queued `orgx-bulk' item."
  (cond
   ((and (consp raw-args) (plistp raw-args))
    raw-args)
   ((stringp raw-args)
    (let ((trimmed (string-trim raw-args)))
      (if (string-prefix-p "(" trimmed)
          (condition-case nil
              (read trimmed)
            (error (list :plan-id trimmed)))
        (list :plan-id trimmed))))
   (t nil)))

(defun orgx-bulk-asq--complete (item buf-name)
  "Complete ITEM on BUF-NAME using the public ASQ completion function."
  (cond
   ((fboundp 'agent-shell-queue-complete-item)
    (agent-shell-queue-complete-item item buf-name))
   ((fboundp 'agent-shell-queue--complete-item)
    (agent-shell-queue--complete-item item buf-name))))

(defun orgx-bulk-asq-dispatch (item buf-name)
  "Dispatch a queued `orgx-bulk' ITEM against agenda buffer BUF-NAME."
  (let* ((raw (if (fboundp 'agent-shell-queue-item-args)
                  (funcall (symbol-function 'agent-shell-queue-item-args) item)
                (plist-get item :args)))
         (spec-plist (orgx-bulk-asq--parse-item-args raw))
         (plan-id (plist-get spec-plist :plan-id))
         (op-name (plist-get spec-plist :op))
         (op-args (plist-get spec-plist :args))
         (buf (get-buffer buf-name))
         (plan (and plan-id (orgx-bulk-get-plan plan-id))))
    (unwind-protect
        (progn
          (when (and (not plan) op-name (buffer-live-p buf))
            (pcase-let ((`(_ ,targets)
                         (orgx-bulk-batch-for-scope
                          (list :type 'agenda :buffer-name buf-name))))
              (setq plan
                    (orgx-bulk-create-plan
                     :scope (list :type 'agenda :buffer-name buf-name)
                     :targets targets
                     :ops (list (orgx-bulk-op--create :name op-name :args op-args))
                     :description (format "ASQ %s in %s" op-name buf-name)))))
          (unless plan
            (error "Plan %S not found for buffer %s" plan-id buf-name))
          (orgx-bulk-asq--validate-plan-queueable plan)
          (orgx-bulk-execute plan))
      (orgx-bulk-asq--complete item buf-name)
      (when (and (buffer-live-p buf)
                 (= (orgx-bulk-asq--pending-count buf-name) 0))
        (orgx-bulk-batch-unlock buf)))))

;;; Enqueue Command

(defun orgx-bulk-asq--set-delay-after (item delay)
  "Set the post-completion DELAY in seconds on queue ITEM."
  ;; `eval' so the accessor's `setf' expands at run time, when ASQ is loaded;
  ;; it is an optional dependency and not available when compiling.
  (eval `(setf (agent-shell-queue-item-delay-after ',item) ,delay) t))

(defun orgx-bulk-asq--read-plan-for-buffer (buf)
  "Interactively construct a queueable `orgx-bulk-plan' for agenda buffer BUF."
  (with-current-buffer buf
    (let* ((line-markers (orgx-bulk-agenda--line-markers buf))
           (marked (or (seq-filter (lambda (m) (memq m line-markers))
                                   org-agenda-bulk-marked-entries)
                       orgx-bulk-batch-markers
                       line-markers)))
      (unless marked
        (user-error "No entries in agenda buffer %s to enqueue" (buffer-name buf)))
      (let* ((queueable-ops
              (seq-filter #'orgx-bulk-op-spec-queueable (orgx-bulk-registered-ops)))
             (key (orgx-bulk-agenda--prompt-key))
             (spec (or (orgx-bulk-op-for-key key)
                       (user-error "Unknown bulk operation key: %c" key))))
        (unless (memq spec queueable-ops)
          (user-error "Operation `%s' is not queueable" (orgx-bulk-op-spec-name spec)))
        (let* ((op-args (when-let* ((reader (orgx-bulk-op-spec-read-args spec)))
                          (funcall reader)))
               (log-notes (if (eq orgx-bulk-log-notes 'each) 'ignore orgx-bulk-log-notes))
               (note (when (and (eq (orgx-bulk-op-spec-name spec) 'todo-state)
                                (eq log-notes 'one))
                       (read-string "Log note for queued plan: ")))
               (targets (delq nil
                              (mapcar (lambda (m)
                                        (org-with-point-at m
                                          (orgx-bulk--target-at-point)))
                                      marked)))
               (op (orgx-bulk-op--create :name (orgx-bulk-op-spec-name spec)
                                         :args op-args)))
          (orgx-bulk-create-plan
           :scope (list :type 'agenda :buffer-name (buffer-name buf))
           :targets targets
           :ops (list op)
           :description (format "%s in %s" (orgx-bulk-describe-op op) (buffer-name buf))
           :log-notes log-notes
           :note note))))))

;;;###autoload
(defun orgx-bulk-asq-enqueue (&optional buf plan-or-op delay-after)
  "Enqueue PLAN-OR-OP on agenda buffer BUF in `agent-shell-queue'.
Freezes the batch's step list at enqueue time and locks marks in BUF until
queued bulk operations on BUF finish.  Enforces `orgx-bulk-asq-max-items'
and the `:queueable' flag on operations.  Optional DELAY-AFTER sets the
post-completion pause delay in seconds before the next queued item runs."
  (interactive)
  (let* ((target-buf (cond
                      ((bufferp buf) buf)
                      ((stringp buf) (get-buffer buf))
                      ((derived-mode-p 'org-agenda-mode) (current-buffer))
                      (t nil))))
    (unless (and (buffer-live-p target-buf)
                 (with-current-buffer target-buf (derived-mode-p 'org-agenda-mode)))
      (user-error "Target buffer must be a live `org-agenda-mode' buffer"))
    (let* ((buf-name (buffer-name target-buf))
           (pending (orgx-bulk-asq--pending-count buf-name)))
      (when (>= pending orgx-bulk-asq-max-items)
        (user-error "Agenda buffer %s already has %d queued orgx-bulk items (max %d)"
                    buf-name pending orgx-bulk-asq-max-items))
      (let* ((plan
              (cond
               ((orgx-bulk-plan-p plan-or-op)
                plan-or-op)
               ((or (orgx-bulk-op-p plan-or-op) (consp plan-or-op))
                (with-current-buffer target-buf
                  (pcase-let ((`(_ . ,targets)
                               (orgx-bulk-batch-for-scope
                                (list :type 'agenda :buffer-name buf-name))))
                    (unless targets
                      (user-error "No entries in agenda buffer %s" buf-name))
                    (orgx-bulk-create-plan
                     :scope (list :type 'agenda :buffer-name buf-name)
                     :targets targets
                     :ops (list plan-or-op)
                     :log-notes (if (eq orgx-bulk-log-notes 'each)
                                    'ignore
                                  orgx-bulk-log-notes)))))
               (t
                (orgx-bulk-asq--read-plan-for-buffer target-buf)))))
        (orgx-bulk-asq--validate-plan-queueable plan)
        (unless (fboundp 'agent-shell-queue--enqueue-args)
          (user-error "agent-shell-queue is not available"))
        (orgx-bulk-save plan)
        ;; Freeze marks in the agenda buffer until the queued operation finishes.
        (orgx-bulk-batch-lock target-buf (orgx-bulk-plan-id plan))
        (let* ((first-op (car (orgx-bulk-plan-ops plan)))
               (args-str (prin1-to-string
                          (list :plan-id (orgx-bulk-plan-id plan)
                                :op (and first-op (orgx-bulk-op-name first-op))
                                :args (and first-op (orgx-bulk-op-args first-op)))))
               (queued nil))
          (unwind-protect
              (progn
                (agent-shell-queue--enqueue-args args-str 'orgx-bulk target-buf)
                (setq queued t))
            (unless queued
              (orgx-bulk-batch-unlock target-buf)))
          (when (and delay-after (> delay-after 0))
            (when-let* ((items (orgx-bulk-asq--bucket-items buf-name))
                        (last-item (car (last items))))
              (orgx-bulk-asq--set-delay-after last-item delay-after)))
          plan)))))

;;; Registration

(defun orgx-bulk-asq-register ()
  "Register the `orgx-bulk' item type with `agent-shell-queue'."
  (agent-shell-queue-register-item-type
   :kind 'orgx-bulk
   :label "orgx-bulk"
   :buffer-pred (lambda (buf)
                  (and (buffer-live-p buf)
                       (with-current-buffer buf
                         (derived-mode-p 'org-agenda-mode))))
   :dispatch-fn #'orgx-bulk-asq-dispatch
   :input-spec (list :kind 'special
                     :fn #'orgx-bulk-asq-enqueue)))

(orgx-bulk-asq-register)

(provide 'orgx-bulk-asq)
;;; orgx-bulk-asq.el ends here
