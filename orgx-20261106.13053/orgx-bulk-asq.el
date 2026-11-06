;;; orgx-bulk-asq.el --- Agent-shell-queue entry points for orgx-bulk agenda batches -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, convenience
;; Package-Requires: ((emacs "29.1") (org "9.6") (hitl "0.1.0") (agent-shell-queue "0.1.0"))

;;; Commentary:
;; Queues `org-agenda' bulk plans in `agent-shell-queue' (ASQ).
;; `agent-shell-queue-plan' provides the queue item type, the enqueue
;; validation (`:queueable' operations and the per-buffer cap), the marks
;; locking (through the `org-agenda' domain's :lock-batch and :unlock-batch)
;; and the dispatch, and the domain's :accept-buffer makes agenda buffers
;; valid targets.  This file builds an org plan from the marked entries of an
;; agenda buffer and hands the plan to `agent-shell-queue-plan-enqueue'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org-agenda)
(require 'hitl-plan)
(require 'agent-shell-queue-plan)
(require 'orgx-bulk)
(require 'orgx-bulk-agenda)
(require 'orgx-bulk-ops)

(defvaralias 'orgx-bulk-asq-max-items 'agent-shell-queue-plan-max-items
  "Maximum number of active or running plan items per agenda buffer.")

(defalias 'orgx-bulk-asq-dispatch #'agent-shell-queue-plan-dispatch)

(defun orgx-bulk-asq--marked (buf)
  "Return the marked (else batch, else all) entry markers of agenda BUF."
  (with-current-buffer buf
    (let ((line-markers (orgx-bulk-agenda--line-markers buf)))
      (or (seq-filter (lambda (m) (memq m line-markers))
                      org-agenda-bulk-marked-entries)
          orgx-bulk-batch-markers
          line-markers))))

(defun orgx-bulk-asq--plan-for-op (buf op &optional log-notes note)
  "Create a plan applying OP to the entries of agenda BUF."
  (let ((targets (delq nil (mapcar (lambda (m)
                                     (org-with-point-at m
                                       (orgx-bulk--target-at-point)))
                                   (orgx-bulk-asq--marked buf)))))
    (unless targets
      (user-error "No entries in agenda buffer %s to enqueue" (buffer-name buf)))
    (orgx-bulk-create-plan
     :scope (list :type 'agenda :buffer-name (buffer-name buf))
     :targets targets
     :ops (list op)
     :description (format "%s in %s" (hitl-plan-describe-op 'org-agenda op) (buffer-name buf))
     :log-notes (or log-notes
                    (if (eq orgx-bulk-log-notes 'each) 'ignore orgx-bulk-log-notes))
     :note note)))

(defun orgx-bulk-asq--read-plan-for-buffer (buf)
  "Interactively construct a queueable plan for agenda buffer BUF."
  (let* ((key (orgx-bulk-agenda--prompt-key))
         (spec (or (orgx-bulk-op-for-key key)
                   (user-error "Unknown bulk operation key: %c" key))))
    (unless (hitl-plan-op-spec-queueable spec)
      (user-error "Operation `%s' is not queueable" (hitl-plan-op-spec-name spec)))
    (let* ((args (when-let* ((reader (hitl-plan-op-spec-read-args spec)))
                   (funcall reader)))
           (note (when (and (eq (hitl-plan-op-spec-name spec) 'todo-state)
                            (eq orgx-bulk-log-notes 'one))
                   (read-string "Log note for queued plan: "))))
      (orgx-bulk-asq--plan-for-op
       buf (orgx-bulk-op--create :name (hitl-plan-op-spec-name spec) :args args)
       nil note))))

;;;###autoload
(defun orgx-bulk-asq-enqueue (&optional buf plan-or-op delay-after)
  "Enqueue PLAN-OR-OP on agenda buffer BUF in `agent-shell-queue'.
PLAN-OR-OP is a plan, an operation (built into a plan over BUF's entries),
or nil to read an operation interactively.  Queueing, marks locking and
the `:queueable' and per-buffer limits are `agent-shell-queue-plan-enqueue's.
DELAY-AFTER is the pause in seconds after the item completes."
  (interactive)
  (let ((approve (called-interactively-p 'any))
        (target-buf (cond ((bufferp buf) buf)
                          ((stringp buf) (get-buffer buf))
                          ((derived-mode-p 'org-agenda-mode) (current-buffer)))))
    (unless (and (buffer-live-p target-buf)
                 (with-current-buffer target-buf (derived-mode-p 'org-agenda-mode)))
      (user-error "Target buffer must be a live `org-agenda-mode' buffer"))
    (agent-shell-queue-plan-enqueue
     target-buf
     (cond ((hitl-plan-p plan-or-op) plan-or-op)
           ((or (hitl-plan-op-p plan-or-op) (consp plan-or-op))
            (orgx-bulk-asq--plan-for-op target-buf plan-or-op))
           (t (orgx-bulk-asq--read-plan-for-buffer target-buf)))
     delay-after
     ;; Only the user's own command approves a plan an agent created.
     approve)))

(provide 'orgx-bulk-asq)
;;; orgx-bulk-asq.el ends here
