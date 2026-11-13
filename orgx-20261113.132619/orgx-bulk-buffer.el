;;; orgx-bulk-buffer.el --- Review buffer and resume entry points for orgx-bulk -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, convenience

;;; Commentary:
;; Thin facade over the `hitl-plan' review buffer (`hitl-plan-mode' in
;; `hitl-plan-ui').  Hitl renders, edits and executes plans.  This file
;; keeps the `orgx-bulk-*' command names as aliases and defines the
;; org-specific `orgx-bulk-resume' entry point, which restricts the picker
;; to `org-agenda' plans in `orgx-bulk-directory' and rebuilds the plan's
;; agenda batch before opening the plan.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'hitl-plan)
(require 'hitl-plan-ui)
(require 'orgx-bulk)
(require 'orgx-bulk-ops)
(require 'orgx-bulk-agenda)

(defalias 'orgx-bulk-plan-mode #'hitl-plan-mode)
(defalias 'orgx-bulk-buffer-execute #'hitl-plan-ui-execute)
(defalias 'orgx-bulk-buffer-toggle-skip #'hitl-plan-ui-toggle-skip)
(defalias 'orgx-bulk-buffer-add-op #'hitl-plan-ui-add-op)
(defalias 'orgx-bulk-buffer-edit-note #'hitl-plan-ui-edit-note)
(defalias 'orgx-bulk-buffer-visit #'hitl-plan-ui-visit-target)
(defalias 'orgx-bulk-buffer-discard #'hitl-plan-ui-discard)

(defun orgx-bulk-review (plan &optional display)
  "Create or update the review buffer for PLAN and return the buffer.
The buffer is `hitl-plan-review''s; it is only popped up when DISPLAY
is non-nil."
  (hitl-plan-review plan display))

(defun orgx-bulk--read-resume-id ()
  "Prompt for an unfinished `org-agenda' plan in `orgx-bulk-directory'.
Signal a `user-error' saying so when there is none."
  (unless (hitl-plan-list '(draft paused running) nil 'org-agenda)
    (user-error "No unfinished bulk plans to resume"))
  (hitl-plan-ui-read-plan '(draft paused running) "Resume bulk plan: "
                          nil 'org-agenda))

;;;###autoload
(defun orgx-bulk-resume (&optional plan-id)
  "Resume unfinished `org-agenda' plan PLAN-ID (or prompt if omitted).
Rebuilds the plan's agenda batch, then opens it with `hitl-plan-resume'."
  (interactive)
  (let* ((id (or plan-id (orgx-bulk--read-resume-id)))
         (plan (or (orgx-bulk-get-plan id)
                   (user-error "No such bulk plan: %s" id))))
    (orgx-bulk-batch-rebuild plan)
    (hitl-plan-resume id nil 'org-agenda)))

(provide 'orgx-bulk-buffer)
;;; orgx-bulk-buffer.el ends here
