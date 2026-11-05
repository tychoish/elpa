;;; orgx-bulk-buffer.el --- Plan review buffer and resume picker for orgx-bulk -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, convenience

;;; Commentary:
;; Interactive review buffer (`orgx-bulk-plan-mode') and `orgx-bulk-resume'
;; picker for `orgx-bulk' plans.
;;
;; One line per step: checkbox, operation summary, heading, file and outline
;; path.  Done steps carry the `strike-through' face and a check mark `[✓]'.
;; Failed steps carry the `error' face and failure message.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'annotated-completing-read)
(require 'orgx-bulk)
(require 'orgx-bulk-agenda)
(require 'orgx-bulk-ops)

(defface orgx-bulk-done-face
  '((t :inherit shadow :strike-through t))
  "Face used for completed steps in `orgx-bulk-plan-mode' buffers."
  :group 'orgx-bulk)

(defvar-local orgx-bulk--current-plan nil
  "Buffer-local `orgx-bulk-plan' displayed in this plan buffer.")

(defvar orgx-bulk-resume-history nil
  "Minibuffer history list for `orgx-bulk-resume'.")

(defvar orgx-bulk-op-history nil
  "Minibuffer history list for adding operations in `orgx-bulk-plan-mode'.")

;;; Plan Buffer Naming and Rendering

(defun orgx-bulk-plan-buffer-name (plan)
  "Return the review buffer name for PLAN."
  (format "*orgx-bulk: %s*" (orgx-bulk-plan-id plan)))

(defun orgx-bulk--step-checkbox (status)
  "Return the checkbox string for step STATUS."
  (pcase status
    ('done "[✓]")
    ('pending "[ ]")
    ('skipped "[-]")
    ('failed "[!]")
    ('missing "[?]")
    (_ "[ ]")))

(defun orgx-bulk--step-face (status)
  "Return the display face for step STATUS."
  (pcase status
    ('done 'orgx-bulk-done-face)
    ('failed 'error)
    ('missing 'warning)
    ('skipped 'shadow)
    (_ nil)))

(defun orgx-bulk--format-step-line (step)
  "Format one line for `orgx-bulk-step' STEP with text properties."
  (let* ((status (orgx-bulk-step-status step))
         (box (orgx-bulk--step-checkbox status))
         (face (orgx-bulk--step-face status))
         (op-desc (orgx-bulk-describe-op (orgx-bulk-step-op step)))
         (target (orgx-bulk-step-target step))
         (heading (or (plist-get target :heading) "<unnamed>"))
         (file (plist-get target :file))
         (path (plist-get target :outline-path))
         (loc (concat (if file (file-name-nondirectory file) "<buffer>")
                      (if path (concat ":" (string-join path "/")) "")))
         (msg (orgx-bulk-step-message step))
         (body (format "%-18s  %-32s  %s"
                       (truncate-string-to-width op-desc 18 nil nil t)
                       (truncate-string-to-width heading 32 nil nil t)
                       loc))
         (styled-body (if face
                          (propertize body 'face face)
                        body))
         (box-str (if face
                      (propertize box 'face face)
                    box))
         (msg-str (if msg
                      (concat "  — "
                              (propertize msg 'face (if (eq status 'failed)
                                                        'error
                                                      'shadow)))
                    ""))
         (line (format "  %s %s%s\n" box-str styled-body msg-str)))
    (propertize line 'orgx-bulk-step step)))

(defun orgx-bulk--render-plan (plan)
  "Render PLAN into the current `orgx-bulk-plan-mode' buffer."
  (let* ((inhibit-read-only t)
         (prev-step (get-text-property (point) 'orgx-bulk-step))
         (prev-id (and prev-step (orgx-bulk-step-id prev-step)))
         (prev-line (line-number-at-pos))
         (steps (orgx-bulk-plan-steps plan))
         (ops (orgx-bulk-plan-ops plan))
         (counts (seq-group-by #'orgx-bulk-step-status steps))
         (n-done (length (alist-get 'done counts)))
         (n-pending (length (alist-get 'pending counts)))
         (n-skipped (length (alist-get 'skipped counts)))
         (n-failed (length (alist-get 'failed counts)))
         (n-missing (length (alist-get 'missing counts)))
         (ops-summary (if ops
                          (mapconcat #'orgx-bulk-describe-op ops ", ")
                        "none")))
    (erase-buffer)
    (insert (format "Plan %s [%s]%s\n"
                    (orgx-bulk-plan-id plan)
                    (orgx-bulk-plan-status plan)
                    (if-let* ((d (orgx-bulk-plan-description plan)))
                        (format " — %s" d)
                      "")))
    (insert (format "Operations: %s\n" ops-summary))
    (insert (format "Notes mode: %s%s\n"
                    (orgx-bulk-plan-log-notes plan)
                    (if-let* ((n (orgx-bulk-plan-note plan)))
                        (format " (%S)" n)
                      "")))
    (insert (format "Steps: %d total (%d pending, %d done, %d skipped, %d failed, %d missing)\n"
                    (length steps) n-pending n-done n-skipped n-failed n-missing))
    (insert "Keys: C-c C-c/c execute, x skip/restore, a add op, n note, RET visit, g refresh, d discard, q bury\n\n")
    (let (restore-pos)
      (dolist (step steps)
        (let ((line-start (point)))
          (insert (orgx-bulk--format-step-line step))
          (when (and prev-id (eql (orgx-bulk-step-id step) prev-id))
            (setq restore-pos line-start))))
      (goto-char (or restore-pos (point-min)))
      (unless restore-pos
        (forward-line (max 0 (1- (max 7 prev-line))))))))

(defun orgx-bulk-review (plan &optional display)
  "Create or update the review buffer for PLAN and return the buffer.
When DISPLAY is non-nil, pop to the buffer."
  (let ((buf (get-buffer-create (orgx-bulk-plan-buffer-name plan))))
    (with-current-buffer buf
      (orgx-bulk-plan-mode)
      (setq orgx-bulk--current-plan plan)
      (orgx-bulk--render-plan plan))
    (when display
      (pop-to-buffer buf))
    buf))

;;; Plan Buffer Commands

(defun orgx-bulk--require-current-plan ()
  "Return `orgx-bulk--current-plan' or signal `user-error'."
  (or orgx-bulk--current-plan
      (user-error "No bulk plan in current buffer")))

(defun orgx-bulk--step-at-point ()
  "Return the `orgx-bulk-step' on the current line, or signal `user-error'."
  (or (get-text-property (line-beginning-position) 'orgx-bulk-step)
      (get-text-property (point) 'orgx-bulk-step)
      (user-error "No bulk step on this line")))

(defun orgx-bulk-buffer-execute ()
  "Execute pending steps of the current plan and refresh the plan buffer."
  (interactive)
  (let ((plan (orgx-bulk--require-current-plan)))
    (orgx-bulk-execute plan)
    (when (buffer-live-p (current-buffer))
      (orgx-bulk--render-plan plan))
    (let* ((steps (orgx-bulk-plan-steps plan))
           (done (seq-count (lambda (s) (eq (orgx-bulk-step-status s) 'done)) steps))
           (failed (seq-count (lambda (s) (eq (orgx-bulk-step-status s) 'failed)) steps)))
      (message "Plan %s [%s]: %d done, %d failed"
               (orgx-bulk-plan-id plan)
               (orgx-bulk-plan-status plan)
               done failed))))

(defun orgx-bulk-buffer-toggle-skip ()
  "Toggle the step at point between `pending' and `skipped'."
  (interactive)
  (let ((plan (orgx-bulk--require-current-plan))
        (step (orgx-bulk--step-at-point)))
    (pcase (orgx-bulk-step-status step)
      ('pending
       (setf (orgx-bulk-step-status step) 'skipped
             (orgx-bulk-step-message step) "skipped by user"))
      ('skipped
       (setf (orgx-bulk-step-status step) 'pending
             (orgx-bulk-step-message step) nil))
      (st
       (user-error "Cannot toggle step with status `%s'" st)))
    (orgx-bulk-save plan)
    (orgx-bulk--render-plan plan)
    (forward-line 1)))

(defun orgx-bulk-buffer-add-op ()
  "Prompt for a registered operation and add it to the current plan."
  (interactive)
  (let* ((plan (orgx-bulk--require-current-plan))
         (candidates
          (mapcar (lambda (spec)
                    (cons (symbol-name (orgx-bulk-op-spec-name spec))
                          (format "[%c] %s"
                                  (or (orgx-bulk-op-spec-key spec) ?\s)
                                  (or (orgx-bulk-op-spec-label spec) ""))))
                  (orgx-bulk-registered-ops)))
         (choice (annotated-completing-read
                  :prompt "Add operation: "
                  :candidates candidates
                  :category 'orgx-bulk-op
                  :history 'orgx-bulk-op-history
                  :require-match t))
         (spec (or (orgx-bulk-get-op (intern choice))
                   (user-error "Unknown operation: %s" choice)))
         (args (when-let* ((reader (orgx-bulk-op-spec-read-args spec)))
                 (funcall reader)))
         (op (orgx-bulk-op--create :name (orgx-bulk-op-spec-name spec)
                                   :args args)))
    (orgx-bulk-plan-add-op plan op)
    (orgx-bulk--render-plan plan)))

(defun orgx-bulk-buffer-edit-note ()
  "Edit the plan-level log note for the current plan."
  (interactive)
  (let* ((plan (orgx-bulk--require-current-plan))
         (note (read-string "Plan log note: " (orgx-bulk-plan-note plan))))
    (setf (orgx-bulk-plan-note plan) (unless (string-empty-p note) note))
    (when (and (not (string-empty-p note))
               (eq (orgx-bulk-plan-log-notes plan) 'ignore))
      (setf (orgx-bulk-plan-log-notes plan) 'one))
    (orgx-bulk-save plan)
    (orgx-bulk--render-plan plan)))

(defun orgx-bulk-buffer-visit ()
  "Visit the heading for the step at point."
  (interactive)
  (let* ((step (orgx-bulk--step-at-point))
         (marker (orgx-bulk--resolve-target (orgx-bulk-step-target step))))
    (unless (and marker (marker-buffer marker))
      (user-error "Target heading no longer resolves"))
    (pop-to-buffer (marker-buffer marker))
    (goto-char marker)
    (org-fold-show-context 'agenda)))

(defun orgx-bulk-buffer-refresh ()
  "Reload and re-render the current plan."
  (interactive)
  (let* ((plan (orgx-bulk--require-current-plan))
         (reloaded (or (ignore-errors (orgx-bulk-load (orgx-bulk-plan-id plan)))
                       plan)))
    (setq orgx-bulk--current-plan reloaded)
    (orgx-bulk--render-plan reloaded)))

(defun orgx-bulk-buffer-discard ()
  "Discard the current plan after confirmation and kill its buffer."
  (interactive)
  (let ((plan (orgx-bulk--require-current-plan)))
    (when (yes-or-no-p (format "Discard bulk plan %s? " (orgx-bulk-plan-id plan)))
      (orgx-bulk-discard plan))))

(defvar-keymap orgx-bulk-plan-mode-map
  :doc "Keymap for `orgx-bulk-plan-mode'."
  :parent special-mode-map
  "C-c C-c" #'orgx-bulk-buffer-execute
  "c"       #'orgx-bulk-buffer-execute
  "x"       #'orgx-bulk-buffer-toggle-skip
  "a"       #'orgx-bulk-buffer-add-op
  "n"       #'orgx-bulk-buffer-edit-note
  "RET"     #'orgx-bulk-buffer-visit
  "g"       #'orgx-bulk-buffer-refresh
  "d"       #'orgx-bulk-buffer-discard
  "q"       #'quit-window)

(define-derived-mode orgx-bulk-plan-mode special-mode "OrgX-Bulk"
  "Major mode for reviewing and executing an `orgx-bulk-plan'."
  :group 'orgx-bulk
  (setq truncate-lines t))

;;; Resume Command and Picker

(defun orgx-bulk--plan-age-string (created)
  "Return a concise age or timestamp description for CREATED."
  (if (and (stringp created) (not (string-empty-p created)))
      (condition-case nil
          (let* ((time (date-to-time created))
                 (secs (float-time (time-subtract (current-time) time))))
            (cond
             ((< secs 60) (format "%ds ago" (max 0 (truncate secs))))
             ((< secs 3600) (format "%dm ago" (truncate (/ secs 60))))
             ((< secs 86400) (format "%dh ago" (truncate (/ secs 3600))))
             (t (format "%dd ago" (truncate (/ secs 86400))))))
        (error created))
    "unknown age"))

(defun orgx-bulk--annotate-plan (plan)
  "Build the annotation string for PLAN in `orgx-bulk-resume'."
  (let* ((steps (orgx-bulk-plan-steps plan))
         (done (seq-count (lambda (s) (eq (orgx-bulk-step-status s) 'done)) steps))
         (total (length steps))
         (age (orgx-bulk--plan-age-string (orgx-bulk-plan-created plan)))
         (desc (or (orgx-bulk-plan-description plan)
                   (mapconcat #'orgx-bulk-describe-op (orgx-bulk-plan-ops plan) ", "))))
    (format "[%s] %d/%d steps (%s) — %s"
            (orgx-bulk-plan-status plan)
            done total age desc)))

;;;###autoload
(defun orgx-bulk-resume (&optional plan-id)
  "Resume unfinished plan PLAN-ID (or prompt if omitted).
Prompts with `annotated-completing-read' over plans in `draft', `paused', or
`running' status, rebuilds the plan's agenda batch, and opens its plan buffer."
  (interactive)
  (let* ((unfinished (orgx-bulk-list-plans '(draft paused running)))
         (chosen-id
          (or plan-id
              (progn
                (unless unfinished
                  (user-error "No unfinished bulk plans to resume"))
                (let ((candidates
                       (mapcar (lambda (p)
                                 (cons (orgx-bulk-plan-id p)
                                       (orgx-bulk--annotate-plan p)))
                               unfinished)))
                  (annotated-completing-read
                   :prompt "Resume bulk plan: "
                   :candidates candidates
                   :category 'orgx-bulk-plan
                   :history 'orgx-bulk-resume-history
                   :require-match t)))))
         (plan (or (orgx-bulk-get-plan chosen-id)
                   (orgx-bulk-load chosen-id))))
    (orgx-bulk-batch-rebuild plan)
    (orgx-bulk-review plan t)))

(provide 'orgx-bulk-buffer)
;;; orgx-bulk-buffer.el ends here
