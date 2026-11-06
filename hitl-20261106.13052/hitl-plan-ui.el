;;; hitl-plan-ui.el --- Review buffer and resume picker for hitl-plan -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, agent, ai

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides the interactive review buffer (`hitl-plan-mode') and
;; the `hitl-plan-resume' picker for `hitl-plan' plans.  The picker uses
;; `annotated-completing-read' when available and falls back to
;; `completing-read'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'hitl-plan)

(declare-function annotated-completing-read "annotated-completing-read")
(declare-function hitl--update-indicator "hitl-ui")
(defvar hitl-indicator-mode)

(defface hitl-plan-pending-face
  '((t :inherit default))
  "Face for pending steps in `hitl-plan-mode' buffers."
  :group 'hitl-plan)

(defface hitl-plan-done-face
  '((t :inherit shadow :strike-through t))
  "Face for completed steps in `hitl-plan-mode' buffers."
  :group 'hitl-plan)

(defface hitl-plan-failed-face
  '((t :inherit error))
  "Face for failed steps in `hitl-plan-mode' buffers."
  :group 'hitl-plan)

(defface hitl-plan-skipped-face
  '((t :inherit shadow))
  "Face for skipped steps in `hitl-plan-mode' buffers."
  :group 'hitl-plan)

(defface hitl-plan-missing-face
  '((t :inherit error))
  "Face for steps with status `missing' (target gone) in review buffers."
  :group 'hitl-plan)

;;; Untrusted Text

(defcustom hitl-plan-ui-max-field-width 200
  "Maximum display width of any single untrusted string in review buffers.
Longer descriptions, notes, labels and messages are truncated with an
ellipsis."
  :type 'integer
  :group 'hitl-plan)

(defcustom hitl-plan-ui-file-width 40
  "Display width of the file column in review buffers.
A longer path is shortened from the left so the file name stays visible."
  :type 'integer
  :group 'hitl-plan)

(defconst hitl-plan-ui--unsafe-char-regexp
  (concat "[\u0000-\u001f\u007f-\u009f\u061c\u200b-\u200f\u2028-\u202e"
          "\u2060-\u2069\ufeff\U000E0000-\U000E007F]")
  "Control, invisible and bidirectional-formatting characters to escape.")

(defun hitl-plan-ui-clean-string (value &optional max)
  "Return VALUE as a single safe line of at most MAX columns.
VALUE (any object; nil is empty) is formatted with `%s'.  Newlines, control
characters and invisible or bidirectional-override characters are replaced
by visible escapes such as \\n and \\u202E, so untrusted text can neither
start a new line in a review buffer nor reorder the text around it.  MAX
defaults to `hitl-plan-ui-max-field-width'; longer text ends in an ellipsis."
  (let* ((str (cond ((null value) "") ((stringp value) value) (t (format "%s" value))))
         (clean (replace-regexp-in-string
                 hitl-plan-ui--unsafe-char-regexp
                 (lambda (m)
                   (let ((c (string-to-char m)))
                     (pcase c
                       (?\n "\\n") (?\r "\\r") (?\t "\\t")
                       (_ (if (< c #x100)
                              (format "\\x%02x" c)
                            (format "\\u%04X" c))))))
                 str t t))
         (max (or max hitl-plan-ui-max-field-width)))
    (truncate-string-to-width clean max nil nil "…")))

(defun hitl-plan-ui--clean-left (value width)
  "Return VALUE cleaned and, when wider than WIDTH, shortened from the left."
  (let* ((clean (hitl-plan-ui-clean-string value (max 1 (* 4 (string-width (format "%s" value))))))
         (w (string-width clean)))
    (if (<= w width)
        clean
      (concat "…" (truncate-string-to-width clean w (- w (1- width)))))))

(defvar-local hitl-plan-ui--plan nil
  "The `hitl-plan' displayed in this review buffer.")

(defvar hitl-plan-resume-history nil
  "Minibuffer history for `hitl-plan-resume'.")

(defvar hitl-plan-op-history nil
  "Minibuffer history for adding operations in `hitl-plan-mode'.")

;;; Rendering

(defun hitl-plan-ui-buffer-name (plan)
  "Return the review buffer name for PLAN."
  (format "*hitl-plan: %s*" (hitl-plan-id plan)))

(defun hitl-plan-ui--step-face (status)
  "Return the face for step STATUS."
  (pcase status
    ('done 'hitl-plan-done-face)
    ('failed 'hitl-plan-failed-face)
    ('skipped 'hitl-plan-skipped-face)
    ('missing 'hitl-plan-missing-face)
    ('pending 'hitl-plan-pending-face)
    ;; Any other domain-chosen terminal status renders like failed.
    ((pred symbolp) 'hitl-plan-failed-face)
    (_ 'hitl-plan-pending-face)))

(defun hitl-plan-ui--count (plan status)
  "Return the number of PLAN steps with STATUS."
  (seq-count (lambda (s) (eq (hitl-plan-step-status s) status))
             (hitl-plan-steps plan)))

(defun hitl-plan-ui--format-target (plan step)
  "Return (FILE . LABEL) strings for STEP of PLAN."
  (let* ((domain (hitl-plan-get-domain (hitl-plan-domain plan)))
         (fmt (and domain (hitl-plan-domain-format-target domain)))
         (target (hitl-plan-step-target step))
         (cols (and fmt (ignore-errors (funcall fmt target))))
         (file (and (consp target) (plistp target) (plist-get target :file))))
    ;; Show the full (abbreviated) path, not just the name, so look-alike
    ;; files in different directories can be told apart.  Both columns are
    ;; single-line, bounded strings.
    (cons (hitl-plan-ui--clean-left
           (if (stringp file) (abbreviate-file-name file) (car cols))
           hitl-plan-ui-file-width)
          (hitl-plan-ui-clean-string (cdr cols)))))

(defun hitl-plan-ui--format-step (plan step multi-op)
  "Format STEP of PLAN as a propertized line.  MULTI-OP adds an op column."
  (let* ((status (hitl-plan-step-status step))
         (face (hitl-plan-ui--step-face status))
         (cols (hitl-plan-ui--format-target plan step))
         (msg (hitl-plan-step-message step))
         (note (hitl-plan-step-note step))
         (op (hitl-plan-step-op step))
         (body (format "%3s [%-7s] %s%s %s"
                       (hitl-plan-ui-clean-string (hitl-plan-step-id step) 6)
                       (hitl-plan-ui-clean-string status 7)
                       (if multi-op
                           (format "%-14s "
                                   (hitl-plan-ui-clean-string
                                    (if op (hitl-plan-describe-op
                                            (hitl-plan-domain plan) op)
                                      "?")
                                    14))
                         "")
                       (let ((file (car cols)))
                         (concat file (make-string (max 0 (- hitl-plan-ui-file-width
                                                             (string-width file)))
                                                   ?\s)))
                       (cdr cols))))
    (propertize
     (concat "  " (propertize body 'face face)
             (if msg (concat " — " (propertize (hitl-plan-ui-clean-string msg) 'face (if (memq status '(failed missing))
                                                             'hitl-plan-failed-face
                                                           'shadow)))
               "")
             (if (and note (not (string-empty-p note)))
                 (propertize (format "  {%s}" (hitl-plan-ui-clean-string note)) 'face 'shadow)
               "")
             "\n")
     'hitl-plan-step step)))

(defun hitl-plan-ui--render (plan)
  "Render PLAN into the current buffer, keeping point on the same step."
  (let* ((inhibit-read-only t)
         (prev (get-text-property (line-beginning-position) 'hitl-plan-step))
         (prev-id (and prev (hitl-plan-step-id prev)))
         (domain (hitl-plan-domain plan))
         (ops (hitl-plan-ops plan))
         (multi (> (length ops) 1))
         restore)
    (erase-buffer)
    (insert (format "Plan:        %s  [%s]%s\n"
                    (hitl-plan-ui-clean-string (hitl-plan-id plan) 80)
                    (hitl-plan-ui-clean-string (hitl-plan-status plan) 20)
                    (if (hitl-plan-origin-mcp-p plan) "  (created over MCP)" "")))
    (when-let* ((d (hitl-plan-description plan)))
      (insert (format "Description: %s\n" (hitl-plan-ui-clean-string d))))
    (insert (format "Domain:      %s\n" (hitl-plan-ui-clean-string domain 80)))
    (insert (format "Created:     %s\n" (hitl-plan-ui-clean-string (or (hitl-plan-created plan) "-") 40)))
    (insert (format "Updated:     %s\n" (hitl-plan-ui-clean-string (or (hitl-plan-updated plan) "-") 40)))
    (insert (format "Log-notes:   %s\n" (hitl-plan-ui-clean-string (hitl-plan-log-notes plan) 20)))
    (when-let* ((n (hitl-plan-note plan)))
      (insert (format "Plan note:   %s\n" (hitl-plan-ui-clean-string n))))
    (insert "Operations:  "
            (if ops
                (string-join
                 (seq-map-indexed
                  (lambda (op i)
                    (let ((spec (hitl-plan-get-op domain (hitl-plan-op-name op))))
                      (format "%d. %s%s" (1+ i)
                              (hitl-plan-ui-clean-string (hitl-plan-describe-op domain op))
                              (if (and spec (hitl-plan-op-spec-order-last spec))
                                  " (order-last)" ""))))
                  ops)
                 "   ")
              "none")
            "\n")
    (insert (format "Progress:    %d/%d done, %d pending, %d failed, %d skipped%s\n"
                    (hitl-plan-ui--count plan 'done) (length (hitl-plan-steps plan))
                    (hitl-plan-ui--count plan 'pending)
                    (hitl-plan-ui--count plan 'failed)
                    (hitl-plan-ui--count plan 'skipped)
                    (concat
                     (let ((n (hitl-plan-ui--count plan 'missing)))
                       (if (> n 0) (format ", %d missing" n) ""))
                     (mapconcat (lambda (c) (format ", %d %s" (cdr c)
                                                    (hitl-plan-ui-clean-string (car c) 40)))
                                (hitl-plan-other-counts plan) ""))))
    (insert "\nKeys: [C-c C-c/c/x] execute  [m/u] skip/unskip  [a] add op  [n] note  \
[L] log-notes  [RET] visit  [g] refresh  [d/k] discard  [q] quit\n\n")
    (let ((first-step (point)))
      (dolist (step (hitl-plan-steps plan))
        (let ((start (point)))
          (insert (hitl-plan-ui--format-step plan step multi))
          (when (and prev-id (eql prev-id (hitl-plan-step-id step)))
            (setq restore start))))
      (goto-char (or restore first-step)))))

;;; Entry Points

(defun hitl-plan-ui--resolve-plan (plan-or-id &optional directory)
  "Return the `hitl-plan' for PLAN-OR-ID or signal `user-error'.
A plan ID is looked up in DIRECTORY when given; a plan struct is returned
as is (its own `directory' slot stays authoritative)."
  (cond ((hitl-plan-p plan-or-id) plan-or-id)
        ((stringp plan-or-id)
         (or (hitl-plan-get plan-or-id directory)
             (user-error "No such plan: %s" plan-or-id)))
        (t (user-error "Invalid plan: %S" plan-or-id))))

;;;###autoload
(cl-defun hitl-plan-review (plan-or-id &optional (display t))
  "Create or update the review buffer for PLAN-OR-ID and return it.
DISPLAY (default t) pops the buffer up and selects it; pass nil to only
build the buffer without touching windows or the selected buffer."
  (interactive (list (hitl-plan-ui-read-plan nil "Review plan: ")))
  (let* ((plan (hitl-plan-ui--resolve-plan plan-or-id))
         (buf (get-buffer-create (hitl-plan-ui-buffer-name plan))))
    (with-current-buffer buf
      (hitl-plan-mode)
      (setq hitl-plan-ui--plan plan)
      (hitl-plan-ui--render plan))
    (when display (pop-to-buffer buf))
    buf))

(defun hitl-plan-ui--age (created)
  "Return a concise age string for the ISO-8601 time string CREATED."
  (let ((unknown "unknown age"))
    (condition-case nil
        (let ((secs (float-time (time-subtract (current-time) (date-to-time created)))))
          (cond ((< secs 60) (format "%ds ago" (max 0 (truncate secs))))
                ((< secs 3600) (format "%dm ago" (truncate (/ secs 60))))
                ((< secs 86400) (format "%dh ago" (truncate (/ secs 3600))))
                (t (format "%dd ago" (truncate (/ secs 86400))))))
      (error unknown))))

(defun hitl-plan-ui--annotate (plan)
  "Return the picker annotation for PLAN."
  (format "[%s] %d/%d steps (%s) — %s"
          (hitl-plan-status plan)
          (hitl-plan-ui--count plan 'done) (length (hitl-plan-steps plan))
          (hitl-plan-ui--age (hitl-plan-created plan))
          (hitl-plan-ui-clean-string
           (or (hitl-plan-description plan)
               (mapconcat (lambda (o) (hitl-plan-describe-op (hitl-plan-domain plan) o))
                          (hitl-plan-ops plan) ", ")))))

(defun hitl-plan-ui--complete (prompt candidates category history)
  "Read one of CANDIDATES, an alist of (NAME . ANNOTATION), and return NAME.
PROMPT is the minibuffer prompt.  CATEGORY and HISTORY are the completion
category symbol and the history variable.  Uses `annotated-completing-read'
when available, else `completing-read' with an annotation function.  A
match is required."
  (if (fboundp 'annotated-completing-read)
      (annotated-completing-read candidates :prompt prompt :category category
                                 :history history :require-match t)
    (completing-read prompt
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function
                              . ,(lambda (c) (when-let* ((a (cdr (assoc c candidates))))
                                               (concat "  " a)))))
                         (complete-with-action action candidates str pred)))
                     nil t nil history)))

(defun hitl-plan-ui-read-from (plans prompt)
  "Prompt with PROMPT for one of PLANS, a non-empty list; return its ID.
Candidates are annotated with status, progress, age and description."
  (hitl-plan-ui--complete
   prompt
   (mapcar (lambda (p) (cons (hitl-plan-id p) (hitl-plan-ui--annotate p))) plans)
   'hitl-plan 'hitl-plan-resume-history))

(defun hitl-plan-ui-read-plan (statuses prompt &optional directory domain)
  "Prompt with PROMPT for a plan in STATUSES (nil for all); return its ID.
DIRECTORY and DOMAIN restrict the candidates as in `hitl-plan-list'."
  (let ((plans (hitl-plan-list statuses directory domain)))
    (unless plans (user-error "No matching plans"))
    (hitl-plan-ui-read-from plans prompt)))

;;;###autoload
(defun hitl-plan-resume (&optional plan-id directory domain)
  "Resume an unfinished plan PLAN-ID (prompting if nil) in its review buffer.
The picker lists plans that are draft, paused, or running; DIRECTORY and
DOMAIN restrict both the picker and the lookup of PLAN-ID."
  (interactive)
  (let ((id (or plan-id
                (hitl-plan-ui-read-plan '(draft paused running) "Resume plan: "
                                        directory domain))))
    (hitl-plan-review (or (hitl-plan-get id directory domain)
                          (user-error "No such plan: %s" id)))))

;;; Commands

(defun hitl-plan-ui--plan ()
  "Return the buffer's plan or signal `user-error'."
  (or hitl-plan-ui--plan (user-error "No plan in this buffer")))

(defun hitl-plan-ui--step ()
  "Return the step on the current line or signal `user-error'."
  (or (get-text-property (line-beginning-position) 'hitl-plan-step)
      (user-error "No step on this line")))

(defun hitl-plan-ui--update-indicator ()
  "Refresh the HITL mode line indicator when active."
  (when (and (bound-and-true-p hitl-indicator-mode) (fboundp 'hitl--update-indicator))
    (hitl--update-indicator)))

(defun hitl-plan-ui--confirm-mcp-plan (plan)
  "Ask the user to approve running PLAN when it was created over MCP.
The prompt names the plan, its operations and the number of pending steps.
Signal `user-error' unless the answer is yes.  Plans of other origins are
not asked."
  (when (hitl-plan-origin-mcp-p plan)
    (unless (yes-or-no-p
             (format "Plan %s was created by an agent over MCP.  Run %d step(s) of [%s]? "
                     (hitl-plan-ui-clean-string (hitl-plan-id plan) 60)
                     (hitl-plan-ui--count plan 'pending)
                     (hitl-plan-ui-clean-string
                      (mapconcat (lambda (o) (symbol-name (hitl-plan-op-name o)))
                                 (hitl-plan-ops plan) ", ")
                      80)))
      (user-error "Plan %s not executed" (hitl-plan-id plan)))))

(defun hitl-plan-ui-execute ()
  "Execute the pending steps of the current plan and refresh.
A plan created over MCP first needs an explicit `yes-or-no-p' approval."
  (interactive)
  (let ((plan (hitl-plan-ui--plan)))
    (hitl-plan-ui--confirm-mcp-plan plan)
    (when (eq (hitl-plan-log-notes plan) 'each)
      (hitl-plan-ui--prompt-step-notes plan))
    (unwind-protect (hitl-plan-execute plan)
      (when (buffer-live-p (current-buffer))
        (hitl-plan-ui--render plan))
      (hitl-plan-ui--update-indicator))
    (message "Plan %s [%s]: %d done, %d failed"
             (hitl-plan-ui-clean-string (hitl-plan-id plan) 60) (hitl-plan-status plan)
             (hitl-plan-ui--count plan 'done) (hitl-plan-ui--count plan 'failed))))

(defun hitl-plan-ui-toggle-skip ()
  "Toggle the step at point between pending and skipped."
  (interactive)
  (let ((plan (hitl-plan-ui--plan))
        (step (hitl-plan-ui--step)))
    (pcase (hitl-plan-step-status step)
      ('pending (setf (hitl-plan-step-status step) 'skipped
                      (hitl-plan-step-message step) "skipped by user"))
      ('skipped (setf (hitl-plan-step-status step) 'pending
                      (hitl-plan-step-message step) nil))
      (st (user-error "Cannot toggle step with status `%s'" st)))
    (hitl-plan-save plan)
    (hitl-plan-ui--render plan)
    (forward-line 1)))

(defun hitl-plan-ui-add-op ()
  "Prompt for a registered operation and add it to the current plan."
  (interactive)
  (let* ((plan (hitl-plan-ui--plan))
         (domain (hitl-plan-domain plan))
         (cands (mapcar (lambda (spec)
                          (cons (symbol-name (hitl-plan-op-spec-name spec))
                                (format "[%c] %s"
                                        (or (hitl-plan-op-spec-key spec) ?\s)
                                        (or (hitl-plan-op-spec-label spec) ""))))
                        (hitl-plan-registered-ops domain)))
         (choice (progn (unless cands (user-error "No operations registered"))
                        (hitl-plan-ui--complete "Add operation: " cands
                                                'hitl-plan-op 'hitl-plan-op-history)))
         (spec (or (hitl-plan-get-op domain choice)
                   (user-error "Unknown operation: %s" choice)))
         (args (when-let* ((reader (hitl-plan-op-spec-read-args spec)))
                 (funcall reader))))
    (hitl-plan-add-op plan (cons (hitl-plan-op-spec-name spec) args))
    (hitl-plan-ui--render plan)))

(defun hitl-plan-ui--prompt-step-notes (plan)
  "Prompt for a note for each pending step of PLAN, then save PLAN.
An empty answer clears the step's note; the current note is the default."
  (dolist (step (hitl-plan-steps plan))
    (when (eq (hitl-plan-step-status step) 'pending)
      (let* ((cols (hitl-plan-ui--format-target plan step))
             (n (read-string (format "Note for step %s (%s %s): "
                                     (hitl-plan-ui-clean-string (hitl-plan-step-id step) 6)
                                     (car cols) (cdr cols))
                             (hitl-plan-step-note step))))
        (setf (hitl-plan-step-note step) (unless (string-empty-p n) n)))))
  (hitl-plan-save plan))

(defun hitl-plan-ui-edit-note ()
  "Edit the step note (in `each' mode, on a step line) or the plan note."
  (interactive)
  (let* ((plan (hitl-plan-ui--plan))
         (step (and (eq (hitl-plan-log-notes plan) 'each)
                    (get-text-property (line-beginning-position) 'hitl-plan-step))))
    (if step
        (let ((n (read-string (format "Step %d note: " (hitl-plan-step-id step))
                              (hitl-plan-step-note step))))
          (setf (hitl-plan-step-note step) (unless (string-empty-p n) n)))
      (let ((n (read-string "Plan log note: " (hitl-plan-note plan))))
        (setf (hitl-plan-note plan) (unless (string-empty-p n) n))
        (when (and (not (string-empty-p n)) (eq (hitl-plan-log-notes plan) 'ignore))
          (setf (hitl-plan-log-notes plan) 'one))))
    (hitl-plan-save plan)
    (hitl-plan-ui--render plan)))

(defun hitl-plan-ui-cycle-log-notes ()
  "Cycle the plan's log-notes mode: ignore, one, each.
Entering `one' prompts for the plan note when empty; entering `each'
prompts for a note on every pending step."
  (interactive)
  (let* ((plan (hitl-plan-ui--plan))
         (next (pcase (hitl-plan-log-notes plan)
                 ('ignore 'one) ('one 'each) (_ 'ignore))))
    (setf (hitl-plan-log-notes plan) next)
    (pcase next
      ('one (when (string-empty-p (or (hitl-plan-note plan) ""))
              (let ((n (read-string "Plan log note: ")))
                (setf (hitl-plan-note plan) (unless (string-empty-p n) n)))))
      ('each (hitl-plan-ui--prompt-step-notes plan)))
    (hitl-plan-save plan)
    (hitl-plan-ui--render plan)))

(defun hitl-plan-ui-visit-target ()
  "Visit the target of the step at point in another window."
  (interactive)
  (let* ((plan (hitl-plan-ui--plan))
         (step (hitl-plan-ui--step))
         (domain (hitl-plan-get-domain (hitl-plan-domain plan)))
         (visit (and domain (hitl-plan-domain-visit-target domain))))
    (unless visit (user-error "Domain %s cannot visit targets" (hitl-plan-domain plan)))
    (unless (funcall visit (hitl-plan-step-target step) t)
      (user-error "Target no longer resolves"))))

(defun hitl-plan-ui-refresh-plan (plan)
  "Re-render PLAN's review buffer if it is live; return the buffer or nil.
For callers (such as the MCP tools) that change PLAN outside the buffer."
  (when-let* ((buf (get-buffer (hitl-plan-ui-buffer-name plan)))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (setq hitl-plan-ui--plan plan)
      (hitl-plan-ui--render plan))
    buf))

(defun hitl-plan-ui-refresh ()
  "Reload the current plan from disk and re-render."
  (interactive)
  (let* ((plan (hitl-plan-ui--plan))
         (fresh (or (hitl-plan-load (hitl-plan-id plan) (hitl-plan-directory plan))
                    plan)))
    (setq hitl-plan-ui--plan fresh)
    (hitl-plan-ui--render fresh)))

(defun hitl-plan-ui-discard ()
  "Discard the current plan after confirmation and close its buffer."
  (interactive)
  (let ((plan (hitl-plan-ui--plan)))
    (when (yes-or-no-p (format "Discard plan %s? " (hitl-plan-id plan)))
      (hitl-plan-discard plan)
      (hitl-plan-ui--update-indicator)
      (if (get-buffer-window (current-buffer))
          (quit-window t)
        (kill-buffer (current-buffer))))))

(defvar-keymap hitl-plan-mode-map
  :doc "Keymap for `hitl-plan-mode'."
  :parent special-mode-map
  "C-c C-c" #'hitl-plan-ui-execute
  "c"       #'hitl-plan-ui-execute
  "x"       #'hitl-plan-ui-execute
  "m"       #'hitl-plan-ui-toggle-skip
  "u"       #'hitl-plan-ui-toggle-skip
  "a"       #'hitl-plan-ui-add-op
  "n"       #'hitl-plan-ui-edit-note
  "L"       #'hitl-plan-ui-cycle-log-notes
  "RET"     #'hitl-plan-ui-visit-target
  "g"       #'hitl-plan-ui-refresh
  "d"       #'hitl-plan-ui-discard
  "k"       #'hitl-plan-ui-discard
  "q"       #'quit-window)

(define-derived-mode hitl-plan-mode special-mode "HITL-Plan"
  "Major mode for reviewing and executing a `hitl-plan'."
  :group 'hitl-plan
  (setq truncate-lines t))

(provide 'hitl-plan-ui)
;;; hitl-plan-ui.el ends here
