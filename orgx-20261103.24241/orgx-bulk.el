;;; orgx-bulk.el --- Bulk task state changes with a review buffer -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, convenience

;;; Commentary:
;; Bulk operations on Org tasks across files or an agenda buffer.
;;
;; A bulk change is two steps: build a plan (`orgx-bulk-plan'), a list of
;; candidate headings plus a target TODO state, then review and apply it.
;; The review buffer (`orgx-bulk-review-mode') lists every affected heading
;; with a selection checkbox; nothing is modified until it is confirmed.
;;
;; Behavior attaches through two abnormal hooks:
;; - `orgx-bulk-item-functions': called per changed heading (ITEM PLAN).
;; - `orgx-bulk-after-apply-functions': called per touched file (FILE ITEMS PLAN),
;;   before the buffer is saved.  `orgx-bulk-denote-sync-execution-status' is
;;   the default and keeps a denote plan's `#+execution_status:' in step.
;;
;; The same plans are exposed over MCP by `orgx-mcp'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-agenda)

(declare-function orgx-denote-plan-file-p "orgx-denote")
(declare-function orgx-denote-files "orgx-denote")
(defvar org-agenda-bulk-marked-entries)

(defgroup orgx-bulk nil
  "Bulk task operations for orgx."
  :group 'orgx
  :prefix "orgx-bulk-")

(defcustom orgx-bulk-item-functions nil
  "Abnormal hook run after each heading changes state.
Each function receives the `orgx-bulk-item' and its `orgx-bulk-plan', with
point on the heading in the item's buffer."
  :type 'hook
  :group 'orgx-bulk)

(defcustom orgx-bulk-after-apply-functions '(orgx-bulk-denote-sync-execution-status)
  "Abnormal hook run once per touched file after a plan is applied.
Each function receives FILE, the list of changed `orgx-bulk-item's in it, and
the `orgx-bulk-plan'.  Runs with FILE's buffer current, before it is saved."
  :type 'hook
  :group 'orgx-bulk)

(defcustom orgx-bulk-save-buffers t
  "When non-nil, save each file buffer touched by an applied plan."
  :type 'boolean
  :group 'orgx-bulk)

;;; Data

(cl-defstruct (orgx-bulk-item (:constructor orgx-bulk-item--create))
  "One heading considered by a bulk plan."
  id marker file heading from-state path (selected t) problem result)

(cl-defstruct (orgx-bulk-plan (:constructor orgx-bulk-plan--create))
  "A pending, applied or discarded bulk state change."
  id target items description (status 'pending))

(defvar orgx-bulk--plans (make-hash-table :test #'equal)
  "Plans by id, kept so MCP clients can refer back to them.")

(defvar orgx-bulk--counter 0)

(defun orgx-bulk-get-plan (id)
  "Return the plan registered under ID, or nil."
  (map-elt orgx-bulk--plans id))

(defun orgx-bulk--item-at-point ()
  "Return an `orgx-bulk-item' for the heading at point."
  (orgx-bulk-item--create
   :marker (point-marker)
   :file (buffer-file-name (org-base-buffer (current-buffer)))
   :heading (org-get-heading t t t t)
   :from-state (org-get-todo-state)
   :path (org-get-outline-path)))

;;; Collection

(cl-defun orgx-bulk-collect-files (&key files match from)
  "Return items for todo headings in FILES.
FILES is anything `org-map-entries' accepts (nil for the current buffer,
`agenda', or a list of files).  MATCH is an optional `org-map-entries' match
string.  FROM is a list of states to include; by default every not-done state."
  ;; `org-map-entries' keeps nil results, so drop the non-matching headings.
  (seq-keep #'identity
            (org-map-entries
             (lambda ()
               (when-let* ((state (org-get-todo-state))
                           ((if from
                                (member state from)
                              (not (member state org-done-keywords)))))
                 (orgx-bulk--item-at-point)))
             match files)))

(defun orgx-bulk-collect-agenda ()
  "Return items for marked entries in the current agenda, else all its todo lines."
  (let ((markers (or (bound-and-true-p org-agenda-bulk-marked-entries)
                     (let (acc)
                       (save-excursion
                         (goto-char (point-min))
                         (while (not (eobp))
                           (when-let* ((m (get-text-property (point) 'org-hd-marker)))
                             (push m acc))
                           (forward-line 1)))
                       (nreverse acc)))))
    (seq-keep (lambda (m)
                (when (marker-buffer m)
                  (org-with-point-at m
                    (when (org-get-todo-state)
                      (orgx-bulk--item-at-point)))))
              markers)))

;;; Planning

(defun orgx-bulk-plan (target items &optional description)
  "Create and register a plan moving ITEMS to TARGET state.
Items that cannot take TARGET are deselected with a problem noted."
  (let ((plan (orgx-bulk-plan--create
               :id (format "bulk-%d" (setq orgx-bulk--counter (1+ orgx-bulk--counter)))
               :target target :items items :description description)))
    (seq-do-indexed
     (lambda (item i)
       (setf (orgx-bulk-item-id item) (1+ i))
       (let ((problem (org-with-point-at (orgx-bulk-item-marker item)
                        (cond
                         ((not (member target org-todo-keywords-1))
                          (format "%s is not a todo keyword in this file" target))
                         ((equal target (orgx-bulk-item-from-state item))
                          (format "already %s" target))))))
         (when problem
           (setf (orgx-bulk-item-problem item) problem
                 (orgx-bulk-item-selected item) nil))))
     items)
    (setf (map-elt orgx-bulk--plans (orgx-bulk-plan-id plan)) plan)
    plan))

(defun orgx-bulk-plan-selected (plan)
  "Return the items of PLAN that will be changed when applied."
  (seq-filter (lambda (item)
                (and (orgx-bulk-item-selected item)
                     (not (orgx-bulk-item-problem item))
                     (not (orgx-bulk-item-result item))))
              (orgx-bulk-plan-items plan)))

(defun orgx-bulk-plan-set-selection (plan ids)
  "Select only the items of PLAN whose id is in IDS (nil to select none)."
  (seq-do (lambda (item)
            (setf (orgx-bulk-item-selected item)
                  (and (memq (orgx-bulk-item-id item) ids) t)))
          (orgx-bulk-plan-items plan)))

;;; Applying

(defun orgx-bulk--change-item (item plan)
  "Move ITEM to PLAN's target state, recording the outcome on ITEM."
  (let ((target (orgx-bulk-plan-target plan))
        (org-inhibit-logging 'note))
    (condition-case err
        (org-with-point-at (orgx-bulk-item-marker item)
          (org-todo target)
          (if (equal (org-get-todo-state) target)
              (progn
                (setf (orgx-bulk-item-result item) 'changed)
                (run-hook-with-args 'orgx-bulk-item-functions item plan))
            (setf (orgx-bulk-item-result item) 'failed
                  (orgx-bulk-item-problem item) "state unchanged (blocked?)")))
      (error
       (setf (orgx-bulk-item-result item) 'failed
             (orgx-bulk-item-problem item) (error-message-string err))))))

(defun orgx-bulk-apply (plan)
  "Apply the selected items of PLAN and return PLAN.
Never prompts.  Hooks run per item and then per file, and touched file buffers
are saved when `orgx-bulk-save-buffers' is non-nil."
  (unless (eq (orgx-bulk-plan-status plan) 'pending)
    (user-error "Plan %s is already %s" (orgx-bulk-plan-id plan) (orgx-bulk-plan-status plan)))
  (seq-do (lambda (item) (orgx-bulk--change-item item plan))
          (orgx-bulk-plan-selected plan))
  (let ((changed (seq-filter (lambda (item) (eq (orgx-bulk-item-result item) 'changed))
                             (orgx-bulk-plan-items plan))))
    (map-do
     (lambda (file items)
       (with-current-buffer (org-base-buffer (marker-buffer (orgx-bulk-item-marker (car items))))
         (run-hook-with-args 'orgx-bulk-after-apply-functions file items plan)
         (when (and orgx-bulk-save-buffers (buffer-modified-p))
           (let ((inhibit-message t))
             (save-buffer)))))
     (seq-group-by #'orgx-bulk-item-file changed)))
  (setf (orgx-bulk-plan-status plan) 'applied)
  plan)

(defun orgx-bulk-discard (plan)
  "Discard PLAN if still pending and kill its review buffer."
  (when (eq (orgx-bulk-plan-status plan) 'pending)
    (setf (orgx-bulk-plan-status plan) 'discarded))
  (when-let* ((buf (get-buffer (orgx-bulk-review-buffer-name plan))))
    (kill-buffer buf))
  plan)

(defun orgx-bulk-plan-summary (plan)
  "Return PLAN as a JSON-ready plist."
  (let ((items (orgx-bulk-plan-items plan)))
    (list :plan_id (orgx-bulk-plan-id plan)
          :status (symbol-name (orgx-bulk-plan-status plan))
          :target_state (orgx-bulk-plan-target plan)
          :selected (length (seq-filter #'orgx-bulk-item-selected items))
          :items (vconcat
                  (seq-map
                   (lambda (item)
                     (list :id (orgx-bulk-item-id item)
                           :file (or (orgx-bulk-item-file item) :null)
                           :heading (orgx-bulk-item-heading item)
                           :path (vconcat (orgx-bulk-item-path item))
                           :from_state (orgx-bulk-item-from-state item)
                           :selected (if (orgx-bulk-item-selected item) t :json-false)
                           :result (if-let* ((r (orgx-bulk-item-result item)))
                                       (symbol-name r)
                                     :null)
                           :problem (or (orgx-bulk-item-problem item) :null)))
                   items)))))

;;; Denote plan integration

(defun orgx-bulk-denote-sync-execution-status (file _items _plan)
  "Sync `#+execution_status:' of denote plan FILE with its open tasks.
Becomes `complete' when no open todo remains, else `in-progress'.  Only an
existing keyword line is rewritten; the current buffer must visit FILE."
  (when (and (fboundp 'orgx-denote-plan-file-p)
             (orgx-denote-plan-file-p file))
    (save-excursion
      (save-restriction
        (widen)
        ;; Compute first: `org-map-entries' clobbers the match data.
        (let ((new (if (seq-some #'identity
                                 (org-map-entries
                                  (lambda ()
                                    (when-let* ((s (org-get-todo-state)))
                                      (not (member s org-done-keywords))))))
                       "in-progress"
                     "complete")))
          (goto-char (point-min))
          (when (and (re-search-forward "^#\\+execution_status:[ \t]*\\(.*?\\)[ \t]*$" nil t)
                     (not (equal (match-string 1) new)))
            (replace-match new t t nil 1)))))))

;;; Review buffer

(defvar-local orgx-bulk--review-plan nil
  "The `orgx-bulk-plan' shown in this review buffer.")

(defvar-keymap orgx-bulk-review-mode-map
  :doc "Keymap for `orgx-bulk-review-mode'."
  "x" #'orgx-bulk-review-toggle
  "a" #'orgx-bulk-review-select-all
  "u" #'orgx-bulk-review-select-none
  "c" #'orgx-bulk-review-confirm
  "RET" #'orgx-bulk-review-visit
  "g" #'orgx-bulk-review-refresh
  "q" #'orgx-bulk-review-quit)

(define-derived-mode orgx-bulk-review-mode special-mode "OrgBulk"
  "Review and confirm a bulk task state change.
\\{orgx-bulk-review-mode-map}"
  (setq-local truncate-lines t))

(defun orgx-bulk-review-buffer-name (plan)
  "Return the review buffer name for PLAN."
  (format "*orgx-bulk: %s*" (orgx-bulk-plan-id plan)))

(defun orgx-bulk-review-render ()
  "Redraw the current review buffer from its plan."
  (let* ((plan orgx-bulk--review-plan)
         (items (orgx-bulk-plan-items plan))
         (line (line-number-at-pos))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (format "Bulk change %s: -> %s   [%s]\n"
                    (orgx-bulk-plan-id plan)
                    (orgx-bulk-plan-target plan)
                    (orgx-bulk-plan-status plan)))
    (when-let* ((d (orgx-bulk-plan-description plan)))
      (insert d "\n"))
    (insert (format "%d of %d selected.  x toggle, a all, u none, c confirm, RET visit, q quit\n\n"
                    (length (seq-filter #'orgx-bulk-item-selected items))
                    (length items)))
    (seq-do
     (lambda (item)
       (let ((start (point)))
         (insert (format "[%s] %-12s -> %-10s %s  (%s%s)%s\n"
                         (cond
                          ((eq (orgx-bulk-item-result item) 'changed) "✓")
                          ((orgx-bulk-item-result item) "!")
                          ((orgx-bulk-item-selected item) "x")
                          (t " "))
                         (orgx-bulk-item-from-state item)
                         (orgx-bulk-plan-target plan)
                         (orgx-bulk-item-heading item)
                         (file-name-nondirectory (or (orgx-bulk-item-file item) "?"))
                         (if-let* ((p (orgx-bulk-item-path item)))
                             (concat " :: " (string-join p "/"))
                           "")
                         (if-let* ((p (orgx-bulk-item-problem item)))
                             (concat "  -- " p)
                           "")))
         (put-text-property start (point) 'orgx-bulk-item item)))
     items)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun orgx-bulk--review-item ()
  "Return the item on the current line, or signal a user error."
  (or (get-text-property (line-beginning-position) 'orgx-bulk-item)
      (user-error "No item on this line")))

(defun orgx-bulk--review-pending ()
  "Signal a user error unless the current review's plan is still pending."
  (unless (eq (orgx-bulk-plan-status orgx-bulk--review-plan) 'pending)
    (user-error "Plan already %s" (orgx-bulk-plan-status orgx-bulk--review-plan))))

(defun orgx-bulk-review-toggle ()
  "Toggle selection of the item on this line."
  (interactive)
  (orgx-bulk--review-pending)
  (let ((item (orgx-bulk--review-item)))
    (when (orgx-bulk-item-problem item)
      (user-error "Cannot select: %s" (orgx-bulk-item-problem item)))
    (setf (orgx-bulk-item-selected item) (not (orgx-bulk-item-selected item)))
    (orgx-bulk-review-render)))

(defun orgx-bulk-review-select-all ()
  "Select every item that can be changed."
  (interactive)
  (orgx-bulk--review-pending)
  (seq-do (lambda (item)
            (setf (orgx-bulk-item-selected item) (not (orgx-bulk-item-problem item))))
          (orgx-bulk-plan-items orgx-bulk--review-plan))
  (orgx-bulk-review-render))

(defun orgx-bulk-review-select-none ()
  "Deselect every item."
  (interactive)
  (orgx-bulk--review-pending)
  (orgx-bulk-plan-set-selection orgx-bulk--review-plan nil)
  (orgx-bulk-review-render))

(defun orgx-bulk-review-refresh ()
  "Redraw the review buffer."
  (interactive)
  (orgx-bulk-review-render))

(defun orgx-bulk-review-visit ()
  "Visit the heading of the item on this line."
  (interactive)
  (let ((marker (orgx-bulk-item-marker (orgx-bulk--review-item))))
    (pop-to-buffer (marker-buffer marker))
    (widen)
    (goto-char marker)
    (org-fold-show-context)))

(defun orgx-bulk-review-confirm ()
  "Apply the selected changes of the plan in this buffer."
  (interactive)
  (orgx-bulk--review-pending)
  (let ((n (length (orgx-bulk-plan-selected orgx-bulk--review-plan))))
    (when (zerop n)
      (user-error "Nothing selected"))
    (orgx-bulk-apply orgx-bulk--review-plan)
    (orgx-bulk-review-render)
    (message "Changed %d heading(s)" n)))

(defun orgx-bulk-review-quit ()
  "Discard a pending plan and close the review buffer."
  (interactive)
  (let ((plan orgx-bulk--review-plan))
    (quit-window t)
    (orgx-bulk-discard plan)))

(defun orgx-bulk-review (plan &optional select)
  "Show PLAN in its review buffer and return the buffer.
With SELECT, also select the window; otherwise only display it."
  (let ((buf (get-buffer-create (orgx-bulk-review-buffer-name plan))))
    (with-current-buffer buf
      (orgx-bulk-review-mode)
      (setq orgx-bulk--review-plan plan)
      (orgx-bulk-review-render))
    (if select
        (pop-to-buffer buf)
      (display-buffer buf))
    buf))

;;; Interactive entry point

(defun orgx-bulk--read-scope ()
  "Return (ITEMS . DESCRIPTION) for the scope chosen by the user."
  (let ((scope (if (derived-mode-p 'org-agenda-mode)
                   "agenda"
                 (completing-read "Scope: "
                                  '("buffer" "agenda-files" "denote")
                                  nil t nil nil "buffer")))
        (match (let ((m (read-string "Match (tags/props, empty for all): ")))
                 (unless (string-empty-p m) m))))
    (pcase scope
      ("agenda" (cons (orgx-bulk-collect-agenda) "Agenda entries"))
      ("buffer" (cons (orgx-bulk-collect-files :match match) (format "Buffer %s" (buffer-name))))
      ("agenda-files" (cons (orgx-bulk-collect-files :files 'agenda :match match) "Agenda files"))
      ("denote" (cons (orgx-bulk-collect-files :files (orgx-denote-files) :match match)
                      "Denote files")))))

;;;###autoload
(defun orgx-bulk-todo-state (state)
  "Move many tasks to STATE after reviewing them in a buffer.
Scope is the agenda's marked (or all) entries when called from an agenda,
otherwise the current buffer, the agenda files, or the denote files."
  (interactive
   (list (completing-read "Move to state: "
                          (seq-uniq (append org-todo-keywords-1 org-done-keywords))
                          nil nil)))
  (pcase-let ((`(,items . ,desc) (orgx-bulk--read-scope)))
    (unless items
      (user-error "No matching tasks"))
    (orgx-bulk-review (orgx-bulk-plan state items desc) t)))

(provide 'orgx-bulk)
;;; orgx-bulk.el ends here
