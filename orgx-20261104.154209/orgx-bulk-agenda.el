;;; orgx-bulk-agenda.el --- Agenda-buffer batches and B driver for orgx-bulk -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, agenda, convenience

;;; Commentary:
;; Agenda-buffer batch construction and the `B' (`orgx-agenda-bulk-action')
;; driver for `orgx-bulk'.
;;
;; Every batch is an `org-agenda-mode' buffer plus a buffer-local marker list
;; (`orgx-bulk-batch-markers').  Non-agenda scopes (file lists, current buffer,
;; `org-agenda-files', `orgx-denote-files') are converted into sticky agenda
;; buffers via `org-tags-view' so one execution path handles all scopes.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-agenda)
(require 'orgx-bulk)

(declare-function orgx-denote-files "orgx-denote")
(declare-function orgx-bulk-review "orgx-bulk-buffer")

(defvar org-agenda-sticky)
(defvar org-agenda-files)
(defvar org-agenda-window-setup)
(defvar org-agenda-bulk-marked-entries)
(defvar org-agenda-bulk-custom-functions)

(defvar-local orgx-bulk-batch-markers nil
  "Buffer-local list of `org-hd-marker' markers in this agenda batch.")

(defvar-local orgx-bulk-batch-plan-id nil
  "Buffer-local plan ID currently associated with this agenda buffer.")

(defvar-local orgx-bulk-batch-locked nil
  "When non-nil, changing marks in this agenda buffer is forbidden.")

(defvar orgx-bulk--internal-marking nil
  "Non-nil when `orgx-bulk' itself is manipulating agenda marks.")

(defun orgx-bulk-batch-lock (buffer &optional reason)
  "Lock agenda marks in BUFFER for REASON until unlocked."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local orgx-bulk-batch-locked (or reason t)))))

(defun orgx-bulk-batch-unlock (buffer)
  "Unlock agenda marks in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local orgx-bulk-batch-locked nil))))

(defun orgx-bulk-agenda--check-mark-lock (&rest _)
  "Signal `user-error' if marks are locked in the current agenda buffer."
  (when (and (derived-mode-p 'org-agenda-mode)
             orgx-bulk-batch-locked
             (not orgx-bulk--internal-marking))
    (user-error "Agenda marks are locked while bulk operation %s is pending"
                orgx-bulk-batch-locked)))

(dolist (fn '(org-agenda-bulk-mark
              org-agenda-bulk-mark-all
              org-agenda-bulk-mark-regexp
              org-agenda-bulk-unmark
              org-agenda-bulk-unmark-all
              org-agenda-bulk-toggle
              org-agenda-bulk-toggle-all))
  (advice-add fn :before #'orgx-bulk-agenda--check-mark-lock))

;;; Agenda Line Marker Helpers

(defun orgx-bulk-agenda--line-markers (&optional buffer)
  "Return all `org-hd-marker' markers present on lines in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let (markers)
        (while (not (eobp))
          (when-let* ((m (or (org-get-at-bol 'org-hd-marker)
                             (org-get-at-bol 'org-marker))))
            (push m markers))
          (forward-line 1))
        (nreverse markers)))))

(defun orgx-bulk-agenda--find-line-for-marker (target-marker &optional buffer)
  "Find line position in BUFFER whose `org-hd-marker' equals TARGET-MARKER.
Returns `(POS . LINE-MARKER)' where LINE-MARKER is the exact (`eq') marker
property stored on that agenda line, or nil if not found."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let (found)
        (while (and (not found) (not (eobp)))
          (when-let* ((m (or (org-get-at-bol 'org-hd-marker)
                             (org-get-at-bol 'org-marker))))
            (when (and (eq (marker-buffer m) (marker-buffer target-marker))
                       (= (marker-position m) (marker-position target-marker)))
              (setq found (cons (line-beginning-position) m))))
          (forward-line 1))
        found))))

(defun orgx-bulk-agenda--append-line-for-marker (marker &optional buffer)
  "Append a synthetic agenda line for MARKER in BUFFER and return MARKER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((inhibit-read-only t))
      (unless (derived-mode-p 'org-agenda-mode)
        (org-agenda-mode))
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (let ((start (point))
              (heading (if (and (markerp marker) (marker-buffer marker))
                           (org-with-point-at marker (org-get-heading t t t t))
                         "entry"))
              (todo (and (markerp marker) (marker-buffer marker)
                         (org-with-point-at marker (org-get-todo-state)))))
          (insert (format "  bulk:        %s\n" heading))
          (add-text-properties start (1- (point))
                               (list 'org-hd-marker marker
                                     'org-marker marker
                                     'todo-state todo)))))
    marker))

;;; Scope and Batch Builders

(defun orgx-bulk-agenda--scope-files (scope)
  "Resolve the list of Org files for SCOPE plist."
  (let ((type (or (plist-get scope :type) 'files))
        (files (plist-get scope :files))
        (file (plist-get scope :file)))
    (seq-filter
     (lambda (f) (and (stringp f) (file-exists-p f)))
     (mapcar #'expand-file-name
             (pcase type
               ('buffer (and file (list file)))
               ('agenda-files (org-agenda-files))
               ('denote-files (and (fboundp 'orgx-denote-files) (orgx-denote-files)))
               ('files files)
               (_ (or files (and file (list file)))))))))

(defun orgx-bulk-agenda--heading-matches-filter-p (marker scope)
  "Return non-nil if MARKER satisfies `:from-states' and `:only-open' in SCOPE."
  (when (and (markerp marker) (marker-buffer marker))
    (org-with-point-at marker
      (let ((state (org-get-todo-state))
            (from-states (plist-get scope :from-states))
            (only-open (plist-get scope :only-open)))
        (and (or (null from-states)
                 (member state from-states))
             (or (null only-open)
                 (and state (not (member state org-done-keywords)))))))))

(defun orgx-bulk-batch-for-scope (scope)
  "Build or reuse an agenda-buffer batch for SCOPE.
Returns `(AGENDA-BUFFER . TARGETS)' and sets buffer-local
`orgx-bulk-batch-markers' in AGENDA-BUFFER.  Preserves global
`org-agenda-bulk-marked-entries' so other sticky agendas are unaffected."
  (let ((type (or (plist-get scope :type) 'files))
        (saved-global-marks org-agenda-bulk-marked-entries)
        (orgx-bulk--internal-marking t))
    (unwind-protect
        (if (eq type 'agenda)
            (let* ((buf-name (plist-get scope :buffer-name))
                   (buf (or (and buf-name (get-buffer buf-name))
                            (and (derived-mode-p 'org-agenda-mode) (current-buffer))
                            (error "Agenda buffer not found for scope: %S" scope))))
              (with-current-buffer buf
                (let* ((line-markers (orgx-bulk-agenda--line-markers buf))
                       (marked (or (seq-filter (lambda (m) (memq m line-markers))
                                               org-agenda-bulk-marked-entries)
                                   orgx-bulk-batch-markers
                                   line-markers))
                       (filtered (seq-filter
                                  (lambda (m)
                                    (orgx-bulk-agenda--heading-matches-filter-p m scope))
                                  marked))
                       (targets (delq nil
                                      (mapcar (lambda (m)
                                                (org-with-point-at m
                                                  (orgx-bulk--target-at-point)))
                                              filtered))))
                  (setq-local orgx-bulk-batch-markers filtered)
                  (cons buf targets))))
          (let* ((files (orgx-bulk-agenda--scope-files scope))
                 (match (or (plist-get scope :match) "LEVEL>=1"))
                 (match-str (if (string-empty-p match) "LEVEL>=1" match))
                 (agenda-buf nil)
                 (batch-markers nil)
                 (targets nil))
            (if (null files)
                (let ((empty-buf (get-buffer-create "*Org Agenda(orgx-bulk-empty)*")))
                  (with-current-buffer empty-buf
                    (unless (derived-mode-p 'org-agenda-mode)
                      (org-agenda-mode))
                    (setq-local orgx-bulk-batch-markers nil))
                  (cons empty-buf nil))
              (save-window-excursion
                (let ((org-agenda-files files)
                      (org-agenda-sticky t)
                      (org-agenda-window-setup 'current-window)
                      (org-agenda-bulk-marked-entries nil))
                  (org-tags-view nil match-str)
                  (setq agenda-buf (current-buffer))
                  (goto-char (point-min))
                  (while (not (eobp))
                    (let ((m (or (org-get-at-bol 'org-hd-marker)
                                 (org-get-at-bol 'org-marker))))
                      (if (and m (orgx-bulk-agenda--heading-matches-filter-p m scope))
                          (progn
                            (push m batch-markers)
                            (push (org-with-point-at m (orgx-bulk--target-at-point))
                                  targets)
                            ;; `org-agenda-bulk-mark' moves forward 1 line.
                            (org-agenda-bulk-mark))
                        (forward-line 1))))
                  (setq batch-markers (nreverse batch-markers)
                        targets (nreverse (delq nil targets)))
                  (setq-local orgx-bulk-batch-markers batch-markers)))
              (cons agenda-buf targets))))
      (setq org-agenda-bulk-marked-entries saved-global-marks))))

(defun orgx-bulk-batch-ensure-agenda (plan resolved-markers)
  "Ensure an agenda buffer for PLAN contains lines for RESOLVED-MARKERS.
Returns `(AGENDA-BUFFER . BATCH-MARKERS)' where each element of BATCH-MARKERS
is the exact (`eq') `org-hd-marker' property on a line in AGENDA-BUFFER."
  (let* ((scope (orgx-bulk-plan-scope plan))
         (buf-name (or (plist-get scope :buffer-name)
                       (format "*Org Agenda(orgx-bulk:%s)*" (orgx-bulk-plan-id plan))))
         (buf (get-buffer buf-name)))
    (unless (buffer-live-p buf)
      (let ((files (or (orgx-bulk-agenda--scope-files scope)
                       (seq-uniq
                        (delq nil
                              (mapcar (lambda (m)
                                        (and (markerp m) (marker-buffer m)
                                             (buffer-file-name (marker-buffer m))))
                                      resolved-markers))))))
        (if files
            (save-window-excursion
              (let ((org-agenda-files files)
                    (org-agenda-sticky t)
                    (org-agenda-window-setup 'current-window)
                    (match (or (plist-get scope :match) "LEVEL>=1")))
                (org-tags-view nil (if (string-empty-p match) "LEVEL>=1" match))
                (setq buf (current-buffer))))
          (setq buf (get-buffer-create buf-name))
          (with-current-buffer buf
            (unless (derived-mode-p 'org-agenda-mode)
              (org-agenda-mode))))))
    (with-current-buffer buf
      (let (batch-markers)
        (dolist (rm resolved-markers)
          (if-let* ((found (orgx-bulk-agenda--find-line-for-marker rm buf)))
              (push (cdr found) batch-markers)
            (push (orgx-bulk-agenda--append-line-for-marker rm buf) batch-markers)))
        (setq batch-markers (nreverse batch-markers))
        (setq-local orgx-bulk-batch-markers batch-markers)
        (setq-local orgx-bulk-batch-plan-id (orgx-bulk-plan-id plan))
        (cons buf batch-markers)))))

(defun orgx-bulk-batch-rebuild (plan)
  "Rebuild the agenda buffer and marks for a resumed PLAN.
Opens or recreates PLAN's agenda buffer and re-marks the steps whose targets
still resolve via `orgx-bulk--resolve-target'.  Steps whose targets no longer
resolve are marked `missing'.  Returns the agenda buffer."
  (let ((resolved-markers nil)
        (rebuilt-marks nil)
        (orgx-bulk--internal-marking t))
    (dolist (step (orgx-bulk-plan-steps plan))
      (when (eq (orgx-bulk-step-status step) 'pending)
        (if-let* ((m (orgx-bulk--resolve-target (orgx-bulk-step-target step))))
            (push m resolved-markers)
          (setf (orgx-bulk-step-status step) 'missing
                (orgx-bulk-step-message step) "Target heading not found"))))
    (setq resolved-markers (nreverse resolved-markers))
    (pcase-let ((`(,buf . ,batch-markers)
                 (orgx-bulk-batch-ensure-agenda plan resolved-markers)))
      (with-current-buffer buf
        ;; Mark with an isolated list, then add the result to the global marks
        ;; so the resumed batch stays marked without disturbing other agendas.
        (let ((org-agenda-bulk-marked-entries nil))
          (org-agenda-bulk-unmark-all)
          (dolist (bm batch-markers)
            (when-let* ((pos (text-property-any (point-min) (point-max) 'org-hd-marker bm)))
              (goto-char pos)
              (org-agenda-bulk-mark)))
          (setq rebuilt-marks org-agenda-bulk-marked-entries))
        (setq-local orgx-bulk-batch-markers batch-markers))
      (setq org-agenda-bulk-marked-entries
            (append rebuilt-marks org-agenda-bulk-marked-entries))
      (orgx-bulk-save plan)
      buf)))

;;; Interactive Scope Reader (used by `orgx-bulk-todo-state')

(defun orgx-bulk-batch-read-scope ()
  "Interactively read a bulk scope and return `(SCOPE TARGETS DESCRIPTION)'."
  (if (derived-mode-p 'org-agenda-mode)
      (let* ((scope (list :type 'agenda :buffer-name (buffer-name)))
             (pair (orgx-bulk-batch-for-scope scope)))
        (list (plist-put scope :buffer-name (buffer-name (car pair)))
              (cdr pair)
              (format "agenda %s" (buffer-name (car pair)))))
    (let* ((choice (completing-read
                    "Scope: "
                    (append (when (and (derived-mode-p 'org-mode) (buffer-file-name))
                              '("current buffer"))
                            '("agenda files")
                            (when (fboundp 'orgx-denote-files)
                              '("denote files")))
                    nil t))
           (match (string-trim (read-string "Match (empty for all open TODOs): ")))
           (match-or-nil (unless (string-empty-p match) match))
           (scope
            (pcase choice
              ("current buffer"
               (list :type 'buffer
                     :file (buffer-file-name)
                     :files (list (buffer-file-name))
                     :match match-or-nil
                     :only-open (null match-or-nil)))
              ("agenda files"
               (list :type 'agenda-files
                     :files (org-agenda-files)
                     :match match-or-nil
                     :only-open (null match-or-nil)))
              ("denote files"
               (list :type 'denote-files
                     :files (orgx-denote-files)
                     :match match-or-nil
                     :only-open (null match-or-nil)))))
           (pair (orgx-bulk-batch-for-scope scope))
           (buf (car pair))
           (targets (cdr pair)))
      (list (plist-put scope :buffer-name (buffer-name buf))
            targets
            choice))))

;;; Agenda `B' Driver (`orgx-agenda-bulk-action')

(defun orgx-bulk-agenda--prefix-log-notes (arg)
  "Return `orgx-bulk-log-notes' override for prefix ARG."
  (if (numberp arg)
      (cond
       ((<= arg 0) 'ignore)
       ((= arg 1) 'one)
       (t 'each))
    orgx-bulk-log-notes))

(defun orgx-bulk-agenda--prompt-key ()
  "Prompt for a registered bulk operation key and return the character."
  (let* ((ops (sort (copy-sequence (orgx-bulk-registered-ops))
                    (lambda (a b)
                      (< (or (orgx-bulk-op-spec-key a) 0)
                         (or (orgx-bulk-op-spec-key b) 0)))))
         (keys-desc
          (mapconcat (lambda (spec)
                       (if-let* ((k (orgx-bulk-op-spec-key spec)))
                           (format "[%c]%s" k (orgx-bulk-op-spec-label spec))
                         ""))
                     ops " ")))
    (message "orgx-bulk: %s" keys-desc)
    (read-char-exclusive)))

;;;###autoload
(defun orgx-agenda-bulk-action (&optional arg)
  "Plan or execute a bulk operation on marked entries in the current agenda.
By default, builds and saves an `orgx-bulk-plan' and opens its plan buffer.
With `\\[universal-argument]' (ARG = `(4)'), executes the plan immediately.
With a numeric prefix ARG, sets `orgx-bulk-log-notes' for the plan (`0' =
`ignore', `1' = `one', `2+' = `each')."
  (interactive "P")
  (require 'orgx-bulk-ops)
  (require 'orgx-bulk-buffer)
  (unless (derived-mode-p 'org-agenda-mode)
    (user-error "Must be called from an Org agenda buffer"))
  (when orgx-bulk-batch-locked
    (user-error "Agenda marks are locked while bulk operation %s is pending"
                orgx-bulk-batch-locked))
  (let* ((line-markers (orgx-bulk-agenda--line-markers (current-buffer)))
         (marked (seq-filter (lambda (m) (memq m line-markers))
                             org-agenda-bulk-marked-entries)))
    (unless marked
      (let ((orgx-bulk--internal-marking t))
        (save-excursion
          (org-agenda-bulk-mark)))
      (setq marked (seq-filter (lambda (m) (memq m line-markers))
                               org-agenda-bulk-marked-entries)))
    (unless marked
      (user-error "No agenda entries marked for bulk operation"))
    (setq-local orgx-bulk-batch-markers marked)
    (let* ((key (orgx-bulk-agenda--prompt-key))
           (spec (or (orgx-bulk-op-for-key key)
                     (user-error "Unknown bulk operation key: %c" key)))
           (op-args (when-let* ((reader (orgx-bulk-op-spec-read-args spec)))
                      (funcall reader)))
           (log-notes (orgx-bulk-agenda--prefix-log-notes arg))
           (note (when (and (eq (orgx-bulk-op-spec-name spec) 'todo-state)
                            (eq log-notes 'one))
                   (read-string "Log note for plan: ")))
           (targets (delq nil
                          (mapcar (lambda (m)
                                    (org-with-point-at m
                                      (orgx-bulk--target-at-point)))
                                  marked)))
           (scope (list :type 'agenda :buffer-name (buffer-name)))
           (op (orgx-bulk-op--create :name (orgx-bulk-op-spec-name spec)
                                     :args op-args))
           (plan (orgx-bulk-create-plan
                  :scope scope
                  :targets targets
                  :ops (list op)
                  :description (format "%s in %s"
                                       (orgx-bulk-describe-op op)
                                       (buffer-name))
                  :log-notes log-notes
                  :note note)))
      (setq-local orgx-bulk-batch-plan-id (orgx-bulk-plan-id plan))
      (if (equal arg '(4))
          (orgx-bulk-execute plan)
        (orgx-bulk-review plan t)))))

(provide 'orgx-bulk-agenda)
;;; orgx-bulk-agenda.el ends here
