;;; orgx-bulk-ops.el --- Built-in operations for orgx-bulk -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, convenience

;;; Commentary:
;; Built-in bulk operations registered with `orgx-bulk-register-op':
;; - `todo-state' (`t'): `org-todo' with `orgx-bulk-log-notes' and blocking policy
;; - `schedule' (`s'): `org-schedule'
;; - `deadline' (`d'): `org-deadline'
;; - `tag-add' (`+'): `org-toggle-tag' on
;; - `tag-remove' (`-'): `org-toggle-tag' off
;; - `archive' (`$'): `org-archive-subtree' (runs last per target)
;; - `archive-sibling' (`A'): `org-archive-to-archive-sibling' (runs last)
;; - `refile' (`r'): `org-refile' to a stored location (runs last)
;; - `priority' (`p'): `org-priority'
;; - `property-set' (`P'): `org-set-property' (custom oab key)
;; - `function' (`f'): named function called at the heading

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-refile)
(require 'org-archive)
(require 'orgx-bulk)

(defvar org-todo-keywords-1)
(defvar org-done-keywords)
(defvar org-inhibit-logging)
(defvar org-inhibit-blocking)
(defvar org-enforce-todo-dependencies)
(defvar org-blocker-hook)
(defvar org-log-reschedule)
(defvar org-log-redeadline)
(defvar org-log-refile)
(defvar org-refile-allow-creating-parent-nodes)

(declare-function org-block-todo-from-children-or-siblings-or-parent "org")

(defun orgx-bulk-ops--json-get (obj &rest keys)
  "Extract the first non-nil value for KEYS (symbols or strings) from OBJ."
  (seq-some (lambda (k)
              (let ((sym (if (symbolp k) k (intern (string-trim-left k ":"))))
                    (kw (if (keywordp k) k (intern (concat ":" (format "%s" k)))))
                    (str (format "%s" k)))
                (or (and (plistp obj) (plist-get obj kw))
                    (and (plistp obj) (plist-get obj sym))
                    (and (listp obj) (alist-get sym obj))
                    (and (listp obj) (alist-get str obj nil nil #'equal))
                    (and (hash-table-p obj) (or (gethash str obj) (gethash sym obj))))))
            keys))

;;; 1. `todo-state' (`t')

(defun orgx-bulk-ops--validate-todo-state (state)
  "Return a problem string if STATE is not a valid TODO keyword, or nil."
  (cond
   ((or (null state) (string-empty-p state))
    "empty target state")
   ((and org-todo-keywords-1 (not (member state org-todo-keywords-1)))
    (format "unknown state %s" state))))

(defun orgx-bulk-ops--apply-todo-state (state)
  "Apply `todo-state' STATE to the heading at point honoring notes and blocking."
  (let* ((from-state (org-get-todo-state))
         (plan orgx-bulk--active-plan)
         (log-mode (if plan (orgx-bulk-plan-log-notes plan) orgx-bulk-log-notes))
         (step (and orgx-bulk--active-step-map
                    (cdr (seq-find
                          (lambda (cell)
                            (let ((m (car cell)))
                              (and (markerp m)
                                   (eq (marker-buffer m) (current-buffer))
                                   (= (marker-position m) (line-beginning-position)))))
                          orgx-bulk--active-step-map))))
         (note-text
          (pcase log-mode
            ('one (and plan (orgx-bulk-plan-note plan)))
            ('each (and step (orgx-bulk-step-note step)))
            (_ nil)))
         (org-inhibit-blocking orgx-bulk-inhibit-blocking)
         (org-inhibit-logging t)
         (org-blocker-hook
          (if (and (not orgx-bulk-inhibit-blocking)
                   org-enforce-todo-dependencies
                   (fboundp 'org-block-todo-from-children-or-siblings-or-parent)
                   (not (memq #'org-block-todo-from-children-or-siblings-or-parent
                              org-blocker-hook)))
              (cons #'org-block-todo-from-children-or-siblings-or-parent
                    org-blocker-hook)
            org-blocker-hook)))
    ;; A blocked `org-todo' quietly leaves the buffer untouched.  An unchanged
    ;; state alone is not failure: repeating tasks revert to TODO but edit the
    ;; heading.
    (let ((tick (buffer-chars-modified-tick)))
      (org-todo state)
      (unless (or (equal (org-get-todo-state) state)
                  (/= tick (buffer-chars-modified-tick)))
        (error "TODO state change to %s blocked" state)))
    (when (and (memq log-mode '(one each))
               note-text
               (not (string-empty-p note-text)))
      (orgx-bulk-inject-log-note note-text state from-state))))

(orgx-bulk-register-op 'todo-state
  :key ?t
  :label "todo"
  :builtin-oab t
  :queueable t
  :read-args (lambda ()
               (list (completing-read
                      "Todo state: "
                      (seq-uniq (append org-todo-keywords-1 org-done-keywords))
                      nil nil)))
  :validate #'orgx-bulk-ops--validate-todo-state
  :apply #'orgx-bulk-ops--apply-todo-state
  :describe (lambda (state) (format "todo -> %s" state))
  :from-json (lambda (args)
               (list (orgx-bulk-ops--json-get args 'state 'to_state 'todo)))
  :schema '((:name "state" :type "string" :description "Target Org TODO keyword")))

;;; 2. `schedule' (`s')

(orgx-bulk-register-op 'schedule
  :key ?s
  :label "schedule"
  :builtin-oab t
  :queueable t
  :read-args (lambda ()
               (list (org-read-date nil nil nil "(Re)Schedule to")))
  :apply (lambda (time)
           (let ((org-log-reschedule (and org-log-reschedule 'time)))
             (if (or (null time) (equal time "") (eq time 'remove))
                 (org-schedule '(4))
               (org-schedule nil time))))
  :describe (lambda (time)
              (if (or (null time) (equal time "") (eq time 'remove))
                  "schedule clear"
                (format "schedule %s" time)))
  :from-json (lambda (args)
               (list (orgx-bulk-ops--json-get args 'time 'date 'schedule)))
  :schema '((:name "time" :type "string" :description "Date/time string or empty to clear")))

;;; 3. `deadline' (`d')

(orgx-bulk-register-op 'deadline
  :key ?d
  :label "deadline"
  :builtin-oab t
  :queueable t
  :read-args (lambda ()
               (list (org-read-date nil nil nil "(Re)Set Deadline to")))
  :apply (lambda (time)
           (let ((org-log-redeadline (and org-log-redeadline 'time)))
             (if (or (null time) (equal time "") (eq time 'remove))
                 (org-deadline '(4))
               (org-deadline nil time))))
  :describe (lambda (time)
              (if (or (null time) (equal time "") (eq time 'remove))
                  "deadline clear"
                (format "deadline %s" time)))
  :from-json (lambda (args)
               (list (orgx-bulk-ops--json-get args 'time 'date 'deadline)))
  :schema '((:name "time" :type "string" :description "Date/time string or empty to clear")))

;;; 4. `tag-add' (`+')

(orgx-bulk-register-op 'tag-add
  :key ?+
  :label "+tag"
  :builtin-oab t
  :queueable t
  :read-args (lambda ()
               (list (string-trim (read-string "Tag to add: "))))
  :validate (lambda (tag)
              (when (or (null tag) (string-empty-p tag))
                "empty tag"))
  :apply (lambda (tag)
           (org-toggle-tag tag 'on))
  :describe (lambda (tag) (format "+%s" tag))
  :from-json (lambda (args)
               (list (orgx-bulk-ops--json-get args 'tag)))
  :schema '((:name "tag" :type "string" :description "Tag to add to heading")))

;;; 5. `tag-remove' (`-')

(orgx-bulk-register-op 'tag-remove
  :key ?-
  :label "-tag"
  :builtin-oab t
  :queueable t
  :read-args (lambda ()
               (list (string-trim (read-string "Tag to remove: "))))
  :validate (lambda (tag)
              (when (or (null tag) (string-empty-p tag))
                "empty tag"))
  :apply (lambda (tag)
           (org-toggle-tag tag 'off))
  :describe (lambda (tag) (format "-%s" tag))
  :from-json (lambda (args)
               (list (orgx-bulk-ops--json-get args 'tag)))
  :schema '((:name "tag" :type "string" :description "Tag to remove from heading")))

;;; 6. `archive' (`$')

(orgx-bulk-register-op 'archive
  :key ?$
  :label "archive"
  :builtin-oab t
  :queueable t
  :order-last t
  :read-args nil
  :apply (lambda (&rest _)
           (org-archive-subtree))
  :describe (lambda (&rest _) "archive")
  :from-json (lambda (_args) nil)
  :schema nil)

;;; 7. `archive-sibling' (`A')

(orgx-bulk-register-op 'archive-sibling
  :key ?A
  :label "arch->sib"
  :builtin-oab t
  :queueable t
  :order-last t
  :read-args nil
  :apply (lambda (&rest _)
           (org-archive-to-archive-sibling))
  :describe (lambda (&rest _) "archive-sibling")
  :from-json (lambda (_args) nil)
  :schema nil)

;;; 8. `refile' (`r')

(defun orgx-bulk-ops--read-refile-args ()
  "Interactively read a refile location and return a serializable arg list."
  (let* ((rfloc (org-refile-get-location
                 "Refile to" nil org-refile-allow-creating-parent-nodes))
         (heading (nth 0 rfloc))
         (file (nth 1 rfloc))
         (pos (nth 3 rfloc)))
    (list (list :file file
                :heading heading
                :pos (if (markerp pos) (marker-position pos) pos)))))

(defun orgx-bulk-ops--apply-refile (loc)
  "Refile the heading at point to serializable location LOC."
  (let* ((file (cond
                ((plistp loc) (plist-get loc :file))
                ((consp loc) (nth 1 loc))))
         (heading (cond
                   ((plistp loc) (plist-get loc :heading))
                   ((consp loc) (nth 0 loc))))
         (hint-pos (cond
                    ((plistp loc) (plist-get loc :pos))
                    ((consp loc) (let ((p (nth 3 loc)))
                                   (if (markerp p) (marker-position p) p)))))
         (id (and (plistp loc) (plist-get loc :id)))
         (path (and (plistp loc) (plist-get loc :outline-path))))
    (unless (and file (file-exists-p file))
      (error "Refile target file does not exist: %S" file))
    (let* ((resolved (orgx-bulk--resolve-target
                      (list :file file :id id :outline-path path
                            :heading heading :pos hint-pos)))
           ;; Without a position or a distinct heading the target is the file.
           (file-level (and (null hint-pos)
                            (member heading (list nil (file-name-nondirectory file)))))
           (dst-pos (cond
                     (resolved (marker-position resolved))
                     (file-level nil)
                     (t (error "Refile target heading not found: %s" heading))))
           (rfloc (list (or heading (file-name-nondirectory file))
                        (expand-file-name file)
                        nil
                        dst-pos))
           (org-log-refile (if org-log-refile 'time nil)))
      (org-refile nil nil rfloc)
      ;; The source file is saved after execution; make sure the destination
      ;; is too, so the heading is never only in an unsaved buffer.
      (when orgx-bulk-save-buffers
        (with-current-buffer (find-file-noselect file)
          (when (buffer-modified-p)
            (let ((inhibit-message t))
              (save-buffer))))))))

(orgx-bulk-register-op 'refile
  :key ?r
  :label "refile"
  :builtin-oab t
  :queueable t
  :order-last t
  :read-args #'orgx-bulk-ops--read-refile-args
  :apply #'orgx-bulk-ops--apply-refile
  :describe (lambda (loc)
              (let ((file (if (plistp loc) (plist-get loc :file) (nth 1 loc)))
                    (hd (if (plistp loc) (plist-get loc :heading) (nth 0 loc))))
                (format "refile -> %s:%s"
                        (if file (file-name-nondirectory file) "?")
                        (or hd ""))))
  :from-json (lambda (args)
               (list (list :file (orgx-bulk-ops--json-get args 'file)
                           :heading (orgx-bulk-ops--json-get args 'heading)
                           :id (orgx-bulk-ops--json-get args 'id)
                           :pos (orgx-bulk-ops--json-get args 'pos))))
  :schema '((:name "file" :type "string" :description "Destination Org file")
            (:name "heading" :type "string" :description "Destination heading name")))

;;; 9. `priority' (`p')

(defun orgx-bulk-ops--normalize-priority (prio)
  "Normalize PRIO into a character or `remove'."
  (cond
   ((or (null prio) (eq prio 'remove) (equal prio ?\s)
        (and (stringp prio)
             (or (string-empty-p (string-trim prio))
                 (string-equal-ignore-case (string-trim prio) "remove"))))
    'remove)
   ((characterp prio) (upcase prio))
   ((stringp prio) (upcase (string-to-char (string-trim prio))))
   ((symbolp prio) (upcase (string-to-char (symbol-name prio))))
   (t (error "Invalid priority: %S" prio))))

(orgx-bulk-register-op 'priority
  :key ?p
  :label "priority"
  :builtin-oab t
  :queueable t
  :read-args (lambda ()
               (let ((ch (read-char-exclusive "Priority [A-Z, SPC to remove]: ")))
                 (list (if (eql ch ?\s) "remove" (char-to-string (upcase ch))))))
  :apply (lambda (prio)
           (let ((norm (orgx-bulk-ops--normalize-priority prio)))
             (org-priority norm)))
  :describe (lambda (prio)
              (let ((norm (orgx-bulk-ops--normalize-priority prio)))
                (if (eq norm 'remove)
                    "priority clear"
                  (format "priority [#%c]" norm))))
  :from-json (lambda (args)
               (list (orgx-bulk-ops--json-get args 'priority 'value)))
  :schema '((:name "priority" :type "string" :description "Priority letter (e.g. A, B, C) or 'remove'")))

;;; 10. `property-set' (`P')

(orgx-bulk-register-op 'property-set
  :key ?P
  :label "property"
  :builtin-oab nil
  :queueable t
  :read-args (lambda ()
               (let* ((prop (org-read-property-name))
                      (val (org-read-property-value prop)))
                 (list prop val)))
  :validate (lambda (prop _val)
              (when (or (null prop) (string-empty-p prop))
                "empty property name"))
  :apply (lambda (prop val)
           (org-set-property prop (format "%s" (or val ""))))
  :describe (lambda (prop val)
              (format "%s=%s" prop (or val "")))
  :from-json (lambda (args)
               (list (orgx-bulk-ops--json-get args 'property 'name)
                     (orgx-bulk-ops--json-get args 'value)))
  :schema '((:name "property" :type "string" :description "Org property name")
            (:name "value" :type "string" :description "Org property value")))

;;; 11. `function' (`f')

(orgx-bulk-register-op 'function
  :key ?f
  :label "function"
  :builtin-oab t
  :queueable nil
  :read-args (lambda ()
               (list (intern (completing-read "Function: " obarray #'fboundp t))))
  :validate (lambda (fn &rest _)
              (let ((sym (if (stringp fn) (intern fn) fn)))
                (unless (or (functionp sym) (and (symbolp sym) (fboundp sym)))
                  (format "undefined function %S" fn))))
  :apply (lambda (fn &optional fn-args)
           (let ((callable (if (stringp fn) (intern fn) fn)))
             (unless (or (functionp callable) (and (symbolp callable) (fboundp callable)))
               (error "Undefined function: %S" fn))
             (if fn-args
                 (apply callable fn-args)
               (funcall callable))))
  :describe (lambda (fn &rest _)
              (format "fn %s" fn))
  :from-json (lambda (args)
               (let ((fn (orgx-bulk-ops--json-get args 'function 'fn 'name)))
                 (list (if (stringp fn) (intern fn) fn))))
  :schema '((:name "function" :type "string" :description "Interactive or 0-arg Elisp function name")))

(provide 'orgx-bulk-ops)
;;; orgx-bulk-ops.el ends here
