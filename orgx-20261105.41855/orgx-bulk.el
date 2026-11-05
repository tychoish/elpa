;;; orgx-bulk.el --- Resumable bulk operations driven by org-agenda-bulk-action -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, convenience

;;; Commentary:
;; Resumable bulk operations on Org headings built on top of
;; `org-agenda-bulk-action'.
;;
;; `orgx-bulk' provides:
;; - Data model (`orgx-bulk-plan', `orgx-bulk-op', `orgx-bulk-step') with
;;   atomic `.eld' persistence in `orgx-bulk-directory'.
;; - Target capture and resolution by ID, outline path + heading, or position.
;; - Operation registry (`orgx-bulk-register-op') bridged into
;;   `org-agenda-bulk-custom-functions'.
;; - Note-logging modes (`ignore', `one', `each') via `orgx-bulk-log-notes'.
;; - Synchronous, quit-safe execution through `org-agenda-bulk-action'
;;   (`orgx-bulk-execute').

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-agenda)
(require 'org-id)

(declare-function orgx-denote-plan-file-p "orgx-denote")
(declare-function orgx-denote-files "orgx-denote")
(declare-function orgx-bulk-batch-for-scope "orgx-bulk-agenda")
(declare-function orgx-bulk-batch-ensure-agenda "orgx-bulk-agenda")
(declare-function orgx-bulk-batch-read-scope "orgx-bulk-agenda")
(declare-function orgx-bulk-plan-buffer-name "orgx-bulk-buffer")
(declare-function orgx-bulk-review "orgx-bulk-buffer")

(defvar org-agenda-bulk-marked-entries)
(defvar org-agenda-bulk-custom-functions)
(defvar org-log-setup)
(defvar org-inhibit-logging)
(defvar org-inhibit-blocking)
(defvar orgx-bulk--internal-marking nil)

(defgroup orgx-bulk nil
  "Bulk task operations for orgx."
  :group 'orgx
  :prefix "orgx-bulk-")

(defcustom orgx-bulk-directory (locate-user-emacs-file "orgx-bulk")
  "Directory where saved `orgx-bulk-plan' files (`.eld') are stored."
  :type 'directory
  :group 'orgx-bulk)

(defcustom orgx-bulk-log-notes 'ignore
  "How bulk TODO state changes record log notes.
- `ignore': suppress log notes (`org-inhibit-logging' is bound to t).
- `one': record a single note message on every changed heading.
- `each': prompt for a note on each heading at execution time and store it
  on the step so resuming does not ask twice."
  :type '(choice (const :tag "Ignore notes" ignore)
                 (const :tag "One note for all headings" one)
                 (const :tag "Prompt per heading" each))
  :group 'orgx-bulk)

(defcustom orgx-bulk-inhibit-blocking nil
  "When non-nil, ignore Org TODO blocking rules during bulk state changes.
By default (nil), blocking rules such as `org-enforce-todo-dependencies'
and `org-blocker-hook' are honored and blocked steps are marked `failed'."
  :type 'boolean
  :group 'orgx-bulk)

(defcustom orgx-bulk-assign-ids nil
  "When non-nil, assign an `:ID:' property to targeted headings when planning."
  :type 'boolean
  :group 'orgx-bulk)

(defvaralias 'orgx-bulk-item-functions 'orgx-bulk-step-functions)

(defcustom orgx-bulk-step-functions nil
  "Abnormal hook run after each step succeeds.
Each function receives the `orgx-bulk-step' and its `orgx-bulk-plan', with
point on the heading in the step's base Org buffer."
  :type 'hook
  :group 'orgx-bulk)

(defcustom orgx-bulk-after-apply-functions '(orgx-bulk-denote-sync-execution-status)
  "Abnormal hook run once per touched file after a plan executes.
Each function receives FILE, the list of done `orgx-bulk-step's in it, and
the `orgx-bulk-plan'.  Runs with FILE's buffer current, before it is saved."
  :type 'hook
  :group 'orgx-bulk)

(defcustom orgx-bulk-save-buffers t
  "When non-nil, save each file buffer touched by an executed plan."
  :type 'boolean
  :group 'orgx-bulk)

(defconst orgx-bulk-format-version 1
  "Format version for saved `.eld' plan files.")

(defconst orgx-bulk-oab-builtin-keys
  '(?$ ?A ?t ?+ ?- ?s ?d ?r ?w ?S ?f ?p)
  "Action keys owned by `org-agenda-bulk-action'.")

;;; Data Model

(cl-defstruct (orgx-bulk-plan (:constructor orgx-bulk-plan--create))
  "A saved, resumable bulk operation plan.
STATUS is one of `draft', `running', `paused', `complete', or `discarded'."
  id description created (status 'draft) scope
  (log-notes orgx-bulk-log-notes) note ops steps)

(cl-defstruct (orgx-bulk-op (:constructor orgx-bulk-op--create))
  "One operation in an `orgx-bulk-plan'.
STATUS is one of `pending', `running', `done', `failed', or `paused'."
  name args (status 'pending))

(cl-defstruct (orgx-bulk-step (:constructor orgx-bulk-step--create))
  "One target heading and operation pairing in an `orgx-bulk-plan'.
STATUS is one of `pending', `done', `failed', `skipped', or `missing'."
  id target op (status 'pending) message note)

(cl-defstruct (orgx-bulk-op-spec (:constructor orgx-bulk-op-spec--create))
  "Registered operation specification."
  name key label read-args apply describe from-json schema validate
  queueable order-last builtin-oab)

(defvar orgx-bulk--plans (make-hash-table :test #'equal)
  "In-memory cache of `orgx-bulk-plan' objects keyed by ID.")

(defvar orgx-bulk--counter 0
  "Monotonic counter for generating unique plan IDs.")

(defun orgx-bulk--next-id ()
  "Return a fresh plan ID string that does not collide with saved plans."
  (let (id)
    (while (progn
             (setq orgx-bulk--counter (1+ orgx-bulk--counter))
             (setq id (format "bulk-%d" orgx-bulk--counter))
             (or (map-elt orgx-bulk--plans id)
                 (and orgx-bulk-directory
                      (file-exists-p (orgx-bulk-plan-file id))))))
    id))

(defun orgx-bulk-step-op-name (step)
  "Return the operation symbol for STEP."
  (let ((op (orgx-bulk-step-op step)))
    (if (orgx-bulk-op-p op)
        (orgx-bulk-op-name op)
      op)))

(defun orgx-bulk-step-op-args (step)
  "Return the operation argument list for STEP."
  (let ((op (orgx-bulk-step-op step)))
    (when (orgx-bulk-op-p op)
      (orgx-bulk-op-args op))))

;;; Target Capture and Resolution

(defun orgx-bulk--target-at-point ()
  "Return a serializable target plist for the Org heading at point.
When called in `org-agenda-mode', follows `org-hd-marker' or `org-marker'.
Never includes a live marker."
  (let ((marker (when (derived-mode-p 'org-agenda-mode)
                  (or (org-get-at-bol 'org-hd-marker)
                      (org-get-at-bol 'org-marker)))))
    (if marker
        (when (and (markerp marker) (marker-buffer marker))
          (org-with-point-at marker
            (orgx-bulk--target-at-heading)))
      (orgx-bulk--target-at-heading))))

(defun orgx-bulk--target-at-heading ()
  "Build target plist in the current Org buffer at or inside a heading."
  (save-excursion
    (save-restriction
      (widen)
      (org-back-to-heading t)
      (when orgx-bulk-assign-ids
        (org-id-get-create))
      (list :file (buffer-file-name (org-base-buffer (current-buffer)))
            :id (org-entry-get nil "ID")
            :outline-path (org-get-outline-path)
            :heading (org-get-heading t t t t)
            :pos (line-beginning-position)
            :todo-state (org-get-todo-state)))))

(defun orgx-bulk--resolve-target (target)
  "Resolve TARGET plist to a marker on its heading, or nil if missing.
Resolves by `:id' first, then `:outline-path' and `:heading' (closest to
`:pos' when duplicated), and finally `:pos' with `:heading'."
  (let ((file (plist-get target :file))
        (id (plist-get target :id))
        (path (plist-get target :outline-path))
        (heading (plist-get target :heading))
        (hint-pos (or (plist-get target :pos) 1)))
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (save-restriction
            (widen)
            (or
             ;; 1. Resolve by ID property inside this file.
             (when id
               (let ((pos (org-find-entry-with-id id)))
                 (when pos
                   (goto-char pos)
                   (org-back-to-heading t)
                   (point-marker))))
             ;; 2. Resolve by outline-path + heading (pick closest to hint-pos).
             (when heading
               (let (candidates)
                 (goto-char (point-min))
                 (when (org-at-heading-p)
                   (when (and (equal (org-get-heading t t t t) heading)
                              (equal (org-get-outline-path) path))
                     (push (line-beginning-position) candidates)))
                 (while (outline-next-heading)
                   (when (and (equal (org-get-heading t t t t) heading)
                              (equal (org-get-outline-path) path))
                     (push (line-beginning-position) candidates)))
                 (when candidates
                   (let ((best (car (sort (nreverse candidates)
                                          (lambda (a b)
                                            (< (abs (- a hint-pos))
                                               (abs (- b hint-pos))))))))
                     (goto-char best)
                     (point-marker)))))
             ;; 3. Fallback: position hint with matching heading text.
             (when (and hint-pos (<= (point-min) hint-pos) (<= hint-pos (point-max)))
               (goto-char hint-pos)
               (when (ignore-errors (org-back-to-heading t))
                 (when (or (null heading)
                           (equal (org-get-heading t t t t) heading))
                   (point-marker)))))))))))

;;; Persistence

(defun orgx-bulk-plan-file (id)
  "Return the absolute file path for plan ID in `orgx-bulk-directory'."
  (expand-file-name (format "%s.eld" id) orgx-bulk-directory))

(defun orgx-bulk--plan-to-plist (plan)
  "Convert `orgx-bulk-plan' PLAN into a printable plist."
  (let ((ops (orgx-bulk-plan-ops plan)))
    (list :version orgx-bulk-format-version
          :id (orgx-bulk-plan-id plan)
          :description (orgx-bulk-plan-description plan)
          :created (orgx-bulk-plan-created plan)
          :status (orgx-bulk-plan-status plan)
          :scope (orgx-bulk-plan-scope plan)
          :log-notes (orgx-bulk-plan-log-notes plan)
          :note (orgx-bulk-plan-note plan)
          :ops (mapcar (lambda (op)
                         (list :name (orgx-bulk-op-name op)
                               :args (orgx-bulk-op-args op)
                               :status (orgx-bulk-op-status op)))
                       ops)
          :steps (mapcar (lambda (step)
                           (let* ((sop (orgx-bulk-step-op step))
                                  (idx (and (orgx-bulk-op-p sop)
                                            (seq-position ops sop #'eq))))
                             (list :id (orgx-bulk-step-id step)
                                   :target (orgx-bulk-step-target step)
                                   :op-index idx
                                   :op (if (orgx-bulk-op-p sop)
                                           (list :name (orgx-bulk-op-name sop)
                                                 :args (orgx-bulk-op-args sop)
                                                 :status (orgx-bulk-op-status sop))
                                         sop)
                                   :status (orgx-bulk-step-status step)
                                   :message (orgx-bulk-step-message step)
                                   :note (orgx-bulk-step-note step))))
                         (orgx-bulk-plan-steps plan)))))

(defun orgx-bulk--plist-to-plan (data)
  "Reconstruct an `orgx-bulk-plan' from plist DATA."
  (let* ((ops (mapcar (lambda (op-pl)
                        (orgx-bulk-op--create
                         :name (plist-get op-pl :name)
                         :args (plist-get op-pl :args)
                         :status (or (plist-get op-pl :status) 'pending)))
                      (plist-get data :ops)))
         (steps (mapcar (lambda (st-pl)
                          (let* ((idx (plist-get st-pl :op-index))
                                 (raw-op (plist-get st-pl :op))
                                 (op (cond
                                      ((and (integerp idx) (nth idx ops))
                                       (nth idx ops))
                                      ((and (consp raw-op) (plistp raw-op))
                                       (or (seq-find (lambda (o)
                                                       (and (eq (orgx-bulk-op-name o)
                                                                (plist-get raw-op :name))
                                                            (equal (orgx-bulk-op-args o)
                                                                   (plist-get raw-op :args))))
                                                     ops)
                                           (orgx-bulk-op--create
                                            :name (plist-get raw-op :name)
                                            :args (plist-get raw-op :args)
                                            :status (or (plist-get raw-op :status) 'pending))))
                                      ((symbolp raw-op)
                                       (or (seq-find (lambda (o) (eq (orgx-bulk-op-name o) raw-op))
                                                     ops)
                                           (orgx-bulk-op--create :name raw-op)))
                                      (t raw-op))))
                            (orgx-bulk-step--create
                             :id (plist-get st-pl :id)
                             :target (plist-get st-pl :target)
                             :op op
                             :status (or (plist-get st-pl :status) 'pending)
                             :message (plist-get st-pl :message)
                             :note (plist-get st-pl :note))))
                        (plist-get data :steps))))
    (orgx-bulk-plan--create
     :id (plist-get data :id)
     :description (plist-get data :description)
     :created (plist-get data :created)
     :status (or (plist-get data :status) 'draft)
     :scope (plist-get data :scope)
     :log-notes (or (plist-get data :log-notes) 'ignore)
     :note (plist-get data :note)
     :ops ops
     :steps steps)))

(defun orgx-bulk-save (plan)
  "Save PLAN to `orgx-bulk-directory' atomically via temp file + rename.
Returns PLAN."
  (make-directory orgx-bulk-directory t)
  (let* ((id (orgx-bulk-plan-id plan))
         (target-file (orgx-bulk-plan-file id))
         (temp-file (make-temp-file
                     (expand-file-name (format ".%s-" id) orgx-bulk-directory)
                     nil ".tmp"))
         (print-length nil)
         (print-level nil))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert ";; -*- mode: lisp-data; -*-\n")
            (pp (orgx-bulk--plan-to-plist plan) (current-buffer)))
          (rename-file temp-file target-file t))
      (when (file-exists-p temp-file)
        (ignore-errors (delete-file temp-file)))))
  (setf (map-elt orgx-bulk--plans (orgx-bulk-plan-id plan)) plan)
  plan)

(defun orgx-bulk-load (id-or-file)
  "Load and return the `orgx-bulk-plan' from ID-OR-FILE.
Signals an `error' naming the file if it is missing or corrupt."
  (let ((file (if (and (stringp id-or-file)
                       (or (file-name-absolute-p id-or-file)
                           (string-suffix-p ".eld" id-or-file)
                           (file-exists-p id-or-file)))
                  (expand-file-name id-or-file)
                (orgx-bulk-plan-file id-or-file))))
    (unless (file-exists-p file)
      (error "Corrupt or missing plan file %s: file does not exist" file))
    (let ((data (condition-case err
                    (with-temp-buffer
                      (insert-file-contents file)
                      (goto-char (point-min))
                      (read (current-buffer)))
                  (error
                   (error "Corrupt plan file %s: %s" file (error-message-string err))))))
      (unless (and (consp data)
                   (plistp data)
                   (equal (plist-get data :version) orgx-bulk-format-version)
                   (stringp (plist-get data :id)))
        (error "Corrupt plan file %s: invalid plan structure or version" file))
      (let ((plan (orgx-bulk--plist-to-plan data)))
        (setf (map-elt orgx-bulk--plans (orgx-bulk-plan-id plan)) plan)
        plan))))

(defun orgx-bulk-get-plan (id)
  "Return the plan registered or saved under ID, or nil."
  (or (map-elt orgx-bulk--plans id)
      (let ((file (and id orgx-bulk-directory (orgx-bulk-plan-file id))))
        (when (and file (file-exists-p file))
          (ignore-errors (orgx-bulk-load file))))))

(defun orgx-bulk-list-plans (&optional statuses)
  "Return all saved and in-memory plans, filtered by STATUSES when non-nil."
  (when (and orgx-bulk-directory (file-directory-p orgx-bulk-directory))
    (dolist (file (directory-files orgx-bulk-directory t "\\`[^.].*\\.eld\\'"))
      (unless (map-elt orgx-bulk--plans (file-name-base file))
        (ignore-errors (orgx-bulk-load file)))))
  (let ((all (hash-table-values orgx-bulk--plans)))
    (if statuses
        (seq-filter (lambda (p) (memq (orgx-bulk-plan-status p) statuses)) all)
      all)))

(defun orgx-bulk-discard (plan)
  "Mark PLAN as `discarded', save it, and kill its plan buffer if open."
  (setf (orgx-bulk-plan-status plan) 'discarded)
  (orgx-bulk-save plan)
  (when (fboundp 'orgx-bulk-plan-buffer-name)
    (when-let* ((buf (get-buffer (orgx-bulk-plan-buffer-name plan))))
      (kill-buffer buf)))
  plan)

;;; Operation Registry and oab Bridge

(defvar orgx-bulk--op-registry (make-hash-table :test #'eq)
  "Hash table mapping operation symbol names to `orgx-bulk-op-spec' structs.")

(defvar orgx-bulk--oab-owned-keys (make-hash-table :test #'eql)
  "Hash table mapping custom action character keys to operation symbol names.")

(defvar orgx-bulk--active-plan nil
  "Dynamically bound `orgx-bulk-plan' during `orgx-bulk-execute'.")

(defvar orgx-bulk--active-op nil
  "Dynamically bound `orgx-bulk-op' during `orgx-bulk-execute'.")

(defvar orgx-bulk--active-step-map nil
  "Dynamically bound alist of (MARKER . STEP) during `orgx-bulk-execute'.")

(defvar orgx-bulk--plain-oab-args nil
  "Cached args for plain `org-agenda-bulk-action' runs outside a plan.")

(defvar orgx-bulk--plain-oab-last-markers nil
  "Tracks marker list in plain `org-agenda-bulk-action' runs.")

(defun orgx-bulk--make-oab-wrapper (name)
  "Return 0-arg callback for `org-agenda-bulk-custom-functions' for NAME."
  (lambda ()
    (if orgx-bulk--active-plan
        (orgx-bulk--oab-step-callback)
      (let* ((spec (orgx-bulk-get-op name))
             (marker (or (org-get-at-bol 'org-hd-marker)
                         (org-get-at-bol 'org-marker))))
        (when (and spec marker (marker-buffer marker))
          (unless (eq orgx-bulk--plain-oab-last-markers org-agenda-bulk-marked-entries)
            (setq orgx-bulk--plain-oab-last-markers org-agenda-bulk-marked-entries)
            (setq orgx-bulk--plain-oab-args
                  (when-let* ((reader (orgx-bulk-op-spec-read-args spec)))
                    (funcall reader))))
          (org-with-point-at marker
            (org-back-to-heading t)
            (apply (orgx-bulk-op-spec-apply spec) orgx-bulk--plain-oab-args)))))))

(cl-defun orgx-bulk-register-op
    (name &key key label read-args apply describe from-json schema validate
          queueable order-last builtin-oab)
  "Register bulk operation NAME and bridge KEY into custom functions.
LABEL, READ-ARGS, APPLY, DESCRIBE, FROM-JSON, SCHEMA, VALIDATE,
QUEUEABLE, ORDER-LAST, and BUILTIN-OAB configure the `orgx-bulk-op-spec'."
  (declare (indent 1))
  (unless (symbolp name)
    (user-error "Operation name must be a symbol: %S" name))
  (when (and key (not builtin-oab))
    (when (memq key orgx-bulk-oab-builtin-keys)
      (error "Operation %s key `%c' collides with org-agenda-bulk-action built-in key"
             name key))
    (when-let* ((existing-owner (gethash key orgx-bulk--oab-owned-keys)))
      (unless (eq existing-owner name)
        (error "Operation %s key `%c' collides with existing operation %s"
               name key existing-owner)))
    (when-let* ((custom-entry (assoc key (bound-and-true-p org-agenda-bulk-custom-functions))))
      (unless (eq (gethash key orgx-bulk--oab-owned-keys) name)
        (error "Operation %s key `%c' collides with existing org-agenda-bulk-custom-functions entry"
               name key))))
  ;; Remove previous key binding for NAME if it changed.
  (when-let* ((old (gethash name orgx-bulk--op-registry))
              (old-key (orgx-bulk-op-spec-key old)))
    (when (and (not (eql old-key key))
               (eq (gethash old-key orgx-bulk--oab-owned-keys) name))
      (remhash old-key orgx-bulk--oab-owned-keys)
      (setq org-agenda-bulk-custom-functions
            (assq-delete-all old-key org-agenda-bulk-custom-functions))))
  (let ((spec (orgx-bulk-op-spec--create
               :name name
               :key key
               :label (or label (symbol-name name))
               :read-args read-args
               :apply apply
               :describe describe
               :from-json from-json
               :schema schema
               :validate validate
               :queueable queueable
               :order-last order-last
               :builtin-oab builtin-oab)))
    (puthash name spec orgx-bulk--op-registry)
    (when (and key (not builtin-oab) (not (memq key orgx-bulk-oab-builtin-keys)))
      (puthash key name orgx-bulk--oab-owned-keys)
      (let ((wrapper (orgx-bulk--make-oab-wrapper name)))
        (setq org-agenda-bulk-custom-functions
              (cons (list key wrapper)
                    (assq-delete-all key org-agenda-bulk-custom-functions)))))
    spec))

(defun orgx-bulk-get-op (name)
  "Return the `orgx-bulk-op-spec' registered under NAME, or nil."
  (gethash (if (stringp name) (intern name) name) orgx-bulk--op-registry))

(defun orgx-bulk-registered-ops ()
  "Return all registered `orgx-bulk-op-spec' definitions."
  (hash-table-values orgx-bulk--op-registry))

(defun orgx-bulk-op-for-key (key)
  "Return the `orgx-bulk-op-spec' for character KEY.
Checks registered operations first, then bridges existing entries in
`org-agenda-bulk-custom-functions' as `function' operations."
  (or (seq-find (lambda (spec) (eql (orgx-bulk-op-spec-key spec) key))
                (hash-table-values orgx-bulk--op-registry))
      (when-let* ((entry (assoc key (bound-and-true-p org-agenda-bulk-custom-functions))))
        (let ((fn (nth 1 entry))
              (arg-fn (nth 2 entry)))
          (orgx-bulk-op-spec--create
           :name 'function
           :key key
           :label (format "Custom (%c)" key)
           :read-args (lambda ()
                        (if arg-fn
                            (list fn (funcall arg-fn))
                          (list fn)))
           :apply (or (and (orgx-bulk-get-op 'function)
                           (orgx-bulk-op-spec-apply (orgx-bulk-get-op 'function)))
                      (lambda (f &optional f-args)
                        (if f-args (apply f f-args) (funcall f))))
           :describe (lambda (f &rest _) (format "fn %s" f)))))))

(defun orgx-bulk-describe-op (op)
  "Return a human-readable description string for `orgx-bulk-op' OP."
  (let* ((name (if (orgx-bulk-op-p op) (orgx-bulk-op-name op) op))
         (args (when (orgx-bulk-op-p op) (orgx-bulk-op-args op)))
         (spec (orgx-bulk-get-op name)))
    (if (and spec (orgx-bulk-op-spec-describe spec))
        (condition-case nil
            (apply (orgx-bulk-op-spec-describe spec) args)
          (error (format "%s %S" name args)))
      (if args
          (format "%s %s" name (mapconcat (lambda (a) (format "%s" a)) args " "))
        (format "%s" name)))))

(defun orgx-bulk-order-ops (ops)
  "Order OPS so operations with `:order-last' (e.g. refile, archive) come last."
  (let (normal last)
    (dolist (op ops)
      (let* ((name (if (orgx-bulk-op-p op) (orgx-bulk-op-name op) op))
             (spec (orgx-bulk-get-op name)))
        (if (and spec (orgx-bulk-op-spec-order-last spec))
            (push op last)
          (push op normal))))
    (append (nreverse normal) (nreverse last))))

;;; Notes Policy Helper

(defun orgx-bulk-inject-log-note (note-text &optional state from-state)
  "Record NOTE-TEXT for STATE and FROM-STATE in the current heading's logbook.
Uses `org-add-log-note', inserts NOTE-TEXT into `*Org Note*', and calls
`org-store-log-note'."
  (when (and note-text (not (string-empty-p note-text)))
    (let ((org-inhibit-logging nil)
          (org-log-setup nil))
      (save-window-excursion
        (org-add-log-setup 'state (or state (org-get-todo-state)) from-state 'note)
        (remove-hook 'post-command-hook #'org-add-log-note)
        (org-add-log-note)
        (when-let* ((note-buf (get-buffer "*Org Note*")))
          (with-current-buffer note-buf
            (goto-char (point-max))
            (insert note-text)
            (org-store-log-note)))))))

;;; Plan Construction

(defun orgx-bulk--make-step-for-target (id target op)
  "Build an `orgx-bulk-step' with ID for TARGET and OP, checking initial state."
  (let* ((op-name (orgx-bulk-op-name op))
         (op-args (orgx-bulk-op-args op))
         (already-same (and (eq op-name 'todo-state)
                            (plist-get target :todo-state)
                            (equal (plist-get target :todo-state) (car op-args)))))
    (orgx-bulk-step--create
     :id id
     :target target
     :op op
     :status (if already-same 'skipped 'pending)
     :message (when already-same "already in target state"))))

(cl-defun orgx-bulk-create-plan
    (&key scope targets ops description (log-notes orgx-bulk-log-notes) note (status 'draft) save)
  "Create an `orgx-bulk-plan' over SCOPE, TARGETS, and OPS.
DESCRIPTION, LOG-NOTES, NOTE, and STATUS populate the plan metadata.
When SAVE is non-nil (default t when `orgx-bulk-directory' is set), persists
the plan to disk."
  (let* ((id (orgx-bulk--next-id))
         (norm-ops (orgx-bulk-order-ops
                    (mapcar (lambda (o)
                              (cond
                               ((orgx-bulk-op-p o) o)
                               ((and (consp o) (plistp o) (plist-get o :name))
                                (orgx-bulk-op--create
                                 :name (plist-get o :name)
                                 :args (plist-get o :args)
                                 :status (or (plist-get o :status) 'pending)))
                               ((consp o)
                                (orgx-bulk-op--create :name (car o) :args (cdr o)))
                               ((symbolp o)
                                (orgx-bulk-op--create :name o))
                               (t (error "Invalid operation spec: %S" o))))
                            ops)))
         (step-id 0)
         (steps
          (seq-mapcat
           (lambda (op)
             (mapcar (lambda (target)
                       (setq step-id (1+ step-id))
                       (orgx-bulk--make-step-for-target step-id target op))
                     targets))
           norm-ops))
         (plan (orgx-bulk-plan--create
                :id id
                :description description
                :created (format-time-string "%Y-%m-%dT%H:%M:%S%z")
                :status status
                :scope scope
                :log-notes log-notes
                :note note
                :ops norm-ops
                :steps steps)))
    (setf (map-elt orgx-bulk--plans id) plan)
    (when (or save orgx-bulk-directory)
      (orgx-bulk-save plan))
    plan))

(defun orgx-bulk-plan-add-op (plan op &optional targets)
  "Add operation OP to PLAN for all unique targets in PLAN (or TARGETS).
Re-orders pending operations so archive/refile run last, saves PLAN, and
returns PLAN."
  (let* ((op-struct (if (orgx-bulk-op-p op)
                        op
                      (orgx-bulk-op--create :name (car op) :args (cdr op))))
         (existing-targets
          (or targets
              (seq-uniq (mapcar #'orgx-bulk-step-target (orgx-bulk-plan-steps plan))
                        #'equal)))
         (max-id (apply #'max 0 (mapcar #'orgx-bulk-step-id (orgx-bulk-plan-steps plan))))
         (new-steps
          (mapcar (lambda (target)
                    (setq max-id (1+ max-id))
                    (orgx-bulk--make-step-for-target max-id target op-struct))
                  existing-targets))
         (all-ops (orgx-bulk-order-ops (append (orgx-bulk-plan-ops plan) (list op-struct))))
         (all-steps (append (orgx-bulk-plan-steps plan) new-steps)))
    (setf (orgx-bulk-plan-ops plan) all-ops)
    ;; New pending steps need a runnable plan.
    (when (and (eq (orgx-bulk-plan-status plan) 'complete)
               (seq-some (lambda (s) (eq (orgx-bulk-step-status s) 'pending)) new-steps))
      (setf (orgx-bulk-plan-status plan) 'paused))
    ;; Keep steps ordered by their operation's position in `all-ops'.
    (setf (orgx-bulk-plan-steps plan)
          (seq-mapcat (lambda (o)
                        (seq-filter (lambda (s) (eq (orgx-bulk-step-op s) o))
                                    all-steps))
                      all-ops))
    (orgx-bulk-save plan)))

;;; Executor (`org-agenda-bulk-action' driver)

(defconst orgx-bulk--exec-dispatch-key ?\x1e
  "Internal custom-function key used by `orgx-bulk-execute' to drive oab.")

(defun orgx-bulk--oab-step-callback ()
  "Per-entry callback invoked by `org-agenda-bulk-action' at each agenda line."
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-get-at-bol 'org-marker)))
         (pair (or (assq marker orgx-bulk--active-step-map)
                   (seq-find (lambda (cell) (equal (car cell) marker))
                             orgx-bulk--active-step-map)))
         (step (cdr pair))
         (plan orgx-bulk--active-plan)
         (op orgx-bulk--active-op)
         (spec (and op (orgx-bulk-get-op (orgx-bulk-op-name op)))))
    (when (and step (eq (orgx-bulk-step-status step) 'pending))
      (if (not (and marker (marker-buffer marker) (buffer-live-p (marker-buffer marker))))
          (setf (orgx-bulk-step-status step) 'missing
                (orgx-bulk-step-message step) "Target buffer no longer live")
        (if (not spec)
            (setf (orgx-bulk-step-status step) 'failed
                  (orgx-bulk-step-message step)
                  (format "Unknown operation: %s" (and op (orgx-bulk-op-name op))))
          ;; Prompt for per-step note before entering condition-case so `C-g'
          ;; during `read-string' is caught by the outer quit handler.
          (when (and (eq (orgx-bulk-op-name op) 'todo-state)
                     (eq (orgx-bulk-plan-log-notes plan) 'each)
                     (null (orgx-bulk-step-note step)))
            (let ((heading (plist-get (orgx-bulk-step-target step) :heading)))
              (setf (orgx-bulk-step-note step)
                    (read-string (format "Note for %s: " (or heading "heading"))))))
          (condition-case err
              (org-with-point-at marker
                (save-restriction
                  (widen)
                  (org-back-to-heading t)
                  (when-let* ((validate (orgx-bulk-op-spec-validate spec))
                              (problem (apply validate (orgx-bulk-op-args op))))
                    (error "%s" problem))
                  (let ((heading (org-get-heading t t t t)))
                    (apply (orgx-bulk-op-spec-apply spec) (orgx-bulk-op-args op))
                    (setf (orgx-bulk-step-status step) 'done
                          (orgx-bulk-step-message step) nil)
                    ;; Refresh the target snapshot only while this heading is
                    ;; still here; refile and archive leave point on a neighbor.
                    (when (and (ignore-errors (org-back-to-heading t))
                               (equal (org-get-heading t t t t) heading))
                      (setf (orgx-bulk-step-target step)
                            (orgx-bulk--target-at-heading))))
                  (run-hook-with-args 'orgx-bulk-step-functions step plan)))
            (error
             (setf (orgx-bulk-step-status step) 'failed
                   (orgx-bulk-step-message step) (error-message-string err)))))))))

(defun orgx-bulk--run-after-apply-hooks (plan)
  "Run `orgx-bulk-after-apply-functions' and save touched files for PLAN."
  (let ((done-steps (seq-filter (lambda (s) (eq (orgx-bulk-step-status s) 'done))
                                (orgx-bulk-plan-steps plan))))
    (map-do
     (lambda (file steps)
       (when (and file (file-exists-p file))
         (with-current-buffer (find-file-noselect file)
           (run-hook-with-args 'orgx-bulk-after-apply-functions file steps plan)
           (when (and orgx-bulk-save-buffers (buffer-modified-p))
             (let ((inhibit-message t))
               (save-buffer))))))
     (seq-group-by (lambda (s) (plist-get (orgx-bulk-step-target s) :file))
                   done-steps))))

(defun orgx-bulk-execute (plan &optional only-op)
  "Execute pending steps of PLAN (or ONLY-OP) through `org-agenda-bulk-action'.
Skips `done' and `skipped' steps.  Catches `quit', marks the plan `paused',
and saves.  Saves PLAN after each operation completes and runs
`orgx-bulk-step-functions' and `orgx-bulk-after-apply-functions'."
  (require 'orgx-bulk-ops)
  (require 'orgx-bulk-agenda)
  (when (eq (orgx-bulk-plan-status plan) 'discarded)
    (user-error "Plan %s is discarded" (orgx-bulk-plan-id plan)))
  (when (eq (orgx-bulk-plan-status plan) 'complete)
    (user-error "Plan %s is already complete" (orgx-bulk-plan-id plan)))
  ;; When `log-notes' is `one' and no note is recorded yet, read it once if interactive.
  (when (and (eq (orgx-bulk-plan-log-notes plan) 'one)
             (null (orgx-bulk-plan-note plan))
             (seq-some (lambda (op) (eq (orgx-bulk-op-name op) 'todo-state))
                       (orgx-bulk-plan-ops plan))
             (not noninteractive))
    (setf (orgx-bulk-plan-note plan) (read-string "Log note for plan: ")))
  (setf (orgx-bulk-plan-status plan) 'running)
  (orgx-bulk-save plan)
  (let ((ops-to-run (if only-op
                        (list only-op)
                      (orgx-bulk-order-ops (orgx-bulk-plan-ops plan))))
        (quitted nil)
        (orgx-bulk--internal-marking t))
    (condition-case nil
        (dolist (op ops-to-run)
          (let* ((op-steps (seq-filter (lambda (s)
                                         (and (eq (orgx-bulk-step-op s) op)
                                              (eq (orgx-bulk-step-status s) 'pending)))
                                       (orgx-bulk-plan-steps plan)))
                 (spec (orgx-bulk-get-op (orgx-bulk-op-name op)))
                 (step-map nil)
                 (resolved-markers nil))
            (when op-steps
              (if (not spec)
                  (dolist (s op-steps)
                    (setf (orgx-bulk-step-status s) 'failed
                          (orgx-bulk-step-message s)
                          (format "Unknown operation: %s" (orgx-bulk-op-name op))))
                ;; Resolve each pending step's target before driving oab.
                (dolist (s op-steps)
                  (let ((m (orgx-bulk--resolve-target (orgx-bulk-step-target s))))
                    (if (not m)
                        (setf (orgx-bulk-step-status s) 'missing
                              (orgx-bulk-step-message s) "Target heading not found")
                      (push (cons m s) step-map)
                      (push m resolved-markers))))
                (setq step-map (nreverse step-map)
                      resolved-markers (nreverse resolved-markers))
                (when resolved-markers
                  (let* ((agenda-pair
                          (orgx-bulk-batch-ensure-agenda plan resolved-markers))
                         (agenda-buf (car agenda-pair))
                         (batch-markers (cdr agenda-pair))
                         ;; Remap step-map keys to the exact `eq' markers in agenda-buf.
                         (mapped-step-map
                          (cl-mapcar (lambda (bm cell) (cons bm (cdr cell)))
                                     batch-markers step-map))
                         (dispatch-key
                          (if (and (orgx-bulk-op-spec-key spec)
                                   (not (orgx-bulk-op-spec-builtin-oab spec))
                                   (not (memq (orgx-bulk-op-spec-key spec)
                                              orgx-bulk-oab-builtin-keys)))
                              (orgx-bulk-op-spec-key spec)
                            orgx-bulk--exec-dispatch-key))
                         (orgx-bulk--active-plan plan)
                         (orgx-bulk--active-op op)
                         (orgx-bulk--active-step-map mapped-step-map))
                    (with-current-buffer agenda-buf
                      (let ((org-agenda-bulk-marked-entries (copy-sequence batch-markers))
                            (org-agenda-persistent-marks nil)
                            (org-agenda-bulk-custom-functions
                             (cons (list dispatch-key (orgx-bulk--make-oab-wrapper
                                                       (orgx-bulk-op-name op)))
                                   (assq-delete-all dispatch-key org-agenda-bulk-custom-functions)))
                            (unread-command-events (list dispatch-key)))
                        ;; Suppress `org-agenda-redo' inside oab if the buffer has no
                        ;; `org-agenda-redo-command' or while more operations remain.
                        (cl-letf (((symbol-function 'org-agenda-redo)
                                   (if (and (bound-and-true-p org-agenda-redo-command)
                                            (eq op (car (last ops-to-run))))
                                       (symbol-function 'org-agenda-redo)
                                     #'ignore)))
                          (org-agenda-bulk-action))))
                    ;; Any step still `pending' after oab was skipped by oab (e.g.
                    ;; entry disappeared from the agenda buffer after a mid-run redo).
                    (dolist (cell mapped-step-map)
                      (let ((st (cdr cell)))
                        (when (eq (orgx-bulk-step-status st) 'pending)
                          (setf (orgx-bulk-step-status st) 'skipped
                                (orgx-bulk-step-message st)
                                "Skipped by org-agenda-bulk-action (entry disappeared)")))))))
              (setf (orgx-bulk-op-status op)
                    (if (seq-some (lambda (s)
                                    (and (eq (orgx-bulk-step-op s) op)
                                         (eq (orgx-bulk-step-status s) 'pending)))
                                  (orgx-bulk-plan-steps plan))
                        'paused
                      'done))
              (orgx-bulk-save plan))))
      (quit
       (setq quitted t)
       (setf (orgx-bulk-plan-status plan) 'paused)
       (orgx-bulk-save plan)))
    (orgx-bulk--run-after-apply-hooks plan)
    (unless quitted
      (setf (orgx-bulk-plan-status plan)
            (if (seq-some (lambda (s) (eq (orgx-bulk-step-status s) 'pending))
                          (orgx-bulk-plan-steps plan))
                'paused
              'complete))
      (orgx-bulk-save plan))
    plan))

;;; JSON Summary (for MCP)

(defun orgx-bulk-plan-summary (plan)
  "Return PLAN as a JSON-ready plist."
  (let ((steps (orgx-bulk-plan-steps plan))
        (ops (orgx-bulk-plan-ops plan)))
    (list :plan_id (orgx-bulk-plan-id plan)
          :description (or (orgx-bulk-plan-description plan) :null)
          :created (or (orgx-bulk-plan-created plan) :null)
          :status (symbol-name (orgx-bulk-plan-status plan))
          :log_notes (symbol-name (or (orgx-bulk-plan-log-notes plan) 'ignore))
          :note (or (orgx-bulk-plan-note plan) :null)
          :ops (vconcat
                (mapcar (lambda (op)
                          (list :name (symbol-name (orgx-bulk-op-name op))
                                :args (vconcat (orgx-bulk-op-args op))
                                :description (orgx-bulk-describe-op op)
                                :status (symbol-name (orgx-bulk-op-status op))))
                        ops))
          :steps (vconcat
                  (mapcar (lambda (step)
                            (let ((tgt (orgx-bulk-step-target step)))
                              (list :id (orgx-bulk-step-id step)
                                    :op (symbol-name (orgx-bulk-step-op-name step))
                                    :args (vconcat (orgx-bulk-step-op-args step))
                                    :file (or (plist-get tgt :file) :null)
                                    :heading (or (plist-get tgt :heading) :null)
                                    :path (vconcat (plist-get tgt :outline-path))
                                    :status (symbol-name (orgx-bulk-step-status step))
                                    :message (or (orgx-bulk-step-message step) :null)
                                    :note (or (orgx-bulk-step-note step) :null))))
                          steps)))))

;;; Denote Plan Integration

(defun orgx-bulk-denote-sync-execution-status (file _steps _plan)
  "Sync `#+execution_status:' of denote plan FILE with its open tasks.
Becomes `complete' when no open todo remains, else `in-progress'.  Only an
existing keyword line is rewritten; the current buffer must visit FILE."
  (when (and (fboundp 'orgx-denote-plan-file-p)
             (orgx-denote-plan-file-p file))
    (save-excursion
      (save-restriction
        (widen)
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

;;; Interactive Entry Point (`C-c o b')

;;;###autoload
(defun orgx-bulk-todo-state (state)
  "Create a bulk plan moving headings to STATE and open its plan buffer.
Scope is the agenda's marked (or all) entries when called from an agenda,
otherwise the current buffer, the agenda files, or the denote files."
  (interactive
   (list (completing-read "Move to state: "
                          (seq-uniq (append org-todo-keywords-1 org-done-keywords))
                          nil nil)))
  (require 'orgx-bulk-ops)
  (require 'orgx-bulk-agenda)
  (require 'orgx-bulk-buffer)
  (pcase-let ((`(,scope ,targets ,desc) (orgx-bulk-batch-read-scope)))
    (unless targets
      (user-error "No matching tasks"))
    (let* ((note (when (eq orgx-bulk-log-notes 'one)
                   (read-string (format "Log note for -> %s: " state))))
           (plan (orgx-bulk-create-plan
                  :scope scope
                  :targets targets
                  :ops (list (orgx-bulk-op--create :name 'todo-state :args (list state)))
                  :description desc
                  :log-notes orgx-bulk-log-notes
                  :note note)))
      (orgx-bulk-review plan t))))

(provide 'orgx-bulk)
;;; orgx-bulk.el ends here
