;;; orgx-bulk.el --- Resumable bulk operations driven by org-agenda-bulk-action -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, convenience
;; Package-Requires: ((emacs "29.1") (org "9.6") (hitl "0.1.0"))

;;; Commentary:
;; Resumable bulk operations on Org headings built on top of
;; `org-agenda-bulk-action'.  orgx supplies only the `org-agenda' domain
;; (and its operations) for `hitl-plan', while hitl and agent-shell-queue
;; provide plan storage, execution, review buffers, MCP tools and the queue
;; integration.  A small back-compat layer keeps the remaining `orgx-bulk-*'
;; names (see the comment block before `orgx-bulk-op--create'), and every
;; other caller uses `hitl-plan' directly.
;;
;; `orgx-bulk' provides:
;; - Plans persisted as `.eld' files in `orgx-bulk-directory'.  Orgx-only
;;   plan files without a `:domain' still load.
;; - Target capture and resolution by ID, by outline path and heading, or by
;;   position.
;; - An operation registry (`orgx-bulk-register-op'), which registers each
;;   operation with `hitl-plan-register-op' in the `org-agenda' domain and
;;   bridges it into `org-agenda-bulk-custom-functions'.
;; - A domain :driver that runs `org-agenda-bulk-action' and calls
;;   `hitl-plan-run-step' for each heading (`orgx-bulk-execute').

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-agenda)
(require 'org-id)
(require 'hitl-plan)
(require 'hitl-plan-ui)

(declare-function orgx-denote-plan-file-p "orgx-denote")
(declare-function orgx-denote-files "orgx-denote")
(declare-function orgx-bulk-batch-read-scope "orgx-bulk-agenda")
(declare-function orgx-bulk-review "orgx-bulk-buffer")

;; Domain functions implemented in `orgx-bulk-agenda'.
(autoload 'orgx-bulk-batch-for-scope "orgx-bulk-agenda")
(autoload 'orgx-bulk-batch-ensure-agenda "orgx-bulk-agenda")
(autoload 'orgx-bulk-batch-rebuild-batch "orgx-bulk-agenda")
(autoload 'orgx-bulk-batch-lock "orgx-bulk-agenda")
(autoload 'orgx-bulk-batch-unlock "orgx-bulk-agenda")

(defvar org-agenda-bulk-marked-entries)
(defvar org-agenda-bulk-custom-functions)
(defvar org-log-setup)
(defvar org-inhibit-logging)
(defvar org-inhibit-blocking)
(defvar orgx-bulk--internal-marking nil)
(defvar denote-directory)
(defvar enable-local-variables)
(defvar enable-dir-local-variables)

(defgroup orgx-bulk nil
  "Bulk task operations for orgx."
  :group 'orgx
  :prefix "orgx-bulk-")

(defcustom orgx-bulk-directory (locate-user-emacs-file "orgx-bulk")
  "Directory where saved `org-agenda' plan files (`.eld') are stored.
Plans carry this directory, so `hitl-plan' persists them here rather than
in `hitl-plan-directory'."
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

(defcustom orgx-bulk-step-functions nil
  "Abnormal hook run after each step succeeds.
Each function receives the `hitl-plan-step' and its `hitl-plan', with
point on the heading in the step's base Org buffer.  Runs from
`hitl-plan-step-functions' for plans in the `org-agenda' domain."
  :type 'hook
  :group 'orgx-bulk)

(defcustom orgx-bulk-after-apply-functions '(orgx-bulk-denote-sync-execution-status)
  "Abnormal hook run once per touched file after a plan executes.
Each function receives FILE, the list of done `hitl-plan-step's in it, and
the `hitl-plan'.  Runs with FILE's buffer current, before it is saved.
Runs from `hitl-plan-after-apply-functions' for `org-agenda' plans, so the
step list holds the steps completed by that execution."
  :type 'hook
  :group 'orgx-bulk)

(defcustom orgx-bulk-mcp-allowed-files nil
  "Extra files and directories plans made over MCP may touch.
Plans an agent creates over MCP (origin `mcp', see `hitl-plan-origin-mcp-p')
may only read or modify local Org files that are in `org-agenda-files',
under `denote-directory', or listed here (a directory allows every Org file
beneath it).  This governs scope files, target files, refile destinations
and archive destinations.  Plans you make yourself are not restricted."
  :type '(repeat file)
  :group 'orgx-bulk)

(defconst orgx-bulk-oab-builtin-keys
  '(?$ ?A ?t ?+ ?- ?s ?d ?r ?w ?S ?f ?p)
  "Action keys owned by `org-agenda-bulk-action'.")

;;; Back-compat Layer

;; The rest of the `orgx-bulk-*' surface that still exists is:
;; - the commands and options named in docs/bulk.org (`orgx-bulk-todo-state',
;;   `orgx-bulk-execute', `orgx-bulk-register-op', `orgx-bulk-directory',
;;   `orgx-bulk-log-notes', ...), which are orgx's own;
;; - thin conveniences that bind the `org-agenda' domain: `orgx-bulk-create-plan',
;;   `orgx-bulk-get-plan', `orgx-bulk-list-plans', `orgx-bulk-discard',
;;   `orgx-bulk-load' and `orgx-bulk-op--create'.
;; Plan, step and operation accessors are `hitl-plan''s own; there are no
;; `orgx-bulk-plan-*' or `orgx-bulk-step-*' aliases.

(cl-defun orgx-bulk-op--create (&key name args)
  "Return a `hitl-plan-op' for operation NAME with ARGS."
  (hitl-plan-op--make :name name :args args))

(defun orgx-bulk-op-spec-builtin-oab (spec)
  "Return non-nil if operation SPEC maps to a built-in oab key."
  (plist-get (hitl-plan-op-spec-extra spec) :builtin-oab))

;;; Allowed Files (untrusted, MCP-origin plans)

(defun orgx-bulk--local-name-p (file)
  "Return non-nil if FILE is a plain local file name string.
Purely syntactic, so it never contacts a remote host: remote and
TRAMP-like names, empty names and names with control characters fail."
  (and (stringp file)
       (not (string-empty-p file))
       (not (string-match-p "[[:cntrl:]]" file))
       (not (string-match-p "\\`/[^/]*:" file))
       (not (file-remote-p file))))

(defun orgx-bulk--denote-roots ()
  "Return the denote directories, if denote is configured."
  (let ((d (and (boundp 'denote-directory) denote-directory)))
    (cond ((stringp d) (list d))
          ((and (consp d) (cl-every #'stringp d)) d))))

(defun orgx-bulk--allowed-entries ()
  "Return the local file and directory names MCP plans may use."
  (append (ignore-errors (org-agenda-files))
          (orgx-bulk--denote-roots)
          (let ((a orgx-bulk-mcp-allowed-files))
            (if (listp a) a (list a)))))

(defun orgx-bulk-file-allowed-p (file)
  "Return non-nil if an MCP-origin plan may use FILE.
FILE must be a local, existing, regular Org file (`.org' or `.org_archive')
that is one of `org-agenda-files', is under `denote-directory', or is in
\(or under a directory in) `orgx-bulk-mcp-allowed-files'.  Symbolic links are
resolved first, so a link cannot smuggle in another file."
  (and (orgx-bulk--local-name-p file)
       (let ((truename (ignore-errors (file-truename (expand-file-name file)))))
         (and truename
              (file-regular-p truename)
              (string-match-p "\\.org\\(?:_archive\\)?\\'" truename)
              (cl-some
               (lambda (entry)
                 (and (orgx-bulk--local-name-p entry)
                      (let ((root (ignore-errors (file-truename (expand-file-name entry)))))
                        (and root
                             (or (equal root truename)
                                 (and (file-directory-p root)
                                      (file-in-directory-p truename root)))))))
               (orgx-bulk--allowed-entries))))))

(defun orgx-bulk--check-file (file)
  "Return FILE, expanded, if MCP plans may use it, else signal an error.
The error is the same whether FILE is disallowed, missing or not Org, so
callers learn nothing about the file system."
  (unless (orgx-bulk-file-allowed-p file)
    (error "File not allowed, not an Org file, or does not exist: %s"
           (if (stringp file) (truncate-string-to-width file 120 nil nil "...") file)))
  (expand-file-name file))

(defun orgx-bulk--file-usable-p (file)
  "Return non-nil if FILE may be opened now.
Under restriction (an MCP-origin plan) that is `orgx-bulk-file-allowed-p';
otherwise the file only has to exist."
  (if (hitl-plan-restricted-p)
      (orgx-bulk-file-allowed-p file)
    (and (stringp file) (file-exists-p file))))

(defmacro orgx-bulk--with-safe-visiting (&rest body)
  "Run BODY, ignoring file-local and directory-local variables when restricted.
File-local `eval' forms must never run on behalf of an MCP-origin plan."
  (declare (indent 0))
  `(let ((enable-local-variables (if (hitl-plan-restricted-p) nil enable-local-variables))
         (enable-dir-local-variables (if (hitl-plan-restricted-p) nil enable-dir-local-variables)))
     ,@body))

(defun orgx-bulk--visit (file)
  "Return the buffer visiting FILE, opened safely when restricted."
  (orgx-bulk--with-safe-visiting (find-file-noselect file)))

(defun orgx-bulk--sanitize-target (target)
  "Return a freshly captured target for caller-supplied org TARGET.
Used for targets an agent supplies over MCP.  TARGET must name an allowed
file and a heading by `:id' or `:heading'.  The heading is resolved and
captured again, so what the review shows and what execution acts on are the
real ones; the claimed `:id', `:heading', `:outline-path' and `:pos' must
agree with the capture.  Signal an error naming the target otherwise."
  (let* ((hitl-plan-restricted t)
         (file (and (consp target) (plist-get target :file)))
         (id (plist-get target :id))
         (heading (plist-get target :heading))
         (path (plist-get target :outline-path))
         (pos (plist-get target :pos))
         (what (format "%s%s" (if (stringp file) (file-name-nondirectory file) "?")
                       (cond ((stringp heading) (format ": %s" (truncate-string-to-width heading 60 nil nil "...")))
                             ((stringp id) (format " (id %s)" (truncate-string-to-width id 60 nil nil "...")))
                             (t "")))))
    (orgx-bulk--check-file file)
    (unless (and (or (null id) (stringp id)) (or (null heading) (stringp heading))
                 (or (null pos) (integerp pos))
                 (or (null path) (and (proper-list-p path) (cl-every #'stringp path))))
      (error "Invalid target (%s)" what))
    (unless (or id heading)
      (error "Target (%s) needs an :id or a :heading" what))
    (let ((marker (orgx-bulk--resolve-target
                   (list :file file :id id :outline-path path :heading heading :pos pos))))
      (unless marker
        (error "Target heading not found (%s)" what))
      (unwind-protect
          (let ((fresh (with-current-buffer (marker-buffer marker)
                         (save-excursion
                           (goto-char marker)
                           (let ((orgx-bulk-assign-ids nil))
                             (orgx-bulk--target-at-heading))))))
            (dolist (check `((:id ,id) (:heading ,heading) (:outline-path ,path) (:pos ,pos)))
              (when (and (cadr check)
                         (not (equal (cadr check) (plist-get fresh (car check)))))
                (error "Target (%s) disagrees with the heading it resolves to: %s"
                       what (substring (symbol-name (car check)) 1))))
            fresh)
        (set-marker marker nil)))))

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
    (when (and file (orgx-bulk--file-usable-p file))
      (with-current-buffer (orgx-bulk--visit file)
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

(defun orgx-bulk--normalize-legacy (data)
  "Return plist DATA, from an orgx-only `.eld' file, in `hitl-plan' shape.
Adds the missing `:domain' and gives steps saved with an inline `:op'
an `:op-index'."
  (let* ((data (copy-sequence data))
         (ops (copy-sequence (plist-get data :ops)))
         (steps
          (mapcar
           (lambda (st)
             (if (integerp (plist-get st :op-index))
                 st
               (let* ((raw (plist-get st :op))
                      (name (cond ((and (consp raw) (plistp raw)) (plist-get raw :name))
                                  ((and raw (symbolp raw)) raw)))
                      (args (and (consp raw) (plistp raw) (plist-get raw :args)))
                      (idx (and name
                                (seq-position
                                 ops nil
                                 (lambda (o _)
                                   (and (eq (plist-get o :name) name)
                                        (or (not (consp raw))
                                            (equal (plist-get o :args) args))))))))
                 (when (and name (null idx))
                   (setq ops (append ops (list (list :name name :args args)))
                         idx (1- (length ops))))
                 (if idx
                     (plist-put (copy-sequence st) :op-index idx)
                   st))))
           (plist-get data :steps))))
    (setq data (plist-put data :ops ops))
    (setq data (plist-put data :steps steps))
    (unless (plist-get data :domain)
      (setq data (plist-put data :domain 'org-agenda)))
    data))

(defun orgx-bulk-load (id-or-file)
  "Load and return the plan from ID-OR-FILE, accepting legacy orgx files.
ID-OR-FILE is a plan ID (looked up in `orgx-bulk-directory') unless it is
an absolute path or ends in `.eld', in which case it names a file; a
relative path without `.eld' is always an ID, never a file.  Unlike
`hitl-plan-load' this signals an `error' naming the file when it is
missing or corrupt.  Legacy files lacking `:domain' are migrated by the
`org-agenda' domain's :migrate-plist."
  (let ((file (if (and (stringp id-or-file)
                       (or (file-name-absolute-p id-or-file)
                           (string-suffix-p ".eld" id-or-file)))
                  (expand-file-name id-or-file)
                (hitl-plan-file id-or-file orgx-bulk-directory))))
    (unless (file-exists-p file)
      (error "Corrupt or missing plan file %s: file does not exist" file))
    (or (hitl-plan-load file nil 'org-agenda)
        (error "Corrupt plan file %s" file))))

(defun orgx-bulk-get-plan (id)
  "Return the `org-agenda' plan registered or saved under ID, or nil."
  (when id
    (let ((plan (hitl-plan-get id nil 'org-agenda)))
      (and plan (hitl-plan-in-domain-p plan 'org-agenda) plan))))

(defun orgx-bulk-list-plans (&optional statuses)
  "Return all saved and in-memory `org-agenda' plans, filtered by STATUSES."
  (hitl-plan-list statuses nil 'org-agenda))

(defun orgx-bulk-discard (plan)
  "Mark PLAN as `discarded', save it, and kill its review buffer if open."
  (hitl-plan-discard plan)
  (when-let* ((buf (get-buffer (hitl-plan-ui-buffer-name plan))))
    (kill-buffer buf))
  plan)

;;; Operation Registry and oab Bridge

(defvar orgx-bulk--active-plan nil
  "Dynamically bound plan while the `org-agenda' driver runs an operation.")

(defvar orgx-bulk--active-op nil
  "Dynamically bound `orgx-bulk-op' while the `org-agenda' driver runs.")

(defvar orgx-bulk--active-step-map nil
  "Dynamically bound alist of (MARKER . STEP) while the driver runs.")

(defvar orgx-bulk--only-op nil
  "Operation passed as ONLY-OP to `orgx-bulk-execute', if any.")

(defvar orgx-bulk--plain-oab-args nil
  "Cached args for plain `org-agenda-bulk-action' runs outside a plan.")

(defvar orgx-bulk--plain-oab-last-markers nil
  "Tracks marker list in plain `org-agenda-bulk-action' runs.")

(defun orgx-bulk--make-oab-wrapper (name)
  "Return 0-arg callback for `org-agenda-bulk-custom-functions' for NAME."
  (lambda ()
    (if orgx-bulk--active-plan
        (orgx-bulk--oab-step-callback)
      (let* ((spec (hitl-plan-get-op 'org-agenda name))
             (marker (or (org-get-at-bol 'org-hd-marker)
                         (org-get-at-bol 'org-marker))))
        (when (and spec marker (marker-buffer marker))
          (unless (eq orgx-bulk--plain-oab-last-markers org-agenda-bulk-marked-entries)
            (setq orgx-bulk--plain-oab-last-markers org-agenda-bulk-marked-entries)
            (setq orgx-bulk--plain-oab-args
                  (when-let* ((reader (hitl-plan-op-spec-read-args spec)))
                    (funcall reader))))
          (org-with-point-at marker
            (org-back-to-heading t)
            (apply (hitl-plan-op-spec-apply spec) orgx-bulk--plain-oab-args)))))))

(cl-defun orgx-bulk-register-op
    (name &key key label read-args apply describe from-json schema validate
          queueable order-last builtin-oab)
  "Register bulk operation NAME and bridge KEY into custom functions.
The operation is registered with `hitl-plan-register-op' in the `org-agenda'
domain.  LABEL, READ-ARGS, APPLY, DESCRIBE, FROM-JSON, SCHEMA, VALIDATE,
QUEUEABLE, and ORDER-LAST configure the `hitl-plan-op-spec'; BUILTIN-OAB
marks KEY as owned by `org-agenda-bulk-action' (stored in the spec's
:extra)."
  (declare (indent 1))
  (unless (symbolp name)
    (user-error "Operation name must be a symbol: %S" name))
  (when (and key (not builtin-oab))
    (when (memq key orgx-bulk-oab-builtin-keys)
      (error "Operation %s key `%c' collides with org-agenda-bulk-action built-in key"
             name key))
    ;; An existing entry is ours only when a registered operation owns the key.
    (let ((owner (hitl-plan-op-for-key 'org-agenda key)))
      (when (and owner (not (eq (hitl-plan-op-spec-name owner) name)))
        (error "Operation %s key `%c' collides with existing operation %s"
               name key (hitl-plan-op-spec-name owner)))
      (when (and (not owner)
                 (assoc key (bound-and-true-p org-agenda-bulk-custom-functions)))
        (error "Operation %s key `%c' collides with existing org-agenda-bulk-custom-functions entry"
               name key))))
  ;; Remove the previous custom key binding for NAME if its key changed.
  (when-let* ((old (hitl-plan-get-op 'org-agenda name))
              ((eq (hitl-plan-op-spec-domain old) 'org-agenda))
              (old-key (hitl-plan-op-spec-key old)))
    (unless (or (eql old-key key) (orgx-bulk-op-spec-builtin-oab old))
      (setq org-agenda-bulk-custom-functions
            (assq-delete-all old-key org-agenda-bulk-custom-functions))))
  (let ((spec (hitl-plan-register-op 'org-agenda name
                :key key :label label :read-args read-args :apply apply
                :describe describe :from-json from-json :schema schema
                :validate validate :queueable queueable
                :order-last order-last
                :extra (and builtin-oab '(:builtin-oab t)))))
    (when (and key (not builtin-oab) (not (memq key orgx-bulk-oab-builtin-keys)))
      (setq org-agenda-bulk-custom-functions
            (cons (list key (orgx-bulk--make-oab-wrapper name))
                  (assq-delete-all key org-agenda-bulk-custom-functions))))
    spec))

(defun orgx-bulk-op-for-key (key)
  "Return the `hitl-plan-op-spec' for character KEY.
Checks registered operations first, then bridges existing entries in
`org-agenda-bulk-custom-functions' as `function' operations."
  (or (hitl-plan-op-for-key 'org-agenda key)
      (when-let* ((entry (assoc key (bound-and-true-p org-agenda-bulk-custom-functions))))
        (let ((fn (nth 1 entry))
              (arg-fn (nth 2 entry)))
          (hitl-plan-op-spec--make
           :domain 'org-agenda :name 'function
           :key key
           :label (format "Custom (%c)" key)
           :read-args (lambda ()
                        (if arg-fn
                            (list fn (funcall arg-fn))
                          (list fn)))
           :apply (or (and (hitl-plan-get-op 'org-agenda 'function)
                           (hitl-plan-op-spec-apply (hitl-plan-get-op 'org-agenda 'function)))
                      (lambda (f &optional f-args)
                        (if f-args (apply f f-args) (funcall f))))
           :describe (lambda (f &rest _) (format "fn %s" f)))))))

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

(defun orgx-bulk--already-in-target-state-p (step)
  "Return non-nil if STEP is a `todo-state' op its target already satisfies."
  (let ((op (hitl-plan-step-op step))
        (target (hitl-plan-step-target step)))
    (and (eq (hitl-plan-op-name op) 'todo-state)
         (plist-get target :todo-state)
         (equal (plist-get target :todo-state) (car (hitl-plan-op-args op))))))

(defun orgx-bulk--skip-satisfied-steps (steps)
  "Mark pending STEPS that already satisfy their operation as skipped."
  (dolist (step steps)
    (when (and (eq (hitl-plan-step-status step) 'pending)
               (orgx-bulk--already-in-target-state-p step))
      (setf (hitl-plan-step-status step) 'skipped
            (hitl-plan-step-message step) "already in target state"))))

(cl-defun orgx-bulk-create-plan
    (&key scope targets ops description (log-notes orgx-bulk-log-notes) note (status 'draft) save)
  "Create an `org-agenda' plan over SCOPE, TARGETS, and OPS.
DESCRIPTION, LOG-NOTES, NOTE, and STATUS populate the plan metadata.
Targets that already satisfy a `todo-state' operation are marked skipped.
The plan is saved in `orgx-bulk-directory' (the domain's :directory) when
SAVE is non-nil or `orgx-bulk-directory' is non-nil, which it is by
default; otherwise it is only created in memory."
  (hitl-plan-create
   :domain 'org-agenda :scope scope
   :targets targets :ops ops :description description
   :log-notes log-notes :note note :status status
   :save (and (or save orgx-bulk-directory) t)))

;;; Hook Bridges

(defun orgx-bulk--step-hook (step plan)
  "Refresh STEP's target snapshot and run `orgx-bulk-step-functions'.
A member of `hitl-plan-step-functions'; acts only on `org-agenda' PLANs,
with point on the heading in its base buffer."
  (when (hitl-plan-in-domain-p plan 'org-agenda)
    ;; Refresh the target snapshot only while this heading is still here;
    ;; refile and archive leave point on a neighbor.
    (let ((heading (plist-get (hitl-plan-step-target step) :heading)))
      (when (and (ignore-errors (org-back-to-heading t))
                 (or (null heading)
                     (equal (org-get-heading t t t t) heading)))
        (setf (hitl-plan-step-target step) (orgx-bulk--target-at-heading))))
    (run-hook-with-args 'orgx-bulk-step-functions step plan)))

(defun orgx-bulk--after-apply-hook (file steps plan)
  "Run `orgx-bulk-after-apply-functions' for FILE, STEPS and `org-agenda' PLAN."
  (when (hitl-plan-in-domain-p plan 'org-agenda)
    (run-hook-with-args 'orgx-bulk-after-apply-functions file steps plan)))

;; Run before any user function so snapshots are fresh.
(add-hook 'hitl-plan-step-functions #'orgx-bulk--step-hook -90)
(add-hook 'hitl-plan-after-apply-functions #'orgx-bulk--after-apply-hook)

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
         (op orgx-bulk--active-op))
    ;; After C-g (`quit-flag' set by an interrupted step) leave the rest pending.
    (when (and step (not quit-flag) (eq (hitl-plan-step-status step) 'pending))
      ;; Prompt for per-step note before running the step so `C-g' during
      ;; `read-string' is caught by the driver's quit handler.
      (when (and (eq (hitl-plan-op-name op) 'todo-state)
                 (eq (hitl-plan-log-notes plan) 'each)
                 (null (hitl-plan-step-note step)))
        (let ((heading (plist-get (hitl-plan-step-target step) :heading)))
          (setf (hitl-plan-step-note step)
                (read-string (format "Note for %s: " (or heading "heading"))))))
      ;; With a dead marker `hitl-plan-run-step' re-resolves the target, and
      ;; marks it per the domain's :unresolved-status if that fails too.
      (hitl-plan-run-step plan step op
                          (and marker (marker-buffer marker)
                               (buffer-live-p (marker-buffer marker))
                               marker)))))

(defun orgx-bulk--oab-batch-driver (plan op _pending-steps resolved)
  "`org-agenda' domain :driver: run OP's RESOLVED steps of PLAN through oab.
RESOLVED is an alist of (STEP . MARKER).  The markers are the exact (`eq')
`org-hd-marker' objects on lines of the batch agenda buffer
`hitl-plan-batch-buffer' (created here from the markers if the domain gave
none).  oab, bound buffer-locally in that buffer, calls back into
`hitl-plan-run-step' for each line."
  (let* ((spec (hitl-plan-get-op 'org-agenda (hitl-plan-op-name op)))
         (agenda-buf hitl-plan-batch-buffer)
         (step-map (mapcar (lambda (cell) (cons (cdr cell) (car cell))) resolved)))
    (unless agenda-buf
      (pcase-let ((`(,buf . ,batch-markers)
                   (orgx-bulk-batch-ensure-agenda plan (mapcar #'cdr resolved))))
        (setq agenda-buf buf
              ;; Remap to the exact `eq' markers in the agenda buffer.
              step-map (cl-mapcar (lambda (bm cell) (cons bm (car cell)))
                                  batch-markers resolved))))
    (let* ((batch-markers (mapcar #'car step-map))
           (ordered (hitl-plan-order-ops 'org-agenda (hitl-plan-ops plan)))
           (last-op-p (or orgx-bulk--only-op (eq op (car (last ordered)))))
           (dispatch-key
            (if (and spec
                     (hitl-plan-op-spec-key spec)
                     (not (orgx-bulk-op-spec-builtin-oab spec))
                     (not (memq (hitl-plan-op-spec-key spec)
                                orgx-bulk-oab-builtin-keys)))
                (hitl-plan-op-spec-key spec)
              orgx-bulk--exec-dispatch-key))
           (orgx-bulk--active-plan plan)
           (orgx-bulk--active-op op)
           (orgx-bulk--active-step-map step-map)
           (orgx-bulk--internal-marking t)
           (quitted nil))
      (condition-case nil
          (with-current-buffer agenda-buf
            (setq-local orgx-bulk-batch-markers (copy-sequence batch-markers))
            (setq-local orgx-bulk-batch-plan-id (hitl-plan-id plan))
            ;; Bind inside the agenda buffer: oab reads these buffer-locally.
            (let ((org-agenda-bulk-marked-entries (copy-sequence batch-markers))
                  (org-agenda-persistent-marks nil)
                  (org-agenda-bulk-custom-functions
                   (cons (list dispatch-key (orgx-bulk--make-oab-wrapper
                                             (hitl-plan-op-name op)))
                         (assq-delete-all dispatch-key org-agenda-bulk-custom-functions)))
                  (unread-command-events (list dispatch-key)))
              ;; Suppress `org-agenda-redo' inside oab if the buffer has no
              ;; `org-agenda-redo-command' or while more operations remain.
              (cl-letf (((symbol-function 'org-agenda-redo)
                         (if (and (bound-and-true-p org-agenda-redo-command)
                                  last-op-p)
                             (symbol-function 'org-agenda-redo)
                           #'ignore)))
                (org-agenda-bulk-action))))
        (quit (setq quitted t)))
      (if (or quitted quit-flag)
          (progn (setq quit-flag nil)
                 (signal 'quit nil))
        ;; Any step still `pending' after oab was skipped by oab (e.g. the
        ;; entry disappeared from the agenda buffer after a mid-run redo).
        (dolist (cell step-map)
          (let ((st (cdr cell)))
            (when (eq (hitl-plan-step-status st) 'pending)
              (setf (hitl-plan-step-status st) 'skipped
                    (hitl-plan-step-message st)
                    "Skipped by org-agenda-bulk-action (entry disappeared)"))))
        (hitl-plan-save plan)))))

(defun orgx-bulk--domain-format-target (target)
  "Return (FILE-COLUMN . LABEL-COLUMN) strings describing org TARGET."
  (cons (if-let* ((file (plist-get target :file)))
            (file-name-nondirectory file)
          "")
        (concat (or (plist-get target :heading) "<unnamed>")
                (if-let* ((path (plist-get target :outline-path)))
                    (concat "  " (string-join path "/"))
                  ""))))

(defun orgx-bulk--domain-visit-target (target &optional other-window)
  "Visit the heading of org TARGET, in another window if OTHER-WINDOW.
Return non-nil on success, as `hitl-plan-ui-visit-target' expects."
  (let ((marker (orgx-bulk--resolve-target target)))
    (unless (and marker (marker-buffer marker))
      (user-error "Target heading no longer resolves"))
    (if other-window
        (pop-to-buffer (marker-buffer marker))
      (switch-to-buffer (marker-buffer marker)))
    (goto-char marker)
    (org-fold-show-context 'agenda)
    t))

(defun orgx-bulk--agenda-buffer-p (buffer)
  "Return non-nil if BUFFER is an `org-agenda-mode' buffer.
The domain's :accept-buffer, so agent-shell-queue-plan accepts agendas."
  (and (buffer-live-p buffer)
       (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                'org-agenda-mode)))

(hitl-plan-register-domain 'org-agenda
  :label "Org Agenda Batch"
  :resolve-target #'orgx-bulk--resolve-target
  :capture-target #'orgx-bulk--target-at-heading
  :format-target #'orgx-bulk--domain-format-target
  :visit-target #'orgx-bulk--domain-visit-target
  :build-batch #'orgx-bulk-batch-for-scope
  :rebuild-batch #'orgx-bulk-batch-rebuild-batch
  :lock-batch #'orgx-bulk-batch-lock
  :unlock-batch #'orgx-bulk-batch-unlock
  :driver #'orgx-bulk--oab-batch-driver
  :unresolved-status 'missing
  :unresolved-message "Target heading not found"
  :accept-buffer #'orgx-bulk--agenda-buffer-p
  :sanitize-target #'orgx-bulk--sanitize-target
  :directory (lambda () orgx-bulk-directory)
  :migrate-plist #'orgx-bulk--normalize-legacy
  :after-steps-added (lambda (_plan new-steps)
                       (orgx-bulk--skip-satisfied-steps new-steps)))

(defun orgx-bulk-execute (plan &optional only-op)
  "Execute pending steps of PLAN (or ONLY-OP) through `org-agenda-bulk-action'.
Runs `hitl-plan-execute' with the `org-agenda' domain driver, which saves
PLAN after each step and runs `orgx-bulk-step-functions' and
`orgx-bulk-after-apply-functions'.  Interactively (not in batch) a
`todo-state' plan in `one' note mode without a note asks for it first."
  (require 'orgx-bulk-ops)
  (require 'orgx-bulk-agenda)
  (when (and (eq (hitl-plan-log-notes plan) 'one)
             (null (hitl-plan-note plan))
             (seq-some (lambda (op) (eq (hitl-plan-op-name op) 'todo-state))
                       (hitl-plan-ops plan))
             (not noninteractive))
    (setf (hitl-plan-note plan) (read-string "Log note for plan: ")))
  (let ((orgx-bulk--only-op only-op))
    (hitl-plan-execute plan only-op)))

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
