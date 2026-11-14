;;; hitl-plan.el --- Resumable, reviewable bulk action plans -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, agent, ai
;; URL: https://github.com/tychoish/hitl

;; This file is not part of GNU Emacs.

;;; Commentary:

;; hitl-plan is a domain-agnostic engine for resumable bulk action plans.
;; A plan is a set of targets crossed with an ordered list of operations,
;; and each (target, operation) pair is a step with its own status: that
;; per-step status is what lets a plan be reviewed, partially executed,
;; interrupted with C-g, persisted to disk, and resumed later.
;;
;; The engine has four building blocks:
;; - Domains (`hitl-plan-register-domain') describe how to capture, resolve,
;;   format, and visit targets, and may supply a batch :driver.
;; - Operations (`hitl-plan-register-op') are registered per domain, and the
;;   domain `t' acts as a fallback for domain-neutral operations.
;; - Each plan is saved atomically as one `.eld' file in
;;   `hitl-plan-directory'.
;; - `hitl-plan-execute' runs a plan through the built-in quit-safe per-step
;;   iterator, or through the domain's :driver, which iterates natively and
;;   calls `hitl-plan-run-step' for each target.
;;
;; A step interrupted mid-apply stays pending and is applied again on
;; resume, so operations should be idempotent.  A plan whose steps all
;; failed still ends `complete', so inspect the step statuses for the
;; outcome.
;;
;; The built-in `file-target' domain handles (:file :line :pos :snippet)
;; targets, so the engine works without any other package.
;;
;; Operation :apply functions receive the operation's args.  When the
;; resolved target is a marker, the apply function runs with that marker's
;; buffer current and point at the marker.  The engine dynamically binds
;; `hitl-plan-current-plan', `hitl-plan-current-step', `hitl-plan-current-op'
;; and `hitl-plan-current-target' for any resolved value.
;;
;; Optional domain slots (keywords of `hitl-plan-register-domain'):
;; - :directory, a string or a function of no arguments returning one, is the
;;   domain's plan store.  `hitl-plan-create' uses it when no :directory is
;;   given, and `hitl-plan-list' and `hitl-plan-get' use it when a domain is
;;   given without a directory.  A plan's own `directory' slot is
;;   authoritative for saving, reloading and refreshing.
;; - :migrate-plist, (lambda (PLIST)) returning PLIST, upgrades legacy saved
;;   data.  `hitl-plan-from-plist' applies it before building the struct when
;;   the data's :domain names the domain, or when the data has no :domain and
;;   a legacy domain is supplied: via the LEGACY-DOMAIN argument of
;;   `hitl-plan-from-plist' and `hitl-plan-load', or else, in
;;   `hitl-plan-load', the registered domain whose store (:directory) is the
;;   file's directory, falling back to `file-target'.
;; - :after-steps-added, (lambda (PLAN NEW-STEPS)), runs in `hitl-plan-create'
;;   (with all created steps) once the steps are built and before the first
;;   save, and in `hitl-plan-add-op' (with only the newly added steps) after
;;   they are appended and before the save, so it may adjust steps.  It is
;;   not called when there are no new steps.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(defgroup hitl-plan nil
  "Resumable bulk action plans."
  :group 'tools
  :prefix "hitl-plan-")

(defcustom hitl-plan-directory (locate-user-emacs-file "hitl-plans/")
  "Directory where saved `hitl-plan' files (`.eld') are stored."
  :type 'directory
  :group 'hitl-plan)

(defcustom hitl-plan-log-notes 'ignore
  "Default note-logging mode for new plans.
`ignore' records no notes, `one' uses a single plan note for every step,
and `each' uses a note stored on each step."
  :type '(choice (const :tag "Ignore notes" ignore)
                 (const :tag "One note for all steps" one)
                 (const :tag "Note per step" each))
  :group 'hitl-plan)

(defcustom hitl-plan-save-buffers t
  "When non-nil, save each file buffer touched by an executed plan."
  :type 'boolean
  :group 'hitl-plan)

(defcustom hitl-plan-step-functions nil
  "Abnormal hook run after each step succeeds.
Each function receives the `hitl-plan-step' and its `hitl-plan'.  When the
target resolved to a marker, point is at that marker in its buffer."
  :type 'hook
  :group 'hitl-plan)

(defcustom hitl-plan-after-apply-functions nil
  "Abnormal hook run once per touched file after a plan executes.
Each function receives FILE, the list of `hitl-plan-step's newly done in it
during this execution, and the `hitl-plan'.  FILE's buffer is current
when FILE exists, and is saved afterwards if `hitl-plan-save-buffers'.
Steps are grouped by the `:file' of their target, so this assumes plist
targets; steps whose target has no `:file' are passed together with FILE
nil and no buffer is made current or saved.  A domain whose targets are
not file plists can leave `hitl-plan-save-buffers' nil and ignore FILE."
  :type 'hook
  :group 'hitl-plan)

(defcustom hitl-plan-agent-may-apply nil
  "When non-nil, unattended callers may execute plans without user approval.
This covers the MCP `plan_execute' tool and queued `hitl-plan' items for
plans created over MCP (see `hitl-plan-origin-mcp-p').  Otherwise such plans
are only reviewed by the user, who executes them in the review buffer
\(after an explicit confirmation) or by queueing them interactively."
  :type 'boolean
  :group 'hitl-plan)

(defvar hitl-plan-restricted nil
  "Non-nil while code runs on behalf of an MCP-origin (untrusted) plan.
Bound by `hitl-plan-execute' and by the MCP plan creation functions.  Domains
and operations consult `hitl-plan-restricted-p' to refuse risky actions, such
as touching files outside an allow-list.")

(defconst hitl-plan-format-version 1
  "Format version for saved `.eld' plan files.")

;;; Data Model

(cl-defstruct (hitl-plan (:constructor hitl-plan--make)
                         (:copier nil))
  "A saved, resumable bulk action plan.
STATUS is one of `draft', `running', `paused', `complete', or `discarded'.
LOG-NOTES is one of `ignore', `one', or `each'."
  id domain created updated description scope ops
  (log-notes hitl-plan-log-notes) note steps (status 'draft)
  directory metadata)

(cl-defstruct (hitl-plan-op (:constructor hitl-plan-op--make)
                            (:copier nil))
  "One operation in a `hitl-plan'."
  name args)

(cl-defstruct (hitl-plan-step (:constructor hitl-plan-step--make)
                              (:copier nil))
  "One target and operation pairing in a `hitl-plan'.
OP is the `hitl-plan-op' (the same object held in the plan's ops).
STATUS is one of `pending', `done', `failed', `skipped', or a domain-chosen
terminal status such as `missing' (see `hitl-plan-domain' UNRESOLVED-STATUS)."
  id target op (status 'pending) note message)

(cl-defstruct (hitl-plan-op-spec (:constructor hitl-plan-op-spec--make)
                                 (:copier nil))
  "Registered operation specification."
  domain name key label read-args apply describe from-json schema validate
  queueable order-last extra)

(cl-defstruct (hitl-plan-domain (:constructor hitl-plan-domain--make)
                                (:predicate hitl-plan-domain-struct-p)
                                (:copier nil))
  "Registered target domain.
DRIVER is (lambda (PLAN OP PENDING-STEPS RESOLVED-ALIST)); see
`hitl-plan-execute'.
UNRESOLVED-STATUS (symbol, default `failed') and UNRESOLVED-MESSAGE (string
or (lambda (PLAN STEP)) returning one; default \"target no longer
resolves\") describe how steps whose target cannot be resolved are
marked.  ACCEPT-BUFFER is an optional predicate on a buffer, used by
integrations such as agent-shell-queue-plan.
DIRECTORY (string or function of no arguments) is the domain's default plan
store; MIGRATE-PLIST is (lambda (PLIST)) returning upgraded legacy saved
data; AFTER-STEPS-ADDED is (lambda (PLAN NEW-STEPS)) run by `hitl-plan-create'
and `hitl-plan-add-op' after steps are added and before the save.
SANITIZE-TARGET is (lambda (TARGET)) returning a trustworthy replacement for
a caller-supplied TARGET, or signalling an error; the MCP tools apply it to
explicit targets so an agent cannot show one thing and act on another."
  name label resolve-target capture-target format-target visit-target
  build-batch rebuild-batch lock-batch unlock-batch driver inject-note
  unresolved-status unresolved-message accept-buffer
  directory migrate-plist after-steps-added sanitize-target)

(defvar hitl-plan--store (make-hash-table :test #'equal)
  "In-memory cache of `hitl-plan' objects keyed by ID.
The cache is keyed by ID alone, so plan IDs are assumed to be unique
across every store (directory): loading a same-ID plan from another
directory replaces the cached one, and `hitl-plan-create' refuses an
explicit ID that is already cached or saved in its target directory.")

(defvar hitl-plan-current-plan nil
  "The `hitl-plan' whose step is running, while a step applies.")

(defvar hitl-plan-current-step nil
  "The `hitl-plan-step' being applied.")

(defvar hitl-plan-current-op nil
  "The `hitl-plan-op' being applied.")

(defvar hitl-plan-current-target nil
  "The resolved target (marker or domain context) being applied.")

(defvar hitl-plan-batch-buffer nil
  "Buffer returned by the domain's batch rebuild during a :driver call.")

;;; Trust Origin

(defun hitl-plan-origin-mcp-p (plan)
  "Return non-nil if PLAN was created over MCP (metadata `:origin' is `mcp').
Such plans are untrusted: they are subject to the restrictions of
`hitl-plan-restricted-p' and need user approval to run unattended."
  (and (hitl-plan-p plan)
       (eq (plist-get (hitl-plan-metadata plan) :origin) 'mcp)))

(defun hitl-plan-restricted-p ()
  "Return non-nil when running on behalf of an MCP-origin plan.
True when `hitl-plan-restricted' is set or `hitl-plan-current-plan' is an
MCP-origin plan."
  (or hitl-plan-restricted
      (hitl-plan-origin-mcp-p hitl-plan-current-plan)))

(defun hitl-plan-approved-p (plan)
  "Return non-nil if PLAN may run unattended (queue dispatch, MCP execute).
That holds for plans not created over MCP, when `hitl-plan-agent-may-apply'
is non-nil, and for plans carrying the `:approved' metadata marker a user
sets by queueing the plan interactively (see
`agent-shell-queue-plan-enqueue')."
  (or (not (hitl-plan-origin-mcp-p plan))
      hitl-plan-agent-may-apply
      (plist-get (hitl-plan-metadata plan) :approved)))

;;; Domain Registry

(defvar hitl-plan--domain-registry (make-hash-table :test #'eq)
  "Hash table mapping domain names to `hitl-plan-domain' structs.")

(cl-defun hitl-plan-register-domain
    (name &key label resolve-target capture-target format-target visit-target
          build-batch rebuild-batch lock-batch unlock-batch driver inject-note
          unresolved-status unresolved-message accept-buffer
          directory migrate-plist after-steps-added sanitize-target)
  "Register (or replace) target domain NAME and return its struct.
The keyword arguments LABEL (default: the symbol name), RESOLVE-TARGET,
CAPTURE-TARGET, FORMAT-TARGET, VISIT-TARGET, BUILD-BATCH, REBUILD-BATCH,
LOCK-BATCH, UNLOCK-BATCH, DRIVER, INJECT-NOTE, UNRESOLVED-STATUS,
UNRESOLVED-MESSAGE, ACCEPT-BUFFER, DIRECTORY, MIGRATE-PLIST,
AFTER-STEPS-ADDED and SANITIZE-TARGET populate the like-named
`hitl-plan-domain' slots.
Domains are kept in registration order (re-registering keeps a
domain's place), which breaks ties whenever several domains match, for
example two domains sharing one :directory store."
  (declare (indent 1))
  (unless (and name (symbolp name))
    (user-error "Domain name must be a symbol: %S" name))
  (puthash name
           (hitl-plan-domain--make
            :name name :label (or label (symbol-name name))
            :resolve-target resolve-target :capture-target capture-target
            :format-target format-target :visit-target visit-target
            :build-batch build-batch :rebuild-batch rebuild-batch
            :lock-batch lock-batch :unlock-batch unlock-batch
            :driver driver :inject-note inject-note
            :unresolved-status unresolved-status
            :unresolved-message unresolved-message
            :accept-buffer accept-buffer :directory directory
            :migrate-plist migrate-plist :after-steps-added after-steps-added
            :sanitize-target sanitize-target)
           hitl-plan--domain-registry))

(defun hitl-plan-in-domain-p (plan name)
  "Return non-nil if PLAN belongs to domain NAME (symbol or string).
Convenient for filtering `hitl-plan-step-functions' and
`hitl-plan-after-apply-functions', which run for every domain."
  (let ((dom (hitl-plan-domain plan)))
    (eq dom (if (stringp name) (intern name) name))))

(defun hitl-plan-get-domain (name)
  "Return the `hitl-plan-domain' registered as NAME (symbol or string), or nil."
  (gethash (if (stringp name) (intern name) name) hitl-plan--domain-registry))

(defun hitl-plan-domain-store (domain)
  "Return the default plan directory of DOMAIN, or nil.
DOMAIN is a name (symbol or string) or a `hitl-plan-domain'.  The domain's
:directory slot may be a string or a function of no arguments."
  (when-let* ((dom (if (hitl-plan-domain-struct-p domain)
                       domain
                     (and domain (hitl-plan-get-domain domain))))
              (dir (hitl-plan-domain-directory dom))
              (dir (if (functionp dir) (funcall dir) dir)))
    (and (stringp dir) dir)))

(defun hitl-plan-registered-domains ()
  "Return all registered `hitl-plan-domain' structs, in registration order."
  (let (domains)
    (maphash (lambda (_name dom) (push dom domains)) hitl-plan--domain-registry)
    (nreverse domains)))

;;; Operation Registry

(defvar hitl-plan--op-registry (make-hash-table :test #'equal)
  "Hash table mapping (DOMAIN . NAME) to `hitl-plan-op-spec' structs.")

(cl-defun hitl-plan-register-op
    (domain name &key key label queueable read-args validate apply describe
            from-json schema order-last extra)
  "Register operation NAME under DOMAIN (a symbol, or t for any domain).
KEY is an optional character that must be unique within DOMAIN.  APPLY is
called with the operation's args; READ-ARGS interactively returns an arg
list; VALIDATE is called with the args and returns a problem string or nil;
DESCRIBE receives the args and returns a string; FROM-JSON maps a JSON
alist to an arg list; SCHEMA documents arguments; QUEUEABLE marks
operations safe to run without prompting; ORDER-LAST runs the operation
after all others; EXTRA is an arbitrary plist of domain metadata, read
with `hitl-plan-op-spec-extra'.  Returns the `hitl-plan-op-spec'."
  (declare (indent 2))
  (unless (and domain (symbolp domain))
    (user-error "Domain must be a symbol: %S" domain))
  (unless (and name (symbolp name))
    (user-error "Operation name must be a symbol: %S" name))
  (when key
    (dolist (other (hitl-plan--domain-specs domain))
      (when (and (eql (hitl-plan-op-spec-key other) key)
                 (not (eq (hitl-plan-op-spec-name other) name)))
        (error "Operation %s key `%c' collides with operation %s in domain %s"
               name key (hitl-plan-op-spec-name other) domain))))
  (puthash (cons domain name)
           (hitl-plan-op-spec--make
            :domain domain :name name :key key
            :label (or label (symbol-name name))
            :read-args read-args :apply apply :describe describe
            :from-json from-json :schema schema :validate validate
            :queueable queueable :order-last order-last :extra extra)
           hitl-plan--op-registry))

(defun hitl-plan--domain-specs (domain)
  "Return specs registered exactly under DOMAIN."
  (let (specs)
    (maphash (lambda (k spec) (when (eq (car k) domain) (push spec specs)))
             hitl-plan--op-registry)
    specs))

(defun hitl-plan-get-op (domain name)
  "Return the `hitl-plan-op-spec' NAME in DOMAIN, falling back to domain t."
  (let ((name (if (stringp name) (intern name) name)))
    (or (gethash (cons domain name) hitl-plan--op-registry)
        (gethash (cons t name) hitl-plan--op-registry))))

(defun hitl-plan-registered-ops (&optional domain)
  "Return registered `hitl-plan-op-spec's.
With DOMAIN, return that domain's operations plus domain-t operations not
shadowed by it.  Without DOMAIN, return every registered operation."
  (if (null domain)
      (hash-table-values hitl-plan--op-registry)
    (let ((own (hitl-plan--domain-specs domain)))
      (append own
              (seq-remove (lambda (spec)
                            (seq-find (lambda (o) (eq (hitl-plan-op-spec-name o)
                                                      (hitl-plan-op-spec-name spec)))
                                      own))
                          (hitl-plan--domain-specs t))))))

(defun hitl-plan-op-for-key (domain key)
  "Return the `hitl-plan-op-spec' in DOMAIN (or fallback t) bound to KEY."
  (seq-find (lambda (spec) (eql (hitl-plan-op-spec-key spec) key))
            (hitl-plan-registered-ops domain)))

(defun hitl-plan--op-name (op)
  "Return the operation name symbol of OP (struct or symbol)."
  (if (hitl-plan-op-p op) (hitl-plan-op-name op) op))

;; Unattended callers (MCP, queues) may only run `:queueable' operations.
(defun hitl-plan-queueable-spec (domain name)
  "Return the spec of op NAME in DOMAIN.
Signal `user-error' if there is no such operation or it is not queueable."
  (let ((spec (hitl-plan-get-op domain name)))
    (unless spec
      (user-error "Unknown operation `%s' in domain %s" name domain))
    (unless (hitl-plan-op-spec-queueable spec)
      (user-error "Operation `%s' is not queueable" name))
    spec))

(defun hitl-plan-check-unattended (plan)
  "Signal `user-error' unless PLAN can run without prompting.
That requires log-notes other than `each' and only queueable operations."
  (when (eq (hitl-plan-log-notes plan) 'each)
    (user-error "Cannot run unattended with `each' log-notes mode"))
  (dolist (op (hitl-plan-ops plan))
    (hitl-plan-queueable-spec (hitl-plan-domain plan) (hitl-plan-op-name op))))

(defun hitl-plan-order-ops (domain ops)
  "Order OPS of DOMAIN so operations registered `:order-last' come last.
OPS may be `hitl-plan-op' structs or symbols; relative order is kept."
  (let (normal last)
    (dolist (op ops)
      (let ((spec (hitl-plan-get-op domain (hitl-plan--op-name op))))
        (if (and spec (hitl-plan-op-spec-order-last spec))
            (push op last)
          (push op normal))))
    (append (nreverse normal) (nreverse last))))

(defun hitl-plan-describe-op (domain op)
  "Return a human-readable description of OP (struct or symbol) in DOMAIN."
  (let* ((name (hitl-plan--op-name op))
         (args (when (hitl-plan-op-p op) (hitl-plan-op-args op)))
         (spec (hitl-plan-get-op domain name)))
    (if (and spec (hitl-plan-op-spec-describe spec))
        (condition-case nil
            (apply (hitl-plan-op-spec-describe spec) args)
          (error (format "%s %S" name args)))
      (if args
          (let ((print-escape-newlines t)
                (print-escape-control-characters t))
            (format "%s %s" name (mapconcat (lambda (a) (format "%S" a)) args " ")))
        (format "%s" name)))))

;;; Built-in file-target Domain

(defun hitl-plan-file-target-capture ()
  "Return a target plist for the line at point in the current file buffer."
  (let ((snippet (string-trim (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position)))))
    (list :file (buffer-file-name (buffer-base-buffer))
          :line (line-number-at-pos nil t)
          :pos (line-beginning-position)
          :snippet (unless (string-empty-p snippet) snippet))))

(defun hitl-plan-file-target-resolve (target)
  "Resolve file TARGET plist to a marker at the start of its line, or nil.
With :snippet, the line containing the snippet closest to :pos (or :line)
wins; without it, :line then :pos is used directly."
  (let ((file (plist-get target :file))
        (line (plist-get target :line))
        (pos (plist-get target :pos))
        (snippet (let ((sn (plist-get target :snippet)))
                   (and (stringp sn) (not (string-empty-p sn)) sn))))
    (when (and (stringp file) (file-exists-p file)
               (not (and (hitl-plan-restricted-p) (file-remote-p file))))
      (with-current-buffer (let ((enable-local-variables
                                  (if (hitl-plan-restricted-p) nil enable-local-variables))
                                 (enable-dir-local-variables
                                  (and (not (hitl-plan-restricted-p))
                                       enable-dir-local-variables)))
                             (find-file-noselect file))
        (save-excursion
          (save-restriction
            (widen)
            (cond
             (snippet
              (let ((hint (or pos
                              (and line (progn (goto-char (point-min))
                                               (forward-line (1- line))
                                               (point)))
                              1))
                    best)
                (goto-char (point-min))
                (while (search-forward snippet nil t)
                  (let ((bol (line-beginning-position)))
                    (when (or (null best)
                              (< (abs (- bol hint)) (abs (- best hint))))
                      (setq best bol))
                    (forward-line 1)))
                (when best
                  (goto-char best)
                  (point-marker))))
             (line
              (goto-char (point-min))
              (when (zerop (forward-line (1- line)))
                (point-marker)))
             ((and pos (<= (point-min) pos) (<= pos (point-max)))
              (goto-char pos)
              (goto-char (line-beginning-position))
              (point-marker)))))))))

(defun hitl-plan-file-target-format (target)
  "Return (FILE-COLUMN . LABEL-COLUMN) strings describing TARGET."
  (cons (if-let* ((file (plist-get target :file)))
            (file-name-nondirectory file)
          "")
        (or (plist-get target :snippet)
            (when-let* ((line (plist-get target :line)))
              (format "line %d" line))
            "")))

(defun hitl-plan-file-target-visit (target &optional other-window)
  "Visit TARGET, in another window when OTHER-WINDOW is non-nil."
  (when-let* ((m (hitl-plan-file-target-resolve target)))
    (if other-window
        (switch-to-buffer-other-window (marker-buffer m))
      (switch-to-buffer (marker-buffer m)))
    (goto-char m)
    (set-marker m nil)))

(hitl-plan-register-domain 'file-target
  :label "File target"
  :resolve-target #'hitl-plan-file-target-resolve
  :capture-target #'hitl-plan-file-target-capture
  :format-target #'hitl-plan-file-target-format
  :visit-target #'hitl-plan-file-target-visit)

;;; Persistence

(defun hitl-plan--directory (&optional plan)
  "Return the storage directory for PLAN (default `hitl-plan-directory')."
  (file-name-as-directory
   (expand-file-name (or (and plan (hitl-plan-directory plan))
                         hitl-plan-directory))))

(defun hitl-plan-file (id &optional directory)
  "Return the absolute `.eld' path of plan ID in DIRECTORY or the default."
  (expand-file-name (format "%s.eld" id)
                    (or directory (hitl-plan--directory))))

(defun hitl-plan--now ()
  "Return the current time as an ISO-8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun hitl-plan-to-plist (plan)
  "Convert PLAN into a printable plist.
Signal an error if a step's operation is not one of the plan's ops, since
the file would not load back."
  (let ((ops (hitl-plan-ops plan)))
    (list :version hitl-plan-format-version
          :id (hitl-plan-id plan)
          :domain (hitl-plan-domain plan)
          :description (hitl-plan-description plan)
          :created (hitl-plan-created plan)
          :updated (hitl-plan-updated plan)
          :status (hitl-plan-status plan)
          :scope (hitl-plan-scope plan)
          :log-notes (hitl-plan-log-notes plan)
          :note (hitl-plan-note plan)
          :metadata (hitl-plan-metadata plan)
          :ops (mapcar (lambda (op) (list :name (hitl-plan-op-name op)
                                          :args (hitl-plan-op-args op)))
                       ops)
          :steps (mapcar (lambda (step)
                           (list :id (hitl-plan-step-id step)
                                 :target (hitl-plan-step-target step)
                                 :op-index (or (seq-position ops (hitl-plan-step-op step) #'eq)
                                               (error "Step %S of plan %s has no operation in the plan"
                                                      (hitl-plan-step-id step)
                                                      (hitl-plan-id plan)))
                                 :status (hitl-plan-step-status step)
                                 :message (hitl-plan-step-message step)
                                 :note (hitl-plan-step-note step)))
                         (hitl-plan-steps plan)))))

(defun hitl-plan-from-plist (data &optional legacy-domain)
  "Reconstruct a `hitl-plan' from plist DATA.
When DATA's domain (or, if DATA has no `:domain', LEGACY-DOMAIN) names a
domain with a :migrate-plist function, DATA is passed through it first.
Data still lacking a domain afterwards belongs to LEGACY-DOMAIN when that
is registered, else `file-target'.
Signal an error for malformed data: non-list :ops or :steps, or a step
whose :op-index is missing or out of range."
  (let* ((data (let* ((name (or (plist-get data :domain) legacy-domain))
                      (dom (and name (hitl-plan-get-domain name)))
                      (fn (and dom (hitl-plan-domain-migrate-plist dom))))
                 (if fn (funcall fn data) data)))
         (raw-ops (plist-get data :ops))
         (raw-steps (plist-get data :steps))
         (_ (unless (and (proper-list-p raw-ops) (proper-list-p raw-steps))
              (error "Invalid plan structure: :ops and :steps must be lists")))
         (ops (mapcar (lambda (pl)
                        (unless (and (consp pl) (plistp pl)
                                     (symbolp (plist-get pl :name))
                                     (plist-get pl :name)
                                     (proper-list-p (plist-get pl :args)))
                          (error "Invalid plan operation: %S" pl))
                        (hitl-plan-op--make :name (plist-get pl :name)
                                            :args (plist-get pl :args)))
                      raw-ops))
         (steps (mapcar (lambda (pl)
                          (unless (and (consp pl) (plistp pl))
                            (error "Invalid plan step: %S" pl))
                          (let ((idx (plist-get pl :op-index)))
                            (unless (and (integerp idx) (<= 0 idx) (< idx (length ops)))
                              (error "Step %S has missing or invalid :op-index %S"
                                     (plist-get pl :id) idx))
                            (hitl-plan-step--make
                             :id (plist-get pl :id)
                             :target (plist-get pl :target)
                             :op (nth idx ops)
                             :status (or (plist-get pl :status) 'pending)
                             :message (plist-get pl :message)
                             :note (plist-get pl :note))))
                        raw-steps)))
    (hitl-plan--make
     :id (plist-get data :id)
     :domain (or (plist-get data :domain)
                 (and legacy-domain (hitl-plan-get-domain legacy-domain)
                      legacy-domain)
                 'file-target)
     :description (plist-get data :description)
     :created (plist-get data :created)
     :updated (plist-get data :updated)
     :status (or (plist-get data :status) 'draft)
     :scope (plist-get data :scope)
     :log-notes (or (plist-get data :log-notes) 'ignore)
     :note (plist-get data :note)
     :metadata (plist-get data :metadata)
     :ops ops
     :steps steps)))


(defun hitl-plan-save (plan)
  "Save PLAN atomically (temp file + rename) and cache it.  Return PLAN."
  (let* ((dir (hitl-plan--directory plan))
         (id (hitl-plan-id plan))
         (file (hitl-plan-file id dir))
         (print-length nil)
         (print-level nil)
         temp)
    (make-directory dir t)
    (setf (hitl-plan-updated plan) (hitl-plan--now))
    (setq temp (make-temp-file (expand-file-name (format ".%s-" id) dir) nil ".tmp"))
    (unwind-protect
        (progn
          (with-temp-file temp
            (insert ";; -*- mode: lisp-data; -*-\n")
            (pp (hitl-plan-to-plist plan) (current-buffer)))
          (rename-file temp file t))
      (when (file-exists-p temp)
        (ignore-errors (delete-file temp))))
    (puthash id plan hitl-plan--store)
    plan))

(defun hitl-plan-load (id-or-file &optional directory legacy-domain)
  "Load and cache the plan stored under ID-OR-FILE.
An ID is looked up in DIRECTORY (default `hitl-plan-directory').
LEGACY-DOMAIN names the domain of files lacking `:domain'.  By default it
is the registered domain whose store (`hitl-plan-domain-store') is the
file's directory, else `file-target'; see `hitl-plan--domain-for-directory'.
Return nil (after a message) when the file is missing, unreadable, or
corrupt; never signals."
  (let ((file (if (and (stringp id-or-file)
                       (or (file-name-absolute-p id-or-file)
                           (string-suffix-p ".eld" id-or-file)))
                  (expand-file-name id-or-file)
                (hitl-plan-file id-or-file directory))))
    (when (file-exists-p file)
      (condition-case err
          (let ((data (with-temp-buffer
                        (insert-file-contents file)
                        (goto-char (point-min))
                        (read (current-buffer)))))
            (unless (and (consp data) (plistp data)
                         (equal (plist-get data :version) hitl-plan-format-version)
                         (stringp (plist-get data :id)))
              (error "Invalid plan structure or version"))
            (let ((plan (hitl-plan-from-plist
                         data
                         (or legacy-domain
                             (and (null (plist-get data :domain))
                                  (hitl-plan--domain-for-directory
                                   (file-name-directory file))))))
                  (dir (file-name-directory file)))
              (unless (equal (file-name-as-directory dir) (hitl-plan--directory))
                (setf (hitl-plan-directory plan) dir))
              (puthash (hitl-plan-id plan) plan hitl-plan--store)
              plan))
        (error
         (message "hitl-plan: ignoring corrupt plan file %s: %s"
                  file (error-message-string err))
         nil)))))

(defun hitl-plan--domain-for-directory (directory)
  "Return the name of a registered domain whose store is DIRECTORY, or nil.
Paths are compared expanded and with a trailing slash.  Domains without a
:directory never match.  If several domains share the store, the first
registered one wins."
  (let ((want (file-name-as-directory (expand-file-name directory)))
        found)
    (dolist (dom (hitl-plan-registered-domains))
      (when-let* (((not found))
                  (store (hitl-plan-domain-store dom))
                  ((equal want (file-name-as-directory (expand-file-name store)))))
        (setq found (hitl-plan-domain-name dom))))
    found))

(defun hitl-plan--in-directory-p (plan directory)
  "Return non-nil if PLAN is stored in DIRECTORY (default directory if nil)."
  (equal (hitl-plan--directory plan)
         (file-name-as-directory
          (expand-file-name (or directory hitl-plan-directory)))))

(defun hitl-plan-get (id &optional directory domain)
  "Return the plan registered or saved under ID, or nil.
With DIRECTORY, only a plan stored in that directory matches; the cache
and the file lookup both respect it.  Without DIRECTORY but with DOMAIN,
the domain's :directory (see `hitl-plan-domain-store') is used, if any."
  (when id
    (let ((directory (or directory (hitl-plan-domain-store domain)))
          (cached (gethash id hitl-plan--store)))
      (if (and cached (or (null directory)
                          (hitl-plan--in-directory-p cached directory)))
          cached
        (hitl-plan-load (hitl-plan-file id directory) directory)))))

(defun hitl-plan-list (&optional statuses directory domain)
  "Return all cached and saved plans, optionally filtered.
STATUSES limits by plan status.  DIRECTORY, when given, restricts the
result to plans stored in it (and scans it for saved plans); otherwise the
default directory is scanned and every cached plan is included.  DOMAIN,
a symbol or string, restricts to plans of that domain; without DIRECTORY,
the domain's :directory (see `hitl-plan-domain-store') acts as DIRECTORY.
Corrupt files are skipped.  Sorted by creation time, then ID."
  (setq directory (or directory (hitl-plan-domain-store domain)))
  (let ((dir (file-name-as-directory
              (expand-file-name (or directory hitl-plan-directory)))))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\`[^.].*\\.eld\\'"))
        (unless (let ((c (gethash (file-name-base file) hitl-plan--store)))
                  (and c (hitl-plan--in-directory-p c dir)))
          (hitl-plan-load file)))))
  (let ((all (sort (hash-table-values hitl-plan--store)
                   (lambda (a b)
                     (string< (format "%s%s" (hitl-plan-created a) (hitl-plan-id a))
                              (format "%s%s" (hitl-plan-created b) (hitl-plan-id b)))))))
    (when directory
      (setq all (seq-filter (lambda (p) (hitl-plan--in-directory-p p directory)) all)))
    (when domain
      (setq all (seq-filter (lambda (p) (hitl-plan-in-domain-p p domain)) all)))
    (if statuses
        (seq-filter (lambda (p) (memq (hitl-plan-status p) statuses)) all)
      all)))

(defun hitl-plan-discard (plan)
  "Mark PLAN `discarded', save it, and return it."
  (setf (hitl-plan-status plan) 'discarded)
  (hitl-plan-save plan))

;;; Plan Construction

(defun hitl-plan--next-id (&optional directory)
  "Return a fresh plan ID that collides with no cached or saved plan.
Saved plans are looked for in DIRECTORY (default `hitl-plan-directory')."
  (let (id)
    (while (progn
             (setq id (format "plan-%s-%04x"
                              (format-time-string "%Y%m%d-%H%M%S")
                              (random #x10000)))
             (or (gethash id hitl-plan--store)
                 (file-exists-p (hitl-plan-file id directory)))))
    id))

(defun hitl-plan--normalize-op (op)
  "Coerce OP (struct, (NAME . ARGS), plist with :name, or symbol) to a struct."
  (cond
   ((hitl-plan-op-p op) op)
   ((and (consp op) (plistp op) (plist-get op :name))
    (hitl-plan-op--make :name (plist-get op :name) :args (plist-get op :args)))
   ((consp op) (hitl-plan-op--make :name (car op) :args (cdr op)))
   ((and op (symbolp op)) (hitl-plan-op--make :name op))
   (t (error "Invalid operation spec: %S" op))))

(defun hitl-plan--make-step (id target op)
  "Return a pending step with ID for TARGET and OP."
  (hitl-plan-step--make :id id :target target :op op))

(cl-defun hitl-plan-create
    (&key (domain 'file-target) scope targets ops description
          (log-notes hitl-plan-log-notes) note (status 'draft) directory
          metadata (save t) id)
  "Create a `hitl-plan' of OPS crossed with TARGETS in DOMAIN.
SCOPE, DESCRIPTION, LOG-NOTES, NOTE, STATUS and METADATA populate the
plan.  OPS entries may be `hitl-plan-op's, (NAME . ARGS), or symbols.  Steps are
grouped by operation, with `:order-last' operations last.  When SAVE is
non-nil the plan is persisted (in DIRECTORY, else the domain's :directory,
else `hitl-plan-directory').  ID, when non-nil, is used instead of a
generated plan ID; it is a `user-error' if a plan with that ID is already
cached or saved in the target directory (see `hitl-plan--store').  The
domain's :after-steps-added function is called with the plan and all
created steps (when any) after the steps are built and before the first
save."
  (unless (hitl-plan-get-domain domain)
    (user-error "Unknown hitl-plan domain: %S" domain))
  (when (and id (or (gethash id hitl-plan--store)
                    (file-exists-p
                     (hitl-plan-file id (file-name-as-directory
                                         (expand-file-name
                                          (or directory (hitl-plan-domain-store domain)
                                              hitl-plan-directory)))))))
    (user-error "Plan %s already exists" id))
  (let* ((norm-ops (hitl-plan-order-ops domain (mapcar #'hitl-plan--normalize-op ops)))
         (step-id 0)
         (steps (seq-mapcat
                 (lambda (op)
                   (mapcar (lambda (target)
                             (hitl-plan--make-step (cl-incf step-id) target op))
                           targets))
                 norm-ops))
         (plan (hitl-plan--make
                :id (or id (hitl-plan--next-id
                          (or directory (hitl-plan-domain-store domain))))
                :domain domain
                :created (hitl-plan--now) :description description
                :scope scope :ops norm-ops :log-notes log-notes :note note
                :steps steps :status status
                :directory (or directory (hitl-plan-domain-store domain))
                :metadata metadata)))
    (when-let* ((hook (hitl-plan-domain-after-steps-added
                       (hitl-plan-get-domain domain)))
                (steps steps))
      (funcall hook plan steps))
    (puthash (hitl-plan-id plan) plan hitl-plan--store)
    (when save (hitl-plan-save plan))
    plan))

(defun hitl-plan-add-op (plan op &optional targets)
  "Add OP to PLAN for TARGETS (default: PLAN's unique existing targets).
Re-orders operations (`:order-last' last), reopens a complete plan if new
steps are pending, saves, and returns PLAN.  The domain's
:after-steps-added function is called with PLAN and only the new steps
\(when any) after they are appended and before the save."
  (let* ((op (hitl-plan--normalize-op op))
         (targets (or targets
                      (seq-uniq (mapcar #'hitl-plan-step-target (hitl-plan-steps plan))
                                #'equal)))
         (max-id (apply #'max 0 (mapcar #'hitl-plan-step-id (hitl-plan-steps plan))))
         (new-steps (mapcar (lambda (target)
                              (hitl-plan--make-step (cl-incf max-id) target op))
                            targets))
         (all-ops (hitl-plan-order-ops (hitl-plan-domain plan)
                                       (append (hitl-plan-ops plan) (list op))))
         (all-steps (append (hitl-plan-steps plan) new-steps)))
    (setf (hitl-plan-ops plan) all-ops)
    (setf (hitl-plan-steps plan)
          (seq-mapcat (lambda (o)
                        (seq-filter (lambda (s) (eq (hitl-plan-step-op s) o)) all-steps))
                      all-ops))
    (when-let* ((new-steps new-steps)
                (hook (hitl-plan-domain-after-steps-added
                       (hitl-plan-get-domain (hitl-plan-domain plan)))))
      (funcall hook plan new-steps))
    (when (and (eq (hitl-plan-status plan) 'complete)
               (seq-some (lambda (s) (eq (hitl-plan-step-status s) 'pending))
                         new-steps))
      (setf (hitl-plan-status plan) 'paused))
    (hitl-plan-save plan)))

;;; Execution

(defun hitl-plan--unresolved (plan step)
  "Mark STEP of PLAN unresolvable using the domain's status and message."
  (let* ((domain (hitl-plan-get-domain (hitl-plan-domain plan)))
         (status (or (and domain (hitl-plan-domain-unresolved-status domain))
                     'failed))
         (msg (and domain (hitl-plan-domain-unresolved-message domain))))
    (setf (hitl-plan-step-status step) status
          (hitl-plan-step-message step)
          (cond ((functionp msg) (funcall msg plan step))
                ((stringp msg) msg)
                (t "target no longer resolves")))))

(defun hitl-plan--fail (step message)
  "Mark STEP failed with MESSAGE."
  (setf (hitl-plan-step-status step) 'failed
        (hitl-plan-step-message step) message))

(defun hitl-plan--call-at (resolved fn)
  "Call FN with RESOLVED's buffer current and point at it, when a live marker."
  (if (and (markerp resolved) (marker-buffer resolved)
           (buffer-live-p (marker-buffer resolved)))
      (with-current-buffer (marker-buffer resolved)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char resolved)
            (funcall fn))))
    (funcall fn)))

(defun hitl-plan--resolve (plan step)
  "Resolve STEP's target through PLAN's domain, or nil."
  (when-let* ((domain (hitl-plan-get-domain (hitl-plan-domain plan)))
              (resolver (hitl-plan-domain-resolve-target domain)))
    (condition-case nil
        (funcall resolver (hitl-plan-step-target step))
      (error nil))))

(defun hitl-plan-run-step (plan step op &optional resolved)
  "Run pending STEP of PLAN under OP, quit-safely, and save PLAN.
RESOLVED is the already-resolved target; when nil the domain resolves it.
Domain drivers call this for each target.  On success STEP becomes `done'
and `hitl-plan-step-functions' runs; errors mark it `failed'.  If \\[keyboard-quit]
interrupts the application the step stays `pending' and `quit-flag' is left
set for the caller.  Returns the step's resulting status, or nil when STEP
was not pending."
  (when (eq (hitl-plan-step-status step) 'pending)
    (let* ((dom-name (hitl-plan-domain plan))
           (domain (hitl-plan-get-domain dom-name))
           (spec (hitl-plan-get-op dom-name (hitl-plan-op-name op)))
           (ours (null resolved))
           (resolved (or resolved (and spec (hitl-plan--resolve plan step))))
           (hitl-plan-current-plan plan)
           (hitl-plan-current-step step)
           (hitl-plan-current-op op)
           (hitl-plan-current-target resolved)
           (inhibit-quit t)
           (completed nil))
      (cond
       ((null spec)
        (hitl-plan--fail step (format "Unknown operation: %s" (hitl-plan-op-name op))))
       ((null resolved)
        (hitl-plan--unresolved plan step))
       (t
        (condition-case err
            (progn
              (with-local-quit
                (hitl-plan--call-at
                 resolved
                 (lambda ()
                   (let ((args (hitl-plan-op-args op)))
                     (when-let* ((validate (hitl-plan-op-spec-validate spec))
                                 (problem (apply validate args)))
                       (error "%s" problem))
                     (apply (hitl-plan-op-spec-apply spec) args)
                     (when-let* ((inject (and domain (hitl-plan-domain-inject-note domain)))
                                 (text (pcase (hitl-plan-log-notes plan)
                                         ('one (hitl-plan-note plan))
                                         ('each (hitl-plan-step-note step))))
                                 ((not (string-empty-p text))))
                       (funcall inject text step plan)))))
                (setq completed t))
              (when completed
                (setf (hitl-plan-step-status step) 'done
                      (hitl-plan-step-message step) nil)
                (condition-case herr
                    (hitl-plan--call-at
                     resolved
                     (lambda ()
                       (run-hook-with-args 'hitl-plan-step-functions step plan)))
                  (error
                   (setf (hitl-plan-step-message step)
                         (format "step hook error: %s" (error-message-string herr)))))))
          (error (hitl-plan--fail step (error-message-string err))))))
      (when (and ours (markerp resolved))
        (set-marker resolved nil))
      (hitl-plan-save plan)
      (hitl-plan-step-status step))))

(defun hitl-plan--pending-steps (plan op)
  "Return PLAN's pending steps belonging to OP."
  (seq-filter (lambda (s) (and (eq (hitl-plan-step-op s) op)
                               (eq (hitl-plan-step-status s) 'pending)))
              (hitl-plan-steps plan)))

(defun hitl-plan-rebuild-batch (plan op)
  "Resolve PLAN's pending steps for OP; return (BUFFER . ALIST).
ALIST maps each resolvable step to its resolved target.  Unresolvable
pending steps are marked via the domain's UNRESOLVED-STATUS and
UNRESOLVED-MESSAGE (default `failed', \"target no longer resolves\").
Uses the domain's :rebuild-batch (returning (BUFFER . ALIST)) when set,
otherwise :resolve-target per step with BUFFER nil."
  (let* ((domain (hitl-plan-get-domain (hitl-plan-domain plan)))
         (pending (hitl-plan--pending-steps plan op))
         (rebuild (and domain (hitl-plan-domain-rebuild-batch domain)))
         (batch (if rebuild
                    (let ((result (funcall rebuild plan)))
                      (cons (car result)
                            (seq-filter (lambda (cell) (memq (car cell) pending))
                                        (cdr result))))
                  (cons nil
                        (delq nil (mapcar (lambda (s)
                                            (when-let* ((r (hitl-plan--resolve plan s)))
                                              (cons s r)))
                                          pending))))))
    (dolist (s pending)
      (unless (assq s (cdr batch))
        (hitl-plan--unresolved plan s)))
    batch))

(defun hitl-plan--run-after-apply (plan before-done)
  "Run `hitl-plan-after-apply-functions' for steps of PLAN newly done.
BEFORE-DONE is the list of steps already done before this execution."
  (let ((fresh (seq-filter (lambda (s) (and (eq (hitl-plan-step-status s) 'done)
                                            (not (memq s before-done))))
                           (hitl-plan-steps plan))))
    (when (and fresh (or hitl-plan-after-apply-functions hitl-plan-save-buffers))
      (pcase-dolist (`(,file . ,steps)
                     (seq-group-by (lambda (s) (plist-get (hitl-plan-step-target s) :file))
                                   fresh))
        (condition-case err
            (if (and file (file-exists-p file))
                (with-current-buffer (let ((enable-local-variables
                                           (if (hitl-plan-restricted-p)
                                               nil enable-local-variables)))
                                      (find-file-noselect file))
                  (run-hook-with-args 'hitl-plan-after-apply-functions file steps plan)
                  (when (and hitl-plan-save-buffers buffer-file-name (buffer-modified-p))
                    (let ((inhibit-message t))
                      (save-buffer))))
              (run-hook-with-args 'hitl-plan-after-apply-functions file steps plan))
          (error (message "hitl-plan: after-apply hook failed for %s: %s"
                          file (error-message-string err))))))))

(defun hitl-plan--run-op (plan op domain)
  "Run pending steps of OP in PLAN via DOMAIN.  Return non-nil if quit.
A quit inside the domain driver is absorbed (and returned as non-nil); one
inside the batch rebuild propagates to `hitl-plan-execute'."
  (let ((driver (and domain (hitl-plan-domain-driver domain))))
    (if driver
        (let* ((batch (hitl-plan-rebuild-batch plan op))
               (resolved (cdr batch))
               (quitted nil))
          (hitl-plan-save plan)
          (when resolved
            (condition-case nil
                (let ((hitl-plan-batch-buffer (car batch)))
                  (funcall driver plan op (mapcar #'car resolved) resolved))
              (quit (setq quitted t))))
          (when quit-flag
            (setq quit-flag nil quitted t))
          quitted)
      ;; Keep `inhibit-quit' bound across the loop so a C-g arriving during
      ;; a step is observed here, between steps, rather than signalled.
      (let ((quitted nil)
            (inhibit-quit t))
        (dolist (step (hitl-plan--pending-steps plan op))
          (unless quitted
            (hitl-plan-run-step plan step op)
            (when quit-flag
              (setq quit-flag nil quitted t))))
        quitted))))

(defun hitl-plan-execute (plan &optional only-op)
  "Execute pending steps of PLAN (or only those of ONLY-OP), return PLAN.
Operations run in `hitl-plan-order-ops' order.  If the plan's domain has a
:driver, it is called per operation with (PLAN OP PENDING-STEPS
RESOLVED-ALIST) and must call `hitl-plan-run-step' for each target;
`hitl-plan-batch-buffer' is bound to the batch buffer.  Otherwise the
built-in iterator runs each pending step quit-safely, saving the plan after
every step.  \\[keyboard-quit] between (or during) steps pauses the plan
cleanly; call again to resume.  Unresolvable steps are marked failed.  Runs
`hitl-plan-after-apply-functions' afterwards, except when a driver or
batch rebuild signals an error or a quit escapes the rebuild: the plan is
then paused and saved, the condition is re-signalled, and the hooks do
not run."
  (pcase (hitl-plan-status plan)
    ('discarded (user-error "Plan %s is discarded" (hitl-plan-id plan)))
    ('complete (user-error "Plan %s is already complete" (hitl-plan-id plan))))
  (let* ((hitl-plan-restricted (or hitl-plan-restricted (hitl-plan-origin-mcp-p plan)))
         (domain (hitl-plan-get-domain (hitl-plan-domain plan)))
         (before-done (seq-filter (lambda (s) (eq (hitl-plan-step-status s) 'done))
                                  (hitl-plan-steps plan)))
         (ops (if only-op
                  (list only-op)
                (hitl-plan-order-ops (hitl-plan-domain plan) (hitl-plan-ops plan))))
         (quitted nil))
    (setf (hitl-plan-status plan) 'running)
    (hitl-plan-save plan)
    ;; An error from a driver or batch rebuild, or a quit during the rebuild
    ;; (quits inside a driver are handled by `hitl-plan--run-op'), must not
    ;; leave the plan `running' on disk: pause it, save, and re-signal.
    (condition-case err
        (dolist (op ops)
          (unless quitted
            (setq quitted (hitl-plan--run-op plan op domain))))
      ((error quit)
       (let ((inhibit-quit t))
         (setf (hitl-plan-status plan) 'paused)
         (hitl-plan-save plan))
       (signal (car err) (cdr err))))
    (when quitted
      (setf (hitl-plan-status plan) 'paused)
      (hitl-plan-save plan))
    (hitl-plan--run-after-apply plan before-done)
    (unless quitted
      (setf (hitl-plan-status plan)
            (if (seq-some (lambda (s) (eq (hitl-plan-step-status s) 'pending))
                          (hitl-plan-steps plan))
                'paused
              'complete))
      (hitl-plan-save plan))
    plan))

;;; Summary (JSON-ready, for MCP)

(defconst hitl-plan--known-counts '(pending done failed missing skipped)
  "Step statuses with their own key in `hitl-plan-summary' :counts.")

(defun hitl-plan-other-counts (plan)
  "Return an alist (STATUS . N) for step statuses of PLAN beyond the known ones.
Known statuses are pending, done, failed, missing and skipped; any other,
for example a domain-chosen terminal status, is counted here, sorted by name."
  (let (alist)
    (dolist (step (hitl-plan-steps plan))
      (let ((st (hitl-plan-step-status step)))
        (unless (memq st hitl-plan--known-counts)
          (if-let* ((cell (assq st alist)))
              (cl-incf (cdr cell))
            (push (cons st 1) alist)))))
    (sort alist (lambda (a b) (string< (format "%s" (car a)) (format "%s" (car b)))))))

(defun hitl-plan-summary (plan)
  "Return PLAN as a JSON-ready plist.
:counts has :total plus one key per common status (:pending :done :failed
:missing :skipped), and :other, a plist mapping each remaining status
present (as a keyword) to its count, so the counts always sum to :total."
  (let* ((dom-name (hitl-plan-domain plan))
         (domain (hitl-plan-get-domain dom-name))
         (fmt (and domain (hitl-plan-domain-format-target domain)))
         (steps (hitl-plan-steps plan))
         (count (lambda (st) (seq-count (lambda (s) (eq (hitl-plan-step-status s) st))
                                        steps))))
    (list :plan_id (hitl-plan-id plan)
          :domain (symbol-name dom-name)
          :description (or (hitl-plan-description plan) :null)
          :created (or (hitl-plan-created plan) :null)
          :updated (or (hitl-plan-updated plan) :null)
          :status (symbol-name (hitl-plan-status plan))
          :log_notes (symbol-name (or (hitl-plan-log-notes plan) 'ignore))
          :note (or (hitl-plan-note plan) :null)
          :counts (list :total (length steps)
                        :pending (funcall count 'pending)
                        :done (funcall count 'done)
                        :failed (funcall count 'failed)
                        :missing (funcall count 'missing)
                        :skipped (funcall count 'skipped)
                        :other (mapcan (lambda (c)
                                         (list (intern (format ":%s" (car c))) (cdr c)))
                                       (hitl-plan-other-counts plan)))
          :ops (vconcat
                (mapcar (lambda (op)
                          (list :name (symbol-name (hitl-plan-op-name op))
                                :args (vconcat (hitl-plan-op-args op))
                                :description (hitl-plan-describe-op dom-name op)))
                        (hitl-plan-ops plan)))
          :steps (vconcat
                  (mapcar
                   (lambda (step)
                     (let* ((tgt (hitl-plan-step-target step))
                            (cols (and fmt (ignore-errors (funcall fmt tgt))))
                            (op (hitl-plan-step-op step)))
                       (list :id (hitl-plan-step-id step)
                             :op (if op (symbol-name (hitl-plan-op-name op)) :null)
                             :args (vconcat (and op (hitl-plan-op-args op)))
                             :file (or (plist-get tgt :file) (car cols) :null)
                             :label (or (cdr cols) :null)
                             :status (symbol-name (hitl-plan-step-status step))
                             :message (or (hitl-plan-step-message step) :null)
                             :note (or (hitl-plan-step-note step) :null))))
                   steps)))))

(provide 'hitl-plan)
;;; hitl-plan.el ends here
