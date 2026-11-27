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
LOG-NOTES is one of `ignore', `one', or `each'.
EXECUTION is `bulk' for operation batches, or `steps' for ordered workflows."
  id domain created updated description scope ops
  (log-notes hitl-plan-log-notes) note steps (status 'draft)
  directory metadata (execution 'bulk))

(cl-defstruct (hitl-plan-op (:constructor hitl-plan-op--make)
                            (:copier nil))
  "One operation in a `hitl-plan'."
  name args)

(cl-defstruct (hitl-plan-step (:constructor hitl-plan-step--make)
                              (:copier nil))
  "One target and operation pairing in a `hitl-plan'.
OP is the `hitl-plan-op' (the same object held in the plan's ops).
STATUS is one of `pending', `done', `failed', `skipped', or a domain-chosen
terminal status such as `missing' (see `hitl-plan-domain' UNRESOLVED-STATUS).
Ordered plans also use `waiting' and `ready'.  REQUEST and RESPONSE hold
suspension data.  HISTORY records transitions, including prior failures."
  id target op (status 'pending) note message request response history)

(cl-defstruct (hitl-plan-op-spec (:constructor hitl-plan-op-spec--make)
                                 (:copier nil))
  "Registered operation specification."
  domain name key label read-args apply describe from-json schema validate
  queueable order-last extra resume)

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
  directory migrate-plist after-steps-added sanitize-target
  target-directory regenerate recover-step after-reset)

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

(defvar hitl-plan--active-plans nil
  "Dynamically bound list of (ID . DIRECTORY) keys currently executing.")

(defun hitl-plan--active-p (plan)
  "Return non-nil when PLAN's ID and store are executing in this Emacs."
  (member (cons (hitl-plan-id plan) (hitl-plan--directory plan))
          hitl-plan--active-plans))

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
          directory migrate-plist after-steps-added sanitize-target
          target-directory regenerate recover-step after-reset)
  "Register (or replace) target domain NAME and return its struct.
The keyword arguments LABEL (default: the symbol name), RESOLVE-TARGET,
CAPTURE-TARGET, FORMAT-TARGET, VISIT-TARGET, BUILD-BATCH, REBUILD-BATCH,
LOCK-BATCH, UNLOCK-BATCH, DRIVER, INJECT-NOTE, UNRESOLVED-STATUS,
UNRESOLVED-MESSAGE, ACCEPT-BUFFER, DIRECTORY, MIGRATE-PLIST,
AFTER-STEPS-ADDED and SANITIZE-TARGET populate the like-named
`hitl-plan-domain' slots.
TARGET-DIRECTORY takes a target and returns its working directory.
REGENERATE takes a plan and returns fresh targets for its operations.
RECOVER-STEP takes a plan and failed step and dispatches a recovery action.
AFTER-RESET takes a plan and its reset steps to clear domain-specific state.
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
            :sanitize-target sanitize-target
            :target-directory target-directory :regenerate regenerate
            :recover-step recover-step :after-reset after-reset)
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
            from-json schema order-last extra resume)
  "Register operation NAME under DOMAIN (a symbol, or t for any domain).
KEY is an optional character that must be unique within DOMAIN.  APPLY is
called with the operation's args (and may return `hitl-plan-skip' to mark
the step skipped); READ-ARGS interactively returns an arg
list; VALIDATE is called with the args and returns a problem string or nil;
DESCRIBE receives the args and returns a string; FROM-JSON maps a JSON
alist to an arg list; SCHEMA documents arguments; QUEUEABLE marks
operations safe to run without prompting; ORDER-LAST runs the operation
after all others; EXTRA is an arbitrary plist of domain metadata, read
with `hitl-plan-op-spec-extra'.  RESUME receives a saved response followed
by the operation's args when a suspended ordered step resumes; APPLY is
not called again.  Returns the `hitl-plan-op-spec'."
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
            :queueable queueable :order-last order-last :extra extra :resume resume)
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
          :execution (hitl-plan-execution plan)
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
                                 :request (hitl-plan-step-request step)
                                 :response (hitl-plan-step-response step)
                                 :history (hitl-plan-step-history step)
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
                             :request (plist-get pl :request)
                             :response (plist-get pl :response)
                             :history (plist-get pl :history)
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
     :execution (or (plist-get data :execution) 'bulk)
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
         (print-circle t)
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
              (when (and (eq (hitl-plan-status plan) 'running)
                         (not (hitl-plan--active-p plan)))
                (setf (hitl-plan-status plan) 'paused))
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
          metadata (save t) id (execution 'bulk))
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
save.  EXECUTION is `bulk' (operation order, using domain drivers) or
`steps' (strict step order, pausing at failures and suspended steps).
Ordered execution does not use the domain's batch driver."
  (unless (memq execution '(bulk steps))
    (user-error "Unknown plan execution mode: %S" execution))
  (unless (hitl-plan-get-domain domain)
    (user-error "Unknown hitl-plan domain: %S" domain))
  (when (and id (or (gethash id hitl-plan--store)
                    (file-exists-p
                     (hitl-plan-file id (file-name-as-directory
                                         (expand-file-name
                                          (or directory (hitl-plan-domain-store domain)
                                              hitl-plan-directory)))))))
    (user-error "Plan %s already exists" id))
  (let* ((norm-ops (seq-map #'hitl-plan--normalize-op ops))
         (norm-ops (if (eq execution 'steps)
                       norm-ops
                     (hitl-plan-order-ops domain norm-ops)))
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
                :metadata metadata :execution execution)))
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
\(when any) after they are appended and before the save.  Ordered plans
append through `hitl-plan-insert-steps', preserving existing step order."
  (if (eq (hitl-plan-execution plan) 'steps)
      (progn
        (hitl-plan-insert-steps
         plan nil
         (seq-map (lambda (target) (list :op op :target target))
                  (or targets
                      (seq-uniq (seq-map #'hitl-plan-step-target
                                        (hitl-plan-steps plan)) #'equal))))
        plan)
    (let* ((op (hitl-plan--normalize-op op))
           (targets (or targets
                        (seq-uniq (seq-map #'hitl-plan-step-target
                                           (hitl-plan-steps plan)) #'equal)))
           (max-id (seq-reduce (lambda (value step)
                                (max value (hitl-plan-step-id step)))
                              (hitl-plan-steps plan) 0))
           (new-steps (seq-map (lambda (target)
                                 (setq max-id (1+ max-id))
                                 (hitl-plan--make-step max-id target op))
                               targets))
           (all-ops (hitl-plan-order-ops (hitl-plan-domain plan)
                                         (append (hitl-plan-ops plan) (list op))))
           (all-steps (append (hitl-plan-steps plan) new-steps)))
      (setf (hitl-plan-ops plan) all-ops)
      (setf (hitl-plan-steps plan)
            (seq-mapcat
             (lambda (operation)
               (seq-filter (lambda (step) (eq (hitl-plan-step-op step) operation))
                           all-steps))
             all-ops))
      (when-let* ((new-steps new-steps)
                  (hook (hitl-plan-domain-after-steps-added
                         (hitl-plan-get-domain (hitl-plan-domain plan)))))
        (funcall hook plan new-steps))
      (when (and (eq (hitl-plan-status plan) 'complete)
                 (seq-some (lambda (step) (eq (hitl-plan-step-status step) 'pending))
                           new-steps))
        (setf (hitl-plan-status plan) 'paused))
      (hitl-plan-save plan))))

(defun hitl-plan-reset (plan &optional statuses)
  "Reset PLAN steps to pending and save, returning PLAN.
Only steps in STATUSES are reset when non-nil.  Otherwise reset all steps,
including successful ones: applying them again may repeat side effects.
Domain-specific state is cleared by the domain's :after-reset callback.
Running and discarded plans cannot be reset.  Agent approval is revoked."
  (when (memq (hitl-plan-status plan) '(running discarded))
    (user-error "Cannot reset a %s plan" (hitl-plan-status plan)))
  (let ((steps (seq-filter
                (lambda (step)
                  (or (null statuses) (memq (hitl-plan-step-status step) statuses)))
                (hitl-plan-steps plan))))
    (when steps
      (seq-do (lambda (step)
                (setf (hitl-plan-step-status step) 'pending
                      (hitl-plan-step-message step) nil
                      (hitl-plan-step-request step) nil
                      (hitl-plan-step-response step) nil)
                (hitl-plan--record-event plan step 'reset))
              steps)
      (setf (hitl-plan-status plan) 'draft
            (hitl-plan-metadata plan)
            (plist-put (hitl-plan-metadata plan) :approved nil))
      (when-let* ((callback (hitl-plan-domain-after-reset
                            (hitl-plan-get-domain (hitl-plan-domain plan)))))
        (funcall callback plan steps))
      (hitl-plan-save plan)))
  plan)

(defun hitl-plan-regenerate (plan)
  "Rebuild PLAN from fresh domain targets, reset its steps and save it.
The domain's :regenerate callback receives PLAN and returns a target list.
Operations, identity, scope and origin are retained.  Old step progress is
discarded, so execution may repeat side effects.  Return PLAN."
  (when (memq (hitl-plan-status plan) '(running discarded))
    (user-error "Cannot regenerate a %s plan" (hitl-plan-status plan)))
  (let* ((domain (hitl-plan-get-domain (hitl-plan-domain plan)))
         (callback (hitl-plan-domain-regenerate domain)))
    (unless callback
      (user-error "Domain %s cannot regenerate plans" (hitl-plan-domain plan)))
    (let ((targets (funcall callback plan))
          (step-id 0))
      (unless (proper-list-p targets)
        (error "Regenerated targets must be a list"))
      (setf (hitl-plan-steps plan)
            (seq-mapcat
             (lambda (op)
               (seq-map (lambda (target)
                          (setq step-id (1+ step-id))
                          (hitl-plan--make-step step-id target op))
                        targets))
             (hitl-plan-ops plan))
            (hitl-plan-status plan) 'draft
            (hitl-plan-metadata plan)
            (plist-put (hitl-plan-metadata plan) :approved nil))
      (when-let* ((hook (hitl-plan-domain-after-steps-added domain))
                  (steps (hitl-plan-steps plan)))
        (funcall hook plan steps))
      (when-let* ((hook (hitl-plan-domain-after-reset domain)))
        (funcall hook plan (hitl-plan-steps plan)))
      (hitl-plan-save plan))))

(defun hitl-plan-step-directory (plan step)
  "Return the working directory of STEP in PLAN, or nil.
Use the domain's :target-directory callback, falling back to a plist
target's :directory or the parent directory of its :file."
  (let* ((target (hitl-plan-step-target step))
         (domain (hitl-plan-get-domain (hitl-plan-domain plan)))
         (callback (and domain (hitl-plan-domain-target-directory domain)))
         (directory
          (or (and callback (funcall callback target))
              (and (consp target) (plistp target)
                   (or (plist-get target :directory)
                       (when-let* ((file (plist-get target :file)))
                         (file-name-directory (expand-file-name file))))))))
    (when (stringp directory)
      (file-name-as-directory (expand-file-name directory)))))

;;; Execution

(define-error 'hitl-plan-suspension "Plan step suspended")

(defun hitl-plan--flow-step (plan step-id)
  "Validate ordered PLAN and return its step identified by STEP-ID."
  (unless (and (hitl-plan-p plan) (eq (hitl-plan-execution plan) 'steps))
    (user-error "This operation requires an ordered plan"))
  (when (eq (hitl-plan-status plan) 'discarded)
    (user-error "Plan %s is discarded" (hitl-plan-id plan)))
  (when (and (hitl-plan--active-p plan)
             (not (eq plan hitl-plan-current-plan)))
    (user-error "Plan %s is running" (hitl-plan-id plan)))
  (or (seq-find (lambda (step) (equal (hitl-plan-step-id step) step-id))
                (hitl-plan-steps plan))
      (user-error "Unknown step %S in plan %s" step-id (hitl-plan-id plan))))

(defun hitl-plan--record-event (plan step event &optional phase)
  "Append an EVENT snapshot for PHASE to ordered PLAN's STEP history."
  (when (eq (hitl-plan-execution plan) 'steps)
    (setf (hitl-plan-step-history step)
          (append (hitl-plan-step-history step)
                  (list (list :event event :time (hitl-plan--now) :phase phase
                              :request (hitl-plan--check-data
                                        (hitl-plan-step-request step))
                              :response (hitl-plan--check-data
                                         (hitl-plan-step-response step))
                              :message (hitl-plan-step-message step)))))))

(defun hitl-plan--save-transition (plan)
  "Save PLAN unless its executing handler will save the transition.
Mutations within a handler are saved together with its resulting status."
  (unless (eq plan hitl-plan-current-plan)
    (hitl-plan-save plan))
  plan)

(defun hitl-plan--check-data (value)
  "Return a readable copy of VALUE, rejecting unsavable suspension data."
  (condition-case nil
      (let ((print-circle t)
            (print-length nil)
            (print-level nil))
        (read (prin1-to-string value)))
    (error (user-error "Plan suspension data must be readable Lisp data"))))

(defun hitl-plan-suspend (request)
  "Suspend the current ordered step with data-only REQUEST.
Call from an operation's APPLY or RESUME function.  This does not return.
REQUEST describes a confirmation, choice, external task, or other event.
The executor saves it and stops before the next step.  Resolve the wait
with `hitl-plan-resolve-step'; a later execution calls the operation's
registered RESUME function, never APPLY.  Handlers must be registered
again after restarting Emacs; no executable code is saved in the plan."
  (unless (and hitl-plan-current-plan hitl-plan-current-step
               (eq (hitl-plan-execution hitl-plan-current-plan) 'steps))
    (user-error "Only an executing ordered step can suspend"))
  (unless (hitl-plan-op-spec-resume
           (hitl-plan-get-op (hitl-plan-domain hitl-plan-current-plan)
                             (hitl-plan-op-name hitl-plan-current-op)))
    (user-error "Operation %s has no resume handler"
                (hitl-plan-op-name hitl-plan-current-op)))
  (hitl-plan--check-data request)
  (signal 'hitl-plan-suspension (list request)))

(defun hitl-plan-resolve-step (plan step-id response)
  "Record RESPONSE for waiting STEP-ID of ordered PLAN and save PLAN.
The step becomes `ready'.  No handler or subsequent work is executed.
A nil RESPONSE is a valid answer; resolving a step twice is an error."
  (let ((step (hitl-plan--flow-step plan step-id)))
    (unless (eq (hitl-plan-step-status step) 'waiting)
      (user-error "Step %s is not waiting" step-id))
    (hitl-plan--check-data response)
    (setf (hitl-plan-step-response step) response
          (hitl-plan-step-status step) 'ready)
    (hitl-plan--record-event plan step 'resolved)
    (hitl-plan--save-transition plan)))

(defun hitl-plan-insert-steps (plan before steps)
  "Insert STEPS before step ID BEFORE in ordered PLAN, then save PLAN.
Nil BEFORE appends.  Each entry is a plist with :op (accepted by
`hitl-plan-create') and :target.  Return the newly created step objects.
IDs are fresh and stable; execution follows the resulting step order,
including insertions made by running handlers.  Domain batch drivers and
:order-last do not reorder these steps.  Completed plans reopen paused.
The domain's :after-steps-added callback runs for the new steps.
Restricted callers may not mutate the executable structure of a plan."
  (unless (and (hitl-plan-p plan) (eq (hitl-plan-execution plan) 'steps))
    (user-error "Insertion requires an ordered plan"))
  (when (or (hitl-plan-restricted-p) (hitl-plan-origin-mcp-p plan))
    (user-error "Restricted plans cannot insert executable steps"))
  (when (or (eq (hitl-plan-status plan) 'discarded)
            (and (hitl-plan--active-p plan)
                 (not (eq plan hitl-plan-current-plan))))
    (user-error "Cannot insert into a %s plan" (hitl-plan-status plan)))
  (unless (proper-list-p steps)
    (user-error "Steps must be a list"))
  (let* ((anchor (and before (hitl-plan--flow-step plan before)))
         (max-id (seq-reduce (lambda (value step)
                              (max value (hitl-plan-step-id step)))
                            (hitl-plan-steps plan) 0))
         (new-steps
          (seq-map
           (lambda (entry)
             (unless (and (plistp entry) (plist-member entry :target)
                          (plist-get entry :op))
               (user-error "Step needs :target and :op: %S" entry))
             (let ((op (hitl-plan--normalize-op (plist-get entry :op))))
               (unless (hitl-plan-get-op (hitl-plan-domain plan)
                                         (hitl-plan-op-name op))
                 (user-error "Unknown operation: %s" (hitl-plan-op-name op)))
               (setq max-id (1+ max-id))
               (hitl-plan--make-step max-id (plist-get entry :target) op)))
           steps))
         (position (if anchor
                       (seq-position (hitl-plan-steps plan) anchor #'eq)
                     (length (hitl-plan-steps plan)))))
    (when new-steps
      (setf (hitl-plan-steps plan)
            (append (seq-take (hitl-plan-steps plan) position) new-steps
                    (nthcdr position (hitl-plan-steps plan)))
            (hitl-plan-ops plan)
            (seq-uniq (append (hitl-plan-ops plan)
                              (seq-map #'hitl-plan-step-op new-steps)) #'eq)
            (hitl-plan-metadata plan)
            (plist-put (hitl-plan-metadata plan) :approved nil))
      (when (eq (hitl-plan-status plan) 'complete)
        (setf (hitl-plan-status plan) 'paused))
      (when-let* ((callback (hitl-plan-domain-after-steps-added
                            (hitl-plan-get-domain (hitl-plan-domain plan)))))
        (funcall callback plan new-steps))
      (hitl-plan--save-transition plan))
    new-steps))

(defun hitl-plan-retry-step (plan step-id)
  "Retry failed STEP-ID of ordered PLAN, retaining its failure history.
A failed RESUME handler retries RESUME with its saved response, rather
than repeating APPLY.  The domain's unresolved status is also retryable.
Domain-specific :after-reset state is cleared."
  (let ((step (hitl-plan--flow-step plan step-id)))
    (unless (memq (hitl-plan-step-status step)
                  (list 'failed
                        (or (hitl-plan-domain-unresolved-status
                             (hitl-plan-get-domain (hitl-plan-domain plan)))
                            'failed)))
      (user-error "Step %s has not failed" step-id))
    (setf (hitl-plan-step-status step)
          (if (eq (plist-get (car (last (hitl-plan-step-history step)))
                              :phase) 'resume)
              'ready
            'pending)
          (hitl-plan-step-message step) nil)
    (hitl-plan--record-event plan step 'retry)
    (when-let* ((callback (hitl-plan-domain-after-reset
                          (hitl-plan-get-domain (hitl-plan-domain plan)))))
      (funcall callback plan (list step)))
    (hitl-plan--save-transition plan)))

(defun hitl-plan-skip-step (plan step-id &optional message)
  "Skip unfinished STEP-ID in ordered PLAN with optional MESSAGE.
Use this in a resume handler to deselect a branch.  Completed steps and
the step currently applying cannot be skipped.  History is retained."
  (let ((step (hitl-plan--flow-step plan step-id)))
    (when (or (memq (hitl-plan-step-status step) '(done skipped))
              (eq step hitl-plan-current-step))
      (user-error "Cannot skip step %s" step-id))
    (setf (hitl-plan-step-status step) 'skipped
          (hitl-plan-step-message step) message)
    (hitl-plan--record-event plan step 'skipped)
    (hitl-plan--save-transition plan)))

(defun hitl-plan-pause (plan)
  "Pause ordered PLAN after the current handler returns normally.
Unlike `hitl-plan-suspend', the current step completes.  A later explicit
execution continues at the next unfinished step."
  (unless (and (eq plan hitl-plan-current-plan)
               (eq (hitl-plan-execution plan) 'steps))
    (user-error "Only the executing ordered plan can pause"))
  (setf (hitl-plan-status plan) 'paused)
  plan)

(defun hitl-plan-skip (&optional reason)
  "Return a skip marker for an operation `:apply' function.
When `:apply' returns this marker, `hitl-plan-run-step' marks the step
`skipped' with REASON (a string, or nil) as its message instead of `done',
and skips `inject-note' and `hitl-plan-step-functions'."
  (cons :hitl-plan-skip reason))

(defun hitl-plan-skip-p (value)
  "Return non-nil if VALUE is a skip marker from `hitl-plan-skip'."
  (eq (car-safe value) :hitl-plan-skip))

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
  "Run pending or ready STEP of PLAN under OP, quit-safely, and save PLAN.
RESOLVED is the already-resolved target; when nil the domain resolves it.
Domain drivers call this for each target.  On success STEP becomes `done'
and `hitl-plan-step-functions' runs; if `:apply' returns a marker from
`hitl-plan-skip', STEP becomes `skipped' with its reason as message and
notes/hooks are skipped; errors mark it `failed'.  If \\[keyboard-quit]
interrupts the application the step stays `pending' and `quit-flag' is left
set for the caller.  Returns the step's resulting status, or nil when STEP
was not runnable.  A ready ordered step calls RESUME with its response
and the operation's args.  Suspension saves a `waiting' step.  Quitting
a ready step leaves it ready, so only RESUME is repeated."
  (when (or (eq (hitl-plan-step-status step) 'pending)
            (and (eq (hitl-plan-execution plan) 'steps)
                 (eq (hitl-plan-step-status step) 'ready)))
    (let* ((dom-name (hitl-plan-domain plan))
           (domain (hitl-plan-get-domain dom-name))
           (spec (hitl-plan-get-op dom-name (hitl-plan-op-name op)))
           (ours (null resolved))
           (resolved (or resolved (and spec (hitl-plan--resolve plan step))))
           (hitl-plan-current-plan plan)
           (hitl-plan-current-step step)
           (hitl-plan-current-op op)
           (hitl-plan-current-target resolved)
           (resuming (eq (hitl-plan-step-status step) 'ready))
           (inhibit-quit t)
           (completed nil)
           (apply-res nil))
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
                     (setq apply-res
                           (if resuming
                               (progn
                                 (unless (hitl-plan-op-spec-resume spec)
                                   (error "Operation %s has no resume handler"
                                          (hitl-plan-op-name op)))
                                 (apply (hitl-plan-op-spec-resume spec)
                                        (hitl-plan-step-response step) args))
                             (apply (hitl-plan-op-spec-apply spec) args)))
                     (unless (hitl-plan-skip-p apply-res)
                       (when-let* ((inject (and domain (hitl-plan-domain-inject-note domain)))
                                   (text (pcase (hitl-plan-log-notes plan)
                                           ('one (hitl-plan-note plan))
                                           ('each (hitl-plan-step-note step))))
                                   ((not (string-empty-p text))))
                         (funcall inject text step plan))))))
                (setq completed t))
              (when completed
                (if (hitl-plan-skip-p apply-res)
                    (let ((reason (cdr apply-res)))
                      (setf (hitl-plan-step-status step) 'skipped
                            (hitl-plan-step-message step)
                            (cond ((null reason) nil)
                                  ((stringp reason) (unless (string-empty-p reason) reason))
                                  (t (format "%s" reason)))))
                  (setf (hitl-plan-step-status step) 'done
                        (hitl-plan-step-message step) nil)
                  (condition-case herr
                      (hitl-plan--call-at
                       resolved
                       (lambda ()
                         (run-hook-with-args 'hitl-plan-step-functions step plan)))
                    (error
                     (setf (hitl-plan-step-message step)
                           (format "step hook error: %s" (error-message-string herr))))))))
          (hitl-plan-suspension
           (setf (hitl-plan-step-status step) 'waiting
                 (hitl-plan-step-request step) (cadr err)
                 (hitl-plan-step-response step) nil
                 (hitl-plan-step-message step) nil))
          (error (hitl-plan--fail step (error-message-string err))))))
      (unless (memq (hitl-plan-step-status step) '(pending ready))
        (hitl-plan--record-event plan step (hitl-plan-step-status step)
                                 (if resuming 'resume 'apply)))
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

(defun hitl-plan--run-op (plan op domain &optional limit)
  "Run pending steps of OP in PLAN via DOMAIN.  Return non-nil if quit.
When LIMIT is a positive integer, run at most that many steps.
A quit inside the domain driver is absorbed (and returned as non-nil); one
inside the batch rebuild propagates to `hitl-plan-execute'."
  (let ((driver (and domain (hitl-plan-domain-driver domain))))
    (if driver
        (let* ((batch (hitl-plan-rebuild-batch plan op))
               (resolved (if limit
                             (seq-take (cdr batch) limit)
                           (cdr batch)))
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
            (ran 0)
            (inhibit-quit t))
        (dolist (step (hitl-plan--pending-steps plan op))
          (unless (or quitted (and limit (>= ran limit)))
            (hitl-plan-run-step plan step op)
            (setq ran (1+ ran))
            (when quit-flag
              (setq quit-flag nil quitted t))))
        quitted))))

(defun hitl-plan--run-ordered (plan limit)
  "Run PLAN in step order, stopping at a blocker or after LIMIT attempts.
Return non-nil on quit.  Rescan after each step to honor inserted work."
  (let ((attempted 0)
        (quitted nil)
        (inhibit-quit t)
        step)
    (while (and (eq (hitl-plan-status plan) 'running)
                (or (null limit) (< attempted limit))
                (setq step (seq-find
                            (lambda (candidate)
                              (not (memq (hitl-plan-step-status candidate)
                                         '(done skipped))))
                            (hitl-plan-steps plan)))
                (memq (hitl-plan-step-status step) '(pending ready))
                (not quitted))
      (hitl-plan-run-step plan step (hitl-plan-step-op step))
      (setq attempted (1+ attempted))
      (when quit-flag
        (setq quit-flag nil)
        (setq quitted t)))
    quitted))

(defun hitl-plan-execute (plan &optional only-op limit)
  "Execute pending steps of PLAN (or only those of ONLY-OP), return PLAN.
When LIMIT is a positive integer, stop once that many pending steps have
been attempted (unresolvable steps count) and leave the plan `paused'
with the rest still pending, so it can be stepped by hand and resumed.
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
not run.  For ordered plans (`steps' execution), LIMIT counts apply or
resume attempts; failures and waits block subsequent steps.  ONLY-OP is
not supported, since it could bypass a gate.  Domain drivers are unused."
  (unless (memq (hitl-plan-execution plan) '(bulk steps))
    (user-error "Unknown plan execution mode: %S" (hitl-plan-execution plan)))
  (when (and limit (not (and (integerp limit) (> limit 0))))
    (user-error "Execution limit must be a positive integer"))
  (when (and only-op (eq (hitl-plan-execution plan) 'steps))
    (user-error "Cannot filter operations in an ordered plan"))
  (when (hitl-plan--active-p plan)
    (user-error "Plan %s is already running" (hitl-plan-id plan)))
  (pcase (hitl-plan-status plan)
    ('discarded (user-error "Plan %s is discarded" (hitl-plan-id plan)))
    ('complete (user-error "Plan %s is already complete" (hitl-plan-id plan))))
  (let* ((hitl-plan--active-plans
          (cons (cons (hitl-plan-id plan) (hitl-plan--directory plan))
                hitl-plan--active-plans))
         (hitl-plan-restricted (or hitl-plan-restricted (hitl-plan-origin-mcp-p plan)))
         (domain (hitl-plan-get-domain (hitl-plan-domain plan)))
         (before-done (seq-filter (lambda (s) (eq (hitl-plan-step-status s) 'done))
                                  (hitl-plan-steps plan)))
         (ops (if only-op
                  (list only-op)
                (hitl-plan-order-ops (hitl-plan-domain plan) (hitl-plan-ops plan))))
         (quitted nil)
         (pending-count (lambda ()
                          (length (seq-filter
                                   (lambda (s) (eq (hitl-plan-step-status s) 'pending))
                                   (hitl-plan-steps plan)))))
         (pending-before (funcall pending-count)))
    (setf (hitl-plan-status plan) 'running)
    (hitl-plan-save plan)
    ;; An error from a driver or batch rebuild, or a quit during the rebuild
    ;; (quits inside a driver are handled by `hitl-plan--run-op'), must not
    ;; leave the plan `running' on disk: pause it, save, and re-signal.
    (condition-case err
        (if (eq (hitl-plan-execution plan) 'steps)
            (setq quitted (hitl-plan--run-ordered plan limit))
          (seq-do
           (lambda (op)
             (let ((remaining (and limit
                                   (- limit (- pending-before (funcall pending-count))))))
               (unless (or quitted (and remaining (<= remaining 0)))
                 (setq quitted (hitl-plan--run-op plan op domain remaining)))))
           ops))
      ((error quit)
       (let ((inhibit-quit t))
         (setf (hitl-plan-status plan) 'paused)
         (hitl-plan-save plan))
       (signal (car err) (cdr err))))
    (when quitted
      (setf (hitl-plan-status plan) 'paused)
      (hitl-plan-save plan))
    (hitl-plan--run-after-apply plan before-done)
    (unless (or quitted (eq (hitl-plan-status plan) 'discarded))
      (setf (hitl-plan-status plan)
            (if (or (eq (hitl-plan-status plan) 'paused)
                    (seq-some
                     (lambda (step)
                       (if (eq (hitl-plan-execution plan) 'steps)
                           (not (memq (hitl-plan-step-status step) '(done skipped)))
                         (eq (hitl-plan-step-status step) 'pending)))
                     (hitl-plan-steps plan)))
                'paused
              'complete))
      (hitl-plan-save plan))
    plan))

(defun hitl-plan-execute-one (plan)
  "Hand-crank PLAN: execute exactly one pending step, then stay paused.
Returns PLAN.  Signals `user-error' when no step is pending.  Saved state
is the ordinary paused state, so `hitl-plan-execute' or another call
continues where this one stopped."
  (unless (seq-some (lambda (s) (memq (hitl-plan-step-status s) '(pending ready)))
                    (hitl-plan-steps plan))
    (user-error "Plan %s has no pending step" (hitl-plan-id plan)))
  (hitl-plan-execute plan nil 1))

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
          :execution (symbol-name (hitl-plan-execution plan))
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
