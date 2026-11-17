;;; hitl-plan-mcp.el --- MCP tools for hitl-plan bulk action plans -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0"))
;; Keywords: tools, mcp, hitl, agent, ai
;; URL: https://github.com/tychoish/hitl

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Exposes `hitl-plan' resumable bulk action plans on the `hitl' MCP
;; service: plan_domains, plan_ops, plan_create, plan_list, plan_status,
;; plan_discard and plan_execute.
;;
;; Plans created here carry the metadata `:origin mcp' and are untrusted
;; (see the "Security model" section of README.org).  The tools type check
;; and size cap arguments, never steal focus when displaying a plan, and
;; discard only plans of mcp origin.  The user must approve running such a
;; plan, either through `hitl-plan-agent-may-apply' or through an explicit
;; confirmation in the review buffer.
;;
;; MCP handlers never prompt.  Only operations registered `:queueable' may
;; be planned or executed, and the `each' note-logging mode is rejected.
;; plan_execute applies a plan only when `hitl-plan-agent-may-apply' is
;; non-nil, and otherwise opens the review buffer for the user.  Handlers
;; report failures as (:error MESSAGE), like the other hitl tools.
;;
;; Each tool is a thin wrapper over a plain function that takes the tool's
;; arguments as keywords and returns the tool's result plist:
;; `hitl-plan-mcp-domains', `-ops', `-create', `-list', `-status',
;; `-discard' and `-execute'.  Other packages (such as orgx) wrap those
;; functions instead of reaching into mcpkit.  Arguments are JSON-shaped:
;; `:null' counts as absent, and keys use the tool's names (`:plan_id',
;; `:log_notes', ...).
;;
;; `plan_create' may build targets from a domain's :build-batch, which
;; returns (BUFFER . TARGETS).  The tool kills a BUFFER it created once the
;; targets are extracted, so no batch buffers are left behind.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'mcpkit)
(require 'hitl-plan)
(require 'hitl-plan-ui)

(defcustom hitl-plan-max-steps 1000
  "Maximum number of steps (targets times operations) of a plan made over MCP."
  :type 'integer
  :group 'hitl-plan)

(defcustom hitl-plan-max-string-length 4096
  "Maximum length of any string argument of `plan_create'.
Applies to the description, the note and every string inside targets,
scope and operation arguments."
  :type 'integer
  :group 'hitl-plan)

;; `hitl-plan-agent-may-apply' is defined in `hitl-plan'.

(unless (mcpkit-get-service 'hitl)
  (mcpkit-define-service 'hitl
    :description "Human-in-the-Loop Question and Approval Queue"))

;; Idempotent: `hitl-mcp' registers the same service, so loading either file
;; alone leaves the service autostarted.
(mcpkit-register-autostart 'hitl :on-collision 'namespace)

(defun hitl-plan-mcp-denull (obj)
  "Return OBJ with every `:null' (JSON null) replaced by nil, recursively.
Lists are walked iteratively, so arbitrarily long lists are fine."
  (cond ((eq obj :null) nil)
        ((consp obj)
         (let (acc (tail obj))
           (while (consp tail)
             (push (hitl-plan-mcp-denull (car tail)) acc)
             (setq tail (cdr tail)))
           (let ((res (nreverse acc)))
             (when tail (setcdr (last res) (hitl-plan-mcp-denull tail)))
             res)))
        ((vectorp obj) (vconcat (mapcar #'hitl-plan-mcp-denull obj)))
        (t obj)))

(defmacro hitl-plan-mcp--guard (&rest body)
  "Run BODY, converting errors into an (:error MESSAGE) plist.
Inside BODY the variable `args' (the caller's argument plist) is rebound
with every `:null' replaced by nil, so handlers never see JSON nulls."
  (declare (indent 0))
  `(condition-case err
       (let ((args (hitl-plan-mcp-denull args)))
         (ignore args)
         ,@body)
     (error (list :error (error-message-string err)))))

(defun hitl-plan-mcp--domain (args)
  "Return the domain symbol named by ARGS' `:domain', or signal an error."
  (let ((name (or (plist-get args :domain) "file-target")))
    (unless (hitl-plan-get-domain name)
      (error "Unknown plan domain: %s" name))
    (if (stringp name) (intern name) name)))

(defun hitl-plan-mcp--allowed-directories ()
  "Return the plan stores MCP callers may name: the default and domain stores."
  (mapcar (lambda (d) (file-name-as-directory (expand-file-name d)))
          (delq nil (cons hitl-plan-directory
                          (mapcar #'hitl-plan-domain-store
                                  (hitl-plan-registered-domains))))))

(defun hitl-plan-mcp--plan-allowed-p (plan)
  "Return non-nil if PLAN is stored in one of the allowed plan directories."
  (member (hitl-plan--directory plan) (hitl-plan-mcp--allowed-directories)))

(defun hitl-plan-mcp--directory (args)
  "Return ARGS' validated `:directory', or nil when absent.
The directory must be `hitl-plan-directory' or a registered domain's store
\(see `hitl-plan-domain-store'); MCP callers may not name arbitrary paths."
  (let ((dir (plist-get args :directory)))
    (when dir
      (unless (stringp dir)
        (error "Invalid directory: %S" dir))
      (let ((full (file-name-as-directory (expand-file-name dir))))
        (unless (member full (hitl-plan-mcp--allowed-directories))
          (error "Directory not allowed: %s" dir))
        full))))

(defun hitl-plan-mcp--plan-id (args)
  "Return ARGS' `:plan_id' if it is a plain ID, else signal an error.
Path-like IDs (containing a directory separator, or ending in `.eld') are
rejected so MCP callers cannot load arbitrary files."
  (let ((id (plist-get args :plan_id)))
    (unless (and (stringp id)
                 (string-match-p "\\`[A-Za-z0-9][A-Za-z0-9_.-]*\\'" id)
                 (not (string-suffix-p ".eld" id)))
      (error "Invalid plan_id: %S" id))
    id))

(defun hitl-plan-mcp--plan (args)
  "Return the plan named by ARGS' `:plan_id', or signal an error.
ARGS' optional `:directory' and `:domain' say where to look for it; a
domain with a :directory slot supplies that store."
  (let* ((id (hitl-plan-mcp--plan-id args))
         (plan (hitl-plan-get id (hitl-plan-mcp--directory args) (plist-get args :domain))))
    ;; The in-memory cache is keyed by ID alone and may hold plans loaded
    ;; from anywhere; only plans in the allowed stores are reachable.
    (if (and plan (hitl-plan-mcp--plan-allowed-p plan))
        plan
      (error "Unknown plan: %s" id))))

(defun hitl-plan-mcp-show-p (args)
  "Return non-nil unless plist ARGS explicitly passes a false `:show'.
JSON false may arrive as nil or `:json-false'; an absent key means show."
  (let ((cell (plist-member args :show)))
    (or (null cell)
        (not (memq (cadr cell) '(nil :json-false))))))

(defun hitl-plan-mcp--parse-log-notes (raw)
  "Convert RAW log_notes into `ignore' or `one'; reject `each'."
  (let ((sym (cond ((null raw) hitl-plan-log-notes)
                   ((symbolp raw) raw)
                   ((stringp raw) (intern (downcase raw)))
                   (t (error "Invalid log_notes value: %S" raw)))))
    (when (eq sym 'each)
      (error "Log_notes mode `each' is not supported over MCP"))
    (unless (memq sym '(ignore one))
      (error "Unsupported log_notes mode: %s" sym))
    sym))

(defun hitl-plan-mcp--json-get (obj &rest keys)
  "Return the first non-nil value in OBJ (plist, alist or hash) for KEYS."
  (seq-some
   (lambda (k)
     (let ((name (symbol-name k)))
       (cond
        ((hash-table-p obj) (or (gethash name obj) (gethash k obj)))
        ((and (consp obj) (keywordp (car obj)))
         (plist-get obj (intern (concat ":" name))))
        ((consp obj)
         (cdr (or (assq k obj) (assoc name obj)))))))
   keys))

;;; Argument validation

(defun hitl-plan-mcp--plist-p (obj)
  "Return non-nil if OBJ is a proper plist with keyword keys (maybe empty)."
  (and (proper-list-p obj)
       (zerop (% (length obj) 2))
       (cl-loop for (k) on obj by #'cddr always (keywordp k))))

(defun hitl-plan-mcp--list-of (what obj pred)
  "Return OBJ (a list or vector) as a list, each element satisfying PRED.
WHAT names the argument for the error signalled otherwise."
  (unless (and (or (proper-list-p obj) (vectorp obj)) (not (stringp obj)))
    (error "`%s' must be an array" what))
  (let ((items (append obj nil)))
    (dolist (e items)
      (unless (funcall pred e)
        (error "`%s' must be an array of objects" what)))
    items))

(defun hitl-plan-mcp--object-p (obj)
  "Return non-nil if OBJ looks like a decoded JSON object."
  (or (and (consp obj) (hitl-plan-mcp--plist-p obj))
      (hash-table-p obj)
      (and (consp obj) (proper-list-p obj)
           (cl-every (lambda (c) (and (consp c) (or (symbolp (car c)) (stringp (car c))))) obj))))

(defun hitl-plan-mcp--check-string (what str)
  "Signal an error if STR (named WHAT) is longer than the limit."
  (when (> (length str) hitl-plan-max-string-length)
    (error "%s: string too long (%d characters, max %d)"
           what (length str) hitl-plan-max-string-length)))

(defun hitl-plan-mcp--check-strings (what obj)
  "Signal an error if any string within OBJ (named WHAT) is too long.
Walks lists, vectors and hash tables, to a bounded depth."
  (let ((stack (list (cons obj 0))) (nodes 0))
    (while stack
      (pcase-let ((`(,x . ,depth) (pop stack)))
        (when (> (cl-incf nodes) 2000000)
          (error "%s: too many values" what))
        (when (> depth 32)
          (error "%s: nested too deeply" what))
        (cond ((stringp x) (hitl-plan-mcp--check-string what x))
              ((consp x)
               (let ((tail x))
                 (while (consp tail)
                   (push (cons (car tail) (1+ depth)) stack)
                   (setq tail (cdr tail)))
                 (when tail (push (cons tail (1+ depth)) stack))))
              ((vectorp x)
               (mapc (lambda (e) (push (cons e (1+ depth)) stack)) x))
              ((hash-table-p x)
               (maphash (lambda (k v)
                          (push (cons k (1+ depth)) stack)
                          (push (cons v (1+ depth)) stack))
                        x)))))))

(defun hitl-plan-mcp--check-count (steps)
  "Signal an error when STEPS exceeds `hitl-plan-max-steps'."
  (when (> steps hitl-plan-max-steps)
    (error "Plan too large: %d steps (max %d)" steps hitl-plan-max-steps)))

(defun hitl-plan-mcp--check-optional-string (what value)
  "Signal an error unless VALUE (named WHAT) is nil or a short string."
  (when value
    (unless (stringp value) (error "`%s' must be a string" what))
    (hitl-plan-mcp--check-string what value)))

(defconst hitl-plan-mcp--forbidden-create-args '(:origin :status :metadata :id)
  "Keys that `plan_create' never accepts: they are set by hitl, not callers.")

(defun hitl-plan-mcp--validate-create-args (args)
  "Validate the `plan_create' plist ARGS and return (TARGETS OPS).
TARGETS is nil when absent (the domain builds them).  Signal an error for
forbidden or ill-typed arguments, or when the plan would exceed
`hitl-plan-max-steps' or `hitl-plan-max-string-length'."
  (dolist (k hitl-plan-mcp--forbidden-create-args)
    (when (plist-member args k)
      (error "Argument `%s' is not accepted" (substring (symbol-name k) 1))))
  (hitl-plan-mcp--check-optional-string "description" (plist-get args :description))
  (hitl-plan-mcp--check-optional-string "note" (plist-get args :note))
  (hitl-plan-mcp--check-optional-string "directory" (plist-get args :directory))
  (let ((scope (plist-get args :scope))
        (ops (hitl-plan-mcp--list-of "ops" (plist-get args :ops) #'hitl-plan-mcp--object-p))
        (targets (and (plist-get args :targets)
                      (hitl-plan-mcp--list-of "targets" (plist-get args :targets)
                                              (lambda (e) (and e (hitl-plan-mcp--plist-p e)))))))
    (when (and scope (not (hitl-plan-mcp--plist-p scope)))
      (error "`scope' must be an object"))
    (dolist (e ops)
      (let ((raw (hitl-plan-mcp--json-get e 'args)))
        (unless (or (null raw) (hash-table-p raw) (vectorp raw)
                    (and (consp raw) (proper-list-p raw)))
          (error "Operation `args' must be an object or an array"))))
    (hitl-plan-mcp--check-count (* (length targets) (max 1 (length ops))))
    (hitl-plan-mcp--check-strings "scope" scope)
    (hitl-plan-mcp--check-strings "targets" targets)
    (hitl-plan-mcp--check-strings "ops" ops)
    (list targets ops)))

(defmacro hitl-plan-mcp--cleaning-buffers (&rest body)
  "Run BODY, then kill the unmodified file buffers it created.
Building a plan must not leave buffers (and so file-local state) behind."
  (declare (indent 0))
  `(let ((before (buffer-list)))
     (unwind-protect (progn ,@body)
       (dolist (b (buffer-list))
         (when (and (not (memq b before)) (buffer-live-p b)
                    (buffer-file-name b) (not (buffer-modified-p b)))
           (let ((kill-buffer-query-functions nil))
             (kill-buffer b)))))))

(defun hitl-plan-mcp--show (plan)
  "Build PLAN's review buffer and show it without selecting it."
  (let ((buf (hitl-plan-review plan nil)))
    ;; Never reuse the selected window: the user may be typing in it.
    (display-buffer buf '(nil (inhibit-same-window . t)))
    (message "hitl: plan %s awaits your review in %s" (hitl-plan-id plan) (buffer-name buf))
    buf))

(defun hitl-plan-mcp--parse-op (domain entry)
  "Convert JSON ENTRY (`{op, args}') for DOMAIN into a `hitl-plan-op'."
  (let* ((raw-name (or (hitl-plan-mcp--json-get entry 'op 'name)
                       (error "Missing `op' in operation entry: %S" entry)))
         (name (if (symbolp raw-name) raw-name (intern (format "%s" raw-name))))
         (spec (hitl-plan-queueable-spec domain name))
         (raw (hitl-plan-mcp--json-get entry 'args))
         (parsed
          (cond
           ((null raw) nil)
           ((and (hitl-plan-op-spec-from-json spec)
                 (or (hash-table-p raw)
                     (and (consp raw) (or (keywordp (car raw)) (consp (car raw))))))
            (funcall (hitl-plan-op-spec-from-json spec) raw))
           ((vectorp raw) (append raw nil))
           ((listp raw) raw)
           (t (list raw)))))
    (when-let* ((validate (hitl-plan-op-spec-validate spec))
                (problem (apply validate parsed)))
      (error "Invalid args for `%s': %s" name problem))
    (hitl-plan-op--make :name name :args parsed)))

(defun hitl-plan-mcp--targets (domain args explicit)
  "Return the targets for ARGS in DOMAIN.
EXPLICIT is the validated caller-supplied target list, or nil to use the
domain's :build-batch on ARGS' `:scope'.  Explicit targets are passed
through the domain's :sanitize-target, which replaces each with a trusted
capture or signals an error.  A :build-batch may return (BUFFER . TARGETS),
\(nil . TARGETS) or a bare list of targets.  A BUFFER that did not exist
before the call is killed once the targets are extracted; a pre-existing
one is left alone."
  (let* ((dom (hitl-plan-get-domain domain))
         (build (hitl-plan-domain-build-batch dom))
         (sanitize (hitl-plan-domain-sanitize-target dom)))
    (cond
     (explicit (if sanitize (mapcar sanitize explicit) explicit))
     (build
      (let* ((before (buffer-list))
             (res (funcall build (plist-get args :scope)))
             (paired (and (consp res) (or (bufferp (car res)) (null (car res)))
                          (listp (cdr res))))
             (buf (and paired (car res))))
        (when (and buf (buffer-live-p buf) (not (memq buf before)))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf)))
        (if paired (cdr res) res)))
     (t (error "`targets' is required for domain %s" domain)))))

(defun hitl-plan-mcp--exclude (plan ids)
  "Mark PLAN's pending steps whose ID is in IDS (a sequence) skipped."
  (when-let* ((excluded (append ids nil)))
    (unless (cl-every #'integerp excluded)
      (error "`exclude' must be an array of integers"))
    (dolist (step (hitl-plan-steps plan))
      (when (and (memql (hitl-plan-step-id step) excluded)
                 (eq (hitl-plan-step-status step) 'pending))
        (setf (hitl-plan-step-status step) 'skipped
              (hitl-plan-step-message step) "excluded over MCP")))))

;;; Plain function API

(defun hitl-plan-mcp-domains (&rest args)
  "Return `plan_domains' result: the registered plan domains.  ARGS is ignored."
  (ignore args)
  (list :domains
        (vconcat
         (mapcar (lambda (d)
                   (list :name (symbol-name (hitl-plan-domain-name d))
                         :label (or (hitl-plan-domain-label d) :null)))
                 (sort (copy-sequence (hitl-plan-registered-domains))
                       (lambda (a b) (string< (symbol-name (hitl-plan-domain-name a))
                                              (symbol-name (hitl-plan-domain-name b)))))))))

(defun hitl-plan-mcp-ops (&rest args)
  "Return `plan_ops' result for ARGS (keyword `:domain'): a domain's operations."
  (hitl-plan-mcp--guard
    (let* ((domain (hitl-plan-mcp--domain args))
           (specs (sort (copy-sequence (hitl-plan-registered-ops domain))
                        (lambda (a b) (string< (symbol-name (hitl-plan-op-spec-name a))
                                               (symbol-name (hitl-plan-op-spec-name b)))))))
      (list :domain (symbol-name domain)
            :ops (vconcat
                  (mapcar
                   (lambda (spec)
                     (list :name (symbol-name (hitl-plan-op-spec-name spec))
                           :key (if-let* ((k (hitl-plan-op-spec-key spec)))
                                    (char-to-string k)
                                  :null)
                           :label (or (hitl-plan-op-spec-label spec) :null)
                           :queueable (if (hitl-plan-op-spec-queueable spec) t :json-false)
                           :order_last (if (hitl-plan-op-spec-order-last spec) t :json-false)
                           :schema (vconcat (hitl-plan-op-spec-schema spec))))
                   specs))))))

(defun hitl-plan-mcp-create (&rest args)
  "Create and save a plan from keyword ARGS; return its summary plist.
ARGS are the `plan_create' arguments: `:domain', `:targets', `:scope',
`:ops', `:description', `:log_notes', `:note', `:directory' and `:show'.
The arguments are validated and size capped (`hitl-plan-max-steps',
`hitl-plan-max-string-length') before anything is saved.  The plan is
marked `:origin mcp' (never taken from ARGS), so it is untrusted: see
`hitl-plan-origin-mcp-p'.  Errors are returned as (:error MESSAGE)."
  (hitl-plan-mcp--guard
    (let* ((hitl-plan-restricted t)
           (domain (hitl-plan-mcp--domain args))
           (checked (hitl-plan-mcp--validate-create-args args))
           (explicit (car checked))
           (raw-ops (cadr checked))
           (log-notes (hitl-plan-mcp--parse-log-notes (plist-get args :log_notes)))
           (_ (unless raw-ops (error "At least one operation in `ops' is required")))
           (ops (mapcar (lambda (e) (hitl-plan-mcp--parse-op domain e)) raw-ops))
           (note (plist-get args :note))
           (_ (when (and (eq log-notes 'one) (null note))
                (error "`note' is required when log_notes is `one'")))
           (targets (hitl-plan-mcp--cleaning-buffers
                      (hitl-plan-mcp--targets domain args explicit)))
           (_ (hitl-plan-mcp--check-count (* (length targets) (length ops))))
           (plan (hitl-plan-create
                  :domain domain :targets targets :ops ops
                  :scope (plist-get args :scope)
                  :directory (hitl-plan-mcp--directory args)
                  :metadata (list :origin 'mcp)
                  :description (or (plist-get args :description)
                                   (format "MCP plan (%s)"
                                           (mapconcat (lambda (o) (hitl-plan-describe-op domain o))
                                                      ops ", ")))
                  :log-notes log-notes
                  :note (when (eq log-notes 'one) note))))
      (when (hitl-plan-mcp-show-p args)
        (hitl-plan-mcp--show plan))
      (hitl-plan-summary plan))))

(defun hitl-plan-mcp-list (&rest args)
  "Return `plan_list' result for keyword ARGS.
ARGS may hold `:status' (a string or sequence), `:domain' and `:directory'."
  (hitl-plan-mcp--guard
    (let* ((raw (plist-get args :status))
           (statuses (cond ((null raw) nil)
                           ((stringp raw) (list (intern raw)))
                           ((or (listp raw) (vectorp raw))
                            (mapcar (lambda (s)
                                      (cond ((symbolp s) s)
                                            ((stringp s) (intern s))
                                            (t (error "Invalid status: %S" s))))
                                    (append raw nil)))
                           (t (error "Invalid status: %S" raw)))))
      (dolist (st statuses)
        (unless (memq st '(draft running paused complete discarded))
          (error "Unknown status: %s" st)))
      (list :plans (vconcat (mapcar #'hitl-plan-summary
                                    (seq-filter
                                     #'hitl-plan-mcp--plan-allowed-p
                                     (hitl-plan-list statuses (hitl-plan-mcp--directory args)
                                                     (plist-get args :domain)))))))))

(defun hitl-plan-mcp-status (&rest args)
  "Return `plan_status' result for keyword ARGS.
ARGS are `:plan_id' and optionally `:domain' and `:directory'."
  (hitl-plan-mcp--guard
    (hitl-plan-summary (hitl-plan-mcp--plan args))))

(defun hitl-plan-mcp-discard (&rest args)
  "Discard the plan named by keyword ARGS and close its review buffer.
ARGS are as for `hitl-plan-mcp-status'.  Only plans created over MCP can be
discarded this way; the user's own plans are refused.  Return the plan
summary."
  (hitl-plan-mcp--guard
    (let* ((plan (hitl-plan-mcp--plan args))
           (_ (unless (hitl-plan-origin-mcp-p plan)
                (error "Only plans created over MCP can be discarded over MCP")))
           (buf (get-buffer (hitl-plan-ui-buffer-name plan))))
      (hitl-plan-discard plan)
      (when (buffer-live-p buf) (kill-buffer buf))
      (hitl-plan-summary plan))))

(defun hitl-plan-mcp-execute (&rest args)
  "Execute the plan named by keyword ARGS when `hitl-plan-agent-may-apply'.
ARGS are as for `hitl-plan-mcp-status' plus `:exclude', step IDs to skip
\(message \"excluded over MCP\") before executing.  Without
`hitl-plan-agent-may-apply' the review buffer is opened and the result
reports `awaiting_user_confirmation'.  A live review buffer is re-rendered
after execution."
  (hitl-plan-mcp--guard
    (let ((plan (hitl-plan-mcp--plan args)))
      (hitl-plan-check-unattended plan)
      (cond
       ((memq (hitl-plan-status plan) '(complete discarded))
        (hitl-plan-summary plan))
       ((not hitl-plan-agent-may-apply)
        (hitl-plan-mcp--show plan)
        (list :status "awaiting_user_confirmation"
              :plan_buffer (hitl-plan-ui-buffer-name plan)
              :plan_id (hitl-plan-id plan)))
       (t
        (unwind-protect
            (progn (hitl-plan-mcp--exclude plan (plist-get args :exclude))
                   (hitl-plan-execute plan))
          (hitl-plan-ui-refresh-plan plan))
        (hitl-plan-summary plan))))))

;;; Tools

(mcpkit-register-tool 'plan_domains 'hitl
  :description "List registered hitl-plan target domains."
  :input-schema '(:type "object" :properties ())
  (ignore args)
  (hitl-plan-mcp-domains))

(mcpkit-register-tool 'plan_ops 'hitl
  :description "List operations available in a plan domain, with queueability and argument schemas.  Only queueable operations may be used via plan_create."
  :input-schema '(:type "object"
                  :properties (:domain (:type "string" :description "Domain name; default file-target"))
                  :required [])
  (apply #'hitl-plan-mcp-ops args))

(mcpkit-register-tool 'plan_create 'hitl
  :description "Create and save a bulk action plan (targets x operations) and open its review buffer.  Nothing is modified until executed.  Only queueable operations are accepted; log_notes `each' is rejected."
  :input-schema '(:type "object"
                  :properties (:domain (:type "string" :description "Domain name; default file-target")
                               :targets (:type "array" :items (:type "object")
                                         :description "Targets as the domain defines (file-target: {file, line, pos, snippet})")
                               :scope (:type "object" :description "Domain-defined scope, used to build targets when targets is omitted")
                               :ops (:type "array" :items (:type "object")
                                     :description "Operations as [{op, args}]")
                               :description (:type "string" :description "Plan description")
                               :log_notes (:type "string" :enum ["ignore" "one"]
                                           :description "Note logging mode (each is rejected over MCP)")
                               :note (:type "string" :description "Note text when log_notes is one")
                               :directory (:type "string" :description "Directory to store the plan in; must be the global plan directory or a registered domain's directory (default: the domain's directory, else the global plan directory)")
                               :show (:type "boolean" :description "Open the review buffer (default true)"))
                  :required ["ops"])
  (apply #'hitl-plan-mcp-create args))

(mcpkit-register-tool 'plan_list 'hitl
  :description "List saved plans, optionally filtered by status, domain and directory."
  :input-schema '(:type "object"
                  :properties (:status (:type "string"
                                        :description "Optional status filter (draft, running, paused, complete, discarded)")
                               :domain (:type "string" :description "Only plans of this domain (its directory is searched by default)")
                               :directory (:type "string" :description "Only plans stored in this directory")))
  (apply #'hitl-plan-mcp-list args))

(mcpkit-register-tool 'plan_status 'hitl
  :description "Return the status and step details of a saved plan."
  :input-schema '(:type "object"
                  :properties (:plan_id (:type "string")
                               :domain (:type "string" :description "Domain whose directory holds the plan")
                               :directory (:type "string" :description "Directory holding the plan"))
                  :required ["plan_id"])
  (apply #'hitl-plan-mcp-status args))

(mcpkit-register-tool 'plan_discard 'hitl
  :description "Discard a plan and close its review buffer if open."
  :input-schema '(:type "object"
                  :properties (:plan_id (:type "string")
                               :domain (:type "string" :description "Domain whose directory holds the plan")
                               :directory (:type "string" :description "Directory holding the plan"))
                  :required ["plan_id"])
  (apply #'hitl-plan-mcp-discard args))

(mcpkit-register-tool 'plan_execute 'hitl
  :description "Execute a saved plan when `hitl-plan-agent-may-apply' is non-nil; otherwise open the review buffer and return awaiting_user_confirmation."
  :input-schema '(:type "object"
                  :properties (:plan_id (:type "string")
                               :domain (:type "string" :description "Domain whose directory holds the plan")
                               :directory (:type "string" :description "Directory holding the plan")
                               :exclude (:type "array" :items (:type "integer")
                                         :description "Step IDs to skip before executing"))
                  :required ["plan_id"])
  (apply #'hitl-plan-mcp-execute args))

(provide 'hitl-plan-mcp)
;;; hitl-plan-mcp.el ends here
