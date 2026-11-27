;;; agent-shell-queue-train-contract.el --- Durable train contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure-data schemas, state enums, authorization rules, limit validators,
;; artifact checks, DAG cycle detection, and JSON round-trip helpers for
;; durable work trains in `agent-shell-queue'.
;;
;; This module has zero dependencies on workflow, HITL, GitHub, or mail
;; packages and performs no side effects at load time.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

;;; Schema and Enum Constants

(defconst agent-shell-queue-train-schema-version 1
  "Current persisted schema version for durable train records.")

(defconst agent-shell-queue-train-states
  '(staged active paused completed cancelled expired)
  "Valid lifecycle states for a durable train.")

(defconst agent-shell-queue-train-terminal-states
  '(completed cancelled expired)
  "Terminal lifecycle states for a durable train.")

(defconst agent-shell-queue-train-task-states
  '(pending ready claimed running waiting needs-human
            succeeded failed blocked cancelled)
  "Valid lifecycle states for a task within a train.")

(defconst agent-shell-queue-train-task-terminal-states
  '(succeeded failed cancelled)
  "Terminal lifecycle states for a train task.")

(defconst agent-shell-queue-train-mutation-outcomes
  '(accepted duplicate conflict forbidden invalid)
  "Standardized outcome symbols returned by train mutations.")

(defconst agent-shell-queue-train-evidence-channels
  '(structured-submission provider-protocol validated-artifact transcript-extraction)
  "Evidence channels ordered from strongest (index 0) to weakest.")

(defconst agent-shell-queue-train-effect-kinds
  '(dispatch observe request-review publish-artifact apply-authorized-operation)
  "Declared effect kinds permitted in persisted train records.")

(defconst agent-shell-queue-train-required-limit-keys
  '(:max-turns :max-retries :max-duration)
  "Limit keys that must be finite positive integers on every train.")

(defconst agent-shell-queue-train-optional-limit-keys
  '(:max-repairs :max-observations)
  "Optional finite positive integer limit keys on a train.")

;;; Internal Validation Helpers

(defun agent-shell-queue-train--nonempty-string-p (val)
  "Return non-nil when VAL is a non-empty string."
  (and (stringp val) (not (string-empty-p val))))

(defun agent-shell-queue-train--positive-integer-p (val)
  "Return non-nil when VAL is a positive integer."
  (and (integerp val) (> val 0)))

(defun agent-shell-queue-train--nonneg-integer-p (val)
  "Return non-nil when VAL is a non-negative integer."
  (and (integerp val) (>= val 0)))

(defun agent-shell-queue-train--normalize-sym (val)
  "Normalize string or symbol VAL into a symbol, or nil if neither."
  (cond
   ((symbolp val) val)
   ((stringp val) (intern val))
   (t nil)))

(defun agent-shell-queue-train--validation-result (errors)
  "Return a validation plist from ERRORS list (empty means valid)."
  (if errors
      (list :ok nil :errors (nreverse errors))
    (list :ok t :errors nil)))

;;; Public Outcome and Evidence Helpers

(defun agent-shell-queue-train-make-outcome (outcome &rest plist)
  "Build a standardized mutation response plist with OUTCOME and PLIST.
OUTCOME must be a member of `agent-shell-queue-train-mutation-outcomes'."
  (unless (memq outcome agent-shell-queue-train-mutation-outcomes)
    (error "Invalid mutation outcome: %S" outcome))
  (append (list :outcome outcome) plist))

(defun agent-shell-queue-train-evidence-rank (channel)
  "Return integer rank for evidence CHANNEL (lower is stronger), or nil."
  (let ((sym (agent-shell-queue-train--normalize-sym channel)))
    (seq-position agent-shell-queue-train-evidence-channels sym)))

(defun agent-shell-queue-train-evidence-stronger-p (candidate existing)
  "Return non-nil if evidence channel CANDIDATE is stronger than EXISTING."
  (let ((r-cand (agent-shell-queue-train-evidence-rank candidate))
        (r-exist (agent-shell-queue-train-evidence-rank existing)))
    (and r-cand (or (null r-exist) (< r-cand r-exist)))))

;;; Schema, Limit, and Authorization Validators

(defun agent-shell-queue-train-validate-schema-version (version)
  "Validate schema VERSION against `agent-shell-queue-train-schema-version'.
Return a validation plist (:ok BOOL :errors LIST)."
  (cond
   ((not (integerp version))
    (list :ok nil :errors (list (format "Schema version must be an integer: %S" version))))
   ((> version agent-shell-queue-train-schema-version)
    (list :ok nil :errors (list (format "Unsupported newer schema version %d (supported: %d)"
                                        version agent-shell-queue-train-schema-version))))
   ((< version 1)
    (list :ok nil :errors (list (format "Invalid schema version: %d" version))))
   (t (list :ok t :errors nil))))

(defun agent-shell-queue-train-validate-limits (limits)
  "Validate train LIMITS plist.
All keys in `agent-shell-queue-train-required-limit-keys' must be finite
positive integers.  Optional keys, if present, must also be finite positive
integers.  Unknown keys are rejected."
  (let ((errors nil)
        (allowed (append agent-shell-queue-train-required-limit-keys
                         agent-shell-queue-train-optional-limit-keys)))
    (if (not (and (listp limits) (cl-evenp (length limits))))
        (push "Limits must be a plist" errors)
      (dolist (key agent-shell-queue-train-required-limit-keys)
        (let ((val (plist-get limits key)))
          (unless (agent-shell-queue-train--positive-integer-p val)
            (push (format "Limit %s must be a positive integer (got %S)" key val) errors))))
      (let ((rest limits))
        (while rest
          (let ((k (car rest))
                (v (cadr rest)))
            (cond
             ((not (memq k allowed))
              (push (format "Unknown limit key: %S" k) errors))
             ((and (memq k agent-shell-queue-train-optional-limit-keys)
                   (not (null v))
                   (not (agent-shell-queue-train--positive-integer-p v)))
              (push (format "Optional limit %s must be a positive integer (got %S)" k v) errors))))
          (setq rest (cddr rest)))))
    (agent-shell-queue-train--validation-result errors)))

(defun agent-shell-queue-train-validate-authorization (auth)
  "Validate deny-by-default authorization grant AUTH plist.
AUTH may be nil (representing an empty deny-all grant), or a plist with
`:grant-id', `:allowed-effects' (list of effect kinds), `:allowed-resources'
(list of canonical resource strings), and optional `:expires-at' epoch."
  (if (null auth)
      (list :ok t :errors nil)
    (let ((errors nil))
      (if (not (and (listp auth) (cl-evenp (length auth))))
          (push "Authorization must be a plist or nil" errors)
        (unless (agent-shell-queue-train--nonempty-string-p (plist-get auth :grant-id))
          (push "Authorization :grant-id must be a non-empty string" errors))
        (let ((effects (plist-get auth :allowed-effects)))
          (unless (listp effects)
            (push "Authorization :allowed-effects must be a list" errors))
          (dolist (eff effects)
            (unless (memq (agent-shell-queue-train--normalize-sym eff)
                          agent-shell-queue-train-effect-kinds)
              (push (format "Unknown effect kind in authorization: %S" eff) errors))))
        (let ((resources (plist-get auth :allowed-resources)))
          (unless (and (listp resources)
                       (seq-every-p #'agent-shell-queue-train--nonempty-string-p resources))
            (push "Authorization :allowed-resources must be a list of non-empty strings" errors)))
        (let ((exp (plist-get auth :expires-at)))
          (when (and exp (not (numberp exp)))
            (push "Authorization :expires-at must be a numeric epoch or nil" errors))))
      (agent-shell-queue-train--validation-result errors))))

(defun agent-shell-queue-train-authorized-p (auth effect-kind resource &optional now)
  "Return non-nil iff AUTH explicitly grants EFFECT-KIND on RESOURCE at NOW.
Default is deny-by-default: nil AUTH or missing explicit grant returns nil."
  (when (and auth (listp auth))
    (let* ((eff (agent-shell-queue-train--normalize-sym effect-kind))
           (allowed-effects (mapcar #'agent-shell-queue-train--normalize-sym
                                    (plist-get auth :allowed-effects)))
           (allowed-resources (plist-get auth :allowed-resources))
           (expires-at (plist-get auth :expires-at))
           (ts (or now (float-time))))
      (and eff
           (memq eff allowed-effects)
           (agent-shell-queue-train--nonempty-string-p resource)
           (member resource allowed-resources)
           (or (null expires-at) (<= ts expires-at))))))

(defun agent-shell-queue-train-narrow-authorization (parent child)
  "Validate and return CHILD authorization narrowed from PARENT.
Signals `user-error' if CHILD attempts to expand effects, resources, or
expiry beyond PARENT.  Nil CHILD yields nil (deny-all)."
  (when child
    (unless parent
      (user-error "Cannot narrow authorization from an empty parent grant"))
    (let* ((p-eff (mapcar #'agent-shell-queue-train--normalize-sym
                          (plist-get parent :allowed-effects)))
           (c-eff (mapcar #'agent-shell-queue-train--normalize-sym
                          (plist-get child :allowed-effects)))
           (p-res (plist-get parent :allowed-resources))
           (c-res (plist-get child :allowed-resources))
           (p-exp (plist-get parent :expires-at))
           (c-exp (plist-get child :expires-at)))
      (dolist (eff c-eff)
        (unless (memq eff p-eff)
          (user-error "Child authorization expands effect scope: %S" eff)))
      (dolist (res c-res)
        (unless (member res p-res)
          (user-error "Child authorization expands resource scope: %S" res)))
      (when (and p-exp (or (null c-exp) (> c-exp p-exp)))
        (user-error "Child authorization expands expiry beyond parent"))
      child)))

;;; Event, Result, Artifact, and Graph Patch Validators

(defun agent-shell-queue-train-validate-event (event)
  "Validate incoming inbox EVENT plist."
  (let ((errors nil))
    (if (not (and (listp event) (cl-evenp (length event))))
        (push "Event must be a plist" errors)
      (let ((sv (agent-shell-queue-train-validate-schema-version
                 (plist-get event :schema-version))))
        (unless (plist-get sv :ok)
          (setq errors (append (reverse (plist-get sv :errors)) errors))))
      (unless (agent-shell-queue-train--nonempty-string-p (plist-get event :event-id))
        (push "Event :event-id must be a non-empty string" errors))
      (unless (agent-shell-queue-train--nonempty-string-p (plist-get event :source))
        (push "Event :source must be a non-empty string" errors))
      (unless (agent-shell-queue-train--normalize-sym (plist-get event :type))
        (push "Event :type must be a symbol or string" errors))
      (unless (numberp (plist-get event :observed-at))
        (push "Event :observed-at must be a numeric timestamp" errors))
      (unless (numberp (plist-get event :received-at))
        (push "Event :received-at must be a numeric timestamp" errors))
      (let ((gen (plist-get event :generation)))
        (when (and gen (not (agent-shell-queue-train--nonneg-integer-p gen)))
          (push "Event :generation must be a non-negative integer" errors))))
    (agent-shell-queue-train--validation-result errors)))

(defun agent-shell-queue-train-validate-artifact (artifact &optional allowed-roots)
  "Validate ARTIFACT manifest plist, optionally enforcing ALLOWED-ROOTS.
Rejects path traversal (`..') and locators outside ALLOWED-ROOTS."
  (let ((errors nil))
    (if (not (and (listp artifact) (cl-evenp (length artifact))))
        (push "Artifact must be a plist" errors)
      (let ((sv (agent-shell-queue-train-validate-schema-version
                 (plist-get artifact :schema-version))))
        (unless (plist-get sv :ok)
          (setq errors (append (reverse (plist-get sv :errors)) errors))))
      (unless (agent-shell-queue-train--normalize-sym (plist-get artifact :type))
        (push "Artifact :type is required" errors))
      (unless (agent-shell-queue-train--nonempty-string-p (plist-get artifact :digest))
        (push "Artifact :digest must be a non-empty string" errors))
      (let ((locator (plist-get artifact :locator)))
        (if (not (agent-shell-queue-train--nonempty-string-p locator))
            (push "Artifact :locator must be a non-empty string" errors)
          (when (string-match-p "\\(?:^\\|/\\)\\.\\.$\\|\\(?:^\\|/\\)\\.\\./" locator)
            (push "Artifact :locator must not contain path traversal" errors))
          (when allowed-roots
            (let ((expanded (expand-file-name locator)))
              (unless (seq-some (lambda (root)
                                  (let ((r (file-name-as-directory (expand-file-name root))))
                                    (string-prefix-p r expanded)))
                                allowed-roots)
                (push (format "Artifact :locator %s is outside allowed roots" locator)
                      errors)))))))
    (agent-shell-queue-train--validation-result errors)))

(defun agent-shell-queue-train-validate-result (result &optional allowed-roots)
  "Validate agent RESULT submission plist.
When ALLOWED-ROOTS is non-nil, each artifact locator is checked against it."
  (let ((errors nil))
    (if (not (and (listp result) (cl-evenp (length result))))
        (push "Result submission must be a plist" errors)
      (dolist (k '(:train-id :task-id :attempt-id :execution-token :idempotency-key))
        (unless (agent-shell-queue-train--nonempty-string-p (plist-get result k))
          (push (format "Result %s must be a non-empty string" k) errors)))
      (unless (agent-shell-queue-train--nonneg-integer-p (plist-get result :generation))
        (push "Result :generation must be a non-negative integer" errors))
      (unless (agent-shell-queue-train--normalize-sym (plist-get result :outcome))
        (push "Result :outcome is required" errors))
      (dolist (art (plist-get result :artifacts))
        (let ((res (agent-shell-queue-train-validate-artifact art allowed-roots)))
          (unless (plist-get res :ok)
            (setq errors (append (reverse (plist-get res :errors)) errors))))))
    (agent-shell-queue-train--validation-result errors)))

(defun agent-shell-queue-train-dag-cycle-p (adjacency)
  "Return non-nil if directed graph ADJACENCY contains a cycle.
ADJACENCY is an alist of (NODE-ID . DEP-ID-LIST)."
  (let ((state (make-hash-table :test #'equal))
        (found nil))
    (cl-labels
        ((visit (node)
           (let ((st (map-elt state node)))
             (cond
              ((eq st 'visiting) (setq found t))
              ((eq st 'visited) nil)
              (t
               (setf (map-elt state node) 'visiting)
               (dolist (dep (cdr (assoc node adjacency)))
                 (unless found
                   (visit dep)))
               (setf (map-elt state node) 'visited))))))
      (dolist (entry adjacency)
        (unless found
          (visit (car entry)))))
    found))

(defun agent-shell-queue-train-validate-graph-patch (patch existing-adjacency)
  "Validate graph PATCH plist against EXISTING-ADJACENCY alist.
Rejects invalid revisions, missing task IDs, duplicate task IDs, unknown
dependencies, and cycles."
  (let ((errors nil))
    (if (not (and (listp patch) (cl-evenp (length patch))))
        (push "Graph patch must be a plist" errors)
      (unless (agent-shell-queue-train--nonneg-integer-p (plist-get patch :base-revision))
        (push "Graph patch :base-revision must be a non-negative integer" errors))
      (let* ((added (plist-get patch :added-tasks))
             (deps (plist-get patch :dependencies))
             (known (make-hash-table :test #'equal))
             (combined (copy-alist existing-adjacency)))
        (dolist (entry existing-adjacency)
          (setf (map-elt known (car entry)) t))
        (dolist (task added)
          (let ((tid (plist-get task :id)))
            (cond
             ((not (agent-shell-queue-train--nonempty-string-p tid))
              (push "Added task missing non-empty :id" errors))
             ((map-elt known tid)
              (push (format "Duplicate task ID in patch: %s" tid) errors))
             (t
              (setf (map-elt known tid) t)
              (push (cons tid (plist-get task :depends-on)) combined)))))
        (dolist (dep-edge deps)
          (let ((from (car dep-edge))
                (to (cdr dep-edge)))
            (if (and (map-elt known from) (map-elt known to))
                (let ((cell (assoc from combined)))
                  (if cell
                      (setcdr cell (cons to (cdr cell)))
                    (push (list from to) combined)))
              (push (format "Dependency edge references unknown task: %S -> %S" from to)
                    errors))))
        (dolist (entry combined)
          (dolist (d (cdr entry))
            (unless (map-elt known d)
              (push (format "Task %s depends on unknown task %s" (car entry) d) errors))))
        (when (and (null errors)
                   (agent-shell-queue-train-dag-cycle-p combined))
          (push "Graph patch introduces a dependency cycle" errors))))
    (agent-shell-queue-train--validation-result errors)))

;;; Plain-Data JSON Round-Trip Helpers

(defun agent-shell-queue-train--plist-to-json-obj (obj)
  "Recursively convert plain-data plist/list OBJ to `json-serialize' form."
  (cond
   ((null obj) :null)
   ((eq obj t) t)
   ((eq obj :false) :false)
   ((symbolp obj) (symbol-name obj))
   ((or (stringp obj) (numberp obj)) obj)
   ((and (listp obj) (keywordp (car obj)))
    (let ((ht (make-hash-table :test #'equal))
          (rest obj))
      (while rest
        (let ((k (substring (symbol-name (car rest)) 1))
              (v (cadr rest)))
          (setf (map-elt ht k) (agent-shell-queue-train--plist-to-json-obj v)))
        (setq rest (cddr rest)))
      ht))
   ((listp obj)
    (vconcat (mapcar #'agent-shell-queue-train--plist-to-json-obj obj)))
   ((vectorp obj)
    (vconcat (mapcar #'agent-shell-queue-train--plist-to-json-obj (append obj nil))))
   (t (error "Unsupported value for train JSON serialization: %S" obj))))

(defun agent-shell-queue-train--json-obj-to-plist (obj)
  "Recursively convert parsed JSON OBJ into keyword-keyed plist/list form."
  (cond
   ((eq obj :null) nil)
   ((eq obj :false) nil)
   ((hash-table-p obj)
    (let (out)
      (map-do (lambda (k v)
                (setq out (plist-put out (intern (concat ":" k))
                                     (agent-shell-queue-train--json-obj-to-plist v))))
              obj)
      out))
   ((vectorp obj)
    (mapcar #'agent-shell-queue-train--json-obj-to-plist (append obj nil)))
   (t obj)))

(defun agent-shell-queue-train-to-json-string (record)
  "Serialize plain-data train RECORD plist to a JSON string."
  (json-serialize (agent-shell-queue-train--plist-to-json-obj record)
                  :null-object :null
                  :false-object :false))

(defun agent-shell-queue-train-from-json-string (json-str)
  "Parse JSON-STR into a keyword-keyed plain-data plist."
  (agent-shell-queue-train--json-obj-to-plist
   (json-parse-string json-str
                      :object-type 'hash-table
                      :array-type 'array
                      :null-object :null
                      :false-object :false)))

(provide 'agent-shell-queue-train-contract)
;;; agent-shell-queue-train-contract.el ends here
