;;; orgx-mcp.el --- MCP tools for orgx capture and bulk operations -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, tools, mcp

;;; Commentary:
;; The single mcpkit entry point for orgx.  Registers tools on the `orgx'
;; service:
;;
;; Capture (into agentic.org):
;; - `org_capture_task', `org_capture_journal', `org_capture_clock_in'.
;;
;; Bulk operations (see `orgx-bulk'):
;; - `bulk_ops': list registered bulk operations with their argument schemas.
;; - `bulk_plan_create': build and save a bulk plan and open its plan buffer.
;; - `bulk_plan_list': list saved bulk plans (optionally filtered by status).
;; - `bulk_plan_status': read a plan and its step statuses back.
;; - `bulk_plan_discard': mark a plan discarded and close its buffer.
;; - `bulk_plan_execute': execute a plan when `orgx-mcp-agent-may-apply' is
;;   non-nil; otherwise return `awaiting_user_confirmation'.
;;
;; Handlers never prompt: `each' log-notes mode is rejected over MCP.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'org-capture)
(require 'mcpkit)
(require 'orgx-bulk)
(require 'orgx-bulk-ops)
(require 'orgx-bulk-agenda)
(require 'orgx-bulk-buffer)
(require 'orgx-denote)

(defvar orgx-mcp-service
  (or (mcpkit-get-service 'orgx)
      (mcpkit-define-service 'orgx
        :description "Org capture and bulk task operations"))
  "The orgx `mcpkit-service' instance.")

;;; Capture

(defun orgx-mcp-capture-target (keys)
  "Return the target of the first `org-capture-templates' entry matching KEYS.
Falls back to the \"t\" template, then to ~/org/agentic.org."
  (if-let* ((tpl (seq-some (lambda (k) (assoc k org-capture-templates)) keys)))
      (nth 3 tpl)
    (if-let* ((default-entry (assoc "t" org-capture-templates)))
        (nth 3 default-entry)
      (list 'file (expand-file-name "~/org/agentic.org")))))

(defun orgx-mcp-capture-run (target heading &optional body-str clock-in-p)
  "Capture HEADING and BODY-STR at TARGET, clocking in if CLOCK-IN-P."
  (let* ((timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (content (concat "** " heading "\n"
                          ":PROPERTIES:\n:CREATED:  " timestamp "\n:END:\n"
                          (if (and body-str (not (string-empty-p body-str)))
                              (concat body-str "\n")
                            "")))
         (org-capture-templates
          `(("_" "" entry ,target ,content
             :immediate-finish t :kill-buffer t :prepend t
             :clock-in ,(if clock-in-p t nil)))))
    (save-window-excursion
      (org-capture nil "_"))))

(mcpkit-register-tool 'org_capture_task 'orgx
  :description "Capture a task heading and optional body to the Tasks section of agentic.org."
  :input-schema '(:type "object"
                  :properties (:heading (:type "string" :description "Task heading (with or without TODO)")
                               :body (:type "string" :description "Optional task body text"))
                  :required ["heading"])
  (let ((heading (plist-get args :heading)))
    (unless (string-prefix-p "TODO " heading)
      (setq heading (concat "TODO " heading)))
    (orgx-mcp-capture-run (orgx-mcp-capture-target '("att" "at")) heading (plist-get args :body))
    (list :status "ok" :target "task" :heading heading)))

(mcpkit-register-tool 'org_capture_journal 'orgx
  :description "Capture a journal entry heading and optional body to today's Journal datetree in agentic.org."
  :input-schema '(:type "object"
                  :properties (:heading (:type "string" :description "Journal entry heading")
                               :body (:type "string" :description "Optional journal entry body text"))
                  :required ["heading"])
  (let ((heading (plist-get args :heading)))
    (orgx-mcp-capture-run (orgx-mcp-capture-target '("ajj" "aj")) heading (plist-get args :body))
    (list :status "ok" :target "journal" :heading heading)))

(mcpkit-register-tool 'org_capture_clock_in 'orgx
  :description "Capture a task heading and body to agentic.org and immediately clock into it."
  :input-schema '(:type "object"
                  :properties (:heading (:type "string" :description "Task heading to clock in")
                               :body (:type "string" :description "Optional body text"))
                  :required ["heading"])
  (let ((heading (plist-get args :heading)))
    (unless (string-prefix-p "TODO " heading)
      (setq heading (concat "TODO " heading)))
    (orgx-mcp-capture-run (orgx-mcp-capture-target '("att" "at")) heading (plist-get args :body) t)
    (list :status "ok" :target "clock-in" :heading heading)))

;;; Bulk Operations

(defcustom orgx-mcp-agent-may-apply nil
  "When non-nil, `bulk_plan_execute' executes plans without user confirmation.
Otherwise it reports `awaiting_user_confirmation' with the plan buffer name."
  :type 'boolean
  :group 'orgx-bulk)

(defun orgx-mcp--plan (args)
  "Return the plan named by ARGS' `:plan_id', or signal an error."
  (let ((id (plist-get args :plan_id)))
    (or (orgx-bulk-get-plan id)
        (error "Unknown plan: %s" id))))

(defun orgx-mcp--parse-log-notes (raw)
  "Validate and convert RAW log_notes string/symbol into `ignore' or `one'.
Rejects `each' over MCP because handlers must never prompt."
  (let ((sym (cond
              ((null raw) orgx-bulk-log-notes)
              ((symbolp raw) raw)
              ((stringp raw) (intern (downcase raw)))
              (t (error "Invalid log_notes value: %S" raw)))))
    (when (eq sym 'each)
      (error "Log_notes mode `each' is not supported over MCP"))
    (unless (memq sym '(ignore one))
      (error "Unsupported log_notes mode: %s" sym))
    sym))

(defun orgx-mcp--parse-op (op-entry)
  "Convert JSON OP-ENTRY (`{:op NAME, :args ...}') into an `orgx-bulk-op'."
  (let* ((raw-name (or (orgx-bulk-ops--json-get op-entry 'op 'name)
                       (error "Missing `op' in operation entry: %S" op-entry)))
         (op-sym (if (symbolp raw-name) raw-name (intern (format "%s" raw-name))))
         (spec (orgx-bulk-get-op op-sym))
         (raw-args (orgx-bulk-ops--json-get op-entry 'args))
         (parsed-args
          (cond
           ((null raw-args) nil)
           ((and spec (orgx-bulk-op-spec-from-json spec)
                 (or (plistp raw-args) (hash-table-p raw-args)
                     (and (consp raw-args) (consp (car raw-args)))))
            (funcall (orgx-bulk-op-spec-from-json spec) raw-args))
           ((vectorp raw-args) (append raw-args nil))
           ((listp raw-args) raw-args)
           (t (list raw-args)))))
    (orgx-bulk-op--create :name op-sym :args parsed-args)))

(defun orgx-mcp--build-scope (args)
  "Construct an `orgx-bulk' scope plist from MCP ARGS."
  (let* ((files (append (plist-get args :files) nil))
         (scope-str (or (plist-get args :scope)
                        (if files "files" "agenda_files")))
         (match (plist-get args :match))
         (from-states (append (plist-get args :from_states) nil))
         (buf-name (plist-get args :buffer_name))
         (scope-type
          (pcase scope-str
            ("files" 'files)
            ("agenda_files" 'agenda-files)
            ("agenda-files" 'agenda-files)
            ("denote" 'denote-files)
            ("denote_files" 'denote-files)
            ("agenda" 'agenda)
            (_ (error "Unknown bulk scope: %s" scope-str)))))
    (list :type scope-type
          :files (pcase scope-type
                   ('files files)
                   ('agenda-files (org-agenda-files))
                   ('denote-files (orgx-denote-files))
                   (_ nil))
          :buffer-name buf-name
          :match match
          :from-states from-states
          :only-open (and (null match) (null from-states)))))

(mcpkit-register-tool 'bulk_ops 'orgx
  :description "List registered orgx-bulk operations with their keys, queueability, and argument schemas."
  :input-schema '(:type "object" :properties ())
  (ignore args)
  (let ((ops (sort (copy-sequence (orgx-bulk-registered-ops))
                   (lambda (a b)
                     (string< (symbol-name (orgx-bulk-op-spec-name a))
                              (symbol-name (orgx-bulk-op-spec-name b)))))))
    (list :ops
          (vconcat
           (mapcar (lambda (spec)
                     (list :name (symbol-name (orgx-bulk-op-spec-name spec))
                           :key (if-let* ((k (orgx-bulk-op-spec-key spec)))
                                    (char-to-string k)
                                  :null)
                           :label (or (orgx-bulk-op-spec-label spec) :null)
                           :queueable (if (orgx-bulk-op-spec-queueable spec) t :json-false)
                           :order_last (if (orgx-bulk-op-spec-order-last spec) t :json-false)
                           :schema (vconcat (orgx-bulk-op-spec-schema spec))))
                   ops)))))

(mcpkit-register-tool 'bulk_plan_create 'orgx
  :description "Create and save a bulk operation plan over a scope and open its review buffer. Nothing is modified until executed."
  :input-schema '(:type "object"
                  :properties (:scope (:type "string" :enum ["files" "agenda_files" "denote" "agenda"]
                                       :description "Where to collect headings; defaults to files if files is given, else agenda_files")
                               :files (:type "array" :items (:type "string") :description "Org files to search when scope is files")
                               :buffer_name (:type "string" :description "Agenda buffer name when scope is agenda")
                               :match (:type "string" :description "Optional Org tags/todo/property match string")
                               :from_states (:type "array" :items (:type "string")
                                             :description "Only include headings in these TODO states")
                               :ops (:type "array"
                                     :description "Operations to apply as [{op, args}]"
                                     :items (:type "object"))
                               :log_notes (:type "string" :enum ["ignore" "one"]
                                           :description "Note logging mode: ignore or one (each is rejected over MCP)")
                               :message (:type "string" :description "Note message (for log_notes=one) or plan description")
                               :show (:type "boolean" :description "Prepare/display the plan buffer (default true)"))
                  :required ["ops"])
  (let* ((log-notes (orgx-mcp--parse-log-notes (plist-get args :log_notes)))
         (raw-ops (append (plist-get args :ops) nil))
         (_ (unless raw-ops (error "At least one operation in `ops' is required")))
         (ops (mapcar #'orgx-mcp--parse-op raw-ops))
         (msg (or (plist-get args :message) (plist-get args :note)))
         (_ (when (and (eq log-notes 'one) (null msg))
              (error "`message' is required when log_notes is `one'")))
         (scope (orgx-mcp--build-scope args))
         (batch (orgx-bulk-batch-for-scope scope))
         (agenda-buf (car batch))
         (targets (cdr batch))
         (desc (or (plist-get args :description)
                   (plist-get args :message)
                   (format "MCP plan (%s)"
                           (mapconcat #'orgx-bulk-describe-op ops ", "))))
         (full-scope (plist-put (copy-sequence scope)
                                :buffer-name (and (buffer-live-p agenda-buf)
                                                  (buffer-name agenda-buf))))
         (plan (orgx-bulk-create-plan
                :scope full-scope
                :targets targets
                :ops ops
                :description desc
                :log-notes log-notes
                :note (when (eq log-notes 'one) msg))))
    (unless (eq (plist-get args :show) :json-false)
      (orgx-bulk-review plan))
    (orgx-bulk-plan-summary plan)))

(mcpkit-register-tool 'bulk_plan_list 'orgx
  :description "List saved orgx-bulk plans, optionally filtered by status."
  :input-schema '(:type "object"
                  :properties (:status (:type "string"
                                        :description "Optional status filter (draft, running, paused, complete, discarded)")))
  (let* ((status-arg (plist-get args :status))
         (statuses (cond
                    ((null status-arg) nil)
                    ((stringp status-arg) (list (intern status-arg)))
                    ((or (vectorp status-arg) (listp status-arg))
                     (mapcar (lambda (s) (if (symbolp s) s (intern s)))
                             (append status-arg nil)))))
         (plans (orgx-bulk-list-plans statuses)))
    (list :plans (vconcat (mapcar #'orgx-bulk-plan-summary plans)))))

(mcpkit-register-tool 'bulk_plan_status 'orgx
  :description "Return the current status and step details of a saved bulk plan."
  :input-schema '(:type "object"
                  :properties (:plan_id (:type "string"))
                  :required ["plan_id"])
  (orgx-bulk-plan-summary (orgx-mcp--plan args)))

(mcpkit-register-tool 'bulk_plan_discard 'orgx
  :description "Discard a bulk plan and close its review buffer if open."
  :input-schema '(:type "object"
                  :properties (:plan_id (:type "string"))
                  :required ["plan_id"])
  (orgx-bulk-plan-summary (orgx-bulk-discard (orgx-mcp--plan args))))

(mcpkit-register-tool 'bulk_plan_execute 'orgx
  :description "Execute a saved bulk plan when `orgx-mcp-agent-may-apply' is non-nil; otherwise return awaiting_user_confirmation."
  :input-schema '(:type "object"
                  :properties (:plan_id (:type "string")
                               :exclude (:type "array" :items (:type "integer")
                                         :description "Step IDs to skip before executing"))
                  :required ["plan_id"])
  (let ((plan (orgx-mcp--plan args)))
    (when (eq (orgx-bulk-plan-log-notes plan) 'each)
      (error "Log_notes mode `each' is not supported over MCP"))
    (cond
     ((memq (orgx-bulk-plan-status plan) '(complete discarded))
      (orgx-bulk-plan-summary plan))
     ((not orgx-mcp-agent-may-apply)
      (let ((buf-name (orgx-bulk-plan-buffer-name plan)))
        (list :status "awaiting_user_confirmation"
              :plan_buffer buf-name
              :review_buffer buf-name
              :plan_id (orgx-bulk-plan-id plan))))
     (t
      (when-let* ((excluded (append (plist-get args :exclude) nil)))
        (dolist (step (orgx-bulk-plan-steps plan))
          (when (and (memql (orgx-bulk-step-id step) excluded)
                     (eq (orgx-bulk-step-status step) 'pending))
            (setf (orgx-bulk-step-status step) 'skipped
                  (orgx-bulk-step-message step) "excluded over MCP"))))
      (orgx-bulk-execute plan)
      (when-let* ((buf (get-buffer (orgx-bulk-plan-buffer-name plan))))
        (when (buffer-live-p buf)
          (orgx-bulk-review plan)))
      (orgx-bulk-plan-summary plan)))))

(provide 'orgx-mcp)
;;; orgx-mcp.el ends here
