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
;; Bulk operations (see `orgx-bulk'), as thin wrappers over the generic
;; `hitl-plan-mcp' functions (`hitl-plan-mcp-ops', `-create', `-list',
;; `-status', `-discard', `-execute'), scoped to the `org-agenda' domain
;; and `orgx-bulk-directory':
;; - `bulk_ops', `bulk_plan_create', `bulk_plan_list', `bulk_plan_status',
;;   `bulk_plan_discard' and `bulk_plan_execute'.
;;
;; Only `bulk_plan_create' translates its arguments (orgx scope names and
;; `message' become a `:scope' plist and a `:note'), and the other tools just
;; add the domain and directory.  Hitl produces the results and the
;; (:error MESSAGE) failures.  Handlers never prompt, the tools accept only
;; `:queueable' operations, and `hitl-plan-agent-may-apply' (alias
;; `orgx-mcp-agent-may-apply') gates execution.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'org-capture)
(require 'mcpkit)
(require 'hitl-plan)
(require 'hitl-plan-mcp)
(require 'orgx)
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

(defconst orgx-mcp--capture-forbidden-body-regexp
  (concat "^[ \t]*\\(?:#\\+begin_src\\|:PROPERTIES:\\|# Local Variables\\)"
          "\\|^\\*+[ \t]")
  "Body lines an agent may not capture.
They would open a source block, add a property drawer, start a file-local
variables block (which can run code), or start a new heading.")

(defun orgx-mcp--capture-heading (heading)
  "Return HEADING as a safe one-line string, or signal an error.
HEADING must be a non-empty string.  Newlines and other control characters
become spaces, so it cannot span or start lines."
  (unless (stringp heading)
    (error "`heading' must be a string"))
  (when (> (length heading) hitl-plan-max-string-length)
    (error "`heading' is too long (max %d characters)" hitl-plan-max-string-length))
  (let ((clean (string-trim (replace-regexp-in-string "[[:cntrl:]]+" " " heading))))
    (when (string-empty-p clean)
      (error "`heading' is empty"))
    clean))

(defun orgx-mcp--capture-body (body)
  "Return BODY (nil or a string) checked for safety, or signal an error.
Multi-line text is fine, but no line may start a heading, a source block, a
property drawer or a file-local variables block."
  (cond
   ((null body) nil)
   ((not (stringp body)) (error "`body' must be a string"))
   ((> (length body) (* 8 hitl-plan-max-string-length))
    (error "`body' is too long"))
   (t
    (let ((text (replace-regexp-in-string "\r\n?" "\n" body)))
      (when (string-match-p "[\0-\10\13\14\16-\37\177]" text)
        (error "`body' contains control characters"))
      (let ((case-fold-search t))
        (when (string-match-p orgx-mcp--capture-forbidden-body-regexp text)
          (error "`body' may not contain lines starting a heading, source block, property drawer or local-variables block")))
      text))))

(defun orgx-mcp-capture-run (target heading &optional body-str clock-in-p)
  "Capture HEADING and BODY-STR at TARGET, clocking in if CLOCK-IN-P.
HEADING and BODY-STR come from an untrusted agent: they are validated (see
`orgx-mcp--capture-heading' and `orgx-mcp--capture-body') and are never
part of the capture template, whose `%'-escapes can evaluate Lisp.  The
template holds inert placeholders that are replaced by the literal text
just before the entry is finalized."
  (let* ((heading (orgx-mcp--capture-heading heading))
         (body-str (orgx-mcp--capture-body body-str))
         (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (token (md5 (format "%s%s%s" (random) (float-time) (emacs-pid))))
         (h-token (concat "ORGXHEADING" token))
         (b-token (concat "ORGXBODY" token))
         (has-body (and body-str (not (string-empty-p body-str))))
         (content (concat "** " h-token "\n"
                          ":PROPERTIES:\n:CREATED:  " timestamp "\n:END:\n"
                          (if has-body (concat b-token "\n") "")))
         (fill (lambda ()
                 (save-excursion
                   (save-restriction
                     (widen)
                     (dolist (pair `((,h-token . ,heading) (,b-token . ,body-str)))
                       (goto-char (point-min))
                       (when (and (cdr pair) (search-forward (car pair) nil t))
                         (replace-match (cdr pair) t t))))))))
    (let ((org-capture-templates
           `(("_" "" entry ,target ,content
              :immediate-finish t :kill-buffer t :prepend t
              :clock-in ,(if clock-in-p t nil))))
          (org-capture-before-finalize-hook
           (cons fill org-capture-before-finalize-hook)))
      (save-window-excursion
        (org-capture nil "_")))))

(defun orgx-mcp--capture-tool (args keys target-name clock-in-p add-todo)
  "Run a capture tool for ARGS, returning its result plist or (:error MSG).
KEYS selects the template target; TARGET-NAME labels the result; CLOCK-IN-P
clocks in; ADD-TODO prefixes \"TODO \" when missing."
  (hitl-plan-mcp--guard
    (let ((heading (orgx-mcp--capture-heading (plist-get args :heading)))
          (body (orgx-mcp--capture-body (plist-get args :body))))
      (when (and add-todo (not (string-prefix-p "TODO " heading)))
        (setq heading (concat "TODO " heading)))
      (if clock-in-p
          (orgx-mcp-capture-run (orgx-mcp-capture-target keys) heading body t)
        (orgx-mcp-capture-run (orgx-mcp-capture-target keys) heading body))
      (list :status "ok" :target target-name :heading heading))))

(mcpkit-register-tool 'org_capture_task 'orgx
  :description "Capture a task heading and optional body to the Tasks section of agentic.org."
  :input-schema '(:type "object"
                  :properties (:heading (:type "string" :description "Task heading (with or without TODO)")
                               :body (:type "string" :description "Optional task body text"))
                  :required ["heading"])
  (orgx-mcp--capture-tool args '("att" "at") "task" nil t))

(mcpkit-register-tool 'org_capture_journal 'orgx
  :description "Capture a journal entry heading and optional body to today's Journal datetree in agentic.org."
  :input-schema '(:type "object"
                  :properties (:heading (:type "string" :description "Journal entry heading")
                               :body (:type "string" :description "Optional journal entry body text"))
                  :required ["heading"])
  (orgx-mcp--capture-tool args '("ajj" "aj") "journal" nil nil))

(mcpkit-register-tool 'org_capture_clock_in 'orgx
  :description "Capture a task heading and body to agentic.org and immediately clock into it."
  :input-schema '(:type "object"
                  :properties (:heading (:type "string" :description "Task heading to clock in")
                               :body (:type "string" :description "Optional body text"))
                  :required ["heading"])
  (orgx-mcp--capture-tool args '("att" "at") "clock-in" t t))

;;; Bulk Operations

(defvaralias 'orgx-mcp-agent-may-apply 'hitl-plan-agent-may-apply
  "When non-nil, `bulk_plan_execute' executes plans without user confirmation.
Otherwise it reports `awaiting_user_confirmation' and opens the review buffer.")

(defun orgx-mcp--build-scope (args)
  "Construct an `org-agenda' scope plist from `bulk_plan_create' ARGS."
  (let* ((files (let ((raw (plist-get args :files)))
                  (unless (or (null raw) (proper-list-p raw) (vectorp raw))
                    (error "`files' must be an array of strings"))
                  (append raw nil)))
         (scope-str (or (plist-get args :scope)
                        (if files "files" "agenda_files")))
         (match (plist-get args :match))
         (from-states (let ((raw (plist-get args :from_states)))
                        (unless (or (null raw) (proper-list-p raw) (vectorp raw))
                          (error "`from_states' must be an array of strings"))
                        (append raw nil)))
         (_ (progn
              (unless (stringp scope-str) (error "`scope' must be a string"))
              (unless (cl-every #'stringp files) (error "`files' must be an array of strings"))
              (unless (cl-every #'stringp from-states)
                (error "`from_states' must be an array of strings"))
              (unless (or (null match) (stringp match)) (error "`match' must be a string"))
              (let ((bn (plist-get args :buffer_name)))
                (unless (or (null bn) (stringp bn)) (error "`buffer_name' must be a string")))))
         (scope-type
          (pcase scope-str
            ("files" 'files)
            ((or "agenda_files" "agenda-files") 'agenda-files)
            ((or "denote" "denote_files") 'denote-files)
            ("agenda" 'agenda)
            (_ (error "Unknown bulk scope: %s" scope-str)))))
    (list :type scope-type
          :files (pcase scope-type
                   ('files files)
                   ('agenda-files (org-agenda-files))
                   ('denote-files (orgx-denote-files)))
          :buffer-name (plist-get args :buffer_name)
          :match match
          :from-states from-states
          :only-open (and (null match) (null from-states)))))

(defun orgx-mcp--create (args)
  "Create a plan for `bulk_plan_create' ARGS through `hitl-plan-mcp-create'.
The `org-agenda' domain stores the plan in `orgx-bulk-directory' and, like
plans made by `orgx-bulk-create-plan', skips steps whose target is already
in the requested state.  Like every MCP handler it returns (:error MESSAGE)
rather than signalling.  The plan is origin `mcp' (untrusted); its review
buffer is displayed without taking focus."
  (hitl-plan-mcp--guard
    (let* ((msg (or (plist-get args :message) (plist-get args :note)))
           (show (hitl-plan-mcp-show-p args))
           (result
            (hitl-plan-mcp-create
             :domain "org-agenda"
             :scope (orgx-mcp--build-scope args)
             :ops (plist-get args :ops)
             :log_notes (plist-get args :log_notes)
             :note msg
             :description (or (plist-get args :description) msg)
             :show :json-false)))
      (when-let* (show
                  ((not (plist-get result :error)))
                  (plan (hitl-plan-get (plist-get result :plan_id))))
        ;; Build the review buffer but never select it: the user may be typing.
        (let ((buf (orgx-bulk-review plan nil)))
          (when (bufferp buf)
            (display-buffer buf '(nil (inhibit-same-window . t)))
            (message "orgx: plan %s awaits your review in %s"
                     (hitl-plan-id plan) (buffer-name buf)))))
      result)))

(mcpkit-register-tool 'bulk_ops 'orgx
  :description "List registered orgx-bulk operations with their keys, queueability, and argument schemas."
  :input-schema '(:type "object" :properties ())
  (ignore args)
  (hitl-plan-mcp-ops :domain "org-agenda"))

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
  (orgx-mcp--create args))

(mcpkit-register-tool 'bulk_plan_list 'orgx
  :description "List saved orgx-bulk plans, optionally filtered by status."
  :input-schema '(:type "object"
                  :properties (:status (:type "string"
                                        :description "Optional status filter (draft, running, paused, complete, discarded)")))
  (apply #'hitl-plan-mcp-list :domain "org-agenda" args))

(mcpkit-register-tool 'bulk_plan_status 'orgx
  :description "Return the current status and step details of a saved bulk plan."
  :input-schema '(:type "object"
                  :properties (:plan_id (:type "string"))
                  :required ["plan_id"])
  (apply #'hitl-plan-mcp-status :domain "org-agenda" args))

(mcpkit-register-tool 'bulk_plan_discard 'orgx
  :description "Discard a bulk plan and close its review buffer if open."
  :input-schema '(:type "object"
                  :properties (:plan_id (:type "string"))
                  :required ["plan_id"])
  (apply #'hitl-plan-mcp-discard :domain "org-agenda" args))

(mcpkit-register-tool 'bulk_plan_execute 'orgx
  :description "Execute a saved bulk plan when `orgx-mcp-agent-may-apply' is non-nil; otherwise open the review buffer and return awaiting_user_confirmation."
  :input-schema '(:type "object"
                  :properties (:plan_id (:type "string")
                               :exclude (:type "array" :items (:type "integer")
                                         :description "Step IDs to skip before executing"))
                  :required ["plan_id"])
  (apply #'hitl-plan-mcp-execute :domain "org-agenda" args))

;;; Entry editing

(defun orgx-mcp--string-args (args keys)
  "Signal an error unless every key in KEYS is a non-empty string in ARGS."
  (dolist (k keys)
    (let ((v (plist-get args k)))
      (unless (and (stringp v) (not (string-empty-p v)))
        (error "`%s' must be a non-empty string" (substring (symbol-name k) 1))))))

(defun orgx-mcp--entry-edit (args keys fn)
  "Run FN on the heading ARGS selects, returning the result plist.
KEYS are the required string arguments.  FILE must be allowed (see
`orgx-bulk-file-allowed-p'); file-local variables are ignored."
  (hitl-plan-mcp--guard
    (orgx-mcp--string-args args (append '(:file :match_property :match_value) keys))
    (let* ((file (orgx-bulk--check-file (plist-get args :file)))
           (enable-local-variables nil)
           (enable-dir-local-variables nil)
           (res (orgx-entry-update-in-file
                 file (plist-get args :match_property) (plist-get args :match_value)
                 fn)))
      (append res (list :status "updated")))))

(defconst orgx-mcp--match-schema
  '(:file (:type "string" :description "Org file containing the heading")
    :match_property (:type "string" :description "Property identifying the heading (e.g. CUSTOM_ID)")
    :match_value (:type "string" :description "Value of match_property on the heading"))
  "Input schema properties shared by the entry editing tools.")

(mcpkit-register-tool 'org_heading_set_property 'orgx
  :description "Set a property on the heading whose match_property equals match_value."
  :input-schema `(:type "object"
                  :properties (,@orgx-mcp--match-schema
                               :property (:type "string" :description "Property to set")
                               :value (:type "string" :description "New property value"))
                  :required ["file" "match_property" "match_value" "property" "value"])
  (orgx-mcp--entry-edit
   args '(:property :value)
   (lambda (pos) (orgx-entry-set-property (plist-get args :property)
                                          (plist-get args :value) pos))))

(mcpkit-register-tool 'org_heading_set_todo 'orgx
  :description "Set the TODO state of the heading whose match_property equals match_value."
  :input-schema `(:type "object"
                  :properties (,@orgx-mcp--match-schema
                               :state (:type "string" :description "TODO keyword, e.g. TODO or DONE"))
                  :required ["file" "match_property" "match_value" "state"])
  (orgx-mcp--entry-edit
   args '(:state)
   (lambda (pos) (orgx-entry-set-todo (plist-get args :state) pos))))

(mcpkit-register-tool 'org_heading_append_note 'orgx
  :description "Append a timestamped bullet note to the body of the heading whose match_property equals match_value, after its drawers; optionally under a child heading."
  :input-schema `(:type "object"
                  :properties (,@orgx-mcp--match-schema
                               :note (:type "string" :description "Note text")
                               :target_section (:type "string"
                                                :description "Optional child heading title to append under (created if missing)"))
                  :required ["file" "match_property" "match_value" "note"])
  (orgx-mcp--entry-edit
   args '(:note)
   (lambda (pos)
     (let ((section (plist-get args :target_section)))
       (unless (or (null section) (stringp section))
         (error "`target_section' must be a string"))
       (orgx-entry-append-note (plist-get args :note) pos section)))))

(defun orgx-mcp--entry-json (entry file)
  "Convert ENTRY plist from `orgx-entry-get' into a JSON-safe plist for FILE."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pair (plist-get entry :properties))
      (puthash (car pair) (cdr pair) ht))
    (list :file file
          :heading (or (plist-get entry :heading) "")
          :todo (plist-get entry :todo)
          :properties ht
          :body (or (plist-get entry :body) "")
          :children (vconcat (mapcar (lambda (c) (orgx-mcp--entry-json c file))
                                     (plist-get entry :children))))))

(mcpkit-register-tool 'org_heading_get 'orgx
  :description "Get the details of the heading whose match_property equals match_value (heading, TODO state, properties, body, and children)."
  :input-schema `(:type "object"
                  :properties ,orgx-mcp--match-schema
                  :required ["file" "match_property" "match_value"])
  (hitl-plan-mcp--guard
    (orgx-mcp--string-args args '(:file :match_property :match_value))
    (let* ((file (orgx-bulk--check-file (plist-get args :file)))
           (enable-local-variables nil)
           (enable-dir-local-variables nil)
           (entry (orgx-entry-read-in-file
                   file (plist-get args :match_property) (plist-get args :match_value))))
      (orgx-mcp--entry-json entry file))))

(provide 'orgx-mcp)
;;; orgx-mcp.el ends here
