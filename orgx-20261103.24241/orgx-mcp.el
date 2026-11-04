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
;; Bulk task-state changes (see `orgx-bulk'):
;; - `bulk_state_preview': build a plan and open its review buffer.
;; - `bulk_state_status': read a plan back (e.g. after the user confirmed).
;; - `bulk_state_apply': apply a plan, only if `orgx-mcp-agent-may-apply'.
;; - `bulk_state_discard': drop a plan and close its review buffer.
;;
;; Other packages call `orgx-mcp-capture-run' directly to capture a heading.
;; Services start only via `mcpkit-start-service'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'org-capture)
(require 'mcpkit)
(require 'orgx-bulk)
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

;;; Bulk task-state changes

(defcustom orgx-mcp-agent-may-apply nil
  "When non-nil, `bulk_state_apply' applies plans without the user confirming.
Otherwise it reports that user confirmation in the review buffer is needed."
  :type 'boolean
  :group 'orgx-bulk)

(defun orgx-mcp--plan (args)
  "Return the plan named by ARGS' `:plan_id', or signal an error."
  (let ((id (plist-get args :plan_id)))
    (or (orgx-bulk-get-plan id)
        (error "Unknown plan: %s" id))))

(mcpkit-register-tool 'bulk_state_preview 'orgx
  :description "Plan moving todo headings to another state (e.g. CANCELED, DONE) and open a review buffer for the user to confirm. Nothing changes until applied. Returns plan_id and the affected headings."
  :input-schema '(:type "object"
                  :properties (:target_state (:type "string" :description "Todo keyword to move headings to")
                               :scope (:type "string" :enum ["files" "agenda_files" "denote"]
                                       :description "Where to look; defaults to files if files is given, else agenda_files")
                               :files (:type "array" :items (:type "string") :description "Org files to search")
                               :match (:type "string" :description "Optional org match string, e.g. +project-someday")
                               :from_states (:type "array" :items (:type "string")
                                             :description "Only headings in these states; default all not-done")
                               :show (:type "boolean" :description "Display the review buffer (default true)"))
                  :required ["target_state"])
  (let* ((files (append (plist-get args :files) nil))
         (scope (or (plist-get args :scope) (if files "files" "agenda_files")))
         (items (orgx-bulk-collect-files
                 :files (pcase scope
                          ("files" files)
                          ("denote" (orgx-denote-files))
                          (_ 'agenda))
                 :match (plist-get args :match)
                 :from (append (plist-get args :from_states) nil)))
         (plan (orgx-bulk-plan (plist-get args :target_state) items
                               (format "Requested over MCP (scope: %s)" scope))))
    (unless (eq (plist-get args :show) :json-false)
      (orgx-bulk-review plan))
    (orgx-bulk-plan-summary plan)))

(mcpkit-register-tool 'bulk_state_status 'orgx
  :description "Return the current state of a bulk plan, including the user's selection and per-heading results."
  :input-schema '(:type "object"
                  :properties (:plan_id (:type "string"))
                  :required ["plan_id"])
  (orgx-bulk-plan-summary (orgx-mcp--plan args)))

(mcpkit-register-tool 'bulk_state_apply 'orgx
  :description "Apply a pending bulk plan. Unless the user has enabled agent apply, this only reports that the user must confirm in the review buffer."
  :input-schema '(:type "object"
                  :properties (:plan_id (:type "string")
                               :exclude (:type "array" :items (:type "integer")
                                         :description "Item ids to leave unchanged"))
                  :required ["plan_id"])
  (let ((plan (orgx-mcp--plan args)))
    (cond
     ((not (eq (orgx-bulk-plan-status plan) 'pending))
      (orgx-bulk-plan-summary plan))
     ((not orgx-mcp-agent-may-apply)
      (list :status "awaiting_user_confirmation"
            :review_buffer (orgx-bulk-review-buffer-name plan)
            :plan_id (orgx-bulk-plan-id plan)))
     (t
      (let ((excluded (append (plist-get args :exclude) nil)))
        (orgx-bulk-plan-set-selection
         plan
         (seq-difference (seq-map #'orgx-bulk-item-id (orgx-bulk-plan-selected plan))
                         excluded)))
      (orgx-bulk-apply plan)
      (orgx-bulk-plan-summary plan)))))

(mcpkit-register-tool 'bulk_state_discard 'orgx
  :description "Discard a pending bulk plan and close its review buffer."
  :input-schema '(:type "object"
                  :properties (:plan_id (:type "string"))
                  :required ["plan_id"])
  (orgx-bulk-plan-summary (orgx-bulk-discard (orgx-mcp--plan args))))

(provide 'orgx-mcp)
;;; orgx-mcp.el ends here
