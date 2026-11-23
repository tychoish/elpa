;;; mailx-mcp.el --- Read-only mail inspection over MCP -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0") (hitl "0.1.0"))
;; Keywords: mail, convenience
;; URL: https://github.com/tychoish/mailx

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Explicitly register a service for inspecting a chosen local mail database.
;; Rule persistence and bulk actions require separate review and are not exposed.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'mcpkit)
(require 'mailx-rules)
(require 'mailx-bulk)

(declare-function mailx-test-rule "mailx-rules" (root rule))
(declare-function mailx-rules-load "mailx-rules" (root &optional refresh))
(declare-function mailx-rules-save "mailx-rules" (root rules))
(declare-function mailx-rules-export-all "mailx-rules" (root))
(declare-function make-mailx-rule "mailx-rules" (&rest plist))
(declare-function mailx--validate-rule "mailx-rules" (rule))
(declare-function hitl-plan-create "hitl-plan" (&rest plist))

(defgroup mailx-mcp nil "Mail inspection over MCP." :group 'mail)

(defcustom mailx-mcp-allowed-roots nil
  "Mail roots authorized for MCP inspection.  An empty list denies access."
  :type '(repeat directory) :group 'mailx-mcp)

(defcustom mailx-mcp-unclassified-query
  "NOT (tag:\"tier:reply\" OR tag:\"tier:read\" OR tag:\"tier:cursory\" OR tag:\"tier:bulk\" OR tag:\"tier:junk\")"
  "Administrator-controlled mu query selecting unclassified messages."
  :type 'string :group 'mailx-mcp)

(defun mailx-mcp--root (root)
  "Validate ROOT against the configured canonical local root allowlist."
  (unless (and (stringp root) (not (string-empty-p root))
               (not (file-remote-p root))
               (file-directory-p root))
    (error "Root must contain a local .mu database"))
  (let* ((canonical (file-name-as-directory (file-truename root)))
         (database (expand-file-name ".mu" canonical)))
    (unless (and (seq-some (lambda (allowed)
                             (and (stringp allowed)
                                  (not (file-remote-p allowed))
                                  (file-directory-p allowed)
                                  (equal canonical
                                         (file-name-as-directory
                                          (file-truename allowed)))))
                           mailx-mcp-allowed-roots)
                 (file-directory-p database)
                 (not (file-symlink-p database)))
      (error "Mail root is not authorized or has no local .mu database"))
    canonical))

(defun mailx-mcp--list-messages (root limit)
  "Return up to LIMIT unclassified header summaries in ROOT."
  (setq root (mailx-mcp--root root))
  (unless (and (stringp mailx-mcp-unclassified-query)
               (not (string-empty-p (string-trim mailx-mcp-unclassified-query))))
    (error "A nonempty unclassified query must be configured"))
  (unless (and (integerp limit) (<= 1 limit 100))
    (error "Limit must be between 1 and 100"))
  (let ((stderr-file (make-temp-file "mailx-mcp-mu-"))
         (process-environment (cons "LC_ALL=C" process-environment)))
    (unwind-protect
        (with-temp-buffer
          (let ((status (process-file "mu" nil (list (current-buffer) stderr-file) nil "find"
                                      (concat "--muhome=" (expand-file-name ".mu" root))
                                      "--format=sexp" (format "--maxnum=%d" limit)
                                      mailx-mcp-unclassified-query))
                (messages nil))
            (unless (zerop status)
              (let ((diagnostic (with-temp-buffer
                                  (insert-file-contents stderr-file)
                                  (string-trim (buffer-string)))))
                (unless (and (equal status 2) (= (buffer-size) 0)
                             (equal diagnostic "no matches for search expression"))
                  (error "mu find failed (%s): %s" status diagnostic)))
              (erase-buffer))
            (goto-char (point-min))
            (while (progn (skip-chars-forward " \t\r\n") (not (eobp)))
              (let ((message (read (current-buffer))))
                (unless (and (listp message) (keywordp (car message)))
                  (error "Unexpected mu message output"))
                (push (list :subject (map-elt message :subject)
                            :from (format "%s" (map-elt message :from))
                            :message_id (map-elt message :message-id)
                            :maildir (map-elt message :maildir))
                      messages)))
            (vconcat (nreverse messages))))
      (delete-file stderr-file))))

(defun mailx-mcp--test-rule (root rule-data)
  "Test candidate RULE-DATA against ROOT's mail database."
  (setq root (mailx-mcp--root root))
  (let* ((id (intern (format "%s" (or (map-elt rule-data :id) "preview-rule"))))
         (match (map-elt rule-data :match))
         (folder (map-elt rule-data :folder))
         (tier (when-let* ((t-val (map-elt rule-data :tier)))
                 (intern (format "%s" t-val))))
         (tags (map-elt rule-data :tags))
         (rule (make-mailx-rule :id id :match match :folder folder :tier tier :tags tags :source 'preview)))
    (mailx-test-rule root rule)))

(defun mailx-mcp--add-rule (root rule-data)
  "Persist validated RULE-DATA to ROOT's rule store and update export filters."
  (setq root (mailx-mcp--root root))
  (let* ((id (intern (format "%s" (or (map-elt rule-data :id)
                                      (error "Rule must have an :id")))))
         (match (or (map-elt rule-data :match)
                    (error "Rule must have a :match condition")))
         (folder (map-elt rule-data :folder))
         (tier (when-let* ((t-val (map-elt rule-data :tier)))
                 (intern (format "%s" t-val))))
         (tags (map-elt rule-data :tags))
         (rule (make-mailx-rule :id id :match match :folder folder :tier tier :tags tags
                                :source 'mcp :created (format-time-string "%Y-%m-%d")))
         (existing (mailx-rules-load root)))
    (mailx--validate-rule rule)
    (when (seq-some (lambda (r) (eq (mailx-rule-id r) id)) existing)
      (error "Rule ID already exists: %s" id))
    (mailx-rules-save root (append existing (list rule)))
    (mailx-rules-export-all root)
    (list :status "ok" :id (symbol-name id) :total_rules (length (mailx-rules-load root)))))

(defun mailx-mcp--stage-triage-plan (root title items)
  "Create a `mailx' hitl-plan for ITEMS in ROOT."
  (setq root (mailx-mcp--root root))
  (unless (and (listp items) items)
    (error "Items must be a non-empty list of targets"))
  (let* ((targets (seq-map (lambda (item)
                             (list :root root
                                   :message-id (map-elt item :message_id)
                                   :context (map-elt item :context)
                                   :subject (map-elt item :subject)
                                   :path (map-elt item :path)))
                           items))
         (plan (hitl-plan-create :domain 'mailx
                                 :title (or title "mcp-mailx-triage")
                                 :targets targets)))
    (list :status "ok" :plan_id (format "%s" plan))))

;;;###autoload
(defun mailx-mcp-register ()
  "Register the mailx MCP service and tools."
  (let ((service (or (mcpkit-get-service 'mailx)
                     (mcpkit-define-service 'mailx :description "Mail classification inspection and triage"))))
    (mcpkit-register-tool list_unclassified service
      :description "Inspect unclassified messages in an authorized mail root."
      :input-schema '(:type "object"
                      :properties (:root (:type "string")
                                   :limit (:type "integer" :minimum 1 :maximum 100))
                      :required ["root"])
      (mailx-mcp--list-messages (map-elt args :root)
                                (or (map-elt args :limit) 25)))
    (mcpkit-register-tool test_rule service
      :description "Dry-run a candidate mailx rule against historical mail in an authorized root."
      :input-schema '(:type "object"
                      :properties (:root (:type "string")
                                   :rule (:type "object"
                                          :properties (:match (:type "array"))
                                          :required ["match"]))
                      :required ["root" "rule"])
      (mailx-mcp--test-rule (map-elt args :root) (map-elt args :rule)))
    (mcpkit-register-tool add_rule service
      :description "Add and persist a new classification rule to an authorized mail root."
      :input-schema '(:type "object"
                      :properties (:root (:type "string")
                                   :rule (:type "object"
                                          :properties (:id (:type "string")
                                                       :match (:type "array")
                                                       :folder (:type "string")
                                                       :tier (:type "string")
                                                       :tags (:type "array" :items (:type "string")))
                                          :required ["id" "match"]))
                      :required ["root" "rule"])
      (mailx-mcp--add-rule (map-elt args :root) (map-elt args :rule)))
    (mcpkit-register-tool stage_triage_plan service
      :description "Stage a batch of mail messages into a reviewable hitl-plan."
      :input-schema '(:type "object"
                      :properties (:root (:type "string")
                                   :title (:type "string")
                                   :items (:type "array" :items (:type "object")))
                      :required ["root" "items"])
      (mailx-mcp--stage-triage-plan (map-elt args :root)
                                    (map-elt args :title)
                                    (map-elt args :items)))
    service))

(provide 'mailx-mcp)
;;; mailx-mcp.el ends here
