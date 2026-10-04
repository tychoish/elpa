;;; hitl-mcp.el --- Human-in-the-Loop MCP service integration for mcpkit -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0") (hitl "0.1.0"))
;; Keywords: tools, mcp, hitl, agent, ai
;; URL: https://github.com/tychoish/hitl

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Exposes the universal `hitl' Human-in-the-Loop question queue as a Model
;; Context Protocol (MCP) service via `mcpkit.el'.
;;
;; External AI agents and background workers can post questions, poll answers,
;; iterate over pending items using cursor protocols, and cancel questions
;; over native HTTP JSON-RPC 2.0.
;;
;; Tools are registered at top-level on the `hitl' service upon loading.
;; Services are started only when explicitly requested via `mcpkit-start-service'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mcpkit)
(require 'hitl)

(defgroup hitl-mcp nil
  "Human-in-the-Loop MCP service integration for mcpkit."
  :group 'hitl
  :prefix "hitl-mcp-")

(defcustom hitl-mcp-port 8766
  "Default TCP port for the HITL MCP service."
  :type 'integer
  :group 'hitl-mcp)

;;; Service Definition & Top-Level Tool Registration

(defvar hitl-mcp-service
  (or (mcpkit-get-service 'hitl)
      (mcpkit-define-service 'hitl
        :port hitl-mcp-port
        :description "Human-in-the-Loop Question and Approval Queue"))
  "The HITL `mcpkit-service' instance.")

;; 1. ask_user
(mcpkit-register-tool 'ask_user 'hitl
  :description "Ask human operator a question and wait for asynchronous response."
  :input-schema '(:type "object"
                  :properties (:prompt (:type "string" :description "Prompt text shown to user")
                               :kind (:type "string" :description "Question type: single-choice, multi-choice, text, boolean, confirm, file, number")
                               :options (:type "array" :items (:type "string") :description "List of choices")
                               :default_value (:type "string" :description "Default fallback value")
                               :target (:type "string" :description "Target shell buffer name or context identifier")
                               :target_shell (:type "string" :description "Legacy alias for target shell name")
                               :timeout (:type "integer" :description "Timeout in seconds"))
                  :required ["prompt"])
  (let* ((prompt (plist-get args :prompt))
         (kind (plist-get args :kind))
         (options (plist-get args :options))
         (default-val (plist-get args :default_value))
         (target (or (plist-get args :target) (plist-get args :target_shell)))
         (timeout (plist-get args :timeout))
         (q (hitl-ask :prompt prompt
                      :kind (if kind (intern (format ":%s" (string-remove-prefix ":" kind))) :single-choice)
                      :options (when (vectorp options) (seq-into options 'list))
                      :default-value default-val
                      :target target
                      :timeout timeout)))
    (list :question_id (hitl-question-id q)
          :status (symbol-name (hitl-question-status q)))))

;; 2. poll_question
(mcpkit-register-tool 'poll_question 'hitl
  :description "Poll the status and response of a question by ID."
  :input-schema '(:type "object"
                  :properties (:question_id (:type "string" :description "Question ID to poll"))
                  :required ["question_id"])
  (let* ((qid (plist-get args :question_id))
         (q (hitl-get qid)))
    (if (null q)
        (list :error (format "Question %s not found" qid))
      (list :question_id qid
            :status (symbol-name (hitl-question-status q))
            :response (hitl-question-response q)))))

;; 3. get_next_question
(mcpkit-register-tool 'get_next_question 'hitl
  :description "Get next pending human question using cursor iteration."
  :input-schema '(:type "object"
                  :properties (:cursor_id (:type "string" :description "Cursor ID string")
                               :target (:type "string" :description "Target context filter")
                               :target_shell (:type "string" :description "Legacy alias for target filter")))
  (let* ((cid (plist-get args :cursor_id))
         (target (or (plist-get args :target) (plist-get args :target_shell)))
         (q (hitl-cursor-next cid target)))
    (if (null q)
        (list :status "none_pending")
      (list :question_id (hitl-question-id q)
            :prompt (hitl-question-prompt q)
            :kind (symbol-name (hitl-question-kind q))
            :options (hitl-question-options q)
            :status (symbol-name (hitl-question-status q))))))

;; 4. list_pending_questions
(mcpkit-register-tool 'list_pending_questions 'hitl
  :description "List all pending questions."
  :input-schema '(:type "object"
                  :properties (:target (:type "string" :description "Optional target context filter")))
  (let* ((target (or (plist-get args :target) (plist-get args :target_shell)))
         (pending (hitl-list-pending target)))
    (mapcar (lambda (q)
              (list :question_id (hitl-question-id q)
                    :prompt (hitl-question-prompt q)
                    :kind (symbol-name (hitl-question-kind q))
                    :status (symbol-name (hitl-question-status q))))
            pending)))

;; 5. cancel_question
(mcpkit-register-tool 'cancel_question 'hitl
  :description "Cancel a pending question."
  :input-schema '(:type "object"
                  :properties (:question_id (:type "string" :description "Question ID to cancel")
                               :reason (:type "string" :description "Optional reason for cancellation"))
                  :required ["question_id"])
  (let* ((qid (plist-get args :question_id))
         (reason (plist-get args :reason))
         (q (hitl-cancel qid reason)))
    (if q
        (list :question_id qid :status "cancelled")
      (list :error (format "Question %s not found" qid)))))

;; 6. answer_question
(mcpkit-register-tool 'answer_question 'hitl
  :description "Submit an answer response for a pending question."
  :input-schema '(:type "object"
                  :properties (:question_id (:type "string" :description "Question ID to answer")
                               :response (:type "string" :description "Answer payload"))
                  :required ["question_id" "response"])
  (let* ((qid (plist-get args :question_id))
         (resp (plist-get args :response)))
    (condition-case err
        (let ((q (hitl-answer qid resp)))
          (list :question_id (hitl-question-id q)
                :status (symbol-name (hitl-question-status q))
                :response (hitl-question-response q)))
      (error
       (list :error (error-message-string err))))))

;;;###autoload
(defun hitl-mcp-register ()
  "Ensure `hitl-mcp-service' is registered in `mcpkit-registry' and return it."
  (interactive)
  (unless (mcpkit-get-service 'hitl)
    (mcpkit-register-service hitl-mcp-service))
  hitl-mcp-service)

;; Backward compatibility alias for legacy mcpkit-ask
(defalias 'mcpkit-register-ask-tools #'hitl-mcp-register)

(provide 'hitl-mcp)
;;; hitl-mcp.el ends here
