;;; agent-shell-queue-train-mcp.el --- Train results, capture, and MCP -*- lexical-binding: t; -*-

;;; Commentary:
;; Public result submission API, multi-channel output capture adapter
;; (structured submission > provider protocol > validated artifact >
;; bounded unverified transcript fallback), and MCP tool registration for
;; durable trains on the `asq' service.
;;
;; Load-time pure: call `agent-shell-queue-train-mcp-register' to expose tools.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-queue-train-contract)
(require 'agent-shell-queue-train)
(require 'agent-shell-queue-dispatcher)

(declare-function mcpkit-register-tool "mcpkit" (&rest args))
(declare-function mcpkit-get-service "mcpkit" (name-or-service))

;;; Evidence Channel Capture Adapter

(defcustom agent-shell-queue-capture-transcript-max-bytes 16384
  "Maximum byte length retained for unverified transcript fallback segments."
  :type 'integer
  :group 'agent-shell-queue)

(defvar agent-shell-queue-train--attempt-evidence (make-hash-table :test #'equal)
  "Map of attempt-id to list of preserved evidence records.")

(cl-defun agent-shell-queue-capture-output (attempt-id channel payload
                                                       &key offset sequence-id now)
  "Record output evidence for ATTEMPT-ID from CHANNEL.
Preserves stronger channels without overwriting them.  CHANNEL is one of
`agent-shell-queue-train-evidence-channels'.  Transcript fallback is bounded
to `agent-shell-queue-capture-transcript-max-bytes' and marked `:verified nil'."
  (let* ((chan (agent-shell-queue-train--sym channel))
         (rank (agent-shell-queue-train-evidence-rank chan)))
    (unless rank
      (user-error "Unknown evidence channel: %S" channel))
    (let* ((bounded-payload
            (if (and (eq chan 'transcript-extraction) (stringp payload)
                     (> (string-bytes payload) agent-shell-queue-capture-transcript-max-bytes))
                (substring payload 0 agent-shell-queue-capture-transcript-max-bytes)
              payload))
           (digest (agent-shell-queue-train--digest
                    (list :channel chan :payload bounded-payload)))
           (rec (list :attempt-id attempt-id
                      :channel chan
                      :rank rank
                      :verified (eq chan 'validated-artifact)
                      :digest digest
                      :offset offset
                      :sequence-id sequence-id
                      :recorded-at (or now (float-time))
                      :payload bounded-payload))
           (existing (map-elt agent-shell-queue-train--attempt-evidence attempt-id))
           (updated (cons rec existing)))
      (setf (map-elt agent-shell-queue-train--attempt-evidence attempt-id) updated)
      rec)))

(defun agent-shell-queue-best-evidence (attempt-id)
  "Return the strongest preserved evidence record for ATTEMPT-ID, or nil."
  (let ((records (map-elt agent-shell-queue-train--attempt-evidence attempt-id))
        (best nil))
    (dolist (rec records)
      (when (or (null best)
                (agent-shell-queue-train-evidence-stronger-p
                 (plist-get rec :channel) (plist-get best :channel)))
        (setq best rec)))
    best))

;;; Public Result Submission

(cl-defun agent-shell-queue-submit-result (result &key allowed-roots now state-dir)
  "Submit structured agent RESULT for a claimed attempt.
Validates schema, execution token, and artifact roots.  Accepted submission
means recorded (`:recorded t :verified nil'), not domain-verified."
  (let ((check (agent-shell-queue-train-validate-result result allowed-roots)))
    (if (not (plist-get check :ok))
        (agent-shell-queue-train-make-outcome
         'invalid :errors (plist-get check :errors))
      (let* ((train-id (plist-get result :train-id))
             (train (map-elt agent-shell-queue-train--store train-id)))
        (if (null train)
            (agent-shell-queue-train-make-outcome
             'invalid :reason "train-not-found" :train-id train-id)
          (let* ((ts (or now (float-time)))
                 (ev (list :schema-version agent-shell-queue-train-schema-version
                           :event-id (format "res-%s-%s"
                                             (plist-get result :attempt-id)
                                             (plist-get result :idempotency-key))
                           :source "agent-result"
                           :type 'task-result
                           :train-id train-id
                           :generation (plist-get result :generation)
                           :observed-at ts
                           :received-at ts
                           :payload result))
                 (step (agent-shell-queue-train-step train ev ts))
                 (outcome (plist-get step :outcome))
                 (new-train (plist-get step :train)))
            (when (memq outcome '(accepted conflict))
              (setf (map-elt agent-shell-queue-train--store train-id) new-train)
              (when state-dir
                (agent-shell-queue-train-save-record new-train state-dir)))
            (when (eq outcome 'accepted)
              (agent-shell-queue-capture-output
               (plist-get result :attempt-id) 'structured-submission result :now ts))
            (agent-shell-queue-train-make-outcome
             outcome
             :train-id train-id
             :task-id (plist-get result :task-id)
             :attempt-id (plist-get result :attempt-id)
             :revision (plist-get new-train :revision)
             :recorded (eq outcome 'accepted)
             :verified nil
             :reason (plist-get step :reason))))))))

;;; Caller Capability Gate for Event Publication

(defconst agent-shell-queue-train-agent-allowed-event-types
  '(task-waiting observation)
  "Event types an agent caller may publish via `asq_event_publish'.
Agents cannot spoof human decisions or CI observer events.")

(defun agent-shell-queue-train-publish-event-checked (event caller-role &optional state-dir)
  "Publish EVENT to durable inbox after enforcing CALLER-ROLE capability.
CALLER-ROLE is `agent', `adapter', or `human'."
  (let ((ev-type (agent-shell-queue-train--sym (plist-get event :type)))
        (source (plist-get event :source)))
    (cond
     ((and (eq caller-role 'agent)
           (not (memq ev-type agent-shell-queue-train-agent-allowed-event-types)))
      (agent-shell-queue-train-make-outcome
       'forbidden :reason (format "Agent caller cannot publish event type %S" ev-type)))
     ((and (eq caller-role 'agent)
           (member source '("human" "github" "hitl" "coordinator")))
      (agent-shell-queue-train-make-outcome
       'forbidden :reason (format "Agent caller cannot spoof source %S" source)))
     (t
      (agent-shell-queue-publish-event event state-dir)))))

;;; MCP Tool Registration

;;;###autoload
(defun agent-shell-queue-train-mcp-register ()
  "Register durable train MCP tools on the `asq' mcpkit service."
  (unless (and (fboundp 'mcpkit-register-tool)
               (fboundp 'mcpkit-get-service)
               (mcpkit-get-service 'asq))
    (user-error "Service `asq' is not registered in mcpkit"))
  (eval
   '(progn
      (mcpkit-register-tool 'asq_train_list 'asq
        :description "List durable work trains (plain records, never starts work)."
        :input-schema '(:type "object"
                        :properties (:state (:type "string"
                                             :description "Optional state filter (staged, active, paused, ...)")))
        :handler (lambda (args)
                   (agent-shell-queue-train-list (plist-get args :state))))
      (mcpkit-register-tool 'asq_train_status 'asq
        :description "Inspect one durable train by ID without side effects."
        :input-schema '(:type "object"
                        :properties (:id (:type "string"))
                        :required ["id"])
        :handler (lambda (args)
                   (or (agent-shell-queue-train-status (plist-get args :id))
                       (agent-shell-queue-train-make-outcome 'invalid :reason "train-not-found"))))
      (mcpkit-register-tool 'asq_train_control 'asq
        :description "Control a staged/active/paused train (activate, pause, resume, cancel) with revision check."
        :input-schema '(:type "object"
                        :properties (:id (:type "string")
                                     :action (:type "string"
                                              :enum ["activate" "pause" "resume" "cancel"])
                                     :expected_revision (:type "integer")
                                     :interrupt (:type "string" :enum ["after-turn" "interrupt"]))
                        :required ["id" "action"])
        :handler (lambda (args)
                   (let ((id (plist-get args :id))
                         (rev (plist-get args :expected_revision)))
                     (pcase (plist-get args :action)
                       ("activate" (agent-shell-queue-train-activate id :expected-revision rev))
                       ("pause" (agent-shell-queue-train-pause id :expected-revision rev))
                       ("resume" (agent-shell-queue-train-resume id :expected-revision rev))
                       ("cancel" (agent-shell-queue-train-cancel
                                  id :expected-revision rev
                                  :interrupt (intern (or (plist-get args :interrupt) "after-turn"))))
                       (other (agent-shell-queue-train-make-outcome
                               'invalid :reason (format "unknown-action:%s" other)))))))
      (mcpkit-register-tool 'asq_train_propose_tasks 'asq
        :description "Propose a validated task graph patch for human review (never auto-approves)."
        :input-schema '(:type "object"
                        :properties (:id (:type "string")
                                     :expected_revision (:type "integer")
                                     :patch (:type "object"))
                        :required ["id" "patch"])
        :handler (lambda (args)
                   (agent-shell-queue-train-add-tasks
                    (plist-get args :id)
                    (plist-get args :patch)
                    :expected-revision (plist-get args :expected_revision)
                    :authorized-p nil)))
      (mcpkit-register-tool 'asq_event_publish 'asq
        :description "Publish a typed event to the durable inbox with caller capability checks."
        :input-schema '(:type "object"
                        :properties (:event (:type "object"))
                        :required ["event"])
        :handler (lambda (args)
                   (agent-shell-queue-train-publish-event-checked
                    (plist-get args :event) 'agent)))
      (mcpkit-register-tool 'asq_result_submit 'asq
        :description "Submit a structured partial or final attempt result using the claimed execution token. Recorded does not mean verified."
        :input-schema '(:type "object"
                        :properties (:result (:type "object"))
                        :required ["result"])
        :handler (lambda (args)
                   (agent-shell-queue-submit-result (plist-get args :result))))
      (mcpkit-register-tool 'asq_dispatch_status 'asq
        :description "Look up a durable dispatch token (returns not-found, found, or ambiguous)."
        :input-schema '(:type "object"
                        :properties (:token (:type "string"))
                        :required ["token"])
        :handler (lambda (args)
                   (agent-shell-queue-find-dispatch (plist-get args :token)))))
   t))

(provide 'agent-shell-queue-train-mcp)
;;; agent-shell-queue-train-mcp.el ends here
