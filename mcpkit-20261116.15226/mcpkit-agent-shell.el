;;; mcpkit-agent-shell.el --- Optional agent-shell bridge -*- lexical-binding: t; -*-


;;; Commentary:
;; Opt-in integration between MCPKit and agent-shell.  Per-agent server lists
;; take precedence over the global list.  Caller environment entries take
;; precedence over the bridge's defaults.  Loading this file starts nothing.

;;; Code:
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'mcpkit-services)

(declare-function agent-shell--mcp-servers "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function mcpkit-proxy-list-routes "mcpkit-proxy")
(declare-function ws-process "web-server")

(defvar agent-shell-mcp-servers)
(defvar agent-shell--state)
(defvar sprite-instance-id)
(defvar server-name)
(defvar mcpkit-proxy--server)
(defvar mcpkit-agent-shell-mode)

(defgroup mcpkit-agent-shell nil
  "Optional agent-shell integration."
  :group 'mcpkit)

(defun mcpkit-agent-shell--port ()
  "Return the current direct port, starting services if needed.
Reads the active port set when the MCP service starts; if called in the narrow
startup window before services are up, initializes them synchronously."
  (or (mcpkit-active-port)
      (progn
        (ignore-errors (mcpkit-init-services-sync))
        (mcpkit-active-port))))

(defun mcpkit-agent-shell--proxy-port (port instance)
  "Return a live proxy port for INSTANCE routed to PORT, if available."
  (when (and instance (bound-and-true-p mcpkit-proxy--server)
             (fboundp 'mcpkit-proxy-list-routes)
             (seq-some (lambda (route)
                         (and (equal (plist-get route :target_id) instance)
                              (equal (plist-get route :port) port)))
                       (mcpkit-proxy-list-routes)))
    (let ((proc (ws-process mcpkit-proxy--server)))
      (when (and (processp proc) (process-live-p proc))
        (plist-get (process-contact proc t) :service)))))

(defun mcpkit-agent-shell--server-spec ()
  "Resolve the current MCPKit endpoint as an ACP server, or nil."
  (when-let* ((port (mcpkit-agent-shell--port)))
    (let* ((instance (bound-and-true-p sprite-instance-id))
           (proxy (mcpkit-agent-shell--proxy-port port instance))
           (url (if proxy
                    (format "http://127.0.0.1:%d/sprite/%s/mcp" proxy instance)
                  (format "http://127.0.0.1:%d/mcp" port))))
      `((name . "emacs") (type . "http") (url . ,url) (headers . ())))))

(defvar mcpkit-agent-shell--resolver
  (lambda () (mcpkit-agent-shell--server-spec))
  "Identity of the entry installed in `agent-shell-mcp-servers'.")

(defun mcpkit-agent-shell--resolver-p (entry)
  "Return non-nil if ENTRY is an MCPKit resolver function."
  (or (eq entry mcpkit-agent-shell--resolver)
      (equal entry mcpkit-agent-shell--resolver)
      (and (functionp entry)
           (not (symbolp entry))
           (or (equal entry (lambda () (mcpkit-agent-shell--server-spec)))
               (string-match-p "mcpkit-agent-shell--server-spec" (format "%s" entry))))))

(defun mcpkit-agent-shell--environment ()
  "Return environment defaults for the current direct endpoint."
  (when-let* ((port (mcpkit-agent-shell--port)))
    (list (format "MCPKIT_PORT=%d" port)
          (format "EMACS_MCP_PORT=%d" port)
          (format "EMACS_MCP_URL=http://127.0.0.1:%d/mcp" port)
          (format "EMACS_SPRITE_ID=%s" (or (bound-and-true-p sprite-instance-id) (system-name)))
          (format "EMACS_SERVER_NAME=%s" (or (bound-and-true-p server-name) "server")))))

(defun mcpkit-agent-shell--resolve-servers (original &rest args)
  "Resolve our entry before ORIGINAL, omitting it when unavailable.
ARGS are passed to ORIGINAL.  Agent-specific lists are left untouched."
  (if (or (map-nested-elt agent-shell--state '(:agent-config :mcp-servers))
          (not (seq-some #'mcpkit-agent-shell--resolver-p agent-shell-mcp-servers)))
      (apply original args)
    (let* ((spec (funcall mcpkit-agent-shell--resolver))
           (replaced nil)
           (agent-shell-mcp-servers
            (seq-mapcat (lambda (entry)
                          (if (mcpkit-agent-shell--resolver-p entry)
                              (if (not replaced)
                                  (progn (setq replaced t) (and spec (list spec)))
                                nil)
                            (list entry)))
                        agent-shell-mcp-servers)))
      (let ((resolved (apply original args)))
        (when (and spec
                   (> (length (seq-filter (lambda (entry)
                                            (equal (map-elt entry 'name) "emacs"))
                                          resolved))
                      1))
          (user-error "MCPKit agent-shell: an MCP server named emacs already exists"))
        resolved))))

(defun mcpkit-agent-shell--inject-environment (original &rest args)
  "Add defaults to ORIGINAL's ARGS without replacing caller entries."
  (let* ((env (plist-get args :environment-variables))
         (defaults (mcpkit-agent-shell--environment))
         (missing (seq-remove
                   (lambda (entry)
                     (let ((key (concat (car (split-string entry "=")) "=")))
                       (seq-some (lambda (existing) (string-prefix-p key existing)) env)))
                   defaults)))
    (apply original (plist-put (copy-sequence args) :environment-variables
                               (append env missing)))))

(defun mcpkit-agent-shell--install ()
  "Install bridge after agent-shell is loaded, if still enabled."
  (when mcpkit-agent-shell-mode
    (when (seq-some (lambda (entry)
                      (and (listp entry) (not (functionp entry))
                           (equal (map-elt entry 'name) "emacs")))
                    agent-shell-mcp-servers)
      (mcpkit-agent-shell-mode -1)
      (user-error "MCPKit agent-shell: an MCP server named emacs already exists"))
    (setq agent-shell-mcp-servers
          (cons mcpkit-agent-shell--resolver
                (seq-remove #'mcpkit-agent-shell--resolver-p agent-shell-mcp-servers)))
    (advice-add 'agent-shell--mcp-servers :around #'mcpkit-agent-shell--resolve-servers)
    (advice-add 'agent-shell--make-acp-client :around #'mcpkit-agent-shell--inject-environment)))

;;;###autoload
(define-minor-mode mcpkit-agent-shell-mode
  "Globally bridge MCPKit into agent-shell sessions.
Per-agent server lists override the global list.  Explicit client environment
entries win over the MCPKit defaults.  Disabling leaves services running."
  :global t
  :group 'mcpkit-agent-shell
  (if mcpkit-agent-shell-mode
      (if (featurep 'agent-shell)
          (mcpkit-agent-shell--install)
        (with-eval-after-load 'agent-shell
          (mcpkit-agent-shell--install)))
    (when (boundp 'agent-shell-mcp-servers)
      (setq agent-shell-mcp-servers
            (seq-remove #'mcpkit-agent-shell--resolver-p agent-shell-mcp-servers)))
    (advice-remove 'agent-shell--mcp-servers #'mcpkit-agent-shell--resolve-servers)
    (advice-remove 'agent-shell--make-acp-client #'mcpkit-agent-shell--inject-environment)))

(provide 'mcpkit-agent-shell)
;;; mcpkit-agent-shell.el ends here
