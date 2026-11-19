;;; mcpkit-search.el --- Native tool search and proxy collation for mcpkit -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: comm, tools, mcp, search
;; URL: https://github.com/tychoish/mcpkit.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; mcpkit-search provides native search, filtering, and collation for MCP tools.
;; When an MCP server registers dozens of tools, exposing the full catalog
;; over `tools/list' consumes large numbers of context tokens.
;;
;; This library introduces:
;; 1. Multi-token candidate generation and search powered by `orderless-filter'
;;    (with a pure-Elisp fallback when orderless is unavailable).
;; 2. Exact-match and prefix-boosted result re-ranking.
;; 3. Collation of proxy/sprite instances to deduplicate tools across sub-daemons.
;; 4. A synthetic `search_tools' MCP tool for on-demand tool discovery.
;; 5. A synthetic `invoke_tool' MCP tool that calls a discovered (local or
;;    proxied) tool by name.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'map)
(require 'logx)
(require 'mcpkit)
(require 'orderless nil t)

(defvar orderless-matching-styles)

(declare-function mcpkit-proxy-list-routes "mcpkit-proxy")
(declare-function mcpkit-proxy--forward-request "mcpkit-proxy")

(defgroup mcpkit-search nil
  "Native tool search and proxy collation for mcpkit."
  :group 'mcpkit
  :prefix "mcpkit-search-")

(defcustom mcpkit-search-matching-styles
  '(orderless-literal orderless-regexp orderless-flex)
  "Orderless matching styles used for tool filtering."
  :type '(repeat symbol)
  :group 'mcpkit-search)

(defcustom mcpkit-search-default-limit 5
  "Default maximum number of tools returned by search queries."
  :type 'integer
  :group 'mcpkit-search)

;;; Candidate Generation & Parameter Extraction

(defun mcpkit-search--tool-parameter-names (tool)
  "Extract parameter names as strings from TOOL's input schema."
  (let* ((schema (mcpkit-tool-input-schema tool))
         (props (when (listp schema) (plist-get schema :properties))))
    (cond
     ((hash-table-p props)
      (hash-table-keys props))
     ((and (listp props) (keywordp (car props)))
      (let (names)
        (while props
          (let ((key (car props)))
            (when (keywordp key)
              (push (substring (symbol-name key) 1) names)))
          (setq props (cddr props)))
        (nreverse names)))
     ((listp props)
      (mapcar (lambda (entry)
                (if (consp entry)
                    (format "%s" (car entry))
                  (format "%s" entry)))
              props))
     (t nil))))

(defun mcpkit-search--format-instances-note (instances)
  "Format a human-readable instance availability string for INSTANCES list."
  (when (and instances (> (length instances) 1))
    (if (> (length instances) 6)
        (format "\n\n[Available in %d active processes; pass target-id via route or header to target a specific process]"
                (length instances))
      (format "\n\n[Available in processes: %s]"
              (string-join instances ", ")))))

(defun mcpkit-search--tool-candidate-string (tool &optional service-name instances)
  "Format a searchable candidate string for TOOL.
SERVICE-NAME overrides the tool's owning service if provided.
INSTANCES is an optional list of process identifiers."
  (let* ((name (mcpkit-tool-name tool))
         (svc (or service-name (mcpkit-tool-service tool) "core"))
         (desc (or (mcpkit-tool-description tool) ""))
         (params (mcpkit-search--tool-parameter-names tool))
         (param-str (if params (format " (params: %s)" (string-join params " ")) ""))
         (inst-str (if (and instances (> (length instances) 1))
                       (format " [instances: %s]" (string-join instances " "))
                     "")))
    (format "%s [%s]: %s%s%s" name svc desc param-str inst-str)))

;;; Filtering & Ranking

(defun mcpkit-search--fallback-filter (query strings)
  "Filter STRINGS against QUERY tokens without `orderless'."
  (let* ((clean-q (string-trim query))
         (tokens (split-string (downcase clean-q) "[ \t]+" t)))
    (if (null tokens)
        strings
      (seq-filter
       (lambda (cand)
         (let ((cand-lower (downcase cand)))
           (seq-every-p (lambda (tok)
                          (string-match-p (regexp-quote tok) cand-lower))
                        tokens)))
       strings))))

(defun mcpkit-search--filter (query strings)
  "Filter STRINGS matching QUERY using `orderless' or fallback."
  (if (fboundp 'orderless-filter)
      (let ((orderless-matching-styles (or mcpkit-search-matching-styles
                                           '(orderless-literal orderless-regexp orderless-flex))))
        (orderless-filter query strings))
    (mcpkit-search--fallback-filter query strings)))

(defun mcpkit-search--score-tool (query tool)
  "Calculate a numeric relevance score for TOOL matching QUERY.
Higher score indicates greater relevance."
  (let* ((q (downcase (string-trim query)))
         (name (downcase (mcpkit-tool-name tool)))
         (desc (downcase (or (mcpkit-tool-description tool) "")))
         (params (mapcar #'downcase (mcpkit-search--tool-parameter-names tool)))
         (score 0))
    ;; Tool name exact match
    (when (string= name q)
      (setq score (+ score 1000)))
    ;; Tool name prefix match
    (when (and (not (string= name q)) (string-prefix-p q name))
      (setq score (+ score 500)))
    ;; Substring in tool name
    (when (and (not (string-prefix-p q name)) (string-match-p (regexp-quote q) name))
      (setq score (+ score 250)))
    ;; Multi-token name matches
    (let ((tokens (split-string q "[ \t]+" t)))
      (when (> (length tokens) 1)
        (let ((name-matches (seq-count (lambda (tok) (string-match-p (regexp-quote tok) name)) tokens)))
          (setq score (+ score (* name-matches 100))))))
    ;; Match in description
    (when (string-match-p (regexp-quote q) desc)
      (setq score (+ score 100)))
    ;; Match in parameter names
    (when (seq-some (lambda (p) (string-match-p (regexp-quote q) p)) params)
      (setq score (+ score 75)))
    ;; Tie breaker: shorter tool names preferred
    (+ score (max 0 (- 50 (min 50 (length name)))))))

(defun mcpkit-search--rank-tools (query tools)
  "Sort TOOLS by relevance score to QUERY in descending order."
  (let ((scored (mapcar (lambda (tool)
                          (cons (mcpkit-search--score-tool query tool) tool))
                        tools)))
    (mapcar #'cdr
            (sort scored (lambda (a b) (> (car a) (car b)))))))

;;; Collation & Proxy Integration

(defvar mcpkit-search--proxy-catalogs-cache (make-hash-table :test #'equal)
  "Cache of proxy sprite tool catalogs: target-id -> (timestamp . tools).")

(defcustom mcpkit-search-proxy-cache-ttl 30.0
  "Time in seconds to cache remote proxy route tool lists."
  :type 'number
  :group 'mcpkit-search)

(defun mcpkit-search-clear-proxy-cache ()
  "Clear cached proxy tool catalogs."
  (interactive)
  (clrhash mcpkit-search--proxy-catalogs-cache))

(defun mcpkit-search--fetch-proxy-tools (target-id port)
  "Fetch tools list from localhost:PORT for TARGET-ID.
Return `mcpkit-tool' list, or nil on failure or timeout without
signaling an error."
  (condition-case err
      (let* ((url-request-method "POST")
             (url-request-extra-headers
              '(("Content-Type" . "application/json; charset=utf-8")))
             (body (mcpkit--serialize-response
                    (list :jsonrpc "2.0" :id "search-proxy-probe" :method "tools/list" :params (make-hash-table))))
             (url-request-data (encode-coding-string body 'utf-8))
             (target-url (format "http://127.0.0.1:%d/mcp" port))
             (buf (url-retrieve-synchronously target-url t t 0.5)))
        (when buf
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (when (re-search-forward "\r?\n\r?\n" nil t)
                  (let* ((json-str (buffer-substring-no-properties (point) (point-max)))
                         (parsed (mcpkit--parse-request json-str))
                         (res (plist-get parsed :result))
                         (raw-tools (plist-get res :tools)))
                    (when (vectorp raw-tools)
                      (mapcar
                       (lambda (raw)
                         (make-mcpkit-tool
                          :name (plist-get raw :name)
                          :description (plist-get raw :description)
                          :input-schema (plist-get raw :inputSchema)
                          :service (intern (format "proxy:%s" target-id))))
                       (append raw-tools nil))))))
            (kill-buffer buf))))
    (error
     (logx-warn :cat 'mcpkit-search
                :msg (format "Failed to probe proxy target %s on port %d: %s"
                             target-id port (error-message-string err))
                :target target-id
                :port port
                :error (error-message-string err))
     nil)))

(defun mcpkit-search--get-proxy-tools ()
  "Collect and return all active proxy route tools, consulting the TTL cache."
  (let ((tools nil))
    (when (fboundp 'mcpkit-proxy-list-routes)
      (let ((routes (mcpkit-proxy-list-routes))
            (now (float-time)))
        (dolist (route routes)
          (let* ((target-id (plist-get route :target_id))
                 (port (plist-get route :port)))
            (cond
             ((fboundp 'mcpkit-proxy-get-cached-tools)
              (when-let* ((fetched (mcpkit-proxy-get-cached-tools target-id)))
                (setq tools (nconc tools (copy-sequence fetched)))))
             (t
              (let ((cached (gethash target-id mcpkit-search--proxy-catalogs-cache)))
                (if (and cached (< (- now (car cached)) mcpkit-search-proxy-cache-ttl))
                    (setq tools (nconc tools (copy-sequence (cdr cached))))
                  (when-let* ((fetched (mcpkit-search--fetch-proxy-tools target-id port)))
                    (puthash target-id (cons now fetched) mcpkit-search--proxy-catalogs-cache)
                    (setq tools (nconc tools (copy-sequence fetched))))))))))))
    tools))

(defun mcpkit-search--collate-tools (local-tools proxy-tools)
  "Collate LOCAL-TOOLS and PROXY-TOOLS into deduplicated canonical tools.
Attaches instance lists and appends availability notes to descriptions."
  (let ((tool-map (make-hash-table :test #'equal))
        (order nil))
    ;; Process local tools first
    (dolist (tool local-tools)
      (let ((name (mcpkit-tool-name tool)))
        (unless (gethash name tool-map)
          (push name order)
          (puthash name (list :tool (copy-mcpkit-tool tool) :instances (list "local")) tool-map))))
    ;; Merge proxy tools
    (dolist (tool proxy-tools)
      (let* ((name (mcpkit-tool-name tool))
             (svc (symbol-name (or (mcpkit-tool-service tool) 'proxy)))
             (inst (if (string-prefix-p "proxy:" svc)
                       (format "sprite:%s" (substring svc 6))
                     svc))
             (entry (gethash name tool-map)))
        (if entry
            (let ((instances (plist-get entry :instances)))
              (unless (member inst instances)
                (setq entry (plist-put entry :instances (append instances (list inst))))
                (puthash name entry tool-map)))
          (push name order)
          (puthash name (list :tool (copy-mcpkit-tool tool) :instances (list inst)) tool-map))))
    ;; Reconstruct canonical tool structs with collated instance metadata
    (mapcar
     (lambda (name)
       (let* ((entry (gethash name tool-map))
              (canonical (plist-get entry :tool))
              (instances (plist-get entry :instances)))
         (when (and instances (> (length instances) 1))
           (let ((orig-desc (or (mcpkit-tool-description canonical) ""))
                 (note (mcpkit-search--format-instances-note instances)))
             (setf (mcpkit-tool-description canonical) (concat orig-desc note))))
         canonical))
     (nreverse order))))

;;; Search Entry Point

(cl-defun mcpkit-search-query (query &key (limit mcpkit-search-default-limit)
                                     service
                                     (include-proxies t)
                                     active-services)
  "Search registered MCP tools matching QUERY string.
LIMIT specifies the maximum number of matches returned.
SERVICE optionally filters by owning service name (string or symbol).
INCLUDE-PROXIES controls collation of proxy sprite tools (default t).
ACTIVE-SERVICES optionally provides service set
\(defaults to `mcpkit--active-services')."
  (let* ((services (or active-services mcpkit--active-services))
         (local-tools (mcpkit--normalize-tools services))
         (proxy-tools (when include-proxies (mcpkit-search--get-proxy-tools)))
         (all-tools (if proxy-tools
                        (mcpkit-search--collate-tools local-tools proxy-tools)
                      local-tools))
         ;; Filter by service if specified
         (filtered-tools
          (if service
              (let ((target-svc (if (symbolp service) (symbol-name service) (format "%s" service))))
                (seq-filter (lambda (tool)
                              (let ((svc-sym (mcpkit-tool-service tool)))
                                (and svc-sym (string= (symbol-name svc-sym) target-svc))))
                            all-tools))
            all-tools)))
    (if (or (null query) (string-empty-p (string-trim query)))
        (seq-take filtered-tools limit)
      ;; Build candidate strings and mapping
      (let* ((cand-map (make-hash-table :test #'equal))
             (candidates
              (mapcar (lambda (tool)
                        (let ((cand-str (mcpkit-search--tool-candidate-string tool)))
                          (puthash cand-str tool cand-map)
                          cand-str))
                      filtered-tools))
             (matched-cands (mcpkit-search--filter query candidates))
             (matched-tools (delq nil (mapcar (lambda (c) (gethash c cand-map)) matched-cands)))
             (ranked (mcpkit-search--rank-tools query matched-tools)))
        (seq-take ranked limit)))))

;;; Synthetic `search_tools' Definition

(defun mcpkit-search--format-tool-summary (tool)
  "Format a concise text description of TOOL for search results."
  (let* ((name (mcpkit-tool-name tool))
         (desc (or (mcpkit-tool-description tool) ""))
         (params (mcpkit-search--tool-parameter-names tool))
         (param-str (if params (format "  Parameters: %s\n" (string-join params ", ")) "")))
    (format "- %s: %s\n%s" name desc param-str)))

(defun mcpkit-search-tools-handler (args &optional done)
  "Handle execution of synthetic `search_tools' with ARGS and DONE."
  (let* ((query (plist-get args :query))
         (limit (or (plist-get args :limit) mcpkit-search-default-limit))
         (service (plist-get args :service))
         (include-proxies (if (plist-member args :include_proxies)
                              (not (eq (plist-get args :include_proxies) :json-false))
                            t))
         (results (mcpkit-search-query (or query "")
                                       :limit limit
                                       :service service
                                       :include-proxies include-proxies))
         (text-lines
          (if (null results)
              (format "No tools found matching query: %S" (or query ""))
            (concat (format "Found %d matching tool%s:\n\n"
                            (length results)
                            (if (= (length results) 1) "" "s"))
                    (mapconcat #'mcpkit-search--format-tool-summary results "\n")
                    "\nTo call any of these tools, use the invoke_tool tool with "
                    "`name' (the tool name above) and `arguments' (an object "
                    "matching the tool's parameters).")))
         (tool-descriptors
          (vconcat
           (mapcar (lambda (tool)
                     (list :name (mcpkit-tool-name tool)
                           :description (mcpkit-tool-description tool)
                           :inputSchema (mcpkit--wire-input-schema
                                         (or (mcpkit-tool-input-schema tool)
                                             '(:type "object")))))
                   results)))
         (res (list :content (vector (list :type "text" :text text-lines))
                    :tools tool-descriptors)))
    (if done
        (funcall done nil res)
      res)))

(defun mcpkit-search-make-search-tool ()
  "Construct the synthetic `search_tools' `mcpkit-tool' definition."
  (make-mcpkit-tool
   :name "search_tools"
   :description "Search available MCP tools using keywords, names, descriptions, or parameters."
   :input-schema
   '(:type "object"
     :properties
     (:query
      (:type "string" :description "Search query keywords or pattern")
      :limit
      (:type "integer" :description "Maximum number of tools to return (default 5)")
      :service
      (:type "string" :description "Optional service name to filter by")
      :include_proxies
      (:type "boolean" :description "Whether to include tools from proxy sprite routes (default true)"))
     :required ["query"])
   :handler #'mcpkit-search-tools-handler
   :service 'core))

;;; Synthetic `invoke_tool' Definition

(defun mcpkit-search--keep-args (args)
  "Return ARGS unchanged.
Used as the `invoke_tool' decoder so the target tool's own decoder sees
the original JSON-derived arguments."
  args)

(defun mcpkit-search--find-proxy-tool (name target-id)
  "Return the proxy `mcpkit-tool' named NAME, or nil.
When TARGET-ID is non-nil, only consider tools from that proxy target."
  (seq-find (lambda (tool)
              (and (equal (mcpkit-tool-name tool) name)
                   (or (null target-id)
                       (equal (format "%s" (mcpkit-tool-service tool))
                              (format "proxy:%s" target-id)))))
            (mcpkit-search--get-proxy-tools)))

(defun mcpkit-search--call-proxy-tool (tool args)
  "Call proxy TOOL with ARGS via its backend route.
Return the content list of the result, or signal an error."
  (let* ((svc (format "%s" (mcpkit-tool-service tool)))
         (target-id (substring svc (length "proxy:")))
         (route (seq-find (lambda (r) (equal (format "%s" (plist-get r :target_id)) target-id))
                          (mcpkit-proxy-list-routes)))
         (body (mcpkit--serialize-response
                (list :jsonrpc "2.0" :id "invoke-tool" :method "tools/call"
                      :params (list :name (mcpkit-tool-name tool)
                                    :arguments (or args (make-hash-table))))))
         (reply (progn
                  (unless route
                    (error "No route registered for target: %s" target-id))
                  (mcpkit-proxy--forward-request (plist-get route :port) body)))
         (parsed (mcpkit--parse-request (cdr reply)))
         (err (plist-get parsed :error)))
    (when err
      (error "%s" (or (plist-get err :message) "proxy tool call failed")))
    (append (plist-get (plist-get parsed :result) :content) nil)))

(defun mcpkit-search--local-tool-p (name)
  "Return non-nil if NAME is a tool in the active services."
  (seq-some (lambda (tool) (equal (mcpkit-tool-name tool) name))
            (mcpkit--normalize-tools mcpkit--active-services)))

(defun mcpkit-search-invoke-tool-handler (args &optional done)
  "Handle execution of synthetic `invoke_tool' with ARGS and DONE.
ARGS has `:name' (tool name from `search_tools') and `:arguments' (object).
Signal an error for an unknown tool, a malformed `:arguments', or missing
required parameters; otherwise return (or pass to DONE) the tool's content."
  (let* ((name (plist-get args :name))
         (arguments (plist-get args :arguments))
         (target-id (plist-get args :target_id))
         (outcome nil)
         (finish (lambda (err content)
                   (setq outcome (cons err content))
                   (when done
                     (funcall done err content)))))
    (cond
     ((or (not (stringp name)) (string-empty-p name))
      (funcall finish "Invalid params: missing or invalid tool name" nil))
     ((member name '("invoke_tool" "search_tools"))
      (funcall finish (format "Tool %s cannot be invoked via invoke_tool; call it directly" name) nil))
     ((and arguments (not (and (listp arguments) (keywordp (car arguments)))))
      (funcall finish "Invalid params: arguments must be an object" nil))
     ((and (null target-id) (mcpkit-search--local-tool-p name))
      (mcpkit--handle-tools-call
       "invoke-tool" (list :name name :arguments arguments) mcpkit--active-services
       (lambda (status resp &rest _)
         (cond
          ((plist-get resp :error)
           (funcall finish (plist-get (plist-get resp :error) :message) nil))
          ((eq status 'success)
           (funcall finish nil (append (plist-get (plist-get resp :result) :content) nil)))
          (t
           (funcall finish (format "Tool call failed: %s" name) nil))))))
     ((and (fboundp 'mcpkit-proxy-list-routes)
           (mcpkit-search--find-proxy-tool name target-id))
      (condition-case err
          (let* ((tool (mcpkit-search--find-proxy-tool name target-id))
                 (missing (mcpkit--missing-required-argument tool arguments)))
            (cond
             (missing
              (funcall finish (format "Invalid params: missing required parameter: %s" missing) nil))
             (t
              (funcall finish nil (mcpkit-search--call-proxy-tool tool arguments)))))
        (error
         (funcall finish (error-message-string err) nil))))
     (t
      (funcall finish (format "Unknown tool: %s; use search_tools to find available tools" name) nil)))
    (cond
     (done nil)
     ((car outcome)
      (error "%s" (car outcome)))
     (t
      (cdr outcome)))))

(defun mcpkit-search-make-invoke-tool ()
  "Construct the synthetic `invoke_tool' `mcpkit-tool' definition."
  (make-mcpkit-tool
   :name "invoke_tool"
   :description "Invoke a tool discovered with search_tools, by name, passing its arguments as an object."
   :input-schema
   '(:type "object"
     :properties
     (:name
      (:type "string" :description "Tool name exactly as returned by search_tools")
      :arguments
      (:type "object" :description "Arguments for the tool, matching its parameters")
      :target_id
      (:type "string" :description "Optional proxy target id to route the call to"))
     :required ["name"])
   :handler #'mcpkit-search-invoke-tool-handler
   :decode #'mcpkit-search--keep-args
   :service 'core))

;;; Synthetic `discover_services' Definition

(defun mcpkit-search-discover-services-handler (_args &optional done)
  "Return high-level catalog of active services and tool counts."
  (let* ((active (seq-map (lambda (e) (if (consp e) (car e) e))
                          (and (boundp 'mcpkit--active-services) mcpkit--active-services)))
         (registered (and (boundp 'mcpkit-registry) (seq-map #'cdr mcpkit-registry)))
         (seen (make-hash-table :test #'equal))
         (services nil))
    (dolist (svc (or active registered))
      (when (and svc (mcpkit-service-p svc))
        (let ((sname (symbol-name (mcpkit-service-name svc))))
          (unless (map-elt seen sname)
            (setf (map-elt seen sname) t)
            (push svc services)))))
    (setq services (nreverse services))
    (let* ((entries (mapcar (lambda (svc)
                              (list :service (symbol-name (mcpkit-service-name svc))
                                    :description (or (mcpkit-service-description svc) "")
                                    :tool_count (hash-table-count (mcpkit-service-tools svc))
                                    :port (mcpkit-service-port svc)))
                            services))
           (text (if entries
                     (mapconcat (lambda (e)
                                  (format "- %s (%d tools): %s"
                                          (plist-get e :service)
                                          (plist-get e :tool_count)
                                          (plist-get e :description)))
                                entries "\n")
                   "No services currently registered."))
           (res (list :content (vector (list :type "text" :text text))
                      :services (vconcat entries))))
      (if done (funcall done nil res) res))))

(defun mcpkit-search-make-discover-services-tool ()
  "Construct the synthetic `discover_services' `mcpkit-tool' definition."
  (make-mcpkit-tool
   :name "discover_services"
   :description "Discover available MCP services, their capabilities, and tool counts without dumping individual schemas."
   :input-schema '(:type "object" :properties #s(hash-table test equal))
   :handler #'mcpkit-search-discover-services-handler
   :decode #'mcpkit-search--keep-args
   :service 'core))

(provide 'mcpkit-search)

;;; mcpkit-search.el ends here
