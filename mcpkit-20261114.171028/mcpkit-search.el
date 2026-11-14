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

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'mcpkit)
(require 'orderless nil t)

(defvar orderless-matching-styles)

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
     (message "mcpkit-search: failed to probe proxy target %s on port %d: %s"
              target-id port (error-message-string err))
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
                    (mapconcat #'mcpkit-search--format-tool-summary results "\n"))))
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

(provide 'mcpkit-search)

;;; mcpkit-search.el ends here
