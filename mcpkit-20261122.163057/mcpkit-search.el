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
(defvar mcpkit-stats-hidden-tools)
(declare-function mcpkit-stats-usage-entries "mcpkit-stats")

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

(defcustom mcpkit-search-structured-tools nil
  "When non-nil, `search_tools' also returns a structured `:tools' descriptor list.
When nil the descriptors are omitted, since the text output already
renders each tool's parameters, which avoids sending the same data twice."
  :type 'boolean
  :group 'mcpkit-search)

(defcustom mcpkit-search-catalog-in-description t
  "When non-nil, append a service catalog to the `search_tools' description.
The catalog lists each active service with its tool count, description and
most used tools, so clients can discover namespaces without a search."
  :type 'boolean
  :group 'mcpkit-search)

(defconst mcpkit-search--catalog-max-chars 1500
  "Approximate maximum length of the service catalog in characters.")

(defcustom mcpkit-search-no-match-hint
  "If nothing fits, `emacs__eval' can run arbitrary Elisp (last resort)."
  "Hint appended to `search_tools' output when no tool matches.
Nil or the empty string omits the hint."
  :type '(choice (const :tag "None" nil) string)
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

(defun mcpkit-search--tool-parameter-specs (tool)
  "Return a list of (NAME TYPE REQUIRED-P) for TOOL's input parameters.
TYPE is a string or nil when the schema does not give one."
  (let* ((schema (mcpkit-tool-input-schema tool))
         (props (when (listp schema) (plist-get schema :properties)))
         (req (when (listp schema) (plist-get schema :required)))
         (req-names (mapcar (lambda (r)
                              (let ((n (format "%s" r)))
                                (string-remove-prefix ":" n)))
                            (if (vectorp req) (append req nil) req))))
    (mapcar
     (lambda (pname)
       (let* ((key (intern (concat ":" pname)))
              (spec (cond
                     ((hash-table-p props) (gethash pname props))
                     ((and (listp props) (keywordp (car props))) (plist-get props key))
                     (t nil)))
              (type (when (and (listp spec) (keywordp (car spec)))
                      (plist-get spec :type))))
         (list pname
               (and type (format "%s" type))
               (and (member pname req-names) t))))
     (mcpkit-search--tool-parameter-names tool))))

(defun mcpkit-search--format-parameters (tool)
  "Render TOOL's parameters compactly as `name (type, required)' items."
  (mapconcat
   (lambda (spec)
     (let ((extras (delq nil (list (nth 1 spec) (and (nth 2 spec) "required")))))
       (if extras
           (format "%s (%s)" (car spec) (string-join extras ", "))
         (car spec))))
   (mcpkit-search--tool-parameter-specs tool)
   ", "))

(defun mcpkit-search--format-tool-summary (tool)
  "Format a concise text description of TOOL for search results."
  (let* ((name (mcpkit-tool-name tool))
         (desc (or (mcpkit-tool-description tool) ""))
         (params (mcpkit-search--format-parameters tool))
         (param-str (if (string-empty-p params) "" (format "  Parameters: %s\n" params))))
    (format "- %s: %s\n%s" name desc param-str)))

(defun mcpkit-search-tools-handler (args &optional done)
  "Handle execution of synthetic `search_tools' with ARGS and DONE."
  (let* ((query (plist-get args :query))
         (service (plist-get args :service))
         (limit (or (plist-get args :limit)
                    ;; An empty query with a service lists the whole service.
                    (if (and service
                             (not (equal service ""))
                             (string-empty-p (string-trim (or query ""))))
                        most-positive-fixnum
                      mcpkit-search-default-limit)))
         (include-proxies (if (plist-member args :include_proxies)
                              (not (eq (plist-get args :include_proxies) :json-false))
                            t))
         (results (mcpkit-search-query (or query "")
                                       :limit limit
                                       :service service
                                       :include-proxies include-proxies))
         (text-lines
          (if (null results)
              (concat "No matching tool."
                      (if (and (stringp mcpkit-search-no-match-hint)
                               (not (string-empty-p mcpkit-search-no-match-hint)))
                          (concat " " mcpkit-search-no-match-hint)
                        ""))
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
         (res (append (list :content (vector (list :type "text" :text text-lines)))
                      (when mcpkit-search-structured-tools
                        (list :tools tool-descriptors)))))
    (if done
        (funcall done nil res)
      res)))

;;; Service Catalog

(defconst mcpkit-search--base-description
  "Search available MCP tools using keywords, names, descriptions, or parameters."
  "Base sentence of the `search_tools' description.")

(defun mcpkit-search--bare-name (tool)
  "Return the name of TOOL without its `<service>__' prefix."
  (let ((name (mcpkit-tool-name tool))
        (svc (mcpkit-tool-service tool)))
    (if (and svc (string-prefix-p (format "%s__" svc) name))
        (substring name (+ 2 (length (symbol-name svc))))
      name)))

(defun mcpkit-search--catalog-line (entry popular-p)
  "Format catalog ENTRY (SERVICE COUNT DESC POPULAR); include POPULAR if POPULAR-P."
  (let ((desc (nth 2 entry))
        (popular (and popular-p (nth 3 entry))))
    (concat (format "- %s (%d tools)" (nth 0 entry) (nth 1 entry))
            (and desc (not (string-empty-p desc)) (concat ": " desc))
            (and popular
                 (format "%spopular: %s"
                         (if (and desc (not (string-empty-p desc))) "; " ": ")
                         (string-join popular ", "))))))

(defun mcpkit-search--catalog-fit (entries)
  "Join ENTRIES into catalog lines of about `mcpkit-search--catalog-max-chars'.
Popular lists are dropped first (last service first), then descriptions are
truncated, longest first."
  (let* ((entries (mapcar #'copy-sequence entries))
         (render (lambda (keep-popular)
                   (let ((i -1))
                     (mapconcat (lambda (e)
                                  (setq i (1+ i))
                                  (mcpkit-search--catalog-line e (< i keep-popular)))
                                entries "\n"))))
         (keep (length entries)))
    (while (and (> (length (funcall render keep)) mcpkit-search--catalog-max-chars)
                (> keep 0))
      (setq keep (1- keep)))
    (let ((excess (- (length (funcall render keep)) mcpkit-search--catalog-max-chars)))
      (while (and (> excess 0)
                  (let* ((longest (car (sort (copy-sequence entries)
                                             (lambda (a b) (> (length (nth 2 a))
                                                              (length (nth 2 b)))))))
                         (old (nth 2 longest)))
                    (when (and longest (> (length old) 12))
                      (let ((new (concat (substring old 0 (max 8 (- (length old) excess 3)))
                                         "...")))
                        (when (< (length new) (length old))
                          (setf (nth 2 longest) new)
                          t)))))
        (setq excess (- (length (funcall render keep)) mcpkit-search--catalog-max-chars))))
    (funcall render keep)))

(defun mcpkit-search--catalog-entries (services)
  "Return (SERVICE COUNT DESC POPULAR) entries for the active SERVICES."
  (let* ((tools (mcpkit--normalize-tools services))
         (hidden (append mcpkit--gateway-tool-names
                         (and (boundp 'mcpkit-stats-hidden-tools) mcpkit-stats-hidden-tools)))
         (usage (and (require 'mcpkit-stats nil t)
                     (fboundp 'mcpkit-stats-usage-entries)
                     (mcpkit-stats-usage-entries)))
         (order nil)
         (by-svc (make-hash-table :test #'eq)))
    (dolist (tool tools)
      (let ((svc (mcpkit-tool-service tool)))
        (when (and svc (not (member (mcpkit-tool-name tool) mcpkit--gateway-tool-names)))
          (unless (gethash svc by-svc) (push svc order))
          (push tool (gethash svc by-svc)))))
    (mapcar
     (lambda (svc)
       (let* ((stools (reverse (gethash svc by-svc)))
              (obj (or (seq-some (lambda (e)
                                   (let ((x (if (consp e) (car e) e)))
                                     (and (mcpkit-service-p x)
                                          (eq (mcpkit-service-name x) svc)
                                          x)))
                                 (if (mcpkit-service-p services) (list services) services))
                       (mcpkit-get-service svc)))
              (scored (delq nil
                            (mapcar (lambda (tool)
                                      (let ((sc (nth 2 (assoc (mcpkit-tool-name tool) usage))))
                                        (when (and sc (> sc 0)
                                                   (not (member (mcpkit-tool-name tool) hidden)))
                                          (cons (mcpkit-search--bare-name tool) sc))))
                                    stools)))
              (top (seq-take (sort scored (lambda (a b) (or (> (cdr a) (cdr b))
                                                            (and (= (cdr a) (cdr b))
                                                                 (string< (car a) (car b))))))
                             3)))
         (list (symbol-name svc) (length stools)
               (and obj (mcpkit-service-description obj))
               (mapcar #'car top))))
     (nreverse order))))

(defun mcpkit-search--catalog (&optional active-services)
  "Return the service catalog text for ACTIVE-SERVICES, or nil if empty.
The result is a snapshot kept in `mcpkit--catalog-snapshot' so the
description stays stable; see `mcpkit-search-refresh-usage-pins'."
  (let ((key (or active-services mcpkit--active-services)))
    (unless (and mcpkit--catalog-snapshot (eq (car mcpkit--catalog-snapshot) key))
      (setq mcpkit--catalog-snapshot
            (cons key (let ((entries (mcpkit-search--catalog-entries key)))
                        (and entries (mcpkit-search--catalog-fit entries))))))
    (cdr mcpkit--catalog-snapshot)))

(defun mcpkit-search-description (&optional active-services)
  "Return the `search_tools' description for ACTIVE-SERVICES."
  (let ((catalog (and mcpkit-search-catalog-in-description
                      (mcpkit-search--catalog active-services))))
    (if (null catalog)
        mcpkit-search--base-description
      (concat mcpkit-search--base-description
              "\n\nServices:\n" catalog
              "\nsearch_tools with `service' and an empty query lists every tool in that service."))))

;;;###autoload
(defun mcpkit-search-refresh-usage-pins ()
  "Recompute the usage pin snapshot and the `search_tools' catalog.
The snapshots are otherwise taken once after the services start and are
reset when a service starts or stops.  Return the usage-pinned names."
  (interactive)
  (mcpkit--invalidate-snapshots)
  (let ((names (mcpkit--usage-pin-names
                (mcpkit--normalize-tools mcpkit--active-services))))
    (mcpkit-search--catalog)
    (when (called-interactively-p 'interactive)
      (message "Usage pins: %s" (if names (string-join names ", ") "(none)")))
    names))

(defun mcpkit-search-make-search-tool (&optional active-services)
  "Construct the synthetic `search_tools' `mcpkit-tool' definition.
ACTIVE-SERVICES (default `mcpkit--active-services') feeds the catalog in
the description."
  (make-mcpkit-tool
   :name "search_tools"
   :description (mcpkit-search-description active-services)
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
  "Return non-nil if NAME resolves to a tool in the active services.
NAME may be namespaced, unprefixed (when unambiguous) or an alias; see
`mcpkit-resolve-tool-name'."
  (and (mcpkit-resolve-tool-name name) t))

(defun mcpkit-search--ambiguous-candidates (name)
  "Return the candidate names when NAME is an ambiguous bare tool name."
  (let ((cands (mcpkit--tool-name-candidates
                name (mcpkit--normalize-tools mcpkit--active-services))))
    (and (cdr cands) cands)))

(defun mcpkit-search-invoke-tool-handler (args &optional done)
  "Handle execution of synthetic `invoke_tool' with ARGS and DONE.
ARGS has `:name' (tool name from `search_tools') and `:arguments' (object).
Signal an error for an unknown tool, a malformed `:arguments', or missing
required parameters; otherwise return (or pass to DONE) the tool's content."
  (let* ((requested (plist-get args :name))
         (name (or (and (stringp requested) (null (plist-get args :target_id))
                        (mcpkit-resolve-tool-name requested))
                   requested))
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
     ((and (null target-id) (stringp name) (mcpkit-search--ambiguous-candidates name))
      (funcall finish (format "Ambiguous tool name: %s matches %s; use the full name"
                              name
                              (string-join (mcpkit-search--ambiguous-candidates name) ", "))
               nil))
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

(defun mcpkit-search--invoke-log-name (args active-services)
  "Return the target tool name to log for `invoke_tool' ARGS, or nil.
Proxy calls (with `:target_id') log the remote name as given; local calls
log the canonical name on ACTIVE-SERVICES."
  (let* ((args (and (listp args) args))
         (target (plist-get args :name)))
    (when (and (stringp target) (not (string-empty-p target)))
      (if (plist-get args :target_id)
          target
        (mcpkit--canonical-call-name target active-services)))))

(defun mcpkit-search-make-invoke-tool ()
  "Construct the synthetic `invoke_tool' `mcpkit-tool' definition."
  (make-mcpkit-tool
   :name "invoke_tool"
   :log-name #'mcpkit-search--invoke-log-name
   :description "Invoke a tool discovered with search_tools, by name, passing its arguments as an object."
   :input-schema
   '(:type "object"
     :properties
     (:name
      (:type "string" :description "Tool name as returned by search_tools (the unprefixed name also works when unambiguous)")
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
