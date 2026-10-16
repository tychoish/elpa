;;; mcpkit-proxy.el --- HTTP reverse-proxy/router for mcpkit services -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0"))
;; Keywords: comm, tools, mcp, rpc
;; URL: https://github.com/tychoish/mcpkit.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; mcpkit-proxy.el implements a thin HTTP reverse-proxy/router that sits in
;; front of one or more mcpkit MCP services running on localhost.  Requests
;; arrive at a single parent-side gateway (started with
;; `mcpkit-proxy-start') and are forwarded, verbatim, to whichever backend
;; is registered for the opaque "target-id" named in the request.
;;
;; Routing is deliberately target-agnostic: a `target-id' is just an opaque
;; string key into a routing table maintained with
;; `mcpkit-proxy-register-route' / `mcpkit-proxy-unregister-route'.  This
;; module has no notion of what a target-id "is" -- other packages (e.g. a
;; daemon-manager living in a separate repository) decide that and register
;; routes accordingly.  Nothing in this file should be read as assuming
;; targets are sprite daemons specifically, even though the URI convention
;; itself (a product-level decision made elsewhere) happens to use the
;; literal string "sprite" in its path shape.
;;
;; Primary routing is via the URI path `/sprite/<target-id>/mcp'.
;; Secondary routing via the `X-Sprite-ID' HTTP header is supported only
;; when `mcpkit-proxy-enable-header-routing' is explicitly enabled; it
;; remains off by default.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'url)
(require 'mcpkit)

(defgroup mcpkit-proxy nil
  "HTTP reverse-proxy/router for mcpkit services."
  :group 'mcpkit
  :prefix "mcpkit-proxy-")

(defcustom mcpkit-proxy-port 8765
  "Default TCP port for the mcpkit-proxy gateway."
  :type 'integer
  :group 'mcpkit-proxy)

(defcustom mcpkit-proxy-enable-header-routing nil
  "When non-nil, allow routing via the `X-Sprite-ID' HTTP header.
By default only URI-path routing (`/sprite/<target-id>/mcp') is honored.
Header-based routing is a secondary/fallback mechanism, only consulted
when no path match is found, and must be explicitly opted into by
setting this to non-nil -- it stays off by default."
  :type 'boolean
  :group 'mcpkit-proxy)

;;; Route Registry

(defvar mcpkit-proxy--routes (make-hash-table :test #'equal)
  "Hash table mapping opaque target-id strings to backend TCP ports.")

(defvar mcpkit-proxy--server nil
  "The live `ws-server' instance backing the proxy gateway, or nil.")

(defconst mcpkit-proxy--route-path-rx
  "\\`/sprite/\\([^/]+\\)/mcp\\'"
  "Regexp matching proxy request paths, capturing the target-id group.")

(defconst mcpkit-proxy--http-method-keywords
  '(:GET :HEAD :POST :PUT :DELETE :TRACE)
  "Keyword keys `ws-parse' uses for the HTTP method+path header entry.
The (METHOD . PATH) pair is not guaranteed to be the first element of a
`ws-request' headers alist -- the struct's default initform is `(list
nil)', so real parsed headers commonly carry a leading nil cell.")

;;;###autoload
(defun mcpkit-proxy-register-route (target-id port)
  "Register a routing rule mapping TARGET-ID to localhost:PORT."
  (puthash target-id port mcpkit-proxy--routes))

;;;###autoload
(defun mcpkit-proxy-unregister-route (target-id)
  "Remove the routing rule for TARGET-ID."
  (remhash target-id mcpkit-proxy--routes))

(defun mcpkit-proxy--route-port (target-id)
  "Return the backend port registered for TARGET-ID, or nil."
  (and target-id (gethash target-id mcpkit-proxy--routes)))

;;;###autoload
(defun mcpkit-proxy-list-routes ()
  "Return a list of plists `(:target_id ID :port PORT)' for all registered routes."
  (let ((routes nil))
    (maphash (lambda (id port)
               (push (list :target_id id :port port) routes))
             mcpkit-proxy--routes)
    (nreverse routes)))

;;; Request Parsing & Route Resolution

(defun mcpkit-proxy--header-value (headers key)
  "Return the raw string value for KEY (a keyword) in HEADERS alist."
  (cdr (assoc key headers)))

(defun mcpkit-proxy--request-path (headers)
  "Return the requested URI path from HEADERS alist, or nil."
  (let ((entry (seq-find (lambda (h) (and (consp h) (memq (car h) mcpkit-proxy--http-method-keywords)))
                         headers)))
    (and entry (cdr entry))))

(defun mcpkit-proxy--target-from-path (path)
  "Return the target-id string parsed from PATH, or nil if unmatched."
  (when (and path (string-match mcpkit-proxy--route-path-rx path))
    (match-string 1 path)))

(defun mcpkit-proxy--target-from-header (headers)
  "Return a target-id string from the `X-Sprite-ID' header in HEADERS.
Only consulted when `mcpkit-proxy-enable-header-routing' is non-nil."
  (when mcpkit-proxy-enable-header-routing
    (let ((val (mcpkit-proxy--header-value headers :X-SPRITE-ID)))
      (and (stringp val) (not (string-empty-p val)) val))))

(defun mcpkit-proxy--resolve-target (headers)
  "Resolve a target-id from HEADERS.
Returns a cons (TARGET-ID . SOURCE) where SOURCE is `path', `header', or
nil when no routing information (path match or, if enabled, header)
could be found."
  (let* ((path (mcpkit-proxy--request-path headers))
         (path-target (mcpkit-proxy--target-from-path path)))
    (cond
     (path-target (cons path-target 'path))
     (t (let ((header-target (mcpkit-proxy--target-from-header headers)))
          (if header-target
              (cons header-target 'header)
            (cons nil nil)))))))

;;; Reverse Proxy Transport

(defun mcpkit-proxy--forward-request (port body)
  "Forward BODY as an HTTP POST to localhost:PORT/mcp.
Return a cons (STATUS-CODE . RESPONSE-BODY-STRING).  Signals an error on
transport failure (connection refused, timeout, etc)."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json; charset=utf-8")))
         (url-request-data (encode-coding-string (or body "") 'utf-8))
         (target-url (format "http://127.0.0.1:%d/mcp" port))
         (buf (url-retrieve-synchronously target-url t t 10)))
    (unless buf
      (error "No response from backend at %s" target-url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          ;; `url-retrieve-synchronously' does not signal an error on a
          ;; refused/dead connection -- it silently returns an empty
          ;; buffer with no HTTP status line at all.  Treat "no status
          ;; line" as a hard failure (rather than defaulting to 200, as
          ;; this used to do) so a dead backend surfaces as a real
          ;; forwarding error instead of masquerading as a successful,
          ;; empty-bodied response.
          (let ((status-code
                 (if (looking-at "HTTP/[0-9.]+[[:space:]]+\\([0-9]+\\)")
                     (string-to-number (match-string 1))
                   (error "No response from backend at %s" target-url))))
            (goto-char (point-min))
            (cons status-code
                  (if (re-search-forward "\r?\n\r?\n" nil t)
                      (buffer-substring-no-properties (point) (point-max))
                    ""))))
      (kill-buffer buf))))

;;; Response Helpers

(defun mcpkit-proxy--send-json (proc code json-string)
  "Send JSON-STRING as an HTTP CODE response body to PROC."
  (let ((bytes (string-bytes json-string)))
    (ws-response-header proc code
                        '("Content-Type" . "application/json; charset=utf-8")
                        (cons "Content-Length" (number-to-string bytes)))
    (process-send-string proc json-string)))

(defun mcpkit-proxy--extract-id (body)
  "Best-effort extraction of the JSON-RPC `id' field from BODY, or nil."
  (condition-case nil
      (plist-get (mcpkit--parse-request body) :id)
    (error nil)))

(defun mcpkit-proxy--error-body (id code message)
  "Serialize a JSON-RPC 2.0 error response for ID, CODE, and MESSAGE."
  (mcpkit--serialize-response (mcpkit--make-error-response id code message)))

;;; HTTP Handler

;;;###autoload
(defun mcpkit-proxy-handler (request)
  "HTTP request handler for the mcpkit-proxy gateway.
REQUEST is a `ws-request' instance.  Resolves a target-id from the
request's URI path (primary) or `X-Sprite-ID' header (secondary, opt-in
via `mcpkit-proxy-enable-header-routing'), then reverse-proxies the
request body to the registered backend and returns its response
verbatim.  Missing routing information or an unregistered target-id
produce structured JSON-RPC error responses instead of a crash."
  (let* ((proc (oref request process))
         (body (oref request body))
         (headers (oref request headers))
         (path (mcpkit-proxy--request-path headers))
         (resolved (mcpkit-proxy--resolve-target headers))
         (target-id (car resolved))
         (id (mcpkit-proxy--extract-id body)))
    (cond
     ((or (equal path "/routes") (equal path "/sprite/routes") (equal path "/mcp/routes"))
      (let* ((routes (mcpkit-proxy-list-routes))
             (json-str (json-serialize (list :routes (vconcat routes)
                                             :count (length routes)))))
        (mcpkit-proxy--send-json proc 200 json-str)))
     ((null target-id)
      (let ((msg (format "mcpkit-proxy: missing/unsupported routing information (path=%s, header-routing=%s)"
                         (or (mcpkit-proxy--request-path headers) "<none>")
                         (if mcpkit-proxy-enable-header-routing "enabled" "disabled"))))
        (mcpkit--log 'error nil 0.0 msg)
        (mcpkit-proxy--send-json
         proc 400
         (mcpkit-proxy--error-body id mcpkit-error-invalid-request msg))))
     (t
      (let ((port (mcpkit-proxy--route-port target-id)))
        (cond
         ((null port)
          (let ((msg (format "mcpkit-proxy: no backend registered for target `%s'" target-id)))
            (mcpkit--log 'error target-id 0.0 msg)
            (mcpkit-proxy--send-json
             proc 502
             (mcpkit-proxy--error-body id mcpkit-error-internal-error msg))))
         (t
          (condition-case err
              (let* ((result (mcpkit-proxy--forward-request port body))
                     (status (car result))
                     (resp-body (cdr result)))
                (mcpkit--log 'success target-id 0.0 nil)
                (mcpkit-proxy--send-json proc status resp-body))
            (error
             (let ((msg (format "mcpkit-proxy: forwarding to target `%s' (port %s) failed: %s"
                                target-id port (error-message-string err))))
               (mcpkit--log 'error target-id 0.0 msg)
               (mcpkit-proxy--send-json
                proc 502
                (mcpkit-proxy--error-body id mcpkit-error-internal-error msg))))))))))))

;;; Transport Lifecycle

;;;###autoload
(defun mcpkit-proxy-start (&optional port)
  "Start the mcpkit-proxy HTTP gateway on PORT (default `mcpkit-proxy-port').
Reuses the same `web-server.el' machinery `mcpkit-start-service' relies
on, rather than reinventing an HTTP layer."
  (interactive)
  (let ((listen-port (or port mcpkit-proxy-port)))
    (unless mcpkit-proxy--server
      (setq mcpkit-proxy--server
            (ws-start (list (cons (lambda (_req) t) #'mcpkit-proxy-handler))
                      listen-port
                      "*mcpkit-proxy*"
                      :host 'local)))
    mcpkit-proxy--server))

;;;###autoload
(defun mcpkit-proxy-stop ()
  "Stop the mcpkit-proxy HTTP gateway, if running."
  (interactive)
  (when mcpkit-proxy--server
    (ignore-errors (ws-stop mcpkit-proxy--server))
    (setq mcpkit-proxy--server nil)))

(provide 'mcpkit-proxy)
;;; mcpkit-proxy.el ends here
