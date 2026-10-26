;;; gptel-mcp.el --- Gptel tool-calling bridge for mcpkit -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0") (gptel "0.9.0"))
;; Keywords: tools, mcp, gptel, llm
;; URL: https://github.com/tychoish/gptel-mcp

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Bridges mcpkit.el service/tool registrations onto gptel's tool-calling
;; framework, so that tools defined via `mcpkit-register-tool' can be made
;; available to an LLM through `gptel-make-tool' without re-declaring their
;; argument schemas or handlers.
;;
;; Usage:
;;
;;   (gptel-mcp-register-service-tools 'emacs)
;;
;; registers a gptel tool (via `gptel-make-tool') for every `mcpkit-tool' on
;; the `emacs' mcpkit service, grouped under a gptel tool category matching
;; the service name.
;;
;; To automatically inject mcpkit tools into gptel chat sessions:
;;
;;   (gptel-mcp-mode 1)
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mcpkit)
(require 'gptel)

;;; Customization

(defgroup gptel-mcp nil
  "Gptel tool-calling bridge for mcpkit."
  :group 'gptel
  :prefix "gptel-mcp-")

(defcustom gptel-mcp-default-services '(emacs)
  "List of mcpkit service name symbols to enable in gptel sessions by default.
Can be a list of symbols such as `'(emacs denote)', or the symbol `all'
or t to enable all registered services."
  :type '(choice (const :tag "All services" all)
                 (repeat :tag "Services" symbol))
  :group 'gptel-mcp)

(defcustom gptel-mcp-auto-enable-tools t
  "Whether to enable tool-calling automatically in gptel sessions.
When non-nil, `gptel-mcp-setup-session' enables `gptel-use-tools' buffer-locally
and populates `gptel-tools' with tools from `gptel-mcp-default-services'."
  :type 'boolean
  :group 'gptel-mcp)

;;; Schema Conversion

(defun gptel-mcp--keyword-name (key)
  "Return the property name string for KEY (a keyword or symbol)."
  (if (keywordp key)
      (substring (symbol-name key) 1)
    (format "%s" key)))

(defun gptel-mcp--convert-schema-node (node)
  "Recursively convert JSON-schema :type string values in NODE to symbols.
NODE is a JSON-schema plist fragment (or vector/array of such fragments), as
used in `mcpkit-tool-input-schema'.  gptel requires :type values to be
symbols (e.g. `string', `integer') rather than the JSON-schema strings
mcpkit uses internally (e.g. \"string\", \"integer\")."
  (cond
   ((vectorp node)
    (vconcat (seq-map #'gptel-mcp--convert-schema-node node)))
   ((and (consp node) (keywordp (car node)))
    (let (result)
      (cl-loop for (k v) on node by #'cddr
               do (push k result)
                  (push (if (and (eq k :type) (stringp v))
                            (intern v)
                          (gptel-mcp--convert-schema-node v))
                        result))
      (nreverse result)))
   (t node)))

(defun gptel-mcp--schema-to-args (input-schema)
  "Convert mcpkit INPUT-SCHEMA (a JSON-schema plist) into gptel :args.
Returns a list of plists suitable for `gptel-make-tool's :args slot: one
plist per JSON-schema property, with :name, :type (a symbol), :description,
and any of :enum / :items / :properties carried over (with nested :type
strings converted to symbols).  A property is marked :optional t unless its
name string appears in the schema's :required list/vector."
  (let* ((properties (plist-get input-schema :properties))
         (required (append (plist-get input-schema :required) nil))
         args)
    (cl-loop for (key spec) on properties by #'cddr
             do (let* ((prop-name (gptel-mcp--keyword-name key))
                       (converted (gptel-mcp--convert-schema-node spec))
                       (optional (not (member prop-name required)))
                       (arg (append (list :name prop-name) converted)))
                  (when optional
                    (setq arg (append arg (list :optional t))))
                  (push arg args)))
    (nreverse args)))

(defun gptel-mcp--make-tool-function (tool args)
  "Build a gptel-async :function wrapper around mcpkit TOOL.
ARGS is the gptel :args plist-list built by `gptel-mcp--schema-to-args'
for TOOL; it fixes the order in which the generated function expects its
positional argument values."
  (let ((arg-names (mapcar (lambda (a) (plist-get a :name)) args)))
    (lambda (callback &rest arg-values)
      (let (handler-args)
        (cl-loop for name in arg-names
                 for val in arg-values
                 do (setq handler-args
                          (plist-put handler-args (intern (concat ":" name)) val)))
        (condition-case err
            (funcall (mcpkit-tool-handler tool)
                     handler-args
                     (lambda (error-string result)
                       (if error-string
                           (funcall callback (format "Error: %s" error-string))
                         (funcall callback result))))
          (error
           (funcall callback (format "Error: %s" (error-message-string err)))))))))

;;; Registration

;;;###autoload
(defun gptel-mcp-register-service-tools (service-name)
  "Register a gptel tool for every `mcpkit-tool' on SERVICE-NAME.
SERVICE-NAME is resolved via `mcpkit-get-service' (a symbol, string, or
`mcpkit-service' instance).  Each generated gptel tool wraps the underlying
mcpkit tool's handler, translating its JSON-schema `input-schema' into
gptel's :args format and its callback-based `handler' into gptel's
asynchronous :function calling convention.  Tools are grouped in gptel's UI
under a :category matching the service name.

Return the list of `gptel-tool' structs created."
  (let ((service (mcpkit-get-service service-name)))
    (unless service
      (user-error "mcpkit service `%s' not found" service-name))
    (let ((category (format "%s" (mcpkit-service-name service)))
          created)
      (maphash
       (lambda (_name tool)
         (let* ((args (gptel-mcp--schema-to-args (mcpkit-tool-input-schema tool)))
                (function (gptel-mcp--make-tool-function tool args))
                (gtool (gptel-make-tool
                        :name (mcpkit-tool-name tool)
                        :function function
                        :description (mcpkit-tool-description tool)
                        :args args
                        :async t
                        :category category)))
           (push gtool created)))
       (mcpkit-service-tools service))
      (nreverse created))))

;;;###autoload
(defun gptel-mcp-register-all-services (&optional load-services)
  "Register gptel tools for every service currently in `mcpkit-registry'.
When LOAD-SERVICES is non-nil or when `mcpkit-registry' is empty,
calls `mcpkit-load-all-services' first if available.
Return an alist of (SERVICE-NAME . TOOLS) for each registered service."
  (interactive)
  (when (and (or load-services (null (and (boundp 'mcpkit-registry) mcpkit-registry)))
             (fboundp 'mcpkit-load-all-services))
    (mcpkit-load-all-services))
  (let (results)
    (dolist (entry (and (boundp 'mcpkit-registry) mcpkit-registry) (nreverse results))
      (let ((name (car entry)))
        (condition-case err
            (let ((tools (gptel-mcp-register-service-tools name)))
              (push (cons name tools) results))
          (error
           (message "[gptel-mcp]: failed to register service `%s`: %s"
                    name (error-message-string err))))))))

;;;###autoload
(defun gptel-mcp-get-tools (&optional services)
  "Return a list of `gptel-tool' structs for SERVICES.
If SERVICES is nil, uses `gptel-mcp-default-services'.
If SERVICES is `all' or t, collects tools from all registered services.
SERVICES can also be a single symbol or a list of service name symbols.
Tools are retrieved from `gptel--known-tools'."
  (let* ((all-services (mapcar #'car (and (boundp 'mcpkit-registry) mcpkit-registry)))
         (target-services
          (cond
           ((or (eq services 'all) (eq services t)) all-services)
           ((null services)
            (if (or (eq gptel-mcp-default-services 'all) (eq gptel-mcp-default-services t))
                all-services
              gptel-mcp-default-services))
           ((listp services) services)
           (t (list services))))
         tools)
    (dolist (svc-name target-services (nreverse tools))
      (when-let* ((category (format "%s" svc-name))
                  (category-tools (cdr (assoc category gptel--known-tools))))
        (dolist (tool-entry category-tools)
          (push (cdr tool-entry) tools))))))

;;;###autoload
(defun gptel-mcp-setup-session (&optional buffer services)
  "Configure BUFFER (defaults to current buffer) for mcpkit tool calling.
Registers all mcpkit services if not already registered, sets `gptel-use-tools'
buffer-locally to t (if `gptel-mcp-auto-enable-tools' is non-nil), and populates
`gptel-tools' buffer-locally with tools for SERVICES (defaulting to
`gptel-mcp-default-services')."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (gptel-mcp-register-all-services)
    (when gptel-mcp-auto-enable-tools
      (setq-local gptel-use-tools t))
    (let ((tools (gptel-mcp-get-tools services)))
      (when tools
        (setq-local gptel-tools
                    (cl-delete-duplicates (append gptel-tools tools)
                                          :key #'gptel-tool-name
                                          :test #'equal))))
    (when (called-interactively-p 'interactive)
      (message "[gptel-mcp]: configured %d tools for buffer `%s'"
               (length gptel-tools) (buffer-name)))))

;;;###autoload
(defun gptel-mcp-enable (&optional services)
  "Enable mcpkit tools for SERVICES in the current gptel buffer."
  (interactive)
  (gptel-mcp-setup-session (current-buffer) services)
  (message "[gptel-mcp]: enabled %d tools in %s"
           (length gptel-tools) (buffer-name)))

;;;###autoload
(defun gptel-mcp-disable ()
  "Remove all mcpkit tools from the current buffer's `gptel-tools'."
  (interactive)
  (let ((all-categories (mapcar (lambda (e) (format "%s" (car e)))
                                (and (boundp 'mcpkit-registry) mcpkit-registry))))
    (setq-local gptel-tools
                (cl-remove-if (lambda (tool)
                                (member (gptel-tool-category tool) all-categories))
                              gptel-tools))
    (message "[gptel-mcp]: disabled MCP tools in %s (remaining tools: %d)"
             (buffer-name) (length gptel-tools))))

;;;###autoload
(define-minor-mode gptel-mcp-mode
  "Global minor mode to automatically inject mcpkit tools into gptel chat sessions.
When enabled, attaches `gptel-mcp-setup-session' to `gptel-mode-hook'."
  :global t
  :group 'gptel-mcp
  (if gptel-mcp-mode
      (add-hook 'gptel-mode-hook #'gptel-mcp-setup-session)
    (remove-hook 'gptel-mode-hook #'gptel-mcp-setup-session)))

(provide 'gptel-mcp)
;;; gptel-mcp.el ends here
