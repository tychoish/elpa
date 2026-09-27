;;; mcpkit-emacs.el --- Emacs Core MCP service integration for mcpkit -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0"))
;; Keywords: tools, mcp, emacs
;; URL: https://github.com/tychoish/mcpkit.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Exposes core Emacs management, buffer inspection, and safe evaluation tools
;; as an MCP service via mcpkit.el.
;;
;; Tools are registered at top-level on the `emacs' service upon loading.
;; Services are started only when explicitly requested via `mcpkit-start-service'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mcpkit)

(defgroup mcpkit-emacs nil
  "Emacs Core MCP service integration for mcpkit."
  :group 'mcpkit
  :prefix "mcpkit-emacs-")

(defcustom mcpkit-emacs-port 8765
  "Default TCP port for the Emacs MCP service."
  :type 'integer
  :group 'mcpkit-emacs)

;;; Service Definition & Top-Level Tool Registration

(defvar mcpkit-emacs-service
  (or (mcpkit-get-service 'emacs)
      (mcpkit-define-service 'emacs
        :port mcpkit-emacs-port
        :description "GNU Emacs Core Management and Inspection"))
  "The Emacs `mcpkit-service' instance.")

;; 1. emacs_server_status
(mcpkit-register-tool 'emacs_server_status 'emacs
  :description "Return Emacs process status, versions, uptime, and active mcpkit services."
  :input-schema '(:type "object" :properties ())
  (ignore args)
  (list :emacs_version emacs-version
        :system_type (symbol-name system-type)
        :daemon_p (if (daemonp) t :json-false)
        :server_name (or (bound-and-true-p server-name) "none")
        :uptime_seconds (floor (float-time (time-subtract (current-time) before-init-time)))
        :active_mcp_services (vconcat (seq-map (lambda (entry)
                                                 (symbol-name (mcpkit-service-name (car entry))))
                                               (bound-and-true-p mcpkit--active-services)))))

;; 2. emacs_get_buffer
(mcpkit-register-tool 'emacs_get_buffer 'emacs
  :description "Inspect buffer metadata and contents by name or visited file path."
  :input-schema '(:type "object"
                  :properties (:buffer_or_file (:type "string" :description "Buffer name or file path")
                               :max_chars (:type "integer" :description "Max characters to return"))
                  :required ["buffer_or_file"])
  (let* ((target (plist-get args :buffer_or_file))
         (max-c (plist-get args :max_chars))
         (buf (or (get-buffer target)
                  (find-buffer-visiting (expand-file-name target)))))
    (if (not buf)
        (list :found :json-false :target target)
      (with-current-buffer buf
        (let* ((size (buffer-size))
               (end-pos (if (and max-c (> size max-c)) (+ (point-min) max-c) (point-max)))
               (content (buffer-substring-no-properties (point-min) end-pos)))
          (list :found t
                :name (buffer-name)
                :file_name (or (buffer-file-name) "")
                :modified (if (buffer-modified-p) t :json-false)
                :major_mode (symbol-name major-mode)
                :size size
                :truncated (if (and max-c (> size max-c)) t :json-false)
                :content content))))))

;; 3. emacs_eval
(mcpkit-register-tool 'emacs_eval 'emacs
  :description "Safely evaluate an Emacs Lisp expression string with prompt suppression."
  :input-schema '(:type "object"
                  :properties (:expression (:type "string" :description "Emacs Lisp code to evaluate"))
                  :required ["expression"])
  (let* ((expr-str (plist-get args :expression)))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (let ((enable-local-variables nil)
            (revert-without-query '(".*"))
            (coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8))
        (condition-case err
            (let* ((read-expr (read expr-str))
                   (res (eval read-expr lexical-binding)))
              (list :status "success"
                    :result (format "%S" res)))
          (error
           (list :status "error"
                 :error (error-message-string err))))))))

;;;###autoload
(defun mcpkit-emacs-register ()
  "Ensure `mcpkit-emacs-service' is registered in `mcpkit-registry' and return it.
Kept for backward compatibility."
  (interactive)
  (unless (mcpkit-get-service 'emacs)
    (mcpkit-register-service mcpkit-emacs-service))
  mcpkit-emacs-service)

(provide 'mcpkit-emacs)
;;; mcpkit-emacs.el ends here
