;;; builder-mcp.el --- Model Context Protocol service for builder -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0"))
;; Keywords: tools, compilation, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Exposes builder task discovery, compilation execution, output buffer
;; retrieval, and asynchronous process awaiting over Model Context Protocol (MCP)
;; using `mcpkit'.
;;
;; Tools are registered at top-level on the `'builder' service upon loading.
;; Services merge into the centralized `mcpkit' HTTP server (default port 8765)
;; via `mcpkit-start-service' or `mcpkit-register-autostart' with `:on-collision 'namespace'.

;;; Code:

(require 'compile)
(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'mcpkit)
(require 'builder-core)

(defgroup builder-mcp nil
  "Model Context Protocol service for builder."
  :group 'builder
  :prefix "builder-mcp-")

(defvar builder-mcp-service
  (or (mcpkit-get-service 'builder)
      (mcpkit-define-service 'builder
        :description "Compilation and Task Runner Service"))
  "The builder `mcpkit-service' instance.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tool 1: discover_tasks

(mcpkit-register-tool 'discover_tasks 'builder
  :description "Discover available build, test, and lint tasks for a project directory."
  :input-schema '(:type "object"
                  :properties (:directory (:type "string" :description "Project root directory (defaults to current)")
                               :command (:type "string" :description "Optional command filter or rerun command"))
                  :required [])
  (let* ((dir (or (plist-get args :directory) default-directory))
         (cmd (plist-get args :command))
         (table (builder--get-candidates dir cmd))
         (tasks nil))
    (when (hash-table-p table)
      (maphash
       (lambda (_name cand)
         (push (list :name (builder-candidate-name cand)
                     :command (or (builder-candidate-command cand) (builder-candidate-name cand))
                     :directory (or (builder-candidate-directory cand) dir)
                     :priority (builder-candidate-priority cand)
                     :annotation (or (builder-candidate-annotation cand) ""))
               tasks))
       table))
    (seq-sort-by (lambda (t-item) (plist-get t-item :priority)) #'< tasks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tool 2: run_task

(mcpkit-register-tool 'run_task 'builder
  :description "Execute a build or test command in an Emacs compilation buffer."
  :input-schema '(:type "object"
                  :properties (:command (:type "string" :description "Shell command to run")
                               :directory (:type "string" :description "Working directory (defaults to current)")
                               :buffer_name (:type "string" :description "Custom compilation buffer name"))
                  :required ["command"])
  (let* ((cmd (plist-get args :command))
         (dir (or (plist-get args :directory) default-directory))
         (custom-buf (plist-get args :buffer_name))
         (target-name (or custom-buf
                          (format "*compilation*<%s>" (file-name-nondirectory (directory-file-name dir))))))
    (let ((compilation-buffer-name-function (lambda (_mode) target-name))
          (default-directory dir))
      (let ((buf (compilation-start cmd)))
        (list :buffer_name (buffer-name buf)
              :command cmd
              :directory dir
              :status "started")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tool 3: read_buffer

(mcpkit-register-tool 'read_buffer 'builder
  :description "Read output lines, status, and line count of a compilation buffer."
  :input-schema '(:type "object"
                  :properties (:buffer_name (:type "string" :description "Compilation buffer name")
                               :tail_lines (:type "integer" :description "Number of tail lines to return (default 100, 0 for all)"))
                  :required ["buffer_name"])
  (let* ((buf-name (plist-get args :buffer_name))
         (tail (or (plist-get args :tail_lines) 100))
         (buf (get-buffer buf-name)))
    (if (null buf)
        (list :buffer_name buf-name :exists nil :error "Buffer not found")
      (with-current-buffer buf
        (let* ((proc (get-buffer-process buf))
               (live (and proc (process-live-p proc)))
               (status (cond
                        (live "running")
                        ((and proc (zerop (process-exit-status proc))) "finished")
                        (proc "failed")
                        (t "finished")))
               (total-lines (count-lines (point-min) (point-max)))
               (content
                (if (zerop tail)
                    (buffer-substring-no-properties (point-min) (point-max))
                  (save-excursion
                    (goto-char (point-max))
                    (forward-line (- tail))
                    (buffer-substring-no-properties (point) (point-max))))))
          (list :buffer_name buf-name
                :exists t
                :status status
                :exit_status (when (and proc (not live)) (process-exit-status proc))
                :total_lines total-lines
                :output content))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tool 4: await_build

(mcpkit-register-tool 'await_build 'builder
  :description "Wait for a compilation buffer process to exit with a timeout."
  :input-schema '(:type "object"
                  :properties (:buffer_name (:type "string" :description "Compilation buffer name")
                               :timeout_seconds (:type "number" :description "Timeout in seconds (default 30)"))
                  :required ["buffer_name"])
  (let* ((buf-name (plist-get args :buffer_name))
         (timeout (or (plist-get args :timeout_seconds) 30))
         (buf (get-buffer buf-name)))
    (if (null buf)
        (list :buffer_name buf-name :status "not_found" :error "Buffer not found")
      (let ((proc (get-buffer-process buf)))
        (if (null proc)
            (list :buffer_name buf-name :status "finished" :message "No active process in buffer")
          (let ((start-time (float-time)))
            (while (and (process-live-p proc)
                        (< (- (float-time) start-time) timeout))
              (accept-process-output proc 0.2))
            (if (process-live-p proc)
                (list :buffer_name buf-name :status "timeout" :message (format "Timed out after %s seconds" timeout))
              (list :buffer_name buf-name
                    :status (if (zerop (process-exit-status proc)) "finished" "failed")
                    :exit_status (process-exit-status proc)
                    :elapsed_seconds (- (float-time) start-time)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tool 5: list_buffers

(mcpkit-register-tool 'list_buffers 'builder
  :description "List all active or recent compilation buffers."
  :input-schema '(:type "object" :properties () :required [])
  (ignore args)
  (let (buffers)
    (seq-do (lambda (buf)
              (when (and (buffer-live-p buf)
                         (with-current-buffer buf (derived-mode-p 'compilation-mode)))
                (let* ((proc (get-buffer-process buf))
                       (live (and proc (process-live-p proc))))
                  (push (list :buffer_name (buffer-name buf)
                              :status (cond (live "running")
                                            ((and proc (zerop (process-exit-status proc))) "finished")
                                            (proc "failed")
                                            (t "idle"))
                              :lines (with-current-buffer buf (count-lines (point-min) (point-max))))
                        buffers))))
            (buffer-list))
    (nreverse buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lifecycle & Orchestration

;; Register with centralized mcpkit autostart: merges into active server
;; on after-init-hook using namespace resolution for collision prevention.
(mcpkit-register-autostart 'builder :on-collision 'namespace)

;;;###autoload
(defun builder-mcp-start ()
  "Start or merge the builder MCP service into the central mcpkit server."
  (interactive)
  (mcpkit-start-service 'builder :on-collision 'namespace))

(defun builder-mcp-call-tool (tool-name &optional args)
  "Call MCP tool TOOL-NAME on service 'builder with ARGS."
  (let* ((service (mcpkit-get-service 'builder))
         (tool-str (if (symbolp tool-name) (symbol-name tool-name) tool-name))
         (tool (map-elt (mcpkit-service-tools service) tool-str))
         result)
    (unless tool
      (error "Tool %s not found on service builder" tool-str))
    (funcall (mcpkit-tool-handler tool)
             (or args '())
             (lambda (err res)
               (if err
                   (error "%s" err)
                 (setq result res))))
    result))

(provide 'builder-mcp)
;;; builder-mcp.el ends here
