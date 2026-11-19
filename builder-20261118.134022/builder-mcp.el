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
(require 'builder-isolate-elisp)

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
         (push (list :id (when-let* ((id (builder-candidate-id cand))) (symbol-name id))
                     :action (when-let* ((act (builder-candidate-action cand))) (symbol-name act))
                     :name (builder-candidate-name cand)
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
                  :properties (:target (:type "string" :description "Candidate symbol ID, semantic action (e.g. 'rust-test-workspace, 'test, 'build), or command name")
                               :id (:type "string" :description "Alias for target")
                               :command (:type "string" :description "Raw shell command to run directly")
                               :directory (:type "string" :description "Working directory (defaults to current)")
                               :buffer_name (:type "string" :description "Custom compilation buffer name"))
                  :required [])
  (let* ((target-str (or (plist-get args :target)
                         (plist-get args :id)))
         (cmd (plist-get args :command))
         (dir (or (plist-get args :directory) default-directory))
         (custom-buf (plist-get args :buffer_name)))
    (if target-str
        (let* ((target-sym (intern target-str))
               (buf (builder-select target-sym :force t :directory dir :buffer-name custom-buf)))
          (list :buffer_name (buffer-name buf)
                :target target-str
                :directory dir
                :status "started"))
      (if (null cmd)
          (error "Either target/id or command must be provided to run_task")
        (let* ((target-name (or custom-buf
                                (format "*compilation*<%s>" (file-name-nondirectory (directory-file-name dir))))))
          (let ((compilation-buffer-name-function (lambda (_mode) target-name))
                (default-directory dir))
            (let ((buf (compilation-start cmd)))
              (list :buffer_name (buffer-name buf)
                    :command cmd
                    :directory dir
                    :status "started"))))))))

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
;; Tool 6: builder_byte_compile / byte_compile

(mcpkit-register-tool 'builder_byte_compile 'builder
  :description "Byte-compile an Elisp file in an isolated subprocess and delete the artifact (.elc)."
  :input-schema '(:type "object"
                  :properties (:file (:type "string" :description "Path to Elisp file (relative to Emacs dir or absolute)"))
                  :required ["file"])
  (let* ((file (plist-get args :file))
         (start-time (float-time))
         (ok (builder-emacs-conf-byte-compile-and-delete-artifact file))
         (duration (* 1000.0 (- (float-time) start-time)))
         (log (with-current-buffer (get-buffer-create "*Compile-Log*")
                (save-excursion
                  (goto-char (point-max))
                  (forward-line -20)
                  (buffer-substring-no-properties (point) (point-max))))))
    (list :file file
          :status (if ok "ok" "error")
          :success (if ok t :json-false)
          :duration_ms duration
          :log log)))

(mcpkit-register-tool 'byte_compile 'builder
  :description "Alias for builder_byte_compile."
  :input-schema '(:type "object"
                  :properties (:file (:type "string" :description "Path to Elisp file"))
                  :required ["file"])
  (builder-mcp-call-tool 'builder_byte_compile args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tool 7: builder_check_parens / check_parens

(mcpkit-register-tool 'builder_check_parens 'builder
  :description "Check parenthesis balance in an Elisp file or buffer using check-parens."
  :input-schema '(:type "object"
                  :properties (:file (:type "string" :description "Path to Elisp file")
                               :buffer_name (:type "string" :description "Buffer name to check"))
                  :required [])
  (let* ((file (plist-get args :file))
         (buf-name (plist-get args :buffer_name))
         (target-buf (or (and buf-name (get-buffer buf-name))
                         (and file (find-file-noselect file)))))
    (unless target-buf
      (user-error "Must specify an existing file or buffer_name"))
    (with-current-buffer target-buf
      (condition-case err
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (check-parens)
              (list :status "ok" :balanced t :file file :buffer_name (buffer-name target-buf))))
        (error
         (list :status "error"
               :balanced :json-false
               :file file
               :buffer_name (buffer-name target-buf)
               :line (line-number-at-pos)
               :column (current-column)
               :error (error-message-string err)))))))

(mcpkit-register-tool 'check_parens 'builder
  :description "Alias for builder_check_parens."
  :input-schema '(:type "object"
                  :properties (:file (:type "string" :description "Path to Elisp file")
                               :buffer_name (:type "string" :description "Buffer name"))
                  :required [])
  (builder-mcp-call-tool 'builder_check_parens args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tool 8: builder_run_tests / run_tests

(mcpkit-register-tool 'builder_run_tests 'builder
  :description "Run ERT tests for an Elisp package at ROOT or with an ERT selector."
  :input-schema '(:type "object"
                  :properties (:root (:type "string" :description "Optional package directory to test in isolated subprocess")
                               :selector (:type "string" :description "Optional ERT selector regexp"))
                  :required [])
  (let* ((root (plist-get args :root))
         (selector (plist-get args :selector)))
    (if (and root (not (string-empty-p root)))
        (let* ((ok (builder-emacs-conf-elisp-package-test-isolated root))
               (log (with-current-buffer (get-buffer-create "*builder-test-check-log*")
                      (buffer-substring-no-properties (point-min) (point-max)))))
          (list :root root :status (if ok "ok" "error") :success (if ok t :json-false) :log log))
      (let* ((sel (if selector (read selector) t))
             (stats (ert sel))
             (total (ert-stats-total stats))
             (passed (ert-stats-completed-expected stats))
             (unexpected (ert-stats-completed-unexpected stats)))
        (list :selector (format "%s" sel)
              :status (if (zerop unexpected) "ok" "failed")
              :total total
              :passed passed
              :failed unexpected)))))

(mcpkit-register-tool 'run_tests 'builder
  :description "Alias for builder_run_tests."
  :input-schema '(:type "object"
                  :properties (:root (:type "string") :selector (:type "string"))
                  :required [])
  (builder-mcp-call-tool 'builder_run_tests args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tool 9: builder_load_file / load_file

(mcpkit-register-tool 'builder_load_file 'builder
  :description "Safely load an Elisp file into the live Emacs instance, catching any errors."
  :input-schema '(:type "object"
                  :properties (:file (:type "string" :description "Path to Elisp file to load"))
                  :required ["file"])
  (let* ((file (plist-get args :file)))
    (unless (and file (file-exists-p file))
      (user-error "File does not exist: %s" file))
    (condition-case err
        (progn
          (load-file file)
          (list :file file :status "ok" :loaded t))
      (error
       (list :file file :status "error" :loaded :json-false :error (error-message-string err))))))

(mcpkit-register-tool 'load_file 'builder
  :description "Alias for builder_load_file."
  :input-schema '(:type "object"
                  :properties (:file (:type "string" :description "Path to Elisp file"))
                  :required ["file"])
  (builder-mcp-call-tool 'builder_load_file args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lifecycle & Orchestration


;;;###autoload
(defun builder-mcp-start ()
  "Start or merge the builder MCP service into the central mcpkit server."
  (interactive)
  (mcpkit-start-service 'builder :on-collision 'namespace))

(defun builder-mcp-call-tool (tool-name &optional args)
  "Call MCP tool TOOL-NAME on service \\='builder with ARGS."
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
