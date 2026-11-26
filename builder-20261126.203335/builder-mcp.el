;;; builder-mcp.el --- Model Context Protocol service for builder -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: tools, compilation, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Exposes builder task discovery, compilation execution, output buffer
;; retrieval, and asynchronous process awaiting over Model Context Protocol (MCP)
;; using `mcpkit', plus Elisp package tools: `elisp_preflight',
;; `builder_run_tests', `builder_package_build', `builder_package_unused',
;; `elisp_package_spinout', `elisp_ci_setup', `doc_generate_api' and
;; `doc_validate'.  When ELPAish is installed, `builder-elpaish' adds its
;; `elpaish_*' tools on a separate `elpaish' service.
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

(defvar builder-elisp-check-use-cache)
(declare-function builder-elisp-check-package "builder-elisp-check")
(declare-function builder-scaffold-package "builder-scaffold")
(declare-function builder-scaffold-ci "builder-scaffold")
(declare-function builder-docs-validate "builder-docs")
(declare-function builder-docs-generate-api "builder-docs")

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
;; Tool 6: builder_byte_compile (alias: byte_compile)

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

(when (fboundp (quote mcpkit-register-tool-alias))
  (mcpkit-register-tool-alias "byte_compile" "builder_byte_compile"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tool 7: builder_check_parens (alias: check_parens)

(mcpkit-register-tool 'builder_check_parens 'builder
  :description "Check parenthesis balance in an Elisp file or buffer using check-parens. Files are checked in a scratch buffer, not visited."
  :input-schema '(:type "object"
                  :properties (:file (:type "string" :description "Path to Elisp file")
                               :buffer_name (:type "string" :description "Buffer name to check"))
                  :required [])
  (let* ((file (plist-get args :file))
         (buf-name (plist-get args :buffer_name))
         (live (and buf-name (get-buffer buf-name))))
    (unless (or live (and file (file-readable-p file)))
      (user-error "Must specify an existing file or buffer_name"))
    (let ((check (lambda ()
                   (condition-case err
                       (save-excursion
                         (save-restriction
                           (widen)
                           (goto-char (point-min))
                           (check-parens)
                           (list :status "ok" :balanced t :file file
                                 :buffer_name (and live (buffer-name live)))))
                     (error
                      (list :status "error"
                            :balanced :json-false
                            :file file
                            :buffer_name (and live (buffer-name live))
                            :line (line-number-at-pos)
                            :column (current-column)
                            :error (error-message-string err)))))))
      (if live
          (with-current-buffer live (funcall check))
        (with-temp-buffer
          (insert-file-contents file)
          (delay-mode-hooks (emacs-lisp-mode))
          (funcall check))))))

(when (fboundp (quote mcpkit-register-tool-alias))
  (mcpkit-register-tool-alias "check_parens" "builder_check_parens"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tool 8: builder_run_tests / run_tests

(mcpkit-register-tool 'builder_run_tests 'builder
  :description "Run ERT tests. With `root' (alias `directory'), load that package and run its tests in a fast isolated subprocess, returning structured results: status, total, passed, failed, skipped and the failing tests with their conditions. `selector' is then a test-name regexp; `prefix' and `glob' narrow it further. Without a root, run an ERT selector form in the live Emacs."
  :input-schema '(:type "object"
                  :properties (:root (:type "string" :description "Package directory to test in an isolated subprocess")
                               :directory (:type "string" :description "Alias for root")
                               :selector (:type "string" :description "With root: test-name regexp. Without: ERT selector form")
                               :prefix (:type "string" :description "Only tests whose name starts with this (root only)")
                               :glob (:type "string" :description "Only tests whose name matches this wildcard (root only)"))
                  :required [])
  (let* ((root (or (plist-get args :root) (plist-get args :directory)))
         (selector (plist-get args :selector)))
    (if (and root (not (string-empty-p root)))
        (let ((report (builder-emacs-conf-package-test-report
                       root :selector selector
                       :prefix (plist-get args :prefix)
                       :glob (plist-get args :glob))))
          (append (list :success (if (equal (plist-get report :status) "ok") t :json-false))
                  report))
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

(when (fboundp (quote mcpkit-register-tool-alias))
  (mcpkit-register-tool-alias "run_tests" "builder_run_tests"))

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

(when (fboundp (quote mcpkit-register-tool-alias))
  (mcpkit-register-tool-alias "load_file" "builder_load_file"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp package tools: preflight, build, scaffold, docs

(defun builder-mcp--list (value)
  "Return JSON array VALUE (vector or list) as a list, or nil."
  (cond ((vectorp value) (append value nil))
        ((listp value) value)))

(defun builder-mcp--flag (args key default)
  "Return the boolean ARGS value for KEY, or DEFAULT when absent.
JSON false arrives as `:json-false'."
  (let ((value (plist-get args key)))
    (cond ((eq value :json-false) nil)
          ((null value) default)
          (t t))))

(defun builder-mcp--directory (args)
  "Return the :directory in ARGS, or `default-directory'."
  (let ((dir (plist-get args :directory)))
    (if (and (stringp dir) (not (string-empty-p dir))) dir default-directory)))

(defun builder-mcp--required-directory (args)
  "Return the required :directory in ARGS."
  (let ((dir (plist-get args :directory)))
    (unless (and (stringp dir) (not (string-empty-p dir)))
      (user-error "Directory is required"))
    dir))

(mcpkit-register-tool 'elisp_preflight 'builder
  :description "Lint an Elisp package in one in-process call: check-parens, byte-compile, checkdoc, package-lint and relint (when installed), plus builder static checks. Returns structured findings (file, line, severity, message, tool); line 0 means unknown. Only errors fail the run; tools not installed are listed under skipped. Results are cached per file, so re-running after an edit is fast (`cached' counts answered checks; pass cache=false to force). isolated=true runs it in a fresh subprocess instead (slower, immune to this session's state)."
  :input-schema '(:type "object"
                  :properties (:directory (:type "string" :description "Package root (defaults to the daemon's default-directory)")
                               :files (:type "array" :items (:type "string")
                                       :description "Files to check instead of the discovered sources (relative to directory)")
                               :tools (:type "array"
                                       :items (:type "string"
                                               :enum ["parens" "compile" "checkdoc" "package-lint" "relint" "static"])
                                       :description "Subset of checks to run (default all)")
                               :cache (:type "boolean" :description "Reuse findings for unchanged files (default true)")
                               :isolated (:type "boolean" :description "Run in a fresh emacs subprocess (default false)"))
                  :required [])
  (require 'builder-elisp-check)
  (let ((dir (builder-mcp--directory args))
        (files (builder-mcp--list (plist-get args :files)))
        (tools (mapcar #'intern (builder-mcp--list (plist-get args :tools)))))
    (if (builder-mcp--flag args :isolated nil)
        (builder-emacs-conf--fast-batch-eval
         `(progn (require 'builder-elisp-check)
                 (let ((builder-elisp-check-use-cache nil))
                   (builder-elisp-check-package ,dir :files ',files :tools ',tools))))
      (let ((builder-elisp-check-use-cache (builder-mcp--flag args :cache t)))
        (builder-elisp-check-package dir :files files :tools tools)))))

(mcpkit-register-tool 'builder_package_build 'builder
  :description "Build an installable .tar for an Elisp package into build/packages/ (from git HEAD; uncommitted changes are not included), or clean build artifacts (build/, .cache/, *.elc). action is build (default), clean or rebuild."
  :input-schema '(:type "object"
                  :properties (:directory (:type "string" :description "Package root (defaults to the daemon's default-directory)")
                               :action (:type "string" :enum ["build" "clean" "rebuild"]
                                        :description "build (default), clean, or clean then build"))
                  :required [])
  (let* ((default-directory (file-name-as-directory
                             (expand-file-name (builder-mcp--directory args))))
         (action (or (plist-get args :action) "build"))
         (noninteractive nil)
         artifact)
    (unless (member action '("build" "clean" "rebuild"))
      (user-error "Unknown action: %s" action))
    (when (member action '("clean" "rebuild"))
      (builder-elisp-package-clean))
    (when (member action '("build" "rebuild"))
      (builder-elisp-package-build)
      (setq artifact (car (seq-sort-by (lambda (f) (float-time (file-attribute-modification-time
                                                                  (file-attributes f))))
                                       #'> (file-expand-wildcards
                                            (expand-file-name "build/packages/*.tar")))))
      (unless artifact (user-error "Build produced no archive")))
    (list :status "ok" :action action :directory default-directory :artifact artifact)))

(mcpkit-register-tool 'builder_package_unused 'builder
  :description "Audit packages installed under package-user-dir that are not in the dependency closure of package-selected-packages and the use-package declarations in init files."
  :input-schema '(:type "object" :properties () :required [])
  (ignore args)
  (let ((unused (builder-package-unused package-selected-packages)))
    (list :status "ok"
          :unused_count (length unused)
          :unused_packages (vconcat (mapcar #'symbol-name unused)))))

(mcpkit-register-tool 'elisp_package_spinout 'builder
  :description "Scaffold a standalone package repository (.gitignore, starter <pkg>.el, starter test, CI workflow). Local file writes only: no git, no network. Existing files are never overwritten. dry_run defaults to true; pass dry_run=false to write."
  :input-schema '(:type "object"
                  :properties (:package (:type "string" :description "Package name")
                               :target_dir (:type "string" :description "Repository directory (default ~/src/<package>)")
                               :summary (:type "string" :description "One-line package description")
                               :url (:type "string" :description "Homepage URL")
                               :dependencies (:type "array" :items (:type "string")
                                              :description "Packages the CI workflow must install")
                               :dry_run (:type "boolean" :description "Report without writing (default true)"))
                  :required ["package"])
  (require 'builder-scaffold)
  (let ((pkg (plist-get args :package)))
    (unless (and (stringp pkg) (not (string-empty-p pkg)))
      (user-error "Package is required"))
    (builder-scaffold-package pkg
                              :directory (plist-get args :target_dir)
                              :summary (plist-get args :summary)
                              :url (plist-get args :url)
                              :dependencies (builder-mcp--list (plist-get args :dependencies))
                              :dry-run (builder-mcp--flag args :dry_run t))))

(mcpkit-register-tool 'elisp_ci_setup 'builder
  :description "Generate .github/workflows/ci.yml using composite actions (Emacs matrix, lint, ERT, coverage, Codecov). Dependencies default to the Package-Requires headers. Returns the generated workflow; an existing workflow is only replaced with overwrite=true. dry_run defaults to true; pass dry_run=false to write."
  :input-schema '(:type "object"
                  :properties (:directory (:type "string" :description "Package directory")
                               :dependencies (:type "array" :items (:type "string")
                                              :description "Override the inferred dependency list")
                               :with_lint (:type "boolean" :description "Include the lint step (default true)")
                               :with_coverage (:type "boolean" :description "Include coverage (default true)")
                               :with_codecov (:type "boolean" :description "Include the Codecov upload (default true)")
                               :overwrite (:type "boolean" :description "Replace an existing ci.yml")
                               :dry_run (:type "boolean" :description "Report without writing (default true)"))
                  :required ["directory"])
  (require 'builder-scaffold)
  (builder-scaffold-ci
   (builder-mcp--required-directory args)
   :dependencies (builder-mcp--list (plist-get args :dependencies))
   :with-lint (builder-mcp--flag args :with_lint t)
   :with-coverage (builder-mcp--flag args :with_coverage t)
   :with-codecov (builder-mcp--flag args :with_codecov t)
   :overwrite (builder-mcp--flag args :overwrite nil)
   :dry-run (builder-mcp--flag args :dry_run t)))

(mcpkit-register-tool 'doc_generate_api 'builder
  :description "Scaffold (when missing) or regenerate docs/api.org for a package with org-docsgen. dry_run reports the action without running it."
  :input-schema '(:type "object"
                  :properties (:directory (:type "string" :description "Package directory")
                               :dry_run (:type "boolean" :description "Report the action without running it"))
                  :required ["directory"])
  (require 'builder-docs)
  (builder-docs-generate-api (builder-mcp--required-directory args)
                             (builder-mcp--flag args :dry_run nil)))

(mcpkit-register-tool 'doc_validate 'builder
  :description "Validate Org links (files, line numbers, CUSTOM_IDs, headings, internal anchors) in a package's *.org and docs/*.org. Returns errors with file, line and message."
  :input-schema '(:type "object"
                  :properties (:directory (:type "string" :description "Package directory"))
                  :required ["directory"])
  (require 'builder-docs)
  (builder-docs-validate (builder-mcp--required-directory args)))

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

(require 'builder-elpaish nil t)

(provide 'builder-mcp)
;;; builder-mcp.el ends here
