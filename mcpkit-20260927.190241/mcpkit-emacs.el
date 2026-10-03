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
;; Exposes core Emacs management, buffer inspection, safe evaluation, file/buffer
;; navigation, documentation introspection, selection, search-replace, and
;; content sending as an MCP service via mcpkit.el.
;;
;; Tools are registered at top-level on the `emacs' service upon loading.
;; Services are started only when explicitly requested via `mcpkit-start-service'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dired)
(require 'projectile nil t)
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

;; 1b. emacs_whoami
(mcpkit-register-tool 'emacs_whoami 'emacs
  :description "Identify the Emacs instance: daemon name, process ID (PID), active MCP port, user, and active services."
  :input-schema '(:type "object" :properties ())
  (ignore args)
  (list :daemon_name (or (bound-and-true-p server-name)
                         (and (fboundp 'daemonp) (daemonp))
                         "standalone")
        :pid (emacs-pid)
        :active_port (or (and (fboundp 'mcpkit-active-port) (mcpkit-active-port)) :json-null)
        :user (user-login-name)
        :system_type (symbol-name system-type)
        :emacs_version emacs-version
        :active_services (vconcat (seq-map (lambda (entry)
                                             (symbol-name (mcpkit-service-name (car entry))))
                                           (bound-and-true-p mcpkit--active-services)))))

;; 1c. emacs_proxy_routes
(mcpkit-register-tool 'emacs_proxy_routes 'emacs
  :description "List all daemon and sprite routes registered with mcpkit-proxy."
  :input-schema '(:type "object" :properties ())
  (ignore args)
  (if (fboundp 'mcpkit-proxy-list-routes)
      (let ((routes (mcpkit-proxy-list-routes)))
        (list :count (length routes)
              :routes (vconcat routes)))
    (list :count 0 :routes [] :error "mcpkit-proxy not loaded")))

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

;; 4. emacs_open
(mcpkit-register-tool 'emacs_open 'emacs
  :description "Open a file or switch to an existing buffer in Emacs, optionally moving point to a line number."
  :input-schema '(:type "object"
                  :properties (:file (:type "string" :description "Absolute or relative file path to open")
                               :line (:type "integer" :description "Optional line number to navigate to")
                               :buffer (:type "string" :description "Optional buffer name to switch to")))
  (let* ((file (plist-get args :file))
         (line (plist-get args :line))
         (buf-name (plist-get args :buffer))
         (buf (cond
               (file (find-file-noselect (expand-file-name file)))
               (buf-name (get-buffer buf-name))
               (t (current-buffer)))))
    (if (not buf)
        (list :status "error" :error (format "Buffer or file not found: %s" (or file buf-name)))
      (let ((win (display-buffer buf)))
        (with-selected-window (or win (selected-window))
          (set-buffer buf)
          (when (and line (> line 0))
            (goto-char (point-min))
            (forward-line (1- line))))
        (list :status "ok"
              :buffer (buffer-name buf)
              :file (or (buffer-file-name buf) "")
              :line (or line 1))))))

;; 5. emacs_dired
(mcpkit-register-tool 'emacs_dired 'emacs
  :description "Open a dired buffer at directory with optional files marked."
  :input-schema '(:type "object"
                  :properties (:directory (:type "string" :description "Directory path to open")
                               :files (:type "array" :items (:type "string") :description "Optional list of filenames to mark"))
                  :required ["directory"])
  (let* ((dir (expand-file-name (plist-get args :directory)))
         (files (plist-get args :files))
         (file-list (if (vectorp files) (append files nil) files)))
    (unless (file-directory-p dir)
      (user-error "Directory does not exist: %s" dir))
    (let ((buf (dired-noselect dir)))
      (with-current-buffer buf
        (when file-list
          (dired-unmark-all-marks)
          (dolist (f file-list)
            (dired-goto-file (expand-file-name f dir))
            (dired-mark 1))))
      (display-buffer buf)
      (list :status "ok"
            :directory dir
            :marked_files (if file-list (length file-list) 0)))))

;; 6. emacs_describe
(mcpkit-register-tool 'emacs_describe 'emacs
  :description "Look up symbol documentation (function, variable, face, key binding, and related apropos matches)."
  :input-schema '(:type "object"
                  :properties (:query (:type "string" :description "Symbol name or key sequence to describe"))
                  :required ["query"])
  (let* ((query (plist-get args :query))
         (sym (intern-soft query))
         (sections nil))
    ;; Function documentation
    (when (and sym (fboundp sym))
      (push (format "## Function: %s\n\n%s\n\nSignature: %s\n\n%s"
                    query
                    (cond ((subrp (symbol-function sym)) "Built-in function")
                          ((macrop sym) "Macro")
                          ((commandp sym) "Interactive command")
                          (t "Function"))
                    (or (help-function-arglist sym t) "()")
                    (or (documentation sym t) "No documentation available."))
            sections))
    ;; Variable documentation
    (when (and sym (boundp sym))
      (let ((val (symbol-value sym)))
        (push (format "## Variable: %s\n\nCurrent value: %s\n\n%s"
                      query
                      (let ((printed (format "%S" val)))
                        (if (> (length printed) 200)
                            (concat (substring printed 0 200) "...")
                            printed))
                      (or (documentation-property sym 'variable-documentation t)
                          "No documentation available."))
              sections)))
    ;; Face documentation
    (when (and sym (facep sym))
      (push (format "## Face: %s\n\n%s"
                    query
                    (or (documentation-property sym 'face-documentation t)
                        "No documentation available."))
            sections))
    ;; Key binding
    (when (string-match-p "\\`[CMSs]-\\|\\`<" query)
      (let* ((keyseq (ignore-errors (kbd query)))
             (binding (and keyseq (key-binding keyseq))))
        (when binding
          (push (format "## Key binding: %s\n\n%s runs `%s'\n\n%s"
                        query query binding
                        (or (documentation binding t)
                            "No documentation available."))
                sections))))
    ;; Related symbols (apropos)
    (let ((matches nil))
      (mapatoms
       (lambda (s)
         (when (and (string-match-p (regexp-quote query) (symbol-name s))
                    (not (eq s sym))
                    (or (fboundp s) (boundp s)))
           (push (format "  %s%s"
                         (symbol-name s)
                         (cond ((and (fboundp s) (boundp s)) " [function, variable]")
                               ((fboundp s) " [function]")
                               (t " [variable]")))
                 matches))))
      (when matches
        (let ((sorted (sort matches #'string<)))
          (push (format "## Related symbols\n\n%s%s"
                        (mapconcat #'identity (seq-take sorted 20) "\n")
                        (if (> (length sorted) 20)
                            (format "\n  ... and %d more" (- (length sorted) 20))
                          ""))
                sections))))
    (list :query query
          :documentation (if sections
                             (mapconcat #'identity (nreverse sections) "\n\n")
                           (format "No documentation found for \"%s\"." query)))))

;; 7. emacs_select
(mcpkit-register-tool 'emacs_select 'emacs
  :description "Inspect current buffer, point, and selection state, or navigate to a file and activate region selection."
  :input-schema '(:type "object"
                  :properties (:file (:type "string" :description "Optional file path to visit")
                               :start_line (:type "integer" :description "Optional starting line for selection")
                               :end_line (:type "integer" :description "Optional ending line for selection")))
  (let* ((file (plist-get args :file))
         (start (plist-get args :start_line))
         (end (plist-get args :end_line)))
    (when file
      (find-file (expand-file-name file)))
    (when (and start (> start 0))
      (goto-char (point-min))
      (forward-line (1- start))
      (set-mark (point))
      (if (and end (> end start))
          (forward-line (- end start))
        (forward-line 0))
      (end-of-line)
      (activate-mark))
    (list :buffer (buffer-name)
          :file (or (buffer-file-name) "")
          :major_mode (symbol-name major-mode)
          :point (point)
          :region_active (if (use-region-p) t :json-false)
          :selection (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       ""))))

;; 8. emacs_search_replace
(mcpkit-register-tool 'emacs_search_replace 'emacs
  :description "Search and replace text within a buffer, a single file, or project-wide."
  :input-schema '(:type "object"
                  :properties (:old (:type "string" :description "Target text or regexp to search for")
                               :new (:type "string" :description "Replacement text")
                               :scope (:type "string" :enum ["buffer" "file" "project"] :description "Scope of replacement (default: file)")
                               :target (:type "string" :description "Buffer name, file path, or project directory")
                               :regexp (:type "boolean" :description "Treat OLD as regular expression")
                               :file_ext (:type "string" :description "Optional file extension filter for project scope"))
                  :required ["old" "new"])
  (let* ((old (plist-get args :old))
         (new (plist-get args :new))
         (scope (or (plist-get args :scope) "file"))
         (target (plist-get args :target))
         (is-regexp (eq (plist-get args :regexp) t))
         (file-ext (plist-get args :file_ext)))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (cond
       ((equal scope "buffer")
        (let ((buf (if target (get-buffer target) (current-buffer))))
          (unless buf (user-error "Buffer not found: %s" target))
          (with-current-buffer buf
            (let ((count 0)
                  (case-fold-search nil))
              (save-excursion
                (goto-char (point-min))
                (if is-regexp
                    (while (re-search-forward old nil t)
                      (replace-match new t nil)
                      (setq count (1+ count)))
                  (while (search-forward old nil t)
                    (replace-match new t t)
                    (setq count (1+ count)))))
              (when (and (> count 0) (derived-mode-p 'org-mode))
                (ignore-errors (org-element-cache-reset)))
              (list :status "ok" :scope "buffer" :buffer (buffer-name buf) :replacements count)))))

       ((equal scope "file")
        (let* ((file-path (expand-file-name (or target (buffer-file-name) (user-error "No file specified"))))
               (buf (find-file-noselect file-path)))
          (with-current-buffer buf
            (let ((count 0)
                  (case-fold-search nil))
              (save-excursion
                (goto-char (point-min))
                (if is-regexp
                    (while (re-search-forward old nil t)
                      (replace-match new t nil)
                      (setq count (1+ count)))
                  (while (search-forward old nil t)
                    (replace-match new t t)
                    (setq count (1+ count)))))
              (when (and (> count 0) (derived-mode-p 'org-mode))
                (ignore-errors (org-element-cache-reset)))
              (when (> count 0)
                (save-buffer))
              (list :status "ok" :scope "file" :file file-path :replacements count)))))

       ((equal scope "project")
        (unless (fboundp 'projectile-project-root)
          (user-error "Project scope requires projectile"))
        (let* ((dir (or target default-directory))
               (root (or (projectile-project-root dir)
                         (user-error "No project root found at or above %s" dir)))
               (default-directory root)
               (files (seq-map (lambda (f) (expand-file-name f root))
                               (projectile-project-files root)))
               (files (if file-ext
                          (seq-filter (lambda (f) (equal (file-name-extension f) file-ext)) files)
                        files))
               (results nil)
               (total-count 0))
          (dolist (f files)
            (when (file-regular-p f)
              (let* ((buf (find-file-noselect f))
                     (count (with-current-buffer buf
                              (let ((c 0)
                                    (case-fold-search nil))
                                (save-excursion
                                  (goto-char (point-min))
                                  (if is-regexp
                                      (while (re-search-forward old nil t)
                                        (replace-match new t nil)
                                        (setq c (1+ c)))
                                    (while (search-forward old nil t)
                                      (replace-match new t t)
                                      (setq c (1+ c)))))
                                (when (and (> c 0) (derived-mode-p 'org-mode))
                                  (ignore-errors (org-element-cache-reset)))
                                c))))
                (when (> count 0)
                  (setq total-count (+ total-count count))
                  (push (list :file f :replacements count) results)))))
          (let ((default-directory root))
            (projectile-save-project-buffers))
          (list :status "ok" :scope "project" :project_root root :total_replacements total-count :files (vconcat (nreverse results)))))))))

;; 9. emacs_send
(mcpkit-register-tool 'emacs_send 'emacs
  :description "Send text output directly to a named Emacs buffer and display it."
  :input-schema '(:type "object"
                  :properties (:content (:type "string" :description "Text content to insert into buffer")
                               :buffer (:type "string" :description "Buffer name (default '*agent-output*')")
                               :mode (:type "string" :description "Major mode symbol name (default 'markdown-mode')"))
                  :required ["content"])
  (let* ((content (plist-get args :content))
         (buf-name (or (plist-get args :buffer) "*agent-output*"))
         (mode-str (or (plist-get args :mode) "markdown-mode"))
         (mode-fn (intern-soft mode-str))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (when (and mode-fn (fboundp mode-fn))
        (funcall mode-fn))
      (erase-buffer)
      (insert content)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (display-buffer buf)
    (list :status "ok" :buffer buf-name :length (length content))))

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
