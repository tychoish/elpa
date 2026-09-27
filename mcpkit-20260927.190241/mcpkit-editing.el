;;; mcpkit-editing.el --- Editing/inspection MCP tools for mcpkit -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0"))
;; Keywords: tools, mcp, editing
;; URL: https://github.com/tychoish/mcpkit.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Provides a registration function, `mcpkit-editing-register-tools', that
;; attaches a set of file/buffer editing and inspection tools onto an
;; arbitrary, dynamically-named `mcpkit-service'.  Unlike `mcpkit-emacs.el',
;; which registers its tools at top level on a single fixed service symbol,
;; this module is meant to be called at runtime (for example once per
;; spawned sub-daemon) with a service value that is not known until then.
;;
;; Tools registered:
;;   - mcp_buffer_read
;;   - mcp_buffer_replace_region
;;   - mcp_ast_grep
;;   - mcp_project_diagnostics
;;
;; This module is optional and carries no autoload cookies: loading the
;; base `mcpkit' package never pulls this file in.  Callers must
;; explicitly `(require 'mcpkit-editing)' to activate it.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'json)
(require 'flymake)
(require 'mcpkit)

(defgroup mcpkit-editing nil
  "Editing/inspection MCP tools for mcpkit."
  :group 'mcpkit
  :prefix "mcpkit-editing-")

(defcustom mcpkit-editing-ast-grep-executable "ast-grep"
  "Name or path of the `ast-grep' executable used by `mcp_ast_grep'."
  :type 'string
  :group 'mcpkit-editing)

;;; Helpers

(defun mcpkit-editing--read-file-or-buffer (file)
  "Return the contents of FILE, preferring an already-visited buffer.
Signals a user-error if FILE does not exist and no buffer visits it."
  (let* ((path (expand-file-name file))
         (buf (find-buffer-visiting path)))
    (cond
     (buf
      (with-current-buffer buf
        (buffer-substring-no-properties (point-min) (point-max))))
     ((file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-substring-no-properties (point-min) (point-max))))
     (t
      (user-error "File not found: %s" file)))))

(defun mcpkit-editing--slice-lines (content start line-count)
  "Return the slice of CONTENT starting at line START for LINE-COUNT lines.
START is a 1-based line number.  When START or LINE-COUNT is nil, CONTENT is
returned unsliced."
  (if (not (or start line-count))
      content
    (let* ((lines (split-string content "\n"))
           (total (length lines))
           (start-idx (max 0 (1- (or start 1))))
           (end-idx (if line-count
                        (min total (+ start-idx line-count))
                      total)))
      (string-join (seq-subseq lines (min start-idx total) end-idx) "\n"))))

(defun mcpkit-editing-register-tools (service-or-name)
  "Register the editing/inspection MCP tools onto SERVICE-OR-NAME.
SERVICE-OR-NAME is anything accepted by `mcpkit-get-service' (a service
struct, a symbol, or a string naming an already-defined service)."
  ;; 1. mcp_buffer_read
  (mcpkit-register-tool 'mcp_buffer_read service-or-name
    :description "Read the contents of a file, preferring an already-open buffer's unsaved contents over disk."
    :input-schema '(:type "object"
                    :properties (:file (:type "string" :description "Path to the file to read")
                                 :start (:type "integer" :description "1-based line number to start reading from")
                                 :line-count (:type "integer" :description "Number of lines to read starting at START"))
                    :required ["file"])
    (let* ((file (plist-get args :file))
           (start (plist-get args :start))
           (line-count (plist-get args :line-count))
           (content (mcpkit-editing--read-file-or-buffer file))
           (sliced (mcpkit-editing--slice-lines content start line-count)))
      (list :found t
            :file_name file
            :content sliced)))

  ;; 2. mcp_buffer_replace_region
  (mcpkit-register-tool 'mcp_buffer_replace_region service-or-name
    :description "Replace a character region [START, END) in the buffer visiting FILE with REPLACEMENT, then save."
    :input-schema '(:type "object"
                    :properties (:file (:type "string" :description "Path to the file to edit")
                                 :start (:type "integer" :description "Start character position (1-based, inclusive)")
                                 :end (:type "integer" :description "End character position (1-based, exclusive)")
                                 :replacement (:type "string" :description "Replacement text"))
                    :required ["file" "start" "end" "replacement"])
    (let* ((file (plist-get args :file))
           (start (plist-get args :start))
           (end (plist-get args :end))
           (replacement (plist-get args :replacement))
           (path (expand-file-name file)))
      (unless (or (file-exists-p path) (find-buffer-visiting path))
        (user-error "File not found: %s" file))
      (let ((buf (or (find-buffer-visiting path) (find-file-noselect path))))
        (with-current-buffer buf
          (let ((pmin (point-min))
                (pmax (point-max)))
            (unless (and (integerp start) (integerp end)
                         (<= pmin start end) (<= end pmax))
              (user-error "Region [%s, %s) out of range for buffer visiting %s (valid range %s-%s)"
                          start end file pmin pmax))
            (goto-char start)
            (delete-region start end)
            (insert replacement)
            (save-buffer)
            (list :file file
                  :bytes-written (string-bytes replacement)))))))

  ;; 3. mcp_ast_grep
  (mcpkit-register-tool 'mcp_ast_grep service-or-name
    :description "Run the `ast-grep' CLI with PATTERN over PATH (defaulting to `default-directory') and return parsed matches."
    :input-schema '(:type "object"
                    :properties (:pattern (:type "string" :description "ast-grep pattern")
                                 :path (:type "string" :description "File or directory to search, defaults to default-directory")
                                 :language (:type "string" :description "Language to pass to ast-grep's --lang flag"))
                    :required ["pattern"])
    (let* ((pattern (plist-get args :pattern))
           (path (or (plist-get args :path) default-directory))
           (language (plist-get args :language))
           (exe (executable-find mcpkit-editing-ast-grep-executable)))
      (unless exe
        (user-error "ast-grep executable not found (looked for `%s' on PATH)" mcpkit-editing-ast-grep-executable))
      (let* ((cmd-args (append (list "run" "--pattern" pattern "--json")
                               (when language (list "--lang" language))
                               (list path))))
        (with-temp-buffer
          (let ((status (apply #'call-process exe nil t nil cmd-args)))
            (unless (eql status 0)
              (user-error "ast-grep exited with status %s: %s" status
                          (string-trim (buffer-string))))
            (let ((matches (condition-case err
                                (json-parse-string (buffer-string) :object-type 'plist :array-type 'list)
                              (error
                               (user-error "Failed to parse ast-grep JSON output: %s"
                                           (error-message-string err))))))
              (list :matches matches)))))))

  ;; 4. mcp_project_diagnostics
  (mcpkit-register-tool 'mcp_project_diagnostics service-or-name
    :description "Return best-effort Flymake diagnostics for the buffer visiting PATH, if any is open. Never forces a buffer open or waits on a linter; returns an empty list otherwise."
    :input-schema '(:type "object"
                    :properties (:path (:type "string" :description "Path to the file to check for diagnostics"))
                    :required ["path"])
    (let* ((path (plist-get args :path))
           (buf (find-buffer-visiting (expand-file-name path))))
      (if (not (and buf (buffer-live-p buf)))
          (list :diagnostics [])
        (with-current-buffer buf
          (if (not (bound-and-true-p flymake-mode))
              (list :diagnostics [])
            (let ((diags (condition-case nil
                             (flymake-diagnostics)
                           (error nil))))
              (list :diagnostics
                    (vconcat
                     (mapcar
                      (lambda (diag)
                        (let* ((beg (flymake-diagnostic-beg diag))
                               (line (save-excursion (goto-char beg) (line-number-at-pos)))
                               (column (save-excursion (goto-char beg) (current-column)))
                               (severity (format "%s" (flymake-diagnostic-type diag)))
                               (message (flymake-diagnostic-text diag)))
                          (list :line line
                                :column column
                                :severity severity
                                :message message)))
                      diags)))))))))
  service-or-name)

(provide 'mcpkit-editing)
;;; mcpkit-editing.el ends here
