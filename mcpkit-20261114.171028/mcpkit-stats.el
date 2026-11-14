;;; mcpkit-stats.el --- Execution statistics and inspection for mcpkit -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: comm, tools, mcp, stats
;; URL: https://github.com/tychoish/mcpkit.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; mcpkit-stats tracks per-tool invocation counts, error ratios, latency
;; metrics, and recent failure histories.  It provides both a tabulated
;; summary view (`*mcpkit-stats*') and an inspection detail view
;; (`*mcpkit-stats-tool: <name>*') with direct links to Elisp source
;; definitions and centralized log entries.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'ring)
(require 'tabulated-list)
(require 'mcpkit)

(defvar mcpkit-log-buffer-name)

(defgroup mcpkit-stats nil
  "Tool usage analytics and inspection for mcpkit."
  :group 'mcpkit
  :prefix "mcpkit-stats-")

(defcustom mcpkit-stats-recent-errors-limit 10
  "Maximum number of recent errors kept per tool in memory."
  :type 'integer
  :group 'mcpkit-stats)

;;; Data Structures

(cl-defstruct (mcpkit-stats-record
               (:constructor make-mcpkit-stats-record)
               (:conc-name mcpkit-stats-record-))
  "Execution telemetry for a single MCP tool."
  (tool-name "" :type string)
  (service nil :type (or null symbol))
  (call-count 0 :type integer)
  (success-count 0 :type integer)
  (error-count 0 :type integer)
  (total-duration-ms 0.0 :type float)
  (min-duration-ms 0.0 :type float)
  (max-duration-ms 0.0 :type float)
  (last-duration-ms 0.0 :type float)
  (last-called-at nil :type (or null string))
  (recent-errors (make-ring 10) :type ring)
  (source-file nil :type (or null string))
  (source-line nil :type (or null integer)))

(defvar mcpkit-stats--table (make-hash-table :test #'equal)
  "Hash table mapping tool-name strings to `mcpkit-stats-record' instances.")

;;; Collection Engine

(defun mcpkit-stats-get (tool-name)
  "Retrieve or initialize the `mcpkit-stats-record' for TOOL-NAME."
  (let ((tname (if (symbolp tool-name) (symbol-name tool-name) (format "%s" tool-name))))
    (or (gethash tname mcpkit-stats--table)
        (let* ((rec (make-mcpkit-stats-record
                     :tool-name tname
                     :recent-errors (make-ring mcpkit-stats-recent-errors-limit)))
               (tool (and (boundp 'mcpkit--active-services)
                          (mcpkit--find-tool tname mcpkit--active-services))))
          (when tool
            (setf (mcpkit-stats-record-service rec) (mcpkit-tool-service tool))
            (setf (mcpkit-stats-record-source-file rec) (mcpkit-tool-source-file tool))
            (setf (mcpkit-stats-record-source-line rec) (mcpkit-tool-source-line tool)))
          (puthash tname rec mcpkit-stats--table)
          rec))))

(defun mcpkit-stats-reset (&optional tool-name)
  "Reset telemetry metrics.
If TOOL-NAME is provided, reset only that tool.  Otherwise reset all."
  (interactive)
  (if tool-name
      (let ((tname (if (symbolp tool-name) (symbol-name tool-name) (format "%s" tool-name))))
        (remhash tname mcpkit-stats--table))
    (clrhash mcpkit-stats--table))
  (when (get-buffer "*mcpkit-stats*")
    (with-current-buffer "*mcpkit-stats*"
      (revert-buffer))))

(defun mcpkit-stats-record-call (tool-name status duration-ms &optional err-msg req-args)
  "Record execution metrics for TOOL-NAME.
STATUS is `success' or `error'.  DURATION-MS is execution time in milliseconds.
ERR-MSG is the error message string on failure.
REQ-ARGS is the argument payload passed to the tool."
  (condition-case nil
      (when (and tool-name (not (string-empty-p (format "%s" tool-name))))
        (let* ((rec (mcpkit-stats-get tool-name))
               (dur (or duration-ms 0.0))
               (success-p (eq status 'success)))
          ;; Update basic counters
          (cl-incf (mcpkit-stats-record-call-count rec))
          (if success-p
              (cl-incf (mcpkit-stats-record-success-count rec))
            (cl-incf (mcpkit-stats-record-error-count rec))
            (ring-insert (mcpkit-stats-record-recent-errors rec)
                         (list :time (format-time-string "%Y-%m-%d %H:%M:%S")
                               :message (or err-msg "Unknown error")
                               :args req-args)))
          ;; Update latency stats
          (setf (mcpkit-stats-record-last-duration-ms rec) dur)
          (setf (mcpkit-stats-record-total-duration-ms rec)
                (+ (mcpkit-stats-record-total-duration-ms rec) dur))
          (if (= (mcpkit-stats-record-min-duration-ms rec) 0.0)
              (setf (mcpkit-stats-record-min-duration-ms rec) dur)
            (setf (mcpkit-stats-record-min-duration-ms rec)
                  (min (mcpkit-stats-record-min-duration-ms rec) dur)))
          (setf (mcpkit-stats-record-max-duration-ms rec)
                (max (mcpkit-stats-record-max-duration-ms rec) dur))
          ;; Update timestamp
          (setf (mcpkit-stats-record-last-called-at rec)
                (format-time-string "%Y-%m-%d %H:%M:%S"))
          ;; Populate source location if previously missing
          (unless (mcpkit-stats-record-source-file rec)
            (when-let* ((tool (and (boundp 'mcpkit--active-services)
                                   (mcpkit--find-tool (mcpkit-stats-record-tool-name rec)
                                                      mcpkit--active-services))))
              (setf (mcpkit-stats-record-service rec) (mcpkit-tool-service tool))
              (setf (mcpkit-stats-record-source-file rec) (mcpkit-tool-source-file tool))
              (setf (mcpkit-stats-record-source-line rec) (mcpkit-tool-source-line tool))))))
    (error nil)))

;; Register collection hook into core dispatcher
(add-hook 'mcpkit-post-tool-call-functions #'mcpkit-stats-record-call)

;;; Tabular List Mode

(defvar mcpkit-stats-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'mcpkit-stats-inspect-tool)
    (define-key map (kbd "v")   #'mcpkit-stats-inspect-tool)
    (define-key map (kbd "i")   #'mcpkit-stats-jump-to-source)
    (define-key map (kbd "l")   #'mcpkit-stats-view-logs)
    (define-key map (kbd "c")   #'mcpkit-stats-clear-prompt)
    (define-key map (kbd "g")   #'mcpkit-stats-refresh)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `mcpkit-stats-mode'.")

(define-derived-mode mcpkit-stats-mode tabulated-list-mode "MCPKit-Stats"
  "Major mode for inspecting MCP tool execution metrics."
  (setq tabulated-list-format
        [("Tool Name" 32 t)
         ("Service" 12 t)
         ("Calls" 7 t)
         ("Success" 8 t)
         ("Errors" 8 t)
         ("Err %" 8 t)
         ("Avg (ms)" 9 t)
         ("Last Run" 19 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Calls" t))
  (add-hook 'tabulated-list-revert-hook #'mcpkit-stats-tabulated-entries nil t)
  (tabulated-list-init-header))

(defun mcpkit-stats-tabulated-entries ()
  "Generate `tabulated-list-entries' from active tools and statistics."
  (let* ((seen (make-hash-table :test #'equal))
         (entries nil))
    ;; First include all tools from telemetry table
    (maphash
     (lambda (tname rec)
       (puthash tname t seen)
       (let* ((calls (mcpkit-stats-record-call-count rec))
              (succ (mcpkit-stats-record-success-count rec))
              (errs (mcpkit-stats-record-error-count rec))
              (err-pct (if (> calls 0) (* (/ (float errs) calls) 100.0) 0.0))
              (avg-dur (if (> calls 0) (/ (mcpkit-stats-record-total-duration-ms rec) calls) 0.0))
              (svc (or (mcpkit-stats-record-service rec) "-"))
              (last-run (or (mcpkit-stats-record-last-called-at rec) "never")))
         (push (list tname
                     (vector tname
                             (format "%s" svc)
                             (number-to-string calls)
                             (number-to-string succ)
                             (number-to-string errs)
                             (format "%.1f%%" err-pct)
                             (format "%.2f" avg-dur)
                             last-run))
               entries)))
     mcpkit-stats--table)
    ;; Next include registered tools that haven't been invoked yet
    (when (boundp 'mcpkit--active-services)
      (dolist (tool (mcpkit--normalize-tools mcpkit--active-services))
        (let ((tname (mcpkit-tool-name tool)))
          (unless (gethash tname seen)
            (puthash tname t seen)
            (push (list tname
                        (vector tname
                                (format "%s" (or (mcpkit-tool-service tool) "-"))
                                "0" "0" "0" "0.0%" "0.00" "never"))
                  entries)))))
    (setq tabulated-list-entries (nreverse entries))))

(defun mcpkit-stats-refresh ()
  "Refresh the statistics table."
  (interactive)
  (mcpkit-stats-tabulated-entries)
  (tabulated-list-print t))

(defun mcpkit-stats-clear-prompt ()
  "Prompt to clear all telemetry statistics."
  (interactive)
  (when (y-or-n-p "Reset all MCPKit tool statistics? ")
    (mcpkit-stats-reset)
    (mcpkit-stats-refresh)
    (message "MCPKit tool statistics reset.")))

;;;###autoload
(defun mcpkit-stats ()
  "Open the MCPKit tool usage statistics monitor."
  (interactive)
  (let ((buf (get-buffer-create "*mcpkit-stats*")))
    (with-current-buffer buf
      (mcpkit-stats-mode)
      (mcpkit-stats-tabulated-entries)
      (tabulated-list-print))
    (pop-to-buffer buf)))

;;; Detail Inspection Mode

(defvar mcpkit-stats-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "i") #'mcpkit-stats-jump-to-source)
    (define-key map (kbd "l") #'mcpkit-stats-view-logs)
    (define-key map (kbd "g") #'mcpkit-stats-refresh-detail)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `mcpkit-stats-detail-mode'.")

(define-derived-mode mcpkit-stats-detail-mode special-mode "MCPKit-Stats-Detail"
  "Major mode for inspecting granular metrics and schemas of an MCP tool."
  (setq buffer-read-only t))

(defvar-local mcpkit-stats-detail--current-tool nil
  "Tool name currently displayed in this detail buffer.")

(defun mcpkit-stats-inspect-tool (&optional tool-name)
  "Inspect detailed metrics, schema, and error logs for TOOL-NAME.
When called interactively without argument, uses tool at point."
  (interactive (list (or (tabulated-list-get-id)
                         (read-string "Inspect tool: "))))
  (let* ((tname (or tool-name (tabulated-list-get-id)))
         (buf-name (format "*mcpkit-stats-tool: %s*" tname))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (setq mcpkit-stats-detail--current-tool tname)
      (mcpkit-stats-render-detail tname)
      (mcpkit-stats-detail-mode))
    (pop-to-buffer buf)))

(defun mcpkit-stats-refresh-detail ()
  "Refresh the current tool detail buffer."
  (interactive)
  (when mcpkit-stats-detail--current-tool
    (mcpkit-stats-render-detail mcpkit-stats-detail--current-tool)))

(defun mcpkit-stats-render-detail (tool-name)
  "Render detail inspection content for TOOL-NAME in current buffer."
  (let* ((inhibit-read-only t)
         (rec (mcpkit-stats-get tool-name))
         (tool (and (boundp 'mcpkit--active-services)
                    (mcpkit--find-tool tool-name mcpkit--active-services)))
         (calls (mcpkit-stats-record-call-count rec))
         (succ (mcpkit-stats-record-success-count rec))
         (errs (mcpkit-stats-record-error-count rec))
         (err-pct (if (> calls 0) (* (/ (float errs) calls) 100.0) 0.0))
         (avg-dur (if (> calls 0) (/ (mcpkit-stats-record-total-duration-ms rec) calls) 0.0))
         (min-dur (mcpkit-stats-record-min-duration-ms rec))
         (max-dur (mcpkit-stats-record-max-duration-ms rec))
         (last-dur (mcpkit-stats-record-last-duration-ms rec))
         (last-run (or (mcpkit-stats-record-last-called-at rec) "never"))
         (src-file (or (mcpkit-stats-record-source-file rec)
                       (and tool (mcpkit-tool-source-file tool))))
         (src-line (or (mcpkit-stats-record-source-line rec)
                       (and tool (mcpkit-tool-source-line tool))))
         (svc (or (mcpkit-stats-record-service rec)
                  (and tool (mcpkit-tool-service tool))
                  "-")))
    (erase-buffer)
    ;; Title & Actions
    (insert (propertize (format "Tool: %s\n" tool-name) 'face 'header-line))
    (insert "Actions: ")
    (insert-text-button "[Jump to Source (i)]"
                        'action (lambda (_) (mcpkit-stats-jump-to-source tool-name))
                        'follow-link t)
    (insert "  ")
    (insert-text-button "[View Logs (l)]"
                        'action (lambda (_) (mcpkit-stats-view-logs tool-name))
                        'follow-link t)
    (insert "  ")
    (insert-text-button "[Refresh (g)]"
                        'action (lambda (_) (mcpkit-stats-refresh-detail))
                        'follow-link t)
    (insert "\n\n")

    ;; Metadata Section
    (insert (propertize "METADATA\n" 'face 'bold))
    (insert (format "  Service:     %s\n" svc))
    (insert (format "  Source File: %s\n" (or src-file "(not recorded)")))
    (insert (format "  Source Line: %s\n" (if src-line (number-to-string src-line) "(not recorded)")))
    (when tool
      (insert (format "  Description: %s\n" (or (mcpkit-tool-description tool) ""))))
    (insert "\n")

    ;; Execution Metrics
    (insert (propertize "EXECUTION METRICS\n" 'face 'bold))
    (insert (format "  Total Invocations: %d\n" calls))
    (insert (format "  Successful Calls:  %d\n" succ))
    (insert (format "  Failed Calls:      %d (%.1f%% error rate)\n" errs err-pct))
    (insert (format "  Average Latency:   %.2f ms\n" avg-dur))
    (insert (format "  Minimum Latency:   %.2f ms\n" min-dur))
    (insert (format "  Maximum Latency:   %.2f ms\n" max-dur))
    (insert (format "  Last Call Latency: %.2f ms\n" last-dur))
    (insert (format "  Last Invocated At: %s\n" last-run))
    (insert "\n")

    ;; Input Schema
    (insert (propertize "INPUT SCHEMA\n" 'face 'bold))
    (if (and tool (mcpkit-tool-input-schema tool))
        (condition-case nil
            (insert (format "  %s\n"
                            (mcpkit--serialize-response (mcpkit-tool-input-schema tool))))
          (error (insert (format "  %S\n" (mcpkit-tool-input-schema tool)))))
      (insert "  (no input schema registered)\n"))
    (insert "\n")

    ;; Recent Errors Ring
    (insert (propertize "RECENT ERRORS (last 10)\n" 'face 'bold))
    (let* ((ring (mcpkit-stats-record-recent-errors rec))
           (err-items (unless (ring-empty-p ring) (ring-elements ring))))
      (if (null err-items)
          (insert "  (no recent errors recorded)\n")
        (dolist (item err-items)
          (let ((err-time (plist-get item :time))
                (err-msg (plist-get item :message))
                (err-args (plist-get item :args)))
            (insert (format "  [%s] %s\n" err-time err-msg))
            (when err-args
              (insert (format "    arguments: %S\n" err-args)))))))
    (goto-char (point-min))))

;;; Source Navigation and Log Viewing

(defun mcpkit-stats-jump-to-source (&optional tool-name)
  "Jump to the Elisp source definition of TOOL-NAME."
  (interactive (list (or (tabulated-list-get-id)
                         mcpkit-stats-detail--current-tool
                         (read-string "Jump to tool source: "))))
  (let* ((tname (or tool-name (tabulated-list-get-id) mcpkit-stats-detail--current-tool))
         (rec (mcpkit-stats-get tname))
         (tool (and (boundp 'mcpkit--active-services)
                    (mcpkit--find-tool tname mcpkit--active-services)))
         (src-file (or (mcpkit-stats-record-source-file rec)
                       (and tool (mcpkit-tool-source-file tool))))
         (src-line (or (mcpkit-stats-record-source-line rec)
                       (and tool (mcpkit-tool-source-line tool)))))
    (cond
     ((and src-file (file-exists-p src-file))
      (find-file-other-window src-file)
      (when src-line
        (goto-char (point-min))
        (forward-line (1- src-line)))
      (when (fboundp 'pulse-momentary-highlight-one-line)
        (pulse-momentary-highlight-one-line (point))))
     ((and tool (functionp (mcpkit-tool-handler tool)) (symbolp (mcpkit-tool-handler tool)))
      (find-function-other-window (mcpkit-tool-handler tool)))
     (t
      (message "No source file location recorded for tool %s" tname)))))

(defun mcpkit-stats-view-logs (&optional tool-name)
  "Open `*mcpkit-logs*' and highlight or search for TOOL-NAME entries."
  (interactive (list (or (tabulated-list-get-id)
                         mcpkit-stats-detail--current-tool
                         (read-string "View logs for tool: "))))
  (let* ((tname (or tool-name (tabulated-list-get-id) mcpkit-stats-detail--current-tool))
         (log-buf (get-buffer-create (or mcpkit-log-buffer-name "*mcpkit-logs*"))))
    (pop-to-buffer log-buf)
    (when (and tname (not (string-empty-p tname)))
      (goto-char (point-max))
      (if (search-backward tname nil t)
          (message "Found recent log entry for %s" tname)
        (message "No log entries found for %s in %s" tname (buffer-name log-buf))))))

(provide 'mcpkit-stats)

;;; mcpkit-stats.el ends here
