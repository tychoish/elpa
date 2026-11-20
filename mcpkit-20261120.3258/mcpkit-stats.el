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
(require 'logx)
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

(defcustom mcpkit-stats-file nil
  "File where per-tool statistics aggregates are persisted, or nil.
Nil disables persistence.  Only aggregates are written (counts, durations,
top argument digests and usage score); recent errors and request arguments
are never saved.  Set this before mcpkit loads, e.g. through a state
directory mapping, so the file is read when `mcpkit-stats' loads."
  :type '(choice (const :tag "No persistence" nil) file)
  :group 'mcpkit-stats)

(defcustom mcpkit-stats-save-interval 300
  "Seconds of idle time before dirty statistics are written to disk."
  :type 'integer
  :group 'mcpkit-stats)

(defcustom mcpkit-stats-half-life-days 14
  "Half-life in days of the decayed usage score used for pinning."
  :type 'number
  :group 'mcpkit-stats)

(defconst mcpkit-stats--file-version 1
  "Version of the on-disk statistics format.")

(defconst mcpkit-stats--saved-digest-limit 20
  "Number of top argument digests persisted per tool.")

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
  (digests (make-hash-table :test #'equal) :type hash-table)
  (score 0.0 :type float)
  (score-time nil :type (or null number))
  (source-file nil :type (or null string))
  (source-line nil :type (or null integer)))

(defcustom mcpkit-stats-hidden-tools
  '("search_tools" "invoke_tool" "discover_services")
  "Tool names omitted from the statistics list.
These are the synthetic gateway tools.  Calls made through `invoke_tool' are
also recorded under the target tool's own name, so listing the gateway would
double count them.  Records are still collected, only the display hides them."
  :type '(repeat string)
  :group 'mcpkit)

;;; Argument Digests

(defconst mcpkit-stats--digest-descend-forms
  '(progn let let* condition-case unwind-protect)
  "Forms the eval digester looks inside to find the first real call.")

(defun mcpkit-stats--digest-find-call (form)
  "Return the head symbol of the first non-special-form call within FORM, or nil."
  (when (and (consp form) (symbolp (car form)))
    (let ((head (car form)))
      (cond
       ((eq head 'progn) (seq-some #'mcpkit-stats--digest-find-call (cdr form)))
       ((memq head '(let let*)) (seq-some #'mcpkit-stats--digest-find-call (cddr form)))
       ((eq head 'condition-case) (mcpkit-stats--digest-find-call (nth 2 form)))
       ((eq head 'unwind-protect) (mcpkit-stats--digest-find-call (nth 1 form)))
       ((special-form-p head) nil)
       (t head)))))

(defun mcpkit-stats--eval-digest (args)
  "Return a short digest key for an eval tool call with request ARGS.
Reads the `:expression' string and returns the name of the first
function called, looking inside `progn', `let', `let*', `condition-case'
and `unwind-protect'.  Return \"<unparsed>\" if it cannot be read."
  (condition-case nil
      (let* ((expr (plist-get args :expression))
             (form (if (stringp expr)
                       (car (read-from-string expr))
                     (signal 'wrong-type-argument (list 'stringp expr)))))
        (cond
         ((not (consp form)) "<atom>")
         ((not (symbolp (car form))) "<non-symbol>")
         (t (symbol-name (or (mcpkit-stats--digest-find-call form) (car form))))))
    (error "<unparsed>")))

(defcustom mcpkit-stats-argument-digesters
  '(("emacs__eval" . mcpkit-stats--eval-digest)
    ("emacs__emacs_eval" . mcpkit-stats--eval-digest)
    ("emacs_eval" . mcpkit-stats--eval-digest))
  "Alist of (TOOL-NAME . FUNCTION) producing per-call argument digests.
FUNCTION receives the request arguments plist and returns a short string
key.  Keys are counted per tool and shown in the tool detail view.
Errors signaled by FUNCTION are ignored."
  :type '(alist :key-type string :value-type function)
  :group 'mcpkit-stats)

;;; Table, Score and Persistence State

(defvar mcpkit-stats--table (make-hash-table :test #'equal)
  "Hash table mapping tool-name strings to `mcpkit-stats-record' instances.")

(defvar mcpkit-stats--canonical-table mcpkit-stats--table
  "The live statistics table captured when this library loaded.
Persistence only happens while `mcpkit-stats--table' is this object, so
tests that bind a private table never touch `mcpkit-stats-file'.")

(defvar mcpkit-stats--dirty nil
  "Non-nil when the live table has changes not yet written to disk.")

(defvar mcpkit-stats--last-saved nil
  "Timestamp string of the last successful save or load, or nil.")

(defvar mcpkit-stats--synced-stamp nil
  "The `:saved' stamp of the file contents already reflected in memory.
Used so loading the same file twice does not double count.")

(defvar mcpkit-stats--timer nil
  "Pending idle timer that will save dirty statistics, or nil.")

(defun mcpkit-stats--worker-p ()
  "Return non-nil in a sprite worker daemon, where nothing is persisted."
  (and (fboundp 'sprite-worker-p)
       (condition-case nil (and (funcall 'sprite-worker-p) t) (error nil))))

(defun mcpkit-stats--persist-p ()
  "Return non-nil when the live table may be saved to `mcpkit-stats-file'."
  (and (stringp mcpkit-stats-file)
       (not (string-empty-p mcpkit-stats-file))
       (eq mcpkit-stats--table mcpkit-stats--canonical-table)
       (not (mcpkit-stats--worker-p))))

(defun mcpkit-stats--decay (score from to)
  "Return SCORE decayed from time FROM to time TO (float seconds)."
  (if (or (null from) (<= score 0.0))
      0.0
    (let ((half (* 86400.0 (max 1e-9 mcpkit-stats-half-life-days))))
      (* score (expt 0.5 (/ (max 0.0 (- to from)) half))))))

(defun mcpkit-stats-score (tool-name &optional now)
  "Return the decayed usage score of TOOL-NAME as of NOW (float seconds).
NOW defaults to the current time.  Unknown tools score 0.0."
  (let ((rec (gethash (format "%s" tool-name) mcpkit-stats--table)))
    (if rec
        (mcpkit-stats--decay (mcpkit-stats-record-score rec)
                             (mcpkit-stats-record-score-time rec)
                             (or now (float-time)))
      0.0)))

(defun mcpkit-stats-usage-entries (&optional now)
  "Return (NAME CALLS SCORE) for every recorded tool, SCORE decayed to NOW."
  (let (entries)
    (maphash (lambda (name rec)
               (push (list name (mcpkit-stats-record-call-count rec)
                           (mcpkit-stats-score name now))
                     entries))
             mcpkit-stats--table)
    entries))

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
  "Reset telemetry metrics and return the sorted list of cleared tool names.
If TOOL-NAME is provided, reset only that tool.  Otherwise reset all.
The persisted file, when enabled, is updated immediately."
  (interactive)
  (let (cleared)
    (if tool-name
        (let ((tname (if (symbolp tool-name) (symbol-name tool-name) (format "%s" tool-name))))
          (when (gethash tname mcpkit-stats--table)
            (push tname cleared))
          (remhash tname mcpkit-stats--table))
      (maphash (lambda (k _v) (push k cleared)) mcpkit-stats--table)
      (clrhash mcpkit-stats--table))
    (mcpkit-stats--mark-dirty)
    (when (mcpkit-stats--persist-p)
      (mcpkit-stats-save))
    (when (get-buffer "*mcpkit-stats*")
      (with-current-buffer "*mcpkit-stats*"
        (revert-buffer)))
    (sort cleared #'string<)))

(defun mcpkit-stats--known-tool-p (tool-name)
  "Return non-nil if TOOL-NAME is an active tool or a synthetic gateway tool."
  (let ((name (format "%s" tool-name)))
    (and (or (member name mcpkit--gateway-tool-names)
             (and (boundp 'mcpkit--active-services)
                  (mcpkit--find-tool name mcpkit--active-services)))
         t)))

(defun mcpkit-stats--record-digest (rec tool-name req-args)
  "Count the argument digest of REQ-ARGS for TOOL-NAME in record REC.
Never signals."
  (ignore-errors
    (when-let* ((fn (cdr (assoc tool-name mcpkit-stats-argument-digesters)))
                (key (funcall fn req-args)))
      (when (stringp key)
        (cl-incf (gethash key (mcpkit-stats-record-digests rec) 0))))))

(defun mcpkit-stats-record-call (tool-name status duration-ms &optional err-msg req-args)
  "Record execution metrics for TOOL-NAME.
STATUS is `success' or `error'.  DURATION-MS is execution time in milliseconds.
ERR-MSG is the error message string on failure.
REQ-ARGS is the argument payload passed to the tool.
Calls to names that are neither active tools nor synthetic gateway tools
are ignored."
  (condition-case nil
      (when (and tool-name (not (string-empty-p (format "%s" tool-name)))
                 (mcpkit-stats--known-tool-p tool-name))
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
          (mcpkit-stats--record-digest rec (format "%s" tool-name) req-args)
          (let ((now (float-time)))
            (setf (mcpkit-stats-record-score rec)
                  (+ 1.0 (mcpkit-stats--decay (mcpkit-stats-record-score rec)
                                              (mcpkit-stats-record-score-time rec)
                                              now))
                  (mcpkit-stats-record-score-time rec) now))
          (mcpkit-stats--mark-dirty)
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

;;; Persistence

(defun mcpkit-stats--tool-plist (rec)
  "Return the persistable aggregate plist for record REC.
Never includes the recent errors ring or any request arguments."
  (list :tool (mcpkit-stats-record-tool-name rec)
        :service (mcpkit-stats-record-service rec)
        :calls (mcpkit-stats-record-call-count rec)
        :successes (mcpkit-stats-record-success-count rec)
        :errors (mcpkit-stats-record-error-count rec)
        :total-ms (mcpkit-stats-record-total-duration-ms rec)
        :min-ms (mcpkit-stats-record-min-duration-ms rec)
        :max-ms (mcpkit-stats-record-max-duration-ms rec)
        :last-called-at (mcpkit-stats-record-last-called-at rec)
        :digests (mcpkit-stats--top-digests
                  rec mcpkit-stats--saved-digest-limit)
        :score (mcpkit-stats-record-score rec)
        :score-time (mcpkit-stats-record-score-time rec)))

(defun mcpkit-stats--stamp ()
  "Return a high resolution timestamp string for the saved file."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%3N%z"))

(defun mcpkit-stats--build-form (stamp)
  "Return the `mcpkit-stats' form for the live table, stamped with STAMP."
  (let (tools)
    (maphash (lambda (_k rec) (push (mcpkit-stats--tool-plist rec) tools))
             mcpkit-stats--table)
    (list 'mcpkit-stats :version mcpkit-stats--file-version
          :saved stamp
          :tools (sort tools (lambda (a b) (string< (plist-get a :tool)
                                                    (plist-get b :tool)))))))

(defun mcpkit-stats--write-atomically (file form)
  "Write FORM readably to FILE through a temp file and an atomic rename."
  (let* ((file (expand-file-name file))
         (dir (file-name-directory file))
         (tmp nil))
    (make-directory dir t)
    (setq tmp (make-temp-file (expand-file-name ".mcpkit-stats-" dir) nil ".tmp"))
    (condition-case err
        (progn
          (set-file-modes tmp #o600)
          (with-temp-buffer
            (let ((print-length nil) (print-level nil) (print-circle nil)
                  (print-escape-newlines t)
                  (standard-output (current-buffer))
                  (coding-system-for-write 'utf-8-unix))
              (insert ";; mcpkit statistics aggregates; safe to delete.\n")
              (prin1 form)
              (insert "\n")
              (write-region (point-min) (point-max) tmp nil 'silent)))
          (rename-file tmp file t)
          (set-file-modes file #o600))
      (error (ignore-errors (delete-file tmp))
             (signal (car err) (cdr err))))))

;;;###autoload
(defun mcpkit-stats-save ()
  "Write the statistics aggregates to `mcpkit-stats-file'.
Do nothing (and return nil) unless persistence is enabled for the live
table.  Return the file name on success.  Never signals."
  (interactive)
  (when (mcpkit-stats--persist-p)
    (condition-case err
        (let ((stamp (mcpkit-stats--stamp)))
          (mcpkit-stats--write-atomically mcpkit-stats-file
                                          (mcpkit-stats--build-form stamp))
          (setq mcpkit-stats--dirty nil
                mcpkit-stats--last-saved stamp
                mcpkit-stats--synced-stamp stamp)
          (when (timerp mcpkit-stats--timer)
            (cancel-timer mcpkit-stats--timer))
          (setq mcpkit-stats--timer nil)
          mcpkit-stats-file)
      (error
       (logx-warn :cat 'mcpkit-stats
                  :msg (format "Could not save statistics to %s: %s"
                               mcpkit-stats-file (error-message-string err)))
       nil))))

(defun mcpkit-stats--idle-save ()
  "Idle timer action: save when dirty."
  (setq mcpkit-stats--timer nil)
  (when mcpkit-stats--dirty
    (mcpkit-stats-save)))

(defun mcpkit-stats--mark-dirty ()
  "Flag unsaved changes and schedule an idle save when persistence is on."
  (when (eq mcpkit-stats--table mcpkit-stats--canonical-table)
    (setq mcpkit-stats--dirty t))
  (when (and (null mcpkit-stats--timer) (mcpkit-stats--persist-p))
    (setq mcpkit-stats--timer
          (run-with-idle-timer mcpkit-stats-save-interval nil
                               #'mcpkit-stats--idle-save))))

(defun mcpkit-stats--save-on-exit ()
  "Save dirty statistics from `kill-emacs-hook'."
  (when mcpkit-stats--dirty
    (ignore-errors (mcpkit-stats-save))))

(defun mcpkit-stats--merge-score (rec score time)
  "Merge persisted SCORE at TIME into record REC."
  (when (and (numberp score) (numberp time))
    (let* ((own-time (mcpkit-stats-record-score-time rec))
           (to (max time (or own-time time))))
      (setf (mcpkit-stats-record-score rec)
            (float (+ (mcpkit-stats--decay score time to)
                      (mcpkit-stats--decay (mcpkit-stats-record-score rec)
                                           own-time to)))
            (mcpkit-stats-record-score-time rec) to))))

(defun mcpkit-stats--merge-entry (entry)
  "Merge one persisted tool plist ENTRY into the live table."
  (let ((name (plist-get entry :tool)))
    (when (and (stringp name) (not (string-empty-p name)))
      (let* ((rec (mcpkit-stats-get name))
             (calls (plist-get entry :calls))
             (min-ms (plist-get entry :min-ms))
             (svc (plist-get entry :service))
             (last (plist-get entry :last-called-at)))
        (when (and (null (mcpkit-stats-record-service rec)) (symbolp svc))
          (setf (mcpkit-stats-record-service rec) svc))
        (let ((had-calls (> (mcpkit-stats-record-call-count rec) 0)))
          (cl-incf (mcpkit-stats-record-call-count rec) (if (integerp calls) calls 0))
          (cl-incf (mcpkit-stats-record-success-count rec)
                   (if (integerp (plist-get entry :successes)) (plist-get entry :successes) 0))
          (cl-incf (mcpkit-stats-record-error-count rec)
                   (if (integerp (plist-get entry :errors)) (plist-get entry :errors) 0))
          (cl-incf (mcpkit-stats-record-total-duration-ms rec)
                   (if (numberp (plist-get entry :total-ms)) (float (plist-get entry :total-ms)) 0.0))
          (when (numberp min-ms)
            (setf (mcpkit-stats-record-min-duration-ms rec)
                  (if (and had-calls (> (mcpkit-stats-record-min-duration-ms rec) 0.0))
                      (float (min min-ms (mcpkit-stats-record-min-duration-ms rec)))
                    (float min-ms))))
          (when (numberp (plist-get entry :max-ms))
            (setf (mcpkit-stats-record-max-duration-ms rec)
                  (float (max (plist-get entry :max-ms)
                              (mcpkit-stats-record-max-duration-ms rec))))))
        (when (and (stringp last)
                   (or (null (mcpkit-stats-record-last-called-at rec))
                       (string> last (mcpkit-stats-record-last-called-at rec))))
          (setf (mcpkit-stats-record-last-called-at rec) last))
        (dolist (d (plist-get entry :digests))
          (when (and (consp d) (stringp (car d)) (integerp (cdr d)))
            (cl-incf (gethash (car d) (mcpkit-stats-record-digests rec) 0) (cdr d))))
        (mcpkit-stats--merge-score rec (plist-get entry :score)
                                   (plist-get entry :score-time))))))

;;;###autoload
(defun mcpkit-stats-load (&optional force)
  "Merge the statistics in `mcpkit-stats-file' into the live table.
Counts are added, so a file already reflected in memory (same stamp) is
skipped unless FORCE (prefix argument) is non-nil.  Unreadable files or
unknown versions are ignored with a logged warning.  Never signals.
Return non-nil if a file was merged."
  (interactive "P")
  (when (and (mcpkit-stats--persist-p) (file-readable-p mcpkit-stats-file))
    (condition-case err
        (let* ((form (with-temp-buffer
                       (insert-file-contents mcpkit-stats-file)
                       (goto-char (point-min))
                       (read (current-buffer))))
               (plist (and (consp form) (eq (car form) 'mcpkit-stats) (cdr form))))
          (cond
           ((not (and plist (eql (plist-get plist :version) mcpkit-stats--file-version)))
            (logx-warn :cat 'mcpkit-stats
                       :msg (format "Ignoring statistics file %s: unknown format or version"
                                    mcpkit-stats-file))
            nil)
           ((and (not force) (equal (plist-get plist :saved) mcpkit-stats--synced-stamp))
            nil)
           (t
            (dolist (entry (plist-get plist :tools))
              (condition-case nil
                  (mcpkit-stats--merge-entry entry)
                (error nil)))
            (setq mcpkit-stats--synced-stamp (plist-get plist :saved)
                  mcpkit-stats--last-saved (plist-get plist :saved))
            t)))
      (error
       (logx-warn :cat 'mcpkit-stats
                  :msg (format "Could not read statistics file %s: %s"
                               mcpkit-stats-file (error-message-string err)))
       nil))))

(defun mcpkit-stats-persistence-info ()
  "Return a plist (:persisted FILE-OR-NIL :last-saved STAMP-OR-NIL)."
  (list :persisted (and (mcpkit-stats--persist-p) mcpkit-stats-file)
        :last-saved mcpkit-stats--last-saved))

(add-hook 'kill-emacs-hook #'mcpkit-stats--save-on-exit)

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
         ("Score" 7 t)
         ("Pin" 8 t)
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
       (unless (member tname mcpkit-stats-hidden-tools)
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
                               (format "%.2f" (mcpkit-stats-score tname))
                               (or (mcpkit-stats--pin-kind tname) "-")
                               last-run))
                 entries))))
     mcpkit-stats--table)
    ;; Next include registered tools that haven't been invoked yet
    (when (boundp 'mcpkit--active-services)
      (dolist (tool (mcpkit--normalize-tools mcpkit--active-services))
        (let ((tname (mcpkit-tool-name tool)))
          (unless (or (gethash tname seen) (member tname mcpkit-stats-hidden-tools))
            (puthash tname t seen)
            (push (list tname
                        (vector tname
                                (format "%s" (or (mcpkit-tool-service tool) "-"))
                                "0" "0" "0" "0.0%" "0.00" "0.00"
                                (or (mcpkit-stats--pin-kind tname) "-") "never"))
                  entries)))))
    (setq tabulated-list-entries (nreverse entries))))

(defun mcpkit-stats--update-mode-line ()
  "Show the persistence file and last save time in the mode line."
  (let ((info (mcpkit-stats-persistence-info)))
    (setq mode-line-process
          (format " [%s%s]"
                  (or (plist-get info :persisted) "not persisted")
                  (if (plist-get info :last-saved)
                      (format ", saved %s" (plist-get info :last-saved))
                    "")))))

(defun mcpkit-stats-refresh ()
  "Refresh the statistics table."
  (interactive)
  (mcpkit-stats-tabulated-entries)
  (mcpkit-stats--update-mode-line)
  (tabulated-list-print t))

(defun mcpkit-stats-clear-prompt ()
  "Prompt to clear all telemetry statistics."
  (interactive)
  (when (y-or-n-p "Reset all MCPKit tool statistics? ")
    (mcpkit-stats-reset)
    (mcpkit-stats-refresh)
    (logx-info :cat 'mcpkit-stats :msg "MCPKit tool statistics reset.")
    (message "MCPKit tool statistics reset.")))

;;;###autoload
(defun mcpkit-stats ()
  "Open the MCPKit tool usage statistics monitor."
  (interactive)
  (let ((buf (get-buffer-create "*mcpkit-stats*")))
    (with-current-buffer buf
      (mcpkit-stats-mode)
      (mcpkit-stats-tabulated-entries)
      (mcpkit-stats--update-mode-line)
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

(defun mcpkit-stats--top-digests (rec n)
  "Return up to N (DIGEST . COUNT) pairs from REC, most frequent first."
  (let (pairs)
    (maphash (lambda (k v) (push (cons k v) pairs)) (mcpkit-stats-record-digests rec))
    (seq-take (sort pairs (lambda (a b) (or (> (cdr a) (cdr b))
                                            (and (= (cdr a) (cdr b))
                                                 (string< (car a) (car b))))))
              n)))

(declare-function mcpkit-search-pin-kind "mcpkit")

(defun mcpkit-stats--pin-kind (tool-name)
  "Return how TOOL-NAME is pinned (\"static\", \"service\", \"usage\") or nil."
  (ignore-errors (mcpkit-search-pin-kind tool-name)))

(defun mcpkit-stats-rows (&optional tool limit)
  "Return recorded statistics as a list of plists, busiest tool first.
Tools in `mcpkit-stats-hidden-tools' are omitted.  If TOOL is non-nil
keep only that tool (resolved through `mcpkit-resolve-tool-name').  LIMIT
caps the number of rows.  Each row also carries the decayed usage `:score'
and `:pinned' (how it is pinned in gated `tools/list', or nil)."
  (let* ((wanted (and tool (or (mcpkit-resolve-tool-name tool) tool)))
         (rows nil))
    (maphash
     (lambda (tname rec)
       (unless (or (member tname mcpkit-stats-hidden-tools)
                   (and wanted (not (equal tname wanted))))
         (let ((calls (mcpkit-stats-record-call-count rec)))
           (push (list :tool tname
                       :service (format "%s" (or (mcpkit-stats-record-service rec) "-"))
                       :calls calls
                       :errors (mcpkit-stats-record-error-count rec)
                       :avg_ms (if (> calls 0)
                                   (/ (mcpkit-stats-record-total-duration-ms rec) calls)
                                 0.0)
                       :last_run (or (mcpkit-stats-record-last-called-at rec) "never")
                       :score (/ (fround (* 1000.0 (mcpkit-stats-score tname))) 1000.0)
                       :pinned (mcpkit-stats--pin-kind tname)
                       :top_digests (vconcat
                                     (mapcar (lambda (d) (list :digest (car d) :count (cdr d)))
                                             (mcpkit-stats--top-digests rec 10))))
                 rows))))
     mcpkit-stats--table)
    (setq rows (sort rows (lambda (a b)
                            (or (> (plist-get a :calls) (plist-get b :calls))
                                (and (= (plist-get a :calls) (plist-get b :calls))
                                     (string< (plist-get a :tool) (plist-get b :tool)))))))
    (if (and (integerp limit) (> limit 0)) (seq-take rows limit) rows)))

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
    (insert (format "  Usage Score:       %.2f (pinned: %s)\n"
                    (mcpkit-stats-score tool-name)
                    (or (mcpkit-stats--pin-kind tool-name) "no")))
    (let ((info (mcpkit-stats-persistence-info)))
      (insert (format "  Persisted To:      %s (last saved %s)\n"
                      (or (plist-get info :persisted) "(not persisted)")
                      (or (plist-get info :last-saved) "never"))))
    (insert "\n")

    ;; Argument digests
    (let ((top (mcpkit-stats--top-digests rec 10)))
      (when top
        (insert (propertize "TOP ARGUMENT DIGESTS (top 10)\n" 'face 'bold))
        (dolist (d top)
          (insert (format "  %-40s %d\n" (car d) (cdr d))))
        (insert "\n")))

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
      (logx-warn :cat 'mcpkit-stats :msg (format "No source file location recorded for tool %s" tname))
      (message "No source file location recorded for tool %s" tname)))))

(defun mcpkit-stats-view-logs (&optional tool-name)
  "Open filtered log viewer for TOOL-NAME using logx-view or `*mcpkit-logs*'."
  (interactive (list (or (tabulated-list-get-id)
                         mcpkit-stats-detail--current-tool
                         (read-string "View logs for tool: "))))
  (let* ((tname (or tool-name (tabulated-list-get-id) mcpkit-stats-detail--current-tool)))
    (if (and (require 'logx-view nil t) (fboundp 'logx-fork-filtered-buffer))
        (let ((buf (logx-fork-filtered-buffer
                    (format "mcpkit:%s" (or tname "all"))
                    (lambda (ev)
                      (let ((cat (format "%s" (or (plist-get ev :cat) "")))
                            (tool (plist-get ev :tool)))
                        (and (string-prefix-p "mcpkit" cat)
                             (or (null tname) (string-empty-p tname)
                                 (equal tool tname))))))))
          (pop-to-buffer buf))
      (let ((log-buf (get-buffer-create (or mcpkit-log-buffer-name "*mcpkit-logs*"))))
        (pop-to-buffer log-buf)
        (when (and tname (not (string-empty-p tname)))
          (goto-char (point-max))
          (if (search-backward tname nil t)
              (progn
                (logx-info :cat 'mcpkit-stats :msg (format "Found recent log entry for %s" tname))
                (message "Found recent log entry for %s" tname))
            (logx-warn :cat 'mcpkit-stats :msg (format "No log entries found for %s in %s" tname (buffer-name log-buf)))
            (message "No log entries found for %s in %s" tname (buffer-name log-buf))))))))

(mcpkit-stats-load)

(provide 'mcpkit-stats)

;;; mcpkit-stats.el ends here
