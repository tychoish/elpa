;;; logx-view.el --- Structured log viewer specialization for jsonl-view -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, logging
;; URL: https://github.com/tychoish/logx

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Provides log-specific presentation, semantic coloring, filtering, and live
;; ring-buffer bridging for `logx' layered on top of `jsonl-view-mode'.
;;
;; All faces inherit strictly from standard Emacs semantic faces (warning,
;; error, shadow, font-lock-builtin-face) without hardcoded hex colors,
;; ensuring seamless adaptability across light/dark and Modus themes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'jsonl-view)
(require 'logx)

(defgroup logx-view nil
  "Structured log viewing interface layered on jsonl-view."
  :group 'logx
  :prefix "logx-view-")

;;; ----------------------------------------------------------------------------
;;; Semantic Faces (Zero Hardcoded Hex Colors)
;;; ----------------------------------------------------------------------------

(defface logx-view-timestamp
  '((default :inherit shadow))
  "Face for log event timestamp column."
  :group 'logx-view)

(defface logx-view-level-debug
  '((default :inherit shadow))
  "Face for debug log level."
  :group 'logx-view)

(defface logx-view-level-info
  '((default :inherit font-lock-builtin-face))
  "Face for info log level."
  :group 'logx-view)

(defface logx-view-level-notice
  '((default :inherit font-lock-function-name-face))
  "Face for notice log level."
  :group 'logx-view)

(defface logx-view-level-warn
  '((default :inherit warning))
  "Face for warn log level."
  :group 'logx-view)

(defface logx-view-level-error
  '((default :inherit error))
  "Face for error log level."
  :group 'logx-view)

(defface logx-view-level-critical
  '((default :inherit error :weight bold))
  "Face for critical log level."
  :group 'logx-view)

(defface logx-view-level-alert
  '((default :inherit error :underline t :weight bold))
  "Face for alert log level."
  :group 'logx-view)

(defface logx-view-level-fatal
  '((default :inherit error :inverse-video t :weight bold))
  "Face for fatal log level."
  :group 'logx-view)

(defface logx-view-category
  '((default :inherit font-lock-type-face))
  "Face for category/logger name."
  :group 'logx-view)

(defface logx-view-key
  '((default :inherit font-lock-variable-name-face))
  "Face for structured attribute keys."
  :group 'logx-view)

(defface logx-view-source-link
  '((default :inherit link))
  "Face for call-site origin hyperlinks."
  :group 'logx-view)

;;; ----------------------------------------------------------------------------
;;; Customization & Schema Mappings
;;; ----------------------------------------------------------------------------

(defcustom logx-view-render-interval 0.05
  "Interval in seconds between throttled batch flushes to visible viewer buffers."
  :type 'number
  :group 'logx-view)

(defcustom logx-view-buffer-max-lines 10000
  "Maximum number of lines retained in live viewer buffers before truncation."
  :type 'integer
  :group 'logx-view)

(defcustom logx-view-schema-mappings
  '((:ts  . ("ts" "time" "timestamp" "@timestamp"))
    (:lvl . ("level" "lvl" "severity" "log_level"))
    (:cat . ("cat" "category" "logger" "subsystem" "component"))
    (:msg . ("msg" "message" "event" "summary"))
    (:src . ("src" "caller" "source" "file")))
  "Alist mapping standard semantic keys to candidate JSON property names."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'logx-view)

;;; ----------------------------------------------------------------------------
;;; Buffer-Local State
;;; ----------------------------------------------------------------------------

(defvar-local logx-view-timestamp-mode :time
  "Current timestamp display mode (:time, :delta, or :iso).")

(defvar-local logx-view-min-level nil
  "Active minimum severity filter symbol, or nil if unfiltered.")

(defvar-local logx-view-category-filter nil
  "Active category filter regex string, or nil if unfiltered.")

;;; ----------------------------------------------------------------------------
;;; Schema Projection & Formatting
;;; ----------------------------------------------------------------------------

(defun logx-view--find-prop (plist candidates)
  "Find first property in PLIST matching any key in CANDIDATES strings."
  (let ((found nil)
        (tail plist))
    (while (and tail (not found))
      (let* ((key-sym (car tail))
             (key-str (if (keywordp key-sym)
                          (substring (symbol-name key-sym) 1)
                        (symbol-name key-sym))))
        (when (member (downcase key-str) candidates)
          (setq found (cadr tail))))
      (setq tail (cddr tail)))
    found))

;;;###autoload
(defun logx-view-project-record (record)
  "Project raw JSON plist RECORD into standardized log components.
Returns a plist (:ts TS :lvl LVL :cat CAT :msg MSG :src SRC :data DATA)."
  (if (null record)
      nil
    (let* ((ts-cand (alist-get :ts logx-view-schema-mappings))
           (lvl-cand (alist-get :lvl logx-view-schema-mappings))
           (cat-cand (alist-get :cat logx-view-schema-mappings))
           (msg-cand (alist-get :msg logx-view-schema-mappings))
           (src-cand (alist-get :src logx-view-schema-mappings))
           (ts (logx-view--find-prop record ts-cand))
           (lvl-raw (logx-view--find-prop record lvl-cand))
           (cat (logx-view--find-prop record cat-cand))
           (msg (logx-view--find-prop record msg-cand))
           (src (logx-view--find-prop record src-cand))
           (lvl (when lvl-raw
                  (let ((s (downcase (format "%s" lvl-raw))))
                    (cond
                     ((string-match-p "debug\\|trace" s) 'debug)
                     ((string-match-p "info" s) 'info)
                     ((string-match-p "notice" s) 'notice)
                     ((string-match-p "warn" s) 'warn)
                     ((string-match-p "crit" s) 'critical)
                     ((string-match-p "alert" s) 'alert)
                     ((string-match-p "err" s) 'error)
                     ((string-match-p "fatal\\|emerg" s) 'fatal)
                     (t (intern s))))))
           (data nil)
           (tail record))
      (while tail
        (let* ((k (car tail))
               (v (cadr tail))
               (k-str (if (keywordp k) (substring (symbol-name k) 1) (symbol-name k))))
          (unless (or (member (downcase k-str) ts-cand)
                      (member (downcase k-str) lvl-cand)
                      (member (downcase k-str) cat-cand)
                      (member (downcase k-str) msg-cand)
                      (member (downcase k-str) src-cand)
                      (null v)
                      (eq v :null))
            (push k data)
            (push v data)))
        (setq tail (cddr tail)))
      (list :ts ts
            :lvl (or lvl 'info)
            :cat (or cat "root")
            :msg (or msg "")
            :src src
            :data (nreverse data)))))

(defun logx-view--level-face (lvl)
  "Return face corresponding to severity symbol LVL."
  (pcase lvl
    ('debug    'logx-view-level-debug)
    ('info     'logx-view-level-info)
    ('notice   'logx-view-level-notice)
    ('warn     'logx-view-level-warn)
    ('error    'logx-view-level-error)
    ('critical 'logx-view-level-critical)
    ('alert    'logx-view-level-alert)
    ('fatal    'logx-view-level-fatal)
    (_         'logx-view-level-info)))

(defun logx-view--format-timestamp (ts-val mode &optional prev-ts)
  "Format timestamp TS-VAL according to MODE (:time, :delta, :iso)."
  (let ((time (cond
               ((numberp ts-val) (seconds-to-time ts-val))
               ((stringp ts-val)
                (condition-case nil
                    (date-to-time ts-val)
                  (error (current-time))))
               (t (current-time)))))
    (pcase mode
      (:iso
       (format-time-string "%Y-%m-%dT%H:%M:%S" time))
      (:delta
       (if (and prev-ts (numberp ts-val) (numberp prev-ts))
           (let ((diff-ms (* 1000.0 (- ts-val prev-ts))))
             (if (< (abs diff-ms) 1000)
                 (format "+%.0fms" diff-ms)
               (format "+%.1fs" (/ diff-ms 1000.0))))
         "+0ms"))
      (_
       (format-time-string "%H:%M:%S" time)))))

(defun logx-view--source-jump-action (btn)
  "Jump to source location referenced by BTN."
  (let ((src (button-get btn 'logx-source)))
    (when src
      (let ((path (or (plist-get src :path) (plist-get src :file)))
            (line (plist-get src :line))
            (fn (plist-get src :fn)))
        (if path
            (progn
              (find-file-other-window path)
              (cond
               ((and line (numberp line) (> line 0))
                (goto-char (point-min))
                (forward-line (1- line)))
               (fn
                (goto-char (point-min))
                (or (re-search-forward (format "(\\(?:def[a-z*]+\\|cl-def[a-z*]+\\)\\s-+%s\\_>" (regexp-quote (symbol-name fn))) nil t)
                    (re-search-forward (regexp-quote (symbol-name fn)) nil t)))))
          (when fn
            (find-function fn)))))))

;;;###autoload
(defun logx-view-jump-to-source ()
  "Jump to the call-site source definition of record at point."
  (interactive)
  (let ((src (get-text-property (point) 'logx-source)))
    (if src
        (let ((path (or (plist-get src :path) (plist-get src :file)))
              (line (plist-get src :line))
              (fn (plist-get src :fn)))
          (if path
              (progn
                (find-file-other-window path)
                (cond
                 ((and line (numberp line) (> line 0))
                  (goto-char (point-min))
                  (forward-line (1- line)))
                 (fn
                  (goto-char (point-min))
                  (or (re-search-forward (format "(\\(?:def[a-z*]+\\|cl-def[a-z*]+\\)\\s-+%s\\_>" (regexp-quote (symbol-name fn))) nil t)
                      (re-search-forward (regexp-quote (symbol-name fn)) nil t)))))
            (if fn
                (find-function fn)
              (message "Source file path not available."))))
      (message "No source call-site metadata for record at point."))))

;;;###autoload
(defun logx-view-render-record (raw-record &optional prev-rec)
  "Render RAW-RECORD plist into formatted log string with semantic text properties."
  (let* ((proj (logx-view-project-record raw-record))
         (prev-ts (and prev-rec (plist-get (logx-view-project-record prev-rec) :ts)))
         (ts-val (plist-get proj :ts))
         (lvl (plist-get proj :lvl))
         (cat (plist-get proj :cat))
         (msg (plist-get proj :msg))
         (src (plist-get proj :src))
         (data (plist-get proj :data))
         (ts-str (logx-view--format-timestamp ts-val logx-view-timestamp-mode prev-ts))
         (lvl-str (format "%-8s" (upcase (symbol-name lvl)))))
    (with-temp-buffer
      ;; Timestamp
      (insert (propertize ts-str 'face 'logx-view-timestamp) " ")
      ;; Level badge
      (insert (propertize (format "[%s]" lvl-str) 'face (logx-view--level-face lvl)) " ")
      ;; Category
      (insert (propertize (format "%s:" cat) 'face 'logx-view-category) " ")
      ;; Message
      (insert (format "%s" msg))
      ;; Data key-values with disclosure
      (let ((tail data))
        (while tail
          (let* ((k (car tail))
                 (v (cadr tail)))
            (unless (or (null v) (eq v :null))
              (let* ((k-name (if (keywordp k) (substring (symbol-name k) 1) (format "%s" k)))
                     (v-str (format "%s" v)))
                (insert "  " (propertize (format "%s:" k-name) 'face 'logx-view-key) " ")
                (if (or (consp v) (hash-table-p v))
                    (insert-text-button "▶"
                                        'action #'jsonl-view--toggle-disclosure-action
                                        'jsonl-value v
                                        'jsonl-expanded nil
                                        'face 'jsonl-view-disclosure-button
                                        'help-echo "mouse-1, RET: expand payload"
                                        'follow-link t)
                  (if (> (length v-str) jsonl-view-truncate-length)
                      (progn
                        (insert (propertize (substring v-str 0 jsonl-view-truncate-length)
                                            'face 'font-lock-string-face))
                        (insert " ")
                        (insert-text-button "…"
                                            'action #'jsonl-view--toggle-disclosure-action
                                            'jsonl-value v
                                            'jsonl-expanded nil
                                            'face 'jsonl-view-disclosure-button
                                            'help-echo "mouse-1, RET: expand full text"
                                            'follow-link t))
                    (insert (propertize v-str 'face (if (numberp v) 'font-lock-constant-face 'font-lock-string-face))))))))
          (setq tail (cddr tail))))
      ;; Source hyperlink
      (when src
        (let* ((file (or (plist-get src :file)
                         (and (plist-get src :path) (file-name-nondirectory (plist-get src :path)))))
               (line (plist-get src :line))
               (fn (plist-get src :fn))
               (label (cond
                       ((and file line (numberp line) (> line 0)) (format "%s:%s" file line))
                       ((and file fn) (format "%s:%s" file fn))
                       (file file)
                       (fn (format "%s" fn))
                       (t "[src]"))))
          (insert "   ")
          (insert-text-button label
                              'action #'logx-view--source-jump-action
                              'logx-source src
                              'face 'logx-view-source-link
                              'help-echo "mouse-1, RET: visit source"
                              'follow-link t)))
      (insert "\n")
      (propertize (buffer-string)
                  'jsonl-record raw-record
                  'logx-ts ts-val
                  'logx-lvl lvl
                  'logx-cat cat
                  'logx-source src))))

;;; ----------------------------------------------------------------------------
;;; Timestamp Cycling & Filtering
;;; ----------------------------------------------------------------------------

;;;###autoload
(defun logx-view-cycle-timestamp-mode ()
  "Cycle timestamp display mode between Time-of-day, Delta, and ISO-8601."
  (interactive)
  (setq logx-view-timestamp-mode
        (pcase logx-view-timestamp-mode
          (:time :delta)
          (:delta :iso)
          (_ :time)))
  (let ((inhibit-read-only t)
        (pos (point-min))
        (prev-ts nil))
    (save-excursion
      (while (< pos (point-max))
        (goto-char pos)
        (let ((ts-val (get-text-property pos 'logx-ts)))
          (when ts-val
            (let ((eol (line-end-position))
                  (new-str (logx-view--format-timestamp ts-val logx-view-timestamp-mode prev-ts)))
              (put-text-property pos (min (+ pos (length new-str)) eol)
                                 'display new-str)
              (setq prev-ts (if (numberp ts-val) ts-val prev-ts))))
          (forward-line 1)
          (setq pos (point))))))
  (message "Timestamp mode: %s" logx-view-timestamp-mode))

;;;###autoload
(defun logx-view-filter-level (level)
  "Filter buffer to show only log entries with severity >= LEVEL."
  (interactive
   (list (intern (completing-read "Minimum severity: "
                                  '("debug" "info" "notice" "warn" "error" "critical" "alert" "fatal")
                                  nil t nil nil "info"))))
  (setq logx-view-min-level level)
  (logx-view--apply-filters)
  (message "Filtered minimum level: %s" level))

;;;###autoload
(defun logx-view-filter-category (regex)
  "Filter buffer to show only log entries matching category REGEX."
  (interactive "sFilter category (regex): ")
  (setq logx-view-category-filter (if (string-empty-p regex) nil regex))
  (logx-view--apply-filters)
  (message "Filtered category: %s" (or logx-view-category-filter "all")))

;;;###autoload
(defun logx-view-clear-filters ()
  "Clear active severity and category filters, restoring all lines."
  (interactive)
  (setq logx-view-min-level nil
        logx-view-category-filter nil)
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max) '(invisible nil)))
  (message "Cleared all filters."))

(defun logx-view--apply-filters ()
  "Apply active level and category filters via buffer-invisibility-spec."
  (let ((inhibit-read-only t)
        (parent-hidden nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((pos (point))
               (rec (get-text-property pos 'jsonl-record))
               (lvl (get-text-property pos 'logx-lvl))
               (cat (get-text-property pos 'logx-cat))
               (is-expansion (get-text-property pos 'jsonl-expansion))
               (eol (line-end-position))
               (line-end (min (1+ eol) (point-max))))
          (cond
           (is-expansion
            (if parent-hidden
                (put-text-property pos line-end 'invisible 'jsonl-filter-hidden)
              (remove-text-properties pos line-end '(invisible nil))))
           ((or rec lvl)
            (let ((hide nil))
              (when (and logx-view-min-level
                         (< (logx-level-weight lvl) (logx-level-weight logx-view-min-level)))
                (setq hide t))
              (when (and logx-view-category-filter
                         (not (string-match-p logx-view-category-filter (format "%s" (or cat "")))))
                (setq hide t))
              (when (and jsonl-view-filter-predicate
                         rec
                         (not (funcall jsonl-view-filter-predicate rec)))
                (setq hide t))
              (setq parent-hidden hide)
              (if hide
                  (put-text-property pos line-end 'invisible 'jsonl-filter-hidden)
                (remove-text-properties pos line-end '(invisible nil)))))
           (t
            (setq parent-hidden nil))))
        (forward-line 1)))))

;;; ----------------------------------------------------------------------------
;;; Live Ring Buffer Bridge & Batch Renderer
;;; ----------------------------------------------------------------------------

(defvar logx-view--active-viewers nil
  "Alist of (BUFFER . PREDICATE) active live viewer buffers.")

(defvar logx-view--pending-events nil
  "List of events emitted since last batch flush, in reverse chronological order.")

(defvar logx-view--flush-timer nil
  "Timer driving the throttled viewer buffer flush.")

(defun logx-view--subscriber-handler (event)
  "Subscriber hook called when `logx' emits EVENT."
  (push event logx-view--pending-events)
  (logx-view--schedule-flush))

(defun logx-view-register-viewer (buffer &optional predicate)
  "Register BUFFER to receive live events matching PREDICATE."
  (setq logx-view--active-viewers
        (cons (cons buffer (or predicate (lambda (_ev) t)))
              (assq-delete-all buffer logx-view--active-viewers)))
  (logx-register-subscriber 'logx-view #'logx-view--subscriber-handler))

(defun logx-view-unregister-viewer (buffer)
  "Unregister BUFFER from live log event delivery."
  (setq logx-view--active-viewers (assq-delete-all buffer logx-view--active-viewers))
  (unless logx-view--active-viewers
    (logx-unregister-subscriber 'logx-view)
    (when logx-view--flush-timer
      (cancel-timer logx-view--flush-timer)
      (setq logx-view--flush-timer nil))
    (setq logx-view--pending-events nil)))

(defun logx-view--on-kill-buffer ()
  "Cleanup hook when a `logx-view-mode' buffer is killed."
  (logx-view-unregister-viewer (current-buffer)))

(defun logx-view--flush-active-viewers ()
  "Flush pending events to all active viewer buffers in batches."
  (setq logx-view--flush-timer nil)
  (when logx-view--pending-events
    (let ((events (nreverse logx-view--pending-events)))
      (setq logx-view--pending-events nil)
      (setq logx-view--active-viewers
            (seq-filter (lambda (entry) (buffer-live-p (car entry)))
                        logx-view--active-viewers))
      (dolist (entry logx-view--active-viewers)
        (let ((buf (car entry))
              (pred (cdr entry)))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (let ((inhibit-read-only t)
                    (orig-window (get-buffer-window buf 'visible))
                    (should-follow jsonl-view-follow-mode))
                (save-excursion
                  (goto-char (point-max))
                  (dolist (ev events)
                    (when (funcall pred ev)
                      (insert (logx-view-render-record ev))))
                  (when (> (count-lines (point-min) (point-max)) logx-view-buffer-max-lines)
                    (let ((excess (- (count-lines (point-min) (point-max)) logx-view-buffer-max-lines)))
                      (goto-char (point-min))
                      (forward-line excess)
                      (delete-region (point-min) (point)))))
                (when should-follow
                  (if orig-window
                      (with-selected-window orig-window
                        (goto-char (point-max)))
                    (goto-char (point-max))))))))))))

(defun logx-view--schedule-flush ()
  "Ensure a throttled flush timer is scheduled."
  (unless logx-view--flush-timer
    (setq logx-view--flush-timer
          (run-with-timer logx-view-render-interval nil #'logx-view--flush-active-viewers))))

;;;###autoload
(defun logx-show-logs (&optional buffer-name)
  "Display master structured logs viewer buffer, or BUFFER-NAME if specified."
  (interactive)
  (let* ((name (or buffer-name "*logs*"))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (unless (eq major-mode 'logx-view-mode)
        (logx-view-mode)
        (setq-local jsonl-view-follow-mode t)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (ev (logx--ring-to-list))
            (insert (logx-view-render-record ev))))
        (goto-char (point-max))))
    (logx-view-register-viewer buf)
    (pop-to-buffer buf)))

;;;###autoload
(defun logx-fork-filtered-buffer (name filter-pred)
  "Create and display a specialized live viewer buffer.
Creates *logs:NAME* containing events matching FILTER-PRED."
  (interactive
   (let* ((min-lvl (intern (completing-read "Minimum severity: "
                                            '("debug" "info" "notice" "warn" "error" "critical" "alert" "fatal")
                                            nil t nil nil "info")))
          (cat-rx (read-string "Category regex (empty for all): "))
          (default-name (if (string-empty-p cat-rx)
                            (symbol-name min-lvl)
                          (format "%s.%s" cat-rx min-lvl)))
          (n (read-string (format "Filtered buffer name suffix (default %s): " default-name)
                          nil nil default-name)))
     (list n
           (lambda (ev)
             (let ((l (plist-get ev :lvl))
                   (c (format "%s" (or (plist-get ev :cat) ""))))
               (and (>= (logx-level-weight l) (logx-level-weight min-lvl))
                    (if (string-empty-p cat-rx) t (string-match-p cat-rx c))))))))
  (let* ((buf-name (format "*logs:%s*" (if (string-empty-p name) "custom" name)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (logx-view-mode)
      (setq-local jsonl-view-follow-mode t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (ev (logx--ring-to-list))
          (when (funcall filter-pred ev)
            (insert (logx-view-render-record ev))))
        (goto-char (point-max))))
    (logx-view-register-viewer buf filter-pred)
    (pop-to-buffer buf)))

;;;###autoload
(defun logx-view-refresh (&optional _ignore-auto _noconfirm)
  "Refresh `logx-view' buffer from the in-memory ring buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (pred (or (alist-get (current-buffer) logx-view--active-viewers)
                  (lambda (_ev) t))))
    (erase-buffer)
    (dolist (ev (logx--ring-to-list))
      (when (funcall pred ev)
        (insert (logx-view-render-record ev))))
    (when jsonl-view-follow-mode
      (goto-char (point-max)))))

;;; ----------------------------------------------------------------------------
;;; Major Mode & Keymap
;;; ----------------------------------------------------------------------------

(defvar logx-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jsonl-view-mode-map)
    (define-key map (kbd "RET")     #'logx-view-jump-to-source)
    (define-key map (kbd "t")       #'logx-view-cycle-timestamp-mode)
    (define-key map (kbd "l")       #'logx-view-filter-level)
    (define-key map (kbd "c")       #'logx-view-filter-category)
    (define-key map (kbd "C")       #'logx-view-clear-filters)
    (define-key map (kbd "B")       #'logx-fork-filtered-buffer)
    (define-key map (kbd "g")       #'revert-buffer)
    map)
  "Keymap for `logx-view-mode'.")

;;;###autoload
(define-derived-mode logx-view-mode jsonl-view-mode "Logx-View"
  "Major mode for viewing live and persisted logx event streams."
  :group 'logx-view
  (setq-local jsonl-view-formatter-function #'logx-view-render-record)
  (setq-local revert-buffer-function #'logx-view-refresh)
  (add-hook 'kill-buffer-hook #'logx-view--on-kill-buffer nil t))

(provide 'logx-view)
;;; logx-view.el ends here
