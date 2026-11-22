;;; logx.el --- Structured logging framework for Emacs -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, logging
;; URL: https://github.com/tychoish/logx

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; High-performance structured logging framework for Emacs.
;;
;; Features:
;; - Zero-cost disabled logging fast-path via severity gating macros.
;; - Compile-time call-site source tracking via `symbol-with-pos'.
;; - Standard library `ring.el' circular ring buffer with zero-allocation append.
;; - Severity levels: debug, info, notice, warn, error, critical, alert, fatal.
;; - Fast C-level JSON Lines (`json-serialize') disk appender with object sanitization.
;; - Optional message-quarantine mode to silence chatty background subsystems.
;; - Headless core: decoupled from UI viewers via autoloads.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'json)
(require 'ring)

(defgroup logx nil
  "High-performance structured logging framework."
  :group 'tools
  :prefix "logx-")

;;; ----------------------------------------------------------------------------
;;; Severity Levels and Thresholds
;;; ----------------------------------------------------------------------------

(defconst logx--severity-weights
  '((debug    . 10)
    (info     . 20)
    (notice   . 30)
    (warn     . 40)
    (error    . 50)
    (critical . 60)
    (alert    . 70)
    (fatal    . 80))
  "Alist mapping severity symbols to numeric priorities.")

(defcustom logx-default-level 'info
  "Default minimum logging severity threshold."
  :type '(choice (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Notice" notice)
                 (const :tag "Warn" warn)
                 (const :tag "Error" error)
                 (const :tag "Critical" critical)
                 (const :tag "Alert" alert)
                 (const :tag "Fatal" fatal))
  :group 'logx)

(defcustom logx-default-event-level 'debug
  "Default severity level for `logx' events when no level is specified."
  :type '(choice (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Notice" notice)
                 (const :tag "Warn" warn)
                 (const :tag "Error" error)
                 (const :tag "Critical" critical)
                 (const :tag "Alert" alert)
                 (const :tag "Fatal" fatal))
  :group 'logx)

(defconst logx--level-aliases
  '((trace   . debug)
    (warning . warn))
  "Alist mapping severity aliases to canonical severity symbols.")

(defcustom logx-category-levels nil
  "Alist mapping category symbols or strings to severity thresholds."
  :type '(alist :key-type (choice symbol string) :value-type symbol)
  :group 'logx)

(defcustom logx-ring-buffer-size 10000
  "Maximum number of structured events retained in the circular ring buffer."
  :type 'integer
  :group 'logx)

(defcustom logx-log-file nil
  "File path for appending JSON Lines log records, or nil to disable disk sink."
  :type '(choice (const :tag "Disabled" nil)
                 (file :tag "File Path"))
  :group 'logx)

(defcustom logx-async-mode t
  "When non-nil, flush events to disk and network backends asynchronously."
  :type 'boolean
  :group 'logx)

(defcustom logx-async-transport 'timer
  "Transport driving asynchronous flushing: `timer', `thread', or `sync'."
  :type '(choice (const :tag "Timer-driven (recommended)" timer)
                 (const :tag "Native thread" thread)
                 (const :tag "Synchronous" sync))
  :group 'logx)

(defcustom logx-async-flush-interval 0.05
  "Interval in seconds between asynchronous batch flushes."
  :type 'number
  :group 'logx)

(defcustom logx-async-max-queue 10000
  "Maximum pending events retained in the async queue before applying backpressure."
  :type 'integer
  :group 'logx)

(defcustom logx-async-backpressure-policy :drop-oldest
  "Backpressure strategy when async queue is full: `:drop-oldest' or `:yield'."
  :type '(choice (const :tag "Drop oldest pending events (non-blocking)" :drop-oldest)
                 (const :tag "Yield execution to worker" :yield))
  :group 'logx)

(defcustom logx-journald-socket "/run/systemd/journal/socket"
  "Path to systemd-journald Unix domain datagram socket."
  :type 'string
  :group 'logx)

(defcustom logx-syslog-socket "/dev/log"
  "Path to Unix domain syslog datagram socket."
  :type 'string
  :group 'logx)

(defcustom logx-sanitize-max-depth 8
  "Maximum recursion depth when sanitizing complex Lisp objects."
  :type 'integer
  :group 'logx)

(defcustom logx-quarantined-packages '("eglot" "flymake" "magit" "eldoc")
  "List of package prefix strings whose `message' calls are intercepted."
  :type '(repeat string)
  :group 'logx)

(defvar logx-event-hook nil
  "Hook run with one argument (the newly emitted event plist).")

;;;###autoload
(defun logx-canonical-level (level)
  "Resolve LEVEL (symbol, keyword, or string) to a canonical severity symbol."
  (let* ((sym (cond ((keywordp level) (intern (substring (symbol-name level) 1)))
                    ((symbolp level) level)
                    ((stringp level) (intern-soft level))
                    (t nil)))
         (aliased (and sym (or (alist-get sym logx--level-aliases) sym))))
    (and aliased (assq aliased logx--severity-weights) aliased)))

;;;###autoload
(defun logx-level-weight (level)
  "Return numeric priority for LEVEL symbol, or 0 if unknown."
  (let ((canonical (logx-canonical-level level)))
    (if canonical
        (or (alist-get canonical logx--severity-weights) 0)
      0)))

;;;###autoload
(defun logx-set-category-level (category level)
  "Set threshold for CATEGORY (symbol or string) to LEVEL."
  (let ((cat-sym (if (symbolp category) category (intern (format "%s" category)))))
    (setf (map-elt logx-category-levels cat-sym) level)))

;;;###autoload
(defun logx--level-enabled-p (level &optional category)
  "Return non-nil if LEVEL meets or exceeds active threshold for CATEGORY."
  (let* ((weight (logx-level-weight level))
         (cat-thresh (when category
                       (let* ((cat-sym (cond ((keywordp category)
                                              (intern (substring (symbol-name category) 1)))
                                             ((symbolp category) category)
                                             ((stringp category) (intern-soft category))
                                             (t nil)))
                              (cat-str (if (keywordp category)
                                           (substring (symbol-name category) 1)
                                         (format "%s" category))))
                         (or (and cat-sym (map-elt logx-category-levels cat-sym))
                             (map-elt logx-category-levels cat-str)
                             (map-elt logx-category-levels category)))))
         (threshold (or cat-thresh logx-default-level)))
    (>= weight (logx-level-weight threshold))))

;;; ----------------------------------------------------------------------------
;;; Call-Site Capture
;;; ----------------------------------------------------------------------------

(defun logx--capture-call-site (macro-sym)
  "Extract source coordinates at expansion time for MACRO-SYM.
Returns a plist (:file FILE :path PATH :line LINE :fn FN)."
  (let* ((file (or (macroexp-file-name)
                   (and (boundp 'byte-compile-current-file)
                        byte-compile-current-file)
                   (buffer-file-name)))
         (line nil)
         (fn (when (boundp 'byte-compile-current-form)
               (cond
                ((symbolp byte-compile-current-form) byte-compile-current-form)
                ((and (consp byte-compile-current-form)
                      (memq (car byte-compile-current-form) '(defun cl-defun defmacro defalias)))
                 (cadr byte-compile-current-form))))))
    ;; 1. Check byte-compilation form stack and buffer
    (when (and (boundp 'byte-compile-current-buffer)
               (buffer-live-p byte-compile-current-buffer)
               (boundp 'byte-compile-form-stack))
      (let ((offset (and (fboundp 'byte-compile--warning-source-offset)
                         (byte-compile--warning-source-offset))))
        (when offset
          (with-current-buffer byte-compile-current-buffer
            (setq line (line-number-at-pos offset))))))
    ;; 2. Check symbol-with-pos on macro-sym
    (when (and (not line)
               (fboundp 'symbol-with-pos-p)
               (fboundp 'symbol-with-pos-pos)
               (symbol-with-pos-p macro-sym))
      (let ((pos (symbol-with-pos-pos macro-sym)))
        (when (numberp pos)
          (if (and (boundp 'byte-compile-current-buffer)
                   (buffer-live-p byte-compile-current-buffer))
              (with-current-buffer byte-compile-current-buffer
                (setq line (line-number-at-pos pos)))
            (when (and file (buffer-file-name) (equal file (buffer-file-name)))
              (setq line (line-number-at-pos pos)))))))
    ;; 3. Check active buffer ONLY if visiting the same file
    (when (and (not line)
               file
               (buffer-file-name)
               (equal file (buffer-file-name)))
      (setq line (ignore-errors (line-number-at-pos))))
    (list :file (and file (file-name-nondirectory file))
          :path file
          :line line
          :fn fn)))

;;; ----------------------------------------------------------------------------
;;; Object Sanitization
;;; ----------------------------------------------------------------------------

;;;###autoload
(defun logx--sanitize-value (val &optional depth max-depth seen)
  "Sanitize VAL into JSON-serializable primitives up to MAX-DEPTH.
Prevents crashes on non-serializable Lisp objects (buffers, markers, etc.)
and guards against circular references via SEEN."
  (let ((d (or depth 0))
        (md (or max-depth logx-sanitize-max-depth)))
    (cond
     ((> d md) "[Truncated: max depth reached]")
     ((null val) :null)
     ((eq val t) t)
     ((eq val :false) :false)
     ((numberp val) val)
     ((stringp val) val)
     ((keywordp val) (substring (symbol-name val) 1))
     ((bufferp val) (format "#<buffer %s>" (buffer-name val)))
     ((markerp val) (marker-position val))
     ((processp val) (format "#<process %s>" (process-name val)))
     ((windowp val) (format "#<window %s>" (window-use-time val)))
     ((framep val) (format "#<frame %s>" (frame-parameter val 'name)))
     ((hash-table-p val)
      (if (memq val seen)
          "[Circular Reference]"
        (let ((items nil)
              (new-seen (cons val seen)))
          (map-do (lambda (k v)
                    (push (intern (format ":%s" k)) items)
                    (push (logx--sanitize-value v (1+ d) md new-seen) items))
                  val)
          (nreverse items))))
     ((vectorp val)
      (if (memq val seen)
          "[Circular Reference]"
        (let ((new-seen (cons val seen)))
          (vconcat (mapcar (lambda (elem)
                             (logx--sanitize-value elem (1+ d) md new-seen))
                           val)))))
     ((consp val)
      (if (memq val seen)
          "[Circular Reference]"
        (let ((new-seen (cons val seen)))
          (if (keywordp (car val))
              (let ((res nil)
                    (tail val))
                (while (and (consp tail) (keywordp (car tail)))
                  (push (car tail) res)
                  (push (logx--sanitize-value (cadr tail) (1+ d) md new-seen) res)
                  (setq tail (cddr tail)))
                (nreverse res))
            (vconcat (mapcar (lambda (elem)
                               (logx--sanitize-value elem (1+ d) md new-seen))
                             val))))))
     ((symbolp val) (symbol-name val))
     (t (format "%s" val)))))

;;; ----------------------------------------------------------------------------
;;; Ring Buffer (using standard library ring.el)
;;; ----------------------------------------------------------------------------

(defvar logx--ring (make-ring 10000)
  "Standard library ring buffer retaining recent structured log events.")

(defvar logx--ring-seq 0
  "Monotonically increasing sequence number for log events.")

;;;###autoload
(defun logx--ring-init (&optional size)
  "Initialize or reallocate the ring buffer with capacity SIZE."
  (let ((cap (or size logx-ring-buffer-size 10000)))
    (setq logx--ring (make-ring cap)
          logx--ring-seq 0)))

;;;###autoload
(defun logx--ring-push (event)
  "Push EVENT into the circular ring buffer in O(1) time.
Overwrites the oldest element when the buffer is full."
  (unless (ring-p logx--ring)
    (logx--ring-init))
  (ring-insert logx--ring event)
  event)

;;;###autoload
(defun logx--ring-clear ()
  "Reset ring buffer contents without altering capacity."
  (when (ring-p logx--ring)
    (setq logx--ring (make-ring (ring-size logx--ring)))))

;;;###autoload
(defun logx--ring-to-list (&optional limit)
  "Return ring buffer events ordered from oldest to newest.
If LIMIT is non-nil, return at most LIMIT entries."
  (unless (ring-p logx--ring)
    (logx--ring-init))
  (let ((elems (nreverse (ring-elements logx--ring))))
    (if (and limit (< limit (length elems)))
        (seq-take elems limit)
      elems)))

;;;###autoload
(defun logx--ring-length ()
  "Return the number of events currently held in the ring buffer."
  (if (ring-p logx--ring)
      (ring-length logx--ring)
    0))

;;; ----------------------------------------------------------------------------
;;; Serialization and Disk Sink
;;; ----------------------------------------------------------------------------

;;;###autoload
(defun logx--serialize-jsonl (event)
  "Serialize EVENT plist into a single line-delimited JSON string."
  (let* ((sanitized-data (logx--sanitize-value (plist-get event :data)))
         (msg (plist-get event :msg))
         (payload (list :seq (plist-get event :seq)
                        :ts (plist-get event :ts)
                        :lvl (symbol-name (plist-get event :lvl))
                        :cat (format "%s" (or (plist-get event :cat) "root"))
                        :msg (if (stringp msg) msg (format "%s" (or msg "")))
                        :src (logx--sanitize-value (plist-get event :src))
                        :data sanitized-data)))
    (json-serialize payload)))

;;; ----------------------------------------------------------------------------
;;; Pluggable Output Backends (File, Journald, Syslog, Custom)
;;; ----------------------------------------------------------------------------

(defvar logx--backends nil
  "Alist of (ID . (:handler HANDLER :enabled ENABLED-P)) registered backends.")

;;;###autoload
(defun logx-register-backend (id handler &optional enabled)
  "Register backend ID with batch HANDLER function.
HANDLER is called with a list of event plists: (funcall HANDLER events)."
  (setq logx--backends
        (cons (cons id (list :handler handler :enabled (if (null enabled) t enabled)))
              (assq-delete-all id logx--backends))))

;;;###autoload
(defun logx-unregister-backend (id)
  "Unregister backend ID."
  (setq logx--backends (assq-delete-all id logx--backends)))

;;;###autoload
(defun logx-enable-backend (id)
  "Enable backend ID."
  (let ((entry (assq id logx--backends)))
    (when entry
      (setf (plist-get (cdr entry) :enabled) t))))

;;;###autoload
(defun logx-disable-backend (id)
  "Disable backend ID."
  (let ((entry (assq id logx--backends)))
    (when entry
      (setf (plist-get (cdr entry) :enabled) nil))))

;;;###autoload
(defun logx-backend-enabled-p (id)
  "Return non-nil if backend ID is registered and enabled."
  (let ((entry (assq id logx--backends)))
    (and entry (plist-get (cdr entry) :enabled))))

;; File (JSON Lines) Backend
(defun logx--file-backend-handler (events)
  "Batch append EVENTS to `logx-log-file'."
  (when (and logx-log-file (stringp logx-log-file) events)
    (condition-case err
        (with-temp-buffer
          (dolist (ev events)
            (insert (logx--serialize-jsonl ev) "\n"))
          (append-to-file (point-min) (point-max) logx-log-file))
      (error
       (message "[logx] Disk appender failed: %s" (error-message-string err))))))

(defun logx--level-to-syslog-priority (level)
  "Convert severity LEVEL symbol to numeric RFC 5424/3164 syslog priority (0-7)."
  (pcase level
    ('fatal    2)
    ('alert    1)
    ('critical 2)
    ('error    3)
    ('warn     4)
    ('notice   5)
    ('info     6)
    ('debug    7)
    (_         6)))

;; systemd-journald Native Socket Backend
(defvar logx--journald-process nil
  "Persistent datagram process for systemd-journald.")

(defun logx--journald-ensure-process ()
  "Ensure persistent datagram socket connection to systemd-journald."
  (unless (and logx--journald-process (process-live-p logx--journald-process))
    (when (and (stringp logx-journald-socket) (file-exists-p logx-journald-socket))
      (condition-case nil
          (setq logx--journald-process
                (make-network-process :name "logx-journald"
                                      :family 'local
                                      :service logx-journald-socket
                                      :type 'datagram))
        (error nil))))
  logx--journald-process)

(defun logx--journald-format-event (ev)
  "Format EVENT plist into systemd-journald native socket datagram."
  (let* ((lvl (plist-get ev :lvl))
         (prio (logx--level-to-syslog-priority lvl))
         (cat (format "%s" (or (plist-get ev :cat) "root")))
         (msg (format "%s" (or (plist-get ev :msg) "")))
         (seq (plist-get ev :seq))
         (src (plist-get ev :src))
         (file (or (plist-get src :file) (plist-get src :path)))
         (line (plist-get src :line))
         (data (plist-get ev :data)))
    (concat "MESSAGE=" msg "\n"
            "PRIORITY=" (number-to-string prio) "\n"
            "SYSLOG_IDENTIFIER=emacs\n"
            "EMACS_CATEGORY=" cat "\n"
            (when seq (concat "EMACS_SEQ=" (number-to-string seq) "\n"))
            (when file (concat "CODE_FILE=" (format "%s" file) "\n"))
            (when line (concat "CODE_LINE=" (number-to-string line) "\n"))
            (when data
              (concat "EMACS_DATA=" (json-serialize (logx--sanitize-value data)) "\n"))
            "\n")))

(defun logx--journald-backend-handler (events)
  "Send EVENTS batch to systemd-journald over datagram socket."
  (let ((proc (logx--journald-ensure-process)))
    (when proc
      (dolist (ev events)
        (condition-case nil
            (process-send-string proc (logx--journald-format-event ev))
          (error
           (ignore-errors (delete-process proc))
           (setq logx--journald-process nil)))))))

;; Legacy syslog Datagram Backend
(defvar logx--syslog-process nil
  "Persistent datagram process for syslog.")

(defun logx--syslog-ensure-process ()
  "Ensure persistent datagram socket connection to syslog."
  (unless (and logx--syslog-process (process-live-p logx--syslog-process))
    (when (and (stringp logx-syslog-socket) (file-exists-p logx-syslog-socket))
      (condition-case nil
          (setq logx--syslog-process
                (make-network-process :name "logx-syslog"
                                      :family 'local
                                      :service logx-syslog-socket
                                      :type 'datagram))
        (error nil))))
  logx--syslog-process)

(defun logx--syslog-format-event (ev)
  "Format EVENT plist into RFC 3164 syslog datagram."
  (let* ((lvl (plist-get ev :lvl))
         (prio (logx--level-to-syslog-priority lvl))
         (facility 8) ; user-level facility (1 * 8 = 8)
         (pri-val (+ facility prio))
         (cat (format "%s" (or (plist-get ev :cat) "root")))
         (msg (format "%s" (or (plist-get ev :msg) "")))
         (ts-str (format-time-string "%b %e %T" (seconds-to-time (or (plist-get ev :ts) (float-time))))))
    (format "<%d>%s emacs[%d]: [%s] %s\n" pri-val ts-str (emacs-pid) cat msg)))

(defun logx--syslog-backend-handler (events)
  "Send EVENTS batch to syslog over datagram socket."
  (let ((proc (logx--syslog-ensure-process)))
    (when proc
      (dolist (ev events)
        (condition-case nil
            (process-send-string proc (logx--syslog-format-event ev))
          (error
           (ignore-errors (delete-process proc))
           (setq logx--syslog-process nil)))))))

;; Initialize default backends
(logx-register-backend 'file #'logx--file-backend-handler t)
(logx-register-backend 'journald #'logx--journald-backend-handler nil)
(logx-register-backend 'syslog #'logx--syslog-backend-handler nil)

;;; ----------------------------------------------------------------------------
;;; Asynchronous Flushing Pipeline & Backpressure
;;; ----------------------------------------------------------------------------

(defvar logx--async-mutex (make-mutex "logx-async")
  "Mutex serializing access to `logx--async-queue'.")

(defvar logx--async-queue nil
  "FIFO list of events waiting to be flushed to active backends.")

(defvar logx--async-queue-tail nil
  "Pointer to the last cons cell of `logx--async-queue' for O(1) enqueue.")

(defvar logx--async-queue-len 0
  "Number of pending events in `logx--async-queue'.")

(defvar logx--dropped-count 0
  "Total count of events dropped due to backpressure.")

(defvar logx--async-timer nil
  "Timer driving the async flush worker.")

(defvar logx--async-thread nil
  "Thread driving the async flush worker.")

;;;###autoload
(defun logx-stop-async-worker ()
  "Stop running asynchronous worker timer and thread."
  (when logx--async-timer
    (cancel-timer logx--async-timer)
    (setq logx--async-timer nil))
  (when (and logx--async-thread (thread-live-p logx--async-thread))
    (ignore-errors (thread-signal logx--async-thread 'quit nil))
    (setq logx--async-thread nil)))

;;;###autoload
(defun logx-flush ()
  "Synchronously flush all pending events in the async queue to active backends.
Returns the number of flushed events."
  (interactive)
  (let ((flushed-count 0)
        (events nil))
    (with-mutex logx--async-mutex
      (when logx--async-queue
        (setq events logx--async-queue
              flushed-count logx--async-queue-len
              logx--async-queue nil
              logx--async-queue-tail nil
              logx--async-queue-len 0)))
    (when events
      (dolist (b logx--backends)
        (when (plist-get (cdr b) :enabled)
          (condition-case err
              (funcall (plist-get (cdr b) :handler) events)
            (error
             (message "[logx] Backend %s failed: %s" (car b) (error-message-string err)))))))
    flushed-count))

(defun logx--async-schedule-flush ()
  "Ensure the flush worker is scheduled according to `logx-async-transport'."
  (pcase logx-async-transport
    ('sync
     (logx-flush))
    ('thread
     (unless (and logx--async-thread (thread-live-p logx--async-thread))
       (setq logx--async-thread
             (make-thread
              (lambda ()
                (while (and logx-async-mode (eq logx-async-transport 'thread))
                  (when (> logx--async-queue-len 0)
                    (logx-flush))
                  (sleep-for logx-async-flush-interval)))
              "logx-flush-worker"))))
    (_ ;; 'timer
     (unless logx--async-timer
       (setq logx--async-timer
             (run-with-timer logx-async-flush-interval nil
                             (lambda ()
                               (setq logx--async-timer nil)
                               (logx-flush))))))))

(defun logx--async-enqueue (event)
  "Enqueue EVENT for asynchronous backend flushing with backpressure control."
  (with-mutex logx--async-mutex
    ;; Backpressure check
    (when (>= logx--async-queue-len logx-async-max-queue)
      (pcase logx-async-backpressure-policy
        (:yield
         (logx-flush)
         (when (fboundp 'thread-yield) (thread-yield)))
        (_ ;; :drop-oldest
         (setq logx--async-queue (cdr logx--async-queue)
               logx--async-queue-len (max 0 (1- logx--async-queue-len)))
         (cl-incf logx--dropped-count))))
    ;; Append to queue
    (let ((cell (cons event nil)))
      (if logx--async-queue-tail
          (progn
            (setcdr logx--async-queue-tail cell)
            (setq logx--async-queue-tail cell))
        (setq logx--async-queue cell
              logx--async-queue-tail cell))
      (cl-incf logx--async-queue-len)))
  ;; Schedule flush
  (logx--async-schedule-flush))

;; Flush on Emacs exit
(add-hook 'kill-emacs-hook #'logx-flush)

;;; ----------------------------------------------------------------------------
;;; Event Emission Pipeline & Headless Subscriber Dispatch
;;; ----------------------------------------------------------------------------

(defvar logx--active-subscribers nil
  "Alist of (ID-OR-BUFFER . HANDLER) active event subscribers.")

;;;###autoload
(defun logx-register-subscriber (id handler)
  "Register subscriber ID with HANDLER function called on each event."
  (setq logx--active-subscribers
        (cons (cons id handler)
              (assq-delete-all id logx--active-subscribers))))

;;;###autoload
(defun logx-unregister-subscriber (id)
  "Unregister subscriber ID from log event delivery."
  (setq logx--active-subscribers (assq-delete-all id logx--active-subscribers)))

(defun logx--emit-event (level category msg src data)
  "Construct, store, and dispatch structured event record."
  (let* ((resolved-src
          (cond
           ((and src (plist-get src :line) (> (plist-get src :line) 1))
            src)
           ((and src (plist-get src :fn) (symbolp (plist-get src :fn)))
            (let ((loc (ignore-errors (find-function-noselect (plist-get src :fn) t))))
              (if (and loc (car loc) (cdr loc))
                  (let ((buf (car loc)) (pos (cdr loc)))
                    (with-current-buffer buf
                      (list :file (and (buffer-file-name buf) (file-name-nondirectory (buffer-file-name buf)))
                            :path (buffer-file-name buf)
                            :line (line-number-at-pos pos)
                            :fn (plist-get src :fn))))
                src)))
           (t
            (let ((fn nil) (i 1))
              (while (and (not fn) (< i 25))
                (let ((frame (backtrace-frame i)))
                  (when (and frame (car frame) (symbolp (cadr frame))
                             (not (string-prefix-p "logx" (symbol-name (cadr frame))))
                             (not (memq (cadr frame) '(backtrace-frame apply funcall eval))))
                    (setq fn (cadr frame))))
                (cl-incf i))
              (if (and fn (symbolp fn))
                  (let ((loc (ignore-errors (find-function-noselect fn t))))
                    (if (and loc (car loc) (cdr loc))
                        (let ((buf (car loc)) (pos (cdr loc)))
                          (with-current-buffer buf
                            (list :file (and (buffer-file-name buf) (file-name-nondirectory (buffer-file-name buf)))
                                  :path (buffer-file-name buf)
                                  :line (line-number-at-pos pos)
                                  :fn fn)))
                      (let ((sfile (find-lisp-object-file-name fn 'defun)))
                        (list :file (and sfile (file-name-nondirectory sfile))
                              :path sfile
                              :line (and src (plist-get src :line))
                              :fn fn))))
                src)))))
         (event (list :seq (cl-incf logx--ring-seq)
                      :ts (float-time)
                      :lvl level
                      :cat (or category 'root)
                      :msg (or msg "")
                      :src resolved-src
                      :data data)))
    ;; 1. Synchronous core in-memory ring buffer (Primary Ingress)
    (logx--ring-push event)

    ;; 2. Async queue to active output backends (file, journald, syslog)
    (if (and logx-async-mode (not (eq logx-async-transport 'sync)))
        (logx--async-enqueue event)
      ;; Direct sync dispatch if async disabled
      (dolist (b logx--backends)
        (when (plist-get (cdr b) :enabled)
          (condition-case nil
              (funcall (plist-get (cdr b) :handler) (list event))
            (error nil)))))

    ;; 3. Live UI subscribers & hook
    (dolist (sub logx--active-subscribers)
      (condition-case nil
          (funcall (cdr sub) event)
        (error nil)))
    (run-hook-with-args 'logx-event-hook event)
    event))

;;; ----------------------------------------------------------------------------
;;; Macro Expansion & Public Logging API
;;; ----------------------------------------------------------------------------

(defun logx--canonical-level (form)
  "Resolve FORM in macro args to a canonical severity symbol.
Returns nil if not a level. Matches quoted symbols (e.g. \\='info)
and keywords (e.g. :info)."
  (let ((sym (cond
              ((and (consp form) (eq (car form) 'quote) (symbolp (cadr form)))
               (cadr form))
              ((keywordp form)
               (intern (substring (symbol-name form) 1)))
              (t nil))))
    (and sym (logx-canonical-level sym))))

(defun logx--parse-macro-args (args)
  "Extract category, message, source, and remaining key-values from ARGS.
Supports :cat, :category, and :domain for category; :msg and :message
for message; :src, :source, and :caller for source coordinates.
If a non-keyword argument is encountered before a message keyword,
it is treated as the positional log message."
  (let ((cat nil)
        (msg nil)
        (src nil)
        (rest nil))
    (while args
      (let ((key (car args)))
        (cond
         ((memq key '(:cat :category :domain))
          (setq cat (cadr args)
                args (cddr args)))
         ((memq key '(:msg :message))
          (if (null msg)
              (setq msg (cadr args))
            (push key rest)
            (push (cadr args) rest))
          (setq args (cddr args)))
         ((memq key '(:src :source :caller))
          (setq src (cadr args)
                args (cddr args)))
         ((and (null msg) (not (keywordp key)))
          (setq msg key
                args (cdr args)))
         ((keywordp key)
          (push key rest)
          (push (cadr args) rest)
          (setq args (cddr args)))
         (t
          (push key rest)
          (setq args (cdr args))))))
    (list :cat cat :msg msg :src src :data (nreverse rest))))

(defun logx--expand-macro (level args macro-sym)
  "Expand logging macro for LEVEL with ARGS and MACRO-SYM.
When LEVEL is nil, dynamically evaluates `logx-default-event-level' at runtime."
  (let* ((parsed (logx--parse-macro-args args))
         (cat-form (plist-get parsed :cat))
         (msg-form (plist-get parsed :msg))
         (src-form (plist-get parsed :src))
         (data-forms (plist-get parsed :data))
         (src (logx--capture-call-site macro-sym)))
    (cond
     ;; 1. Static compile-time level (zero-cost path)
     (level
      (let ((body-fn
             (lambda (cat-expr)
               (if src-form
                   (let ((src-sym (gensym "src-")))
                     `(let ((,src-sym (or ,src-form ',src)))
                        (logx--emit-event ',level
                                          ,cat-expr
                                          ,msg-form
                                          ,src-sym
                                          (list ,@data-forms))))
                 `(logx--emit-event ',level
                                    ,cat-expr
                                    ,msg-form
                                    ',src
                                    (list ,@data-forms))))))
        (if (and cat-form (not (or (symbolp cat-form) (keywordp cat-form) (stringp cat-form))))
            (let ((cat-sym (gensym "cat-")))
              `(let ((,cat-sym ,cat-form))
                 (when (logx--level-enabled-p ',level ,cat-sym)
                   ,(funcall body-fn cat-sym))))
          `(when (logx--level-enabled-p ',level ,cat-form)
             ,(funcall body-fn cat-form)))))

     ;; 2. Dynamic runtime default level (hygienic gensym bindings)
     (t
      (let* ((lvl-sym (gensym "lvl-"))
             (body-fn
              (lambda (cat-expr)
                (if src-form
                    (let ((src-sym (gensym "src-")))
                      `(let ((,src-sym (or ,src-form ',src)))
                         (logx--emit-event ,lvl-sym
                                           ,cat-expr
                                           ,msg-form
                                           ,src-sym
                                           (list ,@data-forms))))
                  `(logx--emit-event ,lvl-sym
                                     ,cat-expr
                                     ,msg-form
                                     ',src
                                     (list ,@data-forms))))))
        (if (and cat-form (not (or (symbolp cat-form) (keywordp cat-form) (stringp cat-form))))
            (let ((cat-sym (gensym "cat-")))
              `(let ((,lvl-sym (or (and (boundp 'logx-default-event-level)
                                        logx-default-event-level)
                                   'debug))
                     (,cat-sym ,cat-form))
                 (when (logx--level-enabled-p ,lvl-sym ,cat-sym)
                   ,(funcall body-fn cat-sym))))
          `(let ((,lvl-sym (or (and (boundp 'logx-default-event-level)
                                    logx-default-event-level)
                               'debug)))
             (when (logx--level-enabled-p ,lvl-sym ,cat-form)
               ,(funcall body-fn cat-form)))))))))

;;;###autoload
(defmacro logx (&rest args)
  "Emit a structured log event with optional leading severity LEVEL.
When LEVEL is omitted, defaults to `logx-default-event-level'.
LEVEL can be a quoted symbol (e.g. \\='info), bare symbol (e.g. info),
or keyword (e.g. :info). Aliases like `warning' -> `warn' and
`trace' -> `debug' are normalized automatically.

ARGS can include a positional message string/expression, :cat (or
:category, :domain), :msg (or :message), and arbitrary KEY VALUE pairs."
  (let ((level (logx--canonical-level (car args))))
    (if level
        (logx--expand-macro level (cdr args) 'logx)
      (logx--expand-macro nil args 'logx))))

;;;###autoload
(defmacro logx-debug (&rest args)
  "Emit a DEBUG level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'debug args 'logx-debug))

;;;###autoload
(defmacro logx-info (&rest args)
  "Emit an INFO level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'info args 'logx-info))

;;;###autoload
(defmacro logx-notice (&rest args)
  "Emit a NOTICE level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'notice args 'logx-notice))

;;;###autoload
(defmacro logx-warn (&rest args)
  "Emit a WARN level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'warn args 'logx-warn))

;;;###autoload
(defmacro logx-error (&rest args)
  "Emit an ERROR level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'error args 'logx-error))

;;;###autoload
(defmacro logx-critical (&rest args)
  "Emit a CRITICAL level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'critical args 'logx-critical))

;;;###autoload
(defmacro logx-alert (&rest args)
  "Emit an ALERT level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'alert args 'logx-alert))

;;;###autoload
(defmacro logx-fatal (&rest args)
  "Emit a FATAL level log event.
ARGS can include :cat CATEGORY, :msg MESSAGE, and arbitrary KEY VALUE pairs."
  (logx--expand-macro 'fatal args 'logx-fatal))

;;; ----------------------------------------------------------------------------
;;; Legacy Message Interception Quarantine Bridge
;;; ----------------------------------------------------------------------------

(defvar logx--in-quarantine-p nil
  "Reentrancy guard flag preventing infinite recursion in message advice.")

(defun logx--caller-matches-quarantine-p (pkgs)
  "Return non-nil if caller stack matches any package prefix in PKGS."
  (let ((matched nil)
        (frame-idx 3))
    (while (and (not matched) (< frame-idx 20))
      (let ((frame (backtrace-frame frame-idx)))
        (when frame
          (let ((fn (cadr frame)))
            (when (symbolp fn)
              (let ((fn-name (symbol-name fn)))
                (dolist (pkg pkgs)
                  (when (string-prefix-p pkg fn-name)
                    (setq matched pkg))))))))
      (cl-incf frame-idx))
    matched))

(defun logx--caller-call-site ()
  "Inspect call stack for originating caller coordinates."
  (let ((frame (backtrace-frame 4)))
    (when frame
      (list :fn (cadr frame)))))

(defun logx--message-advice (orig-fun &rest args)
  "Advice intercepting chatty background `message' invocations."
  (if (or logx--in-quarantine-p
          (null (car args))
          inhibit-message)
      (apply orig-fun args)
    (let ((logx--in-quarantine-p t))
      (condition-case _err
          (let ((pkg (and (not this-command)
                          (logx--caller-matches-quarantine-p logx-quarantined-packages))))
            (if pkg
                (let ((formatted (apply #'format-message args)))
                  (logx--emit-event 'info
                                    (intern (format "legacy.%s" pkg))
                                    formatted
                                    (logx--caller-call-site)
                                    (list :raw-args (mapcar #'logx--sanitize-value (cdr args)))))
              (apply orig-fun args)))
        (error
         (apply orig-fun args))))))

;;;###autoload
(define-minor-mode logx-quarantine-mode
  "Global minor mode diverting background package messages to logx."
  :global t
  :group 'logx
  (if logx-quarantine-mode
      (advice-add 'message :around #'logx--message-advice)
    (advice-remove 'message #'logx--message-advice)))

;;;###autoload (autoload 'logx-show-logs "logx-view" "Display master structured logs viewer buffer." t)
;;;###autoload (autoload 'logx-fork-filtered-buffer "logx-view" "Create specialized live viewer buffer." t)

(provide 'logx)
;;; logx.el ends here
