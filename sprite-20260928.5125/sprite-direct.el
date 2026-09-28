;;; sprite-direct.el --- Direct socket communication with server processes -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (seq "2.24") (map "3.0"))
;; URL: https://github.com/tychoish/sprite
;; Keywords: tools, daemon, processes

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Implements the Emacs server wire protocol over Unix domain sockets and TCP
;; connections, allowing Lisp forms to be evaluated in a running Emacs daemon
;; without invoking the emacsclient binary.
;;
;; The protocol is text-based:
;;   Client → Server:  -auth KEY -eval ENCODED_FORM \n
;;   Server → Client:  -print ENCODED_RESULT\n  or  -error ENCODED_MSG\n
;;
;; Special characters in encoded values: & → &&, - → &-, space → &_,
;; newline → &n.  This mirrors `server-quote-arg'/`server-unquote-arg'.
;;
;; The central abstraction is `sprite-direct-conn', a lightweight context
;; object created by `sprite-direct-open' or `with-sprite-direct'.  Each
;; operation opens a transient socket, exchanges one request/response pair,
;; and closes it — the conn holds no persistent socket.
;;
;; Blocking receive uses generator.el: `sprite-direct--recv-gen' yields
;; `:pending' until the connection closes, allowing `accept-process-output'
;; to run between polls.  Timeout behaviour is
;; governed by `sprite-direct-blocking-timeout' and
;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'map)
(require 'seq)
(require 'subr-x)

;; Entry points:
;;   `with-sprite-direct'               — open a connection context
;;   `sprite-direct-open'               — open a conn imperatively
;;   `sprite-direct-eval-blocking'      — synchronous eval (generator-backed)
;;   `sprite-direct-call-and-read'      — drop-in for sprite--call-and-read
;;   `sprite-direct-eval-non-blocking'  — async eval returning a promise
;;   `sprite-direct-promise-pending-p'  — predicate: not yet resolved
;;   `sprite-direct-promise-resolved-p' — predicate: successfully resolved
;;   `sprite-direct-promise-rejected-p' — predicate: failed
;;   `sprite-direct-promise-wait'       — block until promise resolves
;;   `sprite-direct-promise-then'       — run a callback on resolution, no blocking
;;   `sprite-direct-list-buffers'       — list buffer names in the sprite
;;   `sprite-direct-insert-into-buffer' — insert text at a position
;;   `sprite-direct-with-current-buffer'— eval body in a remote buffer
;;   `sprite-direct-read-buffer'        — read buffer contents
;;   `sprite-direct-value-of-symbol'    — read a symbol's value
;;   `sprite-direct-read-tcp-server-file' — read a TCP server file

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'generator)
(require 'map)
(require 'seq)
(require 'subr-x)

;; Declare server.el variables; server.el is loaded by any running daemon
;; but may be absent in batch/test contexts.
(defvar server-socket-dir)
(defvar server-auth-dir)

;;;; Timeout and interval configuration

(defvar sprite-direct-blocking-timeout nil
  "Seconds to wait in blocking receives, or nil to block indefinitely.
Dynamically override per call-site with `let' to impose a local limit.")

(defvar sprite-direct-yield-interval 0.05
  "Seconds passed to `accept-process-output' between generator yields.
Smaller values increase responsiveness at the cost of CPU spin.")

(defvar sprite-direct-async-check-interval 0.1
  "Seconds between event polls in `sprite-direct-promise-wait'.")

;;;; Wire-protocol encoding

(defun sprite-direct--encode (str)
  "Encode STR for the Emacs server wire protocol.
Mirrors `server-quote-arg': & → &&, - → &-, space → &_, newline → &n."
  (replace-regexp-in-string
   "[-& \n]"
   (lambda (s)
     (pcase s
       ("&" "&&")
       ("-" "&-")
       ("\n" "&n")
       (_ "&_")))
   str t t))

(defun sprite-direct--decode (str)
  "Decode STR from the Emacs server wire protocol.
Mirrors `server-unquote-arg': && → &, &- → -, &_ → space, &n → newline."
  (replace-regexp-in-string
   "&."
   (lambda (s)
     (pcase (aref s 1)
       (?& "&")
       (?- "-")
       (?n "\n")
       (_ " ")))
   str t t))

;;;; Connection parsing

(defun sprite-direct--parse-tcp (target)
  "Parse TARGET as a HOST:PORT:KEY TCP server string.
Returns a connection plist with :type, :host, :port, :key, or nil when
TARGET does not match the HOST:PORT:KEY pattern."
  (when (string-match "^\\(.*\\):\\([0-9]+\\):\\(.*\\)$" target)
    (list :type 'tcp
          :host (match-string 1 target)
          :port (string-to-number (match-string 2 target))
          :key (match-string 3 target))))

(defun sprite-direct--read-auth-key (name)
  "Return the auth key for server NAME from `server-auth-dir', or nil.
The auth file is written by server.el when the Emacs server starts."
  (when (and (boundp 'server-auth-dir) server-auth-dir)
    (let ((path (expand-file-name name server-auth-dir)))
      (when (file-readable-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (string-trim (buffer-string)))))))

(defun sprite-direct--socket-path (name)
  "Return the Unix socket path for server NAME, or nil when unavailable."
  (when (and (boundp 'server-socket-dir) server-socket-dir)
    (expand-file-name name server-socket-dir)))

(defun sprite-direct--parse-connection (target)
  "Parse TARGET into a connection plist.
TARGET is either a Unix socket name (e.g. \"work.0.render\") or a TCP
string \"HOST:PORT:KEY\".  Returns a plist with :type and :key at minimum."
  (or (sprite-direct--parse-tcp target)
      (list :type 'unix
            :name target
            :path (sprite-direct--socket-path target)
            :key (sprite-direct--read-auth-key target))))

;;;; Network process

(defun sprite-direct--filter (proc string)
  "Append STRING from PROC to the end of its process buffer."
  (when-let* ((buf (process-buffer proc)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert string))))

(defun sprite-direct--open (conn)
  "Open a network process described by CONN plist.
Returns the live process.  Signals an error and kills the buffer on failure."
  (let ((buf (generate-new-buffer " *sprite-direct*")))
    (condition-case err
        (pcase (map-elt conn :type)
          ('unix
           (make-network-process
            :name " sprite-direct"
            :buffer buf
            :filter #'sprite-direct--filter
            :family 'local
            :service (map-elt conn :path)
            :coding 'utf-8-unix
            :noquery t))
          ('tcp
           (make-network-process
            :name " sprite-direct"
            :buffer buf
            :filter #'sprite-direct--filter
            :host (map-elt conn :host)
            :service (map-elt conn :port)
            :coding 'utf-8-unix
            :noquery t))
          (type
           (user-error "sprite-direct: unknown connection type %S" type)))
      (error
       (kill-buffer buf)
       (signal (car err) (cdr err))))))

(defun sprite-direct--send (proc key form)
  "Send an eval request for FORM to PROC, authenticating with KEY when non-nil.
Emacs 29+ authenticates local Unix sockets via peer UID (SO_PEERCRED) rather
than cookie files, so KEY may be nil for local connections."
  (process-send-string
   proc
   (concat (when key (concat "-auth " key " "))
           "-eval "
           (sprite-direct--encode (format "%S" form))
           " \n")))

(defun sprite-direct--parse-response (raw)
  "Parse the RAW server response string; return the read Lisp value or nil.
A value too large for one message arrives as an opening `-print LINE'
followed by zero or more `-print-nonl LINE' continuation lines -- see
`server-reply-print' in server.el, which splits at `server-msg-size'
bytes and never marks which line is the last one; a real `emacsclient'
session simply prints each as it arrives and relies on the connection
closing to know the value is complete.  This reassembles every `-print'/
`-print-nonl' line in RAW, in order, decoding and concatenating each
segment before reading the result, so RAW must contain the *complete*
response (wait for the connection to close first; see
`sprite-direct--recv-gen' and `sprite-direct--promise-sentinel').
Returns nil for `-error' responses, when RAW is nil or not yet
newline-terminated (still streaming in), or when no recognised response
line is found or the reassembled text is unreadable."
  (when (and raw (string-suffix-p "\n" raw))
    (let (chunks errored)
      (seq-do
       (lambda (line)
         (cond
          ((string-match "\\`-print \\(.*\\)\\'" line)
           (push (match-string 1 line) chunks))
          ((string-match "\\`-print-nonl \\(.*\\)\\'" line)
           (push (match-string 1 line) chunks))
          ((string-prefix-p "-error " line)
           (setq errored t))))
       (split-string raw "\n" t))
      (unless (or errored (null chunks))
        (condition-case nil
            (read (sprite-direct--decode (apply #'concat (nreverse chunks))))
          (error nil))))))

;;;; Connection context

(cl-defstruct (sprite-direct-conn
               (:constructor sprite-direct--conn-make)
               (:copier nil))
  "Context for sprite-direct operations.
Holds the parsed connection plist from `sprite-direct--parse-connection'.
Does not own a persistent socket; each operation opens a transient one."
  target   ; original target string (socket name or HOST:PORT:KEY)
  plist)   ; cached result of sprite-direct--parse-connection

(defun sprite-direct-open (target)
  "Return a `sprite-direct-conn' for TARGET.
TARGET is a Unix socket name (e.g. \"work.0.render\") or HOST:PORT:KEY.
For TCP targets an auth key is required and `user-error' is signalled when
none is found.  For Unix sockets the auth key is optional: Emacs 29+
authenticates local connections via peer UID, so no cookie file is written."
  (let ((plist (sprite-direct--parse-connection target)))
    (when (and (null (map-elt plist :key))
               (eq (map-elt plist :type) 'tcp))
      (user-error "sprite-direct: no auth key found for TCP target %S" target))
    (sprite-direct--conn-make :target target :plist plist)))

(cl-defmacro with-sprite-direct ((var target) &rest body)
  "Bind VAR to a connection context for TARGET and evaluate BODY.
Each operation in BODY opens a transient socket as needed.
TARGET is evaluated once; the conn is created by `sprite-direct-open'."
  (declare (indent 1))
  (let ((gconn (make-symbol "conn")))
    `(let* ((,gconn (sprite-direct-open ,target))
            (,var ,gconn))
       ,@body)))

;;;; Generator-based receive

(defun sprite-direct--response-present-p (str)
  "Return t when STR contains a complete, newline-terminated response line.
Requires the closing newline, not just the `-print'/`-print-nonl'/
`-error' prefix, since the prefix alone would match the instant a line
*starts* arriving, before the rest of it (or a following continuation
line) has streamed in.  Requiring the prefix over any bare newline is
still necessary because the server sends `-emacs-pid PID\\n' immediately
on connection open, before the eval result.  This only detects that *a*
response line is present, not that the full (possibly multi-line) value
has arrived -- see `sprite-direct--recv-gen', which waits for the
connection to close instead of relying on this alone."
  (string-match-p "\\(?:^\\|\n\\)-\\(?:print\\|print-nonl\\|error\\) .*\n" str))

(iter-defun sprite-direct--recv-gen (proc)
  "Generator that yields `:pending' until PROC's connection closes.
A value too large for one message arrives as multiple `-print'/
`-print-nonl' lines with no marker on which one is last (see
`sprite-direct--parse-response'), so -- like a real `emacsclient'
session -- completion is signalled by the server closing the
connection, not by any recognisable line appearing.  The final return
value is the raw response string, or nil when PROC's buffer no longer
exists once it does."
  (while (process-live-p proc)
    (iter-yield :pending))
  (when-let* ((buf (process-buffer proc))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (buffer-string))))

(cl-defun sprite-direct--await (gen &key timeout proc)
  "Drive generator GEN to completion; return its final value.
Calls `accept-process-output' on each `:pending' yield, targeting PROC
when given (or any process when nil).  TIMEOUT overrides
`sprite-direct-blocking-timeout'; nil means block indefinitely.
Returns nil when the timeout expires before GEN produces a value."
  (let ((deadline (let ((limit (or timeout sprite-direct-blocking-timeout)))
                    (when limit (+ (float-time) limit)))))
    (catch 'done
      (while t
        (condition-case seq-val
            (when (eq :pending (iter-next gen))
              (when (and deadline (> (float-time) deadline))
                (iter-close gen)
                (throw 'done nil))
              (if proc
                  (accept-process-output proc sprite-direct-yield-interval nil t)
                (accept-process-output nil sprite-direct-yield-interval)))
          (iter-end-of-sequence
           (throw 'done (cdr seq-val))))))))

;;;; Blocking evaluation

(cl-defun sprite-direct-eval-blocking (conn form &key timeout)
  "Evaluate FORM in the sprite via CONN; return the Lisp value.
Opens a transient socket, sends the form, and blocks using the generator
receive loop.  TIMEOUT overrides `sprite-direct-blocking-timeout'.
Returns nil on connection failure or server error."
  (let* ((plist (sprite-direct-conn-plist conn))
         (key (map-elt plist :key))
         (proc (condition-case nil
                   (sprite-direct--open plist)
                 (error nil)))
         (buf (when proc (process-buffer proc))))
    (when proc
      (unwind-protect
          (progn
            (sprite-direct--send proc key form)
            (sprite-direct--parse-response
             (sprite-direct--await (sprite-direct--recv-gen proc) :timeout timeout :proc proc)))
        (when (process-live-p proc)
          (delete-process proc))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(cl-defun sprite-direct-call-and-read (target form &key timeout)
  "Evaluate FORM in the Emacs server at TARGET; return the read Lisp value.
Drop-in replacement for `sprite--call-and-read' in sprite.el.
TARGET is a Unix socket name or HOST:PORT:KEY TCP string.
TIMEOUT is passed to `sprite-direct-eval-blocking'.
Returns nil on connection failure, missing auth key, or server error."
  (condition-case nil
      (with-sprite-direct (conn target)
        (sprite-direct-eval-blocking conn form :timeout timeout))
    (user-error nil)))

;;;; Non-blocking evaluation (promise)

(defvar sprite-direct--op-counter 0
  "Monotonic counter for unique promise operation IDs.")

(defun sprite-direct--next-op-id ()
  "Return the next unique integer operation ID."
  (setq sprite-direct--op-counter (1+ sprite-direct--op-counter)))

(cl-defstruct (sprite-direct-promise
               (:constructor sprite-direct--promise-make)
               (:copier nil))
  "Result handle for a non-blocking sprite-direct evaluation.
`state' is one of :pending, :resolved, or :rejected.
`value' holds the Lisp result when :resolved, nil otherwise.
`result-buffer' is an optional buffer name where the result is appended."
  op-id          ; unique integer ID
  conn           ; originating sprite-direct-conn
  state          ; :pending :resolved :rejected
  value          ; Lisp result (nil until resolved)
  result-buffer  ; optional buffer name for result appending
  proc)          ; underlying transient network process

(defun sprite-direct--promise-sentinel (proc event)
  "Resolve the promise attached to PROC when the connection closes."
  (when (string-match-p "\\(?:finished\\|connection broken\\|deleted\\)" event)
    (when-let* ((promise (process-get proc 'sprite-direct-promise)))
      (let* ((raw (when-let* ((buf (process-buffer proc))
                              ((buffer-live-p buf)))
                    (with-current-buffer buf (buffer-string))))
             (value (when raw (sprite-direct--parse-response raw)))
             (result-buf (sprite-direct-promise-result-buffer promise)))
        (setf (sprite-direct-promise-state promise)
              (if (and raw (string-match-p "\n" raw)) :resolved :rejected)
              (sprite-direct-promise-value promise) value)
        (when (and result-buf value (get-buffer result-buf))
          (with-current-buffer result-buf
            (goto-char (point-max))
            (insert (format "%S\n" value))))
        (when-let* ((buf (process-buffer proc))
                    ((buffer-live-p buf)))
          (kill-buffer buf))))))

(cl-defun sprite-direct-eval-non-blocking (conn form &key result-buffer)
  "Evaluate FORM in CONN asynchronously; return a `sprite-direct-promise'.
The promise resolves when the server closes the connection (via sentinel).
RESULT-BUFFER is an optional buffer name; when the promise resolves, the
result is appended to it as a `prin1' line.  Returns the promise immediately.
If the connection cannot be opened, the promise is in :rejected state."
  (let* ((plist (sprite-direct-conn-plist conn))
         (key (map-elt plist :key))
         (op-id (sprite-direct--next-op-id))
         (proc (condition-case nil
                   (sprite-direct--open plist)
                 (error nil)))
         (promise (sprite-direct--promise-make
                   :op-id op-id
                   :conn conn
                   :state (if proc :pending :rejected)
                   :result-buffer result-buffer
                   :proc proc)))
    (when proc
      (process-put proc 'sprite-direct-promise promise)
      (set-process-sentinel proc #'sprite-direct--promise-sentinel)
      (sprite-direct--send proc key form))
    promise))

(defun sprite-direct-promise-pending-p (promise)
  "Return t when PROMISE has not yet resolved or been rejected."
  (eq :pending (sprite-direct-promise-state promise)))

(defun sprite-direct-promise-resolved-p (promise)
  "Return t when PROMISE resolved successfully."
  (eq :resolved (sprite-direct-promise-state promise)))

(defun sprite-direct-promise-rejected-p (promise)
  "Return t when PROMISE failed.
Either the connection could not be opened, or it closed without ever
producing a `-print'/`-error' response line."
  (eq :rejected (sprite-direct-promise-state promise)))

(cl-defun sprite-direct-promise-wait (promise &key timeout)
  "Block until PROMISE resolves; return its value or nil on timeout/rejection.
Drives process output while waiting.  TIMEOUT overrides
`sprite-direct-blocking-timeout'; nil means wait indefinitely."
  (let* ((proc (sprite-direct-promise-proc promise))
         (limit (or timeout sprite-direct-blocking-timeout))
         (deadline (when limit (+ (float-time) limit))))
    (while (and (sprite-direct-promise-pending-p promise)
                (or (null deadline) (< (float-time) deadline)))
      (accept-process-output proc sprite-direct-async-check-interval nil t))
    (sprite-direct-promise-value promise)))

(defun sprite-direct-promise-then (promise callback)
  "Arrange for CALLBACK to run once PROMISE leaves its `:pending' state.
Unlike `sprite-direct-promise-wait', this does not block: it polls PROMISE
on a repeating timer (`sprite-direct-async-check-interval') and cancels
itself the first time `sprite-direct-promise-pending-p' is nil, at which
point it calls CALLBACK with two arguments, STATE (`:resolved' or
`:rejected') and VALUE (`sprite-direct-promise-value', nil when rejected).
The promise's own sentinel still does the actual work of receiving the
result; this only supplies the missing \"notify me when done\" callback on
top of the pending/resolved/rejected state it already exposes.  Returns
the timer, so the caller can `cancel-timer' it to abandon the callback."
  (letrec ((timer
            (run-with-timer
             0 sprite-direct-async-check-interval
             (lambda ()
               (unless (sprite-direct-promise-pending-p promise)
                 (cancel-timer timer)
                 (funcall callback (sprite-direct-promise-state promise)
                          (sprite-direct-promise-value promise)))))))
    timer))

;;;; Helper operations

(defun sprite-direct-list-buffers (conn)
  "Return a list of buffer names in the sprite via CONN."
  (sprite-direct-eval-blocking conn '(mapcar #'buffer-name (buffer-list))))

(cl-defun sprite-direct-insert-into-buffer (conn buf-name text &key position)
  "Insert TEXT into BUF-NAME in the sprite via CONN.
When POSITION is non-nil, go there first; otherwise insert at `point-max'."
  (sprite-direct-eval-blocking
   conn
   `(with-current-buffer ,buf-name
      ,(if position
           `(progn (goto-char ,position) (insert ,text))
         `(progn (goto-char (point-max)) (insert ,text))))))

(cl-defmacro sprite-direct-with-current-buffer ((conn buf-name) &rest body)
  "Evaluate BODY forms in the sprite via CONN with BUF-NAME current.
BODY is literal Emacs Lisp sent verbatim to the sprite; local variables
are not in scope.  The result of the last BODY form is returned."
  (declare (indent 1))
  (let ((gconn (make-symbol "conn"))
        (gbuf (make-symbol "buf")))
    `(let ((,gconn ,conn)
           (,gbuf ,buf-name))
       (sprite-direct-eval-blocking
        ,gconn
        (append (list 'with-current-buffer ,gbuf) ',body)))))

(cl-defun sprite-direct-read-buffer (conn buf-name &key start end)
  "Return the contents of BUF-NAME in the sprite via CONN.
START and END are buffer positions (defaults: `point-min' and `point-max').
Returns a string, or nil on failure."
  (sprite-direct-eval-blocking
   conn
   `(with-current-buffer ,buf-name
      (buffer-substring-no-properties
       ,(or start '(point-min))
       ,(or end '(point-max))))))

(defun sprite-direct-value-of-symbol (conn symbol)
  "Return the value of SYMBOL in the sprite via CONN.
Evaluates `(symbol-value \\='SYMBOL)' in the sprite; returns nil on failure."
  (sprite-direct-eval-blocking conn `(symbol-value ',symbol)))

;;;; TCP server file utility

(defun sprite-direct-read-tcp-server-file (path)
  "Read the TCP server file at PATH; return its HOST:PORT:KEY string.
Returns nil when PATH is absent or unreadable."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (string-trim (buffer-string)))))

(provide 'sprite-direct)
;;; sprite-direct.el ends here
