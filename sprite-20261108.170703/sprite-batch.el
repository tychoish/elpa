;;; sprite-batch.el --- Yielding batch execution engine for sprite -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Keywords: tools, processes, convenience

;;; Commentary:
;; Sequential batch execution of operations yielding between tasks via idle
;; timers, with argument passing, error isolation, slow-op logging, and
;; execution in designated frame and buffer contexts.
;; Cancel with `sprite-batch-cancel' (ACR menu).  Opt in to `C-g' / `C-x C-g'
;; keys with `sprite-batch-mode' (buffer) or `global-sprite-batch-mode'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'sprite)

(defgroup sprite-batch nil
  "Yielding batch execution for sprite."
  :group 'tools
  :prefix "sprite-batch-")

(cl-defstruct (sprite-batch-handle (:constructor sprite-batch-handle--make) (:copier nil))
  "Handle for an active batch execution."
  id                 ; unique integer ID
  name               ; human-readable batch name or prefix
  state              ; 'running, 'cancelled, 'done
  (current 0)        ; completed items count
  total              ; total items count (integer or nil)
  timer              ; idle timer handle
  cancel-fn          ; cancellation function
  created-at)        ; start timestamp

(defvar sprite-batch--active-handles nil
  "List of currently active `sprite-batch-handle' instances.")

(defvar sprite-batch--id-counter 0
  "Monotonically increasing ID counter for batch operations.")

(defun sprite-batch-active-handles ()
  "Return a copy of the currently active `sprite-batch-handle' instances."
  (copy-sequence sprite-batch--active-handles))

(defun sprite-batch--progress (handle)
  "Return HANDLE's progress as \"N/TOTAL\", or \"N\" when TOTAL is unknown."
  (let ((curr (sprite-batch-handle-current handle))
        (tot (sprite-batch-handle-total handle)))
    (if tot (format "%d/%d" curr tot) (format "%d" curr))))

(defmacro sprite-batch--with-context (frame buffer &rest body)
  "Execute BODY with optional FRAME and BUFFER context.
If BUFFER is `temp', BODY executes inside `with-temp-buffer'.
If BUFFER is a buffer or string name, BODY executes in that buffer.
If FRAME is non-nil and live, BODY executes with FRAME selected."
  (declare (indent 2))
  (let ((buf-sym (gensym "buf"))
        (frm-sym (gensym "frm")))
    `(let ((,frm-sym ,frame)
           (,buf-sym ,buffer))
       (when (or (null ,frm-sym) (frame-live-p ,frm-sym))
         (with-selected-frame (or ,frm-sym (selected-frame))
           (cond ((eq ,buf-sym 'temp)
                  (with-temp-buffer ,@body))
                 ((bufferp ,buf-sym)
                  (when (buffer-live-p ,buf-sym)
                    (with-current-buffer ,buf-sym ,@body)))
                 ((stringp ,buf-sym)
                  (with-current-buffer (get-buffer-create ,buf-sym) ,@body))
                 (t ,@body)))))))

(defun sprite-batch--execute-operation (fn &optional args frame buffer log-prefix)
  "Run operation FN with ARGS in FRAME and BUFFER context, reporting errors.
When `sprite-slow-op-reporting' or `init-file-debug' is non-nil,
log operation duration with LOG-PREFIX if it exceeds `sprite-slow-op-threshold'."
  (condition-case err
      (sprite-batch--with-context frame buffer
        (when (and (symbolp fn) (fboundp fn) (autoloadp (symbol-function fn)))
          (autoload-do-load (symbol-function fn) fn))
        (with-sprite-slow-op-timer (format "%s %s" (or log-prefix "[run]") fn)
          (if (equal (cdr (func-arity fn)) 0)
              (funcall fn)
            (apply fn (ensure-list args)))))
    (error (message "[sprite] %s failed: %s" fn (error-message-string err)))))

;;;###autoload
(cl-defun sprite-run-batch (&key operations ops (delay 0.1) run-after initial-delay
                                 args arglist on-done frame buffer (log-prefix "[run]")
                                 name)
  "Run OPERATIONS in order, yielding between each in interactive sessions.
OPERATIONS (or OPS) may be a list of functions, a vector, a generator
iterator, a gen stream, a single function, or a symbol whose value is
a list of functions (such as a hook).

Supported keyword arguments:
  :operations or :ops
    List of operations, vector, generator iterator, single function,
    or hook symbol.
  :run-after or :initial-delay SECONDS
    Initial delay before the first operation (defaults to :delay).
  :delay SECONDS
    Amount of idle time between operations (defaults to 0.1).
  :args or :arglist LIST or ATOM
    Arguments passed to any non-nilary operation.
  :on-done FUNCTION
    Callback invoked when all operations finish.
  :frame FRAME
    Optional frame context for `with-selected-frame'.
  :buffer BUFFER
    Optional buffer context: `temp' executes in `with-temp-buffer',
    a buffer object or string name executes in `with-current-buffer'.
  :log-prefix STRING
    Timer log prefix (defaults to \"[run]\").
  :name STRING
    Human-readable batch identifier for status display and cancel menus.

In noninteractive (batch) sessions, runs all operations synchronously.
Return a cancellation function."
  (let* ((source (or operations ops))
         (items (cond ((and (symbolp source) (boundp source)
                            (listp (symbol-value source)))
                       ;; Hooks may contain `t' (run the global value); drop it.
                       (remq t (symbol-value source)))
                      ((and (symbolp source) (fboundp source))
                       (list source))
                      ((and (functionp source)
                            (not (equal (func-arity source) '(2 . 2)))
                            (not (and (fboundp 'gen-p) (funcall 'gen-p source))))
                       (list source))
                      (t source)))
         (iter (sprite--make-iterator items))
         (total (car iter))
         (next-fn (cdr iter))
         (wait (or run-after initial-delay delay 0.1))
         (delay-sec (or delay 0.1))
         (fn-args (ensure-list (or args arglist))))
    (if (or noninteractive (eq total 0))
        (let (next)
          (while (setq next (funcall next-fn))
            (sprite-batch--execute-operation (cdr next) fn-args frame buffer log-prefix))
          (when on-done (funcall on-done))
          #'ignore)
      (let* ((batch-id (cl-incf sprite-batch--id-counter))
             (handle (sprite-batch-handle--make
                      :id batch-id
                      :name (or name (format "batch-%d" batch-id))
                      :state 'running
                      :total total
                      :created-at (current-time))))
        (letrec ((timer nil)
                 ;; Move HANDLE from `running' to STATE exactly once.
                 ;; Return non-nil only for the call that made the transition.
                 ;; Cancellation logs how far the batch got.
                 (finish
                  (lambda (state &optional reason)
                    (when (eq (sprite-batch-handle-state handle) 'running)
                      (setf (sprite-batch-handle-state handle) state
                            (sprite-batch-handle-timer handle) nil)
                      (when timer (cancel-timer timer))
                      (setq timer nil
                            sprite-batch--active-handles
                            (delq handle sprite-batch--active-handles))
                      (when (eq state 'cancelled)
                        (message "[sprite] cancelled #%d %s at %s ops completed%s"
                                 (sprite-batch-handle-id handle)
                                 (sprite-batch-handle-name handle)
                                 (sprite-batch--progress handle)
                                 (if reason (format " (%s)" reason) "")))
                      t)))
                 (cancel-fn (lambda () (funcall finish 'cancelled)))
                 (schedule
                  (lambda (secs)
                    (setq timer (run-with-idle-timer secs nil step))
                    (setf (sprite-batch-handle-timer handle) timer)))
                 (step
                  (lambda ()
                    (setq timer nil)
                    (when (eq (sprite-batch-handle-state handle) 'running)
                      (let ((next (condition-case err (funcall next-fn)
                                    (error
                                     (message "[sprite] %s iterator failed: %s"
                                              (sprite-batch-handle-name handle)
                                              (error-message-string err))
                                     nil))))
                        (if (null next)
                            (when (funcall finish 'done)
                              (when on-done
                                (condition-case err (funcall on-done)
                                  (error (message "[sprite] %s on-done failed: %s"
                                                  (sprite-batch-handle-name handle)
                                                  (error-message-string err))))))
                          ;; C-g during an operation cancels the batch instead
                          ;; of leaving it registered but never rescheduled.
                          (condition-case nil
                              (sprite-batch--execute-operation
                               (cdr next) fn-args frame buffer log-prefix)
                            (quit (funcall finish 'cancelled
                                           "interrupted by quit during an operation")))
                          (cl-incf (sprite-batch-handle-current handle))
                          (when (eq (sprite-batch-handle-state handle) 'running)
                            (funcall schedule delay-sec))))))))
          (setf (sprite-batch-handle-cancel-fn handle) cancel-fn)
          (push handle sprite-batch--active-handles)
          (funcall schedule wait)
          cancel-fn)))))

;;;###autoload
(defalias 'sprite-batch-run #'sprite-run-batch)

;;;###autoload
(defalias 'sprite-run #'sprite-run-batch)

;;;###autoload
(defun sprite-batch-startup (&optional operations &rest args)
  "Run startup OPERATIONS yielding between each in interactive sessions.
OPERATIONS defaults to `sprite-startup-operations'.
Supported ARGS keyword arguments include :delay, :run-after, :on-done, :frame,
:buffer, and :name.
Specialized case of `sprite-run-batch' with default args `(1)' and log
prefix \"[startup]\"."
  (let* ((has-ops (not (or (null operations) (keywordp operations))))
         (ops (if has-ops operations sprite-startup-operations))
         (rest-args (if (keywordp operations)
                        (cons operations args)
                      args))
         (defaults (list :operations ops
                         :args '(1)
                         :log-prefix "[startup]"
                         :name "startup")))
    (apply #'sprite-run-batch (append rest-args defaults))))

;;;###autoload
(defalias 'sprite-run-startup-operations #'sprite-batch-startup)

;;;###autoload
(defun sprite-batch-cancel (&optional target)
  "Cancel in-flight batch execution identified by TARGET.
TARGET may be:
- `:all' to cancel all active batches
- A `sprite-batch-handle' instance
- An integer batch ID
- A cancellation function returned by `sprite-run-batch'
- nil: interactively, prompt via `sprite-batch-cancel-menu'; from Lisp,
  same as `:all'.
Return the list of handles that were actually cancelled."
  (interactive)
  (if (and (null target) (called-interactively-p 'interactive))
      (sprite-batch-cancel-menu)
    (let ((handles
           (cond
            ((memq target '(nil :all)) (copy-sequence sprite-batch--active-handles))
            ((sprite-batch-handle-p target) (list target))
            ((integerp target)
             (seq-filter (lambda (h) (= (sprite-batch-handle-id h) target))
                         sprite-batch--active-handles))
            ((functionp target)
             (seq-filter (lambda (h) (eq (sprite-batch-handle-cancel-fn h) target))
                         sprite-batch--active-handles))
            (t (user-error "Unknown batch target: %S" target)))))
      (seq-filter (lambda (h)
                    (let ((fn (sprite-batch-handle-cancel-fn h)))
                      (and fn (funcall fn))))
                  handles))))

(defun sprite-batch--candidate (handle)
  "Return the completion candidate string for HANDLE."
  (format "#%d: %s" (sprite-batch-handle-id handle) (sprite-batch-handle-name handle)))

(defun sprite-batch--annotation (handle)
  "Return the progress annotation for HANDLE."
  (format "%s completed" (sprite-batch--progress handle)))

;;;###autoload
(defun sprite-batch-cancel-menu (&optional include-quit)
  "Interactively select an active batch to cancel, cancel all, or do nothing.
With INCLUDE-QUIT, also offer (as the default) \"Quit current operation\",
the stock `keyboard-quit', which leaves batches running.  Pressing `C-g'
inside the menu aborts it as a normal quit."
  (interactive)
  (let ((active (sprite-batch-active-handles)))
    (if (null active)
        (progn (message "[sprite-batch] No active batches running") nil)
      (let* ((quit-candidate "Quit current operation")
             (all-candidate "Cancel ALL active batches")
             (nothing-candidate "Do nothing")
             (table (append
                     (when include-quit
                       (list (cons quit-candidate "Normal C-g; batches keep running")))
                     (mapcar (lambda (h) (cons (sprite-batch--candidate h)
                                               (sprite-batch--annotation h)))
                             active)
                     (when (cdr active)
                       (list (cons all-candidate
                                   (format "%d batches active" (length active)))))
                     (list (cons nothing-candidate "Dismiss this menu"))))
             (choice (sprite--annotated-or-plain-read
                      "Select batch to cancel: " table
                      (and include-quit quit-candidate))))
        (cond
         ((string= choice nothing-candidate) nil)
         ((member choice (list nil "" quit-candidate))
          (keyboard-quit))
         ((string= choice all-candidate)
          (sprite-batch-cancel :all))
         (t
          (let ((handle (cl-find choice active :key #'sprite-batch--candidate
                                 :test #'string=)))
            (unless (and handle (sprite-batch-cancel handle))
              (message "[sprite-batch] Batch %s no longer active" choice)))))))))

;;;###autoload
(defun sprite-batch-cancel-or-quit (&optional force-quit)
  "Offer `sprite-batch-cancel-menu' when batches are running, else quit.
Behaves exactly like `keyboard-quit' when no batches are active, inside the
minibuffer, with an active region, or with FORCE-QUIT (\\[universal-argument])."
  (interactive "P")
  (if (or force-quit
          (minibufferp)
          (use-region-p)
          (null sprite-batch--active-handles))
      (keyboard-quit)
    (sprite-batch-cancel-menu t)))

;;;###autoload
(defvar sprite-batch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit] #'sprite-batch-cancel-or-quit)
    (define-key map (kbd "C-x C-g") #'sprite-batch-cancel)
    map)
  "Keymap for `sprite-batch-mode'.
`C-g' offers the batch menu while batches run (`C-u C-g' always quits);
`C-x C-g' opens the cancel menu directly, from anywhere.")

;;;###autoload
(define-minor-mode sprite-batch-mode
  "Opt-in batch cancellation keys in the current buffer.
While batches are running, `C-g' offers a menu to cancel them; otherwise
`C-g' is the normal `keyboard-quit'.  `C-x C-g' always opens the cancel
menu.  Without this mode, `C-g' is never altered; use `sprite-batch-cancel'.
See `global-sprite-batch-mode' to enable it everywhere."
  :lighter " Batch"
  :keymap sprite-batch-mode-map
  :group 'sprite-batch)

;;;###autoload
(define-globalized-minor-mode global-sprite-batch-mode
  sprite-batch-mode sprite-batch-mode
  :group 'sprite-batch)

(provide 'sprite-batch)
;;; sprite-batch.el ends here
