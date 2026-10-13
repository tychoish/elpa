;;; sprite-fleet.el --- Fleet-level combinators over sprite futures -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (seq "2.24"))
;; URL: https://github.com/tychoish/sprite
;; Keywords: tools, daemon, processes

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Higher-order combinators dispatching to multiple sprites concurrently,
;; built entirely on the future API in sprite-future.el.
;;
;; Features:
;;   - Yielding stepper engine via timers preventing event-loop starvation
;;   - Unified flow control: cancel, pause, resume
;;   - Streaming and progress reporting
;;   - Configurable error policies (:collect-nil, :collect-error, :abort)
;;   - Flexible ordering (:preserve, :fifo, :lifo, :unordered)
;;   - Support for lists, vectors, generator iterators, and lazy streams
;;
;; Entry points:
;;   `sprite-fleet-pool'           — low-level pool runner returning handle
;;   `sprite-fleet-map'            — modern async map returning future with handle
;;   `sprite-fleet-each'           — streaming worker runner with callbacks
;;   `sprite-fleet-wait'           — wait synchronously on fleet handle or future
;;   `sprite-fleet-cancel'         — cancel in-flight fleet execution
;;   `sprite-fleet-pause'          — pause dispatching new items
;;   `sprite-fleet-resume'         — resume paused fleet execution
;;   `sprite-fleet-progress'       — get (completed . total) progress pair
;;   `sprite-fleet-mapcar'         — legacy wrapper over sprite-fleet-map
;;   `sprite-fleet-mapcar-sprites' — collect, ordered (mirrors `mapcar')
;;   `sprite-fleet-mapc'           — fire-and-forget (mirrors `mapc')
;;   `sprite-fleet-let'            — parallel destructuring bind (mirrors `let')

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'generator)
(require 'sprite)
(require 'sprite-daemon)
(require 'sprite-future)

(declare-function gen-p "gen" (sequence))
(declare-function gen--next "gen" (sequence))

;;;; Structs and types

(cl-defstruct (sprite-fleet-handle (:constructor sprite-fleet-handle--make) (:copier nil))
  "Controller handle for an in-flight fleet execution."
  state              ; :running, :paused, :canceled, :completed
  future             ; overall sprite-future resolving to results
  fn-or-form         ; function or form to evaluate
  iterator           ; closure: 0 args -> (cons idx item) or nil
  total-items        ; integer count or nil
  (completed-count 0); integer count of completed items
  (dispatched-count 0); integer count of dispatched items
  active-workers     ; hash-table: target-name -> in-flight future
  available-targets  ; list of idle target names
  timer              ; active run-with-timer handle or nil
  delay              ; seconds between dispatches (default 0.0)
  order              ; 'preserve, 'fifo, 'lifo, 'unordered
  error-policy       ; :collect-nil, :collect-error, :abort
  on-item            ; callback: (lambda (item result idx))
  on-done            ; callback: (lambda (results))
  results            ; vector (when total known & preserve) or alist/list
  exhausted-p)       ; non-nil when iterator has returned nil

(defvar sprite-fleet--handle-by-future
  (make-hash-table :weakness 'key :test 'eq)
  "Weak mapping from fleet `sprite-future' to its `sprite-fleet-handle'.")

;;;; Predicates

(defun sprite-fleet-running-p (handle)
  "Return non-nil if HANDLE is actively running."
  (eq :running (sprite-fleet-handle-state handle)))

(defun sprite-fleet-paused-p (handle)
  "Return non-nil if HANDLE is paused."
  (eq :paused (sprite-fleet-handle-state handle)))

(defun sprite-fleet-canceled-p (handle)
  "Return non-nil if HANDLE was canceled."
  (eq :canceled (sprite-fleet-handle-state handle)))

(defun sprite-fleet-completed-p (handle)
  "Return non-nil if HANDLE finished execution."
  (eq :completed (sprite-fleet-handle-state handle)))

(defun sprite-fleet-progress (handle)
  "Return (COMPLETED . TOTAL) for HANDLE.
TOTAL may be nil if input sequence was a lazy iterator of unknown size."
  (cons (sprite-fleet-handle-completed-count handle)
        (sprite-fleet-handle-total-items handle)))

(defun sprite-future-fleet-handle (future)
  "Return the `sprite-fleet-handle' associated with FUTURE, if any."
  (when (sprite-future-p future)
    (gethash future sprite-fleet--handle-by-future)))

;;;; Helper utilities

(defun sprite-fleet--make-item-form (fn-or-form item)
  "Construct expression to evaluate FN-OR-FORM for ITEM."
  (cond
   ((and (symbolp fn-or-form) (fboundp fn-or-form))
    `(,fn-or-form ',item))
   ((and (consp fn-or-form) (eq (car fn-or-form) 'lambda))
    `(funcall ',fn-or-form ',item))
   ((consp fn-or-form)
    `(let ((item ',item) (_ ',item)) ,fn-or-form))
   (t `(funcall ,fn-or-form ',item))))

(defun sprite-fleet--make-iterator (sequence)
  "Normalize SEQUENCE into (TOTAL-COUNT . NEXT-FN).
NEXT-FN is a function of zero arguments returning (IDX . ITEM) or nil."
  (let ((idx 0))
    (cond
     ((null sequence)
      (cons 0 (lambda () nil)))
     ((vectorp sequence)
      (let ((total (length sequence)))
        (cons total
              (lambda ()
                (when (< idx total)
                  (let ((curr idx))
                    (setq idx (1+ idx))
                    (cons curr (aref sequence curr))))))))
     ;; gen struct duck-typing (gen-p or has gen--next)
     ((and (fboundp 'gen-p) (funcall 'gen-p sequence))
      (cons nil
            (lambda ()
              (let ((v (funcall 'gen--next sequence)))
                (when v
                  (let ((curr idx))
                    (setq idx (1+ idx))
                    (cons curr (car v))))))))
     ;; generator.el iterators, functions, or closures
     ;; Note: in Emacs 29, closures satisfy `functionp' and `listp';
     ;; checking `functionp' before `listp' prevents misidentifying
     ;; generator iterators as lists.
     ((functionp sequence)
      (cons nil
            (lambda ()
              (condition-case nil
                  (let ((item (iter-next sequence))
                        (curr idx))
                    (setq idx (1+ idx))
                    (cons curr item))
                (iter-end-of-sequence nil)
                (error nil)))))
     ((listp sequence)
      (let ((total (length sequence))
            (remaining sequence))
        (cons total
              (lambda ()
                (when remaining
                  (let ((curr idx)
                        (item (pop remaining)))
                    (setq idx (1+ idx))
                    (cons curr item)))))))
     (t
      (cons nil
            (lambda ()
              (condition-case nil
                  (let ((item (iter-next sequence))
                        (curr idx))
                    (setq idx (1+ idx))
                    (cons curr item))
                (iter-end-of-sequence nil)
                (error nil))))))))

(defun sprite-fleet--format-error (err target)
  "Format error ERR from TARGET according to fleet error policy."
  (list :error err :target target))

(defun sprite-fleet--record-result (handle idx val)
  "Store VAL at IDX in HANDLE's result accumulator according to order."
  (let ((order (sprite-fleet-handle-order handle))
        (results (sprite-fleet-handle-results handle)))
    (cond
     ((and (vectorp results) (memq order '(preserve t)))
      (aset results idx val))
     ((memq order '(preserve t))
      ;; Lazy iterator without known total length: store (idx . val) pairs
      (setf (sprite-fleet-handle-results handle)
            (cons (cons idx val) results)))
     (t
      ;; fifo, lifo, unordered: push directly
      (setf (sprite-fleet-handle-results handle)
            (cons val results))))))

(defun sprite-fleet--finalize-results (handle)
  "Extract ordered list of results from HANDLE."
  (let ((order (sprite-fleet-handle-order handle))
        (results (sprite-fleet-handle-results handle)))
    (cond
     ((vectorp results)
      (append results nil))
     ((memq order '(preserve t))
      (let ((sorted (sort results (lambda (a b) (< (car a) (car b))))))
        (mapcar #'cdr sorted)))
     ((eq order 'fifo)
      (nreverse results))
     (t
      results))))

(defun sprite-fleet--notify-item (handle item val idx)
  "Invoke on-item callback if defined on HANDLE."
  (when-let* ((cb (sprite-fleet-handle-on-item handle)))
    (condition-case cb-err
        (funcall cb item val idx)
      (error
       (display-warning 'sprite
                        (format "Error in sprite-fleet on-item callback: %S" cb-err)
                        :warning)))))

;;;; Stepper Engine

(defun sprite-fleet--schedule-step (handle)
  "Schedule a stepper invocation for HANDLE on the event loop."
  (when (and (sprite-fleet-running-p handle)
             (null (sprite-fleet-handle-timer handle)))
    (setf (sprite-fleet-handle-timer handle)
          (run-with-timer (sprite-fleet-handle-delay handle) nil
                          #'sprite-fleet--step handle))))

(defun sprite-fleet--step (handle)
  "Execute one stepper cycle for HANDLE."
  (setf (sprite-fleet-handle-timer handle) nil)
  (when (sprite-fleet-running-p handle)
    (let ((target (car (sprite-fleet-handle-available-targets handle))))
      (if (and target (not (sprite-fleet-handle-exhausted-p handle)))
          (let ((next (funcall (sprite-fleet-handle-iterator handle))))
            (if (null next)
                (progn
                  (setf (sprite-fleet-handle-exhausted-p handle) t)
                  (when (= 0 (hash-table-count (sprite-fleet-handle-active-workers handle)))
                    (sprite-fleet--finalize handle)))
              (pop (sprite-fleet-handle-available-targets handle))
              (sprite-fleet--dispatch-worker handle target (car next) (cdr next))
              (when (and (sprite-fleet-handle-available-targets handle)
                         (not (sprite-fleet-handle-exhausted-p handle)))
                (sprite-fleet--schedule-step handle))))
        (when (and (sprite-fleet-handle-exhausted-p handle)
                   (= 0 (hash-table-count (sprite-fleet-handle-active-workers handle))))
          (sprite-fleet--finalize handle))))))

(defun sprite-fleet--dispatch-worker (handle target idx item)
  "Dispatch worker evaluation for ITEM at IDX on TARGET for HANDLE."
  (cl-incf (sprite-fleet-handle-dispatched-count handle))
  (let* ((form (sprite-fleet--make-item-form (sprite-fleet-handle-fn-or-form handle) item))
         (future (condition-case err
                     (sprite-future-eval target form)
                   (error
                    (let ((f (sprite-future--make :target target :state :pending)))
                      (sprite-future--settle f :rejected err)
                      f)))))
    (puthash target future (sprite-fleet-handle-active-workers handle))
    (sprite-future-then
     future
     (lambda (val)
       (sprite-fleet--on-worker-settled handle target idx item val :resolved))
     (lambda (err)
       (sprite-fleet--on-worker-settled handle target idx item err :rejected)))))

(defun sprite-fleet--on-worker-settled (handle target idx item val status)
  "Handle completion of worker on TARGET for ITEM at IDX in HANDLE with STATUS."
  (unless (sprite-fleet-canceled-p handle)
    (remhash target (sprite-fleet-handle-active-workers handle))
    (push target (sprite-fleet-handle-available-targets handle))
    (cl-incf (sprite-fleet-handle-completed-count handle))
    (let ((error-p (eq status :rejected)))
      (if (and error-p (eq (sprite-fleet-handle-error-policy handle) :abort))
          (sprite-fleet-cancel handle (list :worker-failed target val))
        (let ((result-val (cond
                           ((not error-p) val)
                           ((eq (sprite-fleet-handle-error-policy handle) :collect-error)
                            (sprite-fleet--format-error val target))
                           (t nil))))
          (sprite-fleet--record-result handle idx result-val)
          (sprite-fleet--notify-item handle item val idx)
          (if (and (sprite-fleet-handle-exhausted-p handle)
                   (= 0 (hash-table-count (sprite-fleet-handle-active-workers handle))))
              (sprite-fleet--finalize handle)
            (when (sprite-fleet-running-p handle)
              (sprite-fleet--schedule-step handle))))))))

(defun sprite-fleet--finalize (handle)
  "Finalize HANDLE once all items have completed."
  (unless (or (sprite-fleet-canceled-p handle)
              (sprite-fleet-completed-p handle))
    (setf (sprite-fleet-handle-state handle) :completed)
    (when (sprite-fleet-handle-timer handle)
      (cancel-timer (sprite-fleet-handle-timer handle))
      (setf (sprite-fleet-handle-timer handle) nil))
    (let ((results (sprite-fleet--finalize-results handle)))
      (sprite-future--settle (sprite-fleet-handle-future handle) :resolved results)
      (when (sprite-fleet-handle-on-done handle)
        (condition-case done-err
            (funcall (sprite-fleet-handle-on-done handle) results)
          (error
           (display-warning 'sprite
                            (format "Error in sprite-fleet on-done callback: %S" done-err)
                            :warning)))))))

;;;; Flow Control Operations

(defun sprite-fleet-cancel (target &optional reason)
  "Cancel in-flight fleet execution for TARGET (handle or future).
Aborts scheduled timers, severs client sockets / kills processes for
all active worker futures, and settles the overall future as rejected with
(cons :canceled (or REASON :user-cancel))."
  (let ((handle (cond
                 ((sprite-fleet-handle-p target) target)
                 ((sprite-future-p target) (sprite-future-fleet-handle target))
                 (t (error "Expected sprite-fleet-handle or sprite-future: %S" target)))))
    (unless (or (sprite-fleet-canceled-p handle)
                (sprite-fleet-completed-p handle))
      (setf (sprite-fleet-handle-state handle) :canceled)
      (when (sprite-fleet-handle-timer handle)
        (cancel-timer (sprite-fleet-handle-timer handle))
        (setf (sprite-fleet-handle-timer handle) nil))
      (maphash (lambda (_target future)
                 (when (sprite-future-p future)
                   (sprite-future-cancel future (or reason :user-cancel))))
               (sprite-fleet-handle-active-workers handle))
      (clrhash (sprite-fleet-handle-active-workers handle))
      (sprite-future--settle (sprite-fleet-handle-future handle)
                             :rejected
                             (cons :canceled (or reason :user-cancel))))
    handle))

(defun sprite-fleet-pause (handle)
  "Pause dispatching new items for HANDLE.
In-flight worker futures continue to run and collect results, but
freed targets will not be dispatched to until resumed."
  (when (sprite-fleet-running-p handle)
    (setf (sprite-fleet-handle-state handle) :paused)
    (when (sprite-fleet-handle-timer handle)
      (cancel-timer (sprite-fleet-handle-timer handle))
      (setf (sprite-fleet-handle-timer handle) nil)))
  handle)

(defun sprite-fleet-resume (handle)
  "Resume dispatching items for paused HANDLE."
  (when (sprite-fleet-paused-p handle)
    (setf (sprite-fleet-handle-state handle) :running)
    (sprite-fleet--schedule-step handle))
  handle)

(defun sprite-fleet-wait (target &optional timeout)
  "Block until TARGET (handle or future) settles; return results or nil.
TIMEOUT overrides `sprite-direct-blocking-timeout'."
  (let ((future (if (sprite-fleet-handle-p target)
                    (sprite-fleet-handle-future target)
                  target)))
    (sprite-future-wait future :timeout timeout)))

;;;; High-Level Combinators

;;;###autoload
(cl-defun sprite-fleet-pool (fn-or-form items
                             &key fleet-size sprites (delay 0.0)
                                  (order 'preserve) (error-policy :collect-nil)
                                  on-item on-done)
  "Create and start a `sprite-fleet-handle' executing FN-OR-FORM over ITEMS.
FLEET-SIZE is the maximum number of concurrent sprites.
SPRITES is a list of sprite structs or target name strings.
DELAY is the yielding delay in seconds between dispatches (default 0.0).
ORDER specifies result ordering: `preserve' (default), `fifo', `lifo',
or `unordered'.
ERROR-POLICY specifies failure behavior: `:collect-nil' (default),
`:collect-error', or `:abort'.
ON-ITEM is an optional callback (lambda (item result idx)).
ON-DONE is an optional callback (lambda (results)).

Returns a `sprite-fleet-handle'."
  (cl-check-type order (member preserve fifo lifo unordered))
  (cl-check-type error-policy (member :collect-nil :collect-error :abort))
  (when (and delay (< delay 0.0))
    (error "delay must be non-negative: %S" delay))
  (let* ((iter-pair (sprite-fleet--make-iterator items))
         (total (car iter-pair))
         (iterator (cdr iter-pair))
         (empty-p (and (numberp total) (= total 0)))
         (effective-size (cond
                          (empty-p 0)
                          (fleet-size fleet-size)
                          (total (min total sprite-max-count))
                          (t sprite-max-count)))
         (worker-sprites (if empty-p
                             nil
                           (or sprites (sprite-get-or-create-fleet (max 1 effective-size)))))
         (target-names (seq-map #'sprite--target-name worker-sprites))
         (overall-future (sprite-future--make :target "fleet" :state :pending))
         (results (when (and (numberp total) (memq order '(preserve t)))
                    (make-vector total nil)))
         (handle (sprite-fleet-handle--make
                  :state :running
                  :future overall-future
                  :fn-or-form fn-or-form
                  :iterator iterator
                  :total-items total
                  :completed-count 0
                  :dispatched-count 0
                  :active-workers (make-hash-table :test 'equal)
                  :available-targets (copy-sequence target-names)
                  :timer nil
                  :delay (or delay 0.0)
                  :order order
                  :error-policy error-policy
                  :on-item on-item
                  :on-done on-done
                  :results results
                  :exhausted-p empty-p)))
    (puthash overall-future handle sprite-fleet--handle-by-future)
    (cond
     (empty-p
      (setf (sprite-fleet-handle-state handle) :completed)
      (sprite-future--settle overall-future :resolved nil)
      (when on-done (funcall on-done nil)))
     ((null target-names)
      (setf (sprite-fleet-handle-state handle) :completed)
      (sprite-future--settle overall-future :rejected "No available sprites"))
     (t
      (sprite-fleet--schedule-step handle)))
    handle))

;;;###autoload
(cl-defun sprite-fleet-map (fn-or-form items
                            &key fleet-size sprites (delay 0.0)
                                 (order 'preserve) (error-policy :collect-nil)
                                 on-item on-done wait timeout (async t))
  "Evaluate FN-OR-FORM over ITEMS across a fleet of sprites.
By default returns a `sprite-future' that resolves to the ordered results.
The future's controller is accessible via `sprite-future-fleet-handle'.

If WAIT is non-nil (or ASYNC is nil), blocks until complete and returns
the results."
  (let* ((handle (sprite-fleet-pool fn-or-form items
                                    :fleet-size fleet-size
                                    :sprites sprites
                                    :delay delay
                                    :order order
                                    :error-policy error-policy
                                    :on-item on-item
                                    :on-done on-done))
         (future (sprite-fleet-handle-future handle)))
    (if (or wait (not async))
        (sprite-fleet-wait future timeout)
      future)))

;;;###autoload
(cl-defun sprite-fleet-each (fn-or-form items
                             &key fleet-size sprites (delay 0.0)
                                  (error-policy :collect-nil)
                                  on-item on-done)
  "Stream evaluation of FN-OR-FORM over ITEMS across a fleet of sprites.
Returns the controller `sprite-fleet-handle' immediately for flow control
(`sprite-fleet-pause', `sprite-fleet-resume', `sprite-fleet-cancel').
Results are streamed to ON-ITEM as each worker completes."
  (sprite-fleet-pool fn-or-form items
                     :fleet-size fleet-size
                     :sprites sprites
                     :delay delay
                     :order 'unordered
                     :error-policy error-policy
                     :on-item on-item
                     :on-done on-done))

;;;###autoload
(cl-defun sprite-fleet-mapcar (fn-or-form items
                               &key fleet-size sprites timeout async (delay 0.0)
                                    (order 'preserve) (error-policy :collect-nil)
                                    on-item on-done)
  "Evaluate FN-OR-FORM for each item in ITEMS using a fleet of sprites.
Mirrors `mapcar' across the daemon fleet.

Preserves backward compatibility: defaults to synchronous execution
blocking until all results settle unless ASYNC is non-nil.
Delegates to `sprite-fleet-pool' and the yielding stepper engine."
  (let* ((handle (sprite-fleet-pool fn-or-form items
                                    :fleet-size fleet-size
                                    :sprites sprites
                                    :delay delay
                                    :order order
                                    :error-policy error-policy
                                    :on-item on-item
                                    :on-done on-done))
         (future (sprite-fleet-handle-future handle)))
    (if async
        future
      (sprite-fleet-wait future timeout))))

;;; Legacy Sprites & Let Combinators

(cl-defun sprite-fleet-mapcar-sprites (form sprites &key timeout)
  "Evaluate FORM in each of SPRITES concurrently; return results in order.
Mirrors `mapcar': one result per input, order preserved.  A sprite that
times out or rejects contributes nil at its position.  SPRITES is a list
of `sprite' structs, e.g. from `sprite-resolve-list'."
  (let ((futures (seq-map (lambda (s) (sprite-future-eval (sprite--target-name s) form))
                          sprites)))
    (seq-map (lambda (f) (sprite-future-wait f :timeout timeout)) futures)))

(defun sprite-fleet-mapc (form sprites)
  "Evaluate FORM in each of SPRITES concurrently; discard results.
Mirrors `mapc': dispatches to every sprite and returns SPRITES immediately
without waiting for any of them to settle.  SPRITES is a list of `sprite'
structs, e.g. from `sprite-resolve-list'."
  (seq-do (lambda (s) (sprite-future-eval (sprite--target-name s) form)) sprites)
  sprites)

(cl-defmacro sprite-fleet-let (bindings &rest body)
  "Like `let', but each binding's value comes from a concurrent sprite eval.
Each element of BINDINGS is (VAR SPRITE FORM): SPRITE is an expression
evaluated to a sprite full-name string, FORM is unquoted and evaluated
remotely (quoted automatically, as with `with-sprite').  All FORMs are
dispatched in parallel; BODY runs once every binding has settled, with
VAR bound to its resolved value."
  (declare (indent 1))
  (let ((gfutures (make-symbol "futures")))
    `(let* ((,gfutures
             (list ,@(seq-map (lambda (b)
                                 `(sprite-future-eval ,(nth 1 b) ',(nth 2 b)))
                               bindings)))
            ,@(seq-map-indexed
               (lambda (b i) `(,(nth 0 b) (sprite-future-wait (nth ,i ,gfutures))))
               bindings))
       ,@body)))

(provide 'sprite-fleet)
;;; sprite-fleet.el ends here
