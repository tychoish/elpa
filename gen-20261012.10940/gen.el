;;; gen.el --- Generator iterators as lazy seq.el sequences -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: extensions, lisp, sequences, generators
;; URL: https://github.com/tychoish/gen.el

;;; Commentary:
;; Provides seq.el sequence operations over generator.el iterators
;; (iter-lambda, iter-defun, iter-make).
;;
;; Raw generator.el iterators are plain closures and cannot be dispatched on
;; directly by cl-generic.  gen-wrap wraps a generator iterator in
;; a gen struct.  Once wrapped, standard seq.el operations
;; (seq-map, seq-filter, seq-take, etc.) produce further lazy generators
;; that pull from the source on demand, while draining operations (seq-length,
;; seq-reverse, seq-reduce, seq-into, etc.) consume elements as needed.

;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'seq)

;;; Core struct & wrapper

;;;###autoload
(cl-defstruct (gen
               (:constructor gen--make)
               (:copier nil))
  "A wrapped `generator.el' iterator providing lazy `seq.el' operations."
  (iter nil :documentation "The raw generator.el closure iterator.")
  (peeked nil :documentation "Cons (VALUE . t) when a value was peeked, else nil.")
  (exhausted nil :documentation "Non-nil when iterator reached `iter-end-of-sequence'.")
  (infinite nil :documentation "Non-nil when generator is known to be infinite."))

;;;###autoload
(defun gen-wrap (iter-or-gen &optional infinite)
  "Wrap ITER-OR-GEN in a `gen' struct for `seq.el' compatibility.
If ITER-OR-GEN is already a `gen', return it unchanged (updating
its infinite flag if INFINITE is non-nil).
If ITER-OR-GEN is a raw `generator.el' iterator closure or function, wrap it.
When INFINITE is non-nil, marks the generator as infinite so draining operations
can issue a warning before attempting full consumption.

Note on infinite generators: lazy operations (`seq-map', `seq-filter',
`seq-take', `seq-take-while', `seq-drop', `seq-drop-while', etc.) are safe
on infinite generators.  Draining operations (`seq-length', `seq-reverse',
`seq-sort', `seq-into', etc.) will consume forever unless bounded."
  (cond
   ((gen-p iter-or-gen)
    (when infinite
      (setf (gen-infinite iter-or-gen) t))
    iter-or-gen)
   ((functionp iter-or-gen)
    (gen--make :iter iter-or-gen :infinite infinite))
   (t
    (signal 'wrong-type-argument (list 'functionp iter-or-gen)))))

;;; Internal iteration helpers

(defun gen--check-infinite (g)
  "Warn if G is marked as infinite before a draining operation."
  (when (and (gen-p g) (gen-infinite g))
    (display-warning 'gen
                     (format "Draining infinite generator %S; this operation may not terminate." g)
                     :warning)))

(defun gen--next (g)
  "Fetch the next value from G as a cons (VALUE . t), or nil if exhausted.
Checks the peek buffer first, then the raw generator iterator."
  (unless (gen-exhausted g)
    (cond
     ((gen-peeked g)
      (let ((peek (gen-peeked g)))
        (setf (gen-peeked g) nil)
        peek))
     (t
      (condition-case nil
          (cons (iter-next (gen-iter g)) t)
        (iter-end-of-sequence
         (setf (gen-exhausted g) t)
         nil))))))

(defun gen--peek (g)
  "Peek at the next value in G without consuming it.
Returns (VALUE . t), or nil if exhausted."
  (unless (gen-exhausted g)
    (cond
     ((gen-peeked g))
     (t
      (when-let* ((next (gen--next g)))
        (setf (gen-peeked g) next))))))

(defun gen--drain (g)
  "Consume all remaining values from G and return them as a list.
Calling this exhausts G."
  (gen--check-infinite g)
  (let (acc v)
    (while (setq v (gen--next g))
      (push (car v) acc))
    (nreverse acc)))

(defun gen--normalize (sequence)
  "Normalize SEQUENCE into a `gen' instance."
  (cond
   ((gen-p sequence) sequence)
   ((functionp sequence) (gen-wrap sequence))
   ((listp sequence)
    (gen-wrap
     (iter-make
      (dolist (item sequence)
        (iter-yield item)))))
   ((seqp sequence)
    (let ((len (seq-length sequence)))
      (gen-wrap
       (iter-make
        (dotimes (i len)
          (iter-yield (seq-elt sequence i)))))))
   (t
    (signal 'wrong-type-argument (list 'seqp sequence)))))

;;; seq.el protocol implementation

(cl-defmethod seqp ((_sequence gen))
  "Return t, declaring that `gen' is a `seq.el' sequence."
  t)

;;; Draining / eager methods

(cl-defmethod seq-length ((sequence gen))
  "Return the number of remaining elements in SEQUENCE.
Consumes and exhausts SEQUENCE."
  (gen--check-infinite sequence)
  (let ((count 0))
    (while (gen--next sequence)
      (setq count (1+ count)))
    count))

(cl-defmethod seq-count (predicate (sequence gen))
  "Count remaining elements in SEQUENCE for which PREDICATE is non-nil.
Consumes and exhausts SEQUENCE."
  (gen--check-infinite sequence)
  (let ((count 0)
        v)
    (while (setq v (gen--next sequence))
      (when (funcall predicate (car v))
        (setq count (1+ count))))
    count))

(cl-defmethod seq-reduce (function (sequence gen) initial-value)
  "Reduce FUNCTION across remaining elements of SEQUENCE with INITIAL-VALUE.
Consumes and exhausts SEQUENCE as a direct fold."
  (gen--check-infinite sequence)
  (let ((acc initial-value)
        v)
    (while (setq v (gen--next sequence))
      (setq acc (funcall function acc (car v))))
    acc))

(cl-defmethod seq-reverse ((sequence gen))
  "Return a list of remaining elements in SEQUENCE in reverse order.
Consumes and exhausts SEQUENCE."
  (gen--check-infinite sequence)
  (let (acc v)
    (while (setq v (gen--next sequence))
      (push (car v) acc))
    acc))

(cl-defmethod seq-sort (predicate (sequence gen))
  "Return a sorted list of remaining SEQUENCE elements per PREDICATE.
Consumes and exhausts SEQUENCE."
  (seq-sort predicate (gen--drain sequence)))

(cl-defmethod seq-elt ((sequence gen) n)
  "Return the Nth element of SEQUENCE (0-indexed).
Consumes up to N+1 elements from SEQUENCE."
  (unless (and (integerp n) (>= n 0))
    (error "Index out of range: %s" n))
  (let ((i 0)
        v found)
    (while (and (not found) (setq v (gen--next sequence)))
      (cond
       ((= i n) (setq found v))
       (t (setq i (1+ i)))))
    (cond
     (found (car found))
     (t (error "Index out of range: %s" n)))))

(cl-defmethod seq-subseq ((sequence gen) start &optional end)
  "Return a list of elements of SEQUENCE from index START up to END.
Consumes elements up to END (or the whole sequence if END is nil or negative)."
  (seq-subseq (gen--drain sequence) start end))

(cl-defmethod seq-into ((sequence gen) type)
  "Convert remaining elements of SEQUENCE into TYPE.
If TYPE is `gen', returns SEQUENCE.
Otherwise drains SEQUENCE and converts to TYPE."
  (cond
   ((eq type 'gen)
    sequence)
   (t
    (seq-into (gen--drain sequence) type))))

(cl-defmethod seq-into-sequence ((sequence gen))
  "Convert SEQUENCE to a sequence.  Returns SEQUENCE unchanged."
  sequence)

(cl-defmethod seq-copy ((sequence gen))
  "Return a copy of SEQUENCE.
Note: Since generator iterators are stateful closures, `seq-copy' returns
a new `gen' wrapping the remaining elements drained into a fresh iterator."
  (let ((remaining (gen--drain sequence)))
    (gen-wrap
     (iter-make
      (dolist (item remaining)
        (iter-yield item))))))

;;; Lazy / composable methods

(cl-defmethod seq-empty-p ((sequence gen))
  "Return non-nil if SEQUENCE has no remaining elements.
Uses lookahead peek without consuming the next element."
  (null (gen--peek sequence)))

(cl-defmethod seq-do (function (sequence gen))
  "Apply FUNCTION to each remaining element of SEQUENCE for side effects.
Consumes and exhausts SEQUENCE, returning SEQUENCE."
  (let (v)
    (while (setq v (gen--next sequence))
      (funcall function (car v))))
  sequence)

(cl-defmethod seq-map (function (sequence gen))
  "Return a new lazy `gen' applying FUNCTION to each element of SEQUENCE.
Does not consume elements from SEQUENCE until returned generator is consumed."
  (gen-wrap
   (iter-make
    (let (v)
      (while (setq v (gen--next sequence))
        (iter-yield (funcall function (car v))))))))

(cl-defmethod seq-filter (predicate (sequence gen))
  "Return a new lazy `gen' yielding elements for which PREDICATE is non-nil."
  (gen-wrap
   (iter-make
    (let (v)
      (while (setq v (gen--next sequence))
        (when (funcall predicate (car v))
          (iter-yield (car v))))))))

(cl-defmethod seq-remove (predicate (sequence gen))
  "Return a new lazy `gen' yielding elements for which PREDICATE returns nil."
  (gen-wrap
   (iter-make
    (let (v)
      (while (setq v (gen--next sequence))
        (unless (funcall predicate (car v))
          (iter-yield (car v))))))))

(cl-defmethod seq-take ((sequence gen) n)
  "Return a new lazy `gen' yielding up to N elements from SEQUENCE."
  (gen-wrap
   (iter-make
    (let ((count (max 0 n))
          (i 0)
          v)
      (while (and (< i count) (setq v (gen--next sequence)))
        (setq i (1+ i))
        (iter-yield (car v)))))))

(cl-defmethod seq-take-while (predicate (sequence gen))
  "Return a new lazy `gen' yielding from SEQUENCE while PREDICATE is non-nil."
  (gen-wrap
   (iter-make
    (let (v done)
      (while (and (not done) (setq v (gen--next sequence)))
        (if (funcall predicate (car v))
            (iter-yield (car v))
          (setq done t)))))))

(cl-defmethod seq-drop ((sequence gen) n)
  "Return a new lazy `gen' yielding elements after skipping first N elements."
  (gen-wrap
   (iter-make
    (let ((count (max 0 n))
          (i 0))
      (while (and (< i count) (gen--next sequence))
        (setq i (1+ i)))
      (let (v)
        (while (setq v (gen--next sequence))
          (iter-yield (car v))))))))

(cl-defmethod seq-drop-while (predicate (sequence gen))
  "Return a new lazy `gen' after dropping elements satisfying PREDICATE."
  (gen-wrap
   (iter-make
    (let (v found)
      (while (and (not found) (setq v (gen--next sequence)))
        (unless (funcall predicate (car v))
          (setq found t)
          (iter-yield (car v))))
      (when found
        (while (setq v (gen--next sequence))
          (iter-yield (car v))))))))

(cl-defmethod seq-some (predicate (sequence gen))
  "Return first non-nil result of applying PREDICATE to element of SEQUENCE.
Short-circuits: stops pulling from SEQUENCE as soon as a match is found."
  (let (res v)
    (while (and (null res) (setq v (gen--next sequence)))
      (let ((r (funcall predicate (car v))))
        (when r
          (setq res r))))
    res))

(cl-defmethod seq-every-p (predicate (sequence gen))
  "Return non-nil if PREDICATE returns non-nil for all elements in SEQUENCE.
Short-circuits: stops pulling from SEQUENCE as soon as non-match is found."
  (let ((all-true t)
        v)
    (while (and all-true (setq v (gen--next sequence)))
      (unless (funcall predicate (car v))
        (setq all-true nil)))
    all-true))

(cl-defmethod seq-find (predicate (sequence gen) &optional default)
  "Return first element of SEQUENCE for which PREDICATE is non-nil, or DEFAULT.
Short-circuits: stops pulling from SEQUENCE as soon as a match is found."
  (let (found v)
    (while (and (not found) (setq v (gen--next sequence)))
      (when (funcall predicate (car v))
        (setq found v)))
    (if found
        (car found)
      default)))

;;; Asynchronous stream consumption

;;;###autoload
(cl-defun gen-run-async (fn sequence &key (concurrency 1) (delay 0.0) on-done on-error)
  "Consume SEQUENCE asynchronously using FN with bounded CONCURRENCY.

FN is called with two arguments: (ITEM DONE-CALLBACK).  When FN finishes
processing ITEM, it must invoke DONE-CALLBACK with zero or more
arguments to signal completion and release its slot.

SEQUENCE is normalized to a `gen' instance if not already one.

CONCURRENCY limits the number of concurrent asynchronous operations in
flight; defaults to 1.

DELAY is the number of seconds to wait before dispatching the next item;
defaults to 0.0 (yielding cooperatively to the Emacs event loop).

ON-DONE, if non-nil, is a function called with no arguments once all
items have been processed and all active operations have finished.

ON-ERROR, if non-nil, is called with an error descriptor when FN signals
an error synchronously.  If ON-ERROR is nil, a warning is logged.

Returns a runner handle function `(lambda (&optional action) ...)`.

Calling it with no arguments or `:cancel` aborts pending scheduled
dispatches and prevents further items from being consumed.

Passing `:pause` halts dequeueing new items while letting in-flight
workers finish.

Passing `:resume` resumes dequeueing.

Passing `:paused-p`, `:canceled-p`, `:finished-p`, or `:active-count`
queries runner state."
  (let* ((g (gen--normalize sequence))
         (active 0)
         (exhausted nil)
         (canceled nil)
         (paused nil)
         (finished nil)
         (timer nil)
         (concurrency (max 1 (or concurrency 1)))
         (delay (max 0.0 (or delay 0.0))))
    (cl-labels
        ((finish ()
           (unless (or finished canceled)
             (setq finished t)
             (when timer
               (cancel-timer timer)
               (setq timer nil))
             (when on-done
               (funcall on-done))))
         (schedule-drain ()
           (when (and (not timer) (not canceled) (not paused) (not finished))
             (setq timer (run-with-timer delay nil #'drain))))
         (release-slot ()
           (setq active (1- active))
           (cond
            ((and exhausted (= active 0))
             (finish))
            ((and (not exhausted) (not paused))
             (schedule-drain))))
         (drain ()
           (setq timer nil)
           (unless (or canceled paused finished)
             (cond
              ((and (< active concurrency) (not exhausted))
               (let ((v (gen--next g)))
                 (cond
                  ((null v)
                   (setq exhausted t)
                   (when (= active 0)
                     (finish)))
                  (t
                   (setq active (1+ active))
                   (when (and (< active concurrency) (not exhausted) (not paused))
                     (schedule-drain))
                   (let ((done-called nil))
                     (condition-case err
                         (funcall fn (car v)
                                  (lambda (&rest _args)
                                    (unless (or done-called canceled)
                                      (setq done-called t)
                                      (release-slot))))
                       (error
                        (unless done-called
                          (setq done-called t)
                          (release-slot))
                        (if on-error
                            (funcall on-error err)
                          (display-warning
                           'gen
                           (format "Error in `gen-run-async' worker: %S" err)
                           :warning)))))))))
              ((and exhausted (= active 0))
               (finish))))))
      (if (zerop delay)
          (while (and (< active concurrency) (not exhausted) (not canceled) (not paused) (not finished))
            (drain))
        (schedule-drain))
      (lambda (&optional action)
        (pcase (or action :cancel)
          (:cancel
           (unless (or canceled finished)
             (setq canceled t)
             (when timer
               (cancel-timer timer)
               (setq timer nil))
             t))
          (:pause
           (unless (or canceled paused finished)
             (setq paused t)
             (when timer
               (cancel-timer timer)
               (setq timer nil))
             t))
          (:resume
           (when (and paused (not canceled) (not finished))
             (setq paused nil)
             (schedule-drain)
             t))
          (:paused-p paused)
          (:canceled-p canceled)
          (:finished-p finished)
          (:active-count active)
          (_ (error "Unknown handle action: %S" action)))))))

;;;###autoload
(cl-defun gen-pool-map (fn sequence &key (concurrency 1) (delay 0.0) (order 'preserve) on-done on-error)
  "Map FN asynchronously over SEQUENCE, accumulating results for ON-DONE.
FN is called with two arguments: (ITEM DONE-CALLBACK).  FN signals completion
by calling DONE-CALLBACK with the result value for that item:
  (funcall DONE RESULT).

CONCURRENCY limits the number of concurrent asynchronous operations in flight;
defaults to 1.
DELAY is the number of seconds to wait before dispatching the next item;
defaults to 0.0 (yielding cooperatively to the Emacs event loop).
ORDER specifies how results are ordered in ON-DONE:
  - `preserve' (or t): Preserves input sequence order (the default).
  - `fifo': Chronological completion order (first finished is first).
  - `lifo': Reverse chronological completion order (last finished is first).
  - `unordered' (or nil): Unspecified / cheapest order (zero reordering cost).
ON-DONE is required and called with a single argument, the list of results.
ON-ERROR, if non-nil, is called with an error descriptor when FN signals an
error synchronously.  If ON-ERROR is nil, a warning is logged.

Returns a cancellation function of zero arguments that aborts execution."
  (unless on-done
    (error "`gen-pool-map' requires an :on-done callback"))
  (unless (memq order '(preserve fifo lifo unordered t nil))
    (error "Invalid :order %S; expected `preserve', `fifo', `lifo', or `unordered'" order))
  (let* ((preserve-p (memq order '(preserve t)))
         (results (when preserve-p (make-hash-table :test 'eql)))
         (total 0)
         (stream
          (if preserve-p
              (let ((g (gen--normalize sequence)))
                (gen-wrap
                 (iter-make
                  (let (v)
                    (while (setq v (gen--next g))
                      (let ((idx total))
                        (setq total (1+ total))
                        (iter-yield (cons idx (car v)))))))))
            sequence))
         (worker
          (if preserve-p
              (lambda (pair done)
                (let ((idx (car pair))
                      (item (cdr pair)))
                  (funcall fn item
                           (lambda (&optional result)
                             (puthash idx result results)
                             (funcall done)))))
            (lambda (item done)
              (funcall fn item
                       (lambda (&optional result)
                         (push result results)
                         (funcall done))))))
         (finalize
          (cond
           (preserve-p
            (lambda ()
              (let (acc)
                (dotimes (i total)
                  (push (gethash (- (1- total) i) results) acc))
                acc)))
           ((eq order 'fifo)
            (lambda () (nreverse results)))
           (t
            (lambda () results)))))
    (gen-run-async
     worker
     stream
     :concurrency concurrency
     :delay delay
     :on-error on-error
     :on-done (lambda ()
                (funcall on-done (funcall finalize))))))

(provide 'gen)
;;; gen.el ends here
