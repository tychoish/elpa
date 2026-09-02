;;; sprite-future.el --- Backend-agnostic futures and async/await for sprite -*- lexical-binding: t; -*-

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
;; A backend-agnostic future/promise layer over sprite eval calls: the same
;; `sprite-future' struct and API regardless of whether the evaluation used
;; the `emacsclient' subprocess transport or the `direct' socket transport
;; (see `sprite-communication-backend' in sprite.el).  Built on top of
;; `sprite-direct-promise' for the `direct' backend, and a `make-process'
;; sentinel for the `emacsclient' backend.
;;
;; Also provides a generator-based async/await layer: `sprite-future-async-defun'
;; bodies read as straight-line code while `sprite-future-await' suspends without
;; blocking the parent Emacs event loop.
;;
;; Entry points:
;;   `sprite-future-eval'       — start an async eval; returns a future
;;   `sprite-future-then'       — chain a callback; returns a new future
;;   `sprite-future-wait'       — block until a future settles
;;   `sprite-future-pending-p'  — predicate: not yet settled
;;   `sprite-future-resolved-p' — predicate: settled successfully
;;   `sprite-future-rejected-p' — predicate: settled with failure
;;   `sprite-future-async-defun' — define a generator-based async workflow
;;   `sprite-future-await'       — suspend a `sprite-future-async-defun' body on a future

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'generator)
(require 'sprite)
(require 'sprite-direct)

;;;; Struct

(cl-defstruct (sprite-future (:constructor sprite-future--make) (:copier nil))
  "Backend-agnostic handle for an in-flight sprite eval."
  target          ; sprite full-name
  state           ; :pending :resolved :rejected
  value
  callbacks       ; list of (on-resolve . on-reject), applied in order on settle
  backend-handle) ; sprite-direct-promise, or a process for the emacsclient path

;;;; Predicates

(defun sprite-future-pending-p (future)
  "Return t when FUTURE has not yet resolved or been rejected."
  (eq :pending (sprite-future-state future)))

(defun sprite-future-resolved-p (future)
  "Return t when FUTURE resolved successfully."
  (eq :resolved (sprite-future-state future)))

(defun sprite-future-rejected-p (future)
  "Return t when FUTURE failed.
For the `direct' backend this delegates to `sprite-direct-promise-rejected-p'
via the promise sentinel; for `emacsclient' it reflects a non-zero exit
code from the `emacsclient' process."
  (eq :rejected (sprite-future-state future)))

;;;; Settling and callback chaining

(defun sprite-future--run-callback (future cb)
  "Invoke callback pair CB, (ON-RESOLVE . ON-REJECT), according to FUTURE's state."
  (cond
   ((and (sprite-future-resolved-p future) (car cb))
    (funcall (car cb) (sprite-future-value future)))
   ((and (sprite-future-rejected-p future) (cdr cb))
    (funcall (cdr cb) (sprite-future-value future)))))

(defun sprite-future--settle (future state value)
  "Transition FUTURE to STATE with VALUE, then run its pending callbacks.
No-op if FUTURE has already settled."
  (when (sprite-future-pending-p future)
    (setf (sprite-future-state future) state
          (sprite-future-value future) value)
    (let ((callbacks (sprite-future-callbacks future)))
      (setf (sprite-future-callbacks future) nil)
      (seq-do (lambda (cb) (sprite-future--run-callback future cb)) callbacks))))

(defun sprite-future--fulfill-chain (chained fn value default-state)
  "Settle CHAINED from applying FN to VALUE, or pass VALUE through unchanged.
When FN is non-nil, its return value resolves CHAINED; an error signaled by
FN rejects CHAINED with the error as the value.  When FN is nil, CHAINED
settles directly to DEFAULT-STATE/VALUE, passing the source future's outcome
through unchanged."
  (if fn
      (condition-case err
          (sprite-future--settle chained :resolved (funcall fn value))
        (error (sprite-future--settle chained :rejected err)))
    (sprite-future--settle chained default-state value)))

(defun sprite-future-then (future on-resolve &optional on-reject)
  "Chain ON-RESOLVE (and optionally ON-REJECT) onto FUTURE.
Returns a new `sprite-future' that settles once FUTURE does: ON-RESOLVE
runs (and its return value resolves the new future) if FUTURE resolves;
ON-REJECT runs if FUTURE rejects.  Whichever callback is omitted passes
FUTURE's own state/value through unchanged.  If FUTURE has already
settled, the callback runs immediately and the new future settles
synchronously; otherwise it runs later, when FUTURE settles."
  (let ((chained (sprite-future--make :target (sprite-future-target future)
                                       :state :pending :callbacks nil)))
    (cond
     ((sprite-future-pending-p future)
      (setf (sprite-future-callbacks future)
            (append (sprite-future-callbacks future)
                    (list (cons (lambda (v) (sprite-future--fulfill-chain chained on-resolve v :resolved))
                                (lambda (v) (sprite-future--fulfill-chain chained on-reject v :rejected)))))))
     ((sprite-future-resolved-p future)
      (sprite-future--fulfill-chain chained on-resolve (sprite-future-value future) :resolved))
     (t
      (sprite-future--fulfill-chain chained on-reject (sprite-future-value future) :rejected)))
    chained))

(defun sprite-future--handle-process (future)
  "Return the underlying process driving FUTURE's settlement, or nil.
Handles both backend shapes: a raw process (`emacsclient' backend) or a
`sprite-direct-promise' (`direct' backend, whose own process is nested)."
  (let ((handle (sprite-future-backend-handle future)))
    (cond
     ((processp handle) handle)
     ((sprite-direct-promise-p handle) (sprite-direct-promise-proc handle))
     (t nil))))

(cl-defun sprite-future-wait (future &key timeout)
  "Block until FUTURE settles; return its value or nil on timeout/rejection.
Drives process output while waiting, mirroring `sprite-direct-promise-wait'.
TIMEOUT overrides `sprite-direct-blocking-timeout'; nil waits indefinitely."
  (let* ((proc (sprite-future--handle-process future))
         (limit (or timeout sprite-direct-blocking-timeout))
         (deadline (when limit (+ (float-time) limit))))
    (while (and (sprite-future-pending-p future)
                (or (null deadline) (< (float-time) deadline))
                (or (null proc) (process-live-p proc)))
      (accept-process-output proc sprite-direct-async-check-interval nil t))
    (sprite-future-value future)))

;;;; Backend: direct socket

(defun sprite-future--eval-direct (future form)
  "Start FUTURE's evaluation of FORM via the `direct' socket backend.
Opens a connection to FUTURE's target, dispatches a non-blocking
`sprite-direct' eval, and arranges for `sprite-direct-promise-then' to
settle FUTURE once the underlying promise does.  Rejects FUTURE
immediately if the connection itself cannot be opened -- e.g. a TCP
target with no discoverable auth key (see `sprite--direct-target')."
  (let* ((conn (condition-case nil
                   (sprite-direct-open (sprite--direct-target (sprite-future-target future)))
                 (error nil)))
         (promise (when conn (sprite-direct-eval-non-blocking conn form))))
    (setf (sprite-future-backend-handle future) promise)
    (if promise
        (sprite-direct-promise-then
         promise
         (lambda (state value) (sprite-future--settle future state value)))
      (sprite-future--settle future :rejected nil))))

;;;; Backend: emacsclient subprocess

(defun sprite-future--settle-from-process (future proc)
  "Settle FUTURE from PROC once its `emacsclient' invocation has exited.
Exit code 0 resolves FUTURE with PROC's buffer contents read as a Lisp
value (nil if unreadable, mirroring `sprite--call-and-read-emacsclient');
any other exit code rejects FUTURE.  Kills PROC's buffer afterward."
  (let ((buf (process-buffer proc)))
    (if (= 0 (process-exit-status proc))
        (sprite-future--settle
         future :resolved
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (condition-case nil (read (buffer-string)) (error nil)))))
      (sprite-future--settle future :rejected nil))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun sprite-future--spawn-emacsclient (target form future)
  "Start an async emacsclient eval of FORM against TARGET, settling FUTURE."
  (let ((proc (make-process
               :name (format "sprite-future-%s" target)
               :buffer (generate-new-buffer " *sprite-future*")
               :command (list "emacsclient" "--socket-name" target
                              "--eval" (format "%S" form))
               :sentinel (lambda (proc _event)
                           (unless (process-live-p proc)
                             (sprite-future--settle-from-process future proc))))))
    (setf (sprite-future-backend-handle future) proc)))

;;;; Entry point

(defun sprite-future-eval (target form)
  "Evaluate FORM in sprite TARGET asynchronously; return a `sprite-future'.
Dispatches on `sprite-communication-backend', the same way
`sprite--call-and-read' does for the synchronous API."
  (let ((future (sprite-future--make :target target :state :pending :callbacks nil)))
    (pcase sprite-communication-backend
      ('direct (sprite-future--eval-direct future form))
      (_ (sprite-future--spawn-emacsclient target form future)))
    future))

;;;; Async/await
;;
;; `generator.el's `iter-next' has no mechanism to inject a signal into a
;; suspended generator -- it only accepts (ITERATOR &optional YIELD-RESULT),
;; where YIELD-RESULT becomes `iter-yield's return value, never a thrown
;; condition.  So a rejected awaited future is resumed as an ordinary value:
;; a `sprite-future--rejection' wrapper, which `sprite-future-await' unwraps and
;; re-signals as `sprite-future-rejected' at the suspension point, letting
;; workflow bodies catch it with a plain `condition-case'.

(define-error 'sprite-future-rejected "Awaited sprite-future rejected")

(cl-defstruct (sprite-future--rejection (:constructor sprite-future--rejection-make) (:copier nil))
  "Sentinel resumed into a generator when an awaited future rejects.
See `sprite-future-await', which unwraps this and re-signals its VALUE."
  value)

(cl-defmacro sprite-future-async-defun (name arglist &rest body)
  "Define NAME as an async sprite workflow.
Inside BODY, `sprite-future-await' suspends the generator until a future settles,
without blocking the parent Emacs event loop.  Calling NAME returns
immediately with a `sprite-future' for the workflow's overall result."
  (declare (indent defun) (doc-string 3))
  (let ((doc (when (stringp (car body)) (pop body)))
        (interactive-form (when (eq 'interactive (car-safe (car body))) (pop body))))
    `(defun ,name ,arglist
       ,@(when doc (list doc))
       ,@(when interactive-form (list interactive-form))
       (sprite-future--async-run (iter-make ,@body)))))

(defmacro sprite-future-await (future)
  "Inside a `sprite-future-async-defun' body, suspend until FUTURE settles.
Resumes with FUTURE's resolved value when it resolves.  When FUTURE
rejects, signals `sprite-future-rejected' with the rejection value at
this point, so a `condition-case' wrapping `sprite-future-await' can catch it."
  (let ((gresult (make-symbol "result")))
    `(let ((,gresult (iter-yield ,future)))
       (if (sprite-future--rejection-p ,gresult)
           (signal 'sprite-future-rejected (list (sprite-future--rejection-value ,gresult)))
         ,gresult))))

(defun sprite-future--async-step (gen driver-future yielded)
  "Advance GEN, resuming with YIELDED's settled outcome; drive DRIVER-FUTURE.
YIELDED is the `sprite-future' most recently produced by an `iter-yield' in
GEN, or nil to start the generator for the first time.  Steps GEN once it
is available (immediately if YIELDED is already settled, otherwise via
`sprite-future-then'), resuming with the resolved value or, on rejection,
a `sprite-future--rejection' wrapper for `sprite-future-await' to re-signal."
  (if (null yielded)
      (sprite-future--async-drive gen driver-future)
    (sprite-future-then
     yielded
     (lambda (value) (sprite-future--async-drive gen driver-future value))
     (lambda (err) (sprite-future--async-drive gen driver-future (sprite-future--rejection-make :value err))))))

(defun sprite-future--async-drive (gen driver-future &optional value)
  "Resume GEN with VALUE; drive DRIVER-FUTURE to its next step or settlement.
On `iter-end-of-sequence' (GEN returned normally), resolves DRIVER-FUTURE
with the returned value.  On any other error -- including an unhandled
`sprite-future-rejected' from `sprite-future-await' -- rejects DRIVER-FUTURE."
  (condition-case resume-error
      (let ((yielded (iter-next gen value)))
        (sprite-future--async-step gen driver-future yielded))
    (iter-end-of-sequence
     (sprite-future--settle driver-future :resolved (cdr resume-error)))
    (error
     (sprite-future--settle driver-future :rejected resume-error))))

(defun sprite-future--async-run (gen)
  "Drive generator GEN to completion; return a `sprite-future' for its result.
Steps GEN immediately; each time it yields a `sprite-future', resumes it
via `sprite-future-then' once that future settles rather than polling.
The returned future resolves when GEN returns, or rejects if an error
propagates out of GEN uncaught."
  (let ((driver-future (sprite-future--make :target nil :state :pending :callbacks nil)))
    (sprite-future--async-step gen driver-future nil)
    driver-future))

(provide 'sprite-future)
;;; sprite-future.el ends here
