;;; sprite-async.el --- Daemon-side token/result registry for async eval -*- lexical-binding: t; -*-

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
;; This module runs inside the Emacs daemon that receives eval requests
;; from `sprite-direct' clients (the Python/Go/Rust/JS libraries under
;; python/, go/, rust/, js/).  It introduces no new wire-protocol message
;; type: both entry points below are invoked exactly like any other form
;; a client sends via the existing `-eval' command.
;;
;; A client "starts" an async operation by sending an ordinary eval
;; request whose form is a call to `sprite-async-start', and "polls" it
;; by sending an ordinary eval request calling `sprite-async-poll' with
;; the token that `sprite-async-start' returned.
;;
;; The reason this exists at all: a client-side future (in the
;; per-language libraries) needs to be able to recover its eventual
;; result even if the connection or client process that started it
;; drops and reconnects later.  Since every eval -- including a poll --
;; is a fresh, stateless socket round trip with no persistent connection
;; object, there is nothing to hang a "the client is still there"
;; heartbeat off of.  Instead the token, not any connection, is the
;; operation's identity: the registry entry lives independently of
;; whichever socket happens to ask about it, and it stays alive only as
;; long as something keeps polling it (see the sweep mechanism below).
;;
;; Entry points:
;;   `sprite-async-start' — schedule FORM's evaluation; returns a token
;;   `sprite-async-poll'  — look up a token's current state by TOKEN

;;; Code:

(require 'cl-lib)

;;;; Customization

(defcustom sprite-async-default-ttl-seconds 300
  "Default idle-timeout, in seconds, for a `sprite-async' registry entry.
An entry is swept once this many seconds elapse since it was last
touched by `sprite-async-poll' (or created, if never polled).  Can be
overridden per-call via the TTL-SECONDS argument to `sprite-async-start'."
  :type 'integer
  :group 'sprite)

;;;; Registry

(cl-defstruct (sprite-async--entry (:constructor sprite-async--entry-make) (:copier nil))
  "A single in-flight or settled `sprite-async' operation."
  token
  state         ; :pending :resolved :rejected
  value
  ttl-seconds
  last-accessed) ; time value, as returned by `current-time'

(defvar sprite-async--registry (make-hash-table :test 'equal)
  "Hash table mapping token strings to `sprite-async--entry' structs.")

(defvar sprite-async--sweep-timer nil
  "The repeating timer that sweeps expired `sprite-async--registry' entries.
See `sprite-async--start-sweep-timer'.")

(defconst sprite-async--sweep-interval-seconds 45
  "How often, in seconds, `sprite-async--sweep-timer' runs.")

(defun sprite-async--make-token ()
  "Return a fresh, unique token string."
  (format "sprite-async-%d-%d-%s"
          (float-time)
          (random most-positive-fixnum)
          (gensym "")))

(defun sprite-async--touch (entry)
  "Update ENTRY's last-accessed timestamp to now."
  (setf (sprite-async--entry-last-accessed entry) (current-time)))

(defun sprite-async--entry-expired-p (entry now)
  "Return non-nil when ENTRY has been idle longer than its own ttl, as of NOW."
  (> (float-time (time-subtract now (sprite-async--entry-last-accessed entry)))
     (sprite-async--entry-ttl-seconds entry)))

(defun sprite-async--sweep ()
  "Remove any registry entry idle longer than its own ttl-seconds."
  (let ((now (current-time)))
    (maphash (lambda (token entry)
               (when (sprite-async--entry-expired-p entry now)
                 (remhash token sprite-async--registry)))
             sprite-async--registry)))

(defun sprite-async--start-sweep-timer ()
  "Start the repeating sweep timer, cancelling any prior one first.
Guards against duplicate sweep timers accumulating when this file is
loaded more than once."
  (when (timerp sprite-async--sweep-timer)
    (cancel-timer sprite-async--sweep-timer))
  (setq sprite-async--sweep-timer
        (run-with-timer sprite-async--sweep-interval-seconds
                         sprite-async--sweep-interval-seconds
                         #'sprite-async--sweep)))

(sprite-async--start-sweep-timer)

;;;; Entry points

(defun sprite-async--run (token form)
  "Evaluate FORM and settle TOKEN's registry entry with the outcome.
Any error signaled while evaluating FORM settles the entry as
`:rejected' with a human-readable string instead of propagating out of
the timer callback -- an error escaping a timer callback would just be
printed to `*Messages*', not surfaced anywhere a client could see it."
  (let ((entry (gethash token sprite-async--registry)))
    (when entry
      (condition-case err
          (let ((value (eval form t)))
            (setf (sprite-async--entry-state entry) :resolved
                  (sprite-async--entry-value entry) value))
        (error
         (setf (sprite-async--entry-state entry) :rejected
               (sprite-async--entry-value entry) (error-message-string err))))
      (sprite-async--touch entry))))

(defun sprite-async-start (form &optional ttl-seconds)
  "Register and schedule evaluation of FORM; return a fresh token string.
FORM is an already-quoted Elisp form (e.g. `(+ 1 2)').  Evaluation is
scheduled via a zero-delay timer so this call itself returns almost
immediately, matching the pattern `sprite-direct' already uses when
replying to an ordinary `-eval' request.

TTL-SECONDS, if given, overrides `sprite-async-default-ttl-seconds' as
this entry's idle-timeout: the entry is swept once this many seconds
elapse since it was last touched by `sprite-async-poll'.

Poll the returned token with `sprite-async-poll' to observe the
eventual outcome."
  (let* ((token (sprite-async--make-token))
         (entry (sprite-async--entry-make
                 :token token
                 :state :pending
                 :value nil
                 :ttl-seconds (or ttl-seconds sprite-async-default-ttl-seconds)
                 :last-accessed (current-time))))
    (puthash token entry sprite-async--registry)
    (run-with-timer 0 nil #'sprite-async--run token form)
    token))

(defun sprite-async-poll (token)
  "Return the current state of the operation registered under TOKEN.
One of:
  (:pending)          -- still running
  (:resolved VALUE)   -- completed successfully, VALUE is FORM's result
  (:rejected ERROR)   -- completed with an error, ERROR is a string
  (:unknown)          -- TOKEN was never registered, or has expired

Every poll of a known TOKEN updates that entry's last-accessed
timestamp, which is what keeps it alive against the sweep in
`sprite-async--sweep' -- a client that keeps polling keeps its own
entry alive, with no separate heartbeat or connection-tracking
machinery required."
  (let ((entry (gethash token sprite-async--registry)))
    (if (null entry)
        (list :unknown)
      (sprite-async--touch entry)
      (pcase (sprite-async--entry-state entry)
        (:pending (list :pending))
        (:resolved (list :resolved (sprite-async--entry-value entry)))
        (:rejected (list :rejected (sprite-async--entry-value entry)))))))

(provide 'sprite-async)
;;; sprite-async.el ends here
