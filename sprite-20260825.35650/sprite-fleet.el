;;; sprite-fleet.el --- Fleet-level combinators over sprite futures -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (seq "2.24"))
;; Keywords: tools, daemon, processes

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Higher-order combinators dispatching to multiple sprites concurrently,
;; built entirely on the future API in sprite-future.el.  All three are
;; dispatch-then-collect: every future launches in parallel first, *then*
;; the combinator waits, so wall-clock cost is the slowest single call,
;; not the sum of every call.
;;
;; Entry points:
;;   `sprite-mapcar' — collect, ordered (mirrors `mapcar')
;;   `sprite-mapc'   — fire-and-forget (mirrors `mapc')
;;   `sprite-let'    — parallel destructuring bind (mirrors `let')

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'sprite)
(require 'sprite-future)

(cl-defun sprite-mapcar (form sprites &key timeout)
  "Evaluate FORM in each of SPRITES concurrently; return results in order.
Mirrors `mapcar': one result per input, order preserved.  A sprite that
times out or rejects contributes nil at its position.  SPRITES is a list
of `sprite' structs, e.g. from `sprite-resolve-list'."
  (let ((futures (seq-map (lambda (s) (sprite-future-eval (sprite-name s) form))
                           sprites)))
    (seq-map (lambda (f) (sprite-future-wait f :timeout timeout)) futures)))

(defun sprite-mapc (form sprites)
  "Evaluate FORM in each of SPRITES concurrently; discard results.
Mirrors `mapc': dispatches to every sprite and returns SPRITES immediately
without waiting for any of them to settle.  SPRITES is a list of `sprite'
structs, e.g. from `sprite-resolve-list'."
  (seq-do (lambda (s) (sprite-future-eval (sprite-name s) form)) sprites)
  sprites)

(cl-defmacro sprite-let (bindings &rest body)
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
