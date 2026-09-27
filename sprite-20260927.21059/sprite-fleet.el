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
;; built entirely on the future API in sprite-future.el.  All three are
;; dispatch-then-collect: every future launches in parallel first, *then*
;; the combinator waits, so wall-clock cost is the slowest single call,
;; not the sum of every call.
;;
;; Entry points:
;;   `sprite-fleet-mapcar-sprites' — collect, ordered (mirrors `mapcar')
;;   `sprite-fleet-mapc'           — fire-and-forget (mirrors `mapc')
;;   `sprite-fleet-mapcar'         — map over items using a fleet of sprites
;;   `sprite-fleet-let'            — parallel destructuring bind (mirrors `let')

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'sprite)
(require 'sprite-future)

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

;;;###autoload
(cl-defun sprite-fleet-mapcar (fn-or-form items &key fleet-size sprites timeout async)
  "Evaluate FN-OR-FORM for each item in ITEMS using a fleet of sprites.
FN-OR-FORM can be a unary function/lambda symbol or a form to evaluate.
When FN-OR-FORM is a function symbol or lambda, each item is passed as an arg.
When FN-OR-FORM is a form containing `item' or `_', that form is evaluated
remotely with the item bound.

FLEET-SIZE is the maximum number of concurrent sprites to use (defaults to
`sprite-max-count' or the length of ITEMS).

SPRITES is an optional list of sprite structs or target name strings to use
as the worker fleet.  When omitted, sprites are obtained via
`sprite-get-or-create-fleet'.

TIMEOUT is passed to `sprite-future-wait' when operating synchronously.

If ASYNC is non-nil, returns a `sprite-future' immediately that resolves
to the ordered list of results when all items complete.  Otherwise, blocks
and returns the ordered list of results."
  (let* ((item-list (if (vectorp items) (append items nil) (copy-sequence items)))
         (num-items (length item-list))
         (overall-future (sprite-future--make :target "fleet" :state :pending)))
    (cond
     ((null item-list)
      (sprite-future--settle overall-future :resolved nil))
     (t
      (let* ((effective-size (or fleet-size (min num-items sprite-max-count)))
             (worker-sprites (or sprites (sprite-get-or-create-fleet effective-size)))
             (target-names (seq-map #'sprite--target-name worker-sprites)))
        (if (null target-names)
            (sprite-future--settle overall-future :rejected "No available sprites")
          (let ((results (make-vector num-items nil))
                (remaining (seq-map-indexed (lambda (item idx) (cons idx item)) item-list))
                (active-count 0))
            (cl-labels
                ((dispatch-next (target-name)
                   (if (null remaining)
                       (progn
                         (cl-decf active-count)
                         (when (<= active-count 0)
                           (sprite-future--settle overall-future :resolved (append results nil))))
                     (let* ((cell (pop remaining))
                            (idx (car cell))
                            (item (cdr cell))
                            (form (sprite-fleet--make-item-form fn-or-form item))
                            (future (sprite-future-eval target-name form))
                            (finish (lambda (val)
                                      (aset results idx val)
                                      (dispatch-next target-name))))
                       (sprite-future-then future finish (lambda (_) (funcall finish nil)))))))
              (let ((initial-workers (seq-take target-names (min num-items (length target-names)))))
                (setq active-count (length initial-workers))
                (dolist (target initial-workers)
                  (dispatch-next target)))))))))
    (if async
        overall-future
      (sprite-future-wait overall-future :timeout timeout))))


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
