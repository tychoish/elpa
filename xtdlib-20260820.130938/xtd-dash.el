;;; xtd-dash.el --- Extensions and additions for dash.el -*- lexical-binding: t -*-

;; Author: sam kleinman (tychoish)
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4"))
;; Keywords: utility, library, elisp.
;; URL: https://github.com/tychoish/xtdlib.el

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; List helpers in the style of `dash.el', implemented on top of
;; `seq.el', `map.el', and `subr-x' (all built in) rather than an
;; actual dependency on the `dash' package. Has no dependency on
;; `xtd-s', `xtd-f', `xtd-ht', `xtd-macro', or the main `xtdlib' file
;; -- and nothing here requires any of them back, so this is a leaf
;; module.

;;; Code:

(require 'seq)
(require 'map)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lists -- helpers, mostly a-la dash.el

(defun larger (&optional first second)
  "Return the larger value. If either FIRST or SECOND are not numbers, treat them as 0."
  (if (> (setq first (if (numberp first) first 0))
	 (setq second (if (numberp second) second 0)))
      first
    second))

(defun smaller (&optional first second)
  "Return the smaller value. If either FIRST or SECOND are not numbers, treat them as 0."
  (if (< (setq first (if (numberp first) first 0))
	 (setq second (if (numberp second) second 0)))
      first
    second))

(cl-defmacro make-add-to-list-fn (list &optional &key append)
  "Return a lambda that appends ITEM to LIST via `add-to-list'.
LIST may be a bare symbol or a quoted symbol."
  (let ((sym (if (and (consp list) (eq (car list) 'quote))
                 (cadr list)
               list)))
    `(lambda (item) (add-to-list ',sym item ,append))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `dash.el' -- extensions and additions

(cl-defun -distinct-by-car (cell &optional &key (test #'equal))
  "Take a list of cons cells and return a new list that contains only the
elements that have unique car values, ignoring the cdr entirely. Compare
values using the test function, which defaults to `equal'."
  (seq-uniq cell (lambda (a b) (funcall test (car a) (car b)))))

(cl-defun -distinct-by-alist-key (key cell &optional &key (test #'equal))
  "Compare a list of alists, and return a new list that contains only the
alists that have distinct values for a specific key. Compare values using the
test function, which defaults to `equal'."
  (seq-uniq cell (lambda (a b) (funcall test (alist-get key a) (alist-get key b)))))

(defun -unwind (list)
  "Flatten LIST by exactly one level, collecting nested list items into a single list.
Unlike `-flatten', which recurses fully, `-unwind' only removes one layer of nesting."
  (apply #'append list))

(defalias '-flat-map #'mapcan
  "`-flat-map' applies (maps) a function, which returns a list, to each
item in a list. The lists that result from the map operation are then
concatenated or joined. This provides a dash.el conforming API for the
`mapcan' operation.")

(defalias '-mapc #'mapc)
(defalias '-join #'nconc)
(defalias '-append #'append)
(defalias '-reverse #'nreverse)

(defun -sparse (list)
  "Return LIST with all nil elements removed."
  (seq-remove #'null list))

(defalias '-c #'cons)
(defalias '-l #'list)

(defun -strings (&rest input)
  "Return INPUT as a flat list, optionally coercing non-string elements.
A trailing `:options OPT' pair in INPUT selects how non-strings are handled:
`filter' drops them, `stringify' formats them with `format'. OPT may be a
list combining these symbols. When `:options' is not provided or its value
is nil, INPUT is returned as-is without coercion. A `user-error' is signaled
only when OPT is some other, unrecognized value."
  (let* ((idx (cl-position :options input))
	 (options (and idx (nth (1+ idx) input)))
	 (input (if idx
		    (append (cl-subseq input 0 idx)
			    (cl-subseq input (+ idx 2)))
		  input)))
    (cond
     ((null options) input)
     ((option-set-p 'filter options)
      (seq-filter #'stringp input))
     ((option-set-p 'stringify options)
      (seq-map (lambda (it) (if (stringp it) it (format "%s" it))) input))
     (t (user-error "invalid `:options' value for `-strings': %S" options)))))

(defun -sparse-append (&rest items)
  "Append ITEMS and remove all nil values from the concatenated result."
  (-sparse (apply #'append items)))

(defun -map-in-place (mapper items)
  "Apply the `mapper' function to every item in the list `items' and
replace the items in the original list with the results of the function,
returning the list. This is a destructive operation."
  (let ((output items)
	(head items))
    (while head
      (setf (car head) (funcall mapper (car head)))
      (setq head (cdr head)))
    output))

(defun -in-place (mapper items)
  "Destructively apply MAPPER to every item in ITEMS, replacing each element in place.
Returns the count of elements processed. Use this when you need the element count rather
than the list itself; prefer `-map-in-place' when you need the modified list back."
  (let ((head items)
	(count 0))
    (while head
      (setf (car head) (funcall mapper (car head)))
      (setq head (cdr head))
      (cl-incf count))
    count))

(cl-defun -map-uniq (mapper input &optional &key (test #'equal))
  "Apply the `mapper' function to every item in the `input' list, returning
a new list that contains the unique output of the list. Comparisons use
the `test' function, which defaults to `equal'."
  (let ((head input)
	(seen (make-hash-table :test test))
	current
	output)
    (while head
      (unless (map-contains-key seen (setq current (funcall mapper (car head))))
	(setf (map-elt seen current) current)
	(push current output))
      (setq head (cdr head)))
    output))

(defmacro --mapc (form input-list)
  "Apply the form (with the current element avalible as the variable `it')
to all item in the list, primarily for side effects. Returns the input
list. This is an anaphoric equivalent to `mapc'. As opposed to `--each'
and `-each', which return nil, `-mapc' returns the input list.

This is the anaphoric counterpart to `-mapc'."
  (declare (indent defun) (debug (def-form form)))
  `(mapc (lambda (it) (ignore it) ,form) ,input-list))

(defmacro --flat-map (form input-list)
  "`--flat-map' evaluates a form for very item in `input-list' with the
item bound to `it'. The form must return a list, and the returned lists
are then concatenated or joined into a single flattened list. This
provides a dash.el conforming API for the `mapcan' operation.

This is the anaphoric counterpart to `-flat-map'."
  (declare (indent defun) (debug (def-form form)))
  `(mapcan (lambda (it) (ignore it) ,form) ,input-list))

(defmacro --map-in-place (form items)
  "Apply the form, (with the current element as `it') to each item in the
list, distructively setting the return value of the form to the value in
the list.

This is the anaphoric counterpart to `-map-in-place'."
  (declare (indent defun) (debug (def-form form)))
  `(-map-in-place (lambda (it) (ignore it) ,form) ,items))

(defmacro --in-place (form items)
  "Take a list and replace each element in the list with the result of
evaluating `FORM' for that element. The original element is accessable
in the `FORM' as `it`. Returns the number of items in the list. This is
a destructive operation.

This is the anaphoric counterpart to `-in-place'."
  (declare (indent defun) (debug (def-form form)))
  `(-in-place (lambda (it) (ignore it) ,form) ,items))

(defmacro --map-uniq (form input)
  "Apply `FORM' to every item in the input list, available as `it', and
collect the UNIQUE results, and returning them as a list.
Items are compared for uniqueness with `equal'.

This is the anaphoric counterpart to `-map-uniq'."
  (declare (indent defun) (debug (def-form form)))
  `(-map-uniq (lambda (it) (ignore it) ,form) ,input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; options -- tiny dependency-free predicate so lower-level modules
;; such as `xtd-f' can use it without requiring `xtdlib' itself and
;; creating a require cycle.

(defun option-set-p (opt options)
  "Return non-nil if OPT is present in OPTIONS.
OPTIONS may be a single symbol or a list of symbols."
  (or (eq opt options)
      (and (listp options) (memq opt options))))

(defun -filter-s-trim (strs)
  "Return STRS with non-strings removed and remaining strings trimmed of whitespace.
Empty strings and strings that are entirely whitespace are excluded from the result."
  (cl-check-type strs list "strs must be list")
  (thread-last
    strs
    (seq-filter #'stringp)
    (seq-map #'string-trim)
    (seq-remove #'string-empty-p)))

(provide 'xtd-dash)
;;; xtd-dash.el ends here
