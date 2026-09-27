;;; xtd-ht.el --- Extensions and additions for ht.el -*- lexical-binding: t -*-

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

;; Accessor-generating macros in the style of `ht.el', implemented on
;; top of `map.el' and built-in hash-table primitives rather than an
;; actual dependency on the `ht' package -- every real `ht.el' symbol
;; these macros used turned out to be a trivial alias/wrapper (`ht-get'
;; is `gethash', `ht-set' is `puthash', `ht-p' is `hash-table-p',
;; `ht-create' is `make-hash-table', and `ht-contains-p' reimplements
;; what `map-contains-key' already does generically). Has no dependency
;; on the `f' or `s' extension modules.

;;; Code:

(require 'map)

(eval-when-compile
  (require 'cl-lib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `ht.el'-style accessors, on top of `map.el'

(defmacro ht-get-lambda (table)
  "Return a lambda of one argument KEY that looks up KEY in TABLE."
  `(lambda (key) (map-elt ,table key)))

(defmacro ht-set-lambda (table)
  "Return a lambda of two arguments KEY and VALUE that sets KEY to VALUE in TABLE."
  `(lambda (key value) (setf (map-elt ,table key) value)))

(defmacro ht-contains-p-lambda (table)
  "Return a lambda of one argument KEY that tests whether KEY is present in TABLE."
  `(lambda (key) (map-contains-key ,table key)))

(defmacro ht-get-function (table)
  (let ((name (symbol-name table)))
    `(defun ,(intern (format "ht-%s-get" name)) (key)
       "Get the value for KEY from the named hash table."
       (map-elt ,table key))))

(defmacro ht-set-function (table)
  (let ((name (symbol-name table)))
    `(defun ,(intern (format "ht-%s-set" name)) (key value)
       "Set KEY to VALUE in the named hash table, returning VALUE."
       (setf (map-elt ,table key) value))))

(defmacro ht-contains-p-function (table)
  (let ((name (symbol-name table)))
    `(defun ,(intern (format "ht-%s-contains-p" name)) (key)
       "Return non-nil if KEY is present in the named hash table."
       (map-contains-key ,table key))))

(cl-defmacro ht-make-named-table (name &optional &key (test #'equal))
  "Define a named hash table variable and named accessor functions.
Creates `ht-NAME-get', `ht-NAME-set', and `ht-NAME-contains-p'. TEST defaults to `equal'."
  (let ((name (or (when (stringp name) (intern name))
		  (when (symbolp name) name)
		  (intern (format "%S" name))))
	(table (or (when (hash-table-p name) name)
		   (make-hash-table :test test))))
    `(progn
       (defvar ,name ,table
	 ,(format "Hash table `%s' with named accessor functions." name))
       (ht-get-function ,name)
       (ht-set-function ,name)
       (ht-contains-p-function ,name))))

(provide 'xtd-ht)
;;; xtd-ht.el ends here
