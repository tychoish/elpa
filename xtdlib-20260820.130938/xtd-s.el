;;; xtd-s.el --- Extensions and additions for s.el -*- lexical-binding: t -*-

;; Author: sam kleinman (tychoish)
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4") (s "1.12"))
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

;; String helpers built on top of `s.el'.  Kept free of any dependency
;; on `dash', the `f' or `ht' extension modules so it can be loaded on
;; its own.

;;; Code:

(require 's)
(require 'seq)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; strings -- helper functions for handling strings

(defalias 's-equal #'string-equal)
(defalias 's-empty-p #'string-empty-p)
(defalias 's-plural-for #'resolve-plural-form)

(defun resolve-plural-form (quantity singular plural)
  (cond
   ((listp quantity) (resolve-plural-form (length quantity) singular plural))
   ((stringp quantity) (resolve-plural-form (string-to-number quantity) singular plural))
   ((= -1 quantity) singular)
   ((= 1 quantity) singular)
   (t plural)))

(defun s-or-char-equal (char value)
  "Return t when CHAR equals VALUE, where VALUE may be a character or single-char string."
  (cl-check-type char  character         "char must be a character")
  (cl-check-type value (or string character) "value must be a string or character")
  (if (stringp value)
      (string-equal (char-to-string char) value)
    (char-equal char value)))

(eval-and-compile
  (defconst xtdlib--join-char-names
    '((?- "hyphen" "kebab")
      (?_ "underscore" "snake")
      (?. "period" "dot")
      (?\s "space" "spc")
      (?= "equal" "equal")
      (?+ "plus" "plus")
      (?| "pipe" "pipe")
      (?> "gt" "gt"))
    "Alist of (CHAR canonical-name jargon-name) for `s-define-join-string-function'."))

(cl-defmacro s-define-join-string-function (char &optional &key use-jargon-names space-padding)
  "Define a function `s-join-with-NAME' that joins non-empty strings with CHAR as separator.
NAME is derived from CHAR via `xtdlib--join-char-names', falling back to the Unicode character
name. When USE-JARGON-NAMES is non-nil, the alternate jargon name is used (e.g. kebab vs
hyphen). When SPACE-PADDING is non-nil, the separator is surrounded by spaces."
  (cl-check-type char character "must create join function using the character to join the strings")
  (let* ((entry (assoc char xtdlib--join-char-names))
         (name (downcase (if entry
                             (if use-jargon-names (nth 2 entry) (nth 1 entry))
                           (mapconcat #'identity (split-string (char-to-name char) " ") "-"))))
	 (op-name (concat "s-join-with-" name))
	 (padding (if space-padding " " ""))
	 (join-with (concat padding (char-to-string char) padding)))
    `(defun ,(intern op-name) (&rest words)
       ,(format "Joins a variadic sequence of strings with `%s' (%s). When the kwarg `:space-padding',  the join char is padded with space characters on both ends before joining." char name)
       (mapconcat #'identity
		  (thread-last
		    words
		    (seq-filter #'stringp)
		    (seq-map #'string-trim)
		    (seq-remove #'string-empty-p))
		  ,join-with))))

(s-define-join-string-function ?-)
(s-define-join-string-function ? )
(s-define-join-string-function ?_)
(s-define-join-string-function ?- :use-jargon-names t)
(s-define-join-string-function ?  :use-jargon-names t)
(s-define-join-string-function ?_ :use-jargon-names t)
(s-define-join-string-function ?| :space-padding t)

(defun s-shortest (a b)
  "Return the shorter of strings A and B. When equal in length, return A."
  (if (>= (length b) (length a))
      a
    b))

(defun s-collapse-hyphens (str)
  "Replace runs of three or more consecutive hyphens in STR with a single hyphen."
  (replace-regexp-in-string "-\\{3,\\}" "-" str))

(defconst xtdlib--symbol-name-punctuation '("=" "_" " " "'" "\"" "\\" "/")
  "Characters `s-normalize-symbol-name' replaces with a hyphen.")

(defun s-normalize-symbol-name (name)
  "Normalize NAME to a clean hyphen-separated string suitable for use as a symbol name.
Trims outer whitespace, collapses internal whitespace, replaces common punctuation
with hyphens, and collapses runs of three or more hyphens."
  (let* ((sanatized (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " name)))
	 (canonicalized (s-replace-all (seq-map (lambda (char) (cons char "-")) xtdlib--symbol-name-punctuation) sanatized)))
    (s-collapse-hyphens canonicalized)))

(defun s-trimmed-or-nil (value)
  "Return VALUE trimmed of surrounding whitespace, or nil if VALUE is not a string or is empty after trimming."
  (and (stringp value)
       (unless (string-empty-p (setq value (string-trim value))) value)
       value))

(defun s-trim-non-word-chars (value)
  "Trim leading and trailing non-word characters from VALUE.
Return nil if VALUE is not a string or is empty after trimming."
  (and (stringp value)
       (unless (string-empty-p (setq value (string-trim value "\\W+" "\\W+"))) value)
       value))

(defun s-blank-p (value)
  "Return t when VALUE is a string that is empty or consists entirely of whitespace.
Returns nil for non-strings, non-empty strings, and strings with non-whitespace content.
Use this to guard against blank user input or empty configuration values."
  (and (stringp value)
       (string-empty-p (string-trim value))))

(defalias 's-contains-whitespace-p #'s-blank-p)

(defun s-default (default input)
  "Return DEFAULT when INPUT is nil or an empty string, otherwise return INPUT."
  (if (or (null input) (string-equal input ""))
      default
    input))

(eval-and-compile
  (defun s-number-word (num)
    "Return the English word for integer NUM. Supports values 1 through 20."
    (cond
     ((eql num 1) "one")
     ((eql num 2) "two")
     ((eql num 3) "three")
     ((eql num 4) "four")
     ((eql num 5) "five")
     ((eql num 6) "six")
     ((eql num 7) "seven")
     ((eql num 8) "eight")
     ((eql num 9) "nine")
     ((eql num 10) "ten")
     ((eql num 11) "eleven")
     ((eql num 12) "twelve")
     ((eql num 13) "thirteen")
     ((eql num 14) "fourteen")
     ((eql num 15) "fifteen")
     ((eql num 16) "sixteen")
     ((eql num 17) "seventeen")
     ((eql num 18) "eighteen")
     ((eql num 19) "nineteen")
     ((eql num 20) "twenty")
     (:else (user-error "no string form for %d" num)))))

(provide 'xtd-s)
;;; xtd-s.el ends here
