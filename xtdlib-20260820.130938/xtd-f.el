;;; xtd-f.el --- Extensions and additions for f.el -*- lexical-binding: t -*-

;; Author: sam kleinman (tychoish)
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4") (f "0.20"))
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

;; File and path helpers built on top of `f.el'.

;;; Code:

(require 'f)
(require 'seq)
(require 'xtd-s)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `f.el' -- extensions and additions

(defun f-mtime (filename)
  "Return the modification time of FILENAME as a time value."
  (file-attribute-modification-time (file-attributes filename)))

(defun f-atime (filename)
  "Return the access time of FILENAME as a time value."
  (file-attribute-access-time (file-attributes filename)))

(defun f-make-slug (s)
  "Turn a string, S, into a hyphen-seperated slug for filename."
  (downcase
   (replace-regexp-in-string
    "[^A-Za-z0-9]" "-"
    (string-clean-whitespace s))))

(defun f-filename-is-p (entry name)
  "Return non-nil if the filename component of path ENTRY equals NAME."
  (f-equal-p (file-name-nondirectory entry) name))

(defun f-when-file-exists (path)
  "Return PATH if it exists on the filesystem, otherwise nil."
  (when (file-exists-p path)
    path))

(defun f-distinct (sequence)
  "Return SEQUENCE with duplicate paths removed using `f-equal-p' for comparison.
Handles paths that differ only in trailing slashes or relative vs. absolute
form when the filesystem resolves them to the same location."
  (seq-uniq sequence #'f-equal-p))

(defalias 'f-distinct-paths #'f-distinct)

(defmacro f-directories-containing-file-with-extension-function (extension)
  "Define `f-EXT-file-p' and `f-directories-containing-file-with-extension-EXT' for EXTENSION."
  (when (string-prefix-p "." extension)
    (setq extension (string-trim-left extension "^\\.")))

  `(progn
     (defun ,(intern (format "f-%s-file-p" (string-replace "." "" (downcase extension)))) (file)
       (and (file-regular-p file) (string-equal (file-name-extension file) ,extension)))

     (defun ,(intern (format "f-directories-containing-file-with-extension-%s" (string-replace "." "" (downcase extension)))) (paths)
       (when (stringp paths)
	 (setq paths (list paths)))
       (thread-last
	 paths
	 (mapcan (lambda (it) (f-entries it #'file-regular-p)))
	 (seq-filter (lambda (it) (string-equal (file-name-extension it) ,extension)))
	 (seq-map #'f-dirname)
	 (seq-uniq)))))

(defun f-files-in-directory (path)
  "Return a flat list of all files under PATH.
PATH may be a directory, a file (returns siblings), or a list of paths."
  (cond
   ((stringp path)
    (cond
     ((file-directory-p path) (f-entries path #'file-regular-p))
     ((file-regular-p path) (f-entries (f-dirname path) #'file-regular-p))))
   ((listp path) (mapcan (lambda (it) (f-entries it #'file-regular-p)) path))))

(defun f-recursive-directories-containing (filename &optional path)
  "Return a list of directories under PATH that contain a file named FILENAME.
Searches recursively. PATH defaults to `default-directory' when nil."
  (thread-last
    (f-entries path (lambda (f) (f-filename-is-p f filename)) t)
    (seq-map #'f-dirname)))

(defmacro f-directories-containing-file-function (filename &rest files)
  "Define helper predicates and a search function for directories containing FILENAME.
Also accepts additional FILES as alternate names to match."
  (let* ((filenames (cons filename files))
	 (symbol-filename (string-replace "." "-" (downcase filename)))
	 (pred (intern (s-join-with-hyphen "f-directory-contains" symbol-filename "file"))))
    `(progn
       (defun ,(intern (format "f-filename-is-%s-p" symbol-filename)) (path)
	 (seq-some (lambda (candidate) (f-filename-is-p path candidate)) ',filenames))

       (defun ,pred (path)
	 (let ((matching (thread-last
			   (f-files-in-directory path)
			   (seq-filter (lambda (it) (f-equal-p ,filename (file-name-nondirectory it))))
			   (seq-uniq)
			   (seq-filter 'identity))))
	   (when matching
	     (car matching))))

       (defun ,(intern (s-join-with-hyphen "f-directories-containing-file" symbol-filename)) (path &rest paths)
	 (let ((filenames (list ,@filenames))
	       (paths (cons path paths)))
	   (thread-last
	     paths
	     (mapcan #'f-files-in-directory)
	     (seq-filter (lambda (it) (car (member (file-name-nondirectory it) filenames))))
	     (seq-map #'f-dirname)
	     (seq-uniq)))))))

(defun f-collapse-homedir (path)
  "Replace the expanded home directory prefix in PATH with `~/'."
  (string-replace (expand-file-name "~/") "~/" path))

(defun f-visually-compress-path (num path)
  "Truncate each component of PATH to at most NUM characters, preserving the separator."
  (mapconcat #'identity
	     (thread-last
	       (f-split path)
	       (seq-map (lambda (it) (if (length> it num)
				    (substring it 0 num)
				  it)))
	       (seq-map (lambda (it) (if (string-equal it (f-path-separator)) "" it))))
	     (f-path-separator)))

(defmacro f-visual-compression-function (num)
  "Define `f-visually-compress-to-NAME' that truncates each path component to NUM characters."
  `(defun ,(intern (concat "f-visually-compress-to-" (s-number-word num))) (path)
     (f-visually-compress-path ,num path)))

(f-visual-compression-function 1)
(f-visual-compression-function 2)
(f-visual-compression-function 3)
(f-visual-compression-function 4)
(f-visual-compression-function 5)
(f-visual-compression-function 6)
(f-visual-compression-function 7)
(f-visual-compression-function 8)
(f-visual-compression-function 9)
(f-visual-compression-function 10)

(provide 'xtd-f)
;;; xtd-f.el ends here
