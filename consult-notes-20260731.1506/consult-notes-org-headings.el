;;; consult-notes-org-headings.el --- find org heading notes using consult -*- lexical-binding: t; coding: utf-8-emacs -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Colin McLear
;; Homepage: https://github.com/mclear-tools/consult-notes

;; Copyright (C) 2022 Colin McLear

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Manage notes by search org file headings as part of `consult-notes'.

;;; Code:
(require 'consult-notes)
(require 'org)

;;;; Variables
(defvar consult-notes-org-headings--history nil)

(defcustom consult-notes-org-headings-files nil
  "Files used by the `consult-notes' org-headings source.

The value may be:
- nil (the default), meaning use the current value of
  `org-agenda-files', resolved at call time;
- a list of files and/or directories;
- a single file name, naming a file that contains the list of
  files, as with the variable `org-agenda-files';
- a function of no arguments returning a list of files and/or
  directories."
  :group 'consult-notes
  :type '(choice (const :tag "Use org-agenda-files" nil)
                 (repeat :tag "List of files and directories" file)
                 (file :tag "File containing a list of files")
                 (function :tag "Function returning a file list")))

(define-obsolete-variable-alias 'consult-org-headings-narrow-key
  'consult-notes-org-headings-narrow-key "0.9")

(defcustom consult-notes-org-headings-narrow-key ?h
  "Key for narrowing using `consult-notes' function."
  :group 'consult-notes
  :type 'key)

;;;; Functions
;; The expansion logic is adapted from the function `org-agenda-files'.
(defun consult-notes-org-headings-files ()
  "Return the resolved list of org-headings files.

See the variable `consult-notes-org-headings-files' for the
possible values. When it is nil, delegate to the function
`org-agenda-files', which already expands directories and skips
unavailable files."
  (if (null consult-notes-org-headings-files)
      (org-agenda-files)
    (let ((files
	       (cond
	        ((functionp consult-notes-org-headings-files)
	         (funcall consult-notes-org-headings-files))
	        ((stringp consult-notes-org-headings-files)
	         (org-read-agenda-file-list))
	        ((listp consult-notes-org-headings-files)
	         consult-notes-org-headings-files)
	        (t (error "Invalid value of `consult-notes-org-headings-files'")))))
      (setq files (apply #'append
		                 (mapcar (lambda (f)
				                   (if (file-directory-p f)
				                       (directory-files
				                        f t org-agenda-file-regexp)
				                     (list (expand-file-name f org-directory))))
			                     files)))
      (when org-agenda-skip-unavailable-files
        (setq files (seq-filter #'file-readable-p files)))
      files)))

(defun consult-notes--org-headings (match scope &rest skip)
  "Return a list of Org heading candidates.

MATCH, SCOPE and SKIP are as in `org-map-entries'."
  (let (buffer)
    (apply
     #'org-map-entries
     (lambda ()
       (unless (eq buffer (buffer-name))
         (setq buffer (buffer-name)
               org-outline-path-cache nil))
       (pcase-let ((`(_ ,level ,todo ,prio ,_hl ,tags) (org-heading-components))
                   (cand (org-format-outline-path
                          (org-get-outline-path 'with-self 'use-cache))))
         (when tags
           (setq tags (concat " " (propertize tags 'face `(:height 0.8 :inherit org-tag)))))

         (setq cand (concat (propertize cand 'face 'consult-file)
                            tags (consult--tofu-encode (point))
                            (propertize " " 'display `(space :align-to center))

                            (format "%18s" (propertize (concat "@" buffer) 'face 'consult-notes-sep))))
         (add-text-properties 0 1
                              `(org-marker ,(point-marker)
                                           consult-org--heading (,level ,todo . ,prio))
                              cand)
         cand))
     match scope skip)))

(defun consult-notes-org-headings--mrkr (cand)
  "Return the org marker stored in CAND."
  (and cand (get-text-property 0 'org-marker cand)))

(defun consult-notes-org-headings--state ()
  "Org headings state function."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (lambda (action cand)
      (unless cand
        (funcall open))
      (funcall jump action (consult-notes-org-headings--mrkr cand)))))

;;;; Annotations
(defun consult-notes-org-headings-annotations (cand)
  "Annotate heading CAND with its file's size and modification time."
  (let* ((mrkr (get-text-property 0 'org-marker cand))
         (buf (and mrkr (marker-buffer mrkr)))
         (path (and buf (buffer-file-name buf)))
         (attrs (and path (file-attributes path))))
    (when attrs
      (let ((ftime (consult-notes--time (file-attribute-modification-time attrs)))
            (fsize (file-size-human-readable (or (file-attribute-size attrs) 0))))
        (put-text-property 0 (length fsize) 'face 'consult-notes-size fsize)
        (put-text-property 0 (length ftime) 'face 'consult-notes-time ftime)
        (format "%8s  %8s" fsize ftime)))))

;;;; Source
(defconst consult-notes-org-headings--source
  (list :name (propertize "Org Headings" 'face 'consult-notes-sep)
        :narrow consult-notes-org-headings-narrow-key
        :category 'org-heading
        :require-match t
        :items (lambda ()
                 (consult-notes--org-headings t (consult-notes-org-headings-files)))
        :state #'consult-notes-org-headings--state
        :annotate #'consult-notes-org-headings-annotations
        :history 'consult-notes-org-headings--history
        :lookup (lambda (selected &rest _) (get-text-property 0 'org-marker selected)))
  "Source for the `consult-notes' function.")

(provide 'consult-notes-org-headings)

;; Local Variables:
;; coding: utf-8-emacs
;; End:
;;; consult-notes-org-headings.el ends here
