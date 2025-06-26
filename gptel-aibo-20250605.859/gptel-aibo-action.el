;;; gptel-aibo-action.el --- Action for gptel-aibo -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Sun Yi Ming
;;
;; Author: Sun Yi Ming <dolmens@gmail.com>
;; Keywords: emacs tools editing gptel ai assistant code-completion productivity

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Actions parse and apply for gptel-aibo

;;; Code:

(require 'gptel-aibo-context)
(require 'text-property-search)
(require 'cl-lib)

(defvar gptel-aibo--delete-confirmation nil
  "Stores user confirmation preference for file deletion.")

(cl-defgeneric gptel-aibo-parse-action (_parser response)
  "Parse RESPONSE string into a list of operations.
Returns ops list on success, or (error . message) on failure.")

(defun gptel-aibo--is-in-project (working-buffer buffer-or-filename)
  "Determine if BUFFER-OR-FILENAME is in the same project as WORKING-BUFFER.

Returns t if:
1. BUFFER-OR-FILENAME is the same as WORKING-BUFFER.
2. BUFFER-OR-FILENAME is within the project root of WORKING-BUFFER.
Otherwise, returns nil."
  (or (and (bufferp buffer-or-filename)
           (eq working-buffer buffer-or-filename))
      (with-current-buffer working-buffer
        (when-let* ((project-root
                     (or gptel-aibo--working-project
                         (when-let ((project (project-current)))
                           (gptel-aibo--project-root project))))
                    (file-name
                     (if (bufferp buffer-or-filename)
                         (buffer-file-name buffer-or-filename)
                       buffer-or-filename)))
          (file-in-directory-p file-name project-root)))))

(cl-defgeneric gptel-aibo-execute (op &optional dry-run)
  "Execute an operation OP. If DRY-RUN is non-nil, simulate the operation.")

(cl-defstruct (gptel-aibo-op (:constructor gptel-aibo-make-op))
  "Base class for all gptel-aibo operations.")

(cl-defstruct (gptel-aibo-mod-op (:include gptel-aibo-op)
                                 (:constructor gptel-aibo-make-mod-op))
  "Represents a buffer modification operation."
  target
  replacements
  full-content)

(cl-defmethod gptel-aibo-execute ((op gptel-aibo-mod-op) &optional dry-run)
  "Execute a modification operation OP.
If DRY-RUN is non-nil, simulate the operation without making any changes."
  (let ((buffer-name (gptel-aibo-mod-op-target op))
        (replacements (gptel-aibo-mod-op-replacements op))
        (full-content (gptel-aibo-mod-op-full-content op)))
    (message "Applying MODIFY%s: %s" (if dry-run "[dry-run]" "") buffer-name)
    ;; We require LLM to use buffer name, but LLM doesn't always follow it.
    (let ((op-buffer (or (get-buffer buffer-name)
                         (get-file-buffer buffer-name))))
      (cond
       ((not op-buffer)
        (error "Buffer not found: %s" buffer-name))
       ((not (gptel-aibo--is-in-project (current-buffer) op-buffer))
        (error "Modifications outside the working project are not allowed: %s"
               buffer-name))
       (full-content
        (with-current-buffer op-buffer
          (unless dry-run
            (erase-buffer)
            (insert full-content)
            (when (buffer-file-name)
              (save-buffer)))))
       (t
        (with-current-buffer op-buffer
          (goto-char (point-min))
          (dolist (search-replace-pair replacements)
            (goto-char (point-min))
            (let ((search (car search-replace-pair))
                  (replace (cdr search-replace-pair)))
              (unless (search-forward search nil t)
                (error "Searching fail: [%s]" search))
              (unless dry-run
                (replace-match replace t t))))
          (when (buffer-file-name)
            (save-buffer))))))))

(cl-defstruct (gptel-aibo-creation-op (:include gptel-aibo-op)
                                      (:constructor gptel-aibo-make-creation-op))
  "Represents a file creation operation."
  filename
  content)

(cl-defmethod gptel-aibo-execute ((op gptel-aibo-creation-op) &optional dry-run)
  "Execute a creation operation OP.
If DRY-RUN is non-nil, simulate the operation without creating the file."
  (let ((filename (gptel-aibo-creation-op-filename op))
        (content (gptel-aibo-creation-op-content op)))
    (message "Applying CREATE%s: %s" (if dry-run "[dry-run]" "") filename)
    (unless (gptel-aibo--is-in-project (current-buffer) filename)
      (error "Creating file outside the working project is not allowed: %s"
             filename))
    (when (file-exists-p filename)
      (error "File already exists: %s" filename))
    (unless dry-run
      (with-current-buffer (create-file-buffer filename)
        (insert content)
        (set-visited-file-name filename)
        (save-buffer)))))

(cl-defstruct (gptel-aibo-del-op (:include gptel-aibo-op)
                                 (:constructor gptel-aibo-make-del-op))
  "Represents a file deletion operation."
  filename)

(cl-defmethod gptel-aibo-execute ((op gptel-aibo-del-op) &optional dry-run)
  "Execute a deletion operation OP.
If DRY-RUN is non-nil, simulate deletion without actually removing the file."
  (let ((filename (gptel-aibo-del-op-filename op)))
    (message "Applying DELETE%s: %s" (if dry-run "[dry-run]" "") filename)
    (unless (gptel-aibo--is-in-project (current-buffer) filename)
      (error "Deleting files outside the working project is not allowed: %s"
             filename))
    (unless (file-exists-p filename)
      (error "File not found: %s" filename))
    (unless dry-run
      (when-let ((file-buffer (get-file-buffer filename)))
        (kill-buffer file-buffer)))
    (unless dry-run
      (cond
       ((eq gptel-aibo--delete-confirmation 'never)
        (message "File deletion refused by user: %s" filename))
       ((eq gptel-aibo--delete-confirmation 'always)
        (delete-file filename))
       (t
        (let ((response
               (read-char-choice
                (format "Delete file %s? (y)es/(n)o/(a)lways/(N)ever: " filename)
                '(?y ?n ?a ?N))))
          (pcase response
            (?y (delete-file filename))
            (?n (message "File deletion refused by user: %s" filename))
            (?a (setq gptel-aibo--delete-confirmation 'always)
                (delete-file filename))
            (?N (setq gptel-aibo--delete-confirmation 'never)
                (message "File deletion refused by user: %s" filename)))))))))

(provide 'gptel-aibo-action)
;;; gptel-aibo-action.el ends here
