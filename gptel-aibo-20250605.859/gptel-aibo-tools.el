;;; gptel-aibo-tools.el --- gptel-aibo llm tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Sun Yi Ming

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

;; gptel-aibo llm tools

;;; Code:

(require 'gptel)

(gptel-make-tool
 :name "list_directory"
 :function
 (lambda (path &optional recursive)
   (let ((full-path (expand-file-name path (gptel-aibo-tools--project-root))))
     (if (not (file-directory-p full-path))
         (format "Error: %s is not a valid directory" full-path)
       (let* ((files (if recursive
                         (directory-files-recursively full-path ".*" t)
                       (directory-files full-path t "^[^.].*")))
              (simplified
               (mapcar (lambda (file)
                         (if (file-directory-p file)
                             (concat file "/")
                           (format "%s %d"
                                   file
                                   (file-attribute-size (file-attributes file)))))
                       files)))
         (mapconcat #'identity simplified "\n")))))
 :description "List the contents of a directory. Folders end with '/', files include size in bytes. Optional recursive listing."
 :args (list '(:name "path"
               :type string
               :description "The path of the directory to list")
             '(:name "recursive"
               :type boolean
               :optional t
               :description "Whether to list contents recursively"))
 :category "filesystem")

(gptel-make-tool
 :name "read_file"
 :function
 (lambda (filepath)
   (let ((full-path (expand-file-name filepath (gptel-aibo-tools--project-root))))
     (cond
      ((not (file-exists-p full-path))
       (format "Error: File does not exist: %s" full-path))
      ((not (file-readable-p full-path))
       (format "Error: File is not readable: %s" full-path))
      (t
       (with-temp-buffer
         (insert-file-contents full-path)
         (buffer-string))))))
 :description "Read and return the contents of a file at the specified path"
 :args (list '(:name "filepath"
               :type string
               :description "The full path to the file to read"))
 :category "filesystem")

(defun gptel-aibo-tools--project-root ()
  "Get the root directory of current project.
Returns: The project root directory as a string, or nil if not found."
  (cond
   ((fboundp 'project-root)
    (project-root (project-current)))
   ((fboundp 'project-roots)
    (car (project-roots (project-current))))))

(provide 'gptel-aibo-tools)
;;; gptel-aibo-tools.el ends here
