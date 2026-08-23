;;; xtd-project.el --- Lightweight project-context wrappers -*- lexical-binding: t -*-

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

;; Lightweight wrappers over `projectile' and `project.el' (both
;; optional, soft dependencies checked via `featurep' at call time --
;; neither is `require'd here) plus mode-derived buffer filtering.
;; Depends on `xtd-s' for `s-trimmed-or-nil'/`s-trim-non-word-chars'.

;;; Code:

(require 'seq)
(require 'xtd-s)

(eval-when-compile
  (require 'cl-lib))

(declare-function projectile-project-root "projectile")
(declare-function projectile-project-name "projectile")
(declare-function projectile-project-buffers "projectile")

(declare-function project-root "project")
(declare-function project-current "project")
(declare-function project-buffers "project")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; project context -- lightweight wrappers over projectile / project.el

;;;###autoload
(defun buffer-derived-mode-p (buffer mode)
  "Returns t when the major-mode of `buffer' is or is derived from `mode.'"
  (with-current-buffer buffer
    (when (derived-mode-p mode)
      t)))

;;;###autoload
(cl-defun mode-buffers-for-project (&optional &key (mode major-mode))
  "Return buffers in the current project whose major mode derives from MODE.
MODE defaults to `major-mode' of the calling buffer. Uses `approximate-project-buffers'
to determine project membership."
  (seq-filter (lambda (it) (buffer-derived-mode-p it mode)) (approximate-project-buffers)))

;;;###autoload
(cl-defun mode-buffers (&optional (mode major-mode))
  "Return all live buffers whose major mode derives from MODE.
MODE defaults to `major-mode' of the calling buffer. Searches across all buffers,
not just the current project."
  (seq-keep (lambda (it)
	      (with-current-buffer it
		(when (derived-mode-p mode)
		  (current-buffer))))
	    (buffer-list)))


;;;###autoload
(defun approximate-project-root ()
  "Return the current project root, falling back to `default-directory'."
  (or (when (featurep 'projectile)
        (s-trimmed-or-nil (projectile-project-root)))
      (when (and (featurep 'project) (project-current))
        (project-root (project-current)))
      (expand-file-name default-directory)))

;;;###autoload
(defun approximate-project-name ()
  "Return the current project name, falling back to the directory basename."
  (s-trim-non-word-chars
   (or (when (featurep 'projectile)
         (projectile-project-name))
       (when (project-current)
         (file-name-nondirectory
          (directory-file-name (project-root (project-current)))))
       (file-name-nondirectory (directory-file-name (expand-file-name default-directory))))))

;;;###autoload
(defun approximate-project-buffers ()
  "Return buffers belonging to the current project."
  (or (when (featurep 'projectile)
        (projectile-project-buffers))
      (when (and (featurep 'project) (project-current))
        (project-buffers (project-current)))
      (let ((directory (expand-file-name default-directory)))
        (seq-filter
	 (lambda (it)
	   (with-current-buffer it
	     (file-in-directory-p default-directory directory)))
	 (buffer-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; misc

(defun compile-buffer-name (name)
  "Return a function suitable for `compilation-buffer-name-function' that always returns NAME.
The returned function accepts one optional argument (the compilation mode) and ignores it."
  (lambda (&optional _) name))

(provide 'xtd-project)
;;; xtd-project.el ends here
