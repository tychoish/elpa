;;; gptel-aibo-context.el --- Context for gptel-aibo -*- lexical-binding: t; -*-

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

;; Context handling functions for gptel-aibo

;;; Code:


(require 'imenu)

(defcustom gptel-aibo-max-buffer-size 16000
  "The maximum size of the buffer's content to include in the context.

If the working buffer's content exceeds this size, only a fragment of the
context around the cursor (e.g., a function or a class) will be sent.

For other buffers in the same project: if their size exceeds this limit and
they have an outline, only the outline will be sent; otherwise, their content
will be discarded."
  :type 'natnum
  :group 'gptel-aibo
  :safe #'natnump)

(defcustom gptel-aibo-max-buffer-count 2
  "The maximum number of buffers to include in the project context."
  :type 'natnum
  :group 'gptel-aibo
  :safe #'natnump)

(defcustom gptel-aibo-max-fragment-size 1024
  "Maximum size (in characters) for context fragments around cursor position."
  :type 'natnum
  :group 'gptel-aibo
  :safe #'natnump)

(defcustom gptel-aibo-max-fragment-expand 80
  "Maximum size (in characters) for context fragments expand line size."
  :type 'natnum
  :group 'gptel-aibo
  :safe #'natnump)

(defconst gptel-aibo--truncated-marker "<<< TRUNCATED >>>"
  "Marker indicating that preceding content has been omitted.")

(defconst gptel-aibo--omitted-marker "<<< REMAINING OMITTED >>>"
  "Marker indicating that subsequent content has been omitted.")

(defconst gptel-aibo--end-marker "<<< END OF CONTENT >>>"
  "Marker indicating the definitive end of the content.")

(defvar-local gptel-aibo--working-project nil
  "Current working project of `gptel-aibo'.")

(defvar-local gptel-aibo--working-buffer nil
  "Current working buffer of `gptel-aibo'.")

(defvar-local gptel-aibo--trigger-buffer nil
  "The buffer in which the `gptel-aibo' command was triggered.")

(defun gptel-aibo-context-info (&optional buffer)
  "Get context information for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (concat (gptel-aibo--working-buffer-info)
            "\n\n"
            (gptel-aibo--project-buffers-info))))

(defun gptel-aibo--working-buffer-info ()
  "Get context information about the current buffer."
  (let* ((active-buffer-size (- (point-max) (point-min))))
    (concat
     (format "Current working buffer: `%s`\n" (buffer-name))
     (if (<= active-buffer-size gptel-aibo-max-buffer-size)
         (concat
          (gptel-aibo--buffer-info)
          (gptel-aibo--cursor-position-info))
       (gptel-aibo--fragment-info)))))

(defun gptel-aibo--fragment-info (&optional buffer)
  "Get fragment information around cursor about BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((fragment-boundaries
            (gptel-aibo--max-fragment-boundaries gptel-aibo-max-buffer-size))
           (truncate-info
            (when (> (car fragment-boundaries) (point-min))
              (concat gptel-aibo--truncated-marker "\n")))
           (remain-info
            (if (< (cdr fragment-boundaries) (point-max))
                (concat gptel-aibo--omitted-marker "\n")
              (when (> (car fragment-boundaries) (point-min))
                (concat gptel-aibo--end-marker "\n"))))
           (language-identifier
            (gptel-aibo--mode-to-language-identifier major-mode)))
      (concat
       (gptel-aibo--buffer-filename-info)
       "Fragment around the cursor:\n"
       truncate-info
       (gptel-aibo--make-code-block
        (buffer-substring-no-properties
         (car fragment-boundaries) (cdr fragment-boundaries))
        language-identifier)
       "\n"
       remain-info
       (and remain-info "\n")
       (gptel-aibo--cursor-position-info-in-fragment
        (car fragment-boundaries) (cdr fragment-boundaries))))))

(defun gptel-aibo--buffer-info (&optional buffer)
  "Get buffer information including file path and content.

When BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((buffer-content
           (buffer-substring-no-properties (point-min) (point-max)))
          (language-identifier
           (gptel-aibo--mode-to-language-identifier major-mode)))
      (concat (gptel-aibo--buffer-filename-info)
              "Content:\n"
              (if buffer-content
                  (gptel-aibo--make-code-block
                   buffer-content
                   language-identifier)
                "(empty)")
              "\n"))))

(defun gptel-aibo--buffer-filename-info (&optional buffer)
  "Return the file path info associated with BUFFER.

BUFFER is the buffer to check, or the current buffer if nil."
  (format "Filepath: %s\n"
          (if-let ((file-name (buffer-file-name buffer)))
              (concat "`" file-name "`")
            "(not associated with a file)")))

(defun gptel-aibo-summon-context-info (&optional buffer)
  "Get context information for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (concat (gptel-aibo--summon-buffer-info)
            "\n\n"
            (gptel-aibo--project-buffers-info))))

(defun gptel-aibo--summon-buffer-info ()
  "Get context information about the current buffer for summon."
  (let ((active-buffer-size (- (point-max) (point-min))))
    (concat
     (format "Current working buffer: `%s`\n" (buffer-name))
     (cond
      ((zerop active-buffer-size)
       "Content: (empty)")
      ((<= active-buffer-size gptel-aibo-max-buffer-size)
       (gptel-aibo--cursoring-buffer-info))
      (t
       (gptel-aibo--cursoring-fragment-info))))))

(defvar gptel-aibo--cursor-notes
  "Note the marker `%s` serves only as a cursor position indicator. It is not
part of the actual content.")

(defun gptel-aibo--cursor-symbol (content)
  "Return the first unused cursor symbol from a predefined list.
CONTENT is the string to search for existing cursor symbols."
  (let ((candidates '("{{CURSOR}}" "<<CURSOR>>" "[[CURSOR]]"
                      "{{<CURSOR>}}")))
    (catch 'found
      (dolist (candidate candidates)
        (unless (string-match-p (regexp-quote candidate) content)
          (throw 'found candidate)))
      nil)))

(defun gptel-aibo--cursor-at-word-boundary-p ()
  "Return t if the cursor is at a word boundary."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (or (not bounds)
        (= (point) (car bounds))
        (= (point) (cdr bounds)))))

(defun gptel-aibo--cursoring-buffer-info (&optional buffer)
  "Get buffer information including file path and content.

When BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((language-identifier
           (gptel-aibo--mode-to-language-identifier major-mode))
          (cursor-symbol
           (gptel-aibo--cursor-symbol
            (buffer-substring-no-properties (point-min) (point-max)))))
      (if cursor-symbol
          (let ((before-cursor
                 (buffer-substring-no-properties (point-min) (point)))
                (after-cursor
                 (buffer-substring-no-properties (point) (point-max))))
            (concat (gptel-aibo--buffer-filename-info)
                    "Content:\n"
                    (gptel-aibo--make-code-block
                     (concat before-cursor cursor-symbol after-cursor)
                     language-identifier)
                    "\n\n"
                    (format gptel-aibo--cursor-notes cursor-symbol)))
        (concat (gptel-aibo--buffer-info)
                "\n"
                (gptel-aibo--cursor-position-info))))))

(defun gptel-aibo--cursoring-fragment-info (&optional buffer)
  "Get buffer information including file path and content.

When BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((language-identifier
            (gptel-aibo--mode-to-language-identifier major-mode))
           (fragment-boundaries
            (gptel-aibo--max-fragment-boundaries gptel-aibo-max-buffer-size))
           (cursor-symbol
            (gptel-aibo--cursor-symbol
             (buffer-substring-no-properties (point-min) (point-max))))
           (truncate-info
            (when (> (car fragment-boundaries) (point-min))
              (concat gptel-aibo--truncated-marker "\n")))
           (remain-info
            (if (< (cdr fragment-boundaries) (point-max))
                (concat gptel-aibo--omitted-marker "\n")
              (when (> (car fragment-boundaries) (point-min))
                (concat gptel-aibo--end-marker "\n")))))
      (if cursor-symbol
          (let ((before-cursor
                 (buffer-substring-no-properties (car fragment-boundaries) (point)))
                (after-cursor
                 (buffer-substring-no-properties (point) (cdr fragment-boundaries))))
            (concat (gptel-aibo--buffer-filename-info)
                    "Fragment around cursor:\n"
                    truncate-info
                    (gptel-aibo--make-code-block
                     (concat before-cursor cursor-symbol after-cursor)
                     language-identifier)
                    "\n"
                    remain-info
                    "\n"
                    (format gptel-aibo--cursor-notes cursor-symbol)))
        (gptel-aibo--fragment-info)))))

(defun gptel-aibo--cursor-line-distinct-p ()
  "Return t if cursor line is distinct to indicate the cursor."
  (let ((cursor-line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
    (and (>= (length cursor-line) 12)
         (< (length cursor-line) 120)
         (gptel-aibo--cursor-line-unique-p cursor-line))))

(defun gptel-aibo--cursor-line-unique-p (cursor-line)
  "Return t if CURSOR-LINE appears only once in the buffer."
  (save-excursion
    (goto-char (point-min))
    (let* ((trimmed-line (string-trim cursor-line))
           (re (format "^\\s-*%s\\s-*$" (regexp-quote trimmed-line))))
      (if (re-search-forward re nil t)
          (and (>= (match-end 0) (point))
               (not (re-search-forward re nil t)))
        t))))

(defun gptel-aibo--cursor-position-info ()
  "Return a string describing the cursor position info."
  (cond
   ((bobp)
    "The cursor is positioned at the beginning of the buffer.")
   ((eobp)
    "The cursor is positioned at the end of the buffer.")
   ((eq (line-number-at-pos) 1)
    (gptel-aibo--cursor-on-first-line-info))
   ((eq (line-number-at-pos) (line-number-at-pos (point-max)))
    (gptel-aibo--cursor-on-last-line-info))
   ((gptel-aibo--cursor-line-distinct-p)
    (gptel-aibo--cursor-line-info))
   (t
    (gptel-aibo--cursor-snippet-info))))

(defun gptel-aibo--cursor-position-info-in-fragment (start end)
  "Return a string describing the cursor position info betwwen START and END."
  (cond
   ((<= (point) start)
    "The cursor is positioned at the beginning of the fragment.")
   ((>= (point) end)
    "The cursor is positioned at the end of the fragment.")
   ((eq (line-number-at-pos) (line-number-at-pos start))
    (gptel-aibo--cursor-on-first-line-info-in-fragment start end))
   ((eq (line-number-at-pos) (line-number-at-pos end))
    (gptel-aibo--cursor-on-last-line-info-in-fragment start end))
   ((gptel-aibo--cursor-line-distinct-p)
    (gptel-aibo--cursor-line-info))
   (t
    (gptel-aibo--cursor-snippet-info-in-fragment start end))))

(defun gptel-aibo--cursor-on-first-line-info ()
  "Return a string describing the cursor position and line content.
The string includes the cursor line content and position information."
  (cond
   ((= (point) (line-beginning-position))
    "The cursor is positioned at the beginning of the first line of the buffer.")
   ((= (point) (line-end-position))
    "The cursor is positioned at the end of the first line of the buffer.")
   (t
    (let* ((before-cursor (buffer-substring-no-properties
                           (line-beginning-position)
                           (point)))
           (after-cursor (buffer-substring-no-properties
                          (point)
                          (line-end-position)))
           (before (if (> (length before-cursor) 20)
                       (substring before-cursor -20)
                     before-cursor))
           (after (if (> (length after-cursor) 20)
                      (substring after-cursor 0 20)
                    after-cursor)))
      (format "The cursor is on the first line of the buffer, after %s and before %s."
              (gptel-aibo--make-inline-code-block before)
              (gptel-aibo--make-inline-code-block after))))))

(defun gptel-aibo--cursor-on-last-line-info ()
  "Return a string describing the cursor position and line content.
The string includes the cursor line content and position information."
  (cond
   ((= (point) (line-beginning-position))
    "The cursor is positioned at the beginning of the last line of the buffer.")
   ((= (point) (line-end-position))
    "The cursor is positioned at the end of the last line of the buffer.")
   (t
    (let* ((before-cursor (buffer-substring-no-properties
                           (line-beginning-position)
                           (point)))
           (after-cursor (buffer-substring-no-properties
                          (point)
                          (line-end-position)))
           (before (if (> (length before-cursor) 20)
                       (substring before-cursor -20)
                     before-cursor))
           (after (if (> (length after-cursor) 20)
                      (substring after-cursor 0 20)
                    after-cursor)))
      (format "The cursor is on the last line of the buffer, after %s and before %s."
              (gptel-aibo--make-inline-code-block before)
              (gptel-aibo--make-inline-code-block after))))))

(defun gptel-aibo--cursor-on-first-line-info-in-fragment (start end)
  "Return a string describing the cursor position and line content.

The string includes the cursor line content and position information
and restricted by START and END"
  (let ((line-beginning (max (line-beginning-position) start))
        (line-end (min (line-end-position) end)))
    (cond
     ((= (point) line-beginning)
      "The cursor is positioned at the beginning of the first line of the fragment.")
     ((= (point) line-end)
      "The cursor is positioned at the end of the first line of the fragment.")
     (t
      (let* ((before-cursor (buffer-substring-no-properties
                             line-beginning
                             (point)))
             (after-cursor (buffer-substring-no-properties
                            (point)
                            line-end))
             (before (if (> (length before-cursor) 20)
                         (substring before-cursor -20)
                       before-cursor))
             (after (if (> (length after-cursor) 20)
                        (substring after-cursor 0 20)
                      after-cursor)))
        (format "The cursor is on the first line of the fragment, after %s and before %s."
                (gptel-aibo--make-inline-code-block before)
                (gptel-aibo--make-inline-code-block after)))))))

(defun gptel-aibo--cursor-on-last-line-info-in-fragment (start end)
  "Return a string describing the cursor position and line content.

The string includes the cursor line content and position information
and restricted by START and END."
  (let ((line-beginning (max (line-beginning-position) start))
        (line-end (min (line-end-position) end)))
    (cond
     ((= (point) line-beginning)
      "The cursor is positioned at the beginning of the last line of the fragment.")
     ((= (point) line-end)
      "The cursor is positioned at the end of the last line of the fragment.")
     (t
      (let* ((before-cursor (buffer-substring-no-properties
                             line-beginning
                             (point)))
             (after-cursor (buffer-substring-no-properties
                            (point)
                            line-end))
             (before (if (> (length before-cursor) 20)
                         (substring before-cursor -20)
                       before-cursor))
             (after (if (> (length after-cursor) 20)
                        (substring after-cursor 0 20)
                      after-cursor)))
        (format "The cursor is on the last line of the fragment, after %s and before %s."
                (gptel-aibo--make-inline-code-block before)
                (gptel-aibo--make-inline-code-block after)))))))

(defun gptel-aibo--cursor-line-info ()
  "Return a string describing the cursor position and line content.
The string includes the cursor line content and position information."
  (let* ((cursor-line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))
         (line-block (gptel-aibo--make-code-block
                      cursor-line
                      (gptel-aibo--mode-to-language-identifier major-mode))))

    (cond
     ((= (point) (line-beginning-position))
      (concat
       "The cursor is positioned at the beginning of the following line:\n"
       line-block))
     ((= (point) (line-end-position))
      (concat
       "The cursor is positioned at the end of the following line:\n"
       line-block))
     (t
      (let* ((before-cursor (buffer-substring-no-properties
                             (line-beginning-position)
                             (point)))
             (after-cursor (buffer-substring-no-properties
                            (point)
                            (line-end-position)))
             (before (if (> (length before-cursor) 20)
                         (substring before-cursor -20)
                       before-cursor))
             (after (if (> (length after-cursor) 20)
                        (substring after-cursor 0 20)
                      after-cursor)))
        (format "The cursor is on the following line:
%s
and is positioned after %s and before %s."
                line-block
                (gptel-aibo--make-inline-code-block before)
                (gptel-aibo--make-inline-code-block after)))))))

(defun gptel-aibo--cursor-snippet-info ()
  "Return cursor line info with preceding lines."
  (save-excursion
    (let ((pos (point))
          (start (line-beginning-position))
          (line-beginning (line-beginning-position))
          (line-end (line-end-position)))

      (while (and (> start (point-min))
                  (< (- line-end start) 24))
        (forward-line -1)
        (setq start (line-beginning-position)))

      ;; Ensure before uniqueness
      (while (and (> start (point-min))
                  (save-excursion
                    (goto-char (point-min))
                    (re-search-forward
                     (regexp-quote (buffer-substring-no-properties start line-end))
                     nil t 2)))
        (forward-line -1)
        (setq start (line-beginning-position)))

      (let* ((language-identifier
              (gptel-aibo--mode-to-language-identifier major-mode))
             (snippet (gptel-aibo--make-code-block
                       (buffer-substring-no-properties start line-end)
                       language-identifier)))
        (cond
         ((= pos line-beginning)
          (concat
           "The cursor is positioned at the beginning of the last line in the snippet below:\n"
           snippet))
         ((= pos line-end)
          (concat
           "The cursor is positioned at the end of the last line in the snippet below:\n"
           snippet))
         (t
          (let* ((before-cursor (buffer-substring-no-properties
                                 line-beginning
                                 pos))
                 (after-cursor (buffer-substring-no-properties
                                pos
                                line-end))
                 (before (if (> (length before-cursor) 20)
                             (substring before-cursor -20)
                           before-cursor))
                 (after (if (> (length after-cursor) 20)
                            (substring after-cursor 0 20)
                          after-cursor)))
            (format "The cursor is on the last line in the snippet below:
%s
and is positioned after %s and before %s."
                    snippet
                    (gptel-aibo--make-inline-code-block before)
                    (gptel-aibo--make-inline-code-block after)))))))))

(defun gptel-aibo--cursor-snippet-info-in-fragment (&optional point-min point-max)
  "Return cursor line info with preceding lines.

POINT-MIN and POINT-MAX, if provided, limit the region of extraction."
  (save-excursion
    (let* ((point-min (max (or point-min (point-min)) (point-min)))
           (point-max (min (or point-max (point-max)) (point-max)))
           (pos (point))
           (line-beginning (max (line-beginning-position) point-min))
           (line-end (min (line-end-position) point-max))
           (start line-beginning))

      (while (and (> start point-min)
                  (< (- line-end start) 24))
        (forward-line -1)
        (setq start (line-beginning-position)))

      (setq start (max start point-min))

      ;; Ensure before uniqueness
      (while (and (> start point-min)
                  (save-excursion
                    (goto-char point-min)
                    (re-search-forward
                     (regexp-quote (buffer-substring-no-properties start line-end))
                     point-max t 2)))
        (forward-line -1)
        (setq start (line-beginning-position)))

      (setq start (max start point-min))

      (let* ((language-identifier
              (gptel-aibo--mode-to-language-identifier major-mode))
             (snippet (gptel-aibo--make-code-block
                       (buffer-substring-no-properties start line-end)
                       language-identifier)))
        (cond
         ((= pos line-beginning)
          (concat
           "The cursor is positioned at the beginning of the last line in the snippet below:\n"
           snippet))
         ((= pos line-end)
          (concat
           "The cursor is positioned at the end of the last line in the snippet below:\n"
           snippet))
         (t
          (let* ((before-cursor (buffer-substring-no-properties
                                 line-beginning
                                 pos))
                 (after-cursor (buffer-substring-no-properties
                                pos
                                line-end))
                 (before (if (> (length before-cursor) 20)
                             (substring before-cursor -20)
                           before-cursor))
                 (after (if (> (length after-cursor) 20)
                            (substring after-cursor 0 20)
                          after-cursor)))
            (format "The cursor is on the last line in the snippet below:
%s
and is positioned after %s and before %s."
                    snippet
                    (gptel-aibo--make-inline-code-block before)
                    (gptel-aibo--make-inline-code-block after)))))

        ))))

(defun gptel-aibo--buffer-supports-imenu-p (&optional buffer)
  "Return non-nil if BUFFER supports imenu indexing.

If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (or (not (eq imenu-create-index-function
                 'imenu-default-create-index-function))
        (or (and imenu-prev-index-position-function
                 imenu-extract-index-name-function)
            (and imenu-generic-expression)))))

(defun gptel-aibo--buffer-outline-info (&optional buffer)
  "Get buffer information including file path and outline.

When BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (if-let ((outline (gptel-aibo--imenu-outline (current-buffer))))
        (concat (format "Filepath: `%s`\n" buffer-file-name)
                "Outline:\n"
                outline))))

(defun gptel-aibo--imenu-outline (&optional buffer)
  "Generate hierarchical outline from imenu index of BUFFER.
Return empty string if BUFFER is nil or imenu index unavailable."
  (with-current-buffer (or buffer (current-buffer))
    (when-let
        ((index (let ((imenu-auto-rescan t))
                  (ignore-errors (imenu--make-index-alist)))))
      (gptel-aibo--imenu-index-to-string index 0))))

(defun gptel-aibo--imenu-index-to-string (index depth)
  "Convert an imenu INDEX alist to a hierarchical string.
DEPTH is the current depth for indentation."
  (mapconcat
   (lambda (item)
     (cond
      ((and (listp item) (listp (car item)))
       (gptel-aibo--imenu-index-to-string item depth))
      ((listp (cdr item))
       (let ((heading (car item))
             (subitems (cdr item)))
         (unless (string= heading ".")
           (concat
            (make-string (* 2 depth) ?\s)
            (format "- %s\n" (gptel-aibo--imenu-item-title heading))
            (gptel-aibo--imenu-index-to-string subitems (1+ depth))))))
      ((and (consp item) (not (string= (car item) ".")))
       (concat
        (make-string (* 2 depth) ?\s)
        (format "- %s\n"
                (gptel-aibo--imenu-item-title (car item)))))
      (t "")))
   index ""))

(defun gptel-aibo--imenu-item-title (item)
  "Extract the string title from ITEM, stripping text properties if present."
  (cond
   ((stringp item) (substring-no-properties item))
   ((and (vectorp item) (stringp (aref item 0)))
    (substring-no-properties item))
   (t (format "%s" item))))

(cl-defun gptel-aibo--project-buffers-info (&optional quota)
  "Get information about other buffers in the same project of BUFFER.

The total size of the returned information will be limited by QUOTA."
  (let* ((buffers (gptel-aibo--project-buffers))
         (buffer-infos nil)
         (current-size 0)
         (buffer-count 0))

    (cl-loop
     for buf in buffers
     until (>= buffer-count gptel-aibo-max-buffer-count)
     do
     (when-let*
         ((buffer-size (buffer-size buf))
          (buffer-info
           (if (<= buffer-size gptel-aibo-max-buffer-size)
               (gptel-aibo--buffer-info buf)
             (gptel-aibo--buffer-outline-info buf)))
          (buffer-info-size (length buffer-info)))
       (when (or (not quota) (<= (+ current-size buffer-info-size) quota))
         (push (cons buf buffer-info) buffer-infos)
         (setq current-size (+ current-size buffer-info-size))
         (setq buffer-count (1+ buffer-count)))))

    (concat
     (when buffer-infos
       (concat "Other buffers in the same project:\n\n"
               (mapconcat (lambda (info)
                            (concat "`" (buffer-name (car info)) "`:\n"
                                    (cdr info)))
                          (nreverse buffer-infos) "\n")
               "\n\n")))))

(defun gptel-aibo--project-buffers ()
  "Get buffers in the same project as current buffer, itself excluded."
  (when-let ((project-current (project-current)))
    (seq-filter
     (lambda (buf)
       (and (not (eq buf (current-buffer)))
            (buffer-file-name buf)
            (with-current-buffer buf
              (equal (project-current) project-current))))
     (buffer-list))))

(defun gptel-aibo--max-fragment (max-length)
  "Extract the text fragment around the point.

The total length is limited by MAX-LENGTH."
  (let* ((boundaries
          (gptel-aibo--max-fragment-boundaries max-length))
         (start (car boundaries))
         (end (cdr boundaries))
         (before-text (buffer-substring-no-properties start (point)))
         (after-text (buffer-substring-no-properties (point) end)))
    (cons before-text after-text)))

(defun gptel-aibo--max-fragment-boundaries (max-length &optional buffer pos)
  "Find the largest contiguous defun block around POS within MAX-LENGTH chars.
BUFFER is the buffer to use, or the current buffer if nil.
POS is the position to center on, or the current point if nil."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (let* ((pos (or pos (point)))
             (start pos)
             (end pos))

        ;; Step 1: Move to the end of the current defun
        (end-of-defun)
        (setq end (point))

        ;; Step 2: Find the beginning of the current defun
        (beginning-of-defun)
        (setq start (point))

        ;; Step 3: If `start` is beyond `pos`, it means `pos` was between defuns
        (when (> start pos)
          (beginning-of-defun)
          (setq start (point)))

        ;; Expand while within max-length
        (let* ((start-changed t)
               (end-changed t))
          (while (and (or start-changed end-changed)
                      (< (- end start) max-length)
                      (or (> start (point-min)) (< end (point-max))))
          ;; Move to `start` and extend backwards
          (goto-char start)
          (beginning-of-defun)
          ;; Check whether the value of `start` changes
          (setq start-changed (not (eq start (point))))
          (setq start (point))

          ;; Move to `start` again and extend forwards
          (end-of-defun)
          (let ((new-end-pos (point)))
            (if (> new-end-pos end)
                (setq end new-end-pos)
              ;; Otherwise, move to previous `end` and extend
              (goto-char end)
              (end-of-defun)
              ;; Check whether the value of `end` changes
              (setq end-changed (not (eq end (point))))
              (setq end (point))))))

        ;; Adjust boundaries if exceeding max-length
        (if (<= (- end start) max-length)
            (cons start end)
          (let* ((half-limit (/ max-length 2))
                 (adjusted-start (max start (- pos half-limit)))
                 (adjusted-end (min end (+ adjusted-start max-length))))
            (cons adjusted-start adjusted-end)))))))

(defun gptel-aibo--unique-region-p (beg end)
  "Check if the text between BEG and END appears uniquely in the buffer.

BEG is the starting position of the region.
END is the ending position of the region."
  (let ((region-text (buffer-substring-no-properties beg end)))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward region-text nil t)
        (eq (match-beginning 0) beg)))))

(defun gptel-aibo--project-current-directory-info ()
  "Return current directory listing as a string if in a project.
If not in a project, return empty string.
The listing includes files and directories, with '/' appended to directory
names."
  (if-let ((proj (project-current)))
      (let ((current-dir (file-name-directory
                          (or buffer-file-name default-directory))))
        (with-temp-buffer
          (insert "Files in the project's current directory:\n```\n")
          (dolist (file (directory-files current-dir))
            (unless (member file '("." ".."))
              (insert file)
              (when (file-directory-p (expand-file-name file current-dir))
                (insert "/"))
              (insert "\n")))
          (insert "```\n")
          (buffer-string)))
    ""))

(defun gptel-aibo--project-root (project)
  "Get the root directory of PROJECT.
Returns: The project root directory as a string, or nil if not found."
  (cond
   ((fboundp 'project-root)
    (project-root project))
   ((fboundp 'project-roots)
    (car (project-roots project)))))

(defun gptel-aibo--project-name (project)
  "Get the name of PROJECT."
  (file-name-nondirectory (directory-file-name
                           (gptel-aibo--project-root project))))

(defun gptel-aibo--project-directory-info ()
  "Return project directory information based on current location."
  (if (project-current)
      (let ((top-info (gptel-aibo--project-top-directory-info))
            (current-info (gptel-aibo--project-current-directory-info)))
        (if (string-empty-p top-info)
            ""
          (if (string= (file-name-directory
                        (or buffer-file-name default-directory))
                       (gptel-aibo--project-root (project-current)))
              top-info
            (concat top-info "\n" current-info))))
    ""))

(defun gptel-aibo--project-top-directory-info ()
  "Return formatted string of top-level directory listing.
If in a project, returns the listing, else returns empty string."
  (if-let ((proj (project-current)))
      (let ((project-root (gptel-aibo--project-root proj)))
        (with-temp-buffer
          (insert "Files in the project's top directory:\n```\n")
          (dolist (file (directory-files project-root))
            (unless (member file '("." ".."))
              (insert file)
              (when (file-directory-p (expand-file-name file project-root))
                (insert "/"))
              (insert "\n")))
          (insert "```\n")
          (buffer-string)))
    ""))

(defun gptel-aibo--make-code-block (content &optional language)
  "Wrap CONTENT in a fenced code block with optional LANGUAGE identifier."
  (let ((fence (gptel-aibo--make-code-fence content)))
    (concat fence (or language "") "\n" content "\n" fence)))

(defun gptel-aibo--make-code-fence (content)
  "Generate a code fence string that safely encapsulates CONTENT.
The fence length is determined by:
1. The longest sequence of consecutive backticks in CONTENT
2. Always at least one backtick longer than the longest sequence
3. Minimum length of 3 backticks

CONTENT: String to be wrapped in code fence
Returns: String containing the appropriate number of backticks"
  (let ((max-backticks 0)
        (start 0))
    (while (string-match "`+" content start)
      (setq max-backticks (max max-backticks
                               (- (match-end 0) (match-beginning 0))))
      (setq start (match-end 0)))
    (make-string (max 3 (1+ max-backticks)) ?`)))

(defun gptel-aibo--make-inline-code-block (content)
  "Generate an inline code fence string to safely encapsulate CONTENT.
The fence length is determined by:
1. The longest sequence of consecutive backticks in CONTENT.
2. Always at least one backtick longer than the longest sequence.
3. If CONTENT contains backticks, add a space inside the fence.
4. Returns the generated inline code string in the format:
   `N CONTENT N`, where N is the determined backtick length.

CONTENT: String to be wrapped in inline code fence.
Returns: Properly fenced inline code string."
  (let ((max-backticks 0)
        (start 0))
    (while (string-match "`+" content start)
      (setq max-backticks (max max-backticks
                               (- (match-end 0) (match-beginning 0))))
      (setq start (match-end 0)))
    (let* ((fence (make-string (1+ max-backticks) ?`))
           (needs-space (string-match "`" content))
           (wrapped-content (if needs-space
                                (concat " " content " ")
                              content)))
      (concat fence wrapped-content fence))))

(defun gptel-aibo--mode-to-language-identifier (mode)
  "Convert MODE to code block language identifier."
  (let* ((mode-name (symbol-name mode))
         (mode-mapping
          '(("emacs-lisp-mode" . "elisp")
            ("lisp-mode" . "lisp")
            ("clojure-mode" . "clojure")
            ("python-mode" . "python")
            ("ruby-mode" . "ruby")
            ("js-mode" . "javascript")
            ("js2-mode" . "javascript")
            ("typescript-mode" . "typescript")
            ("c-mode" . "c")
            ("c++-mode" . "cpp")
            ("rustic-mode" . "rust")
            ("java-mode" . "java")
            ("go-mode" . "go")
            ("rust-mode" . "rust")
            ("sh-mode" . "shell")
            ("shell-mode" . "shell")
            ("css-mode" . "css")
            ("scss-mode" . "scss")
            ("html-mode" . "html")
            ("xml-mode" . "xml")
            ("sql-mode" . "sql")
            ("markdown-mode" . "markdown")
            ("yaml-mode" . "yaml")
            ("dockerfile-mode" . "dockerfile")
            ("json-mode" . "json")
            ("text-mode" . "text")))
         (lang (cdr (assoc mode-name mode-mapping))))
    (or lang
        (replace-regexp-in-string "-mode$" "" mode-name))))

(defun gptel-aibo--indent (content depth)
  "Indent CONTENT by DEPTH spaces at the start of each line.
Returns the indented content as a string."
  (let ((lines (split-string content "\n")))
    (mapconcat (lambda (line)
                 (concat (make-string depth ? ) line))
               lines
               "\n")))

(provide 'gptel-aibo-context)
;;; gptel-aibo-context.el ends here
