;;; orgx-format.el --- Functional formatting and linting for Org files -*- lexical-binding: t; -*-

;;; Commentary:
;; Functional Org-mode formatter and linter.
;;
;; Provides:
;; - `orgx-format-buffer' and `orgx-format-region' to flow paragraphs,
;;   realign tables, and remove section-initial blank lines.
;; - `orgx-format-lint' to report lint errors for lines exceeding
;;   `orgx-format-max-line-length' and formatting violations.
;; - Incremental yielding between sections using `thread-yield'.
;; - Batch formatting (`orgx-format-batch') with buffer locking, revert,
;;   and cancellation (`orgx-format-batch-cancel').

;;; Code:

(require 'org)
(require 'org-table)
(require 'seq)
(require 'subr-x)

(declare-function sprite-direct-run "sprite-direct")

;;; Configuration

(defgroup orgx-format nil
  "Org-mode formatting and linting tools."
  :group 'orgx
  :prefix "orgx-format-")

(defcustom orgx-format-line-length 80
  "Target fill column width for reflowing paragraphs."
  :type 'integer
  :group 'orgx-format)

(defcustom orgx-format-max-line-length 100
  "Maximum permitted line length in linting mode."
  :type 'integer
  :group 'orgx-format)

(defcustom orgx-format-trim-section-start t
  "When non-nil, delete blank lines immediately following a heading."
  :type 'boolean
  :group 'orgx-format)

(defcustom orgx-format-trim-section-end nil
  "When non-nil, delete blank lines immediately preceding the next heading."
  :type 'boolean
  :group 'orgx-format)

(defcustom orgx-format-align-tables t
  "When non-nil, realign all tables during formatting."
  :type 'boolean
  :group 'orgx-format)

(defcustom orgx-format-flow-paragraphs t
  "When non-nil, reflow prose paragraphs during formatting."
  :type 'boolean
  :group 'orgx-format)

;;; Buffer locking state for batch operations

(defvar orgx-format--locked-buffers nil
  "Alist of (BUFFER . WAS-READ-ONLY) currently locked by batch formatting.")

(defun orgx-format-lock-buffer (buffer)
  "Lock BUFFER in preparation for background formatting.
Saves previous read-only status and makes BUFFER read-only."
  (when (buffer-live-p buffer)
    (unless (assq buffer orgx-format--locked-buffers)
      (with-current-buffer buffer
        (push (cons buffer buffer-read-only) orgx-format--locked-buffers)
        (setq buffer-read-only t)))))

(defun orgx-format-unlock-buffer (buffer &optional revert)
  "Unlock BUFFER and optionally REVERT it to match disk."
  (when (buffer-live-p buffer)
    (when-let* ((cell (assq buffer orgx-format--locked-buffers)))
      (setq orgx-format--locked-buffers (delq cell orgx-format--locked-buffers))
      (with-current-buffer buffer
        (setq buffer-read-only (cdr cell))
        (when revert
          (revert-buffer :ignore-auto :noconfirm))))))

;;;###autoload
(defun orgx-format-batch-cancel ()
  "Clear all active buffer locks from batch formatting."
  (interactive)
  (let ((count (length orgx-format--locked-buffers)))
    (seq-do (lambda (cell)
              (when (buffer-live-p (car cell))
                (with-current-buffer (car cell)
                  (setq buffer-read-only (cdr cell)))))
            orgx-format--locked-buffers)
    (setq orgx-format--locked-buffers nil)
    (message "Cleared %d buffer locks." count)))

;;; Core formatting helpers

(defun orgx-format--in-protected-block-p ()
  "Return non-nil if point is inside a code block, property drawer, or table."
  (save-excursion
    (beginning-of-line)
    (or (org-at-table-p)
        (looking-at-p "^[ \t]*:[A-Z_]+:")
        (looking-at-p "^[ \t]*:END:")
        (looking-at-p "^[ \t]*#\\+begin_")
        (looking-at-p "^[ \t]*#\\+end_")
        (looking-at-p "^[ \t]*#\\+")
        (looking-at-p "^[ \t]*# ")
        (org-in-src-block-p)
        (and (fboundp 'org-in-drawer-p) (org-in-drawer-p)))))

(defun orgx-format--trim-section-blank-lines (beg end)
  "Delete excess blank lines in region BEG to END.
Optionally trim blank lines at the start and end of the region according to
`orgx-format-trim-section-start' and `orgx-format-trim-section-end'."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; If starting at a heading, advance past heading, planning lines, and drawers
      (when (looking-at-p org-outline-regexp)
        (forward-line 1)
        (while (and (not (eobp))
                    (or (looking-at-p "^[ \t]*\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):")
                        (looking-at-p "^[ \t]*:[A-Z_]+:")))
          (if (looking-at-p "^[ \t]*:[A-Z_]+:")
              (if (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                  (forward-line 1)
                (forward-line 1))
            (forward-line 1)))
        ;; Remove blank lines directly following heading/properties
        (when orgx-format-trim-section-start
          (while (and (not (eobp))
                      (looking-at-p "^[ \t]*$"))
            (delete-region (point) (line-beginning-position 2)))))
      ;; Optionally trim blank lines at end of section
      (when orgx-format-trim-section-end
        (goto-char (point-max))
        (skip-chars-backward " \t\n")
        (delete-region (point) (point-max))
        (insert "\n")))))

(defun orgx-format--reflow-paragraphs (beg end)
  "Reflow prose paragraphs between BEG and END to `orgx-format-line-length'."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((fill-column orgx-format-line-length))
        (while (< (point) (point-max))
          (cond
           ((looking-at-p org-outline-regexp)
            (forward-line 1))
           ((orgx-format--in-protected-block-p)
            (forward-line 1))
           ((looking-at-p "^[ \t]*$")
            (forward-line 1))
           (t
            (let ((para-beg (point)))
              (forward-paragraph)
              (let ((para-end (point)))
                (fill-region-as-paragraph para-beg para-end)
                (goto-char para-end))))))))))

(defun orgx-format--realign-tables (beg end)
  "Realign all Org tables between BEG and END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*|" nil t)
        (beginning-of-line)
        (when (org-at-table-p)
          (org-table-align)
          (goto-char (org-table-end)))))))

(defun orgx-format-section-at-point ()
  "Format the current Org section (or current subtree)."
  (interactive)
  (let* ((beg (save-excursion
                (if (org-before-first-heading-p)
                    (point-min)
                  (org-back-to-heading t)
                  (point))))
         (end (save-excursion
                (goto-char beg)
                (if (org-before-first-heading-p)
                    (if (re-search-forward org-outline-regexp nil t)
                        (match-beginning 0)
                      (point-max))
                  (org-end-of-subtree t t)
                  (point)))))
    (orgx-format-region beg end)))

;;;###autoload
(defun orgx-format-region (beg end)
  "Format the Org region between BEG and END.
Trims section-initial blank lines, aligns tables, and reflows paragraphs."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (when orgx-format-trim-section-start
        (orgx-format--trim-section-blank-lines (point-min) (point-max)))
      (when orgx-format-align-tables
        (orgx-format--realign-tables (point-min) (point-max)))
      (when orgx-format-flow-paragraphs
        (orgx-format--reflow-paragraphs (point-min) (point-max))))))

;;;###autoload
(defun orgx-format-buffer (&optional buffer)
  "Format the entire BUFFER (defaulting to current buffer).
Refuses to format non-interactively if BUFFER has unsaved modifications.
Yields incrementally between sections to keep Emacs responsive."
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (user-error "Buffer %s is not in org-mode" (buffer-name buf)))
      (when (and (not (called-interactively-p 'any))
                 (buffer-file-name buf)
                 (buffer-modified-p buf))
        (user-error "Buffer %s has unsaved modifications; save before formatting"
                    (buffer-name buf)))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          ;; Format text before first heading
          (let ((first-heading (save-excursion
                                 (if (re-search-forward org-outline-regexp nil t)
                                     (match-beginning 0)
                                   (point-max)))))
            (when (> first-heading (point-min))
              (orgx-format-region (point-min) first-heading)
              (thread-yield)))
          ;; Format section by section
          (while (re-search-forward org-outline-regexp nil t)
            (beginning-of-line)
            (let* ((sec-beg (point-marker))
                   (sec-end (save-excursion
                              (forward-line 1)
                              (if (re-search-forward org-outline-regexp nil t)
                                  (copy-marker (match-beginning 0))
                                (point-max-marker)))))
              (orgx-format-region (marker-position sec-beg) (marker-position sec-end))
              (goto-char sec-end)
              (set-marker sec-beg nil)
              (set-marker sec-end nil)
              (thread-yield))))))
    (message "Formatted %s." (buffer-name buf))))

;;; Linting functionality

(defun orgx-format--line-too-long-p (line)
  "Return non-nil if LINE exceeds `orgx-format-max-line-length'.
Ignores table rows, comments, and lines that are exclusively URLs or links."
  (and (> (length line) orgx-format-max-line-length)
       (not (string-match-p "^[ \t]*|" line))
       (not (string-match-p "^[ \t]*#" line))
       (not (string-match-p "https?://[^ \t\n]+$" line))
       (not (string-match-p "\\[\\[[^]]+\\]\\[?[^]]*\\]?\\]" line))))

(defun orgx-format-lint (&optional buffer)
  "Analyze BUFFER for formatting and line length errors without modifying it.
Returns a list of diagnostic plists:
  ((:line LINE :col COL :type TYPE :message MSG) ...)"
  (let ((buf (or buffer (current-buffer)))
        diagnostics)
    (with-current-buffer buf
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((line-num 1)
                (prev-heading nil))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
                ;; Check line length
                (when (orgx-format--line-too-long-p line)
                  (push (list :line line-num
                              :col 1
                              :type 'line-length
                              :message (format "Line length %d exceeds max %d"
                                               (length line)
                                               orgx-format-max-line-length))
                        diagnostics))
                ;; Check blank lines immediately following headings
                (if (looking-at-p org-outline-regexp)
                    (setq prev-heading line-num)
                  (when (and prev-heading
                             (= line-num (1+ prev-heading))
                             (string-empty-p (string-trim line)))
                    (push (list :line line-num
                                :col 1
                                :type 'blank-line
                                :message "Blank line immediately follows heading")
                          diagnostics))
                  (unless (string-empty-p (string-trim line))
                    (setq prev-heading nil))))
              (forward-line 1)
              (setq line-num (1+ line-num)))))))
    (nreverse diagnostics)))

;;;###autoload
(defun orgx-format-lint-buffer (&optional buffer)
  "Lint the current BUFFER and report issues in a compilation-style view."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (errors (orgx-format-lint buf)))
    (if (null errors)
        (message "Org lint passed: no formatting issues in %s." (buffer-name buf))
      (let ((out-buf (get-buffer-create "*orgx-lint*")))
        (with-current-buffer out-buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Orgx lint findings for %s:\n\n" (buffer-name buf)))
            (seq-do (lambda (err)
                      (insert (format "%s:%d:%d: [%s] %s\n"
                                      (buffer-file-name buf)
                                      (plist-get err :line)
                                      (plist-get err :col)
                                      (plist-get err :type)
                                      (plist-get err :message))))
                    errors)))
        (display-buffer out-buf)
        (message "Found %d lint issues in %s." (length errors) (buffer-name buf))))))

;;; Batch formatting

;;;###autoload
(defun orgx-format-batch (files &optional revert-open)
  "Format a list of Org FILES.
Locks open buffers locally, formats the files, then optionally reverts
and unlocks open buffers if REVERT-OPEN is non-nil.
Uses sprite workers when `sprite-direct' is available."
  (interactive (list (list (buffer-file-name))))
  (let ((locked-count 0))
    ;; Lock currently open buffers
    (seq-do (lambda (file)
              (when-let* ((b (find-buffer-visiting file)))
                (orgx-format-lock-buffer b)
                (setq locked-count (1+ locked-count))))
            files)
    (unwind-protect
        (seq-do
         (lambda (file)
           (if (fboundp 'sprite-direct-run)
               (sprite-direct-run
                :form `(with-temp-buffer
                         (insert-file-contents ,file)
                         (org-mode)
                         (orgx-format-buffer)
                         (write-region (point-min) (point-max) ,file nil 'quiet)))
             ;; Local isolated execution
             (with-temp-buffer
               (insert-file-contents file)
               (org-mode)
               (orgx-format-buffer)
               (write-region (point-min) (point-max) file nil 'quiet))))
         files)
      ;; Unlock open buffers and revert
      (seq-do (lambda (file)
                (when-let* ((b (find-buffer-visiting file)))
                  (orgx-format-unlock-buffer b (or revert-open t))))
              files))
    (message "Batch formatted %d files (%d buffers locked and reverted)."
             (length files) locked-count)))

(provide 'orgx-format)
;;; orgx-format.el ends here
