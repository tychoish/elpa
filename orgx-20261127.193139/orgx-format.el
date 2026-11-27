;;; orgx-format.el --- Functional formatting and linting for Org files -*- lexical-binding: t; -*-

;;; Commentary:
;; Functional Org-mode formatter and linter using native Org-element AST APIs.
;;
;; Provides:
;; - `orgx-format-buffer' and `orgx-format-region' to flow paragraphs,
;;   realign tables, and remove section-initial blank lines.
;; - `orgx-format-lint' to report lint errors for lines exceeding
;;   `orgx-format-max-line-length' and formatting violations.
;; - Incremental yielding between sections using `thread-yield'.
;; - Batch formatting (`orgx-format-batch') with buffer locking, revert,
;;   and cancellation (`orgx-format-batch-cancel').
;;
;; All formatting operations leverage native Org-mode AST parsers (`org-element')
;; and Org filling/alignment routines (`org-fill-element', `org-table-align',
;; `org-end-of-meta-data').  No regex-based reformatting rules are used.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-table)
(require 'outline)
(require 'seq)
(require 'subr-x)

(declare-function org-element--property "org-element-ast")
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

;;; Core formatting helpers (AST-driven, zero regexp reformatting)

(defun orgx-format--trim-section-blank-lines (beg end)
  "Delete excess blank lines in region BEG to END using Org metadata navigation.
Optionally trim blank lines at the start and end of sections according to
`orgx-format-trim-section-start' and `orgx-format-trim-section-end'."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (when orgx-format-trim-section-start
        (let ((headlines (nreverse (org-element-map (org-element-parse-buffer)
                                       'headline #'identity))))
          (dolist (hl headlines)
            (goto-char (org-element-property :begin hl))
            (org-end-of-meta-data nil)
            (while (and (not (eobp))
                        (progn (skip-chars-forward " \t") (eolp)))
              (delete-region (line-beginning-position)
                             (min (point-max) (1+ (line-end-position))))))))
      (when orgx-format-trim-section-end
        (goto-char (point-max))
        (skip-chars-backward " \t\n")
        (when (< (point) (point-max))
          (delete-region (point) (point-max))
          (insert "\n"))))))

(defun orgx-format--reflow-paragraphs (beg end)
  "Reflow prose paragraphs between BEG and END to `orgx-format-line-length'.
Uses Org AST to locate paragraphs and leverages `org-fill-element' to respect
Org-mode syntax (including hard line breaks, lists, and quote blocks)."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((fill-column orgx-format-line-length))
        (dolist (p (nreverse (org-element-map (org-element-parse-buffer)
                                 'paragraph #'identity)))
          (when-let* ((pos (or (org-element-property :contents-begin p)
                               (org-element-property :begin p))))
            (goto-char pos)
            (org-fill-element)))))))

(defun orgx-format--realign-tables (beg end)
  "Realign all Org tables between BEG and END using Org AST."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (dolist (table (nreverse (org-element-map (org-element-parse-buffer)
                                   'table #'identity)))
        (when (eq (org-element-property :type table) 'org)
          (goto-char (or (org-element-property :post-affiliated table)
                         (org-element-property :begin table)))
          (org-table-align))))))

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
                    (if (outline-next-heading)
                        (point)
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
                                 (if (outline-next-heading)
                                     (point)
                                   (point-max)))))
            (when (> first-heading (point-min))
              (orgx-format-region (point-min) first-heading)
              (thread-yield)))
          ;; Format section by section
          (goto-char (point-min))
          (while (outline-next-heading)
            (let* ((sec-beg (point-marker))
                   (sec-end (save-excursion
                              (forward-line 1)
                              (if (outline-next-heading)
                                  (point-marker)
                                (point-max-marker)))))
              (orgx-format-region (marker-position sec-beg)
                                  (marker-position sec-end))
              (goto-char sec-end)
              (set-marker sec-beg nil)
              (set-marker sec-end nil)
              (thread-yield))))))
    (message "Formatted %s." (buffer-name buf))))

;;; Linting functionality

(defun orgx-format--line-exempt-from-length-p (line)
  "Return non-nil if LINE is exempt from line length checks.
Exempt lines are those containing standalone links or URLs."
  (let ((trimmed (string-trim line)))
    (or (string-prefix-p "http://" trimmed)
        (string-prefix-p "https://" trimmed)
        (and (string-prefix-p "[[" trimmed)
             (string-suffix-p "]]" trimmed)))))

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
          (let ((ast (org-element-parse-buffer)))
            ;; Check blank lines immediately following headings
            (dolist (hl (org-element-map ast 'headline #'identity))
              (save-excursion
                (goto-char (org-element-property :begin hl))
                (org-end-of-meta-data nil)
                (when (and (not (eobp))
                           (progn (skip-chars-forward " \t") (eolp)))
                  (push (list :line (line-number-at-pos)
                              :col 1
                              :type 'blank-line
                              :message "Blank line immediately follows heading")
                        diagnostics))))
            ;; Check line length in prose paragraphs
            (dolist (p (org-element-map ast 'paragraph #'identity))
              (save-excursion
                (let ((end (or (org-element-property :contents-end p)
                               (org-element-property :end p))))
                  (goto-char (or (org-element-property :contents-begin p)
                                 (org-element-property :begin p)))
                  (while (< (point) end)
                    (let ((line-len (- (line-end-position)
                                       (line-beginning-position))))
                      (when (> line-len orgx-format-max-line-length)
                        (let ((line (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))
                          (unless (orgx-format--line-exempt-from-length-p line)
                            (push (list :line (line-number-at-pos)
                                        :col 1
                                        :type 'line-length
                                        :message (format "Line length %d exceeds max %d"
                                                         line-len
                                                         orgx-format-max-line-length))
                                  diagnostics)))))
                    (forward-line 1)))))))))
    (sort diagnostics (lambda (a b) (< (plist-get a :line) (plist-get b :line))))))

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
