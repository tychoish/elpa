;;; magit-dash-review-analysis.el --- Show code-review analysis in diff-review -*- lexical-binding: t; -*-

;;; Commentary:
;; Glue between the code_review.analysis key of a diff-review/1 review file
;; (see docs/review-json.org) and the diff-review package, which stays
;; unmodified.
;;
;; `magit-dash-review-analysis-mode' only DISPLAYS analysis already in the
;; review JSON: a risk rank, [TAG] markers such as [NO-TEST] and [CI-GAME],
;; and criteria ids, appended to a file's line in diff-review's sidebar and
;; overview and shown at the end of each matching hunk's first line in a
;; file buffer.
;;
;; diff-review has no decoration hooks, so the mode installs two named
;; `advice-add' pieces on its internals (`diff-review--file-line' and
;; `diff-review--decorate'), which are removed again when the mode is
;; turned off.  File-buffer decorations are overlays tagged with
;; `magit-dash-review-analysis', also pushed onto `diff-review--overlays' so
;; diff-review's own redecoration removes them.
;;
;; Analysis is keyed by path, then by the raw @@ ranges text of a hunk
;; (for example "-1,3 +1,4").  diff-review parses its hunks with
;; `git diff -U0', whose old-side ranges differ from code-review's, so a
;; hunk matches when the NEW-side range of the key overlaps the hunk's new
;; lines.  A key that matches no hunk is ignored.
;;
;; Computing analysis is explicit and asynchronous:
;; `magit-dash-review-analysis-refresh' runs the export with analysis in a
;; throwaway child Emacs and stores the result in the review file with the
;; adapter's safe writer.

;;; Code:

(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'magit-dash-review-json)

(defvar diff-review--data)
(defvar diff-review-file-mode)
(defvar diff-review--overlays)
(defvar diff-review--hunks)
(defvar diff-review--file-path)
(declare-function diff-review--file "diff-review" (&optional index))
(declare-function diff-review--review-buffer "diff-review" ())
(declare-function diff-review--line-pos "diff-review" (line))
(declare-function diff-review--hunk-line "diff-review" (hunk))
(declare-function diff-review--render-all "diff-review" ())
(declare-function diff-review--redecorate-files "diff-review" ())

(defgroup magit-dash-review-analysis nil
  "Show code-review analysis in diff-review."
  :group 'magit-dash-review-json)

(defcustom magit-dash-review-analysis-child-init-files nil
  "Files the child Emacs of `magit-dash-review-analysis-refresh' loads first.
The child runs with -Q, so code-review's database and account setup must
come from here (for example a file that sets them up)."
  :type '(repeat file)
  :group 'magit-dash-review-analysis)

(defface magit-dash-review-analysis-rank
  '((t :inherit warning))
  "Face of the risk rank."
  :group 'magit-dash-review-analysis)

(defface magit-dash-review-analysis-tag
  '((t :inherit error))
  "Face of the [TAG] markers."
  :group 'magit-dash-review-analysis)

(defface magit-dash-review-analysis-criteria
  '((t :inherit shadow))
  "Face of the criteria ids."
  :group 'magit-dash-review-analysis)

;;; Data

(defun magit-dash-review-analysis-parse-ranges (key)
  "Parse the @@ ranges text KEY into (OLD-START OLD-COUNT NEW-START NEW-COUNT).
A missing count is 1.  Return nil when KEY is not of the form -A[,B] +C[,D]."
  (when (and (stringp key)
             (string-match "\\`-\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?\\'"
                           key))
    (seq-mapn (lambda (group default)
               (if (match-string group key)
                   (string-to-number (match-string group key))
                 default))
             '(1 2 3 4)
             '(0 1 0 1))))

(defun magit-dash-review-analysis-for-path (table path)
  "Return the analysis of PATH in review TABLE, or nil.
The result is the hash of hunk key to (rank tags criteria)."
  (when-let* (((hash-table-p table))
              (code-review (map-elt table "code_review"))
              ((hash-table-p code-review))
              (analysis (map-elt code-review "analysis"))
              ((hash-table-p analysis))
              (by-hunk (map-elt analysis path))
              ((hash-table-p by-hunk)))
    by-hunk))

(defun magit-dash-review-analysis--entry-parts (entry)
  "Return (RANK TAGS CRITERIA) from analysis ENTRY, a hash table."
  (list (map-elt entry "rank")
        (append (map-elt entry "tags") nil)
        (append (map-elt entry "criteria") nil)))

(defun magit-dash-review-analysis-text (entry)
  "Return the display text of analysis ENTRY: rank, [TAG]s and criteria ids.
Return nil when ENTRY has none of them."
  (when (hash-table-p entry)
    (pcase-let* ((`(,rank ,tags ,criteria) (magit-dash-review-analysis--entry-parts entry))
                 (parts (delq nil
                              (list (and (integerp rank)
                                         (propertize (format "r%d" rank)
                                                     'face 'magit-dash-review-analysis-rank))
                                    (and tags
                                         (propertize (mapconcat (lambda (tag) (format "[%s]" tag))
                                                                tags " ")
                                                     'face 'magit-dash-review-analysis-tag))
                                    (and criteria
                                         (propertize (mapconcat (lambda (id) (format "%s" id))
                                                                criteria ",")
                                                     'face 'magit-dash-review-analysis-criteria))))))
      (when parts
        (string-join parts " ")))))

(defun magit-dash-review-analysis-file-text (by-hunk)
  "Return the one-line summary of BY-HUNK, a path's hunk key to entry hash.
It shows the highest rank, the union of the tags and of the criteria ids.
Return nil when BY-HUNK is nil or carries no usable entry."
  (when (hash-table-p by-hunk)
    (let ((summary (make-hash-table :test #'equal))
          (ranks nil)
          (tags nil)
          (criteria nil))
      (map-do (lambda (_key entry)
                (when (hash-table-p entry)
                  (pcase-let ((`(,rank ,entry-tags ,entry-criteria)
                               (magit-dash-review-analysis--entry-parts entry)))
                    (when (integerp rank)
                      (push rank ranks))
                    (setq tags (seq-union tags entry-tags))
                    (setq criteria (seq-union criteria entry-criteria)))))
              by-hunk)
      (when ranks
        (setf (map-elt summary "rank") (seq-max ranks)))
      (setf (map-elt summary "tags") (vconcat tags))
      (setf (map-elt summary "criteria") (vconcat criteria))
      (magit-dash-review-analysis-text summary))))

(defun magit-dash-review-analysis--rank (entry)
  "Return the integer rank of analysis ENTRY, or 0 when it has none."
  (let ((rank (map-elt entry "rank")))
    (if (integerp rank)
        rank
      0)))

(defun magit-dash-review-analysis-span (start count)
  "Return the (FIRST . LAST) new-side lines covered by START and COUNT.
A pure deletion (COUNT 0) sits after line START and is shown on line
START + 1, as `diff-review--hunk-line' does, so that is its whole span."
  (if (zerop count)
      (cons (1+ start) (1+ start))
    (cons start (+ start count -1))))

(defun magit-dash-review-analysis-hunk-entries (by-hunk hunks)
  "Match BY-HUNK against HUNKS; return a list of (HUNK . ENTRY).
HUNKS are diff-review plists with :line and :count (new side).  A hunk
matches an entry whose key's new-side span overlaps its own, both computed
by `magit-dash-review-analysis-span' (so a deletion at :line N lives on line
N+1, including :line 0 at the top of a file).  Keys that cannot be parsed
or match no hunk are ignored.  A hunk matching several entries gets the one
with the highest integer rank; a non-integer rank counts as 0."
  (let ((best nil))
    (when (hash-table-p by-hunk)
      (map-do
       (lambda (key entry)
         (when-let* ((ranges (magit-dash-review-analysis-parse-ranges key))
                     ((hash-table-p entry)))
           (let ((key-span (magit-dash-review-analysis-span (nth 2 ranges) (nth 3 ranges)))
                 (rank (magit-dash-review-analysis--rank entry)))
             (seq-do (lambda (hunk)
                       (let ((span (magit-dash-review-analysis-span
                                    (plist-get hunk :line) (plist-get hunk :count))))
                         (when (and (<= (car key-span) (cdr span))
                                    (<= (car span) (cdr key-span)))
                           (let ((known (assq hunk best)))
                             (cond
                              ((null known)
                               (push (cons hunk entry) best))
                              ((> rank (magit-dash-review-analysis--rank (cdr known)))
                               (setcdr known entry)))))))
                     hunks))))
       by-hunk))
    (seq-sort-by (lambda (pair) (plist-get (car pair) :line)) #'< best)))

;;; Decoration

(defun magit-dash-review-analysis-clear-buffer ()
  "Delete this buffer's analysis overlays.
In a diff-review file buffer they are also dropped from
`diff-review--overlays'; no other buffer gets that variable made local."
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) 'magit-dash-review-analysis t))
  (when (bound-and-true-p diff-review-file-mode)
    (setq diff-review--overlays (seq-filter #'overlay-buffer diff-review--overlays))))

(defun magit-dash-review-analysis-decorate-buffer (by-hunk hunks line-pos-function)
  "Overlay the analysis of BY-HUNK onto HUNKS in the current buffer.
Each matching hunk gets an after-string at the end of its first line.
LINE-POS-FUNCTION maps a line number to its start position.  Return the
overlays, which in a diff-review file buffer are also pushed onto
`diff-review--overlays'."
  (magit-dash-review-analysis-clear-buffer)
  (thread-last
    (magit-dash-review-analysis-hunk-entries by-hunk hunks)
    (seq-map
     (lambda (pair)
       (when-let* ((text (magit-dash-review-analysis-text (cdr pair)))
                   (line (car (magit-dash-review-analysis-span
                               (plist-get (car pair) :line) (plist-get (car pair) :count))))
                   (pos (funcall line-pos-function line))
                   ((<= pos (point-max)))
                   (end (save-excursion
                          (save-restriction
                            (widen)
                            (goto-char pos)
                            (line-end-position))))
                   (overlay (make-overlay end end nil t nil)))
         (overlay-put overlay 'magit-dash-review-analysis t)
         (overlay-put overlay 'after-string (concat " " text))
         (when (bound-and-true-p diff-review-file-mode)
           (push overlay diff-review--overlays))
         overlay)))
    (seq-filter #'identity)))

(defvar magit-dash-review-analysis--warned nil
  "Non-nil once an advice failure was reported, so it is reported once.")

(defun magit-dash-review-analysis--warn (err)
  "Report the advice failure ERR once per session, never signalling."
  (unless magit-dash-review-analysis--warned
    (setq magit-dash-review-analysis--warned t)
    (message "magit-dash-review-analysis: decoration failed (%s); diff-review is unaffected"
             (error-message-string err))))

(defun magit-dash-review-analysis--decorate-advice (&rest _)
  "Add analysis overlays after diff-review decorates a file buffer.
Never signals: a failure is reported once and diff-review carries on."
  (condition-case err
      (when-let* (((fboundp 'diff-review--review-buffer))
                  (review (diff-review--review-buffer))
                  (path diff-review--file-path)
                  (table (buffer-local-value 'diff-review--data review))
                  (by-hunk (magit-dash-review-analysis-for-path table path)))
        (magit-dash-review-analysis-decorate-buffer by-hunk diff-review--hunks
                                                    #'diff-review--line-pos))
    (error (magit-dash-review-analysis--warn err) nil)))

(defun magit-dash-review-analysis--file-line-advice (fn i indent)
  "Append the file's analysis to the list line FN renders for file I.
FN is `diff-review--file-line'; INDENT is passed through.  Never signals:
on a failure the original line is returned and the failure reported once."
  (let ((line (funcall fn i indent)))
    (condition-case err
        (or (when-let* ((review (diff-review--review-buffer))
                        (path (plist-get (diff-review--file i) :path))
                        (table (buffer-local-value 'diff-review--data review))
                        (text (magit-dash-review-analysis-file-text
                               (magit-dash-review-analysis-for-path table path)))
                        ((string-suffix-p "\n" line)))
              (let ((new (concat (substring line 0 -1) " " text "\n")))
                (seq-do (lambda (prop)
                          (when-let* ((value (get-text-property 0 prop line)))
                            (put-text-property 0 (length new) prop value new)))
                        '(diff-review-index keymap follow-link mouse-face help-echo))
                new))
            line)
      (error (magit-dash-review-analysis--warn err) line))))

(defun magit-dash-review-analysis--redraw ()
  "Redraw every open diff-review buffer and its file buffers."
  (seq-do (lambda (buffer)
            (with-current-buffer buffer
              (when (and (derived-mode-p 'diff-review-mode)
                         (fboundp 'diff-review--render-all))
                (diff-review--render-all)
                (diff-review--redecorate-files))))
          (buffer-list)))

;;;###autoload
(define-minor-mode magit-dash-review-analysis-mode
  "Show code-review analysis from the review JSON in diff-review.
Only displays what is already in code_review.analysis; use
`magit-dash-review-analysis-refresh' to compute it.  Turning the mode off
removes the decorations.  Enabling signals a `user-error' (and leaves the
mode off) when diff-review lacks the functions this advises."
  :global t
  :group 'magit-dash-review-analysis
  (cond
   (magit-dash-review-analysis-mode
    (require 'diff-review)
    (unless (and (fboundp 'diff-review--decorate)
                 (fboundp 'diff-review--file-line)
                 (fboundp 'diff-review--review-buffer)
                 (fboundp 'diff-review--file)
                 (fboundp 'diff-review--line-pos)
                 (equal (func-arity 'diff-review--file-line) '(2 . 2)))
      (setq magit-dash-review-analysis-mode nil)
      (user-error "magit-dash-review-analysis: diff-review %s lacks the internals this mode decorates (diff-review--decorate, diff-review--file-line/2)"
                  (if (boundp 'diff-review-version) diff-review-version "?")))
    (advice-add 'diff-review--decorate :after #'magit-dash-review-analysis--decorate-advice)
    (advice-add 'diff-review--file-line :around #'magit-dash-review-analysis--file-line-advice)
    (setq magit-dash-review-analysis--warned nil)
    (magit-dash-review-analysis--redraw))
   (t
    (advice-remove 'diff-review--decorate #'magit-dash-review-analysis--decorate-advice)
    (advice-remove 'diff-review--file-line #'magit-dash-review-analysis--file-line-advice)
    (seq-do (lambda (buffer)
              (with-current-buffer buffer
                (when (bound-and-true-p diff-review-file-mode)
                  (magit-dash-review-analysis-clear-buffer))))
            (buffer-list))
    (magit-dash-review-analysis--redraw))))

;;; Explicit, asynchronous refresh

(defvar magit-dash-review-analysis--processes (make-hash-table :test #'equal)
  "Review file to its in-flight analysis child process.")

(defun magit-dash-review-analysis-write (file analysis)
  "Store ANALYSIS, a hash table, as code_review.analysis in review FILE.
Replaces any analysis there.  Uses the adapter's reader and writer: a
modified visiting buffer is handled by
`magit-dash-review-json-modified-buffer-action', and a corrupt FILE (only
its backup parses) is never overwritten: `user-error', no prompt, so it is
safe from a sentinel.  Return nil when ANALYSIS is empty (nothing written),
`written' when written and the visiting buffer, if any, is current, and
`buffer-skipped' when written but a modified visiting buffer was left alone."
  (when (and (hash-table-p analysis) (> (map-length analysis) 0))
    (magit-dash-review-json-save-visiting-buffer file)
    (let* ((data (magit-dash-review-json-read-file file))
           (code-review (if (hash-table-p (map-elt data "code_review"))
                            (map-elt data "code_review")
                          (make-hash-table :test #'equal))))
      (when magit-dash-review-json-last-read-fallback
        (user-error "%s is corrupt (only its backup parses); not overwriting it" file))
      (setf (map-elt code-review "analysis") analysis)
      (setf (map-elt data "code_review") code-review)
      (magit-dash-review-json-write-file file data)
      (if (magit-dash-review-json-refresh-visiting-buffer file)
          'written
        'buffer-skipped))))

(defcustom magit-dash-review-analysis-timeout 600
  "Seconds after which a running analysis child Emacs is killed."
  :type 'natnum
  :group 'magit-dash-review-analysis)

(defun magit-dash-review-analysis--log (process)
  "Return the trimmed output of PROCESS's log buffer."
  (let ((log (process-buffer process)))
    (if (buffer-live-p log)
        (string-trim (with-current-buffer log (buffer-string)))
      "")))

(defun magit-dash-review-analysis--timeout (process)
  "Kill PROCESS, a child whose time ran out."
  (when (process-live-p process)
    (process-put process 'magit-dash-review-analysis-reason "timed out")
    (kill-process process)))

(defun magit-dash-review-analysis--sentinel (process _event)
  "Store the analysis the child Emacs PROCESS produced, once it exits."
  (when (memq (process-status process) '(exit signal))
    (let ((file (process-get process 'magit-dash-review-analysis-file))
          (output (process-get process 'magit-dash-review-analysis-output))
          (reason (process-get process 'magit-dash-review-analysis-reason))
          (timer (process-get process 'magit-dash-review-analysis-timer)))
      (unwind-protect
          (progn
            (when (timerp timer)
              (cancel-timer timer))
            (when (eq (map-elt magit-dash-review-analysis--processes file) process)
              (map-delete magit-dash-review-analysis--processes file))
            (cond
             (reason
              (message "magit-dash-review-analysis: analysis for %s %s" file reason))
             ((not (and (eq (process-status process) 'exit) (zerop (process-exit-status process))))
              (message "magit-dash-review-analysis: child Emacs failed for %s: %s" file
                       (magit-dash-review-analysis--log process)))
             (t
              (pcase (condition-case err
                         (magit-dash-review-analysis-write
                          file (magit-dash-review-json-read-file output))
                       (error
                        (message "magit-dash-review-analysis: could not store analysis in %s: %s"
                                 file (error-message-string err))
                        'failed))
                ('written
                 (message "magit-dash-review-analysis: analysis stored in %s" file))
                ('buffer-skipped
                 (message "magit-dash-review-analysis: analysis stored in %s; its buffer has unsaved edits and was not refreshed" file))
                ('failed nil)
                (_
                 (message "magit-dash-review-analysis: no analysis produced for %s (child output: %s)"
                          file (magit-dash-review-analysis--log process)))))))
        (when (buffer-live-p (process-buffer process))
          (kill-buffer (process-buffer process)))
        (ignore-errors (delete-file output))))))

(defun magit-dash-review-analysis-child-command (pr-key output)
  "Return the argv of the child Emacs that writes PR-KEY's analysis to OUTPUT.
The child exports with analysis from an empty source so a stale analysis in
the file is not reused, and writes only code_review.analysis as JSON.  The
--eval form is printed with the print limits off, so a long `load-path'
is not truncated."
  (append
   (list (expand-file-name invocation-name invocation-directory) "--batch" "-Q")
   (seq-mapcat (lambda (file) (list "-l" (expand-file-name file)))
               magit-dash-review-analysis-child-init-files)
   (list "--eval"
         (let ((print-length nil)
               (print-level nil)
               (print-circle nil))
           (format "%S"
                   `(progn
                      (setq load-path ',load-path)
                      (require 'magit-dash-review-json)
                      (let* ((magit-dash-review-json-source-function #'ignore)
                             (table (magit-dash-review-json-export ',pr-key t))
                             (analysis (map-elt (map-elt table "code_review") "analysis")))
                        (with-temp-file ,output
                          (insert (decode-coding-string
                                   (json-serialize (or analysis (make-hash-table :test #'equal))
                                                   :null-object nil :false-object :false)
                                   'utf-8))))))))))

(defun magit-dash-review-analysis-cancel (&optional file)
  "Kill the analysis child running for review FILE, if any."
  (interactive
   (list (completing-read "Cancel analysis for: "
                          (map-keys magit-dash-review-analysis--processes) nil t)))
  (if-let* ((process (map-elt magit-dash-review-analysis--processes file))
            ((process-live-p process)))
      (progn
        (process-put process 'magit-dash-review-analysis-reason "was cancelled")
        (kill-process process))
    (message "magit-dash-review-analysis: no analysis running for %s" file)))

;;;###autoload
(defun magit-dash-review-analysis-refresh (&optional file)
  "Compute code-review analysis for the review FILE and store it in the file.
FILE defaults to the file of the current diff-review buffer.  The analysis
runs in a throwaway child Emacs because it is synchronous and can take
seconds; this command returns the process at once and a message reports the
outcome.  A child still running for the same FILE is replaced, and any child
is killed after `magit-dash-review-analysis-timeout' seconds.  The child
runs with -Q, so unless code-review is on `load-path' it needs
`magit-dash-review-analysis-child-init-files'.  Display needs
`magit-dash-review-analysis-mode'."
  (interactive
   (list (or (and (fboundp 'diff-review--review-buffer)
                  (when-let* ((review (diff-review--review-buffer)))
                    (buffer-file-name review)))
             buffer-file-name
             (user-error "No review file for this buffer"))))
  (unless (or magit-dash-review-analysis-child-init-files
              (fboundp 'code-review-api-analysis)
              (locate-library "code-review-api-analysis"))
    (user-error "The child Emacs cannot find code-review: put it on `load-path' or set `magit-dash-review-analysis-child-init-files'"))
  (let* ((table (magit-dash-review-json-read-file file))
         (pr-key (or (magit-dash-review-json-pr-key table)
                     (user-error "%s has no magit_dash repo and number" file)))
         (previous (map-elt magit-dash-review-analysis--processes file))
         (output nil)
         (buffer nil)
         (process nil))
    (when (process-live-p previous)
      (process-put previous 'magit-dash-review-analysis-reason "was replaced by a newer run")
      (kill-process previous))
    (unwind-protect
        (progn
          (setq output (make-temp-file "magit-dash-analysis-" nil ".json"))
          (setq buffer (generate-new-buffer " *magit-dash-review-analysis*"))
          (setq process (make-process
                         :name "magit-dash-review-analysis"
                         :buffer buffer
                         :command (magit-dash-review-analysis-child-command pr-key output)
                         :noquery t
                         :connection-type 'pipe
                         :sentinel #'magit-dash-review-analysis--sentinel)))
      (unless process
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (when output
          (ignore-errors (delete-file output)))))
    (process-put process 'magit-dash-review-analysis-file file)
    (process-put process 'magit-dash-review-analysis-output output)
    (process-put process 'magit-dash-review-analysis-timer
                 (run-at-time magit-dash-review-analysis-timeout nil
                              #'magit-dash-review-analysis--timeout process))
    (setf (map-elt magit-dash-review-analysis--processes file) process)
    (message "magit-dash-review-analysis: computing analysis for %s#%s in the background"
             (car pr-key) (cdr pr-key))
    process))

(provide 'magit-dash-review-analysis)
;;; magit-dash-review-analysis.el ends here
