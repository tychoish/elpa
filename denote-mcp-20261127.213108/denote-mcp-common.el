;;; denote-mcp-common.el --- Plan note helpers: frontmatter, drawers, questions -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers shared by the typed-note tools in `denote-mcp-notes': the plan
;; frontmatter template, property-drawer repair, sequence placement and
;; open-question answering.  Note creation itself lives in `denote-mcp-notes'.

;;; Code:

(require 'org)
(require 'org-element)
(require 'denote)
(require 'denote-mcp)

;; Plan skeleton: top-of-file frontmatter directives carrying execution status,
;; creation timestamp, and start/finish date stamps (#+execution_status:,
;; #+created_at:, #+started:, #+finished:).
;; The body (Overview, Goal, Tasks, Execution Plan, ...) is appended after
;; this by the helpers below.
(defun denote-mcp-common--plan-template ()
  "Generate initial frontmatter directives for a plan note."
  (format "#+execution_status: proposed\n#+created_at: %s\n#+started:\n#+finished:\n\n"
          (format-time-string "[%Y-%m-%d %a %H:%M]")))

(setq denote-templates (assq-delete-all 'plan denote-templates))
(add-to-list 'denote-templates
             '(plan . denote-mcp-common--plan-template))

(defun denote-mcp-common--normalize-frontmatter ()
  "Tidy the plan frontmatter in the current buffer.
Ensure a `#+signature:' line follows `#+identifier:' (empty when the
note has no sequence) and drop blank lines before `#+execution_status:'."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+identifier:.*\n" nil t)
      (unless (looking-at-p "#\\+signature:")
        (insert "#+signature:\n"))
      (when (re-search-forward "^#\\+execution_status:" nil t)
        (beginning-of-line)
        (while (and (not (bobp))
                    (save-excursion (forward-line -1) (looking-at-p "^[ \t]*$")))
          (forward-line -1)
          (delete-line))))))

(defun denote-mcp-common--normalize-property-drawers ()
  "Fix malformed inline Org property drawers on headings.
Converts lines like:
  ** TODO [#A] Title :PROPERTIES: :CUSTOM_ID: slug :END:
into proper multiline Org property drawers:
  ** TODO [#A] Title
  :PROPERTIES:
  :CUSTOM_ID: slug
  :END:"
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (when (string-match "\\`\\(\\*+.*?\\)[ \t]+:PROPERTIES:[ \t]*\\(.*?\\)[ \t]*:END:[ \t]*\\'" line)
          (let* ((heading (match-string 1 line))
                 (props-raw (match-string 2 line))
                 (tokens (split-string props-raw "[ \t]+" t))
                 (props nil)
                 (cur-k nil)
                 (cur-v nil))
            (dolist (tok tokens)
              (if (and (string-prefix-p ":" tok) (string-suffix-p ":" tok))
                  (progn
                    (when cur-k
                      (push (format "%s %s" cur-k (string-join (nreverse cur-v) " ")) props))
                    (setq cur-k tok cur-v nil))
                (push tok cur-v)))
            (when cur-k
              (push (format "%s %s" cur-k (string-join (nreverse cur-v) " ")) props))
            (delete-region (line-beginning-position) (line-end-position))
            (insert heading "\n:PROPERTIES:\n"
                    (mapconcat #'identity (nreverse props) "\n")
                    "\n:END:"))))
      (forward-line 1))))

(defvar denote-mcp-common--last-created-file nil
  "Absolute path of the most recently created plan/report note in this
Emacs session. Set by `denote-mcp-notes-create'. Read by
`denote-mcp-plan-resolve-target' (denote-plan skill) so an
argument-less denote-plan invocation can resume the plan it (or a
prior step in the same session) just created, instead of re-deriving
the file path from scratch via `ls'/`grep'.")

(defvar denote-mcp-common-directory
  (cond
   ((and (boundp 'denote-directory) (listp denote-directory))
    (file-name-as-directory (expand-file-name (car denote-directory))))
   ((and (boundp 'denote-directory) (stringp denote-directory))
    (file-name-as-directory (expand-file-name denote-directory)))
   ((fboundp 'denote-directory)
    (file-name-as-directory (expand-file-name (denote-directory))))
   (t (expand-file-name "~/denote/")))
  "Directory for agent-created plan and report notes.")

(defun denote-mcp-common--find-by-sequence (seq)
  "Return the path of the note whose signature is exactly SEQ, or nil."
  (when-let* ((note (car (denote-mcp--find-notes :sequence seq))))
    (denote-mcp--note-path note)))

(defun denote-mcp-common--next-child (parent)
  "Return the next free child signature under PARENT."
  (denote-mcp--with-scheme
    (denote-sequence-get-new 'child parent (denote-mcp--sequences))))

(defconst denote-mcp-common--project-tags
  '("casap" "asq" "sardis" "denote" "odem" "emacs" "singing" "politics" "fiction"
    "knitting" "essay")
  "Project tags that must agree between a parent note and its new child.")

(defun denote-mcp-common--validate-tags-congruent (ref-file new-tags)
  "Signal an error when REF-FILE and NEW-TAGS carry disjoint project tags."
  (let* ((ref (cl-intersection (denote-retrieve-filename-keywords-as-list ref-file)
                               denote-mcp-common--project-tags :test #'string=))
         (new (cl-intersection new-tags denote-mcp-common--project-tags :test #'string=)))
    (when (and ref new (not (cl-intersection ref new :test #'string=)))
      (error "Tag incongruence: reference note has project tags %S but new note has %S"
             ref new))))

(defun denote-mcp-common--place-in-sequence (file-path sequence-or-parent tags)
  "Place FILE-PATH in SEQUENCE-OR-PARENT and return its new path.
Nil SEQUENCE-OR-PARENT leaves the file alone.
A parent signature like \"7f\" gets the next child, after checking that the
parent carries TAGS; an unused exact signature is assigned as given."
  (if (not (and sequence-or-parent file-path))
      file-path
    (if-let* ((target-file (denote-mcp-common--find-by-sequence sequence-or-parent)))
        (progn
          (denote-mcp-common--validate-tags-congruent target-file tags)
          (denote-mcp--set-sequence
           file-path (denote-mcp-common--next-child sequence-or-parent)))
      (denote-mcp--set-sequence file-path sequence-or-parent))))

;;; Open Questions

;; Answers are logged in the heading's :LOGBOOK: drawer (created when absent)
;; so they never land in body prose.

(defun denote-mcp-common--answer-here (question-title answer-text &optional new-state)
  "Answer QUESTION-TITLE in the current buffer with ANSWER-TEXT.
Switch the heading to NEW-STATE (default \"ANSWERED\") and log the
transition and answer in its :LOGBOOK: drawer.  Does not save."
  (let ((state (or new-state "ANSWERED"))
        found)
    (save-excursion
      (org-map-entries
       (lambda ()
         (when (and (not found)
                    (equal (org-get-heading t t t t) question-title))
           (setq found (point))))
       nil 'file)
      (unless found
        (error "Question heading %S not found" question-title))
      (goto-char found)
      (org-back-to-heading t)
      (org-todo state)
      (let ((log-entry (format "- State \"%-12s\" from \"QUESTION\"   %s \\\\\n  %s\n"
                               state (format-time-string "[%Y-%m-%d %a %H:%M]")
                               answer-text)))
        (save-restriction
          (org-narrow-to-subtree)
          (let ((log-drawer (org-element-map (org-element-parse-buffer) 'drawer
                              (lambda (d)
                                (when (equal (org-element-property :drawer-name d) "LOGBOOK")
                                  d))
                              nil t)))
            (if log-drawer
                (progn
                  (goto-char (org-element-property :contents-begin log-drawer))
                  (insert log-entry))
              (org-end-of-meta-data nil)
              (insert ":LOGBOOK:\n" log-entry ":END:\n"))))))
    state))

;;;###autoload
(defun denote-mcp-common-answer-questions (file-or-slug answers)
  "Apply ANSWERS to the questions in FILE-OR-SLUG with one visit and one save.
ANSWERS is a list of (QUESTION-TITLE ANSWER-TEXT [NEW-STATE]).
Returns the absolute file path."
  (let ((file (if (file-exists-p file-or-slug)
                  file-or-slug
                (denote-mcp--find-file file-or-slug))))
    (unless file
      (error "Could not find file for %S" file-or-slug))
    (with-current-buffer (find-file-noselect file)
      (dolist (a answers)
        (apply #'denote-mcp-common--answer-here a))
      (when (buffer-modified-p)
        (let ((save-silently t)) (save-buffer)))
      (buffer-file-name))))

;;;###autoload
(defun denote-mcp-common-answer-question (file-or-slug question-title answer-text &optional new-state)
  "Mark QUESTION-TITLE in FILE-OR-SLUG as answered with ANSWER-TEXT.
Logs the state transition and answer note into the heading's :LOGBOOK: drawer.
NEW-STATE defaults to \"ANSWERED\"."
  (denote-mcp-common-answer-questions
   file-or-slug (list (list question-title answer-text new-state))))

(provide 'denote-mcp-common)
;;; denote-mcp-common.el ends here
