;;; denote-mcp-plan.el --- Mutate denote plan notes during execution -*- lexical-binding: t; -*-

;;; Commentary:
;; Helper for the denote-plan skill.  denote-plan ships
;; denote-mcp-common.el for *creating* a plan note correctly in one call;
;; this file is the equivalent for the routine mutations denote-plan
;; makes to a plan note while executing it: flipping a task's TODO state
;; (and keeping the "* Execution Plan" table in sync), appending an
;; in-flight design-decision or resolved-question note under a heading,
;; and updating the top-of-file #+execution_status:/#+started:/#+finished:
;; directives.
;;
;; Before this file existed, each of these mutations was hand-rolled as a
;; one-off /tmp/*.el script per call site, and the Execution Plan table's
;; Status column was kept in sync via ad hoc regex substitution against
;; the table text -- independent of the heading TODO state. That regex
;; broke twice in one session: once greedy enough to flip unrelated rows,
;; once too narrow to match rows whose dependency-column link text/spacing
;; varied. `denote-mcp-plan-set-status' fixes this class of bug by
;; construction: it sets the heading TODO state, then re-derives the
;; table's Status cell from THAT heading via its own :CUSTOM_ID:, so the
;; heading is always the single source of truth and no regex has to
;; reason about link text at all.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-table)
(require 'cl-lib)
(require 'map)

(require 'denote-mcp-common)

;;; ─── Execution Plan table sync ──────────────────────────────────────────────

(defun denote-mcp-plan--table-bounds ()
  "Return (BEG . END) of the \"* Execution Plan\" table in the current buffer.
Point is left undefined; search always starts from `point-min'."
  (save-excursion
    (goto-char (point-min))
    (let (table-bounds)
      (org-map-entries
       (lambda ()
         (when (and (not table-bounds)
                    (equal (org-get-heading t t t t) "Execution Plan"))
           (save-restriction
             (org-narrow-to-subtree)
             (let ((tbl (org-element-map (org-element-parse-buffer) 'table
                          #'identity nil t)))
               (when tbl
                 (setq table-bounds (cons (org-element-property :begin tbl)
                                          (org-element-property :end tbl))))))))
       nil 'file)
      (or table-bounds
          (error "No table found under \"* Execution Plan\"")))))

(defun denote-mcp-plan--status-column (table-beg)
  "Return the 1-based column index of the Status column in the table at TABLE-BEG."
  (save-excursion
    (goto-char table-beg)
    (let* ((header (buffer-substring (line-beginning-position) (line-end-position)))
           (cells (mapcar #'string-trim
                           (split-string header "|" t "[ \t]+")))
           (idx (cl-position "Status" cells :test #'string-equal)))
      (unless idx
        (error "No \"Status\" column found in the Execution Plan table header"))
      (1+ idx))))

;;;###autoload
(defun denote-mcp-plan-set-status (file ids status)
  "Set STATUS on every task heading in FILE whose :CUSTOM_ID: (or :ID:) is in IDS.
IDS is a single id string or a list of them.  STATUS is an org TODO
keyword (\"INPROGRESS\", \"DONE\", \"BLOCKED\", \"CANCELED\", ...).

The heading is the source of truth: the Execution Plan table's Status
cells are re-derived from the headings' ids, never regex-edited.
Trackers have no table, which is skipped silently.  Returns the absolute
file path."
  (denote-mcp-plan-update
   file :transitions (mapcar (lambda (id) (cons id status))
                             (if (listp ids) ids (list ids)))))

;;; ─── In-flight notes ─────────────────────────────────────────────────────────

(defun denote-mcp-plan--append-note-here (heading-target text)
  "Insert TEXT as a paragraph under the heading matching HEADING-TARGET.
HEADING-TARGET may be a string heading title or a regexp.
Operates on the current buffer without saving."
  (save-excursion
    (let (found)
      (org-map-entries
       (lambda ()
         (when (and (not found)
                    (let ((title (org-get-heading t t t t))
                          (line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                      (or (equal title heading-target)
                          (and (stringp heading-target)
                               (or (string-match-p heading-target title)
                                   (string-match-p heading-target line))))))
           (setq found (point))))
       nil 'file)
      (unless found
        (error "Heading matching %S not found" heading-target))
      (goto-char found)
      (org-back-to-heading t)
      (org-end-of-meta-data t)
      (unless (bolp) (insert "\n"))
      (unless (and (>= (point) (+ (point-min) 2))
                   (string= (buffer-substring-no-properties (- (point) 2) (point)) "\n\n"))
        (insert "\n"))
      (insert text)
      (unless (bolp) (insert "\n")))))

;;;###autoload
(defun denote-mcp-plan-append-note (file heading-regexp text)
  "Append TEXT as a paragraph under the heading in FILE matching HEADING-REGEXP.
HEADING-REGEXP is searched from `point-min' against the heading line
itself (e.g. \"^\\\\*\\\\* Two distinct activity gates\").  The note is
inserted after any PROPERTIES/LOGBOOK drawers immediately under the
heading, never between them, so it always renders as ordinary body
prose.  Use this to record an in-flight design decision, a
mid-implementation benchmark result, or a scope amendment discovered
during a task.  Prefer `denote-mcp-plan-update' when several
mutations belong together: it visits and saves once.

Returns the absolute file path."
  (denote-mcp-plan-update file :notes (list (cons heading-regexp text))))

;;; ─── Top-of-file execution status ───────────────────────────────────────────

(defun denote-mcp-plan--set-execution-status-here (status)
  "Set #+execution_status: to STATUS in the current buffer, stamping timestamps.
Stamps #+started: the first time STATUS is \"in-progress\" and #+finished:
the first time it is \"complete\"; an already stamped directive is left
alone, so repeated calls are idempotent.  Does not save."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+execution_status:.*$" nil t)
        (replace-match (concat "#+execution_status: " status))
      (error "No #+execution_status: directive found"))
    (goto-char (point-min))
    (cond
     ((string= status "in-progress")
      (when (re-search-forward "^#\\+started:\\s-*$" nil t)
        (replace-match (concat "#+started: " (format-time-string "[%Y-%m-%d %a %H:%M]")))))
     ((string= status "complete")
      (when (re-search-forward "^#\\+finished:\\s-*$" nil t)
        (replace-match (concat "#+finished: " (format-time-string "[%Y-%m-%d %a %H:%M]"))))))))

;;;###autoload
(defun denote-mcp-plan-set-execution-status (file status)
  "Set #+execution_status: in FILE to STATUS (\"proposed\", \"in-progress\",
\"complete\", \"blocked\", or \"abandoned\").  Stamps #+started: and
#+finished: once; see `denote-mcp-plan--set-execution-status-here'.

Returns the absolute file path."
  (denote-mcp-plan-update file :execution-status status))

;;; ─── Batched update: one visit, one parse, one save ─────────────────────────

(defun denote-mcp-plan--fix-drawers-maybe ()
  "Normalize inline property drawers, but only when one is present."
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward "^\\*+ .*[ \t]:PROPERTIES:" nil t))
    (denote-mcp-common--normalize-property-drawers)))

(defun denote-mcp-plan--id-table ()
  "Return a hash of :CUSTOM_ID:/:ID: value -> marker at its heading.
One scan of the buffer, so any number of lookups costs a single parse."
  (let ((tbl (make-hash-table :test #'equal)))
    (org-map-entries
     (lambda ()
       (when-let* ((id (or (org-entry-get (point) "CUSTOM_ID")
                           (org-entry-get (point) "ID"))))
         (unless (map-elt tbl id)
           (setf (map-elt tbl id) (point-marker)))))
     nil 'file)
    tbl))

(defun denote-mcp-plan--apply-transitions (transitions)
  "Apply TRANSITIONS, a list of (ID . STATUS), to task headings.
Every id is resolved before any heading changes, so a typo cannot leave
the note half updated.  Re-derive the Execution Plan table's Status
cells from the headings afterwards.  Return the number applied."
  (let ((tbl (denote-mcp-plan--id-table)))
    (dolist (tr transitions)
      (unless (map-elt tbl (car tr))
        (error "ID %S not found" (car tr))))
    (dolist (tr transitions)
      (goto-char (map-elt tbl (car tr)))
      (org-todo (cdr tr)))
    (ignore-errors (org-update-statistics-cookies t))
    (denote-mcp-plan--sync-table transitions)
    (length transitions)))

(defun denote-mcp-plan--sync-table (transitions)
  "Write each (ID . STATUS) of TRANSITIONS into the Execution Plan table.
Rows are matched by the Task column's link target only, so a Dependencies
link back to a changed task never overwrites that row.  Edit the row text
directly and align once; do nothing when the note has no table."
  (when-let* ((bounds (ignore-errors (denote-mcp-plan--table-bounds))))
    (let* ((col (denote-mcp-plan--status-column (car bounds)))
           (want (make-hash-table :test #'equal))
           (beg (copy-marker (car bounds)))
           (end (copy-marker (cdr bounds)))
           (changed nil))
      (dolist (tr transitions)
        (setf (map-elt want (car tr)) (cdr tr)))
      (save-excursion
        (goto-char beg)
        (forward-line 2)                ; header row and separator
        (while (< (point) end)
          (let* ((cells (split-string
                         (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))
                         "|"))
                 (task (nth 1 cells)))
            (when (and task (> (length cells) (1+ col))
                       (string-match "#\\([^]]+\\)\\]" task))
              (when-let* ((st (map-elt want (match-string 1 task))))
                (setf (nth col cells) (format " %s " st))
                (delete-region (line-beginning-position) (line-end-position))
                (insert (mapconcat #'identity cells "|"))
                (setq changed t))))
          (forward-line 1))
        (when changed
          (goto-char beg)
          (let ((buffer-invisibility-spec nil)
                (org-link-descriptive nil))
            (org-table-align)))))))

(defun denote-mcp-plan--add-decision-here (title text &optional status under)
  "Record a decision TITLE with body TEXT in the current buffer.
Without UNDER, append an entry below `* Decisions' (created before
`* Open Questions' when absent) with a :STATUS: property, default
ACCEPTED.  With UNDER, a heading regexp, add it as a dated paragraph
under that heading instead.  Does not save."
  (let ((timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (if under
        (denote-mcp-plan--append-note-here
         under (format "*Decision %s: %s* %s" timestamp title text))
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward "^\\* Decisions[ \t]*$" nil t)
          (goto-char (point-max))
          (if (re-search-backward "^\\* Open Questions[ \t]*$" nil t)
              (beginning-of-line)
            (goto-char (point-max)))
          (unless (bolp) (insert "\n"))
          (insert "\n* Decisions\n"))
        (goto-char (if (re-search-forward "^\\* [^*]" nil t)
                       (match-beginning 0)
                     (point-max)))
        (unless (bolp) (insert "\n"))
        (insert (format "** %s %s\n:PROPERTIES:\n:STATUS: %s\n:END:\n%s"
                        timestamp title (or status "ACCEPTED") text))
        (unless (string-suffix-p "\n" text) (insert "\n"))))))

;;;###autoload
(defun denote-mcp-plan-add-decision (file title text &optional status under)
  "Record a decision in FILE; see `denote-mcp-plan--add-decision-here'.
Works on plans and trackers alike.  Returns the absolute file path."
  (denote-mcp-plan-update
   file :decisions (list (list title text status under))))

(defun denote-mcp-plan--count-tasks ()
  "Return (TOTAL . OPEN) counts of task headings in the current buffer.
Question headings (QUESTION, ANSWERED, IGNORED or tagged :question:) are
not tasks."
  (let ((total 0) (open 0))
    (org-map-entries
     (lambda ()
       (when-let* ((kw (org-get-todo-state)))
         (unless (or (member kw '("QUESTION" "ANSWERED" "IGNORED"))
                     (member "question" (org-get-tags nil t)))
           (cl-incf total)
           (unless (member kw org-done-keywords)
             (cl-incf open))))))
    (cons total open)))

(defun denote-mcp-plan--plan-p (file)
  "Return non-nil when FILE carries the denote keyword `plan'."
  (member "plan" (denote-retrieve-filename-keywords-as-list file)))

(cl-defun denote-mcp-plan-update
    (file &key transitions execution-status decisions answers notes mark-executed)
  "Apply any combination of mutations to FILE with one visit and one save.
This is the primitive behind the `write_plan_update' MCP tool.  Order:
TRANSITIONS, NOTES, DECISIONS, ANSWERS, EXECUTION-STATUS.

TRANSITIONS: list of (ID . STATUS), see
`denote-mcp-plan--apply-transitions'.
NOTES: list of (HEADING-REGEXP . TEXT).
DECISIONS: list of (TITLE TEXT [STATUS [UNDER]]).
ANSWERS: list of (QUESTION-TITLE ANSWER-TEXT [NEW-STATE]).
EXECUTION-STATUS: a string for #+execution_status:.
MARK-EXECUTED: non-nil asks for the `x' filetag when TRANSITIONS leave a
plan (not a tracker, whose items accumulate over time) with every task
done.

Return a plist (:file :applied :total :open :all-done :marked)."
  (let (applied counts marked path)
    (with-current-buffer (find-file-noselect file)
      (denote-mcp-plan--fix-drawers-maybe)
      (when transitions
        (setq applied (denote-mcp-plan--apply-transitions transitions)))
      (dolist (n notes)
        (denote-mcp-plan--append-note-here (car n) (cdr n)))
      (dolist (d decisions)
        (apply #'denote-mcp-plan--add-decision-here d))
      (dolist (a answers)
        (apply #'denote-mcp-common--answer-here a))
      (when execution-status
        (denote-mcp-plan--set-execution-status-here execution-status))
      (setq counts (denote-mcp-plan--count-tasks))
      (when (buffer-modified-p)
        (let ((save-silently t)) (save-buffer)))
      (setq path (buffer-file-name)))
    (let ((all-done (and (> (car counts) 0) (zerop (cdr counts)))))
      (when (and mark-executed transitions all-done
                 (denote-mcp-plan--plan-p path))
        (setq marked t
              path (or (plist-get (denote-mcp--mark-executed path) :path) path)))
      (list :file path :applied (or applied 0)
            :total (car counts) :open (cdr counts)
            :all-done all-done :marked marked))))

;;; ─── Execution Plan rounds and ready tasks ──────────────────────────────────

;;;###autoload
(defun denote-mcp-plan-next-tasks (file)
  "Return the Execution Plan state of FILE as a plist.
Statuses come from the task headings (the source of truth), while the
table supplies order and Dependencies.  A task is ready when its status is
not done and every dependency is done.  Result keys: :ready (list of
\(:id :title :status)), :rounds (list of (:round N :ids (..))) where a
task's round is one more than its latest dependency's, and :counts (alist
of status to number)."
  (with-current-buffer (find-file-noselect file)
    (let* ((bounds (denote-mcp-plan--table-bounds))
           (status (make-hash-table :test #'equal))
           (title (make-hash-table :test #'equal))
           (deps (make-hash-table :test #'equal))
           (order nil))
      ;; Heading state: one scan.
      (org-map-entries
       (lambda ()
         (when-let* ((id (or (org-entry-get (point) "CUSTOM_ID")
                             (org-entry-get (point) "ID"))))
           (setf (map-elt status id) (or (org-get-todo-state) ""))
           (setf (map-elt title id) (org-get-heading t t t t))))
       nil 'file)
      ;; Table: order and dependencies.
      (save-excursion
        (goto-char (car bounds))
        (forward-line 2)
        (while (< (point) (cdr bounds))
          (let* ((cells (split-string
                         (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))
                         "|"))
                 (task (nth 1 cells))
                 (dep (or (nth 2 cells) "")))
            (when (and task (string-match "#\\([^]]+\\)\\]" task))
              (let ((id (match-string 1 task)) (ds nil) (start 0))
                (while (string-match "#\\([^]]+\\)\\]" dep start)
                  (push (match-string 1 dep) ds)
                  (setq start (match-end 0)))
                (push id order)
                (setf (map-elt deps id) (nreverse ds)))))
          (forward-line 1)))
      (setq order (nreverse order))
      (let ((round (make-hash-table :test #'equal))
            (done (lambda (id) (member (map-elt status id) org-done-keywords))))
        (cl-labels ((round-of (id)
                      (or (map-elt round id)
                          (progn
                            ;; Seed so a dependency cycle terminates.
                            (setf (map-elt round id) 1)
                            (setf (map-elt round id)
                                  (1+ (apply #'max 0 (mapcar #'round-of
                                                             (map-elt deps id)))))
                            (map-elt round id)))))
          (let ((rounds nil) (ready nil) (counts nil))
            (dolist (id order)
              (let ((r (round-of id)) (st (or (map-elt status id) "")))
                (if-let* ((cell (assq r rounds)))
                    (setcdr cell (append (cdr cell) (list id)))
                  (setq rounds (append rounds (list (list r id)))))
                (cl-incf (alist-get st counts 0 nil #'equal))
                (when (and (not (funcall done id))
                           (cl-every done (map-elt deps id)))
                  (push (list :id id :title (or (map-elt title id) "") :status st)
                        ready))))
            (list :ready (nreverse ready)
                  :rounds (mapcar (lambda (c) (list :round (car c) :ids (cdr c)))
                                  (sort rounds (lambda (a b) (< (car a) (car b)))))
                  :counts counts)))))))

;;; ─── Closing and finalization ───────────────────────────────────────────────

(declare-function orgx-mcp-close-task "orgx-mcp" (plan-file))

;;;###autoload
(defun denote-mcp-plan-sweep-open-questions (file)
  "Move any remaining open QUESTION or IGNORED headings in FILE to ANSWERED."
  (with-current-buffer (find-file-noselect file)
    (org-map-entries
     (lambda ()
       (when (member (org-get-todo-state) '("QUESTION" "IGNORED"))
         (org-todo "ANSWERED")
         (save-restriction
           (org-narrow-to-subtree)
           (let* ((log-drawer (org-element-map (org-element-parse-buffer) 'drawer
                                (lambda (d)
                                  (when (equal (org-element-property :drawer-name d) "LOGBOOK")
                                    d))
                                nil t))
                  (log-entry (format "- State \"ANSWERED\" from \"QUESTION\" %s \\\\\n  Resolved upon plan completion.\n"
                                     (format-time-string "[%Y-%m-%d %a %H:%M]"))))
             (if log-drawer
                 (progn
                   (goto-char (org-element-property :contents-begin log-drawer))
                   (insert log-entry))
               (org-end-of-meta-data nil)
               (insert ":LOGBOOK:\n" log-entry ":END:\n"))))))
     nil 'file)
    (save-buffer)))

;;;###autoload
(defun denote-mcp-plan-close-plan (file)
  "Mark the plan note at FILE complete, executed, and close its agentic.org task.
Resolves any remaining open questions to ANSWERED, sets
`#+execution_status: complete', closes the corresponding TODO
heading in `agentic.org' via `orgx-mcp-close-task', and marks
the note executed via `denote-mcp--mark-executed' (which appends `:x:' to the
filetags and renames the file).

NOTE: In the denote-plan workflow, agents must ALWAYS ask the user
for explicit confirmation before calling this function or executing
these closeout steps. Marking a plan complete renames the note file,
updates document status, and primarily modifies `agentic.org' by
marking the standing TODO as DONE on the user's active agenda.

Returns the absolute path of the renamed plan file."
  (denote-mcp-plan-sweep-open-questions file)
  (denote-mcp-plan-set-execution-status file "complete")
  (when (fboundp 'orgx-mcp-close-task)
    (orgx-mcp-close-task file))
  (or (plist-get (denote-mcp--mark-executed file) :path) file))

;;; ─── Resolving which plan to resume ──────────────────────────────────────────

;;;###autoload
(defun denote-mcp-plan-resolve-note (identifier tags last-created kind)
  "Resolve the KIND note (e.g. \"plan\") to act on from IDENTIFIER.
IDENTIFIER may be:
- nil: prefer LAST-CREATED (the file this session created most recently)
  when it still exists; otherwise the most recently modified note
  tagged with TAGS, via `denote-mcp--find-notes'.
- a denote sequence signature (e.g. \"7a11c\"):
  `denote-mcp-common--find-by-sequence'.
- an absolute file path: returned as-is if it exists.
- a slug fragment: `denote-mcp--find-file'.
Signals an error if nothing resolves, rather than guessing.  Shared by
plans and trackers."
  (cond
   ((and (stringp identifier) (file-exists-p identifier))
    identifier)
   ((and identifier (denote-mcp-common--find-by-sequence identifier)))
   ((and identifier (denote-mcp--find-file identifier)))
   ((and (not identifier) last-created (file-exists-p last-created))
    last-created)
   ((not identifier)
    (or (when-let* ((note (car (denote-mcp--find-notes :tags tags :sort 'recent))))
          (denote-mcp--note-path note))
        (error "No %s note found in the denote directory" kind)))
   (t (error "Could not resolve a %s note for %S" kind identifier))))

;;;###autoload
(defun denote-mcp-plan-resolve-target (&optional identifier)
  "Resolve the plan note to resume from IDENTIFIER.
See `denote-mcp-plan-resolve-note'."
  (denote-mcp-plan-resolve-note
   identifier '("agent" "plan")
   (and (boundp 'denote-mcp-common--last-created-file) denote-mcp-common--last-created-file)
   "agent+plan"))

(provide 'denote-mcp-plan)
;;; denote-mcp-plan.el ends here
