;;; denote-mcp-tracker.el --- Create and mutate denote tracker notes -*- lexical-binding: t; -*-

;;; Commentary:
;; Helper for the with-tracker skill. A tracker is a denote note, tagged
;; (agent tracker), for an initiative shaped like many small, independently
;; actionable items discovered and resolved over a long session or several
;; sessions -- an adversarial code review, a bug-bash, a migration sweep --
;; as opposed to `denote-plan''s Plan format, which designs one bounded piece
;; of work up front and executes it in dependency-ordered rounds.
;;
;; A tracker's tasks are flatter and lighter-weight than a Plan's: each one
;; gets a short, stable, globally-unique :ID: (a kebab-case slug, not a
;; denote identifier and not necessarily a :CUSTOM_ID:) so it can be
;; addressed, retitled, re-parented under a different category, and
;; journaled against without ever searching for it by heading text. Tasks
;; are grouped under `**' category headings that are expected to be
;; re-examined and reshuffled as the tracker grows -- `denote-mcp-tracker-move'
;; exists specifically to relocate a task's subtree to a different category
;; without touching its :ID: or history.
;;
;; This file only implements what denote-plan do not already
;; cover. Open Questions use the exact same QUESTION/:question: convention
;; as a Plan, so `denote-mcp-common-answer-question' is reused unchanged --
;; see operations/decisions-questions.md.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)
(require 'denote-mcp-common)
(require 'denote-mcp-plan)

;; Tracker skeleton: same top-of-file execution_status/started/finished
;; directives as a Plan (so #+execution_status: proposed|in-progress|
;; complete|blocked|abandoned and `denote-mcp--mark-executed''s :x: filetag convention
;; both apply unchanged -- see skill://denote/operations/mark-executed.md),
;; plus an empty `* Tasks' heading ready to receive categories.
(setq denote-templates (assq-delete-all 'tracker denote-templates))
(add-to-list 'denote-templates
             '(tracker . "#+execution_status: proposed\n#+started:\n#+finished:\n\n"))

(defvar denote-mcp-tracker-directory denote-mcp-common-directory
  "Directory for agent-created tracker notes. Shares denote-plan's directory
resolution since both live in the same denote tree.")

(defvar denote-mcp-tracker--last-created-file nil
  "Absolute path of the most recently created tracker note in this Emacs
session. Read by `denote-mcp-tracker-resolve-target'.")

;;;###autoload
(defun denote-mcp-tracker-close (file)
  "Mark the tracker at FILE complete: closes its standing agentic.org
TODO (captured by `denote-mcp-notes-create') via
`orgx-mcp-close-task', on top of whatever
`operations/update.md''s closeout steps already do
(`#+execution_status: complete' and `denote-mcp--mark-executed'). Call this
exactly once, as the last closeout step, after every task in FILE is
`DONE'/`CANCELLED' and every `QUESTION' is `ANSWERED' -- see
`operations/reference.md' rules 4 and 5.

Returns the absolute path of agentic.org if a matching TODO was found
and closed, or nil if none was found (already closed, or the tracker
predates this function's introduction and was never captured)."
  (when (fboundp 'orgx-mcp-close-task)
    (orgx-mcp-close-task file)))

;;; ─── ID generation and lookup ────────────────────────────────────────────────

(defun denote-mcp-tracker--slugify (text)
  "Lowercase, hyphenate TEXT into a kebab-case slug fragment."
  (let* ((lower (downcase text))
         (hyphenated (replace-regexp-in-string "[^a-z0-9]+" "-" lower))
         (trimmed (string-trim hyphenated "-+" "-+")))
    trimmed))

(defun denote-mcp-tracker--existing-ids (file)
  "Return every :ID: value present on a task heading in FILE, as strings."
  (with-current-buffer (find-file-noselect file)
    (let (ids)
      (org-map-entries
       (lambda ()
         (when-let* ((id (org-entry-get (point) "ID")))
           (push id ids)))
       nil 'file)
      (nreverse ids))))

;;;###autoload
(defun denote-mcp-tracker-next-id (file proposed-slug)
  "Return a globally-unique task :ID: in FILE derived from PROPOSED-SLUG.
PROPOSED-SLUG is slugified first (lowercased, non-alphanumeric runs
collapsed to single hyphens). If the resulting slug already exists as
a task :ID: in FILE, numeric suffixes -2, -3, ... are tried until one
is free. Prefer a short, descriptive, domain-prefixed slug as input
(e.g. \"queue iterator eager pull leak\" -> \"queue-iterator-eager-pull-leak\")
over a generic one -- see operations/ids.md for the naming convention
this assumes."
  (let* ((base (denote-mcp-tracker--slugify proposed-slug))
         (existing (denote-mcp-tracker--existing-ids file))
         (candidate base)
         (n 2))
    (while (member candidate existing)
      (setq candidate (format "%s-%d" base n))
      (cl-incf n))
    candidate))

(defun denote-mcp-tracker--find-task (file id)
  "Move point in FILE's buffer to the heading whose :ID: is ID.
Signals an error if not found. Returns the buffer."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (let ((pos (or (org-find-property "ID" id)
                     (org-find-property "CUSTOM_ID" id))))
        (unless pos
          (error "Task ID %S not found in %s" id file))
        (goto-char pos)
        (org-back-to-heading t)
        buf))))

;;; ─── Adding a task ───────────────────────────────────────────────────────────

(defun denote-mcp-tracker--find-or-create-category (category)
  "Move point to the end of the `**' heading text line for CATEGORY under
`* Tasks', creating both `* Tasks' and the category heading if either
is missing. Point ends at the end of the category's subtree, ready for
a new `***' child to be inserted."
  (let ((tasks-pos (save-excursion
                     (let (pos)
                       (org-map-entries
                        (lambda ()
                          (when (and (not pos)
                                     (= (org-current-level) 1)
                                     (equal (org-get-heading t t t t) "Tasks"))
                            (setq pos (point))))
                        nil 'file)
                       pos))))
    (unless tasks-pos
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n* Tasks\n")
      (setq tasks-pos (line-beginning-position)))
    (let ((category-pos
           (save-excursion
             (goto-char tasks-pos)
             (let ((end (save-excursion (org-end-of-subtree t t)))
                   pos)
               (while (and (not pos)
                           (outline-next-heading)
                           (< (point) end))
                 (when (and (= (org-current-level) 2)
                            (equal (org-get-heading t t t t) category))
                   (setq pos (line-beginning-position))))
               pos))))
      (if category-pos
          (progn
            (goto-char category-pos)
            (org-end-of-subtree t t)
            (skip-chars-backward " \t\n")
            (end-of-line))
        ;; Not found: append a new category heading at the end of * Tasks subtree
        (goto-char tasks-pos)
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (insert (format "** %s\n" category))
        (forward-line -1)
        (end-of-line)))))

;;;###autoload
(defun denote-mcp-tracker-add-task (file category id title &optional status priority props body)
  "Add a new task heading under CATEGORY in FILE (creating the category
if absent), with the given TASK ID, TITLE, STATUS (default \"TODO\"),
and PRIORITY (one of \"A\" \"B\" \"C\", default \"B\").

PROPS is an alist of extra (NAME . VALUE) pairs added to the
PROPERTIES drawer beyond :ID: itself (e.g. (\"SEVERITY\" . \"HIGH\")
(\"AGENT_TYPE\" . \"engineering\") (\"DEPENDS\" . \"other-id\")). BODY,
if given, is inserted as the task's prose body (Where/Problem/Fix/Test
-- see operations/item-design.md for how to fill this in well).

Get ID from `denote-mcp-tracker-next-id' first so it is guaranteed
unique. Returns the absolute file path."
  (let ((status (or status "TODO"))
        (priority (or priority "B")))
    (with-current-buffer (find-file-noselect file)
      (denote-mcp-tracker--find-or-create-category category)
      (insert (format "\n*** %s [#%s] %s\n" status priority title))
      (insert ":PROPERTIES:\n")
      (insert (format ":ID: %s\n" id))
      (dolist (pair props)
        (insert (format ":%s: %s\n" (car pair) (cdr pair))))
      (insert ":END:\n")
      (when body
        (insert body)
        (unless (string-suffix-p "\n" body) (insert "\n")))
      (save-buffer)
      (buffer-file-name))))

;;; ─── Status transitions ──────────────────────────────────────────────────────

;;;###autoload
(defun denote-mcp-tracker-set-status (file ids status)
  "Set STATUS on every task heading in FILE whose :ID: is in IDS.
IDS is a single ID string or a list of them.  Delegates to
`denote-mcp-plan-update', which a plan shares: trackers simply have
no Execution Plan table to sync.  Returns the absolute file path."
  (plist-get (denote-mcp-plan-update
              file :transitions (mapcar (lambda (id) (cons id status))
                                        (if (listp ids) ids (list ids))))
             :file))

;;; ─── Result / status journaling ──────────────────────────────────────────────

;;;###autoload
(defun denote-mcp-tracker-append-result (file id text &optional label)
  "Append a journal bullet under the task whose :ID: is ID in FILE.
LABEL defaults to \"Result\" (use \"Status\" for an in-flight progress
note that is not yet a final result -- matches the *Status (date)*:
convention used throughout prior trackers for paused/in-progress work).
The bullet is inserted after the PROPERTIES drawer and any existing
body text or child subtasks' preceding prose, but before the first
child heading, so results always read as the task's own trailing
journal rather than getting buried under a subtask.

Every call appends a new bullet; it never overwrites a prior one --
a task's Result history is cumulative, exactly as it was in
[[denote:20261002T101742][the pubsub/irt adversarial review tracker]], where a
single task could carry three or four dated *Status* notes before a
final *Result*.

Returns the absolute file path."
  (let ((label (or label "Result"))
        (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (with-current-buffer (denote-mcp-tracker--find-task file id)
      (let* ((head (point))
             (end (save-excursion
                    (end-of-line)
                    (if (outline-next-heading) (point) (point-max)))))
        (goto-char end)
        (when (and (eobp) (not (bolp))) (insert "\n"))
        ;; Sit after existing body text but before trailing blank lines.
        (skip-chars-backward " \t\n" head)
        (end-of-line)
        (insert (format "\n- *%s%s*: %s"
                        label
                        (if (string= label "Status") (format " %s" timestamp) "")
                        text))
        (unless (looking-at-p "\n") (insert "\n")))
      (save-buffer)
      (buffer-file-name))))

;;; ─── Retitling and moving tasks ──────────────────────────────────────────────

;;;###autoload
(defun denote-mcp-tracker-retitle (file id new-title)
  "Rename the title text of the task whose :ID: is ID in FILE to NEW-TITLE.
Preserves the heading's stars, TODO keyword, priority cookie, and any
trailing tags -- only the title text between them changes. The :ID:
itself never changes; it is the stable handle callers keep using.

Returns the absolute file path."
  (with-current-buffer (denote-mcp-tracker--find-task file id)
    (org-edit-headline new-title)
    (save-buffer)
    (buffer-file-name)))

(defun denote-mcp-tracker--subtree-bounds ()
  "Return (BEG . END) of the subtree at point, including trailing blank
lines up to (but not including) the next heading at the same or
shallower level."
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)))
      (org-end-of-subtree t t)
      (cons beg (point)))))

;;;###autoload
(defun denote-mcp-tracker-move (file id new-category)
  "Move the task whose :ID: is ID (and all its child subtasks) from its
current category to NEW-CATEGORY under `* Tasks' in FILE, creating
NEW-CATEGORY if it does not already exist. The task's own heading
level, :ID:, properties, and body are unchanged -- only its parent
category changes.

Use this when `operations/categories.md''s periodic re-examination
decides a task belongs somewhere else, or when a new category is
split out of an overgrown one.

Returns the absolute file path."
  (with-current-buffer (denote-mcp-tracker--find-task file id)
    (let* ((bounds (denote-mcp-tracker--subtree-bounds))
           (text (buffer-substring (car bounds) (cdr bounds))))
      (delete-region (car bounds) (cdr bounds))
      ;; Clean up a now-blank line left behind, if any.
      (when (and (bolp) (eolp) (not (bobp)))
        (delete-char -1))
      (denote-mcp-tracker--find-or-create-category new-category)
      (unless (bolp) (insert "\n"))
      (insert "\n" (string-trim-right text) "\n"))
    (save-buffer)
    (buffer-file-name)))

;;; ─── Querying ────────────────────────────────────────────────────────────────

;;;###autoload
(defun denote-mcp-tracker-list (file &optional status-filter category-filter)
  "Return a list of plists describing every task in FILE, each shaped
(:id ID :title TITLE :status TODO-KEYWORD :priority PRIORITY
 :category CATEGORY :severity SEVERITY-OR-NIL :level LEVEL).

STATUS-FILTER and CATEGORY-FILTER, if given, restrict the results to
tasks with that exact status keyword or category heading text.

Use this for `operations/categories.md''s periodic re-examination
(group by :category to see which ones have grown too large or too
thin) and for `operations/coordination.md''s batch selection (filter
by status \"TODO\" to see what is ready to dispatch)."
  (with-current-buffer (find-file-noselect file)
    (let (results (current-category nil))
      (org-map-entries
       (lambda ()
         (let* ((level (org-current-level))
                (heading (substring-no-properties (org-get-heading t t t t))))
           (if (= level 2)
               (setq current-category heading)
             (let* ((id (org-entry-get nil "ID"))
                    (status (let ((s (org-get-todo-state))) (and s (substring-no-properties s))))
                    (priority (org-entry-get nil "PRIORITY"))
                    (severity (org-entry-get nil "SEVERITY")))
               (when (and id
                          (or (null status-filter) (equal status status-filter))
                          (or (null category-filter) (equal current-category category-filter)))
                 (push (list :id id :title heading :status status :priority priority
                             :category current-category :severity severity :level level)
                       results))))))
       nil nil)
      (nreverse results))))

;;; ─── Decisions ───────────────────────────────────────────────────────────────

;;;###autoload
(defun denote-mcp-tracker-add-decision (file title text &optional status)
  "Append a Decision entry TITLE with body TEXT under `* Decisions' in FILE.
STATUS defaults to \"ACCEPTED\".  Delegates to
`denote-mcp-plan-add-decision', shared with plans.  Use
`operations/decisions-questions.md''s Context/
Alternatives/Decision/Rationale/External-Impact shape for TEXT.
Returns the absolute file path."
  (plist-get (denote-mcp-plan-update
              file :decisions (list (list title text status)))
             :file))

;;; ─── Resolving which tracker to act on ───────────────────────────────────────

;;;###autoload
(defun denote-mcp-tracker-resolve-target (&optional identifier)
  "Resolve the tracker note to act on from IDENTIFIER.
Same resolution as `denote-mcp-plan-resolve-note', over agent+tracker
notes and the last tracker this session created."
  (denote-mcp-plan-resolve-note
   identifier '("agent" "tracker") denote-mcp-tracker--last-created-file
   "agent+tracker"))

(provide 'denote-mcp-tracker)
;;; denote-mcp-tracker.el ends here
