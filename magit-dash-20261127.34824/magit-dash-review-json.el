;;; magit-dash-review-json.el --- diff-review/1 JSON adapter for code-review -*- lexical-binding: t; -*-

;;; Commentary:
;; Interchange between the diff-review/1 review JSON file and code-review.
;; See docs/review-json.org.
;;
;; - `magit-dash-review-json-export' builds the diff-review/1 table for a
;;   PR from the review file plus code-review's comments.
;; - `magit-dash-review-json-import' mirrors the JSON comments and verdict
;;   into code-review and reports (:added N :updated N :skipped N :deleted N
;;   :verdict-failed N :assigned ALIST); `-apply-assigned' writes the
;;   cr_ids code-review assigned back into a table.  Conflict rule: the
;;   review file wins on overlapping draft edits, and stale code-review
;;   drafts matching an already-published JSON comment are pruned.
;; - `magit-dash-review-json-merge' and `-transcode-thread' hold the
;;   GitHub thread merge that used to live in magit-dash-review.el.
;;
;; Every call into code-review goes through one overridable seam,
;; `magit-dash-review-json-code-review-function', whose default implements
;; the ops on code-review's public `code-review-api-*' functions (the audit
;; is in docs/review-json.org).  This file never touches closql objects.

;;; Code:

(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function magit-dash-review-file "magit-dash-review" (root pr-number &optional repo))
(declare-function magit-dash-worktree-main-dir "magit-dash-worktree" (&optional directory))
(declare-function diff-review-refresh "diff-review" ())
(declare-function code-review-api-pullreq "code-review-api" (repo number))
(declare-function code-review-api-comments "code-review-api" (repo number))
(declare-function code-review-api-upsert-comment "code-review-api" (repo number plist &key dry-run))
(declare-function code-review-api-delete-comment "code-review-api" (repo number id &key dry-run))
(declare-function code-review-api-verdict "code-review-api" (repo number))
(declare-function code-review-api-set-verdict "code-review-api" (repo number state &optional head-oid &key dry-run))
(declare-function code-review-api-viewed "code-review-api-viewed" (repo number))
(declare-function code-review-api-set-viewed "code-review-api-viewed" (repo number path blob viewed-p))
(declare-function code-review-api-analysis "code-review-api-analysis" (repo number))
(declare-function code-review-api-analysis-available-p "code-review-api-analysis" (repo number))

(defgroup magit-dash-review-json nil
  "diff-review/1 JSON adapter for code-review."
  :group 'magit-dash)

(defcustom magit-dash-review-json-code-review-function
  #'magit-dash-review-json-code-review-default
  "Function through which the adapter reads and writes code-review state.
Called as (FUNCTION OP PR-KEY &optional ARG) where PR-KEY is (REPO . NUMBER)
and OP is one of:
  `comments'  return code-review's comments as a list of tables shaped
              like diff-review/1 comments (path, lines, author, at,
              body, resolved, replies, cr_id, remote_id, status).
  `pullreq'   return a table with pullreq_id and account, or nil.
  `write'     store the comment table ARG; return the stored table (with
              cr_id set), or nil when nothing could be stored (skipped),
              or signal an error.
  `delete'    delete the local draft with cr_id ARG; return non-nil when
              removed or nil when absent.
  `verdict'   with ARG, store the verdict table ARG and return non-nil or
              signal; without ARG, return the stored verdict table or nil.
  `viewed'    return a table of path to (blob, viewed) for every file of
              the stored diff; code-review marks are local only and follow
              the diff block, so compare blobs yourself (see
              `magit-dash-review-json-viewed-agreement').
  `set-viewed' ARG is a table of path to blob; mark each viewed in
              code-review and return a table of path to `set',
              `unchanged' or `stale'.
  `analysis'  return code_review.analysis (path, then @@ ranges text, to
              rank, tags, criteria), or nil when the engines cannot run.
              SYNCHRONOUS and potentially SLOW: never call it on the UI
              thread or in a live daemon.
While `magit-dash-review-json-dry-run' is non-nil, `write', `delete' and
`verdict' must validate and roll back instead of persisting.
Tables passed and returned are diff-review/1 JSON values: hash tables
\(equal test), vectors, strings, numbers, t, nil and :false.  Lists are
tolerated in what the seam returns: export turns them into vectors.
`magit-dash-review-json-deep-copy' accepts exactly these shapes.
The default calls the `code-review-api-*' functions.  When one is not
`fboundp' it messages why and returns nil for reads and signals a
`user-error' for writes, so comments stay drafts."
  :type 'function
  :group 'magit-dash-review-json)

(defcustom magit-dash-review-json-source-function
  #'magit-dash-review-json-default-source
  "Function returning the current review JSON table for a PR key, or nil.
Called with (REPO . NUMBER)."
  :type 'function
  :group 'magit-dash-review-json)

(defconst magit-dash-review-json--remote-keys
  '("gh_thread_id" "gh_comment_id" "gh_resolved" "origin" "path" "lines"
    "blob" "author" "at" "body" "outdated" "diff_side")
  "Comment keys that GitHub owns: a merge overwrites them with remote values.
Every other key keeps its local value when the local thread has one.")

;;; Seam default and source

(defvar magit-dash-review-json-dry-run nil
  "When non-nil the seam validates `write' and `verdict' and rolls back.
Bound by `magit-dash-review-json-import-dry-run'.  A custom seam must honour
it and say so with a `magit-dash-review-json-dry-run-supported' symbol
property, or the command refuses.")

(defun magit-dash-review-json-draft-id (comment &optional parent)
  "Return the deterministic code-review id for the local COMMENT table, or nil.
An integer in [2^44, 2^53), the range code-review accepts for drafts, derived
from COMMENT's id and, for a reply, PARENT's id, so the same comment always
upserts the same draft: that is what makes export, import, export idempotent.
Nil when COMMENT (or PARENT) has no id: a comment must carry an id (export
stamps one) because nothing else identifies it across edits."
  (let ((parent-id (and parent (magit-dash-review-json-draft-id parent))))
    (unless (or (magit-dash-review-json--empty-p (map-elt comment "id"))
                (and parent (not parent-id)))
      (+ (ash 1 44)
         (string-to-number
          (substring (secure-hash 'sha1 (format "cr-draft\0%s\0%s" parent-id (map-elt comment "id")))
                     0 13)
          16)))))

(defun magit-dash-review-json--sentish-p (comment)
  "Return non-nil when COMMENT table belongs to GitHub rather than being a draft."
  (or (member (map-elt comment "status") '("sent" "pending"))
      (equal (map-elt comment "origin") "github")
      (seq-some (lambda (key) (not (magit-dash-review-json--empty-p (map-elt comment key))))
                '("remote_id" "gh_thread_id" "gh_comment_id"))))

(defun magit-dash-review-json-published-p (comment)
  "Return non-nil when COMMENT table is already published on GitHub."
  (or (equal (map-elt comment "status") "sent")
      (equal (map-elt comment "origin") "github")
      (integerp (map-elt comment "gh_database_id"))
      (seq-some (lambda (key) (not (magit-dash-review-json--empty-p (map-elt comment key))))
                '("remote_id" "gh_comment_id"))))

(defun magit-dash-review-json--plist-table (plist)
  "Return the diff-review/1 comment table for code-review comment PLIST."
  (let ((table (make-hash-table :test #'equal)))
    (seq-do (lambda (spec)
              (when-let* ((value (plist-get plist (cadr spec))))
                (setf (map-elt table (car spec)) value)))
            '(("cr_id" :id) ("remote_id" :remote-id) ("path" :path) ("diff_side" :side)
              ("author" :author) ("at" :at)))
    (setf (map-elt table "body") (or (plist-get plist :body) ""))
    (setf (map-elt table "status") (symbol-name (or (plist-get plist :status) 'draft)))
    (setf (map-elt table "resolved") (if (plist-get plist :resolved) t :false))
    (when-let* ((line (plist-get plist :line)))
      (setf (map-elt table "lines")
            (vector (or (car (plist-get plist :lines)) line) line)))
    table))

(defun magit-dash-review-json--op-comments (pr-key _arg)
  "Seam `comments': code-review's comments of PR-KEY as tables, replies nested."
  (let* ((plists (code-review-api-comments (car pr-key) (cdr pr-key)))
         (ids (seq-map (lambda (p) (plist-get p :id)) plists))
         (reply-p (lambda (p) (and (plist-get p :reply-to)
                                   (member (plist-get p :reply-to) ids)))))
    (seq-map (lambda (top)
               (let ((table (magit-dash-review-json--plist-table top)))
                 (setf (map-elt table "replies")
                       (vconcat
                        (thread-last plists
                                     (seq-filter (lambda (p)
                                                   (and (funcall reply-p p)
                                                        (equal (plist-get p :reply-to)
                                                               (plist-get top :id)))))
                                     (seq-map #'magit-dash-review-json--plist-table))))
                 table))
             (seq-remove reply-p plists))))

(defun magit-dash-review-json--op-write (pr-key comment)
  "Seam `write': store COMMENT table for PR-KEY, return the stored table or nil.
A local draft is upserted under its cr_id, else its derived draft id; a
draft with neither (no id) signals `user-error' and is counted skipped.
A comment GitHub owns is read-only in code-review: only its unpublished
replies are upserted, as replies to its cr_id; nil when nothing was stored.
Replies to a draft are not supported by code-review and are ignored.  Reply
ids are derived from the reply and parent ids and not written back.  The
resolved flag is read-only in code-review and never written."
  (let ((repo (car pr-key))
        (number (cdr pr-key)))
    (cond
     ((and (not (magit-dash-review-json--sentish-p comment))
           (not (map-elt comment "cr_id"))
           (not (magit-dash-review-json-draft-id comment)))
      (user-error "Comment on %s has no id, so it cannot be stored under a stable id"
                  (or (map-elt comment "path") "the review")))
     ((not (magit-dash-review-json--sentish-p comment))
      (let ((lines (append (map-elt comment "lines") nil)))
        (magit-dash-review-json--plist-table
         (code-review-api-upsert-comment
          repo number
          (append (list :id (or (map-elt comment "cr_id") (magit-dash-review-json-draft-id comment))
                        :path (map-elt comment "path")
                        :line (cadr lines)
                        :lines (and (cadr lines) (< (car lines) (cadr lines)) lines)
                        :body (map-elt comment "body"))
                  (when-let* ((side (map-elt comment "diff_side")))
                    (list :side side))
                  (when-let* ((author (map-elt comment "author")))
                    (list :author author)))
          :dry-run magit-dash-review-json-dry-run))))
     ((null (map-elt comment "cr_id"))
      (message "magit-dash-review-json: %s has no cr_id in code-review; not stored"
               (or (map-elt comment "gh_thread_id") (map-elt comment "id")))
      nil)
     (t
      (let ((stored (seq-keep
                     (lambda (reply)
                       (unless (magit-dash-review-json-published-reply-p reply)
                         (code-review-api-upsert-comment
                          repo number
                          (list :id (or (map-elt reply "cr_id")
                                        (magit-dash-review-json-draft-id reply comment)
                                        (user-error "Reply to %s has no id" (map-elt comment "cr_id")))
                                :reply-to (map-elt comment "cr_id")
                                :path (map-elt comment "path")
                                :body (map-elt reply "body")
                                :author (map-elt reply "author"))
                          :dry-run magit-dash-review-json-dry-run)))
                     (append (map-elt comment "replies") nil))))
        (when stored
          (let ((copy (magit-dash-review-json-deep-copy comment)))
            (setf (map-elt copy "cr_id") (map-elt comment "cr_id"))
            copy)))))))

(defun magit-dash-review-json--op-verdict (pr-key verdict)
  "Seam `verdict': read the stored verdict of PR-KEY, or store VERDICT.
Only state and head are stored; by, at and the submitted_* stamps stay in the
file.  Return the verdict table, or nil when none."
  (cond
   ((and verdict (magit-dash-review-json--empty-p (map-elt verdict "state")))
    (user-error "Refusing to clear code-review's verdict: verdict has no state"))
   (verdict
    (code-review-api-set-verdict (car pr-key) (cdr pr-key)
                                 (map-elt verdict "state") (map-elt verdict "head")
                                 :dry-run magit-dash-review-json-dry-run))
   (t
    (when-let* ((stored (code-review-api-verdict (car pr-key) (cdr pr-key)))
                (table (make-hash-table :test #'equal)))
      (setf (map-elt table "state") (symbol-name (plist-get stored :state)))
      (when-let* ((head (plist-get stored :head-oid)))
        (setf (map-elt table "head") head))
      (when-let* ((at (plist-get stored :at)))
        (setf (map-elt table "at") at))
      table))))

(defun magit-dash-review-json--op-viewed (pr-key _arg)
  "Seam `viewed': table of path to (blob, viewed) from code-review for PR-KEY."
  (let ((table (make-hash-table :test #'equal)))
    (seq-do (lambda (entry)
              (let ((info (make-hash-table :test #'equal)))
                (when-let* ((blob (plist-get (cdr entry) :blob)))
                  (setf (map-elt info "blob") blob))
                (setf (map-elt info "viewed") (if (plist-get (cdr entry) :viewed-p) t :false))
                (setf (map-elt table (car entry)) info)))
            (code-review-api-viewed (car pr-key) (cdr pr-key)))
    table))

(defun magit-dash-review-json--op-set-viewed (pr-key marks)
  "Seam `set-viewed': mark the paths of MARKS (path to blob) viewed for PR-KEY.
Return a table of path to \"set\", \"unchanged\" or \"stale\" (unknown path or
blob mismatch).  These marks are local to code-review: its GitHub seeding can
overwrite them on the next fetch."
  (let ((result (make-hash-table :test #'equal)))
    (map-do (lambda (path blob)
              (setf (map-elt result path)
                    (pcase (code-review-api-set-viewed (car pr-key) (cdr pr-key) path blob t)
                      ('t "set")
                      (:unchanged "unchanged")
                      (_ "stale"))))
            marks)
    result))

(defun magit-dash-review-json--op-analysis (pr-key _arg)
  "Seam `analysis': code_review.analysis for PR-KEY, or nil.
SYNCHRONOUS and potentially SLOW (worktree-wide git grep and blame): never
call it on the UI thread or in a live daemon.  Nil, with a message, unless
`code-review-api-analysis-available-p', since without it the result would
be partial and look like \"no findings\".  Hunks are keyed by the raw @@
ranges text, e.g. -1,3 +1,4."
  (cond
   ((not (code-review-api-analysis-available-p (car pr-key) (cdr pr-key)))
    (message "magit-dash-review-json: analysis engines cannot run for %s#%s (no stored diff or worktree)"
             (car pr-key) (cdr pr-key))
    nil)
   (t
    (let ((table (make-hash-table :test #'equal)))
      (seq-do (lambda (file)
                (let ((hunks (make-hash-table :test #'equal)))
                  (seq-do (lambda (hunk)
                            (let ((info (make-hash-table :test #'equal)))
                              (setf (map-elt info "rank") (plist-get (cdr hunk) :rank))
                              (setf (map-elt info "tags") (vconcat (plist-get (cdr hunk) :tags)))
                              (setf (map-elt info "criteria") (vconcat (plist-get (cdr hunk) :criteria)))
                              (setf (map-elt hunks (car hunk)) info)))
                          (cdr file))
                  (setf (map-elt table (car file)) hunks)))
              (code-review-api-analysis (car pr-key) (cdr pr-key)))
      table))))

(defun magit-dash-review-json--op-pullreq (pr-key _arg)
  "Seam `pullreq': return pullreq_id and account table for PR-KEY, or nil."
  (when-let* ((meta (code-review-api-pullreq (car pr-key) (cdr pr-key)))
              (table (make-hash-table :test #'equal)))
    (when-let* ((id (plist-get meta :id))
                ((not (magit-dash-review-json--empty-p id))))
      (setf (map-elt table "pullreq_id") id))
    (when-let* ((account (plist-get meta :account))
                ((not (magit-dash-review-json--empty-p account))))
      (setf (map-elt table "account") account))
    (when (> (map-length table) 0)
      table)))

(defun magit-dash-review-json--op-delete (pr-key id)
  "Seam `delete': delete local draft ID from PR-KEY in code-review."
  (code-review-api-delete-comment (car pr-key) (cdr pr-key) id
                                  :dry-run magit-dash-review-json-dry-run))

(defconst magit-dash-review-json--ops
  '((comments (code-review-api-comments) magit-dash-review-json--op-comments)
    (pullreq (code-review-api-pullreq) magit-dash-review-json--op-pullreq)
    (write (code-review-api-upsert-comment) magit-dash-review-json--op-write t)
    (delete (code-review-api-delete-comment) magit-dash-review-json--op-delete t)
    (verdict (code-review-api-verdict) magit-dash-review-json--op-verdict)
    (viewed (code-review-api-viewed) magit-dash-review-json--op-viewed)
    (set-viewed (code-review-api-set-viewed) magit-dash-review-json--op-set-viewed t)
    (analysis (code-review-api-analysis-available-p code-review-api-analysis)
              magit-dash-review-json--op-analysis))
  "Seam ops: (OP FUNCTIONS HANDLER [WRITE-P]).
FUNCTIONS are every code-review function HANDLER calls; the op is skipped
unless each is `fboundp' (an autoload counts, so it loads on first call).
WRITE-P ops signal when skipped, the others return nil.  A `verdict' with an
argument needs `code-review-api-set-verdict' instead.")

(defun magit-dash-review-json-code-review-default (op pr-key &optional arg)
  "Default `magit-dash-review-json-code-review-function' for OP, PR-KEY and ARG.
Calls code-review's `code-review-api-*' functions (see
`magit-dash-review-json--ops').  When a function the op needs is not
`fboundp' (code-review's API is not loaded and has no autoload), message which
one and skip: nil for reads, a `user-error' for writes.  It never loads
anything itself."
  (let* ((spec (map-elt magit-dash-review-json--ops op))
         (writing (or (nth 2 spec) (and (eq op 'verdict) arg)))
         (missing (seq-remove #'fboundp
                              (if (and (eq op 'verdict) arg)
                                  '(code-review-api-set-verdict)
                                (car spec)))))
    (cond
     ((null spec)
      (error "Unknown code-review seam op %s" op))
     ((null missing)
      (funcall (nth 1 spec) pr-key arg))
     (t
      (message "magit-dash-review-json: skipping %s, %s is not available (code-review API not loaded)"
               op (mapconcat #'symbol-name missing ", "))
      (when writing
        (user-error "code-review API unavailable; cannot %s" op))))))

(defun magit-dash-review-json-default-source (pr-key)
  "Read the review file for PR-KEY from the main worktree of `default-directory'.
Return nil, with a message, when the file cannot be read or its magit_dash
repo is not (car PR-KEY)."
  (require 'magit-dash-review)
  (condition-case err
      (when-let* ((dir (and (fboundp 'magit-dash-worktree-main-dir)
                            (magit-dash-worktree-main-dir default-directory)))
                  (file (magit-dash-review-file dir (cdr pr-key) (car pr-key)))
                  ((file-readable-p file))
                  (table (magit-dash-review-json-read-file file)))
        (cond
         ((equal (car (magit-dash-review-json-pr-key table)) (car pr-key))
          table)
         (t
          (message "magit-dash-review-json: %s is for %s, not %s; ignoring it"
                   file (car (magit-dash-review-json-pr-key table)) (car pr-key))
          nil)))
    (error (message "magit-dash-review-json: cannot read review file for %s: %s"
                    pr-key (error-message-string err))
           nil)))

;;; File I/O and small helpers

(defun magit-dash-review-json--parse-file (file)
  "Parse FILE as UTF-8 JSON into a hash table."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (json-parse-string (buffer-string)
                       :object-type 'hash-table
                       :array-type 'array
                       :null-object nil
                       :false-object :false)))

(define-error 'magit-dash-review-json-conflict
              "Review file changed on disk since it was read")

(defun magit-dash-review-json-file-stamp (file)
  "Return the (MTIME . SIZE) stamp of FILE, or `:missing' when absent."
  (let ((attrs (file-attributes (file-truename (expand-file-name file)))))
    (if attrs
        (cons (file-attribute-modification-time attrs)
              (file-attribute-size attrs))
      :missing)))

(defcustom magit-dash-review-json-modified-buffer-action 'skip
  "What writers do when the buffer visiting the review file has unsaved edits.
`skip' (the default) leaves the buffer alone: the file on disk is still
updated, but the buffer is neither reverted nor refreshed (a message says
so), so unsaved edits are never discarded.  `save' saves the buffer before
the file is read, so its edits are part of what is rewritten.  `refuse'
signals a `user-error' before reading or writing when the visiting buffer
has unsaved edits."
  :type '(choice (const :tag "Skip refreshing a modified buffer" skip)
                 (const :tag "Save a modified buffer first" save)
                 (const :tag "Refuse to overwrite a modified buffer" refuse))
  :group 'magit-dash-review-json)

(defvar magit-dash-review-json-last-read-fallback nil
  "Backup file the last `magit-dash-review-json-read-file' fell back to, or nil.
Non-nil means the review file itself was corrupt and the returned data is
the (older) backup.")

(defun magit-dash-review-json--parseable-p (file)
  "Return non-nil when FILE exists and parses as a JSON object."
  (and (file-readable-p file)
       (condition-case nil
           (hash-table-p (magit-dash-review-json--parse-file file))
         (error nil))))

(defun magit-dash-review-json-read-file (review-file)
  "Parse REVIEW-FILE as a diff-review/1 JSON hash table.
When REVIEW-FILE is unparsable and the REVIEW-FILE.bak left by
`magit-dash-review-json-write-file' parses, message and return the backup,
and set `magit-dash-review-json-last-read-fallback' to it so writers can
refuse to build on older data (see `magit-dash-review-json-read-for-write')."
  (unless (and review-file (file-readable-p review-file))
    (error "Review file not readable: %s" review-file))
  (setq magit-dash-review-json-last-read-fallback nil)
  (let ((backup (concat (file-truename review-file) ".bak")))
    (condition-case err
        (magit-dash-review-json--parse-file review-file)
      (error
       (if (magit-dash-review-json--parseable-p backup)
           (progn
             (message "magit-dash-review-json: WARNING %s is corrupt (%s); using older backup %s"
                      review-file (error-message-string err) backup)
             (setq magit-dash-review-json-last-read-fallback backup)
             (magit-dash-review-json--parse-file backup))
         (signal (car err) (cdr err)))))))

(defun magit-dash-review-json-save-visiting-buffer (file)
  "Handle a modified buffer visiting FILE.
Follows `magit-dash-review-json-modified-buffer-action': `save' saves the
buffer; `refuse' signals `user-error'."
  (when-let* ((buf (find-buffer-visiting file))
              ((buffer-modified-p buf)))
    (pcase magit-dash-review-json-modified-buffer-action
      ('save
       (with-current-buffer buf
         (save-buffer)))
      ('refuse
       (user-error "Buffer %s visiting %s has unsaved edits; refusing to overwrite"
                   (buffer-name buf) file)))))

(defun magit-dash-review-json-read-with-stamp (review-file)
  "Read REVIEW-FILE for modification and return (DATA . STAMP).
A modified visiting buffer is saved first when
`magit-dash-review-json-modified-buffer-action' is `save', or refused with a
`user-error' when it is `refuse'.  STAMP is the post-save mtime+size stamp
from `magit-dash-review-json-file-stamp', for passing as EXPECTED-STAMP to
`magit-dash-review-json-write-file'.  When the file was corrupt and the
backup was read instead, ask for confirmation and signal `user-error' unless
it is given."
  (magit-dash-review-json-save-visiting-buffer review-file)
  (let ((stamp (magit-dash-review-json-file-stamp review-file))
        (data (magit-dash-review-json-read-file review-file)))
    (when (and magit-dash-review-json-last-read-fallback
               (not (yes-or-no-p
                     (format "%s is corrupt; build on the older backup %s? "
                             review-file magit-dash-review-json-last-read-fallback))))
      (user-error "Refusing to write %s from its backup" review-file))
    (cons data stamp)))

(defun magit-dash-review-json-read-for-write (review-file)
  "Read REVIEW-FILE to be modified and written back; return its table.
See `magit-dash-review-json-read-with-stamp', which also returns the file
stamp for compare-and-swap writes."
  (car (magit-dash-review-json-read-with-stamp review-file)))

(defun magit-dash-review-json-refresh-visiting-buffer (file)
  "Refresh the buffer visiting FILE after FILE changed on disk.
Return non-nil when there is no such buffer or it was refreshed.  A modified
buffer is never reverted or refreshed: message and return nil, so unsaved
edits survive (saving it later asks about the newer file on disk)."
  (let ((buf (find-buffer-visiting file)))
    (cond
     ((not (buffer-live-p buf)) t)
     ((buffer-modified-p buf)
      (message "magit-dash-review: %s has unsaved edits and was not refreshed; %s changed on disk"
               (buffer-name buf) file)
      nil)
     (t
      (with-current-buffer buf
        (cond
         ((and (derived-mode-p 'diff-review-mode) (fboundp 'diff-review-refresh))
          (diff-review-refresh))
         (t (revert-buffer t t t))))
      t))))

(defun magit-dash-review-json--clean-stale-temp-files (file)
  "Delete leftover FILE.tmp-* files in FILE's directory older than a day."
  (seq-do (lambda (stale)
            (when (time-less-p (file-attribute-modification-time (file-attributes stale))
                               (time-subtract nil (* 24 3600)))
              (ignore-errors (delete-file stale))))
          (directory-files (file-name-directory file) t
                           (concat "\\`" (regexp-quote (file-name-nondirectory file)) "\\.tmp-"))))

(defun magit-dash-review-json-write-file (review-file data &optional expected-stamp)
  "Serialize DATA hash table to REVIEW-FILE as pretty-printed UTF-8 JSON.
The write is atomic: the JSON goes to a temporary file in the same
directory which is renamed over REVIEW-FILE, so a failure leaves the old
file intact.  A symlinked REVIEW-FILE is resolved first, so the link stays a
link and its target is replaced; the file modes of the old file are kept (a
new file is 0600).  The previous file is copied to REVIEW-FILE.bak only when
it parses as JSON, so the backup is always the last good file.  Stale
REVIEW-FILE.tmp-* leftovers older than a day are removed.  When
`magit-dash-review-json-modified-buffer-action' is `refuse' and a visiting
buffer is modified, signal `user-error'.  When EXPECTED-STAMP is non-nil
\(e.g. from `magit-dash-review-json-read-with-stamp'), check mtime and size
right before replacing the target and signal
`magit-dash-review-json-conflict' if the file changed on disk."
  (when (eq magit-dash-review-json-modified-buffer-action 'refuse)
    (when-let* ((buf (find-buffer-visiting review-file))
                ((buffer-modified-p buf)))
      (user-error "Buffer %s visiting %s has unsaved edits; refusing to overwrite"
                  (buffer-name buf) review-file)))
  (let* ((file (file-truename (expand-file-name review-file)))
         (temp nil)
         (json (decode-coding-string
                (json-serialize data :null-object nil :false-object :false)
                'utf-8)))
    (make-directory (file-name-directory file) t)
    (magit-dash-review-json--clean-stale-temp-files file)
    (unwind-protect
        (progn
          (setq temp (make-temp-file (concat file ".tmp-")))
          (with-temp-buffer
            (insert json)
            (json-pretty-print-buffer)
            (insert "\n")
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (point-min) (point-max) temp nil 'silent)))
          (set-file-modes temp (or (and (file-exists-p file) (file-modes file)) #o600))
          (when expected-stamp
            (let ((current (magit-dash-review-json-file-stamp file)))
              (unless (or (eq current expected-stamp)
                          (and (consp current) (consp expected-stamp)
                               (equal (cdr current) (cdr expected-stamp))
                               (time-equal-p (car current) (car expected-stamp))))
                (signal 'magit-dash-review-json-conflict
                        (list review-file expected-stamp current)))))
          (when (magit-dash-review-json--parseable-p file)
            (copy-file file (concat file ".bak") t))
          (rename-file temp file t)
          (setq temp nil))
      (when (and temp (file-exists-p temp))
        (delete-file temp))))
  review-file)

(defun magit-dash-review-json-relocate-worktree (repo-dir old-path new-path)
  "Point the review files of REPO-DIR that name OLD-PATH at NEW-PATH.
Rewrites the root and magit_dash.worktree of every <git-common-dir>/review
JSON file whose value is OLD-PATH, after a worktree was moved.  Unreadable
files are left alone.  Return the number of files rewritten."
  (let* ((common (with-temp-buffer
                   (when (zerop (call-process "git" nil (list (current-buffer) nil) nil
                                              "-C" (expand-file-name repo-dir)
                                              "rev-parse" "--path-format=absolute"
                                              "--git-common-dir"))
                     (string-trim (buffer-string)))))
         (review-dir (and common (not (string-empty-p common))
                          (expand-file-name "review" common)))
         (old (file-name-as-directory (expand-file-name old-path)))
         (new (file-name-as-directory (expand-file-name new-path)))
         (same (lambda (value)
                 (and (stringp value) (not (string-empty-p value))
                      (equal (file-name-as-directory (expand-file-name value)) old)))))
    (if (not (and review-dir (file-directory-p review-dir)))
        0
      (length
       (seq-filter
        (lambda (file)
          (condition-case err
              (let* ((pair (magit-dash-review-json-read-with-stamp file))
                     (table (car pair))
                     (stamp (cdr pair))
                     (md (map-elt table "magit_dash"))
                     (hit nil))
                (when (funcall same (map-elt table "root"))
                  (setf (map-elt table "root") new)
                  (setq hit t))
                (when (and (hash-table-p md) (funcall same (map-elt md "worktree")))
                  (setf (map-elt md "worktree") new)
                  (setq hit t))
                (when hit
                  (magit-dash-review-json-write-file file table stamp)
                  (magit-dash-review-json-refresh-visiting-buffer file))
                hit)
            (error (message "magit-dash-review-json: cannot relocate %s: %s"
                            file (error-message-string err))
                   nil)))
        (directory-files review-dir t "\\`[^.].*\\.json\\'"))))))

(defun magit-dash-review-json-deep-copy (value)
  "Return a deep copy of VALUE.
Hash tables (same test), vectors, lists and strings are copied
recursively; numbers, symbols (t, nil, :false) are returned as is."
  (cond
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value) :size (hash-table-count value))))
      (map-do (lambda (key val)
                (setf (map-elt copy key) (magit-dash-review-json-deep-copy val)))
              value)
      copy))
   ((vectorp value) (vconcat (seq-map #'magit-dash-review-json-deep-copy value)))
   ((stringp value) (copy-sequence value))
   ((proper-list-p value) (seq-map #'magit-dash-review-json-deep-copy value))
   ((consp value)
    (cons (magit-dash-review-json-deep-copy (car value))
          (magit-dash-review-json-deep-copy (cdr value))))
   (t value)))

(defun magit-dash-review-json-make-id (thread-id)
  "Generate a comment id from THREAD-ID string.
For a non-empty THREAD-ID the id is the first 12 hex digits (48 bits) of
its SHA-1, stable across sessions and Emacs versions (`sxhash' is neither).
Otherwise it is 12 random hex digits."
  (if (and thread-id (not (string-empty-p thread-id)))
      (substring (secure-hash 'sha1 thread-id) 0 12)
    (format "%012x" (random (ash 1 48)))))

(defun magit-dash-review-json-resolve-blob (root rev path)
  "Resolve the git blob SHA for PATH at REV in repository at ROOT, or nil."
  (when (and root rev path (file-directory-p root))
    (let ((default-directory (file-name-as-directory (expand-file-name root))))
      (with-temp-buffer
        (when (= 0 (call-process "git" nil t nil "rev-parse" (format "%s:%s" rev path)))
          (string-trim (buffer-string)))))))

(defun magit-dash-review-json--empty-p (value)
  "Return non-nil when VALUE is nil or the empty string."
  (or (null value) (equal value "")))

(defun magit-dash-review-json--vectorize (value)
  "Return VALUE with every non-empty proper list below it turned into a vector.
The top level of a list VALUE (the comments list) is kept as a list; JSON
only serializes vectors, so export normalizes whatever the seam returned.
Hash tables are modified in place."
  (cond
   ((hash-table-p value)
    (map-do (lambda (key val)
              (setf (map-elt value key) (magit-dash-review-json--vectorize-nested val)))
            value)
    value)
   ((proper-list-p value) (seq-map #'magit-dash-review-json--vectorize value))
   (t value)))

(defun magit-dash-review-json--vectorize-nested (value)
  "Vectorize VALUE, turning a non-empty proper list into a vector."
  (cond
   ((and value (proper-list-p value))
    (vconcat (seq-map #'magit-dash-review-json--vectorize-nested value)))
   ((vectorp value) (vconcat (seq-map #'magit-dash-review-json--vectorize-nested value)))
   (t (magit-dash-review-json--vectorize value))))

(defun magit-dash-review-json--read-op (op pr-key)
  "Call the seam read OP for PR-KEY; message and return nil when it signals."
  (condition-case err
      (funcall magit-dash-review-json-code-review-function op pr-key)
    (error (message "magit-dash-review-json: cannot read %s: %s" op (error-message-string err))
           nil)))

(defun magit-dash-review-json--comments (table)
  "Return the comments of TABLE as a list."
  (append (or (map-elt table "comments") []) nil))

(defun magit-dash-review-json-pr-key (table)
  "Return (REPO . NUMBER) from the magit_dash key of TABLE, or nil."
  (let ((md (map-elt table "magit_dash")))
    (when (and md (map-elt md "repo") (map-elt md "number"))
      (cons (map-elt md "repo") (map-elt md "number")))))

;;; Matching

(defun magit-dash-review-json-content-hash (comment)
  "Return a hash of the path, all line numbers and body of COMMENT."
  (secure-hash 'sha1
               (mapconcat (lambda (part) (format "%s" part))
                          (list (or (map-elt comment "path") "")
                                (mapconcat (lambda (n) (format "%s" n))
                                           (append (map-elt comment "lines") nil)
                                           ",")
                                (or (map-elt comment "body") ""))
                          "\0")))

(defun magit-dash-review-json-find-match (comment candidates)
  "Return the member of CANDIDATES that is the same thread as COMMENT, or nil.
Tiers, first hit wins: GitHub node id (remote_id or gh_comment_id), the REST
id gh_database_id (set when a send was acknowledged without a node id), cr_id
\(also the id `magit-dash-review-json-draft-id' derives for a comment
without one, in either direction), gh_thread_id, then content hash of path,
line and body.  A candidate whose
gh_thread_id differs from COMMENT's never matches.  The hash tier skips
candidates with origin local or agent unless COMMENT has that origin, so
locally authored comments are never absorbed by a GitHub thread."
  (let* ((thread-id (map-elt comment "gh_thread_id"))
         (node-ids (seq-remove #'magit-dash-review-json--empty-p
                               (list (map-elt comment "remote_id")
                                     (map-elt comment "gh_comment_id"))))
         (cr-id (map-elt comment "cr_id"))
         (hash (and (not (magit-dash-review-json--empty-p (map-elt comment "body")))
                    (magit-dash-review-json-content-hash comment)))
         (eligible (seq-remove
                    (lambda (cand)
                      (let ((other (map-elt cand "gh_thread_id")))
                        (and (not (magit-dash-review-json--empty-p thread-id))
                             (not (magit-dash-review-json--empty-p other))
                             (not (equal thread-id other)))))
                    candidates)))
    (seq-some
     (lambda (tier) (seq-find tier eligible))
     (list (lambda (cand)
             (seq-some (lambda (id) (member id (list (map-elt cand "remote_id")
                                                     (map-elt cand "gh_comment_id"))))
                       node-ids))
           (lambda (cand)
             (and (integerp (map-elt comment "gh_database_id"))
                  (equal (map-elt comment "gh_database_id") (map-elt cand "gh_database_id"))))
           (lambda (cand) (and cr-id (equal cr-id (map-elt cand "cr_id"))))
           (lambda (cand)
             (or (and (not cr-id)
                      (map-elt cand "cr_id")
                      (equal (map-elt cand "cr_id") (magit-dash-review-json-draft-id comment)))
                 (and (not (map-elt cand "cr_id"))
                      cr-id
                      (equal cr-id (magit-dash-review-json-draft-id cand)))))
           (lambda (cand)
             (and (not (magit-dash-review-json--empty-p thread-id))
                  (equal thread-id (map-elt cand "gh_thread_id"))))
           (lambda (cand)
             (and hash
                  (or (not (member (map-elt cand "origin") '("local" "agent")))
                      (equal (map-elt cand "origin") (map-elt comment "origin")))
                  (equal hash (magit-dash-review-json-content-hash cand))))))))

;;; Transcoding and merge

(defun magit-dash-review-json-transcode-thread (node root head-sha base-sha)
  "Transcode a GraphQL reviewThread NODE into a diff-review/1 comment hash table.
ROOT is the repository root directory.
HEAD-SHA and BASE-SHA are the commit SHAs for resolving blob hashes."
  (let* ((thread-id (or (map-elt node 'id) (map-elt node :id) ""))
         (is-resolved (or (eq (map-elt node 'isResolved) t)
                          (eq (map-elt node :resolved) t)))
         (is-outdated (or (eq (map-elt node 'isOutdated) t)
                          (eq (map-elt node :outdated) t)))
         (path (or (map-elt node 'path) (map-elt node :path) ""))
         (line (or (map-elt node 'line) (map-elt node :line)))
         (start-line (or (map-elt node 'startLine) (map-elt node :start_line)))
         (orig-line (or (map-elt node 'originalLine) (map-elt node :original_line)))
         (orig-start-line (or (map-elt node 'originalStartLine) (map-elt node :original_start_line)))
         (diff-side (or (map-elt node 'diffSide) (map-elt node :diff_side) "RIGHT"))
         (eff-line (or line orig-line))
         (eff-start-line (or start-line orig-start-line eff-line))
         (lines (when eff-line
                  (vector (min eff-start-line eff-line) (max eff-start-line eff-line))))
         (comments-data (or (map-nested-elt node '(comments nodes))
                            (map-elt node :comments)))
         (comments-list (seq-into comments-data 'list))
         (first-c (car comments-list))
         (first-id (or (map-elt first-c 'id) (map-elt first-c :id) ""))
         (first-commit-oid (or (map-nested-elt first-c '(commit oid))
                               (map-elt first-c :commit_oid)))
         (rev (cond ((equal diff-side "LEFT") (or base-sha head-sha))
                    (first-commit-oid first-commit-oid)
                    (t head-sha)))
         (blob (and (not (string-empty-p path))
                    (magit-dash-review-json-resolve-blob root rev path)))
         (comment-ht (make-hash-table :test #'equal))
         (replies (seq-map
                   (lambda (c)
                     (let ((r-ht (make-hash-table :test #'equal))
                           (c-id (or (map-elt c 'id) (map-elt c :id) "")))
                       (setf (map-elt r-ht "id") (magit-dash-review-json-make-id c-id))
                       (unless (string-empty-p c-id)
                         (setf (map-elt r-ht "gh_comment_id") c-id))
                       (when-let* ((db (or (map-elt c 'databaseId) (map-elt c :database_id)))
                                   ((integerp db)))
                         (setf (map-elt r-ht "gh_database_id") db))
                       (setf (map-elt r-ht "author") (or (map-nested-elt c '(author login))
                                                         (map-elt c :author)
                                                         "unknown"))
                       (when-let* ((at (or (map-elt c 'createdAt) (map-elt c :created_at))))
                         (setf (map-elt r-ht "at") at))
                       (setf (map-elt r-ht "body") (or (map-elt c 'body) (map-elt c :body) ""))
                       r-ht))
                   (cdr comments-list))))
    (setf (map-elt comment-ht "id") (magit-dash-review-json-make-id thread-id))
    (setf (map-elt comment-ht "gh_thread_id") thread-id)
    (setf (map-elt comment-ht "gh_comment_id") first-id)
    (when-let* ((db (or (map-elt first-c 'databaseId) (map-elt first-c :database_id)))
                ((integerp db)))
      (setf (map-elt comment-ht "gh_database_id") db))
    (setf (map-elt comment-ht "gh_resolved")
          (if is-resolved
              t
            :false))
    (setf (map-elt comment-ht "origin") "github")
    (setf (map-elt comment-ht "diff_side") diff-side)
    (unless (string-empty-p path)
      (setf (map-elt comment-ht "path") path))
    (when lines
      (setf (map-elt comment-ht "lines") lines))
    (when blob
      (setf (map-elt comment-ht "blob") blob))
    (setf (map-elt comment-ht "author") (or (map-nested-elt first-c '(author login))
                                            (map-elt first-c :author)
                                            (map-elt node :author)
                                            "unknown"))
    (when-let* ((at (or (map-elt first-c 'createdAt) (map-elt first-c :created_at))))
      (setf (map-elt comment-ht "at") at))
    (setf (map-elt comment-ht "body") (or (map-elt first-c 'body)
                                          (map-elt first-c :body)
                                          (map-elt node :body)
                                          ""))
    (setf (map-elt comment-ht "resolved")
          (if is-resolved
              t
            :false))
    (when is-outdated
      (setf (map-elt comment-ht "outdated") t))
    (setf (map-elt comment-ht "replies") (vconcat replies))
    comment-ht))

(defun magit-dash-review-json-published-reply-p (reply)
  "Return non-nil when the REPLY table is already on GitHub.
It has a gh_comment_id, or was sent (status \"sent\") with a REST
gh_database_id, which is all a send acknowledged without a node id leaves."
  (or (not (magit-dash-review-json--empty-p (map-elt reply "gh_comment_id")))
      (and (equal (map-elt reply "status") "sent")
           (integerp (map-elt reply "gh_database_id")))))

(defun magit-dash-review-json-merge-thread (local remote)
  "Merge the REMOTE thread into a copy of the LOCAL thread and return it.
Keys in `magit-dash-review-json--remote-keys' take the remote value, and are
removed when REMOTE omits them (a thread that stops being outdated clears).
Every other key keeps the local value (id, cr_id, remote_id, status and
unknown keys), falling back to the remote one when local has none.  Resolved
is sticky: once either side says resolved it stays resolved.  Intent to
resolve on GitHub is tracked apart from it: a thread whose last known
GitHub state was resolved and which GitHub now reports open is flagged
`gh_reopened' (and loses `local_resolved'), so submit does not re-resolve
it; GitHub resolving it again clears both.  `gh_missing' is cleared on a
match.
Replies are all remote replies, then unpublished local ones (no
gh_comment_id), then published local replies the remote list lacks, kept
with gh_missing t because the remote fetch may be truncated.  The result
shares nothing with LOCAL or REMOTE."
  (let ((merged (magit-dash-review-json-deep-copy local)))
    (seq-do (lambda (key)
              (unless (map-contains-key remote key)
                (map-delete merged key)))
            magit-dash-review-json--remote-keys)
    (map-do (lambda (key value)
              (when (or (member key magit-dash-review-json--remote-keys)
                        (not (map-contains-key merged key)))
                (setf (map-elt merged key) (magit-dash-review-json-deep-copy value))))
            remote)
    (setf (map-elt merged "resolved")
          (if (or (eq (map-elt remote "resolved") t)
                  (eq (map-elt local "resolved") t))
              t
            :false))
    (map-delete merged "gh_missing")
    (cond
     ((eq (map-elt remote "gh_resolved") t)
      (map-delete merged "local_resolved")
      (map-delete merged "gh_reopened"))
     ((eq (map-elt local "gh_resolved") t)
      (map-delete merged "local_resolved")
      (setf (map-elt merged "gh_reopened") t)))
    (let* ((remote-ids (seq-remove #'magit-dash-review-json--empty-p
                                   (seq-map (lambda (reply) (map-elt reply "gh_comment_id"))
                                            (map-elt remote "replies"))))
           (remote-db-ids (seq-remove #'null
                                      (seq-map (lambda (reply) (map-elt reply "gh_database_id"))
                                               (map-elt remote "replies"))))
           (local-replies (append (map-elt local "replies") nil))
           (missing (seq-filter
                     (lambda (reply)
                       (and (magit-dash-review-json-published-reply-p reply)
                            (not (and (not (magit-dash-review-json--empty-p (map-elt reply "gh_comment_id")))
                                      (member (map-elt reply "gh_comment_id") remote-ids)))
                            (not (and (map-elt reply "gh_database_id")
                                      (member (map-elt reply "gh_database_id") remote-db-ids)))))
                     local-replies))
           (flagged (seq-map (lambda (reply)
                               (let ((copy (magit-dash-review-json-deep-copy reply)))
                                 (setf (map-elt copy "gh_missing") t)
                                 copy))
                             missing)))
      (setf (map-elt merged "replies")
            (magit-dash-review-json-deep-copy
             (vconcat (map-elt remote "replies")
                      (seq-remove #'magit-dash-review-json-published-reply-p local-replies)
                      flagged))))
    merged))

(defun magit-dash-review-json-merge (existing-ht remote-metadata remote-threads)
  "Merge EXISTING-HT with REMOTE-METADATA and REMOTE-THREADS.
Return a new diff-review/1 table.  Unknown top-level keys (code_review and
others), unknown magit_dash keys, viewed and verdict are preserved.  Remote
threads are matched to local ones with `magit-dash-review-json-find-match',
each local thread at most once; unmatched remote threads are added and
unmatched local threads are kept whole.  Local order is preserved (a merged
thread keeps its position) and new remote threads are appended.  When
Local threads with a gh_thread_id that no remote thread matched get
`gh_missing' t when REMOTE-METADATA has a non-nil `:threads-complete' (the
caller's fetch was not truncated); submit skips such threads.  When
EXISTING-HT belongs to a different repo than REMOTE-METADATA, it is ignored
\(message) and a fresh table is built.  The result is fully independent
of EXISTING-HT and REMOTE-THREADS: mutating it changes neither."
  (let* ((incoming-repo (or (map-elt remote-metadata 'repo) (map-elt remote-metadata :repo)))
         (existing-repo (map-elt (map-elt existing-ht "magit_dash") "repo"))
         (foreign (and existing-ht
                       (not (magit-dash-review-json--empty-p incoming-repo))
                       (not (magit-dash-review-json--empty-p existing-repo))
                       (not (equal incoming-repo existing-repo))))
         (result (cond
                  (foreign
                   (message "magit-dash-review-json: existing review is for %s, not %s; starting fresh"
                            existing-repo incoming-repo)
                   (make-hash-table :test #'equal))
                  (existing-ht (magit-dash-review-json-deep-copy existing-ht))
                  (t (make-hash-table :test #'equal))))
         (md (if (hash-table-p (map-elt result "magit_dash"))
                 (map-elt result "magit_dash")
               (make-hash-table :test #'equal)))
         (state (seq-reduce
                 (lambda (acc remote-c)
                   (let ((local-c (magit-dash-review-json-find-match remote-c (cdr acc))))
                     (cons (cons (cons local-c
                                       (if local-c
                                           (magit-dash-review-json-merge-thread local-c remote-c)
                                         (magit-dash-review-json-deep-copy remote-c)))
                                 (car acc))
                           (seq-remove (lambda (c) (eq c local-c)) (cdr acc)))))
                 (seq-into remote-threads 'list)
                 (cons nil (magit-dash-review-json--comments result)))))
    (setf (map-elt md "provider") "github")
    (seq-do (lambda (spec)
              (when-let* ((value (or (map-elt remote-metadata (nth 1 spec))
                                     (map-elt remote-metadata (nth 2 spec)))))
                (setf (map-elt md (car spec)) value)))
            `(("repo" repo :repo) ("number" number :number) ("url" url :url)
              ("head_oid" headRefOid :head_oid) ("base_oid" baseRefOid :base_oid)))
    (setf (map-elt md "synced_at") (format-time-string "%FT%T%:z"))
    (setf (map-elt result "format") "diff-review/1")
    (when-let* ((worktree (or (map-elt remote-metadata 'worktree)
                              (map-elt remote-metadata :worktree)
                              (map-elt result "root"))))
      (setf (map-elt md "worktree") worktree)
      (setf (map-elt result "root") (file-name-as-directory (expand-file-name worktree))))
    (when-let* ((base-ref (or (map-elt remote-metadata 'baseRefName)
                              (map-elt remote-metadata :base-ref)
                              (map-elt result "base"))))
      (setf (map-elt result "base") base-ref))
    (when-let* ((head-ref (or (map-elt remote-metadata 'headRefName)
                              (map-elt remote-metadata :head-ref)
                              (map-elt result "head"))))
      (setf (map-elt result "head") head-ref))
    (setf (map-elt result "magit_dash") md)
    (unless (hash-table-p (map-elt result "viewed"))
      (setf (map-elt result "viewed") (make-hash-table :test #'equal)))
    (let ((pairs (nreverse (car state))))
      (when (or (map-elt remote-metadata :threads-complete)
                (map-elt remote-metadata 'threads-complete))
        (seq-do (lambda (local-c)
                  (when (and (not (magit-dash-review-json--empty-p (map-elt local-c "gh_thread_id")))
                             (not (seq-find (lambda (pair) (eq (car pair) local-c)) pairs)))
                    (setf (map-elt local-c "gh_missing") t)))
                (magit-dash-review-json--comments result)))
      (setf (map-elt result "comments")
            (vconcat (seq-map (lambda (local-c)
                                (or (cdr (seq-find (lambda (pair) (eq (car pair) local-c)) pairs))
                                    local-c))
                              (magit-dash-review-json--comments result))
                     (seq-map #'cdr (seq-remove #'car pairs)))))
    result))

;;; Export and import

(defun magit-dash-review-json-export (pr-key &optional with-analysis)
  "Return the diff-review/1 hash table for PR-KEY, (REPO . NUMBER).
Starts from the review file (`magit-dash-review-json-source-function'),
then overlays code-review's cr_id, remote_id and status onto matching
threads (never overwriting local values), appends code-review comments
with no match, and fills missing code_review keys and the magit_dash repo
and number from PR-KEY, so the result can be imported.  Never mutates the
source table.  A local draft without an id is stamped one (from its content), and one
without a cr_id gets the deterministic
`magit-dash-review-json-draft-id' (and status draft), so import stores it
under that id and a second export is identical.  The verdict comes from the
file; code-review's pending verdict fills it only when the file has none.
With WITH-ANALYSIS, code_review.analysis is filled from the seam `analysis'
op when the file has none: SYNCHRONOUS and potentially SLOW, never call it
on the UI thread or in a live daemon."
  (let* ((source (funcall magit-dash-review-json-source-function pr-key))
         (table (if source
                    (magit-dash-review-json-deep-copy source)
                  (make-hash-table :test #'equal)))
         (stored (magit-dash-review-json--vectorize
                  (funcall magit-dash-review-json-code-review-function 'comments pr-key)))
         (pullreq (magit-dash-review-json--vectorize
                   (magit-dash-review-json--read-op 'pullreq pr-key)))
         (verdict (magit-dash-review-json--read-op 'verdict pr-key))
         (analysis (and with-analysis
                        (magit-dash-review-json--vectorize
                         (magit-dash-review-json--read-op 'analysis pr-key))))
         (state (seq-reduce
                 (lambda (acc stored-c)
                   (let ((local-c (magit-dash-review-json-find-match stored-c (car acc))))
                     (cond
                      (local-c
                       (seq-do (lambda (key)
                                 (when (and (magit-dash-review-json--empty-p (map-elt local-c key))
                                            (map-elt stored-c key))
                                   (setf (map-elt local-c key) (map-elt stored-c key))))
                               '("cr_id" "remote_id"))
                       (when (and (not (magit-dash-review-json--empty-p (map-elt stored-c "status")))
                                  (not (equal (map-elt stored-c "status") (map-elt local-c "status"))))
                         (setf (map-elt local-c "status") (map-elt stored-c "status")))
                       (cons (seq-remove (lambda (c) (eq c local-c)) (car acc)) (cdr acc)))
                      (t
                       (let ((new-c (magit-dash-review-json-deep-copy stored-c)))
                         (when (magit-dash-review-json--empty-p (map-elt new-c "id"))
                           (setf (map-elt new-c "id")
                                 (magit-dash-review-json-make-id
                                  (format "%s" (or (map-elt new-c "remote_id")
                                                   (map-elt new-c "cr_id"))))))
                         (when (magit-dash-review-json--empty-p (map-elt new-c "origin"))
                           (setf (map-elt new-c "origin") "code-review"))
                         (cons (car acc) (cons new-c (cdr acc))))))))
                 stored
                 (cons (magit-dash-review-json--comments table) nil)))
         (code-review (or (map-elt table "code_review")
                          (make-hash-table :test #'equal))))
    (seq-do (lambda (c)
              (when (and (magit-dash-review-json--empty-p (map-elt c "cr_id"))
                         (not (magit-dash-review-json--sentish-p c)))
                (when (magit-dash-review-json--empty-p (map-elt c "id"))
                  (setf (map-elt c "id")
                        (magit-dash-review-json-make-id (magit-dash-review-json-content-hash c))))
                (setf (map-elt c "cr_id") (magit-dash-review-json-draft-id c))
                (when (magit-dash-review-json--empty-p (map-elt c "status"))
                  (setf (map-elt c "status") "draft"))))
            (magit-dash-review-json--comments table))
    (setf (map-elt table "comments")
          (vconcat (magit-dash-review-json--comments table) (nreverse (cdr state))))
    (when (and (hash-table-p verdict) (not (map-elt table "verdict")))
      (setf (map-elt table "verdict") (magit-dash-review-json-deep-copy verdict)))
    (when (and analysis (hash-table-p analysis) (> (map-length analysis) 0)
               (magit-dash-review-json--empty-p (map-elt code-review "analysis")))
      (setf (map-elt code-review "analysis") analysis))
    (map-do (lambda (key value)
              (when (and (not (magit-dash-review-json--empty-p value))
                         (magit-dash-review-json--empty-p (map-elt code-review key)))
                (setf (map-elt code-review key) value)))
            pullreq)
    (when (> (map-length code-review) 0)
      (setf (map-elt table "code_review") code-review))
    (unless (map-elt table "format")
      (setf (map-elt table "format") "diff-review/1"))
    (let ((md (if (hash-table-p (map-elt table "magit_dash"))
                  (map-elt table "magit_dash")
                (make-hash-table :test #'equal))))
      (when (magit-dash-review-json--empty-p (map-elt md "repo"))
        (setf (map-elt md "repo") (car pr-key)))
      (when (magit-dash-review-json--empty-p (map-elt md "number"))
        (setf (map-elt md "number") (cdr pr-key)))
      (setf (map-elt table "magit_dash") md))
    (seq-do (lambda (key)
              (unless (map-elt table key)
                (setf (map-elt table key) "")))
            '("root" "base"))
    (unless (map-elt table "viewed")
      (setf (map-elt table "viewed") (make-hash-table :test #'equal)))
    table))

(defun magit-dash-review-json--reply-content (comment)
  "Return the bodies of the unpublished replies of COMMENT as a list.
A reply that is published (see `magit-dash-review-json-published-reply-p')
is on GitHub, where code-review's API lists it as its own comment rather than
a reply, so only unpublished replies (code-review drafts, which have no
gh_comment_id) are comparable.  Bodies alone are
compared: code-review stamps its own author and time on a draft."
  (thread-last (append (map-elt comment "replies") nil)
               (seq-filter (lambda (reply)
                             (not (magit-dash-review-json-published-reply-p reply))))
               (seq-map (lambda (reply) (map-elt reply "body")))))

(defun magit-dash-review-json-apply-assigned (table result)
  "Set cr_id on the comments of TABLE that RESULT says code-review numbered.
RESULT is the plist `magit-dash-review-json-import' returned for TABLE.
Mutates and returns TABLE.  Persisting TABLE afterwards makes the next
import match those comments by cr_id instead of adding them again."
  (seq-do (lambda (pair)
            (setf (map-elt (elt (map-elt table "comments") (car pair)) "cr_id") (cdr pair)))
          (plist-get result :assigned))
  table)

(defun magit-dash-review-json-import (table)
  "Mirror the comments and verdict of TABLE into code-review.
TABLE is a diff-review/1 table with magit_dash repo and number.  Returns
\(:added N :updated N :skipped N :deleted N :verdict-failed N :assigned ALIST).
ALIST holds (INDEX . CR-ID) for each comment of TABLE that has no cr_id but
is stored in code-review under one; see `magit-dash-review-json-apply-assigned'.
Comments are matched with `magit-dash-review-json-find-match'.  Conflict
rule: the file wins on overlapping draft edits (the file body and replies
overwrite code-review's draft).  When a comment in TABLE is already
published (`magit-dash-review-json-published-p') and code-review still holds
a matching draft, that stale draft is pruned via `delete' (counted in
:deleted) unless `magit-dash-review-json-dry-run' is non-nil (reported in
:deleted without deleting).  A comment is unchanged, and skipped, when body
and unpublished reply bodies equal the stored ones (resolved is read-only in
code-review, so never compared); status is ignored because code-review
assigns it.  A write that signals leaves the comment counted as skipped and
TABLE untouched, so drafts stay drafts.  The verdict is written only when it
differs from the stored one in state or head; a failed write counts in
:verdict-failed.  TABLE is never mutated.  With
`magit-dash-review-json-dry-run' bound non-nil the seam validates and rolls
back, and the result carries :dry-run t."
  (let* ((pr-key (or (magit-dash-review-json-pr-key table)
                     (user-error "Review JSON has no magit_dash repo and number")))
         (fn magit-dash-review-json-code-review-function)
         (counts (list :added 0 :updated 0 :skipped 0 :deleted 0 :verdict-failed 0))
         (fetch-failed nil)
         (remaining (condition-case err
                        (funcall fn 'comments pr-key)
                      (error (message "magit-dash-review-json: cannot read comments: %s"
                                      (error-message-string err))
                             (setq fetch-failed t)
                             nil)))
         (assigned nil))
    (seq-do-indexed
     (lambda (comment index)
       (let* ((published (magit-dash-review-json-published-p comment))
              (pool (if published
                        (seq-remove (lambda (c) (equal (map-elt c "status") "draft"))
                                    remaining)
                      remaining))
              (match (magit-dash-review-json-find-match comment pool))
              (deleted-draft-ids nil))
         (when (and published (not fetch-failed))
           (let* ((cr-id (map-elt comment "cr_id"))
                  (draft-id (magit-dash-review-json-draft-id comment))
                  (hash (and (not (magit-dash-review-json--empty-p (map-elt comment "body")))
                             (magit-dash-review-json-content-hash comment)))
                  (dups (seq-filter
                         (lambda (cand)
                           (and (equal (map-elt cand "status") "draft")
                                (or (and cr-id (equal cr-id (map-elt cand "cr_id")))
                                    (and draft-id (equal draft-id (map-elt cand "cr_id")))
                                    (and hash (equal hash (magit-dash-review-json-content-hash cand))))))
                         remaining)))
             (seq-do
              (lambda (dup)
                (when-let* ((dup-id (map-elt dup "cr_id")))
                  (cond
                   (magit-dash-review-json-dry-run
                    (push dup-id deleted-draft-ids)
                    (setq counts (plist-put counts :deleted (1+ (plist-get counts :deleted))))
                    (setq remaining (seq-remove (lambda (c) (eq c dup)) remaining)))
                   (t
                    (condition-case err
                        (when (funcall fn 'delete pr-key dup-id)
                          (push dup-id deleted-draft-ids)
                          (setq counts (plist-put counts :deleted (1+ (plist-get counts :deleted))))
                          (setq remaining (seq-remove (lambda (c) (eq c dup)) remaining)))
                      (error (message "magit-dash-review-json: %s" (error-message-string err))))))))
              dups)))
         (let* ((kind (cond
                       ((null match) :added)
                       ((and (equal (map-elt comment "body") (map-elt match "body"))
                             (equal (magit-dash-review-json--reply-content comment)
                                    (magit-dash-review-json--reply-content match)))
                        nil)
                       (t :updated)))
                (written (when (and kind (not fetch-failed))
                           (let ((payload (magit-dash-review-json-deep-copy comment)))
                             (when (and (null match)
                                        (member (map-elt payload "cr_id") deleted-draft-ids))
                               (map-delete payload "cr_id"))
                             (when (and match (map-elt match "cr_id"))
                               (setf (map-elt payload "cr_id") (map-elt match "cr_id")))
                             (condition-case err
                                 (funcall fn 'write pr-key payload)
                               (error (message "magit-dash-review-json: %s" (error-message-string err))
                                      nil)))))
                (key (if (and kind written)
                         kind
                       :skipped))
                (cr-id (or (and (hash-table-p written) (map-elt written "cr_id"))
                           (map-elt match "cr_id"))))
           (setq counts (plist-put counts key (1+ (plist-get counts key))))
           (when (and cr-id (magit-dash-review-json--empty-p (map-elt comment "cr_id")))
             (push (cons index cr-id) assigned))
           (setq remaining (seq-remove (lambda (c) (eq c match)) remaining)))))
     (magit-dash-review-json--comments table))
    (when-let* ((verdict (map-elt table "verdict"))
                ((not (magit-dash-review-json--empty-p (map-elt verdict "state")))))
      (condition-case err
          (let ((current (funcall fn 'verdict pr-key)))
            (unless (and current
                         (equal (map-elt verdict "state") (map-elt current "state"))
                         (equal (map-elt verdict "head") (map-elt current "head")))
              (unless (funcall fn 'verdict pr-key verdict)
                (error "verdict write returned nil"))))
        (error (message "magit-dash-review-json: %s" (error-message-string err))
               (setq counts (plist-put counts :verdict-failed 1)))))
    (plist-put counts :assigned (nreverse assigned))
    (when magit-dash-review-json-dry-run
      (plist-put counts :dry-run t))
    counts))

(defun magit-dash-review-json-import-dry-run (pr-key)
  "Run the code-review import for PR-KEY without persisting, and report it.
PR-KEY is (REPO . NUMBER) or the string \"owner/repo#N\".  The table is the
review file from `magit-dash-review-json-source-function' (read from the main
worktree of `default-directory'), which is what the mirror imports after it
merges GitHub state into it; the merge itself is not run.  Signal a
`user-error' when there is no such file, rather than reporting an empty
import.  The seam validates each write inside a rolled back transaction
\(`magit-dash-review-json-dry-run'); a seam other than the default must have a
non-nil `magit-dash-review-json-dry-run-supported' symbol property or the
command refuses, since it would really write.  Message what would be added,
updated and skipped (skipped includes writes code-review would refuse, each
with its own message), and return the import result plist."
  (interactive (list (read-string "PR (owner/repo#N): ")))
  (let* ((key (cond
               ((consp pr-key) pr-key)
               ((string-match "\\`\\(.+\\)#\\([0-9]+\\)\\'" pr-key)
                (cons (match-string 1 pr-key) (string-to-number (match-string 2 pr-key))))
               (t (user-error "Expected owner/repo#N, got %S" pr-key))))
         (source (funcall magit-dash-review-json-source-function key))
         (result nil))
    (unless (or (eq magit-dash-review-json-code-review-function
                    #'magit-dash-review-json-code-review-default)
                (and (symbolp magit-dash-review-json-code-review-function)
                     (get magit-dash-review-json-code-review-function
                          'magit-dash-review-json-dry-run-supported)))
      (user-error "`magit-dash-review-json-code-review-function' does not declare dry-run support"))
    (unless source
      (user-error "No review file for %s#%s from %s (run from the PR's repository)"
                  (car key) (cdr key) default-directory))
    (setq result (let ((magit-dash-review-json-dry-run t))
                   (magit-dash-review-json-import source)))
    (message "Import dry run for %s#%s: would add %d, update %d, skip %d, delete %d, verdict failed %d; nothing written"
             (car key) (cdr key) (plist-get result :added) (plist-get result :updated)
             (plist-get result :skipped) (or (plist-get result :deleted) 0)
             (plist-get result :verdict-failed))
    result))

(defun magit-dash-review-json-viewed-blob-match-p (a b)
  "Return non-nil when blob hashes A and B name the same blob.
Both must be strings of at least 7 characters, and one must be a prefix of
the other, since code-review stores the index line's abbreviated hash."
  (and (stringp a) (stringp b)
       (>= (min (length a) (length b)) 7)
       (or (string-prefix-p a b) (string-prefix-p b a))))

(defun magit-dash-review-json-viewed-agreement (local api)
  "Return the entries of LOCAL that code-review's API also marks viewed.
LOCAL is the diff-review viewed table (path to blob); API is the seam
`viewed' table.  An entry counts only when API says viewed and the blobs
match (`magit-dash-review-json-viewed-blob-match-p'): code-review marks
follow the diff block, not the blob, so its own viewed flag alone is not
trusted.  API marks are local to code-review, never pushed to GitHub."
  (let ((agreed (make-hash-table :test #'equal)))
    (map-do (lambda (path blob)
              (let ((info (map-elt api path)))
                (when (and info
                           (eq (map-elt info "viewed") t)
                           (magit-dash-review-json-viewed-blob-match-p
                            blob (map-elt info "blob")))
                  (setf (map-elt agreed path) blob))))
            local)
    agreed))

(provide 'magit-dash-review-json)
;;; magit-dash-review-json.el ends here
