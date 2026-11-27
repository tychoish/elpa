;;; magit-dash-review.el --- Native buffer diff review integration for magit-dash -*- lexical-binding: t; -*-

;; Author: sam kleinman
;; Maintainer: tychoish
;; Version: 0.1
;; Keywords: vc, git, review, github

;;; Commentary:

;; Orchestrates native-buffer code review via diff-review and code-review,
;; manages review JSON hydration, 3-way merge, GitHub submission,
;; worktrees, agent remediation, and ACR selection.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'json)

(require 'magit-dash-gh)
(require 'magit-dash-review-json)
(require 'magit-dash-pr)
(require 'magit-dash-worktree)

;; External optional interfaces
(defvar diff-review--root)
(defvar diff-review--base)
(defvar diff-review--base-name)
(defvar diff-review--head)
(defvar diff-review--data)
(defvar diff-review--comments)
(defvar diff-review--file-path)
(defvar diff-review--hunks)
(defvar diff-review-file-mode)

(declare-function diff-review-find-file "diff-review" (file &optional base root head))
(declare-function diff-review-overview "diff-review" ())
(declare-function diff-review-refresh "diff-review" ())
(declare-function diff-review--common-dir "diff-review" (root))
(declare-function diff-review--toplevel "diff-review" (&optional directory))
(declare-function diff-review--modify-comments "diff-review" (fn))
(declare-function diff-review--review-buffer "diff-review" ())
(declare-function diff-review--region-lines "diff-review" ())
(declare-function diff-review--threads-at-point "diff-review" ())
(declare-function diff-review-branch "diff-review" (branch))
(declare-function annotated-completing-read "annotated-completing-read" (collection &rest args))
(declare-function agent-shell-workflow-dispatch "agent-shell-workflow" (id &rest args))
(declare-function agent-shell-prompt-queue "agent-shell-prompt-queue" (prompt &optional buffer))
(declare-function gptel-request "gptel" (&optional prompt &rest args))
(declare-function hitl-plan-list "hitl-plan" (&optional directory domain))
(declare-function hitl-plan-id "hitl-plan" (plan))
(declare-function hitl-plan-status "hitl-plan" (plan))
(declare-function hitl-plan-description "hitl-plan" (plan))
(declare-function hitl-plan-steps "hitl-plan" (plan))
(declare-function hitl-plan-step-status "hitl-plan" (step))
(declare-function hitl-plan-review "hitl-plan-ui" (plan &optional select))
(declare-function magit-dash-review-stage-hitl "magit-dash-review-hitl" (&optional review-file op-name))
(declare-function code-review-agents-shell-prompt "code-review-agents-shell" (criterion worktree))
(declare-function code-review-start "code-review" (url))
(declare-function code-review-api-submit "code-review-api-send" (repo number &rest args))
(declare-function code-review-api-reconcile "code-review-api-send" (repo number &rest args))
(declare-function code-review-api-set-verdict "code-review-api" (repo number state &optional head-oid &rest args))
(defvar code-review-api-send-timeout)
(defvar code-review-api-send-max-retries)
(defvar code-review-api-send-max-wait)

(defgroup magit-dash-review nil
  "Diff-review and code-review integration for magit-dash."
  :group 'magit-dash)

(defcustom magit-dash-review-agent-auto-resolve nil
  "When non-nil, agent remediation marks review threads resolved automatically.
When nil (the default), agent replies are appended leaving the thread open
for human verification and sign-off."
  :type 'boolean
  :group 'magit-dash-review)

(defcustom magit-dash-review-submit-via-code-review nil
  "When non-nil, `magit-dash-review-submit' sends through code-review.
The review JSON is exported and handed to `code-review-api-submit'
\(ghub, `code-review-auth' accounts) instead of the `gh api graphql'
mutations.  The call blocks Emacs while requests are in flight; see the
\"Submit through code-review\" commentary.  The old GraphQL path stays for one
release, then goes, so this defaults to nil."
  :type 'boolean
  :group 'magit-dash-review)

(defcustom magit-dash-review-sync-viewed t
  "When non-nil, sync viewed marks on submit and on open (code-review path).
Only effective while `magit-dash-review-submit-via-code-review' is non-nil.
On submit, diff-review's blob-keyed marks are mirrored into code-review and
pushed to GitHub (markFileAsViewed, unmarkFileAsViewed through `gh'); on
open, GitHub's viewed files are imported into the review file.  Marks are
used only where the blobs agree.  Best effort: a failure messages and never
blocks the submit."
  :type 'boolean
  :group 'magit-dash-review)

(defcustom magit-dash-review-submit-time-limit 600
  "Seconds the whole send through code-review may take before it is abandoned.
It covers the lookups before and after the send too, so the no-quit region
cannot outlast it.  Abandoning leaves the items in `submit_pending', which
blocks every later submit until `magit-dash-review-clear-submit-pending'."
  :type 'number
  :group 'magit-dash-review)

(defcustom magit-dash-review-use-code-review-adapter nil
  "When non-nil, mirror review JSON into code-review after a pr open or sync.
Calls `magit-dash-review-json-import' on the merged file, which creates or
updates local draft comments (and the pending verdict) on the PR's stored
code-review record through code-review's public `code-review-api-*' functions,
then writes the cr_ids it assigned back into the review file.  The PR must
already be in code-review's database (open it there once), otherwise every
comment is skipped with a message.  Resolved state and GitHub-owned comments
are read-only in code-review and are never written.  Off by default: it is
undecided whether the mirror should be on, and `magit-dash-review-submit'
keeps its GraphQL mutations until submit is routed through code-review.
To see what turning it on would do for a PR without persisting anything, run
`magit-dash-review-json-import-dry-run'.  The merge itself (unknown-key
preserving) is always the adapter's."
  :type 'boolean
  :group 'magit-dash-review)

(defun magit-dash-review-worktree-resolver (repo-dir number)
  "Return the existing magit-dash review worktree of PR NUMBER in REPO-DIR, or nil.
For `code-review-worktree-resolver', which the user's config sets to this
function.  Looks for the pr-N or cr-N worktree
with `magit-dash-pr--find-existing-worktree' only: it never creates one and
never prompts, and any failure yields nil so code-review falls back to its own
worktree."
  (ignore-errors
    (when-let* ((dir (magit-dash-pr--find-existing-worktree
                      (directory-file-name (expand-file-name repo-dir))
                      (if (stringp number) (string-to-number number) number))))
      (directory-file-name (expand-file-name dir)))))

(defun magit-dash-review--maybe-mirror (merged file)
  "Import MERGED into code-review when `magit-dash-review-use-code-review-adapter'.
Comments code-review numbered on import get their cr_id written back into
FILE, so editing one later updates it instead of adding a second copy."
  (when magit-dash-review-use-code-review-adapter
    (let ((stamp (magit-dash-review-json-file-stamp file))
          (result (magit-dash-review-json-import merged)))
      (when (plist-get result :assigned)
        (condition-case nil
            (magit-dash-review--write-json-file
             file (magit-dash-review-json-apply-assigned merged result) stamp)
          (magit-dash-review-json-conflict
           (let* ((pair (magit-dash-review-json-read-with-stamp file))
                  (fresh (car pair))
                  (fresh-stamp (cdr pair))
                  (comments (append (or (map-elt fresh "comments") []) nil))
                  (merged-comments (or (map-elt merged "comments") [])))
             (seq-do (lambda (pair)
                       (when-let* ((src (and (< (car pair) (length merged-comments))
                                             (aref merged-comments (car pair))))
                                   (cid (map-elt src "id"))
                                   (target (seq-find (lambda (c) (equal (map-elt c "id") cid))
                                                     comments)))
                         (setf (map-elt target "cr_id") (cdr pair))))
                     (plist-get result :assigned))
             (magit-dash-review--write-json-file file fresh fresh-stamp))))
        (magit-dash-review--refresh-visiting-buffer file)))))

;;; Core Path & Git Helpers

(defun magit-dash-review--common-dir (root)
  "Return the common git directory for repository at ROOT."
  (or (and (fboundp 'diff-review--common-dir)
           (condition-case nil
               (diff-review--common-dir root)
             (error nil)))
      (let ((default-directory (file-name-as-directory (expand-file-name root))))
        (file-name-as-directory
         (file-truename
          (expand-file-name
           (string-trim
            (with-temp-buffer
              (call-process "git" nil t nil "rev-parse" "--git-common-dir")
              (buffer-string)))
           default-directory))))))

(defun magit-dash-review-file (root pr-number &optional repo)
  "Return path to the review JSON file for PR-NUMBER in repository at ROOT.
The file is under <git-common-dir>/review/.  Without REPO it is pr-<num>.json.
With REPO (owner/name) it is pr-<owner>_<name>-<num>.json so the same PR
number in two repos sharing a git directory (forks) never collides; an
existing legacy pr-<num>.json is still returned only when its recorded repo
is REPO; one recording no repo cannot be verified and is ignored (message)."
  (let* ((common-dir (magit-dash-review--common-dir root))
         (review-dir (expand-file-name "review" common-dir))
         (legacy (expand-file-name (format "pr-%s.json" pr-number) review-dir)))
    (cond
     ((null repo) legacy)
     ((and (file-readable-p legacy)
           (let ((stored (ignore-errors
                           (map-elt (map-elt (magit-dash-review--read-json-file legacy)
                                             "magit_dash")
                                    "repo"))))
             (cond
              ((equal stored repo) t)
              ((member stored '(nil ""))
               (message "magit-dash-review: %s records no repo, so it cannot be verified for %s; ignoring it (add magit_dash.repo to adopt it)"
                        legacy repo)
               nil))))
      legacy)
     (t (expand-file-name (format "pr-%s-%s.json"
                                  (replace-regexp-in-string "[^A-Za-z0-9._-]" "_" repo)
                                  pr-number)
                          review-dir)))))

(defun magit-dash-review--trusted-worktree (file wt-dir)
  "Return WT-DIR when it is a directory of the repository that owns review FILE.
FILE lives in <git-common-dir>/review/.  The worktree recorded inside a
review file is untrusted: when it is missing or belongs to another
repository, message and return the main checkout of FILE's repository."
  (let* ((common (file-name-directory
                  (directory-file-name (file-name-directory (expand-file-name file)))))
         (main (if (equal (file-name-nondirectory (directory-file-name common)) ".git")
                   (file-name-directory (directory-file-name common))
                 common)))
    (cond
     ((and (stringp wt-dir) (file-directory-p wt-dir)
           (equal (file-truename (magit-dash-review--common-dir wt-dir))
                  (file-truename (file-name-as-directory common))))
      wt-dir)
     (t
      (message "magit-dash-review: worktree %s in %s is not a worktree of this repository; using %s"
               wt-dir file main)
      main))))

(defalias 'magit-dash-review--resolve-blob #'magit-dash-review-json-resolve-blob)

;;; Transcoding and Three-Way Merge
;; The implementations live in `magit-dash-review-json'; the old names stay
;; as aliases for existing callers.

(defalias 'magit-dash-review--make-comment-id #'magit-dash-review-json-make-id)
(defalias 'magit-dash-review--transcode-thread #'magit-dash-review-json-transcode-thread)
(defalias 'magit-dash-review--merge-json #'magit-dash-review-json-merge)

;;; JSON File I/O Helpers

(defalias 'magit-dash-review--read-json-file #'magit-dash-review-json-read-file)
(defalias 'magit-dash-review--write-json-file #'magit-dash-review-json-write-file)
(defalias 'magit-dash-review--read-for-write #'magit-dash-review-json-read-for-write)
(defalias 'magit-dash-review--refresh-visiting-buffer
  #'magit-dash-review-json-refresh-visiting-buffer)

(defun magit-dash-review--blank-p (value)
  "Return non-nil when VALUE is nil or the empty string."
  (or (null value) (equal value "")))

(defvar magit-dash-review--id-counter 0
  "Counter mixed into generated ids so ids made in the same second differ.")

(defun magit-dash-review--unique-id (seed)
  "Return a comment id derived from SEED, a counter and a random number."
  (magit-dash-review--make-comment-id
   (format "%s-%d-%d" seed (cl-incf magit-dash-review--id-counter) (random most-positive-fixnum))))

(defun magit-dash-review--current-review-file ()
  "Return the review JSON file path for the current diff-review buffer, or nil."
  (cond
   ((and (derived-mode-p 'diff-review-mode) buffer-file-name)
    buffer-file-name)
   ((and (fboundp 'diff-review--review-buffer)
         (diff-review--review-buffer))
    (buffer-file-name (diff-review--review-buffer)))
   (t nil)))

;;; Diff Hunk Extraction, Finding Extraction, and Safe Reply Writer

(defun magit-dash-review--filter-diff-hunks (diff-text lines)
  "Filter unified DIFF-TEXT to hunks overlapping LINES ([first last]).
If LINES is nil or no specific hunk matches, return DIFF-TEXT."
  (if (or (null lines) (string-empty-p diff-text) (< (length lines) 2))
      diff-text
    (let* ((first-line (elt lines 0))
           (last-line (elt lines 1))
           (raw-parts (split-string diff-text "^@@ -" nil))
           (header (if (and raw-parts (not (string-prefix-p "@@ -" diff-text)))
                       (pop raw-parts)
                     ""))
           (parts (mapcar (lambda (p) (concat "@@ -" p)) raw-parts))
           (matching nil))
      (dolist (hunk parts)
        (if (string-match
             "\\`@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
             hunk)
            (let* ((start (string-to-number (match-string 1 hunk)))
                   (count (if (match-string 2 hunk)
                              (string-to-number (match-string 2 hunk))
                            1))
                   (end (+ start (max count 1) 5))
                   (beg (max 1 (- start 5))))
              (when (and (<= beg last-line) (>= end first-line))
                (push hunk matching)))
          (push hunk matching)))
      (if matching
          (string-trim (concat header (string-join (nreverse matching) "")))
        (string-trim diff-text)))))

(defun magit-dash-review--extract-file-hunk (root base head path &optional lines)
  "Extract unified diff hunk for PATH between BASE and HEAD in ROOT.
When LINES is a 2-element sequence [first last], narrows to matching hunks."
  (if (not (and root (file-directory-p root) path (not (string-empty-p path))))
      ""
    (let ((default-directory (file-name-as-directory (expand-file-name root)))
          (diff-out ""))
      (with-temp-buffer
        (let* ((range-args (cond
                            ((and base head (not (string-empty-p head)))
                             (list (format "%s...%s" base head)))
                            ((and base (not (string-empty-p base)))
                             (list base))
                            (t nil)))
               (code (apply #'call-process "git" nil t nil
                            "diff" "-U5" (append range-args (list "--" path)))))
          (when (or (/= code 0) (zerop (buffer-size)))
            (erase-buffer)
            (when (and base head (not (string-empty-p head)))
              (call-process "git" nil t nil "diff" "-U5" base head "--" path)))
          (setq diff-out (buffer-string))))
      (magit-dash-review--filter-diff-hunks diff-out lines))))

(defun magit-dash-review-extract-findings (review-file &optional unresolved-only)
  "Extract review findings from REVIEW-FILE (diff-review/1 JSON).
When UNRESOLVED-ONLY is non-nil, only unresolved non-draft threads are
included.  Each finding is returned as a plist with keys:
  :id :gh-thread-id :path :lines :line :end-line :blob :author :body
  :resolved :status :replies :hunk :worktree :root :base :head :review-file."
  (let* ((file (expand-file-name review-file))
         (data (magit-dash-review--read-json-file file))
         (root (map-elt data "root"))
         (base (map-elt data "base"))
         (head (map-elt data "head"))
         (md (map-elt data "magit_dash"))
         (worktree (or (and md (map-elt md "worktree")) root))
         (comments (append (or (map-elt data "comments") []) nil))
         (filtered (if unresolved-only
                       (seq-filter
                        (lambda (c)
                          (and (not (eq (map-elt c "resolved") t))
                               (not (equal (map-elt c "status") "draft"))))
                        comments)
                     comments)))
    (mapcar
     (lambda (c)
       (let* ((path (map-elt c "path"))
              (raw-lines (map-elt c "lines"))
              (lines (cond
                      ((vectorp raw-lines) raw-lines)
                      ((and (listp raw-lines) (>= (length raw-lines) 2))
                       (vector (car raw-lines) (cadr raw-lines)))
                      (t nil)))
              (line (and lines (>= (length lines) 1) (aref lines 0)))
              (end-line (and lines (>= (length lines) 2) (aref lines 1)))
              (replies-vec (or (map-elt c "replies") []))
              (replies (mapcar
                        (lambda (r)
                          (list :id (map-elt r "id")
                                :author (map-elt r "author")
                                :at (map-elt r "at")
                                :body (map-elt r "body")))
                        (append replies-vec nil)))
              (hunk (and path
                         (magit-dash-review--extract-file-hunk
                          (or worktree root) base head path lines))))
         (list :id (map-elt c "id")
               :gh-thread-id (map-elt c "gh_thread_id")
               :path path
               :lines lines
               :line line
               :end-line end-line
               :blob (map-elt c "blob")
               :author (map-elt c "author")
               :body (map-elt c "body")
               :resolved (eq (map-elt c "resolved") t)
               :status (map-elt c "status")
               :replies replies
               :hunk (or hunk "")
               :worktree (or worktree root)
               :root root
               :base base
               :head head
               :review-file file)))
     filtered)))

(defun magit-dash-review-append-reply (review-file comment-id author body &optional resolve)
  "Append a reply by AUTHOR with BODY to COMMENT-ID in REVIEW-FILE.
When RESOLVE is non-nil, or when RESOLVE is nil and
`magit-dash-review-agent-auto-resolve' is non-nil, also marks the thread
resolved and sets its `local_resolved' intent marker.  A buffer visiting
FILE with unsaved edits is not refreshed.  Returns the updated comment hash table."
  (let* ((file (expand-file-name review-file))
         (should-resolve (if (null resolve)
                             magit-dash-review-agent-auto-resolve
                           resolve))
         (timestamp (format-time-string "%FT%T%:z"))
         (reply-ht (make-hash-table :test #'equal))
         (live-buf (find-buffer-visiting file))
         (updated-comment nil))
    (setf (map-elt reply-ht "id")
          (magit-dash-review--unique-id
           (format "%s-%s-%s" comment-id (or author "agent-shell") timestamp)))
    (setf (map-elt reply-ht "author") (or author "agent-shell"))
    (setf (map-elt reply-ht "at") timestamp)
    (setf (map-elt reply-ht "body") body)

    (cond
     ((and live-buf
           (with-current-buffer live-buf (derived-mode-p 'diff-review-mode))
           (fboundp 'diff-review--modify-comments))
      (magit-dash-review-json-save-visiting-buffer file)
      (with-current-buffer live-buf
        (diff-review--modify-comments
         (lambda ()
           (let ((target (seq-find
                          (lambda (c)
                            (or (equal (map-elt c "id") comment-id)
                                (equal (map-elt c "gh_thread_id") comment-id)))
                          diff-review--comments)))
             (unless target
               (error "No comment with id %s in %s" comment-id file))
             (setf (map-elt target "replies")
                   (vconcat (or (map-elt target "replies") [])
                            (vector reply-ht)))
             (when should-resolve
               (setf (map-elt target "resolved") t)
               (setf (map-elt target "local_resolved") t)
               (map-delete target "gh_reopened"))
             (setq updated-comment target))))))
     (t
      (let ((attempt 0)
            (done nil))
        (while (not done)
          (setq attempt (1+ attempt))
          (let* ((pair (magit-dash-review-json-read-with-stamp file))
                 (data (car pair))
                 (stamp (cdr pair))
                 (comments (or (map-elt data "comments") []))
                 (target (seq-find
                          (lambda (c)
                            (or (equal (map-elt c "id") comment-id)
                                (equal (map-elt c "gh_thread_id") comment-id)))
                          comments)))
            (unless target
              (error "No comment with id %s in %s" comment-id file))
            (setf (map-elt target "replies")
                  (vconcat (or (map-elt target "replies") [])
                           (vector reply-ht)))
            (when should-resolve
              (setf (map-elt target "resolved") t)
              (setf (map-elt target "local_resolved") t)
              (map-delete target "gh_reopened"))
            (setf (map-elt data "comments") comments)
            (condition-case err
                (progn
                  (magit-dash-review--write-json-file file data stamp)
                  (setq updated-comment target)
                  (setq done t))
              (magit-dash-review-json-conflict
               (when (>= attempt 3)
                 (signal (car err) (cdr err)))))))
        (magit-dash-review--refresh-visiting-buffer file))))
    updated-comment))

(defun magit-dash-review-current-hunk ()
  "Extract review context and diff hunk around point in a diff-review buffer.
Returns a plist (:review-file :root :base :head :path :line :end-line :hunk
:comment-id :removed)."
  (let* ((review-buf (and (fboundp 'diff-review--review-buffer)
                          (diff-review--review-buffer)))
         (review-file (and review-buf (buffer-file-name review-buf)))
         (root (or (and review-buf (buffer-local-value 'diff-review--root review-buf))
                   default-directory))
         (base (and review-buf
                    (or (buffer-local-value 'diff-review--base-name review-buf)
                        (buffer-local-value 'diff-review--base review-buf))))
         (head (and review-buf (buffer-local-value 'diff-review--head review-buf)))
         (path (or (bound-and-true-p diff-review--file-path)
                   (and buffer-file-name root
                        (file-relative-name buffer-file-name root))))
         (line-pair (if (and (fboundp 'diff-review--region-lines)
                             (bound-and-true-p diff-review-file-mode))
                        (diff-review--region-lines)
                      (if (use-region-p)
                          (cons (line-number-at-pos (region-beginning) t)
                                (line-number-at-pos (max (region-beginning) (1- (region-end))) t))
                        (let ((l (line-number-at-pos nil t)))
                          (cons l l)))))
         (first-line (car line-pair))
         (last-line (cdr line-pair))
         (local-hunk (and (bound-and-true-p diff-review--hunks)
                          (seq-find
                           (lambda (h)
                             (let* ((h-start (plist-get h :line))
                                    (h-count (max 1 (or (plist-get h :count) 1)))
                                    (h-end (+ h-start h-count)))
                               (and (<= (max 1 (- h-start 3)) last-line)
                                    (>= (+ h-end 3) first-line))))
                           diff-review--hunks)))
         (removed (and local-hunk (plist-get local-hunk :removed)))
         (threads-here (and (fboundp 'diff-review--threads-at-point)
                            (bound-and-true-p diff-review-file-mode)
                            (diff-review--threads-at-point)))
         (comment-here (and threads-here
                            (overlay-get (car threads-here) 'diff-review-comment)))
         (comment-id (and comment-here (map-elt comment-here "id")))
         (hunk-text (if (and path root)
                        (magit-dash-review--extract-file-hunk
                         root base head path (vector first-line last-line))
                      "")))
    (list :review-file review-file
          :root root
          :base base
          :head head
          :path path
          :line first-line
          :end-line last-line
          :hunk hunk-text
          :comment-id comment-id
          :removed removed)))

;;; PR Worktree Orchestration, Hydration, and Front-End Entrypoints

(defun magit-dash-review--parse-pr-target (pr-number-or-url &optional repo-dir)
  "Normalize PR-NUMBER-OR-URL and REPO-DIR into `(:number :repo :repo-dir :pr)'."
  (let* ((dir (expand-file-name
               (magit-dash-worktree-main-dir
                (or repo-dir
                    (ignore-errors (magit-dash-worktree-main-dir default-directory))
                    default-directory))))
         (slug nil)
         (num nil)
         (pr-data nil))
    (cond
     ((and (listp pr-number-or-url) (plist-get pr-number-or-url :number))
      (setq num (plist-get pr-number-or-url :number)
            slug (plist-get pr-number-or-url :repo)
            pr-data (plist-get pr-number-or-url :pr)))
     ((integerp pr-number-or-url)
      (setq num pr-number-or-url))
     ((and (stringp pr-number-or-url)
           (string-match "github\\.com/\\([^/]+/[^/]+\\)/pull/\\([0-9]+\\)" pr-number-or-url))
      (setq slug (match-string 1 pr-number-or-url)
            num (string-to-number (match-string 2 pr-number-or-url))))
     ((and (stringp pr-number-or-url)
           (string-match "\\`#?\\([0-9]+\\)\\'" (string-trim pr-number-or-url)))
      (setq num (string-to-number (match-string 1 (string-trim pr-number-or-url))))))
    (unless slug
      (setq slug (or (ignore-errors
                       (let ((default-directory dir))
                         (magit-dash-pr-dashboard--current-repo-slug)))
                     "local/repo")))
    (unless (and (integerp num) (> num 0))
      (error "Invalid pull request number or URL: %S" pr-number-or-url))
    (list :number num :repo slug :repo-dir dir :pr pr-data)))

(defun magit-dash-review--fetch-pr-metadata (slug pr-number repo-dir)
  "Fetch pull request metadata for PR-NUMBER in SLUG via `gh pr view'."
  (pcase-let ((`(,code . ,output)
               (magit-dash-gh--call-process
                (list "pr" "view" (number-to-string pr-number)
                      "-R" slug
                      "--json" "number,title,url,headRefName,baseRefName,headRefOid,baseRefOid,state,reviewDecision,isCrossRepository,headRepositoryOwner")
                repo-dir)))
    (unless (zerop code)
      (error "Failed to fetch PR metadata for %s#%d: %s" slug pr-number output))
    (json-parse-string output :object-type 'alist)))

(defun magit-dash-review--open-front-end (review-file)
  "Open REVIEW-FILE in `diff-review-find-file' when available, else `find-file'."
  (if (fboundp 'diff-review-find-file)
      (diff-review-find-file review-file)
    (find-file-noselect review-file))
  review-file)

(defun magit-dash-review--merge-and-write (file remote-meta transcoded &optional allow-missing)
  "Read FILE, merge REMOTE-META and TRANSCODED, and write atomically.
When ALLOW-MISSING is non-nil, FILE need not exist yet.  Retries up to 3
times if `magit-dash-review-json-conflict' signals because FILE changed
on disk between read and write."
  (let ((attempt 0)
        (done nil)
        (merged nil))
    (while (not done)
      (setq attempt (1+ attempt))
      (let* ((exists (file-exists-p file))
             (_ (when exists (magit-dash-review-json-save-visiting-buffer file)))
             (stamp (if exists (magit-dash-review-json-file-stamp file) :missing))
             (existing (cond
                        (exists
                         (magit-dash-review--read-for-write file))
                        (allow-missing nil)
                        (t (error "Review file not readable: %s" file)))))
        (setq merged (magit-dash-review--merge-json existing remote-meta transcoded))
        (condition-case err
            (progn
              (magit-dash-review--write-json-file file merged stamp)
              (setq done t))
          (magit-dash-review-json-conflict
           (when (>= attempt 3)
             (signal (car err) (cdr err)))))))
    merged))

;;;###autoload
(defun magit-dash-review-pr (&optional pr-number-or-url repo-dir)
  "Hydrate and open a native-buffer `diff-review' session for PR-NUMBER-OR-URL.
Reuses or creates the `.wt/pr-<num>' worktree via `magit-dash-pr', fetches
PR metadata and GraphQL reviewThreads, performs a three-way merge into
`<git-common-dir>/review/pr-<num>.json', and opens `diff-review-find-file'."
  (interactive
   (list (read-string "PR number or URL: ") default-directory))
  (let* ((target (magit-dash-review--parse-pr-target pr-number-or-url repo-dir))
         (pr-num (plist-get target :number))
         (slug (plist-get target :repo))
         (dir (plist-get target :repo-dir))
         (pr-info (or (plist-get target :pr)
                      (magit-dash-review--fetch-pr-metadata slug pr-num dir)))
         (head-ref (or (map-elt pr-info 'headRefName) (format "pr-%d" pr-num)))
         (base-ref (or (map-elt pr-info 'baseRefName) "main"))
         (head-oid (map-elt pr-info 'headRefOid))
         (base-oid (map-elt pr-info 'baseRefOid))
         (url (map-elt pr-info 'url))
         (existing-wt (cond
                       ((map-elt pr-info 'headRefName)
                        (alist-get 'path (magit-dash-pr--find-corroborated-worktree
                                          dir pr-num pr-info slug)))
                       (pr-info
                        (let ((branch-hint (unless (magit-dash-pr--fork-pr-p pr-info slug)
                                             (map-elt pr-info 'headRefName))))
                          (magit-dash-pr--find-existing-worktree dir pr-num branch-hint)))
                       (t
                        (magit-dash-pr--find-existing-worktree dir pr-num nil))))
         (wt-dir (or existing-wt
                     (condition-case err
                         (magit-dash-pr--create-worktree-for-entry
                          (list :number pr-num :repo slug :pr pr-info :repo-dir dir) nil)
                       (error
                        (user-error "Cannot create a worktree for PR #%d (%s); not reviewing in the main checkout"
                                    pr-num (error-message-string err))))))
         (raw-threads (or (ignore-errors
                            (magit-dash-pr-fetch-threads-graphql slug pr-num 50 4000 dir t))
                          nil))
         (transcoded (mapcar
                      (lambda (node)
                        (magit-dash-review--transcode-thread node wt-dir head-oid base-oid))
                      raw-threads))
         (review-file (magit-dash-review-file dir pr-num slug))
         (live-buf (find-buffer-visiting review-file))
         (remote-meta (list :repo slug
                            :number pr-num
                            :url url
                            :worktree (file-name-as-directory (expand-file-name wt-dir))
                            :head-ref head-ref
                            :base-ref base-ref
                            :head_oid head-oid
                            :base_oid base-oid
                            :threads-complete (and raw-threads (< (length raw-threads) 50)))))
    (cond
     ((and live-buf (buffer-modified-p live-buf))
      (message "magit-dash-review-pr: %s has unsaved edits; not merging remote threads into it"
               (buffer-name live-buf)))
     (t
      ;; Read only now, after every network call above, so edits made
      ;; meanwhile are merged rather than overwritten.
      (let ((merged (magit-dash-review--merge-and-write
                     review-file remote-meta transcoded t)))
        (magit-dash-review--maybe-mirror merged review-file)
        (magit-dash-review--refresh-visiting-buffer review-file)
        (magit-dash-review--maybe-import-viewed review-file slug pr-num dir))))
    (magit-dash-review--open-front-end review-file)))

;;;###autoload
(defun magit-dash-review-worktree-diff (&optional worktree-dir base-ref)
  "Open a `diff-review' session for uncommitted changes in WORKTREE-DIR.
Writes `<git-common-dir>/review/worktree-<name>.json' with `head' omitted
so `diff-review' compares the live working tree against BASE-REF (default
\"HEAD\")."
  (interactive)
  (let* ((wt-dir (file-name-as-directory
                  (expand-file-name (or worktree-dir default-directory))))
         (wt-name (file-name-nondirectory (directory-file-name wt-dir)))
         (common-dir (magit-dash-review--common-dir wt-dir))
         (review-file (expand-file-name
                       (format "review/worktree-%s.json" wt-name)
                       common-dir))
         (pair (when (file-exists-p review-file)
                 (magit-dash-review-json-read-with-stamp review-file)))
         (existing-ht (car pair))
         (stamp (if pair (cdr pair) :missing))
         (data (or existing-ht (make-hash-table :test #'equal))))
    (setf (map-elt data "format") "diff-review/1")
    (setf (map-elt data "root") wt-dir)
    (setf (map-elt data "base") (or base-ref (and existing-ht (map-elt existing-ht "base")) "HEAD"))
    (map-delete data "head")
    (unless (hash-table-p (map-elt data "viewed"))
      (setf (map-elt data "viewed") (make-hash-table :test #'equal)))
    (unless (vectorp (map-elt data "comments"))
      (setf (map-elt data "comments") []))
    (magit-dash-review--write-json-file review-file data stamp)
    (magit-dash-review--refresh-visiting-buffer review-file)
    (magit-dash-review--open-front-end review-file)))

;;;###autoload
(defun magit-dash-review-sync (&optional review-file)
  "Refetch remote PR threads and metadata and three-way merge into REVIEW-FILE."
  (interactive)
  (let* ((file (expand-file-name
                (or review-file
                    (magit-dash-review--current-review-file)
                    (user-error "No active review JSON file to sync"))))
         (existing-ht (magit-dash-review--read-json-file file))
         (md (map-elt existing-ht "magit_dash"))
         (slug (and md (map-elt md "repo")))
         (pr-num (and md (map-elt md "number")))
         (wt-dir (magit-dash-review--trusted-worktree
                  file
                  (or (and md (map-elt md "worktree"))
                      (map-elt existing-ht "root")
                      default-directory))))
    (unless (and slug pr-num)
      (user-error "Review file %s has no `magit_dash' PR metadata to sync" file))
    (let* ((pr-info (magit-dash-review--fetch-pr-metadata slug pr-num wt-dir))
           (head-oid (or (map-elt pr-info 'headRefOid) (map-elt md "head_oid")))
           (base-oid (or (map-elt pr-info 'baseRefOid) (map-elt md "base_oid")))
           (raw-threads (magit-dash-pr-fetch-threads-graphql slug pr-num 50 4000 wt-dir t))
           (transcoded (mapcar
                        (lambda (node)
                          (magit-dash-review--transcode-thread node wt-dir head-oid base-oid))
                        raw-threads))
           (remote-meta (list :repo slug
                              :number pr-num
                              :url (or (map-elt pr-info 'url) (map-elt md "url"))
                              :worktree wt-dir
                              :head-ref (or (map-elt pr-info 'headRefName) (map-elt existing-ht "head"))
                              :base-ref (or (map-elt pr-info 'baseRefName) (map-elt existing-ht "base"))
                              :head_oid head-oid
                              :base_oid base-oid
                              :threads-complete (< (length raw-threads) 50)))
           (live-buf (find-buffer-visiting file)))
      (cond
       ((and live-buf (buffer-modified-p live-buf))
        (message "magit-dash-review-sync: %s has unsaved edits; save or revert it, then sync again"
                 (buffer-name live-buf))
        nil)
       (t
        ;; Merge against a fresh read: the file may have changed while the
        ;; network calls above ran.
        (let ((merged (magit-dash-review--merge-and-write file remote-meta transcoded)))
          (magit-dash-review--maybe-mirror merged file)
          (magit-dash-review--refresh-visiting-buffer file)
          (magit-dash-review--maybe-import-viewed file slug pr-num wt-dir)
          file))))))

;;; GraphQL Delta Submission
;; Legacy path, used unless `magit-dash-review-submit-via-code-review' is set (removed later).  Every
;; request goes through `magit-dash-review--gh-graphql': the query and its
;; variables travel as one JSON body in a temporary file (`gh api --input'),
;; never as `-F key=value' fields, because `-F' reads `@file' values from
;; disk and coerces numbers, true and null, which would let review text
;; choose what gets sent.

(defconst magit-dash-review--submit-chunk-size 40
  "Obsolete: unused now that new threads join one pending review.")

(defun magit-dash-review--gh-graphql (query variables dir)
  "Run GraphQL QUERY with VARIABLES, a hash table, with `gh' in DIR.
Return the parsed response alist.  Signal on a non-zero exit or when the
response has errors."
  (let ((input-file (make-temp-file "magit-dash-gh-" nil ".json"))
        (payload (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (setf (map-elt payload "query") query)
          (setf (map-elt payload "variables") variables)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert (json-serialize payload :null-object nil :false-object :false))
            (let ((coding-system-for-write 'binary))
              (write-region (point-min) (point-max) input-file nil 'silent)))
          (pcase-let ((`(,code . ,output)
                       (magit-dash-gh--call-process
                        (list "api" "graphql"
                              "-H" "Content-Type: application/json"
                              "--input" input-file)
                        dir)))
            (unless (zerop code)
              (error "gh api graphql failed: %s" output))
            (let ((resp (json-parse-string output :object-type 'alist :array-type 'list)))
              (when (map-elt resp 'errors)
                (error "GraphQL errors: %S" (map-elt resp 'errors)))
              resp)))
      (when (file-exists-p input-file)
        (delete-file input-file)))))

(defun magit-dash-review--verdict-to-gh-event (verdict-ht)
  "Map VERDICT-HT state to a GitHub PullRequestReviewEvent string, or nil.
An unknown or missing state gives nil, so no review is posted for it."
  (let ((state (and (hash-table-p verdict-ht) (map-elt verdict-ht "state"))))
    (pcase state
      ("approved" "APPROVE")
      ("changes" "REQUEST_CHANGES")
      ("comment" "COMMENT")
      (_ nil))))

(defun magit-dash-review--verdict-pending-p (verdict-ht)
  "Return non-nil when VERDICT-HT names a known state not yet sent as it stands.
A verdict is sent when it has no `submitted_at', or its state or head differ
from `submitted_state' and `submitted_head'."
  (and (hash-table-p verdict-ht)
       (magit-dash-review--verdict-to-gh-event verdict-ht)
       (not (and (map-elt verdict-ht "submitted_at")
                 (equal (map-elt verdict-ht "submitted_state") (map-elt verdict-ht "state"))
                 (or (magit-dash-review--blank-p (map-elt verdict-ht "head"))
                     (equal (map-elt verdict-ht "head") (map-elt verdict-ht "submitted_head")))))))

(defun magit-dash-review--collect-unsynced-threads (comments)
  "Return non-draft local comments in COMMENTS never sent to GitHub.
Those have no `gh_thread_id' and a status other than draft or sent."
  (seq-filter
   (lambda (c)
     (let ((gh-id (map-elt c "gh_thread_id"))
           (status (map-elt c "status"))
           (path (map-elt c "path")))
       (and (or (null gh-id) (string-empty-p gh-id))
            (not (member status '("draft" "sent")))
            path (not (string-empty-p path)))))
   (append (or comments []) nil)))

(defun magit-dash-review--fetch-pr-node-id (slug pr-number dir)
  "Fetch the GraphQL node ID for PR-NUMBER in SLUG."
  (let* ((parts (split-string slug "/"))
         (vars (make-hash-table :test #'equal))
         (query "query($owner:String! $repo:String! $number:Int!){
  repository(owner:$owner name:$repo){
    pullRequest(number:$number){ id }
  }
}"))
    (setf (map-elt vars "owner") (car parts))
    (setf (map-elt vars "repo") (cadr parts))
    (setf (map-elt vars "number") pr-number)
    (or (map-nested-elt (magit-dash-review--gh-graphql query vars dir)
                        '(data repository pullRequest id))
        (error "Missing pullRequest.id in GraphQL response for %s#%d" slug pr-number))))

(defun magit-dash-review--thread-payload (c)
  "Return the DraftPullRequestReviewThread table for local comment C."
  (let* ((t-ht (make-hash-table :test #'equal))
         (lines (append (map-elt c "lines") nil))
         (l1 (nth 0 lines))
         (l2 (nth 1 lines))
         (side (map-elt c "diff_side")))
    (setf (map-elt t-ht "path") (map-elt c "path"))
    (setf (map-elt t-ht "body") (or (map-elt c "body") ""))
    (when (or l2 l1)
      (setf (map-elt t-ht "line") (or l2 l1)))
    (when (and l1 l2 (< l1 l2))
      (setf (map-elt t-ht "startLine") l1))
    (when (member side '("LEFT" "RIGHT"))
      (setf (map-elt t-ht "side") side)
      (when (map-elt t-ht "startLine")
        (setf (map-elt t-ht "startSide") side)))
    t-ht))

(defun magit-dash-review--node-matches-p (c node strict)
  "Return non-nil when returned review comment NODE is local comment C.
Path and trimmed body must be equal; when STRICT the line must be too,
as far as NODE reports one."
  (let* ((lines (append (map-elt c "lines") nil))
         (line (or (nth 1 lines) (nth 0 lines)))
         (node-line (or (map-elt node 'line) (map-elt node 'originalLine))))
    (and (equal (map-elt c "path") (map-elt node 'path))
         (equal (string-trim (or (map-elt c "body") ""))
                (string-trim (or (map-elt node 'body) "")))
         (or (not strict) (null node-line) (null line) (equal line node-line)))))

(defun magit-dash-review--pair-review-nodes (locals nodes)
  "Return an alist (LOCAL . NODE) pairing LOCALS with returned NODES.
Each node is used once.  Pairing is by path, line and body first, then by
path and body for the comments left over; never by position."
  (let ((remaining (copy-sequence nodes))
        (pairs nil))
    (seq-do (lambda (strict)
              (seq-do (lambda (c)
                        (unless (assq c pairs)
                          (when-let* ((node (seq-find (lambda (n)
                                                        (magit-dash-review--node-matches-p c n strict))
                                                      remaining)))
                            (setq remaining (delq node remaining))
                            (push (cons c node) pairs))))
                      locals))
            '(t nil))
    pairs))

(defun magit-dash-review--create-pending-review (pr-id head dir)
  "Create a PENDING GitHub review on PR-ID at commit HEAD in DIR.
Return the new pull request review GraphQL node ID."
  (let* ((input-ht (make-hash-table :test #'equal))
         (vars-ht (make-hash-table :test #'equal))
         (mutation "mutation($input:AddPullRequestReviewInput!){
  addPullRequestReview(input:$input){
    pullRequestReview{
      id
      state
    }
  }
}"))
    (setf (map-elt input-ht "pullRequestId") pr-id)
    (when head
      (setf (map-elt input-ht "commitOID") head))
    (setf (map-elt vars-ht "input") input-ht)
    (or (map-nested-elt (magit-dash-review--gh-graphql mutation vars-ht dir)
                        '(data addPullRequestReview pullRequestReview id))
        (error "Missing pullRequestReview.id in GraphQL response"))))

(defun magit-dash-review--start-pending-review (data pr-id slug pr-num head save dir)
  "Create a pending review on GitHub and persist it in DATA via SAVE.
Records `pending_review' with PR-ID, SLUG, PR-NUM and HEAD in DIR, then
returns the new pending review GraphQL ID."
  (let ((review-id (magit-dash-review--create-pending-review pr-id head dir))
        (rec (make-hash-table :test #'equal)))
    (setf (map-elt rec "id") review-id)
    (setf (map-elt rec "pr_node_id") pr-id)
    (setf (map-elt rec "repo") slug)
    (setf (map-elt rec "number") pr-num)
    (when head
      (setf (map-elt rec "head_oid") head))
    (setf (map-elt rec "created_at") (format-time-string "%FT%T%:z"))
    (setf (map-elt data "pending_review") rec)
    (funcall save)
    review-id))

(defun magit-dash-review--add-review-thread (review-id c dir)
  "Add local comment C as a thread on pending review REVIEW-ID in DIR.
Stamp returned GitHub IDs onto C and mark it sent."
  (let* ((input-ht (magit-dash-review--thread-payload c))
         (vars-ht (make-hash-table :test #'equal))
         (mutation "mutation($input:AddPullRequestReviewThreadInput!){
  addPullRequestReviewThread(input:$input){
    thread{
      id
      comments(first:10){
        nodes{ id path body line originalLine }
      }
    }
  }
}"))
    (setf (map-elt input-ht "pullRequestReviewId") review-id)
    (setf (map-elt vars-ht "input") input-ht)
    (let* ((resp (magit-dash-review--gh-graphql mutation vars-ht dir))
           (thread (map-nested-elt resp '(data addPullRequestReviewThread thread)))
           (comment-nodes (map-nested-elt thread '(comments nodes)))
           (pairs (magit-dash-review--pair-review-nodes (list c) comment-nodes))
           (node (alist-get c pairs nil nil #'eq))
           (cid (map-elt node 'id))
           (tid (if node
                    (or (map-nested-elt node '(pullRequestReviewThread id))
                        (map-elt thread 'id))
                  (unless comment-nodes
                    (map-elt thread 'id)))))
      (unless node
        (message "magit-dash-review-submit: posted %s:%s but GitHub returned no matching comment; the next sync will link it"
                 (map-elt c "path") (map-elt c "lines")))
      (when tid
        (setf (map-elt c "gh_thread_id") tid))
      (when cid
        (setf (map-elt c "gh_comment_id") cid))
      (setf (map-elt c "origin") "github")
      (setf (map-elt c "status") "sent")
      (setf (map-elt c "gh_resolved") :false))))

(defun magit-dash-review--submit-pending-review (review-id event feedback dir)
  "Submit pending review REVIEW-ID with EVENT and optional FEEDBACK in DIR.
Return the resulting review state string."
  (let* ((input-ht (make-hash-table :test #'equal))
         (vars-ht (make-hash-table :test #'equal))
         (mutation "mutation($input:SubmitPullRequestReviewInput!){
  submitPullRequestReview(input:$input){
    pullRequestReview{
      id
      state
    }
  }
}"))
    (setf (map-elt input-ht "pullRequestReviewId") review-id)
    (setf (map-elt input-ht "event") event)
    (when (and feedback (not (string-empty-p feedback)))
      (setf (map-elt input-ht "body") feedback))
    (setf (map-elt vars-ht "input") input-ht)
    (or (map-nested-elt (magit-dash-review--gh-graphql mutation vars-ht dir)
                        '(data submitPullRequestReview pullRequestReview state))
        (error "Missing pullRequestReview.state in GraphQL response"))))

(defun magit-dash-review--fetch-review-node (review-id dir)
  "Fetch `PullRequestReview' state and commit oid for REVIEW-ID in DIR."
  (let ((vars (make-hash-table :test #'equal))
        (query "query($id:ID!){
  node(id:$id){
    ... on PullRequestReview{
      id
      state
      commit{ oid }
    }
  }
}"))
    (setf (map-elt vars "id") review-id)
    (map-nested-elt (magit-dash-review--gh-graphql query vars dir)
                    '(data node))))

(defun magit-dash-review--stale-pending-review-error-p (err)
  "Return non-nil when ERR indicates a pending review is missing or submitted."
  (let ((case-fold-search t))
    (string-match-p
     "already been submitted\\|already submitted\\|Could not resolve to a node\\|not pending\\|no longer pending\\|pullRequestReview.*not found\\|\\breview\\b.*not found"
     (error-message-string err))))

(defun magit-dash-review--stamp-verdict (verdict-ht head)
  "Stamp VERDICT-HT as submitted at HEAD."
  (setf (map-elt verdict-ht "submitted_state") (map-elt verdict-ht "state"))
  (setf (map-elt verdict-ht "submitted_head") (or head ""))
  (setf (map-elt verdict-ht "submitted_at") (format-time-string "%FT%T%:z")))

(defun magit-dash-review--submit-threads (chunk event feedback pr-id head dir)
  "Post the new threads in CHUNK as one review with EVENT and FEEDBACK.
PR-ID is the pull request node id and HEAD the commit to attach it to.
Stamp the GitHub ids onto each comment of CHUNK, mark it sent, and return
the review state string."
  (let* ((input-ht (make-hash-table :test #'equal))
         (vars-ht (make-hash-table :test #'equal))
         (mutation "mutation($input:AddPullRequestReviewInput!){
  addPullRequestReview(input:$input){
    pullRequestReview{
      id
      state
      comments(first:50){
        nodes{ id path body line originalLine pullRequestReviewThread{ id } }
      }
    }
  }
}"))
    (setf (map-elt input-ht "pullRequestId") pr-id)
    (setf (map-elt input-ht "event") event)
    (when head
      (setf (map-elt input-ht "commitOID") head))
    (when (and feedback (not (string-empty-p feedback)))
      (setf (map-elt input-ht "body") feedback))
    (when chunk
      (setf (map-elt input-ht "threads")
            (vconcat (seq-map #'magit-dash-review--thread-payload chunk))))
    (setf (map-elt vars-ht "input") input-ht)
    (let* ((resp (magit-dash-review--gh-graphql mutation vars-ht dir))
           (review (map-nested-elt resp '(data addPullRequestReview pullRequestReview)))
           (pairs (magit-dash-review--pair-review-nodes
                   chunk (map-nested-elt review '(comments nodes)))))
      (seq-do (lambda (c)
                (let* ((node (alist-get c pairs nil nil #'eq))
                       (cid (map-elt node 'id))
                       (tid (map-nested-elt node '(pullRequestReviewThread id))))
                  (unless node
                    (message "magit-dash-review-submit: posted %s:%s but GitHub returned no matching comment; the next sync will link it"
                             (map-elt c "path") (map-elt c "lines")))
                  (when tid (setf (map-elt c "gh_thread_id") tid))
                  (when cid (setf (map-elt c "gh_comment_id") cid))
                  (setf (map-elt c "origin") "github")
                  (setf (map-elt c "status") "sent")
                  (setf (map-elt c "gh_resolved") :false)))
              chunk)
      (map-elt review 'state))))

(defun magit-dash-review--post-reply (c reply dir)
  "Post REPLY on the GitHub thread of comment C and stamp it."
  (let ((vars (make-hash-table :test #'equal))
        (mutation "mutation($threadId:ID! $body:String!){
  addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:$threadId body:$body}){
    comment{ id }
  }
}"))
    (setf (map-elt vars "threadId") (map-elt c "gh_thread_id"))
    (setf (map-elt vars "body") (or (map-elt reply "body") ""))
    (let ((new-cid (map-nested-elt (magit-dash-review--gh-graphql mutation vars dir)
                                   '(data addPullRequestReviewThreadReply comment id))))
      (when new-cid
        (setf (map-elt reply "gh_comment_id") new-cid))
      (setf (map-elt reply "status") "sent"))))

(defun magit-dash-review--set-thread-resolution (c resolve dir)
  "Resolve (RESOLVE non-nil) or unresolve the GitHub thread of comment C."
  (let ((vars (make-hash-table :test #'equal))
        (mutation (if resolve
                      "mutation($threadId:ID!){ resolveReviewThread(input:{threadId:$threadId}){ thread{ id isResolved } } }"
                    "mutation($threadId:ID!){ unresolveReviewThread(input:{threadId:$threadId}){ thread{ id isResolved } } }")))
    (setf (map-elt vars "threadId") (map-elt c "gh_thread_id"))
    (magit-dash-review--gh-graphql mutation vars dir)
    (setf (map-elt c "gh_resolved") (if resolve t :false))
    (map-delete c "local_resolved")))

(defun magit-dash-review--resolution-intent (c)
  "Return `resolve', `unresolve' or nil: what submit should do to C's thread.
Resolve only for a local intent: `local_resolved' set by a local resolve, or
a local resolved state GitHub never saw resolved (no `gh_reopened').  A
thread reopened on GitHub after the local resolve is left open."
  (let ((local-res (eq (map-elt c "resolved") t))
        (remote-res (eq (map-elt c "gh_resolved") t)))
    (cond
     ((and local-res (not remote-res)
           (or (eq (map-elt c "local_resolved") t)
               (not (eq (map-elt c "gh_reopened") t))))
      'resolve)
     ((and (not local-res) remote-res) 'unresolve))))

(defun magit-dash-review--check-verdict-head (md verdict-ht event current-head &optional extra-head)
  "Refuse an APPROVE or REQUEST_CHANGES EVENT for a stale review.
The head the review was made at (MD head_oid, VERDICT-HT head, and optional
EXTRA-HEAD such as a pending review's head_oid) must be CURRENT-HEAD, the
pull request's head now."
  (when (member event '("APPROVE" "REQUEST_CHANGES"))
    (seq-do (lambda (reviewed)
              (when (and (not (magit-dash-review--blank-p reviewed))
                         current-head
                         (not (equal reviewed current-head)))
                (user-error "Refusing to send %s: reviewed %s but the PR head is now %s; sync and re-review the new commits first"
                            event (substring reviewed 0 (min 12 (length reviewed)))
                            (substring current-head 0 (min 12 (length current-head))))))
            (list (map-elt md "head_oid") (map-elt verdict-ht "head") extra-head))))

;;;###autoload
(defun magit-dash-review-submit (&optional review-file feedback)
  "Publish non-draft threads, replies, resolutions, and verdict in REVIEW-FILE.
Optionally attach FEEDBACK as the review summary.  Excludes comments with
`status: \"draft\"'.  Stamps returned GitHub GraphQL node IDs back into
REVIEW-FILE, writing the file after every successful mutation, so a failure
part way leaves what was sent recorded and a repeated submit sends only the
remainder.  New threads join a single pending review (persisted as
`pending_review' before any thread is posted), are added one by one and
matched by path, line and body, then submitted once at the end.  The verdict
is sent once (stamped `submitted_state', `submitted_head', `submitted_at')
and again only when it changes; APPROVE and REQUEST_CHANGES are refused when
the PR head moved past the reviewed head.  Replies and resolutions skip
threads flagged `gh_missing'; an error on one thread is collected and the
others still run, then reported together.

With `magit-dash-review-submit-via-code-review' non-nil the review is sent by
`code-review-api-submit' instead (see \"Submit through code-review\"): every
acknowledged item is stamped into REVIEW-FILE as it is acknowledged, an
uncertain failure is reconciled against GitHub and never resent, and the call
blocks Emacs while GitHub answers."
  (interactive
   (list (or (magit-dash-review--current-review-file)
             (user-error "No active review JSON file to submit"))
         (read-string "Overall review summary (optional): ")))
  (let* ((file (expand-file-name
                (or review-file
                    (magit-dash-review--current-review-file)
                    (user-error "No active review JSON file to submit")))))
    (if magit-dash-review-submit-via-code-review
        (magit-dash-review--cr-submit file feedback)
      (magit-dash-review--legacy-submit file feedback))))

(defun magit-dash-review--legacy-submit (file feedback)
  "Submit the review in FILE with FEEDBACK through the GraphQL mutations.
The legacy body of `magit-dash-review-submit'; see there."
  (let ((lock (magit-dash-review--cr-lock file)))
    (unwind-protect
        (let* ((pair (magit-dash-review-json-read-with-stamp file))
               (data (car pair))
               (save-stamp (cdr pair))
               (_pending (when (map-elt data "submit_pending")
                           (user-error "%s has an unfinished code-review submit (`submit_pending'): turn `magit-dash-review-submit-via-code-review' back on so it is reconciled first, or run M-x magit-dash-review-clear-submit-pending" file)))
               (md (map-elt data "magit_dash"))
               (slug (and md (map-elt md "repo")))
               (pr-num (and md (map-elt md "number")))
               (dir (magit-dash-review--trusted-worktree
                     file
                     (or (and md (map-elt md "worktree"))
                         (map-elt data "root")
                         default-directory)))
               (comments (append (or (map-elt data "comments") []) nil))
               (verdict-ht (map-elt data "verdict"))
               (new-threads (magit-dash-review--collect-unsynced-threads comments))
               (event (magit-dash-review--verdict-to-gh-event verdict-ht))
               (verdict-pending (magit-dash-review--verdict-pending-p verdict-ht))
               (feedback-p (and feedback (not (string-empty-p feedback))))
               (errors nil)
               (save (lambda ()
                       (setf (map-elt data "comments") (vconcat comments))
                       (magit-dash-review--write-json-file file data save-stamp)
                       (setq save-stamp (magit-dash-review-json-file-stamp file))))
               (pending-rev (map-elt data "pending_review"))
               (pending-valid
                (and (hash-table-p pending-rev)
                     (not (magit-dash-review--blank-p (map-elt pending-rev "id")))
                     (or (null (map-elt pending-rev "repo"))
                         (equal (map-elt pending-rev "repo") slug))
                     (or (null (map-elt pending-rev "number"))
                         (equal (map-elt pending-rev "number") pr-num))))
               (pending-id (and pending-valid (map-elt pending-rev "id"))))
          (unless (and slug pr-num)
            (user-error "Review file %s lacks `magit_dash' PR metadata" file))
          (when (and pending-rev (not pending-valid))
            (map-delete data "pending_review")
            (funcall save))
          (when (and verdict-ht (not event))
            (message "magit-dash-review-submit: unknown verdict state %S; no verdict sent"
                     (map-elt verdict-ht "state")))

          ;; 1. New threads via one pending review, or verdict/feedback only.
          (when (or new-threads verdict-pending feedback-p pending-id)
            (let* ((current-head (map-elt (magit-dash-review--fetch-pr-metadata slug pr-num dir)
                                          'headRefOid))
                   (pr-id (or (map-elt md "pr_node_id")
                              (magit-dash-review--fetch-pr-node-id slug pr-num dir)))
                   (review-head (or (map-elt md "head_oid") current-head)))
              (when (and pending-id
                         (map-elt pending-rev "pr_node_id")
                         (not (equal (map-elt pending-rev "pr_node_id") pr-id)))
                (setq pending-id nil)
                (map-delete data "pending_review")
                (funcall save))
              (when verdict-pending
                (magit-dash-review--check-verdict-head
                 md verdict-ht event current-head
                 (and pending-id (map-elt pending-rev "head_oid"))))
              (cond
               ((and (not new-threads) (not pending-id))
                (magit-dash-review--submit-threads
                 nil
                 (if verdict-pending
                     event
                   "COMMENT")
                 feedback pr-id review-head dir)
                (when verdict-pending
                  (magit-dash-review--stamp-verdict verdict-ht review-head))
                (funcall save))
               (t
                (let ((reused-p (not (null pending-id)))
                      (added-any-p nil))
                  ;; Phase A: create and persist one pending review before adding threads.
                  (unless pending-id
                    (setq pending-id
                          (magit-dash-review--start-pending-review
                           data pr-id slug pr-num review-head save dir)))
                  ;; Phase B: add each unsynced thread and persist immediately.
                  (seq-do
                   (lambda (c)
                     (condition-case err
                         (magit-dash-review--add-review-thread pending-id c dir)
                       (error
                        (if (and reused-p
                                 (not added-any-p)
                                 (magit-dash-review--stale-pending-review-error-p err))
                            (progn
                              (message "magit-dash-review-submit: stale pending review %s (%s); creating a fresh pending review"
                                       pending-id (error-message-string err))
                              (setq reused-p nil)
                              (map-delete data "pending_review")
                              (funcall save)
                              (setq pending-id
                                    (magit-dash-review--start-pending-review
                                     data pr-id slug pr-num review-head save dir))
                              (magit-dash-review--add-review-thread pending-id c dir))
                          (signal (car err) (cdr err)))))
                     (setq added-any-p t)
                     (funcall save))
                   new-threads)
                  ;; Phase C: submit the pending review once and clear pending_review.
                  (condition-case err
                      (progn
                        (magit-dash-review--submit-pending-review
                         pending-id
                         (if verdict-pending
                             event
                           "COMMENT")
                         feedback dir)
                        (when verdict-pending
                          (magit-dash-review--stamp-verdict
                           verdict-ht
                           (or (and reused-p (map-elt pending-rev "head_oid"))
                               review-head)))
                        (map-delete data "pending_review")
                        (funcall save))
                    (error
                     (if (and reused-p
                              (not added-any-p)
                              (magit-dash-review--stale-pending-review-error-p err))
                         (cond
                          ((let ((case-fold-search t))
                             (string-match-p "already been submitted\\|already submitted"
                                             (error-message-string err)))
                           (if (or verdict-pending feedback-p)
                               (let* ((expected-state
                                       (pcase (if verdict-pending
                                                  event
                                                "COMMENT")
                                         ("APPROVE" "APPROVED")
                                         ("REQUEST_CHANGES" "CHANGES_REQUESTED")
                                         (_ "COMMENTED")))
                                      (node (magit-dash-review--fetch-review-node
                                             pending-id dir))
                                      (gh-state (map-elt node 'state))
                                      (gh-oid (or (map-nested-elt node '(commit oid))
                                                  (map-elt pending-rev "head_oid")
                                                  review-head)))
                                 (unless (equal gh-state expected-state)
                                   (user-error "Pending review %s was already submitted on GitHub as %s (expected %s); run M-x magit-dash-review-sync and verify before submitting again"
                                               pending-id (or gh-state "unknown") expected-state))
                                 (when verdict-pending
                                   (magit-dash-review--stamp-verdict verdict-ht gh-oid))
                                 (map-delete data "pending_review")
                                 (funcall save)
                                 (message "magit-dash-review-submit: pending review %s was already submitted as %s; reconciled pending_review"
                                          pending-id gh-state))
                             (map-delete data "pending_review")
                             (funcall save)
                             (message "magit-dash-review-submit: pending review %s was already submitted on GitHub; cleared pending_review"
                                      pending-id)))
                          (t
                           (map-delete data "pending_review")
                           (funcall save)
                           (user-error "Pending review %s no longer exists on GitHub; cleared pending_review (sync and resubmit)"
                                       pending-id)))
                       (signal (car err) (cdr err))))))))))

          ;; 2. Pending replies on existing GitHub threads, one write per reply.
          (seq-do
           (lambda (c)
             (let ((tid (map-elt c "gh_thread_id"))
                   (failed nil)
                   (pending (seq-filter
                             (lambda (reply)
                               (and (magit-dash-review--blank-p (map-elt reply "gh_comment_id"))
                                    (not (member (map-elt reply "status") '("draft" "sent")))))
                             (append (or (map-elt c "replies") []) nil))))
               (cond
                ((magit-dash-review--blank-p tid) nil)
                ((eq (map-elt c "gh_missing") t)
                 (when pending
                   (message "magit-dash-review-submit: thread %s is gone on GitHub; skipping %d repl%s"
                            tid (length pending) (if (cdr pending) "ies" "y"))))
                (t
                 (seq-do (lambda (reply)
                           (unless failed
                             (condition-case err
                                 (progn
                                   (magit-dash-review--post-reply c reply dir)
                                   (funcall save))
                               (error
                                (setq failed t)
                                (push (format "reply on %s: %s" tid (error-message-string err))
                                      errors)))))
                         pending)))))
           comments)

          ;; 3. Resolution changes that are local intent.
          (seq-do
           (lambda (c)
             (let ((tid (map-elt c "gh_thread_id")))
               (when (and (not (magit-dash-review--blank-p tid))
                          (not (eq (map-elt c "gh_missing") t)))
                 (when-let* ((intent (magit-dash-review--resolution-intent c)))
                   (condition-case err
                       (progn
                         (magit-dash-review--set-thread-resolution c (eq intent 'resolve) dir)
                         (funcall save))
                     (error
                      (push (format "%s on %s: %s" intent tid (error-message-string err))
                            errors)))))))
           comments)

          (funcall save)
          (magit-dash-review--refresh-visiting-buffer file)
          (when errors
            (error "Submit incomplete, %d error(s); what was sent is saved, resubmit to retry: %s"
                   (length errors) (string-join (nreverse errors) "; ")))
          file)
      (magit-dash-review--cr-unlock lock))))

;;; Submit through code-review
;; `magit-dash-review-submit' takes this path when
;; `magit-dash-review-submit-via-code-review' is non-nil.  The review JSON is
;; exported, and `code-review-api-submit' does the sending; this section only
;; turns the file into items, checkpoints every acknowledged item back into
;; the file, and reconciles uncertain failures.  Item ids are strings
;; "c:COMMENT-ID", "r:REPLY-ID" and "t:THREAD-ID" (the review itself is the
;; symbol `review'), so an acknowledgement can be located in the file.
;;
;; Limits.  The review is ONE POST (no chunking, no pending review: that is an
;; open design question), so a very large review lands whole or fails whole.
;;
;; BLOCKING.  `code-review-api-submit' waits for GitHub, up to
;; `code-review-api-send-timeout' per request and more with retries, and so do
;; the reconcile lookups before and after it.  `inhibit-quit' is bound from the
;; send until the file is settled, so a quit cannot leave a half-stamped file; a
;; quit typed meanwhile is reported and dropped afterwards.  When not in batch
;; mode the user must confirm, with the worst case stated.  The GraphQL
;; fallback for replies (below) also runs `gh' synchronously.  Run this from a
;; batch or child Emacs when that wait is not acceptable.
;;
;; Crash safety.  The submit takes a lock file next to the review file FIRST
;; (exclusive create with an owner token; a stale one is renamed away) and holds
;; it to the end, including the GraphQL fallback replies; the file is read after
;; that.  Before sending, `submit_pending' is written: the whole submission (the
;; event and summary, per item the id, kind, body text and hash, path, lines,
;; parent and the verdict), the head it was made for, and a baseline of the
;; review and comment ids GitHub already had.  An item leaves it only when it
;; is stamped from a response, or definitely failed (a rejected request).
;;
;; NEVER AUTOMATIC RESEND.  If the record still holds an item, the send may have
;; reached GitHub without being stamped (a timed-out POST can land later).  The
;; next submit looks each item up on GitHub with an exact match (account, body,
;; path, line, commit, not in the baseline, not already stamped on another
;; item) and stamps the ones found.  Any item still unresolved (not found, not
;; checkable, edited or deleted since, or found but already imported by a sync
;; as another thread) blocks the WHOLE submit: nothing is sent, not even other
;; items, and the legacy path refuses too.  A retry with a different summary
;; does not post a second review: the review entry is matched by the summary
;; it was sent with, and while it is unresolved nothing goes out.  A human
;; decides with `magit-dash-review-clear-submit-pending', which shows what
;; GitHub has for each item and offers to mark it sent, discard it (the next
;; submit sends it) or keep it.  The reconcile entries expose only the current
;; commit and line, so after a push a landed comment may not match: that is
;; what the human step is for.  Residual windows: a kill between the
;; baseline snapshot and the POST leaves a record for a send that never
;; happened (blocked until cleared), never the reverse; a stale-lock takeover
;; is guarded by rename plus a content check but is not a kernel lock.
;;
;; Replies need the REST id of the thread's first comment (`gh_database_id',
;; stored by `magit-dash-review-sync' and by the submit itself).  A reply to a
;; thread without one makes the submit refuse before sending anything: sync
;; first.  There is no GraphQL fallback.
;;
;; CAVEATS.  This path is experimental and OFF by default.  Emacs blocks, and
;; cannot be quit, for up to `magit-dash-review-submit-time-limit' seconds.  An
;; uncertain send blocks ALL later submits, including the legacy path, until
;; `magit-dash-review-clear-submit-pending' resolves it; its \"mark sent\" and
;; \"discard\" are trusted human decisions (mark sent on an unposted item loses
;; it, discard of a posted one posts it twice, hence the confirmations).  After
;; a push a landed comment can show as not found.  Do not use code-review's own
;; submit commands on the same PR meanwhile: they know nothing of the record.
;; The lock is a file lock (exclusive create), not a kernel lock: a live process
;; on this host is never overridden, but another host cannot be seen.  It is
;; held across the confirmation and token prompts, so an abandoned prompt blocks
;; other instances until it is answered or C-g'd; delete FILE.submit-lock by
;; hand only after confirming the owning Emacs is gone.
;;
;; More behaviour to know.  After a landed review, a verdict set again (a new
;; state or head) is refused once, with the landed review stamped as what it
;; was, and is sent by the NEXT submit.  New comments after a landed review with
;; the same summary are posted singly, outside a review.  A crash in the middle
;; of a send can leave items already stamped in the file but still listed in the
;; record: the submit settles those itself, and the clear command shows them as
;; already stamped (mark sent and discard are both safe for them).  Discarding an
;; item whose lookup failed or that was found on GitHub asks for confirmation;
;; only a positive "not found" does not.
;;
;; More caveats.  Item ids must be unique among the comments of a review file
;; and among its replies.  Do not run a submit from another Emacs on the same
;; file while the clear command is prompting (it refuses to apply if the record
;; changed, but the other submit would still have run).  After a crash that had
;; already stamped the review, retrying with the same summary is refused once,
;; and a second retry posts the summary again as a COMMENT review: leave the
;; summary empty then.  A verdict set again is sent by the next submit only when
;; it carries a head; one without a head is not resent after the review head
;; moves.  A resolve marked on a comment this path just posted is not applied by
;; the submit that posts it, and refuses later submits until
;; `magit-dash-review-sync' has fetched its thread id.

(defvar code-review-github-host)
(defvar code-review-auth-backend)

(defun magit-dash-review--cr-account (dir slug)
  "Return the gh account plist (:user :host) for DIR or SLUG, or nil."
  (or (magit-dash-gh--repo-account dir)
      (magit-dash-gh--repo-account slug)))

(defun magit-dash-review--cr-comment-item (c)
  "Return the `code-review-api-submit' comment item for local comment C."
  (let* ((lines (append (map-elt c "lines") nil))
         (l1 (nth 0 lines))
         (l2 (nth 1 lines))
         (side (cond
                ((member (map-elt c "diff_side") '("LEFT" "RIGHT"))
                 (map-elt c "diff_side"))
                (t "RIGHT")))
         (item (list :id (concat "c:" (or (map-elt c "id") ""))
                     :path (map-elt c "path")
                     :body (or (map-elt c "body") "")
                     :line (or l2 l1)
                     :side side)))
    (cond
     ((and l1 l2 (< l1 l2))
      (append item (list :start-line l1 :start-side side)))
     (t item))))

(defun magit-dash-review--cr-match-posted (kind item recon head account &optional review-id)
  "Return the entry of RECON that is the already-posted ITEM of KIND, or nil.
KIND is `review', `comment' or `reply'; RECON is the result of
`code-review-api-reconcile'.  An entry must be by ACCOUNT, have the trimmed
body of ITEM, and: for a review the commit HEAD (both must be known) and the
state of ITEM's :event; for a comment the commit HEAD, the path and the line,
and, when REVIEW-ID and the entry's :review-id are both known, that review;
for a reply the parent comment.  Without ACCOUNT nothing matches."
  (let ((body (string-trim (or (plist-get item :body) "")))
        (mine (lambda (entry)
                (and (stringp account)
                     (stringp (plist-get entry :user))
                     (string-equal-ignore-case account (plist-get entry :user)))))
        (commit-ok (lambda (entry)
                     (and head
                          (plist-get entry :commit-id)
                          (equal head (plist-get entry :commit-id))))))
    (pcase kind
      ('review
       (seq-find (lambda (r)
                   (and (funcall mine r)
                        (equal body (string-trim (or (plist-get r :body) "")))
                        (funcall commit-ok r)
                        (equal (plist-get r :state)
                               (pcase (plist-get item :event)
                                 ("APPROVE" "APPROVED")
                                 ("REQUEST_CHANGES" "CHANGES_REQUESTED")
                                 (_ "COMMENTED")))))
                 (plist-get recon :reviews)))
      ('comment
       (seq-find (lambda (c)
                   (and (funcall mine c)
                        (equal body (string-trim (or (plist-get c :body) "")))
                        (equal (plist-get item :path) (plist-get c :path))
                        (equal (plist-get item :line) (plist-get c :line))
                        (or (not (plist-member item :start-line))
                            (not (plist-member c :start-line))
                            (equal (plist-get item :start-line) (plist-get c :start-line)))
                        (funcall commit-ok c)
                        (or (null review-id)
                            (null (plist-get c :review-id))
                            (equal review-id (plist-get c :review-id)))))
                 (plist-get recon :comments)))
      ('reply
       (seq-find (lambda (c)
                   (and (funcall mine c)
                        (equal body (string-trim (or (plist-get c :body) "")))
                        (equal (plist-get item :reply-to-id) (plist-get c :in-reply-to-id))))
                 (plist-get recon :comments))))))

(defun magit-dash-review--cr-stamp (file id remote node &optional verdict)
  "Checkpoint into FILE that the item ID was acknowledged.
REMOTE is its REST id (an integer) or nil, NODE its GraphQL node id or nil.
ID is `review' (stamp the verdict from the plist VERDICT, (:state S :head H),
when given), \"c:ID\" (the comment becomes sent: `gh_comment_id' and
`remote_id' are NODE when known, so a later sync matches it, and
`gh_database_id' is REMOTE), \"r:ID\" (the reply becomes sent, likewise) or
\"t:THREAD\" (the local resolved state is now GitHub's).  FILE is read fresh
\(through the modified-buffer guard) and written at once, so a crash loses
nothing acknowledged.  Return non-nil when something was stamped."
  (let* ((pair (magit-dash-review-json-read-with-stamp file))
         (data (car pair))
         (stamp (cdr pair))
         (comments (append (or (map-elt data "comments") []) nil))
         (key (and (stringp id) (substring id 2)))
         (stamped nil))
    (cond
     ((eq id 'review)
      (when verdict
        (let ((v (cond
                  ((hash-table-p (map-elt data "verdict"))
                   (map-elt data "verdict"))
                  (t (make-hash-table :test #'equal)))))
          (when (magit-dash-review--blank-p (map-elt v "state"))
            (setf (map-elt v "state") (plist-get verdict :state)))
          ;; What was SENT, from the verdict plist: never the file's current state,
          ;; which may have changed since (it then stays pending).
          (setf (map-elt v "submitted_state") (or (plist-get verdict :state) (map-elt v "state")))
          (setf (map-elt v "submitted_head") (or (plist-get verdict :head) ""))
          (setf (map-elt v "submitted_at") (format-time-string "%FT%T%:z"))
          (setf (map-elt data "verdict") v)
          (setq stamped t))))
     ((string-prefix-p "c:" id)
      (when-let* ((c (seq-find (lambda (c) (equal (map-elt c "id") key)) comments)))
        (when remote
          (setf (map-elt c "gh_database_id") remote))
        (when-let* ((ref (or node (and remote (number-to-string remote)))))
          (setf (map-elt c "remote_id") ref))
        (when node
          (setf (map-elt c "gh_comment_id") node))
        (setf (map-elt c "origin") "github")
        (setf (map-elt c "status") "sent")
        (setf (map-elt c "gh_resolved") :false)
        (setq stamped t)))
     ((string-prefix-p "r:" id)
      (seq-do (lambda (c)
                (when-let* ((reply (seq-find (lambda (r) (equal (map-elt r "id") key))
                                             (append (or (map-elt c "replies") []) nil))))
                  (when remote
                    (setf (map-elt reply "gh_database_id") remote))
                  (when node
                    (setf (map-elt reply "gh_comment_id") node))
                  (setf (map-elt reply "status") "sent")
                  (setq stamped t)))
              comments))
     ((string-prefix-p "t:" id)
      (when-let* ((c (seq-find (lambda (c) (equal (map-elt c "gh_thread_id") key)) comments)))
        (setf (map-elt c "gh_resolved")
              (cond
               ((eq (map-elt c "resolved") t) t)
               (t :false)))
        (map-delete c "local_resolved")
        (setq stamped t))))
    (when stamped
      (magit-dash-review--write-json-file file data stamp))
    stamped))

(defun magit-dash-review--cr-reply-items (comments)
  "Return (VIA-API . NEED-SYNC) pending replies of COMMENTS.
VIA-API are `code-review-api-submit' reply items: a reply goes to any comment
with a REST `gh_database_id' (that is its :reply-to-id), whether or not it has
a thread id.  NEED-SYNC is a list with one list of reply ids per published
thread (it has a thread id) that has no REST id, which the API cannot address.
Comments flagged `gh_missing', and local ones with neither id, are skipped."
  (let* ((usable (seq-remove
                  (lambda (c)
                    (or (eq (map-elt c "gh_missing") t)
                        (not (or (integerp (map-elt c "gh_database_id"))
                                 (not (magit-dash-review--blank-p (map-elt c "gh_thread_id")))))))
                  comments))
         (pending-of (lambda (c)
                       (seq-filter
                        (lambda (reply)
                          (and (magit-dash-review--blank-p (map-elt reply "gh_comment_id"))
                               (magit-dash-review--blank-p (map-elt reply "gh_database_id"))
                               (not (member (map-elt reply "status") '("draft" "sent")))))
                        (append (or (map-elt c "replies") []) nil)))))
    (cons
     (seq-mapcat
      (lambda (c)
        (when (integerp (map-elt c "gh_database_id"))
          (seq-map (lambda (reply)
                     (list :id (concat "r:" (or (map-elt reply "id") ""))
                           :reply-to-id (map-elt c "gh_database_id")
                           :body (or (map-elt reply "body") "")))
                   (funcall pending-of c))))
      usable)
     (seq-filter
      #'identity
      (seq-map
       (lambda (c)
         (unless (integerp (map-elt c "gh_database_id"))
           (seq-map (lambda (reply) (map-elt reply "id")) (funcall pending-of c))))
       usable)))))

(defun magit-dash-review--cr-known-ids (data)
  "Return the GitHub ids stamped in DATA as (:sent IDS :imported IDS).
:sent are REST ids and node ids of comments and replies this submit path
stamped (status \"sent\"): their remote entries belong to those items.
:imported are the ids of every other table that has some: threads a sync
brought in.  A pending item's own landed comment can be among those (a sync
imported it as a thread of its own), which is the \"twin\" case."
  (let* ((tables (seq-mapcat
                  (lambda (c)
                    (cons c (append (or (map-elt c "replies") []) nil)))
                  (append (or (map-elt data "comments") []) nil)))
         (ids-of (lambda (selected)
                   (seq-filter
                    (lambda (id)
                      (and id (not (equal id ""))))
                    (seq-mapcat
                     (lambda (table)
                       (list (map-elt table "gh_database_id")
                             (map-elt table "gh_comment_id")
                             (map-elt table "remote_id")))
                     selected)))))
    (list :sent (funcall ids-of (seq-filter (lambda (tb) (equal (map-elt tb "status") "sent")) tables))
          :imported (funcall ids-of (seq-remove (lambda (tb) (equal (map-elt tb "status") "sent"))
                                                tables)))))

(defun magit-dash-review--cr-reconcile (pending items head account slug pr-num &optional baseline known)
  "Reconcile the PENDING item ids of a submit against GitHub; never resend.
ITEMS maps item id to (KIND . ITEM).  BASELINE is the plist (:reviews IDS
:comments IDS) of what GitHub had before the send, KNOWN the plist
`magit-dash-review--cr-known-ids' returns: remote entries in the baseline or
in :sent are never candidates.  Only reviews, comments and replies by ACCOUNT
count and each remote entry matches at most one item.  A comment matches on
its own (path, line, body, commit, and the matched review when both are
known); a matched review never marks comments.  Without HEAD or BASELINE, or
when a lookup failed, nothing can be told and every item is :unchecked.  An
item whose match is a thread a sync imported (:imported) is a :twin: it is
posted, but another table of the file already holds it.
Return (:matched ((ID REMOTE NODE) ...) :missing IDS :unchecked IDS :twin IDS)."
  (let* ((recon (condition-case err
                    (code-review-api-reconcile slug pr-num :account account)
                  (error (list :errors (list (error-message-string err))))))
         (sent (plist-get known :sent))
         (imported (plist-get known :imported))
         (usable (lambda (entry baseline-ids)
                   (not (or (member (plist-get entry :id) baseline-ids)
                            (and (plist-get entry :id)
                                 (member (plist-get entry :id) sent))
                            (and (plist-get entry :node-id)
                                 (member (plist-get entry :node-id) sent))))))
         (reviews (seq-filter (lambda (e) (funcall usable e (plist-get baseline :reviews)))
                              (plist-get recon :reviews)))
         (comments (seq-filter (lambda (e) (funcall usable e (plist-get baseline :comments)))
                               (plist-get recon :comments)))
         (review-hit nil)
         (matched nil)
         (missing nil)
         (twin nil))
    (cond
     ((or (plist-get recon :errors) (null head) (null baseline))
      (list :matched nil :missing nil :unchecked (copy-sequence pending) :twin nil))
     (t
      (when (memq 'review pending)
        (setq review-hit (magit-dash-review--cr-match-posted
                          'review (cddr (assq 'review items))
                          (list :reviews reviews) head account))
        (cond
         (review-hit
          (push (list 'review (plist-get review-hit :id) (plist-get review-hit :node-id))
                matched))
         (t
          (push 'review missing))))
      (seq-do
       (lambda (id)
         (unless (eq id 'review)
           (let* ((entry (cdr (assoc id items)))
                  (hit (and entry
                            (magit-dash-review--cr-match-posted
                             (car entry) (cdr entry) (list :comments comments) head account
                             (and review-hit (plist-get review-hit :id))))))
             (cond
              ((null hit)
               (push id missing))
              (t
               (setq comments (delq hit comments))
               (cond
                ((or (member (plist-get hit :id) imported)
                     (and (plist-get hit :node-id) (member (plist-get hit :node-id) imported)))
                 (push id twin))
                (t
                 (push (list id (plist-get hit :id) (plist-get hit :node-id)) matched))))))))
       pending)
      (list :matched (nreverse matched) :missing (nreverse missing) :unchecked nil
            :twin (nreverse twin))))))

(defun magit-dash-review--cr-snapshot (slug pr-num account)
  "Return (:reviews IDS :comments IDS) of what GitHub has for PR-NUM, or nil.
Nil when a lookup failed or timed out: nothing is then known about what is
new.  A PR with a very large history can exceed code-review's request limits;
the submit then stops with a message instead of guessing."
  (let ((recon (condition-case nil
                   (code-review-api-reconcile slug pr-num :account account)
                 (error nil))))
    (when (and recon (null (plist-get recon :errors)))
      (list :reviews (seq-map (lambda (e) (plist-get e :id)) (plist-get recon :reviews))
            :comments (seq-map (lambda (e) (plist-get e :id)) (plist-get recon :comments))))))

(defun magit-dash-review--cr-pending-write (file items baseline head &optional verdict)
  "Record ITEMS, an alist of item id to (KIND . ITEM), as pending in FILE.
This is the whole submission, enough to reconcile or retry it with nothing
else: per item the id, kind, a hash and the text of the body, the path, lines,
parent comment and (review) event; the summary is the review item's body.
BASELINE is what GitHub had, HEAD the commit the send was made for, VERDICT
the plist (:state S :head H) the review carries."
  (let* ((pair (magit-dash-review-json-read-with-stamp file))
         (data (car pair))
         (stamp (cdr pair))
         (record (make-hash-table :test #'equal))
         (base (make-hash-table :test #'equal)))
    (setf (map-elt record "at") (format-time-string "%FT%T%:z"))
    (setf (map-elt record "head") (or head ""))
    (setf (map-elt base "reviews") (vconcat (plist-get baseline :reviews)))
    (setf (map-elt base "comments") (vconcat (plist-get baseline :comments)))
    (setf (map-elt record "baseline") base)
    (when verdict
      (let ((v (make-hash-table :test #'equal)))
        (setf (map-elt v "state") (plist-get verdict :state))
        (setf (map-elt v "head") (or (plist-get verdict :head) ""))
        (setf (map-elt record "verdict") v)))
    (setf (map-elt record "items")
          (vconcat
           (seq-map (lambda (entry)
                      (let ((e (make-hash-table :test #'equal))
                            (item (cddr entry)))
                        (setf (map-elt e "id") (format "%s" (car entry)))
                        (setf (map-elt e "kind") (symbol-name (cadr entry)))
                        (setf (map-elt e "body") (or (plist-get item :body) ""))
                        (setf (map-elt e "hash") (secure-hash 'sha1 (or (plist-get item :body) "")))
                        (seq-do (lambda (spec)
                                  (when (plist-get item (car spec))
                                    (setf (map-elt e (cdr spec)) (plist-get item (car spec)))))
                                '((:path . "path") (:line . "line") (:start-line . "start_line")
                                  (:reply-to-id . "reply_to") (:event . "event")))
                        e))
                    items)))
    (setf (map-elt data "submit_pending") record)
    (magit-dash-review--write-json-file file data stamp)))

(defun magit-dash-review--cr-record-items (record)
  "Return the items alist (ID . (KIND . ITEM)) rebuilt from the entries of RECORD."
  (seq-map
   (lambda (e)
     (let* ((name (map-elt e "id"))
            (id (cond
                 ((equal name "review") 'review)
                 (t name)))
            (kind (cond
                   ((eq id 'review) 'review)
                   ((string-prefix-p "r:" name) 'reply)
                   (t 'comment))))
       (cons id (cons kind
                      (append (list :id id :body (or (map-elt e "body") ""))
                              (seq-mapcat (lambda (spec)
                                            (when (map-elt e (cdr spec))
                                              (list (car spec) (map-elt e (cdr spec)))))
                                          '((:path . "path") (:line . "line")
                                            (:start-line . "start_line")
                                            (:reply-to-id . "reply_to") (:event . "event"))))))))
   (append (or (map-elt record "items") []) nil)))

(defun magit-dash-review--cr-host-normalise (host)
  "Return HOST as a bare lower-case hostname, \"github.com\" for the API host."
  (let ((name (downcase (car (split-string
                              (replace-regexp-in-string "\\`https?://" "" (or host "github.com"))
                              "/")))))
    (cond
     ((string-prefix-p "api." name)
      (substring name 4))
     (t name))))

(defun magit-dash-review--cr-lock (file)
  "Take the submit lock of review FILE and return (PATH . TOKEN).
This is a file lock (exclusive create of FILE.submit-lock holding pid, time,
host and a random token), not a kernel lock.  A lock whose process is alive on
this host is never taken, however old.  A lock is stale, and replaced (with a
message), only when its process is dead on this host, or it is from another
host (or unreadable) and older than an hour.  A stale lock is renamed aside,
the moved content checked to be what was judged, and the exclusive create
retried once, so two racers do not both win; otherwise `user-error'."
  (let* ((lock (concat file ".submit-lock"))
         (token (format "%d %f %s %08x" (emacs-pid) (float-time) (system-name)
                        (random #xffffffff)))
         (create (lambda ()
                   (condition-case nil
                       (progn
                         (write-region token nil lock nil 'silent nil 'excl)
                         t)
                     (file-already-exists nil)))))
    (unless (funcall create)
      (let* ((seen (or (ignore-errors
                         (with-temp-buffer
                           (insert-file-contents lock)
                           (buffer-string)))
                       ""))
             (fields (split-string seen))
             (pid (string-to-number (or (nth 0 fields) "0")))
             (at (string-to-number (or (nth 1 fields) "0")))
             (local (and (nth 2 fields) (equal (nth 2 fields) (system-name))))
             (age (- (float-time)
                     (cond
                      ((> at 0) at)
                      (t (float-time (or (file-attribute-modification-time (file-attributes lock))
                                         (current-time)))))))
             (aside (format "%s.stale-%08x" lock (random #xffffffff))))
        (cond
         ((and local (> pid 0) (process-attributes pid))
          (user-error "Another submit of %s holds %s (process %d is alive); wait for it, or delete the lock yourself if you are sure it is dead"
                      file lock pid))
         ((and (not (and local (> pid 0))) (< age 3600))
          (user-error "Another submit of %s holds %s (another host, or unreadable, and only %ds old); wait, or delete the lock yourself"
                      file lock age)))
        (message "magit-dash-review: taking over the stale lock %s (%s)" lock
                 (cond
                  (local "its process is dead")
                  (t "from another host or unreadable, and over an hour old")))
        (condition-case nil
            (rename-file lock aside)
          (file-error
           (user-error "Another submit of %s is taking over the stale lock %s" file lock)))
        (cond
         ((equal seen (with-temp-buffer
                        (insert-file-contents aside)
                        (buffer-string)))
          (delete-file aside))
         (t
          (ignore-errors (rename-file aside lock))
          (user-error "Another submit of %s took %s while it was being checked" file lock)))
        (unless (funcall create)
          (user-error "Another submit of %s took %s first" file lock))))
    (cons lock token)))

(defun magit-dash-review--cr-unlock (lock)
  "Release LOCK, a (PATH . TOKEN) cell from `magit-dash-review--cr-lock'.
Only a lock still holding this owner's token is removed."
  (when (and lock (file-exists-p (car lock)))
    (when (equal (ignore-errors
                   (with-temp-buffer
                     (insert-file-contents (car lock))
                     (buffer-string)))
                 (cdr lock))
      (delete-file (car lock)))))

(defun magit-dash-review--cr-pending-drop (file ids)
  "Remove the item IDS (strings or `review') from the pending record of FILE.
The record goes when no item is left.  Nothing is written when FILE has none."
  (let* ((pair (magit-dash-review-json-read-with-stamp file))
         (data (car pair))
         (stamp (cdr pair))
         (record (map-elt data "submit_pending"))
         (names (seq-map (lambda (id) (format "%s" id)) ids)))
    (when (hash-table-p record)
      (let ((left (seq-remove (lambda (e) (member (map-elt e "id") names))
                              (append (or (map-elt record "items") []) nil))))
        (cond
         (left
          (setf (map-elt record "items") (vconcat left)))
         (t
          (map-delete data "submit_pending")))
        (magit-dash-review--write-json-file file data stamp)))))

(defun magit-dash-review--cr-describe (entry)
  "Return a short name for the pending record ENTRY: its id and path:line."
  (format "%s%s" (map-elt entry "id")
          (cond
           ((map-elt entry "path")
            (format " (%s:%s)" (map-elt entry "path") (map-elt entry "line")))
           (t ""))))

(defun magit-dash-review--cr-entry-id (entry)
  "Return the item id of the pending record ENTRY: `review' or a string."
  (cond
   ((equal (map-elt entry "id") "review") 'review)
   (t (map-elt entry "id"))))

(defun magit-dash-review--cr-record-verdict (record)
  "Return the verdict plist (:state :head) the review of RECORD carried, or nil."
  (when-let* ((v (map-elt record "verdict")))
    (list :state (map-elt v "state") :head (map-elt v "head"))))

(defun magit-dash-review--cr-baseline (record)
  "Return the baseline plist (:reviews IDS :comments IDS) of RECORD, or nil."
  (when-let* ((base (map-elt record "baseline"))
              ((hash-table-p base)))
    (list :reviews (append (map-elt base "reviews") nil)
          :comments (append (map-elt base "comments") nil))))

(defun magit-dash-review--cr-stamped-p (data record entry)
  "Return non-nil when the item of pending ENTRY is already stamped in DATA.
That happens when the process stopped after the item was acknowledged and
stamped but before the record was trimmed.  A comment or reply is stamped when
it is `sent' with a REST or node id; the review when the file's verdict
records the verdict the review of RECORD carried as submitted."
  (let* ((id (magit-dash-review--cr-entry-id entry))
         (comments (append (or (map-elt data "comments") []) nil))
         ;; \"c:ID\" is a top-level comment and \"r:ID\" a reply, never the other.
         (tables (cond
                  ((and (stringp id) (string-prefix-p "r:" id))
                   (seq-mapcat (lambda (c) (append (or (map-elt c "replies") []) nil)) comments))
                  (t comments))))
    (cond
     ((eq id 'review)
      (when-let* ((rec (magit-dash-review--cr-record-verdict record))
                  (v (map-elt data "verdict"))
                  ((hash-table-p v)))
        (and (map-elt v "submitted_at")
             (equal (map-elt v "submitted_state") (plist-get rec :state))
             (equal (map-elt v "submitted_head") (or (plist-get rec :head) "")))))
     (t
      (when-let* ((table (seq-find (lambda (tb) (equal (map-elt tb "id") (substring id 2))) tables)))
        (and (equal (map-elt table "status") "sent")
             (or (integerp (map-elt table "gh_database_id"))
                 (not (magit-dash-review--blank-p (map-elt table "gh_comment_id"))))))))))

(defun magit-dash-review--cr-signature (record)
  "Return what identifies RECORD, a `submit_pending' table, as one particular send.
The time, head and verdict of the send and, per item, the id, hash and event, so
a record another instance replaced with one of the same ids is not mistaken for
it."
  (let ((verdict (magit-dash-review--cr-record-verdict record)))
    (list (map-elt record "at") (map-elt record "head")
          (plist-get verdict :state) (plist-get verdict :head)
          (seq-map (lambda (e) (list (map-elt e "id") (map-elt e "hash") (map-elt e "event")))
                   (append (or (map-elt record "items") []) nil)))))

(defun magit-dash-review--cr-survey (file)
  "Look at the pending record of FILE under the lock and return what GitHub has.
Return (:entries ENTRIES :found RECONCILE-RESULT-OR-NIL :verdict V :signature S).
Blocks on the lookup.  The lock is released before returning, so that nothing
interactive happens while it is held."
  (let ((lock (magit-dash-review--cr-lock file)))
    (unwind-protect
        (let* ((data (magit-dash-review--read-for-write file))
               (record (or (map-elt data "submit_pending")
                           (user-error "%s has no unfinished submit" file)))
               (entries (append (or (map-elt record "items") []) nil))
               (md (map-elt data "magit_dash"))
               (slug (map-elt md "repo"))
               (account (plist-get (magit-dash-review--cr-account
                                    (magit-dash-review--trusted-worktree
                                     file (or (map-elt md "worktree") (map-elt data "root")
                                              default-directory))
                                    slug)
                                   :user)))
          (list :entries entries
                :stamped (seq-map #'magit-dash-review--cr-entry-id
                                  (seq-filter (lambda (e)
                                                (magit-dash-review--cr-stamped-p data record e))
                                              entries))
                :verdict (magit-dash-review--cr-record-verdict record)
                :signature (magit-dash-review--cr-signature record)
                :found (condition-case nil
                           (magit-dash-review--cr-reconcile
                            (seq-map #'magit-dash-review--cr-entry-id
                                     (seq-remove (lambda (e)
                                                   (magit-dash-review--cr-stamped-p data record e))
                                                 entries))
                            (magit-dash-review--cr-record-items record)
                            (map-elt record "head") account slug (map-elt md "number")
                            (magit-dash-review--cr-baseline record)
                            (magit-dash-review--cr-known-ids data))
                         (error nil))))
      (magit-dash-review--cr-unlock lock))))

;;;###autoload
(defun magit-dash-review-clear-submit-pending (&optional review-file)
  "Resolve the unfinished-submit record (`submit_pending') of REVIEW-FILE.
A submit that was cut short may or may not have reached GitHub, and nothing is
sent again until a human has decided each item.  For every item this shows what
GitHub has for it (a lookup: it blocks) and offers `keep' (the default),
`mark sent' (stamp it as sent, with the ids GitHub reported, without posting:
asks again when it was NOT found) or `discard' (forget it, the next submit
sends it: asks again when it WAS found, since it would be posted twice).
These are trusted human decisions.  The lock is held only for the lookup and
for applying the answers, never while prompting; the record is checked to be
unchanged before anything is applied."
  (interactive)
  (let* ((file (expand-file-name
                (or review-file
                    (magit-dash-review--current-review-file)
                    (user-error "No active review JSON file"))))
         (survey (magit-dash-review--cr-survey file))
         (found (plist-get survey :found))
         (decisions
          (seq-map
           (lambda (e)
             (let* ((id (magit-dash-review--cr-entry-id e))
                    (hit (assoc id (plist-get found :matched)))
                    (seen (or hit (member id (plist-get found :twin))))
                    (stamped (member id (plist-get survey :stamped)))
                    (missing (and found (member id (plist-get found :missing))))
                    (finding (cond
                              (stamped "already stamped in the file (marking it sent or discarding it is safe)")
                              ((null found) "GitHub could not be checked")
                              (hit "found on GitHub")
                              (seen "found on GitHub, and a sync imported it as another thread")
                              ((member id (plist-get found :unchecked)) "could not be checked")
                              (t "not found on GitHub (it may still land)")))
                    (choice (completing-read
                             (format "%s: %s.  Resolve: " (magit-dash-review--cr-describe e) finding)
                             '("keep" "mark sent" "discard (the next submit sends it)")
                             nil t nil nil "keep")))
               (cond
                ((and (equal choice "mark sent") (not seen) (not stamped)
                      (not (yes-or-no-p (format "%s was NOT found on GitHub; mark it sent without posting it? "
                                                (map-elt e "id")))))
                 (cons id "keep"))
                ;; Only a positive "not found" (or an item already stamped) may
                ;; be discarded without a second question.
                ((and (string-prefix-p "discard" choice) (not stamped) (not missing)
                      (not (yes-or-no-p (format "%s %s; discarding it will post it again if it landed.  Discard? "
                                                (map-elt e "id")
                                                (cond
                                                 (seen "WAS found on GitHub")
                                                 (t "could NOT be checked"))))))
                 (cons id "keep"))
                (t (cons id choice)))))
           (plist-get survey :entries)))
         (lock (magit-dash-review--cr-lock file)))
    (unwind-protect
        (let* ((record (or (map-elt (magit-dash-review--read-for-write file) "submit_pending")
                           (user-error "The record of %s was cleared meanwhile" file))))
          (unless (equal (plist-get survey :signature)
                         (magit-dash-review--cr-signature record))
            (user-error "The unfinished-submit record of %s changed while you were deciding; nothing was applied" file))
          ;; A sync that imported the landed comment meanwhile would get the same
          ;; ids stamped onto a second local item: refuse, apply nothing.
          (let ((imported (plist-get (magit-dash-review--cr-known-ids
                                      (magit-dash-review--read-json-file file))
                                     :imported)))
            (seq-do (lambda (decision)
                      (let ((hit (assoc (car decision) (plist-get found :matched))))
                        (when (and (equal (cdr decision) "mark sent") hit
                                   (or (member (nth 1 hit) imported) (member (nth 2 hit) imported)))
                          (user-error "%s was imported by a sync while you were deciding; nothing was applied (delete the duplicate or look again)"
                                      (car decision)))))
                    decisions))
          (seq-do
           (lambda (decision)
             (let* ((id (car decision))
                    (hit (assoc id (plist-get found :matched))))
               (cond
                ((equal (cdr decision) "mark sent")
                 (magit-dash-review--cr-stamp file id (nth 1 hit) (nth 2 hit)
                                              (and (eq id 'review) (plist-get survey :verdict)))
                 (magit-dash-review--cr-pending-drop file (list id)))
                ((string-prefix-p "discard" (cdr decision))
                 (magit-dash-review--cr-pending-drop file (list id))))))
           decisions))
      (magit-dash-review--cr-unlock lock))))

(defun magit-dash-review--cr-hosts (account-plist)
  "Return (REVIEW-HOST . SEND-HOST), the normalised host of the gh ACCOUNT-PLIST
and the one code-review would send to."
  (cons (magit-dash-review--cr-host-normalise (plist-get account-plist :host))
        (magit-dash-review--cr-host-normalise
         (or (bound-and-true-p code-review-github-host) "api.github.com"))))

(defun magit-dash-review--cr-plan (file feedback)
  "Read FILE and plan the submit of FEEDBACK: return a plist of everything to send.
Keys: :slug :pr-num :dir :account-plist :account :data :record :record-entries
:event :api-event :feedback :feedback-p :head-oid :verdict-head :review-head
:verdict-pending :cr-verdict-p :verdict-stamp :empty-verdict :comment-items
:api-replies :graphql-replies :resolve-items :items.  Items come from the file:
export marks every local comment without a cr_id a code-review draft, which must
not decide what gets sent; export only supplies the verdict, defaulted from
code-review's pending one.  Nothing is written or sent."
  (let* ((data (magit-dash-review--read-for-write file))
         (md (map-elt data "magit_dash"))
         (slug (and md (map-elt md "repo")))
         (pr-num (and md (map-elt md "number"))))
    (unless (and slug pr-num)
      (user-error "Review file %s lacks `magit_dash' PR metadata" file))
    (let* ((dir (magit-dash-review--trusted-worktree
                 file
                 (or (map-elt md "worktree") (map-elt data "root") default-directory)))
           (account-plist (magit-dash-review--cr-account dir slug))
           (comments (append (or (map-elt data "comments") []) nil))
           (verdict-ht (map-elt (let ((magit-dash-review-json-source-function (lambda (_) data)))
                                  (magit-dash-review-json-export (cons slug pr-num)))
                                "verdict"))
           (verdict-pending (magit-dash-review--verdict-pending-p verdict-ht))
           (new-threads (seq-filter
                         (lambda (c)
                           (magit-dash-review--blank-p (map-elt c "remote_id")))
                         (magit-dash-review--collect-unsynced-threads comments)))
           (feedback-p (and feedback (not (string-empty-p feedback))))
           (event (cond
                   (verdict-pending (magit-dash-review--verdict-to-gh-event verdict-ht))
                   ((or new-threads feedback-p) "COMMENT")))
           (verdict-head (and verdict-pending (map-elt verdict-ht "head")))
           (head-oid (cond
                      ((magit-dash-review--blank-p verdict-head) (map-elt md "head_oid"))
                      (t verdict-head)))
           (comment-items (seq-map #'magit-dash-review--cr-comment-item new-threads))
           (replies (magit-dash-review--cr-reply-items comments))
           (verdict-stamp (and verdict-pending
                               (list :state (map-elt verdict-ht "state") :head head-oid)))
           (empty-verdict (and verdict-pending (equal event "COMMENT")
                               (null comment-items) (not feedback-p)))
           (api-event (and (not empty-verdict) event))
           (record (map-elt data "submit_pending")))
      (list :slug slug :pr-num pr-num :dir dir :data data
            :account-plist account-plist
            :account (or (plist-get account-plist :user)
                         (user-error "No gh account is configured for %s; refusing to send with an unnamed one" slug))
            :record record
            :record-entries (append (and (hash-table-p record) (map-elt record "items")) nil)
            :event event :api-event api-event
            :feedback feedback :feedback-p feedback-p
            :head-oid head-oid :verdict-head verdict-head :review-head (map-elt md "head_oid")
            :verdict-pending verdict-pending
            :cr-verdict-p (magit-dash-review--blank-p
                           (and (hash-table-p (map-elt data "verdict"))
                                (map-elt (map-elt data "verdict") "state")))
            :verdict-stamp verdict-stamp :empty-verdict empty-verdict
            :comment-items comment-items :api-replies (car replies)
            :graphql-replies (cdr replies)
            :resolve-needs-sync (seq-map
                                 (lambda (c) (map-elt c "id"))
                                 (seq-filter
                                  (lambda (c)
                                    (and (magit-dash-review--blank-p (map-elt c "gh_thread_id"))
                                         (not (eq (map-elt c "gh_missing") t))
                                         (or (integerp (map-elt c "gh_database_id"))
                                             (equal (map-elt c "status") "sent"))
                                         (magit-dash-review--resolution-intent c)))
                                  comments))
            :resolve-items (seq-map
                            (lambda (c)
                              (list :id (concat "t:" (map-elt c "gh_thread_id"))
                                    :thread-id (map-elt c "gh_thread_id")
                                    :resolved (eq (magit-dash-review--resolution-intent c) 'resolve)
                                    :remote-resolved (eq (map-elt c "gh_resolved") t)))
                            (seq-filter
                             (lambda (c)
                               (and (not (magit-dash-review--blank-p (map-elt c "gh_thread_id")))
                                    (not (eq (map-elt c "gh_missing") t))
                                    (magit-dash-review--resolution-intent c)))
                             comments))
            :items (append
                    (and api-event
                         (list (cons 'review (cons 'review (list :body (and feedback-p feedback)
                                                                 :event api-event)))))
                    (seq-map (lambda (item) (cons (plist-get item :id) (cons 'comment item)))
                             comment-items)
                    (seq-map (lambda (item) (cons (plist-get item :id) (cons 'reply item)))
                             (car replies)))))))

(defun magit-dash-review--cr-check (plan)
  "Signal a `user-error' when PLAN must not be sent.  Nothing is written or sent.
Refuses: a host that is not the one code-review sends to, a token that is not
the gh account's, REQUEST_CHANGES without a summary, APPROVE or
REQUEST_CHANGES without (or with disagreeing) heads, and replies to threads
without a REST id (sync first: there is no safe fallback)."
  (let ((hosts (magit-dash-review--cr-hosts (plist-get plan :account-plist)))
        (api-event (plist-get plan :api-event))
        (verdict-head (plist-get plan :verdict-head))
        (review-head (plist-get plan :review-head)))
    (unless (equal (car hosts) (cdr hosts))
      (user-error "The account for %s is on %s but code-review would send to %s (`code-review-github-host'); refusing to publish to a different host"
                  (plist-get plan :slug) (car hosts) (cdr hosts)))
    (when (and (boundp 'code-review-auth-backend)
               (not (eq code-review-auth-backend 'gh)))
      (user-error "`code-review-auth-backend' is %S: cannot confirm that its token is the gh account %s; set it to `gh' to submit this way"
                  code-review-auth-backend (plist-get plan :account)))
    (when (and (equal api-event "REQUEST_CHANGES") (not (plist-get plan :feedback-p)))
      (user-error "REQUEST_CHANGES needs a review summary"))
    (when (and (member api-event '("APPROVE" "REQUEST_CHANGES"))
               (magit-dash-review--blank-p (plist-get plan :head-oid)))
      (user-error "Refusing to send %s: no head commit says what was reviewed" api-event))
    (when (and (member (plist-get plan :event) '("APPROVE" "REQUEST_CHANGES"))
               (not (magit-dash-review--blank-p verdict-head))
               (not (magit-dash-review--blank-p review-head))
               (not (equal verdict-head review-head)))
      (user-error "Refusing to send %s: the verdict was made at %s but the review at %s; re-review first"
                  (plist-get plan :event) (substring verdict-head 0 (min 12 (length verdict-head)))
                  (substring review-head 0 (min 12 (length review-head)))))
    (when (plist-get plan :resolve-needs-sync)
      (user-error "Resolving %d comment(s) of %s needs the thread id, which they do not have yet; run `magit-dash-review-sync' first (nothing was sent)"
                  (length (plist-get plan :resolve-needs-sync)) (plist-get plan :slug)))
    (when (plist-get plan :graphql-replies)
      (user-error "Replies to %d thread(s) of %s have no REST id for code-review to address; run `magit-dash-review-sync' first to fetch them (there is no safe fallback)"
                  (length (plist-get plan :graphql-replies)) (plist-get plan :slug)))))

(defun magit-dash-review--cr-confirm (plan)
  "Ask whether to send PLAN, stating that Emacs blocks and the worst case."
  (let* ((retries (or (bound-and-true-p code-review-api-send-max-retries) 2))
         (timeout (or (bound-and-true-p code-review-api-send-timeout) 60))
         (max-wait (or (bound-and-true-p code-review-api-send-max-wait) 60))
         ;; head lookup, review, comment ids, replies, resolves, the two
         ;; baseline GETs, the two GETs of each reconcile (before and after).
         (requests (+ 3 (length (plist-get plan :api-replies))
                      (length (plist-get plan :resolve-items)) 2 2
                      (cond ((plist-get plan :record) 2) (t 0))))
         (worst (* requests (+ (* (1+ retries) timeout) (* retries max-wait)))))
    (unless (y-or-n-p (format "Send %s#%s through code-review?  Emacs blocks, including for lookups before and after, and cannot be quit until GitHub answers (worst case about %ds, never more than %ss). "
                              (plist-get plan :slug) (plist-get plan :pr-num) worst
                              magit-dash-review-submit-time-limit))
      (user-error "Submit cancelled"))))

(defun magit-dash-review--cr-resolve-record (file plan run)
  "Settle the unfinished earlier submit in PLAN's record against GitHub.
RUN is the hash table of mutable submit state.  Items found by an exact
per-item match are stamped (the review with the verdict ITS record carried,
never the current one) and leave the record.  ANY other pending item (not
found, not checkable, edited or deleted since, found but already imported by a
sync) blocks the submit with a `user-error': nothing is sent.  An item already
stamped in the file (the process stopped before the record was trimmed) is
settled without a lookup.  When the earlier review landed but this submit's
event, summary or verdict head differs, it stops too, saying so: it never posts
a different review, nor new comments outside one.  When the earlier review
landed with the SAME event and summary, comments new since then are posted
singly, outside a review (that is what `code-review-api-submit' does without an
event), because a second review with the same summary would be a duplicate."
  (let* ((record (plist-get plan :record))
         (items (plist-get plan :items))
         (entries (plist-get plan :record-entries))
         (rec-items (magit-dash-review--cr-record-items record))
         (current-hash (lambda (id)
                         (secure-hash 'sha1 (or (plist-get (cddr (assoc id items)) :body) ""))))
         (stamped (seq-filter
                   (lambda (e)
                     (magit-dash-review--cr-stamped-p (magit-dash-review--read-json-file file)
                                                      record e))
                   entries))
         (checkable (seq-filter
                     (lambda (e)
                       (let ((id (magit-dash-review--cr-entry-id e)))
                         (or (eq id 'review)
                             (and (assoc id items)
                                  (equal (map-elt e "hash") (funcall current-hash id))))))
                     (seq-remove (lambda (e) (member e stamped)) entries)))
         ;; Nothing checkable means no lookup: no blocking call for nothing.
         (found (and checkable
                     (magit-dash-review--cr-reconcile
                      (seq-map #'magit-dash-review--cr-entry-id checkable) rec-items
                      (or (map-elt record "head") (plist-get plan :head-oid))
                      (plist-get plan :account) (plist-get plan :slug) (plist-get plan :pr-num)
                      (magit-dash-review--cr-baseline record)
                      (magit-dash-review--cr-known-ids (magit-dash-review--read-json-file file)))))
         (matched (plist-get found :matched)))
    (seq-do (lambda (m)
              (magit-dash-review--cr-stamp
               file (car m) (cadr m) (nth 2 m)
               (and (eq (car m) 'review) (magit-dash-review--cr-record-verdict record)))
              (push (car m) (map-elt run 'settled))
              (when (eq (car m) 'review)
                (setf (map-elt run 'sent-review) t))
              (setf (map-elt run 'comment-items)
                    (seq-remove (lambda (i) (equal (plist-get i :id) (car m)))
                                (map-elt run 'comment-items)))
              (setf (map-elt run 'api-replies)
                    (seq-remove (lambda (i) (equal (plist-get i :id) (car m)))
                                (map-elt run 'api-replies))))
            matched)
    (seq-do (lambda (e)
              (push (magit-dash-review--cr-entry-id e) (map-elt run 'settled)))
            stamped)
    (magit-dash-review--cr-pending-drop file (map-elt run 'settled))
    (let ((unresolved (seq-remove
                       (lambda (e)
                         (member (magit-dash-review--cr-entry-id e) (map-elt run 'settled)))
                       entries)))
      (when unresolved
        (user-error "An earlier submit of %s is unresolved: %s.  It may or may not be on GitHub, so nothing is sent (not even other items, and not through the legacy path) until you decide each item with M-x magit-dash-review-clear-submit-pending"
                    file (string-join (seq-map #'magit-dash-review--cr-describe unresolved) ", "))))
    (when (or (assq 'review matched) (member 'review (map-elt run 'settled)))
      (setf (map-elt run 'sent-review) t)
      (let ((earlier (cddr (assq 'review rec-items))))
        (when (and (map-elt run 'api-event)
                   (or (not (equal (map-elt run 'api-event) (plist-get earlier :event)))
                       (and (plist-get plan :verdict-stamp)
                            (magit-dash-review--cr-record-verdict record)
                            (not (equal (plist-get (plist-get plan :verdict-stamp) :head)
                                        (plist-get (magit-dash-review--cr-record-verdict record) :head))))
                       (not (equal (or (and (plist-get plan :feedback-p) (plist-get plan :feedback)) "")
                                   (or (plist-get earlier :body) "")))))
          (user-error "The earlier review of %s (%s) landed on GitHub and is stamped, but this submit has a different event or summary (%s); nothing was posted.  Look at the PR and the file, then resubmit what is still wanted"
                      file (plist-get earlier :event) (map-elt run 'api-event))))
      (setf (map-elt run 'api-event) nil))))

(defun magit-dash-review--cr-send (file plan run)
  "Send what is left of PLAN (see RUN) through `code-review-api-submit' and settle it.
Writes `submit_pending' first (after a baseline snapshot: no snapshot, no
send), stamps each acknowledged item as it is acknowledged, replays failed
stamps from the result, stamps the verdict last, reconciles uncertain failures
(stamping what is found, leaving the rest pending) and drops from the record
only what was stamped or definitely failed.  Problems go to RUN's `errors'."
  (let* ((slug (plist-get plan :slug))
         (pr-num (plist-get plan :pr-num))
         (account (plist-get plan :account))
         (head-oid (plist-get plan :head-oid))
         (verdict-stamp (plist-get plan :verdict-stamp))
         (api-event (map-elt run 'api-event))
         (comment-items (map-elt run 'comment-items))
         (api-replies (map-elt run 'api-replies))
         (sending (seq-filter (lambda (entry)
                                (or (and api-event (eq (car entry) 'review))
                                    (seq-find (lambda (i) (equal (plist-get i :id) (car entry)))
                                              (append comment-items api-replies))))
                              (plist-get plan :items)))
         (baseline nil)
         (result nil))
    (when sending
      (setq baseline (or (magit-dash-review--cr-snapshot slug pr-num account)
                         (user-error "Cannot read the PR's current reviews and comments from GitHub (the lookup failed or timed out; a PR with a very large history can exceed the limit); nothing was sent")))
      (magit-dash-review--cr-pending-write file sending baseline head-oid
                                           (and api-event verdict-stamp)))
    (message "magit-dash-review-submit: sending %s#%s through code-review; Emacs is blocked until GitHub answers"
             slug pr-num)
    (redisplay)
    (setq result
          (condition-case err
              (when (or api-event comment-items api-replies (plist-get plan :resolve-items))
                (code-review-api-submit
                 slug pr-num
                 :event api-event
                 :body (and api-event (plist-get plan :feedback-p) (plist-get plan :feedback))
                 :comments comment-items
                 :replies api-replies
                 :resolve (plist-get plan :resolve-items)
                 :head-oid head-oid
                 :account account
                 :on-item (lambda (id outcome remote detail)
                            (cond
                             ((eq outcome 'skipped)
                              (push id (map-elt run 'settled)))
                             ((not (eq outcome 'sent))
                              nil)
                             ((eq id 'review)
                              (setf (map-elt run 'sent-review) t)
                              (unless verdict-stamp
                                (push id (map-elt run 'settled))))
                             (t
                              (condition-case nil
                                  (progn
                                    (magit-dash-review--cr-stamp
                                     file id remote
                                     (and (listp detail) (plist-get detail :node-id)))
                                    (push id (map-elt run 'settled)))
                                (error (push id (map-elt run 'stamp-failed)))))))))
            ;; `code-review-api-submit' signals `user-error' only BEFORE it sends
            ;; anything (stale or missing head, missing body, bad arguments), and
            ;; this relies on that: a refusal sent nothing, so the record it
            ;; would leave only misleads.  Every failure after sending is
            ;; reported in the result, never signalled.
            (user-error
             (when sending
               (magit-dash-review--cr-pending-drop file (seq-map #'car sending)))
             (signal (car err) (cdr err)))))
    ;; Replay what the result knows for items whose stamp failed.
    (seq-do (lambda (id)
              (condition-case err
                  (progn
                    (magit-dash-review--cr-stamp
                     file id (cdr (assoc id (plist-get result :remote-ids)))
                     (cdr (assoc id (plist-get result :node-ids))))
                    (push id (map-elt run 'settled)))
                (error
                 (setf (map-elt run 'stamp-error-p) t)
                 (push (format "stamp %s: %s" id (error-message-string err)) (map-elt run 'errors)))))
            (reverse (map-elt run 'stamp-failed)))
    ;; The verdict is stamped last, after the comments it carried.
    (when (and (map-elt run 'sent-review) verdict-stamp api-event)
      (condition-case err
          (progn
            (magit-dash-review--cr-stamp file 'review nil nil verdict-stamp)
            (push 'review (map-elt run 'settled)))
        (error
         (setf (map-elt run 'stamp-error-p) t)
         (push (format "stamp verdict: %s" (error-message-string err)) (map-elt run 'errors)))))
    (seq-do (lambda (w)
              (message "magit-dash-review-submit: %s" w))
            (plist-get result :warnings))
    (let* ((failed (plist-get result :failed))
           (uncertain (seq-filter (lambda (f) (plist-get f :maybe-accepted)) failed))
           (definite (seq-remove (lambda (f) (plist-get f :maybe-accepted)) failed))
           (found (and uncertain
                       (magit-dash-review--cr-reconcile
                        (seq-map (lambda (f) (plist-get f :id)) uncertain)
                        (plist-get plan :items) (or (plist-get result :head) head-oid)
                        account slug pr-num baseline
                        (magit-dash-review--cr-known-ids (magit-dash-review--read-json-file file)))))
           (left (append (plist-get found :missing) (plist-get found :unchecked)
                         (plist-get found :twin))))
      (setf (map-elt run 'settled)
            (append (seq-map (lambda (f) (plist-get f :id)) definite) (map-elt run 'settled)))
      (seq-do (lambda (m)
                (condition-case err
                    (progn
                      (magit-dash-review--cr-stamp
                       file (car m) (cadr m) (nth 2 m)
                       (and (eq (car m) 'review) verdict-stamp))
                      (push (car m) (map-elt run 'settled))
                      (when (eq (car m) 'review)
                        (setf (map-elt run 'sent-review) t)))
                  (error
                   (setf (map-elt run 'stamp-error-p) t)
                   (push (format "stamp %s: %s" (car m) (error-message-string err))
                         (map-elt run 'errors)))))
              (plist-get found :matched))
      (setf (map-elt run 'errors)
            (append (seq-map (lambda (group)
                               (format "%s: %s"
                                       (string-join (seq-map (lambda (f) (format "%s" (plist-get f :id)))
                                                             (cdr group))
                                                    ", ")
                                       (car group)))
                             (seq-group-by (lambda (f) (plist-get f :error)) definite))
                    (map-elt run 'errors)))
      (when left
        (push (format "%s may or may not have reached GitHub (a slow request can still land); they stay pending and no later submit sends anything until M-x magit-dash-review-clear-submit-pending resolves them"
                      (string-join (seq-map (lambda (id) (format "%s" id)) left) ", "))
              (map-elt run 'errors))))
    (when (plist-get plan :empty-verdict)
      (message "magit-dash-review-submit: the pending COMMENT verdict has no summary or comments to post; marking it settled")
      (condition-case err
          (progn
            (magit-dash-review--cr-stamp file 'review nil nil verdict-stamp)
            (setf (map-elt run 'sent-review) t))
        (error
         (setf (map-elt run 'stamp-error-p) t)
         (push (format "stamp verdict: %s" (error-message-string err)) (map-elt run 'errors)))))
    ;; Only what was stamped or definitely failed leaves the record.
    (when (or sending (plist-get plan :record))
      (condition-case err
          (magit-dash-review--cr-pending-drop file (map-elt run 'settled))
        (error
         (push (format "pending record: %s" (error-message-string err)) (map-elt run 'errors)))))
    (when (and (plist-get plan :verdict-pending) (plist-get plan :cr-verdict-p)
               (map-elt run 'sent-review) (not (map-elt run 'stamp-error-p)))
      (cond
       ((fboundp 'code-review-api-set-verdict)
        (condition-case err
            (code-review-api-set-verdict slug pr-num nil)
          (error
           (message "magit-dash-review-submit: could not clear code-review's verdict: %s"
                    (error-message-string err)))))
       (t
        (message "magit-dash-review-submit: code-review-api-set-verdict is unavailable; its pending verdict was not cleared"))))))

(defun magit-dash-review--cr-submit (file feedback)
  "Submit the review in FILE with FEEDBACK through `code-review-api-submit'.
See `magit-dash-review-submit'.  Return FILE; signal when anything remains
unsent.  The lock is taken first and held to the end.  While `submit_pending'
holds an item that is not positively resolved nothing at all is sent: no
uncertain item is ever sent again automatically.  When interactive, the user
confirms first and the file is read AGAIN afterwards, so what is sent is what
the file says after the prompt."
  (unless (fboundp 'code-review-api-submit)
    (user-error "`code-review-api-submit' is not available; load code-review or turn off `magit-dash-review-submit-via-code-review'"))
  (let ((lock (magit-dash-review--cr-lock file)))
    (unwind-protect
        (let ((plan (magit-dash-review--cr-plan file feedback))
              (run (make-hash-table :test #'eq)))
          (magit-dash-review--cr-check plan)
          (unless noninteractive
            (magit-dash-review--cr-confirm plan)
            (setq plan (magit-dash-review--cr-plan file feedback))
            (magit-dash-review--cr-check plan))
          (setf (map-elt run 'comment-items) (plist-get plan :comment-items))
          (setf (map-elt run 'api-replies) (plist-get plan :api-replies))
          (setf (map-elt run 'api-event) (plist-get plan :api-event))
          ;; The token is resolved here, where a gh or keyring call may block or
          ;; prompt, never inside the no-quit region below.
          (when (fboundp 'code-review-auth-resolve)
            (condition-case err
                (code-review-auth-resolve 'github
                                          (or (bound-and-true-p code-review-github-host) "api.github.com")
                                          (plist-get plan :account))
              (error
               (user-error "No token for %s: %s" (plist-get plan :account) (error-message-string err)))))
          (let ((inhibit-quit t))
            (unwind-protect
                (progn
                  (with-timeout (magit-dash-review-submit-time-limit (setf (map-elt run 'timed-out) t))
                    (when (plist-get plan :record-entries)
                      (magit-dash-review--cr-resolve-record file plan run))
                    (magit-dash-review--cr-send file plan run))
                  (setf (map-elt run 'completed) t))
              (when quit-flag
                (setq quit-flag nil)
                (setf (map-elt run 'quit-seen) t))
              (when (map-elt run 'quit-seen)
                (message "Quit deferred; submit %s"
                         (cond ((map-elt run 'completed) "completed") (t "interrupted"))))
              (when (map-elt run 'timed-out)
                (push (format "the whole submit timed out after %ss and was abandoned; GitHub may still accept what was sent; the items stay pending until M-x magit-dash-review-clear-submit-pending"
                              magit-dash-review-submit-time-limit)
                      (map-elt run 'errors)))))
          (let ((errors (reverse (map-elt run 'errors))))
            (when (and magit-dash-review-sync-viewed (not errors))
              (condition-case err
                  (magit-dash-review--sync-viewed file (cons (plist-get plan :slug) (plist-get plan :pr-num))
                                                  (plist-get plan :dir) (plist-get plan :account-plist))
                (error
                 (message "magit-dash-review-submit: viewed sync failed: %s" (error-message-string err)))))
            (magit-dash-review--refresh-visiting-buffer file)
            (when errors
              (error "Submit incomplete, %d problem(s); drafts are kept and nothing uncertain is resent (M-x magit-dash-review-clear-submit-pending resolves an unfinished-submit record): %s"
                     (length errors) (string-join errors "; ")))
            file))
      (magit-dash-review--cr-unlock lock))))

;;; Viewed state
;; diff-review's `viewed' maps path to the blob it was viewed at.  code-review
;; (local only) and GitHub (markFileAsViewed, unmarkFileAsViewed) track it too.
;; A mark is used only where code-review's stored blob for the path matches the
;; diff-review blob, because code-review's own flag follows the diff block, not
;; the blob.  `magit_dash.viewed_synced' records the marks (path to blob) known
;; to be on GitHub: a path in it that the file no longer has was un-viewed
;; locally on purpose, so it is unmarked on GitHub (never anything else is) and
;; not imported back.  The record changes only after GitHub confirmed, so a
;; failed push is retried next time.  The `gh' calls use the review's account.
;; Only the first 100 files of a PR are read from GitHub; a larger PR is not
;; pushed.  Everything here is best effort.

(defconst magit-dash-review--viewed-query
  "query($owner:String! $repo:String! $number:Int!){
  repository(owner:$owner name:$repo){
    pullRequest(number:$number){ id headRefOid files(first:100){ pageInfo{ hasNextPage } nodes{ path viewerViewedState } } }
  }
}"
  "GraphQL query for a PR's node id, head and per-file viewed state.")

(defun magit-dash-review--viewed-mutation (viewed-p)
  "Return the GraphQL mutation that marks (VIEWED-P) or unmarks a file viewed."
  (format "mutation($id:ID! $path:String!){ %s(input:{pullRequestId:$id path:$path}){ pullRequest{ id } } }"
          (cond
           (viewed-p "markFileAsViewed")
           (t "unmarkFileAsViewed"))))

(defun magit-dash-review--gh-viewed-state (slug pr-num dir account on-state)
  "Fetch the viewed state of PR-NUM in SLUG with `gh' in DIR, asynchronously.
ACCOUNT is the gh account plist to use.  Call ON-STATE with (PR-ID HEAD-OID
STATES TRUNCATED), STATES an alist of path to viewerViewedState and TRUNCATED
non-nil when the PR has more files than were read.  Text goes as raw `-f'
fields (never `-F', which would read local files); only the number is typed."
  (let ((parts (split-string slug "/")))
    (magit-dash-gh-with-account account
      (magit-dash-gh--run-process
       (list "api" "graphql"
             "-f" (concat "query=" magit-dash-review--viewed-query)
             "-f" (concat "owner=" (car parts))
             "-f" (concat "repo=" (cadr parts))
             "-F" (format "number=%d" pr-num))
       dir
       (lambda (out)
         (condition-case err
             (let* ((resp (json-parse-string out :object-type 'alist :array-type 'list))
                    (pr (map-nested-elt resp '(data repository pullRequest))))
               (unless pr
                 (error "no pull request in the response"))
               (funcall on-state
                        (map-elt pr 'id)
                        (map-elt pr 'headRefOid)
                        (seq-map (lambda (n)
                                   (cons (map-elt n 'path) (map-elt n 'viewerViewedState)))
                                 (map-nested-elt pr '(files nodes)))
                        (eq t (map-nested-elt pr '(files pageInfo hasNextPage)))))
           (error
            (message "magit-dash-review: viewed state of %s#%s unavailable: %s"
                     slug pr-num (error-message-string err)))))
       (lambda (msg _code)
         (message "magit-dash-review: viewed state of %s#%s unavailable: %s" slug pr-num msg))))))

(defun magit-dash-review--push-viewed (pr-id ops dir account on-done)
  "Apply OPS, a list of (PATH . VIEWED-P), to PR-ID on GitHub through `gh' in DIR.
ACCOUNT is the gh account plist.  ON-DONE is called with (PATH VIEWED-P) after
each request GitHub accepted.  One asynchronous request at a time; a failure
messages and the rest still run."
  (when ops
    (let ((op (car ops))
          (next (lambda (&rest _)
                  (magit-dash-review--push-viewed pr-id (cdr ops) dir account on-done))))
      (magit-dash-gh-with-account account
        (magit-dash-gh--run-process
         (list "api" "graphql"
               "-f" (concat "query=" (magit-dash-review--viewed-mutation (cdr op)))
               "-f" (concat "id=" pr-id)
               "-f" (concat "path=" (car op)))
         dir
         (lambda (_out)
           (funcall on-done (car op) (cdr op))
           (funcall next))
         (lambda (msg code)
           (message "magit-dash-review: %s viewed of %s on GitHub failed: %s"
                    (cond
                     ((cdr op) "marking")
                     (t "unmarking"))
                    (car op) msg)
           (funcall next code)))))))

(defun magit-dash-review--viewed-synced-set (file path blob)
  "Record in FILE that GitHub has PATH viewed at BLOB, or (nil BLOB) not viewed."
  (let* ((pair (magit-dash-review-json-read-with-stamp file))
         (data (car pair))
         (stamp (cdr pair))
         (md (map-elt data "magit_dash")))
    (when (hash-table-p md)
      (let ((synced (cond
                     ((hash-table-p (map-elt md "viewed_synced"))
                      (map-elt md "viewed_synced"))
                     (t (make-hash-table :test #'equal)))))
        (cond
         (blob
          (setf (map-elt synced path) blob))
         (t
          (map-delete synced path)))
        (setf (map-elt md "viewed_synced") synced)
        (magit-dash-review--write-json-file file data stamp)))))

(defun magit-dash-review--sync-viewed (file key dir account)
  "Mirror the viewed marks of FILE for PR KEY to code-review and GitHub.
KEY is (REPO . NUMBER), ACCOUNT the review's gh account plist.  Marks whose
blob agrees with code-review's stored blob are set in code-review (seam op
`set-viewed') and, when GitHub does not have them, marked there.  A path in
`viewed_synced' that FILE no longer views was un-viewed on purpose: it is
unmarked on GitHub when GitHub shows it viewed.  Nothing else is ever
unmarked, and a blob mismatch is never marked.  `viewed_synced' changes only
after GitHub confirmed.  GitHub is asked asynchronously through
`magit-dash-gh--run-process' (retrying 5xx), only for the head the review was
made at (`head_oid', which must be known) and a PR of at most 100 files."
  (let* ((data (magit-dash-review--read-for-write file))
         (md (map-elt data "magit_dash"))
         (local (map-elt data "viewed"))
         (synced (map-elt md "viewed_synced"))
         (reviewed (map-elt md "head_oid"))
         (api (funcall magit-dash-review-json-code-review-function 'viewed key))
         (agree (make-hash-table :test #'equal))
         (unviewed nil))
    (when (and (hash-table-p local) (hash-table-p api))
      (map-do (lambda (path blob)
                (when-let* ((info (map-elt api path)))
                  (when (magit-dash-review-json-viewed-blob-match-p blob (map-elt info "blob"))
                    (setf (map-elt agree path) blob))))
              local))
    (when (and (hash-table-p local) (hash-table-p synced))
      (setq unviewed (seq-remove (lambda (path)
                                   (map-elt local path))
                                 (map-keys synced))))
    (when (> (map-length agree) 0)
      (funcall magit-dash-review-json-code-review-function 'set-viewed key agree))
    (cond
     ((not (or (> (map-length agree) 0) unviewed))
      nil)
     ((magit-dash-review--blank-p reviewed)
      (message "magit-dash-review: no head_oid recorded for the review; viewed state not pushed to GitHub"))
     (t
      (magit-dash-review--gh-viewed-state
       (car key) (cdr key) dir account
       (lambda (pr-id head states truncated)
         (cond
          ((not (equal reviewed head))
           (message "magit-dash-review: PR head moved since the review; viewed state not pushed to GitHub"))
          (truncated
           (message "magit-dash-review: %s#%s has more than 100 files; viewed state not pushed to GitHub"
                    (car key) (cdr key)))
          (t
           (let ((github-viewed (lambda (path)
                                  (equal (cdr (assoc path states)) "VIEWED"))))
             ;; Already as wanted on GitHub: just record it.
             (seq-do (lambda (path)
                       (when (funcall github-viewed path)
                         (magit-dash-review--viewed-synced-set file path (map-elt agree path))))
                     (map-keys agree))
             (seq-do (lambda (path)
                       (unless (funcall github-viewed path)
                         (magit-dash-review--viewed-synced-set file path nil)))
                     unviewed)
             (magit-dash-review--push-viewed
              pr-id
              (append
               (seq-map (lambda (path) (cons path t))
                        (seq-remove github-viewed (map-keys agree)))
               (seq-map (lambda (path) (cons path nil))
                        (seq-filter github-viewed unviewed)))
              dir account
              (lambda (path viewed-p)
                (magit-dash-review--viewed-synced-set
                 file path (and viewed-p (map-elt agree path))))))))))))))

(defun magit-dash-review--import-github-viewed (file slug pr-num dir account)
  "Import GitHub's viewed files of PR-NUM in SLUG into the `viewed' map of FILE.
Asynchronous, best effort, with the gh ACCOUNT plist.  A file is imported
only when GitHub's head is the head FILE was made at (which must be known),
code-review's stored blob for it is known, and that blob matches the blob git
resolves for the path at the head in DIR.  Marks FILE already has are kept,
and a file in `viewed_synced' that FILE no longer has was un-viewed on purpose
and is not imported back."
  (magit-dash-review--gh-viewed-state
   slug pr-num dir account
   (lambda (_pr-id head states _truncated)
     (let* ((pair (magit-dash-review-json-read-with-stamp file))
            (data (car pair))
            (stamp (cdr pair))
            (md (map-elt data "magit_dash"))
            (reviewed (map-elt md "head_oid"))
            (api (funcall magit-dash-review-json-code-review-function 'viewed (cons slug pr-num)))
            (local (cond
                    ((hash-table-p (map-elt data "viewed"))
                     (map-elt data "viewed"))
                    (t (make-hash-table :test #'equal))))
            (synced (cond
                     ((hash-table-p (map-elt md "viewed_synced"))
                      (map-elt md "viewed_synced"))
                     (t (make-hash-table :test #'equal))))
            (added 0))
       (cond
        ((not (and (stringp head) (stringp reviewed) (equal head reviewed)))
         (message "magit-dash-review: GitHub's head is not a known reviewed head; viewed state not imported"))
        ((not (hash-table-p api))
         (message "magit-dash-review: code-review has no stored diff for %s#%s; viewed state not imported" slug pr-num))
        (t
         (seq-do (lambda (entry)
                   (when (and (equal (cdr entry) "VIEWED")
                              (not (map-elt local (car entry)))
                              (not (map-elt synced (car entry))))
                     (let ((blob (magit-dash-review-json-resolve-blob dir head (car entry)))
                           (theirs (map-elt (map-elt api (car entry)) "blob")))
                       (when (magit-dash-review-json-viewed-blob-match-p blob theirs)
                         (setf (map-elt local (car entry)) blob)
                         (setf (map-elt synced (car entry)) blob)
                         (setq added (1+ added))))))
                 states)
         (when (> added 0)
           (setf (map-elt data "viewed") local)
           (setf (map-elt md "viewed_synced") synced)
           (magit-dash-review--write-json-file file data stamp)
           (magit-dash-review--refresh-visiting-buffer file))))))))

(defun magit-dash-review--maybe-import-viewed (file slug pr-num dir)
  "Import GitHub's viewed state into FILE when the code-review path is enabled.
Needs a configured gh account for SLUG (the review's own).  Never signals: a
failure messages."
  (when (and magit-dash-review-submit-via-code-review magit-dash-review-sync-viewed)
    (condition-case err
        (if-let* ((account (magit-dash-review--cr-account dir slug)))
            (magit-dash-review--import-github-viewed file slug pr-num dir account)
          (message "magit-dash-review: no gh account configured for %s; viewed state not imported" slug))
      (error
       (message "magit-dash-review: viewed import failed: %s" (error-message-string err))))))

;;; Agent Remediation, ASW/ASQ Dispatch, Hunk Critique, and Draft Promotion

(defvar magit-dash-review-asq-enqueue-function nil
  "Optional override function `(lambda (prompt worktree))' for ASQ prompt queueing.")

(defun magit-dash-review--enqueue-prompt (prompt worktree)
  "Enqueue PROMPT for the agent session associated with WORKTREE."
  (let ((default-directory (file-name-as-directory
                            (expand-file-name (or worktree default-directory)))))
    (cond
     ((functionp magit-dash-review-asq-enqueue-function)
      (funcall magit-dash-review-asq-enqueue-function prompt default-directory))
     ((fboundp 'agent-shell-prompt-queue)
      (agent-shell-prompt-queue prompt))
     (t
      (error "No agent-shell queue dispatcher available for %s" default-directory)))
    prompt))

(defun magit-dash-review--format-remediation-prompt (findings review-file)
  "Format a remediation prompt for FINDINGS in REVIEW-FILE."
  (let* ((first-f (car findings))
         (worktree (or (plist-get first-f :worktree) default-directory))
         (idx 0))
    (concat
     (format "Remediate the following %d review finding(s) in `%s` (review file: `%s`):\n\n"
             (length findings) worktree review-file)
     (mapconcat
      (lambda (f)
        (setq idx (1+ idx))
        (let* ((id (or (plist-get f :id) "unknown"))
               (path (or (plist-get f :path) "(review-wide)"))
               (line (plist-get f :line))
               (end-line (plist-get f :end-line))
               (loc (cond
                     ((and line end-line (/= line end-line))
                      (format "%s:%d-%d" path line end-line))
                     (line (format "%s:%d" path line))
                     (t path)))
               (body (or (plist-get f :body) ""))
               (hunk (or (plist-get f :hunk) "")))
          (concat
           (format "%d. `%s` (id: `%s`):\n   %s\n" idx loc id body)
           (if (not (string-empty-p hunk))
               (format "```diff\n%s\n```\n" hunk)
             "")
           (format "   Reply command: `emacsclient --eval '(magit-dash-review-append-reply %S %S \"agent-shell\" \"<summary>\")'`\n"
                   review-file id))))
      findings
      "\n"))))

(defun magit-dash-review-send-findings-to-agent (findings &optional review-file)
  "Dispatch FINDINGS from REVIEW-FILE to `agent-shell' via ASW or ASQ."
  (unless findings
    (user-error "No unresolved review comments to send"))
  (let* ((first-f (car findings))
         (rf (expand-file-name (or review-file (plist-get first-f :review-file))))
         (worktree (or (plist-get first-f :worktree)
                       (plist-get first-f :root)
                       default-directory)))
    (if (and (fboundp 'agent-shell-workflow-dispatch)
             (not magit-dash-review-asq-enqueue-function))
        (agent-shell-workflow-dispatch
         'diff-review-remediate
         :args (list :review-file rf :findings findings :root worktree))
      (magit-dash-review--enqueue-prompt
       (magit-dash-review--format-remediation-prompt findings rf)
       worktree))))

;;;###autoload
(defun magit-dash-review-enqueue-asq (&optional review-file grouping)
  "Enqueue unresolved findings from REVIEW-FILE into ASQ.
GROUPING is `per-file' (default: one queue item per file) or `per-thread'."
  (interactive)
  (let* ((file (expand-file-name
                (or review-file
                    (magit-dash-review--current-review-file)
                    (user-error "No active review JSON file"))))
         (findings (magit-dash-review-extract-findings file t)))
    (unless findings
      (user-error "No unresolved review comments to send"))
    (let* ((worktree (or (plist-get (car findings) :worktree) default-directory))
           (groups (if (eq grouping 'per-thread)
                       (mapcar #'list findings)
                     (mapcar #'cdr
                             (seq-group-by
                              (lambda (f) (or (plist-get f :path) "(review)"))
                              findings)))))
      (mapcar
       (lambda (grp)
         (let ((prompt (magit-dash-review--format-remediation-prompt grp file)))
           (magit-dash-review--enqueue-prompt prompt worktree)))
       groups))))

;;;###autoload
(defun magit-dash-review-send-to-agent (&optional mode review-file)
  "Send unresolved findings from REVIEW-FILE to `agent-shell'.
MODE is one of:
  `workflow'     (default) — dispatch `diff-review-remediate' via ASW
  `hitl'         (C-u)     — stage findings in `hitl-plan' (`code-review' domain)
  `asq-per-file' (C-u C-u) — enqueue one ASQ work item per file."
  (interactive
   (list (cond
          ((equal current-prefix-arg '(16)) 'asq-per-file)
          (current-prefix-arg 'hitl)
          (t 'workflow))
         (magit-dash-review--current-review-file)))
  (let* ((file (expand-file-name
                (or review-file
                    (magit-dash-review--current-review-file)
                    (user-error "No active review JSON file"))))
         (mode (or mode 'workflow))
         (findings (magit-dash-review-extract-findings file t)))
    (unless findings
      (user-error "No unresolved review comments to send"))
    (pcase mode
      ('hitl
       (require 'magit-dash-review-hitl)
       (magit-dash-review-stage-hitl file 'send-to-agent))
      ('asq-per-file
       (magit-dash-review-enqueue-asq file 'per-file))
      (_
       (magit-dash-review-send-findings-to-agent findings file)))))

(defun magit-dash-review--insert-draft-comment (review-file path line end-line author body)
  "Insert a `status: \"draft\"' comment into REVIEW-FILE and return its table."
  (let* ((file (expand-file-name review-file))
         (pair (magit-dash-review-json-read-with-stamp file))
         (data (car pair))
         (stamp (cdr pair))
         (comments (append (or (map-elt data "comments") []) nil))
         (c-ht (make-hash-table :test #'equal))
         (id (magit-dash-review--unique-id
              (format "%s:%s:%s:%s" (or path "") (or line 0) body (float-time)))))
    (setf (map-elt c-ht "id") id)
    (setf (map-elt c-ht "origin") "agent")
    (setf (map-elt c-ht "status") "draft")
    (when path (setf (map-elt c-ht "path") path))
    (when line (setf (map-elt c-ht "lines") (vector line (max line (or end-line line)))))
    (setf (map-elt c-ht "author") (or author "gptel"))
    (setf (map-elt c-ht "at") (format-time-string "%FT%T%:z"))
    (setf (map-elt c-ht "body") (string-trim body))
    (setf (map-elt c-ht "resolved") :false)
    (setf (map-elt c-ht "replies") [])
    (setf (map-elt data "comments") (vconcat comments (list c-ht)))
    (magit-dash-review--write-json-file file data stamp)
    (magit-dash-review--refresh-visiting-buffer file)
    c-ht))

;;;###autoload
(defun magit-dash-review-critique-hunk (&optional extra-prompt review-file)
  "Run an AI critique on the diff hunk at point and record a draft comment.
Uses `gptel-request' when available, falling back to `agent-shell'."
  (interactive)
  (let* ((ctx (magit-dash-review-current-hunk))
         (rf (expand-file-name
              (or review-file
                  (plist-get ctx :review-file)
                  (magit-dash-review--current-review-file)
                  (user-error "Not in an active diff-review session"))))
         (path (or (plist-get ctx :path) "unknown"))
         (line (or (plist-get ctx :line) 1))
         (end-line (or (plist-get ctx :end-line) line))
         (hunk (or (plist-get ctx :hunk) ""))
         (prompt (format "Critique the following diff hunk in `%s` (lines %d-%d). Identify concrete defects, edge cases, or concurrency issues concisely.%s\n\n```diff\n%s\n```"
                         path line end-line
                         (if (and extra-prompt (not (string-empty-p extra-prompt)))
                             (concat "\nFocus: " extra-prompt)
                           "")
                         hunk)))
    (if (fboundp 'gptel-request)
        (gptel-request
         prompt
         :callback
         (lambda (response _info)
           (when (and (stringp response) (not (string-empty-p (string-trim response))))
             (magit-dash-review--insert-draft-comment
              rf path line end-line "gptel" response))))
      (magit-dash-review--enqueue-prompt prompt (plist-get ctx :root)))))

;;;###autoload
(defun magit-dash-review-draft-test (&optional comment-id review-file)
  "Dispatch a DRAFT-NOT-PUSH regression test authoring task for COMMENT-ID or hunk at point."
  (interactive)
  (let* ((ctx (magit-dash-review-current-hunk))
         (rf (expand-file-name
              (or review-file
                  (plist-get ctx :review-file)
                  (magit-dash-review--current-review-file)
                  (user-error "Not in an active diff-review session"))))
         (cid (or comment-id (plist-get ctx :comment-id)))
         (findings (magit-dash-review-extract-findings rf nil))
         (finding (and cid (seq-find (lambda (f) (equal (plist-get f :id) cid)) findings)))
         (worktree (or (and finding (plist-get finding :worktree))
                       (plist-get ctx :root)
                       default-directory))
         (path (or (and finding (plist-get finding :path))
                   (plist-get ctx :path)
                   "unknown"))
         (line (or (and finding (plist-get finding :line))
                   (plist-get ctx :line)
                   1))
         (body (or (and finding (plist-get finding :body))
                   "Write a regression test covering this change."))
         (hunk (or (and finding (plist-get finding :hunk))
                   (plist-get ctx :hunk)
                   ""))
         (prompt
          (if (fboundp 'code-review-agents-shell-prompt)
              (code-review-agents-shell-prompt
               (list :id (or cid "hunk")
                     :paths (list path)
                     :statement body)
               worktree)
            (format "DRAFT-NOT-PUSH Regression Test Task\n\nWorking tree: %s\nTarget file: %s (line %d)\nReview finding: %s\n\nDiff context:\n```diff\n%s\n```\n\nInstructions:\n1. Author a deterministic regression test in `%s` that fails without the fix and passes with it.\n2. Run the test suite to verify.\n3. Leave changes uncommitted in the working tree (DRAFT-NOT-PUSH) so the reviewer can inspect them."
                    worktree path line body hunk worktree))))
    (magit-dash-review--enqueue-prompt prompt worktree)))

;;;###autoload
(defun magit-dash-review-promote-draft (&optional review-file comment-id)
  "Promote a `status: \"draft\"' comment in REVIEW-FILE to `status: \"accepted\"'."
  (interactive)
  (let* ((ctx (magit-dash-review-current-hunk))
         (file (expand-file-name
                (or review-file
                    (plist-get ctx :review-file)
                    (magit-dash-review--current-review-file)
                    (user-error "No active review JSON file"))))
         (cid (or comment-id (plist-get ctx :comment-id)))
         (pair (magit-dash-review-json-read-with-stamp file))
         (data (car pair))
         (stamp (cdr pair))
         (comments (append (or (map-elt data "comments") []) nil))
         (target (if cid
                     (seq-find (lambda (c) (equal (map-elt c "id") cid)) comments)
                   (seq-find (lambda (c) (equal (map-elt c "status") "draft")) comments))))
    (unless target
      (user-error "No draft comment found to promote in %s" file))
    (setf (map-elt target "status") "accepted")
    (setf (map-elt data "comments") (vconcat comments))
    (magit-dash-review--write-json-file file data stamp)
    (magit-dash-review--refresh-visiting-buffer file)
    target))

;;; EARS Criteria & code-review State Bridge

(defun magit-dash-review--format-ears-criterion (crit)
  "Format a single EARS criterion plist or alist CRIT into a markdown bullet."
  (let ((id (or (plist-get crit :id) (map-elt crit 'id) "req"))
        (stmt (or (plist-get crit :statement) (map-elt crit 'statement)
                  (plist-get crit :text) (map-elt crit 'text) ""))
        (paths (or (plist-get crit :paths) (map-elt crit 'paths))))
    (format "- **[%s]** %s%s"
            id stmt
            (if paths (format " (`%s`)" (string-join paths "`, `")) ""))))

;;;###autoload
(defun magit-dash-review-project-ears-criteria (review-file &optional criteria-list local-comments)
  "Project EARS CRITERIA-LIST and LOCAL-COMMENTS from `code-review' into REVIEW-FILE.
Stores criteria metadata under `\"ears_criteria\"' and updates or inserts a
review-wide summary comment thread (`id: \"ears-criteria\"')."
  (let* ((file (expand-file-name review-file))
         (pair (magit-dash-review-json-read-with-stamp file))
         (data (car pair))
         (stamp (cdr pair))
         (comments (append (or (map-elt data "comments") []) nil)))
    (when criteria-list
      (let* ((bullets (mapconcat #'magit-dash-review--format-ears-criterion
                                 criteria-list "\n"))
             (body (concat "### EARS Review Criteria\n\n" bullets))
             (existing (seq-find (lambda (c) (equal (map-elt c "id") "ears-criteria"))
                                 comments))
             (c-ht (or existing (make-hash-table :test #'equal))))
        (setf (map-elt c-ht "id") "ears-criteria")
        (setf (map-elt c-ht "origin") "code-review")
        (setf (map-elt c-ht "author") "code-review-criteria")
        (setf (map-elt c-ht "at") (format-time-string "%FT%T%:z"))
        (setf (map-elt c-ht "body") body)
        (unless existing
          (setf (map-elt c-ht "resolved") :false)
          (setf (map-elt c-ht "replies") [])
          (setq comments (cons c-ht comments)))
        (setf (map-elt data "ears_criteria")
              (vconcat
               (mapcar
                (lambda (crit)
                  (let ((ht (make-hash-table :test #'equal)))
                    (setf (map-elt ht "id") (or (plist-get crit :id) (map-elt crit 'id) ""))
                    (setf (map-elt ht "statement")
                          (or (plist-get crit :statement) (map-elt crit 'statement) ""))
                    ht))
                criteria-list)))))
    (dolist (lc local-comments)
      (let* ((cid (or (plist-get lc :id) (map-elt lc 'internal-id)
                      (magit-dash-review--make-comment-id
                       (format "%s:%s" (or (plist-get lc :path) (map-elt lc 'path) "")
                               (or (plist-get lc :body) (map-elt lc 'bodyText) "")))))
             (already (seq-find (lambda (c) (equal (map-elt c "id") cid)) comments)))
        (unless already
          (let ((c-ht (make-hash-table :test #'equal))
                (path (or (plist-get lc :path) (map-elt lc 'path)))
                (line (or (plist-get lc :line) (map-elt lc 'line)))
                (body (or (plist-get lc :body) (map-elt lc 'bodyText) "")))
            (setf (map-elt c-ht "id") cid)
            (setf (map-elt c-ht "origin") "code-review")
            (when path (setf (map-elt c-ht "path") path))
            (when line (setf (map-elt c-ht "lines") (vector line line)))
            (setf (map-elt c-ht "author") (or (plist-get lc :author) user-login-name))
            (setf (map-elt c-ht "at") (format-time-string "%FT%T%:z"))
            (setf (map-elt c-ht "body") body)
            (setf (map-elt c-ht "resolved") :false)
            (setf (map-elt c-ht "replies") [])
            (setq comments (append comments (list c-ht)))))))
    (setf (map-elt data "comments") (vconcat comments))
    (magit-dash-review--write-json-file file data stamp)
    (magit-dash-review--refresh-visiting-buffer file)
    data))

;;;###autoload
(defun magit-dash-review-open-backend (&optional review-file)
  "Open the companion `code-review' buffer for REVIEW-FILE."
  (interactive)
  (let* ((file (expand-file-name
                (or review-file
                    (magit-dash-review--current-review-file)
                    (user-error "No active review JSON file"))))
         (data (magit-dash-review--read-json-file file))
         (md (map-elt data "magit_dash"))
         (url (and (hash-table-p md) (map-elt md "url")))
         (repo (and (hash-table-p md) (map-elt md "repo")))
         (num (and (hash-table-p md) (map-elt md "number"))))
    (cond
     ((and url (fboundp 'code-review-start))
      (code-review-start url))
     ((and repo num (fboundp 'code-review-start))
      (code-review-start (format "https://github.com/%s/pull/%d" repo num)))
     ((fboundp 'magit-dash-pr-dashboard-code-review)
      (funcall 'magit-dash-pr-dashboard-code-review
               (list :repo repo :number num)))
     (t
      (user-error "Cannot open code-review backend for %s" file)))))

;;; ACR Unified Review Selector

(defun magit-dash-review--collect-candidates (&optional root fetch-remote)
  "Collect unified review candidates for repository at ROOT.
Returns a list of plists `(:key :annotation :type :action :target)'."
  (let* ((dir (and (or root default-directory)
                   (file-directory-p (or root default-directory))
                   (file-name-as-directory (expand-file-name (or root default-directory)))))
         (in-git-p (and dir
                        (zerop (call-process "git" nil nil nil "-C" dir
                                             "rev-parse" "--is-inside-work-tree"))))
         (candidates nil))

    ;; 1. Uncommitted working-tree diff
    (when in-git-p
      (let ((status-out (string-trim
                         (with-output-to-string
                           (with-current-buffer standard-output
                             (call-process "git" nil t nil "-C" dir "status" "--porcelain"))))))
        (unless (string-empty-p status-out)
          (let ((mod-count (length (split-string status-out "\n" t))))
            (push (list :key "worktree (dirty)"
                        :annotation (format "[worktree] %d modified file%s"
                                            mod-count (if (= mod-count 1) "" "s"))
                        :type 'worktree
                        :target dir
                        :action (lambda () (magit-dash-review-worktree-diff dir)))
                  candidates)))))

    ;; 2. Active <git-common-dir>/review/*.json files
    (when in-git-p
      (let* ((common-dir (ignore-errors (magit-dash-review--common-dir dir)))
             (review-dir (and common-dir (expand-file-name "review" common-dir)))
             (json-files (and review-dir (file-directory-p review-dir)
                              (directory-files review-dir t "\\.json\\'"))))
        (dolist (jf json-files)
          (ignore-errors
            (let* ((data (magit-dash-review--read-json-file jf))
                   (name (file-name-sans-extension (file-name-nondirectory jf)))
                   (viewed-ht (map-elt data "viewed"))
                   (viewed-n (if (hash-table-p viewed-ht) (hash-table-count viewed-ht) 0))
                   (comments (append (or (map-elt data "comments") []) nil))
                   (open-n (seq-count (lambda (c) (not (eq (map-elt c "resolved") t))) comments))
                   (draft-n (seq-count (lambda (c) (equal (map-elt c "status") "draft")) comments))
                   (verdict-ht (map-elt data "verdict"))
                   (v-state (and (hash-table-p verdict-ht) (map-elt verdict-ht "state"))))
              (push (list :key (format "review:%s" name)
                          :annotation (format "[review] %d viewed  %d open%s%s"
                                              viewed-n open-n
                                              (if (> draft-n 0) (format " (%d draft)" draft-n) "")
                                              (if v-state (format "  [%s]" v-state) ""))
                          :type 'review-file
                          :target jf
                          :action (lambda () (magit-dash-review--open-front-end jf)))
                    candidates))))))

    ;; 3. Local worktree branches
    (when in-git-p
      (dolist (wt (ignore-errors (magit-dash-pr--git-worktrees dir)))
        (let ((branch (alist-get 'branch wt))
              (wt-path (alist-get 'path wt)))
          (when (and branch (not (member branch '("main" "master"))))
            (push (list :key (format "branch:%s" branch)
                        :annotation (format "[branch] %s" (or wt-path ""))
                        :type 'branch
                        :target branch
                        :action (lambda ()
                                  (if (fboundp 'diff-review-branch)
                                      (let ((default-directory dir))
                                        (diff-review-branch branch))
                                    (magit-dash-review-worktree-diff (or wt-path dir)))))
                  candidates)))))

    ;; 4. Cached or remote GitHub PRs
    (when (and in-git-p fetch-remote)
      (let* ((slug (ignore-errors
                     (let ((default-directory dir))
                       (magit-dash-pr-dashboard--current-repo-slug))))
             (prs (and slug (magit-dash-pr--fetch-repo-prs slug "open" dir))))
        (dolist (pr prs)
          (let ((num (map-elt pr 'number))
                (title (or (map-elt pr 'title) ""))
                (head (or (map-elt pr 'headRefName) "")))
            (when num
              (push (list :key (format "pr-%d" num)
                          :annotation (format "[PR #%d] %s (%s)" num title head)
                          :type 'pr
                          :target num
                          :action (lambda () (magit-dash-review-pr num dir)))
                    candidates))))))

    ;; 5. Staged hitl-plan items in the code-review domain
    (when (fboundp 'hitl-plan-list)
      (dolist (plan (ignore-errors (hitl-plan-list nil 'code-review)))
        (let ((st (hitl-plan-status plan)))
          (unless (memq st '(complete discarded))
            (let* ((pid (hitl-plan-id plan))
                   (desc (or (hitl-plan-description plan) ""))
                   (steps (hitl-plan-steps plan))
                   (pending (seq-count (lambda (s) (eq (hitl-plan-step-status s) 'pending))
                                       steps)))
              (push (list :key (format "hitl:%s" pid)
                          :annotation (format "[HITL] %d pending steps  %s (%s)"
                                              pending desc st)
                          :type 'hitl-plan
                          :target plan
                          :action (lambda ()
                                    (when (fboundp 'hitl-plan-review)
                                      (hitl-plan-review plan t))
                                    plan))
                    candidates))))))

    (nreverse candidates)))

;;;###autoload
(defun magit-dash-review-select (&optional fetch-remote)
  "Interactively select and open a review target using `annotated-completing-read'.
Aggregates dirty working-tree diffs, active `.git/review/*.json' files,
local worktree branches, GitHub PRs (fetched live with prefix arg
FETCH-REMOTE), and pending `code-review' `hitl-plan' items."
  (interactive "P")
  (let ((candidates (magit-dash-review--collect-candidates default-directory fetch-remote)))
    (unless candidates
      (user-error "No active review targets found"))
    (let* ((table (mapcar (lambda (c)
                            (cons (plist-get c :key) (plist-get c :annotation)))
                          candidates))
           (choice (if (fboundp 'annotated-completing-read)
                       (annotated-completing-read
                        table
                        :prompt "Review target: "
                        :require-match t
                        :category 'magit-dash-review)
                     (completing-read "Review target: " table nil t)))
           (matched (seq-find (lambda (c) (equal (plist-get c :key) choice))
                              candidates)))
      (unless matched
        (user-error "Unknown review target: %s" choice))
      (funcall (plist-get matched :action)))))

(provide 'magit-dash-review)
;;; magit-dash-review.el ends here
