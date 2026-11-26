;;; code-review-api-viewed.el --- Public viewed-state API -*- lexical-binding: t; -*-

;; This file is part of code-review, distributed under the GNU GPL v3 or later.

;;; Commentary:
;; Non-interactive, buffer-free access to the per-file viewed marks that
;; `code-review-viewed-toggle' maintains, for external front ends.
;;
;; How entries are keyed: a mark is a row in the db's `reviewed' table
;; (see `code-review-db-reviewed.el'), keyed by (PULLREQ-ID, PATH,
;; HUNK-KEY).  A file-level mark has a nil HUNK-KEY; this API reads and
;; writes only file-level state, with the same rule the UI uses: a file
;; is viewed when it has an explicit mark or all its hunks are marked
;; (`code-review-viewed--viewed-p').  PULLREQ-ID is the db row for
;; (OWNER REPO NUMBER); the row is replaced whenever the PR is
;; re-opened, and the marks are carried over.
;;
;; The table stores NO blob hash (a schema change would need a db
;; migration), so a mark is bound to the diff block, not to the blob.
;; The blob reported here is the NEW-side git blob from the `index
;; OLD..NEW' line of the file's block in the stored raw diff.  When a
;; refetch changes a file's block the existing expiry
;; (`code-review-viewed--expire') drops the mark.  A change that keeps
;; the block's hunk headers can leave the mark in place, so `:viewed-p'
;; follows the diff block and is NOT proof that the reported blob was
;; the one seen.  Callers that care pass the blob they saw to
;; `code-review-api-set-viewed', which refuses a stale blob.
;;
;; GitHub PRs: this API writes the local db only.  On a GitHub PR the
;; next fetch runs `code-review-viewed--seed-github', which lets
;; GitHub's viewerViewedState overwrite local file marks, so marks made
;; here are not durable there.  Open review buffers are not refreshed;
;; they pick the state up on their next render.  A write is refused
;; while a UI GitHub sync for the same file is in flight
;; (`code-review-viewed--pending').  Repositories are "owner/name"
;; split on the FIRST slash (the name may contain slashes); GitLab
;; nested groups are not supported.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'code-review-db)
(require 'code-review-diff)
(require 'code-review-viewed)

(defun code-review-api-viewed--blobs (diff)
  "Return an alist (PATH . NEW-BLOB) for the file blocks of DIFF.
NEW-BLOB is nil when a block has no `index' line."
  (seq-map (lambda (blk)
             (cons (car blk)
                   (when (string-match "^index [0-9a-f]+\\.\\.\\([0-9a-f]+\\)"
                                       (cdr blk))
                     (match-string 1 (cdr blk)))))
           (code-review--diff--split-by-files diff)))

(defconst code-review-api-viewed--min-blob-length 7
  "Shortest blob prefix `code-review-api-viewed--blob-match-p' accepts.")

(defun code-review-api-viewed--blob-match-p (given current)
  "Non-nil when blob hashes GIVEN and CURRENT agree.
Both must be non-empty strings; either may be abbreviated to at
least `code-review-api-viewed--min-blob-length' characters."
  (and (stringp given)
       (stringp current)
       (>= (length given) code-review-api-viewed--min-blob-length)
       (>= (length current) code-review-api-viewed--min-blob-length)
       (or (string-prefix-p given current)
           (string-prefix-p current given))))

(defun code-review-api-viewed--pr-id (repo number)
  "Return the db id of REPO (\"owner/name\") pull request NUMBER, or signal.
The newest row wins, saved or not: the viewed marks follow the row."
  (unless (and (stringp repo)
               (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" repo))
    (user-error "Repository must be \"owner/name\": %s" repo))
  (let ((owner (match-string 1 repo))
        (name (match-string 2 repo)))
    (if-let* ((row (car (last (emacsql (code-review-db)
                                       [:select [id] :from pullreq
                                                :where (and (= owner $s1)
                                                            (= repo $s2)
                                                            (= number $s3))
                                                :order-by [(asc rowid)]]
                                       owner name (format "%s" number))))))
        (car row)
      (user-error "No stored review for %s#%s" repo number))))

;;;###autoload
(defun code-review-api-viewed (repo number)
  "Return the viewed state of every file in REPO's pull request NUMBER.
REPO is \"owner/name\".  The result is a list of (PATH :blob BLOB
:viewed-p VIEWED-P), one per file of the stored diff, in diff order;
BLOB is the file's new-side blob hash (nil when the diff block has no
index line).  See the Commentary for keying and blob invalidation.
Signals `user-error' when no review is stored for REPO and NUMBER."
  (let* ((code-review-db--pullreq-id (code-review-api-viewed--pr-id repo number))
         (index (code-review-viewed--index)))
    (seq-map (lambda (entry)
               (list (car entry)
                     :blob (cdr entry)
                     :viewed-p (and (code-review-viewed--viewed-p
                                     (map-elt index (car entry)))
                                    t)))
             (code-review-api-viewed--blobs
              (or (code-review-db--pullreq-raw-diff) "")))))

;;;###autoload
(defun code-review-api-set-viewed (repo number path blob viewed-p)
  "Mark PATH of REPO's pull request NUMBER viewed (VIEWED-P) or unviewed.
BLOB is the new-side blob hash the caller saw.  When non-nil it must
match the stored diff's blob for PATH (see
`code-review-api-viewed--blob-match-p'); a nil BLOB skips the check,
and a file whose block has no index line only accepts a nil BLOB.
Returns t when the state was written, :unchanged when it already was
as requested (nothing is written), and nil when PATH is not in the
diff or BLOB is stale.  Uses the UI's own state machine
\(`code-review-viewed--set-state'), so unmarking a file clears its
hunk marks too.  Signals `user-error' while a GitHub sync for PATH is
in flight.  Local db only; see the Commentary."
  (let* ((code-review-db--pullreq-id (code-review-api-viewed--pr-id repo number))
         (current (assoc path (code-review-api-viewed--blobs
                               (or (code-review-db--pullreq-raw-diff) ""))))
         (info (map-elt (code-review-viewed--index) path))
         (mark (and viewed-p t)))
    (cond
     ((null current)
      nil)
     ((and blob
           (not (code-review-api-viewed--blob-match-p blob (cdr current))))
      nil)
     ((map-elt code-review-viewed--pending (list code-review-db--pullreq-id path))
      (user-error "Viewed state for %s is syncing; wait before changing it"
                  path))
     ((if mark
          (code-review-viewed--viewed-p info)
        (null (code-review-viewed--path-rows path)))
      :unchanged)
     (t
      (code-review-viewed--set-state path nil mark info)
      t))))

(provide 'code-review-api-viewed)
;;; code-review-api-viewed.el ends here
