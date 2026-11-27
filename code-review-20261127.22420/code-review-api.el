;;; code-review-api.el --- Public comment data API for code-review -*- lexical-binding: t; -*-

;;; Commentary:

;; A buffer-less, non-interactive API over the comments stored on a
;; saved pullreq record (the `raw-comments' slot), for external
;; front-ends (e.g. a JSON interchange adapter).  It needs no live
;; review buffer and no dynamic db context.
;;
;; STABILITY CONTRACT (see also the README):
;;
;; - The three functions `code-review-api-comments',
;;   `code-review-api-upsert-comment' and `code-review-api-delete-comment',
;;   and the comment plist shape below, are public and stable.  Keys
;;   may be added, never removed or re-typed.
;; - Comment plist keys:
;;     :id         integer, stable across reads (the adapter's cr_id)
;;     :remote-id  forge node id string, or nil
;;     :path       string, or nil
;;     :line       integer (end line of a range), or nil
;;     :lines      (START END) for a multi-line range, else nil
;;     :side       "LEFT" / "RIGHT" or nil
;;     :body       string (unicode safe)
;;     :author     login string or nil
;;     :at         creation time string or nil
;;     :status     `draft' (local, unsent), `pending' (forge review in
;;                 PENDING state) or `sent'
;;     :reply-to   integer id of the parent comment, or nil
;;     :resolved   non-nil when the thread is resolved
;; - Ids: forge comments use their numeric databaseId.  Local drafts use
;;   their stored `api-id' or, for drafts made by the interactive UI, a
;;   number derived from the draft's internal UUID.  Draft ids are in
;;   [2^44, 2^53), so they cannot collide with forge ids; a caller :id
;;   outside that range is rejected.  Adapters must pass :id on upsert
;;   to be idempotent (no :id mints a new draft each call).  Ids are not
;;   namespaced by host or account: unique within one (REPO NUMBER) only.
;; - Only local drafts are mutable.  Upserting or deleting a sent or
;;   pending forge comment signals `user-error'; a sent comment's
;;   remote fields are never altered.
;; - A PR that is not in the db: reads return nil; upsert and delete
;;   signal `user-error' (no pullreq record is ever created here, open
;;   the PR in code-review first).
;; - Comments need a :path (the section renderer rejects path-less
;;   nodes), so a draft without one signals `user-error'.
;; - Concurrency: last writer wins.  Each call is one read-modify-write
;;   inside a db transaction, but an open review buffer does not see
;;   the change until it is rebuilt.
;;
;; The verdict functions (`code-review-api-verdict',
;; `code-review-api-set-verdict', `code-review-api-verdict-stale-p')
;; and `code-review-api-pullreq' are documented in the README and share
;; this stability contract.
;;
;; :dry-run: upsert and delete accept `:dry-run t' as trailing keyword
;; arguments.  They then write inside `closql-with-transaction',
;; validate by reading back, and roll back.  The would-be result is
;; returned and nothing persists.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'a)
(require 'closql)
(require 'uuidgen)
(require 'code-review-db)
(require 'code-review-auth)

(define-error 'code-review-api-dry-run "Dry run, rolling back")

(defconst code-review-api--draft-id-base (ash 1 44)
  "Lowest id handed to local drafts; forge ids stay below it.")

(defun code-review-api--find-pullreq (repo number)
  "Return the pullreq record for REPO and NUMBER, or nil.
Tries `code-review-db-search' (saved rows), then the newest
unfinished row regardless of `saved', since ordinarily opened PRs
are not saved.  NUMBER may be an integer or a string."
  (unless (and (stringp repo)
               (string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" repo))
    (user-error "Repo must be \"owner/name\", got %S" repo))
  (let* ((owner (match-string 1 repo))
         (name (match-string 2 repo))
         (numbers (seq-uniq
                   (list number
                         (if (numberp number)
                             (number-to-string number)
                           (string-to-number number)))))
         (db (code-review-db)))
    (seq-some
     (lambda (n)
       (or (code-review-db-search owner name n)
           (when-let* ((row (car (emacsql db [:select :* :from pullreq
                                              :where (and (= owner $s1)
                                                          (= repo $s2)
                                                          (= number $s3)
                                                          (is finished nil))
                                              :order-by [(desc rowid)]
                                              :limit 1]
                                          owner name n))))
             (closql--remake-instance 'code-review-db-pullreq db row))))
     numbers)))

(defun code-review-api--node-id (node)
  "Return the integer API id of raw-comment NODE."
  (let ((api-id (a-get node 'api-id))
        (database-id (a-get node 'databaseId))
        (internal (a-get node 'internal-id)))
    (cond
     ((integerp api-id)
      api-id)
     ((and (not (a-get node 'reply?)) (not (a-get node 'local?))
           (integerp database-id))
      database-id)
     (internal
      (+ code-review-api--draft-id-base
         (string-to-number
          (substring (secure-hash 'sha1 (format "%s" internal)) 0 10)
          16)))
     (database-id
      (code-review-api--as-integer database-id)))))

(defun code-review-api--as-integer (value)
  "Return VALUE as an integer when it is one or numeric text, else nil."
  (cond
   ((integerp value) value)
   ((and (stringp value) (string-match-p "\\`[0-9]+\\'" value))
    (string-to-number value))))

(defun code-review-api--draft-p (entry node)
  "Return non-nil when NODE of raw-comments ENTRY is an unsent local draft."
  (or (equal (a-get entry 'state) "LOCAL COMMENT")
      (and (a-get node 'internal-id) (a-get node 'reply?))))

(defun code-review-api--node-plist (entry node)
  "Return the public plist for NODE of raw-comments ENTRY."
  (let ((start (a-get node 'startLine))
        (line (a-get node 'line)))
    (list :id (code-review-api--node-id node)
          :remote-id (a-get node 'id)
          :path (a-get node 'path)
          :line line
          :lines (when (and start line (not (equal start line)))
                   (list start line))
          :side (a-get node 'side)
          :body (or (a-get node 'bodyText) (a-get node 'body))
          :author (a-get-in entry '(author login))
          :at (a-get node 'createdAt)
          :status (cond
                   ((code-review-api--draft-p entry node) 'draft)
                   ((equal (a-get entry 'state) "PENDING") 'pending)
                   (t 'sent))
          :reply-to (when (a-get node 'reply?)
                      (code-review-api--as-integer (a-get node 'databaseId)))
          :resolved (and (a-get node 'isResolved) t))))

(defun code-review-api--pairs (raw-comments)
  "Return a list of (ENTRY . NODE) for every node in RAW-COMMENTS."
  (seq-mapcat
   (lambda (entry)
     (seq-map (lambda (node) (cons entry node))
              (a-get-in entry '(comments nodes))))
   raw-comments))

(defun code-review-api--plists (pr)
  "Return the public comment plists of pullreq record PR."
  (seq-map (lambda (pair) (code-review-api--node-plist (car pair) (cdr pair)))
           (code-review-api--pairs (oref pr raw-comments))))

;;;###autoload
(defun code-review-api-pullreq (repo number)
  "Return stored metadata for REPO (\"owner/name\") PR NUMBER, or nil.
Return nil when the PR is not in the db.  Otherwise return a plist
with keys :id (pullreq record UUID), :account (registered GitHub
account or `code-review-gh-default-account', else nil), :head-oid
\(PR head commit OID, or nil), :url (PR URL, or nil) and :state (PR
state string, or nil)."
  (when-let* ((pr (code-review-api--find-pullreq repo number)))
    (list :id (oref pr id)
          :account (or (code-review-gh-pr-account pr)
                       code-review-gh-default-account)
          :head-oid (or (oref pr sha)
                        (a-get (oref pr raw-infos) 'headRefOid))
          :url (and (slot-boundp pr 'url) (oref pr url))
          :state (oref pr state))))

;;;###autoload
(defun code-review-api-comments (repo number)
  "Return the comments of REPO (\"owner/name\") PR NUMBER as plists.
Return nil when the PR is not in the db.  See the file commentary
for the plist shape and the stability contract."
  (when-let* ((pr (code-review-api--find-pullreq repo number)))
    (code-review-api--plists pr)))

;; Public ids are integers in [2^44, 2^53) for drafts.
(defconst code-review-api--id-limit (ash 1 53)
  "Exclusive upper bound of draft ids.")

(defun code-review-api--mint-id ()
  "Return a random draft id."
  (+ code-review-api--draft-id-base (random (ash 1 44))))

(defun code-review-api--find-node (pr id)
  "Return the (ENTRY . NODE) of PR whose id is ID, or nil."
  (seq-find (lambda (p) (equal (code-review-api--node-id (cdr p)) id))
            (code-review-api--pairs (oref pr raw-comments))))

(defun code-review-api--build-node (plist id old parent)
  "Build the stored node for PLIST with API id ID.
OLD is the node being updated (or nil); PARENT is the forge node a
new reply answers (or nil)."
  (let* ((get (lambda (key stored)
                (if (plist-member plist key)
                    (plist-get plist key)
                  (a-get old stored))))
         (lines (plist-get plist :lines))
         (line (funcall get :line 'line))
         (start (cond
                 ((plist-member plist :lines)
                  (car lines))
                 (t
                  (a-get old 'startLine))))
         (line (if (plist-member plist :lines)
                   (or (cadr lines) line)
                 line))
         (start (when (and start line (< start line))
                  start))
         (reply-to (cond
                    (old (and (a-get old 'reply?) (a-get old 'databaseId)))
                    (t (a-get parent 'databaseId))))
         (src (or old parent)))
    (list (cons 'internal-id (or (a-get old 'internal-id) (uuidgen-4)))
          (cons 'api-id id)
          (cons 'bodyText (funcall get :body 'bodyText))
          (cons 'path (if parent (a-get parent 'path) (funcall get :path 'path)))
          (cons 'position (a-get src 'position))
          (cons 'line line)
          (cons 'side (funcall get :side 'side))
          (cons 'startLine start)
          (cons 'startSide (a-get old 'startSide))
          (cons 'databaseId reply-to)
          (cons 'diffHunk (a-get src 'diffHunk))
          (cons 'outdated (a-get old 'outdated))
          (cons 'reply? (and reply-to t))
          (cons 'line-type (a-get old 'line-type))
          (cons 'createdAt (or (a-get old 'createdAt)
                               (format-time-string "%FT%TZ" nil t)))
          (cons 'local? (not reply-to)))))

(defun code-review-api--commit (repo number mutate dry-run)
  "Run MUTATE on the pullreq of REPO and NUMBER in a transaction.
MUTATE takes the freshly fetched record, mutates `raw-comments' and
returns the affected id (or `deleted').  The record is re-fetched
inside the transaction body because `emacsql-with-transaction' may
re-run it.  Write, read back and return the plist for that id (nil
for a delete).  With DRY-RUN signal inside the transaction so it
rolls back, and return the same result."
  (condition-case err
      (closql-with-transaction (code-review-db)
        (let* ((pr (or (code-review-api--find-pullreq repo number)
                       (user-error "PR %s#%s is not in the code-review db; open it first"
                                   repo number)))
               (id (funcall mutate pr))
               (_ (code-review-db-update pr))
               (back (code-review-api--find-pullreq repo number))
               (result (seq-find (lambda (c) (equal (plist-get c :id) id))
                                 (code-review-api--plists back))))
          (when (and (not (eq id 'deleted)) (not result))
            (error "Read-back did not find comment %s" id))
          (when (and (eq id 'deleted) result)
            (error "Read-back still finds a deleted comment"))
          (when dry-run
            (signal 'code-review-api-dry-run (list result)))
          result))
    (code-review-api-dry-run (cadr err))))

(defun code-review-api--editable-pair (pr id)
  "Return the (ENTRY . NODE) of PR whose id is ID, ensuring it is a draft.
Return nil when absent; signal `user-error' when it is not local."
  (when-let* ((pair (code-review-api--find-node pr id)))
    (unless (code-review-api--draft-p (car pair) (cdr pair))
      (user-error "Comment %s was sent to the forge and is read-only" id))
    pair))

;;;###autoload
(cl-defun code-review-api-upsert-comment (repo number plist &key dry-run)
  "Create or update a local draft comment on REPO PR NUMBER from PLIST.
:id selects an existing draft, or fixes the id of a new one; it must
be an integer in [2^44, 2^53).  Without :id a fresh random id is
minted, so repeating such a call creates another draft: adapters
must pass :id for idempotence.  A new top-level draft needs :path
and a string :body.  A new reply passes :reply-to, the databaseId
of an existing forge comment in this PR; it inherits that comment's
path and hunk and is stored the way the UI stores reply drafts.
On update, absent keys keep their stored value; an explicit
`:lines nil' collapses a range; :reply-to must not change.  :author
is used only at creation; :remote-id, :resolved, :status and :at
are read-only and ignored on input.  Return the stored comment's
plist.  Signal `user-error' for a missing PR, a missing :path or
:body, a bad :id or :reply-to, or an attempt to edit a sent comment.
With DRY-RUN, write inside a transaction, validate by reading back,
roll back, and return what would have been stored."
  (let ((id (plist-get plist :id))
        (lines (plist-get plist :lines))
        (minted (code-review-api--mint-id)))
    (when (and id (not (and (integerp id)
                            (<= code-review-api--draft-id-base id)
                            (< id code-review-api--id-limit))))
      (user-error ":id must be an integer in [2^44, 2^53), got %S" id))
    (when (and lines (not (and (= (length lines) 2)
                               (seq-every-p #'integerp lines)
                               (<= (car lines) (cadr lines)))))
      (user-error ":lines must be (START END) integers, got %S" lines))
    (code-review-api--commit
     repo number
     (lambda (pr)
       (let* ((existing (and id (code-review-api--editable-pair pr id)))
              (reply-to (plist-get plist :reply-to))
              (parent (when (and reply-to (not existing))
                        (seq-some
                         (lambda (p)
                           (and (not (code-review-api--draft-p (car p) (cdr p)))
                                (equal (a-get (cdr p) 'databaseId) reply-to)
                                p))
                         (code-review-api--pairs (oref pr raw-comments)))))
              (new-id (or id minted)))
         (while (and (not id) (code-review-api--find-node pr new-id))
           (setq new-id (code-review-api--mint-id)))
         (when (and reply-to existing
                    (not (equal reply-to (a-get (cdr existing) 'databaseId))))
           (user-error ":reply-to of comment %s cannot change" id))
         (when (and reply-to (not existing) (not parent))
           (user-error ":reply-to %S is not a forge comment of this PR" reply-to))
         (let ((node (code-review-api--build-node
                      plist new-id (cdr existing) (cdr parent))))
           (unless (a-get node 'path)
             (user-error "A comment needs a :path"))
           (unless (stringp (a-get node 'bodyText))
             (user-error "A comment needs a string :body"))
           (cond
            (existing
             (oset pr raw-comments
                   (seq-map
                    (lambda (entry)
                      (if (eq entry (car existing))
                          (a-assoc-in entry '(comments nodes) (list node))
                        entry))
                    (oref pr raw-comments))))
            (t
             (oset pr raw-comments
                   (append (oref pr raw-comments)
                           (list `((author (login . ,(or (plist-get plist :author)
                                                         "local")))
                                   (state . ,(if parent
                                                 (a-get (car parent) 'state)
                                               "LOCAL COMMENT"))
                                   (comments (nodes ,node)))))))))
         new-id))
     dry-run)))

;;;###autoload
(cl-defun code-review-api-delete-comment (repo number id &key dry-run)
  "Delete the local draft with ID from REPO PR NUMBER.
Return non-nil when a draft was removed, nil when ID was not found.
Signal `user-error' for a missing PR or a sent comment.  With
DRY-RUN, write, validate by reading back, roll back, and return
what would have been returned."
  (let ((found nil))
    (code-review-api--commit
     repo number
     (lambda (pr)
       (when-let* ((pair (code-review-api--editable-pair pr id)))
         (setq found t)
         (oset pr raw-comments
               (seq-remove (lambda (entry) (eq entry (car pair)))
                           (oref pr raw-comments))))
       'deleted)
     dry-run)
    found))

;;; Verdict

(defconst code-review-api-verdict-states '(approved changes comment)
  "The states a pending verdict may take.")

;;;###autoload
(defun code-review-api-verdict (repo number)
  "Return the pending verdict of REPO PR NUMBER as a plist, or nil.
The plist is (:state STATE :head-oid OID-OR-NIL :at TIMESTAMP) where
STATE is one of `code-review-api-verdict-states'.  Return nil when no
verdict is set or the PR is not in the db.  The verdict lives in the
pullreq record's (otherwise unused) `review' slot."
  (when-let* ((pr (code-review-api--find-pullreq repo number))
              (verdict (oref pr review))
              ((plist-get verdict :state)))
    verdict))

;;;###autoload
(defun code-review-api-verdict-stale-p (repo number current-head-oid)
  "Return non-nil when REPO PR NUMBER's verdict was set on another head.
True when a verdict with a recorded head oid exists and differs from
CURRENT-HEAD-OID.  A missing verdict, or one without a head oid, is
not stale."
  (when-let* ((verdict (code-review-api-verdict repo number))
              (head (plist-get verdict :head-oid)))
    (not (equal head current-head-oid))))

;;;###autoload
(cl-defun code-review-api-set-verdict (repo number state &optional head-oid &key dry-run)
  "Set the pending verdict of REPO PR NUMBER to STATE.
STATE is `approved', `changes', `comment' (symbols or the same
names as strings), or nil to clear the verdict.  HEAD-OID is the
head commit the verdict applies to (see
`code-review-api-verdict-stale-p').  Return the stored plist, nil
when cleared.  Signal `user-error' for an unknown STATE or a PR
that is not in the db.  With DRY-RUN, write in a transaction,
read back, roll back and return what would have been stored."
  (let ((state (if (stringp state) (intern state) state)))
    (when (and state (not (memq state code-review-api-verdict-states)))
      (user-error "Verdict state must be one of %S or nil, got %S"
                  code-review-api-verdict-states state))
    (when (and head-oid (not (stringp head-oid)))
      (user-error "HEAD-OID must be a string, got %S" head-oid))
    (condition-case err
        (closql-with-transaction (code-review-db)
          (let ((pr (or (code-review-api--find-pullreq repo number)
                        (user-error "PR %s#%s is not in the code-review db; open it first"
                                    repo number))))
            (oset pr review
                  (when state
                    (list :state state
                          :head-oid head-oid
                          :at (format-time-string "%FT%TZ" nil t))))
            (code-review-db-update pr)
            (let ((back (code-review-api-verdict repo number)))
              (when dry-run
                (signal 'code-review-api-dry-run (list back)))
              back)))
      (code-review-api-dry-run (cadr err)))))

(provide 'code-review-api)
;;; code-review-api.el ends here
