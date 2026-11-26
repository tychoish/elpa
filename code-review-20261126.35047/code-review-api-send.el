;;; code-review-api-send.el --- Non-interactive send API for code-review -*- lexical-binding: t; -*-

;;; Commentary:
;; `code-review-api-submit' publishes a review (verdict, summary, inline
;; comments), replies, thread resolutions and comment edits to a GitHub
;; pull request without a review buffer or the dynamic db context, and
;; reports what happened per item so the caller can stamp what succeeded.
;;
;; It reuses the transport the interactive commands use
;; (`code-review-send-review', `-send-replies', `-new-code-comment',
;; `-toggle-resolved', `-update-comment') and so the ghub path and the
;; account selection of `code-review-auth' (the buffer-less account is
;; registered with `code-review-gh-register-pr-account'; nothing here
;; reads or changes `gh' global state).  Every request carries its text
;; as a JSON payload value: nothing is read from local files.
;;
;; The call BLOCKS until each request is answered (bounded by
;; `code-review-api-send-timeout'): run it from a worker or batch
;; context, not from a UI path that must stay responsive.
;;
;; Rate limits (429, or 403 with a rate-limit signal) are waited out and
;; retried, bounded by `code-review-api-send-max-wait' and
;; `code-review-api-send-max-retries'.  5xx answers are retried only for
;; idempotent requests (resolve, edit, head lookup); a 5xx on a create
;; may have been accepted, so it is reported as failed with
;; `:maybe-accepted' instead of being resent (a duplicate comment is
;; worse than a retry the caller decides on).
;;
;; Item shapes (plists, all strings without text properties):
;;   comment  (:id ID :path P :body B :line N [:side LEFT|RIGHT (default
;;             RIGHT) :start-line N (needs :start-side) :start-side S
;;             :remote-id R])
;;   reply    (:id ID :reply-to-id DATABASE-ID :body B [:remote-id R])
;;   resolve  (:id ID :thread-id NODE-ID :resolved BOOL [:remote-resolved BOOL])
;;   edit     (:id ID :kind KIND :comment-id N :body B)
;; Items with a :remote-id (or a :remote-resolved equal to :resolved)
;; are skipped, so re-submitting a partly sent batch sends the rest.
;;
;; Failure classes.  A rejected request (4xx, auth, a rate limit) was not
;; processed.  Any other failure (5xx, timeout, connection or read error)
;; of a request that is NOT idempotent (review, comment, reply) may have
;; been accepted: the failure says :maybe-accepted t and :retryable nil,
;; and the caller must RECONCILE BEFORE RESEND with
;; `code-review-api-reconcile' (match by commit_id and body).  For an
;; idempotent request (resolve, edit, reads) such failures are
;; :retryable t and :maybe-accepted nil.  Never both are true.
;;
;; Callers should always pass :account: the buffer-less send has no
;; buffer-local account and falls back to `code-review-gh-default-account'.
;;
;; Limits.  The inline comments of a review go in ONE POST; this API does
;; not chunk or use a pending review (an open design question), so a very
;; large review is one request that either lands whole or fails whole.
;; After the POST the review's comments are fetched to return their
;; remote ids; when that fetch fails a :warnings entry says so.
;;
;; Blocking time.  Each request waits at most
;; `code-review-api-send-timeout'; with retries a request can take
;; (1+ `code-review-api-send-max-retries') waits plus the sleeps between
;; them, each sleep at most `code-review-api-send-max-wait' (the head
;; lookup, the comment fetch and `code-review-api-reconcile' follow the
;; same bound per request).  Errors are matched to the request that
;; produced them by URL; a late answer from a timed-out request is
;; ignored.  Requests on the shared GraphQL URL (resolve) cannot be told
;; apart, which is why only one request is ever in flight.
;;
;; `:on-item' (ID OUTCOME REMOTE-ID DETAIL) runs as soon as each item's
;; outcome (sent, failed or skipped) is known, so callers can checkpoint
;; immediately; its errors are demoted and never abort the send.
;;
;; Node ids.  GitHub has two id spaces: the REST integer id (REMOTE-ID,
;; `:remote-ids') and the GraphQL node id (a string such as PRRC_... or
;; PRR_...), which is what GraphQL-based syncs key on.  The REST
;; responses carry both; `:node-ids' of the result is an alist
;; (ID . NODE-ID) next to `:remote-ids', filled for the review (ID
;; `review'), each comment (from the fetch of the review's comments, or
;; the create response), each reply and each resolve (the :thread-id
;; itself).  Edits get none: the update endpoints' callback carries no
;; response.  An item without a known node id is absent from the alist.
;;
;; Stability notes (changes that keep older callers working).
;; - The result plist gained `:node-ids'; keys are only ever added.
;; - `:on-item' DETAIL used to be nil for a sent item and the failure or
;;   skip plist otherwise.  It is now ALWAYS a plist whose first key is
;;   `:node-id' (nil when unknown), followed by the former contents:
;;   (:node-id N) for sent, (:node-id nil :id .. :kind .. :error ..) for
;;   failed, (:node-id nil :id .. :reason ..) for skipped.  Code that
;;   reads DETAIL with `plist-get' is unaffected; code that tested DETAIL
;;   for non-nil to detect failure or skip must test OUTCOME instead.
;; - `code-review-api-reconcile' reviews and comments gained `:node-id'
;;   (the response `node_id'); `:user' (the author's login string, nil
;;   when GitHub reports no user, e.g. a deleted account) is documented.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'ghub)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'code-review-auth)
(require 'code-review-interfaces)
(require 'code-review-actions)
(require 'code-review-github)

(defgroup code-review-api-send nil
  "Non-interactive sending of reviews to the forge."
  :group 'code-review)

(defcustom code-review-api-send-timeout 60
  "Seconds to wait for one request to be answered before it counts as failed."
  :group 'code-review-api-send
  :type 'number)

(defcustom code-review-api-send-max-retries 2
  "Most times one request is retried after a rate limit or an idempotent 5xx."
  :group 'code-review-api-send
  :type 'integer)

(defcustom code-review-api-send-max-wait 60
  "Longest wait, in seconds, a rate limit may ask for before the request fails."
  :group 'code-review-api-send
  :type 'number)

(defcustom code-review-api-send-backoff 2
  "Base wait in seconds before retrying a 5xx; doubles per retry."
  :group 'code-review-api-send
  :type 'number)

(defvar code-review-api-send-sleep-function #'sleep-for
  "Function called with the seconds to wait before a retry.")

(defvar code-review-api-send--error-box nil
  "While a request is awaited, a list (DONE VALUE ERROR URL-FRAGMENT).")

(defclass code-review-api-send--comment ()
  ((path :initarg :path :initform nil)
   (msg :initarg :msg :initform nil)
   (line :initarg :line :initform nil)
   (side :initarg :side :initform nil)
   (start-line :initarg :start-line :initform nil)
   (start-side :initarg :start-side :initform nil))
  "Minimal local comment accepted by `code-review-new-code-comment'.")

(defun code-review-api-send--classify (status message headers)
  "Return an error plist for an HTTP STATUS with MESSAGE and response HEADERS.
Keys: :error, :status, :kind (rate-limit, server, auth or other) and,
for a rate limit, :retry-after (seconds)."
  (let* ((header (lambda (name)
                   (cdr (assoc-string name headers t))))
         (retry (when-let* ((v (funcall header "Retry-After")))
                  (string-to-number v)))
         (reset (when-let* ((v (funcall header "X-RateLimit-Reset")))
                  (max 0 (- (string-to-number v) (float-time)))))
         (limited (or (eql status 429)
                      (and (eql status 403)
                           (or retry
                               (equal (funcall header "X-RateLimit-Remaining") "0")
                               (string-match-p "rate limit" (downcase (or message "")))))))
         (kind (cond
                (limited 'rate-limit)
                ((and (integerp status) (>= status 500)) 'server)
                ((memq status '(401 403)) 'auth)
                (t 'other))))
    (list :error (format "HTTP %s%s" (or status "?") (if message (concat ": " message) ""))
          :status status
          :kind kind
          :retry-after (when limited (or retry reset)))))

(defun code-review-api-send--parse-error (m)
  "Return an error plist for the ghub errorback argument list M."
  (let* ((err (car m))
         (headers (cadr m)))
    (cond
     ((eq (car-safe err) 'errors)
      (let ((msgs (seq-map (lambda (e)
                             (or (and (consp e) (map-elt e 'message))
                                 (prin1-to-string e)))
                           (cdr err))))
        (list :error (concat "GraphQL: " (string-join msgs "; "))
              :kind (if (string-match-p "rate.?limit" (downcase (string-join msgs " ")))
                        'rate-limit
                      'other))))
     ((and (eq (car-safe err) 'error) (eq (cadr err) 'http))
      (let ((data (nth 3 err)))
        (code-review-api-send--classify
         (nth 2 err)
         (and (consp data) (stringp (map-elt data 'message)) (map-elt data 'message))
         headers)))
     ((and (eq (car-safe err) 'error) (eq (cadr err) 'local))
      (list :error (format "%s" (nth 2 err)) :kind 'local))
     (t (list :error (prin1-to-string err) :kind 'network)))))

(defun code-review-api-send--collect-error (m)
  "Record the failure M (errorback arguments) for the request being awaited.
Ignored when no request is awaited (a late answer of a timed-out request)
or when M's request URL does not contain the awaited request's fragment."
  (let* ((box code-review-api-send--error-box)
         (req (nth 3 m))
         (url (when (ghub--req-p req)
                (ignore-errors (url-recreate-url (ghub--req-url req))))))
    (when (and box
               (not (nth 2 box))
               (or (null (nth 3 box))
                   (null url)
                   (string-search (nth 3 box) url)))
      (setcar (nthcdr 2 box) (code-review-api-send--parse-error m)))))

(add-hook 'code-review-github-error-functions #'code-review-api-send--collect-error)

(defun code-review-api-send--call (fn &optional fragment)
  "Call FN with a callback and wait for its request to finish.
FRAGMENT is a part of the request URL used to match its errors.
Return (:ok VALUE) or (:error . ERROR-PLIST)."
  (let* ((state (list nil nil nil fragment))
         (code-review-api-send--error-box state)
         (deadline (+ (float-time) code-review-api-send-timeout)))
    (condition-case err
        (funcall fn (lambda (&rest args)
                      (setcar state t)
                      (setcar (cdr state) (car args))))
      (error
       (setcar (nthcdr 2 state)
               (list :error (error-message-string err) :kind 'local))))
    (while (and (not (car state))
                (not (nth 2 state))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (cond
     ((nth 2 state) (cons :error (nth 2 state)))
     ((car state) (list :ok (cadr state)))
     (t (cons :error (list :error "timed out" :kind 'timeout))))))

(defun code-review-api-send--retry-wait (info idempotent-p tries)
  "Return seconds to wait before retry number TRIES+1, or nil to give up.
INFO is an error plist; IDEMPOTENT-P says whether resending is safe."
  (pcase (plist-get info :kind)
    ('rate-limit
     (let ((wait (or (plist-get info :retry-after) code-review-api-send-backoff)))
       (when (<= wait code-review-api-send-max-wait)
         wait)))
    ((or 'server 'timeout 'network)
     (when idempotent-p
       (* code-review-api-send-backoff (expt 2 tries))))))

(defun code-review-api-send--attempt (fn idempotent-p &optional fragment tries)
  "Run FN as `code-review-api-send--call' does, retrying per policy.
IDEMPOTENT-P allows retrying a 5xx, timeout or connection error.
FRAGMENT matches the request URL.  TRIES counts retries so far."
  (let* ((tries (or tries 0))
         (result (code-review-api-send--call fn fragment)))
    (if-let* (((eq (car result) :error))
              ((< tries code-review-api-send-max-retries))
              (wait (code-review-api-send--retry-wait (cdr result) idempotent-p tries)))
        (progn
          (funcall code-review-api-send-sleep-function wait)
          (code-review-api-send--attempt fn idempotent-p fragment (1+ tries)))
      result)))

(defun code-review-api-send--get (pr path)
  "GET PATH for PR with the bounded, retried, classified request path.
Return (:ok VALUE) or (:error . ERROR-PLIST)."
  (code-review-api-send--attempt
   (lambda (callback)
     (ghub-get path nil
               :unpaginate t
               :auth (code-review-auth-resolve 'github code-review-github-host pr)
               :username (code-review-auth-username pr)
               :host code-review-github-host
               :callback callback
               :errorback #'code-review-github-errback))
   t path))

(defun code-review-api-send--blank-p (value)
  "Return non-nil when VALUE is nil or a blank string."
  (or (null value)
      (and (stringp value) (string-empty-p (string-trim value)))))

(defun code-review-api-send--text (value)
  "Return VALUE as a plain string without text properties, or nil."
  (when (stringp value)
    (substring-no-properties value)))

(defun code-review-api-send--make-pr (repo number account)
  "Return an in-memory PR object for REPO (\"owner/name\") and NUMBER.
ACCOUNT, when given, is registered for it."
  (let ((parts (and (stringp repo) (split-string repo "/" t))))
    (unless (and (= (length parts) 2) (integerp number))
      (user-error "Need REPO \"owner/name\" and an integer NUMBER"))
    (let ((pr (code-review-github-repo :owner (car parts) :repo (cadr parts) :number number)))
      (when account
        (code-review-gh-register-pr-account pr account))
      pr)))

(defun code-review-api-send--record (acc outcome id &optional remote detail node)
  "Record OUTCOME (sent, failed or skipped) of item ID in ACC and call :on-item.
REMOTE is the remote id, DETAIL the failure or skip plist and NODE the
GraphQL node id.  :on-item gets DETAIL with `:node-id' NODE prepended."
  (pcase outcome
    ('sent
     (aset acc 0 (cons id (aref acc 0)))
     (when remote
       (aset acc 3 (cons (cons id remote) (aref acc 3))))
     (when node
       (aset acc 6 (cons (cons id node) (aref acc 6)))))
    ('failed (aset acc 1 (cons detail (aref acc 1))))
    ('skipped (aset acc 2 (cons detail (aref acc 2)))))
  (when-let* ((callback (aref acc 4)))
    (condition-case err
        (funcall callback id outcome remote (cons :node-id (cons node detail)))
      (error (message "code-review-api-send :on-item: %S" err)))))

(defun code-review-api-send--fail (acc id kind result idempotent-p)
  "Record item ID of KIND as failed in ACC from the error RESULT.
A non-4xx failure of a non-idempotent request is :maybe-accepted and not
:retryable; for an idempotent one it is :retryable."
  (let* ((info (cdr result))
         (uncertain (memq (plist-get info :kind) '(server timeout network)))
         (detail (list :id id
                       :kind kind
                       :error (plist-get info :error)
                       :status (plist-get info :status)
                       :retryable (and (or (eq (plist-get info :kind) 'rate-limit)
                                           (and uncertain idempotent-p))
                                       t)
                       :maybe-accepted (and uncertain (not idempotent-p) t))))
    (code-review-api-send--record acc 'failed id nil detail)))

(defun code-review-api-send--remote-id (value)
  "Return the integer `id' of the response VALUE, or nil."
  (when-let* ((id (ignore-errors (and (consp value) (map-elt value 'id))))
              ((integerp id)))
    id))

(defun code-review-api-send--node-id (value)
  "Return the string `node_id' of the response VALUE, or nil."
  (when-let* ((id (ignore-errors (and (consp value) (map-elt value 'node_id))))
              ((stringp id)))
    id))

(defun code-review-api-send--items (acc kind items &rest spec)
  "Send each of ITEMS (plists) of KIND, recording outcomes in ACC.
SPEC is a plist: :check (item -> error string or nil), :skip (item ->
reason or nil), :send (item callback -> request), :match (item -> URL
fragment), :node (item response -> node id; default the response's
`node_id') and :idempotent."
  (let ((check (plist-get spec :check))
        (skip (plist-get spec :skip))
        (send (plist-get spec :send))
        (match (plist-get spec :match))
        (node (or (plist-get spec :node)
                  (lambda (_item response)
                    (code-review-api-send--node-id response))))
        (idempotent (plist-get spec :idempotent)))
    (seq-do
     (lambda (item)
       (let ((id (plist-get item :id))
             (why-skip (funcall skip item))
             (why-bad (funcall check item)))
         (cond
          (why-skip
           (code-review-api-send--record acc 'skipped id nil (list :id id :reason why-skip)))
          (why-bad
           (code-review-api-send--fail
            acc id kind (cons :error (list :error why-bad :kind 'invalid)) t))
          (t
           (let ((result (code-review-api-send--attempt
                          (lambda (callback)
                            (funcall send item callback))
                          idempotent
                          (funcall match item))))
             (if (eq (car result) :ok)
                 (code-review-api-send--record
                  acc 'sent id (code-review-api-send--remote-id (cadr result)) nil
                  (funcall node item (cadr result)))
               (code-review-api-send--fail acc id kind result idempotent)))))))
     items)))

(defun code-review-api-send--comment-problem (item)
  "Return why comment ITEM cannot be sent, or nil."
  (cond
   ((code-review-api-send--blank-p (plist-get item :path)) "comment has no path")
   ((not (stringp (plist-get item :body))) "comment has no body")
   ((string-empty-p (string-trim (plist-get item :body))) "comment body is empty")
   ((not (integerp (plist-get item :line))) "comment has no line")
   ((not (member (or (plist-get item :side) "RIGHT") '("LEFT" "RIGHT")))
    "comment :side must be LEFT or RIGHT")
   ((and (plist-get item :start-line) (not (integerp (plist-get item :start-line))))
    "comment :start-line must be an integer")
   ((and (plist-get item :start-line)
         (not (member (plist-get item :start-side) '("LEFT" "RIGHT"))))
    "comment :start-line needs :start-side LEFT or RIGHT")))

(defun code-review-api-send--submit-comment (item)
  "Return the `code-review-submit-local-comment' for comment ITEM."
  (code-review-submit-local-comment
   :path (code-review-api-send--text (plist-get item :path))
   :body (code-review-api-send--text (plist-get item :body))
   :line (plist-get item :line)
   :side (or (plist-get item :side) "RIGHT")
   :start-line (plist-get item :start-line)
   :start-side (plist-get item :start-side)
   :position nil
   :internal-id (plist-get item :id)
   :line-type nil))

(defun code-review-api-send--pair-comments (items remotes)
  "Return an alist (ITEM . REMOTE-COMMENT) pairing ITEMS with REMOTES.
Pairing is by path and trimmed body, preferring an equal line; each
remote comment is used once."
  (let ((left (copy-sequence remotes))
        (pairs nil))
    (seq-do
     (lambda (item)
       (let* ((same (lambda (r)
                      (and (equal (plist-get item :path) (map-elt r 'path))
                           (equal (string-trim (plist-get item :body))
                                  (string-trim (or (map-elt r 'body) ""))))))
              (found (or (seq-find (lambda (r)
                                     (and (funcall same r)
                                          (equal (plist-get item :line) (map-elt r 'line))))
                                   left)
                         (seq-find same left))))
         (when found
           (setq left (delq found left))
           (push (cons item found) pairs))))
     items)
    pairs))

(defun code-review-api-send--summarize (alist-list kind)
  "Return plists describing the review or comment alists in ALIST-LIST.
KIND is `review' or `comment'."
  (seq-map
   (lambda (e)
     (append (list :id (map-elt e 'id)
                   :node-id (map-elt e 'node_id)
                   :commit-id (map-elt e 'commit_id)
                   :body (map-elt e 'body)
                   :user (map-nested-elt e '(user login)))
             (if (eq kind 'review)
                 (list :state (map-elt e 'state))
               (list :path (map-elt e 'path)
                     :line (or (map-elt e 'line) (map-elt e 'original_line))
                     :in-reply-to-id (map-elt e 'in_reply_to_id)
                     :review-id (map-elt e 'pull_request_review_id)))))
   alist-list))

;;;###autoload
(cl-defun code-review-api-reconcile (repo number &key account)
  "List what GitHub PR NUMBER of REPO already has, to detect a posted item.
Call this before resending anything that failed with :maybe-accepted.
Return (:reviews R :comments C :errors E): R are plists (:id :node-id
:commit-id :body :user :state), C plists (:id :node-id :commit-id :body
:user :path :line :in-reply-to-id :review-id), E strings for lookups
that failed.  :id is the REST integer id, :node-id the GraphQL id (nil
when absent) and :user the author's login string (nil without one).
Match an item by commit id and body.  ACCOUNT names the GitHub account."
  (let* ((pr (code-review-api-send--make-pr repo number account))
         (base (format "/repos/%s/%s/pulls/%s" (oref pr owner) (oref pr repo) number))
         (reviews (code-review-api-send--get pr (concat base "/reviews")))
         (comments (code-review-api-send--get pr (concat base "/comments"))))
    (list :reviews (when (eq (car reviews) :ok)
                     (code-review-api-send--summarize (cadr reviews) 'review))
          :comments (when (eq (car comments) :ok)
                      (code-review-api-send--summarize (cadr comments) 'comment))
          :errors (seq-map (lambda (r)
                             (plist-get (cdr r) :error))
                           (seq-filter (lambda (r)
                                         (eq (car r) :error))
                                       (list reviews comments))))))

;;;###autoload
(cl-defun code-review-api-submit (repo number &key event body comments replies resolve edits head-oid account on-item)
  "Send a review and follow-ups to GitHub PR NUMBER of REPO (\"OWNER/NAME\").
EVENT is APPROVE, REQUEST_CHANGES or COMMENT (nil with BODY means COMMENT).
BODY is the review summary (required for REQUEST_CHANGES).  COMMENTS are
new inline comments: with an event they go in the one review, without one
each is posted on its own.  REPLIES, RESOLVE and EDITS are item lists (see
the Commentary).  HEAD-OID is the commit the review was made at; it is
required for APPROVE and REQUEST_CHANGES, which are refused (`user-error',
nothing sent) when it differs from the PR head now or the head cannot be
fetched.  ACCOUNT names the GitHub account: always pass it.  ON-ITEM is
called (ID OUTCOME REMOTE-ID DETAIL) as each outcome becomes known;
DETAIL is a plist (:node-id NODE-ID-OR-NIL ...) followed by the failure
or skip plist, if any (see the Commentary).

One failing item never aborts the others.  Return a plist:
  :sent        ids of items sent (`review' for the review itself)
  :failed      plists (:id :kind :error :status :retryable :maybe-accepted)
  :skipped     plists (:id :reason) for items already sent or empty
  :remote-ids  alist (ID . REMOTE-ID) (the review's, replies', comments')
  :node-ids    alist (ID . NODE-ID), GraphQL node ids, same ids as
               :remote-ids plus resolved threads (their :thread-id)
  :warnings    strings (head or comment-id lookups that failed)
  :event, :head the event sent and the commit it was attached to (nil
               when it could not be determined)."
  (unless (or (null event) (stringp event))
    (user-error "code-review-api-submit: EVENT must be a string"))
  (let* ((event (when event (upcase (code-review-api-send--text event))))
         (strict (member event '("APPROVE" "REQUEST_CHANGES"))))
    (unless (member event '(nil "APPROVE" "REQUEST_CHANGES" "COMMENT"))
      (user-error "code-review-api-submit: unknown event %S" event))
    (when (and strict (code-review-api-send--blank-p head-oid))
      (user-error "Refusing to send %s: no :head-oid says which commit was reviewed" event))
    (when (and (equal event "REQUEST_CHANGES") (code-review-api-send--blank-p body))
      (user-error "REQUEST_CHANGES needs a review body"))
    (let* ((pr (code-review-api-send--make-pr repo number account))
           (acc (make-vector 7 nil))
           (review-event (or event (and (not (code-review-api-send--blank-p body)) "COMMENT")))
           (fresh (seq-remove (lambda (c)
                                (or (not (code-review-api-send--blank-p (plist-get c :remote-id)))
                                    (code-review-api-send--comment-problem c)))
                              comments))
           (head (when (or strict (and (null head-oid) (or review-event comments)))
                   (code-review-api-send--get
                    pr (format "/repos/%s/%s/pulls/%s" (oref pr owner) (oref pr repo) number))))
           (live (when (eq (car head) :ok)
                   (map-nested-elt (cadr head) '(head sha)))))
      (aset acc 4 on-item)
      (when strict
        (cond
         ((null live)
          (user-error "Refusing to send %s: cannot verify the PR head (%s)"
                      event (or (plist-get (cdr head) :error) "no head sha in response")))
         ((not (equal head-oid live))
          (user-error "Refusing to send %s: reviewed %s but the PR head is now %s; re-review the new commits first"
                      event
                      (substring head-oid 0 (min 12 (length head-oid)))
                      (substring live 0 (min 12 (length live)))))))
      (when (and head (null live) (not strict))
        (aset acc 5 (list (format "PR head lookup failed (%s); sent without a known head commit"
                                  (or (plist-get (cdr head) :error) "no head sha in response")))))
      (oset pr sha (or head-oid live))

      ;; 1. The review: summary, verdict and (with an event) the inline comments.
      (cond
       ((and review-event (or fresh strict (not (code-review-api-send--blank-p body))))
        (let ((review (code-review-submit-github-review)))
          (oset review state review-event)
          (oset review pr pr)
          (oset review local-comments (seq-map #'code-review-api-send--submit-comment fresh))
          (unless (code-review-api-send--blank-p body)
            (oset review feedback (code-review-api-send--text body)))
          (let ((result (code-review-api-send--attempt
                         (lambda (callback)
                           (code-review-send-review review callback))
                         nil
                         (format "/pulls/%s/reviews" number))))
            (if (eq (car result) :ok)
                (let* ((review-id (code-review-api-send--remote-id (cadr result)))
                       (fetched (when (and fresh review-id)
                                  (code-review-api-send--get
                                   pr (format "/repos/%s/%s/pulls/%s/reviews/%s/comments"
                                              (oref pr owner) (oref pr repo) number review-id))))
                       (pairs (when (eq (car fetched) :ok)
                                (code-review-api-send--pair-comments fresh (cadr fetched)))))
                  (code-review-api-send--record acc 'sent 'review review-id nil
                                                (code-review-api-send--node-id (cadr result)))
                  (when (and fresh (not (eq (car fetched) :ok)))
                    (aset acc 5 (cons (format "comment remote ids unavailable (%s)"
                                              (or (plist-get (cdr fetched) :error) "no review id"))
                                      (aref acc 5))))
                  (seq-do (lambda (c)
                            (code-review-api-send--record
                             acc 'sent (plist-get c :id)
                             (map-elt (alist-get c pairs nil nil #'eq) 'id) nil
                             (map-elt (alist-get c pairs nil nil #'eq) 'node_id)))
                          fresh))
              (seq-do (lambda (c)
                        (code-review-api-send--fail acc (plist-get c :id) 'comment result nil))
                      fresh)
              (code-review-api-send--fail acc 'review 'review result nil)))))
       (review-event
        (code-review-api-send--record acc 'skipped 'review nil (list :id 'review :reason 'empty))))
      (if review-event
          (seq-do (lambda (c)
                    (if (not (code-review-api-send--blank-p (plist-get c :remote-id)))
                        (code-review-api-send--record
                         acc 'skipped (plist-get c :id) nil
                         (list :id (plist-get c :id) :reason 'already-sent))
                      (when-let* ((why (code-review-api-send--comment-problem c)))
                        (code-review-api-send--fail
                         acc (plist-get c :id) 'comment
                         (cons :error (list :error why :kind 'invalid)) t))))
                  comments)
        (code-review-api-send--items
         acc 'comment comments
         :skip (lambda (c)
                 (unless (code-review-api-send--blank-p (plist-get c :remote-id))
                   'already-sent))
         :check #'code-review-api-send--comment-problem
         :match (lambda (_) (format "/pulls/%s/comments" number))
         :send (lambda (c callback)
                 (code-review-new-code-comment
                  pr
                  (code-review-api-send--comment
                   :path (code-review-api-send--text (plist-get c :path))
                   :msg (code-review-api-send--text (plist-get c :body))
                   :line (plist-get c :line)
                   :side (or (plist-get c :side) "RIGHT")
                   :start-line (plist-get c :start-line)
                   :start-side (plist-get c :start-side))
                  callback))))

      ;; 2. Replies, one request each so every outcome is its own.
      (code-review-api-send--items
       acc 'reply replies
       :skip (lambda (r)
               (unless (code-review-api-send--blank-p (plist-get r :remote-id))
                 'already-sent))
       :check (lambda (r)
                (cond
                 ((null (plist-get r :reply-to-id)) "reply has no :reply-to-id")
                 ((not (stringp (plist-get r :body))) "reply has no body")))
       :match (lambda (r) (format "/comments/%s/replies" (plist-get r :reply-to-id)))
       :send (lambda (r callback)
               (let ((obj (code-review-submit-github-replies)))
                 (oset obj pr pr)
                 (oset obj replies
                       (list (code-review-submit-reply
                              :reply-to-id (plist-get r :reply-to-id)
                              :body (code-review-api-send--text (plist-get r :body))
                              :internal-id (plist-get r :id))))
                 (code-review-send-replies
                  obj
                  (lambda (&optional created)
                    (funcall callback (list (cons 'id (car-safe created))
                                            (cons 'node_id (car-safe (oref obj node-ids))))))))))

      ;; 3. Thread resolution; idempotent, so a 5xx is retried.
      (code-review-api-send--items
       acc 'resolve resolve
       :skip (lambda (r)
               (when (and (plist-member r :remote-resolved)
                          (eq (and (plist-get r :remote-resolved) t)
                              (and (plist-get r :resolved) t)))
                 'already-in-state))
       :check (lambda (r)
                (when (code-review-api-send--blank-p (plist-get r :thread-id))
                  "resolve has no :thread-id"))
       :match (lambda (_) "graphql")
       :node (lambda (r _response)
               (plist-get r :thread-id))
       :send (lambda (r callback)
               (code-review-toggle-resolved pr (plist-get r :thread-id)
                                            (and (plist-get r :resolved) t) callback))
       :idempotent t)

      ;; 4. Edits of already-submitted comments; idempotent.
      (code-review-api-send--items
       acc 'edit edits
       :skip #'ignore
       :check (lambda (e)
                (cond
                 ((null (plist-get e :comment-id)) "edit has no :comment-id")
                 ((not (stringp (plist-get e :body))) "edit has no body")))
       :match (lambda (e) (format "/%s" (plist-get e :comment-id)))
       :send (lambda (e callback)
               (code-review-update-comment pr (plist-get e :kind) (plist-get e :comment-id)
                                           (code-review-api-send--text (plist-get e :body))
                                           callback))
       :idempotent t)

      (list :sent (nreverse (aref acc 0))
            :failed (nreverse (aref acc 1))
            :skipped (nreverse (aref acc 2))
            :remote-ids (nreverse (aref acc 3))
            :node-ids (nreverse (aref acc 6))
            :warnings (nreverse (aref acc 5))
            :event review-event
            :head (oref pr sha)))))

(provide 'code-review-api-send)
;;; code-review-api-send.el ends here
