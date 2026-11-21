;;; magit-dash-pr-status.el --- Next-step status for your open PRs -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (magit "3.0"))
;; Keywords: vc, tools, magit, github
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; Collects the state of the author's open pull requests, either in one
;; repository or across all repositories, with one GraphQL query, and
;; classifies each into a next-step bucket (fix CI, address threads, needs
;; reviews, check merge blockers, ready to merge).  Results are plain
;; plists; nothing is written to disk.  In-memory tracking metadata lives
;; in `magit-dash-pr'.
;;
;; Entry points:
;;   M-x magit-dash-pr-status
;;   emacs --batch -l magit-dash -f magit-dash-pr-status-batch [REPO]
;;   MCP tool `magit_dash_pr_status' (see `magit-dash-mcp')

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'magit-dash-gh)
(require 'magit-dash-pr)

(declare-function magit-dash-repo-path "magit-dash")
(declare-function magit-dash-repo-gh-host "magit-dash")
(declare-function magit-dash-repo-gh-account "magit-dash")
(defvar magit-dash-repo-list)


;;;; Customization

(defgroup magit-dash-pr-status nil
  "Next-step status for the author's open pull requests."
  :group 'magit-dash
  :prefix "magit-dash-pr-status-")

(defcustom magit-dash-pr-status-required-approvals 1
  "Approvals needed before a PR is classified as ready to merge.
Overridden per repository by `:required-approvals' in the `:pr' plist
of `magit-dash-register'."
  :type 'natnum)

(defcustom magit-dash-pr-status-author "@me"
  "GitHub login whose open PRs are collected; `@me' is the authenticated user."
  :type 'string)

(defcustom magit-dash-pr-status-ignored-reviewers '("claude")
  "Reviewer logins whose reviews never count as approvals.
Reviewers that GitHub reports as bots are always ignored."
  :type '(repeat string))

(defcustom magit-dash-pr-status-include-archived nil
  "Whether to include open PRs from archived repositories.
When nil (the default), `archived:false' is added to search queries so
archived repositories are ignored."
  :type 'boolean
  :group 'magit-dash-pr-status)

(defcustom magit-dash-pr-status-max-prs 100
  "Maximum number of open PRs collected across all pages.
A search with more results signals an error."
  :type 'natnum
  :group 'magit-dash-pr-status)

(defcustom magit-dash-pr-status-timeout 90
  "Seconds to wait for each account's asynchronous PR query; nil disables.
An account that has not answered by then is reported as having no
response, and its `gh' process is killed, so one hung account cannot keep
the other accounts' results from being shown."
  :type '(choice (const :tag "No timeout" nil) number)
  :group 'magit-dash-pr-status)

(defvar magit-dash-pr-status-page-size 10
  "Number of PRs requested per GraphQL page.
Each PR nests reviews, checks and threads, so large pages exceed
GitHub's gateway timeout (502) deterministically; keep this small.")

(defvar magit-dash-pr-status-review-limit 30
  "Number of most recent reviews requested per PR.")

(defvar magit-dash-pr-status-context-limit 50
  "Number of status-check contexts requested per PR.")

;;;; Small helpers

(defun magit-dash-pr-status--plural (n)
  "Return the plural suffix for a count of N."
  (if (= n 1) "" "s"))

(defun magit-dash-pr-status--one-line (text)
  "Return TEXT with line breaks collapsed to spaces."
  (replace-regexp-in-string "[\n\r]+" " " (or text "")))

;;;; Collection

(defun magit-dash-pr-status--query ()
  "Return the GraphQL query that fetches every open PR matching a search."
  (format "query($q:String! $first:Int! $after:String){
  search(query:$q type:ISSUE first:$first after:$after){
    pageInfo{ hasNextPage endCursor }
    nodes{
      ... on PullRequest{
        number title url state isDraft headRefName baseRefName
        repository{ nameWithOwner }
        reviewDecision mergeStateStatus
        reviews(last:%d){ pageInfo{ hasPreviousPage } nodes{ state author{ login __typename } } }
        reviewRequests(first:50){
          pageInfo{ hasNextPage }
          nodes{ requestedReviewer{ ... on User{ login } ... on Team{ name } } }
        }
        commits(last:1){
          nodes{ commit{ statusCheckRollup{
            contexts(first:%d){
              pageInfo{ hasNextPage }
              nodes{
                __typename
                ... on CheckRun{ name conclusion status detailsUrl }
                ... on StatusContext{ context state targetUrl }
              }
            }
          } } }
        }
        reviewThreads(first:%d){
          pageInfo{ hasNextPage }
          nodes{ isResolved isOutdated }
        }
      }
    }
  }
}" magit-dash-pr-status-review-limit
   magit-dash-pr-status-context-limit
   (min magit-dash-pr-thread-limit 50)))

(defconst magit-dash-pr-status--slug-re
  "\\`[A-Za-z0-9][A-Za-z0-9-]*/\\(?:[A-Za-z0-9_-]\\|\\.[A-Za-z0-9_-]\\|[A-Za-z0-9_-]\\.\\)[A-Za-z0-9_.-]*\\'"
  "Match an owner/name repository slug.
The owner starts with an alphanumeric; the name may not be only dots.")

(defun magit-dash-pr-status--check-slug (slug)
  "Signal `user-error' unless SLUG looks like owner/name."
  (unless (and (stringp slug)
               (string-match-p magit-dash-pr-status--slug-re slug))
    (user-error "Not a GitHub owner/name repository slug: %S" slug)))

(defun magit-dash-pr-status--parse-json (out)
  "Parse OUT as JSON into alists and lists; signal `user-error' if malformed."
  (let ((parsed (condition-case nil
                    (json-parse-string out :object-type 'alist
                                       :array-type 'list :null-object nil)
                  ((json-parse-error json-end-of-file) 'invalid))))
    (when (or (eq parsed 'invalid) (not (listp parsed)))
      (user-error "gh api graphql returned non-JSON output"))
    parsed))

(defun magit-dash-pr-status--fetch (slug dir)
  "Run the PR query for repository SLUG from DIR; return the parsed response.
Signal `user-error' when SLUG is malformed or `gh' fails or returns
something unparseable."
  (magit-dash-pr-status--check-slug slug)
  (magit-dash-pr-status--fetch-search
   (format "repo:%s is:pr is:open%s author:%s"
           slug
           (if magit-dash-pr-status-include-archived "" " archived:false")
           magit-dash-pr-status-author)
   dir))

;; Transient GitHub gateway failures (502/503/504) are common and clear
;; within seconds, so retry them a few times before giving up.
(defvar magit-dash-pr-status-retry-attempts 3
  "Total attempts for the GraphQL query when GitHub returns a 5xx error.")

(defvar magit-dash-pr-status-retry-delay 2
  "Seconds to wait before the first retry; doubles on each further retry.")

(defun magit-dash-pr-status--transient-failure-p (result)
  "Return non-nil when RESULT, a (CODE . OUTPUT) cons, is a transient 5xx failure."
  (and (not (eql (car result) 0))
       (string-match-p "HTTP 50[234]" (cdr result))))

(defun magit-dash-pr-status--call-with-retry (args dir)
  "Run `gh' with ARGS in DIR, retrying transient 5xx failures.
Return (CODE . OUTPUT) of the last attempt."
  (let ((attempt 1)
        (delay magit-dash-pr-status-retry-delay)
        (result (magit-dash-gh--call-process args dir)))
    (while (and (< attempt magit-dash-pr-status-retry-attempts)
                (magit-dash-pr-status--transient-failure-p result))
      (message "magit-dash: gh api graphql failed transiently, retrying in %ss" delay)
      (sleep-for delay)
      (setq delay (* 2 delay))
      (setq attempt (1+ attempt))
      (setq result (magit-dash-gh--call-process args dir)))
    result))

(defun magit-dash-pr-status--truncated-p (node)
  "Return non-nil when a nested list of PR NODE was truncated by its limit."
  (or (eq t (map-nested-elt node '(reviews pageInfo hasPreviousPage)))
      (eq t (map-nested-elt node '(reviewRequests pageInfo hasNextPage)))
      (eq t (map-nested-elt node '(reviewThreads pageInfo hasNextPage)))
      (eq t (map-nested-elt
             (car (map-nested-elt node '(commits nodes)))
             '(commit statusCheckRollup contexts pageInfo hasNextPage)))))

(defun magit-dash-pr-status--validate-response (out)
  "Parse and validate GraphQL response OUT.
Signal `user-error' on GraphQL error or a malformed response.  Nested
lists truncated by their limits degrade to partial data with a message.
Return the parsed response."
  (let ((response (magit-dash-pr-status--parse-json out)))
    (when-let* ((errors (map-elt response 'errors)))
      (user-error "GitHub GraphQL error: %s"
                  (mapconcat (lambda (e) (or (map-elt e 'message) "unknown"))
                             errors "; ")))
    (unless (and (map-nested-elt response '(data search))
                 (assq 'nodes (map-nested-elt response '(data search))))
      (user-error "GitHub GraphQL response is missing search results"))
    (when-let* ((truncated
                 (thread-last (map-nested-elt response '(data search nodes))
                              (seq-filter #'magit-dash-pr-status--truncated-p)
                              (seq-map (lambda (node) (map-elt node 'number))))))
      (message "magit-dash: partial data (nested lists truncated) for PR%s %s"
               (if (cdr truncated) "s" "")
               (mapconcat (lambda (n) (format "#%s" n)) truncated ", ")))
    response))

(defun magit-dash-pr-status--fetch-page (q dir first after)
  "Fetch one page of FIRST PRs matching Q from DIR, starting after cursor AFTER.
AFTER may be nil.  Return the parsed, validated response.  Signal
`user-error' when `gh' fails or returns something unparseable or incomplete."
  (pcase-let* ((`(,code . ,out)
                (condition-case err
                    (magit-dash-pr-status--call-with-retry
                     (list "api" "graphql"
                           "-f" (concat "query=" (magit-dash-pr-status--query))
                           "-f" (concat "q=" q)
                           "-F" (format "first=%d" first)
                           (if after "-f" "-F") (if after (concat "after=" after) "after=null"))
                     dir)
                  (file-error
                   (user-error "gh api graphql could not run: %s"
                               (error-message-string err))))))
    (unless (and (integerp code) (zerop code))
      (user-error "gh api graphql failed (%s): %s" code (string-trim out)))
    (magit-dash-pr-status--validate-response out)))

(defun magit-dash-pr-status--job-register (job handle)
  "Record process or timer HANDLE in JOB so `--job-cancel' can stop it.
JOB is a cons (CANCELLED . HANDLES), or nil when nothing needs cancelling."
  (when job
    (push handle (cdr job))))

(defun magit-dash-pr-status--job-cancel (job)
  "Mark JOB cancelled and stop its processes and timers.
Callbacks of a cancelled job are never called."
  (when job
    (setcar job t)
    (seq-do (lambda (handle)
              (cond
               ((timerp handle)
                (cancel-timer handle))
               ((and (processp handle) (process-live-p handle))
                (delete-process handle))))
            (cdr job))
    (setcdr job nil)))

(defun magit-dash-pr-status--fetch-page-async (q dir first after on-success on-error &optional attempt delay job)
  "Asynchronously fetch one page of FIRST PRs matching Q from DIR starting after cursor AFTER.
Call ON-SUCCESS with validated parsed response.
Call ON-ERROR with error message string on fatal failure, including a
failure to start `gh'.
Transient 5xx failures are retried up to `magit-dash-pr-status-retry-attempts' times.
JOB, when non-nil, collects the process and retry timers (see
`magit-dash-pr-status--job-cancel'); once it is cancelled no callback runs."
  (let* ((att (or attempt 1))
         (del (or delay magit-dash-pr-status-retry-delay))
         (args (list "api" "graphql"
                     "-f" (concat "query=" (magit-dash-pr-status--query))
                     "-f" (concat "q=" q)
                     "-F" (format "first=%d" first)
                     (if after "-f" "-F") (if after (concat "after=" after) "after=null"))))
    (unless (car job)
      (condition-case err
          (magit-dash-pr-status--job-register
           job
           (magit-dash-gh--run-process
            args dir
            (lambda (out)
              (unless (car job)
                (condition-case err
                    (funcall on-success (magit-dash-pr-status--validate-response out))
                  (error (funcall on-error (error-message-string err))))))
            (lambda (out code)
              (cond
               ((car job) nil)
               ((and (< att magit-dash-pr-status-retry-attempts)
                     (magit-dash-pr-status--transient-failure-p (cons code out)))
                (message "magit-dash: gh api graphql failed transiently (%s), retrying in %ss" code del)
                (magit-dash-pr-status--job-register
                 job
                 (run-at-time del nil
                              #'magit-dash-pr-status--fetch-page-async
                              q dir first after on-success on-error
                              (1+ att) (* 2 del) job)))
               (t
                (funcall on-error
                         (format "gh api graphql failed (%s): %s" code (string-trim out))))))))
        (error (funcall on-error (error-message-string err)))))))

(defun magit-dash-pr-status--fetch-search (q dir)
  "Run the PR query for search string Q from DIR; return the parsed response.
Fetch `magit-dash-pr-status-page-size' PRs at a time and merge the nodes
into one response.  Signal `user-error' when `gh' fails (including process
or network failure), returns something unparseable, or the search matches
more than `magit-dash-pr-status-max-prs' PRs.  Touches no cache."
  (let ((nodes nil)
        (after nil)
        (more t)
        (first-response nil))
    (while more
      (let* ((response (magit-dash-pr-status--fetch-page
                        q dir magit-dash-pr-status-page-size after))
             (search (map-nested-elt response '(data search))))
        (setq first-response (or first-response response))
        (setq nodes (append nodes (map-elt search 'nodes)))
        (setq after (map-nested-elt search '(pageInfo endCursor)))
        (setq more (eq t (map-nested-elt search '(pageInfo hasNextPage))))
        (when (and more (or (not (stringp after))
                            (>= (length nodes) magit-dash-pr-status-max-prs)))
          (user-error "GitHub returned more PRs than the query limit"))))
    (setf (map-elt (map-nested-elt first-response '(data search)) 'nodes) nodes)
    first-response))

(defun magit-dash-pr-status--fetch-search-async (q dir on-success on-error &optional job)
  "Run the PR query for search string Q from DIR asynchronously.
Calls ON-SUCCESS with the combined parsed response or ON-ERROR with an error string.
JOB is passed to `magit-dash-pr-status--fetch-page-async'."
  (let ((nodes nil)
        (first-response nil))
    (cl-labels
        ((fetch-next
          (after)
          (magit-dash-pr-status--fetch-page-async
           q dir magit-dash-pr-status-page-size after
           (lambda (response)
             (let* ((search (map-nested-elt response '(data search)))
                    (page-nodes (map-elt search 'nodes))
                    (next-after (map-nested-elt search '(pageInfo endCursor)))
                    (more (eq t (map-nested-elt search '(pageInfo hasNextPage)))))
               (setq first-response (or first-response response))
               (setq nodes (append nodes page-nodes))
               (if (and more (or (not (stringp next-after))
                                 (>= (length nodes) magit-dash-pr-status-max-prs)))
                   (funcall on-error "GitHub returned more PRs than the query limit")
                 (if more
                     (fetch-next next-after)
                   (setf (map-elt (map-nested-elt first-response '(data search)) 'nodes) nodes)
                   (funcall on-success first-response)))))
           on-error nil nil job)))
      (fetch-next nil))))

(defun magit-dash-pr-status--bot-p (author)
  "Return non-nil when review AUTHOR (a GraphQL actor alist) is ignorable."
  (when (and author (not (eq author :null)) (or (consp author) (hash-table-p author)))
    (let ((login (or (map-elt author 'login) "")))
      (or (equal (map-elt author '__typename) "Bot")
          (string-suffix-p "[bot]" login)
          (member login magit-dash-pr-status-ignored-reviewers)))))

(defun magit-dash-pr-status--latest-states (reviews)
  "Return the latest decisive review state of each human in REVIEWS.
REVIEWS is a chronological list of review nodes.  Anything but COMMENTED
or PENDING is decisive, matching GitHub: a later comment does not retract
an approval."
  (let ((latest (make-hash-table :test #'equal)))
    (dolist (review reviews)
      (when (and review (not (eq review :null)) (or (consp review) (hash-table-p review)))
        (let ((author (map-elt review 'author))
              (state (map-elt review 'state)))
          (unless (or (null author)
                      (eq author :null)
                      (not (or (consp author) (hash-table-p author)))
                      (magit-dash-pr-status--bot-p author)
                      (member state '("COMMENTED" "PENDING")))
            (puthash (map-elt author 'login) state latest)))))
    (hash-table-values latest)))

(defun magit-dash-pr-status--approvals (reviews)
  "Count humans in REVIEWS whose latest decisive review is an approval."
  (seq-count (lambda (state) (equal state "APPROVED"))
             (magit-dash-pr-status--latest-states reviews)))

(defun magit-dash-pr-status--changes-requested (reviews)
  "Count humans in REVIEWS whose latest decisive review requests changes."
  (seq-count (lambda (state) (equal state "CHANGES_REQUESTED"))
             (magit-dash-pr-status--latest-states reviews)))

(defun magit-dash-pr-status--context-failing-p (ctx)
  "Return non-nil when the status-check context CTX is failing."
  ;; Reuse the rollup parser on a one-element list so that what counts as a
  ;; failure stays defined in exactly one place.
  (equal (magit-dash-pr--parse-status-rollup (list ctx)) "FAILURE"))

(defun magit-dash-pr-status--failing (contexts)
  "Return (NAME . URL) for each failing check in CONTEXTS."
  (mapcar (lambda (ctx)
            (cons (or (map-elt ctx 'name) (map-elt ctx 'context) "unknown")
                  (or (map-elt ctx 'detailsUrl) (map-elt ctx 'targetUrl))))
          (seq-filter #'magit-dash-pr-status--context-failing-p contexts)))

(defun magit-dash-pr-status--ticket (branch)
  "Return the ticket id such as FND-1269 embedded in BRANCH, or nil."
  (when (and branch (string-match "\\b\\([A-Za-z]+-[0-9]+\\)" branch))
    (upcase (match-string 1 branch))))

(defun magit-dash-pr-status--node-to-pr (node)
  "Convert a GraphQL pull request NODE alist into a PR plist."
  (let* ((rollup (map-nested-elt
                  (car (map-nested-elt node '(commits nodes)))
                  '(commit statusCheckRollup)))
         (contexts (map-nested-elt rollup '(contexts nodes)))
         (threads (map-nested-elt node '(reviewThreads nodes)))
         (reviews (map-nested-elt node '(reviews nodes)))
         (unresolved (seq-remove (lambda (th) (eq (map-elt th 'isResolved) t))
                                 threads)))
    (list :number (map-elt node 'number)
          :repo (map-nested-elt node '(repository nameWithOwner))
          :title (map-elt node 'title)
          :url (map-elt node 'url)
          :branch (map-elt node 'headRefName)
          :base (map-elt node 'baseRefName)
          :state (map-elt node 'state)
          :draft (eq t (map-elt node 'isDraft))
          :review-decision (let ((d (map-elt node 'reviewDecision)))
                             (and d (not (equal d "")) d))
          :merge-state (map-elt node 'mergeStateStatus)
          :approvals (magit-dash-pr-status--approvals reviews)
          :changes-requested (magit-dash-pr-status--changes-requested reviews)
          :requested (delq nil
                           (mapcar (lambda (rr)
                                     (let ((r (map-elt rr 'requestedReviewer)))
                                       (or (map-elt r 'login) (map-elt r 'name))))
                                   (map-nested-elt node '(reviewRequests nodes))))
          :ci (magit-dash-pr--parse-status-rollup contexts)
          :failing (magit-dash-pr-status--failing contexts)
          :threads-open (seq-count (lambda (th) (not (eq (map-elt th 'isOutdated) t)))
                                   unresolved)
          :threads-outdated (seq-count (lambda (th) (eq (map-elt th 'isOutdated) t))
                                       unresolved)
          :ticket (magit-dash-pr-status--ticket (map-elt node 'headRefName))
          :truncated (and (magit-dash-pr-status--truncated-p node) t))))

(defun magit-dash-pr-status--stack-parent (pr prs)
  "Return the number of the PR in PRS whose head branch is PR's base, or nil.
When several PRs share that head branch the first one in PRS wins."
  (when-let* ((base (plist-get pr :base))
              (parent (seq-find (lambda (other)
                                  (and (not (eq other pr))
                                       (equal (plist-get other :branch) base)))
                                prs)))
    (plist-get parent :number)))

(defun magit-dash-pr-status--annotate-stacks (prs)
  "Return PRS with `:stack-parent' set on each stacked PR."
  (mapcar (lambda (pr)
            (plist-put (copy-sequence pr) :stack-parent
                       (magit-dash-pr-status--stack-parent pr prs)))
          prs))

(defun magit-dash-pr-status--annotate-buckets (prs required)
  "Return PRS with `:bucket' set on each PR, given REQUIRED approvals.
REQUIRED is a count, or a function called with the PR plist that returns
the count.  Also set `:required' to that count."
  (mapcar (lambda (pr)
            (let* ((count (if (functionp required) (funcall required pr) required))
                   (new (plist-put (copy-sequence pr) :bucket
                                   (magit-dash-pr-status--bucket pr count))))
              (plist-put new :required count)))
          prs))

(defun magit-dash-pr-status--parse (response required)
  "Return PR plists, sorted by number, parsed from GraphQL RESPONSE.
REQUIRED is the approval count used to bucket each PR."
  (let* ((nodes (map-nested-elt response '(data search nodes)))
         (prs (mapcar #'magit-dash-pr-status--node-to-pr
                      (seq-filter (lambda (n) (map-elt n 'number)) nodes))))
    (magit-dash-pr-status--annotate-buckets
     (magit-dash-pr-status--annotate-stacks
      (sort prs (lambda (a b) (< (plist-get a :number) (plist-get b :number)))))
     required)))

(defun magit-dash-pr-status--collect (slug dir required)
  "Return PR plists for the author's open PRs in repository SLUG, run from DIR.
REQUIRED is the approval count used to bucket each PR."
  (magit-dash-pr-status--parse
   (magit-dash-pr-status--fetch slug dir) required))

;;;; Classification

(defun magit-dash-pr-status--required-approvals (slug &optional dir)
  "Return the approvals required for repository SLUG.
Fall back to `magit-dash-pr-status-required-approvals' when the
registered value is missing or not a natural number.  DIR identifies the checkout."
  (let ((value (magit-dash-pr-repo-option
                slug :required-approvals
                magit-dash-pr-status-required-approvals dir)))
    (if (natnump value) value magit-dash-pr-status-required-approvals)))

(defun magit-dash-pr-status--bucket (pr required)
  "Classify PR into its next step: `fix-ci', `threads', `reviews', `blocked' or `ready'.
REQUIRED is the approval count needed.  Earlier buckets win, so a PR with a
failing check is never ready.  A PR with requested changes is handled as
`threads' because the next step is to address the feedback."
  (cond
   ((plist-get pr :failing) 'fix-ci)
   ((or (> (plist-get pr :threads-open) 0)
        (> (plist-get pr :changes-requested) 0))
    'threads)
   ((< (plist-get pr :approvals) required) 'reviews)
   ((or (plist-get pr :draft)
        (equal (plist-get pr :ci) "PENDING")
        (member (plist-get pr :merge-state) '("BLOCKED" "DIRTY" "BEHIND" "UNKNOWN" "DRAFT" "UNSTABLE")))
    'blocked)
   (t 'ready)))

;;;; Entry points

(defun magit-dash-pr-status-collect (slug dir)
  "Return PR plists for the author's open PRs in repository SLUG, run from DIR.
Each PR carries `:bucket' computed with the approvals required for SLUG.
The result is pure data: nothing is cached or written.  Signal
`user-error' when SLUG is malformed or the query fails; the tracking
cache is never touched."
  (magit-dash-pr-status--check-slug slug)
  (let ((prs (magit-dash-pr-status--collect
              slug dir (magit-dash-pr-status--required-approvals slug dir))))
    (mapcar (lambda (pr)
              (if (plist-get pr :repo) pr (plist-put (copy-sequence pr) :repo slug)))
            prs)))

(defun magit-dash-pr-status--targets (dir)
  "Return the search targets for an all-repositories query run from DIR.
Each target is a plist with `:label' (account name, nil for the default
`gh' account) and `:dir', a representative checkout whose registered
`:gh-account' selects the credentials.  One target exists per distinct
account among `magit-dash-repo-list'; the default account is included
when any repository, or none, lacks a configured account."
  (let* ((repos (and (boundp 'magit-dash-repo-list) magit-dash-repo-list))
         (accounted (seq-filter #'magit-dash-repo-gh-account repos))
         (groups (make-hash-table :test #'equal)))
    (seq-do (lambda (repo)
              (let ((key (format "%s@%s"
                                 (magit-dash-repo-gh-account repo)
                                 (or (magit-dash-repo-gh-host repo) "github.com"))))
                (setf (map-elt groups key)
                      (append (map-elt groups key) (list (magit-dash-repo-path repo))))))
            accounted)
    (let ((targets (seq-map (lambda (pair)
                              (list :label (car pair)
                                    :dir (or (seq-find #'file-directory-p (cdr pair))
                                             (cadr pair))))
                            (map-pairs groups))))
      (if (or (null accounted)
              (seq-find (lambda (repo) (not (magit-dash-repo-gh-account repo))) repos))
          (cons (list :label nil :dir (or dir default-directory)) targets)
        targets))))

(defun magit-dash-pr-status--all-query ()
  "Return the search string for the author's open PRs across repositories."
  (format "is:pr is:open%s author:%s"
          (if magit-dash-pr-status-include-archived "" " archived:false")
          magit-dash-pr-status-author))

(defun magit-dash-pr-status--parse-target (response)
  "Return PR plists parsed from RESPONSE with per-repository approval counts."
  (magit-dash-pr-status--parse
   response
   (lambda (pr)
     (magit-dash-pr-status--required-approvals (plist-get pr :repo)))))

(defun magit-dash-pr-status--summarize (results)
  "Merge RESULTS, a list of (TARGET . (:prs PRS)|(:error MSG)).
Return a plist: `:prs' (deduplicated by repository and number, sorted by
repository then number), `:errors' (\"ACCOUNT: MESSAGE\" strings),
`:notes' (`:errors' plus a count of PRs whose nested lists were truncated)
and `:all-failed' (non-nil when every target failed)."
  (let* ((prs (thread-last
                (seq-uniq (seq-mapcat (lambda (result) (plist-get (cdr result) :prs)) results)
                          (lambda (a b)
                            (and (equal (plist-get a :repo) (plist-get b :repo))
                                 (eql (plist-get a :number) (plist-get b :number)))))
                (seq-sort-by (lambda (pr) (plist-get pr :number)) #'<)
                (seq-sort-by (lambda (pr) (or (plist-get pr :repo) "")) #'string<)))
         (errors (thread-last results
                              (seq-filter (lambda (result) (plist-get (cdr result) :error)))
                              (seq-map (lambda (result)
                                         (format "%s: %s"
                                                 (or (plist-get (car result) :label)
                                                     "default gh account")
                                                 (plist-get (cdr result) :error))))))
         (truncated (seq-count (lambda (pr) (plist-get pr :truncated)) prs)))
    (list :prs prs
          :errors errors
          :notes (append errors
                         (when (> truncated 0)
                           (list (format "%d PR%s with truncated review/check data"
                                         truncated (if (= truncated 1) "" "s")))))
          :all-failed (and errors (= (length errors) (length results))))))

(defvar magit-dash-pr-status-last-notes nil
  "Notes from the most recent all-repositories collection, sync or async.
A list of \"ACCOUNT: MESSAGE\" strings for accounts that failed or timed
out, plus a count of PRs whose review/check data was truncated.  Nil when
the collection was complete.")

(defun magit-dash-pr-status--conclude (summary on-partial on-success on-error)
  "Report SUMMARY, a `--summarize' plist, through the shared channel.
Record the notes in `magit-dash-pr-status-last-notes'.  When every account
failed, return the result of ON-ERROR called with the joined errors.
Otherwise call ON-PARTIAL with the notes, if any (default: `message' each)
and return the result of ON-SUCCESS called with the PRs.  Both the
synchronous and asynchronous collectors end here, so they report
failures identically."
  (setq magit-dash-pr-status-last-notes
        (and (not (plist-get summary :all-failed)) (plist-get summary :notes)))
  (cond
   ((plist-get summary :all-failed)
    (funcall on-error (string-join (plist-get summary :errors) "; ")))
   (t
    (when-let* ((notes (plist-get summary :notes)))
      (funcall (or on-partial
                   (lambda (notes)
                     (seq-do (lambda (note)
                               (message "magit-dash: PR collection: %s" note))
                             notes)))
               notes))
    (funcall on-success (plist-get summary :prs)))))

(defun magit-dash-pr-status-collect-all (&optional dir on-partial)
  "Return PR plists for the author's open PRs across all repositories.
Run from DIR (default `default-directory').  One search query without a
repo qualifier is run per configured GitHub account (see
`magit-dash-pr-status--targets'); each PR's `:repo' is its owner/name
slug and its `:bucket' uses the approvals required for that repository,
falling back to `magit-dash-pr-status-required-approvals'.  The result is
pure data.  A failing account does not stop the others; signal
`user-error' only when every account fails.  Failures and truncation
notes are reported exactly as `magit-dash-pr-status-collect-all-async'
does: ON-PARTIAL is called with the note strings (default: `message'
each), which are also left in `magit-dash-pr-status-last-notes'.
The tracking cache is never touched."
  (magit-dash-pr-status--conclude
   (magit-dash-pr-status--summarize
    (seq-map
     (lambda (target)
       (cons target
             (condition-case err
                 (list :prs (magit-dash-pr-status--parse-target
                             (magit-dash-pr-status--fetch-search
                              (magit-dash-pr-status--all-query)
                              (plist-get target :dir))))
               (error (list :error (error-message-string err))))))
     (magit-dash-pr-status--targets dir)))
   on-partial
   #'identity
   (lambda (errors) (user-error "%s" errors))))

(defun magit-dash-pr-status-collect-all-async (on-success &optional on-error dir on-partial on-progress)
  "Asynchronously collect PR plists for open PRs across all repositories.
Run from DIR (default `default-directory'), one query per configured
GitHub account, in parallel.
ON-SUCCESS is called with the merged list of PR plists.  When some
accounts fail or some PRs have truncated data, ON-PARTIAL is first
called with a list of note strings such as \"ACCOUNT: MESSAGE\"
(default: `message' each; see `magit-dash-pr-status--conclude').
ON-ERROR is called with a message string when every account fails;
if nil, `message' is called.
ON-PROGRESS, when non-nil, is called as each account but the last
answers, with the PRs and note strings so far and the number of accounts
still pending, so a view can render progressively.
An account that has not answered within `magit-dash-pr-status-timeout'
is reported as failed (\"ACCOUNT: no response within Ns\") and its
process is killed.  Return a function that cancels the collection: it
stops every process and timer and no callback is called afterwards."
  (let* ((errback (or on-error
                      (lambda (err) (message "magit-dash: PR collection failed: %s" err))))
         (query (magit-dash-pr-status--all-query))
         (jobs (seq-map (lambda (target) (cons target (list nil)))
                        (magit-dash-pr-status--targets dir)))
         (pending (length jobs))
         (results nil)
         (done nil)
         (timer nil)
         (cancel (lambda ()
                   (setq done t)
                   (when timer
                     (cancel-timer timer))
                   (seq-do (lambda (entry)
                             (magit-dash-pr-status--job-cancel (cdr entry)))
                           jobs)))
         (record
          (lambda (target result)
            (unless (or done (assq target results))
              (push (cons target result) results)
              (setq pending (1- pending))
              (let ((summary (magit-dash-pr-status--summarize (reverse results))))
                (cond
                 ((zerop pending)
                  (funcall cancel)
                  (magit-dash-pr-status--conclude summary on-partial on-success errback))
                 (on-progress
                  (funcall on-progress (plist-get summary :prs)
                           (plist-get summary :notes) pending))))))))
    (when (and (numberp magit-dash-pr-status-timeout) (> magit-dash-pr-status-timeout 0))
      (setq timer
            (run-at-time
             magit-dash-pr-status-timeout nil
             (lambda ()
               (seq-do (lambda (entry)
                         (unless (assq (car entry) results)
                           (magit-dash-pr-status--job-cancel (cdr entry))
                           (funcall record (car entry)
                                    (list :error (format "no response within %ss"
                                                         magit-dash-pr-status-timeout)))))
                       jobs)))))
    (seq-do
     (lambda (entry)
       (let ((target (car entry)))
         (magit-dash-pr-status--fetch-search-async
          query (plist-get target :dir)
          (lambda (response)
            (funcall record target
                     (condition-case err
                         (list :prs (magit-dash-pr-status--parse-target response))
                       (error (list :error (error-message-string err))))))
          (lambda (err) (funcall record target (list :error err)))
          (cdr entry))))
     jobs)
    cancel))

(defun magit-dash-pr-status-run (slug dir)
  "Collect PRs for repository SLUG from DIR; return a data plist.
Return a plist with `:prs' and `:required'.  Touches no files."
  (list :prs (magit-dash-pr-status-collect slug dir)
        :required (magit-dash-pr-status--required-approvals slug dir)))

(defun magit-dash-pr-status--current ()
  "Return (SLUG . DIR) for the repository at point."
  (let* ((dir (magit-dash-gh--repo-dir))
         (info (let ((default-directory dir)) (magit-dash-gh--repo-info))))
    (unless (and (plist-get info :owner) (plist-get info :repo))
      (user-error "Cannot determine the GitHub repository for %s" dir))
    (cons (format "%s/%s" (plist-get info :owner) (plist-get info :repo)) dir)))

(defun magit-dash-pr-status--line (pr)
  "Return a one-line summary of PR."
  (format "%s#%s %s [%s]"
          (or (plist-get pr :repo) "-") (plist-get pr :number)
          (magit-dash-pr-status--one-line (plist-get pr :title))
          (plist-get pr :bucket)))

;;;###autoload
(defun magit-dash-pr-status ()
  "Show the next-step status of your open PRs in the current repository.
Return the data plist from `magit-dash-pr-status-run'."
  (interactive)
  (pcase-let* ((`(,slug . ,dir) (magit-dash-pr-status--current))
               (result (magit-dash-pr-status-run slug dir))
               (prs (plist-get result :prs)))
    (message "magit-dash: %d open PR%s in %s%s"
             (length prs) (magit-dash-pr-status--plural (length prs)) slug
             (if prs
                 (concat "\n" (mapconcat #'magit-dash-pr-status--line prs "\n"))
               ""))
    result))

;;;###autoload
(defun magit-dash-pr-status-batch ()
  "Print one line per open PR from `emacs --batch'; exit non-zero on failure.
An optional command-line argument names the repository directory."
  (let ((default-directory (file-name-as-directory
                            (expand-file-name (or (pop command-line-args-left)
                                                  default-directory)))))
    (condition-case err
        (let ((result (pcase-let ((`(,slug . ,dir) (magit-dash-pr-status--current)))
                        (magit-dash-pr-status-run slug dir))))
          (dolist (pr (plist-get result :prs))
            (princ (concat (magit-dash-pr-status--line pr) "\n"))))
      (error
       (message "magit-dash-pr-status: %s" (error-message-string err))
       (kill-emacs 1)))))

(provide 'magit-dash-pr-status)

;;; magit-dash-pr-status.el ends here
