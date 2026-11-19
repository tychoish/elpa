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

(defvar magit-dash-pr-status-page-size 20
  "Number of PRs requested per GraphQL page.
Each PR nests up to 100 reviews, checks and threads, so large pages
exceed GitHub's gateway timeout (502) deterministically; keep this small.")

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
        reviews(last:100){ pageInfo{ hasPreviousPage } nodes{ state author{ login __typename } } }
        reviewRequests(first:50){
          pageInfo{ hasNextPage }
          nodes{ requestedReviewer{ ... on User{ login } ... on Team{ name } } }
        }
        commits(last:1){
          nodes{ commit{ statusCheckRollup{
            contexts(first:100){
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
}" magit-dash-pr-thread-limit))

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

(defun magit-dash-pr-status--validate-response (out)
  "Parse and validate GraphQL response OUT.
Signal `user-error' on GraphQL error or incomplete data; return parsed response."
  (let ((response (magit-dash-pr-status--parse-json out)))
    (when-let* ((errors (map-elt response 'errors)))
      (user-error "GitHub GraphQL error: %s"
                  (mapconcat (lambda (e) (or (map-elt e 'message) "unknown"))
                             errors "; ")))
    (unless (and (map-nested-elt response '(data search))
                 (assq 'nodes (map-nested-elt response '(data search))))
      (user-error "GitHub GraphQL response is missing search results"))
    (dolist (node (map-nested-elt response '(data search nodes)))
      (when (or (eq t (map-nested-elt node '(reviews pageInfo hasPreviousPage)))
                (eq t (map-nested-elt node '(reviewRequests pageInfo hasNextPage)))
                (eq t (map-nested-elt node '(reviewThreads pageInfo hasNextPage)))
                (eq t (map-nested-elt
                       (car (map-nested-elt node '(commits nodes)))
                       '(commit statusCheckRollup contexts pageInfo hasNextPage))))
        (user-error "GitHub returned incomplete data for PR #%s" (map-elt node 'number))))
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

(defun magit-dash-pr-status--fetch-page-async (q dir first after on-success on-error &optional attempt delay)
  "Asynchronously fetch one page of FIRST PRs matching Q from DIR starting after cursor AFTER.
Call ON-SUCCESS with validated parsed response.
Call ON-ERROR with error message string on fatal failure.
Transient 5xx failures are retried up to `magit-dash-pr-status-retry-attempts' times."
  (let* ((att (or attempt 1))
         (del (or delay magit-dash-pr-status-retry-delay))
         (args (list "api" "graphql"
                     "-f" (concat "query=" (magit-dash-pr-status--query))
                     "-f" (concat "q=" q)
                     "-F" (format "first=%d" first)
                     (if after "-f" "-F") (if after (concat "after=" after) "after=null"))))
    (magit-dash-gh--run-process
     args dir
     (lambda (out)
       (condition-case err
           (funcall on-success (magit-dash-pr-status--validate-response out))
         (error (funcall on-error (error-message-string err)))))
     (lambda (out code)
       (if (and (< att magit-dash-pr-status-retry-attempts)
                (magit-dash-pr-status--transient-failure-p (cons code out)))
           (progn
             (message "magit-dash: gh api graphql failed transiently (%s), retrying in %ss" code del)
             (run-at-time del nil
                          #'magit-dash-pr-status--fetch-page-async
                          q dir first after on-success on-error
                          (1+ att) (* 2 del)))
         (funcall on-error
                  (format "gh api graphql failed (%s): %s" code (string-trim out))))))))

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

(defun magit-dash-pr-status--fetch-search-async (q dir on-success on-error)
  "Run the PR query for search string Q from DIR asynchronously.
Calls ON-SUCCESS with the combined parsed response or ON-ERROR with an error string."
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
           on-error)))
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
          :ticket (magit-dash-pr-status--ticket (map-elt node 'headRefName)))))

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

(defun magit-dash-pr-status-collect-all (&optional dir)
  "Return PR plists for the author's open PRs across all repositories.
Run from DIR (default `default-directory').  One search query without a
repo qualifier is used; each PR's `:repo' is its owner/name slug and its
`:bucket' uses the approvals required for that repository, falling back
to `magit-dash-pr-status-required-approvals'.  The result is pure
data.  Signal `user-error' when the query fails; the tracking cache is
never touched."
  (magit-dash-pr-status--parse
   (magit-dash-pr-status--fetch-search
    (format "is:pr is:open%s author:%s"
            (if magit-dash-pr-status-include-archived "" " archived:false")
            magit-dash-pr-status-author)
    (or dir default-directory))
   (lambda (pr)
     (magit-dash-pr-status--required-approvals (plist-get pr :repo)))))

(defun magit-dash-pr-status-collect-all-async (on-success &optional on-error dir)
  "Asynchronously collect PR plists for open PRs across all repositories.
Run from DIR (default `default-directory').
ON-SUCCESS is called with the list of PR plists on success.
ON-ERROR is called with an error message string on failure;
if nil, `message' is called."
  (let ((errback (or on-error (lambda (err) (message "magit-dash: PR collection failed: %s" err)))))
    (magit-dash-pr-status--fetch-search-async
     (format "is:pr is:open%s author:%s"
             (if magit-dash-pr-status-include-archived "" " archived:false")
             magit-dash-pr-status-author)
     (or dir default-directory)
     (lambda (response)
       (condition-case err
           (let ((prs (magit-dash-pr-status--parse
                       response
                       (lambda (pr)
                         (magit-dash-pr-status--required-approvals (plist-get pr :repo))))))
             (funcall on-success prs))
         (error (funcall errback (error-message-string err)))))
     errback)))

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
