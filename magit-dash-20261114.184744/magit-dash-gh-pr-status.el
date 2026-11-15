;;; magit-dash-gh-pr-status.el --- Next-step status for your open PRs -*- lexical-binding: t -*-

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
;; in `magit-dash-gh-pr'.
;;
;; Entry points:
;;   M-x magit-dash-gh-pr-status
;;   emacs --batch -l magit-dash -f magit-dash-gh-pr-status-batch [REPO]
;;   MCP tool `magit_dash_pr_status' (see `magit-dash-mcp')

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'magit-dash-gh)
(require 'magit-dash-gh-pr)


;;;; Customization

(defgroup magit-dash-gh-pr-status nil
  "Next-step status for the author's open pull requests."
  :group 'magit-dash
  :prefix "magit-dash-gh-pr-status-")

(defcustom magit-dash-gh-pr-status-required-approvals 1
  "Approvals needed before a PR is classified as ready to merge.
Overridden per repository by `:required-approvals' in the `:pr' plist
of `magit-dash-register'."
  :type 'natnum)

(defcustom magit-dash-gh-pr-status-author "@me"
  "GitHub login whose open PRs are collected; `@me' is the authenticated user."
  :type 'string)

(defcustom magit-dash-gh-pr-status-ignored-reviewers '("claude")
  "Reviewer logins whose reviews never count as approvals.
Reviewers that GitHub reports as bots are always ignored."
  :type '(repeat string))

(defvar magit-dash-gh-pr-status-max-prs 50
  "Number of open PRs requested; there is no pagination beyond this.
GitHub caps the `first' argument of a GraphQL connection at 100.
A response with more results signals an error.")

;;;; Small helpers

(defun magit-dash-gh-pr-status--plural (n)
  "Return the plural suffix for a count of N."
  (if (= n 1) "" "s"))

(defun magit-dash-gh-pr-status--one-line (text)
  "Return TEXT with line breaks collapsed to spaces."
  (replace-regexp-in-string "[\n\r]+" " " (or text "")))

;;;; Collection

(defun magit-dash-gh-pr-status--query ()
  "Return the GraphQL query that fetches every open PR matching a search."
  (format "query($q:String! $first:Int!){
  search(query:$q type:ISSUE first:$first){
    pageInfo{ hasNextPage }
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
}" magit-dash-gh-pr-thread-limit))

(defconst magit-dash-gh-pr-status--slug-re
  "\\`[A-Za-z0-9][A-Za-z0-9-]*/\\(?:[A-Za-z0-9_-]\\|\\.[A-Za-z0-9_-]\\|[A-Za-z0-9_-]\\.\\)[A-Za-z0-9_.-]*\\'"
  "Match an owner/name repository slug.
The owner starts with an alphanumeric; the name may not be only dots.")

(defun magit-dash-gh-pr-status--check-slug (slug)
  "Signal `user-error' unless SLUG looks like owner/name."
  (unless (and (stringp slug)
               (string-match-p magit-dash-gh-pr-status--slug-re slug))
    (user-error "Not a GitHub owner/name repository slug: %S" slug)))

(defun magit-dash-gh-pr-status--parse-json (out)
  "Parse OUT as JSON into alists and lists; signal `user-error' if malformed."
  (let ((parsed (condition-case nil
                    (json-parse-string out :object-type 'alist
                                       :array-type 'list :null-object nil)
                  ((json-parse-error json-end-of-file) 'invalid))))
    (when (or (eq parsed 'invalid) (not (listp parsed)))
      (user-error "gh api graphql returned non-JSON output"))
    parsed))

(defun magit-dash-gh-pr-status--fetch (slug dir)
  "Run the PR query for repository SLUG from DIR; return the parsed response.
Signal `user-error' when SLUG is malformed or `gh' fails or returns
something unparseable."
  (magit-dash-gh-pr-status--check-slug slug)
  (magit-dash-gh-pr-status--fetch-search
   (format "repo:%s is:pr is:open author:%s"
           slug magit-dash-gh-pr-status-author)
   dir))

(defun magit-dash-gh-pr-status--fetch-search (q dir)
  "Run the PR query for search string Q from DIR; return the parsed response.
Signal `user-error' when `gh' fails (including process or network
failure) or returns something unparseable.  Touches no cache."
  (pcase-let* ((`(,code . ,out)
                (condition-case err
                    (magit-dash-gh--call-process
                     (list "api" "graphql"
                           "-f" (concat "query=" (magit-dash-gh-pr-status--query))
                           "-f" (concat "q=" q)
                           "-F" (format "first=%d" magit-dash-gh-pr-status-max-prs))
                     dir)
                  (file-error
                   (user-error "gh api graphql could not run: %s"
                               (error-message-string err))))))
    (unless (and (integerp code) (zerop code))
      (user-error "gh api graphql failed (%s): %s" code (string-trim out)))
    (let ((response (magit-dash-gh-pr-status--parse-json out)))
      (when-let* ((errors (map-elt response 'errors)))
        (user-error "GitHub GraphQL error: %s"
                    (mapconcat (lambda (e) (or (map-elt e 'message) "unknown"))
                               errors "; ")))
      (unless (and (map-nested-elt response '(data search))
                   (assq 'nodes (map-nested-elt response '(data search))))
        (user-error "GitHub GraphQL response is missing search results"))
      (when (eq t (map-nested-elt response '(data search pageInfo hasNextPage)))
        (user-error "GitHub returned more PRs than the query limit"))
      (dolist (node (map-nested-elt response '(data search nodes)))
        (when (or (eq t (map-nested-elt node '(reviews pageInfo hasPreviousPage)))
                  (eq t (map-nested-elt node '(reviewRequests pageInfo hasNextPage)))
                  (eq t (map-nested-elt node '(reviewThreads pageInfo hasNextPage)))
                  (eq t (map-nested-elt
                         (car (map-nested-elt node '(commits nodes)))
                         '(commit statusCheckRollup contexts pageInfo hasNextPage))))
          (user-error "GitHub returned incomplete data for PR #%s" (map-elt node 'number))))
      response)))

(defun magit-dash-gh-pr-status--bot-p (author)
  "Return non-nil when review AUTHOR (a GraphQL actor alist) is ignorable."
  (let ((login (or (map-elt author 'login) "")))
    (or (equal (map-elt author '__typename) "Bot")
        (string-suffix-p "[bot]" login)
        (member login magit-dash-gh-pr-status-ignored-reviewers))))

(defun magit-dash-gh-pr-status--latest-states (reviews)
  "Return the latest decisive review state of each human in REVIEWS.
REVIEWS is a chronological list of review nodes.  Anything but COMMENTED
or PENDING is decisive, matching GitHub: a later comment does not retract
an approval."
  (let ((latest (make-hash-table :test #'equal)))
    (dolist (review reviews)
      (let ((author (map-elt review 'author))
            (state (map-elt review 'state)))
        (unless (or (null author)
                    (magit-dash-gh-pr-status--bot-p author)
                    (member state '("COMMENTED" "PENDING")))
          (puthash (map-elt author 'login) state latest))))
    (hash-table-values latest)))

(defun magit-dash-gh-pr-status--approvals (reviews)
  "Count humans in REVIEWS whose latest decisive review is an approval."
  (seq-count (lambda (state) (equal state "APPROVED"))
             (magit-dash-gh-pr-status--latest-states reviews)))

(defun magit-dash-gh-pr-status--changes-requested (reviews)
  "Count humans in REVIEWS whose latest decisive review requests changes."
  (seq-count (lambda (state) (equal state "CHANGES_REQUESTED"))
             (magit-dash-gh-pr-status--latest-states reviews)))

(defun magit-dash-gh-pr-status--context-failing-p (ctx)
  "Return non-nil when the status-check context CTX is failing."
  ;; Reuse the rollup parser on a one-element list so that what counts as a
  ;; failure stays defined in exactly one place.
  (equal (magit-dash-gh-pr--parse-status-rollup (list ctx)) "FAILURE"))

(defun magit-dash-gh-pr-status--failing (contexts)
  "Return (NAME . URL) for each failing check in CONTEXTS."
  (mapcar (lambda (ctx)
            (cons (or (map-elt ctx 'name) (map-elt ctx 'context) "unknown")
                  (or (map-elt ctx 'detailsUrl) (map-elt ctx 'targetUrl))))
          (seq-filter #'magit-dash-gh-pr-status--context-failing-p contexts)))

(defun magit-dash-gh-pr-status--ticket (branch)
  "Return the ticket id such as FND-1269 embedded in BRANCH, or nil."
  (when (and branch (string-match "\\b\\([A-Za-z]+-[0-9]+\\)" branch))
    (upcase (match-string 1 branch))))

(defun magit-dash-gh-pr-status--node-to-pr (node)
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
          :approvals (magit-dash-gh-pr-status--approvals reviews)
          :changes-requested (magit-dash-gh-pr-status--changes-requested reviews)
          :requested (delq nil
                           (mapcar (lambda (rr)
                                     (let ((r (map-elt rr 'requestedReviewer)))
                                       (or (map-elt r 'login) (map-elt r 'name))))
                                   (map-nested-elt node '(reviewRequests nodes))))
          :ci (magit-dash-gh-pr--parse-status-rollup contexts)
          :failing (magit-dash-gh-pr-status--failing contexts)
          :threads-open (seq-count (lambda (th) (not (eq (map-elt th 'isOutdated) t)))
                                   unresolved)
          :threads-outdated (seq-count (lambda (th) (eq (map-elt th 'isOutdated) t))
                                       unresolved)
          :ticket (magit-dash-gh-pr-status--ticket (map-elt node 'headRefName)))))

(defun magit-dash-gh-pr-status--stack-parent (pr prs)
  "Return the number of the PR in PRS whose head branch is PR's base, or nil.
When several PRs share that head branch the first one in PRS wins."
  (when-let* ((base (plist-get pr :base))
              (parent (seq-find (lambda (other)
                                  (and (not (eq other pr))
                                       (equal (plist-get other :branch) base)))
                                prs)))
    (plist-get parent :number)))

(defun magit-dash-gh-pr-status--annotate-stacks (prs)
  "Return PRS with `:stack-parent' set on each stacked PR."
  (mapcar (lambda (pr)
            (plist-put (copy-sequence pr) :stack-parent
                       (magit-dash-gh-pr-status--stack-parent pr prs)))
          prs))

(defun magit-dash-gh-pr-status--annotate-buckets (prs required)
  "Return PRS with `:bucket' set on each PR, given REQUIRED approvals.
REQUIRED is a count, or a function called with the PR plist that returns
the count.  Also set `:required' to that count."
  (mapcar (lambda (pr)
            (let* ((count (if (functionp required) (funcall required pr) required))
                   (new (plist-put (copy-sequence pr) :bucket
                                   (magit-dash-gh-pr-status--bucket pr count))))
              (plist-put new :required count)))
          prs))

(defun magit-dash-gh-pr-status--parse (response required)
  "Return PR plists, sorted by number, parsed from GraphQL RESPONSE.
REQUIRED is the approval count used to bucket each PR."
  (let* ((nodes (map-nested-elt response '(data search nodes)))
         (prs (mapcar #'magit-dash-gh-pr-status--node-to-pr
                      (seq-filter (lambda (n) (map-elt n 'number)) nodes))))
    (magit-dash-gh-pr-status--annotate-buckets
     (magit-dash-gh-pr-status--annotate-stacks
      (sort prs (lambda (a b) (< (plist-get a :number) (plist-get b :number)))))
     required)))

(defun magit-dash-gh-pr-status--collect (slug dir required)
  "Return PR plists for the author's open PRs in repository SLUG, run from DIR.
REQUIRED is the approval count used to bucket each PR."
  (magit-dash-gh-pr-status--parse
   (magit-dash-gh-pr-status--fetch slug dir) required))

;;;; Classification

(defun magit-dash-gh-pr-status--required-approvals (slug &optional dir)
  "Return the approvals required for repository SLUG.
Fall back to `magit-dash-gh-pr-status-required-approvals' when the
registered value is missing or not a natural number.  DIR identifies the checkout."
  (let ((value (magit-dash-gh-pr-repo-option
                slug :required-approvals
                magit-dash-gh-pr-status-required-approvals dir)))
    (if (natnump value) value magit-dash-gh-pr-status-required-approvals)))

(defun magit-dash-gh-pr-status--bucket (pr required)
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

(defun magit-dash-gh-pr-status-collect (slug dir)
  "Return PR plists for the author's open PRs in repository SLUG, run from DIR.
Each PR carries `:bucket' computed with the approvals required for SLUG.
The result is pure data: nothing is cached or written.  Signal
`user-error' when SLUG is malformed or the query fails; the tracking
cache is never touched."
  (magit-dash-gh-pr-status--check-slug slug)
  (let ((prs (magit-dash-gh-pr-status--collect
              slug dir (magit-dash-gh-pr-status--required-approvals slug dir))))
    (mapcar (lambda (pr)
              (if (plist-get pr :repo) pr (plist-put (copy-sequence pr) :repo slug)))
            prs)))

(defun magit-dash-gh-pr-status-collect-all (&optional dir)
  "Return PR plists for the author's open PRs across all repositories.
Run from DIR (default `default-directory').  One search query without a
repo qualifier is used; each PR's `:repo' is its owner/name slug and its
`:bucket' uses the approvals required for that repository, falling back
to `magit-dash-gh-pr-status-required-approvals'.  The result is pure
data.  Signal `user-error' when the query fails; the tracking cache is
never touched."
  (magit-dash-gh-pr-status--parse
   (magit-dash-gh-pr-status--fetch-search
    (format "is:pr is:open author:%s" magit-dash-gh-pr-status-author)
    (or dir default-directory))
   (lambda (pr)
     (magit-dash-gh-pr-status--required-approvals (plist-get pr :repo)))))

(defun magit-dash-gh-pr-status-run (slug dir)
  "Collect PRs for repository SLUG from DIR; return a data plist.
Return a plist with `:prs' and `:required'.  Touches no files."
  (list :prs (magit-dash-gh-pr-status-collect slug dir)
        :required (magit-dash-gh-pr-status--required-approvals slug dir)))

(defun magit-dash-gh-pr-status--current ()
  "Return (SLUG . DIR) for the repository at point."
  (let* ((dir (magit-dash-gh--repo-dir))
         (info (let ((default-directory dir)) (magit-dash-gh--repo-info))))
    (unless (and (plist-get info :owner) (plist-get info :repo))
      (user-error "Cannot determine the GitHub repository for %s" dir))
    (cons (format "%s/%s" (plist-get info :owner) (plist-get info :repo)) dir)))

(defun magit-dash-gh-pr-status--line (pr)
  "Return a one-line summary of PR."
  (format "%s#%s %s [%s]"
          (or (plist-get pr :repo) "-") (plist-get pr :number)
          (magit-dash-gh-pr-status--one-line (plist-get pr :title))
          (plist-get pr :bucket)))

;;;###autoload
(defun magit-dash-gh-pr-status ()
  "Show the next-step status of your open PRs in the current repository.
Return the data plist from `magit-dash-gh-pr-status-run'."
  (interactive)
  (pcase-let* ((`(,slug . ,dir) (magit-dash-gh-pr-status--current))
               (result (magit-dash-gh-pr-status-run slug dir))
               (prs (plist-get result :prs)))
    (message "magit-dash: %d open PR%s in %s%s"
             (length prs) (magit-dash-gh-pr-status--plural (length prs)) slug
             (if prs
                 (concat "\n" (mapconcat #'magit-dash-gh-pr-status--line prs "\n"))
               ""))
    result))

;;;###autoload
(defun magit-dash-gh-pr-status-batch ()
  "Print one line per open PR from `emacs --batch'; exit non-zero on failure.
An optional command-line argument names the repository directory."
  (let ((default-directory (file-name-as-directory
                            (expand-file-name (or (pop command-line-args-left)
                                                  default-directory)))))
    (condition-case err
        (let ((result (pcase-let ((`(,slug . ,dir) (magit-dash-gh-pr-status--current)))
                        (magit-dash-gh-pr-status-run slug dir))))
          (dolist (pr (plist-get result :prs))
            (princ (concat (magit-dash-gh-pr-status--line pr) "\n"))))
      (error
       (message "magit-dash-gh-pr-status: %s" (error-message-string err))
       (kill-emacs 1)))))

(provide 'magit-dash-gh-pr-status)

;;; magit-dash-gh-pr-status.el ends here
