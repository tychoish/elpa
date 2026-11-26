;;; magit-dash-pr.el --- Fetch GitHub PR comments and review threads to disk -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1.0
;; Keywords: vc, tools, magit, github
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides `magit-dash-pr-fetch', which downloads PR review threads and
;; issue comments for the current branch's pull request to a local directory
;; under plans/.  Writes an index file that records the PR state, which
;; threads are unresolved, who created each thread, and who last commented.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)
(require 'savehist)

(require 'magit-dash-gh)
(require 'magit-dash-pr-cache)
(require 'magit-dash-worktree)
(require 'magit-dash-review-json)

(declare-function magit-dash-chain-plan "magit-dash-chain")
(declare-function magit-dash-worktree-clean-plan "magit-dash-worktree-clean")
(declare-function magit-dash-worktree-clean-plan-targets "magit-dash-worktree-clean")
(declare-function magit-dash-worktree-clean-entry-target "magit-dash-worktree-clean")
(declare-function magit-dash-chain-resume "magit-dash-chain")
(declare-function magit-dash-chain-open "magit-dash-chain")

(declare-function magit-dash-pr-status-collect-all-async "magit-dash-pr-status")
(declare-function magit-dash-pr-status-fetch-one "magit-dash-pr-status")
(declare-function magit-toplevel "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-call-git "magit-process")
(declare-function magit-status-setup-buffer "magit-status")
(declare-function magit-dash--format-age "magit-dash")
(declare-function magit-dash-repo-p "magit-dash")
(declare-function magit-dash-repo-path "magit-dash")
(declare-function magit-dash-repo-name "magit-dash")
(declare-function magit-dash-repo-repo "magit-dash")
(declare-function magit-dash-repo-upstream-repo "magit-dash")
(declare-function magit-dash-repo-pr "magit-dash")
(declare-function magit-dash-repo-path "magit-dash")
(defvar magit-dash-repo-list)
(declare-function magit-dash--repo-at-point "magit-dash")
(declare-function magit-dash--repo-at-point-p "magit-dash")
(declare-function magit-dash-worktree-uncommitted-changes-p "magit-dash-worktree")
(declare-function magit-dash-worktree-symlink-worktree "magit-dash-worktree")
(declare-function magit-dash-worktree-main-dir "magit-dash-worktree")
(declare-function magit-dash-gh-actions-fetch-for-pr "magit-dash-gh-actions")
(declare-function magit-dash-review-file "magit-dash-review" (root pr-number &optional repo))
(declare-function magit-dash-review-pr "magit-dash-review" (&optional pr-number-or-url repo-dir))
(declare-function code-review-start "code-review" (url))

(defun magit-dash-pr-code-review-available-p ()
  "Return non-nil when `code-review' is installed and available."
  (or (fboundp 'code-review-start)
      (and (locate-library "code-review")
           (ignore-errors (require 'code-review nil t)))))
;;; Configuration

(defvar magit-dash-pr-thread-limit 100
  "Maximum number of review threads to fetch per pull request.")

(defconst magit-dash-pr-known-options '(:required-approvals)
  "Keys accepted in the `:pr' plist of `magit-dash-register'.
:required-approvals  approvals needed before a PR is ready to merge.")

(defun magit-dash-pr-check-options (name opts)
  "Warn about problems in OPTS, the `:pr' plist registered for repo NAME.
Unknown keys and a `:required-approvals' that is not a natural number are
reported; consumers fall back to their defaults for the latter."
  (cl-loop for (k _) on opts by #'cddr
           unless (memq k magit-dash-pr-known-options)
           do (display-warning 'magit-dash
                               (format "Unknown :pr option %S for %s" k name)))
  (when (and (plist-member opts :required-approvals)
             (not (natnump (plist-get opts :required-approvals))))
    (display-warning
     'magit-dash
     (format ":pr :required-approvals for %s must be a natural number; using the default"
             name))))

(defun magit-dash-pr-repo-option (slug key default &optional dir)
  "Return option KEY from the `:pr' plist of the repo registered as SLUG.
SLUG is an owner/name string.  Return DEFAULT when the repo is not
registered or does not set KEY.  DIR identifies the registered checkout."
  (let* ((repo (seq-find (lambda (r)
                           (or (equal (magit-dash-repo-repo r) slug)
                               (and dir (magit-dash-repo-path r)
                                    (equal (file-truename dir)
                                           (file-truename (magit-dash-repo-path r))))))
                         magit-dash-repo-list))
         (opts (and repo (magit-dash-repo-pr repo))))
    (if (plist-member opts key) (plist-get opts key) default)))

(defvar magit-dash-pr--pending-worktrees (make-hash-table :test #'equal)
  "PR worktrees currently being created, keyed by (REPO . NUMBER).")

;;; Internal helpers

(defun magit-dash-pr--parse-threads (graphql-response)
  "Extract thread summaries from GRAPHQL-RESPONSE alist.
Returns a list of plists :id :resolved :path :creator :last-commentor.
:resolved is t when the thread is resolved, nil otherwise."
  (when-let* ((data      (map-elt graphql-response 'data))
              (repo      (map-elt data 'repository))
              (pr        (map-elt repo 'pullRequest))
              (rt        (map-elt pr 'reviewThreads))
              (nodes     (map-elt rt 'nodes)))
    (seq-map
     (lambda (thread)
       (let* ((comments     (map-elt (map-elt thread 'comments) 'nodes))
              (first-comment (car comments))
              (last-comment  (car (last comments))))
         (list :id             (or (map-elt thread 'id) "")
               :resolved       (eq (map-elt thread 'isResolved) t)
               :path           (or (map-elt thread 'path) "")
               :creator        (or (map-elt (map-elt first-comment 'author) 'login) "")
               :last-commentor (or (map-elt (map-elt last-comment  'author) 'login) ""))))
     nodes)))

(defun magit-dash-pr--thread-table (thread)
  "Convert THREAD plist to a string-keyed hash-table for serialization."
  (let ((ht (make-hash-table :test #'equal)))
    (puthash "id"             (or (plist-get thread :id) "")                  ht)
    (puthash "resolved"       (if (plist-get thread :resolved) t :false)      ht)
    (puthash "path"           (or (plist-get thread :path) "")                ht)
    (puthash "creator"        (or (plist-get thread :creator) "")             ht)
    (puthash "last_commentor" (or (plist-get thread :last-commentor) "")      ht)
    ht))

(defun magit-dash-pr--truncate-text (text max-chars)
  "Truncate TEXT to MAX-CHARS characters, appending \"...\" if truncated."
  (if (and text max-chars (> (length text) max-chars))
      (concat (substring text 0 (max 0 (- max-chars 3))) "...")
    (or text "")))

(defun magit-dash-pr-parse-unresolved-threads (graphql-response &optional limit max-chars include-resolved)
  "Extract review thread summaries from GRAPHQL-RESPONSE alist.
Returns a list of plists with keys :id :path :line :outdated :author :body :comments.
When INCLUDE-RESOLVED is non-nil, includes resolved threads as well.
Caps thread count at LIMIT (default 20, max 50) and each comment's body at
MAX-CHARS (default 1000)."
  (let* ((limit (min (or limit 20) 50))
         (max-chars (or max-chars 1000))
         (data (map-elt graphql-response 'data))
         (repo (map-elt data 'repository))
         (pr (map-elt repo 'pullRequest))
         (rt (map-elt pr 'reviewThreads))
         (nodes (map-elt rt 'nodes))
         (filtered (if include-resolved
                       nodes
                     (seq-filter (lambda (th) (not (eq (map-elt th 'isResolved) t)))
                                 nodes)))
         (capped-nodes (seq-take filtered limit)))
    (seq-map
     (lambda (thread)
       (let* ((comments-nodes (map-nested-elt thread '(comments nodes)))
              (first-comment (car comments-nodes))
              (creator (or (map-nested-elt first-comment '(author login)) "unknown"))
              (outdated (eq (map-elt thread 'isOutdated) t))
              (resolved (eq (map-elt thread 'isResolved) t))
              (comments (mapcar
                         (lambda (c)
                           (list :id (or (map-elt c 'id) "")
                                 :author (or (map-nested-elt c '(author login)) "unknown")
                                 :body (magit-dash-pr--truncate-text
                                        (map-elt c 'body) max-chars)
                                 :created_at (or (map-elt c 'createdAt) "")
                                 :database_id (map-elt c 'databaseId)
                                 :commit_oid (map-nested-elt c '(commit oid))))
                         comments-nodes))
              (body (magit-dash-pr--truncate-text
                     (map-elt first-comment 'body) max-chars)))
         (list :id (or (map-elt thread 'id) "")
               :path (or (map-elt thread 'path) "")
               :line (map-elt thread 'line)
               :start_line (map-elt thread 'startLine)
               :diff_side (map-elt thread 'diffSide)
               :original_line (map-elt thread 'originalLine)
               :original_start_line (map-elt thread 'originalStartLine)
               :resolved (if resolved t :false)
               :outdated (if outdated t :false)
               :author creator
               :body body
               :comments (vconcat comments))))
     capped-nodes)))

(defun magit-dash-pr-fetch-threads-graphql (slug number &optional limit max-chars dir include-resolved)
  "Fetch review threads for PR NUMBER in SLUG synchronously.
SLUG is an \"owner/repo\" string.  DIR is optional working directory.
When INCLUDE-RESOLVED is non-nil, includes resolved threads.
Returns a list of plists."
  (let* ((parts (split-string slug "/"))
         (owner (car parts))
         (repo (cadr parts))
         (first-count (min (max (or limit 20) 1) 50))
         (query "query($owner:String! $repo:String! $number:Int! $first:Int!){
  repository(owner:$owner name:$repo){
    pullRequest(number:$number){
      reviewThreads(first:$first){
        nodes{
          id isResolved isOutdated path line startLine diffSide originalLine originalStartLine
          comments(first:50){
            nodes{ id databaseId author{login} body createdAt commit{oid} }
          }
        }
      }
    }
  }
}"))
    (unless (and owner repo)
      (error "Invalid repository slug: %s" slug))
    (pcase-let* ((`(,code . ,output)
                  (magit-dash-gh--call-process
                   (list "api" "graphql"
                         "-f" (concat "query=" query)
                         "-f" (concat "owner=" owner)
                         "-f" (concat "repo=" repo)
                         "-F" (format "number=%d" number)
                         "-F" (format "first=%d" first-count))
                   (or dir default-directory))))
      (unless (eq code 0)
        (error "Failed to fetch review threads for %s#%d: %s" slug number output))
      (let ((parsed (condition-case err
                        (json-parse-string output :array-type 'list :object-type 'alist)
                      (error (error "Failed to parse GraphQL response: %s"
                                    (error-message-string err))))))
        (magit-dash-pr-parse-unresolved-threads parsed limit max-chars include-resolved)))))

(defun magit-dash-pr--graphql-query ()
  "Return the GraphQL query string for fetching review threads."
  (format "query($owner:String! $repo:String! $number:Int!){
  repository(owner:$owner name:$repo){
    pullRequest(number:$number){
      reviewThreads(first:%d){
        nodes{
          id isResolved path
          comments(first:100){
            nodes{ author{login} body createdAt }
          }
        }
      }
    }
  }
}" magit-dash-pr-thread-limit))

;;; Pipeline steps

(defun magit-dash-pr--step-finalize (ctx)
  "Write the index file and report completion to the user."
  (let* ((dir      (plist-get ctx :dir))
         (pr-info  (plist-get ctx :pr-info))
         (threads  (plist-get ctx :threads))
         (files    (plist-get ctx :files))
         (author   (map-elt (map-elt pr-info 'author) 'login))
         (has-unresolved (seq-some (lambda (th) (not (plist-get th :resolved)))
                                   threads))
         (data (magit-dash-gh--index-table
                "collected_at"           (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                "type"                   "pr"
                "pr_number"              (or (map-elt pr-info 'number) 0)
                "title"                  (or (map-elt pr-info 'title) "")
                "branch"                 (or (map-elt pr-info 'headRefName) "")
                "state"                  (or (map-elt pr-info 'state) "")
                "author"                 (or author "")
                "review_decision"        (or (map-elt pr-info 'reviewDecision) "")
                "has_unresolved_threads" (if has-unresolved t :false)
                "artifact_count"         (length files)
                "files"                  (apply #'vector
                                                (seq-map (lambda (f)
                                                           (magit-dash-gh--file-table
                                                            (plist-get f :path)
                                                            (plist-get f :type)))
                                                         files))
                "threads"                (apply #'vector
                                                (seq-map #'magit-dash-pr--thread-table
                                                         threads)))))
    (magit-dash-gh--write-index dir data)
    (message "magit-gh-pr: done — %d file(s) in %s" (length files) dir)))

(defun magit-dash-pr--step-comments (ctx)
  "Fetch issue-style comments on the PR, write pr-issue-comments.json."
  (let* ((owner    (plist-get ctx :owner))
         (repo     (plist-get ctx :repo))
         (pr-num   (plist-get ctx :pr-number))
         (repo-dir (plist-get ctx :repo-dir))
         (endpoint (format "/repos/%s/%s/issues/%s/comments" owner repo pr-num)))
    (message "magit-gh-pr: fetching issue comments...")
    (magit-dash-gh--run-process
     (list "api" endpoint "--paginate")
     repo-dir
     (lambda (output)
       (let ((file "pr-issue-comments.json"))
         (with-temp-file (expand-file-name file (plist-get ctx :dir))
           (insert output))
         (magit-dash-pr--step-finalize
          (magit-dash-gh--add-file ctx file "issue-comments"))))
     (magit-dash-gh--make-error-handler "magit-gh-pr" "issue-comments"))))

(defun magit-dash-pr--step-threads (ctx)
  "Fetch review threads via GraphQL, write pr-review-threads.json."
  (let* ((owner    (plist-get ctx :owner))
         (repo     (plist-get ctx :repo))
         (pr-num   (plist-get ctx :pr-number))
         (repo-dir (plist-get ctx :repo-dir)))
    (message "magit-gh-pr: fetching review threads...")
    (magit-dash-gh--run-process
     (list "api" "graphql"
           "-f" (format "query=%s" (magit-dash-pr--graphql-query))
           "-f" (format "owner=%s" owner)
           "-f" (format "repo=%s" repo)
           "-F" (format "number=%d" (string-to-number (format "%s" pr-num))))
     repo-dir
     (lambda (output)
       (let* ((file    "pr-review-threads.json")
              (parsed  (json-parse-string output
                                          :array-type  'list
                                          :object-type 'alist))
              (threads (or (magit-dash-pr--parse-threads parsed) nil)))
         (with-temp-file (expand-file-name file (plist-get ctx :dir))
           (insert output))
         (magit-dash-pr--step-comments
          (magit-dash-gh--add-file
           (plist-put ctx :threads threads)
           file "review-threads"))))
     (magit-dash-gh--make-error-handler "magit-gh-pr" "review-threads"))))

(defun magit-dash-pr--step-info (ctx)
  "Fetch PR metadata, write pr-info.json, then continue to threads."
  (let* ((pr-arg   (plist-get ctx :pr-arg))
         (repo-dir (plist-get ctx :repo-dir))
         (args     (append
                    '("pr" "view")
                    (when pr-arg (list (number-to-string pr-arg)))
                    '("--json"
                      "number,title,state,author,body,createdAt,updatedAt,reviewDecision,headRefName,baseRefName"))))
    (message "magit-gh-pr: fetching PR info...")
    (magit-dash-gh--run-process
     args
     repo-dir
     (lambda (output)
       (let* ((pr-info  (json-parse-string output :object-type 'alist))
              (pr-num   (map-elt pr-info 'number))
              (branch   (or (map-elt pr-info 'headRefName)
                            (plist-get ctx :branch)))
              (slug     (magit-dash-gh--branch-slug branch))
              (dir      (magit-dash-gh--collect-dir
                         (plist-get ctx :root) 'pr slug pr-num))
              (file     "pr-info.json")
              (ctx2     (copy-sequence ctx))
              (ctx2     (plist-put ctx2 :pr-info pr-info))
              (ctx2     (plist-put ctx2 :pr-number pr-num))
              (ctx2     (plist-put ctx2 :dir dir)))
         (with-temp-file (expand-file-name file dir)
           (insert output))
         (magit-dash-pr--step-threads
          (magit-dash-gh--add-file ctx2 file "metadata"))))
     (magit-dash-gh--make-error-handler "magit-gh-pr" "pr-info"))))

;;; Public API

;;;###autoload
(defun magit-dash-pr-fetch (&optional pr-number)
  "Download review threads for PR-NUMBER (or current branch's PR).
When PR-NUMBER is nil, auto-detects the PR for the current branch.

Creates an artifact directory under plans/ containing:
  pr-info.json            — PR metadata and review decision
  pr-review-threads.json  — all review threads with resolved status
  pr-issue-comments.json  — top-level (non-review) comments
  index.json              — collection summary with per-thread
                            creator/last-commentor"
  (interactive)
  (magit-dash-gh--check-gh)
  (let* ((repo-dir  (magit-dash-gh--repo-dir))
         (root      (or (magit-toplevel)
                        (user-error "Not inside a git repository")))
         (branch    (magit-get-current-branch))
         (repo-info (magit-dash-gh--repo-info))
         (ctx       (list :branch   branch
                          :root     root
                          :repo-dir repo-dir
                          :owner    (plist-get repo-info :owner)
                          :repo     (plist-get repo-info :repo)
                          :pr-arg   pr-number
                          :files    nil
                          :threads  nil)))
    (magit-dash-pr--step-info ctx)))

;;;; PR dashboard mode

(defcustom magit-dash-pr-worktree-path-format 'dot-wt
  "Format or strategy for PR worktree checkout paths.
Can be:
- `dot-wt' (default): creates `<repo-root>/.wt/<name>'
- `sibling': creates `<parent>/<repo-base>-<name>'
- A string format where `%r' is the repo root, `%b' is the repo base name,
  and `%n' is the worktree name (e.g. \"pr-42\").
- A function called with (REPO-DIR NAME ENTRY) returning an absolute
  path string."
  :type '(choice (const :tag "Sibling directory (<repo>-<name>)" sibling)
                 (const :tag "Subdirectory under .wt/ (<repo>/.wt/<name>)" dot-wt)
                 (string :tag "Format string (%r=repo, %b=repo-base, %n=name)")
                 (function :tag "Custom function"))
  :group 'magit-dash)

(defconst magit-dash-pr--default-columns
  '((org . nil) (repo . t) (pr . t) (author . t) (ci . t) (title . t) (base . nil) (worktree . t) (updated . t) (age . nil) (comments . t) (review . t) (approvals . t) (threads . t) (new . nil))
  "Default enabled state of each PR dashboard column.
Columns missing from a persisted `magit-dash-pr-columns' fall back to this.")

(defvar magit-dash-pr-columns
  (copy-tree magit-dash-pr--default-columns)
  "Alist of (COLUMN-SYMBOL . ENABLED) for the PR dashboard.
Persisted across sessions via `savehist-additional-variables'.")


(defconst magit-dash-pr--all-columns
  '(org repo pr author ci title base worktree updated age comments review approvals threads new)
  "All available PR dashboard columns in display order.")

(defconst magit-dash-pr--column-defs
  '((org      . ("Org"     10 t))
    (repo     . ("Repo"    16 t))
    (pr       . ("PR#"     5  nil))
    (author   . ("Author"  14 t))
    (base     . ("Base"    12 t))
    (ci       . ("CI"      3  t))
    (title    . ("Title"   38 t))
    (worktree . ("WT"      3  t))
    (updated  . ("Updated" 8  nil))
    (age      . ("Age"     6  nil))
    (comments . ("Cmts"    4  nil))
    (review   . ("Review"  14 t))
    (approvals . ("Appr"   6  t))
    (threads  . ("Thr"     3  t))
    (new      . ("New"     4  t)))
  "Alist of COLUMN-SYMBOL to default (LABEL WIDTH SORTABLE) for PR columns.
Org and Repo widths are computed dynamically in
`magit-dash-pr-dashboard--build-format'.")

(defcustom magit-dash-pr-show-org nil
  "When non-nil, display a separate Org column in the PR dashboard.
Kept in sync with `magit-dash-pr-columns'."
  :type 'boolean
  :group 'magit-dash
  :set (lambda (sym val)
         (set-default sym val)
         (when (boundp 'magit-dash-pr-columns)
           (setf (alist-get 'org magit-dash-pr-columns) (and val t)))))

(defun magit-dash-pr--column-enabled-p (col)
  "Return non-nil when column COL is enabled in `magit-dash-pr-columns'."
  (if (eq col 'org)
      (or (and (boundp 'magit-dash-pr-show-org) magit-dash-pr-show-org)
          (cdr (assq 'org magit-dash-pr-columns)))
    (if-let* ((entry (assq col magit-dash-pr-columns)))
        (cdr entry)
      (if (eq col 'updated)
          (if-let* ((age-entry (assq 'age magit-dash-pr-columns)))
              (cdr age-entry)
            t)
        (cdr (assq col magit-dash-pr--default-columns))))))

(defun magit-dash-pr--active-columns ()
  "Return PR column symbols that are currently enabled, in display order."
  (seq-filter #'magit-dash-pr--column-enabled-p
              magit-dash-pr--all-columns))

(defun magit-dash-pr-toggle-column (col)
  "Toggle visibility of column COL in the PR dashboard and refresh."
  (interactive
   (list (intern (completing-read "Toggle PR column: "
                                  (seq-map #'symbol-name
                                           magit-dash-pr--all-columns)
                                  nil t))))
  (let ((new-val (not (magit-dash-pr--column-enabled-p col))))
    (setf (alist-get col magit-dash-pr-columns) new-val)
    (when (eq col 'org)
      (setq magit-dash-pr-show-org new-val)))
  (when (derived-mode-p 'magit-dash-pr-dashboard-mode)
    (setq tabulated-list-format (magit-dash-pr-dashboard--build-format))
    (tabulated-list-init-header)
    (when tabulated-list-entries
      (setq tabulated-list-entries
            (seq-map (lambda (entry)
                       (let ((id (car entry)))
                         (magit-dash-pr-dashboard--build-entry
                          (plist-get id :pr)
                          (plist-get id :repo))))
                     tabulated-list-entries))
      (magit-dash-pr-dashboard--print))
    (magit-dash-pr-dashboard-refresh)))

(defun magit-dash-pr-toggle-org ()
  "Toggle visibility of the Org column in the PR dashboard."
  (interactive)
  (magit-dash-pr-toggle-column 'org))

(defun magit-dash-pr--split-slug (slug)
  "Split SLUG (owner/repo or repo) into cons (OWNER . REPO)."
  (if (and slug (string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" slug))
      (cons (match-string 1 slug) (match-string 2 slug))
    (cons "" (or slug ""))))

(defvar-local magit-dash-pr-dashboard--filters
  (list :state "open" :author "@me" :repo nil :org nil)
  "Current PR dashboard filters plist.
Keys: :state (\"open\"/\"closed\"), :author, :repo (OWNER/NAME), :org.")

(defcustom magit-dash-pr-fetch-limit 100
  "Number of pull requests to request from GitHub per listing.
`gh' defaults to 30, which silently truncates busy repositories.
`magit-dash-pr-dashboard-fetch-more' doubles the limit for one buffer."
  :type 'integer
  :group 'magit-dash)

(defvar-local magit-dash-pr-dashboard--limit nil
  "Per-buffer PR fetch limit; nil means `magit-dash-pr-fetch-limit'.")

(defvar-local magit-dash-pr-dashboard--hide-failing nil
  "When non-nil, rows whose CI is failing are omitted from the dashboard.")

(defvar-local magit-dash-pr-dashboard--marked-entries nil
  "List of marked PR keys (cons of (REPO . NUMBER)) in the current PR dashboard.")

(defface magit-dash-pr-ci-pass-face
  '((t :inherit success))
  "Face for passing CI status in the PR dashboard."
  :group 'magit-dash)

(defface magit-dash-pr-ci-fail-face
  '((t :inherit error))
  "Face for failing CI status in the PR dashboard."
  :group 'magit-dash)

(defface magit-dash-pr-ci-pending-face
  '((t :inherit warning))
  "Face for pending CI status in the PR dashboard."
  :group 'magit-dash)

(defun magit-dash-pr-dashboard--build-format (&optional width entries)
  "Return `tabulated-list-format' vector with elastic columns.
Uses active columns.  WIDTH is available window width; when nil,
`window-width' is used.  ENTRIES is the list of tabulated-list entries."
  (let* ((ents (or entries (and (boundp 'tabulated-list-entries) tabulated-list-entries)))
         (active (magit-dash-pr--active-columns))
         (show-org (magit-dash-pr--column-enabled-p 'org))
         (raw-repo (when ents
                     (seq-reduce (lambda (w e)
                                   (let* ((slug (or (plist-get (car e) :repo) ""))
                                          (pair (magit-dash-pr--split-slug slug))
                                          (name (if show-org (cdr pair) (if (string-empty-p (cdr pair)) (car pair) (cdr pair)))))
                                     (max w (length name))))
                                 ents 0)))
         (raw-org (when (and show-org ents)
                    (seq-reduce (lambda (w e)
                                  (let* ((slug (or (plist-get (car e) :repo) ""))
                                         (pair (magit-dash-pr--split-slug slug)))
                                    (max w (length (car pair)))))
                                ents 0)))
         (org-width (if show-org (max 6 (+ (or raw-org 3) 2)) 0))
         (fixed-width (seq-reduce
                       (lambda (acc col)
                         (if (and (not (eq col 'repo))
                                  (not (eq col 'org)))
                             (+ acc (cadr (alist-get col magit-dash-pr--column-defs)))
                           acc))
                       active 0))
         (avail-repo (max (length "Repo")
                          (- (or width (window-width)) (+ fixed-width org-width 2))))
         (repo-cap (and raw-repo (> raw-repo 0) (max (length "Repo") (+ raw-repo 3))))
         (repo-width (if repo-cap
                         (min avail-repo repo-cap)
                       (max 12 avail-repo))))
    (apply #'vector
           (seq-map (lambda (col)
                      (pcase col
                        ('org  `("Org"  ,org-width  t))
                        ('repo `("Repo" ,repo-width t))
                        (_     (alist-get col magit-dash-pr--column-defs))))
                    active))))

(defvar-keymap magit-dash-pr-dashboard-review-map
  :doc "Review, worktree and CI keys, mounted on `c' in `magit-dash-pr-dashboard-mode-map'."
  "r" #'magit-dash-pr-dashboard-diff-review
  "v" #'magit-dash-pr-dashboard-diff-review
  "w" #'magit-dash-pr-dashboard-create-worktree
  "a" #'magit-dash-pr-dashboard-create-all-worktrees
  "i" #'magit-dash-pr-dashboard-fetch-ci)

(defvar-keymap magit-dash-pr-dashboard-mark-map
  :doc "Mark keys, mounted on `m' in `magit-dash-pr-dashboard-mode-map'."
  "m" #'magit-dash-pr-dashboard-menu
  "?" #'magit-dash-pr-dashboard-menu
  "t" #'magit-dash-pr-dashboard-toggle-mark
  "u" #'magit-dash-pr-dashboard-unmark
  "c" #'magit-dash-pr-dashboard-unmark-all
  "a" #'magit-dash-pr-dashboard-mark-all)

(defvar-keymap magit-dash-pr-dashboard-view-map
  :doc "View keys, mounted on `v' in `magit-dash-pr-dashboard-mode-map'."
  "o" #'magit-dash-pr-toggle-org
  "t" #'magit-dash-pr-toggle-column
  "s" #'magit-dash-pr-dashboard-switch-view
  "v" #'magit-dash-pr-dashboard-diff-review)

(defvar-keymap magit-dash-pr-dashboard-filter-map
  :doc "Filter keys, mounted on `f' in `magit-dash-pr-dashboard-mode-map'."
  "f" #'magit-dash-pr-dashboard-set-filter
  "c" #'magit-dash-pr-dashboard-clear-filters
  "x" #'magit-dash-pr-dashboard-toggle-hide-failing)

(defun magit-dash-pr-dashboard--setup-code-review-keys ()
  "Bind or unbind the code-review key `b' in `magit-dash-pr-dashboard-review-map'."
  (if (magit-dash-pr-code-review-available-p)
      (keymap-set magit-dash-pr-dashboard-review-map "b" #'magit-dash-pr-dashboard-code-review)
    (keymap-unset magit-dash-pr-dashboard-review-map "b" t)))

(defvar magit-dash-pr-dashboard-mode-map
  (let ((m (make-sparse-keymap)))
    (keymap-set m "RET" #'magit-dash-pr-dashboard-open-browser)
    (keymap-set m "b"   #'magit-dash-pr-dashboard-open-browser)
    (keymap-set m "c"   (cons "review" magit-dash-pr-dashboard-review-map))
    (keymap-set m "m"   (cons "mark" magit-dash-pr-dashboard-mark-map))
    (keymap-set m "v"   (cons "view" magit-dash-pr-dashboard-view-map))
    (keymap-set m "f"   (cons "filter" magit-dash-pr-dashboard-filter-map))
    (keymap-set m "w"   #'magit-dash-pr-dashboard-open-worktree)
    (keymap-set m "a"   #'magit-dash-pr-dashboard-create-all-worktrees)
    (keymap-set m "k"   #'magit-dash-pr-dashboard-clean-merged-worktrees)
    (keymap-set m "d"   #'magit-dash-pr-dashboard-open-dired)
    (keymap-set m "*"   #'magit-dash-pr-dashboard-mark-all)
    (keymap-set m "SPC"   #'magit-dash-pr-dashboard-toggle-mark)
    (keymap-set m "g"   #'magit-dash-pr-dashboard-refresh)
    (keymap-set m "r"   #'magit-dash-pr-dashboard-refresh-entries)
    (keymap-set m "+"   #'magit-dash-pr-dashboard-fetch-more)
    (keymap-set m "/"   #'magit-dash-pr-dashboard-set-filter)
    (keymap-set m "C-/"   #'magit-dash-pr-dashboard-clear-filters)
    (keymap-set m "q"   #'magit-dash-pr-dashboard-quit)
    (keymap-set m "?"   #'magit-dash-pr-dashboard-menu)
    m)
  "Keymap for `magit-dash-pr-dashboard-mode'.")

(magit-dash-pr-dashboard--setup-code-review-keys)

(define-derived-mode magit-dash-pr-dashboard-mode tabulated-list-mode "PRs"
  "Major mode for the pull request dashboard."
  (setq tabulated-list-format (magit-dash-pr-dashboard--build-format))
  (setq tabulated-list-sort-key nil)
  (setq-local mode-line-process " [a: create all worktrees, ?: menu]")
  (tabulated-list-init-header)
  (setq-local magit-dash-pr-dashboard--filters
              (list :state "open" :author nil :repo nil :org nil))
  (setq-local magit-dash-pr-dashboard--marked-entries nil)
  (add-hook 'kill-buffer-hook #'magit-dash-pr-dashboard--cancel-status-collection nil t)
  (magit-dash-pr-dashboard--setup-code-review-keys))

(defun magit-dash-pr--parse-status-rollup (rollup)
  "Calculate overall CI state from GitHub ROLLUP check runs or contexts.
ROLLUP may be a list of check runs/contexts, a vector, or an alist with `state'.
Returns \"SUCCESS\", \"FAILURE\", \"PENDING\", \"ERROR\", or nil."
  (cond
   ((null rollup) nil)
   ;; Single object fallback with `state' (alist, plist, or hash-table)
   ((and (consp rollup) (consp (car rollup)) (symbolp (caar rollup))
         (or (assq 'state rollup) (assq :state rollup)))
    (upcase (format "%s" (or (alist-get 'state rollup) (alist-get :state rollup)))))
   ((and (listp rollup) (keywordp (car-safe rollup)) (plist-get rollup :state))
    (upcase (format "%s" (plist-get rollup :state))))
   (t
    (let ((items (if (vectorp rollup) (append rollup nil) rollup)))
      (if (null items)
          nil
        (let ((has-failure nil)
              (has-pending nil)
              (has-success nil))
          (dolist (item items)
            (when (or (consp item) (hash-table-p item))
              (let* ((state (when-let* ((s (or (map-elt item 'state) (map-elt item :state))))
                              (upcase (format "%s" s))))
                     (status (when-let* ((s (or (map-elt item 'status) (map-elt item :status))))
                               (upcase (format "%s" s))))
                     (conclusion (when-let* ((c (or (map-elt item 'conclusion) (map-elt item :conclusion))))
                                   (upcase (format "%s" c)))))
                (cond
                 ;; Direct failure conclusion or error/failure state
                 ((or (member conclusion '("FAILURE" "TIMED_OUT" "STARTUP_FAILURE" "ACTION_REQUIRED" "CANCELLED"))
                      (member state '("FAILURE" "ERROR")))
                  (setq has-failure t))
                 ;; In-progress / pending status or state
                 ((or (member status '("IN_PROGRESS" "QUEUED" "WAITING" "PENDING" "REQUESTED"))
                      (equal state "PENDING")
                      (and (not (equal status "COMPLETED")) (or (null conclusion) (string-empty-p conclusion))))
                  (setq has-pending t))
                 ;; Completed success
                 ((or (member conclusion '("SUCCESS" "NEUTRAL" "SKIPPED"))
                      (equal state "SUCCESS"))
                  (setq has-success t))))))
          (cond
           (has-failure "FAILURE")
           (has-pending "PENDING")
           (has-success "SUCCESS")
           (t nil))))))))

(defun magit-dash-pr-dashboard--format-ci (rollup-state)
  "Format CI ROLLUP-STATE string for display using checkmark/x glyphs.
Uses the same ✓/x/⟳ indicators as the repository dashboard."
  (pcase rollup-state
    ("SUCCESS" (propertize "✓" 'face 'magit-dash-pr-ci-pass-face))
    ((or "FAILURE" "ERROR") (propertize "x" 'face 'magit-dash-pr-ci-fail-face))
    ("PENDING" (propertize "⟳" 'face 'magit-dash-pr-ci-pending-face))
    (_ (propertize "—" 'face 'shadow))))

(defun magit-dash-pr-dashboard--format-review (decision)
  "Format review DECISION string for display."
  (pcase decision
    ("APPROVED" (propertize "approved" 'face 'success))
    ("CHANGES_REQUESTED" (propertize "changes req" 'face 'error))
    ("REVIEW_REQUIRED" (propertize "needed" 'face 'warning))
    (_ "")))

;;;; Cached review status (approvals and unresolved threads)

(defvar-local magit-dash-pr-dashboard--statuses nil
  "Hash table of (REPO . NUMBER) to the cached `:status' plist of that PR.
Created on first use.  Rows read their approvals, threads and fallback CI
cells from it; rows without an entry show blank cells.")

(defvar-local magit-dash-pr-dashboard--status-cancel nil
  "Function cancelling the in-flight status collection of this buffer, or nil.")

(defconst magit-dash-pr-dashboard--status-keys
  '(:ci :approvals :required :requested :changes-requested :threads-open
    :threads-outdated :failing :truncated :bucket)
  "Keys of a PR plist that make up a cached `:status'.")

(put 'magit-dash-pr-dashboard--statuses 'permanent-local t)
(put 'magit-dash-pr-dashboard--status-cancel 'permanent-local t)

(defun magit-dash-pr-dashboard--norm-key (repo number)
  "Return the canonical (REPO . NUMBER) key with REPO downcased.
GitHub repo names are case-insensitive but rows keep their original
case (cache warm-start and live discovery may differ), so the status
table, marks, row matching and status fills all go through this one
helper at their boundary and a row keeps its status and mark when a live
row replaces a warm-start one."
  (cons (if (stringp repo)
            (downcase repo)
          repo)
        number))

(defun magit-dash-pr-dashboard--status (repo number)
  "Return the status plist held for REPO and NUMBER in this buffer, or nil."
  (when magit-dash-pr-dashboard--statuses
    (map-elt magit-dash-pr-dashboard--statuses
             (magit-dash-pr-dashboard--norm-key repo number))))

(defun magit-dash-pr-dashboard--cancel-status-collection ()
  "Cancel this buffer's in-flight status collection, if any."
  (when (functionp magit-dash-pr-dashboard--status-cancel)
    (funcall magit-dash-pr-dashboard--status-cancel))
  (setq magit-dash-pr-dashboard--status-cancel nil))

(put 'magit-dash-pr-dashboard--cancel-status-collection 'permanent-local-hook t)

(defun magit-dash-pr-dashboard--set-status (repo number status)
  "Hold STATUS for REPO and NUMBER in this buffer's status table."
  (unless magit-dash-pr-dashboard--statuses
    (setq magit-dash-pr-dashboard--statuses (make-hash-table :test #'equal)))
  (setf (map-elt magit-dash-pr-dashboard--statuses
                 (magit-dash-pr-dashboard--norm-key repo number))
        status))

(defun magit-dash-pr-dashboard--valid-status-p (status)
  "Return non-nil when STATUS is a plist with correctly typed display fields."
  (and (consp status)
       (plistp status)
       (let ((requested (plist-get status :requested))
             (ci (plist-get status :ci))
             (changes (plist-get status :changes-requested)))
         (and (proper-list-p requested)
              (seq-every-p #'stringp requested)
              (seq-every-p (lambda (key)
                             (let ((value (plist-get status key)))
                               (or (null value) (natnump value))))
                           '(:threads-open :threads-outdated :approvals :required))
              (or (null ci) (stringp ci))
              (or (null changes) (eq changes t) (natnump changes))))))

(defun magit-dash-pr-dashboard--meaningful-status-p (status)
  "Return non-nil when STATUS is valid, not error-shaped and has a non-nil field."
  (and (magit-dash-pr-dashboard--valid-status-p status)
       (null (plist-get status :error))
       (seq-some (lambda (key)
                   (plist-get status key))
                 '(:ci :approvals :required :requested :changes-requested
                   :threads-open :threads-outdated :failing))))

(defun magit-dash-pr-dashboard--hold-record-status (record)
  "Hold the valid `:status' of cache RECORD in this buffer's table; return RECORD."
  (when-let* ((status (plist-get record :status))
              ((magit-dash-pr-dashboard--valid-status-p status)))
    (magit-dash-pr-dashboard--set-status
     (plist-get record :repo) (plist-get record :number) status))
  record)

(defun magit-dash-pr--status-from-pr (pr)
  "Return the cacheable status plist selected from the PR plist PR."
  (seq-mapcat (lambda (key)
                (list key (plist-get pr key)))
              magit-dash-pr-dashboard--status-keys))

(defun magit-dash-pr-dashboard--format-approvals (status)
  "Format the approvals cell for STATUS as approved/required.
A trailing `!' marks requested changes; requested reviewers are in the
help text.  Blank without a valid STATUS."
  (if (not (and (magit-dash-pr-dashboard--valid-status-p status)
                (numberp (plist-get status :approvals))))
      ""
    (let* ((approvals (plist-get status :approvals))
           (required (plist-get status :required))
           (changes (plist-get status :changes-requested))
           (blocked (and changes (not (eql changes 0))))
           (text (concat (if (numberp required)
                             (format "%d/%d" approvals required)
                           (number-to-string approvals))
                         (if blocked "!" "")))
           (requested (plist-get status :requested)))
      (propertize text
                  'face (cond
                         (blocked 'error)
                         ((and (numberp required) (>= approvals required)) 'success)
                         (t 'warning))
                  'help-echo (concat (if blocked "Changes requested. " "")
                                     (if requested
                                         (concat "Requested: " (string-join requested ", "))
                                       "No pending review requests"))))))

(defun magit-dash-pr-dashboard--format-threads (status)
  "Format the open unresolved thread count of STATUS; blank when zero or unknown."
  (let ((open (and (magit-dash-pr-dashboard--valid-status-p status)
                   (plist-get status :threads-open))))
    (if (and (natnump open) (> open 0))
        (propertize (number-to-string open)
                    'face 'warning
                    'help-echo (format "%d unresolved thread(s), %d outdated"
                                       open (or (plist-get status :threads-outdated) 0)))
      "")))

;;;; New since last review

(defconst magit-dash-pr-dashboard--reviewed-negative-ttl 60
  "Seconds a no-review-file result is reused before the path is resolved again.")

(defvar magit-dash-pr-dashboard--reviewed-heads (make-hash-table :test #'equal)
  "Map of (REPO . NUMBER) to a plist describing that PR's review file.
Keys: `:file' (resolved path or nil), `:mtime', `:state' (`ok', `none' or
`unreadable'), `:head', `:source' (`verdict' or `head_oid') and `:checked'.
A `none' entry is reused for `magit-dash-pr-dashboard--reviewed-negative-ttl'
seconds, so a render of unreviewed rows spawns no process after the first;
an `ok' or `unreadable' entry is reused until the file's mtime changes.
Growth is bounded by the PRs shown: it is emptied on every dashboard refresh.")

(defun magit-dash-pr-dashboard--reviewed (repo number)
  "Return a plist (:state :head :source) for the review file of REPO and NUMBER.
:state is `ok' (a head was found), `none' (no file, no head, or no local
checkout) or `unreadable' (malformed JSON, messaged once per file mtime).
Local and read only: no network.  Cached per (REPO . NUMBER); see
`magit-dash-pr-dashboard--reviewed-heads'."
  (let* ((key (magit-dash-pr-dashboard--norm-key repo number))
         (entry (map-elt magit-dash-pr-dashboard--reviewed-heads key))
         (now (float-time))
         (fresh-none (and entry
                          (eq (plist-get entry :state) 'none)
                          (< (- now (plist-get entry :checked))
                             magit-dash-pr-dashboard--reviewed-negative-ttl)))
         (file (and entry (plist-get entry :file)))
         (mtime (and file (file-attribute-modification-time (file-attributes file)))))
    (cond
     (fresh-none
      entry)
     ((and mtime (time-equal-p mtime (plist-get entry :mtime)))
      entry)
     (t
      (when (and file (null mtime))
        (setf (map-elt magit-dash-pr-dashboard--reviewed-heads key) nil))
      (let* ((root (magit-dash-pr-dashboard--find-local-path repo))
             (file (when root
                     (require 'magit-dash-review)
                     (magit-dash-review-file root number repo)))
             (mtime (and file (file-attribute-modification-time (file-attributes file))))
             (result (list :file (and mtime file) :mtime mtime :state 'none
                           :head nil :source nil :checked now)))
        (when mtime
          (condition-case err
              (let* ((data (magit-dash-review-json-read-file file))
                     (verdict (and (hash-table-p data) (map-elt data "verdict")))
                     (md (and (hash-table-p data) (map-elt data "magit_dash")))
                     (usable (lambda (value)
                               (and (stringp value) (not (string-empty-p value)))))
                     (head (seq-find usable
                                     (list (and (hash-table-p verdict) (map-elt verdict "head"))
                                           (and (hash-table-p verdict)
                                                (map-elt verdict "submitted_head"))
                                           (and (hash-table-p md) (map-elt md "head_oid")))))
                     (from-verdict (and (hash-table-p verdict)
                                        (or (funcall usable (map-elt verdict "head"))
                                            (funcall usable
                                                     (map-elt verdict "submitted_head"))))))
                (unless (hash-table-p data)
                  (signal 'json-error (list "not a JSON object")))
                (setq result
                      (plist-put
                       (plist-put
                        (plist-put result :head head)
                        :state (if head 'ok 'none))
                       :source (if from-verdict 'verdict 'head_oid))))
            ((json-error file-error)
             (message "magit-dash-pr: review file %s is unreadable: %s"
                      file (error-message-string err))
             (setq result (plist-put result :state 'unreadable)))))
        (setf (map-elt magit-dash-pr-dashboard--reviewed-heads key) result)
        result)))))

(defun magit-dash-pr-dashboard--format-new (repo number head)
  "Format the new-since-review cell for REPO and NUMBER given the PR's HEAD.
\"new\" when the last-reviewed head differs from HEAD.  Blank otherwise,
with help text saying whether the PR was never reviewed, was reviewed and
is unchanged, or has an unreadable review file; fully empty when HEAD is
unknown.  A head that comes only from `magit_dash.head_oid' (synced, no
verdict) is not a review: it gives a neutral blank cell, never new or
unchanged.  Heads match when one is a prefix of the other (both at least 7
characters), so an abbreviated hash never reads as new."
  (if (not (and (stringp head) (not (string-empty-p head))))
      ""
    (let* ((info (magit-dash-pr-dashboard--reviewed repo number))
           (reviewed (plist-get info :head)))
      (cond
       ((eq (plist-get info :state) 'unreadable)
        (propertize " " 'help-echo "review file unreadable"))
       ((null reviewed)
        (propertize " " 'help-echo "Never reviewed"))
       ((eq (plist-get info :source) 'head_oid)
        (propertize " " 'help-echo "synced review file, no verdict recorded"))
       ((or (equal reviewed head)
            (and (>= (min (length reviewed) (length head)) 7)
                 (or (string-prefix-p reviewed head)
                     (string-prefix-p head reviewed))))
        (propertize " " 'help-echo "Unchanged since last review"))
       (t
        (propertize "new" 'face 'warning
                    'help-echo (format "New commits since last review (was %s, now %s)"
                                       (substring reviewed 0 (min 8 (length reviewed)))
                                       (substring head 0 (min 8 (length head))))))))))

(defun magit-dash-pr-dashboard--format-age (updated-at)
  "Format UPDATED-AT ISO timestamp as a human-readable age string."
  (when (and updated-at (not (equal updated-at "")))
    (condition-case nil
        (magit-dash--format-age
         (float-time (time-since (date-to-time updated-at))))
      (error "?"))))

(defun magit-dash-pr-dashboard--comments-count (pr)
  "Return the comment count from PR alist.
Handles both an integer `comments' field and a nested `(totalCount . N)' alist,
as well as a top-level `commentsCount' integer."
  (let ((c (map-elt pr 'comments)))
    (cond
     ((numberp c) c)
     ((and c (map-elt c 'totalCount)) (map-elt c 'totalCount))
     (t (let ((n (map-elt pr 'commentsCount)))
          (if (numberp n) n 0))))))

(defun magit-dash-pr-dashboard--build-args (filters &optional limit)
  "Return a gh args list for fetching PRs matching FILTERS plist.
When :repo is set, uses `gh pr list -R REPO' (includes CI status).
Otherwise uses `gh search prs' for cross-repo listing.
LIMIT defaults to `magit-dash-pr-fetch-limit'."
  (let* ((limit (number-to-string (or limit magit-dash-pr-fetch-limit)))
         (state (or (plist-get filters :state) "open"))
         (repo (plist-get filters :repo))
         (author (plist-get filters :author))
         (org (plist-get filters :org))
         (review-requested (plist-get filters :review-requested)))
    (if repo
        (append
         (list "pr" "list" "-R" repo "--state" state
               "--json"
               "number,title,state,mergedAt,author,createdAt,updatedAt,comments,reviewDecision,statusCheckRollup,isDraft,url,headRefName,headRefOid,baseRefName,mergeable,mergeStateStatus"
               "--jq" (if (plist-get filters :conflicts)
                          "map(select(.mergeable == \"CONFLICTING\" or .mergeStateStatus == \"DIRTY\") | . + {commentsCount: (.comments | length)} | del(.comments))"
                        "map(. + {commentsCount: (.comments | length)} | del(.comments))")
               "--limit" limit)
         (when author (list "--author" author)))
      (append
       (list "search" "prs"
             "--state" (if (member state '("open" "closed")) state "open")
             "--json"
             "number,title,state,repository,author,createdAt,updatedAt,commentsCount,isDraft,url"
             "--limit" limit)
       (when author
         (list "--author" author))
       (when review-requested
         (list "--review-requested" review-requested))
       (when org
         (list "--owner" org))))))

(defconst magit-dash-pr-dashboard--title-width 36
  "Maximum display width for PR title column.")

(defun magit-dash-pr-dashboard--entry-key (entry)
  "Return unique, case-normalized key `(REPO . NUMBER)' for PR ENTRY plist.
See `magit-dash-pr-dashboard--norm-key'."
  (magit-dash-pr-dashboard--norm-key (plist-get entry :repo) (plist-get entry :number)))

(defun magit-dash-pr-dashboard--entry-marked-p (entry)
  "Return non-nil if PR ENTRY is marked in the current dashboard."
  (and (bound-and-true-p magit-dash-pr-dashboard--marked-entries)
       (member (magit-dash-pr-dashboard--entry-key entry)
               magit-dash-pr-dashboard--marked-entries)
       t))

;;;; Point-preserving redraw and CI filter

(defun magit-dash-pr-dashboard--key-at (pos)
  "Return the (REPO . NUMBER) key of the row at buffer position POS, or nil."
  (save-excursion
    (goto-char pos)
    (when-let* ((id (tabulated-list-get-id)))
      (magit-dash-pr-dashboard--entry-key id))))

(defun magit-dash-pr-dashboard--goto-key (key)
  "Move point to the row for KEY; return non-nil if found."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (not (eobp)))
      (if (equal key (magit-dash-pr-dashboard--key-at (point)))
          (setq found t)
        (forward-line 1)))
    found))

(defun magit-dash-pr-dashboard--print ()
  "Redraw the table, keeping every window's point and scroll position.
Rows are matched by (REPO . NUMBER) because async CI enrichment rebuilds
row ids, which defeats `tabulated-list-print's `equal' matching."
  (let ((saved (seq-map (lambda (win)
                          (list win
                                (magit-dash-pr-dashboard--key-at (window-point win))
                                (magit-dash-pr-dashboard--key-at (window-start win))
                                (save-excursion
                                  (goto-char (window-point win))
                                  (current-column))))
                        (get-buffer-window-list nil nil t))))
    (tabulated-list-print t)
    (seq-do (lambda (item)
              (pcase-let ((`(,win ,point-key ,start-key ,col) item))
                (when (and point-key (window-live-p win))
                  (when start-key
                    (save-excursion
                      (when (magit-dash-pr-dashboard--goto-key start-key)
                        (set-window-start win (point) t))))
                  (save-excursion
                    (when (magit-dash-pr-dashboard--goto-key point-key)
                      (move-to-column col)
                      (set-window-point win (point)))))))
            saved)))

(defun magit-dash-pr-dashboard--failing-entry-p (entry)
  "Return non-nil when tabulated-list ENTRY has failing CI."
  (member (magit-dash-pr--parse-status-rollup
           (map-elt (plist-get (car entry) :pr) 'statusCheckRollup))
          '("FAILURE" "ERROR")))

(defun magit-dash-pr-dashboard--apply-ci-filter (entries)
  "Drop failing-CI rows from ENTRIES when `--hide-failing' is set."
  (if magit-dash-pr-dashboard--hide-failing
      (seq-remove #'magit-dash-pr-dashboard--failing-entry-p entries)
    entries))

(defun magit-dash-pr-dashboard--string-or-empty (value)
  "Return VALUE when it is a string, else the empty string."
  (if (stringp value)
      value
    ""))

(defun magit-dash-pr-dashboard--sanitize-pr (pr)
  "Return PR with `:null' turned into nil and malformed fields neutralized.
A string `number' becomes an integer and a non-alist `author' becomes nil.
Signals `wrong-type-argument' when PR has no usable number."
  (let* ((pr (magit-dash-pr-cache--null-to-nil pr))
         (number (map-elt pr 'number)))
    (when (stringp number)
      (setq number (string-to-number number)))
    (unless (integerp number)
      (signal 'wrong-type-argument (list 'integerp number)))
    (setq pr (cons (cons 'number number)
                   (seq-remove (lambda (cell)
                                 (eq (car-safe cell) 'number))
                               pr)))
    (if (consp (map-elt pr 'author))
        pr
      (cons (cons 'author nil)
            (seq-remove (lambda (cell)
                          (eq (car-safe cell) 'author))
                        pr)))))

(defun magit-dash-pr-dashboard--build-entry (pr repo-name &optional fetched-at)
  "Return a `tabulated-list-entries' entry for PR in REPO-NAME.
Uses active columns.  FETCHED-AT is the cache timestamp of rows loaded
from the PR cache; such rows are dimmed once stale.  A nil FETCHED-AT
marks freshly fetched data and is never dimmed."
  (let* ((pr (magit-dash-pr-dashboard--sanitize-pr pr))
         (number (map-elt pr 'number))
         (entry-id (list :number number
                         :repo repo-name
                         :updated (map-elt pr 'updatedAt)
                         :head-ref (map-elt pr 'headRefName)
                         :mergeable (map-elt pr 'mergeable)
                         :merge-state-status (map-elt pr 'mergeStateStatus)
                         :fetched-at fetched-at
                         :pr pr))
         (is-marked (magit-dash-pr-dashboard--entry-marked-p entry-id))
         (pair (magit-dash-pr--split-slug repo-name))
         (org-str (car pair))
         (short-repo (if (string-empty-p (cdr pair)) (car pair) (cdr pair)))
         (status (magit-dash-pr-dashboard--status repo-name number))
         (active (magit-dash-pr--active-columns)))
    (list entry-id
          (let ((cells
                 (apply #'vector
                        (seq-map
                         (lambda (col)
                           (pcase col
                             ('org org-str)
                             ('repo
                              (if is-marked
                                  (propertize short-repo 'face 'bold)
                                short-repo))
                             ('author
                              (magit-dash-pr-dashboard--string-or-empty
                               (map-elt (map-elt pr 'author) 'login)))
                             ('base
                              (magit-dash-pr-dashboard--string-or-empty
                               (map-elt pr 'baseRefName)))
                             ('worktree
                              (cond
                               ((map-elt magit-dash-pr--pending-worktrees (cons repo-name number))
                                (propertize "?" 'face 'shadow))
                               ((magit-dash-pr-dashboard--worktree-exists-p entry-id)
                                (propertize "✓" 'face 'success))
                               (t (propertize "·" 'face 'shadow))))
                             ('pr
                              (let ((s (number-to-string number)))
                                (if is-marked
                                    (propertize (concat "*" s) 'face 'bold)
                                  s)))
                             ('title
                              (let ((t-str (truncate-string-to-width
                                            (magit-dash-pr-dashboard--string-or-empty
                                             (map-elt pr 'title))
                                            magit-dash-pr-dashboard--title-width nil nil "…")))
                                (if is-marked
                                    (propertize t-str 'face 'bold)
                                  t-str)))
                             ('ci
                              (magit-dash-pr-dashboard--format-ci
                               (or (magit-dash-pr--parse-status-rollup (map-elt pr 'statusCheckRollup))
                                   (plist-get status :ci))))
                             ('approvals
                              (magit-dash-pr-dashboard--format-approvals status))
                             ('threads
                              (magit-dash-pr-dashboard--format-threads status))
                             ('new
                              (magit-dash-pr-dashboard--format-new
                               repo-name number
                               (or (map-elt pr 'headRefOid)
                                   (plist-get (magit-dash-pr-cache-get repo-name number)
                                              :head-sha))))
                             ('updated
                              (or (magit-dash-pr-dashboard--format-age (map-elt pr 'updatedAt)) "?"))
                             ('age
                              (or (magit-dash-pr-dashboard--format-age
                                   (or (map-elt pr 'createdAt) (map-elt pr 'updatedAt))) "?"))
                             ('comments
                              (number-to-string (magit-dash-pr-dashboard--comments-count pr)))
                             ('review
                              (magit-dash-pr-dashboard--format-review (map-elt pr 'reviewDecision)))))
                         active))))
            (if (and fetched-at
                     (magit-dash-pr-cache-stale-p (list :fetched-at fetched-at)))
                (seq-into
                 (seq-map (lambda (cell)
                            (let ((copy (copy-sequence cell)))
                              (add-face-text-property 0 (length copy) 'magit-dash-stale t copy)
                              copy))
                          cells)
                 'vector)
              cells)))))

(defun magit-dash-pr-dashboard--cache-row (pr repo-name &optional search-p)
  "Write PR, an alist for REPO-NAME, to the PR cache.
SEARCH-P says PR came from cross-repo search (see `magit-dash-pr-cache-put').
The cache merges PR over its cached payload, so a sparse discovery
result never discards richer cached data; per-field timestamps keep the
merged-in volatile fields (CI, review) looking as old as they are, and a
PR older than the cached one is ignored.  A bad record or PR is skipped
with a message and never signals."
  (condition-case err
      (when-let* ((number (map-elt pr 'number)))
        (magit-dash-pr-cache-put repo-name number pr nil nil search-p))
    (error
     (message "magit-gh: could not cache %s: %s"
              repo-name (error-message-string err))
     nil)))

(defun magit-dash-pr-dashboard--cached-entries (filters)
  "Return table entries built from the PR cache for FILTERS.
Only the state, repo, org and a literal author filter apply; rows keep
their effective cache timestamp so stale ones are dimmed, and a record
without one (or with a future one) counts as stale.  Ghost (`:gone')
records are skipped only in open-state views, as is any record that
cannot be turned into a row (with a message).  Known
limitation: the `@me' author filter and closed PRs are not reproduced
faithfully from the cache; discovery corrects them once it returns."
  (let* ((state (upcase (or (plist-get filters :state) "open")))
         (org (plist-get filters :org))
         (author (plist-get filters :author)))
    (thread-last
      (magit-dash-pr-cache-list (plist-get filters :repo) org)
      (seq-remove (lambda (record)
                    (and (equal state "OPEN") (plist-get record :gone))))
      (seq-keep
       (lambda (record)
         (condition-case err
             (let* ((pr (magit-dash-pr-dashboard--sanitize-pr
                         (plist-get record :payload)))
                    (repo (plist-get record :repo)))
               (when (and (equal state (upcase (let ((st (map-elt pr 'state)))
                                                 (if (stringp st) st "OPEN"))))
                          (or (null org)
                              (string-prefix-p (concat (downcase org) "/") (downcase repo)))
                          (or (null author)
                              (string-prefix-p "@" author)
                              (equal author (map-elt (map-elt pr 'author) 'login))))
                 (magit-dash-pr-dashboard--hold-record-status record)
                 (magit-dash-pr-dashboard--build-entry
                  pr repo
                  (if (magit-dash-pr-cache-future-p record)
                      0
                    (or (magit-dash-pr-cache-effective-fetched-at record) 0)))))
           (error
            (message "magit-gh: skipping unusable cached PR %s#%s: %s"
                     (plist-get record :repo) (plist-get record :number)
                     (error-message-string err))
            nil))))
      (magit-dash-pr-dashboard--apply-ci-filter))))

(defun magit-dash-pr-dashboard--evict-ghosts (filters entries output)
  "Mark cached PRs missing from a complete discovery of FILTERS as gone.
ENTRIES are the rows built from OUTPUT, the raw `gh' JSON.  The PR
numbers seen come from OUTPUT itself, and nothing is evicted unless every
raw row has an integer number and became exactly one entry, so a row that
failed to build cannot make a live PR look missing (an empty OUTPUT
still means every PR is gone).  Only an unfiltered per-repo discovery
that returned fewer rows than the limit covers its repo completely;
anything else (search across repos, author, conflicts or review filters,
hide-failing, a possibly truncated result) evicts nothing.  Only records
of the covered state scope are marked."
  (let ((repo (plist-get filters :repo))
        (state (upcase (or (plist-get filters :state) "open")))
        (raw (condition-case nil
                 (json-parse-string (string-trim output)
                                    :array-type 'list :object-type 'alist)
               (error :invalid))))
    (when (and repo
               (listp raw)
               (not (plist-get filters :author))
               (not (plist-get filters :conflicts))
               (not (plist-get filters :review-requested))
               (not magit-dash-pr-dashboard--hide-failing)
               (= (length raw) (length entries))
               (< (length raw)
                  (or magit-dash-pr-dashboard--limit magit-dash-pr-fetch-limit)))
      (let ((seen (seq-map (lambda (pr)
                             (and (consp pr) (alist-get 'number pr)))
                           raw)))
        (when (seq-every-p #'integerp seen)
          (seq-do
           (lambda (record)
             (when (and (not (memql (plist-get record :number) seen))
                        (or (equal state "ALL")
                            (equal state (upcase (let ((st (map-elt (plist-get record :payload) 'state)))
                                                   (if (stringp st) st "OPEN"))))))
               (magit-dash-pr-cache-mark-gone repo (plist-get record :number))))
           (magit-dash-pr-cache-list repo)))))))

(defun magit-dash-pr-dashboard--parse-output (output filters)
  "Parse gh JSON OUTPUT into tabulated list entries using FILTERS for context.
Returns nil when OUTPUT is not a JSON array.  A row that cannot be built
is skipped with a message and never aborts the rest."
  (let ((trimmed (string-trim output)))
    (when (string-prefix-p "[" trimmed)
      (when-let* ((prs (json-parse-string trimmed :array-type 'list :object-type 'alist)))
        (let ((per-repo-p (plist-get filters :repo))
              (conflicts-p (plist-get filters :conflicts)))
          (when conflicts-p
            (setq prs (seq-filter (lambda (pr)
                                    (or (string-equal (or (map-elt pr 'mergeable) "") "CONFLICTING")
                                        (string-equal (or (map-elt pr 'mergeStateStatus) "") "DIRTY")))
                                  prs)))
          (magit-dash-pr-dashboard--apply-ci-filter
           (seq-keep (lambda (pr)
                       (condition-case err
                           (magit-dash-pr-dashboard--build-entry
                            pr
                            (if per-repo-p
                                (plist-get filters :repo)
                              (or (map-elt (map-elt pr 'repository) 'nameWithOwner)
                                  "unknown")))
                         (error
                          (message "magit-gh: skipping unusable PR %S: %s"
                                   (map-elt pr 'number) (error-message-string err))
                          nil)))
                     prs)))))))

(defun magit-dash-pr-dashboard--merge-ci (buf repo output)
  "Merge statusCheckRollup, headRefName, state, and mergedAt from OUTPUT into rows.
Updates REPO's rows in BUF.
OUTPUT is `gh pr list' JSON."
  (when (buffer-live-p buf)
    (let ((trimmed (string-trim output)))
      (when (string-prefix-p "[" trimmed)
        (when-let* ((items (json-parse-string trimmed :array-type 'list :object-type 'alist))
                    (table (make-hash-table :test #'eql)))
          (seq-do (lambda (item)
                    (setf (map-elt table (map-elt item 'number)) item))
                  items)
          (with-current-buffer buf
            (let* ((filters magit-dash-pr-dashboard--filters)
                   (conflicts-p (plist-get filters :conflicts))
                   (new-entries
                    (seq-map (lambda (entry)
                               (let* ((id (car entry))
                                      (match (and (equal (plist-get id :repo) repo)
                                                  (map-elt table (plist-get id :number)))))
                                 (if match
                                     (let* ((rollup (map-elt match 'statusCheckRollup))
                                            (head-ref (map-elt match 'headRefName))
                                            (base-ref (map-elt match 'baseRefName))
                                            (st (map-elt match 'state))
                                            (merged-at (map-elt match 'mergedAt))
                                            (mergeable (map-elt match 'mergeable))
                                            (merge-state (map-elt match 'mergeStateStatus))
                                            (pr-data (plist-get id :pr))
                                            (merged (append
                                                     (when rollup (list (cons 'statusCheckRollup rollup)))
                                                     (when head-ref (list (cons 'headRefName head-ref)))
                                                     (when base-ref (list (cons 'baseRefName base-ref)))
                                                     (when st (list (cons 'state st)))
                                                     (when (and merged-at (not (eq merged-at :null)))
                                                       (list (cons 'mergedAt merged-at)))
                                                     (when mergeable (list (cons 'mergeable mergeable)))
                                                     (when merge-state (list (cons 'mergeStateStatus merge-state)))
                                                     pr-data)))
                                       (magit-dash-pr-dashboard--build-entry merged repo))
                                   entry)))
                             tabulated-list-entries)))
              (when conflicts-p
                (setq new-entries
                      (seq-filter (lambda (entry)
                                    (let ((id (car entry)))
                                      (or (string-equal (or (plist-get id :mergeable) "") "CONFLICTING")
                                          (string-equal (or (plist-get id :merge-state-status) "") "DIRTY"))))
                                  new-entries)))
              (setq tabulated-list-entries (magit-dash-pr-dashboard--apply-ci-filter new-entries))
              (magit-dash-pr-dashboard--print))))))))

(defun magit-dash-pr-dashboard--enrich-ci (buf state dir)
  "Fetch CI status and PR metadata per repo and merge it into BUF's rows for STATE.
Uses `gen-run-async'.  Used for aggregate (cross-repo) views, whose
listing endpoint does not return `statusCheckRollup' or `mergedAt'.  DIR is the
working directory for `gh'."
  (when (buffer-live-p buf)
    (let* ((gh-state (if (equal state "closed") "all" state))
           (repos (delete-dups
                   (with-current-buffer buf
                     (seq-remove #'null
                                 (seq-map (lambda (entry) (plist-get (car entry) :repo))
                                          tabulated-list-entries)))))
           (fetch-limit (number-to-string
                         (max 200 (or (buffer-local-value 'magit-dash-pr-dashboard--limit buf)
                                      magit-dash-pr-fetch-limit))))
           (limit (or (bound-and-true-p magit-dash-max-concurrent-ci-jobs)
                      (max 2 (round (* 1.5 (if (fboundp 'num-processors)
                                               (num-processors)
                                             4)))))))
      (if (fboundp 'gen-run-async)
          (gen-run-async
           (lambda (repo done)
             (if (not (buffer-live-p buf))
                  (funcall done)
                (magit-dash-gh--run-process
                 (list "pr" "list" "-R" repo "--state" gh-state
                       "--json" "number,state,mergedAt,statusCheckRollup,headRefName,baseRefName,mergeable,mergeStateStatus" "--limit" fetch-limit)
                 dir
                 (lambda (output)
                   (magit-dash-pr-dashboard--merge-ci buf repo output)
                   (funcall done))
                (lambda (_output _code)
                  (funcall done)))))
           repos
           :concurrency limit
           :delay (or (bound-and-true-p magit-dash-queue-yield-delay) 0.0))
        (seq-do (lambda (repo)
                  (magit-dash-gh--run-process
                   (list "pr" "list" "-R" repo "--state" gh-state
                         "--json" "number,state,mergedAt,statusCheckRollup,headRefName,baseRefName,mergeable,mergeStateStatus" "--limit" fetch-limit)
                   dir
                   (lambda (output) (magit-dash-pr-dashboard--merge-ci buf repo output))
                   (lambda (_output _code) nil)))
                repos)))))

(defconst magit-dash-pr-dashboard--view-fields
  "number,title,state,mergedAt,author,createdAt,updatedAt,comments,reviewDecision,statusCheckRollup,isDraft,url,headRefName,headRefOid,baseRefName,mergeable,mergeStateStatus"
  "JSON fields requested when refreshing a single PR with `gh pr view'.")

(defun magit-dash-pr-dashboard--view-args (key)
  "Return `gh' args viewing the PR identified by KEY, a (REPO . NUMBER) cons."
  (list "pr" "view" (number-to-string (cdr key)) "-R" (car key)
        "--json" magit-dash-pr-dashboard--view-fields
        "--jq" ". + {commentsCount: (.comments | length)} | del(.comments)"))

(defun magit-dash-pr-dashboard--replace-row (buf key output)
  "Replace the row for KEY in BUF using `gh pr view' JSON OUTPUT.
Fields missing from OUTPUT keep their old values.  Under an open-PRs
filter (a nil state means open) a PR that is no longer open is dropped,
as a full refresh would; a missing or null state counts as open.
Malformed OUTPUT is logged and keeps the row."
  (when (and (buffer-live-p buf)
             (string-prefix-p "{" (string-trim output)))
    (with-current-buffer buf
      (if-let* ((fresh (condition-case err
                           (json-parse-string (string-trim output) :object-type 'alist)
                         (error
                          (message "magit-gh: malformed output refreshing %s#%s: %s"
                                   (car key) (cdr key) (error-message-string err))
                          nil)))
                ((listp fresh)))
          (let ((gone-p (and (equal (upcase (or (plist-get magit-dash-pr-dashboard--filters :state)
                                                "open"))
                                    "OPEN")
                             (not (equal (upcase (let ((st (map-elt fresh 'state)))
                                                   (if (stringp st) st "OPEN")))
                                         "OPEN")))))
            (setq tabulated-list-entries
                  (magit-dash-pr-dashboard--apply-ci-filter
                   (seq-keep
                    (lambda (entry)
                      (if (not (equal key (magit-dash-pr-dashboard--entry-key (car entry))))
                          entry
                        (let ((merged (magit-dash-pr-cache-merge-payload
                                       fresh (plist-get (car entry) :pr)))
                              (repo (plist-get (car entry) :repo)))
                          (magit-dash-pr-dashboard--cache-row fresh repo)
                          (unless gone-p
                            (magit-dash-pr-dashboard--build-entry merged repo)))))
                    tabulated-list-entries)))
            (magit-dash-pr-dashboard--print))
        nil))))

;;;; Status fill and per-row status refresh

(defun magit-dash-pr-dashboard--status-worse-p (new old)
  "Return non-nil when truncated status NEW reports less than truncated OLD."
  (seq-some (lambda (key)
              (< (or (plist-get new key) 0) (or (plist-get old key) 0)))
            '(:threads-open :approvals)))

(defun magit-dash-pr-dashboard--store-status (repo number status pr)
  "Hold and cache STATUS for REPO and NUMBER; return non-nil when it was stored.
PR is the row's alist, used as the cache payload when none is cached yet.
Nothing is written (and no cache file is read) when STATUS equals the
held status.  Nothing is written either when STATUS is invalid, has no
meaningful field, is error-shaped, or is truncated and would replace a
complete or a better truncated status.  Only `:status' is rewritten, so a
stale record stays stale.  With no cached record, the record gets the
row's original-case repo rather than the downcased key."
  (let ((held (magit-dash-pr-dashboard--status repo number)))
    (when (and (magit-dash-pr-dashboard--meaningful-status-p status)
               (not (equal status held)))
      (let* ((prior (magit-dash-pr-cache-get repo number))
             (old (or held (plist-get prior :status)))
             (key (magit-dash-pr-dashboard--norm-key repo number))
             (row-repo (or (seq-some
                            (lambda (entry)
                              (when (equal key (magit-dash-pr-dashboard--entry-key (car entry)))
                                (plist-get (car entry) :repo)))
                            (and (not prior) tabulated-list-entries))
                           repo)))
        (when (and (not (equal status old))
                   (not (and old
                             (plist-get status :truncated)
                             (or (not (plist-get old :truncated))
                                 (magit-dash-pr-dashboard--status-worse-p status old)))))
          (magit-dash-pr-dashboard--set-status repo number status)
          (magit-dash-pr-cache-put-status row-repo number status
                                          (or (plist-get prior :payload) pr)
                                          prior)
          t)))))

(defun magit-dash-pr-dashboard--rebuild-keys (keys)
  "Rebuild the rows whose (REPO . NUMBER) key is in KEYS and reprint."
  (setq tabulated-list-entries
        (seq-map (lambda (entry)
                   (let ((id (car entry)))
                     (if (member (magit-dash-pr-dashboard--entry-key id) keys)
                         (magit-dash-pr-dashboard--build-entry
                          (plist-get id :pr) (plist-get id :repo)
                          (plist-get id :fetched-at))
                       entry)))
                 tabulated-list-entries))
  (magit-dash-pr-dashboard--print))

(defun magit-dash-pr-dashboard--fill-statuses (buf prs)
  "Fill the rows of BUF from PRS, status plists from the async collection.
Match on (REPO . NUMBER); PRs with no row are ignored, rows without a
match are left blank."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((rows (map-into (seq-map (lambda (entry)
                                       (cons (magit-dash-pr-dashboard--entry-key (car entry))
                                             (plist-get (car entry) :pr)))
                                     tabulated-list-entries)
                            '(hash-table :test equal)))
            (changed nil))
        (seq-do (lambda (pr)
                  (let ((key (magit-dash-pr-dashboard--norm-key
                              (plist-get pr :repo) (plist-get pr :number))))
                    (when (and (map-contains-key rows key)
                               (magit-dash-pr-dashboard--store-status
                                (car key) (cdr key)
                                (magit-dash-pr--status-from-pr pr)
                                (map-elt rows key)))
                      (push key changed))))
                prs)
        (when changed
          (magit-dash-pr-dashboard--rebuild-keys changed))))))

(defun magit-dash-pr-dashboard--collect-statuses (buf dir)
  "Collect review status for the author's open PRs into BUF's rows.
Rows fill as results arrive.  Cancels a collection still running for BUF."
  (require 'magit-dash-pr-status)
  (with-current-buffer buf
    (magit-dash-pr-dashboard--cancel-status-collection)
    (setq magit-dash-pr-dashboard--status-cancel
          (magit-dash-pr-status-collect-all-async
           (lambda (prs)
             (magit-dash-pr-dashboard--fill-statuses buf prs))
           (lambda (err)
             (message "magit-gh: review status unavailable: %s" err))
           dir
           #'ignore
           (lambda (prs _notes _pending)
             (magit-dash-pr-dashboard--fill-statuses buf prs))))))

(defun magit-dash-pr-dashboard--refresh-status (buf key pr dir &optional done)
  "Fetch the status of KEY (REPO . NUMBER) for BUF and store it with PR.
On failure the existing status, cached and displayed, is left untouched.
DONE, a function of no arguments, runs once the fetch has finished either way."
  (magit-dash-pr-status-fetch-one
   (car key) (cdr key) dir
   (lambda (status)
     (unwind-protect
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (magit-dash-pr-dashboard--store-status (car key) (cdr key) status pr)
               (magit-dash-pr-dashboard--rebuild-keys (list key)))))
       (when done
         (funcall done))))
   (lambda (err)
     (message "magit-gh: could not refresh status of %s#%s: %s"
              (car key) (cdr key) err)
     (when done
       (funcall done)))))

(defcustom magit-dash-pr-refresh-max-processes 4
  "Most `gh' processes `magit-dash-pr-dashboard-refresh-entries' runs at once."
  :type 'integer
  :group 'magit-dash)

(defun magit-dash-pr--run-limited (tasks limit)
  "Run TASKS at most LIMIT at a time.
Each task is a function taking one DONE callback it must call when its
work has finished.  A task that signals is messaged and counts as done."
  (let ((queue (copy-sequence tasks))
        (active 0)
        (pump nil))
    (setq pump
          (lambda ()
            (while (and queue (< active (max 1 limit)))
              (let ((task (pop queue))
                    (finished nil))
                (setq active (1+ active))
                (let ((done (lambda ()
                              (unless finished
                                (setq finished t)
                                (setq active (1- active))
                                (funcall pump)))))
                  (condition-case err
                      (funcall task done)
                    (error
                     (message "magit-gh: refresh task failed: %s"
                              (error-message-string err))
                     (funcall done))))))))
    (funcall pump)))

(defun magit-dash-pr-dashboard-refresh-entries (&optional entries)
  "Refresh only ENTRIES (entry plists) from GitHub, one `gh pr view' each.
Without ENTRIES, refresh the marked PRs, else the PR at point.  No PR
discovery query runs and no other row changes."
  (interactive)
  (unless (derived-mode-p 'magit-dash-pr-dashboard-mode)
    (user-error "Not in a PR dashboard buffer"))
  (let* ((entries (or entries
                      (magit-dash-pr-dashboard--marked-or-at-point-entries)
                      (list (magit-dash-pr-dashboard--entry-at-point))))
         (buf (current-buffer))
         (dir (or (ignore-errors (magit-dash-gh--repo-dir))
                  (expand-file-name "~"))))
    (message "magit-gh: refreshing %d PR(s)..." (length entries))
    (require 'magit-dash-pr-status)
    (magit-dash-pr--run-limited
     (seq-mapcat
      (lambda (entry)
        (let ((key (magit-dash-pr-dashboard--entry-key entry)))
          (list
           (lambda (done)
             (magit-dash-gh--run-process
              (magit-dash-pr-dashboard--view-args key) dir
              (lambda (output)
                (unwind-protect
                    (magit-dash-pr-dashboard--replace-row buf key output)
                  (funcall done)))
              (lambda (output _code)
                (message "magit-gh: could not refresh %s#%s: %s"
                         (car key) (cdr key) (string-trim output))
                (funcall done))))
           (lambda (done)
             (magit-dash-pr-dashboard--refresh-status
              buf key (plist-get entry :pr) dir done)))))
      entries)
     magit-dash-pr-refresh-max-processes)))

(defun magit-dash-pr-dashboard-refresh ()
  "Fetch PRs matching current filters and repopulate the PR table.
When invoked outside `magit-dash-pr-dashboard-mode', safely redirects
to the PR dashboard buffer if live, preventing accidental corruption of
other buffers."
  (interactive)
  (if (not (derived-mode-p 'magit-dash-pr-dashboard-mode))
      (if-let* ((dash-buf (get-buffer (or (bound-and-true-p magit-dash-pr-buffer-name)
                                          "*magit-dash-prs*"))))
          (if (and (buffer-live-p dash-buf)
                   (with-current-buffer dash-buf (derived-mode-p 'magit-dash-pr-dashboard-mode)))
              (with-current-buffer dash-buf
                (magit-dash-pr-dashboard-refresh))
            (when (called-interactively-p 'interactive)
              (user-error "Buffer is not in magit-dash-pr-dashboard-mode")))
        (when (called-interactively-p 'interactive)
          (user-error "No PR dashboard buffer exists")))
    (clrhash magit-dash-pr-dashboard--reviewed-heads)
    (let* ((filters magit-dash-pr-dashboard--filters)
           (args (magit-dash-pr-dashboard--build-args
                  filters magit-dash-pr-dashboard--limit))
           (buf (current-buffer))
           (dir (or (ignore-errors (magit-dash-gh--repo-dir))
                    (expand-file-name "~"))))
      (unless tabulated-list-entries
        (setq tabulated-list-entries
              (magit-dash-pr-dashboard--cached-entries filters))
        (when tabulated-list-entries
          (magit-dash-pr-dashboard--print)))
      (magit-dash-pr--prime-account-logins)
      (message "magit-gh: fetching PRs...")
      (magit-dash-gh--run-process
       args dir
       (lambda (output)
         (cond
          ((not (string-prefix-p "[" (string-trim output)))
           (message "magit-gh: discovery returned no usable result; kept cached rows"))
          ((and (buffer-live-p buf)
                (with-current-buffer buf (derived-mode-p 'magit-dash-pr-dashboard-mode)))
           (with-current-buffer buf
             (setq tabulated-list-entries
                   (or (magit-dash-pr-dashboard--parse-output output filters) nil))
             (seq-do (lambda (entry)
                       (magit-dash-pr-dashboard--cache-row
                        (plist-get (car entry) :pr)
                        (plist-get (car entry) :repo)
                        (not (plist-get filters :repo))))
                     tabulated-list-entries)
             (magit-dash-pr-dashboard--evict-ghosts
              filters tabulated-list-entries output)
             (setq tabulated-list-format
                   (magit-dash-pr-dashboard--build-format
                    (window-width (get-buffer-window buf t))
                    tabulated-list-entries))
             (tabulated-list-init-header)
             (magit-dash-pr-dashboard--print)
             (message "magit-gh: %d PR(s)"
                      (length tabulated-list-entries)))
           (unless (plist-get filters :repo)
             (magit-dash-pr-dashboard--enrich-ci
              buf (or (plist-get filters :state) "open") dir))
           (magit-dash-pr-dashboard--collect-statuses buf dir))))))))

(defun magit-dash-pr-dashboard--entry-at-point ()
  "Return the PR entry plist at point or signal `user-error'."
  (or (tabulated-list-get-id)
      (user-error "No pull request at point")))

(defun magit-dash-pr-dashboard--rebuild-entries ()
  "Rebuild `tabulated-list-entries' in place to reflect current marks."
  (when (derived-mode-p 'tabulated-list-mode)
    (setq tabulated-list-entries
          (seq-map (lambda (entry)
                     (let ((id (car entry)))
                       (magit-dash-pr-dashboard--build-entry
                        (plist-get id :pr)
                        (plist-get id :repo)
                        (plist-get id :fetched-at))))
                   tabulated-list-entries))
    (magit-dash-pr-dashboard--print)))

(defun magit-dash-pr-dashboard-toggle-mark ()
  "Toggle mark on the pull request at point and advance to the next line."
  (interactive)
  (let* ((entry (magit-dash-pr-dashboard--entry-at-point))
         (key (magit-dash-pr-dashboard--entry-key entry)))
    (setq magit-dash-pr-dashboard--marked-entries
          (if (member key magit-dash-pr-dashboard--marked-entries)
              (delete key magit-dash-pr-dashboard--marked-entries)
            (cons key magit-dash-pr-dashboard--marked-entries)))
    (magit-dash-pr-dashboard--rebuild-entries)
    (forward-line 1)))

(defun magit-dash-pr-dashboard-mark ()
  "Mark the pull request at point and advance to the next line."
  (interactive)
  (let* ((entry (magit-dash-pr-dashboard--entry-at-point))
         (key (magit-dash-pr-dashboard--entry-key entry)))
    (unless (member key magit-dash-pr-dashboard--marked-entries)
      (push key magit-dash-pr-dashboard--marked-entries))
    (magit-dash-pr-dashboard--rebuild-entries)
    (forward-line 1)))

(defun magit-dash-pr-dashboard-unmark ()
  "Unmark the pull request at point and advance to the next line."
  (interactive)
  (let* ((entry (magit-dash-pr-dashboard--entry-at-point))
         (key (magit-dash-pr-dashboard--entry-key entry)))
    (setq magit-dash-pr-dashboard--marked-entries
          (delete key magit-dash-pr-dashboard--marked-entries))
    (magit-dash-pr-dashboard--rebuild-entries)
    (forward-line 1)))

(defun magit-dash-pr-dashboard-unmark-all ()
  "Clear all marks in the pull request dashboard."
  (interactive)
  (setq magit-dash-pr-dashboard--marked-entries nil)
  (magit-dash-pr-dashboard--rebuild-entries)
  (message "magit-gh: cleared all marks"))

(defun magit-dash-pr-dashboard-mark-all ()
  "Mark all pull requests currently listed in the dashboard."
  (interactive)
  (setq magit-dash-pr-dashboard--marked-entries
        (seq-map (lambda (entry)
                   (magit-dash-pr-dashboard--entry-key (car entry)))
                 tabulated-list-entries))
  (magit-dash-pr-dashboard--rebuild-entries)
  (message "magit-gh: marked %d PR(s)" (length magit-dash-pr-dashboard--marked-entries)))

(defun magit-dash-pr-dashboard--marked-or-at-point-entries ()
  "Return list of marked PR entry plists, or nil if no entries are marked."
  (when (and (bound-and-true-p magit-dash-pr-dashboard--marked-entries)
             tabulated-list-entries)
    (seq-filter #'magit-dash-pr-dashboard--entry-marked-p
                (seq-map #'car tabulated-list-entries))))

(defun magit-dash-pr-dashboard--pr-url (&optional entry)
  "Return the pull request URL for ENTRY (or entry at point).
ENTRY may be an entry plist, a PR alist/plist, a URL string, or
nil (which resolves to the entry at point)."
  (cond
   ((and (stringp entry) (string-match-p "\\`https?://" entry))
    entry)
   (t
    (let* ((ent (or entry (magit-dash-pr-dashboard--entry-at-point)))
           (pr (or (plist-get ent :pr) ent))
           (url (or (map-elt pr 'url)
                    (map-elt pr :url)
                    (and (listp pr) (plist-get pr :url))
                    (and (listp pr) (cdr (assq 'url pr))))))
      (or url
          (let ((repo (or (plist-get ent :repo)
                          (map-elt pr 'repo)
                          (map-elt pr :repo)))
                (num (or (plist-get ent :number)
                         (map-elt pr 'number)
                         (map-elt pr :number))))
            (when (and repo num)
              (format "https://github.com/%s/pull/%s" repo num))))))))

(defun magit-dash-pr-dashboard-open-browser ()
  "Open the pull request at point in a web browser."
  (interactive)
  (when-let* ((url (magit-dash-pr-dashboard--pr-url)))
    (browse-url url)))

;;;###autoload
(defun magit-dash-pr-code-review (url)
  "Open a `code-review' buffer for pull request URL.
Signals `user-error' if URL is missing or if `code-review'
cannot be loaded."
  (interactive
   (list (or (and (derived-mode-p 'magit-dash-pr-dashboard-mode)
                  (ignore-errors (magit-dash-pr-dashboard--pr-url)))
             (read-string "Pull request URL: "))))
  (unless (and (stringp url) (not (string-empty-p url)))
    (user-error "A pull request URL is required"))
  (unless (magit-dash-pr-code-review-available-p)
    (user-error "Package `code-review' is not available"))
  (code-review-start url))

;;;###autoload
(defun magit-dash-pr-dashboard-code-review (&optional entry)
  "Open a `code-review' buffer for the pull request at point (or ENTRY)."
  (interactive)
  (let ((url (magit-dash-pr-dashboard--pr-url entry)))
    (unless url
      (user-error "No pull request URL found for entry"))
    (magit-dash-pr-code-review url)))

;;;###autoload
(defalias 'magit-dash-gh-pr-dashboard-code-review #'magit-dash-pr-dashboard-code-review)

;;;###autoload
(defun magit-dash-pr-dashboard-diff-review (&optional entry)
  "Open a native-buffer `diff-review' session for the pull request at point (or ENTRY).
Deprecation note: `c r' used to open `code-review' and now opens
`diff-review' like `c v'; `code-review' stays on `c b' when installed.
`c r' remains bound for now but will be removed in a future release."
  (interactive)
  (require 'magit-dash-review)
  (let* ((ent (or entry (magit-dash-pr-dashboard--entry-at-point)))
         (repo (and (listp ent) (plist-get ent :repo)))
         (local-path (and repo (magit-dash-pr-dashboard--find-local-path repo))))
    (unless ent
      (user-error "No pull request entry at point"))
    (magit-dash-review-pr ent (or local-path default-directory))))

;;;###autoload
(defalias 'magit-dash-gh-pr-dashboard-diff-review #'magit-dash-pr-dashboard-diff-review)

(defun magit-dash-pr-dashboard-set-filter ()
  "Interactively set one PR dashboard filter field."
  (interactive)
  (let* ((current magit-dash-pr-dashboard--filters)
         (field (completing-read "Filter field: "
                                 '("state" "author" "repo" "org") nil t))
         (value (pcase field
                  ("state"
                   (completing-read "State: " '("open" "closed") nil t))
                  ("author"
                   (read-string "Author (@me for current user): "
                                (or (plist-get current :author) "@me")))
                  ("repo"
                   (read-string "Repo (OWNER/NAME, empty to clear): "
                                (or (plist-get current :repo) "")))
                  ("org"
                   (read-string "Org (empty to clear): "
                                (or (plist-get current :org) "")))))
         (key (intern (format ":%s" field)))
         (new-val (if (string-empty-p value) nil value)))
    (setq magit-dash-pr-dashboard--filters
          (plist-put (copy-sequence current) key new-val))
    (magit-dash-pr-dashboard-refresh)))

(defvar magit-dash-pr-view-history nil
  "History list for `magit-dash-pr-dashboard-switch-view'.")

(defun magit-dash-pr-dashboard-switch-view (&optional view-key)
  "Switch the PR dashboard to a preset view (VIEW-KEY).
Interactively prompts with available PR views."
  (interactive)
  (let* ((candidates
          '(("all"          . "All open PRs (aggregated across tracked repos)")
            ("my-prs"       . "Authored by me (@me)")
            ("needs-review" . "Review requested (@me)")
            ("repo"         . "Filtered by repository…")
            ("conflicts"    . "PRs with merge conflicts")
            ("closed"       . "Closed / Merged pull requests")))
         (choice (or view-key
                     (if (fboundp 'acr-completing-read)
                         (acr-completing-read
                          "Switch PR view: "
                          (mapcar #'car candidates)
                          :annotation-fn (lambda (k) (concat "  " (alist-get k candidates nil nil #'equal)))
                          :history 'magit-dash-pr-view-history)
                       (completing-read
                        "Switch PR view: "
                        (mapcar (lambda (c) (format "%-14s %s" (car c) (cdr c))) candidates)
                        nil t)))))
    (let ((key (if (string-match "\\`\\([^ ]+\\)" choice)
                   (match-string 1 choice)
                 choice)))
      (pcase key
        ("all"
         (setq magit-dash-pr-dashboard--filters
               (list :state "open" :author nil :repo nil :org nil)))
        ("my-prs"
         (setq magit-dash-pr-dashboard--filters
               (list :state "open" :author "@me" :repo nil :org nil)))
        ("needs-review"
         (setq magit-dash-pr-dashboard--filters
               (list :state "open" :author nil :repo nil :org nil :review-requested "@me")))
        ("repo"
         (let* ((repo-list (when (boundp 'magit-dash-repo-list)
                             (seq-filter #'magit-dash-repo-upstream-repo magit-dash-repo-list)))
                (choices (if repo-list
                             (mapcar #'magit-dash-repo-upstream-repo repo-list)
                           nil))
                (selected (if choices
                              (completing-read "Select repository: " choices nil t)
                            (read-string "Repository (OWNER/NAME): "))))
           (setq magit-dash-pr-dashboard--filters
                 (list :state "open" :author nil :repo selected :org nil))))
        ("conflicts"
         (setq magit-dash-pr-dashboard--filters
               (list :state "open" :conflicts t :author nil :repo nil :org nil)))
        ("closed"
         (setq magit-dash-pr-dashboard--filters
               (list :state "closed" :author nil :repo nil :org nil))))
      (magit-dash-pr-dashboard-refresh))))

(defun magit-dash-pr-dashboard-quit ()
  "Quit the PR dashboard.
Deletes the window (restoring window layout if split beneath) and
buries the buffer."
  (interactive)
  (let ((buf (current-buffer))
        (win (selected-window)))
    (if (> (length (window-list)) 1)
        (progn
          (bury-buffer buf)
          (delete-window win))
      (bury-buffer buf))))

(defun magit-dash-pr-dashboard-toggle-hide-failing ()
  "Toggle hiding of pull requests whose CI is failing, then refresh."
  (interactive)
  (setq magit-dash-pr-dashboard--hide-failing
        (not magit-dash-pr-dashboard--hide-failing))
  (message "PR dashboard: failing CI %s"
           (if magit-dash-pr-dashboard--hide-failing "hidden" "shown"))
  (magit-dash-pr-dashboard-refresh))

(defun magit-dash-pr-dashboard-fetch-more ()
  "Double the number of PRs requested from GitHub and refresh."
  (interactive)
  (setq magit-dash-pr-dashboard--limit
        (* 2 (or magit-dash-pr-dashboard--limit magit-dash-pr-fetch-limit)))
  (message "PR dashboard: fetching up to %d PRs" magit-dash-pr-dashboard--limit)
  (magit-dash-pr-dashboard-refresh))

(defun magit-dash-pr-dashboard-clear-filters ()
  "Reset all dashboard filters to their defaults (open PRs) and refresh."
  (interactive)
  (setq-local magit-dash-pr-dashboard--filters
              (list :state "open" :author nil :repo nil :org nil))
  (setq magit-dash-pr-dashboard--hide-failing nil)
  (magit-dash-pr-dashboard-refresh))

(defconst magit-dash-pr-buffer-name "*magit-dash-pr*"
  "Buffer name for the pull request dashboard.")

(defun magit-dash-pr--resolve-dashboard-default-directory (repo-slug &optional context-dir)
  "Resolve `default-directory' for repository-scoped PR dashboard of REPO-SLUG.
If CONTEXT-DIR is within a worktree of that repository, return it.
Otherwise return the repository main checkout root (or first linked worktree)."
  (when repo-slug
    (when-let* ((repo-path (magit-dash-pr-dashboard--find-local-path repo-slug)))
      (let* ((repo-dir (file-name-as-directory (expand-file-name repo-path)))
             (ctx (and context-dir (file-name-as-directory (expand-file-name context-dir))))
             (wts (ignore-errors (magit-dash-pr--git-worktrees repo-dir))))
        (or
         ;; If opened from within a worktree of that repo, use that worktree
         (and ctx
              (seq-some
               (lambda (wt)
                 (when-let* ((path (alist-get 'path wt)))
                   (let ((wt-dir (file-name-as-directory (expand-file-name path))))
                     (and (file-directory-p wt-dir)
                          (string-prefix-p wt-dir ctx)
                          wt-dir))))
               wts))
         ;; Main checkout if available
         (and (file-directory-p repo-dir) repo-dir)
         ;; First linked worktree if main checkout unavailable
         (seq-some
          (lambda (wt)
            (when-let* ((path (alist-get 'path wt)))
              (let ((wt-dir (file-name-as-directory (expand-file-name path))))
                (and (file-directory-p wt-dir) wt-dir))))
          wts))))))

;;;###autoload
(defun magit-dash-pr (&optional repo)
  "Open the GitHub pull request dashboard buffer.
When opened from a repository dashboard window, splits the window beneath.
Aggregates all open PRs by default across repositories, or for REPO
when provided."
  (interactive)
  (magit-dash-gh--check-gh)
  (let* ((orig-buf (current-buffer))
         (orig-dir default-directory)
         (orig-win (selected-window))
         (from-dash (or (derived-mode-p 'magit-dash-mode)
                        (string= (buffer-name orig-buf) (or (bound-and-true-p magit-dash-buffer-name) "*magit-dash-repos*"))))
         (buf (get-buffer-create magit-dash-pr-buffer-name)))
    (with-current-buffer buf
      (magit-dash-pr-dashboard-mode)
      (when repo
        (setq magit-dash-pr-dashboard--filters
              (plist-put (copy-sequence magit-dash-pr-dashboard--filters) :repo repo))
        (when-let* ((resolved-dir (magit-dash-pr--resolve-dashboard-default-directory repo orig-dir)))
          (setq-local default-directory resolved-dir)))
      (magit-dash-pr-dashboard-refresh))
    (if (and from-dash (window-live-p orig-win))
        (let ((pr-win (or (get-buffer-window buf)
                          (split-window orig-win (max 10 (/ (window-total-height orig-win) 2)) 'below))))
          (set-window-buffer pr-win buf)
          (select-window pr-win))
      (pop-to-buffer buf))))

;;;###autoload
(defalias 'magit-dash-pr-dashboard-open #'magit-dash-pr)

(defun magit-dash-pr-dashboard--open-with-filters (filters)
  "Open the PR dashboard buffer showing pull requests matching FILTERS."
  (magit-dash-gh--check-gh)
  (let* ((orig-dir default-directory)
         (buf (get-buffer-create magit-dash-pr-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'magit-dash-pr-dashboard-mode)
        (magit-dash-pr-dashboard-mode))
      (setq-local magit-dash-pr-dashboard--filters filters)
      (when-let* ((repo (plist-get filters :repo))
                  (resolved-dir (magit-dash-pr--resolve-dashboard-default-directory repo orig-dir)))
        (setq-local default-directory resolved-dir))
      (magit-dash-pr-dashboard-refresh))
    (pop-to-buffer buf)))

(defun magit-dash-pr-dashboard--current-repo-slug ()
  "Return the OWNER/NAME slug for the repo at point or `default-directory'.
Signals `user-error' when no GitHub repository can be determined."
  (let* ((repo-dir (if (magit-dash--repo-at-point-p)
                       (magit-dash-repo-path (magit-dash--repo-at-point))
                     (or (magit-toplevel) (user-error "Not inside a git repository"))))
         (default-directory repo-dir)
         (info (magit-dash-gh--repo-info))
         (owner (plist-get info :owner))
         (repo  (plist-get info :repo)))
    (unless (and owner repo)
      (user-error "Could not determine GitHub repository for %s" repo-dir))
    (format "%s/%s" owner repo)))

;;;###autoload
(defun magit-dash-pr-dash-all ()
  "Open the PR dashboard showing all open pull requests across repositories."
  (interactive)
  (magit-dash-pr-dashboard--open-with-filters
   (list :state "open" :author nil :repo nil :org nil)))

;;;###autoload
(defun magit-dash-pr-dash-mine ()
  "Open the PR dashboard showing your open pull requests across repositories."
  (interactive)
  (magit-dash-pr-dashboard--open-with-filters
   (list :state "open" :author "@me" :repo nil :org nil)))

;;;###autoload
(defun magit-dash-pr-dash-here ()
  "Open the PR dashboard showing open pull requests for the repo at point."
  (interactive)
  (magit-dash-pr-dashboard--open-with-filters
   (list :state "open" :author nil
         :repo (magit-dash-pr-dashboard--current-repo-slug) :org nil)))

;;;###autoload
(defun magit-dash-pr-dash-mine-here ()
  "Open the PR dashboard showing your open pull requests for the repo at point."
  (interactive)
  (magit-dash-pr-dashboard--open-with-filters
   (list :state "open" :author "@me"
         :repo (magit-dash-pr-dashboard--current-repo-slug) :org nil)))

;;;###autoload
(defun magit-dash-pr-dash-conflicts ()
  "Open the PR dashboard showing pull requests with merge conflicts."
  (interactive)
  (magit-dash-pr-dashboard--open-with-filters
   (list :state "open" :conflicts t :author nil :repo nil :org nil)))

(defun magit-dash-pr--git-worktrees (repo-dir)
  "Return a list of alists ((path . PATH) (branch . BRANCH) (head . HEAD)).
Checked for REPO-DIR."
  (magit-dash-gh--git-worktrees repo-dir))

(defvar magit-dash-pr--account-logins (make-hash-table :test #'equal)
  "Cache of gh account login by (USER . HOST), or (:default . HOST).
A value is a login string, or a (:pending . TIME) / (:failed . TIME) marker
for a lookup in flight or recently failed.")

(defvar magit-dash-pr--repo-hosts (make-hash-table :test #'equal)
  "Cache of the git remote host by repository directory.")

(defconst magit-dash-pr--login-timeout 20
  "Seconds a default-login lookup may run before it is killed and marked failed.")

(defconst magit-dash-pr--login-retry-seconds 300
  "Seconds a pending or failed default-login lookup suppresses a new one.")

(defun magit-dash-pr-reset-account-logins ()
  "Forget all cached gh logins and remote hosts.
Call this after switching the active `gh auth' account or changing a
repository's configured account, since the default login is cached per host."
  (interactive)
  (clrhash magit-dash-pr--account-logins)
  (clrhash magit-dash-pr--repo-hosts)
  (magit-dash-gh-reset-token-cache))

(defun magit-dash-pr--repo-host (repo)
  "Return the host of REPO's origin remote, or \"github.com\".
The \"github.com\" fallback is not cached when the git lookup fails, so a
later call can still find the real host."
  (if (not (and (stringp repo) (file-directory-p repo)))
      "github.com"
    (or (map-elt magit-dash-pr--repo-hosts repo)
        (when-let* ((url (ignore-errors
                          (with-temp-buffer
                            (let ((default-directory repo))
                              (when (zerop (call-process
                                            "git" nil (list (current-buffer) nil) nil
                                            "config" "--get" "remote.origin.url"))
                                (string-trim (buffer-string)))))))
                    ((string-match
                      "\\`\\(?:[[:alpha:]+]+://\\)?\\(?:[^@/]+@\\)?\\([^:/]+\\)[:/]"
                      url)))
          (setf (map-elt magit-dash-pr--repo-hosts repo) (match-string 1 url)))
        "github.com")))

(defun magit-dash-pr--account-login (repo)
  "Return the gh login in use for REPO, or nil when not known yet.
Never blocks: a configured account supplies its own login, otherwise the
cached login for the default account on REPO's host is used.  When that is
not cached and REPO is an existing directory, one asynchronous lookup primes
the cache; a pending or failed lookup is not retried for
`magit-dash-pr--login-retry-seconds'."
  (let* ((account (magit-dash-gh--repo-account repo))
         (host (or (plist-get account :host) (magit-dash-pr--repo-host repo)))
         (key (if account
                  (cons (plist-get account :user) host)
                (cons :default host)))
         (cached (map-elt magit-dash-pr--account-logins key)))
    (cond
     ((stringp cached)
      cached)
     (account
      (setf (map-elt magit-dash-pr--account-logins key)
            (plist-get account :user)))
     ((and (stringp repo) (file-directory-p repo)
           (or (null cached)
               (> (float-time (time-since (cdr cached)))
                  magit-dash-pr--login-retry-seconds)))
      (setf (map-elt magit-dash-pr--account-logins key)
            (cons :pending (current-time)))
      (let* ((settled nil)
             (handle (list nil))
             (timer nil)
             (settle (lambda (value)
                       (unless settled
                         (setq settled t)
                         (when (timerp timer)
                           (cancel-timer timer))
                         (setf (map-elt magit-dash-pr--account-logins key) value)))))
        (setq timer (run-with-timer
                     magit-dash-pr--login-timeout nil
                     (lambda ()
                       (unless settled
                         (funcall settle (cons :failed (current-time)))
                         (magit-dash-gh--handle-cancel handle)))))
        (condition-case nil
            (magit-dash-gh--run-process
             (list "api" "user" "--hostname" host "--jq" ".login") repo
             (lambda (output)
               (let ((login (string-trim output)))
                 (funcall settle (if (string-empty-p login)
                                     (cons :failed (current-time))
                                   login))))
             (lambda (&rest _)
               (funcall settle (cons :failed (current-time))))
             nil (lambda () settled) handle)
          (error
           (funcall settle (cons :failed (current-time))))))
      nil))))

(defun magit-dash-pr--prime-account-logins (&optional dirs)
  "Start the gh login lookup for each of DIRS, once per repository.
DIRS defaults to the paths of `magit-dash-repo-list'.  Lookups already
cached or in flight are not repeated, so this is cheap to call on every
refresh; it lets own PRs be named pr-N from their first use."
  (seq-do #'magit-dash-pr--account-login
          (seq-uniq (or dirs
                        (and (boundp 'magit-dash-repo-list)
                             (seq-map #'magit-dash-repo-path magit-dash-repo-list))))))

(defun magit-dash-pr-own-p (author-login repo)
  "Return non-nil when AUTHOR-LOGIN is the gh account login in use for REPO.
An unknown AUTHOR-LOGIN or account login counts as not own."
  (when-let* (((stringp author-login))
              ((not (string-empty-p author-login)))
              (login (magit-dash-pr--account-login repo)))
    (string-equal-ignore-case author-login login)))

(defun magit-dash-pr-worktree-name (number own-p)
  "Return the worktree directory name for PR NUMBER: pr-N when OWN-P, else cr-N."
  (format (if own-p "pr-%d" "cr-%d") number))

(defun magit-dash-pr--entry-worktree-name (entry repo-dir)
  "Return the worktree name for PR ENTRY in REPO-DIR, by author ownership."
  (magit-dash-pr-worktree-name
   (plist-get entry :number)
   (magit-dash-pr-own-p
    (or (map-nested-elt (plist-get entry :pr) '(author login))
        (plist-get entry :author))
    repo-dir)))

(defun magit-dash-pr--compute-worktree-path (repo-dir name &optional entry)
  "Compute projected worktree path for NAME under REPO-DIR.
Uses `magit-dash-pr-worktree-path-format'."
  (let* ((repo-dir (expand-file-name repo-dir))
         (repo-base (file-name-nondirectory (directory-file-name repo-dir)))
         (parent (file-name-directory (directory-file-name repo-dir)))
         (fmt magit-dash-pr-worktree-path-format))
    (cond
     ((functionp fmt)
      (funcall fmt repo-dir name entry))
     ((eq fmt 'dot-wt)
      (expand-file-name (concat ".wt/" name) repo-dir))
     ((stringp fmt)
      (let ((s fmt))
        (setq s (replace-regexp-in-string "%r" (directory-file-name repo-dir) s t t))
        (setq s (replace-regexp-in-string "%b" repo-base s t t))
        (setq s (replace-regexp-in-string "%n" name s t t))
        (expand-file-name s repo-dir)))
     ;; default 'sibling
     (t
      (expand-file-name (format "%s-%s" repo-base name) parent)))))

(defun magit-dash-pr--find-existing-worktree (repo-dir pr-num &optional branch-name preferred-name)
  "Find existing worktree directory for PR-NUM and BRANCH-NAME under REPO-DIR.
Either the pr-N or cr-N name is accepted.  When both exist, the one named
PREFERRED-NAME wins, otherwise pr-N before cr-N.  The main checkout is never
returned, and BRANCH-NAME is ignored when it is generic (main, master,
patch-1 ...; see `magit-dash-pr--generic-branch-p'), because a fork PR from
such a branch says nothing about which worktree is its checkout."
  (when repo-dir
    (let* ((repo-dir (expand-file-name repo-dir))
           (repo-base (file-name-nondirectory (directory-file-name repo-dir)))
           (parent (file-name-directory (directory-file-name repo-dir)))
           (pr-names (let ((names (list (format "pr-%d" pr-num) (format "cr-%d" pr-num))))
                       (if (member preferred-name names)
                           (cons preferred-name (remove preferred-name names))
                         names)))
           (branch-name (and (not (magit-dash-pr--generic-branch-p branch-name repo-dir))
                             branch-name))
           (git-wts (seq-filter
                     (lambda (wt)
                       (when-let* ((path (alist-get 'path wt)))
                         (and (file-directory-p path)
                              (not (magit-dash-pr--path-equal-p path repo-dir)))))
                     (ignore-errors (magit-dash-pr--git-worktrees repo-dir)))))
      (or
       ;; 1. Git worktrees named for the PR, in preference order
       (seq-some
        (lambda (n)
          (seq-some
           (lambda (wt)
             (let ((path (alist-get 'path wt))
                   (br (alist-get 'branch wt)))
               (when (or (member br (list n (concat "wt/" n)))
                         (string-suffix-p (concat "/.wt/" n) path)
                         (string-suffix-p (concat "/" repo-base "-" n) path))
                 path)))
           git-wts))
        pr-names)
       ;; 2. Git worktrees by head branch name
       (when branch-name
         (seq-some
          (lambda (wt)
            (let ((path (alist-get 'path wt))
                  (br (alist-get 'branch wt)))
              (when (or (member br (list branch-name (concat "wt/" branch-name)))
                        (string-suffix-p (concat "/.wt/" branch-name) path)
                        (string-suffix-p (concat "/" repo-base "-" branch-name) path))
                path)))
          git-wts))
       ;; 3. Candidate directories on disk
       (seq-some
        (lambda (dir)
          (and dir (file-directory-p dir) dir))
        (append
         (seq-map (lambda (n) (expand-file-name (concat ".wt/" n) repo-dir)) pr-names)
         (seq-map (lambda (n) (expand-file-name (format "%s-%s" repo-base n) parent)) pr-names)
         (when branch-name
           (list (expand-file-name (concat ".wt/" branch-name) repo-dir)
                 (expand-file-name (format "%s-%s" repo-base branch-name) parent)))))))))

(defun magit-dash-pr--worktree-acceptable-p (repo-dir path pr-num &optional entry)
  "Return non-nil when PATH is the pr-N or cr-N worktree location for PR-NUM.
ENTRY is passed to `magit-dash-pr--compute-worktree-path', so a function
valued `magit-dash-pr-worktree-path-format' sees the same entry it does
when the worktree is created."
  (seq-some
   (lambda (own-p)
     (magit-dash-pr--path-equal-p
      path
      (magit-dash-pr--compute-worktree-path
       repo-dir (magit-dash-pr-worktree-name pr-num own-p) entry)))
   '(t nil)))

(defun magit-dash-pr--checkout-slugs (path)
  "Return the lowercased owner/name slugs of every git remote of checkout PATH."
  (when (and path (file-directory-p path))
    (let ((result (with-temp-buffer
                    (list (call-process "git" nil (list (current-buffer) nil) nil
                                        "-C" (expand-file-name path)
                                        "config" "--get-regexp" "\\`remote\\..*\\.url\\'")
                          (buffer-string)))))
      (when (zerop (car result))
        (thread-last
          (split-string (cadr result) "\n" t)
          (seq-keep (lambda (line)
                      (when-let* ((url (cadr (split-string line " " t)))
                                  (pair (magit-dash-gh--owner-repo-from-url url)))
                        (downcase (format "%s/%s" (car pair) (cdr pair)))))))))))

(defun magit-dash-pr--checkout-matches-slug-p (path slug name)
  "Return non-nil when checkout PATH may be the repository SLUG (owner/name).
When PATH has remotes, one of them must be SLUG exactly.  A checkout whose
owner cannot be told (no remote) is accepted only when its directory name is
NAME."
  (let ((slugs (magit-dash-pr--checkout-slugs path)))
    (if slugs
        (member (downcase slug) slugs)
      (string-equal-ignore-case
       (file-name-nondirectory (directory-file-name (expand-file-name path)))
       name))))

(defun magit-dash-pr-dashboard--find-local-path (repo-slug)
  "Return a local checkout path for REPO-SLUG (owner/name), or nil.
The match is on the full owner/name: a checkout of me/proj is never returned
for upstream/proj unless one of its remotes is upstream/proj.  A checkout
listed in `magit-dash-repo-list' matches by its configured repo URL or its
remotes; one whose owner cannot be told (no URL, no remote) matches by name.
The current repository is the fallback, matched the same way."
  (when (and repo-slug (stringp repo-slug))
    (let* ((slug-clean (string-trim repo-slug))
           (qualified (string-match-p "/" slug-clean))
           (repo-name (if (string-match ".*/\\([^/]+\\)$" slug-clean)
                          (match-string 1 slug-clean)
                        slug-clean)))
      (or
       (when (bound-and-true-p magit-dash-repo-list)
         (seq-some
          (lambda (r)
            (when (magit-dash-repo-p r)
              (let* ((rpath (magit-dash-repo-path r))
                     (rrepo (magit-dash-repo-repo r))
                     (rpair (and rrepo (magit-dash-gh--owner-repo-from-url rrepo)))
                     (rslug (and rpair (format "%s/%s" (car rpair) (cdr rpair)))))
                (when (cond
                       (rslug (string-equal-ignore-case rslug slug-clean))
                       (qualified (and rpath
                                       (or (magit-dash-pr--checkout-matches-slug-p
                                            rpath slug-clean repo-name)
                                           (and (null (magit-dash-pr--checkout-slugs rpath))
                                                (string-equal-ignore-case
                                                 (or (magit-dash-repo-name r) "") repo-name)))))
                       (t (or (string-equal-ignore-case (or (magit-dash-repo-name r) "") repo-name)
                              (and rpath (string-equal-ignore-case
                                          (file-name-nondirectory (directory-file-name (expand-file-name rpath)))
                                          repo-name)))))
                  rpath))))
          magit-dash-repo-list))
       (when-let* ((top (or (magit-toplevel)
                            (ignore-errors (magit-dash-gh--repo-dir)))))
         (let ((top-slug (ignore-errors (magit-dash-pr-dashboard--current-repo-slug))))
           (when (cond
                  (qualified (or (and top-slug (string-equal-ignore-case top-slug slug-clean))
                                 (magit-dash-pr--checkout-matches-slug-p top slug-clean repo-name)))
                  (t (string-equal-ignore-case
                      (file-name-nondirectory (directory-file-name (expand-file-name top)))
                      repo-name)))
             top)))))))

(defun magit-dash-pr-dashboard--worktree-path (entry)
  "Return existing or projected worktree path for PR ENTRY."
  (let* ((pr-num (plist-get entry :number))
         (repo (plist-get entry :repo))
         (pr-data (plist-get entry :pr))
         (branch-name (or (and pr-data (map-elt pr-data 'headRefName))
                          (plist-get entry :head-ref)))
         (local-path (magit-dash-pr-dashboard--find-local-path repo)))
    (when local-path
      (let* ((repo-dir (expand-file-name local-path))
             (name (magit-dash-pr--entry-worktree-name entry repo-dir)))
        (or (magit-dash-pr--find-existing-worktree repo-dir pr-num branch-name name)
            (magit-dash-pr--compute-worktree-path repo-dir name entry))))))

(defun magit-dash-pr-dashboard--worktree-exists-p (entry)
  "Return non-nil if a local worktree directory exists for PR ENTRY."
  (when-let* ((wt-dir (magit-dash-pr-dashboard--worktree-path entry)))
    (file-directory-p wt-dir)))

(defun magit-dash-pr-dashboard-open-dired (&optional entry)
  "Open Dired in the worktree for the pull request at point (or ENTRY).
If the worktree does not exist yet, creates it first."
  (interactive)
  (let* ((entry (or entry (magit-dash-pr-dashboard--entry-at-point)))
         (wt-dir (magit-dash-pr-dashboard--worktree-path entry)))
    (if (and wt-dir (file-directory-p wt-dir))
        (dired wt-dir)
      (magit-dash-pr-dashboard-open-worktree entry)
      (when-let* ((dir (magit-dash-pr-dashboard--worktree-path entry)))
        (when (file-directory-p dir)
          (dired dir))))))

(defun magit-dash-pr--resolve-remote-branch (repo pr-num &optional pr-data dir)
  "Resolve the head branch for PR-NUM in REPO from PR-DATA or GitHub."
  (or (and pr-data (map-elt pr-data 'headRefName))
      (when (executable-find "gh")
        (ignore-errors
          (let* ((result (magit-dash-gh--call-process
                          (list "pr" "view" (number-to-string pr-num) "-R" repo
                                "--json" "headRefName" "--jq" ".headRefName")
                          (or dir default-directory)))
                 (name (string-trim (cdr result))))
            (and (zerop (car result)) (not (string-empty-p name)) name))))
      (format "pr-%d" pr-num)))

(defun magit-dash-pr--pr-merged-p (pr &optional repo pr-num dir)
  "Return non-nil if PR alist (or PR-NUM in REPO) is merged."
  (let* ((state-raw (and pr (map-elt pr 'state)))
         (state (if (stringp state-raw) (upcase state-raw) ""))
         (merged-at (and pr (map-elt pr 'mergedAt))))
    (cond
     ((or (string= state "MERGED")
          (and (stringp merged-at) (not (string-empty-p merged-at)))) t)
     ((string= state "OPEN") nil)
     ((and (or pr-num (map-elt pr 'number)) repo (executable-find "gh"))
      (when-let* ((data (magit-dash-pr--fetch-pr
                         repo (or pr-num (map-elt pr 'number))
                         (or dir default-directory))))
        (magit-dash-pr--pr-merged-p data)))
     (t nil))))

(defun magit-dash-pr--fetch-repo-prs (repo &optional state dir)
  "Fetch PRs for REPO and STATE in DIR, or return nil on error."
  (when (executable-find "gh")
    (ignore-errors
      (let ((result (magit-dash-gh--call-process
                     (list "pr" "list" "-R" repo "--state" (or state "open")
                           "--limit" "100" "--json" "number,title,headRefName,state,mergedAt")
                     (or dir default-directory))))
        (when (zerop (car result))
          (json-parse-string (cdr result) :object-type 'alist :array-type 'list))))))

(defun magit-dash-pr--fetch-pr (repo number dir)
  "Fetch PR NUMBER in REPO using DIR, or return nil on failure."
  (when (executable-find "gh")
    (ignore-errors
      (let ((result (magit-dash-gh--call-process
                     (list "pr" "view" (number-to-string number) "-R" repo "--json"
                           "number,title,headRefName,state,mergedAt") dir)))
        (when (zerop (car result))
          (json-parse-string (cdr result) :object-type 'alist))))))

(defconst magit-dash-pr--generic-branches
  '("main" "master" "trunk" "develop" "dev" "default")
  "Branch names too common to identify a pull request's checkout.")

(defun magit-dash-pr--generic-branch-p (branch &optional repo-dir)
  "Return non-nil when BRANCH (optional `wt/' prefix) is too generic.
That is a name in `magit-dash-pr--generic-branches', GitHub's default
patch-N, or the origin default branch of REPO-DIR."
  (when (stringp branch)
    (let ((short (string-remove-prefix "wt/" branch)))
      (and (or (member short magit-dash-pr--generic-branches)
               (string-match-p "\\`patch-[0-9]+\\'" short)
               (and repo-dir
                    (equal short
                           (with-temp-buffer
                             (when (zerop (call-process
                                           "git" nil (list (current-buffer) nil) nil
                                           "-C" (expand-file-name repo-dir)
                                           "symbolic-ref" "--short" "refs/remotes/origin/HEAD"))
                               (string-remove-prefix "origin/" (string-trim (buffer-string))))))))
           t))))

(defun magit-dash-pr--worktree-pr-branch (wt)
  "Return the PR head branch name for git worktree WT, or nil.
Prefers the remote tracking branch (remote prefix stripped), then falls
back to the local branch name (without any `wt/' prefix)."
  (let* ((path (alist-get 'path wt))
         (local (alist-get 'branch wt))
         (upstream (when (and path local (file-directory-p path))
                     (ignore-errors
                      (let ((default-directory path))
                       (with-temp-buffer
                         (when (zerop (call-process
                                       "git" nil (list (current-buffer) nil) nil
                                       "rev-parse" "--abbrev-ref" "--symbolic-full-name"
                                       (concat local "@{upstream}")))
                           (let ((ref (string-trim (buffer-string))))
                             (when (string-match "\\`[^/]+/\\(.+\\)\\'" ref)
                               (match-string 1 ref))))))))))
    (or upstream
        (and local (string-remove-prefix "wt/" local)))))

(defun magit-dash-pr--fetch-prs-for-branch (repo branch dir)
  "Fetch all PRs in REPO whose head branch is BRANCH using DIR, or nil."
  (when (and branch (executable-find "gh"))
    (ignore-errors
      (let ((result (magit-dash-gh--call-process
                     (list "pr" "list" "-R" repo "--head" branch "--state" "all"
                           "--limit" "30" "--json" "number,title,headRefName,state,mergedAt")
                     dir)))
        (when (zerop (car result))
          (json-parse-string (cdr result) :object-type 'alist :array-type 'list))))))

(defun magit-dash-pr--match-worktree-by-branch (wt prs repo dir)
  "Return the PR for worktree WT chosen by its head branch name, or nil.
Looks in PRS first, then asks GitHub for PRs on that branch in REPO from DIR.
An open PR wins; otherwise the first (most recent) PR on the branch."
  (when-let* ((branch (magit-dash-pr--worktree-pr-branch wt))
              ((not (magit-dash-pr--generic-branch-p branch dir))))
    (let ((candidates
           (or (seq-filter (lambda (pr) (equal (map-elt pr 'headRefName) branch)) prs)
               (magit-dash-pr--fetch-prs-for-branch repo branch dir))))
      (or (seq-find (lambda (pr)
                      (let ((st (map-elt pr 'state)))
                        (and (stringp st) (string-equal (upcase st) "OPEN"))))
                    candidates)
          (car candidates)))))

(defconst magit-dash-pr--worktree-number-regexp
  "\\(?:\\`\\|-\\)\\(?:pr\\|cr\\)-\\(?:[[:alnum:]]+-\\)*\\([0-9]+\\)\\'"
  "Regexp extracting a PR number from a worktree directory name.")

(defun magit-dash-pr--worktree-named-for-p (wt-name number)
  "Return non-nil when WT-NAME is pr-NUMBER or cr-NUMBER, optionally prefixed."
  (seq-some
   (lambda (prefix)
     (string-match-p (format "\\(?:\\`\\|-\\)%s-%d\\'" prefix number) wt-name))
   '("pr" "cr")))

(defun magit-dash-pr--worktree-branch-matches-pr-p (wt pr &optional repo-dir number)
  "Return non-nil when worktree WT is on PR's checkout branch.
Its local branch (without a `wt/' prefix) must be pr-N or cr-N for PR's
number (NUMBER overrides it), or PR's head branch, or track that head branch
upstream.  A generic head or local branch (main, master, patch-1 ...) never
matches by name, and the main checkout of REPO-DIR never matches."
  (let ((branch (alist-get 'branch wt))
        (num (or number (map-elt pr 'number)))
        (head (map-elt pr 'headRefName)))
    (and branch
         (not (and repo-dir (magit-dash-pr--path-equal-p (alist-get 'path wt) repo-dir)))
         (or (and num (member (string-remove-prefix "wt/" branch)
                              (list (format "pr-%d" num) (format "cr-%d" num))))
             (and head
                  (not (magit-dash-pr--generic-branch-p head repo-dir))
                  (not (magit-dash-pr--generic-branch-p branch repo-dir))
                  (or (equal (string-remove-prefix "wt/" branch) head)
                      (equal (magit-dash-pr--worktree-pr-branch wt) head))))
         t)))

(defun magit-dash-pr--worktree-corroborates-pr-p (wt pr number)
  "Return non-nil when worktree WT is credibly the checkout of PR (NUMBER).
See `magit-dash-pr--worktree-branch-matches-pr-p': the branch must agree."
  (magit-dash-pr--worktree-branch-matches-pr-p wt pr nil number))

(defun magit-dash-pr--match-worktree-corroborated (wt prs slug repo-dir)
  "Return the PR in PRS (or fetched for SLUG) that worktree WT is the checkout of.
Candidates come from the branch, name and number tiers in turn, and every
candidate from every tier must pass
`magit-dash-pr--worktree-branch-matches-pr-p', so a directory name alone
never selects a PR."
  (let* ((wt-name (file-name-nondirectory (directory-file-name (alist-get 'path wt))))
         (number (when (string-match magit-dash-pr--worktree-number-regexp wt-name)
                   (string-to-number (match-string 1 wt-name))))
         (open-prs (seq-filter (lambda (pr)
                                 (equal (upcase (or (map-elt pr 'state) "")) "OPEN"))
                               prs)))
    (seq-some
     (lambda (tier)
       (when-let* ((pr (funcall tier))
                   ((magit-dash-pr--worktree-branch-matches-pr-p wt pr repo-dir)))
         pr))
     (list (lambda () (magit-dash-pr--match-worktree-by-branch wt prs slug repo-dir))
           (lambda () (magit-dash-pr--match-worktree-to-pr wt open-prs))
           (lambda ()
             (when number
               (or (seq-find (lambda (pr) (equal (map-elt pr 'number) number)) prs)
                   (magit-dash-pr--fetch-pr slug number repo-dir))))
           (lambda () (magit-dash-pr--match-worktree-to-pr wt prs))))))

(defun magit-dash-pr--find-corroborated-worktree (repo-dir number pr)
  "Return the git worktree alist for PR (NUMBER) in REPO-DIR, or nil.
Uses `magit-dash-pr--find-existing-worktree', then requires that the path be
a linked worktree of REPO-DIR on PR's branch."
  (when-let* ((path (magit-dash-pr--find-existing-worktree
                     repo-dir number (map-elt pr 'headRefName)))
              (wt (seq-find (lambda (wt)
                              (magit-dash-pr--path-equal-p (alist-get 'path wt) path))
                            (magit-dash-pr--git-worktrees repo-dir)))
              ((magit-dash-pr--worktree-branch-matches-pr-p wt pr repo-dir number)))
    wt))

(defun magit-dash-pr--match-worktree-to-pr (wt prs)
  "Match git worktree WT to a pull request in PRS alist list.
Matches by branch name (headRefName, pr-NUM, wt/...) or worktree directory name.
Returns matching PR alist or nil."
  (let ((wt-branch (alist-get 'branch wt))
        (wt-path (alist-get 'path wt)))
    (when (and wt-path (file-directory-p wt-path))
      (let ((wt-name (file-name-nondirectory (directory-file-name wt-path))))
        (seq-some
         (lambda (pr)
           (let* ((num (map-elt pr 'number))
                  (head (map-elt pr 'headRefName))
                  (pr-tags (when num (list (format "pr-%d" num) (format "cr-%d" num)))))
             (when (or (and head (or (equal wt-branch head)
                                     (equal wt-branch (concat "wt/" head))
                                     (equal wt-name head)
                                     (string-suffix-p (concat "-" head) wt-name)))
                       (seq-some (lambda (tag)
                                   (or (equal wt-branch tag)
                                       (equal wt-branch (concat "wt/" tag))))
                                 pr-tags)
                       (and num (magit-dash-pr--worktree-named-for-p wt-name num)))
               pr)))
         prs)))))

(defun magit-dash-pr--path-equal-p (path-a path-b)
  "Return non-nil if PATH-A and PATH-B refer to the same location."
  (when (and path-a path-b)
    (or (string= (directory-file-name (expand-file-name path-a))
                 (directory-file-name (expand-file-name path-b)))
        (ignore-errors (file-equal-p path-a path-b)))))

(defun magit-dash-pr--move-worktree (repo-dir old-path new-path)
  "Move git worktree from OLD-PATH to NEW-PATH in REPO-DIR.
Returns NEW-PATH on success, nil on failure."
  (let* ((repo-dir (expand-file-name repo-dir))
         (old-path (expand-file-name old-path))
         (new-path (expand-file-name new-path))
         (parent (file-name-directory (directory-file-name new-path))))
    (when (file-exists-p new-path)
      (user-error "Destination worktree path already exists: %s" new-path))
    (when parent
      (make-directory parent t))
    (let ((exit-code (call-process "git" nil nil nil
                                   "-C" repo-dir
                                   "worktree" "move" old-path new-path)))
      (if (zerop exit-code)
          (progn
            (magit-dash-worktree-symlink-worktree new-path repo-dir)
            (magit-dash-review-json-relocate-worktree repo-dir old-path new-path)
            new-path)
        (message "magit-gh: failed to move worktree %s -> %s (exit %d)"
                 old-path new-path exit-code)
        nil))))

;;;###autoload
(defun magit-dash-pr-delete-worktree (wt-path &optional repo-dir pushed-oid risks-confirmed)
  "Safely remove the worktree at WT-PATH for REPO-DIR.
Refuses to delete if WT-PATH contains uncommitted changes.  Also refuses,
unless the user confirms (or RISKS-CONFIRMED is non-nil because the caller
already asked), when `magit-dash-worktree-risks' reports unpublished commits
\(none when HEAD is PUSHED-OID) or ignored files that are not build output.
Preserves the underlying branch until branch pruning."
  (let* ((wt-path (expand-file-name wt-path))
         (repo-dir (expand-file-name
                    (or repo-dir (magit-dash-worktree-main-dir wt-path)))))
    (when (magit-dash-worktree-uncommitted-changes-p wt-path)
      (user-error "Cannot delete worktree %s: contains uncommitted changes" wt-path))
    (when-let* ((risks (magit-dash-worktree-risks wt-path pushed-oid))
                ((not risks-confirmed))
                ((not (magit-dash-worktree-confirm-risks wt-path risks))))
      (user-error "Not deleting worktree %s: %s" wt-path (string-join risks "; ")))
    (let ((exit-code (call-process "git" nil nil nil
                                   "-C" repo-dir
                                   "worktree" "remove" wt-path)))
      (if (zerop exit-code)
          (progn
            (call-process "git" nil nil nil "-C" repo-dir "worktree" "prune")
            (message "magit-gh: deleted worktree at %s (branch preserved)" wt-path)
            t)
        (user-error "Failed to remove worktree %s (git exit %d)" wt-path exit-code)))))

(defun magit-dash-pr--create-worktree-for-entry (entry &optional switch)
  "Create a git worktree for single PR ENTRY plist.
When SWITCH is non-nil, switches to the worktree's magit status buffer."
  (let* ((pr-num (plist-get entry :number))
         (repo (plist-get entry :repo))
         (pr-data (plist-get entry :pr))
         (local-path (or (plist-get entry :repo-dir)
                         (magit-dash-pr-dashboard--find-local-path repo)
                         (let ((prompt-path (read-directory-name (format "Local repository path for %s: " repo))))
                           (if (and prompt-path (file-directory-p prompt-path))
                               (expand-file-name prompt-path)
                             (user-error "No registered local checkout for %s" repo)))))
         (repo-dir (expand-file-name (magit-dash-worktree-main-dir local-path)))
         (repo-base (file-name-nondirectory (directory-file-name repo-dir)))
         (branch-name (magit-dash-pr--resolve-remote-branch repo pr-num pr-data repo-dir))
         (target-name (magit-dash-pr--entry-worktree-name entry repo-dir))
         (existing (magit-dash-pr--find-existing-worktree
                    repo-dir pr-num branch-name target-name))
         (local-branch (if (or (magit-dash-pr--generic-branch-p branch-name repo-dir)
                               (zerop (call-process "git" nil nil nil "-C" repo-dir
                                                    "rev-parse" "--verify" "--quiet"
                                                    (concat "refs/heads/" branch-name))))
                           target-name
                         branch-name))
         (target-path (magit-dash-pr--compute-worktree-path repo-dir target-name entry))
         (wt-dir (or existing target-path)))
    ;; A pr-N or cr-N checkout is accepted as is; only offer to rename
    ;; worktrees whose name is not a PR name at all.
    (when (and existing
               (not (magit-dash-pr--worktree-acceptable-p repo-dir existing pr-num entry)))
      (if (y-or-n-p (format "Worktree '%s' does not match PR #%d format ('%s'). Rename it? "
                            (file-name-nondirectory (directory-file-name existing))
                            pr-num
                            (file-name-nondirectory (directory-file-name target-path))))
          (when (magit-dash-pr--move-worktree repo-dir existing target-path)
            (setq wt-dir target-path))
        (setq wt-dir existing)))
    (if (and wt-dir (file-directory-p wt-dir))
        (if switch
            (progn
              (message "magit-gh: opening existing worktree at %s" wt-dir)
              (magit-status-setup-buffer wt-dir))
          (message "magit-gh: worktree for PR #%d already exists at %s" pr-num wt-dir)
          wt-dir)
      (message "magit-gh: fetching PR #%d (%s) in %s..." pr-num branch-name repo-base)
      (let ((default-directory repo-dir))
        (let ((refspec (format "pull/%d/head:%s" pr-num local-branch)))
          (unless (= 0 (magit-call-git "fetch" "origin" refspec))
            (user-error "Failed to fetch PR #%d into %s" pr-num local-branch)))
        (let ((parent (file-name-directory (directory-file-name wt-dir))))
          (when parent
            (ignore-errors (make-directory parent t))))
        (let ((exit-code (magit-call-git "worktree" "add" wt-dir local-branch)))
          (if (= exit-code 0)
              (progn
                (magit-dash-worktree-symlink-worktree wt-dir repo-dir)
                (message "magit-gh: %s worktree for PR #%d at %s"
                         (if switch "opened" "created") pr-num wt-dir)
                (when (derived-mode-p 'magit-dash-pr-dashboard-mode)
                  (magit-dash-pr-dashboard--rebuild-entries))
                (if switch
                    (magit-status-setup-buffer wt-dir)
                  wt-dir))
            (user-error "Failed to create worktree at %s (exit %d)" wt-dir exit-code)))))))

;;;###autoload
(defun magit-dash-pr-dashboard-create-worktree (&optional entry switch)
  "Create a git worktree for the pull request at point (or ENTRY), or marked PRs.
If PRs are marked in the dashboard and ENTRY is nil, creates worktrees for all
marked PRs and clears marks.
When SWITCH is non-nil, switches to the worktree's magit status buffer."
  (interactive)
  (let ((marked (and (null entry)
                     (derived-mode-p 'magit-dash-pr-dashboard-mode)
                     (magit-dash-pr-dashboard--marked-or-at-point-entries))))
    (if marked
        (let* ((to-create (seq-filter (lambda (e)
                                        (let* ((repo (plist-get e :repo))
                                               (num (plist-get e :number))
                                               (path (magit-dash-pr-dashboard--find-local-path repo)))
                                          (not (or (and path (magit-dash-pr--find-existing-worktree path num))
                                                   (magit-dash-pr-dashboard--worktree-exists-p e)))))
                                      marked))
               (skipped (- (length marked) (length to-create)))
               (created 0)
               (failed 0))
          (seq-do (lambda (m-entry)
                    (condition-case err
                        (progn
                          (magit-dash-pr--create-worktree-for-entry m-entry nil)
                          (setq created (1+ created)))
                      (error
                       (message "magit-gh: failed to create worktree for PR #%d: %s"
                                (plist-get m-entry :number) (error-message-string err))
                       (setq failed (1+ failed)))))
                  to-create)
          (setq magit-dash-pr-dashboard--marked-entries nil)
          (magit-dash-pr-dashboard--rebuild-entries)
          (message "magit-gh: created %d worktree(s) for marked PRs (%d already existed, %d failed)"
                   created skipped failed))
      (magit-dash-pr--create-worktree-for-entry
       (or entry (magit-dash-pr-dashboard--entry-at-point))
       switch))))

;;;###autoload
(defun magit-dash-pr-dashboard-open-worktree (&optional entry)
  "Create and open a git worktree for the pull request at point (or ENTRY).
If a worktree for this PR already exists, switches to its magit status buffer.
Otherwise, fetches the remote PR branch into a local branch mirroring its remote
name, creates the worktree directory, and opens its magit status buffer."
  (interactive)
  (magit-dash-pr--create-worktree-for-entry
   (or entry (magit-dash-pr-dashboard--entry-at-point))
   t))

;;;###autoload
(defalias 'magit-dash-pr-create-worktree #'magit-dash-pr-dashboard-create-worktree)

(defun magit-dash-pr--confirm-delete (path prompt-p label risks)
  "Return non-nil when the worktree at PATH may be deleted.
LABEL describes the worktree and its closed PR.  With RISKS (see
`magit-dash-worktree-risks') the answer must be an explicit yes to a prompt
that lists them, and without PROMPT-P it is always nil: a risky worktree is
never deleted unattended."
  (cond
   (risks
    (and prompt-p (magit-dash-worktree-confirm-risks path risks)))
   (prompt-p
    (y-or-n-p (format "%s has no uncommitted changes. Delete worktree? (branch preserved) "
                      label)))
   (t t)))

;;;###autoload
(defun magit-dash-pr-review-worktrees (&optional repo-dir prompt-p prs merged-only)
  "Review existing worktrees in REPO-DIR against GitHub pull requests.
Prompts to rename worktrees that do not match the configured PR naming format,
and prompts to delete worktrees for merged PRs.  A worktree is skipped when it
has uncommitted changes, and deleted only after an explicit yes when it has
unpublished commits or non-build ignored files (never without PROMPT-P).
A worktree matches a PR only when its branch does too.
Leaves branches intact until branch pruning.
PROMPT-P specifies whether to prompt for actions (defaults to interactive).
PRS can be an existing list of PR alists to avoid refetching.
When MERGED-ONLY is non-nil, only checks and cleans worktrees for merged PRs.
Returns a plist with :renamed, :deleted, :matching, and :skipped counts."
  (interactive (list nil t nil nil))
  (let* ((prompt-p (if (called-interactively-p 'interactive) t prompt-p))
         (repo-dir (expand-file-name
                    (magit-dash-worktree-main-dir
                     (or repo-dir (magit-dash-worktree-main-dir) default-directory))))
         (slug (or (ignore-errors
                     (let ((default-directory repo-dir))
                       (magit-dash-pr-dashboard--current-repo-slug)))
                   (file-name-nondirectory (directory-file-name repo-dir))))
         (prs (or prs (magit-dash-pr--fetch-repo-prs slug "all" repo-dir)))
         (git-wts (magit-dash-pr--git-worktrees repo-dir))
         (counts (list :renamed 0 :deleted 0 :matching 0 :skipped 0)))
    (seq-do
     (lambda (wt)
       (let* ((wt-path (alist-get 'path wt)))
         (unless (or (null wt-path)
                     (magit-dash-pr--path-equal-p wt-path repo-dir)
                     (not (file-directory-p wt-path)))
           (let* ((wt-name (file-name-nondirectory (directory-file-name wt-path)))
                  (number (when (string-match magit-dash-pr--worktree-number-regexp wt-name)
                            (string-to-number (match-string 1 wt-name))))
                  (matched (magit-dash-pr--match-worktree-corroborated wt prs slug repo-dir)))
             (when (and (null matched) number)
               (message "magit-gh: could not find PR #%d for worktree %s; skipping" number wt-name)
               (plist-put counts :skipped (1+ (plist-get counts :skipped))))
             (when matched
               (let* ((pr-num (map-elt matched 'number))
                      (state-raw (map-elt matched 'state))
                      (state (if (stringp state-raw) (upcase state-raw) ""))
                      (merged-p (magit-dash-pr--pr-merged-p matched slug pr-num repo-dir))
                      (closed-p (or merged-p (string= state "CLOSED"))))
                 (cond
                  ;; Case 1: PR has merged or closed
                  (closed-p
                   (let* ((pr-label (if pr-num (format "PR #%d" pr-num) "PR"))
                          (status-desc (if merged-p "merged" "closed"))
                          (head-oid (map-elt matched 'headRefOid))
                          (dirty (magit-dash-worktree-uncommitted-changes-p wt-path))
                          (risks (and (not dirty)
                                      (magit-dash-worktree-risks wt-path head-oid))))
                     (cond
                      (dirty
                       (message "magit-gh: worktree %s (%s %s) has uncommitted changes; skipping"
                                wt-name pr-label status-desc)
                       (plist-put counts :skipped (1+ (plist-get counts :skipped))))
                      ((magit-dash-pr--confirm-delete
                        wt-path prompt-p
                        (format "%s is %s and worktree '%s' (branch '%s')"
                                pr-label status-desc wt-name (or (alist-get 'branch wt) "detached"))
                        risks)
                       (magit-dash-pr-delete-worktree wt-path repo-dir head-oid (and risks t))
                       (plist-put counts :deleted (1+ (plist-get counts :deleted))))
                      (t
                       (when risks
                         (message "magit-gh: worktree %s (%s %s) has %s; skipping"
                                  wt-name pr-label status-desc (string-join risks "; ")))
                       (plist-put counts :skipped (1+ (plist-get counts :skipped)))))))
                  ;; Case 2: PR is open - check path match (skipped when merged-only)
                  ((and (not merged-only) (string= state "OPEN") pr-num)
                   (let* ((entry (list :number pr-num :pr matched))
                          (target-path (magit-dash-pr--compute-worktree-path
                                        repo-dir
                                        (magit-dash-pr--entry-worktree-name entry repo-dir)
                                        entry)))
                     (cond
                      ((or (magit-dash-pr--path-equal-p wt-path target-path)
                           (magit-dash-pr--worktree-acceptable-p repo-dir wt-path pr-num entry))
                       (plist-put counts :matching (1+ (plist-get counts :matching))))
                      ((or (not prompt-p)
                           (y-or-n-p (format "Worktree '%s' does not match PR #%d format ('%s'). Rename it to match? "
                                             wt-name pr-num
                                             (file-name-nondirectory (directory-file-name target-path)))))
                       (if (magit-dash-pr--move-worktree repo-dir wt-path target-path)
                           (plist-put counts :renamed (1+ (plist-get counts :renamed)))
                         (plist-put counts :skipped (1+ (plist-get counts :skipped)))))
                      (t
                       (plist-put counts :skipped (1+ (plist-get counts :skipped))))))))))))))
     git-wts)
    (if merged-only
        (message "magit-gh: clean merged worktrees for %s — %d deleted, %d skipped"
                 slug (plist-get counts :deleted) (plist-get counts :skipped))
      (message "magit-gh: reviewed worktrees for %s — %d renamed, %d deleted, %d matching, %d skipped"
               slug
               (plist-get counts :renamed)
               (plist-get counts :deleted)
               (plist-get counts :matching)
               (plist-get counts :skipped)))
    counts))

;;;###autoload
(defun magit-dash-pr-clean-merged-worktrees (&optional repo-dir prompt-p)
  "Delete worktrees for merged PRs in REPO-DIR when clean.
Refuses to delete any worktree with uncommitted changes, and never deletes
one with unpublished commits or non-build ignored files without PROMPT-P and
an explicit yes.  Leaves local branches intact until branch pruning.
PROMPT-P specifies whether to prompt before deleting each clean worktree."
  (interactive (list nil t))
  (let ((counts (magit-dash-pr-review-worktrees repo-dir prompt-p nil t)))
    (list :deleted (plist-get counts :deleted)
          :skipped (plist-get counts :skipped))))

(defun magit-dash-pr--clean-worktree-for-entry (entry &optional prompt-p)
  "Clean worktree for single PR ENTRY plist if it exists and is safe to delete.
The worktree must be on the PR's branch.  Returns `:deleted', `:skipped', or
`:none'.  Unpublished commits or non-build ignored files need PROMPT-P and an
explicit yes."
  (let* ((pr-num (plist-get entry :number))
         (repo (plist-get entry :repo))
         (pr-data (plist-get entry :pr))
         (local-path (magit-dash-pr-dashboard--find-local-path repo))
         (repo-dir (and local-path (expand-file-name (magit-dash-worktree-main-dir local-path))))
         (branch-name (or (and pr-data (map-elt pr-data 'headRefName))
                          (plist-get entry :head-ref)))
         (wt (and repo-dir
                  (magit-dash-pr--find-corroborated-worktree
                   repo-dir pr-num
                   (or pr-data (list (cons 'number pr-num) (cons 'headRefName branch-name))))))
         (wt-path (alist-get 'path wt)))
    (if (not (and wt-path (file-directory-p wt-path)))
        :none
      (let* ((wt-name (file-name-nondirectory (directory-file-name wt-path)))
             (state-raw (or (plist-get entry :state) (and pr-data (map-elt pr-data 'state))))
             (state (if (stringp state-raw) (upcase state-raw) ""))
             (merged-p (magit-dash-pr--pr-merged-p pr-data repo pr-num repo-dir))
             (closed-p (or merged-p (string= state "CLOSED")))
             (head-oid (and pr-data (map-elt pr-data 'headRefOid)))
             (dirty (and closed-p (magit-dash-worktree-uncommitted-changes-p wt-path)))
             (risks (and closed-p (not dirty)
                         (magit-dash-worktree-risks wt-path head-oid))))
        (cond
         ((not closed-p) :skipped)
         (dirty
          (message "magit-gh: worktree %s (PR #%d) has uncommitted changes; skipping"
                   wt-name pr-num)
          :skipped)
         ((magit-dash-pr--confirm-delete
           wt-path prompt-p
           (format "PR #%d is %s and worktree '%s' (branch '%s')"
                   pr-num (if merged-p "merged" "closed") wt-name
                   (or branch-name "detached"))
           risks)
          (magit-dash-pr-delete-worktree wt-path repo-dir head-oid (and risks t))
          :deleted)
         (t
          (when risks
            (message "magit-gh: worktree %s (PR #%d) has %s; skipping"
                     wt-name pr-num (string-join risks "; ")))
          :skipped))))))

;;;###autoload
(defun magit-dash-pr-create-all-worktrees (&optional _repo-dir)
  "Create worktrees for PRs in the current PR dashboard view.
_REPO-DIR is retained for compatibility; the dashboard view determines scope."
  (interactive)
  (magit-dash-pr-dashboard-create-all-worktrees))

(defun magit-dash-pr-dashboard--repo-path-at-point ()
  "Return registered local repository path for PR at point.
Signals `user-error' when not found."
  (let* ((entry (magit-dash-pr-dashboard--entry-at-point))
         (repo (plist-get entry :repo)))
    (or (magit-dash-pr-dashboard--find-local-path repo)
        (user-error "No registered local checkout for %s" repo))))

(defun magit-dash-pr-dashboard-review-worktrees ()
  "Review worktrees for the pull request / repository at point in dashboard."
  (interactive)
  (magit-dash-pr-review-worktrees (magit-dash-pr-dashboard--repo-path-at-point) t)
  (magit-dash-pr-dashboard--rebuild-entries))

(defun magit-dash-pr-dashboard-clean-merged-worktrees ()
  "Plan deleting worktrees of marked PRs, or of every merged PR in the repo.
The plan opens in a `hitl-plan' review buffer; nothing is deleted until you
execute it there."
  (interactive)
  (if-let* ((marked (and (derived-mode-p 'magit-dash-pr-dashboard-mode)
                         (magit-dash-pr-dashboard--marked-or-at-point-entries))))
      (let ((targets (seq-keep #'magit-dash-worktree-clean-entry-target marked)))
        (setq magit-dash-pr-dashboard--marked-entries nil)
        (magit-dash-pr-dashboard--rebuild-entries)
        (magit-dash-worktree-clean-plan-targets targets "Delete worktrees of marked PRs"))
    (magit-dash-worktree-clean-plan (magit-dash-pr-dashboard--repo-path-at-point))))

(defun magit-dash-pr--run-git-async (dir args callback)
  "Run git ARGS in DIR and call CALLBACK with exit code and output."
  (let ((default-directory dir)
        (buffer (generate-new-buffer " *magit-dash-pr-git*")))
    (condition-case err
        (make-process
         :name "magit-dash-pr-git" :buffer buffer :noquery t :connection-type 'pipe
         :command (cons "git" args)
         :sentinel (lambda (process _event)
                     (when (memq (process-status process) '(exit signal))
                       (let ((output (with-current-buffer (process-buffer process) (buffer-string)))
                             (code (process-exit-status process)))
                         (kill-buffer (process-buffer process))
                         (funcall callback code output)))))
      (error (kill-buffer buffer) (funcall callback 1 (error-message-string err))))))

(defun magit-dash-pr--create-batch-async (targets buffer existing missing)
  "Create TARGETS sequentially, updating BUFFER and reporting batch counts."
  (let ((created 0) (failed 0))
    (cl-labels
        ((refresh ()
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (magit-dash-pr-dashboard--rebuild-entries))))
         (next (remaining)
           (if (null remaining)
               (progn (refresh)
                      (message "magit-gh: batch creation complete — %d created, %d existing, %d without local checkout, %d failed"
                               created existing missing failed))
             (let* ((target (car remaining)) (entry (car target)) (branch (cdr target))
                    (repo (plist-get entry :repo)) (number (plist-get entry :number))
                    (key (cons repo number))
                    (dir (magit-dash-pr-dashboard--find-local-path repo))
                    (path (and dir (magit-dash-pr--compute-worktree-path
                                    dir (magit-dash-pr--entry-worktree-name entry dir) entry))))
               (cl-labels ((finish (error-message)
                             (remhash key magit-dash-pr--pending-worktrees)
                             (if error-message
                                 (progn (cl-incf failed)
                                        (message "magit-gh: failed to create %s #%d: %s" repo number error-message))
                               (cl-incf created)
                               (message "magit-gh: created worktree for %s #%d" repo number))
                             (refresh)
                             (next (cdr remaining))))
                 (cond
                  ((not (and dir (file-directory-p dir))) (finish "local checkout unavailable"))
                  ((magit-dash-pr--find-existing-worktree dir number branch)
                   (remhash key magit-dash-pr--pending-worktrees)
                   (cl-incf existing) (refresh) (next (cdr remaining)))
                  (t
                   (message "magit-gh: fetching %s #%d (%s)…" repo number branch)
                   (magit-dash-pr--run-git-async
                    dir (list "fetch" "origin" (format "pull/%d/head:%s" number branch))
                    (lambda (code output)
                      (if (not (zerop code)) (finish (string-trim output))
                        (condition-case err
                            (progn
                              (make-directory (file-name-directory path) t)
                              (magit-dash-pr--run-git-async
                               dir (list "worktree" "add" path branch)
                               (lambda (add-code add-output)
                                 (if (not (zerop add-code)) (finish (string-trim add-output))
                                   (condition-case symlink-error
                                       (progn
                                         (when (fboundp 'magit-dash-worktree-symlink-worktree)
                                           (magit-dash-worktree-symlink-worktree path dir))
                                         (finish nil))
                                     (error (finish (error-message-string symlink-error))))))))
                          (error (finish (error-message-string err))))))))))))))
      (next targets))))

(defun magit-dash-pr-dashboard-create-all-worktrees ()
  "Confirm and create worktrees for PRs in the current dashboard view."
  (interactive)
  (unless (derived-mode-p 'magit-dash-pr-dashboard-mode)
    (user-error "Open a PR dashboard and run this command in its buffer"))
  (let ((targets nil) (existing 0) (missing 0) (queued 0))
    (dolist (item tabulated-list-entries)
      (let* ((entry (car item))
             (repo (plist-get entry :repo))
             (number (plist-get entry :number))
             (path (magit-dash-pr-dashboard--find-local-path repo)))
        (if (not (and path (file-directory-p path)))
            (setq missing (1+ missing))
          (if (or (gethash (cons repo number) magit-dash-pr--pending-worktrees)
                  (magit-dash-pr--find-existing-worktree path number)
                  (magit-dash-pr-dashboard--worktree-exists-p entry))
              (setq existing (1+ existing))
            (let* ((pr (plist-get entry :pr))
                   (branch (or (plist-get entry :head-ref)
                               (and pr (map-elt pr 'headRefName))
                               (magit-dash-pr--resolve-remote-branch repo number pr path)))
                   (prepared (copy-sequence entry)))
              (if (magit-dash-pr--find-existing-worktree path number branch)
                  (setq existing (1+ existing))
                ;; Preserve the reviewed branch for the actual creation.
                (setq prepared (plist-put prepared :pr
                                          (cons (cons 'headRefName branch) pr)))
                (push (cons prepared branch) targets)))))))
    (setq targets (nreverse targets))
    (if (null targets)
        (message "magit-gh: no worktrees to create (%d existing, %d without local checkout)"
                 existing missing)
      (with-output-to-temp-buffer "*PR worktrees to create*"
        (princ "Worktrees to create from the current PR view:\n\n")
        (dolist (target targets)
          (let ((entry (car target)))
            (princ (format "%s #%s  %s\n" (plist-get entry :repo)
                           (plist-get entry :number) (cdr target)))))
        (princ (format "\n%d existing; %d without local checkout.\n" existing missing)))
      (when (yes-or-no-p (format "Create %d worktree(s) listed in *PR worktrees to create*? "
                                 (length targets)))
        (setq queued (length targets))
        (dolist (target targets)
          (let ((entry (car target)))
            (puthash (cons (plist-get entry :repo) (plist-get entry :number))
                     t magit-dash-pr--pending-worktrees)))
        (magit-dash-pr-dashboard--rebuild-entries)
        (magit-dash-pr--create-batch-async targets (current-buffer) existing missing)))
    (list :queued queued :existing existing :missing missing)))

(defun magit-dash-pr-dashboard-fetch-ci ()
  "Fetch GitHub Actions CI logs for the pull request at point."
  (interactive)
  (require 'magit-dash-gh-actions)
  (let ((entry (magit-dash-pr-dashboard--entry-at-point))
        (path (magit-dash-pr-dashboard--repo-path-at-point)))
    (magit-dash-gh-actions-fetch-for-pr (plist-get entry :number) path)))

(transient-define-prefix magit-dash-pr-dashboard-menu ()
  "Actions for the PR dashboard.
Key groups mirror `magit-dash-menu': w worktree, c CI, f filter, m mark."
  [["Pull Request"
    ("b"   "Open in browser"          magit-dash-pr-dashboard-open-browser)
    ("cv"  "Open in diff-review"      magit-dash-pr-dashboard-diff-review)
    ("cr"  "Review (diff-review)"     magit-dash-pr-dashboard-diff-review)
    ("cb"  "Open in code-review"      magit-dash-pr-dashboard-code-review
     :if magit-dash-pr-code-review-available-p)]
   ["Mark / Bulk"
    ("mt"  "Toggle mark"              magit-dash-pr-dashboard-toggle-mark)
    ("mu"  "Unmark"                   magit-dash-pr-dashboard-unmark)
    ("mc"  "Unmark all"               magit-dash-pr-dashboard-unmark-all)
    ("ma"  "Mark all"                 magit-dash-pr-dashboard-mark-all)]
   ["Worktree"
    ("wa"  "Create worktree(s)"       magit-dash-pr-dashboard-create-worktree)
    ("wl"  "Create all worktrees"     magit-dash-pr-dashboard-create-all-worktrees)
    ("wr"  "Review / rename"          magit-dash-pr-dashboard-review-worktrees)
    ("wd"  "Clean merged"             magit-dash-pr-dashboard-clean-merged-worktrees)
    ("wo"  "Open Magit in worktree"   magit-dash-pr-dashboard-open-worktree)
    ("wi"  "Open Dired in worktree"   magit-dash-pr-dashboard-open-dired)]
   ["Chain"
    ("wc"  "Plan chain merge (from PR)" magit-dash-chain-plan)
    ("wm"  "Resume chain"             magit-dash-chain-resume)
    ("wh"  "Open chain plan"          magit-dash-chain-open)]
   ["CI"
    ("cf"  "Fetch CI logs"            magit-dash-pr-dashboard-fetch-ci)]]
  [["View & Filter"
    ("v"   "Switch view…"             magit-dash-pr-dashboard-switch-view)
    ("tc"  "Toggle column…"           magit-dash-pr-toggle-column)
    ("to"  "Toggle Org column"        magit-dash-pr-toggle-org)
    ("ff"  "Set filter…"              magit-dash-pr-dashboard-set-filter)
    ("fx"  (lambda ()
             (if magit-dash-pr-dashboard--hide-failing
                 "Show failing CI"
               "Hide failing CI"))
     magit-dash-pr-dashboard-toggle-hide-failing)
    ("fc"  "Clear all filters"        magit-dash-pr-dashboard-clear-filters)
    ("fm"  "Fetch more PRs"           magit-dash-pr-dashboard-fetch-more)]
   ["Dashboard"
    ("r"   "Refresh PR(s) at point/marked" magit-dash-pr-dashboard-refresh-entries)
    ("g"   "Refresh all (re-query)"   magit-dash-pr-dashboard-refresh)
    ("q"   "Quit"                     magit-dash-pr-dashboard-quit)]])

(transient-define-prefix magit-dash-pr-views-menu ()
  "Jump to a pull request dashboard view."
  [["Pull Requests"
    ("a" "All repos — open PRs"  magit-dash-pr-dash-all)
    ("m" "All repos — my PRs"    magit-dash-pr-dash-mine)
    ("h" "This repo — open PRs"  magit-dash-pr-dash-here)
    ("n" "This repo — my PRs"    magit-dash-pr-dash-mine-here)
    ("c" "Merge conflicts"       magit-dash-pr-dash-conflicts)]])

;;;; In-memory PR tracking

;; Tracking metadata is computed against a session-local snapshot cache.
;; Attaching never mutates the cache; callers commit explicitly once they
;; have consumed the deltas.

(defvar magit-dash-pr--tracking-cache (make-hash-table :test #'equal)
  "Map \"repo#number\" to the previous snapshot plist of that PR.
A snapshot has `:bucket', `:threads-open', `:approvals', `:failing' (a
list of check names) and `:last-seen' (a timestamp string).")

(defun magit-dash-pr--tracking-key (pr)
  "Return the tracking cache key \"repo#number\" for PR."
  (format "%s#%s" (or (plist-get pr :repo) "") (plist-get pr :number)))

(defun magit-dash-pr--tracking-timestamp ()
  "Return the current time as an Org-style inactive timestamp string."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun magit-dash-pr--failing-names (pr)
  "Return the sorted names of the failing checks of PR."
  (sort (mapcar (lambda (f) (if (consp f) (car f) f)) (plist-get pr :failing))
        #'string<))

(defun magit-dash-pr--tracking-snapshot (pr)
  "Return the snapshot plist of PR for the tracking cache."
  (list :bucket (plist-get pr :bucket)
        :threads-open (or (plist-get pr :threads-open) 0)
        :approvals (or (plist-get pr :approvals) 0)
        :failing (magit-dash-pr--failing-names pr)
        :last-seen (magit-dash-pr--tracking-timestamp)))

(defun magit-dash-pr--delta (pr previous)
  "Return (TYPE . REASON) describing how PR changed since snapshot PREVIOUS."
  (let ((bucket (plist-get pr :bucket))
        (failing (magit-dash-pr--failing-names pr))
        (threads (or (plist-get pr :threads-open) 0)))
    (cond
     ((null previous) (cons 'new "newly seen"))
     ((not (eq bucket (plist-get previous :bucket)))
      (cons 'bucket-changed
            (format "%s -> %s" (plist-get previous :bucket) bucket)))
     ((not (equal failing (plist-get previous :failing)))
      (cons 'ci-changed
            (format "failing checks %d -> %d"
                    (length (plist-get previous :failing)) (length failing))))
     ((/= threads (plist-get previous :threads-open))
      (cons 'threads-changed
            (format "open threads %d -> %d"
                    (plist-get previous :threads-open) threads)))
     (t (cons 'unchanged "no change")))))

(defun magit-dash-pr--action-summary (pr)
  "Return a short next-action string for PR."
  (let* ((threads (or (plist-get pr :threads-open) 0))
         (changes (or (plist-get pr :changes-requested) 0))
         (names (magit-dash-pr--failing-names pr))
         (missing (max 0 (- (or (plist-get pr :required) 1)
                            (or (plist-get pr :approvals) 0)))))
    (pcase (plist-get pr :bucket)
      ('fix-ci (concat "fix failing CI: " (string-join names ", ")))
      ('threads
       (string-join
        (delq nil
              (list (when (> threads 0)
                      (format "address %d open review thread%s" threads
                              (if (= threads 1) "" "s")))
                    (when (> changes 0)
                      (format "changes requested by %d reviewer%s" changes
                              (if (= changes 1) "" "s")))))
        "; "))
      ('reviews (format "needs %d review%s" (max missing 1)
                        (if (= (max missing 1) 1) "" "s")))
      ('blocked "check merge blockers")
      ('ready "ready to merge")
      (_ "check merge blockers"))))

(defun magit-dash-pr-attach-tracking (pr)
  "Return a copy of PR with a `:tracking' plist computed against the cache.
The tracking plist holds `:last-seen', `:previous-bucket', `:delta-type'
\(`new', `bucket-changed', `ci-changed', `threads-changed' or
`unchanged'), `:delta-reason', `:action-summary', `:failing-checks'
\(names), `:threads-open', `:approvals', `:stack-parent' and `:ticket'.
The cache is not modified; see `magit-dash-pr-commit-tracking'."
  (let* ((previous (gethash (magit-dash-pr--tracking-key pr)
                            magit-dash-pr--tracking-cache))
         (delta (magit-dash-pr--delta pr previous)))
    (plist-put
     (copy-sequence pr) :tracking
     (list :last-seen (magit-dash-pr--tracking-timestamp)
           :previous-bucket (plist-get previous :bucket)
           :delta-type (car delta)
           :delta-reason (cdr delta)
           :action-summary (magit-dash-pr--action-summary pr)
           :failing-checks (magit-dash-pr--failing-names pr)
           :threads-open (or (plist-get pr :threads-open) 0)
           :approvals (or (plist-get pr :approvals) 0)
           :stack-parent (plist-get pr :stack-parent)
           :ticket (plist-get pr :ticket)))))

(defun magit-dash-pr-commit-tracking (prs)
  "Record a snapshot of each PR in PRS in the tracking cache.
Return PRS.  Call this after consuming `magit-dash-pr-attach-tracking'
deltas so the next collection compares against this state."
  (dolist (pr prs)
    (puthash (magit-dash-pr--tracking-key pr)
             (magit-dash-pr--tracking-snapshot pr)
             magit-dash-pr--tracking-cache))
  prs)

(defun magit-dash-pr-tracking-fingerprint (prs)
  "Return a stable md5 hex string over the volatile fields of PRS.
Covers repo, number, bucket, state, failing checks, open threads and
approvals, independent of the order of PRS."
  (let ((rows (mapcar
               (lambda (pr)
                 (format "%s#%s|%s|%s|%s|%s|%s"
                         (or (plist-get pr :repo) "") (plist-get pr :number)
                         (plist-get pr :bucket) (plist-get pr :state)
                         (string-join (magit-dash-pr--failing-names pr) ",")
                         (or (plist-get pr :threads-open) 0)
                         (or (plist-get pr :approvals) 0)))
               prs)))
    (secure-hash 'md5 (string-join (sort rows #'string<) "\n"))))

(provide 'magit-dash-pr)

;;; magit-gh-pr.el ends here
