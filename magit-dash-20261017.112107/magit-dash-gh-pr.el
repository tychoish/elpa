;;; magit-dash-gh-pr.el --- Fetch GitHub PR comments and review threads to disk -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.0") (annotated-completing-read "0.1"))
;; Keywords: vc, tools, magit, github
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides `magit-dash-gh-pr-fetch', which downloads PR review threads and
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
(require 'magit-dash-gh)

(declare-function magit-toplevel "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-call-git "magit-process")
(declare-function magit-status-setup-buffer "magit-status")
(declare-function magit-dash--format-age "magit-dash")
(declare-function magit-dash-repo-p "magit-dash")
(declare-function magit-dash-repo-path "magit-dash")
(declare-function magit-dash--repo-at-point "magit-dash")
(declare-function magit-dash--repo-at-point-p "magit-dash")
(declare-function magit-dash-gh-actions-fetch-for-pr "magit-dash-gh-actions")
;;; Configuration

(defvar magit-dash-gh-pr-thread-limit 100
  "Maximum number of review threads to fetch per pull request.")

;;; Internal helpers

(defun magit-dash-gh-pr--parse-threads (graphql-response)
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

(defun magit-dash-gh-pr--thread-table (thread)
  "Convert THREAD plist to a string-keyed hash-table for serialization."
  (let ((ht (make-hash-table :test #'equal)))
    (puthash "id"             (or (plist-get thread :id) "")                  ht)
    (puthash "resolved"       (if (plist-get thread :resolved) t :false)      ht)
    (puthash "path"           (or (plist-get thread :path) "")                ht)
    (puthash "creator"        (or (plist-get thread :creator) "")             ht)
    (puthash "last_commentor" (or (plist-get thread :last-commentor) "")      ht)
    ht))

(defun magit-dash-gh-pr--graphql-query ()
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
}" magit-dash-gh-pr-thread-limit))

;;; Pipeline steps

(defun magit-dash-gh-pr--step-finalize (ctx)
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
                                                (seq-map #'magit-dash-gh-pr--thread-table
                                                         threads)))))
    (magit-dash-gh--write-index dir data)
    (message "magit-gh-pr: done — %d file(s) in %s" (length files) dir)))

(defun magit-dash-gh-pr--step-comments (ctx)
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
         (magit-dash-gh-pr--step-finalize
          (magit-dash-gh--add-file ctx file "issue-comments"))))
     (magit-dash-gh--make-error-handler "magit-gh-pr" "issue-comments"))))

(defun magit-dash-gh-pr--step-threads (ctx)
  "Fetch review threads via GraphQL, write pr-review-threads.json."
  (let* ((owner    (plist-get ctx :owner))
         (repo     (plist-get ctx :repo))
         (pr-num   (plist-get ctx :pr-number))
         (repo-dir (plist-get ctx :repo-dir)))
    (message "magit-gh-pr: fetching review threads...")
    (magit-dash-gh--run-process
     (list "api" "graphql"
           "-f" (format "query=%s" (magit-dash-gh-pr--graphql-query))
           "-F" (format "owner=%s" owner)
           "-F" (format "repo=%s"  repo)
           "-F" (format "number=%s" pr-num))
     repo-dir
     (lambda (output)
       (let* ((file    "pr-review-threads.json")
              (parsed  (json-parse-string output
                                          :array-type  'list
                                          :object-type 'alist))
              (threads (or (magit-dash-gh-pr--parse-threads parsed) nil)))
         (with-temp-file (expand-file-name file (plist-get ctx :dir))
           (insert output))
         (magit-dash-gh-pr--step-comments
          (magit-dash-gh--add-file
           (plist-put ctx :threads threads)
           file "review-threads"))))
     (magit-dash-gh--make-error-handler "magit-gh-pr" "review-threads"))))

(defun magit-dash-gh-pr--step-info (ctx)
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
         (magit-dash-gh-pr--step-threads
          (magit-dash-gh--add-file ctx2 file "metadata"))))
     (magit-dash-gh--make-error-handler "magit-gh-pr" "pr-info"))))

;;; Public API

;;;###autoload
(defun magit-dash-gh-pr-fetch (&optional pr-number)
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
    (magit-dash-gh-pr--step-info ctx)))

;;;; PR dashboard mode

(defvar magit-dash-gh-pr-columns
  '((org . nil) (repo . t) (worktree . t) (pr . t) (title . t) (ci . t) (age . t) (comments . t) (review . t))
  "Alist of (COLUMN-SYMBOL . ENABLED) for the PR dashboard.
Persisted across sessions via `savehist-additional-variables'.")

(add-to-list 'savehist-additional-variables 'magit-dash-gh-pr-columns)

(defconst magit-dash-gh-pr--all-columns
  '(org repo worktree pr title ci age comments review)
  "All available PR dashboard columns in display order.")

(defconst magit-dash-gh-pr--column-defs
  '((org      . ("Org"     10 t))
    (repo     . ("Repo"    16 t))
    (worktree . ("WT"      3  t))
    (pr       . ("PR#"     5  nil))
    (title    . ("Title"   38 t))
    (ci       . ("CI"      8  t))
    (age      . ("Age"     6  nil))
    (comments . ("Cmts"    4  nil))
    (review   . ("Review"  14 t)))
  "Alist of COLUMN-SYMBOL to default (LABEL WIDTH SORTABLE) for PR columns.
Org and Repo widths are computed dynamically in `magit-dash-gh-pr-dashboard--build-format'.")

(defcustom magit-dash-gh-pr-show-org nil
  "When non-nil, display a separate Org column in the PR dashboard.
Kept in sync with `magit-dash-gh-pr-columns'."
  :type 'boolean
  :group 'magit-dash
  :set (lambda (sym val)
         (set-default sym val)
         (when (boundp 'magit-dash-gh-pr-columns)
           (setf (alist-get 'org magit-dash-gh-pr-columns) (and val t)))))

(defun magit-dash-gh-pr--column-enabled-p (col)
  "Return non-nil when column COL is enabled in `magit-dash-gh-pr-columns'."
  (if (eq col 'org)
      (or (and (boundp 'magit-dash-gh-pr-show-org) magit-dash-gh-pr-show-org)
          (cdr (assq 'org magit-dash-gh-pr-columns)))
    (if-let* ((entry (assq col magit-dash-gh-pr-columns)))
        (cdr entry)
      nil)))

(defun magit-dash-gh-pr--active-columns ()
  "Return PR column symbols that are currently enabled, in display order."
  (seq-filter #'magit-dash-gh-pr--column-enabled-p
              magit-dash-gh-pr--all-columns))

(defun magit-dash-gh-pr-toggle-column (col)
  "Toggle visibility of column COL in the PR dashboard and refresh."
  (interactive
   (list (intern (completing-read "Toggle PR column: "
                                  (seq-map #'symbol-name
                                           magit-dash-gh-pr--all-columns)
                                  nil t))))
  (let ((new-val (not (magit-dash-gh-pr--column-enabled-p col))))
    (setf (alist-get col magit-dash-gh-pr-columns) new-val)
    (when (eq col 'org)
      (setq magit-dash-gh-pr-show-org new-val)))
  (when (derived-mode-p 'magit-dash-gh-pr-dashboard-mode)
    (setq tabulated-list-format (magit-dash-gh-pr-dashboard--build-format))
    (tabulated-list-init-header)
    (when tabulated-list-entries
      (setq tabulated-list-entries
            (seq-map (lambda (entry)
                       (let ((id (car entry)))
                         (magit-dash-gh-pr-dashboard--build-entry
                          (plist-get id :pr)
                          (plist-get id :repo))))
                     tabulated-list-entries))
      (tabulated-list-print t))
    (magit-dash-gh-pr-dashboard-refresh)))

(defun magit-dash-gh-pr-toggle-org ()
  "Toggle visibility of the Org column in the PR dashboard."
  (interactive)
  (magit-dash-gh-pr-toggle-column 'org))

(defun magit-dash-gh-pr--split-slug (slug)
  "Split SLUG (owner/repo or repo) into cons (OWNER . REPO)."
  (if (and slug (string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" slug))
      (cons (match-string 1 slug) (match-string 2 slug))
    (cons "" (or slug ""))))

(defvar-local magit-dash-gh-pr-dashboard--filters
  (list :state "open" :author "@me" :repo nil :org nil)
  "Current PR dashboard filters plist.
Keys: :state (\"open\"/\"closed\"), :author, :repo (OWNER/NAME), :org.")

(defface magit-dash-gh-pr-ci-pass-face
  '((t :inherit success))
  "Face for passing CI status in the PR dashboard."
  :group 'magit-dash)

(defface magit-dash-gh-pr-ci-fail-face
  '((t :inherit error))
  "Face for failing CI status in the PR dashboard."
  :group 'magit-dash)

(defface magit-dash-gh-pr-ci-pending-face
  '((t :inherit warning))
  "Face for pending CI status in the PR dashboard."
  :group 'magit-dash)

(defun magit-dash-gh-pr-dashboard--build-format (&optional width entries)
  "Return the `tabulated-list-format' vector with elastic columns using active columns.
WIDTH is available window width; when nil, `window-width' is used.
ENTRIES is the list of tabulated-list entries."
  (let* ((ents (or entries (and (boundp 'tabulated-list-entries) tabulated-list-entries)))
         (active (magit-dash-gh-pr--active-columns))
         (show-org (magit-dash-gh-pr--column-enabled-p 'org))
         (raw-repo (when ents
                     (seq-reduce (lambda (w e)
                                   (let* ((slug (or (plist-get (car e) :repo) ""))
                                          (pair (magit-dash-gh-pr--split-slug slug))
                                          (name (if show-org (cdr pair) (if (string-empty-p (cdr pair)) (car pair) (cdr pair)))))
                                     (max w (length name))))
                                 ents 0)))
         (raw-org (when (and show-org ents)
                    (seq-reduce (lambda (w e)
                                  (let* ((slug (or (plist-get (car e) :repo) ""))
                                         (pair (magit-dash-gh-pr--split-slug slug)))
                                    (max w (length (car pair)))))
                                ents 0)))
         (org-width (if show-org (max 6 (+ (or raw-org 3) 2)) 0))
         (fixed-width (seq-reduce
                       (lambda (acc col)
                         (if (and (not (eq col 'repo))
                                  (not (eq col 'org)))
                             (+ acc (cadr (alist-get col magit-dash-gh-pr--column-defs)))
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
                        (_     (alist-get col magit-dash-gh-pr--column-defs))))
                    active))))

(defvar magit-dash-gh-pr-dashboard-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'magit-dash-gh-pr-dashboard-open-browser)
    (define-key m (kbd "w")   #'magit-dash-gh-pr-dashboard-open-worktree)
    (define-key m (kbd "W")   #'magit-dash-gh-pr-dashboard-open-worktree)
    (define-key m (kbd "d")   #'magit-dash-gh-pr-dashboard-open-dired)
    (define-key m (kbd "O")   #'magit-dash-gh-pr-toggle-org)
    (define-key m (kbd "T")   #'magit-dash-gh-pr-toggle-column)
    (define-key m (kbd "v")   #'magit-dash-gh-pr-dashboard-switch-view)
    (define-key m (kbd "g")   #'magit-dash-gh-pr-dashboard-refresh)
    (define-key m (kbd "r")   #'magit-dash-gh-pr-dashboard-refresh)
    (define-key m (kbd "f")   #'magit-dash-gh-pr-dashboard-set-filter)
    (define-key m (kbd "/")   #'magit-dash-gh-pr-dashboard-set-filter)
    (define-key m (kbd "F")   #'magit-dash-gh-pr-dashboard-clear-filters)
    (define-key m (kbd "c")   #'magit-dash-gh-pr-dashboard-fetch-ci)
    (define-key m (kbd "q")   #'magit-dash-gh-pr-dashboard-quit)
    (define-key m (kbd "m")   #'magit-dash-gh-pr-dashboard-menu)
    (define-key m (kbd "?")   #'magit-dash-gh-pr-dashboard-menu)
    m)
  "Keymap for `magit-dash-gh-pr-dashboard-mode'.")
(define-derived-mode magit-dash-gh-pr-dashboard-mode tabulated-list-mode "PRs"
  "Major mode for the pull request dashboard."
  (setq tabulated-list-format (magit-dash-gh-pr-dashboard--build-format))
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (setq-local magit-dash-gh-pr-dashboard--filters
              (list :state "open" :author nil :repo nil :org nil)))

(defun magit-dash-gh-pr--parse-status-rollup (rollup)
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

(defun magit-dash-gh-pr-dashboard--format-ci (rollup-state)
  "Format CI ROLLUP-STATE string for display."
  (pcase rollup-state
    ("SUCCESS" (propertize "pass" 'face 'magit-dash-gh-pr-ci-pass-face))
    ("FAILURE" (propertize "fail" 'face 'magit-dash-gh-pr-ci-fail-face))
    ("PENDING" (propertize "pending" 'face 'magit-dash-gh-pr-ci-pending-face))
    ("ERROR" (propertize "error" 'face 'magit-dash-gh-pr-ci-fail-face))
    (_ "—")))

(defun magit-dash-gh-pr-dashboard--format-review (decision)
  "Format review DECISION string for display."
  (pcase decision
    ("APPROVED" (propertize "approved" 'face 'success))
    ("CHANGES_REQUESTED" (propertize "changes req" 'face 'error))
    ("REVIEW_REQUIRED" (propertize "needed" 'face 'warning))
    (_ "")))

(defun magit-dash-gh-pr-dashboard--format-age (updated-at)
  "Format UPDATED-AT ISO timestamp as a human-readable age string."
  (when (and updated-at (not (equal updated-at "")))
    (condition-case nil
        (magit-dash--format-age
         (float-time (time-since (date-to-time updated-at))))
      (error "?"))))

(defun magit-dash-gh-pr-dashboard--comments-count (pr)
  "Return the comment count from PR alist.
Handles both an integer `comments' field and a nested `(totalCount . N)' alist,
as well as a top-level `commentsCount' integer."
  (let ((c (map-elt pr 'comments)))
    (cond
     ((numberp c) c)
     ((and c (map-elt c 'totalCount)) (map-elt c 'totalCount))
     (t (let ((n (map-elt pr 'commentsCount)))
          (if (numberp n) n 0))))))

(defun magit-dash-gh-pr-dashboard--build-args (filters)
  "Return a gh args list for fetching PRs matching FILTERS plist.
When :repo is set, uses `gh pr list -R REPO' (includes CI status).
Otherwise uses `gh search prs' for cross-repo listing."
  (let* ((state (or (plist-get filters :state) "open"))
         (repo (plist-get filters :repo))
         (author (plist-get filters :author))
         (org (plist-get filters :org))
         (review-requested (plist-get filters :review-requested)))
    (if repo
        (append
         (list "pr" "list" "-R" repo "--state" state
               "--json"
               "number,title,state,author,updatedAt,comments,reviewDecision,statusCheckRollup,isDraft,url"
               "--jq" "map(. + {commentsCount: (.comments | length)} | del(.comments))")
         (when author (list "--author" author)))
      (append
       (list "search" "prs"
             "--state" (if (member state '("open" "closed")) state "open")
             "--json"
             "number,title,state,repository,author,updatedAt,commentsCount,isDraft,url")
       (when author
         (list "--author" author))
       (when review-requested
         (list "--review-requested" review-requested))
       (when org
         (list "--owner" org))))))

(defconst magit-dash-gh-pr-dashboard--title-width 36
  "Maximum display width for PR title column.")

(defun magit-dash-gh-pr-dashboard--build-entry (pr repo-name)
  "Return a `tabulated-list-entries' entry for PR alist in REPO-NAME using active columns."
  (let* ((number (map-elt pr 'number))
         (entry-id (list :number number :repo repo-name :pr pr))
         (pair (magit-dash-gh-pr--split-slug repo-name))
         (org-str (car pair))
         (short-repo (if (string-empty-p (cdr pair)) (car pair) (cdr pair)))
         (active (magit-dash-gh-pr--active-columns)))
    (list entry-id
          (apply #'vector
                 (seq-map
                  (lambda (col)
                    (pcase col
                      ('org org-str)
                      ('repo short-repo)
                      ('worktree
                       (if (magit-dash-gh-pr-dashboard--worktree-exists-p entry-id)
                           (propertize "✓" 'face 'success)
                         (propertize "·" 'face 'shadow)))
                      ('pr (number-to-string number))
                      ('title
                       (truncate-string-to-width
                        (or (map-elt pr 'title) "")
                        magit-dash-gh-pr-dashboard--title-width nil nil "…"))
                      ('ci
                       (magit-dash-gh-pr-dashboard--format-ci
                        (magit-dash-gh-pr--parse-status-rollup (map-elt pr 'statusCheckRollup))))
                      ('age
                       (or (magit-dash-gh-pr-dashboard--format-age (map-elt pr 'updatedAt)) "?"))
                      ('comments
                       (number-to-string (magit-dash-gh-pr-dashboard--comments-count pr)))
                      ('review
                       (magit-dash-gh-pr-dashboard--format-review (map-elt pr 'reviewDecision)))))
                  active)))))

(defun magit-dash-gh-pr-dashboard--parse-output (output filters)
  "Parse gh JSON OUTPUT into tabulated list entries using FILTERS for context.
Returns nil when OUTPUT is not a JSON array."
  (let ((trimmed (string-trim output)))
    (when (string-prefix-p "[" trimmed)
      (when-let* ((prs (json-parse-string trimmed :array-type 'list :object-type 'alist)))
        (let ((per-repo-p (plist-get filters :repo)))
          (seq-map (lambda (pr)
                     (magit-dash-gh-pr-dashboard--build-entry
                      pr
                      (if per-repo-p
                          (plist-get filters :repo)
                        (or (map-elt (map-elt pr 'repository) 'nameWithOwner)
                            "unknown"))))
                   prs))))))

(defun magit-dash-gh-pr-dashboard--merge-ci (buf repo output)
  "Merge statusCheckRollup values from OUTPUT into REPO's rows in BUF.
OUTPUT is `gh pr list' JSON with `number' and `statusCheckRollup' fields."
  (when (buffer-live-p buf)
    (let ((trimmed (string-trim output)))
      (when (string-prefix-p "[" trimmed)
        (when-let* ((rollups (json-parse-string trimmed :array-type 'list :object-type 'alist))
                    (table (make-hash-table :test #'eql)))
          (dolist (r rollups)
            (puthash (map-elt r 'number) (map-elt r 'statusCheckRollup) table))
          (with-current-buffer buf
            (setq tabulated-list-entries
                  (seq-map (lambda (entry)
                             (let* ((id (car entry))
                                    (rollup (and (equal (plist-get id :repo) repo)
                                                 (gethash (plist-get id :number) table))))
                               (if rollup
                                   (magit-dash-gh-pr-dashboard--build-entry
                                    (cons (cons 'statusCheckRollup rollup) (plist-get id :pr))
                                    repo)
                                 entry)))
                           tabulated-list-entries))
            (tabulated-list-print t)))))))

(defun magit-dash-gh-pr-dashboard--enrich-ci (buf state dir)
  "Fetch CI status per repo and merge it into BUF's rows for STATE using `gen-run-async'.
Used for aggregate (cross-repo) views, whose listing endpoint does not
return `statusCheckRollup'.  DIR is the working directory for `gh'."
  (when (buffer-live-p buf)
    (let* ((repos (delete-dups
                   (with-current-buffer buf
                     (seq-remove #'null
                                 (seq-map (lambda (entry) (plist-get (car entry) :repo))
                                          tabulated-list-entries)))))
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
                (list "pr" "list" "-R" repo "--state" state
                      "--json" "number,statusCheckRollup" "--limit" "200")
                dir
                (lambda (output)
                  (magit-dash-gh-pr-dashboard--merge-ci buf repo output)
                  (funcall done))
                (lambda (_output _code)
                  (funcall done)))))
           repos
           :concurrency limit
           :delay (or (bound-and-true-p magit-dash-queue-yield-delay) 0.0))
        (dolist (repo repos)
          (magit-dash-gh--run-process
           (list "pr" "list" "-R" repo "--state" state
                 "--json" "number,statusCheckRollup" "--limit" "200")
           dir
           (lambda (output) (magit-dash-gh-pr-dashboard--merge-ci buf repo output))
           (lambda (_output _code) nil)))))))

(defun magit-dash-gh-pr-dashboard-refresh ()
  "Fetch PRs matching current filters and repopulate the PR table."
  (interactive)
  (let* ((filters magit-dash-gh-pr-dashboard--filters)
         (args (magit-dash-gh-pr-dashboard--build-args filters))
         (buf (current-buffer))
         (dir (or (ignore-errors (magit-dash-gh--repo-dir))
                  (expand-file-name "~"))))
    (message "magit-gh: fetching PRs...")
    (magit-dash-gh--run-process
     args dir
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (setq tabulated-list-entries
                 (or (magit-dash-gh-pr-dashboard--parse-output output filters) nil))
           (setq tabulated-list-format
                 (magit-dash-gh-pr-dashboard--build-format
                  (window-width (get-buffer-window buf t))
                  tabulated-list-entries))
           (tabulated-list-init-header)
           (tabulated-list-print t)
           (message "magit-gh: %d PR(s)"
                    (length tabulated-list-entries)))
         (unless (plist-get filters :repo)
           (magit-dash-gh-pr-dashboard--enrich-ci
            buf (or (plist-get filters :state) "open") dir)))))))

(defun magit-dash-gh-pr-dashboard--entry-at-point ()
  "Return the PR entry plist at point or signal `user-error'."
  (or (tabulated-list-get-id)
      (user-error "No pull request at point")))

(defun magit-dash-gh-pr-dashboard-open-browser ()
  "Open the pull request at point in a web browser."
  (interactive)
  (when-let* ((entry (magit-dash-gh-pr-dashboard--entry-at-point))
              (pr (plist-get entry :pr))
              (url (map-elt pr 'url)))
    (browse-url url)))

(defun magit-dash-gh-pr-dashboard-set-filter ()
  "Interactively set one PR dashboard filter field."
  (interactive)
  (let* ((current magit-dash-gh-pr-dashboard--filters)
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
    (setq magit-dash-gh-pr-dashboard--filters
          (plist-put (copy-sequence current) key new-val))
    (magit-dash-gh-pr-dashboard-refresh)))

(defvar magit-dash-gh-pr-view-history nil
  "History list for `magit-dash-gh-pr-dashboard-switch-view'.")

(defun magit-dash-gh-pr-dashboard-switch-view (&optional view-key)
  "Switch the PR dashboard to a preset view (VIEW-KEY).
Interactively prompts with available PR views."
  (interactive)
  (let* ((candidates
          '(("all"          . "All open PRs (aggregated across tracked repos)")
            ("my-prs"       . "Authored by me (@me)")
            ("needs-review" . "Review requested (@me)")
            ("repo"         . "Filtered by repository…")
            ("closed"       . "Closed / Merged pull requests")))
         (choice (or view-key
                     (if (fboundp 'acr-completing-read)
                         (acr-completing-read
                          "Switch PR view: "
                          (mapcar #'car candidates)
                          :annotation-fn (lambda (k) (concat "  " (alist-get k candidates nil nil #'equal)))
                          :history 'magit-dash-gh-pr-view-history)
                       (completing-read
                        "Switch PR view: "
                        (mapcar (lambda (c) (format "%-14s %s" (car c) (cdr c))) candidates)
                        nil t)))))
    (let ((key (if (string-match "\\`\\([^ ]+\\)" choice)
                   (match-string 1 choice)
                 choice)))
      (pcase key
        ("all"
         (setq magit-dash-gh-pr-dashboard--filters
               (list :state "open" :author nil :repo nil :org nil)))
        ("my-prs"
         (setq magit-dash-gh-pr-dashboard--filters
               (list :state "open" :author "@me" :repo nil :org nil)))
        ("needs-review"
         (setq magit-dash-gh-pr-dashboard--filters
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
           (setq magit-dash-gh-pr-dashboard--filters
                 (list :state "open" :author nil :repo selected :org nil))))
        ("closed"
         (setq magit-dash-gh-pr-dashboard--filters
               (list :state "closed" :author nil :repo nil :org nil))))
      (magit-dash-gh-pr-dashboard-refresh))))

(defun magit-dash-gh-pr-dashboard-quit ()
  "Quit the PR dashboard.
Deletes the window (restoring window layout if split beneath) and buries the buffer."
  (interactive)
  (let ((buf (current-buffer))
        (win (selected-window)))
    (if (> (length (window-list)) 1)
        (progn
          (bury-buffer buf)
          (delete-window win))
      (bury-buffer buf))))

(defun magit-dash-gh-pr-dashboard-clear-filters ()
  "Reset all dashboard filters to their defaults (open PRs) and refresh."
  (interactive)
  (setq-local magit-dash-gh-pr-dashboard--filters
              (list :state "open" :author nil :repo nil :org nil))
  (magit-dash-gh-pr-dashboard-refresh))

(defconst magit-dash-gh-pr-buffer-name "*magit-gh-pr-dash*"
  "Buffer name for the pull request dashboard.")

;;;###autoload
(defun magit-gh-pr-dash (&optional repo)
  "Open the GitHub pull request dashboard buffer.
When opened from a repository dashboard window, splits the window beneath.
Aggregates all open PRs by default across repositories, or for REPO when provided."
  (interactive)
  (magit-dash-gh--check-gh)
  (let* ((orig-buf (current-buffer))
         (orig-win (selected-window))
         (from-dash (or (derived-mode-p 'magit-dash-mode)
                        (string= (buffer-name orig-buf) (or (bound-and-true-p magit-dash-buffer-name) "*magit-dash-repos*"))))
         (buf (get-buffer-create magit-dash-gh-pr-buffer-name)))
    (with-current-buffer buf
      (magit-dash-gh-pr-dashboard-mode)
      (when repo
        (setq magit-dash-gh-pr-dashboard--filters
              (plist-put (copy-sequence magit-dash-gh-pr-dashboard--filters) :repo repo)))
      (magit-dash-gh-pr-dashboard-refresh))
    (if (and from-dash (window-live-p orig-win))
        (let ((pr-win (or (get-buffer-window buf)
                          (split-window orig-win (max 10 (/ (window-total-height orig-win) 2)) 'below))))
          (set-window-buffer pr-win buf)
          (select-window pr-win))
      (pop-to-buffer buf))))

;;;###autoload
(defalias 'magit-dash-gh-pr-dashboard-open #'magit-gh-pr-dash)
;;;###autoload
(defalias 'magit-gh-prs #'magit-gh-pr-dash)
;;;###autoload
(defalias 'magit-dash-gh-pr-dash #'magit-gh-pr-dash)

(defun magit-dash-gh-pr-dashboard--open-with-filters (filters)
  "Open the PR dashboard buffer showing pull requests matching FILTERS."
  (magit-dash-gh--check-gh)
  (let ((buf (get-buffer-create magit-dash-gh-pr-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'magit-dash-gh-pr-dashboard-mode)
        (magit-dash-gh-pr-dashboard-mode))
      (setq-local magit-dash-gh-pr-dashboard--filters filters)
      (magit-dash-gh-pr-dashboard-refresh))
    (pop-to-buffer buf)))

(defun magit-dash-gh-pr-dashboard--current-repo-slug ()
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
(defun magit-dash-gh-pr-dash-all ()
  "Open the PR dashboard showing all open pull requests across repositories."
  (interactive)
  (magit-dash-gh-pr-dashboard--open-with-filters
   (list :state "open" :author nil :repo nil :org nil)))

;;;###autoload
(defun magit-dash-gh-pr-dash-mine ()
  "Open the PR dashboard showing your open pull requests across repositories."
  (interactive)
  (magit-dash-gh-pr-dashboard--open-with-filters
   (list :state "open" :author "@me" :repo nil :org nil)))

;;;###autoload
(defun magit-dash-gh-pr-dash-here ()
  "Open the PR dashboard showing open pull requests for the repo at point."
  (interactive)
  (magit-dash-gh-pr-dashboard--open-with-filters
   (list :state "open" :author nil
         :repo (magit-dash-gh-pr-dashboard--current-repo-slug) :org nil)))

;;;###autoload
(defun magit-dash-gh-pr-dash-mine-here ()
  "Open the PR dashboard showing your open pull requests for the repo at point."
  (interactive)
  (magit-dash-gh-pr-dashboard--open-with-filters
   (list :state "open" :author "@me"
         :repo (magit-dash-gh-pr-dashboard--current-repo-slug) :org nil)))

(defun magit-dash-gh-pr-dashboard--worktree-path (entry)
  "Return existing or projected worktree path for PR ENTRY."
  (let* ((pr-num (plist-get entry :number))
         (repo (plist-get entry :repo))
         (local-path (magit-dash-gh-pr-dashboard--find-local-path repo)))
    (when local-path
      (let* ((repo-dir (expand-file-name local-path))
             (repo-base (file-name-nondirectory (directory-file-name repo-dir))))
        (expand-file-name (format "%s-pr-%d" repo-base pr-num)
                          (file-name-directory (directory-file-name repo-dir)))))))

(defun magit-dash-gh-pr-dashboard--worktree-exists-p (entry)
  "Return non-nil if a local worktree directory exists for PR ENTRY."
  (when-let* ((wt-dir (magit-dash-gh-pr-dashboard--worktree-path entry)))
    (file-directory-p wt-dir)))

(defun magit-dash-gh-pr-dashboard-open-dired (&optional entry)
  "Open Dired in the worktree for the pull request at point (or ENTRY).
If the worktree does not exist yet, creates it first."
  (interactive)
  (let* ((entry (or entry (magit-dash-gh-pr-dashboard--entry-at-point)))
         (wt-dir (magit-dash-gh-pr-dashboard--worktree-path entry)))
    (if (and wt-dir (file-directory-p wt-dir))
        (dired wt-dir)
      (magit-dash-gh-pr-dashboard-open-worktree entry)
      (when-let* ((dir (magit-dash-gh-pr-dashboard--worktree-path entry)))
        (when (file-directory-p dir)
          (dired dir))))))

(defun magit-dash-gh-pr-dashboard--find-local-path (repo-slug)
  "Return a local checkout path for REPO-SLUG (owner/name), or nil.
Searches `magit-dash-repo-list' when bound, matching the repo name
component of REPO-SLUG against the final directory component of each entry."
  (when (and (boundp 'magit-dash-repo-list)
             (string-match ".*/\\([^/]+\\)$" repo-slug))
    (let ((repo-name (match-string 1 repo-slug)))
      (seq-some (lambda (r)
                  (when (and (magit-dash-repo-p r)
                             (string= repo-name
                                      (file-name-nondirectory
                                       (directory-file-name
                                        (magit-dash-repo-path r)))))
                    (magit-dash-repo-path r)))
                magit-dash-repo-list))))

(defun magit-dash-gh-pr-dashboard-open-worktree (&optional entry)
  "Create and open a git worktree for the pull request at point (or ENTRY).
If a worktree for this PR already exists, switches to its magit status buffer.
Otherwise, fetches the PR branch from GitHub, adds the worktree, and opens
its magit status buffer."
  (interactive)
  (let* ((entry (or entry (magit-dash-gh-pr-dashboard--entry-at-point)))
         (pr-num (plist-get entry :number))
         (repo (plist-get entry :repo))
         (local-path (or (magit-dash-gh-pr-dashboard--find-local-path repo)
                         (let ((prompt-path (read-directory-name (format "Local repository path for %s: " repo))))
                           (if (and prompt-path (file-directory-p prompt-path))
                               (expand-file-name prompt-path)
                             (user-error "No registered local checkout for %s" repo)))))
         (repo-dir (expand-file-name local-path))
         (repo-base (file-name-nondirectory (directory-file-name repo-dir)))
         (wt-dir (expand-file-name (format "%s-pr-%d" repo-base pr-num)
                                   (file-name-directory (directory-file-name repo-dir))))
         (branch-name (format "pr-%d" pr-num)))
    (if (file-directory-p wt-dir)
        (progn
          (message "magit-gh: opening existing worktree at %s" wt-dir)
          (magit-status-setup-buffer wt-dir))
      (message "magit-gh: fetching PR #%d in %s..." pr-num repo-base)
      (let ((default-directory repo-dir))
        (magit-call-git "fetch" "origin" (format "pull/%d/head:%s" pr-num branch-name))
        (let ((exit-code (magit-call-git "worktree" "add" wt-dir branch-name)))
          (if (= exit-code 0)
              (progn
                (when (fboundp 'magit-dash-worktree-symlink-worktree)
                  (magit-dash-worktree-symlink-worktree wt-dir repo-dir))
                (message "magit-gh: opened worktree for PR #%d at %s" pr-num wt-dir)
                (magit-status-setup-buffer wt-dir))
            (user-error "Failed to create worktree at %s (exit %d)" wt-dir exit-code)))))))

(defun magit-dash-gh-pr-dashboard-fetch-ci ()
  "Fetch GitHub Actions CI logs for the pull request at point."
  (interactive)
  (require 'magit-dash-gh-actions)
  (let* ((entry (magit-dash-gh-pr-dashboard--entry-at-point))
         (repo (plist-get entry :repo))
         (path (or (magit-dash-gh-pr-dashboard--find-local-path repo)
                   (user-error "No registered local checkout for %s" repo))))
    (magit-dash-gh-actions-fetch-for-pr (plist-get entry :number) path)))

(transient-define-prefix magit-dash-gh-pr-dashboard-menu ()
  "Actions for the PR dashboard."
  [["Pull Request"
    ("b"   "Open in browser"       magit-dash-gh-pr-dashboard-open-browser)]
   ["Worktree"
    ("w"   "Open Magit in worktree" magit-dash-gh-pr-dashboard-open-worktree)
    ("d"   "Open Dired in worktree" magit-dash-gh-pr-dashboard-open-dired)]
   ["CI"
    ("ci"  "Fetch CI logs"         magit-dash-gh-pr-dashboard-fetch-ci)]
   ["View & Filter"
    ("v"   "Switch view…"          magit-dash-gh-pr-dashboard-switch-view)
    ("tc"  "Toggle column…"        magit-dash-gh-pr-toggle-column)
    ("to"  "Toggle Org column"     magit-dash-gh-pr-toggle-org)
    ("fs"  "Set filter…"           magit-dash-gh-pr-dashboard-set-filter)
    ("fc"  "Clear all filters"     magit-dash-gh-pr-dashboard-clear-filters)]
   ["Dashboard"
    ("g"   "Refresh"               magit-dash-gh-pr-dashboard-refresh)
    ("q"   "Quit"                  magit-dash-gh-pr-dashboard-quit)]])

;;;###autoload (autoload 'magit-dash-gh-pr-views-menu "magit-dash-gh-pr" nil t)
(transient-define-prefix magit-dash-gh-pr-views-menu ()
  "Jump to a pull request dashboard view."
  [["Pull Requests"
    ("a" "All repos — open PRs"  magit-dash-gh-pr-dash-all)
    ("m" "All repos — my PRs"    magit-dash-gh-pr-dash-mine)
    ("h" "This repo — open PRs"  magit-dash-gh-pr-dash-here)
    ("n" "This repo — my PRs"    magit-dash-gh-pr-dash-mine-here)]])

(provide 'magit-dash-gh-pr)

;;; magit-gh-pr.el ends here
