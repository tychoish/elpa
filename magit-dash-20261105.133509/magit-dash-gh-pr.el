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
(require 'savehist)
(require 'magit-dash-gh)

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
(declare-function magit-dash--repo-at-point "magit-dash")
(declare-function magit-dash--repo-at-point-p "magit-dash")
(declare-function magit-dash-worktree-uncommitted-changes-p "magit-dash-worktree")
(declare-function magit-dash-worktree-symlink-worktree "magit-dash-worktree")
(declare-function magit-dash-worktree-main-dir "magit-dash-worktree")
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

(defcustom magit-dash-gh-pr-worktree-path-format 'dot-wt
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

(defvar magit-dash-gh-pr-columns
  '((org . nil) (repo . t) (pr . t) (ci . t) (title . t) (worktree . t) (updated . t) (age . nil) (comments . t) (review . t))
  "Alist of (COLUMN-SYMBOL . ENABLED) for the PR dashboard.
Persisted across sessions via `savehist-additional-variables'.")


(defconst magit-dash-gh-pr--all-columns
  '(org repo pr ci title worktree updated age comments review)
  "All available PR dashboard columns in display order.")

(defconst magit-dash-gh-pr--column-defs
  '((org      . ("Org"     10 t))
    (repo     . ("Repo"    16 t))
    (pr       . ("PR#"     5  nil))
    (ci       . ("CI"      3  t))
    (title    . ("Title"   38 t))
    (worktree . ("WT"      3  t))
    (updated  . ("Updated" 8  nil))
    (age      . ("Age"     6  nil))
    (comments . ("Cmts"    4  nil))
    (review   . ("Review"  14 t)))
  "Alist of COLUMN-SYMBOL to default (LABEL WIDTH SORTABLE) for PR columns.
Org and Repo widths are computed dynamically in
`magit-dash-gh-pr-dashboard--build-format'.")

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
      (if (eq col 'updated)
          (if-let* ((age-entry (assq 'age magit-dash-gh-pr-columns)))
              (cdr age-entry)
            t)
        nil))))

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

(defvar-local magit-dash-gh-pr-dashboard--marked-entries nil
  "List of marked PR keys (cons of (REPO . NUMBER)) in the current PR dashboard.")

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
  "Return `tabulated-list-format' vector with elastic columns.
Uses active columns.  WIDTH is available window width; when nil,
`window-width' is used.  ENTRIES is the list of tabulated-list entries."
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
    (keymap-set m "RET" #'magit-dash-gh-pr-dashboard-open-browser)
    (keymap-set m "w"   #'magit-dash-gh-pr-dashboard-open-worktree)
    (keymap-set m "c w" #'magit-dash-gh-pr-dashboard-create-worktree)
    (keymap-set m "c a" #'magit-dash-gh-pr-dashboard-create-all-worktrees)
    (keymap-set m "a"   #'magit-dash-gh-pr-dashboard-create-all-worktrees)
    (keymap-set m "c i" #'magit-dash-gh-pr-dashboard-fetch-ci)
    (keymap-set m "k"   #'magit-dash-gh-pr-dashboard-clean-merged-worktrees)
    (keymap-set m "d"   #'magit-dash-gh-pr-dashboard-open-dired)
    (keymap-set m "*"   #'magit-dash-gh-pr-dashboard-mark-all)
    (keymap-set m "SPC"   #'magit-dash-gh-pr-dashboard-toggle-mark)
    (keymap-set m "m m"   #'magit-dash-gh-pr-dashboard-toggle-mark)
    (keymap-set m "m u"   #'magit-dash-gh-pr-dashboard-unmark)
    (keymap-set m "m c"   #'magit-dash-gh-pr-dashboard-unmark-all)
    (keymap-set m "m a"   #'magit-dash-gh-pr-dashboard-mark-all)
    (keymap-set m "v o"   #'magit-dash-gh-pr-toggle-org)
    (keymap-set m "v t"   #'magit-dash-gh-pr-toggle-column)
    (keymap-set m "v s"   #'magit-dash-gh-pr-dashboard-switch-view)
    (keymap-set m "g"   #'magit-dash-gh-pr-dashboard-refresh)
    (keymap-set m "r"   #'magit-dash-gh-pr-dashboard-refresh)
    (keymap-set m "f"   #'magit-dash-gh-pr-dashboard-set-filter)
    (keymap-set m "/"   #'magit-dash-gh-pr-dashboard-set-filter)
    (keymap-set m "C-/"   #'magit-dash-gh-pr-dashboard-clear-filters)
    (keymap-set m "q"   #'magit-dash-gh-pr-dashboard-quit)
    (keymap-set m "?"   #'magit-dash-gh-pr-dashboard-menu)
    m)
  "Keymap for `magit-dash-gh-pr-dashboard-mode'.")

(define-derived-mode magit-dash-gh-pr-dashboard-mode tabulated-list-mode "PRs"
  "Major mode for the pull request dashboard."
  (setq tabulated-list-format (magit-dash-gh-pr-dashboard--build-format))
  (setq tabulated-list-sort-key nil)
  (setq-local mode-line-process " [a: create all worktrees, ?: menu]")
  (tabulated-list-init-header)
  (setq-local magit-dash-gh-pr-dashboard--filters
              (list :state "open" :author nil :repo nil :org nil))
  (setq-local magit-dash-gh-pr-dashboard--marked-entries nil))

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
  "Format CI ROLLUP-STATE string for display using checkmark/x glyphs.
Uses the same ✓/x/⟳ indicators as the repository dashboard."
  (pcase rollup-state
    ("SUCCESS" (propertize "✓" 'face 'magit-dash-gh-pr-ci-pass-face))
    ((or "FAILURE" "ERROR") (propertize "x" 'face 'magit-dash-gh-pr-ci-fail-face))
    ("PENDING" (propertize "⟳" 'face 'magit-dash-gh-pr-ci-pending-face))
    (_ (propertize "—" 'face 'shadow))))

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
               "number,title,state,mergedAt,author,createdAt,updatedAt,comments,reviewDecision,statusCheckRollup,isDraft,url,headRefName"
               "--jq" "map(. + {commentsCount: (.comments | length)} | del(.comments))")
         (when author (list "--author" author)))
      (append
       (list "search" "prs"
             "--state" (if (member state '("open" "closed")) state "open")
             "--json"
             "number,title,state,repository,author,createdAt,updatedAt,commentsCount,isDraft,url")
       (when author
         (list "--author" author))
       (when review-requested
         (list "--review-requested" review-requested))
       (when org
         (list "--owner" org))))))

(defconst magit-dash-gh-pr-dashboard--title-width 36
  "Maximum display width for PR title column.")

(defun magit-dash-gh-pr-dashboard--entry-key (entry)
  "Return unique key `(REPO . NUMBER)' for PR ENTRY plist."
  (cons (plist-get entry :repo) (plist-get entry :number)))

(defun magit-dash-gh-pr-dashboard--entry-marked-p (entry)
  "Return non-nil if PR ENTRY is marked in the current dashboard."
  (and (bound-and-true-p magit-dash-gh-pr-dashboard--marked-entries)
       (member (magit-dash-gh-pr-dashboard--entry-key entry)
               magit-dash-gh-pr-dashboard--marked-entries)
       t))

(defun magit-dash-gh-pr-dashboard--build-entry (pr repo-name)
  "Return a `tabulated-list-entries' entry for PR in REPO-NAME.
Uses active columns."
  (let* ((number (map-elt pr 'number))
         (entry-id (list :number number
                         :repo repo-name
                         :updated (map-elt pr 'updatedAt)
                         :head-ref (map-elt pr 'headRefName)
                         :pr pr))
         (is-marked (magit-dash-gh-pr-dashboard--entry-marked-p entry-id))
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
                      ('repo
                       (if is-marked
                           (propertize short-repo 'face 'bold)
                         short-repo))
                      ('worktree
                       (if (magit-dash-gh-pr-dashboard--worktree-exists-p entry-id)
                           (propertize "✓" 'face 'success)
                         (propertize "·" 'face 'shadow)))
                      ('pr
                       (let ((s (number-to-string number)))
                         (if is-marked
                             (propertize (concat "*" s) 'face 'bold)
                           s)))
                      ('title
                       (let ((t-str (truncate-string-to-width
                                     (or (map-elt pr 'title) "")
                                     magit-dash-gh-pr-dashboard--title-width nil nil "…")))
                         (if is-marked
                             (propertize t-str 'face 'bold)
                           t-str)))
                      ('ci
                        (magit-dash-gh-pr-dashboard--format-ci
                         (magit-dash-gh-pr--parse-status-rollup (map-elt pr 'statusCheckRollup))))
                      ('updated
                       (or (magit-dash-gh-pr-dashboard--format-age (map-elt pr 'updatedAt)) "?"))
                      ('age
                       (or (magit-dash-gh-pr-dashboard--format-age
                            (or (map-elt pr 'createdAt) (map-elt pr 'updatedAt))) "?"))
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
  "Merge statusCheckRollup, headRefName, state, and mergedAt from OUTPUT into rows.
Updates REPO's rows in BUF.
OUTPUT is `gh pr list' JSON."
  (when (buffer-live-p buf)
    (let ((trimmed (string-trim output)))
      (when (string-prefix-p "[" trimmed)
        (when-let* ((items (json-parse-string trimmed :array-type 'list :object-type 'alist))
                    (table (make-hash-table :test #'eql)))
          (dolist (item items)
            (puthash (map-elt item 'number) item table))
          (with-current-buffer buf
            (setq tabulated-list-entries
                  (seq-map (lambda (entry)
                             (let* ((id (car entry))
                                    (match (and (equal (plist-get id :repo) repo)
                                                (gethash (plist-get id :number) table))))
                               (if match
                                   (let* ((rollup (map-elt match 'statusCheckRollup))
                                          (head-ref (map-elt match 'headRefName))
                                          (st (map-elt match 'state))
                                          (merged-at (map-elt match 'mergedAt))
                                          (pr-data (plist-get id :pr))
                                          (merged (append
                                                   (when rollup (list (cons 'statusCheckRollup rollup)))
                                                   (when head-ref (list (cons 'headRefName head-ref)))
                                                   (when st (list (cons 'state st)))
                                                   (when (and merged-at (not (eq merged-at :null)))
                                                     (list (cons 'mergedAt merged-at)))
                                                   pr-data)))
                                     (magit-dash-gh-pr-dashboard--build-entry merged repo))
                                 entry)))
                           tabulated-list-entries))
            (tabulated-list-print t)))))))

(defun magit-dash-gh-pr-dashboard--enrich-ci (buf state dir)
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
                       "--json" "number,state,mergedAt,statusCheckRollup,headRefName" "--limit" "200")
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
           (list "pr" "list" "-R" repo "--state" gh-state
                 "--json" "number,state,mergedAt,statusCheckRollup,headRefName" "--limit" "200")
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

(defun magit-dash-gh-pr-dashboard--rebuild-entries ()
  "Rebuild `tabulated-list-entries' in place to reflect current marks."
  (when (derived-mode-p 'tabulated-list-mode)
    (setq tabulated-list-entries
          (seq-map (lambda (entry)
                     (let ((id (car entry)))
                       (magit-dash-gh-pr-dashboard--build-entry
                        (plist-get id :pr)
                        (plist-get id :repo))))
                   tabulated-list-entries))
    (tabulated-list-print t)))

(defun magit-dash-gh-pr-dashboard-toggle-mark ()
  "Toggle mark on the pull request at point and advance to the next line."
  (interactive)
  (let* ((entry (magit-dash-gh-pr-dashboard--entry-at-point))
         (key (magit-dash-gh-pr-dashboard--entry-key entry)))
    (setq magit-dash-gh-pr-dashboard--marked-entries
          (if (member key magit-dash-gh-pr-dashboard--marked-entries)
              (delete key magit-dash-gh-pr-dashboard--marked-entries)
            (cons key magit-dash-gh-pr-dashboard--marked-entries)))
    (magit-dash-gh-pr-dashboard--rebuild-entries)
    (forward-line 1)))

(defun magit-dash-gh-pr-dashboard-mark ()
  "Mark the pull request at point and advance to the next line."
  (interactive)
  (let* ((entry (magit-dash-gh-pr-dashboard--entry-at-point))
         (key (magit-dash-gh-pr-dashboard--entry-key entry)))
    (unless (member key magit-dash-gh-pr-dashboard--marked-entries)
      (push key magit-dash-gh-pr-dashboard--marked-entries))
    (magit-dash-gh-pr-dashboard--rebuild-entries)
    (forward-line 1)))

(defun magit-dash-gh-pr-dashboard-unmark ()
  "Unmark the pull request at point and advance to the next line."
  (interactive)
  (let* ((entry (magit-dash-gh-pr-dashboard--entry-at-point))
         (key (magit-dash-gh-pr-dashboard--entry-key entry)))
    (setq magit-dash-gh-pr-dashboard--marked-entries
          (delete key magit-dash-gh-pr-dashboard--marked-entries))
    (magit-dash-gh-pr-dashboard--rebuild-entries)
    (forward-line 1)))

(defun magit-dash-gh-pr-dashboard-unmark-all ()
  "Clear all marks in the pull request dashboard."
  (interactive)
  (setq magit-dash-gh-pr-dashboard--marked-entries nil)
  (magit-dash-gh-pr-dashboard--rebuild-entries)
  (message "magit-gh: cleared all marks"))

(defun magit-dash-gh-pr-dashboard-mark-all ()
  "Mark all pull requests currently listed in the dashboard."
  (interactive)
  (setq magit-dash-gh-pr-dashboard--marked-entries
        (seq-map (lambda (entry)
                   (magit-dash-gh-pr-dashboard--entry-key (car entry)))
                 tabulated-list-entries))
  (magit-dash-gh-pr-dashboard--rebuild-entries)
  (message "magit-gh: marked %d PR(s)" (length magit-dash-gh-pr-dashboard--marked-entries)))

(defun magit-dash-gh-pr-dashboard--marked-or-at-point-entries ()
  "Return list of marked PR entry plists, or nil if no entries are marked."
  (when (and (bound-and-true-p magit-dash-gh-pr-dashboard--marked-entries)
             tabulated-list-entries)
    (seq-filter #'magit-dash-gh-pr-dashboard--entry-marked-p
                (seq-map #'car tabulated-list-entries))))

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
Aggregates all open PRs by default across repositories, or for REPO
when provided."
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

(defun magit-dash-gh-pr--git-worktrees (repo-dir)
  "Return a list of alists ((path . PATH) (branch . BRANCH) (head . HEAD)).
Checked for REPO-DIR."
  (magit-dash-gh--git-worktrees repo-dir))

(defun magit-dash-gh-pr--compute-worktree-path (repo-dir name &optional entry)
  "Compute projected worktree path for NAME under REPO-DIR.
Uses `magit-dash-gh-pr-worktree-path-format'."
  (let* ((repo-dir (expand-file-name repo-dir))
         (repo-base (file-name-nondirectory (directory-file-name repo-dir)))
         (parent (file-name-directory (directory-file-name repo-dir)))
         (fmt magit-dash-gh-pr-worktree-path-format))
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

(defun magit-dash-gh-pr--find-existing-worktree (repo-dir pr-num &optional branch-name)
  "Find existing worktree directory for PR-NUM and BRANCH-NAME under REPO-DIR."
  (when repo-dir
    (let* ((repo-dir (expand-file-name repo-dir))
           (repo-base (file-name-nondirectory (directory-file-name repo-dir)))
           (pr-name (format "pr-%d" pr-num))
           (git-wts (ignore-errors (magit-dash-gh-pr--git-worktrees repo-dir))))
      ;; 1. Check git worktrees by branch or path
      (or
       (seq-some
        (lambda (wt)
          (let ((path (alist-get 'path wt))
                (br (alist-get 'branch wt)))
            (when (and path (file-directory-p path))
              (when (or (and br (or (string= br pr-name)
                                    (and branch-name (string= br branch-name))
                                    (and branch-name (string= br (concat "wt/" branch-name)))
                                    (string= br (concat "wt/" pr-name))))
                        (string-suffix-p (concat "/.wt/" pr-name) path)
                        (string-suffix-p (concat "/" repo-base "-" pr-name) path)
                        (and branch-name (string-suffix-p (concat "/.wt/" branch-name) path))
                        (and branch-name (string-suffix-p (concat "/" repo-base "-" branch-name) path)))
                path))))
        git-wts)
       ;; 2. Check candidate directories on disk
       (seq-some
        (lambda (dir)
          (and dir (file-directory-p dir) dir))
        (list
         (expand-file-name (concat ".wt/" pr-name) repo-dir)
         (expand-file-name (format "%s-%s" repo-base pr-name)
                           (file-name-directory (directory-file-name repo-dir)))
         (and branch-name (expand-file-name (concat ".wt/" branch-name) repo-dir))
         (and branch-name (expand-file-name (format "%s-%s" repo-base branch-name)
                                            (file-name-directory (directory-file-name repo-dir))))))))))

(defun magit-dash-gh-pr-dashboard--find-local-path (repo-slug)
  "Return a local checkout path for REPO-SLUG (owner/name), or nil.
Matches against `magit-dash-repo-list' (by repo slug, repo name, remote URL,
or checkout directory name), falling back to current repo if it matches."
  (when (and repo-slug (stringp repo-slug))
    (let* ((slug-clean (string-trim repo-slug))
           (repo-name (if (string-match ".*/\\([^/]+\\)$" slug-clean)
                          (match-string 1 slug-clean)
                        slug-clean))
           (repo-base (replace-regexp-in-string "\\.el\\'" "" repo-name)))
      (or
       ;; 1. Search magit-dash-repo-list
       (when (bound-and-true-p magit-dash-repo-list)
         (seq-some
          (lambda (r)
            (when (magit-dash-repo-p r)
              (let* ((rpath (magit-dash-repo-path r))
                     (rdir (and rpath (file-name-nondirectory (directory-file-name (expand-file-name rpath)))))
                     (rdir-base (and rdir (replace-regexp-in-string "\\.el\\'" "" rdir)))
                     (rname (magit-dash-repo-name r))
                     (rname-base (and rname (replace-regexp-in-string "<.*>" "" rname)))
                     (rrepo (magit-dash-repo-repo r))
                     (rpair (and rrepo (magit-dash-gh--owner-repo-from-url rrepo)))
                     (rslug (and rpair (format "%s/%s" (car rpair) (cdr rpair)))))
                (when (or (and rslug (string-equal-ignore-case rslug slug-clean))
                          (and rpair (string-equal-ignore-case (cdr rpair) repo-name))
                          (and rname (string-equal-ignore-case rname repo-name))
                          (and rname-base (string-equal-ignore-case rname-base repo-base))
                          (and rdir (string-equal-ignore-case rdir repo-name))
                          (and rdir-base (string-equal-ignore-case rdir-base repo-base)))
                  rpath))))
          magit-dash-repo-list))
       ;; 2. Fall back to current repo if it matches
       (when-let* ((top (or (magit-toplevel)
                            (ignore-errors (magit-dash-gh--repo-dir)))))
         (let* ((top-dir (file-name-nondirectory (directory-file-name (expand-file-name top))))
                (top-base (replace-regexp-in-string "\\.el\\'" "" top-dir))
                (top-slug (ignore-errors (magit-dash-gh-pr-dashboard--current-repo-slug))))
           (when (or (and top-slug (string-equal-ignore-case top-slug slug-clean))
                     (string-equal-ignore-case top-dir repo-name)
                     (string-equal-ignore-case top-base repo-base))
             top)))))))

(defun magit-dash-gh-pr-dashboard--worktree-path (entry)
  "Return existing or projected worktree path for PR ENTRY."
  (let* ((pr-num (plist-get entry :number))
         (repo (plist-get entry :repo))
         (pr-data (plist-get entry :pr))
         (branch-name (or (and pr-data (map-elt pr-data 'headRefName))
                          (plist-get entry :head-ref)))
         (local-path (magit-dash-gh-pr-dashboard--find-local-path repo)))
    (when local-path
      (let* ((repo-dir (expand-file-name local-path))
             (existing (magit-dash-gh-pr--find-existing-worktree repo-dir pr-num branch-name)))
        (or existing
            (magit-dash-gh-pr--compute-worktree-path repo-dir (format "pr-%d" pr-num) entry))))))

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

(defun magit-dash-gh-pr--resolve-remote-branch (repo pr-num &optional pr-data dir)
  "Resolve remote head branch name for PR-NUM in REPO.
Checks PR-DATA first, then queries GitHub via `gh pr view', falling back
to \"pr-PR-NUM\" on failure."
  (or (and pr-data (map-elt pr-data 'headRefName))
      (when (executable-find "gh")
        (let ((default-directory (or dir default-directory)))
          (ignore-errors
            (with-temp-buffer
              (when (zerop (call-process "gh" nil (current-buffer) nil
                                         "pr" "view" (number-to-string pr-num)
                                         "-R" repo
                                         "--json" "headRefName"
                                         "--jq" ".headRefName"))
                (let ((name (string-trim (buffer-string))))
                  (unless (string-empty-p name)
                    name)))))))
      (format "pr-%d" pr-num)))

(defun magit-dash-gh-pr--pr-merged-p (pr &optional repo pr-num dir)
  "Return non-nil if PR alist (or PR-NUM in REPO) is merged.
Handles `:null' from `json-parse-string', case-insensitive state strings,
and queries `gh pr view' when state is ambiguous (e.g. `closed' from search)."
  (let* ((state-raw (and pr (map-elt pr 'state)))
         (state (if (and state-raw (stringp state-raw)) (upcase state-raw) ""))
         (merged-at (and pr (map-elt pr 'mergedAt)))
         (has-merged-at (and merged-at
                             (not (eq merged-at :null))
                             (stringp merged-at)
                             (not (string-empty-p merged-at)))))
    (cond
     ((or (string= state "MERGED") has-merged-at) t)
     ((string= state "OPEN") nil)
     ;; When state is CLOSED (or unknown) without mergedAt, check via gh pr view if possible
     ((and (or (plist-get pr :number) (map-elt pr 'number) pr-num)
           repo
           (executable-find "gh"))
      (let ((num (or pr-num (map-elt pr 'number)))
            (default-directory (or dir default-directory)))
        (ignore-errors
          (with-temp-buffer
            (when (zerop (call-process "gh" nil (current-buffer) nil
                                       "pr" "view" (number-to-string num)
                                       "-R" repo
                                       "--json" "state,mergedAt"))
              (let ((data (json-parse-string (buffer-string) :object-type 'alist)))
                (magit-dash-gh-pr--pr-merged-p data)))))))
     (t nil))))

(defun magit-dash-gh-pr--fetch-repo-prs (repo &optional state dir)
  "Fetch pull requests for REPO from GitHub CLI.
STATE may be `closed', `open', or `all'.
DIR is optional working directory for `gh'.
Returns a list of PR alists or nil on error."
  (when (executable-find "gh")
    (let ((default-directory (or dir default-directory)))
      (with-temp-buffer
        (when (zerop (call-process "gh" nil (current-buffer) nil
                                   "pr" "list" "-R" repo
                                   "--state" (or state "open")
                                   "--limit" "100"
                                   "--json" "number,title,headRefName,state,mergedAt"))
          (ignore-errors
            (json-parse-string (buffer-string) :object-type 'alist :array-type 'list)))))))

(defun magit-dash-gh-pr--match-worktree-to-pr (wt prs)
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
                  (pr-tag (when num (format "pr-%d" num))))
             (when (or (and head (or (equal wt-branch head)
                                     (equal wt-branch (concat "wt/" head))
                                     (equal wt-name head)
                                     (string-suffix-p (concat "-" head) wt-name)))
                       (and pr-tag (or (equal wt-branch pr-tag)
                                       (equal wt-branch (concat "wt/" pr-tag))))
                       (and pr-tag (string-suffix-p pr-tag wt-name))
                       (and num (string-match-p (format "\\(?:^\\|[^0-9]\\)%d\\(?:$\\|[^0-9]\\)" num) wt-name)))
               pr)))
         prs)))))

(defun magit-dash-gh-pr--path-equal-p (path-a path-b)
  "Return non-nil if PATH-A and PATH-B refer to the same location."
  (when (and path-a path-b)
    (or (string= (directory-file-name (expand-file-name path-a))
                 (directory-file-name (expand-file-name path-b)))
        (ignore-errors (file-equal-p path-a path-b)))))

(defun magit-dash-gh-pr--move-worktree (repo-dir old-path new-path)
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
            (when (fboundp 'magit-dash-worktree-symlink-worktree)
              (magit-dash-worktree-symlink-worktree new-path repo-dir))
            new-path)
        (message "magit-gh: failed to move worktree %s -> %s (exit %d)"
                 old-path new-path exit-code)
        nil))))

;;;###autoload
(defun magit-dash-gh-pr-delete-worktree (wt-path &optional repo-dir)
  "Safely remove the worktree at WT-PATH for REPO-DIR.
Refuses to delete if WT-PATH contains uncommitted changes.
Preserves the underlying branch until branch pruning."
  (require 'magit-dash-worktree nil t)
  (let* ((wt-path (expand-file-name wt-path))
         (repo-dir (expand-file-name
                    (or repo-dir
                        (if (fboundp 'magit-dash-worktree-main-dir)
                            (magit-dash-worktree-main-dir wt-path)
                          default-directory)))))
    (when (and (fboundp 'magit-dash-worktree-uncommitted-changes-p)
               (magit-dash-worktree-uncommitted-changes-p wt-path))
      (user-error "Cannot delete worktree %s: contains uncommitted changes" wt-path))
    (let ((exit-code (call-process "git" nil nil nil
                                   "-C" repo-dir
                                   "worktree" "remove" wt-path)))
      (if (zerop exit-code)
          (progn
            (call-process "git" nil nil nil "-C" repo-dir "worktree" "prune")
            (message "magit-gh: deleted worktree at %s (branch preserved)" wt-path)
            t)
        (user-error "Failed to remove worktree %s (git exit %d)" wt-path exit-code)))))

(defun magit-dash-gh-pr--create-worktree-for-entry (entry &optional switch)
  "Create a git worktree for single PR ENTRY plist.
When SWITCH is non-nil, switches to the worktree's magit status buffer."
  (let* ((pr-num (plist-get entry :number))
         (repo (plist-get entry :repo))
         (pr-data (plist-get entry :pr))
         (local-path (or (magit-dash-gh-pr-dashboard--find-local-path repo)
                         (let ((prompt-path (read-directory-name (format "Local repository path for %s: " repo))))
                           (if (and prompt-path (file-directory-p prompt-path))
                               (expand-file-name prompt-path)
                             (user-error "No registered local checkout for %s" repo)))))
         (repo-dir (expand-file-name local-path))
         (repo-base (file-name-nondirectory (directory-file-name repo-dir)))
         (branch-name (magit-dash-gh-pr--resolve-remote-branch repo pr-num pr-data repo-dir))
         (existing (magit-dash-gh-pr--find-existing-worktree repo-dir pr-num branch-name))
         (target-path (magit-dash-gh-pr--compute-worktree-path repo-dir (format "pr-%d" pr-num) entry))
         (wt-dir (or existing target-path)))
    ;; If existing worktree found at a non-matching path, offer to rename it
    (when (and existing (not (magit-dash-gh-pr--path-equal-p existing target-path)))
      (if (y-or-n-p (format "Worktree '%s' does not match PR #%d format ('%s'). Rename it? "
                            (file-name-nondirectory (directory-file-name existing))
                            pr-num
                            (file-name-nondirectory (directory-file-name target-path))))
          (let ((moved (magit-dash-gh-pr--move-worktree repo-dir existing target-path)))
            (when moved (setq wt-dir target-path)))
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
        (let ((refspec (format "pull/%d/head:%s" pr-num branch-name)))
          (unless (= 0 (magit-call-git "fetch" "origin" refspec))
            (magit-call-git "fetch" "origin" (concat "+" refspec))))
        (let ((parent (file-name-directory (directory-file-name wt-dir))))
          (when parent
            (ignore-errors (make-directory parent t))))
        (let ((exit-code (magit-call-git "worktree" "add" wt-dir branch-name)))
          (if (= exit-code 0)
              (progn
                (when (fboundp 'magit-dash-worktree-symlink-worktree)
                  (magit-dash-worktree-symlink-worktree wt-dir repo-dir))
                (message "magit-gh: %s worktree for PR #%d at %s"
                         (if switch "opened" "created") pr-num wt-dir)
                (when (derived-mode-p 'magit-dash-gh-pr-dashboard-mode)
                  (magit-dash-gh-pr-dashboard--rebuild-entries))
                (if switch
                    (magit-status-setup-buffer wt-dir)
                  wt-dir))
            (user-error "Failed to create worktree at %s (exit %d)" wt-dir exit-code)))))))

;;;###autoload
(defun magit-dash-gh-pr-dashboard-create-worktree (&optional entry switch)
  "Create a git worktree for the pull request at point (or ENTRY), or marked PRs.
If PRs are marked in the dashboard and ENTRY is nil, creates worktrees for all
marked PRs and clears marks.
When SWITCH is non-nil, switches to the worktree's magit status buffer."
  (interactive)
  (let ((marked (and (null entry)
                     (derived-mode-p 'magit-dash-gh-pr-dashboard-mode)
                     (magit-dash-gh-pr-dashboard--marked-or-at-point-entries))))
    (if marked
        (let ((created 0)
              (failed 0))
          (dolist (m-entry marked)
            (condition-case err
                (progn
                  (magit-dash-gh-pr--create-worktree-for-entry m-entry nil)
                  (setq created (1+ created)))
              (error
               (message "magit-gh: failed to create worktree for PR #%d: %s"
                        (plist-get m-entry :number) (error-message-string err))
               (setq failed (1+ failed)))))
          (setq magit-dash-gh-pr-dashboard--marked-entries nil)
          (magit-dash-gh-pr-dashboard--rebuild-entries)
          (message "magit-gh: created %d worktree(s) for marked PRs (%d failed)"
                   created failed))
      (magit-dash-gh-pr--create-worktree-for-entry
       (or entry (magit-dash-gh-pr-dashboard--entry-at-point))
       switch))))

;;;###autoload
(defun magit-dash-gh-pr-dashboard-open-worktree (&optional entry)
  "Create and open a git worktree for the pull request at point (or ENTRY).
If a worktree for this PR already exists, switches to its magit status buffer.
Otherwise, fetches the remote PR branch into a local branch mirroring its remote
name, creates the worktree directory, and opens its magit status buffer."
  (interactive)
  (magit-dash-gh-pr--create-worktree-for-entry
   (or entry (magit-dash-gh-pr-dashboard--entry-at-point))
   t))

;;;###autoload
(defalias 'magit-dash-gh-pr-create-worktree #'magit-dash-gh-pr-dashboard-create-worktree)

;;;###autoload
(defun magit-dash-gh-pr-review-worktrees (&optional repo-dir prompt-p prs merged-only)
  "Review existing worktrees in REPO-DIR against GitHub pull requests.
Prompts to rename worktrees that do not match the configured PR naming format,
and prompts to delete worktrees for merged PRs (refusing if
uncommitted changes exist).
Leaves branches intact until branch pruning.
PROMPT-P specifies whether to prompt for actions (defaults to interactive).
PRS can be an existing list of PR alists to avoid refetching.
When MERGED-ONLY is non-nil, only checks and cleans worktrees for merged PRs.
Returns a plist with :renamed, :deleted, :matching, and :skipped counts."
  (interactive (list nil t nil nil))
  (require 'magit-dash-worktree nil t)
  (let* ((prompt-p (if (called-interactively-p 'interactive) t prompt-p))
         (repo-dir (expand-file-name
                    (or repo-dir
                        (if (fboundp 'magit-dash-worktree-main-dir)
                            (magit-dash-worktree-main-dir)
                          (magit-toplevel))
                        default-directory)))
         (slug (or (ignore-errors
                     (let ((default-directory repo-dir))
                       (magit-dash-gh-pr-dashboard--current-repo-slug)))
                   (file-name-nondirectory (directory-file-name repo-dir))))
         (prs (or prs (magit-dash-gh-pr--fetch-repo-prs slug "all" repo-dir)))
         (git-wts (magit-dash-gh-pr--git-worktrees repo-dir))
         (counts (list :renamed 0 :deleted 0 :matching 0 :skipped 0)))
    (dolist (wt git-wts)
      (let* ((wt-path (alist-get 'path wt)))
        (unless (or (null wt-path)
                    (magit-dash-gh-pr--path-equal-p wt-path repo-dir)
                    (not (file-directory-p wt-path)))
          (let* ((wt-name (file-name-nondirectory (directory-file-name wt-path)))
                 (matched (or (magit-dash-gh-pr--match-worktree-to-pr
                               wt (seq-filter (lambda (pr)
                                                (let ((state (map-elt pr 'state)))
                                                  (and (stringp state)
                                                       (string-equal (upcase state) "OPEN"))))
                                              prs))
                              (magit-dash-gh-pr--match-worktree-to-pr wt prs))))
            (when matched
              (let* ((pr-num (map-elt matched 'number))
                     (state-raw (map-elt matched 'state))
                     (state (if (stringp state-raw) (upcase state-raw) ""))
                     (merged-p (magit-dash-gh-pr--pr-merged-p matched slug pr-num repo-dir)))
                (cond
                 ;; Case 1: PR has merged
                 (merged-p
                  (let ((pr-label (if pr-num (format "PR #%d" pr-num) "PR")))
                    (if (and (fboundp 'magit-dash-worktree-uncommitted-changes-p)
                             (magit-dash-worktree-uncommitted-changes-p wt-path))
                        (progn
                          (message "magit-gh: worktree %s (%s merged) has uncommitted changes; skipping"
                                   wt-name pr-label)
                          (plist-put counts :skipped (1+ (plist-get counts :skipped))))
                      (if (or (not prompt-p)
                              (y-or-n-p (format "%s is merged and worktree '%s' has no uncommitted changes. Delete worktree? (branch preserved) "
                                                pr-label wt-name)))
                          (progn
                            (magit-dash-gh-pr-delete-worktree wt-path repo-dir)
                            (plist-put counts :deleted (1+ (plist-get counts :deleted))))
                        (plist-put counts :skipped (1+ (plist-get counts :skipped)))))))
                 ;; Case 2: PR is open - check path match (skipped when merged-only)
                 ((and (not merged-only) (string= state "OPEN") pr-num)
                  (let ((target-path (magit-dash-gh-pr--compute-worktree-path
                                      repo-dir (format "pr-%d" pr-num))))
                    (if (magit-dash-gh-pr--path-equal-p wt-path target-path)
                        (plist-put counts :matching (1+ (plist-get counts :matching)))
                      (if (or (not prompt-p)
                              (y-or-n-p (format "Worktree '%s' does not match PR #%d format ('%s'). Rename it to match? "
                                                wt-name pr-num
                                                (file-name-nondirectory (directory-file-name target-path)))))
                          (if (magit-dash-gh-pr--move-worktree repo-dir wt-path target-path)
                              (plist-put counts :renamed (1+ (plist-get counts :renamed)))
                            (plist-put counts :skipped (1+ (plist-get counts :skipped))))
                        (plist-put counts :skipped (1+ (plist-get counts :skipped))))))))))))))
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
(defun magit-dash-gh-pr-clean-merged-worktrees (&optional repo-dir prompt-p)
  "Delete worktrees for merged PRs in REPO-DIR when clean.
Refuses to delete any worktree with uncommitted changes.
Leaves local branches intact until branch pruning.
PROMPT-P specifies whether to prompt before deleting each clean worktree."
  (interactive (list nil t))
  (let ((counts (magit-dash-gh-pr-review-worktrees repo-dir prompt-p nil t)))
    (list :deleted (plist-get counts :deleted)
          :skipped (plist-get counts :skipped))))

(defun magit-dash-gh-pr--clean-worktree-for-entry (entry &optional prompt-p)
  "Clean worktree for single PR ENTRY plist if it exists and is clean.
Returns `:deleted', `:skipped', or `:none'."
  (require 'magit-dash-worktree nil t)
  (let* ((pr-num (plist-get entry :number))
         (repo (plist-get entry :repo))
         (pr-data (plist-get entry :pr))
         (local-path (magit-dash-gh-pr-dashboard--find-local-path repo))
         (repo-dir (and local-path (expand-file-name local-path)))
         (branch-name (or (and pr-data (map-elt pr-data 'headRefName))
                          (plist-get entry :head-ref)))
         (wt-path (and repo-dir
                       (magit-dash-gh-pr--find-existing-worktree repo-dir pr-num branch-name))))
    (if (not (and wt-path (file-directory-p wt-path)))
        :none
      (let* ((wt-name (file-name-nondirectory (directory-file-name wt-path)))
             (merged-p (magit-dash-gh-pr--pr-merged-p pr-data repo pr-num repo-dir)))
        (cond
         ((not merged-p) :skipped)
         ((and (fboundp 'magit-dash-worktree-uncommitted-changes-p)
               (magit-dash-worktree-uncommitted-changes-p wt-path))
          (message "magit-gh: worktree %s (PR #%d) has uncommitted changes; skipping"
                   wt-name pr-num)
          :skipped)
         ((or (not prompt-p)
              (y-or-n-p (format "PR #%d is merged and worktree '%s' has no uncommitted changes. Delete worktree? (branch preserved) "
                                pr-num wt-name)))
          (magit-dash-gh-pr-delete-worktree wt-path repo-dir)
          :deleted)
         (t :skipped))))))

;;;###autoload
(defun magit-dash-gh-pr-create-all-worktrees (&optional repo-dir)
  "Create worktrees for all open PRs in REPO-DIR.
First reviews existing worktrees, prompting to rename any that do not match
nthe canonical PR format, and prompting to delete any for merged PRs
that are clean.
Then creates worktrees for each open PR that does not yet have one.
Establishes configured symlinks for all created worktrees."
  (interactive)
  (let* ((repo-dir (expand-file-name
                    (or repo-dir
                        (if (fboundp 'magit-dash-worktree-main-dir)
                            (magit-dash-worktree-main-dir)
                          (magit-toplevel))
                        default-directory)))
         (slug (or (ignore-errors
                     (let ((default-directory repo-dir))
                       (magit-dash-gh-pr-dashboard--current-repo-slug)))
                   (file-name-nondirectory (directory-file-name repo-dir))))
         (all-prs (magit-dash-gh-pr--fetch-repo-prs slug "all" repo-dir))
         (open-prs (seq-filter
                    (lambda (pr)
                      (string= (upcase (or (map-elt pr 'state) "")) "OPEN"))
                    all-prs)))
    ;; 1. Review existing worktrees first (prompting to rename mismatches / clean merged)
    (magit-dash-gh-pr-review-worktrees repo-dir t all-prs)
    ;; 2. Create worktree for every open PR without one
    (let ((created 0)
          (existing 0)
          (failed 0))
      (dolist (pr open-prs)
        (let* ((pr-num (map-elt pr 'number))
               (head-ref (map-elt pr 'headRefName))
               (wt-existing (magit-dash-gh-pr--find-existing-worktree repo-dir pr-num head-ref)))
          (if wt-existing
              (setq existing (1+ existing))
            (condition-case err
                (let ((entry (list :number pr-num :repo slug :pr pr)))
                  (magit-dash-gh-pr-dashboard-create-worktree entry nil)
                  (setq created (1+ created)))
              (error
               (message "magit-gh: failed to create worktree for PR #%d: %s"
                        pr-num (error-message-string err))
               (setq failed (1+ failed)))))))
      (message "magit-gh: created %d worktree(s) for %s (%d existing, %d failed)"
               created slug existing failed)
      (list :created created :existing existing :failed failed))))

(defun magit-dash-gh-pr-dashboard--repo-path-at-point ()
  "Return registered local repository path for PR at point.
Signals `user-error' when not found."
  (let* ((entry (magit-dash-gh-pr-dashboard--entry-at-point))
         (repo (plist-get entry :repo)))
    (or (magit-dash-gh-pr-dashboard--find-local-path repo)
        (user-error "No registered local checkout for %s" repo))))

(defun magit-dash-gh-pr-dashboard-review-worktrees ()
  "Review worktrees for the pull request / repository at point in dashboard."
  (interactive)
  (magit-dash-gh-pr-review-worktrees (magit-dash-gh-pr-dashboard--repo-path-at-point) t)
  (magit-dash-gh-pr-dashboard-refresh))

(defun magit-dash-gh-pr-dashboard-clean-merged-worktrees ()
  "Clean worktrees for marked PRs, the selected PR at point, or merged PRs in repo."
  (interactive)
  (let ((marked (and (derived-mode-p 'magit-dash-gh-pr-dashboard-mode)
                     (magit-dash-gh-pr-dashboard--marked-or-at-point-entries))))
    (cond
     ;; 1. Marked PRs exist: clean worktrees for all marked PRs
     (marked
      (let ((deleted 0)
            (skipped 0))
        (dolist (m-entry marked)
          (pcase (magit-dash-gh-pr--clean-worktree-for-entry m-entry t)
            (:deleted (setq deleted (1+ deleted)))
            (:skipped (setq skipped (1+ skipped)))))
        (setq magit-dash-gh-pr-dashboard--marked-entries nil)
        (magit-dash-gh-pr-dashboard--rebuild-entries)
        (message "magit-gh: cleaned marked PR worktrees — %d deleted, %d skipped"
                 deleted skipped)))
     ;; 2. No marked PRs: check selected PR at point and also scan repo merged worktrees
     (t
      (let* ((entry (ignore-errors (magit-dash-gh-pr-dashboard--entry-at-point)))
             (repo-path (magit-dash-gh-pr-dashboard--repo-path-at-point))
             (pr-data (and entry (plist-get entry :pr))))
        (if (and entry
                 pr-data
                 (magit-dash-gh-pr-dashboard--worktree-exists-p entry)
                 (magit-dash-gh-pr--pr-merged-p pr-data (plist-get entry :repo) (plist-get entry :number) repo-path))
            (magit-dash-gh-pr--clean-worktree-for-entry entry t)
          (magit-dash-gh-pr-clean-merged-worktrees repo-path t))
        (magit-dash-gh-pr-dashboard-refresh))))))

(defun magit-dash-gh-pr-dashboard-create-all-worktrees ()
  "Create worktrees for all open PRs in the current dashboard view.
If filtered to a single repository, creates worktrees for that repository.
Otherwise prompts or processes open PRs in the dashboard."
  (interactive)
  (let* ((entries (or tabulated-list-entries nil))
         (repo-filter (plist-get magit-dash-gh-pr-dashboard--filters :repo)))
    (if repo-filter
        (let ((path (magit-dash-gh-pr-dashboard--find-local-path repo-filter)))
          (unless path
            (user-error "No registered local checkout for %s" repo-filter))
          (magit-dash-gh-pr-create-all-worktrees path)
          (magit-dash-gh-pr-dashboard-refresh))
      (let ((created 0)
            (existing 0)
            (failed 0))
        (dolist (item entries)
          (let* ((entry (car item))
                 (pr-num (plist-get entry :number))
                 (repo (plist-get entry :repo))
                 (pr-data (plist-get entry :pr))
                 (local-path (magit-dash-gh-pr-dashboard--find-local-path repo)))
            (when (and local-path (file-directory-p local-path))
              (let* ((repo-dir (expand-file-name local-path))
                     (branch-name (or (and pr-data (map-elt pr-data 'headRefName))
                                      (plist-get entry :head-ref)))
                     (wt-exists (magit-dash-gh-pr--find-existing-worktree repo-dir pr-num branch-name)))
                (if wt-exists
                    (setq existing (1+ existing))
                  (condition-case err
                      (progn
                        (magit-dash-gh-pr-dashboard-create-worktree entry nil)
                        (setq created (1+ created)))
                    (error
                     (message "magit-gh: failed to create worktree for PR #%d: %s"
                              pr-num (error-message-string err))
                     (setq failed (1+ failed)))))))))
        (message "magit-gh: batch creation complete — %d created, %d existing, %d failed"
                 created existing failed)
        (magit-dash-gh-pr-dashboard-refresh)))))

(defun magit-dash-gh-pr-dashboard-fetch-ci ()
  "Fetch GitHub Actions CI logs for the pull request at point."
  (interactive)
  (require 'magit-dash-gh-actions)
  (let ((entry (magit-dash-gh-pr-dashboard--entry-at-point))
        (path (magit-dash-gh-pr-dashboard--repo-path-at-point)))
    (magit-dash-gh-actions-fetch-for-pr (plist-get entry :number) path)))

(transient-define-prefix magit-dash-gh-pr-dashboard-menu ()
  "Actions for the PR dashboard."
  [["Pull Request"
    ("b"   "Open in browser"          magit-dash-gh-pr-dashboard-open-browser)]
   ["Mark / Bulk"
    ("m"   "Toggle mark"              magit-dash-gh-pr-dashboard-toggle-mark)
    ("u"   "Unmark"                   magit-dash-gh-pr-dashboard-unmark)
    ("ua"  "Unmark all"               magit-dash-gh-pr-dashboard-unmark-all)
    ("ma"  "Mark all"                 magit-dash-gh-pr-dashboard-mark-all)]
   ["Worktree"
    ("cw"  "Create worktree(s)"       magit-dash-gh-pr-dashboard-create-worktree)
    ("ca"  "Create all worktrees"     magit-dash-gh-pr-dashboard-create-all-worktrees)
    ("r"   "Review / rename worktrees" magit-dash-gh-pr-dashboard-review-worktrees)
    ("k"   "Clean merged worktrees"   magit-dash-gh-pr-dashboard-clean-merged-worktrees)
    ("w"   "Open Magit in worktree"   magit-dash-gh-pr-dashboard-open-worktree)
    ("d"   "Open Dired in worktree"   magit-dash-gh-pr-dashboard-open-dired)]
   ["CI"
    ("ci"  "Fetch CI logs"            magit-dash-gh-pr-dashboard-fetch-ci)]
   ["View & Filter"
    ("v"   "Switch view…"             magit-dash-gh-pr-dashboard-switch-view)
    ("tc"  "Toggle column…"           magit-dash-gh-pr-toggle-column)
    ("to"  "Toggle Org column"        magit-dash-gh-pr-toggle-org)
    ("fs"  "Set filter…"              magit-dash-gh-pr-dashboard-set-filter)
    ("fc"  "Clear all filters"        magit-dash-gh-pr-dashboard-clear-filters)]
   ["Dashboard"
    ("g"   "Refresh"                  magit-dash-gh-pr-dashboard-refresh)
    ("q"   "Quit"                     magit-dash-gh-pr-dashboard-quit)]])

(transient-define-prefix magit-dash-gh-pr-views-menu ()
  "Jump to a pull request dashboard view."
  [["Pull Requests"
    ("a" "All repos — open PRs"  magit-dash-gh-pr-dash-all)
    ("m" "All repos — my PRs"    magit-dash-gh-pr-dash-mine)
    ("h" "This repo — open PRs"  magit-dash-gh-pr-dash-here)
    ("n" "This repo — my PRs"    magit-dash-gh-pr-dash-mine-here)]])

(provide 'magit-dash-gh-pr)

;;; magit-gh-pr.el ends here
