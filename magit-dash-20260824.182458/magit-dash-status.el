;;; magit-dash-status.el --- GitHub CI and PR sections for magit-status -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.0") (transient "0.4"))
;; Keywords: vc, tools, magit, github, ci

;; This file is not part of GNU Emacs

;;; Commentary:

;; Provides GitHub Actions CI status and Pull Request information directly
;; within `magit-status' buffers:
;;
;;   1. Header lines:
;;      - CI: current branch workflow run status (pass/fail/running) with check icons.
;;      - PR: current branch pull request info (number, title, review status).
;;
;;   2. Section:
;;      - Authored Pull Requests (collapsible list of open PRs with CI checks).
;;
;;   3. Interactive Transient menus on RET for both CI and PR items.
;;
;;   4. Quick dispatcher menu `magit-dash-dispatch' bound to "." in Magit buffers.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magit)

(require 'magit-dash-gh)
(require 'magit-dash-gh-ci)
(require 'magit-dash-gh-pr)
(require 'magit-dash-gh-actions)

(declare-function magit-dash-open "magit-dash")
(declare-function magit-dash-open-repo "magit-dash-open")
(declare-function magit-dash-repo-path "magit-dash")
(declare-function magit-dash-gh-pr-dashboard-open-worktree "magit-dash-gh-pr")
(declare-function magit-dash-ci-dispatch-fix-operation "magit-dash-gh-ci")
(declare-function magit-dash-gh-actions-fetch "magit-dash-gh-actions")
(declare-function magit-dash-gh-actions-fetch-for-pr "magit-dash-gh-actions")
(declare-function magit-gh-pr-dash "magit-dash-gh-pr")
(declare-function magit-gh-actions "magit-gh")
(declare-function magit-gh-pr-checks "magit-gh")
(declare-function magit-gh-pr-diff "magit-gh")
(declare-function magit-dash-gh-prune-merged-branches "magit-dash-gh")
(declare-function magit-dash-gh-auth-switch "magit-dash-gh")
(declare-function magit-dash-gh-pr-fetch "magit-dash-gh-pr")
(declare-function magit-dash-gh-workflow-run "magit-dash-gh-actions")

(defgroup magit-dash-status nil
  "GitHub status and PR integration in `magit-status' buffers."
  :prefix "magit-dash-status-"
  :group 'magit-dash)

(defcustom magit-dash-status-enable t
  "When non-nil, insert GitHub CI and PR sections into `magit-status'."
  :type 'boolean
  :group 'magit-dash-status)

(defcustom magit-dash-status-authored-prs-limit 15
  "Maximum number of authored PRs to display in the status section."
  :type 'integer
  :group 'magit-dash-status)

(defcustom magit-dash-status-cache-ttl 60
  "Time-to-live in seconds for status cache before auto-refreshing in background."
  :type 'integer
  :group 'magit-dash-status)

;;; Faces

(defface magit-dash-status-pr-number-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for PR numbers in `magit-status'."
  :group 'magit-dash-status)

(defface magit-dash-status-pr-title-face
  '((t :inherit default))
  "Face for PR titles in `magit-status'."
  :group 'magit-dash-status)

(defface magit-dash-status-approved-face
  '((t :inherit success :weight bold))
  "Face for approved PR review decision in `magit-status'."
  :group 'magit-dash-status)

(defface magit-dash-status-changes-requested-face
  '((t :inherit error :weight bold))
  "Face for changes requested review decision in `magit-status'."
  :group 'magit-dash-status)

;;; Repository & Remote Detection

(defun magit-dash-status--repo-root (&optional dir)
  "Return repository root directory for DIR (defaults to `default-directory')."
  (let ((default-directory (or dir default-directory)))
    (magit-toplevel)))

(defun magit-dash-status--github-repo-p (&optional dir)
  "Return non-nil if repository at DIR is hosted on GitHub."
  (when-let* ((top (magit-dash-status--repo-root dir)))
    (let ((cached (magit-dash-gh--cache-get top :is-github-repo)))
      (if (not (null cached))
          (eq cached t)
        (let ((is-gh
               (or (and (boundp 'magit-dash-repo-list)
                        (seq-some (lambda (r)
                                    (file-equal-p (magit-dash-repo-path r) top))
                                  magit-dash-repo-list))
                   (let ((default-directory top))
                     (when-let* ((urls (delq nil
                                             (list (magit-get "remote" "origin" "url")
                                                   (magit-get "remote" "pushdefault" "url")
                                                   (magit-get "remote" "upstream" "url")))))
                       (seq-some (lambda (u) (string-match-p "github\\.com" u)) urls))))))
          (magit-dash-gh--cache-set top :is-github-repo (if is-gh t :not-github))
          (and is-gh t))))))

;;; Data Fetching, Debounced Refresh & Async Invalidation

(defvar magit-dash-status--refresh-timers (make-hash-table :test #'eq)
  "Active debounced refresh timers keyed by magit-status buffer.")

(defun magit-dash-status--schedule-refresh (buf)
  "Schedule a debounced refresh for magit-status buffer BUF."
  (when (buffer-live-p buf)
    (when-let* ((existing (gethash buf magit-dash-status--refresh-timers)))
      (cancel-timer existing)
      (remhash buf magit-dash-status--refresh-timers))
    (let ((timer nil))
      (setq timer
            (run-with-timer
             0.05 nil
             (lambda ()
               (remhash buf magit-dash-status--refresh-timers)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (derived-mode-p 'magit-status-mode)
                     (magit-refresh-buffer buf)))))))
      (puthash buf timer magit-dash-status--refresh-timers))))

(defun magit-dash-status--in-flight-p (path key)
  "Return non-nil if a background fetch for KEY on PATH is in flight."
  (magit-dash-gh--cache-get path (intern (format "%s-in-flight" key))))

(defun magit-dash-status--set-in-flight (path key status)
  "Set the in-flight flag for KEY on PATH to STATUS."
  (magit-dash-gh--cache-set path (intern (format "%s-in-flight" key)) status))

(defun magit-dash-status--cache-stale-p (path key)
  "Return non-nil if cache entry KEY for PATH is missing or expired."
  (let ((time (magit-dash-gh--cache-get path (intern (format "%s-time" key)))))
    (or (null time)
        (> (- (float-time) time) magit-dash-status-cache-ttl))))

(defun magit-dash-status--parse-ci-run (run-alist)
  "Parse a single GitHub Actions RUN-ALIST into a normalised plist."
  (when run-alist
    (list :database-id   (map-elt run-alist 'databaseId)
          :name          (map-elt run-alist 'name)
          :workflow-name (map-elt run-alist 'workflowName)
          :status        (map-elt run-alist 'status)
          :conclusion    (map-elt run-alist 'conclusion)
          :created-at    (map-elt run-alist 'createdAt)
          :updated-at    (map-elt run-alist 'updatedAt)
          :url           (map-elt run-alist 'url)
          :head-sha      (map-elt run-alist 'headSha)
          :event         (map-elt run-alist 'event))))

(defun magit-dash-status--parse-pr (pr-alist)
  "Parse a GitHub PR-ALIST into a normalised plist."
  (when pr-alist
    (list :number        (map-elt pr-alist 'number)
          :title         (map-elt pr-alist 'title)
          :state         (map-elt pr-alist 'state)
          :review-decision (map-elt pr-alist 'reviewDecision)
          :url           (map-elt pr-alist 'url)
          :mergeable     (map-elt pr-alist 'mergeable)
          :head-ref      (map-elt pr-alist 'headRefName)
          :base-ref      (map-elt pr-alist 'baseRefName)
          :updated-at    (map-elt pr-alist 'updatedAt)
          :status-rollup (map-elt pr-alist 'statusCheckRollup))))

(defun magit-dash-status--fetch-ci (dir callback &optional force)
  "Fetch latest CI status for current branch in DIR asynchronously.
Calls CALLBACK with the resulting CI plist. When FORCE is nil and
cache is fresh, invokes CALLBACK immediately with cached data."
  (let* ((top (magit-dash-status--repo-root dir))
         (branch (when (and top (file-directory-p top))
                   (let ((default-directory top))
                     (condition-case nil
                         (magit-get-current-branch)
                       (error nil))))))
    (cond
     ((or (null top) (null branch) (string-empty-p branch))
      (when top
        (magit-dash-gh--cache-set top :status-ci :empty)
        (magit-dash-gh--cache-set top :status-ci-time (float-time)))
      (when callback (funcall callback nil)))
     ((and (not force) (not (magit-dash-status--cache-stale-p top :status-ci)))
      (when callback (funcall callback (magit-dash-gh--cache-get top :status-ci))))
     ((magit-dash-status--in-flight-p top :status-ci)
      nil)
     (t
      (magit-dash-status--set-in-flight top :status-ci t)
      (magit-dash-gh--run-process
       (list "run" "list"
             "--branch" branch
             "--limit" "1"
             "--json" "databaseId,name,status,conclusion,createdAt,updatedAt,url,workflowName,headSha,event")
       top
       (lambda (output)
         (magit-dash-status--set-in-flight top :status-ci nil)
         (let* ((runs (condition-case nil
                          (json-parse-string output :array-type 'list :object-type 'alist)
                        (error nil)))
                (parsed (and runs (magit-dash-status--parse-ci-run (car runs)))))
           (magit-dash-gh--cache-set top :status-ci (or parsed :empty))
           (magit-dash-gh--cache-set top :status-ci-time (float-time))
           (when callback (funcall callback parsed))))
       (lambda (_err _code)
         (magit-dash-status--set-in-flight top :status-ci nil)
         (magit-dash-gh--cache-set top :status-ci :empty)
         (magit-dash-gh--cache-set top :status-ci-time (float-time))
         (when callback (funcall callback nil))))))))

(defun magit-dash-status--fetch-branch-pr (dir callback &optional force)
  "Fetch PR for current branch in DIR asynchronously.
Calls CALLBACK with PR plist or nil. When FORCE is nil and cache
is fresh, invokes CALLBACK immediately."
  (let* ((top (magit-dash-status--repo-root dir))
         (branch (when (and top (file-directory-p top))
                   (let ((default-directory top))
                     (condition-case nil
                         (magit-get-current-branch)
                       (error nil))))))
    (cond
     ((or (null top) (null branch) (string-empty-p branch))
      (when top
        (magit-dash-gh--cache-set top :status-branch-pr :none)
        (magit-dash-gh--cache-set top :status-branch-pr-time (float-time)))
      (when callback (funcall callback nil)))
     ((and (not force) (not (magit-dash-status--cache-stale-p top :status-branch-pr)))
      (let ((cached (magit-dash-gh--cache-get top :status-branch-pr)))
        (when callback (funcall callback (if (eq cached :none) nil cached)))))
     ((magit-dash-status--in-flight-p top :status-branch-pr)
      nil)
     (t
      (magit-dash-status--set-in-flight top :status-branch-pr t)
      (magit-dash-gh--run-process
       (list "pr" "view"
             "--json" "number,title,state,reviewDecision,url,mergeable,headRefName,baseRefName,updatedAt,statusCheckRollup")
       top
       (lambda (output)
         (magit-dash-status--set-in-flight top :status-branch-pr nil)
         (let* ((pr-alist (condition-case nil
                              (json-parse-string output :object-type 'alist)
                            (error nil)))
                (parsed (and pr-alist (magit-dash-status--parse-pr pr-alist))))
           (magit-dash-gh--cache-set top :status-branch-pr (or parsed :none))
           (magit-dash-gh--cache-set top :status-branch-pr-time (float-time))
           (when callback (funcall callback parsed))))
       (lambda (_err _code)
         (magit-dash-status--set-in-flight top :status-branch-pr nil)
         (magit-dash-gh--cache-set top :status-branch-pr :none)
         (magit-dash-gh--cache-set top :status-branch-pr-time (float-time))
         (when callback (funcall callback nil))))))))

(defun magit-dash-status--fetch-authored-prs (dir callback &optional force)
  "Fetch open PRs authored by @me for repo in DIR asynchronously.
Calls CALLBACK with list of PR plists."
  (let ((top (magit-dash-status--repo-root dir)))
    (cond
     ((or (null top) (not (file-directory-p top)))
      (when callback (funcall callback nil)))
     ((and (not force) (not (magit-dash-status--cache-stale-p top :status-authored-prs)))
      (when callback (funcall callback (magit-dash-gh--cache-get top :status-authored-prs))))
     ((magit-dash-status--in-flight-p top :status-authored-prs)
      nil)
     (t
      (magit-dash-status--set-in-flight top :status-authored-prs t)
      (magit-dash-gh--run-process
       (list "pr" "list"
             "--author" "@me"
             "--state" "open"
             "--limit" (number-to-string magit-dash-status-authored-prs-limit)
             "--json" "number,title,state,reviewDecision,updatedAt,url,statusCheckRollup,headRefName")
       top
       (lambda (output)
         (magit-dash-status--set-in-flight top :status-authored-prs nil)
         (let* ((prs (condition-case nil
                         (json-parse-string output :array-type 'list :object-type 'alist)
                       (error nil)))
                (parsed (seq-map #'magit-dash-status--parse-pr prs)))
           (magit-dash-gh--cache-set top :status-authored-prs parsed)
           (magit-dash-gh--cache-set top :status-authored-prs-time (float-time))
           (when callback (funcall callback parsed))))
       (lambda (_err _code)
         (magit-dash-status--set-in-flight top :status-authored-prs nil)
         (magit-dash-gh--cache-set top :status-authored-prs nil)
         (magit-dash-gh--cache-set top :status-authored-prs-time (float-time))
         (when callback (funcall callback nil))))))))

;;; Formatting Helpers

(defun magit-dash-status--ci-symbol (conclusion status)
  "Format CI CONCLUSION and STATUS as a propertized symbol string."
  (cond
   ((equal conclusion "success")
    (propertize "✓ pass" 'face 'magit-dash-ci-pass-face))
   ((member conclusion '("failure" "timed_out" "startup_failure"))
    (propertize "✗ fail" 'face 'magit-dash-ci-fail-face))
   ((member status '("in_progress" "queued" "waiting" "pending" "requested"))
    (propertize "● running" 'face 'magit-dash-ci-pending-face))
   ((equal conclusion "cancelled")
    (propertize "⊘ cancelled" 'face 'shadow))
   ((equal conclusion "skipped")
    (propertize "— skipped" 'face 'shadow))
   (t
    (propertize "—" 'face 'shadow))))

(defun magit-dash-status--rollup-ci-symbol (rollup)
  "Format PR ROLLUP status check check-run/status symbol."
  (if (null rollup)
      (propertize "—" 'face 'shadow)
    (let* ((get-val (lambda (obj key)
                      (cond
                       ((null obj) nil)
                       ((and (listp obj) (consp (car-safe obj)))
                        (alist-get key obj))
                       ((and (consp obj) (eq (car obj) key))
                        (cdr obj))
                       ((listp obj)
                        (or (plist-get obj (intern (format ":%s" key)))
                            (map-elt obj key)))
                       (t nil))))
           (items (cond
                   ((and (listp rollup) (consp (car-safe rollup)) (consp (car-safe (car rollup))))
                    rollup)
                   ((and (listp rollup) (consp (car-safe rollup)))
                    (list rollup))
                   ((listp rollup)
                    (list rollup))
                   (t nil)))
           (conclusions
            (delq nil
                  (mapcar (lambda (item)
                            (or (funcall get-val item 'conclusion)
                                (funcall get-val item 'state)
                                (funcall get-val item 'status)))
                          items)))
           (overall
            (cond
             ((null conclusions)
              (cond
               ((stringp rollup) rollup)
               ((funcall get-val rollup 'state))
               ((funcall get-val rollup 'status))
               ((funcall get-val rollup 'conclusion))
               (t "UNKNOWN")))
             ((seq-some (lambda (c) (member (upcase (or c "")) '("FAILURE" "TIMED_OUT" "STARTUP_FAILURE"))) conclusions)
              "FAILURE")
             ((seq-some (lambda (c) (member (upcase (or c "")) '("IN_PROGRESS" "QUEUED" "PENDING" "WAITING"))) conclusions)
              "IN_PROGRESS")
             ((seq-every-p (lambda (c) (equal (upcase (or c "")) "SUCCESS")) conclusions)
              "SUCCESS")
             (t (car conclusions)))))
      (pcase (upcase (or overall ""))
        ("SUCCESS" (propertize "✓" 'face 'magit-dash-ci-pass-face))
        ((or "FAILURE" "TIMED_OUT" "STARTUP_FAILURE") (propertize "✗" 'face 'magit-dash-ci-fail-face))
        ((or "IN_PROGRESS" "QUEUED" "PENDING" "WAITING") (propertize "●" 'face 'magit-dash-ci-pending-face))
        ("CANCELLED" (propertize "⊘" 'face 'shadow))
        (_ (propertize "—" 'face 'shadow))))))
(defun magit-dash-status--format-review-decision (decision)
  "Format PR review DECISION with proper face."
  (pcase decision
    ("APPROVED" (propertize "Approved" 'face 'magit-dash-status-approved-face))
    ("CHANGES_REQUESTED" (propertize "Changes Requested" 'face 'magit-dash-status-changes-requested-face))
    ("REVIEW_REQUIRED" (propertize "Review Required" 'face 'shadow))
    (_ "")))

(defun magit-dash-status--format-pr-row (pr)
  "Format a single PR plist as a row for the status section."
  (let* ((num (format "#%-5d" (plist-get pr :number)))
         (rollup (plist-get pr :status-rollup))
         (ci-sym (magit-dash-status--rollup-ci-symbol rollup))
         (title (truncate-string-to-width (or (plist-get pr :title) "") 46 0 nil "…"))
         (rev (magit-dash-status--format-review-decision (plist-get pr :review-decision)))
         (age (if-let* ((up (plist-get pr :updated-at)))
                  (format "(%s)" (magit-dash-gh-pr-dashboard--format-age up))
                "")))
    (format "  %s  %s  %-46s %s %s"
            (propertize num 'face 'magit-dash-status-pr-number-face)
            ci-sym
            (propertize title 'face 'magit-dash-status-pr-title-face)
            age
            rev)))

;;; Keymaps & Section Handlers
(defvaralias 'magit-magit-dash-status-ci-section-map 'magit-dash-status-ci-section-map)
(defvaralias 'magit-magit-dash-status-pr-section-map 'magit-dash-status-pr-section-map)
(defvaralias 'magit-magit-dash-status-prs-section-map 'magit-dash-status-prs-section-map)

(defvar-keymap magit-dash-status-ci-section-map
  :doc "Keymap for CI header section in `magit-status'."
  "<remap> <magit-visit-thing>" #'magit-dash-status-ci-menu
  "RET"                         #'magit-dash-status-ci-menu
  "C-m"                         #'magit-dash-status-ci-menu)

(defvar-keymap magit-dash-status-pr-section-map
  :doc "Keymap for PR header/row section in `magit-status'."
  "<remap> <magit-visit-thing>" #'magit-dash-status-pr-menu
  "RET"                         #'magit-dash-status-pr-menu
  "C-m"                         #'magit-dash-status-pr-menu)

(defvar-keymap magit-dash-status-prs-section-map
  :doc "Keymap for authored PRs section heading in `magit-status'."
  "<remap> <magit-visit-thing>" #'magit-gh-pr-dash
  "RET"                         #'magit-gh-pr-dash
  "C-m"                         #'magit-gh-pr-dash)

(defclass magit-dash-status-ci-section (magit-section)
  ((keymap :initform 'magit-dash-status-ci-section-map)))

(defclass magit-dash-status-pr-section (magit-section)
  ((keymap :initform 'magit-dash-status-pr-section-map)))

(defclass magit-dash-status-prs-section (magit-section)
  ((keymap :initform 'magit-dash-status-prs-section-map)))
;;; Status Buffer Inserters

(defun magit-dash-status-insert-ci-header ()
  "Insert the GitHub CI status line into the `magit-status' header."
  (when (and magit-dash-status-enable
             (magit-dash-status--github-repo-p))
    (let* ((top (magit-dash-status--repo-root))
           (cached (magit-dash-gh--cache-get top :status-ci))
           (has-time (magit-dash-gh--cache-get top :status-ci-time)))
      (when (and (magit-dash-status--cache-stale-p top :status-ci)
                 (not (magit-dash-status--in-flight-p top :status-ci)))
        (let ((buf (current-buffer)))
          (magit-dash-status--fetch-ci
           top
           (lambda (&rest _)
             (magit-dash-status--schedule-refresh buf)))))
      (magit-insert-section (magit-dash-status-ci-section "ci")
        (let ((start (point)))
          (insert (format "%-10s" "CI: "))
          (cond
           ((and cached (not (eq cached :empty)))
            (let* ((conclusion (plist-get cached :conclusion))
                   (status     (plist-get cached :status))
                   (sym        (magit-dash-status--ci-symbol conclusion status))
                   (wf         (or (plist-get cached :workflow-name)
                                   (plist-get cached :name)
                                   "workflow"))
                   (age        (if-let* ((up (or (plist-get cached :updated-at)
                                                 (plist-get cached :created-at))))
                                   (format "(%s)" (magit-dash-gh-pr-dashboard--format-age up))
                                 "")))
              (insert (format "%s %s [%s]\n" sym age wf))))
           ((or (eq cached :empty) (and has-time (null cached)))
            (insert (propertize "— (no runs)\n" 'face 'shadow)))
           (t
            (insert (propertize "… (fetching)\n" 'face 'shadow))))
          (put-text-property start (point) 'keymap magit-dash-status-ci-section-map))))))

(defun magit-dash-status-insert-pr-header ()
  "Insert the current branch's GitHub PR line into the `magit-status' header."
  (when (and magit-dash-status-enable
             (magit-dash-status--github-repo-p))
    (let* ((top (magit-dash-status--repo-root))
           (cached (magit-dash-gh--cache-get top :status-branch-pr))
           (has-time (magit-dash-gh--cache-get top :status-branch-pr-time)))
      (when (and (magit-dash-status--cache-stale-p top :status-branch-pr)
                 (not (magit-dash-status--in-flight-p top :status-branch-pr)))
        (let ((buf (current-buffer)))
          (magit-dash-status--fetch-branch-pr
           top
           (lambda (&rest _)
             (magit-dash-status--schedule-refresh buf)))))
      (magit-insert-section (magit-dash-status-pr-section "pr")
        (let ((start (point)))
          (insert (format "%-10s" "PR: "))
          (cond
           ((and cached (not (eq cached :none)))
            (let* ((num   (format "#%d" (plist-get cached :number)))
                   (title (plist-get cached :title))
                   (state (plist-get cached :state))
                   (rev   (magit-dash-status--format-review-decision
                           (plist-get cached :review-decision))))
              (insert (format "%s \"%s\" [%s%s]\n"
                              (propertize num 'face 'magit-dash-status-pr-number-face)
                              (propertize title 'face 'magit-dash-status-pr-title-face)
                              (or state "OPEN")
                              (if (string-empty-p rev) "" (format " / %s" rev))))))
           ((or (eq cached :none) (and has-time (null cached)))
            (insert (propertize "none\n" 'face 'shadow)))
           (t
            (insert (propertize "… (fetching)\n" 'face 'shadow))))
          (put-text-property start (point) 'keymap magit-dash-status-pr-section-map))))))

(defun magit-dash-status-insert-authored-prs ()
  "Insert collapsible section for open PRs authored by @me in `magit-status'."
  (when (and magit-dash-status-enable
             (magit-dash-status--github-repo-p))
    (let* ((top (magit-dash-status--repo-root))
           (cached (magit-dash-gh--cache-get top :status-authored-prs)))
      (when (and (magit-dash-status--cache-stale-p top :status-authored-prs)
                 (not (magit-dash-status--in-flight-p top :status-authored-prs)))
        (let ((buf (current-buffer)))
          (magit-dash-status--fetch-authored-prs
           top
           (lambda (&rest _)
             (magit-dash-status--schedule-refresh buf)))))
      (when (and (listp cached) cached)
        (magit-insert-section (magit-dash-status-prs-section "prs")
          (let ((start (point)))
            (magit-insert-heading (format "Pull Requests (author: @me, %d)" (length cached)))
            (put-text-property start (point) 'keymap magit-dash-status-prs-section-map))
          (dolist (pr cached)
            (magit-insert-section (magit-dash-status-pr-section (plist-get pr :number))
              (let ((start (point)))
                (insert (magit-dash-status--format-pr-row pr))
                (insert "\n")
                (put-text-property start (point) 'keymap magit-dash-status-pr-section-map))))
          (insert "\n"))))))
;;; Transient Actions for CI

(defun magit-dash-status--current-ci-run ()
  "Return CI run plist from section at point or cached for current repo."
  (let* ((sec (magit-current-section))
         (val (and sec (oref sec value))))
    (if (and (listp val) (plist-get val :database-id))
        val
      (let ((top (magit-dash-status--repo-root)))
        (magit-dash-gh--cache-get top :status-ci)))))

(defun magit-dash-status-ci-browse ()
  "Open current CI run in browser, or repo Actions page if no run is cached."
  (interactive)
  (let* ((run (magit-dash-status--current-ci-run))
         (url (and (listp run) (plist-get run :url))))
    (if (and url (not (string-empty-p url)))
        (browse-url url)
      (let* ((top (magit-dash-status--repo-root))
             (urls (and top (let ((default-directory top))
                              (delq nil (list (magit-get "remote" "origin" "url")
                                              (magit-get "remote" "pushdefault" "url")
                                              (magit-get "remote" "upstream" "url")))))))
        (if-let* ((gh-url (seq-find (lambda (u) (string-match-p "github\\.com" u)) urls)))
            (let* ((cleaned (replace-regexp-in-string "\\.git\\'" "" gh-url))
                   (web (if (string-match "github\\.com[:/]\\(.+\\)" cleaned)
                            (format "https://github.com/%s/actions" (match-string 1 cleaned))
                          cleaned)))
              (browse-url web))
          (user-error "No CI run or GitHub remote URL available"))))))

(defun magit-dash-status-ci-failed-logs ()
  "Download and display failed logs for current CI run."
  (interactive)
  (let* ((run (magit-dash-status--current-ci-run))
         (run-id (and (listp run) (plist-get run :database-id))))
    (if run-id
        (magit-dash-gh-actions-fetch run-id)
      (magit-dash-gh-actions-fetch))))

(defun magit-dash-status-ci-full-logs ()
  "Download and display full logs for current CI run."
  (interactive)
  (magit-dash-gh-actions-fetch))

(defun magit-dash-status-ci-fix ()
  "Dispatch AI CI-fix prompt via agent-shell for current repo."
  (interactive)
  (let ((top (magit-dash-status--repo-root)))
    (if-let* ((repo (and (boundp 'magit-dash-repo-list)
                         (seq-find (lambda (r) (file-equal-p (magit-dash-repo-path r) top))
                                   magit-dash-repo-list))))
        (magit-dash-ci-dispatch-fix-operation repo)
      (user-error "Repository %s is not registered in `magit-dash-repo-list'" top))))

(defun magit-dash-status-ci-refresh ()
  "Force refresh of CI status for current repository."
  (interactive)
  (let ((top (magit-dash-status--repo-root))
        (buf (current-buffer)))
    (message "magit-dash: refreshing CI status...")
    (magit-dash-status--fetch-ci
     top
     (lambda (_)
       (when (buffer-live-p buf)
         (with-current-buffer buf (magit-refresh-buffer buf)))
       (message "magit-dash: CI status refreshed"))
     t)))

(defun magit-dash-status-ci-runs-list ()
  "Open workflow runs list buffer."
  (interactive)
  (if (fboundp 'magit-gh-actions)
      (magit-gh-actions)
    (magit-dash-gh-actions-fetch)))

(transient-define-prefix magit-dash-status-ci-menu ()
  "Actions for GitHub CI workflow run."
  ["CI Run Actions"
   ("b" "View run in browser"          magit-dash-status-ci-browse)
   ("lf" "Fetch & view failed logs"     magit-dash-status-ci-failed-logs)
   ("la" "Fetch & view full logs"       magit-dash-status-ci-full-logs)
   ("f" "Fix CI with agent-shell"      magit-dash-status-ci-fix)
   ("w" "Trigger workflow run"         magit-dash-gh-workflow-run)
   ("a" "Workflow runs list"           magit-dash-status-ci-runs-list)
   ("r" "Refresh CI status"            magit-dash-status-ci-refresh)
   ("q" "Quit"                         transient-quit-one)])
;;; Transient Actions for PR

(defun magit-dash-status--current-pr ()
  "Return PR plist from section at point or cached for current branch."
  (let* ((sec (magit-current-section))
         (val (and sec (oref sec value))))
    (cond
     ((and (listp val) (plist-get val :number))
      val)
     ((numberp val)
      (let* ((top (magit-dash-status--repo-root))
             (prs (magit-dash-gh--cache-get top :status-authored-prs)))
        (or (seq-find (lambda (p) (equal (plist-get p :number) val)) prs)
            (magit-dash-gh--cache-get top :status-branch-pr))))
     (t
      (let ((top (magit-dash-status--repo-root)))
        (magit-dash-gh--cache-get top :status-branch-pr))))))

(defun magit-dash-status--has-pr-at-point-p ()
  "Return non-nil if a valid PR is selected or cached for branch."
  (let ((pr (magit-dash-status--current-pr)))
    (and (listp pr) (plist-get pr :number) t)))

(defun magit-dash-status-pr-browse ()
  "Open pull request at point in web browser, or repo PRs page."
  (interactive)
  (let* ((pr (magit-dash-status--current-pr))
         (url (and (listp pr) (plist-get pr :url))))
    (if (and url (not (string-empty-p url)))
        (browse-url url)
      (let* ((top (magit-dash-status--repo-root))
             (urls (and top (let ((default-directory top))
                              (delq nil (list (magit-get "remote" "origin" "url")
                                              (magit-get "remote" "pushdefault" "url")
                                              (magit-get "remote" "upstream" "url")))))))
        (if-let* ((gh-url (seq-find (lambda (u) (string-match-p "github\\.com" u)) urls)))
            (let* ((cleaned (replace-regexp-in-string "\\.git\\'" "" gh-url))
                   (web (if (string-match "github\\.com[:/]\\(.+\\)" cleaned)
                            (format "https://github.com/%s/pulls" (match-string 1 cleaned))
                          cleaned)))
              (browse-url web))
          (user-error "No pull request or GitHub remote URL available"))))))

(defun magit-dash-status-pr-diff ()
  "View diff for pull request at point."
  (interactive)
  (let* ((pr (magit-dash-status--current-pr))
         (num (and (listp pr) (plist-get pr :number))))
    (if (and num (fboundp 'magit-gh-pr-diff))
        (magit-gh-pr-diff)
      (if num
          (magit-diff-range (format "origin/main...pull/%d/head" num))
        (user-error "No pull request at point")))))

(defun magit-dash-status-pr-worktree ()
  "Open pull request at point in dedicated worktree."
  (interactive)
  (let* ((pr (magit-dash-status--current-pr))
         (num (and (listp pr) (plist-get pr :number)))
         (top (magit-dash-status--repo-root)))
    (if (and num top)
        (magit-dash-gh-pr-dashboard-open-worktree
         (list :number num :repo (file-name-nondirectory (directory-file-name top))))
      (user-error "No pull request at point"))))

(defun magit-dash-status-pr-checkout ()
  "Checkout pull request branch locally."
  (interactive)
  (let* ((pr (magit-dash-status--current-pr))
         (branch (and (listp pr) (plist-get pr :head-ref))))
    (if (and branch (not (string-empty-p branch)))
        (magit--checkout branch)
      (user-error "No branch found for pull request"))))

(defun magit-dash-status-pr-checks ()
  "View CI checks for pull request."
  (interactive)
  (if (fboundp 'magit-gh-pr-checks)
      (magit-gh-pr-checks)
    (let* ((pr (magit-dash-status--current-pr))
           (num (and (listp pr) (plist-get pr :number)))
           (top (magit-dash-status--repo-root)))
      (if (and num top)
          (progn
            (require 'magit-dash-gh-actions)
            (magit-dash-gh-actions-fetch-for-pr num top))
        (call-interactively #'magit-dash-status-ci-menu)))))
(defun magit-dash-status-pr-fetch-threads ()
  "Fetch comments and review threads for pull request to disk."
  (interactive)
  (let* ((pr (magit-dash-status--current-pr))
         (num (and (listp pr) (plist-get pr :number))))
    (magit-dash-gh-pr-fetch num)))

(defun magit-dash-status-pr-refresh ()
  "Force refresh of PR status for current repository."
  (interactive)
  (let* ((top (magit-dash-status--repo-root))
         (buf (current-buffer))
         (pending 2)
         (on-done (lambda (&rest _)
                    (setq pending (1- pending))
                    (when (buffer-live-p buf)
                      (magit-dash-status--schedule-refresh buf))
                    (when (<= pending 0)
                      (message "magit-dash: PR status refreshed")))))
    (message "magit-dash: refreshing PR status...")
    (magit-dash-status--fetch-branch-pr top on-done t)
    (magit-dash-status--fetch-authored-prs top on-done t)))

(transient-define-prefix magit-dash-status-pr-menu ()
  "Actions for GitHub pull request."
  ["Pull Request Actions"
   ("v" "View in browser"              magit-dash-status-pr-browse)
   ("d" "View diff"                    magit-dash-status-pr-diff
    :inapt-if-not magit-dash-status--has-pr-at-point-p)
   ("w" "Open in worktree"             magit-dash-status-pr-worktree
    :inapt-if-not magit-dash-status--has-pr-at-point-p)
   ("c" "Checkout branch"              magit-dash-status-pr-checkout
    :inapt-if-not magit-dash-status--has-pr-at-point-p)
   ("k" "Checks / CI status"           magit-dash-status-pr-checks)
   ("t" "Fetch comments/threads"       magit-dash-status-pr-fetch-threads
    :inapt-if-not magit-dash-status--has-pr-at-point-p)
   ("p" "Open PR dashboard"            magit-gh-pr-dash)
   ("r" "Refresh PR data"              magit-dash-status-pr-refresh)
   ("q" "Quit"                         transient-quit-one)])

;;; Central Magit Dash Dispatcher (bound to "." and in magit-dispatch)

;;;###autoload
(defun magit-dash-status-refresh-all ()
  "Force refresh all GitHub status information for the current repository."
  (interactive)
  (let* ((top (magit-dash-status--repo-root))
         (buf (current-buffer))
         (pending 3)
         (on-done (lambda (&rest _)
                    (setq pending (1- pending))
                    (when (buffer-live-p buf)
                      (magit-dash-status--schedule-refresh buf))
                    (when (<= pending 0)
                      (message "magit-dash: refreshed all status data")))))
    (message "magit-dash: refreshing all status data...")
    (magit-dash-status--fetch-ci top on-done t)
    (magit-dash-status--fetch-branch-pr top on-done t)
    (magit-dash-status--fetch-authored-prs top on-done t)))

;;;###autoload
(transient-define-prefix magit-dash-dispatch ()
  "Magit Dash operations and repository tools."
  ["Dashboards"
   ("d" "Repository dashboard"         magit-dash-open)
   ("p" "PR dashboard"                 magit-gh-pr-dash)
   ("o" "Open / switch repository"     magit-dash-open-repo)]
  ["Actions"
   ("!" "Prune merged branches"        magit-dash-gh-prune-merged-branches)
   ("l" "Fetch CI logs"                magit-dash-gh-actions-fetch)
   ("c" "Fetch PR comments"            magit-dash-gh-pr-fetch)
   ("w" "Trigger workflow run"         magit-dash-gh-workflow-run)
   ("u" "Switch users"                 magit-dash-gh-auth-switch)
   ("r" "Refresh status data"          magit-dash-status-refresh-all)]
  ["Quit"
   ("q" "Quit"                         transient-quit-one)])
;;; Setup Function

;;;###autoload
(defun magit-dash-status-setup ()
  "Install GitHub status headers, sections, and keybindings into Magit."
  ;; Install headers
  (magit-add-section-hook 'magit-status-headers-hook
                          #'magit-dash-status-insert-ci-header
                          nil t)
  (magit-add-section-hook 'magit-status-headers-hook
                          #'magit-dash-status-insert-pr-header
                          nil t)

  ;; Install authored PRs section before recent/unpushed commits
  (magit-add-section-hook 'magit-status-sections-hook
                          #'magit-dash-status-insert-authored-prs
                          'magit-insert-unpushed-to-upstream-or-recent)

  ;; Bind "." key in magit buffers
  (when (boundp 'magit-mode-map)
    (keymap-set magit-mode-map "." #'magit-dash-dispatch))

  ;; Inject into magit-dispatch essential commands at the end with 7-space alignment
  (with-eval-after-load 'magit
    (transient-remove-suffix 'magit-dispatch ".")
    (transient-append-suffix 'magit-dispatch "C-x i"
      '("." "    Dashboard" magit-dash-dispatch))))

(provide 'magit-dash-status)
;;; magit-dash-status.el ends here
