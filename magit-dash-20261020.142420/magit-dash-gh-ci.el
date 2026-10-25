;;; magit-dash-gh-ci.el --- Lightweight GitHub Actions CI status for magit-dash -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.0") (gen "0.1"))
;; Keywords: vc, tools, magit, github, ci
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; Provides lightweight CI status fetch for the magit-dash main repository
;; dashboard.  Fetches the most recent gh run list for the current branch,
;; caches the result as :ci-status in the shared magit-dash-gh cache, and
;; provides formatting, browser-open, and (stubbed) fix-CI dispatch.
;;
;; This is intentionally separate from magit-dash-gh-actions.el, which
;; downloads full CI logs interactively.  This module only fetches summary
;; data for dashboard rendering.

;;; Code:

(declare-function magit-dash-repo-repo "magit-dash")
(declare-function magit-dash-repo-clone-url "magit-dash")

(require 'map)
(require 'gen)
(require 'magit-dash-gh)
(require 'magit-dash-gh-actions)

(defvar magit-dash-max-concurrent-network-jobs)
(defvar magit-dash-max-concurrent-ci-jobs)
(defvar magit-dash-queue-yield-delay)
(defvar magit-dash-buffer-name)
(declare-function magit-dash-repo-path "magit-dash")
(declare-function magit-dash-repo-name "magit-dash")
(declare-function magit-dash-repo-branch "magit-dash")
(declare-function magit-dash-repo-include-ci "magit-dash")
(declare-function magit-dash--current-branch "magit-dash")
(declare-function magit-dash--update-entry "magit-dash")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-get-config "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell-viewport--show-buffer "agent-shell-viewport")
(declare-function agent-shell-menu-new-shell-in-dir "agent-shell-menu")
(declare-function agent-shell-queue-add-unassigned "agent-shell-queue")
(defvar agent-shell-prefer-viewport-interaction)

;;; Faces

(defface magit-dash-ci-pass-face
  '((t :inherit success))
  "Face for a passing CI run in the repository dashboard."
  :group 'magit-dash)

(defface magit-dash-ci-fail-face
  '((t :inherit error))
  "Face for a failing CI run in the repository dashboard."
  :group 'magit-dash)

(defface magit-dash-ci-pending-face
  '((t :inherit warning))
  "Face for an in-progress CI run in the repository dashboard."
  :group 'magit-dash)

;;; Internal helpers

(defun magit-dash-gh-ci--failure-p (conclusion)
  "Return non-nil when CONCLUSION string indicates a failed run."
  (member conclusion '("failure" "timed_out" "startup_failure")))

(defun magit-dash-gh-ci--parse-runs (runs)
  "Return a CI status plist summarising RUNS (list of alists from gh run list).
Only runs for the newest commit (by `headSha') are considered, since a commit
triggers several workflows and the newest run alone may be a passing one while
another workflow failed.  The overall :conclusion is \"failure\" if any such
run failed, else :status is \"in_progress\" if any is still running, else the
newest run's result.  :run-id and :url point at the first failing run when
there is one.  Returns nil when RUNS is nil or empty."
  (when runs
    (let* ((sha (map-elt (car runs) 'headSha))
           (current (if sha
                        (seq-filter (lambda (r) (equal sha (map-elt r 'headSha)))
                                    runs)
                      (list (car runs))))
           (failed (seq-find (lambda (r)
                               (magit-dash-gh-ci--failure-p (map-elt r 'conclusion)))
                             current))
           (pending (seq-find (lambda (r)
                                (member (map-elt r 'status)
                                        '("in_progress" "queued" "waiting" "pending")))
                              current))
           (latest (or failed pending (car current)))
           (pass (seq-count (lambda (r)
                              (equal "success" (map-elt r 'conclusion)))
                            current))
           (fail (seq-count (lambda (r)
                              (magit-dash-gh-ci--failure-p (map-elt r 'conclusion)))
                            current)))
      (list :conclusion (cond (failed (map-elt failed 'conclusion))
                              (pending nil)
                              (t (map-elt latest 'conclusion)))
            :status (cond (failed "completed")
                          (pending (map-elt pending 'status))
                          (t (map-elt latest 'status)))
            :pass pass
            :fail fail
            :total (length current)
            :run-id (map-elt latest 'databaseId)
            :url (map-elt latest 'url)))))

;;; Display

(defun magit-dash-gh-ci--format-status (ci-status)
  "Format CI-STATUS plist as a short propertized string for the dashboard column.
Returns a shadow \"—\" when CI-STATUS is nil."
  (if (null ci-status)
      (propertize "—" 'face 'shadow)
    (let ((conclusion (plist-get ci-status :conclusion))
          (status (plist-get ci-status :status)))
      (cond
       ((equal conclusion "success")
        (propertize "✓" 'face 'magit-dash-ci-pass-face))
       ((magit-dash-gh-ci--failure-p conclusion)
        (propertize "x" 'face 'magit-dash-ci-fail-face))
       ((member status '("in_progress" "queued"))
        (propertize "⟳" 'face 'magit-dash-ci-pending-face))
       (t (propertize "—" 'face 'shadow))))))

;;; Async fetch

(defun magit-dash-gh-ci-fetch (repo callback)
  "Fetch CI status for REPO asynchronously and call CALLBACK with the result.
CALLBACK receives a CI status plist (see `magit-dash-gh-ci--parse-runs') or
nil on error.  Does nothing when REPO does not have :include-ci set.
Always queries the branch currently checked out at REPO's path rather than a
possibly-stale cached branch, so the CI status reflects what is actually
checked out — important for worktrees, which are frequently switched to a
different branch than their cached stats reflect.
Caches the result as :ci-status in the shared magit-dash-gh cache."
  (when (magit-dash-repo-include-ci repo)
    (let* ((path (magit-dash-repo-path repo))
           (current (magit-dash--current-branch path))
           (branch (if (string-empty-p current)
                       (magit-dash-repo-branch repo)
                     current)))
      (if (not branch)
          (funcall callback nil)
        (magit-dash-gh--run-process
         (list "run" "list"
               "--branch" branch
               "--limit" "20"
               "--json" "databaseId,name,status,conclusion,url,workflowName,headSha")
         path
         (lambda (output)
           (let* ((runs (condition-case nil
                            (json-parse-string output
                                               :array-type 'list
                                               :object-type 'alist)
                          (error nil)))
                  (ci-status (magit-dash-gh-ci--parse-runs runs)))
             (magit-dash-gh--cache-set path :ci-status ci-status)
             (funcall callback ci-status)))
         (lambda (_ _)
           (funcall callback nil)))))))

;;;###autoload
(defun magit-dash-gh-ci-fetch-batch (repos &rest args)
  "Fetch CI status for REPOS using `gen-run-async' with bounded concurrency.
Concurrency is bounded by `magit-dash-max-concurrent-ci-jobs'.
ARGS may contain an optional positional ON-DONE callback or keyword arguments:
  :on-done FUNCTION
  :on-repo-done FUNCTION
  :concurrency INTEGER
  :delay FLOAT
Returns a cancellation handle."
  (let* ((on-done (when (and (car args) (not (keywordp (car args))))
                    (pop args)))
         (on-done (or (plist-get args :on-done) on-done))
         (on-repo-done (plist-get args :on-repo-done))
         (limit (or (plist-get args :concurrency)
                    (bound-and-true-p magit-dash-max-concurrent-ci-jobs)
                    (max 2 (round (* 1.5 (if (fboundp 'num-processors)
                                             (num-processors)
                                           4))))))
         (yield (or (plist-get args :delay)
                    (bound-and-true-p magit-dash-queue-yield-delay)
                    0.0))
         (ci-repos (seq-filter #'magit-dash-repo-include-ci repos))
         (buf (or (get-buffer (or (bound-and-true-p magit-dash-buffer-name) "*magit-dash*"))
                  (current-buffer))))
    (if (null ci-repos)
        (progn
          (when on-done (funcall on-done))
          (lambda (&rest _) nil))
      (gen-run-async
       (lambda (repo done)
         (magit-dash-gh-ci-fetch
          repo
          (lambda (ci-status)
            (when (and buf (buffer-live-p buf))
              (with-current-buffer buf
                (when (fboundp 'magit-dash--update-entry)
                  (magit-dash--update-entry repo))))
            (when on-repo-done
              (funcall on-repo-done repo ci-status))
            (funcall done))))
       ci-repos
       :concurrency limit
       :delay yield
       :on-done on-done))))

;;; Public commands

;;;###autoload
(defun magit-dash-gh-ci-open-last-run (repo)
  "Open the URL of the most recent CI run for REPO in the browser.
Does nothing when no CI status is cached for REPO."
  (when-let* ((path (magit-dash-repo-path repo))
              (ci (magit-dash-gh--cache-get path :ci-status))
              (url (plist-get ci :url)))
    (browse-url url)))

;;; Fix-CI prompt dispatch

(declare-function agent-shell-workflow-dispatch "agent-shell-workflow")
(declare-function agent-shell-workflow-exec "agent-shell-workflow")

(defun magit-dash-ci--repo-slug (repo)
  "Return OWNER/NAME slug for REPO struct or path."
  (let* ((path (magit-dash-repo-path repo))
         (default-directory (file-name-as-directory path)))
    (or (ignore-errors
          (when-let* ((info (magit-dash-gh--repo-info))
                      (o (plist-get info :owner))
                      (r (plist-get info :repo)))
            (format "%s/%s" o r)))
        (when-let* ((pair (or (magit-dash-gh--owner-repo-from-url (magit-dash-repo-repo repo))
                             (magit-dash-gh--owner-repo-from-url (magit-dash-repo-clone-url repo)))))
          (format "%s/%s" (car pair) (cdr pair)))
        (let ((name (magit-dash-repo-name repo)))
          (if (string-match-p "/" name)
              name
            (user-error "Cannot determine GitHub owner/repo slug for %s" name))))))

(defun magit-dash-ci--dispatch-prompt (id args)
  "Dispatch prompt ID with ARGS plist.
Calls dispatch (or fallback exec) with `:args' ARGS."
  (let ((fn (cond ((fboundp 'agent-shell-workflow-dispatch) #'agent-shell-workflow-dispatch)
                  ((fboundp 'agent-shell-workflow-exec) #'agent-shell-workflow-exec)
                  (t (user-error "magit-dash fix-CI requires the agent-shell-workflow library")))))
    (funcall fn id :args args)))

(defun magit-dash-ci--download-and-dispatch (repo run-id)
  "Dispatch the `fix-ci' prompt library workflow for REPO and RUN-ID.
Requires `agent-shell-workflow-dispatch' from `agent-shell-workflow'."
  (let* ((path (magit-dash-repo-path repo))
         (slug (magit-dash-ci--repo-slug repo))
         (default-directory (file-name-as-directory path)))
    (magit-dash-ci--dispatch-prompt 'fix-ci (list :repo slug :run-id run-id))))

;;;###autoload
(defun magit-dash-ci-dispatch-fix-operation (repo)
  "Dispatch the `fix-ci' prompt library workflow for REPO.
Invokes workflow dispatch with `:repo' set to REPO's OWNER/NAME slug."
  (unless (magit-dash-repo-include-ci repo)
    (user-error "magit-dash fix-CI: %s does not have CI enabled (:include-ci)"
                (magit-dash-repo-name repo)))
  (let* ((path (magit-dash-repo-path repo))
         (slug (magit-dash-ci--repo-slug repo))
         (default-directory (file-name-as-directory path)))
    (magit-dash-ci--dispatch-prompt 'fix-ci (list :repo slug))))

(provide 'magit-dash-gh-ci)
;;; magit-dash-gh-ci.el ends here
