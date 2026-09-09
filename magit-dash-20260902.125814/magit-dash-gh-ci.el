;;; magit-dash-gh-ci.el --- Lightweight GitHub Actions CI status for magit-dash -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.0"))
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

(require 'map)
(require 'magit-dash-gh)
(require 'magit-dash-gh-actions)

(declare-function magit-dash-repo-path "magit-dash")
(declare-function magit-dash-repo-name "magit-dash")
(declare-function magit-dash-repo-branch "magit-dash")
(declare-function magit-dash-repo-include-ci "magit-dash")
(declare-function magit-dash--current-branch "magit-dash")
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
  "Face for a passing CI run in the repository dashboard.")

(defface magit-dash-ci-fail-face
  '((t :inherit error))
  "Face for a failing CI run in the repository dashboard.")

(defface magit-dash-ci-pending-face
  '((t :inherit warning))
  "Face for an in-progress CI run in the repository dashboard.")

;;; Internal helpers

(defun magit-dash-gh-ci--failure-p (conclusion)
  "Return non-nil when CONCLUSION string indicates a failed run."
  (member conclusion '("failure" "timed_out" "startup_failure")))

(defun magit-dash-gh-ci--parse-runs (runs)
  "Return a CI status plist summarising RUNS (list of alists from gh run list).
Returns nil when RUNS is nil or empty."
  (when runs
    (let* ((latest (car runs))
           (conclusion (map-elt latest 'conclusion))
           (status (map-elt latest 'status))
           (run-id (map-elt latest 'databaseId))
           (url (map-elt latest 'url))
           (pass (seq-count (lambda (r)
                              (equal "success" (map-elt r 'conclusion)))
                            runs))
           (fail (seq-count (lambda (r)
                              (magit-dash-gh-ci--failure-p (map-elt r 'conclusion)))
                            runs)))
      (list :conclusion conclusion
            :status status
            :pass pass
            :fail fail
            :total (length runs)
            :run-id run-id
            :url url))))

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
               "--limit" "5"
               "--json" "databaseId,name,status,conclusion,url,workflowName")
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

(declare-function agent-shell-prompt-exec "agent-shell-prompt")

(defun magit-dash-ci--download-and-dispatch (repo run-id)
  "Dispatch the `fix-ci' prompt library workflow for REPO and RUN-ID.
Requires `agent-shell-prompt-exec' to be available from the `agent-shell-prompt' library."
  (if (fboundp 'agent-shell-prompt-exec)
      (let* ((path (magit-dash-repo-path repo))
             (repo-name (magit-dash-repo-name repo))
             (default-directory (file-name-as-directory path)))
        (agent-shell-prompt-exec 'fix-ci (list :repo repo-name :run-id run-id)))
    (user-error "magit-dash fix-CI requires the agent-shell-prompt library")))

;;;###autoload
(defun magit-dash-ci-dispatch-fix-operation (repo)
  "Dispatch the `fix-ci' prompt library workflow for REPO.
Invokes `agent-shell-prompt-exec' with `:repo' set to REPO's name so that
`fix-ci' automatically resolves the failing run-id or prompts via ACR."
  (unless (magit-dash-repo-include-ci repo)
    (user-error "magit-dash fix-CI: %s does not have CI enabled (:include-ci)"
                (magit-dash-repo-name repo)))
  (let* ((path (magit-dash-repo-path repo))
         (repo-name (magit-dash-repo-name repo)))
    (if (fboundp 'agent-shell-prompt-exec)
        (let ((default-directory (file-name-as-directory path)))
          (agent-shell-prompt-exec 'fix-ci (list :repo repo-name)))
      (user-error "magit-dash fix-CI requires the agent-shell-prompt library"))))

(provide 'magit-dash-gh-ci)
;;; magit-dash-gh-ci.el ends here
