;;; magit-dash-gh-actions.el --- Fetch GitHub Actions CI logs to disk -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.0") (annotated-completing-read "0.1"))
;; Keywords: vc, tools, magit, github, ci
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides `magit-dash-gh-actions-fetch', which downloads GitHub Actions CI logs for
;; the current branch to a local directory under plans/.  A sequential
;; async pipeline fetches the run list, run metadata, full logs, and
;; optionally a failed-steps-only log, then writes an index file summarising
;; the run state and collected artifacts.

;;; Code:

(require 'cl-lib)
(require 'map)

(require 'magit-dash-gh)

(declare-function magit-toplevel "magit-git")
(declare-function magit-get-current-branch "magit-git")

;;; Configuration

(defvar magit-dash-gh-actions-run-limit 10
  "Maximum number of CI runs shown when interactively selecting a run.")

(defvar magit-dash-gh-actions-include-failed-log t
  "When non-nil, download a failed-steps-only log alongside the full run log.")

(defvar magit-dash-gh-actions-open-dired nil
  "When non-nil, open a dired buffer in the artifact directory after fetch completes.")

;;; Log viewer mode

(define-derived-mode magit-dash-gh-actions-log-mode special-mode "GH-Log"
  "Major mode for viewing GitHub Actions CI log files.
Applies ANSI colour sequences and provides read-only navigation."
  (when (fboundp 'ansi-color-apply-on-region)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(add-to-list 'auto-mode-alist '("\\.ghlog\\'" . magit-dash-gh-actions-log-mode))

;;; Internal helpers

(defun magit-dash-gh-actions--failure-p (conclusion)
  "Return non-nil when CONCLUSION string indicates a failed run."
  (member conclusion '("failure" "timed_out" "startup_failure")))

(defun magit-dash-gh-actions--run-annotation (run &optional ordinal)
  "Return a one-line annotation string for RUN alist.
ORDINAL, when non-nil, is an integer position in the sorted run list (1 = most recent)."
  (let ((sha (map-elt run 'headSha)))
    (format "%4s  %-12s %-12s  %-30s  %s"
            (if ordinal (format "#%d" ordinal) "")
            (or (map-elt run 'status) "")
            (or (map-elt run 'conclusion) "in_progress")
            (or (map-elt run 'workflowName) "")
            (if sha (substring sha 0 (min 8 (length sha))) ""))))

(defun magit-dash-gh-actions--sort-runs (runs)
  "Return RUNS sorted most-recent first by createdAt."
  (sort (copy-sequence runs)
        (lambda (a b)
          (string> (or (map-elt a 'createdAt) "")
                   (or (map-elt b 'createdAt) "")))))

(defun magit-dash-gh-actions--select-run (runs)
  "Prompt the user to select from RUNS via annotated-completing-read.
Runs are presented most-recent first.  The annotation shows an ordinal
\\=#N (where #1 is most recent) and the first 8 characters of the commit SHA.
When RUNS has exactly one entry it is returned directly without prompting."
  (if (= (length runs) 1)
      (car runs)
    (annotated-completing-read
     (map-into
      (seq-map-indexed
       (lambda (run i)
         (let ((key (format "#%s %s"
                            (map-elt run 'databaseId)
                            (or (map-elt run 'name) ""))))
           (cons key (cons (magit-dash-gh-actions--run-annotation run (1+ i)) run))))
       (magit-dash-gh-actions--sort-runs runs))
      '(hash-table :test equal))
     :prompt "CI run => "
     :require-match t)))

;;; Pipeline steps

(defun magit-dash-gh-actions--step-finalize (ctx)
  "Write the index file, report completion, then invoke CTX's :on-complete hook.
:on-complete, when set, is a function called with CTX after the index is
written; it lets other callers (e.g. `magit-dash-gh-ci-dispatch-fix-operation')
reuse this download pipeline and react to its result without altering the
default behaviour of `magit-dash-gh-actions-fetch'."
  (let* ((dir      (plist-get ctx :dir))
         (run-info (plist-get ctx :run-info))
         (files    (plist-get ctx :files))
         (conclusion (or (map-elt run-info 'conclusion) "in_progress"))
         (data (magit-dash-gh--index-table
                "collected_at"  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                "type"          "ci"
                "branch"        (or (map-elt run-info 'headBranch) "")
                "run_id"        (or (map-elt run-info 'databaseId) 0)
                "workflow"      (or (map-elt run-info 'workflowName) "")
                "status"        (or (map-elt run-info 'status) "")
                "conclusion"    conclusion
                "has_failure"   (if (magit-dash-gh-actions--failure-p conclusion) t :false)
                "artifact_count" (length files)
                "files"         (apply #'vector
                                       (seq-map (lambda (f)
                                                  (magit-dash-gh--file-table
                                                   (plist-get f :path)
                                                   (plist-get f :type)))
                                                files)))))
    (magit-dash-gh--write-index dir data)
    (message "magit-gh-ci: done — %d file(s) in %s" (length files) dir)
    (when magit-dash-gh-actions-open-dired
      (dired dir))
    (when-let* ((on-complete (plist-get ctx :on-complete)))
      (funcall on-complete ctx))))

(defun magit-dash-gh-actions--step-failed-logs (ctx)
  "Fetch failed-step-only logs when applicable, then finalize."
  (let* ((run-info   (plist-get ctx :run-info))
         (conclusion (map-elt run-info 'conclusion))
         (run-id     (number-to-string (map-elt run-info 'databaseId)))
         (repo-dir   (plist-get ctx :repo-dir)))
    (if (and magit-dash-gh-actions-include-failed-log (magit-dash-gh-actions--failure-p conclusion))
        (progn
          (message "magit-gh-ci: fetching failed-step logs...")
          (magit-dash-gh--run-process
           (list "run" "view" run-id "--log-failed")
           repo-dir
           (lambda (output)
             (let ((file "run-failed-logs.ghlog"))
               (with-temp-file (expand-file-name file (plist-get ctx :dir))
                 (insert output))
               (magit-dash-gh-actions--step-finalize
                (magit-dash-gh--add-file ctx file "failed-logs"))))
           (magit-dash-gh--make-error-handler "magit-gh-ci" "failed-logs")))
      (magit-dash-gh-actions--step-finalize ctx))))

(defun magit-dash-gh-actions--step-logs (ctx)
  "Fetch the full text log for the selected run, then continue."
  (let* ((run-info (plist-get ctx :run-info))
         (run-id   (number-to-string (map-elt run-info 'databaseId)))
         (repo-dir (plist-get ctx :repo-dir)))
    (message "magit-gh-ci: fetching full logs (run #%s, may be large)..." run-id)
    (magit-dash-gh--run-process
     (list "run" "view" run-id "--log")
     repo-dir
     (lambda (output)
       (let ((file "run-logs.ghlog"))
         (with-temp-file (expand-file-name file (plist-get ctx :dir))
           (insert output))
         (magit-dash-gh-actions--step-failed-logs
          (magit-dash-gh--add-file ctx file "logs"))))
     (magit-dash-gh--make-error-handler "magit-gh-ci" "logs"))))

(defun magit-dash-gh-actions--step-run-info (ctx)
  "Fetch full run metadata, write run-info.json, then fetch logs."
  (let* ((run-id   (number-to-string (plist-get ctx :run-id)))
         (repo-dir (plist-get ctx :repo-dir)))
    (message "magit-gh-ci: fetching run info for #%s..." run-id)
    (magit-dash-gh--run-process
     (list "run" "view" run-id "--json"
           "databaseId,name,status,conclusion,createdAt,updatedAt,headBranch,headSha,event,workflowName,jobs")
     repo-dir
     (lambda (output)
       (let* ((run-info (json-parse-string output :object-type 'alist))
              (file     "run-info.json")
              (slug     (magit-dash-gh--branch-slug
                         (or (map-elt run-info 'headBranch)
                             (plist-get ctx :branch))))
              (dir      (magit-dash-gh--collect-dir
                         (plist-get ctx :root) 'ci slug
                         (map-elt run-info 'databaseId)))
              (ctx2     (plist-put (copy-sequence ctx) :run-info run-info))
              (ctx2     (plist-put ctx2 :dir dir)))
         (with-temp-file (expand-file-name file dir)
           (insert output))
         (magit-dash-gh-actions--step-logs
          (magit-dash-gh--add-file ctx2 file "metadata"))))
     (magit-dash-gh--make-error-handler "magit-gh-ci" "run-info"))))

(defun magit-dash-gh-actions--step-list (ctx)
  "List recent CI runs for the current branch and let the user select one."
  (let* ((branch   (plist-get ctx :branch))
         (repo-dir (plist-get ctx :repo-dir)))
    (message "magit-gh-ci: listing runs for %s..." branch)
    (magit-dash-gh--run-process
     (list "run" "list"
           "--branch" branch
           "--limit" (number-to-string magit-dash-gh-actions-run-limit)
           "--json" "databaseId,name,status,conclusion,createdAt,headBranch,headSha,event,workflowName")
     repo-dir
     (lambda (output)
       (let* ((run (magit-dash-gh-actions--select-run (or (json-parse-string output :array-type 'list :object-type 'alist)
							  (user-error "magit-gh-ci: no CI runs found for branch %s" branch))))
              (run-id (map-elt run 'databaseId)))
           (magit-dash-gh-actions--step-run-info
            (plist-put ctx :run-id run-id)))))
     (magit-dash-gh--make-error-handler "magit-gh-ci" "run-list")))

;;; PR-scoped pipeline entry

(defun magit-dash-gh-actions--step-list-pr (ctx)
  "List CI runs for a pull request and let the user select one."
  (let* ((pr-number (plist-get ctx :pr-number))
         (repo-dir (plist-get ctx :repo-dir)))
    (message "magit-gh-ci: listing runs for PR #%d..." pr-number)
    (magit-dash-gh--run-process
     (list "run" "list"
           "--pr" (number-to-string pr-number)
           "--limit" (number-to-string magit-dash-gh-actions-run-limit)
           "--json" "databaseId,name,status,conclusion,createdAt,headBranch,headSha,event,workflowName")
     repo-dir
     (lambda (output)
       (let* ((run (magit-dash-gh-actions--select-run (or (json-parse-string output :array-type 'list :object-type 'alist)
							  (user-error "magit-gh-ci: no CI runs found for PR #%d" pr-number))))
              (run-id (map-elt run 'databaseId))
              (branch (or (map-elt run 'headBranch) "")))
           (magit-dash-gh-actions--step-run-info (plist-put (plist-put ctx :run-id run-id) :branch branch)))))
     (magit-dash-gh--make-error-handler "magit-gh-ci" "pr-run-list")))

;;; Public API

;;;###autoload
(defun magit-dash-gh-actions-fetch-for-pr (pr-number repo-dir)
  "Fetch GitHub Actions CI logs for PR-NUMBER using REPO-DIR as the working directory.
PR-NUMBER is an integer.  REPO-DIR must be a local checkout of the repository
so that `gh' can resolve the remote when listing and downloading runs."
  (magit-dash-gh--check-gh)
  (magit-dash-gh-actions--step-list-pr (list :pr-number pr-number
					     :branch ""
					     :root repo-dir
					     :repo-dir repo-dir
					     :files nil)))

;;;###autoload
(defun magit-dash-gh-actions-fetch (&optional run-id)
  "Download GitHub Actions CI logs for the current branch.
When RUN-ID is non-nil, fetch that specific run without prompting.
Otherwise list recent runs and prompt for a selection.

Creates an artifact directory under plans/ containing:
  run-info.json        — full run metadata
  run-logs.ghlog       — complete step logs (opens in `magit-dash-gh-actions-log-mode')
  run-failed-logs.ghlog — failed-step logs (when `magit-dash-gh-actions-include-failed-log')
  index.json           — collection summary"
  (interactive)
  (magit-dash-gh--check-gh)
  (let* ((repo-dir (magit-dash-gh--repo-dir))
         (root     (or (magit-toplevel)
                       (user-error "Not inside a git repository")))
         (branch   (or (magit-get-current-branch)
                       (user-error "Not on a branch")))
         (ctx      (list :branch branch
                         :root root
                         :repo-dir repo-dir
                         :files nil)))
    (if run-id
        (magit-dash-gh-actions--step-run-info (plist-put ctx :run-id run-id))
      (magit-dash-gh-actions--step-list ctx))))

(provide 'magit-dash-gh-actions)

;;; magit-dash-gh-actions.el ends here
