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

(defun magit-dash-ci--build-fix-prompt (repo ctx)
  "Return a prompt string describing the CI failure downloaded into CTX for REPO.
CTX is the pipeline context passed to `magit-dash-gh-actions--step-finalize'
once the run's artifacts (metadata, full log, and failed-step log when
applicable) have been written to :dir.  The prompt links to every file in
CTX's :files so an agent can open them for context."
  (let* ((dir (plist-get ctx :dir))
         (run-info (plist-get ctx :run-info))
         (files (plist-get ctx :files))
         (conclusion (or (map-elt run-info 'conclusion) "in_progress"))
         (workflow (or (map-elt run-info 'workflowName) "CI"))
         (branch (or (map-elt run-info 'headBranch) (magit-dash-repo-branch repo) ""))
         (run-id (map-elt run-info 'databaseId)))
    (with-temp-buffer
      (insert (format "The `%s` GitHub Actions workflow %s on branch `%s` of %s (run #%s).\n\n"
                      workflow
                      (if (magit-dash-gh-actions--failure-p conclusion)
                          "failed"
                        "did not complete successfully")
                      branch
                      (magit-dash-repo-name repo)
                      run-id))
      (insert (format "Investigate the failure in the repository at %s and fix it.\n\n"
                      (magit-dash-repo-path repo)))
      (insert "The following CI artifacts were downloaded for reference:\n\n")
      (seq-do (lambda (f)
                (insert (format "- [%s](%s) — %s\n"
                                (plist-get f :path)
                                (expand-file-name (plist-get f :path) dir)
                                (plist-get f :type))))
              files)
      (buffer-string))))

(defun magit-dash-ci--exact-scoped-shell-buffers (dir)
  "Return live agent-shell buffers whose `default-directory' is exactly DIR.
Unlike `agent-shell-menu-project-buffers', this never matches a buffer
belonging to an enclosing superproject — only a session scoped to DIR
itself qualifies as \"open for this project\"."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (equal (file-name-as-directory default-directory) dir)))
              (agent-shell-buffers)))

(defun magit-dash-ci--ci-buffer-name (shell-buffer project-name)
  "Return the CI fix-it buffer name for SHELL-BUFFER's provider and PROJECT-NAME.
Formatted as `*<agent-shell-provider-name>-ci-<project>*'."
  (let* ((config (agent-shell-get-config shell-buffer))
         (provider (downcase (replace-regexp-in-string
                              " " "-" (or (map-elt config :buffer-name) "agent")))))
    (format "*%s-ci-%s*" provider project-name)))

(defun magit-dash-ci--rename-ci-buffer-when-ready (buf project-name)
  "Rename BUF to its CI name once BUF's agent-shell session is ready.
Renaming before the ACP handshake reaches `prompt-ready' races with
agent-shell/shell-maker's buffer-context tracking for in-flight
responses, which can misroute output to an unrelated agent-shell buffer.
Mirrors the readiness check `agent-shell--insert-to-shell-buffer' uses
before submitting to a freshly created session."
  (if (with-current-buffer buf
        (or (map-nested-elt agent-shell--state '(:session :id))
            (eq agent-shell-session-strategy 'new-deferred)))
      (with-current-buffer buf
        (rename-buffer (generate-new-buffer-name (magit-dash-ci--ci-buffer-name buf project-name))))
    (letrec ((token (agent-shell-subscribe-to
                     :shell-buffer buf
                     :event 'prompt-ready
                     :on-event (lambda (_event)
                                 (agent-shell-unsubscribe :subscription token)
                                 (when (buffer-live-p buf)
                                   (magit-dash-ci--rename-ci-buffer-when-ready buf project-name))))))
      token)))

(defun magit-dash-ci--new-shell-buffer (dir project-name)
  "Start a new agent-shell session scoped to DIR, named for PROJECT-NAME.
Diffs `agent-shell-buffers' before and after creation, since
`agent-shell-menu-new-shell-in-dir' does not return the new buffer, then
renames it to `*<agent-shell-provider-name>-ci-<project>*' once the
session is ready (see `magit-dash-ci--rename-ci-buffer-when-ready').
Returns the buffer."
  (let ((before (agent-shell-buffers)))
    (agent-shell-menu-new-shell-in-dir dir)
    (when-let* ((buf (seq-find (lambda (b) (not (memq b before))) (agent-shell-buffers))))
      (magit-dash-ci--rename-ci-buffer-when-ready buf project-name)
      buf)))

(defun magit-dash-ci--focus-shell-buffer (buf)
  "Pop to and focus agent-shell buffer BUF."
  (when (buffer-live-p buf)
    (if (and (fboundp 'agent-shell-viewport--show-buffer)
             (bound-and-true-p agent-shell-prefer-viewport-interaction))
        (agent-shell-viewport--show-buffer :shell-buffer buf)
      (if (fboundp 'agent-shell--display-buffer)
          (agent-shell--display-buffer buf)
        (pop-to-buffer buf)))))

(defun magit-dash-ci--focus-on-turn-complete (shell-buffer)
  "Subscribe to `turn-complete' on SHELL-BUFFER to focus it once the turn completes."
  (when (and shell-buffer (fboundp 'agent-shell-subscribe-to))
    (letrec ((token (agent-shell-subscribe-to
                     :shell-buffer shell-buffer
                     :event 'turn-complete
                     :on-event (lambda (_event)
                                 (agent-shell-unsubscribe :subscription token)
                                 (magit-dash-ci--focus-shell-buffer shell-buffer)))))
      token)))

(defun magit-dash-ci--dispatch-prompt (repo prompt)
  "Send PROMPT to an agent for REPO.
When an agent-shell session already open, scoped exactly to REPO's own
path (not any enclosing superproject), offers to reuse it.  Otherwise, or
if declined, starts a new session scoped to REPO's path, named
`*<agent-shell-provider-name>-ci-<project>*'.  Falls back to
`agent-shell-queue-add-unassigned' when agent-shell-menu isn't loaded.  As a
last resort (neither is loaded), copies PROMPT to the kill ring so it can be
pasted manually."
  (let* ((default-directory (file-name-as-directory (magit-dash-repo-path repo)))
         (project-name (magit-dash-repo-name repo))
         (existing (and (fboundp 'agent-shell-buffers)
                        (magit-dash-ci--exact-scoped-shell-buffers default-directory))))
    (cond
     ((and existing
           (y-or-n-p (format "magit-dash fix-CI: reuse open agent-shell %s for %s? "
                             (buffer-name (car existing)) project-name)))
      (let ((shell-buffer (car existing)))
        (magit-dash-ci--focus-on-turn-complete shell-buffer)
        (agent-shell-insert :text prompt :submit t :shell-buffer shell-buffer)
        (message "magit-dash fix-CI: sent to %s" (buffer-name shell-buffer))))
     ((fboundp 'agent-shell-menu-new-shell-in-dir)
      (if-let* ((shell-buffer (magit-dash-ci--new-shell-buffer default-directory project-name)))
          (progn
            (magit-dash-ci--focus-on-turn-complete shell-buffer)
            (agent-shell-insert :text prompt :submit t :shell-buffer shell-buffer)
            (message "magit-dash fix-CI: started %s" (buffer-name shell-buffer)))
        (message "magit-dash fix-CI: new agent-shell for %s did not initialize" project-name)))
     ((fboundp 'agent-shell-queue-add-unassigned)
      (agent-shell-queue-add-unassigned prompt)
      (message "magit-dash fix-CI: queued (agent-shell-menu not available for %s)" project-name))
     (t
      (kill-new prompt)
      (message "magit-dash fix-CI: agent-shell not available — prompt copied to kill ring")))))

(defun magit-dash-ci--download-and-dispatch (repo run-id)
  "Download RUN-ID's artifacts for REPO and dispatch a fix-CI prompt.
Reuses the download pipeline from `magit-dash-gh-actions.el' to fetch run
metadata, the full log, and (when the run failed) the failed-step-only log
into a directory under plans/, then builds and dispatches a fix-it prompt
linking those files via `magit-dash-ci--dispatch-prompt'."
  (let ((path (magit-dash-repo-path repo)))
    (magit-dash-gh--check-gh)
    (magit-dash-gh-actions--step-run-info
     (list :run-id run-id
           :root path
           :repo-dir path
           :branch (magit-dash-repo-branch repo)
           :files nil
           :on-complete (lambda (ctx)
                          (magit-dash-ci--dispatch-prompt
                           repo (magit-dash-ci--build-fix-prompt repo ctx)))))))

;;;###autoload
(defun magit-dash-ci-dispatch-fix-operation (repo)
  "Download the latest CI run's artifacts for REPO and dispatch a fix-CI prompt.
Uses REPO's cached :ci-status when present; otherwise fetches it first via
`magit-dash-gh-ci-fetch' before proceeding, so this can be called without a
prior manual CI-status fetch.  Signals `user-error' when REPO does not have
:include-ci set, or when no CI run is found for it even after fetching."
  (unless (magit-dash-repo-include-ci repo)
    (user-error "magit-dash fix-CI: %s does not have CI enabled (:include-ci)"
                (magit-dash-repo-name repo)))
  (let* ((path (magit-dash-repo-path repo))
         (cached (magit-dash-gh--cache-get path :ci-status)))
    (if-let* ((run-id (plist-get cached :run-id)))
        (magit-dash-ci--download-and-dispatch repo run-id)
      (progn
        (message "magit-dash fix-CI: fetching CI status for %s..." (magit-dash-repo-name repo))
        (magit-dash-gh-ci-fetch
         repo
         (lambda (ci)
           (if-let* ((run-id (plist-get ci :run-id)))
               (magit-dash-ci--download-and-dispatch repo run-id)
             (message "magit-dash fix-CI: no CI runs found for %s"
                      (magit-dash-repo-name repo)))))))))

(provide 'magit-dash-gh-ci)
;;; magit-dash-gh-ci.el ends here
