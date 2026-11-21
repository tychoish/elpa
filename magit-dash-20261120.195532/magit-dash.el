;;; magit-dash.el --- Repository and PR dashboards for magit-gh -*- lexical-binding: t -*-

;; Author: sam kleinman
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (magit "4.0") (projectile "2.0") (transient "0.4") (annotated-completing-read "0.1") (sprite "0.1") (gen "0.1") (hitl "0.1"))
;; Keywords: vc, tools, magit, github
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; Provides two tabular dashboards:
;;
;; `magit-dash-open' — a tabulated-list view of registered
;; repositories showing branch, fetch time, behind status, and dirty state.
;; Press RET to open a per-repo overview buffer with PR counts and magit action
;; shortcuts.  Press m or ? for the transient actions menu.
;;
;; `magit-dash-pr-dashboard-open' — a tabulated-list view of pull requests with
;; filters for state, author, repo, and org.  Supports CI outcome, age, comment
;; count, and review decision columns.
;;
;; Register repositories with `magit-dash-register' or by adding structs
;; directly to `magit-dash-repo-list' with `add-to-list'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'project)
(require 'transient)
(require 'savehist)
(require 'magit)
(require 'gen)
(require 'format-spec)

(require 'annotated-completing-read)
(require 'sprite)

(require 'magit-dash-global)

(require 'magit-dash-gh-ci)
(require 'magit-dash-chain)
(require 'magit-dash-stack)
(require 'magit-dash-pr)
(require 'magit-dash-pr-status)
(require 'magit-dash-gh)
(require 'magit-dash-open)
(require 'magit-dash-submodules)
(require 'magit-dash-worktree)
(require 'magit-dash-status)
(require 'magit-dash-mcp nil t)

(declare-function magit-status-setup-buffer "magit-status")
(declare-function projectile-save-project-buffers "projectile")
(declare-function magit-diff-dwim "magit-diff")
(declare-function magit-diff "magit-diff")
(declare-function magit-commit-create "magit-commit")
(declare-function magit-fetch "magit-fetch")
(declare-function magit-pull-from-upstream "magit-pull")
(declare-function magit-push-current-to-pushremote "magit-push")
(declare-function magit-log-current "magit-log")
(declare-function magit-log "magit-log")
(declare-function magit-show-commit "magit-diff")
(declare-function magit-checkout "magit-branch")
(declare-function magit-merge-plain "magit-merge")
(declare-function magit-git-rebase "magit-sequence")
(declare-function magit-anything-modified-p "magit-status")
(declare-function magit-dash-worktree-for-branch "magit-dash-worktree")
(declare-function magit-worktree-checkout "magit-worktree")
(declare-function magit-worktree-delete "magit-worktree")
(declare-function magit-stash-clear "magit-stash")
(declare-function agent-shell-switch-buffer "agent-shell")
(declare-function agent-shell-new-shell "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell-select-config "agent-shell")
(declare-function agent-shell--auto-preferred-config "agent-shell")
(declare-function agent-shell-workflow--project-buffers "agent-shell-workflow")
(declare-function agent-shell-queue-buffer-open "agent-shell-queue")
(declare-function agent-shell-menu-project-buffers "agent-shell-menu")
(declare-function agent-shell-menu-switch-project-session "agent-shell-menu")
(declare-function agent-shell-workflow-dispatch-menu "agent-shell-workflow-menu")
(declare-function magit-dash-gh-ci-open "magit-dash-gh-ci")
(declare-function magit-dash-gh-ci-fetch "magit-dash-gh-ci")
(declare-function magit-dash-gh-ci-fetch-batch "magit-dash-gh-ci")
(declare-function magit-dash-gh-workflow-run "magit-dash-gh-actions")
(declare-function magit-dash-pr "magit-dash-pr")
(declare-function magit-dash-pr-views-menu "magit-dash-pr")
(autoload 'magit-dash-actionable-open "magit-dash-actionable" nil t)

(defconst magit-dash-buffer-name "*magit-dash-repos*")

;;;; Repository registry

(cl-defstruct (magit-dash-repo (:constructor magit-dash-repo--make) (:copier nil))
  "Registry entry for a local git repository."
  name
  path
  (repo nil)
  (include-prs nil)
  (include-ci nil)
  (auto-fetch nil)
  (auto-pull nil)
  (auto-commit nil)
  (auto-push nil)
  (hooks nil)
  (tags nil)
  (commands nil)
  (sort-hint nil)
  (worktree nil)
  (submodule nil)
  (branch nil)
  (sync-branches nil)
  (clone-url nil)
  (worktree-symlinks nil)
  (remote-sync nil)
  (sync-on-refresh nil)
  (fetch-on-refresh nil)
  (gh-account nil)
  (gh-host "github.com")
  (pr nil)
  (upstream-repo nil)
  (upstream-branch nil)
  (show-worktrees 'default)
  (show-submodules 'default)
  (parent nil))

(defalias 'magit-dash-repo-account #'magit-dash-repo-gh-account)
(defalias 'magit-dash-repo-host #'magit-dash-repo-gh-host)
(defalias 'magit-dash-repo-upstream #'magit-dash-repo-upstream-repo)
(defalias 'magit-dash-repo-remote-url #'magit-dash-repo-clone-url)
(defalias 'magit-dash-repo-symlinks #'magit-dash-repo-worktree-symlinks)
(defalias 'magit-dash-repo-auto-sync-on-refresh #'magit-dash-repo-sync-on-refresh)
(defalias 'magit-dash-repo-refresh-sync #'magit-dash-repo-sync-on-refresh)
(defalias 'magit-dash-repo-labels #'magit-dash-repo-tags)
(defalias 'magit-dash-repo-worktrees #'magit-dash-repo-show-worktrees)
(defalias 'magit-dash-repo-submodules #'magit-dash-repo-show-submodules)

(defvar magit-dash-repo-list '()
  "List of `magit-dash-repo' structs registered for dashboard display.
Use `magit-dash-register' to add entries.")

;;;; Operation registry and in-flight guarding

(cl-defstruct (magit-dash-operation (:constructor magit-dash-operation--make) (:copier nil))
  "Record of a repository operation."
  id
  repo-name
  repo-path
  label
  status        ; :running, :ok, :skipped, :error, :cancelled
  started-at    ; float-time
  finished-at   ; float-time or nil
  process       ; list of processes started for this op (dead ones pruned)
  error-text)

(defvar magit-dash--active-operations (make-hash-table :test #'equal)
  "Hash table mapping repo path to active `magit-dash-operation' struct.")

(defvar magit-dash--operation-history nil
  "List of completed or cancelled `magit-dash-operation' structs.")

(defvar magit-dash--operation-counter 0
  "Monotonic counter for generating operation IDs.")

(defcustom magit-dash-operation-timeout 900
  "Seconds after which a still-running repo operation is considered stuck.
A stuck operation (one whose completion callback never fired) would
otherwise block every later operation on that repo forever.  It is
reclaimed, recorded as an error, the next time the repo is checked."
  :type 'integer
  :group 'magit-dash)

(defun magit-dash--op-running-p (repo-path)
  "Return non-nil if an operation is currently running for REPO-PATH.
An operation older than `magit-dash-operation-timeout' is finished as
an error and reported as not running."
  (when-let* ((key (expand-file-name repo-path))
              (op (gethash key magit-dash--active-operations))
              ((eq (magit-dash-operation-status op) :running)))
    (if (> (- (float-time) (magit-dash-operation-started-at op))
           magit-dash-operation-timeout)
        (progn
          (message "magit-dash: reclaiming stuck %s operation for %s"
                   (magit-dash-operation-label op)
                   (magit-dash-operation-repo-name op))
          (magit-dash--op-finish op :error "timed out without completing")
          nil)
      t)))

(defun magit-dash--op-start (repo label &optional proc)
  "Start tracking an operation for REPO with LABEL.
If an operation is already running for REPO, log a message and return
nil (guard triggered).  Otherwise, create a `magit-dash-operation'
struct, store it in `magit-dash--active-operations', and return it."
  (let* ((path (expand-file-name (magit-dash-repo-path repo)))
         (name (magit-dash-repo-name repo)))
    (if (magit-dash--op-running-p path)
        (progn
          (message "magit-dash: %s operation already in progress for %s, skipping re-request" label name)
          nil)
      (cl-incf magit-dash--operation-counter)
      (let ((op (magit-dash-operation--make
                 :id (format "op-%d" magit-dash--operation-counter)
                 :repo-name name
                 :repo-path path
                 :label label
                 :status :running
                 :started-at (float-time)
                 :finished-at nil
                 :process (and proc (list proc))
                 :error-text nil)))
        (puthash path op magit-dash--active-operations)
        op))))

(defun magit-dash--op-finish (op status &optional error-text)
  "Mark operation OP as finished with STATUS and optional ERROR-TEXT."
  (when op
    (setf (magit-dash-operation-status op) status)
    (setf (magit-dash-operation-finished-at op) (float-time))
    (setf (magit-dash-operation-error-text op) error-text)
    (remhash (magit-dash-operation-repo-path op) magit-dash--active-operations)
    (push op magit-dash--operation-history)
    (when (> (length magit-dash--operation-history) 50)
      (setq magit-dash--operation-history (seq-take magit-dash--operation-history 50)))))

(defvar magit-dash--guarded-repos nil
  "Expanded repo paths currently executing within
`magit-dash--guarded-op-run'.")

(defun magit-dash--guarded-op-run (repo label op-fn on-complete)
  "Run OP-FN for REPO with operation guard LABEL.
If an operation is already running for REPO, invoke ON-COMPLETE with `skipped'
and skip execution.  Re-entrant invocations for the same repo within the same
guard context are permitted and execute OP-FN directly."
  (let ((path (expand-file-name (magit-dash-repo-path repo))))
    (if (member path magit-dash--guarded-repos)
        (funcall op-fn repo on-complete)
      (if-let* ((op (magit-dash--op-start repo label)))
          (let ((magit-dash--guarded-repos (cons path magit-dash--guarded-repos)))
            (condition-case err
                (funcall op-fn repo
                         (lambda (status &optional text)
                           (magit-dash--op-finish op status text)
                           (funcall on-complete status text)))
              ;; A synchronous failure in OP-FN never reaches its callback;
              ;; without this the op stays :running and blocks the repo.
              (error
               (let ((text (error-message-string err)))
                 (magit-dash--op-finish op :error text)
                 (funcall on-complete 'error text)))))
        (funcall on-complete 'skipped "operation already in progress")))))
(defmacro with-magit-dash (&rest body)
  "Execute BODY in the `magit-dash' buffer.
Refreshes the dashboard and ensures `magit-dash-mode' is active."
  `(progn
     (let* ((buf (get-buffer-create magit-dash-buffer-name))
	    (default-directory (if (eq (current-buffer) buf)
				   (expand-file-name "~/")
				 default-directory)))
     (with-current-buffer buf
       (unless (derived-mode-p 'magit-dash-mode)
	 (magit-dash-mode))
       (magit-dash-refresh)
       ,@body))))

(defmacro magit-dash--with-repo (repo &rest body)
  "Execute BODY with `default-directory' set to REPO path."
  (declare (indent 1))
  (let ((path (make-symbol "path")))
    `(let* ((,path (file-name-as-directory (magit-dash-repo-path ,repo)))
            (default-directory ,path))
       ,@body)))



(defgroup magit-dash nil
  "Multi-repository dashboard for magit."
  :group 'magit
  :prefix "magit-dash-")

(defcustom magit-dash-multi-github-host nil
  "Default GitHub host used by `magit-dash-register' when `:gh-host'
is unspecified.  When nil, defaults to \"github.com\"."
  :type '(choice (const :tag "Default (github.com)" nil) string)
  :group 'magit-dash)

(defcustom magit-dash-run-command-in-background nil
  "When non-nil, `magit-dash-run-command' runs commands in the background."
  :type 'boolean
  :group 'magit-dash)

(defcustom magit-dash-refresh-fetch t
  "When non-nil, `magit-dash-refresh' fetches for eligible repositories.
A repository is eligible if it has fetch or sync configured (e.g. `:auto-fetch',
`:auto-pull', `:sync-on-refresh', or other sync hooks/steps).
Fetches are subject to `magit-dash-refresh-fetch-min-interval'."
  :type 'boolean
  :group 'magit-dash)

(defcustom magit-dash-refresh-fetch-min-interval 600
  "Minimum seconds between automatic fetches during `magit-dash-refresh'.
Defaults to 600 seconds (10 minutes)."
  :type 'integer
  :group 'magit-dash)

(defcustom magit-dash-refresh-sync-min-interval 3600
  "Minimum seconds between automatic full-syncs during `magit-dash-refresh'.
Applied to repositories configured with `:sync-on-refresh'.
Defaults to 3600 seconds (1 hour)."
  :type 'integer
  :group 'magit-dash)

(declare-function magit-dash-register-sync-timer "magit-dash-timer")

(defcustom magit-dash-max-concurrent-jobs 4
  "Maximum concurrent local Git or subprocess jobs in dashboard refresh."
  :type 'integer
  :group 'magit-dash)

(defcustom magit-dash-max-concurrent-network-jobs 2
  "Maximum number of concurrent network jobs (git fetch/sync)."
  :type 'integer
  :group 'magit-dash)

(defcustom magit-dash-max-concurrent-ci-jobs
  (max 2 (round (* 1.5 (if (fboundp 'num-processors) (num-processors) 4))))
  "Maximum number of concurrent GitHub CI status fetch jobs.
Defaults to 1.5x the number of CPU cores."
  :type 'integer
  :group 'magit-dash)

(defcustom magit-dash-queue-yield-delay 0.05
  "Delay in seconds between job dequeues in cooperative async runner."
  :type 'float
  :group 'magit-dash)

(defcustom magit-dash-worktree-name-format "%s@"
  "Format string for worktree repository names in the Name column.
Receives the base repository name as a single argument.
Common values include \"%s@\", \"%s:w\", or \"%s\"."
  :type 'string
  :group 'magit-dash)

(defface magit-dash-status-pr-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for PR badge in repository dashboard status column."
  :group 'magit-dash)

(defcustom magit-dash-status-pr-badge "PR"
  "String for PR indicator badge in the Status column.
Common values are \"PR\" or \"⑂\"."
  :type 'string
  :group 'magit-dash)

(defcustom magit-dash-fetch-pull-submodules-repos '("emacs<conf>" "^emacs.*<conf>$")
  "List of repository names or regexes to auto-update submodules on fetch."
  :type '(repeat string)
  :group 'magit-dash)

(defcustom magit-dash-which-key-idle-delay 0.05
  "Idle delay in seconds for `which-key' in `magit-dash-mode' buffers.
When non-nil and `which-key-mode' is active, `which-key' is started
with this delay while in `magit-dash-mode'.  A small non-zero value like
0.01 or 0.05 is recommended over 0.0 to prevent timer contention.
Set to nil to leave `which-key-idle-delay' untouched."
  :type '(choice (number :tag "Seconds") (const :tag "Disabled" nil))
  :group 'magit-dash)

(defcustom magit-dash-which-key-abbreviate-commands 'strip
  "How to abbreviate command names in `which-key' for `magit-dash-mode'.
When set to `strip' (or t), the `magit-dash-' prefix is removed completely.
When set to `md', `magit-dash-' becomes `md-'.
When nil, no replacements are registered."
  :type '(choice (const :tag "Strip magit-dash- prefix completely" strip)
                 (const :tag "Abbreviate prefix as md-" md)
                 (const :tag "Disabled" nil))
  :group 'magit-dash)

(defcustom magit-dash-auto-commit-message-template
  "chore: auto-commit changes in %r [%h:%i, %s]"
  "Default commit message template for auto-commit operations.
Supports format specifiers expanded via `format-spec':
  %r, %n — repository name
  %p     — repository path
  %b     — current branch
  %h     — host name
  %i     — Emacs instance ID
  %s, %t — sync trigger (`interactive', `timer', etc.)
  %d     — current ISO timestamp
  %u     — user login name
If the template uses `%s' positional format specifiers, `format' is used."
  :type 'string
  :group 'magit-dash)

(defcustom magit-dash-auto-commit-novel-message-template
  "chore: initial auto-commit in novel repo %r [%h:%i, %s]"
  "Commit message template for auto-commit on novel repositories.
Used when a repository has no prior commits (unborn HEAD).
Supports the same format specifiers as `magit-dash-auto-commit-message-template'."
  :type 'string
  :group 'magit-dash)


(defun magit-dash--repo-pull-submodules-on-fetch-p (repo)
  "Return non-nil if REPO should automatically update submodules when fetched."
  (let ((name (magit-dash-repo-name repo)))
    (or (seq-some (lambda (pattern)
                    (or (equal pattern name)
                        (string-match-p pattern name)))
                  magit-dash-fetch-pull-submodules-repos)
        (let ((hooks (magit-dash-repo-hooks repo)))
          (and hooks
               (member 'pull-submodules
                       (append (plist-get (plist-get hooks :fetch) :post)
                               (plist-get (plist-get hooks :fetch) :pre))))))))

(defun magit-dash--repo-is-pr-p (repo stats)
  "Return non-nil if REPO or its current branch represents a pull request."
  (let ((branch (or (plist-get stats :branch) (magit-dash-repo-branch repo) ""))
        (path (magit-dash-repo-path repo)))
    (or (plist-get stats :is-pr)
        (magit-dash-gh--cache-get path :is-pr)
        (string-match-p "\\(?:^\\|/\\)pr-[0-9]+" branch)
        (string-match-p "^pull/[0-9]+" branch)
        (and (magit-dash-repo-worktree repo)
             (string-match-p "-pr-[0-9]+" (file-name-nondirectory (directory-file-name path)))))))

(defvar magit-dash--refresh-guards nil
  "Request tokens for the refresh currently being scheduled.")

(defvar magit-dash--refresh-cancel-handle nil
  "Cancellation handle function for active `gen-run-async' refresh runners.")

(defvar-local magit-dash--dim-overlay nil
  "Overlay used to gray out the dashboard buffer during refresh.")

(defvar-local magit-dash--stale-entries nil
  "Saved copy of `tabulated-list-entries' before refresh for reverting on cancellation.")

(defface magit-dash-dim-overlay
  '((((class color) (min-colors 88) (background dark))
     :foreground "#777777")
    (((class color) (min-colors 88) (background light))
     :foreground "#999999")
    (t :inherit shadow))
  "Face applied to dim the dashboard buffer while refreshing."
  :group 'magit-dash)

(defun magit-dash--dim-buffer (&optional buffer)
  "Gray out BUFFER (defaults to current buffer) and snapshot stale entries."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (unless magit-dash--stale-entries
          (setq magit-dash--stale-entries (copy-tree tabulated-list-entries)))
        (unless (and magit-dash--dim-overlay (overlay-buffer magit-dash--dim-overlay))
          (setq magit-dash--dim-overlay (make-overlay (point-min) (point-max)))
          (overlay-put magit-dash--dim-overlay 'face 'magit-dash-dim-overlay)
          (overlay-put magit-dash--dim-overlay 'priority 100))))))

(defun magit-dash--undim-buffer (&optional buffer)
  "Remove dim overlay and stale entries snapshot from BUFFER."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and magit-dash--dim-overlay (overlay-buffer magit-dash--dim-overlay))
          (delete-overlay magit-dash--dim-overlay))
        (setq magit-dash--dim-overlay nil)
        (setq magit-dash--stale-entries nil)))))

(defun magit-dash--revert-stale-buffer (&optional buffer)
  "Revert BUFFER to `magit-dash--stale-entries' and remove dim overlay."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when magit-dash--stale-entries
          (setq tabulated-list-entries (copy-tree magit-dash--stale-entries))
          (magit-dash--print))
        (magit-dash--undim-buffer buf)))))

(defun magit-dash--cancel-refresh-jobs ()
  "Cancel all in-flight refresh jobs."
  (when (functionp magit-dash--refresh-cancel-handle)
    (ignore-errors (funcall magit-dash--refresh-cancel-handle :cancel)))
  (setq magit-dash--refresh-cancel-handle nil)
  (let ((buf (if (derived-mode-p 'magit-dash-mode)
                 (current-buffer)
               (get-buffer (or (bound-and-true-p magit-dash-buffer-name) "*magit-dash*")))))
    (when (and buf (buffer-live-p buf))
      (magit-dash--revert-stale-buffer buf))))

;;;; Hooks and operations

(defconst magit-dash--hook-operations '(:fetch :pull :stage :stage-all :commit :push :sync)
  "Fixed vocabulary of operations a `:hooks' or `magit-dash-global-hooks'
entry may key on.")

(defconst magit-dash--hook-slots '(:pre :post :operation)
  "Fixed vocabulary of slots within a single operation's hooks plist.")

(defconst magit-dash--remote-sync-slots '(:hosts :branch :path :merge-method)
  "Fixed vocabulary of slots within a repo's `:remote-sync' plist.")

(defun magit-dash--validate-remote-sync (remote-sync)
  "Validate REMOTE-SYNC plist.
Signal `user-error' when REMOTE-SYNC contains keys outside
`magit-dash--remote-sync-slots' or has an invalid `:merge-method'."
  (when remote-sync
    (unless (listp remote-sync)
      (user-error "Magit-dash: :remote-sync must be a plist"))
    (seq-do
     (lambda (pair)
       (let ((slot (car pair)))
         (unless (memq slot magit-dash--remote-sync-slots)
           (user-error "Magit-dash: unknown remote-sync slot %s" slot))))
     (seq-partition remote-sync 2))
    (when-let* ((method (plist-get remote-sync :merge-method)))
      (unless (memq (if (symbolp method) method (intern method)) '(merge rebase))
        (user-error "Magit-dash: :merge-method must be 'merge or 'rebase, got %S" method)))))

(defvar magit-dash-global-hooks nil
  "Plist of `:pre'/`:post' hooks applied to every repo's operations.
Same shape as a repo's `:hooks', keyed by operation (`:fetch'/`:pull'/
`:commit'/`:push'/`:sync'), each value a plist of `:pre'/`:post' target
lists.  `:operation' is not valid here — a global operation override
would replace every repo's operation with the same thing, which is not
a real use case.  Set with `magit-dash-set-global-hooks' so the shape is
validated; do not `setq' directly.")

(defun magit-dash--validate-hooks (hooks &optional allow-operation)
  "Validate HOOKS structure.
Signal `user-error' when HOOKS contains an operation or slot outside the
fixed vocabulary of `magit-dash--hook-operations'/`magit-dash--hook-slots'.
`:operation' is only valid per-repo; pass ALLOW-OPERATION non-nil when
validating a `:hooks' argument, nil for `magit-dash-global-hooks'."
  (seq-do
   (lambda (op-pair)
     (let ((op (car op-pair)) (op-plist (cadr op-pair)))
       (unless (memq op magit-dash--hook-operations)
         (user-error "Magit-dash: unknown hook operation %s" op))
       (seq-do
        (lambda (slot-pair)
          (let ((slot (car slot-pair)))
            (unless (memq slot magit-dash--hook-slots)
              (user-error "Magit-dash: unknown hook slot %s" slot))
            (when (and (eq slot :operation) (not allow-operation))
              (user-error "Magit-dash: :operation is not valid in global hooks"))))
        (seq-partition op-plist 2))))
   (seq-partition hooks 2)))

(defun magit-dash-set-global-hooks (hooks)
  "Validate HOOKS and set `magit-dash-global-hooks' to it."
  (magit-dash--validate-hooks hooks)
  (setq magit-dash-global-hooks hooks))

(defun magit-dash--merge-hook-op (base addition)
  "Merge op-plists BASE and ADDITION.
Prefer BASE's `:operation' and append `:pre'/`:post' lists (BASE's entries
first)."
  (list :pre (append (plist-get base :pre) (plist-get addition :pre))
        :post (append (plist-get base :post) (plist-get addition :post))
        :operation (or (plist-get base :operation) (plist-get addition :operation))))

(defun magit-dash--merge-hooks (base addition)
  "Merge ADDITION's per-operation hooks into BASE without mutating either."
  (seq-reduce
   (lambda (acc op-pair)
     (let ((op (car op-pair)) (op-plist (cadr op-pair)))
       (plist-put (copy-sequence acc) op
                  (magit-dash--merge-hook-op (plist-get acc op) op-plist))))
   (seq-partition addition 2)
   base))

(defun magit-dash--expand-repo-path (path-or-url)
  "Expand PATH-OR-URL if it is a local path, otherwise return as-is."
  (if (and (stringp path-or-url)
           (not (string-match-p "\\`\\(?:[a-zA-Z][-a-zA-Z0-9+.]*://\\|git@\\)" path-or-url)))
      (expand-file-name path-or-url)
    path-or-url))

(defun magit-dash--normalize-tags (raw)
  "Normalize RAW tags or labels into a list of symbols."
  (cond
   ((null raw) nil)
   ((symbolp raw) (list raw))
   ((stringp raw) (list (intern raw)))
   ((listp raw)
    (delq nil
          (delete-dups
           (mapcar (lambda (x)
                     (cond
                      ((null x) nil)
                      ((symbolp x) x)
                      ((stringp x) (intern x))
                      (t nil)))
                   raw))))
   (t nil)))

(cl-defun magit-dash-register (&key name path repo include-prs include-ci auto-fetch auto-pull auto-commit auto-push auto-sync-command hooks tags tag label labels commands sort-hint worktree sync-branches timer clone-url remote-url worktree-symlinks symlinks remote-sync sync-on-refresh auto-sync-on-refresh refresh-sync fetch-on-refresh gh-account gh-host pr upstream-repo upstream upstream-branch (show-worktrees 'default) hide-worktrees (show-submodules 'default) hide-submodules auto-commit-template)
  "Register or replace a repository with NAME at absolute PATH.
REPO is the upstream repository path or URL for bootstrapping.
INCLUDE-PRS, INCLUDE-CI, AUTO-FETCH, AUTO-PULL, AUTO-COMMIT, AUTO-PUSH,
AUTO-SYNC-COMMAND, HOOKS, TAGS, COMMANDS, SORT-HINT, WORKTREE,
SYNC-BRANCHES, TIMER, CLONE-URL, REMOTE-URL, WORKTREE-SYMLINKS, SYMLINKS,
REMOTE-SYNC, SYNC-ON-REFRESH, AUTO-SYNC-ON-REFRESH, REFRESH-SYNC,
and FETCH-ON-REFRESH configure the repository settings.
Replaces any existing entry with the same name or path.

Keyword arguments:
  :repo           path (or URL) of upstream repository for bootstrapping.
  :include-prs    include in PR dashboard fetches.
  :include-ci     include in CI status column fetches (GitHub Actions).
  :pr             plist of per-repository PR tooling options, read through
                  `magit-dash-pr-repo-option'; see
                  `magit-dash-pr-known-options'.
  :auto-fetch     non-nil — run git fetch --all during auto-sync.
  :auto-pull      non-nil — run git pull during auto-sync (implies fetch).
                  Respects :sync-branches.
  :auto-commit    nil | t | STRING | FUNCTION — stage and commit dirty working tree.
                  When STRING, specifies a commit message template (see
                  `magit-dash-auto-commit-message-template').
                  FUNCTION receives the repo struct and returns a commit
                  message string.
  :auto-commit-template alias / explicit template keyword for :auto-commit.
  :auto-push      non-nil — run git push during auto-sync.
                  Respects :sync-branches.
  :hooks          plist keyed by operation (`:fetch'/`:pull'/`:commit'/
                  `:push'/`:sync'), each value a plist of:
                    :pre        list of targets run before the operation;
                                a target signaling `error' skips the
                                operation (reported as `skipped').
                    :post       list of targets run after the operation
                                succeeds (`ok' or a no-op `skipped');
                                failures are logged but never fail sync.
                    :operation  a single target that replaces the default
                                implementation for that operation.  For
                                `:fetch'/`:pull'/`:commit'/`:push' the step
                                still only runs when its own auto-* flag is
                                set; for `:sync' it replaces the *entire*
                                default pipeline (fetch/pull/commit/push
                                and their own settings are not consulted).
                  A target is a SYMBOL (looked up as a label in :commands),
                  a STRING (run as a shell command in the repo directory),
                  or a FUNCTION (called with (REPO ON-COMPLETE), where
                  ON-COMPLETE accepts `ok', `skipped', or `error').
                  See also `magit-dash-global-hooks' for hooks applied
                  across every repo.
  :auto-sync-command  deprecated; nil | SYMBOL | STRING | FUNCTION.
                  Equivalent to :hooks (:sync (:operation VALUE)) — replaces
                  the entire sync pipeline for this repo.  Merged with any
                  explicit :hooks also passed.
  :sync-branches  list of branch names on which auto-pull and auto-push are
                  permitted; nil means any branch.
  :tags           list or atom of symbols or strings for filtering in the dashboard.
  :tag            alias / singular form of :tags.
  :label          alias for :tags.
  :labels         alias for :tags.
  :show-worktrees boolean (t or nil) specifying whether auto-discovered
                  worktrees for this repo are shown by default.
  :hide-worktrees non-nil sets :show-worktrees to nil.
  :show-submodules boolean (t or nil) specifying whether auto-discovered
                  submodules for this repo are shown by default.
  :hide-submodules non-nil sets :show-submodules to nil.
  :commands       alist of (LABEL . FUNCTION) for the repo command picker.
  :sort-hint      number controlling display order; lower values appear first.
                  Repos without a sort-hint appear after all sorted repos.
  :worktree       non-nil when this entry represents a git worktree.
  :timer          plist forwarded to `magit-dash-register-sync-timer' (requires
                  magit-dash-timer).  Example: \\='(:kind idle :idle-delay 300).
  :clone-url      URL to clone repository from when missing on disk.
  :remote-url     alias for :clone-url.
  :worktree-symlinks  list of relative paths to symlink from main repo to
                      worktrees.
  :symlinks       alias for :worktree-symlinks.
  :remote-sync    plist (:hosts, :branch, :path, :merge-method) declaring
                  remote sync mirrors.
  :sync-on-refresh    non-nil — run full auto-sync pipeline during dashboard
                      refresh, subject to
                      `magit-dash-refresh-sync-min-interval'.
  :auto-sync-on-refresh alias for :sync-on-refresh.
  :refresh-sync       alias for :sync-on-refresh.
  :fetch-on-refresh   non-nil — mark repository eligible for fetch on
                      refresh.
  :gh-account         GitHub account login (e.g. \"myuser\") used for
                      gh CLI commands.
  :gh-host            GitHub host (defaults to \"github.com\" or
                      `magit-dash-multi-github-host\').
  :upstream-repo      path or URL of upstream repository for tracking/sync.
  :upstream           alias for :upstream-repo.
  :upstream-branch    upstream branch name to sync from (e.g. \"master\")."
  (unless (and name path)
    (user-error "Magit-dash: must specify name (%s) and path (%s)" name path))
  (when auto-sync-command
    (display-warning
     'magit-dash
     (format ":auto-sync-command is deprecated for %s; use :hooks (:sync (:operation %S)) instead"
             name auto-sync-command)
     :warning)
    (setq hooks (magit-dash--merge-hooks hooks (list :sync (list :operation auto-sync-command)))))
  (magit-dash--validate-hooks hooks t)
  (when remote-sync
    (magit-dash--validate-remote-sync remote-sync))
  (when pr
    (magit-dash-pr-check-options name pr))

  (let* ((abs-path (expand-file-name path))
         (upstream-target (or upstream-repo upstream (when repo (magit-dash--expand-repo-path repo))))
         (upstream (when repo (magit-dash--expand-repo-path repo)))
         (url (or clone-url remote-url))
         (links (or worktree-symlinks symlinks))
         (remote-sync-commands
          (when remote-sync
            (let ((hosts (plist-get remote-sync :hosts))
                  (rs-path (or (plist-get remote-sync :path) abs-path))
                  (rs-branch (or (plist-get remote-sync :branch)
                                 (if (consp sync-branches)
                                     (car sync-branches)
                                   sync-branches)))
                  (rs-merge-method (plist-get remote-sync :merge-method)))
              (seq-map
               (lambda (h)
                 (let ((h-str (if (symbolp h) (symbol-name h) h)))
                   (cons (intern (format "sync-%s" h-str))
                         (magit-dash-remote-sync-target
                          :host h-str
                          :path rs-path
                          :branch rs-branch
                          :merge-method rs-merge-method))))
               hosts))))
         (effective-commands (append commands remote-sync-commands))
         (sync-refresh (or sync-on-refresh auto-sync-on-refresh refresh-sync))
         (effective-tags
          (delete-dups
           (append (magit-dash--normalize-tags tags)
                   (magit-dash--normalize-tags tag)
                   (magit-dash--normalize-tags label)
                   (magit-dash--normalize-tags labels))))
         (effective-auto-commit (or auto-commit-template auto-commit))
         (effective-show-worktrees
          (cond
           (hide-worktrees nil)
           ((not (eq show-worktrees 'default)) show-worktrees)
           (t 'default)))
         (effective-show-submodules
          (cond
           (hide-submodules nil)
           ((not (eq show-submodules 'default)) show-submodules)
           (t 'default))))
    (setq magit-dash-repo-list
          (thread-last magit-dash-repo-list
            (seq-remove (lambda (r)
                          (or (equal name (magit-dash-repo-name r))
                              (equal abs-path (magit-dash-repo-path r)))))
            (append (list (magit-dash-repo--make
                           :name name
                           :path abs-path
                           :repo upstream
                           :include-prs include-prs
                           :include-ci include-ci
                           :auto-fetch auto-fetch
                           :auto-pull auto-pull
                           :auto-commit effective-auto-commit
                           :auto-push auto-push
                           :hooks hooks
                           :tags effective-tags
                           :commands effective-commands
                           :sort-hint sort-hint
                           :worktree worktree
                           :sync-branches sync-branches
                           :clone-url url
                           :worktree-symlinks links
                           :remote-sync remote-sync
                           :sync-on-refresh sync-refresh
                           :fetch-on-refresh fetch-on-refresh
                           :gh-account gh-account
                           :gh-host (or gh-host magit-dash-multi-github-host "github.com")
                           :pr pr
                           :upstream-repo upstream-target
                           :upstream-branch upstream-branch
                           :show-worktrees effective-show-worktrees
                           :show-submodules effective-show-submodules))))))
  (when (and timer (fboundp 'magit-dash-register-sync-timer))
    (apply #'magit-dash-register-sync-timer :name name :repos (list name) timer)))

(defconst magit-dash--register-known-keys
  '(:name :path :repo :include-prs :include-ci :auto-fetch :auto-pull
    :auto-commit :auto-commit-template :auto-push :auto-sync-command :hooks :tags :tag :label :labels
    :commands :sort-hint :worktree :sync-branches :timer :clone-url :remote-url
    :worktree-symlinks :symlinks :remote-sync :sync-on-refresh
    :auto-sync-on-refresh :refresh-sync :fetch-on-refresh :gh-account
    :gh-host :pr :upstream-repo :upstream :upstream-branch
    :show-worktrees :hide-worktrees :show-submodules :hide-submodules)
  "List of keyword arguments accepted by `magit-dash-register'.")

(defun magit-dash--merge-plists (base override)
  "Merge OVERRIDE plist over BASE plist key-by-key.
Keys present in OVERRIDE replace those in BASE; other keys in BASE are retained."
  (let ((result (copy-sequence base)))
    (while override
      (let ((key (pop override))
            (val (pop override)))
        (setq result (plist-put result key val))))
    result))

(defun magit-dash--filter-plist (plist allowed-keys)
  "Return a copy of PLIST retaining only keys in ALLOWED-KEYS."
  (let ((result nil))
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (when (memq k allowed-keys)
          (setq result (nconc result (list k v))))))
    result))

(defun magit-dash--normalize-registration-spec (spec)
  "Return a `magit-dash-register' keyword plist for registration SPEC.
SPEC may be:
  PATH                    name defaults to the directory's basename
  (NAME PATH)             name and path
  (NAME PATH UPSTREAM)    name, path and upstream repository
  (:name N ...)           a plist, used as-is (:path may be derived)."
  (pcase spec
    ((pred stringp)
     (list :name (file-name-nondirectory (directory-file-name spec))
           :path spec))
    (`(,(and (pred keywordp)) . ,_) (copy-sequence spec))
    (`(,(and name (pred stringp)) ,(and path (pred stringp)))
     (list :name name :path path))
    (`(,(and name (pred stringp)) ,(and path (pred stringp)) ,upstream)
     (list :name name :path path :repo upstream))
    (_ (user-error "Magit-dash: invalid registration spec: %S" spec))))

;;;###autoload
(cl-defun magit-dash-register-batch (&key root-path defaults repos)
  "Register repositories using common DEFAULTS and per-repo specs.
Return the list of names registered.
ROOT-PATH is an optional base path used to resolve relative repo paths.
DEFAULTS is a plist of keyword arguments passed to `magit-dash-register'.
  - `:repo-format' in DEFAULTS is a format string receiving the repo's
    directory basename to derive `:repo'.
REPOS is a list of specs, each a plist or a shorthand form accepted by
`magit-dash--normalize-registration-spec' (PATH, (NAME PATH) or
\(NAME PATH UPSTREAM)).
  - If `:path' is omitted, it is derived from ROOT-PATH and `:name'.
  - If `:repo' is omitted and `:repo-format' is available, it is derived via
    `(format repo-format (file-name-nondirectory (directory-file-name path)))'.
  - Any keyword arguments in the repo plist override those in DEFAULTS.
  - If `:ignore' is non-nil in the merged plist, the repository is skipped.
All specs are validated before any is registered.
Additive: repositories whose name or path is already registered in
`magit-dash-repo-list' are skipped so explicit registrations win."
  (let* ((effective-root (or root-path (plist-get defaults :root-path)))
         (resolved
          (thread-last repos
            (seq-map (lambda (repo-spec)
                       (magit-dash--merge-plists
                        defaults (magit-dash--normalize-registration-spec repo-spec))))
            (seq-remove (lambda (merged) (plist-get merged :ignore)))
            (seq-map
             (lambda (merged)
               (let* ((name (or (plist-get merged :name)
                                (user-error "Magit-dash: batch repo entry missing :name: %S" merged)))
                      (path (cond
                             ((plist-get merged :path)
                              (expand-file-name (plist-get merged :path)))
                             (effective-root
                              (expand-file-name name (expand-file-name effective-root)))
                             (t
                              (user-error "Magit-dash: must specify :path or :root-path for repo %s" name))))
                      (repo-format (plist-get merged :repo-format))
                      (repo (or (plist-get merged :repo)
                                (when repo-format
                                  (format repo-format (file-name-nondirectory
                                                       (directory-file-name path)))))))
                 (thread-first (magit-dash--filter-plist merged magit-dash--register-known-keys)
                   (plist-put :name name)
                   (plist-put :path path)
                   (plist-put :repo repo))))))))
    (seq-keep
     (lambda (plist)
       (unless (seq-some (lambda (r)
                           (or (equal (plist-get plist :name) (magit-dash-repo-name r))
                               (equal (plist-get plist :path) (magit-dash-repo-path r))))
                         magit-dash-repo-list)
         (apply #'magit-dash-register plist)
         (plist-get plist :name)))
     resolved)))

;;;###autoload
(cl-defun magit-dash-register-directory (directory &key defaults exceptions)
  "Register git repositories in DIRECTORY using DEFAULTS and EXCEPTIONS.
DIRECTORY is scanned for immediate child directories containing a
`.git' directory or file.
DEFAULTS is a plist of keyword arguments passed to `magit-dash-register'.
  - `:repo-format' supports a `%s' substitution on the directory basename.
EXCEPTIONS is an alist mapping directory basenames (or names) to a plist
of overriding keyword arguments, merged key-by-key over DEFAULTS.
Additive: repositories whose name or path is already registered in
`magit-dash-repo-list' are skipped."
  (let* ((dir (expand-file-name directory))
         (entries (when (file-directory-p dir)
                    (directory-files dir t directory-files-no-dot-files-regexp t)))
         (repos nil))
    (dolist (entry (sort entries #'string<))
      (when (and (file-directory-p entry)
                 (file-exists-p (expand-file-name ".git" entry)))
        (let* ((basename (file-name-nondirectory (directory-file-name entry)))
               (exc (or (cdr (assoc basename exceptions))
                        (cdr (assoc (intern basename) exceptions))))
               (repo-spec (magit-dash--merge-plists (list :name basename :path entry) exc)))
          (push repo-spec repos))))
    (magit-dash-register-batch
     :root-path dir
     :defaults defaults
     :repos (nreverse repos))))

;;;; Registry helpers

(defvar magit-dash-sync-trigger 'interactive
  "How the current auto-sync was triggered.
Bind to `timer' when invoking auto-sync from timer; defaults to `interactive'.")

(defun magit-dash--repo-novel-p (repo)
  "Return non-nil if REPO has no prior commits (a novel repository)."
  (let* ((path (magit-dash-repo-path repo))
         (head-hash (ignore-errors (magit-dash--head-hash path))))
    (or (null head-hash) (string-empty-p head-hash))))

(defun magit-dash--format-commit-message (template repo)
  "Expand TEMPLATE for REPO using `format-spec'.
Supports format characters:
  %r, %n — repository name
  %p     — repository path
  %b     — current branch
  %h     — host name
  %i     — Emacs instance ID
  %s, %t — sync trigger
  %d     — current ISO timestamp
  %u     — user login name
Falls back to `format' if TEMPLATE uses positional %s specifiers."
  (let* ((name (magit-dash-repo-name repo))
         (path (magit-dash-repo-path repo))
         (branch (let ((cb (ignore-errors (magit-dash--current-branch path))))
                   (if (and (stringp cb) (not (string-empty-p cb)))
                       cb
                     (or (magit-dash-repo-branch repo) ""))))
         (host (sprite-system-name))
         (inst (or (and (boundp 'sprite-instance-id) sprite-instance-id) "unknown"))
         (trigger (symbol-name magit-dash-sync-trigger))
         (date (format-time-string "%Y-%m-%d %H:%M:%S"))
         (user (user-login-name))
         (spec `((?r . ,name)
                 (?n . ,name)
                 (?p . ,path)
                 (?b . ,branch)
                 (?h . ,host)
                 (?i . ,inst)
                 (?s . ,trigger)
                 (?t . ,trigger)
                 (?d . ,date)
                 (?u . ,user))))
    (if (and (not (string-match-p "%[rnhibpdu]" template))
             (string-match-p "%s" template))
        (format template name host inst trigger)
      (condition-case nil
          (format-spec template spec)
        (error
         (format template name host inst trigger))))))

(defun magit-dash--commit-message-for-repo (repo)
  "Return the auto-commit message for REPO.
Chooses between a custom function, a custom template string,
the novel repository template, or the default auto-commit template."
  (let ((auto-commit (magit-dash-repo-auto-commit repo)))
    (cond
     ((functionp auto-commit)
      (funcall auto-commit repo))
     ((stringp auto-commit)
      (magit-dash--format-commit-message auto-commit repo))
     ((magit-dash--repo-novel-p repo)
      (magit-dash--format-commit-message magit-dash-auto-commit-novel-message-template repo))
     (t
      (magit-dash--format-commit-message magit-dash-auto-commit-message-template repo)))))

(defun magit-dash--default-commit-message (repo)
  "Return a default auto-commit message for REPO.
Expands `magit-dash-auto-commit-novel-message-template' when REPO is novel
\(has no prior commits), or `magit-dash-auto-commit-message-template' otherwise."
  (magit-dash--commit-message-for-repo repo))

(defun magit-dash--stage-all (repo-or-path)
  "Stage all untracked, unstaged, and deleted files in REPO-OR-PATH.
Follows ~/garen/scripts/git-readd by first removing any deleted tracked
files via `git rm --quiet --ignore-unmatch', then staging all working tree
changes via `git add -A'.  Return t on success, nil on failure."
  (let* ((path (if (stringp repo-or-path)
                   repo-or-path
                 (magit-dash-repo-path repo-or-path)))
         (default-directory (file-name-as-directory (expand-file-name path))))
    (magit-dash-gh--with-repo-dir default-directory
      (condition-case nil
          (let ((deleted (seq-filter (lambda (s) (not (string-empty-p s)))
                                     (ignore-errors
                                       (process-lines (or (bound-and-true-p magit-git-executable) "git")
                                                      "ls-files" "-d")))))
            (when deleted
              (dolist (chunk (seq-partition deleted 50))
                (apply #'magit-call-git "rm" "--quiet" "--ignore-unmatch" "--" chunk)))
            (= 0 (magit-call-git "add" "-A")))
        (error nil)))))

(defun magit-dash--stage-all-async (repo-or-path on-complete)
  "Stage all files in REPO-OR-PATH asynchronously (untracked, unstaged, deleted).
Calls ON-COMPLETE with `ok' on success or `error' on failure."
  (let ((path (if (stringp repo-or-path)
                  repo-or-path
                (magit-dash-repo-path repo-or-path))))
    (magit-dash--run-git
     path '("ls-files" "-d")
     (lambda (deleted-out)
       (let* ((deleted (seq-filter (lambda (s) (not (string-empty-p s)))
                                   (split-string deleted-out "\n" t)))
              (chunks (seq-partition deleted 50)))
         (letrec ((do-add
                   (lambda ()
                     (magit-dash--run-git
                      path '("add" "-A")
                      (lambda (_) (funcall on-complete 'ok))
                      (lambda (out code)
                        (funcall on-complete 'error (format "git add -A failed (exit %d): %s" code out))))))
                  (run-chunks
                   (lambda (rem)
                     (if (null rem)
                         (funcall do-add)
                       (magit-dash--run-git
                        path (append '("rm" "--quiet" "--ignore-unmatch" "--") (car rem))
                        (lambda (_) (funcall run-chunks (cdr rem)))
                        (lambda (out code)
                          (funcall on-complete 'error (format "git rm failed (exit %d): %s" code out))))))))
           (funcall run-chunks chunks))))
     (lambda (out code)
       (funcall on-complete 'error (format "git ls-files failed (exit %d): %s" code out))))))

(defun magit-dash--auto-commit (repo)
  "Stage all modified files in REPO and commit using its :auto-commit message.
Return t when the commit succeeds, nil otherwise."
  (let ((message (magit-dash--commit-message-for-repo repo)))
    (and (magit-dash--stage-all repo)
         (magit-dash-gh--with-repo-dir (magit-dash-repo-path repo)
           (= 0 (magit-call-git "commit" "-m" message))))))

(defun magit-dash--normalize-command-op (op)
  "Normalize OP from a `:commands' entry, returning (TARGET . BACKGROUND-P)."
  (cond
   ((and (listp op) (keywordp (car op)))
    (cons (plist-get op :command) (and (plist-get op :background) t)))
   ((and (listp op) (cdr op) (keywordp (cadr op)))
    (cons (car op) (and (plist-get (cdr op) :background) t)))
   (t (cons op nil))))

(defun magit-dash--execute-command-on-repo (repo op &optional background cmd-name)
  "Execute command OP for REPO.
When BACKGROUND is non-nil or `magit-dash-run-command-in-background' is non-nil,
run the command in the background without popping up or stealing focus.
CMD-NAME is an optional display label for the command."
  (let* ((normalized (magit-dash--normalize-command-op op))
         (target (car normalized))
         (in-bg (or background (cdr normalized) magit-dash-run-command-in-background))
         (repo-name (magit-dash-repo-name repo))
         (repo-path (magit-dash-repo-path repo))
         (name (or cmd-name (format "%s" target)))
         (cmd-slug (replace-regexp-in-string "[[:space:]]+" "-" (string-trim (format "%s" name)))))
    (magit-dash-gh--with-repo-dir repo-path
      (cond
       ((stringp target)
        (let ((buf-name (format "*%s-dash-cmd-%s*" repo-name cmd-slug)))
          (if in-bg
              (let ((display-buffer-alist
                     (cons (cons (regexp-quote buf-name)
                                 (cons #'display-buffer-no-window '((allow-no-window . t))))
                           display-buffer-alist)))
                (message "magit-dash: [%s] running '%s' in background..." repo-name name)
                (let ((comp-buf (compilation-start target nil (lambda (_) buf-name))))
                  (with-current-buffer comp-buf
                    (add-hook 'compilation-finish-functions
                              (lambda (_buf status)
                                (message "magit-dash: [%s] command '%s' %s"
                                         repo-name name (string-trim status)))
                              nil t))
                  comp-buf))
            (compilation-start target nil (lambda (_) buf-name)))))
       ((commandp target)
        (if in-bg
            (progn
              (message "magit-dash: [%s] running command '%s' in background..." repo-name name)
              (save-window-excursion (call-interactively target))
              (message "magit-dash: [%s] command '%s' finished" repo-name name))
          (call-interactively target)))
       ((functionp target)
        (if in-bg
            (progn
              (message "magit-dash: [%s] running function '%s' in background..." repo-name name)
              (save-window-excursion (funcall target))
              (message "magit-dash: [%s] function '%s' finished" repo-name name))
          (funcall target)))
       ((symbolp target)
        (user-error "Command '%s' resolves to undefined function '%s'" name target))
       (t (user-error "Command has unsupported type %s" (type-of target)))))))

(defun magit-dash--run-command-for (repo &optional background)
  "Run a registered command for REPO.
Open an `annotated-completing-read' command picker for REPO and invoke
the selected command with `default-directory' set to REPO's repository root.
When BACKGROUND is non-nil or `magit-dash-run-command-in-background' is non-nil,
run the command in the background without popping up or stealing focus."
  (let* ((explicit-commands (magit-dash-repo-commands repo))
         (commands (if (magit-dash-repo-include-ci repo)
                       (if (assoc "rebuild" explicit-commands)
                           explicit-commands
                         (append explicit-commands
                                 '(("rebuild" . magit-dash-gh-workflow-run))))
                     explicit-commands))
         (commands (if (magit-dash-repo-upstream-repo repo)
                       (let ((cmds commands))
                         (unless (or (assoc "merge-upstream" cmds) (assoc 'merge-upstream cmds))
                           (setq cmds (append cmds '(("merge-upstream" . magit-dash-upstream-merge)))))
                         (unless (or (assoc "rebase-upstream" cmds) (assoc 'rebase-upstream cmds))
                           (setq cmds (append cmds '(("rebase-upstream" . magit-dash-upstream-rebase)))))
                         cmds)
                     commands)))
    (unless commands
      (user-error "No commands registered for %s" (magit-dash-repo-name repo)))
    (let* ((picked (annotated-completing-read
                    (seq-map (lambda (cmd)
                               (let* ((label (format "%s" (car cmd)))
                                      (norm (magit-dash--normalize-command-op (cdr cmd)))
                                      (target (car norm))
                                      (ann (cond
                                            ((eq target 'magit-dash-gh-workflow-run) "workflow run")
                                            ((eq target 'magit-dash-upstream-merge) "merge upstream branch into local main")
                                            ((eq target 'magit-dash-upstream-rebase) "rebase local main onto upstream branch")
                                            ((eq target 'magit-dash-upstream-fetch) "fetch upstream branch")
                                            ((stringp target) target)
                                            (t (format "%s" target)))))
                                 (cons label (cons ann (cons label (cdr cmd))))))
                             commands)
                    :prompt (format "%s command%s: "
                                    (magit-dash-repo-name repo)
                                    (if (or background magit-dash-run-command-in-background)
                                        " (background)" ""))
                    :require-match t))
           (cmd-name (if (consp picked)
                         (car picked)
                       (or (car (seq-find (lambda (c) (equal (cdr c) picked)) commands))
                           "cmd")))
           (op (if (consp picked) (cdr picked) picked)))
      (magit-dash--execute-command-on-repo repo op background cmd-name))))

(defun magit-dash--resolve-git-dir (path)
  "Return the git directory for the repository, worktree, or submodule at PATH.
Handles the case where PATH/.git is a file containing a `gitdir: ...' pointer
\(worktrees and submodules) rather than PATH/.git being a directory itself.
For a linked worktree this is its private directory, which holds its own
HEAD but not FETCH_HEAD or refs — see `magit-dash--resolve-common-git-dir'
for those.  Returns nil when PATH has no .git entry."
  (let ((dot-git (expand-file-name ".git" path)))
    (cond
     ((file-directory-p dot-git) dot-git)
     ((file-exists-p dot-git)
      (let ((line (with-temp-buffer
                    (insert-file-contents dot-git)
                    (string-trim (buffer-string)))))
        (when (string-prefix-p "gitdir: " line)
          (let ((gitdir (substring line 8)))
            (if (file-name-absolute-p gitdir)
                gitdir
              (expand-file-name gitdir path))))))
     (t nil))))

(defun magit-dash--resolve-common-git-dir (path)
  "Return the shared git directory for the repository at PATH.
For a linked worktree, `magit-dash--resolve-git-dir' returns the worktree's
private directory; this follows that directory's `commondir' file to the
directory shared across all worktrees, where FETCH_HEAD and refs actually
live.  Plain repositories and submodules have no `commondir' file, so their
resolved git directory is returned as-is."
  (when-let* ((git-dir (magit-dash--resolve-git-dir path)))
    (let ((commondir-file (expand-file-name "commondir" git-dir)))
      (if (file-exists-p commondir-file)
          (expand-file-name
           (string-trim (with-temp-buffer
                          (insert-file-contents commondir-file)
                          (buffer-string)))
           git-dir)
        git-dir))))

(defun magit-dash--fetch-age (path)
  "Return seconds since last fetch for repo at PATH, or nil if never fetched.
Resolves worktree/submodule gitlinks (and, for worktrees, the shared
commondir) so FETCH_HEAD is read from the correct location instead of
assuming PATH/.git is a plain directory."
  (when-let* ((git-dir (magit-dash--resolve-common-git-dir path))
              (fetch-head (expand-file-name "FETCH_HEAD" git-dir))
              (attrs (and (file-exists-p fetch-head) (file-attributes fetch-head))))
    (float-time (time-since (file-attribute-modification-time attrs)))))

(defun magit-dash--head-hash (path)
  "Return current HEAD commit hash for repo at PATH without spawning a process.
Resolves PATH's git directory (following worktree/submodule gitlinks) and
reads HEAD directly, resolving symbolic refs via file I/O against the
shared commondir so worktree refs resolve correctly.
Returns nil if the repo has no commits yet."
  (when-let* ((git-dir (magit-dash--resolve-git-dir path))
              (head-file (expand-file-name "HEAD" git-dir))
              (head (and (file-exists-p head-file)
                         (string-trim (with-temp-buffer
                                        (insert-file-contents head-file)
                                        (buffer-string))))))
    (if-let* ((_ (string-prefix-p "ref: " head))
              (common-dir (magit-dash--resolve-common-git-dir path))
              (ref-path (expand-file-name (substring head 5) common-dir))
              (_ (file-exists-p ref-path)))
        (string-trim (with-temp-buffer
                       (insert-file-contents ref-path)
                       (buffer-string)))
      head)))

(defun magit-dash--commit-timestamp-to-age (timestamp-string)
  "Convert TIMESTAMP-STRING (unix seconds, or empty) to a seconds-ago float.
Returns nil when TIMESTAMP-STRING is empty (no commits yet)."
  (unless (string-empty-p timestamp-string)
    (- (float-time) (string-to-number timestamp-string))))

(defvar magit-dash--submodule-path-set nil
  "Hash table mapping auto-discovered submodule path → name.
Rebuilt on each refresh.  Used to detect explicitly-registered repos that
are also submodules and to derive their parent<mod> display name.")

(defun magit-dash--classify-porcelain-status (repo-path porcelain-lines)
  "Classify PORCELAIN-LINES for REPO-PATH into plist of dirty flags.
Returns (:uncommitted BOOL :untracked BOOL :submodule-dirty BOOL)."
  (let ((uncommitted nil)
        (untracked nil)
        (submodule-dirty nil))
    (dolist (line porcelain-lines)
      (unless (string-empty-p line)
        (if (string-prefix-p "??" line)
            (setq untracked t)
          (let* ((rel-path (string-trim (substring line 2)))
                 (rel-path (if (string-match "-> \\(.*\\)$" rel-path)
                               (match-string 1 rel-path)
                             rel-path))
                 (abs-path (expand-file-name rel-path repo-path))
                 (is-subm (or (and magit-dash--submodule-path-set
                                   (gethash abs-path magit-dash--submodule-path-set))
                              (let ((sms (magit-dash-gh--cache-get repo-path :submodules)))
                                (seq-some (lambda (sm) (equal (magit-dash-repo-path sm) abs-path)) sms))
                              (file-exists-p (expand-file-name ".git" abs-path)))))
            (if is-subm
                (setq submodule-dirty t)
              (setq uncommitted t))))))
    (list :uncommitted uncommitted
          :untracked untracked
          :submodule-dirty submodule-dirty)))

(defun magit-dash--collect-stats (repo)
  "Synchronously collect git stats for REPO and store them in the cache.
Returns a plist with keys :branch :remote-origin :behind :ahead :dirty
:uncommitted-files :fetch-age :updated-age :head-hash :recent-log."
  (let* ((path (magit-dash-repo-path repo))
         (default-directory path)
         (branch (or (magit-git-string "branch" "--show-current") ""))
         (remote-origin (magit-git-string "config" "remote.origin.url"))
         (behind (string-to-number
                  (or (ignore-errors (magit-git-string "rev-list" "--count" "HEAD..@{u}"))
                      "0")))
         (ahead (string-to-number
                 (or (ignore-errors (magit-git-string "rev-list" "--count" "@{u}..HEAD"))
                     "0")))
         (porcelain-lines (magit-git-lines "status" "--porcelain"))
         (classified (magit-dash--classify-porcelain-status path porcelain-lines))
         (dirty (not (null porcelain-lines)))
         (uncommitted-files (when dirty porcelain-lines))
         (recent-log (mapconcat #'identity
                                (magit-git-lines "log" "--oneline" "-10")
                                "\n"))
         (commit-ts (or (ignore-errors (magit-git-string "log" "-1" "--format=%ct")) ""))
         (stats (list :branch branch
                      :remote-origin remote-origin
                      :behind behind
                      :ahead ahead
                      :dirty dirty
                      :commit-ts (if (string-empty-p commit-ts) 0 (string-to-number commit-ts))
                      :uncommitted (plist-get classified :uncommitted)
                      :untracked (plist-get classified :untracked)
                      :submodule-dirty (plist-get classified :submodule-dirty)
                      :uncommitted-files uncommitted-files
                      :fetch-age (magit-dash--fetch-age path)
                      :updated-age (magit-dash--commit-timestamp-to-age commit-ts)
                      :head-hash (magit-dash--head-hash path)
                      :recent-log recent-log)))
    (magit-dash-gh--cache-set path :stats stats)
    stats))

(defun magit-dash--collect-stats-async (repo callback)
  "Collect git stats for REPO asynchronously; call CALLBACK with a stats plist.
Runs the git subcommands sequentially via `magit-dash--run-git',
accumulating their outputs before assembling the stats plist."
  (let* ((path (magit-dash-repo-path repo))
         (token (magit-dash-gh--cache-request path :stats))
         (commands (list '("branch" "--show-current")
                         '("remote" "get-url" "origin")
                         '("rev-list" "--count" "HEAD..@{u}")
                         '("rev-list" "--count" "@{u}..HEAD")
                         '("status" "--porcelain")
                         '("log" "--oneline" "-10")
                         '("log" "-1" "--format=%ct")))
	 outputs run)
    (setq run
          (lambda (remaining)
            (if (null remaining)
                (let* ((branch (string-trim (or (nth 0 outputs) "")))
                       (raw-origin (string-trim (or (nth 1 outputs) "")))
                       (remote-origin (unless (string-empty-p raw-origin) raw-origin))
                       (behind-str (string-trim (or (nth 2 outputs) "0")))
                       (behind (string-to-number
                                (if (string-empty-p behind-str) "0" behind-str)))
                       (ahead-str (string-trim (or (nth 3 outputs) "0")))
                       (ahead (string-to-number
                               (if (string-empty-p ahead-str) "0" ahead-str)))
                       (porcelain (or (nth 4 outputs) ""))
                       (porcelain-lines (seq-remove #'string-empty-p
                                                    (split-string porcelain "\n")))
                       (classified (magit-dash--classify-porcelain-status path porcelain-lines))
                       (dirty (not (null porcelain-lines)))
                       (uncommitted-files (when dirty porcelain-lines))
                       (recent-log (string-trim (or (nth 5 outputs) "")))
                       (commit-ts (string-trim (or (nth 6 outputs) "")))
                       (stats (list :branch branch
                                    :remote-origin remote-origin
                                    :behind behind
                                    :ahead ahead
                                    :dirty dirty
                                    :commit-ts (if (string-empty-p commit-ts) 0 (string-to-number commit-ts))
                                    :uncommitted (plist-get classified :uncommitted)
                                    :untracked (plist-get classified :untracked)
                                    :submodule-dirty (plist-get classified :submodule-dirty)
                                    :uncommitted-files uncommitted-files
                                    :fetch-age (magit-dash--fetch-age path)
                                    :updated-age (magit-dash--commit-timestamp-to-age commit-ts)
                                    :head-hash (magit-dash--head-hash path)
                                    :recent-log recent-log)))
                  (magit-dash-gh--cache-set-if-current path :stats stats token)
                  (funcall callback stats))
              (magit-dash--run-git
               path (car remaining)
               (lambda (output)
                 (setq outputs (append outputs (list output)))
                 (funcall run (cdr remaining)))
               (lambda (_ _)
                 (setq outputs (append outputs (list "")))
                 (funcall run (cdr remaining)))))))
    (funcall run commands)))



(defun magit-dash--get-stats (repo)
  "Return cached stats for REPO, collecting synchronously if absent or stale.
The cache is invalidated when the HEAD commit hash changes.
For missing submodules, returns minimal placeholder stats."
  (let* ((path (magit-dash-repo-path repo))
         (cached (magit-dash-gh--cache-get path :stats)))
    (cond
     ;; Missing submodules get placeholder stats
     ((eq (magit-dash-repo-submodule repo) 'missing)
      (list :branch "" :remote-origin nil :behind 0 :ahead 0
            :dirty nil :uncommitted-files nil :fetch-age nil
            :head-hash nil :recent-log ""))
     ;; Use cached if valid
     ((and cached
           (equal (magit-dash--head-hash path)
                  (plist-get cached :head-hash)))
      cached)
     ;; Otherwise collect fresh stats
     (t (condition-case err
            (magit-dash--collect-stats repo)
          (error
           (message "magit-dash: failed to collect stats for %s: %s"
                    (magit-dash-repo-name repo) (error-message-string err))
           ;; Return minimal stats on error
           (list :branch "?" :remote-origin nil :behind 0 :ahead 0
                 :dirty nil :uncommitted-files nil :fetch-age nil
                 :head-hash nil :recent-log "")))))))

(defun magit-dash--get-stats-fast (repo)
  "Return cached stats for REPO without validity checking, or a placeholder.
Unlike `magit-dash--get-stats', never blocks: returns whatever is in cache, or
a placeholder plist when nothing is cached.  Caller is responsible for async
collection."
  (cond
   ((eq (magit-dash-repo-submodule repo) 'missing)
    (list :branch "" :remote-origin nil :behind 0 :ahead 0
          :dirty nil :uncommitted-files nil :fetch-age nil
          :head-hash nil :recent-log ""))
   (t
    (or (magit-dash-gh--cache-get (magit-dash-repo-path repo) :stats)
        (list :branch "…" :remote-origin nil :behind 0 :ahead 0
              :dirty nil :uncommitted-files nil :fetch-age nil
              :head-hash nil :recent-log "")))))

;;;; Async git operations

(defun magit-dash--run-git (path args on-success &optional on-error)
  "Run git ARGS in PATH asynchronously using magit's configured git executable.
ON-SUCCESS is called with right-trimmed stdout on exit 0.
ON-ERROR is called with stdout and exit-code on non-zero exit; defaults to a
message.  When PATH no longer exists on disk (for example a worktree removed
outside magit-dash), ON-ERROR is called directly instead of letting
`make-process' signal — callers only ever expect to fail through the callback,
not via a thrown error."
  (if (not (file-directory-p path))
      (let ((msg (format "no such directory: %s" path)))
        (if on-error
            (funcall on-error msg 1)
          (message "magit-dash: git %s failed: %s" (car args) msg)))
    (let* ((default-directory path)
           (proc-buf (generate-new-buffer " *magit-dash-gh-git*"))
           (proc (make-process
                  :name "magit-dash-gh-git"
                  :buffer proc-buf
                  :command (cons magit-git-executable args)
                  :connection-type 'pipe
                  :noquery t
                  :sentinel
                  (lambda (proc _event)
                    (when (memq (process-status proc) '(exit signal))
                      (let ((output (with-current-buffer (process-buffer proc)
                                      (string-trim-right (buffer-string))))
                            (code (process-exit-status proc)))
                        (kill-buffer (process-buffer proc))
                        (if (= code 0)
                            (funcall on-success output)
                          (if on-error
                              (funcall on-error output code)
                            (message "magit-dash: git %s failed (%d): %s"
                                     (car args) code output)))))))))
      (with-current-buffer proc-buf
        (setq default-directory path))
      (when-let* ((active-op (gethash (expand-file-name path) magit-dash--active-operations)))
        (setf (magit-dash-operation-process active-op)
              (cons proc (seq-filter #'process-live-p
                                     (magit-dash-operation-process active-op)))))
      proc)))

(defun magit-dash--fetch-async (repo on-complete)
  "Run git fetch for REPO asynchronously.
Calls ON-COMPLETE with symbol `ok' on success or `error' and error text
on failure. When `magit-dash--repo-pull-submodules-on-fetch-p' is non-nil,
also pulls/updates submodules on successful fetch."
  (magit-dash--record-fetch-attempt repo)
  (magit-dash--run-git
   (magit-dash-repo-path repo)
   '("fetch")
   (lambda (_)
     (if (magit-dash--repo-pull-submodules-on-fetch-p repo)
         (magit-dash--run-git
          (magit-dash-repo-path repo)
          '("submodule" "update" "--init" "--recursive")
          (lambda (_) (funcall on-complete 'ok))
          (lambda (output code)
            (funcall on-complete 'error (format "submodule update exit %d: %s" code output))))
       (funcall on-complete 'ok)))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-dash--pull-async (repo on-complete)
  "Run git pull for REPO asynchronously.
Calls ON-COMPLETE with symbol `ok' on success or `error' and error text
on failure."
  (magit-dash--run-git
   (magit-dash-repo-path repo)
   '("pull")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-dash--submodule-update-async (repo on-complete)
  "Run git submodule update --init --recursive for REPO asynchronously.
Calls ON-COMPLETE with `ok' on success or `error' with message on failure."
  (magit-dash--run-git
   (magit-dash-repo-path repo)
   '("submodule" "update" "--init" "--recursive")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-dash--submodule-pull-async (repo on-complete)
  "Run git submodule foreach git pull for REPO asynchronously.
Calls ON-COMPLETE with `ok' on success or `error' with message on failure."
  (magit-dash--run-git
   (magit-dash-repo-path repo)
   '("submodule" "foreach" "git" "pull")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "submodule pull exit %d: %s" code output)))))

(defun magit-dash--repo-pushable-p (repo)
  "Return non-nil when REPO can be pushed.
Skips repos without commits ahead, without branches configured, or
in detached HEAD state."
  (let* ((path (magit-dash-repo-path repo))
         (stats (when (and path (fboundp 'magit-dash-gh--cache-get))
                  (magit-dash-gh--cache-get path :stats)))
         (branch (and stats (plist-get stats :branch)))
         (ahead (and stats (plist-get stats :ahead))))
    (if (null stats)
        t
      (and (stringp branch)
           (not (string-empty-p branch))
           (not (string= branch "?"))
           (not (string= branch "…"))
           (numberp ahead)
           (> ahead 0)))))

(defun magit-dash--repo-ci-failing-p (repo)
  "Return non-nil if REPO has cached failing CI status."
  (let* ((path (magit-dash-repo-path repo))
         (ci (when (fboundp 'magit-dash-gh--cache-get)
               (magit-dash-gh--cache-get path :ci-status)))
         (conclusion (plist-get ci :conclusion)))
    (and conclusion
         (or (and (fboundp 'magit-dash-gh-ci--failure-p)
                  (magit-dash-gh-ci--failure-p conclusion))
             (member conclusion '("failure" "timed_out" "startup_failure"))))))

(defun magit-dash--repo-pending-status-p (repo)
  "Return non-nil if REPO has pending status items (dirty, untracked, ahead, or behind)."
  (let* ((stats (or (magit-dash--get-stats-fast repo)
                    (and (fboundp 'magit-dash-gh--cache-get)
                         (magit-dash-gh--cache-get (magit-dash-repo-path repo) :stats))))
         (ahead (or (plist-get stats :ahead) 0))
         (behind (or (plist-get stats :behind) 0))
         (dirty (plist-get stats :dirty))
         (untracked (plist-get stats :untracked))
         (uncommitted (plist-get stats :uncommitted))
         (submodule-dirty (plist-get stats :submodule-dirty)))
    (or (> ahead 0)
        (> behind 0)
        dirty
        untracked
        uncommitted
        submodule-dirty)))

(defun magit-dash--repo-recent-timestamp (repo)
  "Return numeric timestamp for REPO's most recent change."
  (let* ((path (magit-dash-repo-path repo))
         (stats (or (magit-dash--get-stats-fast repo)
                    (and (fboundp 'magit-dash-gh--cache-get)
                         (magit-dash-gh--cache-get path :stats))))
         (ts (plist-get stats :commit-ts)))
    (cond
     ((and (numberp ts) (> ts 0)) ts)
     ((and path (file-directory-p path))
      (let* ((git-dir (expand-file-name ".git" path))
             (target (if (file-directory-p git-dir) git-dir path)))
        (float-time (file-attribute-modification-time (file-attributes target)))))
     (t 0))))

(defun magit-dash--repo-priority-score (repo)
  "Return priority tier for REPO: 0 (failing CI), 1 (pending status), 2 (other)."
  (cond
   ((magit-dash--repo-ci-failing-p repo) 0)
   ((magit-dash--repo-pending-status-p repo) 1)
   (t 2)))

(defun magit-dash--sort-repos-by-priority (repos)
  "Sort REPOS by priority:
1. Failing CI
2. Pending items in status (dirty, ahead, behind, untracked)
3. Recently changed (newest first)."
  (sort (copy-sequence repos)
        (lambda (a b)
          (let ((score-a (magit-dash--repo-priority-score a))
                (score-b (magit-dash--repo-priority-score b)))
            (if (/= score-a score-b)
                (< score-a score-b)
              (> (magit-dash--repo-recent-timestamp a)
                 (magit-dash--repo-recent-timestamp b)))))))

(defun magit-dash--push-async (repo on-complete)
  "Run git push for REPO asynchronously.
Calls ON-COMPLETE with symbol `ok' on success or `error' and error text
on failure."
  (magit-dash--run-git
   (magit-dash-repo-path repo)
   '("push")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-dash--auto-commit-async (repo on-complete)
  "Stage and commit modified files in REPO using its auto-commit message.
Calls ON-COMPLETE with `ok' when committed, `skipped' when workdir is clean,
or `error' when git add or commit fails."
  (let* ((path (magit-dash-repo-path repo))
         (msg (magit-dash--commit-message-for-repo repo)))
    (magit-dash--run-git
     path '("status" "--porcelain")
     (lambda (porcelain)
       (if (string-empty-p porcelain)
           (funcall on-complete 'skipped)
         (magit-dash--run-git
          path '("add" "-A")
          (lambda (_)
            (magit-dash--run-git
             path (list "commit" "-m" msg)
             (lambda (_) (funcall on-complete 'ok))
             (lambda (output code)
               (funcall on-complete 'error (format "commit failed, exit %d: %s" code output)))))
          (lambda (output code)
            (funcall on-complete 'error (format "add failed, exit %d: %s" code output))))))
     (lambda (output code)
       (funcall on-complete 'error (format "status failed, exit %d: %s" code output))))))

(defun magit-dash--current-branch (path)
  "Return the current branch name for the repo at PATH synchronously.
Returns \"\" when PATH no longer exists on disk (for example a worktree
removed outside magit-dash) instead of letting `magit-git-string' signal."
  (if (not (file-directory-p path))
      ""
    (let ((default-directory path))
      (string-trim (or (magit-git-string "branch" "--show-current") "")))))

(defun magit-dash--branch-allowed-p (repo)
  "Return current branch name if allowed by REPO's sync-branches, else nil.
When sync-branches is nil any branch is allowed and the current branch
is returned."
  (let* ((allowed (magit-dash-repo-sync-branches repo))
         (current (magit-dash--current-branch (magit-dash-repo-path repo))))
    (if (null allowed)
        current
      (and (member current allowed) current))))

(defun magit-dash--auto-fetch-async (repo on-complete)
  "Run git fetch --all for REPO asynchronously.
Calls ON-COMPLETE with `ok' on success or `error' and error text on failure."
  (magit-dash--record-fetch-attempt repo)
  (magit-dash--run-git
   (magit-dash-repo-path repo)
   '("fetch" "--all")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-dash--auto-pull-async (repo on-complete)
  "Run git pull for REPO if current branch is in sync-branches.
Calls ON-COMPLETE with `ok', `skipped' (branch not allowed), or `error'."
  (if-let* ((branch (magit-dash--branch-allowed-p repo)))
      (magit-dash--run-git
       (magit-dash-repo-path repo)
       '("pull")
       (lambda (_) (funcall on-complete 'ok))
       (lambda (output code)
         (funcall on-complete 'error (format "exit %d: %s" code output))))
    (funcall on-complete 'skipped
             (format "branch %s not in sync-branches"
                     (magit-dash--current-branch
                      (magit-dash-repo-path repo))))))

(defun magit-dash--run-git-chain (path steps on-success on-complete)
  "Execute a chain of git commands in PATH.
STEPS is a list of (ARGS . LABEL) pairs.  On success of all steps call
ON-SUCCESS with no args.  On any failure call ON-COMPLETE with `error'
and a message of the form \"LABEL failed, exit N: output\"."
  (if (null steps)
      (funcall on-success)
    (let* ((step (car steps))
           (args (car step))
           (label (cdr step)))
      (magit-dash--run-git
       path args
       (lambda (_)
         (magit-dash--run-git-chain
          path (cdr steps) on-success on-complete))
       (lambda (output code)
         (funcall on-complete 'error
                  (format "%s failed, exit %d: %s" label code output)))))))

(defun magit-dash--auto-push-async (repo on-complete)
  "Run git push for REPO if current branch is in sync-branches.
Calls ON-COMPLETE with `ok', `skipped' (branch not allowed), or `error'."
  (if-let* ((branch (magit-dash--branch-allowed-p repo)))
      (magit-dash--run-git
       (magit-dash-repo-path repo)
       '("push")
       (lambda (_) (funcall on-complete 'ok))
       (lambda (output code)
         (funcall on-complete 'error (format "exit %d: %s" code output))))
    (funcall on-complete 'skipped
             (format "branch %s not in sync-branches"
                     (magit-dash--current-branch
                      (magit-dash-repo-path repo))))))

(defun magit-dash--run-shell-string-async (cmd path on-complete)
  "Run shell command string CMD in PATH asynchronously.
Calls ON-COMPLETE with `ok' on exit 0, or `error' and output on failure."
  (let ((proc-buf (generate-new-buffer " *magit-dash-gh-cmd*")))
    (with-current-buffer proc-buf
      (setq default-directory path))
    (make-process
     :name "magit-dash-gh-cmd"
     :buffer proc-buf
     :command (list shell-file-name shell-command-switch cmd)
     :connection-type 'pipe
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((output (with-current-buffer (process-buffer proc)
                         (string-trim-right (buffer-string))))
               (code (process-exit-status proc)))
           (kill-buffer (process-buffer proc))
           (if (= code 0)
               (funcall on-complete 'ok)
             (funcall on-complete 'error (format "exit %d: %s" code output)))))))))

(cl-defun magit-dash-remote-sync-target (&key host path branch merge-method)
  "Return a shell command string implementing the remote sync pipeline for HOST.
Stages, fetches/rebases (or merges when MERGE-METHOD is `merge') against BRANCH
\(defaulting to current HEAD), commits, and pushes locally and remotely over SSH
into PATH (defaulting to \".\")."
  (let* ((host-str (if (symbolp host) (symbol-name host) (or host "")))
         (target-path (if (and path (not (string-empty-p path))) path "."))
         (branch-name (when branch (if (symbolp branch) (symbol-name branch) branch)))
         (method-sym (if (stringp merge-method) (intern merge-method) (or merge-method 'rebase)))
         (op (if (eq method-sym 'merge) "merge" "rebase"))
         (ref (if (and branch-name (not (string-empty-p branch-name)))
                  (if (string-prefix-p "origin/" branch-name)
                      branch-name
                    (format "origin/%s" branch-name))
                "origin/$(git rev-parse --abbrev-ref HEAD)"))
         (remote-cmd (format "git add -A && git fetch origin && git %s %s && git commit -a -m \"auto-update: (sync.REMOTE(%s))\"; git push"
                             op ref host-str))
         (ssh-cmd (format "ssh %s 'cd %s && %s'" host-str target-path remote-cmd))
         (local-stage-sync-push (format "git add -A && git fetch origin && git %s %s && git ls-files -d | xargs -r git rm --ignore-unmatch --quiet -- && git add -A && (git commit -a -m \"update: (sync.REMOTE(%s))\" || true) && git push"
                                       op ref host-str))
         (local-finish (format "git fetch origin && git %s %s" op ref)))
    (format "%s && %s && %s && %s" ssh-cmd local-stage-sync-push ssh-cmd local-finish)))

(defun magit-dash--run-target (repo target on-complete)
  "Run TARGET for REPO, calling ON-COMPLETE with `ok', `skipped', or `error'.
The single execution primitive for hook targets, global hook targets, and
`:operation' overrides.  Dispatches by TARGET's type:
  symbol   — looked up as a label in REPO's :commands; runs its target
             (string or function), recursively.
  string   — executed as a shell command in REPO's directory.
  function — called with (REPO ON-COMPLETE)."
  (let* ((path (magit-dash-repo-path repo))
         (default-directory (file-name-as-directory (expand-file-name path))))
    (cond
     ((and (symbolp target) target (not (functionp target)))
      (if-let* ((entry (seq-find (lambda (c) (eq (car c) target))
                                 (magit-dash-repo-commands repo)))
                (resolved (cdr entry)))
          (let ((target-cmd (car (magit-dash--normalize-command-op resolved))))
            (magit-dash--run-target repo target-cmd on-complete))
        (funcall on-complete 'error
                 (format "command %s not found in :commands" target))))
     ((stringp target)
      (magit-dash--run-shell-string-async target path on-complete))
     ((functionp target)
      (let* ((arity (func-arity target))
             (max-args (cdr arity)))
        (cond
         ((and (numberp max-args) (= max-args 0))
          (funcall target)
          (funcall on-complete 'ok))
         ((and (numberp max-args) (= max-args 1))
          (funcall target repo)
          (funcall on-complete 'ok))
         (t
          (funcall target repo on-complete)))))
     (t
      (funcall on-complete 'error "target must be a symbol, string, or function")))))

(defun magit-dash--hook-slot (hooks op slot)
  "Return SLOT (`:pre'/`:post'/`:operation') of OP's plist within HOOKS."
  (plist-get (plist-get hooks op) slot))

(defun magit-dash--repo-operation (repo op)
  "Return REPO's `:operation' override for OP, or nil for the default."
  (magit-dash--hook-slot (magit-dash-repo-hooks repo) op :operation))

(defun magit-dash--hooks-for (repo op phase)
  "Return the ordered target list for REPO's OP at PHASE (`:pre' or `:post').
Global hooks run outermost: before repo hooks for `:pre', after repo hooks
for `:post'."
  (let ((global (magit-dash--hook-slot magit-dash-global-hooks op phase))
        (local (magit-dash--hook-slot (magit-dash-repo-hooks repo) op phase)))
    (if (eq phase :pre)
        (append global local)
      (append local global))))

(defun magit-dash--run-hook-chain (repo phase targets on-complete)
  "Run TARGETS for REPO serially via `magit-dash--run-target'.
At PHASE `:pre', stops and reports `error' on the first failing target.
At PHASE `:post', runs every target regardless of failures (logged, not
propagated) and always reports `ok'."
  (if (null targets)
      (funcall on-complete 'ok)
    (magit-dash--run-target
     repo (car targets)
     (lambda (status &optional text)
       (when (eq status 'error)
         (magit-dash--log-operation (magit-dash-repo-name repo)
                                     (format "%s hook" (symbol-name phase))
                                     status text))
       (if (and (eq phase :pre) (eq status 'error))
           (funcall on-complete 'error text)
         (magit-dash--run-hook-chain repo phase (cdr targets) on-complete))))))

(defun magit-dash--run-hooks-for (repo op phase on-complete)
  "Run hooks for REPO's OP at PHASE, then call ON-COMPLETE.
Run global then repo hooks (or repo then global for `:post') with the
aggregate status."
  (magit-dash--run-hook-chain repo phase (magit-dash--hooks-for repo op phase) on-complete))

(defun magit-dash--default-sync-async (repo on-complete)
  "Run safe default sync (fetch, safe pull, safe push) for REPO.
Calls ON-COMPLETE with `ok', `skipped', or `error'."
  (if (not (magit-dash--branch-allowed-p repo))
      (funcall on-complete 'skipped
               (format "branch %s not in sync-branches"
                       (magit-dash--current-branch (magit-dash-repo-path repo))))
    (let ((path (magit-dash-repo-path repo)))
      (magit-dash--fetch-async
       repo
       (lambda (status &optional err)
         (if (eq status 'error)
             (funcall on-complete 'error (format "fetch failed: %s" err))
           (magit-dash--run-git
            path '("status" "--porcelain" "-uno")
            (lambda (dirty-tracked)
              (let ((clean-tracked (string-empty-p dirty-tracked)))
                (magit-dash--run-git
                 path '("rev-parse" "--abbrev-ref" "@{upstream}")
                 (lambda (upstream)
                   (if (string-empty-p upstream)
                       (funcall on-complete 'skipped "no upstream branch configured")
                     (magit-dash--default-sync-pull-push repo path clean-tracked on-complete)))
                 (lambda (&rest _)
                   (funcall on-complete 'skipped "no remote tracking branch")))))
            (lambda (err-msg code)
              (funcall on-complete 'error (format "git status failed (%d): %s" code err-msg))))))))))

(defun magit-dash--default-sync-pull-push (repo _path clean-tracked on-complete)
  "Execute pull and push steps for REPO when CLEAN-TRACKED is non-nil."
  (let ((do-pull (lambda (then-push)
                   (if clean-tracked
                       (magit-dash--pull-async
                        repo
                        (lambda (p-status &optional p-err)
                          (if (eq p-status 'error)
                              (funcall on-complete 'error (format "pull failed: %s" p-err))
                            (funcall then-push))))
                     (funcall then-push))))
        (do-push (lambda ()
                   (if clean-tracked
                       (magit-dash--push-async repo on-complete)
                     (funcall on-complete 'ok)))))
    (funcall do-pull do-push)))

(defun magit-dash--default-operation (op)
  "Return the default implementation function for OP."
  (pcase op
    (:fetch  #'magit-dash--auto-fetch-async)
    (:pull   #'magit-dash--auto-pull-async)
    ((or :stage :stage-all) #'magit-dash--stage-all-async)
    (:commit #'magit-dash--auto-commit-async)
    (:push   #'magit-dash--auto-push-async)
    (:default-sync #'magit-dash--default-sync-async)
    (:sync   #'magit-dash--auto-sync-pipeline-async)))
(defun magit-dash--resolve-operation (repo op)
  "Return the (REPO ON-COMPLETE) function that implements OP for REPO:
REPO's `:operation' override when set, otherwise OP's default implementation."
  (if-let* ((target (magit-dash--repo-operation repo op)))
      (lambda (r cb) (magit-dash--run-target r target cb))
    (magit-dash--default-operation op)))

(defun magit-dash--run-operation (repo op on-complete)
  "Resolve and run OP for REPO, wrapped by OP's `:pre'/`:post' hooks.
Calls ON-COMPLETE with `ok', `skipped', or `error' [and message text].
A `:pre' hook failure skips the operation (reported as `skipped'); `:post'
hooks never run after an `error' and their own failures never change the
status passed to ON-COMPLETE."
  (magit-dash--run-hooks-for
   repo op :pre
   (lambda (pre-status &optional pre-text)
     (if (eq pre-status 'error)
         (funcall on-complete 'skipped (or pre-text (format "%s pre-hook skipped operation" op)))
       (funcall (magit-dash--resolve-operation repo op)
                repo
                (lambda (status &optional text)
                  (if (eq status 'error)
                      (funcall on-complete 'error text)
                    (magit-dash--run-hooks-for
                     repo op :post
                     (lambda (&rest _) (funcall on-complete status text))))))))))

(defun magit-dash--auto-sync-steps (repo)
  "Return an ordered list of (LABEL . FN) pairs for REPO's configured auto ops.
Steps are: fetch (when :auto-fetch or :auto-pull), pull (when :auto-pull),
commit (when :auto-commit), push (when :auto-push)."
  (seq-filter
   #'identity
   (list
    (when (or (magit-dash-repo-auto-fetch repo) (magit-dash-repo-auto-pull repo))
      (cons "fetch" (lambda (cb) (magit-dash--auto-fetch-async repo cb))))
    (when (magit-dash-repo-auto-pull repo)
      (cons "pull" (lambda (cb) (magit-dash--auto-pull-async repo cb))))
    (when (magit-dash-repo-auto-commit repo)
      (cons "commit" (lambda (cb) (magit-dash--auto-commit-async repo cb))))
    (when (magit-dash-repo-auto-push repo)
      (cons "push" (lambda (cb) (magit-dash--auto-push-async repo cb)))))))

(defun magit-dash--has-sync-configured-p (repo)
  "Return non-nil when REPO has any sync operation configured.
This checks for default pipeline steps (fetch/pull/commit/push) or a
`:hooks' `:sync' `:operation' override."
  (or (magit-dash--auto-sync-steps repo)
      (magit-dash--repo-operation repo :sync)))

(defun magit-dash--auto-sync-op-symbols (repo)
  "Return an ordered list of operation keywords configured for REPO.
These are the operations for the default `:sync' pipeline: `:fetch'/`:pull'
\(pull implies fetch), `:commit', `:push' — each only when its own auto-*
flag is set."
  (seq-filter
   #'identity
   (list
    (when (or (magit-dash-repo-auto-fetch repo) (magit-dash-repo-auto-pull repo)) :fetch)
    (when (magit-dash-repo-auto-pull repo) :pull)
    (when (magit-dash-repo-auto-commit repo) :commit)
    (when (magit-dash-repo-auto-push repo) :push))))

(defun magit-dash--run-op-chain (repo ops on-complete)
  "Run OPS sequentially for REPO in order via `magit-dash--run-operation'.
OPS is a list of operation keywords.  Log each operation.  Abort on `error';
continue on `ok' or `skipped'.  Call ON-COMPLETE with `ok' after all ops,
or `error' on first failure."
  (if (null ops)
      (funcall on-complete 'ok)
    (magit-dash--run-operation
     repo (car ops)
     (lambda (status &optional text)
       (magit-dash--log-operation (magit-dash-repo-name repo) (symbol-name (car ops)) status text)
       (pcase status
         ('error (funcall on-complete 'error text))
         (_ (magit-dash--run-op-chain repo (cdr ops) on-complete)))))))

(defun magit-dash--auto-sync-pipeline-async (repo on-complete)
  "Run default `:sync' pipeline for REPO.
This runs fetch/pull/commit/push per REPO's auto-* flags, in order, each
resolved and wrapped through its own `:hooks' entry.  Call ON-COMPLETE with
`ok', `skipped', or `error'."
  (let ((ops (magit-dash--auto-sync-op-symbols repo)))
    (if (null ops)
        (funcall on-complete 'skipped "no auto operations configured")
      (magit-dash--run-op-chain repo ops on-complete))))

(defun magit-dash--auto-sync-async (repo on-complete)
  "Run REPO's configured `:sync' operation with operation guarding.
This executes its `:hooks' `:sync' `:operation' override (which replaces
the default pipeline entirely, including its own fetch/pull/commit/push
settings) or, absent that, the default pipeline of fetch/pull/commit/push
per REPO's auto-* flags — wrapped by `:sync''s own `:pre'/`:post' hooks.
Call ON-COMPLETE with `ok', `skipped', or `error'."
  (magit-dash--record-sync-attempt repo)
  (magit-dash--guarded-op-run
   repo "sync"
   (lambda (r cb)
     (magit-dash--run-operation r :sync cb))
   on-complete))

(defun magit-dash--log-operation (repo-name operation status &optional error-text)
  "Log REPO-NAME OPERATION with STATUS to *Messages*.
The current timestamp is attached as a tooltip (help-echo) on REPO-NAME.
When ERROR-TEXT is non-nil it is appended to the message."
  (let* ((ts (format-time-string "%Y-%m-%d %H:%M:%S"))
         (name (propertize repo-name 'face 'bold 'help-echo ts))
         (detail (if error-text (format " — %s" error-text) "")))
    (message "magit-dash: %s %s → %s%s" name operation (symbol-name status) detail)))

(defun magit-dash--live-dash-buffers ()
  "Return all live buffers that are in `magit-dash-mode' or named `magit-dash-buffer-name'."
  (let ((default-buf (get-buffer magit-dash-buffer-name)))
    (delete-dups
     (delq nil
           (cons (and default-buf (buffer-live-p default-buf) default-buf)
                 (seq-filter (lambda (b)
                               (and (buffer-live-p b)
                                    (with-current-buffer b
                                      (derived-mode-p 'magit-dash-mode))))
                             (buffer-list)))))))

(defun magit-dash--batch-run (repos op-fn label &optional on-all-done)
  "Run OP-FN asynchronously on each repo in REPOS.
OP-FN is called as (op-fn REPO CALLBACK) where CALLBACK receives a status
symbol: `ok', `skipped', or `error'.  OP-FN may also fail synchronously (for
example a deleted worktree directory makes `make-process' signal before any
async work starts); that is caught per-repo so one bad repo cannot stop the
rest of the batch from running.
When all repos finish, display a LABEL summary message and optionally call
ON-ALL-DONE with an alist of (NAME . STATUS)."
  (let* ((remaining (list (length repos)))
         (results nil))
    (message "magit-dash: starting %s batch operation" label)
    (seq-do
     (lambda (repo)
       (let ((callback
              (lambda (status &optional error-text)
                (magit-dash--log-operation
                 (magit-dash-repo-name repo) label status error-text)
                (push (cons (magit-dash-repo-name repo) status) results)
                (seq-do
                 (lambda (b)
                   (when (buffer-live-p b)
                     (with-current-buffer b
                       (magit-dash-refresh-repo repo nil t))))
                 (magit-dash--live-dash-buffers))
                (setcar remaining (1- (car remaining)))
                (when (= 0 (car remaining))
                  (let* ((ok (seq-count (lambda (r) (eq 'ok (cdr r))) results))
                         (skipped (seq-count (lambda (r) (eq 'skipped (cdr r))) results))
                         (errors (seq-filter (lambda (r) (eq 'error (cdr r))) results)))
                    (message "%s: %d ok%s%s" label ok
                             (if (> skipped 0) (format ", %d skipped" skipped) "")
                             (if errors
                                 (format ", %d failed (%s)"
                                         (length errors)
                                         (mapconcat #'car errors ", "))
                               ""))
                    (message "magit-dash: ending %s batch operation" label)
                    (when on-all-done
                      (funcall on-all-done results)))))))
         (condition-case err
             (magit-dash--with-repo repo
               (magit-dash--guarded-op-run repo label op-fn callback))
           (error (funcall callback 'error (error-message-string err))))))
     repos)))

(defun magit-dash--maybe-refresh (&optional target)
  "Refresh the repo dashboard buffer if it is currently live.
When TARGET is non-nil (a `magit-dash-repo' struct, a list of repos,
or a path), refresh only that target row or rows in-place.
When TARGET is nil, performs a full dashboard refresh."
  (when-let* ((buf (get-buffer magit-dash-buffer-name))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (if target
          (magit-dash-refresh-repo target nil t)
        (magit-dash-refresh)))))

;;;; Cache management

(defvar magit-dash--repo-last-fetch-attempts (make-hash-table :test #'equal)
  "Hash table mapping expanded repo path to float-time of last fetch attempt.")

(defvar magit-dash--repo-last-sync-attempts (make-hash-table :test #'equal)
  "Hash table mapping expanded repo path to float-time of last sync attempt.")

(defun magit-dash-cache-info ()
  "Display cache statistics in the minibuffer."
  (interactive)
  (let* ((total (length magit-dash-repo-list))
         (discovered-wt (seq-count #'magit-dash-repo-worktree magit-dash-repo-list))
         (discovered-subm (seq-count (lambda (r)
                                       (and (magit-dash-repo-submodule r)
                                            (not (eq (magit-dash-repo-submodule r) 'missing))))
                                     magit-dash-repo-list))
         (configured (- total discovered-wt discovered-subm))
         (cached (hash-table-count magit-dash-gh--cache)))
    (message "Repos: %d configured + %d worktrees + %d submodules = %d tracked | Cache: %d entries"
             configured discovered-wt discovered-subm total cached)))

(defun magit-dash-cache-reset (&optional repo-path)
  "Reset cache for REPO-PATH (or all repos if nil)."
  (interactive)
  (if repo-path
      (progn
        (magit-dash-gh--cache-remove repo-path)
        (remhash (expand-file-name repo-path) magit-dash--repo-last-fetch-attempts)
        (remhash (expand-file-name repo-path) magit-dash--repo-last-sync-attempts)
        (message "Cleared cache for %s" repo-path))
    (clrhash magit-dash-gh--cache)
    (clrhash magit-dash-gh--cache-requests)
    (clrhash magit-dash--repo-last-fetch-attempts)
    (clrhash magit-dash--repo-last-sync-attempts)
    (message "Cleared entire cache")))

(defun magit-dash-cache-reset-all ()
  "Clear all caches and repopulate the dashboard asynchronously."
  (interactive)
  (clrhash magit-dash-gh--cache)
  (clrhash magit-dash-gh--cache-requests)
  (magit-dash--maybe-refresh))

(defun magit-dash-cache-reset-at-point ()
  "Clear cache for repository at point, re-collect stats, and refresh."
  (interactive)
  (when-let* ((repo (magit-dash--repo-at-point)))
    (let* ((path (magit-dash-repo-path repo))
           (stats (progn
                    (magit-dash-gh--cache-remove path)
                    (magit-dash--collect-stats repo))))
      (magit-dash-gh--cache-set path :stats stats)
      (magit-dash--maybe-refresh))))

(defun magit-dash-cache-diagnose ()
  "Report cache health for all registered repos.
Shows a one-line summary message and opens a detail buffer when issues
are found."
  (interactive)
  (let ((warnings 0)
        (errors 0)
        (lines nil))
    (seq-do
     (lambda (repo)
       (let* ((path (magit-dash-repo-path repo))
              (name (magit-dash-repo-name repo))
              (stats (magit-dash-gh--cache-get path :stats)))
         (cond
          ((null stats)
           (setq warnings (1+ warnings))
           (push (format "  WARNING %s: no stats cached" name) lines))
          ((not (plist-member stats :head-hash))
           (setq errors (1+ errors))
           (push (format "  ERROR %s: stats missing :head-hash field" name) lines)))))
     magit-dash-repo-list)
    (message "%d repo(s): %d warning(s), %d error(s)"
             (length magit-dash-repo-list) warnings errors)
    (when (or (> warnings 0) (> errors 0))
      (let ((buf (get-buffer-create "*magit-dash-cache-diagnose*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Cache diagnostics — %d warning(s), %d error(s)\n\n"
                            warnings errors))
            (seq-do (lambda (l) (insert l "\n")) (nreverse lines))))
        (pop-to-buffer buf)
        (view-mode 1)))))

(defun magit-dash-cache-stats ()
  "Show per-repository cache status in a read-only buffer."
  (interactive)
  (let ((buf (get-buffer-create "*magit-dash-cache-stats*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (seq-do
         (lambda (repo)
           (let* ((path (magit-dash-repo-path repo))
                  (stats (magit-dash-gh--cache-get path :stats))
                  (pr-counts (magit-dash-gh--cache-get path :pr-counts))
                  (ci (magit-dash-gh--cache-get path :ci-status)))
             (insert (format "Repository: %s\n" (magit-dash-repo-name repo)))
             (insert (format "  Stats cached: %s\n" (if stats "yes" "no")))
             (insert (format "  PR counts cached: %s\n" (if pr-counts "yes" "no")))
             (insert (format "  CI status cached: %s\n" (if ci "yes" "no")))
             (insert "\n")))
         magit-dash-repo-list)))
    (pop-to-buffer buf)
    (view-mode 1)))

;;;; Worktree support

(defun magit-dash--parse-worktrees (main-path lines)
  "Parse LINES from `git worktree list --porcelain' for repo at MAIN-PATH.
Returns a list of `magit-dash-repo' structs for additional worktrees.
The first block (the main worktree) is always skipped.
Each worktree's name uses the registered repo's :name (falling back to the
MAIN-PATH basename when unregistered) so custom names carry over, and each
worktree inherits the parent's :sort-hint and :include-ci so it always sorts
immediately after its parent and gets its own CI status fetched."
  (let* ((main-repo (seq-find (lambda (r) (equal (magit-dash-repo-path r) main-path))
                              magit-dash-repo-list))
         (main-name (if main-repo
                        (magit-dash-repo-name main-repo)
                      (file-name-nondirectory (directory-file-name main-path))))
         (sort-hint (and main-repo (magit-dash-repo-sort-hint main-repo)))
         (include-ci (and main-repo (magit-dash-repo-include-ci main-repo)))
         (sync-on-refresh (and main-repo (magit-dash-repo-sync-on-refresh main-repo)))
         (fetch-on-refresh (and main-repo (magit-dash-repo-fetch-on-refresh main-repo)))
         blocks current)
    (seq-do (lambda (line)
              (if (string-empty-p line)
                  (progn
                    (when current (push (nreverse current) blocks))
                    (setq current nil))
                (push line current)))
            lines)
    (when current (push (nreverse current) blocks))
    (thread-last (cdr (nreverse blocks))
      (seq-map
       (lambda (block)
         (let* ((path-line (seq-find (lambda (l) (string-prefix-p "worktree " l)) block))
                (branch-line (seq-find (lambda (l) (string-prefix-p "branch " l)) block))
                (wt-path (when path-line (substring path-line 9)))
                (branch (when branch-line
                          (replace-regexp-in-string
                           "^refs/heads/" "" (substring branch-line 7)))))
           (when wt-path
             (magit-dash-repo--make
              :name (format "%s@%s" main-name (or branch "detached"))
              :path wt-path
              :worktree t
              :branch (or branch "detached")
              :sort-hint sort-hint
              :include-ci include-ci
              :sync-on-refresh sync-on-refresh
              :fetch-on-refresh fetch-on-refresh
              :parent main-path)))))
      (seq-remove #'null))))

(defun magit-dash--prune-and-list-worktrees (path)
  "Prune stale worktree entries for the repo at PATH and return the survivors.
Runs `git worktree prune' first so a worktree whose directory was deleted
outside of magit-dash (a plain `rm -rf' rather than `git worktree remove')
is forgotten by git, then parses `git worktree list --porcelain' and drops
any remaining entry whose path no longer exists on disk as a final guard."
  (let ((default-directory path))
    (ignore-errors
      (process-lines magit-git-executable "worktree" "prune")))
  (let* ((lines (ignore-errors
                  (let ((default-directory path))
                    (process-lines magit-git-executable
                                   "worktree" "list" "--porcelain"))))
         (found (when lines (magit-dash--parse-worktrees path lines))))
    (seq-filter (lambda (wt) (file-directory-p (magit-dash-repo-path wt))) found)))

(defun magit-dash--discover-worktrees ()
  "Populate the unified cache with worktrees for all registered main repos.
Auto-discovered worktrees that no longer exist on disk are pruned rather
than cached, so they stop appearing in the dashboard."
  (seq-do
   (lambda (repo)
     (unless (magit-dash-repo-worktree repo)
       (let ((path (magit-dash-repo-path repo)))
         (when (file-directory-p path)
           (magit-dash-gh--cache-set path :worktrees
                                     (magit-dash--prune-and-list-worktrees path))))))
   magit-dash-repo-list))

(defun magit-dash--parse-submodules (main-path lines)
  "Parse LINES from `git submodule status' for repo at MAIN-PATH.
Returns a list of `magit-dash-repo' structs, one per submodule
\(initialized or not).
Name is always \"parent<inner>\" where inner is the registered repo name
when the path matches a `magit-dash-repo-list' entry, otherwise the
submodule directory basename.
Missing/uninitialized submodules are marked with :submodule \\='missing."
  (let* ((main-repo (seq-find (lambda (r) (equal (magit-dash-repo-path r) main-path))
                              magit-dash-repo-list))
         (main-name (if main-repo
                        (magit-dash-repo-name main-repo)
                      (file-name-nondirectory (directory-file-name main-path)))))
    (thread-last lines
      (seq-filter (lambda (l) (not (string-empty-p l))))
      (seq-map
       (lambda (line)
         (when (string-match "^\\([-+U ]\\)\\([0-9a-f]+\\) \\([^ ]+\\)" line)
           (let* ((prefix (match-string 1 line))
                  (rel-path (match-string 3 line))
                  (abs-path (expand-file-name rel-path main-path))
                  (registered (seq-find (lambda (r) (equal (magit-dash-repo-path r) abs-path))
                                        magit-dash-repo-list))
                  (missing-p (or (string= prefix "-")
                                 (not (file-directory-p abs-path)))))
             (magit-dash-repo--make
              :name (format "%s<%s>" main-name
                           (if registered
                               (magit-dash-repo-name registered)
                             (file-name-nondirectory (directory-file-name rel-path))))
              :path abs-path
              :submodule (if missing-p 'missing t)
              :parent main-path)))))
      (seq-remove #'null))))

(defun magit-dash--rebuild-submodule-path-set ()
  "Rebuild `magit-dash--submodule-path-set' from cache."
  (setq magit-dash--submodule-path-set
        (let ((paths (make-hash-table :test #'equal)))
          (seq-do (lambda (repo)
                    (seq-do (lambda (sm)
                              (puthash (magit-dash-repo-path sm) (magit-dash-repo-name sm) paths))
                            (or (magit-dash-gh--cache-get (magit-dash-repo-path repo) :submodules) '())))
                  magit-dash-repo-list)
          paths)))

(defun magit-dash--discover-submodules (&optional callback)
  "Populate the unified cache with submodules for all registered main repos.
Executes `git submodule status' asynchronously via `magit-dash--run-git'
throttled by `gen-run-async' with `magit-dash-max-concurrent-jobs'.
When CALLBACK is non-nil, call it when discovery finishes.
Returns a cancellation handle."
  (let ((repos (thread-last
                 magit-dash-repo-list
                 (seq-remove (lambda (repo)
                               (or (magit-dash-repo-worktree repo)
                                   (magit-dash-repo-submodule repo))))
                 (seq-filter (lambda (repo)
                               (file-directory-p (magit-dash-repo-path repo)))))))
    (if (null repos)
        (progn
          (when callback (funcall callback))
          (lambda (&rest _) nil))
      (gen-run-async
       (lambda (repo done)
         (let ((path (magit-dash-repo-path repo)))
           (magit-dash--run-git
            path '("submodule" "status")
            (lambda (output)
              (let* ((lines (and output (split-string output "
" t)))
                     (submodules (when lines
                                   (magit-dash--parse-submodules path lines))))
                (magit-dash-gh--cache-set path :submodules submodules)
                (when (and magit-dash--submodule-path-set submodules)
                  (seq-do (lambda (sm)
                            (puthash (magit-dash-repo-path sm)
                                     (magit-dash-repo-name sm)
                                     magit-dash--submodule-path-set))
                          submodules)))
              (funcall done))
            (lambda (_output _code)
              (funcall done)))))
       repos
       :concurrency magit-dash-max-concurrent-jobs
       :delay magit-dash-queue-yield-delay
       :on-done callback))))

(defalias 'magit-dash--discover-submodules-async #'magit-dash--discover-submodules)


;;;; Column configuration

(defvar magit-dash-columns
  '((org . nil) (name . t) (branch . t) (fetched . t) (updated . nil) (freshness . nil) (ci . t) (status . t) (worktree . t) (sync . t) (cached . nil) (upstream . nil) (why . nil))
  "Alist of (COLUMN-SYMBOL . ENABLED) for the repository dashboard.
Persisted across sessions via `savehist-additional-variables'.")


(defconst magit-dash--all-columns
  '(org name branch fetched updated freshness ci status worktree sync cached upstream why)
  "All available dashboard columns in display order.")

(defconst magit-dash--column-defs
  '((org      . ("Org"      12 nil))
    (fetched  . ("Fetched"  8 nil))
    (updated  . ("Updated"  8 nil))
    (freshness . ("Fetched ⇒ Updated" 17 nil))
    (status   . ("Status"   8 nil))
    (worktree . ("Type"     8 nil))
    (sync     . ("Sync"     8 nil))
    (cached   . ("Cached"   7 nil))
    (ci       . ("CI"       3 nil))
    (upstream . ("Upstream" 16 nil))
    (why      . ("Why"      32 nil)))
  "Alist of COLUMN-SYMBOL to (LABEL WIDTH SORTABLE) for non-name columns.
Name and Branch widths are computed dynamically in `magit-dash--build-format'.")

(defun magit-dash--column-enabled-p (col)
  "Return non-nil when column COL is enabled in `magit-dash-columns'."
  (if-let* ((entry (assq col magit-dash-columns)))
      (cdr entry)
    nil))

(defun magit-dash--active-columns ()
  "Return column symbols that are currently enabled, in display order."
  (seq-filter #'magit-dash--column-enabled-p
              magit-dash--all-columns))

(defun magit-dash-toggle-column (col)
  "Toggle visibility of column COL in the dashboard and refresh."
  (interactive
   (list (intern (completing-read "Toggle column: "
                                  (seq-map #'symbol-name
                                           magit-dash--all-columns)
                                  nil t))))
  (setf (alist-get col magit-dash-columns)
        (not (magit-dash--column-enabled-p col)))
  (magit-dash-refresh))

(defvar magit-dash-show-discovered-submodules nil
  "When non-nil, auto-discovered submodules appear below their parent by default.")

(defvar magit-dash-show-discovered-worktrees t
  "When non-nil, auto-discovered worktrees appear below their parent by default.")

(defvar magit-dash--repo-worktrees-visibility (make-hash-table :test #'equal)
  "Session table mapping expanded repo path strings to worktree visibility boolean.")

(defvar magit-dash--repo-submodules-visibility (make-hash-table :test #'equal)
  "Session table mapping expanded repo path strings to submodule visibility boolean.")

(defun magit-dash--repo-show-worktrees-p (repo)
  "Return non-nil if auto-discovered worktrees should be shown for REPO."
  (let* ((path (expand-file-name (magit-dash-repo-path repo)))
         (session-val (gethash path magit-dash--repo-worktrees-visibility 'unset)))
    (if (not (eq session-val 'unset))
        session-val
      (let ((conf (magit-dash-repo-show-worktrees repo)))
        (cond
         ((eq conf t) t)
         ((null conf) nil)
         (t magit-dash-show-discovered-worktrees))))))

(defun magit-dash--repo-show-submodules-p (repo)
  "Return non-nil if auto-discovered submodules should be shown for REPO."
  (let* ((path (expand-file-name (magit-dash-repo-path repo)))
         (session-val (gethash path magit-dash--repo-submodules-visibility 'unset)))
    (if (not (eq session-val 'unset))
        session-val
      (let ((conf (magit-dash-repo-show-submodules repo)))
        (cond
         ((eq conf t) t)
         ((null conf) nil)
         (t magit-dash-show-discovered-submodules))))))

(defun magit-dash--target-repo-for-toggle (&optional repo)
  "Return REPO or the main repository associated with REPO or point."
  (let* ((target (or repo (magit-dash--repo-at-point)))
         (path (expand-file-name (magit-dash-repo-path target))))
    (if (not (magit-dash-repo-worktree target))
        target
      (let ((parent-path (magit-dash-repo-parent target)))
        (or (when parent-path
              (seq-find (lambda (r)
                          (equal (expand-file-name (magit-dash-repo-path r))
                                 (expand-file-name parent-path)))
                        magit-dash-repo-list))
            (seq-find (lambda (r)
                        (and (not (magit-dash-repo-worktree r))
                             (let ((wts (magit-dash-gh--cache-get (magit-dash-repo-path r) :worktrees)))
                               (seq-some (lambda (wt)
                                           (equal (expand-file-name (magit-dash-repo-path wt)) path))
                                         wts))))
                      magit-dash-repo-list)
            target)))))

(defun magit-dash-toggle-worktrees (&optional repo)
  "Toggle visibility of worktrees for REPO (defaults to repo at point).
When point is not on a repository, prompts for one using `annotated-completing-read'."
  (interactive)
  (let* ((target
          (cond
           (repo (magit-dash--target-repo-for-toggle repo))
           ((magit-dash--repo-at-point-p)
            (magit-dash--target-repo-for-toggle (magit-dash--repo-at-point)))
           (t
            (let* ((candidates
                    (seq-map (lambda (r)
                               (let* ((path (magit-dash-repo-path r))
                                      (status (if (magit-dash--repo-show-worktrees-p r)
                                                  "[worktrees shown] "
                                                "[worktrees hidden] "))
                                      (ann (concat status (magit-dash--repo-annotation r))))
                                 (cons (magit-dash-repo-name r) (cons ann r))))
                             (seq-filter (lambda (r) (not (magit-dash-repo-worktree r)))
                                         magit-dash-repo-list)))
                   (table (map-into candidates '(hash-table :test equal))))
              (annotated-completing-read
               table
               :prompt "Toggle worktrees for repo: "
               :require-match t
               :category 'magit-dash-repo))))))
    (when target
      (let* ((path (expand-file-name (magit-dash-repo-path target)))
             (current (magit-dash--repo-show-worktrees-p target))
             (new-state (not current)))
        (puthash path new-state magit-dash--repo-worktrees-visibility)
        (magit-dash-refresh)
        (message "magit-dash: worktrees %s for %s"
                 (if new-state "shown" "hidden")
                 (magit-dash-repo-name target))))))

(defalias 'magit-dash-toggle-repo-worktrees #'magit-dash-toggle-worktrees)
(defalias 'magit-dash-toggle-discovered-worktrees #'magit-dash-toggle-worktrees)

(defun magit-dash-toggle-submodules (&optional repo)
  "Toggle visibility of submodules for REPO (defaults to repo at point).
When point is not on a repository, prompts for one using `annotated-completing-read'."
  (interactive)
  (let* ((target
          (cond
           (repo (magit-dash--target-repo-for-toggle repo))
           ((magit-dash--repo-at-point-p)
            (magit-dash--target-repo-for-toggle (magit-dash--repo-at-point)))
           (t
            (let* ((candidates
                    (seq-map (lambda (r)
                               (let* ((path (magit-dash-repo-path r))
                                      (status (if (magit-dash--repo-show-submodules-p r)
                                                  "[submodules shown] "
                                                "[submodules hidden] "))
                                      (ann (concat status (magit-dash--repo-annotation r))))
                                 (cons (magit-dash-repo-name r) (cons ann r))))
                             (seq-filter (lambda (r) (not (magit-dash-repo-submodule r)))
                                         magit-dash-repo-list)))
                   (table (map-into candidates '(hash-table :test equal))))
              (annotated-completing-read
               table
               :prompt "Toggle submodules for repo: "
               :require-match t
               :category 'magit-dash-repo))))))
    (when target
      (let* ((path (expand-file-name (magit-dash-repo-path target)))
             (current (magit-dash--repo-show-submodules-p target))
             (new-state (not current)))
        (puthash path new-state magit-dash--repo-submodules-visibility)
        (magit-dash-refresh)
        (message "magit-dash: submodules %s for %s"
                 (if new-state "shown" "hidden")
                 (magit-dash-repo-name target))))))

(defalias 'magit-dash-toggle-repo-submodules #'magit-dash-toggle-submodules)
(defalias 'magit-dash-toggle-discovered-submodules #'magit-dash-toggle-submodules)


;;;; Formatting helpers

(defvar magit-dash-render-branch-name-as-basename t
  "When non-nil, the Branch column shows only the basename of a branch.
Everything up to and including the final \"/\" is dropped (e.g.
\"feature/some/thing\" displays as \"thing\").  Purely cosmetic — git
operations always use the full branch name.")

(defun magit-dash-toggle-branch-basename ()
  "Toggle whether the Branch column displays only a branch's basename."
  (interactive)
  (setq magit-dash-render-branch-name-as-basename (not magit-dash-render-branch-name-as-basename))
  (magit-dash-refresh))

(defun magit-dash--display-branch-name (branch)
  "Return BRANCH for display, trimmed to its basename when configured.
See `magit-dash-render-branch-name-as-basename'."
  (if (and magit-dash-render-branch-name-as-basename branch)
      (file-name-nondirectory branch)
    branch))

(defun magit-dash--format-age (seconds)
  "Format SECONDS duration as a compact string, or \"┄\" if nil."
  (cond
   ((null seconds) "┄")
   ((< seconds 60) (format "%ds" (round seconds)))
   ((< seconds 3600) (format "%dm" (round (/ seconds 60))))
   ((< seconds 86400) (format "%dh" (round (/ seconds 3600))))
   (t (format "%dd" (round (/ seconds 86400))))))

(defun magit-dash--format-freshness (stats)
  "Return \"<fetched> ⇒ <updated>\" ages from STATS as one string."
  (format "%s ⇒ %s"
          (magit-dash--format-age (plist-get stats :fetch-age))
          (magit-dash--format-age (plist-get stats :updated-age))))

(defun magit-dash--format-status (ahead behind dirty &optional untracked submodule-dirty is-pr)
  "Format AHEAD, BEHIND, and dirty states into a compact status indicator.
Each non-zero/non-nil value contributes a segment; segments are joined with
a single space.  Returns an empty string when everything is clean and synced.
DIRTY may be non-nil for uncommitted changes, or UNTRACKED and SUBMODULE-DIRTY
can distinguish untracked files (\"?\") and submodule updates (\"S\").
IS-PR when non-nil adds `magit-dash-status-pr-badge' (\"PR\")."
  (let ((parts nil))
    (when (> ahead 0)
      (push (propertize (format "↑%d" ahead) 'face 'warning) parts))
    (when (> behind 0)
      (push (propertize (format "↓%d" behind) 'face 'font-lock-comment-face) parts))
    (when (or (and (not untracked) (not submodule-dirty) (eq dirty t))
              (and (plistp dirty) (plist-get dirty :uncommitted))
              (and (not (plistp dirty)) dirty))
      (push (propertize "!" 'face 'error) parts))
    (when (or untracked (and (plistp dirty) (plist-get dirty :untracked)))
      (push (propertize "?" 'face 'warning) parts))
    (when (or submodule-dirty (and (plistp dirty) (plist-get dirty :submodule-dirty)))
      (push (propertize "S" 'face 'font-lock-builtin-face) parts))
    (when is-pr
      (push (propertize magit-dash-status-pr-badge 'face 'magit-dash-status-pr-face) parts))
    (mapconcat #'identity (nreverse parts) " ")))

(defun magit-dash--format-worktree-name (repo)
  "Format REPO name for display when REPO is a worktree."
  (let* ((name (magit-dash-repo-name repo))
         (branch (magit-dash-repo-branch repo))
         (base (cond
                ((and branch (not (string-empty-p branch))
                      (string-suffix-p (concat "@" branch) name))
                 (substring name 0 (- (length name) (1+ (length branch)))))
                ((string-match "\\(.*\\)@[^@]+" name)
                 (match-string 1 name))
                (t name))))
    (format magit-dash-worktree-name-format base)))

(defun magit-dash--format-worktree (repo)
  "Format the type indicator for REPO.
Shows \"WT\" for worktrees, \"SUBM\" for initialized submodules,
\"SUBM.EMPTY\" for missing/uninitialized submodules, \"SUBM.TR\" for
explicitly-registered submodules, \"MISSING\" for un-cloned repos,
\"REPO+SM\" for repos with discovered submodules, and \"REPO\" for
ordinary working-tree repos."
  (let ((path (magit-dash-repo-path repo))
        (submodule (magit-dash-repo-submodule repo)))
    (cond
     ((eq submodule 'missing)
      (propertize "SUBM.EMPTY" 'face 'warning))
     ((magit-dash-repo-worktree repo)
      (propertize "WT" 'face 'magit-dash-repo-branch-face))
     ((and submodule
           magit-dash--submodule-path-set
           (gethash path magit-dash--submodule-path-set))
      (propertize "SUBM.TR" 'face 'magit-dash-repo-branch-face))
     (submodule
      (propertize "SUBM" 'face 'magit-dash-repo-branch-face))
     ((magit-dash--repo-missing-p repo)
      (propertize "MISSING" 'face 'warning))
     ((magit-dash-gh--cache-get path :submodules)
      (propertize "REPO+SM" 'face 'shadow))
     (t (propertize "REPO" 'face 'shadow)))))
(defun magit-dash--format-sync (repo)
  "Format a compact sync indicator for REPO based on configured auto operations.
Each enabled operation contributes its name; names are joined with \"+\".
Returns an empty string when nothing is set."
  (let ((parts (seq-filter
                #'identity
                (list
                 (when (magit-dash-repo-auto-fetch repo)        "fetch")
                 (when (magit-dash-repo-auto-pull repo)         "pull")
                 (when (magit-dash-repo-auto-commit repo)       "commit")
                 (when (magit-dash-repo-auto-push repo)         "push")
                 (when (magit-dash--repo-operation repo :sync)  "cmd")))))
    (if parts
        (propertize (mapconcat #'identity parts "+") 'face 'magit-dash-repo-branch-face)
      "")))
;;;; Repo dashboard mode

(defface magit-dash-repo-name-face
  '((t :inherit font-lock-keyword-face))
  "Face for repository names in the repo dashboard."
  :group 'magit-dash)

(defface magit-dash-repo-branch-face
  '((t :inherit font-lock-string-face))
  "Face for branch names in the repo dashboard."
  :group 'magit-dash)

;;; Point-preserving redraw

(defun magit-dash--path-at (pos)
  "Return the repo path of the row at buffer position POS, or nil."
  (save-excursion
    (goto-char pos)
    (when-let* ((id (tabulated-list-get-id)))
      (if (magit-dash-repo-p id)
          (magit-dash-repo-path id)
        (and (stringp id) id)))))

(defun magit-dash--goto-path (path)
  "Move point to the row for repo PATH; return non-nil if found."
  (when path
    (goto-char (point-min))
    (let (found)
      (while (and (not found) (not (eobp)))
        (if (equal path (magit-dash--path-at (point)))
            (setq found t)
          (forward-line 1)))
      found)))

(defun magit-dash--print ()
  "Redraw the table, keeping every window's point and scroll position.
`tabulated-list-print' only restores point in the current window and
matches rows with `equal' on the repo struct, so a redraw from a timer
or async callback (or after repos are rebuilt) sent point to the top.
Rows are matched here by repo path instead, with line/column fallbacks."
  (let* ((buf (current-buffer))
         (buf-path (magit-dash--path-at (point)))
         (buf-line (line-number-at-pos (point)))
         (buf-col (current-column))
         (saved (seq-map (lambda (win)
                           (with-selected-window win
                             (list win
                                   (magit-dash--path-at (window-point win))
                                   (magit-dash--path-at (window-start win))
                                   (current-column)
                                   (line-number-at-pos (window-point win)))))
                         (get-buffer-window-list buf nil t))))
    (tabulated-list-print t)
    ;; 1. Always restore point in current-buffer
    (if (and buf-path (magit-dash--goto-path buf-path))
        (move-to-column buf-col)
      (goto-char (point-min))
      (forward-line (1- (max 1 (min buf-line (count-lines (point-min) (point-max))))))
      (move-to-column buf-col))
    ;; 2. Restore point and scroll in all live windows displaying the buffer
    (seq-do (lambda (entry)
              (pcase-let ((`(,win ,point-path ,start-path ,col ,line) entry))
                (when (window-live-p win)
                  (save-selected-window
                    (select-window win)
                    (when (and start-path (magit-dash--goto-path start-path))
                      (set-window-start win (point) t))
                    (if (and point-path (magit-dash--goto-path point-path))
                        (move-to-column col)
                      (goto-char (point-min))
                      (forward-line (1- (max 1 (min line (count-lines (point-min) (point-max))))))
                      (move-to-column col))
                    (set-window-point win (point))))))
            saved)))

(defun magit-dash--build-format (repos)
  "Return the tabulated-list format vector for REPOS using enabled columns.
Name and Branch are sized to their longest value (Name + 2, Branch + 1),
but each is capped at 25% of the window width.  All other columns have
fixed widths and are right-aligned."
  (let* ((window-cols (or (ignore-errors
                            (window-width (or (get-buffer-window (current-buffer) t)
                                              (selected-window))))
                          97))
         (cap (max 8 (/ window-cols 4)))
         (raw-name (seq-reduce
                    (lambda (w r)
                      (max w (length (or (and magit-dash--submodule-path-set
                                              (gethash (magit-dash-repo-path r)
                                                       magit-dash--submodule-path-set))
                                         (magit-dash-repo-name r)))))
                    repos 0))
         (raw-branch (seq-reduce
                      (lambda (w r)
                        (max w (length (magit-dash--display-branch-name
                                        (or (plist-get (magit-dash-gh--cache-get
                                                        (magit-dash-repo-path r) :stats)
                                                       :branch)
                                            (magit-dash-repo-branch r)
                                            "")))))
                      repos 0))
         (name-width (min cap (max (length "Name") (+ raw-name 2))))
         (branch-width (min cap (max 8 (1+ raw-branch)))))
    (apply #'vector
           (seq-map (lambda (col)
                      (pcase col
                        ('name   `("Name"   ,name-width   t))
                        ('branch `("Branch" ,branch-width t))
                        (_       (append (alist-get col magit-dash--column-defs)
                                         '(:right-align t)))))
                    (magit-dash--active-columns)))))

(defvar magit-dash-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Navigation & primary inspection (single key)
    (keymap-set m "RET" #'magit-dash-magit-status)
    (keymap-set m "SPC" #'magit-dash-toggle-mark)
    (keymap-set m "d"   #'magit-dash-magit-diff)
    (keymap-set m "e"   #'magit-dash-find-file)
    (keymap-set m "o"   #'magit-dash-open-repo)
    (keymap-set m "q"   #'magit-dash-quit-window)
    (keymap-set m "?"   #'magit-dash-menu)
    (keymap-set m "!"   #'magit-dash-magit-dispatch)
    (keymap-set m "/"   #'magit-dash-hide-select)
    (keymap-set m "y"   #'magit-dash-prune-branches)

    ;; Refresh operations
    (keymap-set m "r"   #'magit-dash-refresh-repo)
    (keymap-set m "g"   #'magit-dash-refresh)
    (keymap-set m "M-r" #'magit-dash-hard-refresh)

    ;; Mark & menu operations
    (keymap-set m "*"   #'magit-dash-unmark-all)
    (keymap-set m "m"   (cons "mark/menu" (make-sparse-keymap)))
    (keymap-set m "m m" #'magit-dash-menu)
    (keymap-set m "m ?" #'magit-dash-menu)
    (keymap-set m "m t" #'magit-dash-mark-by-tag)
    (keymap-set m "m u" #'magit-dash-mark-unpushed)
    (keymap-set m "m c" #'magit-dash-unmark-all)

    ;; Fetch prefix: f
    (keymap-set m "f"   (cons "fetch" (make-sparse-keymap)))
    (keymap-set m "f f" #'magit-dash-fetch)
    (keymap-set m "f r" #'magit-dash-fetch)
    (keymap-set m "f a" #'magit-dash-fetch-all)

    ;; Pull prefix: u
    (keymap-set m "u"   (cons "pull" (make-sparse-keymap)))
    (keymap-set m "u u" #'magit-dash-pull)
    (keymap-set m "u r" #'magit-dash-pull)
    (keymap-set m "u a" #'magit-dash-pull-all)

    ;; Push & PR prefix: p
    (keymap-set m "p"   (cons "push/pr" (make-sparse-keymap)))
    (keymap-set m "p p" #'magit-dash-push)
    (keymap-set m "p a" #'magit-dash-push-all)
    (keymap-set m "p r" #'magit-dash-pr-views-menu)
    (keymap-set m "p d" #'magit-dash-pr)

    ;; CI prefix: C
    (keymap-set m "C"   (cons "ci" (make-sparse-keymap)))
    (keymap-set m "C f" #'magit-dash-gh-ci-fetch-at-point)
    (keymap-set m "C o" #'magit-dash-gh-ci-open-at-point)
    (keymap-set m "C x" #'magit-dash-gh-ci-fix-at-point)
    (keymap-set m "C w" #'magit-dash-gh-workflow-run)
    (keymap-set m "C c" #'magit-dash-ci-menu)
    (keymap-set m "C ?" #'magit-dash-ci-menu)

    ;; Commit prefix: c
    (keymap-set m "c"   (cons "commit" (make-sparse-keymap)))
    (keymap-set m "c c" #'magit-dash-magit-commit)
    (keymap-set m "c a" #'magit-dash-commit-all)
    (keymap-set m "c o" #'magit-dash-commit)
    (keymap-set m "c f" #'magit-dash-gh-ci-fetch-at-point)
    (keymap-set m "c x" #'magit-dash-gh-ci-fix-at-point)
    (keymap-set m "c w" #'magit-dash-gh-workflow-run)
    (keymap-set m "c i" #'magit-dash-ci-menu)

    ;; Sync & Stage prefix: s
    (keymap-set m "s"   (cons "sync/stage" (make-sparse-keymap)))
    (keymap-set m "s s" #'magit-dash-sync)
    (keymap-set m "s a" #'magit-dash-sync-all)
    (keymap-set m "s t" #'magit-dash-stage-all)
    (keymap-set m "s m" #'magit-dash-stage-all-all)

    ;; Branch & Buffer & Bootstrap prefix: b
    (keymap-set m "b"   (cons "branch/buffer" (make-sparse-keymap)))
    (keymap-set m "b b" #'magit-dash-visit-buffer)
    (keymap-set m "b s" #'magit-dash-switch-branch)
    (keymap-set m "b r" #'magit-dash-bootstrap-repo)
    (keymap-set m "b c" #'magit-dash-clone-repo)

    ;; Log prefix: l
    (keymap-set m "l"   (cons "log" (make-sparse-keymap)))
    (keymap-set m "l l" #'magit-dash-magit-log)
    (keymap-set m "l f" #'magit-dash-magit-log-full)
    (keymap-set m "l a" #'magit-dash-magit-log-full)

    ;; Worktree prefix: w
    (keymap-set m "w"   (cons "worktree" (make-sparse-keymap)))
    (keymap-set m "w w" #'magit-dash-open-worktree-dispatch)
    (keymap-set m "w d" #'magit-dash-open-worktree-dispatch)
    (keymap-set m "w a" #'magit-dash-worktree-add)
    (keymap-set m "w r" #'magit-dash-worktree-from-pr)
    (keymap-set m "w k" #'magit-dash-worktree-delete)
    (keymap-set m "w s" #'magit-dash-worktree-sync-symlinks)
    (keymap-set m "w t" #'magit-dash-toggle-worktrees)

    ;; Agent Shell & Autosync prefix: a
    (keymap-set m "a"   (cons "agent/autosync" (make-sparse-keymap)))
    (keymap-set m "a p" #'magit-dash-actionable-open)
    (keymap-set m "a a" #'magit-dash-auto-sync)
    (keymap-set m "a s" #'magit-dash-auto-sync)
    (keymap-set m "a o" #'magit-dash-agent-shell)
    (keymap-set m "a n" #'magit-dash-agent-shell-new)
    (keymap-set m "a q" #'magit-dash-agent-shell-queue)
    (keymap-set m "a w" #'magit-dash-agent-shell-workflow)
    (keymap-set m "a d" #'magit-dash-dispatch-workflow)

    ;; Tag & Toggle prefix: t
    (keymap-set m "t"   (cons "tag/toggle" (make-sparse-keymap)))
    (keymap-set m "t t" #'magit-dash-filter-by-tag)
    (keymap-set m "t u" #'magit-dash-filter-unpushed)
    (keymap-set m "t m" #'magit-dash-filter-marked)
    (keymap-set m "t v" #'magit-dash-open-view)
    (keymap-set m "t g" #'magit-dash-group-menu)
    (keymap-set m "t a" #'magit-dash-add-tag)
    (keymap-set m "t n" #'magit-dash-add-tag)
    (keymap-set m "t r" #'magit-dash-remove-tag)
    (keymap-set m "t d" #'magit-dash-clear-tags)
    (keymap-set m "t c" #'magit-dash-toggle-column)
    (keymap-set m "t s" #'magit-dash-toggle-submodules)
    (keymap-set m "t w" #'magit-dash-toggle-worktrees)
    (keymap-set m "t x" #'magit-dash-clear-filter)

    ;; Hide prefix: h (preserved as-is)
    (keymap-set m "h"   (cons "hide" (make-sparse-keymap)))
    (keymap-set m "h r" #'magit-dash-hide-repo)
    (keymap-set m "h c" #'magit-dash-unhide-all)
    (keymap-set m "h u" #'magit-dash-unhide-repo)
    (keymap-set m "h s" #'magit-dash-hide-select)

    ;; Execute prefix: x (preserved as-is)
    (keymap-set m "x"   (cons "command" (make-sparse-keymap)))
    (keymap-set m "x c" #'magit-dash-run-command)
    (keymap-set m "x b" #'magit-dash-run-command-background)

    ;; Convenient single-key convenience aliases
    (keymap-set m "n"   #'magit-dash-sync)
    (keymap-set m "i"   #'magit-dash-add-tag)
    (keymap-set m "k"   #'magit-dash-worktree-delete)
    (keymap-set m "M-s" #'magit-dash-toggle-submodules)
    (keymap-set m "M-t" #'magit-dash-toggle-worktrees)
    m)
  "Keymap for `magit-dash-mode'.")

(defvar-local magit-dash--tag-filter nil
  "When non-nil, a symbol or list of symbols: only repos tagged with these are shown.")

(defvar-local magit-dash--predicate-filter nil
  "When non-nil, a filter predicate for repositories shown in the dashboard.
May be `unpushed', `dirty', a cons (:paths . PATH-LIST), or a function.")

(defvar-local magit-dash--view-name nil
  "When non-nil, the name of the current dashboard view buffer.")

(declare-function magit-dash--get-stats "magit-dash")
(declare-function magit-dash--get-stats-fast "magit-dash")

(defun magit-dash--repo-ahead-p (repo)
  "Return non-nil when REPO has unpushed commits ahead of its upstream."
  (let* ((path (magit-dash-repo-path repo))
         (stats (or (when (and path (fboundp 'magit-dash-gh--cache-get))
                      (magit-dash-gh--cache-get path :stats))
                    (ignore-errors (magit-dash--get-stats repo)))))
    (and stats
         (> (or (plist-get stats :ahead) 0) 0)
         (let ((branch (plist-get stats :branch)))
           (and (stringp branch)
                (not (string-empty-p branch))
                (not (string= branch "?"))
                (not (string= branch "…")))))))

(defun magit-dash--repo-dirty-p (repo)
  "Return non-nil when REPO has uncommitted changes."
  (let ((stats (magit-dash--get-stats-fast repo)))
    (and stats (plist-get stats :dirty) t)))

(defun magit-dash--repo-matches-predicate-filter-p (repo filter)
  "Return non-nil if REPO matches predicate FILTER.
FILTER may be nil (matches all), `unpushed', `dirty',
a cons (:paths . PATH-LIST), or a function taking REPO."
  (cond
   ((null filter) t)
   ((eq filter 'unpushed)
    (magit-dash--repo-ahead-p repo))
   ((eq filter 'dirty)
    (magit-dash--repo-dirty-p repo))
   ((and (consp filter) (eq (car filter) :paths))
    (member (magit-dash-repo-path repo) (cdr filter)))
   ((functionp filter)
    (funcall filter repo))
   (t t)))

(defun magit-dash--repo-matches-tag-filter-p (repo filter)
  "Return non-nil if REPO matches tag FILTER.
FILTER may be nil (matches all), a symbol (matches if REPO has that tag),
or a list of symbols (matches if REPO has all tags in FILTER)."
  (cond
   ((null filter) t)
   ((listp filter)
    (let ((tags (magit-dash--all-tags-for repo)))
      (seq-every-p (lambda (tg) (memq tg tags)) filter)))
   ((symbolp filter)
    (memq filter (magit-dash--all-tags-for repo)))
   (t t)))

(defvar magit-dash--ephemeral-tags (make-hash-table :test #'equal)
  "Hash table mapping repo path strings to lists of runtime tag symbols.
These tags are not saved to the repo registry.  The table is persisted
across sessions via `savehist-additional-variables' (see
`magit-dash-global-mode') and layered over registered tags.")

(defun magit-dash--all-tags-for (repo)
  "Return the combined registered and persistent runtime tags for REPO."
  (append (magit-dash-repo-tags repo)
          (map-elt magit-dash--ephemeral-tags (magit-dash-repo-path repo))))

(defun magit-dash--permanent-tag-set ()
  "Return deduplicated list of permanent tag symbols across registered repos."
  (delete-dups (seq-mapcat #'magit-dash-repo-tags magit-dash-repo-list)))

(defvar-local magit-dash--marked-paths nil
  "List of repo paths currently marked for batch operations.")

(defvar-local magit-dash--hidden-paths nil
  "List of repo paths currently hidden from the dashboard view.")

(defvar-local magit-dash--batch-all nil
  "When non-nil, batch operations act on all repos in the table.
Disabled by default; toggle with `magit-dash-toggle-batch-all'.")

(defun magit-dash--update-default-directory ()
  "Sync `default-directory' with the repo at point, falling back to `~/'.
If the repository at point or its underlying directory does not exist,
set `default-directory' to the parent directory where the repository should be."
  (setq-local default-directory
              (if-let* ((repo (tabulated-list-get-id)))
                  (let ((path (expand-file-name (magit-dash-repo-path repo))))
                    (if (magit-dash--repo-missing-p repo)
                        (file-name-as-directory (file-name-directory (directory-file-name path)))
                      (file-name-as-directory path)))
                (file-name-as-directory (expand-file-name "~/")))))

(defvar which-key-mode)
(defvar which-key-replacement-alist)
(declare-function which-key--start-timer "which-key")

;; `magit-dash--which-key-register-replacements' is defined in `magit-dash-global.el'.

(defun magit-dash--which-key-sync-timer (&optional _window)
  "Adjust `which-key' idle timer based on current window mode.
Sets timer to `magit-dash-which-key-idle-delay' when inside a dashboard
buffer, or restores the default timer when outside."
  (when (and (bound-and-true-p which-key-mode)
             (bound-and-true-p magit-dash-which-key-idle-delay)
             (fboundp 'which-key--start-timer))
    (let ((in-dash (with-current-buffer (window-buffer (selected-window))
                     (derived-mode-p 'magit-dash-mode))))
      (if in-dash
          (which-key--start-timer magit-dash-which-key-idle-delay)
        (which-key--start-timer)))))

(defun magit-dash--which-key-setup ()
  "Hook function to configure `which-key' when entering `magit-dash-mode'.
Registers command abbreviations and adjusts idle delay."
  (magit-dash--which-key-register-replacements)
  (when magit-dash-which-key-idle-delay
    (add-hook 'window-selection-change-functions #'magit-dash--which-key-sync-timer)
    (magit-dash--which-key-sync-timer)))

(define-derived-mode magit-dash-mode tabulated-list-mode "Repos"
  "Major mode for the registered repository dashboard."
  (setq tabulated-list-format (magit-dash--build-format magit-dash-repo-list))
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (setq-local default-directory (file-name-as-directory (expand-file-name "~/")))
  (add-hook 'post-command-hook #'magit-dash--update-default-directory nil t)
  (add-hook 'kill-buffer-hook #'magit-dash--cancel-refresh-jobs nil t)
  (add-hook 'kill-buffer-hook #'magit-dash--which-key-sync-timer nil t)
  (magit-dash--which-key-setup))

(defun magit-dash--build-entry (repo)
  "Return a `tabulated-list-entries' entry for REPO using enabled columns.
Uses cached stats without blocking; stale or absent stats show as placeholders
until `magit-dash--populate-stats-async' updates them."
  (let* ((stats (magit-dash--get-stats-fast repo))
         (active (magit-dash--active-columns)))
    (list repo
          (apply #'vector
                 (seq-map
                  (lambda (col)
                    (pcase col
                      ('name
                       (let* ((subm-name (and magit-dash--submodule-path-set
                                              (gethash (magit-dash-repo-path repo) magit-dash--submodule-path-set)))
                              (is-missing (magit-dash--repo-missing-p repo))
                              (is-marked (member (magit-dash-repo-path repo) magit-dash--marked-paths))
                              (display (cond
                                        (subm-name subm-name)
                                        ((and (magit-dash-repo-worktree repo)
                                              (magit-dash-repo-branch repo)
                                              (not (string-empty-p (magit-dash-repo-branch repo))))
                                         (magit-dash--format-worktree-name repo))
                                        (t (magit-dash-repo-name repo))))
                              (base-face (if is-marked
                                            '(bold magit-dash-repo-name-face)
                                          'magit-dash-repo-name-face))
                              (final-face (if is-missing
                                             (list '(:strike-through t) base-face)
                                           base-face)))
                         (propertize display 'face final-face)))
                      ('org
                       (let* ((slug (or (magit-dash-repo-upstream-repo repo)
                                        (magit-dash-repo-repo repo)
                                        (magit-dash-repo-account repo)
                                        ""))
                              (pair (when (string-match "\\`\\([^/]+\\)/" slug)
                                      (match-string 1 slug))))
                         (or pair (magit-dash-repo-account repo) "")))
                      ('branch
                       (propertize (magit-dash--display-branch-name
                                    (let ((b (plist-get stats :branch)))
                                      (if (and b (not (string-empty-p b)))
                                          b
                                        (or (magit-dash-repo-branch repo) "?"))))
                                   'face 'magit-dash-repo-branch-face))
                      ('fetched
                       (magit-dash--format-age (plist-get stats :fetch-age)))
                      ('updated
                       (magit-dash--format-age (plist-get stats :updated-age)))
                      ('freshness
                       (magit-dash--format-freshness stats))
                      ('status
                       (let ((uncommitted (plist-get stats :uncommitted))
                             (untracked (plist-get stats :untracked))
                             (sm-dirty (plist-get stats :submodule-dirty))
                             (dirty (plist-get stats :dirty))
                             (is-pr (magit-dash--repo-is-pr-p repo stats)))
                         (magit-dash--format-status
                          (or (plist-get stats :ahead) 0)
                          (or (plist-get stats :behind) 0)
                          (if (or uncommitted untracked sm-dirty)
                              uncommitted
                            dirty)
                          untracked
                          sm-dirty
                          is-pr)))
                      ('worktree
                       (magit-dash--format-worktree repo))
                      ('sync
                       (magit-dash--format-sync repo))
                      ('cached
                       (if (magit-dash-gh--cache-get (magit-dash-repo-path repo) :stats)
                           (propertize "✓" 'face 'success)
                         (propertize "·" 'face 'shadow)))
                      ('upstream
                       (or (magit-dash-repo-repo repo) ""))
                      ('why
                       (or (and (boundp 'magit-dash-actionable--why-cache)
                                magit-dash-actionable--why-cache
                                (gethash (magit-dash-repo-path repo) magit-dash-actionable--why-cache))
                           (and (fboundp 'magit-dash-actionable--format-why)
                                (magit-dash-actionable--format-why repo))
                           ""))
                      ('ci
                       (if (not (magit-dash-repo-include-ci repo))
                           ""
                         (let ((ci-status (magit-dash-gh--cache-get
                                           (magit-dash-repo-path repo) :ci-status)))
                           (if ci-status
                               (magit-dash-gh-ci--format-status ci-status)
                             (propertize "?" 'face 'shadow)))))))
                  active)))))

(defun magit-dash--update-entry (repo)
  "Rebuild the dashboard row for REPO from the current cache and re-render."
  (let ((path (magit-dash-repo-path repo)))
    (setq tabulated-list-entries
          (seq-map (lambda (entry)
                     (if (equal (magit-dash-repo-path (car entry)) path)
                         (magit-dash--build-entry repo)
                       entry))
                   tabulated-list-entries))
    (magit-dash--print)))

(defun magit-dash--update-entry-all-buffers (repo)
  "Update the dashboard row for REPO across all live dashboard buffers."
  (seq-do
   (lambda (b)
     (when (buffer-live-p b)
       (with-current-buffer b
         (magit-dash--update-entry repo))))
   (magit-dash--live-dash-buffers)))

(defun magit-dash--populate-stats-async (repos &optional on-done quiet)
  "Asynchronously collect stats for stale or uncached repos in REPOS.
Uses `gen-run-async' with bounded concurrency and cooperative timer yields.
Emits a status message with repo counts; updates rows as each finishes.
When ON-DONE is non-nil, call it when all stats collections finish.
When QUIET is non-nil, suppresses echo area messages.
Returns a cancellation handle."
  (let* ((guards magit-dash--refresh-guards)
         (needs-update
          (magit-dash--sort-repos-by-priority
           (seq-filter
            (lambda (repo)
              (and (not (eq (magit-dash-repo-submodule repo) 'missing))
                   (let* ((path (magit-dash-repo-path repo))
                          (cached (magit-dash-gh--cache-get path :stats)))
                     (not (and cached
                               (equal (magit-dash--head-hash path)
                                      (plist-get cached :head-hash)))))))
            repos)))
         (total (length repos))
         (stale (length needs-update))
         (done (list 0)))
    (if (= stale 0)
        (progn
          (unless quiet
            (message "magit-dash: %d repos (all cached)" total))
          (when on-done (funcall on-done))
          (lambda (&rest _) nil))
      (unless quiet
        (message "magit-dash: %d repos, loading %d…" total stale))
      (gen-run-async
       (lambda (repo done-cb)
         (if (not (magit-dash-gh--refresh-current-p guards (magit-dash-repo-path repo)))
             (funcall done-cb)
           (magit-dash--collect-stats-async
            repo
            (lambda (_stats)
              (setcar done (1+ (car done)))
              (magit-dash--update-entry-all-buffers repo)
              (when (and (= (car done) stale) (not quiet))
		(message "magit-dash: %d repos, %d updated" total stale))
              (funcall done-cb)))))
       needs-update
       :concurrency magit-dash-max-concurrent-jobs
       :delay magit-dash-queue-yield-delay
       :on-done on-done))))

(defun magit-dash--repo-type-rank (repo)
  "Return a sort rank for REPO based on its git context type.
0 = plain repo, 1 = worktree, 2 = tracked submodule, 3 = missing submodule."
  (+ (or (magit-dash-repo-sort-hint repo) 0)
     (cond
      ((magit-dash-repo-worktree repo) 1)
      ((eq (magit-dash-repo-submodule repo) 'missing) 3)
      ((and magit-dash--submodule-path-set
            (map-elt magit-dash--submodule-path-set (magit-dash-repo-path repo))) 3)
      ((magit-dash-repo-submodule repo) 2)
      (t 0))))

(defun magit-dash--sorted-repos (repos)
  "Return REPOS sorted by :sort-hint then type, with discovered worktrees.
Primary sort is :sort-hint ascending (nil hints follow all sorted ones).
Secondary sort within equal hints is by type: repo < worktree < submodule
< missing.  Auto-discovered submodules whose path is already in
`magit-dash-repo-list' are suppressed to avoid duplicate rows — the
registered entry is shown instead."
  (let* ((sorted (seq-sort (lambda (a b)
                             (let ((ha (magit-dash-repo-sort-hint a))
                                   (hb (magit-dash-repo-sort-hint b)))
                               (cond
                                ((and ha hb)
                                 (if (= ha hb)
                                     (< (magit-dash--repo-type-rank a)
                                        (magit-dash--repo-type-rank b))
                                   (< ha hb)))
                                (ha t)
                                (hb nil)
                                (t (< (magit-dash--repo-type-rank a)
                                      (magit-dash--repo-type-rank b))))))
                           repos))
         (registered-paths (let ((paths (make-hash-table :test #'equal)))
                             (seq-do (lambda (r)
                                       (puthash (magit-dash-repo-path r) t paths))
                                     magit-dash-repo-list)
                             paths)))
    (seq-mapcat (lambda (repo)
                  (cons repo
                        (append (when (magit-dash--repo-show-worktrees-p repo)
                                  (magit-dash-gh--cache-get (magit-dash-repo-path repo) :worktrees))
                                (when (magit-dash--repo-show-submodules-p repo)
                                  (seq-remove
                                   (lambda (sm)
                                     (gethash (magit-dash-repo-path sm) registered-paths))
                                   (magit-dash-gh--cache-get (magit-dash-repo-path repo) :submodules))))))
                sorted)))

(defun magit-dash--record-fetch-attempt (repo)
  "Record current timestamp as last fetch attempt for REPO."
  (puthash (expand-file-name (magit-dash-repo-path repo))
           (float-time)
           magit-dash--repo-last-fetch-attempts))

(defun magit-dash--record-sync-attempt (repo)
  "Record current timestamp as last sync attempt for REPO."
  (let ((path (expand-file-name (magit-dash-repo-path repo))))
    (puthash path (float-time) magit-dash--repo-last-sync-attempts)
    (puthash path (float-time) magit-dash--repo-last-fetch-attempts)))

(defun magit-dash--repo-fetch-interval-elapsed-p (repo &optional interval)
  "Return non-nil when INTERVAL seconds have elapsed since last fetch for REPO.
INTERVAL defaults to `magit-dash-refresh-fetch-min-interval'.
Checks both in-memory last attempt timestamp and on-disk FETCH_HEAD age."
  (let* ((min-int (or interval magit-dash-refresh-fetch-min-interval 600))
         (path (magit-dash-repo-path repo))
         (exp-path (expand-file-name path))
         (last-attempt (gethash exp-path magit-dash--repo-last-fetch-attempts))
         (now (float-time))
         (fetch-age (magit-dash--fetch-age path)))
    (cond
     ((and last-attempt (< (- now last-attempt) min-int))
      nil)
     ((and fetch-age (< fetch-age min-int))
      nil)
     (t t))))

(defun magit-dash--repo-sync-interval-elapsed-p (repo &optional interval)
  "Return non-nil when INTERVAL seconds have elapsed since last sync for REPO.
INTERVAL defaults to `magit-dash-refresh-sync-min-interval'.
Checks in-memory last sync attempt timestamp."
  (let* ((min-int (or interval magit-dash-refresh-sync-min-interval 3600))
         (exp-path (expand-file-name (magit-dash-repo-path repo)))
         (last-attempt (gethash exp-path magit-dash--repo-last-sync-attempts))
         (now (float-time)))
    (if last-attempt
        (>= (- now last-attempt) min-int)
      t)))

(defun magit-dash--refresh-fetch-eligible-p (repo)
  "Return non-nil when REPO is eligible for automatic fetch during refresh.
A repository is eligible if it exists on disk, is not a missing submodule,
and has fetch or sync configured."
  (and (not (eq (magit-dash-repo-submodule repo) 'missing))
       (file-directory-p (magit-dash-repo-path repo))
       (or (magit-dash-repo-fetch-on-refresh repo)
           (magit-dash-repo-auto-fetch repo)
           (magit-dash-repo-auto-pull repo)
           (magit-dash-repo-sync-on-refresh repo)
           (magit-dash-repo-remote-sync repo)
           (magit-dash--repo-operation repo :fetch)
           (magit-dash--has-sync-configured-p repo))))

(defun magit-dash--refresh-sync-eligible-p (repo)
  "Return non-nil when REPO is eligible for automatic full sync during refresh."
  (and (not (eq (magit-dash-repo-submodule repo) 'missing))
       (file-directory-p (magit-dash-repo-path repo))
       (magit-dash-repo-sync-on-refresh repo)))

(defun magit-dash--refresh-fetch-or-sync-async (repos &optional on-done)
  "Run automatic sync or fetch for eligible repos in REPOS during refresh.
Bounded by `magit-dash-max-concurrent-network-jobs' and yielded cooperatively.
Honors `magit-dash-refresh-fetch', `magit-dash-refresh-fetch-min-interval',
`magit-dash-refresh-sync-min-interval', and per-repo `:sync-on-refresh'.
Returns a cancellation handle."
  (let* ((guards magit-dash--refresh-guards)
         (buf (get-buffer magit-dash-buffer-name))
         (eligible-repos
          (magit-dash--sort-repos-by-priority
           (seq-filter
            (lambda (repo)
              (or (and (magit-dash--refresh-sync-eligible-p repo)
                       (magit-dash--repo-sync-interval-elapsed-p repo))
                  (and magit-dash-refresh-fetch
                       (magit-dash--refresh-fetch-eligible-p repo)
                       (magit-dash--repo-fetch-interval-elapsed-p repo))))
            repos))))
    (if (null eligible-repos)
        (progn
          (when on-done (funcall on-done))
          (lambda (&rest _) nil))
      (gen-run-async
       (lambda (repo done)
         (let ((finished
                (lambda (_status &optional _text)
                  (when (or (null guards)
                            (gethash (cons (magit-dash-repo-path repo) :refresh)
                                     magit-dash-gh--cache-requests))
                    (magit-dash--collect-stats-async
                     repo
                     (lambda (_stats)
                       (when (and buf (buffer-live-p buf))
			 (with-current-buffer buf
                           (magit-dash--update-entry repo)))))
                    (when (magit-dash-repo-include-ci repo)
                      (magit-dash-gh-ci-fetch
                       repo
                       (lambda (_ci)
			 (when (and buf (buffer-live-p buf))
                           (with-current-buffer buf
                             (magit-dash--update-entry repo)))))))
                  (funcall done))))
           (cond
            ((not (magit-dash-gh--refresh-current-p guards (magit-dash-repo-path repo)))
             (funcall done))
            ((and (magit-dash--refresh-sync-eligible-p repo)
                  (magit-dash--repo-sync-interval-elapsed-p repo))
             (magit-dash--record-sync-attempt repo)
             (magit-dash--auto-sync-async repo finished))
            ((and magit-dash-refresh-fetch
                  (magit-dash--refresh-fetch-eligible-p repo)
                  (magit-dash--repo-fetch-interval-elapsed-p repo))
             (magit-dash--record-fetch-attempt repo)
             (magit-dash--guarded-op-run
              repo "fetch"
              (lambda (r cb) (magit-dash--run-operation r :fetch cb))
              finished))
            (t (funcall done)))))
       eligible-repos
       :concurrency magit-dash-max-concurrent-network-jobs
       :delay magit-dash-queue-yield-delay
       :on-done on-done))))

(defun magit-dash-refresh ()
  "Refresh the dashboard, clearing all per-repo caches and re-fetching.
Discovers worktrees synchronously and submodules asynchronously.
Renders the table immediately with cached state, then cancels any pending
refresh jobs before launching throttled stats, CI, and fetch/sync pipelines
via `gen-run-async'.
When invoked outside `magit-dash-mode', safely redirects to the
`magit-dash-buffer-name' buffer if live, preventing accidental corruption
of other buffers."
  (interactive)
  (if (not (derived-mode-p 'magit-dash-mode))
      (if-let* ((buf (get-buffer magit-dash-buffer-name)))
          (if (and (buffer-live-p buf)
                   (with-current-buffer buf (derived-mode-p 'magit-dash-mode)))
              (with-current-buffer buf
                (magit-dash-refresh))
            (when (called-interactively-p 'interactive)
              (user-error "Buffer %s is not in magit-dash-mode" magit-dash-buffer-name)))
        (when (called-interactively-p 'interactive)
          (user-error "No %s buffer exists" magit-dash-buffer-name)))
    (magit-dash--cancel-refresh-jobs)
    (magit-dash--discover-worktrees)
    (let ((subm-handle (magit-dash--discover-submodules)))
      (magit-dash--rebuild-submodule-path-set)
      (let* ((visible-base
              (seq-filter
               (lambda (r)
                 (and (not (member (magit-dash-repo-path r) magit-dash--hidden-paths))
                      (magit-dash--repo-matches-tag-filter-p r magit-dash--tag-filter)
                      (magit-dash--repo-matches-predicate-filter-p r magit-dash--predicate-filter)))
               magit-dash-repo-list))
             (repos (seq-filter
                     (lambda (r)
                       (not (member (magit-dash-repo-path r) magit-dash--hidden-paths)))
                     (magit-dash--sorted-repos visible-base))))
        (when magit-dash--view-name
          (setq-local mode-name (format "Repos[%s]" magit-dash--view-name)))
        (setq tabulated-list-format (magit-dash--build-format repos))
        (tabulated-list-init-header)
        (setq tabulated-list-entries (seq-map #'magit-dash--build-entry repos))
        (magit-dash--print)
        (let* ((dash-buf (current-buffer))
               (_ (when tabulated-list-entries
                    (magit-dash--dim-buffer dash-buf)))
               (refresh-handle (magit-dash--refresh-repos
                                repos nil nil
                                (lambda ()
                                  (magit-dash--undim-buffer dash-buf)))))
          (setq magit-dash--refresh-cancel-handle
                (lambda (&optional action)
                  (when (functionp subm-handle) (funcall subm-handle action))
                  (when (functionp refresh-handle) (funcall refresh-handle action)))))))))

(defun magit-dash-hard-refresh ()
  "Clear all caches and repopulate the dashboard from scratch.
Unlike `magit-dash-refresh', discards all cached stats so every repo is
re-collected asynchronously, graying out the buffer and keeping existing
rows visible until fresh results arrive."
  (interactive)
  (when-let* ((buf (or (get-buffer magit-dash-buffer-name)
                       (and (derived-mode-p 'magit-dash-mode) (current-buffer)))))
    (when (buffer-live-p buf)
      (magit-dash--dim-buffer buf)))
  (magit-dash--cancel-refresh-jobs)
  (clrhash magit-dash-gh--cache)
  (clrhash magit-dash-gh--cache-requests)
  (clrhash magit-dash--repo-last-fetch-attempts)
  (clrhash magit-dash--repo-last-sync-attempts)
  (magit-dash-refresh))

(defun magit-dash--can-refresh-repo-p ()
  "Return non-nil when repos can be refreshed (marked exist or point on repo)."
  (or (magit-dash--has-marks-p)
      (magit-dash--repo-at-point-p)))

(defun magit-dash--refresh-repos (repos &optional force quiet on-done)
  "Refresh only REPOS, updating their dashboard rows in-place.
When FORCE is non-nil, resets last fetch and sync attempt timestamps
for REPOS so automatic fetch/sync is not throttled.
When QUIET is non-nil, suppresses echo area messages.
When ON-DONE is non-nil, calls it when all refresh operations finish."
  (let* ((repos (magit-dash--sort-repos-by-priority repos))
         (buf (or (get-buffer magit-dash-buffer-name)
                  (and (derived-mode-p 'magit-dash-mode) (current-buffer))))
         (magit-dash--refresh-guards (make-hash-table :test #'equal))
         (stats-done nil)
         (ci-done nil)
         (check-done (lambda ()
                       (when (and stats-done ci-done)
                         (when on-done (funcall on-done))))))
    (seq-do
     (lambda (repo)
       (let* ((path (magit-dash-repo-path repo))
              (exp-path (expand-file-name path)))
         (magit-dash-gh--cache-remove path :stats)
         (magit-dash-gh--cache-remove path :pr-counts)
         (magit-dash-gh--cache-remove path :ci-status)
         (puthash path (magit-dash-gh--cache-request path :refresh)
                  magit-dash--refresh-guards)
         (when force
           (remhash exp-path magit-dash--repo-last-fetch-attempts)
           (remhash exp-path magit-dash--repo-last-sync-attempts))))
     repos)
    (let ((stats-handle (condition-case nil
                            (magit-dash--populate-stats-async
                             repos
                             (lambda ()
                               (setq stats-done t)
                               (funcall check-done))
                             quiet)
                          (wrong-number-of-arguments
                           (magit-dash--populate-stats-async repos))))
          (ci-handle (when (and buf (buffer-live-p buf))
                       (magit-dash-gh-ci-fetch-batch
                        repos
                        :on-done (lambda ()
                                   (setq ci-done t)
                                   (funcall check-done)))))
          (sync-handle (magit-dash--refresh-fetch-or-sync-async repos)))
      (unless (and buf (buffer-live-p buf) (seq-some #'magit-dash-repo-include-ci repos))
        (setq ci-done t))
      (lambda (&optional action)
        (when (functionp stats-handle) (funcall stats-handle action))
        (when (functionp ci-handle) (funcall ci-handle action))
        (when (functionp sync-handle) (funcall sync-handle action))))))

;;;###autoload
(defun magit-dash-refresh-repo (&optional target force quiet)
  "Refresh only TARGET or repository at point, updating row(s) in-place.
When TARGET is nil:
  - If repositories are marked, refreshes all marked repositories.
  - If called interactively with a prefix argument (\[universal-argument]),
    prompts for a tag and refreshes all repositories with that tag.
  - Otherwise, refreshes the repository at point.
TARGET may also be:
  - A `magit-dash-repo' struct to refresh that specific repository.
  - A list of `magit-dash-repo' structs to refresh multiple repositories.
  - A symbol representing a tag, refreshing all repositories with that tag.
  - A string (path or name) identifying a repository.
When FORCE is non-nil (the default), resets in-memory fetch and sync
attempt timestamps so operations are not skipped.
When QUIET is non-nil, suppresses echo area messages."
  (interactive (list (when current-prefix-arg
                       (magit-dash--read-tag "Refresh repos with tag: " :require-match t))
                     t
                     nil))
  (let* ((force (if (null force) t force))
         (repos
          (cond
           ((null target)
            (cond
             ((magit-dash--has-marks-p)
              (seq-filter (lambda (r)
                            (member (magit-dash-repo-path r)
                                    magit-dash--marked-paths))
                          (magit-dash--all-repos)))
             (t
              (list (magit-dash--repo-at-point)))))
           ((magit-dash-repo-p target)
            (list target))
           ((and (listp target) (seq-every-p #'magit-dash-repo-p target))
            target)
           ((symbolp target)
            (let ((tagged (seq-filter (lambda (r)
                                        (memq target (magit-dash--all-tags-for r)))
                                      (magit-dash--all-repos))))
              (unless tagged
                (user-error "No repositories with tag '%s'" (symbol-name target)))
              tagged))
           ((stringp target)
            (let ((matched (seq-filter (lambda (r)
                                         (or (equal (magit-dash-repo-path r) target)
                                             (equal (magit-dash-repo-name r) target)))
                                       (magit-dash--all-repos))))
              (unless matched
                (user-error "No repository matching '%s'" target))
              matched))
           (t
            (user-error "Invalid target for refresh: %s" target)))))
    (unless repos
      (user-error "No repositories to refresh"))
    (unless quiet
      (if (= (length repos) 1)
          (message "magit-dash: refreshing %s…" (magit-dash-repo-name (car repos)))
        (message "magit-dash: refreshing %d repos…" (length repos))))
    (if quiet
        (magit-dash--refresh-repos repos force quiet)
      (magit-dash--refresh-repos repos force))))

;;;###autoload
(defalias 'magit-dash-refresh-row #'magit-dash-refresh-repo)

(defvar magit-dash--inhibit-post-refresh-hook nil
  "Non-nil when `magit-dash--on-magit-post-refresh' is executing.")

;; `magit-dash--on-magit-post-refresh' is defined in `magit-dash-global.el'.


;;;###autoload
(defun magit-dash-refresh-marked ()
  "Refresh only marked repositories in the dashboard."
  (interactive)
  (unless (magit-dash--has-marks-p)
    (user-error "No repositories marked"))
  (let ((marked (seq-filter
                 (lambda (r)
                   (member (magit-dash-repo-path r) magit-dash--marked-paths))
                 (magit-dash--all-repos))))
    (magit-dash-refresh-repo marked t)))

;;;###autoload
(defun magit-dash-refresh-by-tag (&optional tag)
  "Refresh all repositories tagged with TAG chosen via annotated completion.
Updates only rows for matching repositories."
  (interactive)
  (let ((tag (or tag (magit-dash--read-tag "Refresh repos with tag: " :require-match t))))
    (when tag
      (magit-dash-refresh-repo tag t))))


(defun magit-dash--repo-at-point ()
  "Return the `magit-dash-repo' struct at point or signal `user-error'."
  (or (tabulated-list-get-id)
      (user-error "No repository at point")))

(defun magit-dash--repo-at-point-p ()
  "Return t if current point is on a `magit-dash' repo."
  (and
   (derived-mode-p (current-buffer) 'magit-dash-mode)
   (tabulated-list-get-id)
   t))

(defun magit-dash--repo-annotation (repo)
  "Return an annotation string for REPO showing its path and dash cache info.
Format: `PATH  (BRANCH +AHEAD/-BEHIND* · CI:STATUS · N PRs)' when cached stats
exist, or just `PATH' when no extra cache data is available."
  (let* ((path (magit-dash-repo-path repo))
         (abbrev-path (abbreviate-file-name path))
         (stats (when (fboundp 'magit-dash-gh--cache-get)
                  (magit-dash-gh--cache-get path :stats)))
         (branch (or (plist-get stats :branch) (magit-dash-repo-branch repo)))
         (ahead (or (plist-get stats :ahead) 0))
         (behind (or (plist-get stats :behind) 0))
         (dirty (plist-get stats :dirty))
         (ci (when (fboundp 'magit-dash-gh--cache-get)
               (magit-dash-gh--cache-get path :ci-status)))
         (pr-counts (when (fboundp 'magit-dash-gh--cache-get)
                      (magit-dash-gh--cache-get path :pr-counts)))
         (counts-str (cond
                      ((and (> ahead 0) (> behind 0)) (format "+%d/-%d" ahead behind))
                      ((> ahead 0) (format "+%d" ahead))
                      ((> behind 0) (format "-%d" behind))
                      (t nil)))
         (dirty-str (when dirty "*"))
         (branch-str (when (and branch (not (string-empty-p branch)) (not (equal branch "…")))
                       (if (or counts-str dirty-str)
                           (format "%s%s%s" branch (if counts-str (format " %s" counts-str) "") (or dirty-str ""))
                         branch)))
         (ci-str (when ci
                   (let ((conclusion (plist-get ci :conclusion))
                         (status (plist-get ci :status)))
                     (cond
                      ((equal conclusion "success") "CI:✓")
                      ((equal conclusion "failure") "CI:✗")
                      ((equal status "in_progress") "CI:…")
                      (conclusion (format "CI:%s" conclusion))
                      (t nil)))))
         (pr-str (when (and (consp pr-counts) (> (car pr-counts) 0))
                   (format "%d PR%s" (car pr-counts) (if (= (car pr-counts) 1) "" "s"))))
         (extra-parts (seq-remove #'null (list branch-str ci-str pr-str))))
    (if extra-parts
        (format "%s  (%s)" abbrev-path (string-join extra-parts " · "))
      abbrev-path)))

(defun magit-dash--prompt-for-repo-path (&optional prompt)
  "Prompt for one of the registered repositories and return its path.
PROMPT is the prompt string displayed to the user.
The candidate list uses `annotated-completing-read', annotating each
repository with its abbreviated file path and cached dash stats
\(branch, ahead/behind, dirty, CI status, PR counts).
Signals `user-error' when no repositories are registered."
  (unless magit-dash-repo-list
    (user-error "Magit-dash: no registered repositories to choose from"))
  (let ((table (map-into
                (seq-map (lambda (r)
                           (cons (magit-dash-repo-name r)
                                 (cons (magit-dash--repo-annotation r) (magit-dash-repo-path r))))
                         magit-dash-repo-list)
                '(hash-table :test equal))))
    (annotated-completing-read
     table
     :prompt (or prompt "Repository: ")
     :require-match t
     :category 'magit-dash-repo)))

;;;###autoload
(defun magit-dash-open-registered-repo ()
  "Prompt for a registered repository from `magit-dash-repo-list' and open it.
The picker displays repository names annotated with their paths and
cached dashboard stats."
  (interactive)
  (let ((path (magit-dash--prompt-for-repo-path "Open repository: ")))
    (magit-status-setup-buffer path)))

(defun magit-dash--repo-missing-p (repo)
  "Return non-nil if REPO is missing (no directory, or no git repository)."
  (let ((path (magit-dash-repo-path repo)))
    (or (eq (magit-dash-repo-submodule repo) 'missing)
        (not (file-exists-p path))
        (not (file-directory-p path))
        (not (magit-dash--resolve-git-dir path)))))

(defun magit-dash--repo-at-point-missing-p ()
  "Return non-nil if the repo at point in the dashboard is missing."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (magit-dash--repo-missing-p repo)))

(defun magit-dash--all-repos ()
  "Return all visible repos in dashboard buffer, or `magit-dash-repo-list'."
  (or (and (derived-mode-p 'magit-dash-mode)
           tabulated-list-entries
           (seq-map #'car tabulated-list-entries))
      magit-dash-repo-list))

(defun magit-dash--has-missing-repos-p ()
  "Return non-nil if any registered repository in the dashboard is missing."
  (seq-some #'magit-dash--repo-missing-p (magit-dash--all-repos)))


(defun magit-dash--default-clone-url (repo)
  "Return the best default clone URL for REPO."
  (or (magit-dash-repo-clone-url repo)
      (magit-dash-repo-repo repo)
      (plist-get (magit-dash-gh--cache-get (magit-dash-repo-path repo) :stats) :remote-origin)
      (let ((name (magit-dash-repo-name repo)))
        (if (string-match-p "/" name)
            (format "git@github.com:%s.git" name)
          (format "git@github.com:tychoish/%s.git" name)))))

;;;###autoload
(defun magit-dash-clone-repo (repo &optional url on-complete)
  "Clone REPO from URL (or prompt if unset) into REPO's destination path.
ON-COMPLETE is an optional callback called when cloning finishes.
Runs asynchronously and refreshes the dashboard on completion."
  (interactive
   (list (magit-dash--repo-at-point)))
  (unless repo
    (user-error "No repository specified"))
  (let* ((name (magit-dash-repo-name repo))
         (path (expand-file-name (magit-dash-repo-path repo)))
         (default-url (magit-dash--default-clone-url repo))
         (clone-url (if (or url (not (called-interactively-p 'interactive)))
                        (or url default-url)
                      (read-string (format "Clone URL for %s: " name) default-url))))
    (when (magit-dash--resolve-git-dir path)
      (user-error "Repository already exists at %s" path))
    (let ((parent-dir (file-name-directory (directory-file-name path))))
      (unless (file-directory-p parent-dir)
        (make-directory parent-dir t)))
    (message "magit-dash: cloning %s from %s..." name clone-url)
    (let* ((parent-dir (file-name-directory (directory-file-name path)))
           (default-directory (if (and parent-dir (file-directory-p parent-dir))
                                  parent-dir
                                default-directory))
           (proc-buf (generate-new-buffer (format " *magit-dash-clone:%s*" name)))
           (args (list "clone" clone-url path))
           (proc (make-process
                  :name (format "magit-dash-clone-%s" name)
                  :buffer proc-buf
                  :command (cons (magit-git-executable) args)
                  :sentinel
                  (lambda (p _event)
                    (when (memq (process-status p) '(exit signal))
                      (let ((code (process-exit-status p))
                            (out (with-current-buffer (process-buffer p)
                                   (string-trim (buffer-string)))))
                        (kill-buffer (process-buffer p))
                        (if (zerop code)
                            (progn
                              (message "magit-dash: successfully cloned %s" name)
                              (magit-dash-cache-reset path)
                              (magit-dash--maybe-refresh)
                              (when on-complete (funcall on-complete 'ok)))
                          (message "magit-dash: failed to clone %s (exit %d): %s"
                                   name code out)
                          (when on-complete (funcall on-complete 'error out)))))))))
      proc)))

;;;###autoload
(defun magit-dash-clone-all-missing ()
  "Clone all configured but missing repositories asynchronously."
  (interactive)
  (let* ((marked-missing (when magit-dash--marked-paths
                           (seq-filter #'magit-dash--repo-missing-p
                                       (magit-dash--effective-repos))))
         (missing (or marked-missing
                      (seq-filter #'magit-dash--repo-missing-p
                                  (magit-dash--all-repos)))))
    (unless missing
      (user-error "No missing repositories to clone"))
    (message "magit-dash: cloning %d missing repository(ies)..." (length missing))
    (dolist (repo missing)
      (magit-dash-clone-repo repo nil))))

(defun magit-dash--prompt-for-repo (&optional prompt repos)
  "Prompt for one of REPOS and return its struct.
PROMPT is the prompt string displayed to the user."
  (let ((candidate-repos (or repos magit-dash-repo-list)))
    (unless candidate-repos
      (user-error "Magit-dash: no registered repositories to choose from"))
    (let ((table (map-into
                  (seq-map (lambda (r)
                             (cons (magit-dash-repo-name r)
                                   (cons (magit-dash--repo-annotation r) r)))
                           candidate-repos)
                  '(hash-table :test equal))))
      (annotated-completing-read
       table
       :prompt (or prompt "Repository: ")
       :require-match t
       :category 'magit-dash-repo))))

(defun magit-dash--repo-has-upstream-p ()
  "Return non-nil if the repo at point has an upstream :repo configured."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (and (magit-dash-repo-repo repo) t)))

(defun magit-dash--has-missing-bootstrap-repos-p ()
  "Return non-nil if batch bootstrap is permitted and targets at least
one missing repo with :repo set."
  (and (magit-dash--batch-enabled-p)
       (seq-some (lambda (r)
                   (and (magit-dash-repo-repo r)
                        (magit-dash--repo-missing-p r)))
                 (magit-dash--effective-repos))))

;;;###autoload
(defun magit-dash-bootstrap-repo (&optional repo on-complete)
  "Clone REPO from upstream `:repo' path into its destination path.
REPO defaults to the repository at point or is selected interactively.
ON-COMPLETE is an optional callback invoked on completion.
Runs asynchronously and refreshes the dashboard on completion."
  (interactive
   (list (if (magit-dash--repo-at-point-p)
             (magit-dash--repo-at-point)
           (magit-dash--prompt-for-repo "Bootstrap repository: "))))
  (let ((r (or repo
               (if (magit-dash--repo-at-point-p)
                   (magit-dash--repo-at-point)
                 (magit-dash--prompt-for-repo "Bootstrap repository: ")))))
    (unless r
      (user-error "No repository specified"))
    (let ((upstream (magit-dash-repo-repo r))
          (path (expand-file-name (magit-dash-repo-path r))))
      (unless (and upstream (not (string-empty-p upstream)))
        (user-error "Repository %s has no upstream :repo configured" (magit-dash-repo-name r)))
      (when (magit-dash--resolve-git-dir path)
        (user-error "Repository already exists at %s" path))
      (magit-dash-clone-repo r upstream on-complete))))

(defalias 'magit-dash-bootstrap #'magit-dash-bootstrap-repo)
(defalias 'magit-dash-clone-upstream-repo #'magit-dash-bootstrap-repo)
(defalias 'magit-dash-clone-upstream #'magit-dash-bootstrap-repo)

;;;###autoload
(defun magit-dash-bootstrap-marked ()
  "Bootstrap marked missing repositories with an upstream `:repo' configured.
When nothing is marked, targets all visible missing repositories with `:repo'.
Runs asynchronously and refreshes the dashboard on completion."
  (interactive)
  (let* ((effective (magit-dash--effective-repos))
         (repos (seq-filter (lambda (r)
                              (and (magit-dash-repo-repo r)
                                   (magit-dash--repo-missing-p r)))
                            effective)))
    (unless repos
      (user-error (if (magit-dash--has-marks-p)
                      "No marked missing repositories with upstream :repo configured to bootstrap"
                    "No missing repositories with upstream :repo configured to bootstrap")))
    (message "magit-dash: bootstrapping %d missing repository(ies)..." (length repos))
    (magit-dash--batch-run
     repos
     (lambda (r cb)
       (magit-dash-clone-repo r (magit-dash-repo-repo r) cb))
     "magit-dash bootstrap"
     (lambda (_) (magit-dash--maybe-refresh)))))

(defalias 'magit-dash-bootstrap-all #'magit-dash-bootstrap-marked)
(defalias 'magit-dash-bootstrap-batch #'magit-dash-bootstrap-marked)
(defalias 'magit-dash-bootstrap-all-missing #'magit-dash-bootstrap-marked)
(defalias 'magit-dash-clone-all-upstream #'magit-dash-bootstrap-marked)

(defun magit-dash--resolve-repo-path ()
  "Return an absolute path to a git repository for the current command context.
Tries, in order:
  - the `magit-dash-repo' at point, when called from a `magit-dash' buffer.
  - the git repository containing the current buffer's `default-directory',
    which covers any `magit'-derived buffer (status, log, diff, ...) as well
    as an ordinary file or `dired' buffer inside a repository.
  - a `completing-read' prompt over `magit-dash-repo-list', requiring an
    exact match, when no repository can be determined unambiguously."
  (or (and (magit-dash--repo-at-point-p)
           (magit-dash-repo-path (magit-dash--repo-at-point)))
      (magit-toplevel)
      (magit-dash--prompt-for-repo-path)))

(defun magit-dash-magit-dispatch ()
  "Open `magit-dispatch' in the context of the repository at point."
  (interactive)
  (magit-dash--with-repo (magit-dash--repo-at-point)
    (call-interactively #'magit-dispatch)))

(defun magit-dash-magit-status ()
  "Open a magit status buffer for the repository at point."
  (interactive)
  (magit-dash--with-repo (magit-dash--repo-at-point)
    (magit-status-setup-buffer default-directory)))

(defun magit-dash-magit-diff ()
  "Open a magit diff (dwim) buffer for the repository at point."
  (interactive)
  (magit-dash--with-repo (magit-dash--repo-at-point)
    (call-interactively #'magit-diff)))

(defun magit-dash-magit-log ()
  "Open magit log for the current branch in the repository at point."
  (interactive)
  (magit-dash--with-repo (magit-dash--repo-at-point)
    (call-interactively #'magit-log-current)))

(defun magit-dash-magit-log-full ()
  "Open the full magit log menu for the repository at point."
  (interactive)
  (magit-dash--with-repo (magit-dash--repo-at-point)
    (call-interactively #'magit-log)))

(defun magit-dash-magit-commit ()
  "Open a magit commit buffer for the repository at point."
  (interactive)
  (magit-dash--with-repo (magit-dash--repo-at-point)
    (call-interactively #'magit-commit-create)))

(defun magit-dash-fetch ()
  "Run git fetch for the repository at point via magit."
  (interactive)
  (magit-dash--with-repo (magit-dash--repo-at-point)
    (call-interactively #'magit-fetch)))

(defun magit-dash-pull ()
  "Pull from upstream for the repository at point via magit."
  (interactive)
  (magit-dash--with-repo (magit-dash--repo-at-point)
    (call-interactively #'magit-pull-from-upstream)))

(defun magit-dash-commit ()
  "Auto-commit modified files in the repository at point.
Signals `user-error' when :auto-commit is not configured for this repo."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (unless (magit-dash-repo-auto-commit repo)
      (user-error "Auto-commit is not configured for %s" (magit-dash-repo-name repo)))
    (if (magit-dash--auto-commit repo)
        (progn
          (magit-dash-refresh-repo repo nil t)
          (message "magit-dash: committed changes in %s" (magit-dash-repo-name repo)))
      (message "magit-dash: nothing to commit or commit failed in %s"
               (magit-dash-repo-name repo)))))

(defun magit-dash--current-repo-or-path ()
  "Return `magit-dash-repo' struct or path for the current context.
In `magit-dash-mode', returns the repo at point.
In other buffers (such as `magit-status' or a file buffer), returns the
registered `magit-dash-repo' matching `magit-toplevel' if one exists,
otherwise the toplevel path string."
  (cond
   ((and (derived-mode-p 'magit-dash-mode)
         (magit-dash--repo-at-point-p))
    (magit-dash--repo-at-point))
   (t
    (let ((top (or (and (fboundp 'magit-toplevel) (magit-toplevel))
                   (and default-directory
                        (locate-dominating-file default-directory ".git")))))
      (unless top
        (user-error "Not inside a git repository"))
      (setq top (file-name-as-directory (expand-file-name top)))
      (or (seq-find (lambda (r)
                      (file-equal-p (magit-dash-repo-path r) top))
                    (magit-dash--all-repos))
          top)))))

;;;###autoload
(defun magit-dash-stage-all ()
  "Stage all untracked, unstaged, and deleted files in the current repository.
In `magit-dash-mode', stages repo at point (or all marked repos if marks exist).
In `magit-status' and other buffers, stages changes for the current repository
and refreshes Magit.
Follows ~/garen/scripts/git-readd by removing deleted tracked files and staging
all working tree changes."
  (interactive)
  (cond
   ((and (derived-mode-p 'magit-dash-mode) (magit-dash--has-marks-p))
    (magit-dash-stage-all-all))
   (t
    (let* ((target (magit-dash--current-repo-or-path))
           (name (if (stringp target)
                     (file-name-nondirectory (directory-file-name target))
                   (magit-dash-repo-name target))))
      (if (magit-dash--stage-all target)
          (progn
            (when (magit-dash-repo-p target)
              (magit-dash-refresh-repo target nil t))
            (when (fboundp 'magit-refresh)
              (ignore-errors (magit-refresh)))
            (message "magit-dash: staged all changes in %s" name))
        (message "magit-dash: stage all failed in %s" name))))))

(defun magit-dash--repo-has-upstream-repo-p (&optional repo)
  "Return non-nil if REPO or the repo at point has an upstream repo configured."
  (when-let* ((repo (or repo
                        (ignore-errors (magit-dash--repo-at-point))
                        (and (= (length magit-dash-repo-list) 1)
                             (car magit-dash-repo-list)))))
    (and (magit-dash-repo-upstream-repo repo) t)))

(defun magit-dash-upstream--ensure-remote (repo)
  "Ensure an upstream remote exists in REPO and return its remote name.
REPO is a `magit-dash-repo' struct, directory path string, or nil
for current context."
  (let* ((r (cond
             ((magit-dash-repo-p repo) repo)
             (repo
              (let ((top (file-name-as-directory (expand-file-name repo))))
                (seq-find (lambda (it)
                            (file-equal-p (file-name-as-directory (magit-dash-repo-path it)) top))
                          (magit-dash--all-repos))))
             (t (when (fboundp 'magit-dash--current-repo-or-path)
                  (let ((cur (magit-dash--current-repo-or-path)))
                    (if (magit-dash-repo-p cur) cur nil))))))
         (raw-url (and r (magit-dash-repo-upstream-repo r)))
         (url (cond
               ((and raw-url (string-match-p "\\`[^/@:]+/[^/@:]+\\'" raw-url))
                (format "git@%s:%s.git"
                        (or (and r (magit-dash-repo-gh-host r)) "github.com")
                        raw-url))
               (t raw-url)))
         (remotes (magit-list-remotes)))
    (cond
     ((and url (not (string-empty-p url)))
      (let* ((target-pair (magit-dash-gh--owner-repo-from-url url))
             (existing-remote
              (seq-find (lambda (rem)
                          (let ((rem-url (magit-get "remote" rem "url")))
                            (or (and rem-url (string-equal rem-url url))
                                (and target-pair
                                     rem-url
                                     (equal (magit-dash-gh--owner-repo-from-url rem-url) target-pair)))))
                        remotes)))
        (cond
         (existing-remote existing-remote)
         ((not (member "upstream" remotes))
          (magit-call-git "remote" "add" "upstream" url)
          "upstream")
         (t
          (let ((rem-name "dash-upstream"))
            (unless (member rem-name remotes)
              (magit-call-git "remote" "add" rem-name url))
            rem-name)))))
     ((member "upstream" remotes)
      "upstream")
     (t
      (user-error "Repository %s has no upstream remote or :upstream-repo registered"
                  (if r (magit-dash-repo-name r) default-directory))))))

;;;###autoload
(defun magit-dash-upstream-fetch (&optional repo)
  "Fetch configured upstream branch for REPO using Magit primitives.
REPO defaults to the repository for current context or at point."
  (interactive)
  (let* ((target (or repo (magit-dash--current-repo-or-path)))
         (struct (if (magit-dash-repo-p target) target nil))
         (path (if (magit-dash-repo-p target) (magit-dash-repo-path target) target))
         (name (if struct (magit-dash-repo-name struct) (file-name-nondirectory (directory-file-name path))))
         (upstream-branch (or (and struct (magit-dash-repo-upstream-branch struct)) "master")))
    (magit-dash-gh--with-repo-dir path
      (let ((remote (magit-dash-upstream--ensure-remote struct)))
        (message "magit-dash: [%s] fetching %s from %s..." name upstream-branch remote)
        (if (= 0 (magit-call-git "fetch" remote upstream-branch))
            (progn
              (message "magit-dash: [%s] fetched %s/%s" name remote upstream-branch)
              (when (fboundp 'magit-refresh) (magit-refresh))
              t)
          (user-error "magit-dash: [%s] failed to fetch %s from %s" name upstream-branch remote))))))

;;;###autoload
(defun magit-dash-upstream-merge (&optional repo)
  "Fetch upstream and merge it into local main/default branch for REPO.
Uses Magit primitives.  Ensures upstream remote exists and targets
the branch where local changes live.
REPO defaults to the repository for current context or at point."
  (interactive)
  (let* ((target (or repo (magit-dash--current-repo-or-path)))
         (struct (if (magit-dash-repo-p target) target nil))
         (path (if (magit-dash-repo-p target) (magit-dash-repo-path target) target))
         (name (if struct (magit-dash-repo-name struct) (file-name-nondirectory (directory-file-name path))))
         (upstream-branch (or (and struct (magit-dash-repo-upstream-branch struct)) "master"))
         (local-branch (or (and struct (magit-dash-repo-branch struct)) "main"))
         (wt-dir (and (fboundp 'magit-dash-worktree-for-branch)
                      (magit-dash-worktree-for-branch path local-branch)))
         (effective-dir (or wt-dir path)))
    (magit-dash-gh--with-repo-dir effective-dir
      (when (magit-anything-modified-p)
        (user-error "Cannot merge upstream in %s: working tree has uncommitted changes" name))
      (let ((remote (magit-dash-upstream--ensure-remote (or struct effective-dir))))
        (message "magit-dash: [%s] fetching %s from %s..." name upstream-branch remote)
        (unless (= 0 (magit-call-git "fetch" remote upstream-branch))
          (user-error "Failed to fetch %s from %s in %s" upstream-branch remote name))
        (let ((cur-branch (magit-get-current-branch)))
          (unless (equal cur-branch local-branch)
            (message "magit-dash: [%s] checking out %s..." name local-branch)
            (magit-checkout local-branch)))
        (message "magit-dash: [%s] merging %s/%s into %s..." name remote upstream-branch local-branch)
        (magit-merge-plain (format "%s/%s" remote upstream-branch))
        (when (fboundp 'magit-refresh) (magit-refresh))
        (when (and struct (fboundp 'magit-dash-refresh-repo))
          (magit-dash-refresh-repo struct nil t))
        (message "magit-dash: [%s] merged %s/%s into %s" name remote upstream-branch local-branch)))))

;;;###autoload
(defun magit-dash-upstream-rebase (&optional repo)
  "Fetch upstream and rebase local main/default branch onto it for REPO.
Uses Magit primitives.  Ensures upstream remote exists and targets
the branch where local changes live.
REPO defaults to the repository for current context or at point."
  (interactive)
  (let* ((target (or repo (magit-dash--current-repo-or-path)))
         (struct (if (magit-dash-repo-p target) target nil))
         (path (if (magit-dash-repo-p target) (magit-dash-repo-path target) target))
         (name (if struct (magit-dash-repo-name struct) (file-name-nondirectory (directory-file-name path))))
         (upstream-branch (or (and struct (magit-dash-repo-upstream-branch struct)) "master"))
         (local-branch (or (and struct (magit-dash-repo-branch struct)) "main"))
         (wt-dir (and (fboundp 'magit-dash-worktree-for-branch)
                      (magit-dash-worktree-for-branch path local-branch)))
         (effective-dir (or wt-dir path)))
    (magit-dash-gh--with-repo-dir effective-dir
      (when (magit-anything-modified-p)
        (user-error "Cannot rebase upstream in %s: working tree has uncommitted changes" name))
      (let ((remote (magit-dash-upstream--ensure-remote (or struct effective-dir))))
        (message "magit-dash: [%s] fetching %s from %s..." name upstream-branch remote)
        (unless (= 0 (magit-call-git "fetch" remote upstream-branch))
          (user-error "Failed to fetch %s from %s in %s" upstream-branch remote name))
        (let ((cur-branch (magit-get-current-branch)))
          (unless (equal cur-branch local-branch)
            (message "magit-dash: [%s] checking out %s..." name local-branch)
            (magit-checkout local-branch)))
        (message "magit-dash: [%s] rebasing %s onto %s/%s..." name local-branch remote upstream-branch)
        (magit-git-rebase (format "%s/%s" remote upstream-branch) nil)
        (when (fboundp 'magit-refresh) (magit-refresh))
        (when (and struct (fboundp 'magit-dash-refresh-repo))
          (magit-dash-refresh-repo struct nil t))
        (message "magit-dash: [%s] rebasing %s onto %s/%s started" name local-branch remote upstream-branch)))))

(defalias 'magit-dash-merge-upstream #'magit-dash-upstream-merge)
(defalias 'magit-dash-rebase-upstream #'magit-dash-upstream-rebase)
(defalias 'magit-dash-fetch-upstream #'magit-dash-upstream-fetch)

(defun magit-dash-push ()
  "Push current branch to its push remote for the repository at point."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (magit-dash--with-repo repo
      (call-interactively #'magit-push-current-to-pushremote))))

(defun magit-dash-sync ()
  "Run configured auto operations for repo at point asynchronously.
Signals `user-error' when no auto operations are configured for this repo."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (unless (magit-dash--has-sync-configured-p repo)
      (user-error "No auto operations configured for %s" (magit-dash-repo-name repo)))
    (magit-dash--with-repo repo
      (magit-dash--auto-sync-async
       repo
       (lambda (_status &optional _error-text)
         (magit-dash-refresh-repo repo nil t))))))

(defun magit-dash-commit-all ()
  "Auto-commit marked repos (or all if none) with :auto-commit configured.
Uses each repo's :auto-commit message function (or the default chore message).
Displays a summary message and refreshes the dashboard when all complete."
  (interactive)
  (let ((repos (seq-filter #'magit-dash-repo-auto-commit (magit-dash--effective-repos))))
    (unless repos
      (user-error "No repositories have :auto-commit configured"))
    (magit-dash--batch-run
     repos
     #'magit-dash--auto-commit-async
     "magit-dash commit"
     (lambda (_) (magit-dash--maybe-refresh repos)))))

(defun magit-dash-sync-all ()
  "Run auto operations for marked repos (or all if none marked), then refresh.
Operations run asynchronously."
  (interactive)
  (let ((repos (seq-filter #'magit-dash--has-sync-configured-p (magit-dash--effective-repos))))
    (unless repos
      (user-error "No repositories have auto operations configured"))
    (magit-dash--batch-run
     repos
     #'magit-dash--auto-sync-async
     "magit-dash sync"
     (lambda (_) (magit-dash--maybe-refresh repos)))))

;;;###autoload
(defun magit-dash-sync-repo ()
  "Select a repository and run its configured auto-sync operations."
  (interactive)
  (let ((repos (seq-filter #'magit-dash--has-sync-configured-p magit-dash-repo-list)))
    (unless repos
      (user-error "No repositories have auto operations configured"))
    (when-let* ((repo (annotated-completing-read
                       (map-into
                        (seq-map (lambda (r)
                                   (cons (magit-dash-repo-name r)
                                         (cons (magit-dash--repo-annotation r) r)))
                                 repos)
                        '(hash-table :test equal))
                       :prompt "sync repository: "
                       :require-match t)))
      (magit-dash--with-repo repo
        (magit-dash--auto-sync-async
         repo
         (lambda (_status &optional _error-text)
           (magit-dash-refresh-repo repo nil t)))))))

(defun magit-dash-auto-sync ()
  "Run auto operations for marked repos (or all if none marked) asynchronously.
Each repo's steps (fetch, pull, commit, push) run sequentially; each step is
logged individually.  Dashboard refreshes when all repos complete."
  (interactive)
  (let ((repos (seq-filter #'magit-dash--has-sync-configured-p (magit-dash--effective-repos))))
    (unless repos
      (user-error "No repos have auto operations configured"))
    (magit-dash--batch-run
     repos
     #'magit-dash--auto-sync-async
     "magit-dash autosync"
     (lambda (_) (magit-dash--maybe-refresh)))))

(defun magit-dash-run-command (&optional background)
  "Open an `annotated-completing-read' command picker for the repo at point.
Invoke the selected command.  When BACKGROUND is non-nil (or with prefix
argument \\[universal-argument]), run the command in the background without
displaying its buffer."
  (interactive "P")
  (magit-dash--run-command-for (magit-dash--repo-at-point) background))

(defun magit-dash-run-command-background ()
  "Open command picker for repo at point and run in the background."
  (interactive)
  (magit-dash--run-command-for (magit-dash--repo-at-point) t))

(defun magit-dash--build-tag-table ()
  "Build tag completion table.
Build an `annotated-completing-read' alist mapping tag-name strings to
annotation strings, targeting each entry's own tag symbol.
Each annotation lists the count of repos using the tag and up to four names.
Permanent tags (from repo :tags fields) are sorted before runtime-only tags."
  (let* ((permanent-set (magit-dash--permanent-tag-set))
         (tag-repo-map (let ((ht (make-hash-table :test #'eq)))
                         (seq-do
                          (lambda (repo)
                            (seq-do
                             (lambda (tag)
                               (puthash tag (cons repo (gethash tag ht)) ht))
                             (magit-dash--all-tags-for repo)))
                          magit-dash-repo-list)
                         ht))
         (all-tags (delete-dups
                    (seq-mapcat #'magit-dash--all-tags-for
                                magit-dash-repo-list)))
         (sorted-tags (seq-sort
                       (lambda (a b)
                         (let ((a-perm (memq a permanent-set))
                               (b-perm (memq b permanent-set)))
                           (cond
                            ((and a-perm (not b-perm)) t)
                            ((and b-perm (not a-perm)) nil)
                            (t (string< (symbol-name a) (symbol-name b))))))
                       all-tags)))
    (seq-map
     (lambda (tag)
       (let* ((repos (nreverse (gethash tag tag-repo-map)))
              (count (length repos))
              (shown (seq-take (seq-map #'magit-dash-repo-name repos) 4)))
         (cons (symbol-name tag)
               (cons (format "%d repo%s: %s%s"
                             count
                             (if (= count 1) "" "s")
                             (mapconcat #'identity shown ", ")
                             (if (> count (length shown)) "…" ""))
                     tag))))
     sorted-tags)))

(cl-defun magit-dash--read-tag (prompt &key include-clear require-match)
  "Read a tag using `annotated-completing-read' with PROMPT.
Shows permanent tags (from repo :tags fields) before runtime-only tags.
Each candidate is annotated with a repo count and up to four repo names.

When INCLUDE-CLEAR is non-nil a \"(clear)\" option is prepended; selecting it
returns the symbol `clear'.  When REQUIRE-MATCH is non-nil only existing tags
are accepted; otherwise arbitrary input is allowed for new runtime tags.
Returns an interned symbol, `clear', or nil on quit."
  (let* ((permanent-set (magit-dash--permanent-tag-set))
         (full-table (if include-clear
                         (cons (cons "(clear)" (cons "remove tag filter" 'clear))
                               (magit-dash--build-tag-table))
                       (magit-dash--build-tag-table)))
         (group-fn (lambda (candidate)
                     (cond
                      ((equal candidate "(clear)") " ")
                      ((memq (intern candidate) permanent-set) "permanent")
                      (t "runtime"))))
         (sort-fn (lambda (candidates)
                    (let ((perm permanent-set))
                      (seq-sort
                       (lambda (a b)
                         (cond
                          ((equal a "(clear)") t)
                          ((equal b "(clear)") nil)
                          (t (let ((a-perm (memq (intern a) perm))
                                   (b-perm (memq (intern b) perm)))
                               (cond
                                ((and a-perm (not b-perm)) t)
                                ((and b-perm (not a-perm)) nil)
                                (t (string< a b)))))))
                       candidates))))
         (result (annotated-completing-read
                  full-table
                  :prompt prompt
                  :group-name group-fn
                  :sort-fn sort-fn
                  :require-match require-match
                  :or-nil t)))
    (cond
     ((null result) nil)
     ((symbolp result) result)
     (t (intern result)))))

(defun magit-dash-filter-by-tag (&optional tag)
  "Filter the dashboard by tag using annotated completion.
When a filter is already active, allows narrowing (filtering down)
by selecting additional tags, or selecting \"(clear)\" to reset.
Select \"(clear)\" to show all repos; quitting leaves the filter
unchanged."
  (interactive)
  (let* ((active-tags (cond
                       ((null magit-dash--tag-filter) nil)
                       ((listp magit-dash--tag-filter) magit-dash--tag-filter)
                       (t (list magit-dash--tag-filter))))
         (prompt (if active-tags
                     (format "Filter down by tag (current: %s): "
                             (mapconcat #'symbol-name active-tags ", "))
                   "Filter by tag: "))
         (selected (or tag (magit-dash--read-tag prompt :include-clear t))))
    (when selected
      (if (eq selected 'clear)
          (progn
            (setq magit-dash--tag-filter nil)
            (message "magit-dash: tag filter cleared"))
        (let ((new-filters
               (if (memq selected active-tags)
                   (delq selected active-tags)
                 (append active-tags (list selected)))))
          (setq magit-dash--tag-filter new-filters)
          (message "magit-dash: filtering by tag%s: %s"
                   (if (> (length new-filters) 1) "s" "")
                   (mapconcat #'symbol-name new-filters ", "))))
      (magit-dash-refresh))))

(defun magit-dash-clear-tag-filter ()
  "Clear any active tag filter in the dashboard."
  (interactive)
  (setq magit-dash--tag-filter nil)
  (magit-dash-refresh)
  (message "magit-dash: tag filter cleared"))

(defun magit-dash-marked-repos ()
  "Return the marked registered repos.
In a `magit-dash-mode' buffer (including derived views) this is that
buffer's marks only; elsewhere it is the union of marks across all
dashboard buffers.  Marked rows that are not in `magit-dash-repo-list'
\(such as synthetic PR rows) are omitted."
  (let ((paths (if (derived-mode-p 'magit-dash-mode)
                   magit-dash--marked-paths
                 (thread-last (buffer-list)
                              (seq-filter (lambda (b)
                                            (provided-mode-derived-p
                                             (buffer-local-value 'major-mode b)
                                             'magit-dash-mode)))
                              (seq-mapcat (lambda (b)
                                            (buffer-local-value 'magit-dash--marked-paths b)))
                              delete-dups))))
    (seq-filter (lambda (r) (member (magit-dash-repo-path r) paths))
                magit-dash-repo-list)))

(defun magit-dash--tag-targets ()
  "Return marked repos, or the repo at point when nothing is marked."
  (or (magit-dash-marked-repos)
      (list (magit-dash--repo-at-point))))

(defun magit-dash-add-tag ()
  "Add a runtime tag to the marked repositories, or the repository at point.
The tag is not saved to the registry but persists via savehist.
Pick from existing tags or type a new symbol name."
  (interactive)
  (let ((targets (magit-dash--tag-targets)))
    (when-let* ((tag (magit-dash--read-tag
                      (if (cdr targets)
                          (format "Add tag to %d repos: " (length targets))
                        (format "Add tag to %s: " (magit-dash-repo-name (car targets)))))))
      (seq-do (lambda (repo)
                (let* ((path (magit-dash-repo-path repo))
                       (existing (map-elt magit-dash--ephemeral-tags path)))
                  (unless (memq tag existing)
                    (setf (map-elt magit-dash--ephemeral-tags path) (cons tag existing)))))
              targets)
      (magit-dash-refresh))))

(defun magit-dash-remove-tag ()
  "Remove a runtime tag from the marked repositories, or the repo at point."
  (interactive)
  (let* ((targets (magit-dash--tag-targets))
         (candidates (thread-last targets
                                  (seq-mapcat (lambda (r)
                                                (map-elt magit-dash--ephemeral-tags
                                                         (magit-dash-repo-path r))))
                                  delete-dups
                                  (seq-map #'symbol-name))))
    (unless candidates
      (user-error "No runtime tags on the selected repositories"))
    (let ((tag (intern (completing-read "Remove tag: " candidates nil t))))
      (seq-do (lambda (repo)
                (let* ((path (magit-dash-repo-path repo))
                       (remaining (delq tag (copy-sequence
                                             (map-elt magit-dash--ephemeral-tags path)))))
                  (if remaining
                      (setf (map-elt magit-dash--ephemeral-tags path) remaining)
                    (map-delete magit-dash--ephemeral-tags path))))
              targets)
      (magit-dash-refresh))))

(defun magit-dash-clear-tags (&optional all)
  "Clear runtime tags from the marked repositories, or the repo at point.
With prefix argument ALL, clear runtime tags from every repository."
  (interactive "P")
  (cond
   (all
    (clrhash magit-dash--ephemeral-tags)
    (message "magit-dash: cleared all runtime tags"))
   (t
    (let ((targets (magit-dash--tag-targets)))
      (seq-do (lambda (repo)
                (map-delete magit-dash--ephemeral-tags (magit-dash-repo-path repo)))
              targets)
      (message "magit-dash: cleared runtime tags on %d repo%s"
               (length targets) (if (cdr targets) "s" "")))))
  (magit-dash-refresh))

(declare-function agent-shell-workflow-dispatch-group "agent-shell-workflow")
(declare-function agent-shell-workflow-read "agent-shell-workflow-menu")
(declare-function agent-shell-workflow-spec-id "agent-shell-workflow")

(defun magit-dash-dispatch-workflow (&optional by-tag)
  "Dispatch an agent-shell workflow to the marked repositories.
Falls back to the repository at point when nothing is marked.  With
prefix argument BY-TAG, choose a tag and dispatch to every repo carrying it.
Signal `user-error' when agent-shell-workflow is unavailable or there is
nothing to dispatch to."
  (interactive "P")
  (unless (and (fboundp 'agent-shell-workflow-dispatch-group)
               (fboundp 'agent-shell-workflow-read))
    (user-error "agent-shell-workflow is not available; nothing dispatched"))
  (let* ((repos (if by-tag
                    (magit-dash--repos-for-tag
                     (magit-dash--read-tag "Dispatch to tag: " :require-match t)
                     t)
                  (magit-dash--tag-targets)))
         (paths (seq-map #'magit-dash-repo-path repos)))
    (unless paths
      (user-error "No repositories to dispatch to"))
    (agent-shell-workflow-dispatch-group
     (agent-shell-workflow-spec-id
      (agent-shell-workflow-read
       nil (format "Workflow for %d repo%s: " (length paths) (if (cdr paths) "s" ""))))
     paths)))

(defun magit-dash-mark-by-tag ()
  "Mark all repos sharing a tag chosen via annotated completion.
Adds to any existing marks rather than replacing them."
  (interactive)
  (when-let* ((tag (magit-dash--read-tag "Mark by tag: " :require-match t)))
    (let* ((tagged (seq-filter
                    (lambda (r)
                      (memq tag (magit-dash--all-tags-for r)))
                    magit-dash-repo-list))
           (paths (seq-map #'magit-dash-repo-path tagged)))
      (setq magit-dash--marked-paths
            (delete-dups (append paths magit-dash--marked-paths)))
      (magit-dash-refresh)
      (message "Marked %d repo%s with tag '%s"
               (length paths)
               (if (= (length paths) 1) "" "s")
               (symbol-name tag)))))

;;;###autoload
(defun magit-dash-clear-filter ()
  "Clear any active tag or predicate filter in the dashboard."
  (interactive)
  (setq magit-dash--tag-filter nil)
  (setq magit-dash--predicate-filter nil)
  (setq magit-dash--view-name nil)
  (setq-local mode-name "Repos")
  (magit-dash-refresh)
  (message "magit-dash: all filters cleared"))

;;;###autoload
(defun magit-dash-mark-unpushed ()
  "Mark all visible repositories that have unpushed commits."
  (interactive)
  (let* ((visible (magit-dash--visible-repos))
         (unpushed (seq-filter #'magit-dash--repo-ahead-p visible))
         (paths (seq-map #'magit-dash-repo-path unpushed)))
    (unless unpushed
      (user-error "No visible repositories have unpushed commits"))
    (setq magit-dash--marked-paths (delete-dups (append paths magit-dash--marked-paths)))
    (magit-dash-refresh)
    (message "Marked %d repo%s with unpushed commits"
             (length paths)
             (if (= (length paths) 1) "" "s"))))

(defun magit-dash-view-buffer-name (name)
  "Return the buffer name for a dashboard view named NAME."
  (format "*magit-dash-repos: %s*" name))

;;;###autoload
(defun magit-dash-open-view (name &optional predicate tag)
  "Open a new dashboard buffer displaying a filtered view named NAME.
PREDICATE is an optional filter symbol or function (e.g. `unpushed').
TAG is an optional tag symbol or list of tags to filter by.
Switches to the newly created or existing view buffer and refreshes."
  (interactive "sView name: ")
  (let* ((bname (magit-dash-view-buffer-name name))
         (buf (get-buffer-create bname)))
    (with-current-buffer buf
      (unless (derived-mode-p 'magit-dash-mode)
        (magit-dash-mode))
      (setq-local magit-dash--view-name name)
      (setq-local magit-dash--predicate-filter predicate)
      (when tag
        (setq-local magit-dash--tag-filter tag))
      (magit-dash-refresh))
    (pop-to-buffer-same-window buf)
    buf))

;;;###autoload
(defun magit-dash-filter-unpushed (&optional new-buffer)
  "Filter dashboard to only show repositories with unpushed commits.
With prefix argument NEW-BUFFER (or non-nil), opens the view in a new buffer
named `*magit-dash-repos: unpushed*'.
When invoked in an already filtered buffer, toggles the filter back off."
  (interactive "P")
  (if new-buffer
      (magit-dash-open-view "unpushed" 'unpushed)
    (unless (derived-mode-p 'magit-dash-mode)
      (user-error "Not in a magit-dash buffer"))
    (if (eq magit-dash--predicate-filter 'unpushed)
        (progn
          (setq magit-dash--predicate-filter nil)
          (magit-dash-refresh)
          (message "magit-dash: unpushed filter cleared"))
      (setq magit-dash--predicate-filter 'unpushed)
      (magit-dash-refresh)
      (let ((count (length (magit-dash--visible-repos))))
        (message "magit-dash: filtered to %d repositor%s with unpushed commits"
                 count (if (= count 1) "y" "ies"))))))

;;;###autoload
(defun magit-dash-filter-marked (&optional new-buffer)
  "Filter dashboard down to only currently marked repositories.
With prefix argument NEW-BUFFER (or non-nil), opens the view in a new buffer
named `*magit-dash-repos: marked*'.
Signals `user-error' if no repositories are currently marked."
  (interactive "P")
  (unless (magit-dash--has-marks-p)
    (user-error "No repositories are currently marked"))
  (let ((paths (copy-sequence magit-dash--marked-paths)))
    (if new-buffer
        (let ((buf (magit-dash-open-view "marked" (cons :paths paths))))
          (with-current-buffer buf
            (setq magit-dash--marked-paths paths))
          buf)
      (unless (derived-mode-p 'magit-dash-mode)
        (user-error "Not in a magit-dash buffer"))
      (setq magit-dash--predicate-filter (cons :paths paths))
      (magit-dash-refresh)
      (message "magit-dash: narrowed to %d marked repositor%s"
               (length paths) (if (= (length paths) 1) "y" "ies")))))

(defun magit-dash--repos-for-tag (tag &optional all-registered)
  "Return repositories matching TAG.
TAG may be a symbol or string.
If ALL-REGISTERED is non-nil or not in `magit-dash-mode', search all
`magit-dash-repo-list'; otherwise search visible repositories."
  (let* ((tag-sym (if (stringp tag) (intern tag) tag))
         (repos (if (or all-registered (not (derived-mode-p 'magit-dash-mode)))
                    magit-dash-repo-list
                  (magit-dash--all-repos))))
    (seq-filter (lambda (r)
                  (memq tag-sym (magit-dash--all-tags-for r)))
                repos)))

;;;###autoload
(defun magit-dash-group-open-view (&optional tag)
  "Open a dedicated dashboard view buffer for tagged repository group TAG."
  (interactive)
  (let ((tag (or tag (magit-dash--read-tag "Open view for tag group: " :require-match t))))
    (magit-dash-open-view (symbol-name tag) nil tag)))

;;;###autoload
(defun magit-dash-group-auto-sync (&optional tag)
  "Run autosync asynchronously across all repositories in tag group TAG."
  (interactive)
  (let* ((tag (or tag (magit-dash--read-tag "Autosync tag group: " :require-match t)))
         (all-repos (magit-dash--repos-for-tag tag t))
         (repos (seq-filter #'magit-dash--has-sync-configured-p all-repos)))
    (unless all-repos
      (user-error "No repositories found with tag '%s" (symbol-name tag)))
    (unless repos
      (user-error "No repositories with tag '%s' have auto operations configured" (symbol-name tag)))
    (magit-dash--batch-run
     repos
     #'magit-dash--auto-sync-async
     (format "magit-dash autosync [%s]" tag)
     (lambda (_) (magit-dash--maybe-refresh repos)))))

;;;###autoload
(defun magit-dash-group-fetch (&optional tag)
  "Fetch all repositories in tag group TAG."
  (interactive)
  (let* ((tag (or tag (magit-dash--read-tag "Fetch tag group: " :require-match t)))
         (repos (magit-dash--repos-for-tag tag t)))
    (unless repos
      (user-error "No repositories found with tag '%s" (symbol-name tag)))
    (magit-dash--batch-run
     repos
     (lambda (r cb) (magit-dash--run-operation r :fetch cb))
     (format "magit-dash fetch [%s]" tag)
     (lambda (_) (magit-dash--maybe-refresh repos)))))

;;;###autoload
(defun magit-dash-group-pull (&optional tag)
  "Pull all repositories in tag group TAG."
  (interactive)
  (let* ((tag (or tag (magit-dash--read-tag "Pull tag group: " :require-match t)))
         (repos (magit-dash--repos-for-tag tag t)))
    (unless repos
      (user-error "No repositories found with tag '%s" (symbol-name tag)))
    (magit-dash--batch-run
     repos
     (lambda (r cb) (magit-dash--run-operation r :pull cb))
     (format "magit-dash pull [%s]" tag)
     (lambda (_) (magit-dash--maybe-refresh repos)))))

;;;###autoload
(defun magit-dash-group-push (&optional tag)
  "Push all repositories in tag group TAG that have commits ahead."
  (interactive)
  (let* ((tag (or tag (magit-dash--read-tag "Push tag group: " :require-match t)))
         (all-repos (magit-dash--repos-for-tag tag t))
         (repos (seq-filter #'magit-dash--repo-pushable-p all-repos)))
    (unless all-repos
      (user-error "No repositories found with tag '%s" (symbol-name tag)))
    (if (null repos)
        (message "magit-dash: all %d repositories with tag '%s are up to date (nothing to push)"
                 (length all-repos) (symbol-name tag))
      (magit-dash--batch-run
       repos
       #'magit-dash--push-async
       (format "magit-dash push [%s]" tag)
       (lambda (_) (magit-dash--maybe-refresh repos))))))

;;;###autoload
(defun magit-dash-group-stage-all (&optional tag)
  "Stage all changes in all repositories in tag group TAG."
  (interactive)
  (let* ((tag (or tag (magit-dash--read-tag "Stage all for tag group: " :require-match t)))
         (repos (magit-dash--repos-for-tag tag t)))
    (unless repos
      (user-error "No repositories found with tag '%s" (symbol-name tag)))
    (magit-dash--batch-run
     repos
     (lambda (r cb) (magit-dash--run-operation r :stage-all cb))
     (format "magit-dash stage-all [%s]" tag)
     (lambda (_) (magit-dash--maybe-refresh repos)))))

;;;###autoload
(defun magit-dash-group-run-command (&optional tag command background)
  "Dispatch a command across all repositories in tag group TAG.
Prompts for TAG using annotated completion if not specified.
Prompts for COMMAND from configured commands or custom input.
When BACKGROUND is non-nil, runs each command in background."
  (interactive)
  (let* ((tag (or tag (magit-dash--read-tag "Run command on tag group: " :require-match t)))
         (repos (magit-dash--repos-for-tag tag t)))
    (unless repos
      (user-error "No repositories found with tag '%s" (symbol-name tag)))
    (let* ((all-cmds
            (delete-dups
             (seq-mapcat
              (lambda (r)
                (seq-map (lambda (pair)
                           (if (consp pair) (format "%s" (car pair)) (format "%s" pair)))
                         (magit-dash-repo-commands r)))
              repos)))
           (chosen (or command
                       (if all-cmds
                           (completing-read (format "Run command on [%s] (%d repos): "
                                                    (symbol-name tag) (length repos))
                                            all-cmds nil nil)
                         (read-string (format "Run shell command on [%s] (%d repos): "
                                              (symbol-name tag) (length repos)))))))
      (when (and chosen (not (string-empty-p chosen)))
        (seq-do
         (lambda (repo)
           (let ((cmd-entry
                  (or (seq-find (lambda (c) (equal (format "%s" (car-safe c)) chosen))
                                (magit-dash-repo-commands repo))
                      chosen)))
             (magit-dash--execute-command-on-repo repo cmd-entry background)))
         repos)))))

(defun magit-dash--group-menu-title ()
  "Title for `magit-dash-group-menu'."
  (if-let* ((tag (if (listp magit-dash--tag-filter)
                     (car magit-dash--tag-filter)
                   magit-dash--tag-filter)))
      (format "Tagged Group: %s (%d repos)"
              (symbol-name tag)
              (length (magit-dash--repos-for-tag tag t)))
    "Tagged Repository Groups"))

(transient-define-prefix magit-dash-group-menu ()
  "Operations on a tagged repository group."
  [:description magit-dash--group-menu-title
   ["View & Filter"
    ("v"  "Open group view buffer" magit-dash-group-open-view)
    ("f"  "Filter dashboard by tag" magit-dash-filter-by-tag)
    ("m"  "Mark all in group"      magit-dash-mark-by-tag)]
   ["Dispatch Tasks"
    ("x"  "Run command across group" magit-dash-group-run-command)
    ("a"  "Autosync group"          magit-dash-group-auto-sync)
    ("rf" "Refresh group"           magit-dash-refresh-by-tag)]
   ["Batch Operations"
    ("fa" "Fetch group"             magit-dash-group-fetch)
    ("pa" "Pull group"              magit-dash-group-pull)
    ("pu" "Push group"              magit-dash-group-push)
    ("ta" "Stage all"               magit-dash-group-stage-all)]])

(defun magit-dash-visit-buffer ()
  "Switch to a buffer visiting a file in the repository at point."
  (interactive)
  (let* ((repo (magit-dash--repo-at-point))
         (path (magit-dash-repo-path repo))
         (bufs (seq-filter (lambda (b)
                             (string-prefix-p path (or (buffer-file-name b) "")))
                           (buffer-list))))
    (unless bufs
      (user-error "No open buffers visiting files in %s" (magit-dash-repo-name repo)))
    (switch-to-buffer
     (completing-read "Visit buffer: " (seq-map #'buffer-name bufs) nil t))))

(defun magit-dash-find-file ()
  "Open a file in the repository at point."
  (interactive)
  (find-file
   (read-file-name "Find file: "
                   (file-name-as-directory
                    (magit-dash-repo-path (magit-dash--repo-at-point))))))

(defun magit-dash-switch-branch ()
  "Switch branch in the repository at point via magit."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (magit-dash--with-repo repo
      (call-interactively #'magit-checkout))
    (when repo
      (magit-dash-refresh-repo repo nil t))))

(defun magit-dash-prune-branches ()
  "Prune merged branches and clean worktrees in a repository.
Uses the repository at point when called from a `magit-dash' buffer,
otherwise the git repository containing the current buffer (works from any
`magit' buffer or a file/`dired' buffer inside a repo), and finally prompts
among the registered repositories when neither applies."
  (interactive)
  (let* ((repo (ignore-errors (magit-dash--repo-at-point)))
         (path (magit-dash--resolve-repo-path))
         (main-path (if (fboundp 'magit-dash-worktree-main-dir)
                        (magit-dash-worktree-main-dir path)
                      path))
         (default-directory (file-name-as-directory main-path)))
    (magit-dash-prune-merged-branches)
    (if repo
        (magit-dash-refresh-repo repo nil t)
      (when-let* ((target-dir (file-name-as-directory (expand-file-name main-path)))
                  (r (seq-find (lambda (it)
                                 (let ((it-dir (file-name-as-directory
                                                (expand-file-name (magit-dash-repo-path it)))))
                                   (or (string-equal it-dir target-dir)
                                       (ignore-errors (file-equal-p it-dir main-path)))))
                               (magit-dash--all-repos))))
        (magit-dash-refresh-repo r nil t)))))

;;;###autoload
(defun magit-dash-drop-stashes ()
  "Drop all stashes in a repository.
Uses the repository at point when called from a `magit-dash' buffer,
otherwise the git repository containing the current buffer (works from any
`magit' buffer or a file/`dired' buffer inside a repo), and finally prompts
among the registered repositories when neither applies."
  (interactive)
  (let ((default-directory (file-name-as-directory (magit-dash--resolve-repo-path))))
    (call-interactively #'magit-stash-clear)))

;;;###autoload
(defun magit-dash-save-project-buffers ()
  "Save all project buffers for the repository.
Prefers using projectile if `projectile-mode' and falls back to `project.el'.
Uses the repository at point when called from a `magit-dash' buffer,
otherwise the git repository containing the current buffer, and finally
prompts among the registered repositories when neither applies."
  (interactive)
  (let* ((path (magit-dash--resolve-repo-path))
         (default-directory (file-name-as-directory (expand-file-name path))))
    (if (and (bound-and-true-p projectile-mode)
             (fboundp 'projectile-save-project-buffers))
        (projectile-save-project-buffers)
      ;; otherwwie fall back to project.el
      (seq-do (lambda (buf)
                (with-current-buffer buf
                  (when (and buffer-file-name (buffer-modified-p))
                    (save-buffer))))
	      (when-let* ((pr (project-current)))
		(project-buffers pr))))))

;;;###autoload
(defalias 'magit-dash-projectile-save-project-buffers #'magit-dash-save-project-buffers)
(defun magit-dash--at-worktree-p ()
  "Return non-nil when the repo at point is a worktree."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (magit-dash-repo-worktree repo)))

(defun magit-dash-worktree-add ()
  "Add a new worktree for the repository at point via magit."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (when (magit-dash-repo-worktree repo)
      (user-error "Cannot add a worktree from a worktree entry"))
    (magit-dash--with-repo repo
      (call-interactively #'magit-worktree-checkout))
    (magit-dash-refresh)))

(defun magit-dash-worktree-delete ()
  "Delete the worktree at point via magit.
Signals `user-error' when the current row is not a worktree,
or when the worktree contains uncommitted changes."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (unless (magit-dash-repo-worktree repo)
      (user-error "Not a worktree row; use 'k' only on worktree entries"))
    (let ((path (magit-dash-repo-path repo)))
      (when (and (fboundp 'magit-dash-worktree-uncommitted-changes-p)
                 (magit-dash-worktree-uncommitted-changes-p path))
        (user-error "Cannot delete worktree %s: contains uncommitted changes" path)))
    (magit-dash--with-repo repo
      (magit-worktree-delete (magit-dash-repo-path repo)))
    (magit-dash-refresh)))

(defun magit-dash-agent-shell ()
  "Switch to an `agent-shell' session for the repository at point."
  (interactive)
  (magit-dash--with-repo (magit-dash--repo-at-point)
    (call-interactively #'agent-shell-menu-switch-project-session)))

(defun magit-dash-agent-shell-new (&optional config)
  "Start a new `agent-shell' session in the repository at point.
Prompts for agent config and session choice before creating the shell."
  (interactive)
  (magit-dash--with-repo (magit-dash--repo-at-point)
    (let* ((cfg (or config
                    (when (fboundp 'agent-shell-select-config)
                      (agent-shell-select-config :prompt "Start agent: "))
                    (when (fboundp 'agent-shell--auto-preferred-config)
                      (agent-shell--auto-preferred-config))))
           (choices '("New session" "Load previous session"))
           (choice (completing-read "Session (default New session): "
                                    choices nil t nil nil "New session"))
           (strategy (if (string-equal choice "Load previous session")
                         'prompt
                       'new)))
      (if (fboundp 'agent-shell--start)
          (agent-shell--start :config cfg
                              :session-strategy strategy
                              :new-session (eq strategy 'new))
        (agent-shell-new-shell)))))

(defun magit-dash-agent-shell-queue ()
  "Open the `agent-shell' queue buffer."
  (interactive)
  (call-interactively #'agent-shell-queue-buffer-open))

(defun magit-dash-agent-shell-workflow ()
  "Invoke workflow selection for the repository at point."
  (interactive)
  (magit-dash--with-repo (magit-dash--repo-at-point)
    (cond
     ((fboundp 'agent-shell-workflow-select)
      (call-interactively #'agent-shell-workflow-select))
     ((fboundp 'agent-shell-workflow-dispatch-menu)
      (call-interactively #'agent-shell-workflow-dispatch-menu)))))
(defun magit-dash-gh-ci-fetch-at-point ()
  "Fetch CI status for the repository at point and refresh its dashboard row."
  (interactive)
  (when-let* ((repo (magit-dash--repo-at-point)))
    (magit-dash-gh-ci-fetch repo (lambda (_) (magit-dash--update-entry repo)))))

(defun magit-dash-gh-ci-open-at-point ()
  "Open the most recent CI run in the browser for the repository at point."
  (interactive)
  (when-let* ((repo (magit-dash--repo-at-point)))
    (magit-dash-gh-ci-open-last-run repo)))

(defun magit-dash-gh-ci-fix-at-point ()
  "Dispatch a fix-CI agent task for the repository at point."
  (interactive)
  (when-let* ((repo (magit-dash--repo-at-point)))
    (magit-dash-ci-dispatch-fix-operation repo)))

(defun magit-dash--repo-has-ci-p ()
  "Return non-nil when the repository at point has :include-ci set."
  (when-let* ((repo (magit-dash--repo-at-point)))
    (magit-dash-repo-include-ci repo)))

(defun magit-dash--repo-has-ci-status-p ()
  "Return non-nil when the repository at point has cached CI status."
  (when-let* ((repo (magit-dash--repo-at-point))
              (ci (magit-dash-gh--cache-get (magit-dash-repo-path repo) :ci-status)))
    (and (not (plist-get ci :none)))))

(transient-define-prefix magit-dash-ci-menu ()
  "Actions for GitHub CI for repository at point."
  ["CI Operations"
   ("f"  "Fetch CI status" magit-dash-gh-ci-fetch-at-point
    :inapt-if-not magit-dash--repo-has-ci-p)
   ("o"  "Open last run"   magit-dash-gh-ci-open-at-point
    :inapt-if-not magit-dash--repo-has-ci-status-p)
   ("x"  "Fix CI (agent)"  magit-dash-gh-ci-fix-at-point
    :inapt-if-not magit-dash--repo-has-ci-p)
   ("w"  "Trigger workflow" magit-dash-gh-workflow-run
    :inapt-if-not magit-dash--repo-has-ci-p)])

;;;###autoload
(defun magit-dash-open-other-window ()
  "Open the repository dashboard buffer in another window.
Signals `user-error' when `magit-dash-repo-list' is empty."
  (let ((current-prefix-arg t))
    (magit-dash-open)))

;;;###autoload
(defun magit-dash-open ()
  "Open the repository dashboard buffer in another window.
Signals `user-error' when `magit-dash-repo-list' is empty."
  (interactive)

  (with-magit-dash
   (if current-prefix-arg
       (pop-to-buffer buf))
   (pop-to-buffer-same-window buf)))

;;;###autoload
(defalias 'magit-dash #'magit-dash-open)

(defun magit-dash-repo-ensure-configuration ()
  "Raise `user-error' when there is not `magit-dash-repo-list' defined."
  (unless magit-dash-repo-list
    (user-error "No repositories registered; use `magit-dash-register'")))

(defun magit-dash-create ()
  (with-magit-dash
   (message "magit-dash-repo-dashobard: your (miss)adventure awaits!")))

;;;; Transient predicates

(defun magit-dash--dirty-or-unknown-p ()
  "Return non-nil when repo at point is dirty or stats are not yet cached."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (if-let* ((stats (magit-dash-gh--cache-get (magit-dash-repo-path repo) :stats)))
        (plist-get stats :dirty)
      t)))

(defun magit-dash--has-auto-commit-p ()
  "Return non-nil when the repo at point has :auto-commit configured."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (and (magit-dash-repo-auto-commit repo) t)))

(defun magit-dash--has-auto-sync-p ()
  "Return non-nil when the repo at point has any auto operation configured."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (and (magit-dash--has-sync-configured-p repo) t)))

(defun magit-dash--has-commands-p ()
  "Return non-nil when the repo at point has commands registered or CI enabled."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (or (and (magit-dash-repo-commands repo) t)
        (and (magit-dash-repo-include-ci repo) t))))

(defun magit-dash--repo-at-point-behind-p ()
  "Return non-nil when the repo at point has commits behind its upstream."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point)))
              (stats (magit-dash--get-stats repo)))
    (and (> (or (plist-get stats :behind) 0) 0) t)))

(defun magit-dash--repo-at-point-ahead-p ()
  "Return non-nil when the repo at point has commits ahead of its upstream."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (magit-dash--repo-ahead-p repo)))

(defun magit-dash--can-add-worktree-p ()
  "Return non-nil when at a registered non-worktree repo (can add a worktree)."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (not (magit-dash-repo-worktree repo))))

(defun magit-dash--has-hidden-p ()
  "Return non-nil when at least one repository is hidden in the dashboard."
  (and magit-dash--hidden-paths t))

(defun magit-dash--can-hide-p ()
  "Return non-nil when repos can be hidden (marked exist or point on repo)."
  (or (magit-dash--has-marks-p)
      (magit-dash--repo-at-point-p)))

;;;; Mark/select support

(defun magit-dash--update-entry-for (repo)
  "Regenerate the tabulated-list entry for REPO in `tabulated-list-entries'."
  (let ((new-entry (magit-dash--build-entry repo))
        (path (magit-dash-repo-path repo)))
    (setq tabulated-list-entries
          (seq-map (lambda (entry)
                     (if (equal (magit-dash-repo-path (car entry)) path)
                         new-entry
                       entry))
                   tabulated-list-entries))))

(defun magit-dash-toggle-mark ()
  "Toggle the mark on the repository at point and advance to the next line."
  (interactive)
  (unless (derived-mode-p 'magit-dash-mode)
    (user-error "Not in a magit-dash buffer"))
  (when-let* ((repo (magit-dash--repo-at-point)))
    (let ((path (magit-dash-repo-path repo)))
      (setq magit-dash--marked-paths
            (if (member path magit-dash--marked-paths)
                (delete path magit-dash--marked-paths)
              (cons path magit-dash--marked-paths)))
      (magit-dash--update-entry-for repo)
      (magit-dash--print)
      (forward-line 1))))

(defun magit-dash-unmark-all ()
  "Unmark all repositories in the dashboard."
  (interactive)
  (setq magit-dash--marked-paths nil)
  (when (derived-mode-p 'tabulated-list-mode)
    (setq tabulated-list-entries
          (seq-map (lambda (entry)
                     (magit-dash--build-entry (car entry)))
                   tabulated-list-entries))
    (magit-dash--print)))

;;;###autoload
(defun magit-dash-hide-repo (&optional repo)
  "Hide REPO or repository at point from the list view.
If any repositories are marked, hides all marked repositories and clears marks.
Otherwise, hides REPO if non-nil, or the repository at point.
Use `magit-dash-unhide-all' to restore all hidden repositories."
  (interactive)
  (let ((targets (cond
                  (repo (list repo))
                  ((magit-dash--has-marks-p)
                   (prog1
                       (seq-filter (lambda (r)
                                     (member (magit-dash-repo-path r) magit-dash--marked-paths))
                                   (if (derived-mode-p 'magit-dash-mode)
                                       (seq-map #'car tabulated-list-entries)
                                     magit-dash-repo-list))
                     (setq magit-dash--marked-paths nil)))
                  (t (list (magit-dash--repo-at-point))))))
    (unless targets
      (user-error "No repository selected to hide"))
    (dolist (target targets)
      (let ((path (magit-dash-repo-path target)))
        (unless (member path magit-dash--hidden-paths)
          (push path magit-dash--hidden-paths))))
    (if (derived-mode-p 'magit-dash-mode)
        (progn
          (setq tabulated-list-entries
                (seq-remove (lambda (entry)
                              (member (magit-dash-repo-path (car entry))
                                      magit-dash--hidden-paths))
                            tabulated-list-entries))
          (magit-dash--print))
      (magit-dash-refresh))
    (if (= (length targets) 1)
        (message "Hidden repository: %s ('hc' to clear hidden, 'hu' to unhide)"
                 (magit-dash-repo-name (car targets)))
      (message "Hidden %d repositories ('hc' to clear hidden, 'hu' to unhide)"
               (length targets)))))

(defalias 'magit-dash-hide #'magit-dash-hide-repo)

;;;###autoload
(defun magit-dash-unhide-all ()
  "Restore all hidden repositories to the dashboard view."
  (interactive)
  (let ((count (length magit-dash--hidden-paths)))
    (if (zerop count)
        (message "No repositories are hidden")
      (setq magit-dash--hidden-paths nil)
      (magit-dash-refresh)
      (message "Unhid %d repositor%s" count (if (= count 1) "y" "ies")))))

(defalias 'magit-dash-hide-clear #'magit-dash-unhide-all)

;;;###autoload
(defun magit-dash-unhide-repo (&optional repo)
  "Select a hidden repository using `annotated-completing-read' and restore it.
If REPO is non-nil, restores REPO directly."
  (interactive)
  (unless magit-dash--hidden-paths
    (user-error "No repositories are currently hidden"))
  (let* ((candidates
          (seq-map
           (lambda (path)
             (let* ((r (seq-find (lambda (item) (equal (magit-dash-repo-path item) path))
                                 magit-dash-repo-list))
                    (name (if r (magit-dash-repo-name r)
                            (file-name-nondirectory (directory-file-name path))))
                    (ann (if r (magit-dash--repo-annotation r) path)))
               (cons name (cons ann path))))
           magit-dash--hidden-paths))
         (table (map-into candidates '(hash-table :test equal)))
         (target-path
          (or (and repo (magit-dash-repo-path repo))
              (annotated-completing-read
               table
               :prompt "Unhide repository: "
               :require-match t
               :category 'magit-dash-repo))))
    (when target-path
      (setq magit-dash--hidden-paths (delete target-path magit-dash--hidden-paths))
      (magit-dash-refresh)
      (let ((r (seq-find (lambda (item) (equal (magit-dash-repo-path item) target-path))
                         magit-dash-repo-list)))
        (message "Unhid repository: %s"
                 (if r (magit-dash-repo-name r)
                   (file-name-nondirectory (directory-file-name target-path))))))))

;;;###autoload
(defun magit-dash-hide-select ()
  "Select a repository via `annotated-completing-read' to toggle hidden state.
If the chosen repository is hidden, unhides it.  Otherwise, hides it."
  (interactive)
  (unless magit-dash-repo-list
    (user-error "No registered repositories"))
  (let* ((candidates
          (seq-map
           (lambda (r)
             (let* ((path (magit-dash-repo-path r))
                    (hidden-p (member path magit-dash--hidden-paths))
                    (status (if hidden-p "[hidden] " "[visible] "))
                    (base-ann (magit-dash--repo-annotation r))
                    (ann (concat status base-ann)))
               (cons (magit-dash-repo-name r) (cons ann r))))
           magit-dash-repo-list))
         (table (map-into candidates '(hash-table :test equal)))
         (repo (annotated-completing-read
                table
                :prompt "Toggle repository visibility: "
                :require-match t
                :category 'magit-dash-repo)))
    (when repo
      (let ((path (magit-dash-repo-path repo)))
        (if (member path magit-dash--hidden-paths)
            (progn
              (setq magit-dash--hidden-paths (delete path magit-dash--hidden-paths))
              (magit-dash-refresh)
              (message "Unhid repository: %s" (magit-dash-repo-name repo)))
          (push path magit-dash--hidden-paths)
          (if (derived-mode-p 'magit-dash-mode)
              (progn
                (setq tabulated-list-entries
                      (seq-remove (lambda (entry)
                                    (equal (magit-dash-repo-path (car entry)) path))
                                  tabulated-list-entries))
                (magit-dash--print))
            (magit-dash-refresh))
          (message "Hidden repository: %s ('hc' to clear hidden, 'hu' to unhide)"
                   (magit-dash-repo-name repo)))))))

(defun magit-dash--batch-all-active-p ()
  "Return non-nil when batch mode is active for all visible repos.
Returns nil when repositories are marked (since marked repos take precedence)."
  (not (magit-dash--has-marks-p)))

(defun magit-dash--visible-repos ()
  "Return the list of repositories currently visible in the dashboard buffer.
Extracts repo objects from visible lines in the buffer, excluding any lines
marked invisible or hidden via `magit-dash--hidden-paths'."
  (if (derived-mode-p 'magit-dash-mode)
      (let (repos)
        (save-excursion
          (save-restriction
            (goto-char (point-min))
            (while (not (eobp))
              (unless (invisible-p (point))
                (when-let* ((id (tabulated-list-get-id))
                            (repo (cond
                                   ((magit-dash-repo-p id) id)
                                   ((and (consp id) (plist-get id :repo))
                                    (plist-get id :repo)))))
                  (when (and (magit-dash-repo-p repo)
                             (not (member (magit-dash-repo-path repo) magit-dash--hidden-paths)))
                    (push repo repos))))
              (forward-line 1))))
        (nreverse (delete-dups repos)))
    (seq-filter (lambda (r)
                  (and (not (member (magit-dash-repo-path r) magit-dash--hidden-paths))
                       (magit-dash--repo-matches-tag-filter-p r (bound-and-true-p magit-dash--tag-filter))))
                magit-dash-repo-list)))

(defun magit-dash--effective-repos ()
  "Return marked repos if any are marked and visible.
If none are marked, return all visible repos in the dashboard buffer.
Batch operations ONLY impact visible items."
  (let ((all (magit-dash--visible-repos)))
    (if magit-dash--marked-paths
        (seq-filter (lambda (r)
                      (member (magit-dash-repo-path r)
                              magit-dash--marked-paths))
                    all)
      all)))

(defun magit-dash--has-marks-p ()
  "Return non-nil when at least one repository is marked."
  (and magit-dash--marked-paths t))

(defun magit-dash--batch-enabled-p ()
  "Return non-nil when batch operations are permitted.
True when there are any visible repositories."
  (and (magit-dash--effective-repos) t))

(defun magit-dash-toggle-batch-all ()
  "Toggle whether batch operations act on all repos or only marked ones.
When enabled, all repos visible in the dashboard are targeted.
When disabled, only explicitly marked repos are targeted."
  (interactive)
  (setq magit-dash--batch-all (not magit-dash--batch-all))
  (message "Batch: %s" (if magit-dash--batch-all "all repos" "marked repos only (disabled)")))

(defun magit-dash-fetch-all ()
  "Fetch marked repos asynchronously, or all visible repos when none marked."
  (interactive)
  (let ((repos (magit-dash--effective-repos)))
    (unless repos
      (user-error "No repositories to fetch"))
    (magit-dash--batch-run
     repos
     #'magit-dash--fetch-async
     "magit-gh fetch"
     (lambda (_) (magit-dash--maybe-refresh repos)))))

(defun magit-dash-pull-all ()
  "Asynchronously pull marked repos, or all visible repos when none are marked.
Prioritizes repositories with failing CI, pending status, then recently changed."
  (interactive)
  (let ((repos (magit-dash--sort-repos-by-priority (magit-dash--effective-repos))))
    (unless repos
      (user-error "No repositories to pull"))
    (magit-dash--batch-run
     repos
     #'magit-dash--pull-async
     "magit-gh pull"
     (lambda (_) (magit-dash--maybe-refresh repos)))))

(defun magit-dash-push-all ()
  "Asynchronously push marked repos, or all visible repos when none are marked.
Skips repos without commits ahead or without tracking branches configured."
  (interactive)
  (let* ((all-repos (magit-dash--effective-repos))
         (repos (seq-filter #'magit-dash--repo-pushable-p all-repos)))
    (unless repos
      (if all-repos
          (message "magit-dash: all %d repositories are up to date (nothing to push)"
                   (length all-repos))
        (user-error "No repositories to push")))
    (when repos
      (magit-dash--batch-run
       repos
       #'magit-dash--push-async
       "magit-gh push"
       (lambda (_) (magit-dash--maybe-refresh repos))))))

;;;###autoload
(defun magit-dash-stage-all-all ()
  "Stage all changes in marked repos, or visible repos when none marked."
  (interactive)
  (let ((repos (magit-dash--effective-repos)))
    (unless repos
      (user-error "No repositories to stage"))
    (magit-dash--batch-run
     repos
     (lambda (r cb) (magit-dash--run-operation r :stage-all cb))
     "magit-dash stage-all"
     (lambda (_) (magit-dash--maybe-refresh repos)))))

(defalias 'magit-dash-stage-all-marked #'magit-dash-stage-all-all)
(defalias 'magit-dash-stage-all-batch #'magit-dash-stage-all-all)

(defun magit-dash-submodule-update-all ()
  "Run git submodule update --init --recursive for marked or all visible repos."
  (interactive)
  (let ((repos (magit-dash--effective-repos)))
    (unless repos
      (user-error "No repositories to update"))
    (magit-dash--batch-run
     repos
     #'magit-dash--submodule-update-async
     "magit-dash submodule-update"
     (lambda (_) (magit-dash--maybe-refresh repos)))))

(defun magit-dash-submodule-pull-all ()
  "Run git submodule foreach git pull for marked or all visible repos."
  (interactive)
  (let ((repos (magit-dash--effective-repos)))
    (unless repos
      (user-error "No repositories to update"))
    (magit-dash--batch-run
     repos
     #'magit-dash--submodule-pull-async
     "magit-dash submodule-pull"
     (lambda (_) (magit-dash--maybe-refresh repos)))))

;;;; Transient menus

(defun magit-dash--agent-shell-project-buffers-p ()
  "Return non-nil when `agent-shell' buffers exist for the repo at point."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point)))
	      (default-directory (file-name-as-directory (magit-dash-repo-path repo))))
    (or (and (fboundp 'agent-shell-menu-project-buffers)
             (agent-shell-menu-project-buffers)
             t)
        (and (fboundp 'agent-shell-workflow--project-buffers)
             (agent-shell-workflow--project-buffers default-directory)
             t)
        nil)))
(defun magit-dash--agent-shell-workflow-available-p ()
  "Return non-nil when workflow selection is available."
  (or (fboundp 'agent-shell-workflow-select)
      (fboundp 'agent-shell-workflow-dispatch-menu)))

(defun magit-dash--batch-group-title (&optional _)
  "Return the title for the Batch group in `magit-dash-menu'.
Indicates whether batch operations target all repos or marked repos."
  (if (magit-dash--has-marks-p) "Batch (marked)" "Batch (all)"))

(defalias 'magit-dash--name-column-title #'magit-dash--batch-group-title)

(transient-define-prefix magit-dash-menu ()
  "Actions for the repository at point in the repo dashboard."
  [["Navigate"
    ("vb"  "Visit buffer"    magit-dash-visit-buffer
     :inapt-if-not magit-dash--repo-at-point-p)
    ("ff"  "Find file"       magit-dash-find-file
     :inapt-if-not magit-dash--repo-at-point-p)
    ("gb"  "Switch branch"   magit-dash-switch-branch
     :inapt-if-not magit-dash--repo-at-point-p)]
   ["CI"
    ("cf"  "Fetch CI status" magit-dash-gh-ci-fetch-at-point
     :inapt-if-not magit-dash--repo-has-ci-p)
    ("co"  "Open last run"   magit-dash-gh-ci-open-at-point
     :inapt-if-not magit-dash--repo-has-ci-status-p)
    ("cx"  "Fix CI (agent)"  magit-dash-gh-ci-fix-at-point
     :inapt-if-not magit-dash--repo-has-ci-p)
    ("cw"  "Trigger workflow" magit-dash-gh-workflow-run
     :inapt-if-not magit-dash--repo-has-ci-p)]
   ["Upstream"
    :if magit-dash--repo-has-upstream-repo-p
    ("um"  "Merge upstream"  magit-dash-upstream-merge)
    ("ur"  "Rebase upstream" magit-dash-upstream-rebase)
    ("uf"  "Fetch upstream"  magit-dash-upstream-fetch)]
   ["Agent Shell"
    ("av"  "Actions needing attention" magit-dash-actionable-open)
    ("as"  "Open"           magit-dash-agent-shell
     :inapt-if-not magit-dash--agent-shell-project-buffers-p)
    ("an"  "New"            magit-dash-agent-shell-new)
    ("aq"  "Queue"          magit-dash-agent-shell-queue)
    ("ap"  "Workflows" magit-dash-agent-shell-workflow
     :inapt-if-not magit-dash--agent-shell-workflow-available-p)]
   ["Cache"
    ("ci"  "Info"      magit-dash-cache-info)
    ("cr"  "Reset"     magit-dash-cache-reset-at-point
     :inapt-if-not magit-dash--repo-at-point-p)
    ("ce"  "Reset all"       magit-dash-cache-reset-all)]]
  [["Repository"
    ("!"   "Magit dispatch"  magit-dash-magit-dispatch
     :inapt-if-not magit-dash--repo-at-point-p)
    ("RET" "Status"          magit-dash-magit-status
     :inapt-if-not magit-dash--repo-at-point-p)
    ("d"   "Diff…"           magit-dash-magit-diff
     :inapt-if-not magit-dash--dirty-or-unknown-p)
    ("lc"  "Log (current)"   magit-dash-magit-log
     :inapt-if-not magit-dash--repo-at-point-p)
    ("lf"  "Log…"            magit-dash-magit-log-full
     :inapt-if-not magit-dash--repo-at-point-p)
    ("cc" "Commit"          magit-dash-magit-commit
     :inapt-if-not magit-dash--dirty-or-unknown-p)
    ("ga"  "Stage all"       magit-dash-stage-all
     :inapt-if-not magit-dash--dirty-or-unknown-p)
    ("fr"  "Fetch"           magit-dash-fetch
     :inapt-if-not magit-dash--repo-at-point-p)
    ("rp"  "Pull"            magit-dash-pull
     :inapt-if-not magit-dash--repo-at-point-p)
    ("rs"  "Push"            magit-dash-push
     :inapt-if-not magit-dash--repo-at-point-ahead-p)
    ("y"   "Prune branches"  magit-dash-prune-branches
     :inapt-if-not magit-dash--repo-at-point-p)
    ("rr"  (lambda () (if (magit-dash--has-marks-p) "Refresh marked" "Refresh row"))
     magit-dash-refresh-repo
     :inapt-if-not magit-dash--can-refresh-repo-p)]
   ["Worktree"
    ("wp"  "Dispatch task"   magit-dash-open-worktree-dispatch
     :inapt-if-not magit-dash--repo-at-point-p)
    ("wa"  "Add"             magit-dash-worktree-add
     :inapt-if-not magit-dash--can-add-worktree-p)
    ("wr"  "From PR"         magit-dash-worktree-from-pr
     :inapt-if-not magit-dash--repo-at-point-p)
    ("wd"  "Delete"          magit-dash-worktree-delete
     :inapt-if-not magit-dash--at-worktree-p)
    ("ws"  "Sync symlinks"   magit-dash-worktree-sync-symlinks
     :inapt-if-not magit-dash--repo-at-point-p)
    ("wt"  "Toggle worktrees" magit-dash-toggle-worktrees
     :inapt-if-not magit-dash--repo-at-point-p)
    ("wc"  "Plan PR chain merge" magit-dash-chain-plan
     :inapt-if-not magit-dash--repo-at-point-p)
    ("wm"  "Resume PR chain"  magit-dash-chain-resume)
    ("wv"  "View stack"       magit-dash-stack-open)]
   [:description magit-dash--batch-group-title
    ("SPC" "Toggle mark"     magit-dash-toggle-mark
     :inapt-if-not magit-dash--repo-at-point-p
     :transient t)
    ("mt"  "Mark by tag"     magit-dash-mark-by-tag
     :transient t)
    ("mc"  "Clear marks"     magit-dash-unmark-all
     :inapt-if-not magit-dash--has-marks-p
     :transient t)
    ("mu"  "Mark unpushed"   magit-dash-mark-unpushed
     :transient t)
    ("tn"  "New tag"         magit-dash-add-tag)
    ("tr"  "Remove tag"      magit-dash-remove-tag)
    ("td"  "Clear tags"      magit-dash-clear-tags)
    ("ad"  "Dispatch workflow" magit-dash-dispatch-workflow)
    ("fa"  "Fetch all"       magit-dash-fetch-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("pa"  "Pull all"        magit-dash-pull-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("pu"  "Push all"        magit-dash-push-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("sa"  "Sync all"        magit-dash-sync-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("ta"  "Stage all"       magit-dash-stage-all-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("ca"  "Commit all"      magit-dash-commit-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("aa"  "Autosync all"    magit-dash-auto-sync
     :inapt-if-not magit-dash--batch-enabled-p)
    ("su"  "Update submodules" magit-dash-submodule-update-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("sp"  "Pull submodules"   magit-dash-submodule-pull-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("ba"  (lambda () (if (magit-dash--has-marks-p) "Bootstrap marked" "Bootstrap all"))
     magit-dash-bootstrap-marked
     :if magit-dash--has-missing-bootstrap-repos-p)
    ("rm" "Refresh marked"   magit-dash-refresh-marked
     :inapt-if-not magit-dash--has-marks-p)]
   ["Manage"
    ("ac"  "Auto-commit"     magit-dash-commit
     :inapt-if-not magit-dash--has-auto-commit-p)
    ("sy"  "Sync one"        magit-dash-sync
     :inapt-if-not magit-dash--has-auto-sync-p)
    ("sd"  "Default sync"    magit-dash-default-sync)
    ("vo"  "View operations" magit-dash-view-operations)
    ("so"  "Stop operations" magit-dash-stop-operations)
    ("cl"  "Clone repo"      magit-dash-clone-repo
     :inapt-if-not magit-dash--repo-at-point-p)
    ("cm"  "Clone missing"   magit-dash-clone-all-missing
     :inapt-if-not magit-dash--has-missing-repos-p)
    ("cb"  "Bootstrap repo"  magit-dash-bootstrap-repo
     :inapt-if-not magit-dash--repo-has-upstream-p)
    ("xc"  "Run command"     magit-dash-run-command
     :inapt-if-not magit-dash--has-commands-p)
    ("xb"  "Run in background" magit-dash-run-command-background
     :inapt-if-not magit-dash--has-commands-p)
    ("ps"  "project save buffers" magit-dash-save-project-buffers
     :inapt-if-not magit-dash--repo-at-point-p)
    ("sb"  "Bump submodules" magit-dash-bump-submodules-menu
     :inapt-if-not magit-dash--repo-at-point-p)]
   ["Dashboard"
    ("pr"  "PR views…"       magit-dash-pr-views-menu)
    ("nt"  (lambda () (if magit-dash--tag-filter
                         (format "Filter by tag [%s]"
                                 (if (listp magit-dash--tag-filter)
                                     (mapconcat #'symbol-name magit-dash--tag-filter ", ")
                                   magit-dash--tag-filter))
                       "Filter by tag"))
     magit-dash-filter-by-tag)
    ("nu"  (lambda () (if (eq magit-dash--predicate-filter 'unpushed)
                         "Filter unpushed [on]"
                       "Filter unpushed"))
     magit-dash-filter-unpushed)
    ("nm"  "Filter marked"   magit-dash-filter-marked
     :inapt-if-not magit-dash--has-marks-p)
    ("nv"  "Open view buffer" magit-dash-open-view)
    ("nx"  "Clear filters"   magit-dash-clear-filter
     :inapt-if-not (lambda () (or magit-dash--tag-filter magit-dash--predicate-filter)))
    ("tg"  "Group menu…"     magit-dash-group-menu)
    ("rt"  "Refresh by tag"  magit-dash-refresh-by-tag)
    ("hr"  (lambda () (if (magit-dash--has-marks-p) "Hide marked" "Hide repo"))
     magit-dash-hide-repo
     :inapt-if-not magit-dash--can-hide-p
     :transient t)
    ("hc"  (lambda () (format "Hide clear%s"
                              (if magit-dash--hidden-paths
                                  (format " (%d)" (length magit-dash--hidden-paths))
                                "")))
     magit-dash-unhide-all
     :inapt-if-not magit-dash--has-hidden-p
     :transient t)
    ("hu"  "Unhide repo…"
     magit-dash-unhide-repo
     :inapt-if-not magit-dash--has-hidden-p
     :transient t)
    ("hs"  "Hide/unhide (ACR)…"
     magit-dash-hide-select
     :transient t)
    ("nb"  (lambda () (if magit-dash-render-branch-name-as-basename
                          "Branch basename [on]"
                        "Branch basename [off]"))
     magit-dash-toggle-branch-basename
     :transient t)
    ("tc"  "Toggle column"   magit-dash-toggle-column)
    ("gg"  "Refresh"         magit-dash-refresh)
    ("q"   "Quit"            magit-dash-quit-window)]])

(defun magit-dash-default-sync ()
  "Run safe default sync (fetch, safe pull, safe push) for target repo(s)."
  (interactive)
  (let ((repos (or (magit-dash--effective-repos)
                   (when-let* ((r (ignore-errors (magit-dash--repo-at-point))))
                     (list r)))))
    (unless repos
      (user-error "No repositories selected for default sync"))
    (magit-dash--batch-run
     repos
     #'magit-dash--default-sync-async
     "magit-dash default sync"
     (lambda (_) (magit-dash--maybe-refresh)))))

(defun magit-dash-view-operations ()
  "Display active and recent magit-dash operations in a buffer."
  (interactive)
  (let ((buf (get-buffer-create "*magit-dash-operations*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "=== Magit Dash Operations ===\n\n")
      (insert "ACTIVE OPERATIONS:\n")
      (if (= 0 (hash-table-count magit-dash--active-operations))
          (insert "  (none)\n")
        (maphash
         (lambda (_path op)
           (let ((elapsed (- (float-time) (magit-dash-operation-started-at op))))
             (insert (format "  [%s] %s: %s (running for %.1fs)\n"
                             (magit-dash-operation-id op)
                             (magit-dash-operation-repo-name op)
                             (magit-dash-operation-label op)
                             elapsed))))
         magit-dash--active-operations))
      (insert "\nRECENT OPERATION HISTORY:\n")
      (if (null magit-dash--operation-history)
          (insert "  (none)\n")
        (dolist (op (seq-take magit-dash--operation-history 20))
          (let* ((duration (if (magit-dash-operation-finished-at op)
                              (- (magit-dash-operation-finished-at op)
                                 (magit-dash-operation-started-at op))
                            0.0))
                 (detail (or (magit-dash-operation-error-text op) "")))
            (insert (format "  [%s] %s: %s → %s (%.1fs)%s\n"
                            (magit-dash-operation-id op)
                            (magit-dash-operation-repo-name op)
                            (magit-dash-operation-label op)
                            (symbol-name (magit-dash-operation-status op))
                            duration
                            (if (string-empty-p detail) "" (format " (%s)" detail)))))))
      (special-mode))
    (display-buffer buf)))

(defun magit-dash-stop-operations ()
  "Stop and cancel all currently running magit-dash operations."
  (interactive)
  (magit-dash--cancel-refresh-jobs)
  (let ((count 0))
    (maphash
     (lambda (_path op)
       (when (eq (magit-dash-operation-status op) :running)
         (cl-incf count)
         (seq-do (lambda (proc)
                   (when (process-live-p proc)
                     (delete-process proc)))
                 (magit-dash-operation-process op))
         (setf (magit-dash-operation-status op) :cancelled)
         (setf (magit-dash-operation-finished-at op) (float-time))
         (setf (magit-dash-operation-error-text op) "stopped by user")
         (push op magit-dash--operation-history)))
     magit-dash--active-operations)
    (clrhash magit-dash--active-operations)
    (message "magit-dash: stopped %d running operation(s)" count)))

(defun magit-dash-reset-operations ()
  "Reset all operation state, stopping active operations and clearing history."
  (interactive)
  (magit-dash-stop-operations)
  (clrhash magit-dash--active-operations)
  (setq magit-dash--operation-history nil)
  (setq magit-dash--operation-counter 0)
  (message "magit-dash: operation state reset"))



;;;###autoload
(defun magit-dash-quit-window (&optional kill window)
  "Quit the current dashboard window and cancel any active refresh jobs.
KILL and WINDOW are passed through to `quit-window'."
  (interactive "P")
  (when (derived-mode-p 'magit-dash-mode)
    (magit-dash--cancel-refresh-jobs))
  (if (or kill (one-window-p)
          (not (derived-mode-p 'magit-dash-mode
                               'magit-dash-pr-dashboard-mode)))
      (quit-window kill window)
    (delete-window (or window (selected-window)))))

(defalias 'magit-dash--quit-window-advice
  (lambda (orig-fn &optional kill window)
    (when (derived-mode-p 'magit-dash-mode)
      (magit-dash--cancel-refresh-jobs))
    (if (or kill (one-window-p)
            (not (derived-mode-p 'magit-dash-mode
                                 'magit-dash-pr-dashboard-mode)))
        (funcall orig-fn kill window)
      (delete-window (or window (selected-window))))))

;; `magit-dash-global-mode' is defined in `magit-dash-global.el'.

;;;###autoload
(defun magit-dash-install-nerd-icons ()
  "Register `magit-dash' mode icons in `nerd-icons-mode-icon-alist'."
  (interactive)
  (when (boundp 'nerd-icons-mode-icon-alist)
    (add-to-list 'nerd-icons-mode-icon-alist '(magit-dash-mode nerd-icons-devicon "nf-dev-git" :face nerd-icons-orange))
    (add-to-list 'nerd-icons-mode-icon-alist '(magit-dash-pr-dashboard-mode nerd-icons-octicon "nf-oct-git_pull_request" :face nerd-icons-orange))
    (add-to-list 'nerd-icons-mode-icon-alist '(magit-dash-gh-actions-log-mode nerd-icons-octicon "nf-oct-workflow" :face nerd-icons-orange))))

(when (boundp 'nerd-icons-mode-icon-alist)
  (magit-dash-install-nerd-icons))

(provide 'magit-dash)

;;; magit-dash.el ends here
