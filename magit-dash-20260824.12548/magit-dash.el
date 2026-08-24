;;; magit-dash.el --- Repository and PR dashboards for magit-gh -*- lexical-binding: t -*-

;; Author: sam kleinman
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (magit "4.0") (transient "0.4"))
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
;; `magit-dash-gh-pr-dashboard-open' — a tabulated-list view of pull requests with
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
(require 'transient)
(require 'magit)
(require 'project)
(require 'annotated-completing-read)
(require 'sprite)

(require 'magit-dash-gh-ci)
(require 'magit-dash-gh-pr)
(require 'magit-dash-gh)
(require 'magit-dash-open)
(require 'magit-dash-submodules)
(require 'magit-dash-worktree)
(require 'magit-dash-status)

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
(declare-function magit-worktree-checkout "magit-worktree")
(declare-function magit-worktree-delete "magit-worktree")
(declare-function agent-shell-switch-buffer "agent-shell")
(declare-function agent-shell-menu-switch-project-session "agent-shell-menu")
(declare-function magit-dash-gh-ci-open "magit-dash-gh-ci")
(declare-function magit-dash-gh-ci-fetch "magit-dash-gh-ci")
(declare-function magit-dash-gh-workflow-run "magit-dash-gh-actions")
(declare-function builder-compile-project "builder")
(declare-function magit-gh-pr-dash "magit-dash-gh-pr")

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
  (remote-sync nil))

(defalias 'magit-dash-repo-upstream-repo #'magit-dash-repo-repo)
(defalias 'magit-dash-repo-upstream #'magit-dash-repo-repo)
(defalias 'magit-dash-repo-remote-url #'magit-dash-repo-clone-url)
(defalias 'magit-dash-repo-symlinks #'magit-dash-repo-worktree-symlinks)

(defvar magit-dash-repo-list '()
  "List of `magit-dash-repo' structs registered for dashboard display.
Use `magit-dash-register' to add entries.")


(defcustom magit-dash-run-command-in-background nil
  "When non-nil, `magit-dash-run-command' executes commands in the background by default."
  :type 'boolean
  :group 'magit-dash)

(declare-function magit-dash-register-sync-timer "magit-dash-timer")

;;;; Hooks and operations

(defconst magit-dash--hook-operations '(:fetch :pull :commit :push :sync)
  "Fixed vocabulary of operations a `:hooks' or `magit-dash-global-hooks' entry may key on.")

(defconst magit-dash--hook-slots '(:pre :post :operation)
  "Fixed vocabulary of slots within a single operation's hooks plist.")

(defconst magit-dash--remote-sync-slots '(:hosts :branch :path :merge-method)
  "Fixed vocabulary of slots within a repo's `:remote-sync' plist.")

(defun magit-dash--validate-remote-sync (remote-sync)
  "Signal `user-error' when REMOTE-SYNC uses keys outside `magit-dash--remote-sync-slots'
or has an invalid `:merge-method'."
  (when remote-sync
    (unless (listp remote-sync)
      (user-error "magit-dash: :remote-sync must be a plist"))
    (seq-do
     (lambda (pair)
       (let ((slot (car pair)))
         (unless (memq slot magit-dash--remote-sync-slots)
           (user-error "magit-dash: unknown remote-sync slot %s" slot))))
     (seq-partition remote-sync 2))
    (when-let ((method (plist-get remote-sync :merge-method)))
      (unless (memq (if (symbolp method) method (intern method)) '(merge rebase))
        (user-error "magit-dash: :merge-method must be 'merge or 'rebase, got %S" method)))))

(defvar magit-dash-global-hooks nil
  "Plist of `:pre'/`:post' hooks applied to every repo's operations.
Same shape as a repo's `:hooks', keyed by operation (`:fetch'/`:pull'/
`:commit'/`:push'/`:sync'), each value a plist of `:pre'/`:post' target
lists.  `:operation' is not valid here — a global operation override
would replace every repo's operation with the same thing, which is not
a real use case.  Set with `magit-dash-set-global-hooks' so the shape is
validated; do not `setq' directly.")

(defun magit-dash--validate-hooks (hooks &optional allow-operation)
  "Signal `user-error' when HOOKS uses an operation or slot outside the
fixed vocabulary of `magit-dash--hook-operations'/`magit-dash--hook-slots'.
`:operation' is only valid per-repo; pass ALLOW-OPERATION non-nil when
validating a `:hooks' argument, nil for `magit-dash-global-hooks'."
  (seq-do
   (lambda (op-pair)
     (let ((op (car op-pair)) (op-plist (cadr op-pair)))
       (unless (memq op magit-dash--hook-operations)
         (user-error "magit-dash: unknown hook operation %s" op))
       (seq-do
        (lambda (slot-pair)
          (let ((slot (car slot-pair)))
            (unless (memq slot magit-dash--hook-slots)
              (user-error "magit-dash: unknown hook slot %s" slot))
            (when (and (eq slot :operation) (not allow-operation))
              (user-error "magit-dash: :operation is not valid in global hooks"))))
        (seq-partition op-plist 2))))
   (seq-partition hooks 2)))

(defun magit-dash-set-global-hooks (hooks)
  "Validate HOOKS and set `magit-dash-global-hooks' to it."
  (magit-dash--validate-hooks hooks)
  (setq magit-dash-global-hooks hooks))

(defun magit-dash--merge-hook-op (base addition)
  "Merge op-plists BASE and ADDITION, preferring BASE's `:operation' and
appending `:pre'/`:post' lists (BASE's entries first)."
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

(cl-defun magit-dash-register (&key name path repo include-prs include-ci auto-fetch auto-pull auto-commit auto-push auto-sync-command hooks tags commands sort-hint worktree sync-branches timer clone-url remote-url worktree-symlinks symlinks remote-sync)
  "Register or replace a repository with NAME at absolute PATH.
Replaces any existing entry with the same name or path.

Keyword arguments:
  :repo           path (or URL) of upstream repository for bootstrapping.
  :include-prs    include in PR dashboard fetches.
  :include-ci     include in CI status column fetches (GitHub Actions).
  :auto-fetch     non-nil — run git fetch --all during auto-sync.
  :auto-pull      non-nil — run git pull during auto-sync (implies fetch).
                  Respects :sync-branches.
  :auto-commit    nil | t | FUNCTION — stage and commit dirty working tree.
                  FUNCTION receives the repo struct and returns a commit
                  message string.
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
  :tags           list of symbols for filtering in the dashboard.
  :commands       alist of (LABEL . FUNCTION) for the repo command picker.
  :sort-hint      number controlling display order; lower values appear first.
                  Repos without a sort-hint appear after all sorted repos.
  :worktree       non-nil when this entry represents a git worktree.
  :timer          plist forwarded to `magit-dash-register-sync-timer' (requires
                  magit-dash-timer).  Example: \='(:kind idle :idle-delay 300).
  :clone-url      URL to clone repository from when missing on disk.
  :remote-url     alias for :clone-url.
  :worktree-symlinks  list of relative paths to symlink from main repo to worktrees.
  :symlinks       alias for :worktree-symlinks.
  :remote-sync    plist (:hosts, :branch, :path, :merge-method) declaring remote sync mirrors."
  (unless (and name path)
    (user-error "must specify name (%s) and path (%s)" name path))

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

  (let* ((abs-path (expand-file-name path))
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
         (effective-commands (append commands remote-sync-commands)))
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
                           :auto-commit auto-commit
                           :auto-push auto-push
                           :hooks hooks
                           :tags tags
                           :commands effective-commands
                           :sort-hint sort-hint
                           :worktree worktree
                           :sync-branches sync-branches
                           :clone-url url
                           :worktree-symlinks links
                           :remote-sync remote-sync))))))
  (when (and timer (fboundp 'magit-dash-register-sync-timer))
    (apply #'magit-dash-register-sync-timer :name name :repos (list name) timer)))

;;;; Registry helpers

(defvar magit-dash-sync-trigger 'interactive
  "How the current auto-sync was triggered.
Bind to `timer' when invoking auto-sync from a timer; defaults to `interactive'.")

(defun magit-dash--default-commit-message (repo)
  "Return a default auto-commit message for REPO.
Includes hostname, Emacs instance ID, and the current sync trigger."
  (format "chore: auto-commit changes in %s [%s:%s, %s]"
          (magit-dash-repo-name repo)
          (sprite-system-name)
          (or (and (boundp 'sprite-instance-id) sprite-instance-id) "unknown")
          (symbol-name magit-dash-sync-trigger)))

(defun magit-dash--stage-all (repo)
  "Stage all changes in REPO. Returns t on success."
  (magit-dash-gh--with-repo-dir (magit-dash-repo-path repo)
    (= 0 (magit-call-git "add" "-A"))))

(defun magit-dash--auto-commit (repo)
  "Stage all changes in REPO and commit using its :auto-commit message.
Returns t when the commit succeeds, nil otherwise."
  (let* ((auto-commit (magit-dash-repo-auto-commit repo))
         (message (if (functionp auto-commit)
                      (funcall auto-commit repo)
                    (magit-dash--default-commit-message repo))))
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

(defun magit-dash--run-command-for (repo &optional background)
  "Open an `annotated-completing-read' command picker for REPO and invoke
the selected command with `default-directory' set to REPO's repository root.
When BACKGROUND is non-nil or `magit-dash-run-command-in-background' is non-nil,
runs the command in the background without popping up or stealing focus."
  (let* ((explicit-commands (magit-dash-repo-commands repo))
         (commands (if (magit-dash-repo-include-ci repo)
                       (if (assoc "rebuild" explicit-commands)
                           explicit-commands
                         (append explicit-commands
                                 '(("rebuild" . magit-dash-gh-workflow-run))))
                     explicit-commands)))
    (unless commands
      (user-error "No commands registered for %s" (magit-dash-repo-name repo)))
    (let* ((picked (annotated-completing-read
                    (seq-map (lambda (cmd)
                               (let* ((label (format "%s" (car cmd)))
                                      (norm (magit-dash--normalize-command-op (cdr cmd)))
                                      (target (car norm))
                                      (ann (cond
                                            ((eq target 'magit-dash-gh-workflow-run) "workflow run")
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
           (op (if (consp picked) (cdr picked) picked))
           (normalized (magit-dash--normalize-command-op op))
           (target (car normalized))
           (in-bg (or background (cdr normalized) magit-dash-run-command-in-background))
           (repo-name (magit-dash-repo-name repo))
           (repo-path (magit-dash-repo-path repo))
           (cmd-slug (replace-regexp-in-string "[[:space:]]+" "-" (string-trim (format "%s" cmd-name)))))
      (magit-dash-gh--with-repo-dir repo-path
        (cond
         ((stringp target)
          (let ((buf-name (format "*%s-dash-cmd-%s*" repo-name cmd-slug)))
            (if in-bg
                (let ((display-buffer-alist
                       (cons (cons (regexp-quote buf-name)
                                   (cons #'display-buffer-no-window '((allow-no-window . t))))
                             display-buffer-alist)))
                  (message "magit-dash: [%s] running '%s' in background..." repo-name cmd-name)
                  (let ((comp-buf (compilation-start target nil (lambda (_) buf-name))))
                    (with-current-buffer comp-buf
                      (add-hook 'compilation-finish-functions
                                (lambda (_buf status)
                                  (message "magit-dash: [%s] command '%s' %s"
                                           repo-name cmd-name (string-trim status)))
                                nil t))
                    comp-buf))
              (compilation-start target nil (lambda (_) buf-name)))))
         ((commandp target)
          (if in-bg
              (progn
                (message "magit-dash: [%s] running command '%s' in background..." repo-name cmd-name)
                (save-window-excursion (call-interactively target))
                (message "magit-dash: [%s] command '%s' finished" repo-name cmd-name))
            (call-interactively target)))
         ((functionp target)
          (if in-bg
              (progn
                (message "magit-dash: [%s] running function '%s' in background..." repo-name cmd-name)
                (save-window-excursion (funcall target))
                (message "magit-dash: [%s] function '%s' finished" repo-name cmd-name))
            (funcall target)))
         (t (user-error "Command has unsupported type %s" (type-of target))))))))

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
  "Return seconds since last git fetch for repo at PATH, or nil if never fetched.
Resolves worktree/submodule gitlinks (and, for worktrees, the shared
commondir) so FETCH_HEAD is read from the correct location instead of
assuming PATH/.git is a plain directory."
  (when-let* ((git-dir (magit-dash--resolve-common-git-dir path))
              (fetch-head (expand-file-name "FETCH_HEAD" git-dir))
              (attrs (and (file-exists-p fetch-head) (file-attributes fetch-head))))
    (float-time (time-since (file-attribute-modification-time attrs)))))

(defun magit-dash--head-hash (path)
  "Return the current HEAD commit hash for repo at PATH without spawning a process.
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
                       (dirty (not (null porcelain-lines)))
                       (uncommitted-files (when dirty porcelain-lines))
                       (recent-log (string-trim (or (nth 5 outputs) "")))
                       (commit-ts (string-trim (or (nth 6 outputs) "")))
                       (stats (list :branch branch
                                    :remote-origin remote-origin
                                    :behind behind
                                    :ahead ahead
                                    :dirty dirty
                                    :uncommitted-files uncommitted-files
                                    :fetch-age (magit-dash--fetch-age path)
                                    :updated-age (magit-dash--commit-timestamp-to-age commit-ts)
                                    :head-hash (magit-dash--head-hash path)
                                    :recent-log recent-log)))
                  (magit-dash-gh--cache-set path :stats stats)
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
  "Return cached stats for REPO without validity checking, or a loading placeholder.
Unlike `magit-dash--get-stats', never blocks: returns whatever is in cache, or
a placeholder plist when nothing is cached.  Caller is responsible for async collection."
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
ON-ERROR is called with stdout and exit-code on non-zero exit; defaults to a message.
When PATH no longer exists on disk (for example a worktree removed outside
magit-dash), ON-ERROR is called directly instead of letting `make-process'
signal — callers only ever expect to fail through the callback, not via a
thrown error."
  (if (not (file-directory-p path))
      (let ((msg (format "no such directory: %s" path)))
        (if on-error
            (funcall on-error msg 1)
          (message "magit-dash: git %s failed: %s" (car args) msg)))
    (let* ((default-directory path)
           (proc-buf (generate-new-buffer " *magit-dash-gh-git*")))
      (with-current-buffer proc-buf
        (setq default-directory path))
      (make-process
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
                          (car args) code output))))))))))

(defun magit-dash--fetch-async (repo on-complete)
  "Run git fetch for REPO asynchronously.
Calls ON-COMPLETE with symbol `ok' on success or `error' and error text on failure."
  (magit-dash--run-git
   (magit-dash-repo-path repo)
   '("fetch")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-dash--pull-async (repo on-complete)
  "Run git pull for REPO asynchronously.
Calls ON-COMPLETE with symbol `ok' on success or `error' and error text on failure."
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

(defun magit-dash--push-async (repo on-complete)
  "Run git push for REPO asynchronously.
Calls ON-COMPLETE with symbol `ok' on success or `error' and error text on failure."
  (magit-dash--run-git
   (magit-dash-repo-path repo)
   '("push")
   (lambda (_) (funcall on-complete 'ok))
   (lambda (output code)
     (funcall on-complete 'error (format "exit %d: %s" code output)))))

(defun magit-dash--auto-commit-async (repo on-complete)
  "Stage all changes in REPO and commit using its :auto-commit message function.
Calls ON-COMPLETE with `ok' when committed, `skipped' when workdir is clean,
or `error' when git add or commit fails."
  (let* ((path (magit-dash-repo-path repo))
         (auto-commit (magit-dash-repo-auto-commit repo))
         (msg (if (functionp auto-commit)
                  (funcall auto-commit repo)
                (magit-dash--default-commit-message repo))))
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
  "Return current branch name if allowed by REPO's sync-branches, nil otherwise.
When sync-branches is nil any branch is allowed and the current branch is returned."
  (let* ((allowed (magit-dash-repo-sync-branches repo))
         (current (magit-dash--current-branch (magit-dash-repo-path repo))))
    (if (null allowed)
        current
      (and (member current allowed) current))))

(defun magit-dash--auto-fetch-async (repo on-complete)
  "Run git fetch --all for REPO asynchronously.
Calls ON-COMPLETE with `ok' on success or `error' and error text on failure."
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
  "Run git STEPS sequentially in PATH.
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
(defaulting to current HEAD), commits, and pushes locally and remotely over SSH
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
  "Run global then repo hooks (or repo then global for `:post') for REPO's
OP at PHASE, then call ON-COMPLETE with the aggregate status."
  (magit-dash--run-hook-chain repo phase (magit-dash--hooks-for repo op phase) on-complete))

(defun magit-dash--default-operation (op)
  "Return the default implementation function for OP."
  (pcase op
    (:fetch  #'magit-dash--auto-fetch-async)
    (:pull   #'magit-dash--auto-pull-async)
    (:commit #'magit-dash--auto-commit-async)
    (:push   #'magit-dash--auto-push-async)
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
  "Return non-nil when REPO has any sync operation configured — default
pipeline steps (fetch/pull/commit/push) or a `:hooks' `:sync' `:operation'
override."
  (or (magit-dash--auto-sync-steps repo)
      (magit-dash--repo-operation repo :sync)))

(defun magit-dash--auto-sync-op-symbols (repo)
  "Return an ordered list of operation keywords configured for REPO's
default `:sync' pipeline: `:fetch'/`:pull' (pull implies fetch), `:commit',
`:push' — each only when its own auto-* flag is set."
  (seq-filter
   #'identity
   (list
    (when (or (magit-dash-repo-auto-fetch repo) (magit-dash-repo-auto-pull repo)) :fetch)
    (when (magit-dash-repo-auto-pull repo) :pull)
    (when (magit-dash-repo-auto-commit repo) :commit)
    (when (magit-dash-repo-auto-push repo) :push))))

(defun magit-dash--run-op-chain (repo ops on-complete)
  "Run OPS (a list of operation keywords) for REPO in order via
`magit-dash--run-operation', logging each.  Aborts on `error'; continues
on `ok' or `skipped'.  Calls ON-COMPLETE with `ok' after all ops, or
`error' on first failure."
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
  "Default `:operation' implementation for `:sync': run fetch/pull/commit/push
per REPO's auto-* flags, in order, each resolved and wrapped through its own
`:hooks' entry.  Calls ON-COMPLETE with `ok', `skipped', or `error'."
  (let ((ops (magit-dash--auto-sync-op-symbols repo)))
    (if (null ops)
        (funcall on-complete 'skipped "no auto operations configured")
      (magit-dash--run-op-chain repo ops on-complete))))

(defun magit-dash--auto-sync-async (repo on-complete)
  "Run REPO's configured `:sync' operation — its `:hooks' `:sync' `:operation'
override (which replaces the default pipeline entirely, including its own
fetch/pull/commit/push settings) or, absent that, the default pipeline of
fetch/pull/commit/push per REPO's auto-* flags — wrapped by `:sync''s own
`:pre'/`:post' hooks.  Calls ON-COMPLETE with `ok', `skipped', or `error'."
  (magit-dash--run-operation repo :sync on-complete))

(defun magit-dash--log-operation (repo-name operation status &optional error-text)
  "Log REPO-NAME OPERATION with STATUS to *Messages*.
The current timestamp is attached as a tooltip (help-echo) on REPO-NAME.
When ERROR-TEXT is non-nil it is appended to the message."
  (let* ((ts (format-time-string "%Y-%m-%d %H:%M:%S"))
         (name (propertize repo-name 'face 'bold 'help-echo ts))
         (detail (if error-text (format " — %s" error-text) "")))
    (message "magit-dash: %s %s → %s%s" name operation (symbol-name status) detail)))

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
             (with-magit-from-dashboard repo
               (funcall op-fn repo callback))
           (error (funcall callback 'error (error-message-string err))))))
     repos)))

(defun magit-dash--maybe-refresh ()
  "Refresh the repo dashboard buffer if it is currently live."
  (when-let* ((buf (get-buffer magit-dash-buffer-name))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (magit-dash-refresh))))

;;;; Cache management

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
        (message "Cleared cache for %s" repo-path))
    (clrhash magit-dash-gh--cache)
    (message "Cleared entire cache")))

(defun magit-dash-cache-reset-all ()
  "Clear all caches and repopulate the dashboard asynchronously."
  (interactive)
  (clrhash magit-dash-gh--cache)
  (magit-dash--maybe-refresh))

(defun magit-dash-cache-reset-at-point ()
  "Clear cache for repository at point, re-collect stats synchronously, and refresh."
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
Shows a one-line summary message and opens a detail buffer when issues are found."
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
              :include-ci include-ci)))))
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
Returns a list of `magit-dash-repo' structs, one per submodule (initialized or not).
Name is always \"parent<inner>\" where inner is the registered repo name when the path
matches a `magit-dash-repo-list' entry, otherwise the submodule directory basename.
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
              :submodule (if missing-p 'missing t))))))
      (seq-remove #'null))))

(defun magit-dash--discover-submodules ()
  "Populate the unified cache with submodules for all registered main repos."
  (thread-last
    magit-dash-repo-list
    (seq-remove (lambda (repo) (or (magit-dash-repo-worktree repo) (magit-dash-repo-submodule repo))))
    (seq-do
     (lambda (repo)
       (let* ((path (magit-dash-repo-path repo))
              (lines (ignore-errors
                       (let ((default-directory path))
                         (process-lines magit-git-executable
                                        "submodule" "status")))))
         (magit-dash-gh--cache-set path
	  :submodules (when lines
			(magit-dash--parse-submodules path lines))))))))


;;;; Column configuration

(defvar magit-dash-columns
  '((name . t) (branch . t) (fetched . t) (updated . nil) (ci . t) (status . t) (worktree . t) (sync . t) (cached . nil) (upstream . nil))
  "Alist of (COLUMN-SYMBOL . ENABLED) for the repository dashboard.
Persisted across sessions via `savehist-additional-variables'.")

(defconst magit-dash--all-columns
  '(name branch fetched updated ci status worktree sync cached upstream)
  "All available dashboard columns in display order.")

(defconst magit-dash--column-defs
  '((fetched  . ("Fetched"  8 nil))
    (updated  . ("Updated"  8 nil))
    (status   . ("Status"   8 nil))
    (worktree . ("Type"     8 nil))
    (sync     . ("Sync"     8 nil))
    (cached   . ("Cached"   7 nil))
    (ci       . ("CI"       3 nil))
    (upstream . ("Upstream" 16 nil)))
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

(defun magit-dash-toggle-discovered-submodules ()
  "Toggle visibility of auto-discovered submodules in the dashboard and refresh."
  (interactive)
  (setq magit-dash-show-discovered-submodules
        (not magit-dash-show-discovered-submodules))
  (magit-dash-refresh))

(defvar magit-dash-show-discovered-worktrees t
  "When non-nil, auto-discovered worktrees appear below their parent in the dashboard.")

(defun magit-dash-toggle-discovered-worktrees ()
  "Toggle visibility of auto-discovered worktrees in the dashboard and refresh."
  (interactive)
  (setq magit-dash-show-discovered-worktrees
        (not magit-dash-show-discovered-worktrees))
  (magit-dash-refresh))

(defvar savehist-additional-variables nil)
(add-to-list 'savehist-additional-variables 'magit-dash-columns)

;;;; Formatting helpers

(defvar magit-dash-render-branch-name-as-basename nil
  "When non-nil, the Branch column shows only the basename of a branch.
Everything up to and including the final \"/\" is dropped (e.g.
\"feature/some/thing\" displays as \"thing\").  Purely cosmetic — git
operations always use the full branch name.")

(defun magit-dash-toggle-branch-basename ()
  "Toggle whether the Branch column shows only a branch's basename."
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

(defun magit-dash--format-status (ahead behind dirty)
  "Format AHEAD, BEHIND, and DIRTY into a compact status indicator.
Each non-zero/non-nil value contributes a segment; segments are joined with
a single space.  Returns an empty string when everything is clean and synced."
  (let ((parts nil))
    (when (> ahead 0)
      (push (propertize (format "↑%d" ahead) 'face 'warning) parts))
    (when (> behind 0)
      (push (propertize (format "↓%d" behind) 'face 'font-lock-comment-face) parts))
    (when dirty
      (push (propertize "!" 'face 'error) parts))
    (mapconcat #'identity (nreverse parts) " ")))

(defun magit-dash--format-worktree (repo)
  "Format the type indicator for REPO.
Shows \"WT\" for worktrees, \"SUBM\" for initialized submodules,
\"SUBM.EMPTY\" for missing/uninitialized submodules, \"SUBM.TR\" for
explicitly-registered submodules, \"MISSING\" for un-cloned repos,
\"REPO+SM\" for repos with discovered submodules, and \"REPO\" for ordinary working-tree repos."
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
  "Face for repository names in the repo dashboard.")

(defface magit-dash-repo-branch-face
  '((t :inherit font-lock-string-face))
  "Face for branch names in the repo dashboard.")

(defun magit-dash--build-format (repos)
  "Return the tabulated-list format vector for REPOS using enabled columns.
Name and Branch columns are elastic: each is wide enough for its longest value.
When both together exceed the available window space they split it proportionally."
  (let* ((raw-name (seq-reduce
                    (lambda (w r)
                      (max w (length (or (and magit-dash--submodule-path-set
                                              (gethash (magit-dash-repo-path r)
                                                       magit-dash--submodule-path-set))
                                         (magit-dash-repo-name r)))))
                    repos 0))
         (raw-branch (seq-reduce
                      (lambda (w r)
                        (let ((b (magit-dash--display-branch-name
                                  (or (plist-get (magit-dash-gh--cache-get
                                                  (magit-dash-repo-path r) :stats)
                                                 :branch)
                                      (magit-dash-repo-branch r)
                                      ""))))
                          (max w (length b))))
                      repos 0))
         (fixed-width (seq-reduce
                       (lambda (acc pair)
                         (if (magit-dash--column-enabled-p (car pair))
                             (+ acc (cadr (cdr pair)))
                           acc))
                       magit-dash--column-defs 0))
         (available (max 20 (- (or (ignore-errors (window-width)) 97) fixed-width)))
         (name-need (max 12 (1+ raw-name)))
         (branch-need (max 8 (1+ raw-branch)))
         (widths (cond
                  ((<= (+ name-need branch-need) available)
                   (list name-need branch-need))
                  ((<= branch-need (- available 12))
                   (list (max 12 (- available branch-need)) branch-need))
                  ((<= name-need (- available 8))
                   (list name-need (max 8 (- available name-need))))
                  (t
                   (let ((nw (max 12 (floor (* available
                                               (/ (float name-need)
                                                  (+ name-need branch-need)))))))
                     (list nw (max 8 (- available nw)))))))
         (name-width (car widths))
         (branch-width (cadr widths))
         (active (magit-dash--active-columns)))
    (apply #'vector
           (seq-map (lambda (col)
                      (pcase col
                        ('name   `("Name"   ,name-width   t))
                        ('branch `("Branch" ,branch-width t))
                        (_       (alist-get col magit-dash--column-defs))))
                    active))))

(defvar magit-dash-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'magit-dash-magit-status)
    (define-key m (kbd "SPC") #'magit-dash-toggle-mark)
    (define-key m (kbd "a")   #'magit-dash-auto-sync)
    (define-key m (kbd "A")   #'magit-dash-commit-all)
    (define-key m (kbd "b")   #'magit-dash-visit-buffer)
    (define-key m (kbd "B")   #'magit-dash-switch-branch)
    (define-key m (kbd "c")   #'magit-dash-magit-commit)
    (define-key m (kbd "C")   #'magit-dash-commit)
    (define-key m (kbd "d")   #'magit-dash-magit-diff)
    (define-key m (kbd "e")   #'magit-dash-find-file)
    (define-key m (kbd "f")   #'magit-dash-fetch)
    (define-key m (kbd "F")   #'magit-dash-fetch-all)
    (define-key m (kbd "g")   #'magit-dash-refresh)
    (define-key m (kbd "G")   #'magit-dash-stage-all)
    (define-key m (kbd "i")   #'magit-dash-add-tag)
    (define-key m (kbd "j")   #'magit-dash-builder)
    (define-key m (kbd "k")   #'magit-dash-worktree-delete)
    (define-key m (kbd "l")   #'magit-dash-magit-log)
    (define-key m (kbd "L")   #'magit-dash-magit-log-full)
    (define-key m (kbd "m")   #'magit-dash-menu)
    (define-key m (kbd "n")   #'magit-dash-sync)
    (define-key m (kbd "o")   #'magit-dash-open-repo)
    (define-key m (kbd "p")   #'magit-gh-pr-dash)
    (define-key m (kbd "P")   #'magit-dash-push)
    (define-key m (kbd "q")   #'quit-window)
    (define-key m (kbd "Q")   #'magit-dash-agent-shell-queue)
    (define-key m (kbd "r")   #'magit-dash-hard-refresh)
    (define-key m (kbd "s")   #'magit-dash-magit-status)
    (define-key m (kbd "S")   #'magit-dash-sync-all)
    (define-key m (kbd "t")   #'magit-dash-filter-by-tag)
    (define-key m (kbd "T")   #'magit-dash-toggle-column)
    (define-key m (kbd "u")   #'magit-dash-pull)
    (define-key m (kbd "U")   #'magit-dash-pull-all)
    (define-key m (kbd "w")   #'magit-dash-worktree-add)
    (define-key m (kbd "x")   #'magit-dash-run-command)
    (define-key m (kbd "X")   #'magit-dash-run-command-background)
    (define-key m (kbd "B")   #'magit-dash-bootstrap-repo)
    (define-key m (kbd "C")   #'magit-dash-clone-repo)
    (define-key m (kbd "y")   #'magit-dash-prune-branches)
    (define-key m (kbd "Z")   #'magit-dash-agent-shell-new)

    (define-key m (kbd "*")   #'magit-dash-unmark-all)
    (define-key m (kbd "!")   #'magit-dash-magit-dispatch)
    (define-key m (kbd "?")   #'magit-dash-menu)

    (define-key m (kbd "M-s") #'magit-dash-toggle-discovered-submodules)
    (define-key m (kbd "M-t") #'magit-dash-toggle-discovered-worktrees)
    m)
  "Keymap for `magit-dash-mode'.")

(defvar-local magit-dash--tag-filter nil
  "When non-nil, a symbol: only repos tagged with this symbol are shown.")

(defvar magit-dash--ephemeral-tags (make-hash-table :test #'equal)
  "Hash table mapping repo path strings to lists of ephemeral tag symbols.
These tags are session-local and are not saved to the repo registry.")

(defun magit-dash--all-tags-for (repo)
  "Return the combined permanent and ephemeral tags for REPO."
  (append (magit-dash-repo-tags repo)
          (gethash (magit-dash-repo-path repo) magit-dash--ephemeral-tags)))

(defun magit-dash--permanent-tag-set ()
  "Return deduplicated list of all permanent tag symbols across registered repos."
  (delete-dups (seq-mapcat #'magit-dash-repo-tags magit-dash-repo-list)))

(defvar-local magit-dash--marked-paths nil
  "List of repo paths currently marked for batch operations.")

(defvar-local magit-dash--batch-all nil
  "When non-nil, batch operations act on all repos in the table.
Disabled by default; toggle with `magit-dash-toggle-batch-all'.")

(defvar magit-dash-show-discovered-submodules t
  "When non-nil, auto-discovered submodules appear below their parent in the dashboard.")

(defvar magit-dash--submodule-path-set nil
  "Hash table mapping auto-discovered submodule path → \"parent<mod>\" display name.
Rebuilt on each refresh. Used to detect explicitly-registered repos that are also
submodules and to derive their parent<mod> display name.")

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

(define-derived-mode magit-dash-mode tabulated-list-mode "Repos"
  "Major mode for the registered repository dashboard."
  (setq tabulated-list-format (magit-dash--build-format magit-dash-repo-list))
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (setq-local default-directory (file-name-as-directory (expand-file-name "~/")))
  (add-hook 'post-command-hook #'magit-dash--update-default-directory nil t))

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
                                         (string-replace (magit-dash-repo-branch repo) "<B>"
                                                          (magit-dash-repo-name repo)))
                                        (t (magit-dash-repo-name repo))))
                              (base-face (if is-marked
                                            '(bold magit-dash-repo-name-face)
                                          'magit-dash-repo-name-face))
                              (final-face (if is-missing
                                             (list '(:strike-through t) base-face)
                                           base-face)))
                         (propertize display 'face final-face)))
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
                      ('status
                       (magit-dash--format-status
                        (or (plist-get stats :ahead) 0)
                        (or (plist-get stats :behind) 0)
                        (plist-get stats :dirty)))
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
  "Rebuild the dashboard row for REPO from the current cache and re-render the table."
  (let ((path (magit-dash-repo-path repo)))
    (setq tabulated-list-entries
          (seq-map (lambda (entry)
                     (if (equal (magit-dash-repo-path (car entry)) path)
                         (magit-dash--build-entry repo)
                       entry))
                   tabulated-list-entries))
    (tabulated-list-print t)))

(defun magit-dash--populate-stats-async (repos)
  "Asynchronously collect stats for stale or uncached repos in REPOS.
Emits a status message with repo counts; updates dashboard rows as each finishes."
  (let* ((buf (current-buffer))
         (needs-update
          (seq-filter
           (lambda (repo)
             (and (not (eq (magit-dash-repo-submodule repo) 'missing))
                  (let* ((path (magit-dash-repo-path repo))
                         (cached (magit-dash-gh--cache-get path :stats)))
                    (not (and cached
                              (equal (magit-dash--head-hash path)
                                     (plist-get cached :head-hash)))))))
           repos))
         (total (length repos))
         (stale (length needs-update))
         (done (list 0)))
    (if (= stale 0)
        (message "magit-dash: %d repos (all cached)" total)
      (message "magit-dash: %d repos, loading %d…" total stale)
      (seq-do
       (lambda (repo)
         (magit-dash--collect-stats-async
          repo
          (lambda (_stats)
            (setcar done (1+ (car done)))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (magit-dash--update-entry repo)))
            (when (= (car done) stale)
              (message "magit-dash: %d repos, %d updated" total stale))
            (when (magit-dash-repo-include-ci repo)
              (magit-dash-gh-ci-fetch
               repo
               (lambda (_ci-status)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (magit-dash--update-entry repo)))))))))
       needs-update))))

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
  "Return REPOS sorted by :sort-hint then type, with discovered worktrees following each parent.
Primary sort is :sort-hint ascending (nil hints follow all sorted ones).
Secondary sort within equal hints is by type: repo < worktree < submodule < missing.
Auto-discovered submodules whose path is already in `magit-dash-repo-list' are
suppressed to avoid duplicate rows — the registered entry is shown instead."
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
                        (append (when magit-dash-show-discovered-worktrees
                                  (magit-dash-gh--cache-get (magit-dash-repo-path repo) :worktrees))
                                (when magit-dash-show-discovered-submodules
                                  (seq-remove
                                   (lambda (sm)
                                     (gethash (magit-dash-repo-path sm) registered-paths))
                                   (magit-dash-gh--cache-get (magit-dash-repo-path repo) :submodules))))))
                sorted)))

(defun magit-dash-refresh ()
  "Refresh the dashboard, clearing all per-repo caches and re-fetching asynchronously.
Discovers worktrees and submodules synchronously, renders the table with the last
known state, then clears :stats, :pr-counts, and :ci-status for every repo and
re-fetches all three in the background, updating each row as data arrives."
  (interactive)
  (magit-dash--discover-worktrees)
  (magit-dash--discover-submodules)
  (setq magit-dash--submodule-path-set
        (let ((paths (make-hash-table :test #'equal)))
          (seq-do (lambda (repo)
                    (seq-do (lambda (sm)
                              (puthash (magit-dash-repo-path sm) (magit-dash-repo-name sm) paths))
                            (or (magit-dash-gh--cache-get (magit-dash-repo-path repo) :submodules) '())))
                  magit-dash-repo-list)
          paths))
  (let ((repos (magit-dash--sorted-repos
                (if magit-dash--tag-filter
                    (seq-filter (lambda (r)
                                  (memq magit-dash--tag-filter
                                        (magit-dash--all-tags-for r)))
                                magit-dash-repo-list)
                  magit-dash-repo-list))))
    (setq tabulated-list-format (magit-dash--build-format repos))
    (tabulated-list-init-header)
    (setq tabulated-list-entries (seq-map #'magit-dash--build-entry repos))
    (tabulated-list-print t)
    (seq-do (lambda (repo)
              (let ((path (magit-dash-repo-path repo)))
                (magit-dash-gh--cache-remove path :stats)
                (magit-dash-gh--cache-remove path :pr-counts)
                (magit-dash-gh--cache-remove path :ci-status)))
            repos)
    (magit-dash--populate-stats-async repos)
    (let ((buf (current-buffer)))
      (seq-do
       (lambda (repo)
         (when (magit-dash-repo-include-ci repo)
           (magit-dash-gh-ci-fetch
            repo
            (lambda (_)
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (magit-dash--update-entry repo)))))))
       repos))))

(defun magit-dash-hard-refresh ()
  "Clear all caches and repopulate the dashboard from scratch.
Unlike `magit-dash-refresh', discards all cached stats so every repo is
re-collected asynchronously."
  (interactive)
  (clrhash magit-dash-gh--cache)
  (magit-dash-refresh))

(defmacro with-magit-dash (&rest body)
  `(progn
     (magit-dash-repo-ensure-configuration)
     (let* ((buf (get-buffer-create magit-dash-buffer-name))
	    (default-directory (if (eq (current-buffer) buf)
				   (magit-dash-repo-path (magit-dash--repo-at-point))
				 default-directory)))
     (with-current-buffer buf
       (unless (derived-mode-p 'magit-dash-mode)
	 (magit-dash-mode))
       (magit-dash-refresh)
       ,@body))))

(defmacro with-magit-from-dashboard (repo &rest body)
  "Execute BODY with default-directory set to REPO path."
  (declare (indent 1))
  (let ((path (make-symbol "path")))
    `(let* ((,path (file-name-as-directory (magit-dash-repo-path ,repo)))
            (default-directory ,path))
       ,@body)))

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
The candidate list uses `annotated-completing-read', annotating each repository
with its abbreviated file path and cached dash stats (branch, ahead/behind, dirty,
CI status, PR counts).
Signals `user-error' when no repositories are registered."
  (unless magit-dash-repo-list
    (user-error "magit-dash: no registered repositories to choose from"))
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
  "Prompt to select a registered repository from `magit-dash-repo-list' and open its status buffer.
The picker displays repository names annotated with their paths and cached dashboard stats."
  (interactive)
  (let ((path (magit-dash--prompt-for-repo-path "Open repository: ")))
    (magit-status-setup-buffer path)))

(defun magit-dash--repo-missing-p (repo)
  "Return non-nil if REPO is missing (directory does not exist or has no git repository)."
  (let ((path (magit-dash-repo-path repo)))
    (or (eq (magit-dash-repo-submodule repo) 'missing)
        (not (file-exists-p path))
        (not (file-directory-p path))
        (not (magit-dash--resolve-git-dir path)))))

(defun magit-dash--repo-at-point-missing-p ()
  "Return non-nil if the repo at point in the dashboard is missing."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (magit-dash--repo-missing-p repo)))

(defun magit-dash--has-missing-repos-p ()
  "Return non-nil if any registered repository in the dashboard is missing."
  (seq-some #'magit-dash--repo-missing-p (magit-dash--effective-repos)))


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
  (let ((missing (seq-filter #'magit-dash--repo-missing-p (magit-dash--effective-repos))))
    (unless missing
      (user-error "No missing repositories to clone"))
    (message "magit-dash: cloning %d missing repository(ies)..." (length missing))
    (dolist (repo missing)
      (magit-dash-clone-repo repo nil))))

(defun magit-dash--prompt-for-repo (&optional prompt repos)
  "Prompt for one of REPOS (default `magit-dash-repo-list') and return its struct."
  (let ((candidate-repos (or repos magit-dash-repo-list)))
    (unless candidate-repos
      (user-error "magit-dash: no registered repositories to choose from"))
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
  "Return non-nil if batch bootstrap is permitted and targets at least one missing repo with :repo."
  (and (magit-dash--batch-enabled-p)
       (seq-some (lambda (r)
                   (and (magit-dash-repo-repo r)
                        (magit-dash--repo-missing-p r)))
                 (magit-dash--effective-repos))))

;;;###autoload
(defun magit-dash-bootstrap-repo (&optional repo on-complete)
  "Clone REPO from its configured upstream `:repo' path into its destination path.
REPO defaults to the repository at point or is selected interactively.
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
When `magit-dash--batch-all' is enabled, targets all visible missing repositories with `:repo'.
Runs asynchronously and refreshes the dashboard on completion."
  (interactive)
  (let* ((effective (magit-dash--effective-repos))
         (repos (seq-filter (lambda (r)
                              (and (magit-dash-repo-repo r)
                                   (magit-dash--repo-missing-p r)))
                            effective)))
    (unless repos
      (user-error (if magit-dash--batch-all
                      "No missing repositories with upstream :repo configured to bootstrap"
                    "No marked missing repositories with upstream :repo configured to bootstrap")))
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
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-dispatch)))

(defun magit-dash-magit-status ()
  "Open a magit status buffer for the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (magit-status-setup-buffer default-directory)))

(defun magit-dash-magit-diff ()
  "Open a magit diff (dwim) buffer for the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-diff)))

(defun magit-dash-magit-log ()
  "Open magit log for the current branch in the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-log-current)))

(defun magit-dash-magit-log-full ()
  "Open the full magit log menu for the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-log)))

(defun magit-dash-magit-commit ()
  "Open a magit commit buffer for the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-commit-create)))

(defun magit-dash-fetch ()
  "Run git fetch for the repository at point via magit."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-fetch)))

(defun magit-dash-pull ()
  "Pull from upstream for the repository at point via magit."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-pull-from-upstream)))

(defun magit-dash-commit ()
  "Auto-commit changes in the repository at point.
Signals `user-error' when :auto-commit is not configured for this repo."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (unless (magit-dash-repo-auto-commit repo)
      (user-error "Auto-commit is not configured for %s" (magit-dash-repo-name repo)))
    (if (magit-dash--auto-commit repo)
        (message "magit-dash: committed changes in %s" (magit-dash-repo-name repo))
      (message "magit-dash: nothing to commit or commit failed in %s"
               (magit-dash-repo-name repo)))))

(defun magit-dash-stage-all ()
  "Stage all changes in the repository at point."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (if (magit-dash--stage-all repo)
        (message "magit-dash: staged all changes in %s" (magit-dash-repo-name repo))
      (message "magit-dash: stage all failed in %s" (magit-dash-repo-name repo)))))

(defun magit-dash-push ()
  "Push current branch to its push remote for the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-push-current-to-pushremote)))

(defun magit-dash-sync ()
  "Run the configured auto operations for the repository at point asynchronously.
Signals `user-error' when no auto operations are configured for this repo."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (unless (magit-dash--has-sync-configured-p repo)
      (user-error "No auto operations configured for %s" (magit-dash-repo-name repo)))
    (with-magit-from-dashboard repo
      (magit-dash--auto-sync-async
       repo
       (lambda (_status &optional _error-text)
         (magit-dash--maybe-refresh))))))

(defun magit-dash-commit-all ()
  "Auto-commit marked repos (or all if none marked) with :auto-commit configured.
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
     (lambda (_) (magit-dash--maybe-refresh)))))

(defun magit-dash-sync-all ()
  "Run auto operations for marked repos (or all if none marked) asynchronously, then refresh."
  (interactive)
  (let ((repos (seq-filter #'magit-dash--has-sync-configured-p (magit-dash--effective-repos))))
    (unless repos
      (user-error "No repositories have auto operations configured"))
    (magit-dash--batch-run
     repos
     #'magit-dash--auto-sync-async
     "magit-dash sync"
     (lambda (_) (magit-dash--maybe-refresh)))))

;;;###autoload
(defun magit-dash-sync-repo ()
  "Select a repository interactively and run its configured auto-sync operations."
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
      (with-magit-from-dashboard repo
        (magit-dash--auto-sync-async
         repo
         (lambda (_status &optional _error-text)
           (magit-dash--maybe-refresh)))))))

(defun magit-dash-auto-sync ()
  "Run auto operations for marked repos (or all if none marked) asynchronously.
Each repo's steps (fetch, pull, commit, push) run sequentially; each step is
logged individually. Dashboard refreshes when all repos complete."
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
  "Open an `annotated-completing-read' picker for the repo at point and invoke the selected command.
When BACKGROUND is non-nil (or with prefix argument \\[universal-argument]),
run the command in the background without displaying its buffer."
  (interactive "P")
  (magit-dash--run-command-for (magit-dash--repo-at-point) background))

(defun magit-dash-run-command-background ()
  "Open an `annotated-completing-read' picker for the repo at point and run the command in the background."
  (interactive)
  (magit-dash--run-command-for (magit-dash--repo-at-point) t))

(defun magit-dash--build-tag-table ()
  "Build an `annotated-completing-read' alist mapping tag-name strings to
annotation strings, targeting each entry's own tag symbol.
Each annotation lists the count of repos using the tag and up to four names.
Permanent tags (from repo :tags fields) are sorted before ephemeral-only tags."
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
  "Read a tag using annotated-completing-read with PROMPT.
Shows permanent tags (from repo :tags fields) before ephemeral-only tags.
Each candidate is annotated with a repo count and up to four repo names.

When INCLUDE-CLEAR is non-nil a \"(clear)\" option is prepended; selecting it
returns the symbol `clear'.  When REQUIRE-MATCH is non-nil only existing tags
are accepted; otherwise arbitrary input is allowed for new ephemeral tags.
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
                      (t "ephemeral"))))
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

(defun magit-dash-filter-by-tag ()
  "Filter the dashboard by tag using annotated completion.
Select \"(clear)\" to show all repos; quitting leaves the current filter unchanged."
  (interactive)
  (when-let* ((tag (magit-dash--read-tag "Filter by tag: "
                                                       :include-clear t)))
    (setq magit-dash--tag-filter
          (unless (eq tag 'clear) tag))
    (magit-dash-refresh)))

(defun magit-dash-add-tag ()
  "Add an ephemeral session-local tag to the repository at point.
The tag is stored in memory for this session and is not saved to the registry.
Pick from existing tags or type a new symbol name."
  (interactive)
  (when-let* ((repo (magit-dash--repo-at-point))
              (tag (magit-dash--read-tag
                    (format "Add tag to %s: " (magit-dash-repo-name repo)))))
    (let* ((path (magit-dash-repo-path repo))
           (existing (map-elt magit-dash--ephemeral-tags path)))
      (unless (memq tag existing)
        (setf (map-elt magit-dash--ephemeral-tags path) (cons tag existing)))
      (magit-dash-refresh))))

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
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'magit-checkout)))

(defun magit-dash-prune-branches ()
  "Prune merged branches in a repository.
Uses the repository at point when called from a `magit-dash' buffer,
otherwise the git repository containing the current buffer (works from any
`magit' buffer or a file/`dired' buffer inside a repo), and finally prompts
among the registered repositories when neither applies."
  (interactive)
  (let ((default-directory (file-name-as-directory (magit-dash--resolve-repo-path))))
    (magit-dash-gh-prune-merged-branches)))

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
    (with-magit-from-dashboard repo
      (call-interactively #'magit-worktree-checkout))
    (magit-dash-refresh)))

(defun magit-dash-worktree-delete ()
  "Delete the worktree at point via magit.
Signals `user-error' when the current row is not a worktree."
  (interactive)
  (let ((repo (magit-dash--repo-at-point)))
    (unless (magit-dash-repo-worktree repo)
      (user-error "Not a worktree row; use 'k' only on worktree entries"))
    (with-magit-from-dashboard repo
      (call-interactively #'magit-worktree-delete))
    (magit-dash-refresh)))

(defun magit-dash-builder ()
  "Run `builder-compile-project' in the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'builder-compile-project)))

(defun magit-dash-agent-shell ()
  "Switch to an agent-shell session for the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'agent-shell-menu-switch-project-session)))

(defun magit-dash-agent-shell-new ()
  "Start a new agent-shell session in the repository at point."
  (interactive)
  (with-magit-from-dashboard (magit-dash--repo-at-point)
    (call-interactively #'agent-shell-new-shell)))

(defun magit-dash-agent-shell-queue ()
  "Open the agent-shell queue buffer."
  (interactive)
  (call-interactively #'agent-shell-queue-buffer-open))

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
  (when-let* ((repo (magit-dash--repo-at-point)))
    (magit-dash-gh--cache-get (magit-dash-repo-path repo) :ci-status)))

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

(defun magit-dash-repo-ensure-configuration ()
  "Raises `user-error' when there is not `magit-dash-repo-list' defined."
  (unless magit-dash-repo-list
    (user-error "No repositories registered; use `magit-dash-register'")))

(defun magit-dash-create ()
  (with-magit-dash
   (message "magit-dash-repo-dashobard: your (miss)adventure awaits!")))

;;;; Transient predicates

(defun magit-dash--dirty-or-unknown-p ()
  "Return non-nil when the repo at point is dirty or its stats are not yet cached."
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
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point)))
              (stats (magit-dash--get-stats repo)))
    (and (> (or (plist-get stats :ahead) 0) 0) t)))

(defun magit-dash--can-add-worktree-p ()
  "Return non-nil when at a registered non-worktree repo (can add a worktree)."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point))))
    (not (magit-dash-repo-worktree repo))))

;;;; Mark/select support

(defun magit-dash--update-entry-for (repo)
  "Regenerate the tabulated-list entry for REPO in place in `tabulated-list-entries'."
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
  (let* ((repo (magit-dash--repo-at-point))
         (path (magit-dash-repo-path repo)))
    (setq magit-dash--marked-paths
          (if (member path magit-dash--marked-paths)
              (delete path magit-dash--marked-paths)
            (cons path magit-dash--marked-paths)))
    (magit-dash--update-entry-for repo)
    (tabulated-list-print t)
    (forward-line 1)))

(defun magit-dash-unmark-all ()
  "Clear all marks from the dashboard."
  (interactive)
  (setq magit-dash--marked-paths nil)
  (setq tabulated-list-entries
        (seq-map (lambda (entry)
                   (magit-dash--build-entry (car entry)))
                 tabulated-list-entries))
  (tabulated-list-print t))

(defun magit-dash--effective-repos ()
  "Return marked repos if any are marked, else all repos currently in the table.
Falls back to `magit-dash-repo-list' when not in a dashboard buffer."
  (let ((all (if (derived-mode-p 'magit-dash-mode)
                 (seq-map #'car tabulated-list-entries)
               magit-dash-repo-list)))
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
True when `magit-dash--batch-all' is set or at least one repo is marked."
  (or magit-dash--batch-all (magit-dash--has-marks-p)))

(defun magit-dash-toggle-batch-all ()
  "Toggle whether batch operations act on all repos or only marked ones.
When enabled, all repos visible in the dashboard are targeted.
When disabled, only explicitly marked repos are targeted."
  (interactive)
  (setq magit-dash--batch-all (not magit-dash--batch-all))
  (message "Batch: %s" (if magit-dash--batch-all "all repos" "marked repos only (disabled)")))

(defun magit-dash-fetch-all ()
  "Asynchronously fetch marked repos, or all visible repos when none are marked."
  (interactive)
  (let ((repos (magit-dash--effective-repos)))
    (unless repos
      (user-error "No repositories to fetch"))
    (magit-dash--batch-run
     repos
     #'magit-dash--fetch-async
     "magit-gh fetch"
     (lambda (_) (magit-dash--maybe-refresh)))))

(defun magit-dash-pull-all ()
  "Asynchronously pull marked repos, or all visible repos when none are marked."
  (interactive)
  (let ((repos (magit-dash--effective-repos)))
    (unless repos
      (user-error "No repositories to pull"))
    (magit-dash--batch-run
     repos
     #'magit-dash--pull-async
     "magit-gh pull"
     (lambda (_) (magit-dash--maybe-refresh)))))

(defun magit-dash-push-all ()
  "Asynchronously push marked repos, or all visible repos when none are marked."
  (interactive)
  (let ((repos (magit-dash--effective-repos)))
    (unless repos
      (user-error "No repositories to push"))
    (magit-dash--batch-run
     repos
     #'magit-dash--push-async
     "magit-gh push"
     (lambda (_) (magit-dash--maybe-refresh)))))

(defun magit-dash-submodule-update-all ()
  "Run git submodule update --init --recursive for marked repos, or all visible repos."
  (interactive)
  (let ((repos (magit-dash--effective-repos)))
    (unless repos
      (user-error "No repositories to update"))
    (magit-dash--batch-run
     repos
     #'magit-dash--submodule-update-async
     "magit-dash submodule-update"
     (lambda (_) (magit-dash--maybe-refresh)))))

;;;; Transient menus

(defun magit-dash--agent-shell-project-buffers-p ()
  "Return non-nil when agent-shell buffers exist for the repo at point."
  (when-let* ((repo (ignore-errors (magit-dash--repo-at-point)))
	      (default-directory (file-name-as-directory (magit-dash-repo-path repo))))
    (and (boundp 'agent-shell-menu-project-buffers) (agent-shell-menu-project-buffers) t)))

(transient-define-prefix magit-dash-menu ()
  "Actions for the repository at point in the repo dashboard."
  [["Navigate"
    ("b"   "Visit buffer"    magit-dash-visit-buffer
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
   ["Worktree"
    ("wa"  "Add"             magit-dash-worktree-add
     :inapt-if-not magit-dash--can-add-worktree-p)
    ("wd"  "Delete"          magit-dash-worktree-delete
     :inapt-if-not magit-dash--at-worktree-p)
    ("ws"  "Sync symlinks"   magit-dash-worktree-sync-symlinks
     :inapt-if-not magit-dash--repo-at-point-p)
    ("wt"  "Toggle"          magit-dash-toggle-discovered-worktrees)]
   ["Agent Shell"
    ("as"  "Open"     magit-dash-agent-shell
     :inapt-if-not magit-dash--agent-shell-project-buffers-p)
    ("an"  "New"           magit-dash-agent-shell-new)
    ("aq"  "Queue"     magit-dash-agent-shell-queue)]
   ["Cache"
    ("ci"  "Info"      magit-dash-cache-info)
    ("chr" "Reset"     magit-dash-cache-reset-at-point
     :inapt-if-not magit-dash--repo-at-point-p)
    ("cha" "Reset all"       magit-dash-cache-reset-all)]]
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
     :inapt-if-not magit-dash--repo-at-point-ahead-p)]
   ["Batch"
    ("SPC" "Toggle mark"     magit-dash-toggle-mark
     :inapt-if-not magit-dash--repo-at-point-p
     :transient t)
    ("mt"  "Mark by tag"     magit-dash-mark-by-tag
     :transient t)
    ("u"   "Clear marks"     magit-dash-unmark-all
     :inapt-if-not magit-dash--has-marks-p
     :transient t)
    ("ma"   (lambda () (if magit-dash--batch-all "Batch: all [on]" "Batch: all [off]"))
     magit-dash-toggle-batch-all
     :transient t)
    ("fa"  "Fetch all"       magit-dash-fetch-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("pa"  "Pull all"        magit-dash-pull-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("pu"  "Push all"        magit-dash-push-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("sa"  "Sync all"        magit-dash-sync-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("ca"  "Commit all"      magit-dash-commit-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("aa"  "Autosync all"    magit-dash-auto-sync
     :inapt-if-not magit-dash--batch-enabled-p)
    ("su"  "Update submodules" magit-dash-submodule-update-all
     :inapt-if-not magit-dash--batch-enabled-p)
    ("B"  (lambda () (if magit-dash--batch-all "Bootstrap all" "Bootstrap marked"))
     magit-dash-bootstrap-marked
     :inapt-if-not magit-dash--has-missing-bootstrap-repos-p)]
   ["Manage"
    ("ac"  "Auto-commit"     magit-dash-commit
     :inapt-if-not magit-dash--has-auto-commit-p)
    ("sy"  "Sync one"        magit-dash-sync
     :inapt-if-not magit-dash--has-auto-sync-p)
    ("cl"  "Clone repo"      magit-dash-clone-repo
     :inapt-if-not magit-dash--repo-at-point-p)
    ("cm"  "Clone missing"   magit-dash-clone-all-missing
     :inapt-if-not magit-dash--has-missing-repos-p)
    ("cb"  "Bootstrap repo"  magit-dash-bootstrap-repo
     :inapt-if-not magit-dash--repo-has-upstream-p)
    ("j"   "Build"           magit-dash-builder
     :inapt-if-not magit-dash--has-auto-commit-p)
    ("x"   "Run command"     magit-dash-run-command
     :inapt-if-not magit-dash--has-commands-p)
    ("X"   "Run in background" magit-dash-run-command-background
     :inapt-if-not magit-dash--has-commands-p)
    ("y"   "Prune branches"  magit-dash-prune-branches
     :inapt-if-not magit-dash--repo-at-point-p)
    ("ps"  "project save buffers" magit-dash-save-project-buffers
     :inapt-if-not magit-dash--repo-at-point-p)
    ("sb"  "Bump submodules" magit-dash-bump-submodules-menu
     :inapt-if-not magit-dash--repo-at-point-p)]
   ["Dashboard"
    ("pr"  "PR dashboard"    magit-gh-pr-dash)
    ("nt"  "Filter by tag"   magit-dash-filter-by-tag)
    ("nb"  (lambda () (if magit-dash-render-branch-name-as-basename
                          "Branch basename [on]"
                        "Branch basename [off]"))
     magit-dash-toggle-branch-basename
     :transient t)
    ("C-t" "Toggle column"   magit-dash-toggle-column)
    ("M-s" "Toggle submodules" magit-dash-toggle-discovered-submodules)
    ("gg"  "Refresh"         magit-dash-refresh)
    ("q"   "Quit"            quit-window)]])



(defun ad:magit-dash--quit-window (orig-fn &optional kill window)
  "Around advice for `quit-window': delete split window in dashboard buffers.
Only applies in `magit-dash-mode' and `magit-dash-gh-pr-dashboard-mode'.
Falls through otherwise."
  (if (or kill (one-window-p)
          (not (derived-mode-p 'magit-dash-mode
                               'magit-dash-gh-pr-dashboard-mode)))
      (funcall orig-fn kill window)
    (delete-window (or window (selected-window)))))

(advice-add 'quit-window :around #'ad:magit-dash--quit-window)

(provide 'magit-dash)

;;; magit-dash.el ends here
