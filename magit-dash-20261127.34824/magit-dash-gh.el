;;; magit-dash-gh.el --- Shared utilities and GitHub integration for magit-dash -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1
;; Keywords: vc, tools, magit, github
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; Provides `magit-dash-gh-menu', an interactive menu for
;; pruning local branches whose remote-tracking PRs are merged or
;; closed. Per-repo candidate and marked-branch state is cached on a
;; hidden state buffer keyed by repository toplevel.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'transient)

(require 'annotated-completing-read)

(declare-function magit-toplevel "magit-git")
(declare-function magit-get "magit-git")
(declare-function magit-list-local-branch-names "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-branch-delete "magit-branch")
(declare-function magit-dash-gh-actions-fetch "magit-dash-gh-actions")
(declare-function magit-dash-gh-workflow-run "magit-dash-gh-actions")
(declare-function magit-dash-pr-fetch "magit-dash-pr")
(declare-function magit-dash-pr--confirm-delete "magit-dash-pr")
(declare-function magit-dash-pr-clean-merged-worktrees "magit-dash-pr")
(declare-function magit-dash-pr-delete-worktree "magit-dash-pr")
(declare-function magit-dash-worktree-main-dir "magit-dash-worktree")
(declare-function magit-dash-worktree-risks "magit-dash-worktree")
(declare-function magit-dash-worktree-uncommitted-changes-p "magit-dash-worktree")
(declare-function magit-dash-open "magit-dash")
(declare-function magit-dash-open-repo "magit-dash-open")
(declare-function yaml-encode "yaml")
(declare-function magit-dash-pr "magit-dash-pr")
(declare-function magit-dash-status-refresh-all "magit-dash-status")
(declare-function magit-dash-repo-p "magit-dash")
(declare-function magit-dash-repo-name "magit-dash")
(declare-function magit-dash-repo-path "magit-dash")
(declare-function magit-dash-repo-gh-account "magit-dash")
(declare-function magit-dash-repo-gh-host "magit-dash")
(declare-function magit-dash--repo-at-point "magit-dash")
(declare-function magit-dash--repo-at-point-p "magit-dash")
(defvar magit-dash-repo-list)

;;; Core helpers shared by all magit-dash-gh-* modules
(defmacro magit-dash-gh--with-repo-dir (path &rest body)
  "Execute BODY with `default-directory' set to PATH."
  (declare (indent 1))
  (let ((p (make-symbol "path")))
    `(let* ((,p ,path)
             (default-directory ,p))
       ,@body)))


(defvar magit-dash-gh--cache (make-hash-table :test #'equal)
  "Global cache for repository-specific data, keyed by repository path.
Each value is a plist containing data like :stats, :pr-counts, etc.")

(defvar magit-dash-gh--cache-requests (make-hash-table :test #'equal)
  "Latest asynchronous cache request, keyed by path and field.")

(defun magit-dash-gh--cache-request (path key)
  "Register and return a new request token for PATH and KEY."
  (let ((token (cons nil nil)))
    (setf (map-elt magit-dash-gh--cache-requests (cons path key)) token)
    token))

(defun magit-dash-gh--cache-set-if-current (path key value token)
  "Cache VALUE for PATH and KEY if TOKEN is still current."
  (when (eq token (map-elt magit-dash-gh--cache-requests (cons path key)))
    (magit-dash-gh--cache-set path key value)))

(defun magit-dash-gh--refresh-current-p (guards path)
  "Return non-nil when PATH's scheduled refresh in GUARDS is current.
Nil GUARDS means that the work was not scheduled by a refresh."
  (or (null guards)
      (eq (map-elt guards path)
          (map-elt magit-dash-gh--cache-requests (cons path :refresh)))))

(defun magit-dash-gh--cache-get (path key)
  "Return the value for KEY in the cache for repo at PATH."
  (plist-get (map-elt magit-dash-gh--cache path) key))

(defun magit-dash-gh--cache-set (path key value)
  "Set the value for KEY in the cache for repo at PATH."
  (let ((plist (map-elt magit-dash-gh--cache path)))
    (setf (map-elt magit-dash-gh--cache path) (plist-put plist key value))))

(defun magit-dash-gh--cache-remove (path &optional key)
  "Remove data for repo at PATH.  If KEY is nil, remove all data."
  (if key
      (map-delete magit-dash-gh--cache-requests (cons path key))
    (seq-do (lambda (request-key)
              (when (equal (car request-key) path)
                (map-delete magit-dash-gh--cache-requests request-key)))
            (map-keys magit-dash-gh--cache-requests)))
  (if key
      (setf (map-elt magit-dash-gh--cache path)
            (seq-mapcat (lambda (pair) (unless (eq (car pair) key) pair))
                        (seq-partition (map-elt magit-dash-gh--cache path) 2)))
    (map-delete magit-dash-gh--cache path)))

(defun magit-dash-gh--check-gh ()
  "Signal `user-error' when the `gh' CLI is not found on PATH."
  (unless (executable-find "gh")
    (user-error "magit-gh: `gh' CLI not found on PATH")))

(defcustom magit-dash-gh-token-ttl 60
  "Seconds a token from `gh auth token' is reused before asking `gh' again.
Nil or 0 disables the cache."
  :type '(choice (const :tag "No cache" nil) number)
  :group 'magit-dash)

(defcustom magit-dash-gh-token-lookup-timeout 5
  "Seconds to wait for `gh auth token' before timing out.
Bounds both asynchronous token priming and the synchronous fallback."
  :type '(choice (const :tag "No timeout" nil) number)
  :group 'magit-dash)

(defvar magit-dash-gh-stderr-grace 2
  "Seconds to wait for a failed `gh' call's stderr to finish arriving.
Only a fallback: normally the stderr process's own sentinel ends the wait.")

(defcustom magit-dash-gh-message-max-length 500
  "Longest `gh' output, in characters, echoed into messages and error strings."
  :type 'integer
  :group 'magit-dash)

(defvar magit-dash-gh--token-cache (make-hash-table :test #'equal)
  "Tokens by (USER . HOST), each value (TOKEN . TIME).")

(defvar magit-dash-gh--token-pending (make-hash-table :test #'equal)
  "In-flight async token lookups by (USER . HOST).")

(defun magit-dash-gh--settle-token-pending (key entry token err)
  "Settle pending token lookup ENTRY for KEY with TOKEN or ERR.
Cancels timers, kills live process and buffers, removes ENTRY from
`magit-dash-gh--token-pending', caches TOKEN on success, and invokes
each callback while isolating errors."
  (unless (plist-get entry :settled)
    (plist-put entry :settled t)
    (when (timerp (plist-get entry :timer))
      (cancel-timer (plist-get entry :timer)))
    (when (timerp (plist-get entry :grace))
      (cancel-timer (plist-get entry :grace)))
    (when (eq (map-elt magit-dash-gh--token-pending key) entry)
      (map-delete magit-dash-gh--token-pending key))
    (when-let* ((proc (plist-get entry :proc)))
      (when (process-live-p proc)
        (delete-process proc)))
    (when-let* ((buf (plist-get entry :buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    (when-let* ((errbuf (plist-get entry :errbuf)))
      (when (buffer-live-p errbuf)
        (kill-buffer errbuf)))
    (when token
      (setf (map-elt magit-dash-gh--token-cache key)
            (cons token (current-time))))
    (seq-do (lambda (cb)
              (condition-case cb-err
                  (funcall cb token err)
                (error
                 (message "magit-gh: token lookup callback failed: %s"
                          (magit-dash-gh--sanitize (error-message-string cb-err))))))
            (nreverse (plist-get entry :callbacks)))))

(defun magit-dash-gh-reset-token-cache ()
  "Forget every cached account token and settle pending token lookups."
  (interactive)
  (seq-do (lambda (pair)
            (magit-dash-gh--settle-token-pending
             (car pair) (cdr pair) nil
             "magit-gh: token lookup cancelled by cache reset"))
          (map-pairs magit-dash-gh--token-pending))
  (clrhash magit-dash-gh--token-pending)
  (clrhash magit-dash-gh--token-cache))

(defconst magit-dash-gh--secret-regexp
  "\\_<gh[pousr]_[[:alnum:]_]+\\|github_pat_[[:alnum:]_]+\\|[Bb]earer[[:blank:]]+[^[:space:]\"']+"
  "Regexp matching GitHub token shapes that must never reach messages.")

(defun magit-dash-gh--sanitize (text)
  "Return TEXT with token-like strings redacted and its length bounded."
  (let ((redacted (replace-regexp-in-string
                   magit-dash-gh--secret-regexp "[redacted]" (or text "") t t)))
    (if (> (length redacted) magit-dash-gh-message-max-length)
        (concat (substring redacted 0 magit-dash-gh-message-max-length) "...[truncated]")
      redacted)))

(defun magit-dash-gh--cached-account-token (user &optional host)
  "Return cached token for USER on HOST when valid within TTL, else nil."
  (let* ((h (or host "github.com"))
         (hit (map-elt magit-dash-gh--token-cache (cons user h))))
    (when (and hit
               (numberp magit-dash-gh-token-ttl)
               (> magit-dash-gh-token-ttl 0)
               (< (float-time (time-since (cdr hit))) magit-dash-gh-token-ttl))
      (car hit))))

(defun magit-dash-gh--prime-account-token-async (user &optional host callback)
  "Resolve and cache the auth token for USER on HOST asynchronously.
HOST defaults to \"github.com\".  Deduplicates concurrent lookups per
\(USER . HOST) and bounds execution to `magit-dash-gh-token-lookup-timeout'
seconds.  When CALLBACK is non-nil, calls it with (TOKEN ERR) on completion,
or immediately when cached."
  (let* ((h (or host "github.com"))
         (key (cons user h))
         (cached (magit-dash-gh--cached-account-token user h))
         (existing (map-elt magit-dash-gh--token-pending key)))
    (cond
     (cached
      (when callback
        (funcall callback cached nil))
      cached)
     ((and existing
           (not (plist-get existing :settled))
           (process-live-p (plist-get existing :proc)))
      (when callback
        (plist-put existing :callbacks (cons callback (plist-get existing :callbacks))))
      (plist-get existing :proc))
     (t
      (let* ((buf (generate-new-buffer " *magit-dash-gh-token*"))
             (errbuf (generate-new-buffer " *magit-dash-gh-token-err*"))
             (entry (list :proc nil
                          :timer nil
                          :grace nil
                          :buf buf
                          :errbuf errbuf
                          :callbacks (when callback (list callback))
                          :settled nil))
             (status nil)
             (code nil)
             (err-done nil)
             (finish-proc
              (lambda ()
                (unless (plist-get entry :settled)
                  (let ((stdout (if (buffer-live-p buf)
                                    (with-current-buffer buf
                                      (string-trim (buffer-string)))
                                  ""))
                        (stderr (if (buffer-live-p errbuf)
                                    (with-current-buffer errbuf
                                      (string-trim (buffer-string)))
                                  "")))
                    (if (and (eq status 'exit)
                             (eql code 0)
                             (not (string-empty-p stdout)))
                        (magit-dash-gh--settle-token-pending key entry stdout nil)
                      (let ((detail (string-trim
                                     (concat stdout
                                             (if (and (not (string-empty-p stdout))
                                                      (not (string-empty-p stderr)))
                                                 "\n"
                                               "")
                                             stderr))))
                        (magit-dash-gh--settle-token-pending
                         key entry nil
                         (format "magit-gh: failed to obtain token for account '%s' on host '%s': %s"
                                 user h (magit-dash-gh--sanitize detail))))))))))
        (setf (map-elt magit-dash-gh--token-pending key) entry)
        (when (and (numberp magit-dash-gh-token-lookup-timeout)
                   (> magit-dash-gh-token-lookup-timeout 0))
          (plist-put
           entry :timer
           (run-with-timer
            magit-dash-gh-token-lookup-timeout nil
            (lambda ()
              (unless (plist-get entry :settled)
                (magit-dash-gh--settle-token-pending
                 key entry nil
                 (format "magit-gh: timed out obtaining token for account '%s' on host '%s'"
                         user h)))))))
        (let ((proc
               (condition-case err
                   (make-process
                    :name "magit-gh-token"
                    :buffer buf
                    :stderr errbuf
                    :command (list "gh" "auth" "token" "-u" user "-h" h)
                    :connection-type 'pipe
                    :noquery t
                    :sentinel
                    (lambda (p _event)
                      (when (and (memq (process-status p) '(exit signal))
                                 (not (plist-get entry :settled)))
                        (setq status (process-status p))
                        (setq code (process-exit-status p))
                        (cond
                         ((or (and (eq status 'exit) (eql code 0)) err-done)
                          (funcall finish-proc))
                         (t
                          (plist-put
                           entry :grace
                           (run-with-timer magit-dash-gh-stderr-grace
                                           nil finish-proc)))))))
                 (error
                  (magit-dash-gh--settle-token-pending
                   key entry nil
                   (format "magit-gh: failed to obtain token for account '%s' on host '%s': %s"
                           user h (magit-dash-gh--sanitize (error-message-string err))))
                  nil))))
          (when proc
            (if (plist-get entry :settled)
                (progn
                  (when (process-live-p proc)
                    (delete-process proc))
                  (when (buffer-live-p buf)
                    (kill-buffer buf))
                  (when (buffer-live-p errbuf)
                    (kill-buffer errbuf)))
              (if-let* ((errproc (get-buffer-process errbuf)))
                  (set-process-sentinel
                   errproc
                   (lambda (p _event)
                     (unless (process-live-p p)
                       (setq err-done t)
                       (when (and code (not (plist-get entry :settled)))
                         (funcall finish-proc)))))
                (setq err-done t))
              (plist-put entry :proc proc)))
          (unless (plist-get entry :settled)
            proc)))))))

(defalias 'magit-dash-gh-prime-account-token #'magit-dash-gh--prime-account-token-async)

(defun magit-dash-gh--account-token (user &optional host callback)
  "Return the auth token for USER on HOST using `gh auth token'.
HOST defaults to \"github.com\".  A successful lookup is reused for
`magit-dash-gh-token-ttl' seconds.

When CALLBACK is non-nil, resolves asynchronously via
`magit-dash-gh--prime-account-token-async' (calling CALLBACK with
\(TOKEN ERR)) without blocking the UI thread; returns a cached token
immediately when available.

When CALLBACK is nil, returns a cached token immediately or uses a bounded
synchronous fallback (up to `magit-dash-gh-token-lookup-timeout' seconds).
The synchronous fallback exists for synchronous callers such as
`magit-dash-gh-with-account'; async callers should prime tokens via
`magit-dash-gh--prime-account-token-async' to avoid UI-thread blocking.
Signals `user-error' if lookup fails or times out; tokens are never logged."
  (let* ((h (or host "github.com"))
         (hit (magit-dash-gh--cached-account-token user h)))
    (cond
     (hit
      (when callback
        (funcall callback hit nil))
      hit)
     (callback
      (magit-dash-gh--prime-account-token-async user h callback)
      nil)
     (t
      (let ((result nil)
            (done nil)
            (deadline (when (and (numberp magit-dash-gh-token-lookup-timeout)
                                 (> magit-dash-gh-token-lookup-timeout 0))
                        (+ (float-time) magit-dash-gh-token-lookup-timeout 0.2))))
        (magit-dash-gh--prime-account-token-async
         user h
         (lambda (tok err)
           (setq result (cons tok err))
           (setq done t)))
        (while (and (not done)
                    (or (null deadline) (< (float-time) deadline)))
          (accept-process-output nil 0.05))
        (unless done
          (let ((key (cons user h))
                (timeout-msg (format "magit-gh: timed out obtaining token for account '%s' on host '%s'"
                                     user h)))
            (when-let* ((pending (map-elt magit-dash-gh--token-pending key)))
              (magit-dash-gh--settle-token-pending key pending nil timeout-msg))
            (user-error "%s" timeout-msg)))
        (if (cdr result)
            (user-error "%s" (cdr result))
          (car result)))))))

(defun magit-dash-gh--call-with-token (host token fn)
  "Call FN with `process-environment' configured for HOST and TOKEN.
When TOKEN is nil, calls FN unchanged."
  (if (null token)
      (funcall fn)
    (let* ((h (or host "github.com"))
           (env-var (if (string= h "github.com")
                        "GH_TOKEN"
                      "GH_ENTERPRISE_TOKEN"))
           (process-environment
            (cons (format "%s=%s" env-var token)
                  (if (not (string= h "github.com"))
                      (cons (format "GH_HOST=%s" h) process-environment)
                    process-environment))))
      (funcall fn))))

(defun magit-dash-gh--call-with-account (account fn)
  "Call FN with `process-environment' configured for ACCOUNT.
ACCOUNT is an account plist (:user USER :host HOST), a string username,
or nil. When nil, calls FN unchanged.
When ACCOUNT is given, resolves the token via `magit-dash-gh--account-token'
and binds GH_TOKEN (or GH_ENTERPRISE_TOKEN) in `process-environment'."
  (if (null account)
      (funcall fn)
    (let* ((user (if (stringp account)
                     account
                   (plist-get account :user)))
           (host (or (when (listp account) (plist-get account :host)) "github.com"))
           (token (magit-dash-gh--account-token user host)))
      (magit-dash-gh--call-with-token host token fn))))

(defmacro magit-dash-gh-with-account (account &rest body)
  "Execute BODY with environment configured for ACCOUNT.
ACCOUNT is an account plist (:user USER :host HOST) or nil.
When ACCOUNT is nil, executes BODY unchanged.
When ACCOUNT is given, resolves the token via `magit-dash-gh--account-token'
and binds GH_TOKEN (for github.com) or GH_ENTERPRISE_TOKEN (for Enterprise
hosts) in `process-environment' for the duration of BODY."
  (declare (indent 1))
  `(magit-dash-gh--call-with-account ,account (lambda () ,@body)))

(define-obsolete-function-alias 'with-magit-gh-account
  #'magit-dash-gh-with-account "0.1")

(defun magit-dash-gh--repo-account (&optional repo-or-path)
  "Return (:user USER :host HOST) plist for REPO-OR-PATH, or nil.
Resolves REPO-OR-PATH to a `magit-dash-repo' struct. When the resolved repo
has a non-nil `:gh-account' slot, returns `(:user USER :host HOST)' with HOST
defaulting to \"github.com\" when nil. Returns nil when REPO-OR-PATH has
no configured account or cannot be resolved to a registered repo."
  (let ((repo
         (cond
          ((and repo-or-path (fboundp 'magit-dash-repo-p) (magit-dash-repo-p repo-or-path))
           repo-or-path)
          ((stringp repo-or-path)
           (let ((exp (expand-file-name repo-or-path)))
             (or (seq-find (lambda (r)
                             (equal (expand-file-name (magit-dash-repo-path r)) exp))
                           (and (boundp 'magit-dash-repo-list) magit-dash-repo-list))
                 (seq-find (lambda (r)
                             (equal (magit-dash-repo-name r) repo-or-path))
                           (and (boundp 'magit-dash-repo-list) magit-dash-repo-list)))))
          ((and (fboundp 'magit-dash--repo-at-point-p) (magit-dash--repo-at-point-p))
           (magit-dash--repo-at-point))
          (t
           (when-let* ((top (or (ignore-errors (magit-toplevel)) default-directory))
                       (exp (expand-file-name top)))
             (seq-find (lambda (r)
                         (equal (expand-file-name (magit-dash-repo-path r)) exp))
                       (and (boundp 'magit-dash-repo-list) magit-dash-repo-list)))))))
    (when (and repo (magit-dash-repo-gh-account repo))
      (list :user (magit-dash-repo-gh-account repo)
            :host (or (magit-dash-repo-gh-host repo) "github.com")))))

;;;###autoload
(defun magit-dash-gh-validate-accounts (&optional repos)
  "Validate that configured `:gh-account' tokens can be resolved for REPOS.
REPOS defaults to `magit-dash-repo-list'.
Tests each repo with a non-nil `:gh-account' by calling
`magit-dash-gh--account-token'. Returns an alist of (REPO-NAME . STATUS) where
STATUS is `ok' or an error message string.
Also emits a warning for any accounts that fail to authenticate."
  (interactive)
  (let* ((target-repos (or repos (and (boundp 'magit-dash-repo-list) magit-dash-repo-list)))
         (results nil))
    (seq-do
     (lambda (repo)
       (when-let* ((account (magit-dash-repo-gh-account repo))
                   (host (or (magit-dash-repo-gh-host repo) "github.com")))
         (condition-case err
             (progn
               (magit-dash-gh--account-token account host)
               (push (cons (magit-dash-repo-name repo) 'ok) results))
           (error
            (let ((msg (error-message-string err)))
              (display-warning 'magit-dash
                               (format "Failed to authenticate account '%s' for repo '%s': %s"
                                       account (magit-dash-repo-name repo) msg)
                               :warning)
              (push (cons (magit-dash-repo-name repo) msg) results))))))
     target-repos)
    (nreverse results)))

(defalias 'magit-dash-gh-validate-repo-accounts #'magit-dash-gh-validate-accounts)


(defun magit-dash-gh--repo-dir ()
  "Return the current git repository root for running `gh' commands.
If inside a linked worktree, resolves to the main repository checkout.
Signals `user-error' when not inside a git repository."
  (let ((top (or (magit-toplevel)
                 (user-error "magit-gh: not inside a git repository"))))
    (if (fboundp 'magit-dash-worktree-main-dir)
        (magit-dash-worktree-main-dir top)
      top)))

(defun magit-dash-gh--git-worktrees (repo-dir)
  "Return worktree alists for REPO-DIR.
Each entry is ((path . PATH) (branch . BRANCH) (head . HEAD))."
  (when (and repo-dir (file-directory-p repo-dir))
    (with-temp-buffer
      (when (zerop (call-process "git" nil (current-buffer) nil
                                 "-C" (expand-file-name repo-dir)
                                 "worktree" "list" "--porcelain"))
        (let ((lines (split-string (buffer-string) "\n" t))
              (worktrees nil)
              (cur-path nil)
              (cur-head nil)
              (cur-branch nil))
          (seq-do
           (lambda (line)
             (cond
              ((string-prefix-p "worktree " line)
               (when cur-path
                 (push `((path . ,cur-path) (head . ,cur-head) (branch . ,cur-branch)) worktrees)
                 (setq cur-head nil)
                 (setq cur-branch nil))
               (setq cur-path (string-trim (substring line 9))))
              ((string-prefix-p "HEAD " line)
               (setq cur-head (string-trim (substring line 5))))
              ((string-prefix-p "branch " line)
               (let ((ref (string-trim (substring line 7))))
                 (setq cur-branch (replace-regexp-in-string "^refs/heads/" "" ref))))))
           lines)
          (when cur-path
            (push `((path . ,cur-path) (head . ,cur-head) (branch . ,cur-branch)) worktrees))
          (nreverse worktrees))))))

(defun magit-dash-gh--has-sub-worktrees-p (repo-dir)
  "Return non-nil if REPO-DIR contains secondary git worktrees."
  (let ((wts (ignore-errors (magit-dash-gh--git-worktrees repo-dir))))
    (seq-some (lambda (wt)
                (let ((p (alist-get 'path wt)))
                  (and p
                       (not (string= (directory-file-name (expand-file-name p))
                                     (directory-file-name (expand-file-name repo-dir))))
                       (file-directory-p p))))
              wts)))

(defun magit-dash-gh--branch-worktree (branch repo-dir)
  "Return path of secondary worktree checking out BRANCH in REPO-DIR, or nil."
  (let ((wts (ignore-errors (magit-dash-gh--git-worktrees repo-dir))))
    (seq-some (lambda (wt)
                (let ((b (alist-get 'branch wt))
                      (p (alist-get 'path wt)))
                  (when (and p b (equal b branch)
                             (not (string= (directory-file-name (expand-file-name p))
                                           (directory-file-name (expand-file-name repo-dir))))
                             (file-directory-p p))
                    p)))
              wts)))

(defun magit-dash-gh--git-merged-branches (default-branch repo-dir)
  "Return list of local branches merged into DEFAULT-BRANCH in REPO-DIR."
  (when (and repo-dir (file-directory-p repo-dir) default-branch)
    (let ((default-directory (expand-file-name repo-dir)))
      (ignore-errors
        (with-temp-buffer
          (when (= 0 (call-process "git" nil (current-buffer) nil
                                   "branch" "--format=%(refname:short)" "--merged" default-branch))
            (seq-filter (lambda (b) (not (string-empty-p b)))
                        (split-string (buffer-string) "\n" t "[ \t\n]+"))))))))

(defvar magit-dash-prune-pr-limit 50
  "Maximum number of PRs to fetch from GitHub on each incremental scan.")

(defvar magit-dash-prune-cache-dir nil
  "Directory for per-repo closed-PR cache files, or nil to disable disk caching.")

(defun magit-dash-gh--pr-closed-p (pr)
  "Return non-nil when PR alist represents a merged or closed PR."
  (and pr (not (eq pr :null)) (or (listp pr) (hash-table-p pr))
       (let ((state (map-elt pr 'state)))
         (and (stringp state)
              (member (upcase state) '("MERGED" "CLOSED"))))))

(defun magit-dash-gh--default-branch (&optional dir)
  "Return the repository's default branch name, falling back to `main'.
Prioritises cached info and local git references before calling
`gh repo view'."
  (let* ((dir (or dir (magit-dash-gh--repo-dir)))
         (default-directory dir)
         (cached (magit-dash-gh--cache-get dir :default-branch)))
    (if cached
        cached
      (let* ((local-default
              (or (when-let* ((ref (ignore-errors
                                     (magit-git-string "symbolic-ref" "--short" "refs/remotes/origin/HEAD"))))
                    (replace-regexp-in-string "^origin/" "" ref))
                  (ignore-errors (magit-get "init" "defaultBranch"))
                  (when (magit-branch-p "main") "main")
                  (when (magit-branch-p "master") "master")))
             (branch
              (if (and local-default (not (string-empty-p local-default)))
                  local-default
                (let ((output (ignore-errors
                                (magit-dash-gh-with-account (magit-dash-gh--repo-account dir)
                                  (string-trim
                                   (shell-command-to-string
                                    "gh repo view --json defaultBranchRef --jq .defaultBranchRef.name"))))))
                  (if (and output
                           (not (string-empty-p output))
                           (not (string-match-p "[[:space:]]" output)))
                      output
                    "main")))))
        (when branch
          (magit-dash-gh--cache-set dir :default-branch branch))
        branch))))

(defun magit-dash-gh--prune-cache-file ()
  "Return cache file path for current repo, or nil when caching is disabled.
Uses repo directory basename plus a short hash for uniqueness."
  (when magit-dash-prune-cache-dir
    (let* ((dir (magit-dash-gh--repo-dir))
	   (basename (file-name-nondirectory (directory-file-name dir)))
	   (short-hash (substring (secure-hash 'sha1 dir) 0 8)))
      (expand-file-name
       (format "%s-%s.eld" basename short-hash)
       magit-dash-prune-cache-dir))))

(defun magit-dash-gh--prune-load-cache ()
  "Return a hash table of headRefName→PR-alist from the repo's cache file.
Returns an empty hash table when caching is disabled, no cache exists,
or the file is unreadable."
  (let ((empty (make-hash-table :test #'equal)))
    (if-let* ((file (magit-dash-gh--prune-cache-file))
              ((file-exists-p file)))
        (condition-case err
            (map-into
             (seq-map (lambda (pr) (cons (map-elt pr 'headRefName) pr))
                      (with-temp-buffer
                        (insert-file-contents file)
                        (goto-char (point-min))
                        (read (current-buffer))))
             '(hash-table :test equal))
          (error (message "magit-gh prune: ignoring unreadable cache: %s" err) empty))
      empty)))

(defun magit-dash-gh--prune-save-cache (table)
  "Persist the closed-PR hash TABLE to the repo cache file.
No-op when `magit-dash-prune-cache-dir' is nil."
  (when-let* ((file (magit-dash-gh--prune-cache-file)))
    (make-directory magit-dash-prune-cache-dir t)
    (with-temp-file file
      (prin1 (seq-uniq (map-values table)) (current-buffer)))))

(defun magit-dash-gh--fetch-closed-prs (&optional table)
  "Fetch recent PRs from GitHub, merge into TABLE, persist and return it.
TABLE defaults to the on-disk cache for the current repo.
Uses a single gh call fetching up to `magit-dash-prune-pr-limit' PRs."
  (let* ((default-directory (magit-dash-gh--repo-dir))
	 (table (or table (magit-dash-gh--prune-load-cache)))
	 (cmd (format "gh pr list --state all --limit %d --json number,state,headRefName,headRefOid,title,isCrossRepository,headRepositoryOwner"
		      magit-dash-prune-pr-limit))
	 (output (magit-dash-gh-with-account (magit-dash-gh--repo-account default-directory)
                   (string-trim (shell-command-to-string cmd)))))
    (when (string-prefix-p "[" output)
      (thread-last (json-parse-string output :array-type 'list :object-type 'alist)
        (seq-filter #'magit-dash-gh--pr-closed-p)
        (seq-do (lambda (pr)
                  (let* ((num (map-elt pr 'number))
                         (head (map-elt pr 'headRefName))
                         (pr-tag (when num (format "pr-%s" num)))
                         (keys (delq nil (list head
                                               (and head (concat "wt/" head))
                                               pr-tag
                                               (and pr-tag (concat "wt/" pr-tag))))))
                    (seq-do (lambda (key)
                              (let ((existing (map-elt table key)))
                                (when (or (null existing)
                                          (and (not (map-elt existing 'headRefOid))
                                               (map-elt pr 'headRefOid)))
                                  (setf (map-elt table key) pr))))
                            keys))))))
    (magit-dash-gh--prune-save-cache table)
    table))

(defun magit-dash-gh--git-ok-p (path &rest args)
  "Return non-nil when git ARGS run in PATH exit 0.  Output is discarded."
  (zerop (apply #'call-process "git" nil nil nil "-C" (expand-file-name path) args)))

(defun magit-dash-gh--branch-tip (branch path)
  "Return the commit hash of local BRANCH in PATH, or nil."
  (with-temp-buffer
    (when (zerop (call-process "git" nil (list (current-buffer) nil) nil
                               "-C" (expand-file-name path)
                               "rev-parse" "--verify" "--quiet"
                               (concat "refs/heads/" branch "^{commit}")))
      (string-trim (buffer-string)))))

(defun magit-dash-gh--commit-verified (oid pr path default-branch)
  "Return why commit OID in PATH is safe to discard, or nil.
The symbol is `head-oid' when OID is the head of closed PR (its
`headRefOid'), `merged' when OID is reachable from DEFAULT-BRANCH, or
`remote' when some remote-tracking ref contains it.  A name match alone
proves nothing, so nil means the commits may exist nowhere else."
  (let ((pr-oid (and (or (listp pr) (hash-table-p pr)) (map-elt pr 'headRefOid))))
    (cond
     ((not (and (stringp oid) (not (string-empty-p oid)))) nil)
     ((and (stringp pr-oid) (equal pr-oid oid)) 'head-oid)
     ((and default-branch
           (magit-dash-gh--git-ok-p path "merge-base" "--is-ancestor" oid default-branch))
      'merged)
     ((with-temp-buffer
        (and (zerop (call-process "git" nil (list (current-buffer) nil) nil
                                  "-C" (expand-file-name path)
                                  "for-each-ref" "--count=1" "--format=%(refname)"
                                  "--contains" oid "refs/remotes"))
             (not (string-empty-p (string-trim (buffer-string))))))
      'remote))))

(defun magit-dash-gh--branch-verified (branch pr path default-branch)
  "Return why local BRANCH in PATH is safe to delete, or nil.
PR and DEFAULT-BRANCH are as for `magit-dash-gh--commit-verified'."
  (magit-dash-gh--commit-verified
   (magit-dash-gh--branch-tip branch path) pr path default-branch))

(defun magit-dash-gh--prune-scan ()
  "Re-scan local branches against the closed/merged PR cache and git status.
Merges fresh GitHub data into the in-memory table stored in `:closed-prs'
(seeded from disk on first call), persists the result, and rebuilds
`:candidates'.  Excludes the default branch and the currently checked-out
branch, catching squash-merged branches whose remote tracking ref is gone
as well as branches merged locally in git.
Also scans for worktrees associated with merged branches or PRs.
Stale marked branches are dropped from `:marked'.  Returns new candidates."
  (let* ((path (magit-dash-gh--repo-dir))
         (default-directory path)
         (state (magit-dash-gh--cache-get path :prune-state))
         (prev-marked (plist-get state :marked))
         (def-branch (magit-dash-gh--default-branch))
         (cur-branch (magit-get-current-branch))
         (protected (list def-branch cur-branch))
         (closed-prs (magit-dash-gh--fetch-closed-prs (plist-get state :closed-prs)))
         (git-merged (magit-dash-gh--git-merged-branches def-branch path))
         (all-wts (magit-dash-gh--git-worktrees path))
         (sub-wts (seq-filter (lambda (wt)
                                (let ((wt-path (alist-get 'path wt)))
                                  (and wt-path
                                       (not (string= (directory-file-name (expand-file-name wt-path))
                                                     (directory-file-name (expand-file-name path))))
                                       (file-directory-p wt-path))))
                              all-wts))
         (candidates nil)
         (merged-worktrees nil))
    ;; 1. Collect candidate branches: a closed PR matched by name only counts
    ;; when the branch tip is that PR's head, merged, or on a remote.
    (seq-do
     (lambda (branch)
       (unless (member branch protected)
         (let ((pr (or (map-elt closed-prs branch)
                       (when (string-prefix-p "wt/" branch)
                         (map-elt closed-prs (substring branch 3))))))
           (cond
            ((and pr (magit-dash-gh--branch-verified branch pr path def-branch))
             (push (cons branch pr) candidates))
            ((member branch git-merged)
             (push (cons branch '((number . nil) (state . "MERGED")
                                  (title . "merged into default branch (git)")))
                   candidates))))))
     (magit-list-local-branch-names))
    (setq candidates (nreverse candidates))
    ;; 2. Collect merged worktrees, verified the same way by their HEAD.
    (seq-do
     (lambda (wt)
       (let* ((wt-path (alist-get 'path wt))
              (wt-branch (alist-get 'branch wt))
              (name (file-name-nondirectory (directory-file-name wt-path)))
              (named-pr (or (when wt-branch (map-elt closed-prs wt-branch))
                            (when (and wt-branch (string-prefix-p "wt/" wt-branch))
                              (map-elt closed-prs (substring wt-branch 3)))
                            (map-elt closed-prs name)
                            (when (string-prefix-p "wt/" name)
                              (map-elt closed-prs (substring name 3)))))
              (matched-pr (cond
                           ((and named-pr
                                 (magit-dash-gh--commit-verified
                                  (alist-get 'head wt) named-pr path def-branch))
                            named-pr)
                           ((and wt-branch (member wt-branch git-merged))
                            '((number . nil) (state . "MERGED")
                              (title . "branch merged in git"))))))
         (when matched-pr
           (push (cons wt-path (cons wt-branch matched-pr)) merged-worktrees))))
     sub-wts)
    (setq merged-worktrees (nreverse merged-worktrees))
    (magit-dash-gh--cache-set path
     :prune-state (list :candidates candidates
                        :marked (seq-filter (lambda (m) (member m (seq-map #'car candidates)))
                                            prev-marked)
                        :merged-worktrees merged-worktrees
                        :closed-prs closed-prs))
    candidates))

(defun magit-dash-gh--prune-format-annotation (pr)
  "Return a one-line annotation string describing PR alist."
  (if (or (null pr) (eq pr :null) (not (or (listp pr) (hash-table-p pr))))
      ""
    (let ((num (map-elt pr 'number))
          (state (map-elt pr 'state))
          (title (map-elt pr 'title)))
      (if num
          (format "PR #%s %s: %s" num state title)
        (format "Git %s: %s" state title)))))

(defun magit-dash-gh--prune-build-menu (candidates marked &optional merged-worktrees)
  "Build a hash-table menu for the prune command.
CANDIDATES is the alist of (BRANCH . PR-ALIST). MARKED is the list of
branch names currently marked for batch pruning. MERGED-WORKTREES is either
an alist of (WT-PATH . (BRANCH . PR-ALIST)) or `all' when sub-worktrees exist.
Branch entries are labeled `prune: BRANCH' with optional ` [marked]' or
` [worktree]' suffixes, and carry BRANCH itself as their target."
  (let ((has-wts (and merged-worktrees (not (eq merged-worktrees 'all))))
        (any-wts (not (null merged-worktrees))))
    (map-into
     (append
      (list (cons "exit menu" "leave the menu without further action")
            (cons "refresh" "re-scan PRs, branches, and worktrees (returns to menu)"))
      (when any-wts
        (list (cons "clean merged worktrees"
                    (if has-wts
                        (format "delete %d clean worktree(s) for merged PRs" (length merged-worktrees))
                      "scan repository and delete clean worktrees for merged PRs"))))
      (when (and candidates any-wts)
        (list (cons "clean worktrees & prune all branches"
                    (if has-wts
                        (format "clean %d worktree(s), then delete %d branch(es) (with prompt)"
                                (length merged-worktrees) (length candidates))
                      (format "clean merged worktrees, then delete %d candidate branch(es)"
                              (length candidates))))))
      (when candidates
        (list (cons "prune all branches (no prompt)"
                    (format "delete all %d candidate branch(es)" (length candidates)))
              (cons "prune all branches (with prompt)"
                    (format "delete %d candidate(s), confirming each (y/n/q/!)"
                            (length candidates)))
              (cons "mark branch for pruning"
                    "toggle the mark on a branch for batch pruning")))
      (when marked
        (list (cons "prune marked branches"
                    (format "delete %d marked branch(es)" (length marked)))))
      (when has-wts
        (seq-map (lambda (entry)
                   (let* ((wt-path (car entry))
                          (wt-name (file-name-nondirectory (directory-file-name wt-path)))
                          (wt-info (cdr entry))
                          (wt-branch (car wt-info))
                          (pr (cdr wt-info)))
                     (cons (format "clean worktree: %s" wt-name)
                           (cons (format "%s (branch: %s, at %s)"
                                         (magit-dash-gh--prune-format-annotation pr)
                                         (or wt-branch "detached")
                                         wt-path)
                                 wt-path))))
                 merged-worktrees))
      (seq-map (lambda (entry)
                 (let* ((branch (car entry))
                        (suffix (if (member branch marked) " [marked]" "")))
                   (cons (format "prune: %s%s" branch suffix)
                         (cons (magit-dash-gh--prune-format-annotation (cdr entry)) branch))))
               candidates))
     '(hash-table :test equal))))

(defun magit-dash-gh--prune-delete-risks (branch wt-path path)
  "Return (VERIFIED . RISKS) for deleting BRANCH (and WT-PATH) in PATH.
VERIFIED is the reason from `magit-dash-gh--branch-verified', or nil.  RISKS
lists the worktree risks of WT-PATH plus, when VERIFIED is nil, a note that
the branch tip is merged nowhere, on no remote and not its PR's head."
  (let* ((pr (alist-get branch (plist-get (magit-dash-gh--cache-get path :prune-state)
                                          :candidates)
                        nil nil #'equal))
         (pr-oid (and (listp pr) (map-elt pr 'headRefOid)))
         (verified (magit-dash-gh--branch-verified
                    branch pr path (ignore-errors (magit-dash-gh--default-branch)))))
    (cons verified
          (append (when wt-path
                    (magit-dash-worktree-risks wt-path pr-oid))
                  (unless verified
                    (list (format "branch %s is not merged, on no remote and not its closed PR's head"
                                  branch)))))))

(defun magit-dash-gh--prune-delete-branches (branches path &optional prompt-p)
  "Delete each branch in BRANCHES for repo at PATH, prompting only when PROMPT-P.
A branch with an attached worktree has that worktree removed first.  Nothing
is deleted when the worktree has uncommitted changes.  Before any removal
`magit-dash-worktree-risks' (unpublished commits, ignored files that are not
build output, in-progress merges...) and a branch-level check (tip merged,
on a remote or its closed PR's head) run.  An item with risks is skipped
\(and listed in `:details') without PROMPT-P; with PROMPT-P it needs an
explicit yes even in yes-to-all mode.  The branch is deleted with `-D' only
when verified, otherwise with `-d'.
After deletion, re-scan the cache for PATH.
Returns a plist (:deleted N :skipped M :quit BOOL :details STRINGS).
With PROMPT-P, the user is prompted y/n/q/! per branch; `!' enables
yes-to-all for the remainder, `q' terminates the loop."
  (require 'magit-dash-worktree)
  (require 'magit-dash-pr)
  (let ((result
         (seq-reduce
          (lambda (state branch)
            (if (plist-get state :quit)
                state
              (let* ((wt-path (magit-dash-gh--branch-worktree branch path))
                     (prompt-msg (if wt-path
                                     (format "Delete branch %s and its worktree at %s? (y/n/q/!) "
                                             branch (file-name-nondirectory (directory-file-name wt-path)))
                                   (format "Delete %s? (y/n/q/!) " branch)))
                     (answer (if (plist-get state :yes-to-all)
                                 ?y
                               (read-char-choice prompt-msg '(?y ?n ?q ?!))))
                     (skip (lambda (why)
                             (message "magit-gh prune: skipping %s: %s" branch why)
                             (setq state (plist-put state :details
                                                    (append (plist-get state :details)
                                                            (list (format "%s: %s" branch why)))))
                             (plist-put state :skipped (1+ (plist-get state :skipped))))))
                (pcase answer
                  ((or ?y ?!)
                   (when (eq answer ?!)
                     (setq state (plist-put state :yes-to-all t)))
                   (if (and wt-path (magit-dash-worktree-uncommitted-changes-p wt-path))
                       (funcall skip (format "worktree %s has uncommitted changes" wt-path))
                     (let* ((checked (magit-dash-gh--prune-delete-risks branch wt-path path))
                            (verified (car checked))
                            (risks (cdr checked))
                            (confirmed nil))
                       (cond
                        ((and risks
                              (not (setq confirmed
                                         (magit-dash-pr--confirm-delete
                                          (or wt-path (expand-file-name path))
                                          prompt-p branch risks))))
                         (funcall skip (string-join risks "; ")))
                        ((and wt-path
                              (not (magit-dash-gh--git-ok-p path "worktree" "remove" wt-path)))
                         (funcall skip (format "git could not remove worktree %s" wt-path)))
                        (t
                         (when wt-path
                           (magit-dash-gh--git-ok-p path "worktree" "prune"))
                         (if (or verified (magit-dash-gh--git-ok-p path "branch" "-d" branch))
                             (progn
                               (when verified
                                 (magit-dash-gh--with-repo-dir path
                                   (with-no-warnings (magit-branch-delete (list branch) t))))
                               (plist-put state :deleted (1+ (plist-get state :deleted))))
                           (funcall skip (if confirmed
                                             "worktree removed; git refused -d for the unmerged branch"
                                           "git refused -d for the unmerged branch"))))))))
                  (?n (plist-put state :skipped (1+ (plist-get state :skipped))))
                  (?q (plist-put state :quit t))))))
          branches
          (list :deleted 0 :skipped 0 :yes-to-all (not prompt-p) :quit nil :details nil))))
    (magit-dash-gh--prune-scan)
    (message "magit-gh prune: deleted %d, skipped %d%s"
             (plist-get result :deleted)
             (plist-get result :skipped)
             (if (plist-get result :quit) " (quit)" ""))
    (list :deleted (plist-get result :deleted)
          :skipped (plist-get result :skipped)
          :quit (plist-get result :quit)
          :details (plist-get result :details))))

(defun magit-dash-gh--prune-toggle-mark (path)
  "Prompt for a branch from the cache for repo at PATH and toggle its mark.
Updates `:marked' in the cached prune state."
  (let* ((state (magit-dash-gh--cache-get path :prune-state))
	 (candidates (or (plist-get state :candidates)
			 (user-error "No candidate branches to mark")))
	 (marked (plist-get state :marked))
	 (branch (annotated-completing-read
                  (map-into
                   (seq-map (lambda (entry)
                              (let* ((branch (car entry))
                                     (label (format "%s%s" branch
                                                    (if (member branch marked) " [marked]" ""))))
                                (cons label (cons (magit-dash-gh--prune-format-annotation (cdr entry)) branch))))
                            candidates)
                   '(hash-table :test equal))
                  :prompt "toggle mark => "
                  :category 'magit-dash-gh-mark
                  :require-match t))
	 (new-marked (if (member branch marked)
			 (delete branch (copy-sequence marked))
		       (cons branch marked))))
    (magit-dash-gh--cache-set path :prune-state
                              (plist-put state :marked new-marked))))

(defun magit-dash-gh--prune-dispatch (label path)
  "Dispatch the menu action for LABEL given repo PATH.
LABEL is either one of the fixed action strings from
`magit-dash-gh--prune-build-menu', a branch name, or a worktree path.
Throws `magit-dash-gh--prune-exit' to terminate the menu loop."
  (let* ((state (magit-dash-gh--cache-get path :prune-state))
	 (candidates (plist-get state :candidates))
	 (marked (plist-get state :marked))
         (merged-wts (plist-get state :merged-worktrees)))
    (cond
     ((equal label "exit menu")
      (throw 'magit-dash-gh--prune-exit nil))
     ((equal label "refresh")
      (magit-dash-gh--prune-scan))
     ((equal label "clean merged worktrees")
      (if (fboundp 'magit-dash-pr-clean-merged-worktrees)
          (magit-dash-pr-clean-merged-worktrees path t)
        (user-error "magit-dash-pr-clean-merged-worktrees is not available"))
      (magit-dash-gh--prune-scan))
     ((equal label "clean worktrees & prune all branches")
      (if (fboundp 'magit-dash-pr-clean-merged-worktrees)
          (magit-dash-pr-clean-merged-worktrees path t)
        (message "magit-gh: magit-dash-pr-clean-merged-worktrees unavailable; skipping worktree cleanup"))
      (when candidates
        (magit-dash-gh--prune-delete-branches (seq-map #'car candidates) path t))
      (magit-dash-gh--prune-scan))
     ((equal label "prune all branches (no prompt)")
      (magit-dash-gh--prune-delete-branches (seq-map #'car candidates) path nil))
     ((equal label "prune all branches (with prompt)")
      (magit-dash-gh--prune-delete-branches (seq-map #'car candidates) path t))
     ((equal label "mark branch for pruning")
      (magit-dash-gh--prune-toggle-mark path))
     ((equal label "prune marked branches")
      (magit-dash-gh--prune-delete-branches marked path nil))
     ((assoc label candidates)
      (magit-dash-gh--prune-delete-branches (list label) path nil))
     ((assoc label merged-wts)
      (if (fboundp 'magit-dash-pr-delete-worktree)
          (let* ((entry (cdr (assoc label merged-wts)))
                 (pr (cdr entry))
                 (pr-oid (and (or (listp pr) (hash-table-p pr))
                              (map-elt pr 'headRefOid))))
            (if pr-oid
                (magit-dash-pr-delete-worktree label path pr-oid)
              (magit-dash-pr-delete-worktree label path)))
        (user-error "magit-dash-pr-delete-worktree is not available"))
      (magit-dash-gh--prune-scan))
     (t
      (user-error "Unknown menu label: %s" label)))))

;;;###autoload
(defun magit-dash-prune-merged-branches ()
  "Open a menu to prune local branches whose PRs are merged or closed.

Per-repo cache (candidates + marked list) lives on the unified global cache
keyed by repository toplevel. The menu loops until you select `exit
menu'. The menu offers:

  - exit menu
  - refresh
  - clean merged worktrees
  - clean worktrees & prune all branches
  - prune all branches (no prompt)
  - prune all branches (with prompt)
  - mark branch for pruning
  - prune marked branches (when any are marked)
  - clean worktree: WT-NAME  (one entry per merged worktree)
  - prune: BRANCH [marked]   (one entry per candidate)"
  (interactive)
  (magit-dash-gh--check-gh)
  (let ((path (magit-dash-gh--repo-dir)))
    (message "magit-gh prune: scanning for branches and worktrees...")
    (magit-dash-gh--prune-scan)
    (catch 'magit-dash-gh--prune-exit
      (while t
	(let* ((state (magit-dash-gh--cache-get path :prune-state))
               (wts (or (plist-get state :merged-worktrees)
                        (and (magit-dash-gh--has-sub-worktrees-p path) 'all)))
	       (table (magit-dash-gh--prune-build-menu
		       (plist-get state :candidates)
		       (plist-get state :marked)
                       wts))
	       (label (annotated-completing-read
		       table
		       :prompt "magit-gh prune => "
		       :category 'magit-dash-prune
		       :require-match t)))
	  (magit-dash-gh--prune-dispatch label path))))))

;;;###autoload
(defun magit-dash-prune-prefetch ()
  "Seed the in-memory closed-PR cache from disk for the current repo.
Makes no GitHub API calls; safe to call on status load.
Skips silently when caching is disabled, not in a git repo, or the
in-memory cache is already populated."
  (when (and magit-dash-prune-cache-dir
	     (ignore-errors (magit-toplevel)))
    (let* ((path (magit-dash-gh--repo-dir))
           (state (magit-dash-gh--cache-get path :prune-state)))
      (unless (plist-get state :closed-prs)
        (let ((table (magit-dash-gh--prune-load-cache)))
          (magit-dash-gh--cache-set path :prune-state
                               (plist-put state :closed-prs table)))))))

;;;###autoload
(defun magit-dash-prune-lazy-prefetch ()
  "Seed the closed-PR cache for the current repo via a 3-second idle timer."
  (run-with-idle-timer 3 nil #'magit-dash-prune-prefetch))

;;; Shared utilities


(defun magit-dash-gh--add-file (ctx path type)
  "Return CTX with a new {:path PATH :type TYPE} entry appended to :files."
  (plist-put ctx :files
             (append (plist-get ctx :files)
                     (list (list :path path :type type)))))

(defun magit-dash-gh--branch-slug (branch)
  "Return BRANCH lowercased with non-alphanumeric chars replaced by hyphens."
  (downcase (replace-regexp-in-string "[^a-z0-9]+" "-" branch)))

(defun magit-dash-gh--make-error-handler (prefix label)
  "Return an on-error callback that messages with PREFIX for step LABEL."
  (lambda (output code)
    (message "%s: %s step failed (exit %d): %s"
             prefix label code (string-trim output))))

;;; Collect infrastructure

(defvar magit-dash-gh-collect-base-dir nil
  "Base directory for artifact downloads, or nil to use <project-root>/plans/.")

(defvar magit-dash-gh-collect-name-function #'magit-dash-gh--collect-default-name
  "Function (TYPE SLUG ID) => string naming a collection run directory.
TYPE is a symbol (`ci' or `pr'), SLUG is a branch or PR slug string,
ID is a run-id or PR number.  The result is used as a subdirectory
name under the base collection directory.")

(defvar magit-dash-gh-collect-index-format 'json
  "Serialization format for index files written by collect commands.
`json' (default, always available) or `yaml' (requires the `yaml' package).")

(defun magit-dash-gh--collect-default-name (type slug id)
  "Return TYPE-SLUG-ID with non-alphanumeric characters replaced by hyphens."
  (downcase
   (replace-regexp-in-string
    "[^a-z0-9-]+" "-"
    (format "%s-%s-%s" type slug id))))

(defun magit-dash-gh--collect-dir (root type slug id)
  "Create and return an artifact directory under ROOT for this collection run.
ROOT is the project root directory (captured before any async work).
BASE is `magit-dash-gh-collect-base-dir' or <ROOT>/plans/.
The leaf name is produced by `magit-dash-gh-collect-name-function'."
  (let* ((base (or magit-dash-gh-collect-base-dir
                   (expand-file-name "plans" root)))
         (name (funcall magit-dash-gh-collect-name-function type slug id))
         (dir  (expand-file-name name base)))
    (make-directory dir t)
    dir))

(defun magit-dash-gh--index-table (&rest kvs)
  "Build a string-keyed hash-table for JSON/YAML serialization from KVS pairs.
KVS is a flat list of alternating string KEY and VALUE arguments."
  (let ((ht (make-hash-table :test #'equal)))
    (while kvs
      (setf (map-elt ht (pop kvs)) (pop kvs)))
    ht))

(defun magit-dash-gh--file-table (path type)
  "Return a string-keyed hash-table representing a collected file entry."
  (map-into (list (cons "path" path)
                  (cons "type" type))
            '(hash-table :test equal)))

(defun magit-dash-gh--write-index (dir data)
  "Write DATA hash-table as an index file inside DIR.
Format is controlled by `magit-dash-gh-collect-index-format'."
  (pcase magit-dash-gh-collect-index-format
    ('yaml
     (unless (or (fboundp 'yaml-encode)
                 (and (locate-library "yaml")
                      (require 'yaml)))
       (user-error "magit-gh: yaml format requires the `yaml' package"))
     (with-temp-file (expand-file-name "index.yaml" dir)
       (insert (yaml-encode data))))
    (_
     (with-temp-file (expand-file-name "index.json" dir)
       (insert (json-serialize data :false-object :false :null-object nil))))))

(defun magit-dash-gh--call-process (args dir)
  "Run `gh' synchronously with ARGS in DIR using its configured account.
Return (EXIT-CODE . OUTPUT)."
  (magit-dash-gh-with-account (magit-dash-gh--repo-account dir)
    (let ((default-directory dir))
      (with-temp-buffer
        (let ((code (apply #'call-process "gh" nil (current-buffer) nil args)))
          (cons code (buffer-string)))))))

(defvar magit-dash-gh-retry-attempts 3
  "Total attempts for an async `gh' call when GitHub returns a 5xx gateway error.")

(defvar magit-dash-gh-retry-delay 3
  "Seconds before the first async retry; doubles on each further retry.")

(defun magit-dash-gh--retry-process (args dir on-success on-error attempt cancelled-p handle)
  "Timer entry for a retry of `magit-dash-gh--run-process'.
A failure to start the retry calls ON-ERROR (or messages) instead of
escaping from the timer, so the caller always hears a final outcome."
  (when (consp handle)
    (setcdr handle nil))
  (condition-case err
      (magit-dash-gh--run-process args dir on-success on-error attempt cancelled-p handle)
    (error
     (unless (and cancelled-p (funcall cancelled-p))
       (let ((msg (magit-dash-gh--sanitize (error-message-string err))))
         (if on-error
             (funcall on-error msg 1)
           (message "magit-gh: gh %s retry failed: %s" (car args) msg)))))))

(defun magit-dash-gh--spawn-process (args dir host token on-success &optional on-error attempt cancelled-p handle)
  "Spawn async `gh' process with ARGS in DIR using HOST and TOKEN.
See `magit-dash-gh--run-process' for ON-SUCCESS, ON-ERROR, ATTEMPT,
CANCELLED-P, and HANDLE semantics."
  (magit-dash-gh--call-with-token
   host token
   (lambda ()
     (let* ((default-directory dir)
            (buf (generate-new-buffer " *magit-dash-proc*"))
            (errbuf (generate-new-buffer " *magit-dash-proc-err*"))
            (code nil)
            (err-done nil)
            (done nil)
            (grace nil)
            (finish
             (lambda ()
               (unless done
                 (setq done t)
                 (when (timerp grace)
                   (cancel-timer grace))
                 (let* ((stdout (if (buffer-live-p buf)
                                    (with-current-buffer buf (buffer-string))
                                  ""))
                        (output (if (= code 0)
                                    stdout
                                  (concat stdout
                                          (if (buffer-live-p errbuf)
                                              (with-current-buffer errbuf
                                                (buffer-string))
                                            "")))))
                   (when (buffer-live-p buf)
                     (kill-buffer buf))
                   (when (buffer-live-p errbuf)
                     (kill-buffer errbuf))
                   (when (consp handle)
                     (setcar handle nil))
                   (cond
                    ((and cancelled-p (funcall cancelled-p))
                     nil)
                    ((= code 0)
                     (funcall on-success output))
                    ((and (< (or attempt 1) magit-dash-gh-retry-attempts)
                          (string-match-p "HTTP 50[234]" output))
                     (let ((delay (* magit-dash-gh-retry-delay
                                     (expt 2 (1- (or attempt 1)))
                                     (+ 1.0 (/ (random 250) 1000.0)))))
                       (message "magit-gh: gh %s hit a gateway error, retrying in %.1fs"
                                (car args) delay)
                       (let ((timer (run-with-timer delay nil #'magit-dash-gh--retry-process
                                                    args dir on-success on-error
                                                    (1+ (or attempt 1)) cancelled-p handle)))
                         (when (consp handle)
                           (setcdr handle timer)))))
                    (on-error
                     (funcall on-error (magit-dash-gh--sanitize output) code))
                    (t
                     (message "magit-gh: gh %s exited %d: %s"
                              (car args) code
                              (magit-dash-gh--sanitize (string-trim output)))))))))
            (proc (condition-case err
                      (make-process
                       :name "magit-gh"
                       :buffer buf
                       :stderr errbuf
                       :command (cons "gh" args)
                       :connection-type 'pipe
                       :noquery t
                       :sentinel
                       (lambda (proc _event)
                         (when (memq (process-status proc) '(exit signal))
                           (setq code (process-exit-status proc))
                           (cond
                            ((or (= code 0) err-done)
                             (funcall finish))
                            (t
                             (setq grace (run-with-timer magit-dash-gh-stderr-grace
                                                         nil finish)))))))
                    (error
                     (kill-buffer buf)
                     (kill-buffer errbuf)
                     (signal (car err) (cdr err))))))
       (with-current-buffer buf
         (setq default-directory dir))
       (if-let* ((errproc (get-buffer-process errbuf)))
           (set-process-sentinel
            errproc
            (lambda (p _event)
              (unless (process-live-p p)
                (setq err-done t)
                (when (and code (not done))
                  (funcall finish)))))
         (setq err-done t))
       (when (consp handle)
         (setcar handle proc)
         (setcdr handle nil))
       proc))))

(defun magit-dash-gh--run-process (args dir on-success &optional on-error attempt cancelled-p handle)
  "Run `gh' with ARGS in DIR asynchronously.
Transient HTTP 502/503/504 failures are retried up to
`magit-dash-gh-retry-attempts' times with exponential backoff; ATTEMPT is
the internal 1-based attempt counter.
ON-SUCCESS is called with the stdout string when the process exits 0;
stderr never reaches it, so warnings cannot corrupt JSON.
ON-ERROR is called with stdout followed by stderr, and the exit code,
otherwise; when nil, a `message' is emitted instead.  After a nonzero
exit the decision waits for the stderr pipe to close (its own sentinel;
`magit-dash-gh-stderr-grace' bounds the wait), so late stderr is never
lost and no `accept-process-output' runs inside a sentinel.
CANCELLED-P, a function of no arguments, is consulted when the process
exits and before a retry; when it returns non-nil no callback runs and
no retry starts.
HANDLE, when a cons cell, tracks the live work: its car is the current
process and its cdr the pending retry timer, updated across retries, so a
caller can stop whichever is live (see `magit-dash-gh--handle-cancel').
When DIR no longer exists on disk (for example a worktree removed outside
magit-dash), ON-ERROR is called directly instead of letting `make-process'
signal.  A `make-process' failure kills its buffers and re-signals.
When DIR uses a configured account whose token is not cached yet, the
token is primed asynchronously before spawning `gh' so the UI thread does
not block.  Return the process, or nil."
  (cond
   ((and cancelled-p (funcall cancelled-p))
    nil)
   ((not (file-directory-p dir))
    (let ((msg (format "no such directory: %s" dir)))
      (if on-error
          (funcall on-error msg 1)
        (message "magit-gh: gh %s failed: %s" (car args) msg))
      nil))
   (t
    (let ((account (magit-dash-gh--repo-account dir)))
      (if (null account)
          (magit-dash-gh--spawn-process
           args dir nil nil on-success on-error attempt cancelled-p handle)
        (let* ((user (if (stringp account)
                         account
                       (plist-get account :user)))
               (host (or (when (listp account) (plist-get account :host))
                         "github.com"))
               (cached (magit-dash-gh--cached-account-token user host)))
          (if cached
              (magit-dash-gh--spawn-process
               args dir host cached on-success on-error attempt cancelled-p handle)
            (magit-dash-gh--prime-account-token-async
             user host
             (lambda (token err)
               (unless (and cancelled-p (funcall cancelled-p))
                 (cond
                  (err
                   (if on-error
                       (funcall on-error err 1)
                     (message "magit-gh: gh %s failed: %s" (car args) err)))
                  ((not (file-directory-p dir))
                   (let ((msg (format "no such directory: %s" dir)))
                     (if on-error
                         (funcall on-error msg 1)
                       (message "magit-gh: gh %s failed: %s" (car args) msg))))
                  (t
                   (condition-case spawn-err
                       (magit-dash-gh--spawn-process
                        args dir host token on-success on-error attempt cancelled-p handle)
                     (error
                      (let ((msg (magit-dash-gh--sanitize (error-message-string spawn-err))))
                        (if on-error
                            (funcall on-error msg 1)
                          (message "magit-gh: gh %s failed: %s" (car args) msg))))))))))
            nil)))))))

(defun magit-dash-gh--handle-cancel (handle)
  "Kill the process and cancel the retry timer tracked by HANDLE, if live."
  (when (consp handle)
    (when (process-live-p (car handle))
      (delete-process (car handle)))
    (when (timerp (cdr handle))
      (cancel-timer (cdr handle)))
    (setcar handle nil)
    (setcdr handle nil)))

(defun magit-dash-gh--owner-repo-from-url (url)
  "Extract (OWNER . REPO) pair from URL or slug string if possible."
  (when (and url (stringp url))
    (let ((clean (replace-regexp-in-string "\\.git\\'" "" (string-trim url))))
      (cond
       ((string-match "github\\.com[:/]\\([^/]+\\)/\\([^/]+\\)\\'" clean)
        (cons (match-string 1 clean) (match-string 2 clean)))
       ((string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" clean)
        (cons (match-string 1 clean) (match-string 2 clean)))))))

(defun magit-dash-gh--repo-info (&optional dir)
  "Return a plist :owner :repo :branch for the current repository.
Prioritises cached data, local git remotes, and registered repos to avoid
blocking `gh repo view' network calls."
  (let* ((dir (or dir (magit-dash-gh--repo-dir)))
         (default-directory dir)
         (cached (magit-dash-gh--cache-get dir :repo-info)))
    (if cached
        (plist-put (copy-sequence cached) :branch (magit-get-current-branch))
      (let (owner repo)
        ;; 1. Check local git remotes first (fast, synchronous, 0 network/blocking)
        (let ((urls (ignore-errors
                      (delq nil (list (magit-get "remote" "origin" "url")
                                      (magit-get "remote" "pushdefault" "url")
                                      (magit-get "remote" "upstream" "url"))))))
          (seq-some (lambda (u)
                      (when-let* ((pair (magit-dash-gh--owner-repo-from-url u)))
                        (setq owner (car pair)
                              repo  (cdr pair))
                        t))
                    urls))
        ;; 2. If not found in remotes, check registered repo struct
        (unless (and owner repo)
          (when (boundp 'magit-dash-repo-list)
            (when-let* ((r (seq-find (lambda (it)
                                       (file-equal-p (magit-dash-repo-path it) dir))
                                     magit-dash-repo-list))
                        (raw (or (and (fboundp 'magit-dash-repo-repo)
                                      (magit-dash-repo-repo r))
                                 (and (fboundp 'magit-dash-repo-clone-url)
                                      (magit-dash-repo-clone-url r))
                                 (and (fboundp 'magit-dash-repo-upstream-repo)
                                      (magit-dash-repo-upstream-repo r))))
                        (pair (magit-dash-gh--owner-repo-from-url raw)))
              (setq owner (car pair)
                    repo  (cdr pair)))))
        ;; 3. Fallback to `gh repo view` only if completely unknown locally
        (unless (and owner repo)
          (ignore-errors
            (let* ((output (magit-dash-gh-with-account (magit-dash-gh--repo-account dir)
                             (string-trim
                              (shell-command-to-string
                               "gh repo view --json owner,name --jq '[.owner.login,.name]|@tsv'"))))
                   (parts (split-string output "\t")))
              (when (and (= (length parts) 2)
                         (not (string-empty-p (nth 0 parts)))
                         (not (string-empty-p (nth 1 parts)))
                         (not (string-match-p "[[:space:]]" (nth 0 parts)))
                         (not (string-match-p "[[:space:]]" (nth 1 parts))))
                (setq owner (nth 0 parts)
                      repo  (nth 1 parts))))))
        (let ((res (list :owner  owner
                         :repo   repo
                         :branch (magit-get-current-branch))))
          (when (and owner repo)
            (magit-dash-gh--cache-set dir :repo-info res))
          res)))))

;;; Account switching

(defun magit-dash-gh--parse-auth-hosts-json (json-str)
  "Parse JSON-STR from `gh auth status --json hosts' into account plists.
Returns a list of (:host HOST :user USER :active ACTIVE-P) plists."
  (let* ((parsed (json-parse-string json-str :object-type 'alist :array-type 'list :false-object :false))
         (hosts (map-elt parsed 'hosts)))
    (thread-last hosts
                 (seq-mapcat
                  (lambda (host-entry)
                    (let ((host-name (format "%s" (car host-entry))))
                      (seq-map
                       (lambda (u)
                         (list :host (or (map-elt u 'host) host-name)
                               :user (map-elt u 'login)
                               :active (eq (map-elt u 'active) t)))
                       (cdr host-entry))))))))

(defun magit-dash-gh--auth-accounts ()
  "Return a list of (:host HOST :user USER :active ACTIVE-P) plists.
Parsed from `gh auth status --json hosts' (with fallback to regex parsing of
`gh auth status')."
  (magit-dash-gh--check-gh)
  (let ((accounts nil))
    (condition-case nil
        (with-temp-buffer
          (let ((code (call-process "gh" nil (list (current-buffer) nil) nil "auth" "status" "--json" "hosts")))
            (when (= code 0)
              (setq accounts (magit-dash-gh--parse-auth-hosts-json (buffer-string))))))
      (error nil))
    (or accounts
        ;; Fallback to regex parsing
        (with-temp-buffer
          (insert (shell-command-to-string "gh auth status 2>&1"))
          (goto-char (point-min))
          (let (regex-accounts)
            (while (re-search-forward
                    "Logged in to \\([^[:space:]]+\\) account \\([^[:space:]]+\\).*\n[[:space:]]*- Active account: \\(true\\|false\\)"
                    nil t)
              (push (list :host (match-string 1)
                          :user (match-string 2)
                          :active (string= (match-string 3) "true"))
                    regex-accounts))
            (nreverse regex-accounts))))))

(defun magit-dash-gh--auth-select-account (accounts)
  "Prompt to select one of ACCOUNTS via `annotated-completing-read'.
ACCOUNTS is a list of (:host :user :active) plists. Returns the chosen plist."
  (annotated-completing-read
   (map-into
    (seq-map (lambda (a)
               (cons (format "%s (%s)" (plist-get a :user) (plist-get a :host))
                     (cons "switch to this account" a)))
             accounts)
    '(hash-table :test equal))
   :prompt "switch gh account => "
   :category 'magit-dash-gh-auth
   :require-match t))

;;;###autoload
(defun magit-dash-gh-auth-switch ()
  "Switch the active `gh' CLI account.
When more than two accounts are configured, prompt to select which to
switch to via `annotated-completing-read'. Otherwise switch directly
to the other configured account."
  (interactive)
  (let* ((accounts (magit-dash-gh--auth-accounts))
         (current (seq-find (lambda (a) (plist-get a :active)) accounts))
         (others (seq-remove (lambda (a) (plist-get a :active)) accounts)))
    (unless current
      (user-error "magit-gh: no active `gh' account found"))
    (unless others
      (user-error "magit-gh: no other `gh' accounts configured"))
    (let ((target (if (= (length others) 1)
                       (car others)
                     (magit-dash-gh--auth-select-account others))))
      (shell-command-to-string
       (format "gh auth switch --hostname %s --user %s"
               (shell-quote-argument (plist-get target :host))
               (shell-quote-argument (plist-get target :user))))
      (message "magit-gh: switched %s -> %s"
               (plist-get current :user) (plist-get target :user)))))

;;;###autoload
(transient-define-prefix magit-dash-gh-menu ()
  "GitHub and Magit Dash operations."
  ["Dashboards"
   ("d" "Repository dashboard"         magit-dash-open)
   ("p" "PR dashboard"                 magit-dash-pr)
   ("o" "Open / switch repository"     magit-dash-open-repo)]
  ["Actions"
   ("y" "Prune merged branches"        magit-dash-prune-merged-branches)
   ("l" "Fetch CI logs"                magit-dash-gh-actions-fetch)
   ("c" "Fetch PR comments"            magit-dash-pr-fetch)
   ("w" "Trigger workflow run"         magit-dash-gh-workflow-run)
   ("u" "Switch users"                 magit-dash-gh-auth-switch)
   ("r" "Refresh status data"          magit-dash-status-refresh-all)]
  ["Quit"
   ("q" "Quit"                         transient-quit-one)])
(provide 'magit-dash-gh)

;;; magit-dash-gh.el ends here
