;;; magit-dash-gh.el --- Shared utilities and GitHub integration for magit-dash -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (magit "4.0") (annotated-completing-read "0.1"))
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
(require 'transient)

(require 'annotated-completing-read)

(declare-function magit-toplevel "magit-git")
(declare-function magit-list-local-branch-names "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-branch-delete "magit-branch")
(declare-function magit-dash-gh-actions-fetch "magit-dash-gh-actions")
(declare-function magit-dash-gh-workflow-run "magit-dash-gh-actions")
(declare-function magit-dash-gh-pr-fetch "magit-dash-gh-pr")
(declare-function magit-dash-open "magit-dash")
(declare-function magit-dash-open-repo "magit-dash-open")
(declare-function magit-gh-pr-dash "magit-dash-gh-pr")
(declare-function magit-dash-status-refresh-all "magit-dash-status")

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

(defun magit-dash-gh--cache-get (path key)
  "Return the value for KEY in the cache for repo at PATH."
  (plist-get (map-elt magit-dash-gh--cache path) key))

(defun magit-dash-gh--cache-set (path key value)
  "Set the value for KEY in the cache for repo at PATH."
  (let ((plist (map-elt magit-dash-gh--cache path)))
    (setf (map-elt magit-dash-gh--cache path) (plist-put plist key value))))

(defun magit-dash-gh--cache-remove (path &optional key)
  "Remove data for repo at PATH. If KEY is nil, remove all data."
  (if key
      (setf (map-elt magit-dash-gh--cache path)
            (seq-mapcat (lambda (pair) (unless (eq (car pair) key) pair))
                        (seq-partition (map-elt magit-dash-gh--cache path) 2)))
    (map-delete magit-dash-gh--cache path)))

(defun magit-dash-gh--check-gh ()
  "Signal `user-error' when the `gh' CLI is not found on PATH."
  (unless (executable-find "gh")
    (user-error "magit-gh: `gh' CLI not found on PATH")))

(defun magit-dash-gh--repo-dir ()
  "Return the current git repository root for running `gh' commands.
Signals `user-error' when not inside a git repository."
  (or (magit-toplevel)
      (user-error "magit-gh: not inside a git repository")))

(defvar magit-dash-gh-prune-pr-limit 100
  "Maximum number of PRs to fetch from GitHub on each incremental scan.")

(defvar magit-dash-gh-prune-cache-dir nil
  "Directory for per-repo closed-PR cache files, or nil to disable disk caching.")

(defun magit-dash-gh--pr-closed-p (pr)
  "Return non-nil when PR alist represents a merged or closed PR."
  (member (map-elt pr 'state) '("MERGED" "CLOSED")))

(defun magit-dash-gh--default-branch ()
  "Return the repository's default branch name, falling back to \"main\"."
  (let* ((default-directory (magit-dash-gh--repo-dir))
	 (output (string-trim
		  (shell-command-to-string
		   "gh repo view --json defaultBranchRef --jq .defaultBranchRef.name"))))
    (if (string-empty-p output) "main" output)))

(defun magit-dash-gh--prune-cache-file ()
  "Return the cache file path for the current repo, or nil when caching is disabled.
The filename uses the repo's directory basename plus a short hash for uniqueness."
  (when magit-dash-gh-prune-cache-dir
    (let* ((dir (magit-dash-gh--repo-dir))
	   (basename (file-name-nondirectory (directory-file-name dir)))
	   (short-hash (substring (secure-hash 'sha1 dir) 0 8)))
      (expand-file-name
       (format "%s-%s.eld" basename short-hash)
       magit-dash-gh-prune-cache-dir))))

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
No-op when `magit-dash-gh-prune-cache-dir' is nil."
  (when-let* ((file (magit-dash-gh--prune-cache-file)))
    (make-directory magit-dash-gh-prune-cache-dir t)
    (with-temp-file file
      (prin1 (map-values table) (current-buffer)))))

(defun magit-dash-gh--fetch-closed-prs (&optional table)
  "Fetch recent PRs from GitHub, merge closed ones into TABLE, persist and return it.
TABLE defaults to the on-disk cache for the current repo.
Uses a single gh call fetching up to `magit-dash-gh-prune-pr-limit' PRs."
  (let* ((default-directory (magit-dash-gh--repo-dir))
	 (table (or table (magit-dash-gh--prune-load-cache)))
	 (cmd (format "gh pr list --state all --limit %d --json number,state,headRefName,title"
		      magit-dash-gh-prune-pr-limit))
	 (output (string-trim (shell-command-to-string cmd))))
    (when (string-prefix-p "[" output)
      (thread-last (json-parse-string output :array-type 'list :object-type 'alist)
        (seq-filter #'magit-dash-gh--pr-closed-p)
        (seq-do (lambda (pr)
                  (let ((branch (map-elt pr 'headRefName)))
                    (unless (map-elt table branch)
                      (setf (map-elt table branch) pr)))))))
    (magit-dash-gh--prune-save-cache table)
    table))

(defun magit-dash-gh--prune-scan ()
  "Re-scan all local branches against the closed/merged PR cache.
Merges fresh GitHub data into the in-memory table stored in `:closed-prs'
(seeded from disk on first call), persists the result, and rebuilds
`:candidates'. Excludes the default branch and the currently checked-out
branch, catching squash-merged branches whose remote tracking ref is gone.
Stale marked branches are dropped from `:marked'. Returns new candidates."
  (let* ((path (magit-dash-gh--repo-dir))
         (default-directory path)
         (state (magit-dash-gh--cache-get path :prune-state))
         (prev-marked (plist-get state :marked))
         (protected (list (magit-dash-gh--default-branch)
                          (magit-get-current-branch)))
         (closed-prs (magit-dash-gh--fetch-closed-prs (plist-get state :closed-prs)))
         (candidates (thread-last (magit-list-local-branch-names)
                       (seq-remove (lambda (branch) (member branch protected)))
                       (seq-filter (lambda (branch) (map-elt closed-prs branch)))
                       (seq-map (lambda (branch) (cons branch (map-elt closed-prs branch)))))))
    (magit-dash-gh--cache-set path
     :prune-state (list :candidates candidates
                        :marked (seq-filter (lambda (m) (member m (seq-map #'car candidates)))
                                            prev-marked)
                        :closed-prs closed-prs))
    candidates))

(defun magit-dash-gh--prune-format-annotation (pr)
  "Return a one-line annotation string describing PR alist."
  (format "PR #%s %s: %s"
	  (map-elt pr 'number)
	  (map-elt pr 'state)
	  (map-elt pr 'title)))

(defun magit-dash-gh--prune-build-menu (candidates marked)
  "Build a hash-table menu for the prune command.
CANDIDATES is the alist of (BRANCH . PR-ALIST). MARKED is the list of
branch names currently marked for batch pruning. Branch entries are
labeled `prune: BRANCH' with a ` [marked]' suffix when applicable, and
carry BRANCH itself as their `annotated-completing-read' target, so
selecting one returns the branch name directly."
  (map-into
   (append
    (list (cons "exit menu" "leave the menu without further action")
          (cons "refresh" "re-scan PRs and rebuild the cache (returns to menu)"))
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
    (seq-map (lambda (entry)
               (let* ((branch (car entry))
                      (suffix (if (member branch marked) " [marked]" "")))
                 (cons (format "prune: %s%s" branch suffix)
                       (cons (magit-dash-gh--prune-format-annotation (cdr entry)) branch))))
             candidates))
   '(hash-table :test equal)))

(defun magit-dash-gh--prune-delete-branches (branches path &optional prompt-p)
  "Delete each branch in BRANCHES for repo at PATH, prompting only when PROMPT-P.
After deletion, re-scan the cache for PATH.
Returns a plist (:deleted N :skipped M :quit BOOL).
With PROMPT-P, the user is prompted y/n/q/! per branch; `!' enables
yes-to-all for the remainder, `q' terminates the loop."
  (let ((result
         (seq-reduce
          (lambda (state branch)
            (if (plist-get state :quit)
                state
              (let ((answer (if (plist-get state :yes-to-all)
                                ?y
                              (read-char-choice
                               (format "Delete %s? (y/n/q/!) " branch)
                               '(?y ?n ?q ?!)))))
                (pcase answer
                  (?y (magit-dash-gh--with-repo-dir path
                        (with-no-warnings (magit-branch-delete (list branch) t)))
                      (plist-put state :deleted (1+ (plist-get state :deleted))))
                  (?n (plist-put state :skipped (1+ (plist-get state :skipped))))
                  (?q (plist-put state :quit t))
                  (?! (magit-dash-gh--with-repo-dir path
                        (with-no-warnings (magit-branch-delete (list branch) t)))
                      (plist-put (plist-put state :yes-to-all t)
                                 :deleted (1+ (plist-get state :deleted))))))))
          branches
          (list :deleted 0 :skipped 0 :yes-to-all (not prompt-p) :quit nil))))
    (magit-dash-gh--prune-scan)
    (message "magit-gh prune: deleted %d, skipped %d%s"
             (plist-get result :deleted)
             (plist-get result :skipped)
             (if (plist-get result :quit) " (quit)" ""))
    (list :deleted (plist-get result :deleted)
          :skipped (plist-get result :skipped)
          :quit (plist-get result :quit))))

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
`magit-dash-gh--prune-build-menu', or a branch name resolved by
`annotated-completing-read' via that menu's per-branch targets.
Throws `magit-dash-gh--prune-exit' to terminate the menu loop."
  (let* ((state (magit-dash-gh--cache-get path :prune-state))
	 (candidates (plist-get state :candidates))
	 (marked (plist-get state :marked)))
    (cond
     ((equal label "exit menu")
      (throw 'magit-dash-gh--prune-exit nil))
     ((equal label "refresh")
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
     (t
      (user-error "Unknown menu label: %s" label)))))

;;;###autoload
(defun magit-dash-gh-prune-merged-branches ()
  "Open a menu to prune local branches whose PRs are merged or closed.

Per-repo cache (candidates + marked list) lives on the unified global cache
keyed by repository toplevel. The menu loops until you select `exit
menu'. The menu offers:

  - exit menu
  - refresh
  - prune all branches (no prompt)
  - prune all branches (with prompt)
  - mark branch for pruning
  - prune marked branches (when any are marked)
  - prune: BRANCH [marked]   (one entry per candidate)"
  (interactive)
  (magit-dash-gh--check-gh)
  (let ((path (magit-dash-gh--repo-dir)))
    (unless (magit-dash-gh--cache-get path :prune-state)
      (message "magit-gh prune: scanning for branches with closed PRs...")
      (magit-dash-gh--prune-scan))
    (catch 'magit-dash-gh--prune-exit
      (while t
	(let* ((state (magit-dash-gh--cache-get path :prune-state))
	       (table (magit-dash-gh--prune-build-menu
		       (plist-get state :candidates)
		       (plist-get state :marked)))
	       (label (annotated-completing-read
		       table
		       :prompt "magit-gh prune => "
		       :category 'magit-dash-gh-prune
		       :require-match t)))
	  (magit-dash-gh--prune-dispatch label path))))))

;;;###autoload
(defun magit-dash-gh-prune-prefetch ()
  "Seed the in-memory closed-PR cache from disk for the current repo.
Makes no GitHub API calls; safe to call on status load.
Skips silently when caching is disabled, not in a git repo, or the
in-memory cache is already populated."
  (when (and magit-dash-gh-prune-cache-dir
	     (ignore-errors (magit-toplevel)))
    (let* ((path (magit-dash-gh--repo-dir))
           (state (magit-dash-gh--cache-get path :prune-state)))
      (unless (plist-get state :closed-prs)
        (let ((table (magit-dash-gh--prune-load-cache)))
          (magit-dash-gh--cache-set path :prune-state
                               (plist-put state :closed-prs table)))))))

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
      (puthash (pop kvs) (pop kvs) ht))
    ht))

(defun magit-dash-gh--file-table (path type)
  "Return a string-keyed hash-table representing a collected file entry."
  (let ((ht (make-hash-table :test #'equal)))
    (puthash "path" path ht)
    (puthash "type" type ht)
    ht))

(defun magit-dash-gh--write-index (dir data)
  "Write DATA hash-table as an index file inside DIR.
Format is controlled by `magit-dash-gh-collect-index-format'."
  (pcase magit-dash-gh-collect-index-format
    ('yaml
     (unless (require 'yaml nil t)
       (user-error "magit-gh: yaml format requires the `yaml' package"))
     (with-temp-file (expand-file-name "index.yaml" dir)
       (insert (yaml-encode data))))
    (_
     (with-temp-file (expand-file-name "index.json" dir)
       (insert (json-serialize data :false-object :false :null-object nil))))))

(defun magit-dash-gh--run-process (args dir on-success &optional on-error)
  "Run `gh' with ARGS in DIR asynchronously.
ON-SUCCESS is called with the output string when the process exits 0.
ON-ERROR is called with the output string and exit code otherwise;
when nil, a `message' is emitted instead.
When DIR no longer exists on disk (for example a worktree removed outside
magit-dash), ON-ERROR is called directly instead of letting `make-process'
signal."
  (if (not (file-directory-p dir))
      (let ((msg (format "no such directory: %s" dir)))
        (if on-error
            (funcall on-error msg 1)
          (message "magit-gh: gh %s failed: %s" (car args) msg)))
    (let* ((default-directory dir)
           (buf (generate-new-buffer " *magit-dash-gh-proc*"))
           (proc (make-process
                  :name "magit-gh"
                  :buffer buf
                  :command (cons "gh" args)
                  :connection-type 'pipe
                  :noquery t
                  :sentinel
                  (lambda (proc _event)
                    (when (memq (process-status proc) '(exit signal))
                      (let ((output (with-current-buffer (process-buffer proc)
                                      (buffer-string)))
                            (code (process-exit-status proc)))
                        (kill-buffer (process-buffer proc))
                        (if (= code 0)
                            (funcall on-success output)
                          (if on-error
                              (funcall on-error output code)
                            (message "magit-gh: gh %s exited %d: %s"
                                     (car args) code
                                     (string-trim output))))))))))
      (with-current-buffer buf
        (setq default-directory dir))
      proc)))

(defun magit-dash-gh--repo-info ()
  "Return a plist :owner :repo :branch for the current repository.
Uses `gh repo view' and `magit-get-current-branch'."
  (let* ((default-directory (magit-dash-gh--repo-dir))
         (output (string-trim
                  (shell-command-to-string
                   "gh repo view --json owner,name --jq '[.owner.login,.name]|@tsv'")))
         (parts (split-string output "\t")))
    (list :owner  (nth 0 parts)
          :repo   (nth 1 parts)
          :branch (magit-get-current-branch))))

;;; Account switching

(defun magit-dash-gh--auth-accounts ()
  "Return a list of (:host HOST :user USER :active ACTIVE-P) plists.
Parsed from `gh auth status', across all configured hosts."
  (magit-dash-gh--check-gh)
  (with-temp-buffer
    (insert (shell-command-to-string "gh auth status 2>&1"))
    (goto-char (point-min))
    (let (accounts)
      (while (re-search-forward
              "Logged in to \\([^[:space:]]+\\) account \\([^[:space:]]+\\).*\n[[:space:]]*- Active account: \\(true\\|false\\)"
              nil t)
        (push (list :host (match-string 1)
                    :user (match-string 2)
                    :active (string= (match-string 3) "true"))
              accounts))
      (nreverse accounts))))

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
   ("p" "PR dashboard"                 magit-gh-pr-dash)
   ("o" "Open / switch repository"     magit-dash-open-repo)]
  ["Actions"
   ("P" "Prune merged branches"        magit-dash-gh-prune-merged-branches)
   ("l" "Fetch CI logs"                magit-dash-gh-actions-fetch)
   ("c" "Fetch PR comments"            magit-dash-gh-pr-fetch)
   ("w" "Trigger workflow run"         magit-dash-gh-workflow-run)
   ("u" "Switch users"                 magit-dash-gh-auth-switch)
   ("r" "Refresh status data"          magit-dash-status-refresh-all)]
  ["Quit"
   ("q" "Quit"                         transient-quit-one)])
(provide 'magit-dash-gh)

;;; magit-dash-gh.el ends here
