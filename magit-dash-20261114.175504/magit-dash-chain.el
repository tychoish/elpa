;;; magit-dash-chain.el --- Merge chains of dependent PR branches via hitl-plan -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (hitl "0.1"))
;; Keywords: vc, tools, magit, github
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; Discover a chain (or tree) of dependent pull requests -- a PR whose
;; base branch is another PR's head branch -- validate it, render it, and
;; propose a sequence of merges as a `hitl-plan' in the `magit-dash-chain'
;; domain.
;;
;; Each node, in topological order, gets an `update' step: the parent
;; branch (or the base branch for the root) is merged INTO the node's
;; branch in the node's worktree with `git merge --no-ff'.  History is
;; never rewritten: this module never invokes rebase, pull or a forced
;; push (`magit-dash-chain--git' refuses those arguments outright).
;;
;; Nothing is pushed by default.  With the `land' option the plan also
;; contains separate `land' steps (push the branch, merge the PR with
;; `gh', fetch the base).  They are ordinary plan steps, so hitl-plan's
;; approval gating covers them, and they additionally refuse to run for
;; an unapproved MCP-origin plan.
;;
;; A conflicting merge halts the plan with the worktree left in its
;; merge state and the conflicted files recorded in the plan.  The plan
;; is resumed with `magit-dash-chain-resume': a step interrupted by a
;; conflict is retried, notices the in-progress merge, and -- once the
;; conflicts are resolved -- finishes the merge commit and continues.
;; `magit-dash-chain-conflict-strategy' can instead hand the conflict to
;; an agent (agent-shell or gptel); the agent paths are soft
;; dependencies and fall back to halting.
;;
;; The plan's `.eld' file (and a sibling `.org' record) is rewritten as
;; each step runs, so it is a live record that survives an Emacs
;; restart: load the plan and call `magit-dash-chain-resume'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'hitl-plan)
(require 'magit-dash-gh)
(require 'magit-dash-gh-pr-status)
(require 'magit-dash-worktree)

(declare-function agent-shell-insert "agent-shell")
(declare-function gptel-request "gptel")

(defgroup magit-dash-chain nil
  "Merge chains of dependent pull request branches."
  :group 'magit-dash
  :prefix "magit-dash-chain-")

(defcustom magit-dash-chain-conflict-strategy 'halt
  "What to do when a chain merge conflicts.
`halt' leaves the worktree in its merge state and stops the plan.
`agent-shell' and `gptel' first hand the conflict to that agent (when it
is available; otherwise they behave like `halt').  In every case the
step stays blocked until `magit-dash-chain-resume' verifies that the
merge is fully resolved."
  :type '(choice (const :tag "Halt for manual resolution" halt)
                 (const :tag "Hand off to agent-shell" agent-shell)
                 (const :tag "Hand off to gptel" gptel))
  :group 'magit-dash-chain)

(defcustom magit-dash-chain-plan-directory nil
  "Directory holding chain plans.\nWhen nil, a subdirectory of `hitl-plan-directory'."
  :type '(choice (const nil) directory)
  :group 'magit-dash-chain)

(defcustom magit-dash-chain-worktree-directory ".wt"
  "Directory in which missing chain worktrees are created.
A relative name (the default, \".wt\") is resolved against the main
worktree directory of the repository (`magit-dash-worktree-main-dir'),
so worktrees land in <repo>/.wt/.  An absolute directory is used as is.
When nil, a sibling of the repository named REPO.chain-worktrees."
  :type '(choice (const :tag "Sibling REPO.chain-worktrees" nil) directory)
  :group 'magit-dash-chain)

(defcustom magit-dash-chain-verify-command nil
  "Shell command run in a worktree after its merge, or nil for none.
A non-zero exit status fails (halts) the step."
  :type '(choice (const nil) string)
  :group 'magit-dash-chain)

(defvar magit-dash-chain-land-function #'magit-dash-chain--land-default
  "Function called with a `land' target plist to push and merge its PR.")

(defvar magit-dash-chain--allow-push nil
  "Non-nil only while a `land' step may run `git push'.")

;;;; Git helpers

(defconst magit-dash-chain--forbidden-git-args
  '("-f" "--force" "--force-with-lease" "--force-if-includes")
  "Git arguments that are never passed.")

(defconst magit-dash-chain--forbidden-git-verb-re "\\`\\(rebase\\|pull\\)\\'"
  "Git subcommands that are never run.")

(defun magit-dash-chain--git (dir &rest args)
  "Run git with ARGS in DIR and return (EXIT-CODE . OUTPUT).
This is the only place git is invoked.  It refuses history rewriting
\(subcommands and flags that rewrite or fetch-and-integrate), and refuses
`push' outside of a `land' step."
  (dolist (a args)
    (when (or (member a magit-dash-chain--forbidden-git-args)
              (string-match-p magit-dash-chain--forbidden-git-verb-re a)
              (string-match-p "\\`--\\(rebase\\|autostash\\)" a)
              (and (equal a "push") (not magit-dash-chain--allow-push)))
      (error "Refusing to run git with forbidden argument: %s" a)))
  (let ((default-directory (file-name-as-directory (expand-file-name dir)))
        (process-environment (append '("GIT_EDITOR=true" "GIT_TERMINAL_PROMPT=0"
                                       "LC_ALL=C")
                                     process-environment)))
    (with-temp-buffer
      (let ((code (apply #'process-file "git" nil t nil args)))
        (cons code (string-trim-right (buffer-string)))))))

(defun magit-dash-chain--git-ok (dir &rest args)
  "Run git ARGS in DIR; return its output, or signal an error on failure."
  (let ((res (apply #'magit-dash-chain--git dir args)))
    (unless (zerop (car res))
      (error "git %s failed in %s: %s" (string-join args " ") dir (cdr res)))
    (cdr res)))

(defun magit-dash-chain--git-p (dir &rest args)
  "Return non-nil when git ARGS succeed in DIR."
  (zerop (car (apply #'magit-dash-chain--git dir args))))

(defun magit-dash-chain--now ()
  "Return the current time as an ISO-8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun magit-dash-chain--local-ref (dir branch)
  "Return the full ref of local BRANCH in DIR, or nil."
  (let ((ref (concat "refs/heads/" branch)))
    (and (magit-dash-chain--git-p dir "rev-parse" "--verify" "-q" (concat ref "^{commit}"))
         ref)))

(defun magit-dash-chain--remote-ref (dir branch)
  "Return the full ref of BRANCH on origin in DIR, or nil."
  (let ((ref (concat "refs/remotes/origin/" branch)))
    (and (magit-dash-chain--git-p dir "rev-parse" "--verify" "-q" (concat ref "^{commit}"))
         ref)))

(defun magit-dash-chain--default-base (dir)
  "Return the default base branch name of the repository in DIR."
  (or (let ((r (magit-dash-chain--git dir "symbolic-ref" "--short" "-q"
                                      "refs/remotes/origin/HEAD")))
        (and (zerop (car r)) (string-remove-prefix "origin/" (cdr r))))
      (seq-find (lambda (b) (magit-dash-chain--local-ref dir b)) '("main" "master"))
      "main"))

(defun magit-dash-chain--current-branch (dir)
  "Return the branch checked out in DIR, or nil when detached."
  (let ((r (magit-dash-chain--git dir "symbolic-ref" "--short" "-q" "HEAD")))
    (and (zerop (car r)) (cdr r))))

(defun magit-dash-chain--merge-in-progress-p (dir)
  "Return non-nil when a merge is in progress in the worktree DIR."
  (magit-dash-chain--git-p dir "rev-parse" "-q" "--verify" "MERGE_HEAD"))

(defun magit-dash-chain--unmerged-files (dir)
  "Return the list of unmerged paths in DIR."
  (split-string (cdr (magit-dash-chain--git dir "diff" "--name-only" "--diff-filter=U"))
                "\n" t))

(defun magit-dash-chain--conflict-marker-files (dir)
  "Return tracked files in DIR containing conflict markers."
  (let ((r (magit-dash-chain--git dir "grep" "-l" "-I" "-E" "^(<{7}|>{7})( |$)")))
    (and (zerop (car r)) (split-string (cdr r) "\n" t))))

(defun magit-dash-chain--dirty-p (dir)
  "Return non-nil when DIR has uncommitted changes to tracked files."
  (let ((r (magit-dash-chain--git dir "status" "--porcelain" "-uno")))
    (or (not (zerop (car r))) (not (string-empty-p (cdr r))))))

(defun magit-dash-chain--head (dir)
  "Return the full SHA of HEAD in DIR."
  (magit-dash-chain--git-ok dir "rev-parse" "HEAD"))

(defun magit-dash-chain--normalize-dir (dir)
  "Return DIR as a canonical directory name without trailing slash."
  (directory-file-name (file-truename (expand-file-name dir))))

(defalias 'magit-dash-chain--worktree-for-branch #'magit-dash-worktree-for-branch
  "Return the worktree of REPO-DIR on BRANCH, or nil.")

(defun magit-dash-chain--worktree-path (repo-dir branch)
  "Return the path where a worktree for BRANCH of REPO-DIR would be created."
  (let* ((main (directory-file-name (expand-file-name (magit-dash-worktree-main-dir repo-dir))))
         (root (if magit-dash-chain-worktree-directory
                   (expand-file-name magit-dash-chain-worktree-directory main)
                 (concat main ".chain-worktrees"))))
    (expand-file-name (replace-regexp-in-string "[^A-Za-z0-9._-]" "-" branch) root)))

;;;; Discovery

(defun magit-dash-chain--fetch-prs (slug dir)
  "Return PR plists for repository SLUG (run from DIR), or nil without a SLUG.
This is the seam tests stub; it performs the `gh' query."
  (and slug (magit-dash-gh-pr-status-collect slug dir)))

(defun magit-dash-chain--parse-root (root)
  "Return (number . N) or (branch . NAME) for the ROOT argument."
  (cond ((integerp root) (cons 'number root))
        ((and (stringp root) (string-match "\\`#?\\([0-9]+\\)\\'" root))
         (cons 'number (string-to-number (match-string 1 root))))
        ((and (stringp root) (not (string-empty-p (string-trim root))))
         (cons 'branch (string-trim root)))
        (t (user-error "A root PR number or branch is required"))))

(defun magit-dash-chain-discover (repo-dir root &optional base prs)
  "Discover the PR chain/tree under ROOT in REPO-DIR.
ROOT is a PR number or a branch name.  BASE is the branch the chain
lands on (default: the root PR's base, else the repository default).
PRS is the list of open PR plists (see `magit-dash-gh-pr-status-collect');
edges are PR `:base' -> `:branch'.  Signal `user-error' on a cycle.
Return a plist (:repo-dir :base :root :nodes :problems); :nodes are in
topological (parents first, depth-first) order."
  (let* ((spec (magit-dash-chain--parse-root root))
         (root-pr (pcase spec
                    (`(number . ,n)
                     (or (seq-find (lambda (p) (eql (plist-get p :number) n)) prs)
                         (user-error "PR #%d not found among the open PRs" n)))
                    (`(branch . ,b)
                     (seq-find (lambda (p) (equal (plist-get p :branch) b)) prs))))
         (root-branch (if root-pr (plist-get root-pr :branch) (cdr spec)))
         (base (or base (and root-pr (plist-get root-pr :base))
                   (magit-dash-chain--default-base repo-dir)))
         (nodes nil)
         (problems nil))
    (cl-labels
        ((walk (pr branch parent path)
           (when (member branch path)
             (user-error "Cycle detected in PR chain: %s"
                         (string-join (reverse (cons branch path)) " -> ")))
           (push (list :branch branch :parent parent
                       :number (plist-get pr :number) :url (plist-get pr :url)
                       :title (plist-get pr :title) :state (plist-get pr :state)
                       :draft (plist-get pr :draft) :pr-base (plist-get pr :base))
                 nodes)
           (let* ((kids (sort (seq-filter (lambda (p) (equal (plist-get p :base) branch))
                                          prs)
                              (lambda (a b) (< (or (plist-get a :number) 0)
                                               (or (plist-get b :number) 0)))))
                  (seen nil))
             (dolist (kid kids)
               (let ((kb (plist-get kid :branch)))
                 (if (member kb seen)
                     (push (format "Several open PRs share head branch %s" kb) problems)
                   (push kb seen)
                   (walk kid kb branch (cons branch path))))))))
      (walk root-pr root-branch nil nil))
    (list :repo-dir (expand-file-name repo-dir) :base base :root root-branch
          :nodes (nreverse nodes) :problems (nreverse problems))))

(defalias 'magit-dash-chain-fetch-prs #'magit-dash-chain--fetch-prs
  "Return PR plists for repository SLUG (run from DIR); the public PR-data seam.
Callers outside this module (the stack visualizer) use this name.")

;;;; Validation

(defun magit-dash-chain-validate (chain)
  "Validate CHAIN; return it with :nodes annotated and :errors/:warnings set.
Each node gains :worktree (the existing or planned path) and
:worktree-exists.  Nothing in the repository is modified."
  (let* ((dir (plist-get chain :repo-dir))
         (base (plist-get chain :base))
         (errors (copy-sequence (plist-get chain :problems)))
         (warnings nil)
         (nodes (plist-get chain :nodes)))
    (cl-flet ((err (fmt &rest a) (push (apply #'format fmt a) errors))
              (warn* (fmt &rest a) (push (apply #'format fmt a) warnings)))
      (unless (or (magit-dash-chain--local-ref dir base)
                  (magit-dash-chain--remote-ref dir base))
        (err "Base branch %s does not resolve locally or on origin" base))
      (when (and (null (plist-get (car nodes) :number)) (null (cdr nodes)))
        (err "Nothing to merge: %s has no PR and no dependent PRs" (plist-get chain :root)))
      (setq nodes
            (mapcar
             (lambda (node)
               (let* ((b (plist-get node :branch))
                      (n (plist-get node :number))
                      (state (plist-get node :state))
                      (wt (magit-dash-chain--worktree-for-branch dir b)))
                 (when (equal b base)
                   (err "Branch %s is both a chain node and the base" b))
                 (unless (or (magit-dash-chain--local-ref dir b)
                             (magit-dash-chain--remote-ref dir b))
                   (err "Branch %s does not exist locally or on origin" b))
                 (when (and n (not (equal (and state (upcase state)) "OPEN")))
                   (err "PR #%s is %s, not OPEN" n (or state "in an unknown state")))
                 (when (plist-get node :draft)
                   (warn* "PR #%s is a draft" n))
                 (when wt
                   (cond ((magit-dash-chain--merge-in-progress-p wt)
                          (err "Worktree %s has a merge in progress" wt))
                         ((magit-dash-chain--dirty-p wt)
                          (err "Worktree %s (%s) has uncommitted changes" wt b))))
                 (append (list :worktree (or wt (magit-dash-chain--worktree-path dir b))
                               :worktree-exists (and wt t))
                         node)))
             nodes)))
    (append (list :errors (nreverse errors) :warnings (nreverse warnings)
                  :nodes nodes)
            (cl-loop for (k v) on chain by #'cddr
                     unless (memq k '(:nodes :errors :warnings))
                     append (list k v)))))

;;;; Rendering

(defun magit-dash-chain--node-label (node)
  "Return a one-line description of NODE."
  (format "%s%s%s"
          (if (plist-get node :number) (format "#%s " (plist-get node :number)) "")
          (plist-get node :branch)
          (if (plist-get node :state) (format " [%s]" (plist-get node :state)) "")))

(defun magit-dash-chain--render-children (chain parent prefix)
  "Return ASCII lines for the children of PARENT in CHAIN under PREFIX."
  (let* ((kids (seq-filter (lambda (n) (equal (plist-get n :parent) parent))
                           (plist-get chain :nodes)))
         lines)
    (cl-loop for (kid . rest) on kids
             for last = (null rest)
             do (push (concat prefix (if last "`-- " "|-- ")
                              (magit-dash-chain--node-label kid))
                      lines)
             (setq lines (append (reverse (magit-dash-chain--render-children
                                           chain (plist-get kid :branch)
                                           (concat prefix (if last "    " "|   "))))
                                 lines)))
    (nreverse lines)))

(defun magit-dash-chain-render-ascii (chain)
  "Return CHAIN as an ASCII tree rooted at the base branch."
  (let* ((root (car (plist-get chain :nodes)))
         (lines (cons (format "%s (base)" (plist-get chain :base))
                      (cons (concat "`-- " (magit-dash-chain--node-label root))
                            (magit-dash-chain--render-children
                             chain (plist-get root :branch) "    ")))))
    (concat (string-join lines "\n") "\n")))

(defun magit-dash-chain--step-label (target)
  "Return a short label for plan step TARGET."
  (pcase (plist-get target :kind)
    ('update (format "update %s <- %s" (plist-get target :branch) (plist-get target :source)))
    ('land (format "land %s%s -> %s"
                   (if (plist-get target :number) (format "#%s " (plist-get target :number)) "")
                   (plist-get target :branch) (plist-get target :base)))
    (_ (format "%S" target))))

(defun magit-dash-chain-render-org (chain &optional plan)
  "Return CHAIN as an org document; include PLAN's step table when given."
  (concat
   (format "* PR chain: %s onto %s\n" (plist-get chain :root) (plist-get chain :base))
   "#+begin_example\n" (magit-dash-chain-render-ascii chain) "#+end_example\n"
   (when (plist-get chain :errors)
     (concat "\n** Errors\n"
             (mapconcat (lambda (e) (format "- %s\n" e)) (plist-get chain :errors) "")))
   (when (plist-get chain :warnings)
     (concat "\n** Warnings\n"
             (mapconcat (lambda (e) (format "- %s\n" e)) (plist-get chain :warnings) "")))
   (when plan
     (concat "\n** Steps\n"
             (format "Plan =%s=, status =%s=, updated %s\n\n"
                     (hitl-plan-id plan) (hitl-plan-status plan) (magit-dash-chain--now))
             "| id | step | status | merge commit | conflicts | finished |\n|-\n"
             (mapconcat
              (lambda (step)
                (let ((res (magit-dash-chain--result plan (hitl-plan-step-id step))))
                  (format "| %s | %s | %s | %s | %s | %s |\n"
                          (hitl-plan-step-id step)
                          (magit-dash-chain--step-label (hitl-plan-step-target step))
                          (hitl-plan-step-status step)
                          (let ((sha (plist-get res :sha))) (if sha (substring sha 0 (min 10 (length sha))) ""))
                          (string-join (plist-get res :conflicts) ", ")
                          (or (plist-get res :finished) ""))))
              (hitl-plan-steps plan) "")))))

;;;; Plan results (the live record in plan metadata)

(defun magit-dash-chain--meta (plan)
  "Return PLAN's chain metadata plist."
  (plist-get (hitl-plan-metadata plan) :chain))

(defun magit-dash-chain--result (plan id)
  "Return the recorded result plist of step ID in PLAN."
  (cdr (assq id (plist-get (magit-dash-chain--meta plan) :results))))

(defun magit-dash-chain--set-result (plan id &rest props)
  "Merge PROPS into the recorded result of step ID in PLAN and save PLAN."
  (let* ((meta (magit-dash-chain--meta plan))
         (results (plist-get meta :results))
         (new (copy-sequence (cdr (assq id results)))))
    (while props (setq new (plist-put new (pop props) (pop props))))
    (setq results (cons (cons id new) (assq-delete-all id results)))
    (setf (hitl-plan-metadata plan)
          (plist-put (hitl-plan-metadata plan) :chain (plist-put meta :results results)))
    (hitl-plan-save plan)
    (magit-dash-chain--write-record plan)))

(defun magit-dash-chain-record-file (plan)
  "Return the path of PLAN's org record file."
  (concat (file-name-sans-extension (hitl-plan-file (hitl-plan-id plan)
                                                    (hitl-plan-directory plan)))
          ".org"))

(defun magit-dash-chain--write-record (plan)
  "Rewrite PLAN's org record file from its stored chain and results."
  (let ((chain (plist-get (magit-dash-chain--meta plan) :chain-data)))
    (when chain
      (let ((file (magit-dash-chain-record-file plan)))
        (make-directory (file-name-directory file) t)
        (write-region (magit-dash-chain-render-org chain plan) nil file nil 'silent)))))

;;;; Conflict hand-off

(defun magit-dash-chain--conflict-prompt (target files source)
  "Return the agent prompt for conflicts in FILES merging SOURCE per TARGET."
  (format "A git merge is in progress and has conflicts.
Worktree: %s
Branch being updated: %s
Merging in: %s
Conflicted files:
%s

Resolve every conflict in that worktree, preserving the intent of both sides,
and `git add' the resolved files.  Then complete the merge as a merge commit
with `git commit --no-edit'.  Do NOT rebase, do NOT squash, do NOT abort the
merge, do NOT push, and do NOT force anything.  Leave no conflict markers.
Stop once the merge commit exists."
          (plist-get target :worktree) (plist-get target :branch) source
          (mapconcat (lambda (f) (concat "- " f)) files "\n")))

(defun magit-dash-chain--dispatch-agent (strategy dir prompt on-done)
  "Hand PROMPT to the agent named by STRATEGY, working in DIR.
Return non-nil when the prompt was dispatched.  ON-DONE is called with no
arguments once the agent reports it finished, when that is observable.
Return nil (without signalling) if the agent is unavailable."
  (condition-case nil
      (pcase strategy
        ('agent-shell
         (when (and (require 'agent-shell nil t) (fboundp 'agent-shell-insert))
           (let ((default-directory (file-name-as-directory dir)))
             (agent-shell-insert :text prompt :submit t)
             t)))
        ('gptel
         (when (and (require 'gptel nil t) (fboundp 'gptel-request))
           (let ((default-directory (file-name-as-directory dir)))
             (gptel-request prompt
               :callback (lambda (resp _info)
                           (when (stringp resp) (funcall on-done))))
             t))))
    (error nil)))

(defun magit-dash-chain--handle-conflict (plan step target files source)
  "Apply the conflict strategy for a fresh conflict; return the strategy used."
  (let ((strategy magit-dash-chain-conflict-strategy)
        (dir (plist-get target :worktree)))
    (if (and (memq strategy '(agent-shell gptel))
             (magit-dash-chain--dispatch-agent
              strategy dir (magit-dash-chain--conflict-prompt target files source)
              (lambda ()
                (condition-case err (magit-dash-chain-resume plan)
                  (error (message "magit-dash-chain: post-agent resume failed: %s"
                                  (error-message-string err)))))))
        (progn (magit-dash-chain--set-result plan (hitl-plan-step-id step)
                                             :handoff (symbol-name strategy))
               strategy)
      'halt)))

;;;; Step application

(defun magit-dash-chain--ensure-worktree (target)
  "Return the worktree path for TARGET's branch, creating it if needed."
  (let* ((repo (plist-get target :repo-dir))
         (branch (plist-get target :branch))
         (existing (magit-dash-chain--worktree-for-branch repo branch)))
    (or existing
        (let ((path (plist-get target :worktree)))
          (when (file-exists-p path)
            (error "Cannot create worktree for %s: %s exists" branch path))
          (make-directory (file-name-directory path) t)
          (if (magit-dash-chain--local-ref repo branch)
              (magit-dash-chain--git-ok repo "worktree" "add" path branch)
            (magit-dash-chain--git-ok repo "worktree" "add" "-b" branch path
                                      (concat "origin/" branch)))
          path))))

(defun magit-dash-chain--resolve-source (target)
  "Return (REF . LABEL) for the commit TARGET merges in.
A base source prefers origin's copy (fetching first when :fetch); a
parent-branch source prefers the local branch, which carries the parent's
own updates."
  (let* ((repo (plist-get target :repo-dir))
         (name (plist-get target :source))
         (local (magit-dash-chain--local-ref repo name))
         (remote (progn
                   (when (plist-get target :fetch)
                     (magit-dash-chain--git-ok repo "fetch" "origin" name))
                   (magit-dash-chain--remote-ref repo name)))
         (ref (if (eq (plist-get target :source-kind) 'base)
                  (or remote local)
                (or local remote))))
    (unless ref (error "Source branch %s does not resolve" name))
    (cons ref (if (equal ref remote) (concat "origin/" name) name))))

(defun magit-dash-chain--finish-merge (dir src-sha)
  "Complete the in-progress merge of SRC-SHA in DIR if it is fully resolved.
Return a result plist: :outcome `resumed' with :sha, or `conflict' with
:conflicts."
  (let ((mh (magit-dash-chain--git-ok dir "rev-parse" "MERGE_HEAD")))
    (unless (equal mh src-sha)
      (error "Worktree %s has a different merge in progress (MERGE_HEAD %s)" dir mh))
    (if-let* ((files (magit-dash-chain--unmerged-files dir)))
        (list :outcome 'conflict :conflicts files)
      (if-let* ((marked (magit-dash-chain--conflict-marker-files dir)))
          (list :outcome 'conflict :conflicts marked :markers t)
        (magit-dash-chain--git-ok dir "commit" "--no-edit")
        (list :outcome 'resumed :sha (magit-dash-chain--head dir))))))

(defun magit-dash-chain--update-worktree (dir branch source-ref source-label)
  "Merge SOURCE-REF (shown as SOURCE-LABEL) into BRANCH checked out in DIR.
Return a result plist with :outcome one of `merged', `up-to-date',
`resumed' or `conflict' (with :conflicts, and :fresh when this call
started the conflicting merge).  Resumes an in-progress merge of the same
source instead of starting a new one."
  (let ((cur (magit-dash-chain--current-branch dir)))
    (unless (equal cur branch)
      (error "Worktree %s is on %s, expected %s" dir (or cur "a detached HEAD") branch)))
  (let ((src-sha (magit-dash-chain--git-ok dir "rev-parse" "--verify"
                                           (concat source-ref "^{commit}"))))
    (cond
     ((magit-dash-chain--merge-in-progress-p dir)
      (magit-dash-chain--finish-merge dir src-sha))
     ((magit-dash-chain--dirty-p dir)
      (error "Worktree %s has uncommitted changes" dir))
     ((magit-dash-chain--git-p dir "merge-base" "--is-ancestor" src-sha "HEAD")
      (list :outcome 'up-to-date :sha (magit-dash-chain--head dir)))
     (t
      (let ((r (magit-dash-chain--git dir "merge" "--no-ff" "-m"
                                      (format "Merge %s into %s" source-label branch)
                                      src-sha)))
        (cond
         ((zerop (car r))
          (list :outcome 'merged :sha (magit-dash-chain--head dir)))
         ((magit-dash-chain--merge-in-progress-p dir)
          (list :outcome 'conflict :fresh t
                :conflicts (magit-dash-chain--unmerged-files dir)))
         (t (error "git merge failed in %s: %s" dir (cdr r)))))))))

(defun magit-dash-chain--verify (dir branch src-sha)
  "Verify that DIR on BRANCH is a clean, fully merged state containing SRC-SHA."
  (cond
   ((not (equal (magit-dash-chain--current-branch dir) branch))
    (error "Verification failed: %s is not on %s" dir branch))
   ((magit-dash-chain--merge-in-progress-p dir)
    (error "Verification failed: merge still in progress in %s" dir))
   ((magit-dash-chain--unmerged-files dir)
    (error "Verification failed: unmerged paths remain in %s" dir))
   ((magit-dash-chain--conflict-marker-files dir)
    (error "Verification failed: conflict markers remain in %s" dir))
   ((not (magit-dash-chain--git-p dir "merge-base" "--is-ancestor" src-sha "HEAD"))
    (error "Verification failed: %s does not contain %s" branch src-sha)))
  (when magit-dash-chain-verify-command
    (let ((default-directory (file-name-as-directory dir)))
      (unless (zerop (call-process-shell-command magit-dash-chain-verify-command nil nil))
        (error "Verification command failed in %s" dir)))))

(defun magit-dash-chain--apply-update (plan step target)
  "Run the `update' STEP of PLAN for TARGET."
  (let* ((id (hitl-plan-step-id step))
         (branch (plist-get target :branch))
         (prior (magit-dash-chain--result plan id)))
    (magit-dash-chain--set-result
     plan id :status 'running :branch branch
     :started (or (plist-get prior :started) (magit-dash-chain--now))
     :attempts (1+ (or (plist-get prior :attempts) 0)) :error nil)
    (condition-case err
        (let* ((wt (magit-dash-chain--ensure-worktree target))
               (_before (let ((old (plist-get (magit-dash-chain--result plan id) :before))
                              (head (magit-dash-chain--head wt)))
                          ;; Keep the original tip while resuming a merge;
                          ;; otherwise re-record unless it is still an ancestor.
                          (unless (or (magit-dash-chain--merge-in-progress-p wt)
                                      (equal old head)
                                      (and old (magit-dash-chain--git-p
                                                wt "merge-base" "--is-ancestor" old head)))
                            (magit-dash-chain--set-result plan id :before head))))
               (source (magit-dash-chain--resolve-source target))
               (src-sha (magit-dash-chain--git-ok wt "rev-parse" "--verify"
                                                  (concat (car source) "^{commit}")))
               (res (magit-dash-chain--update-worktree wt branch (car source) (cdr source))))
          (if (eq (plist-get res :outcome) 'conflict)
              (let* ((files (plist-get res :conflicts))
                     (handoff (and (plist-get res :fresh)
                                   (magit-dash-chain--handle-conflict
                                    plan step (plist-put (copy-sequence target) :worktree wt)
                                    files (cdr source))))
                     (msg (format "Merge conflict merging %s into %s in %s (%d file(s): %s)%s; resolve and run chain resume"
                                  (cdr source) branch wt (length files)
                                  (string-join files ", ")
                                  (if (memq handoff '(agent-shell gptel))
                                      (format ", handed off to %s" handoff) ""))))
                (magit-dash-chain--set-result plan id :status 'blocked :conflicts files
                                              :worktree-path wt :error msg)
                (error "%s" msg))
            (magit-dash-chain--verify wt branch src-sha)
            (let ((sha (plist-get res :sha)))
              (magit-dash-chain--set-result
               plan id :status 'done :outcome (symbol-name (plist-get res :outcome))
               :sha sha :source (cdr source) :worktree-path wt :conflicts nil :error nil
               :finished (magit-dash-chain--now))
              (setf (hitl-plan-step-note step)
                    (format "%s %s" (plist-get res :outcome) (substring sha 0 (min 10 (length sha))))))))
      (error
       (unless (eq (plist-get (magit-dash-chain--result plan id) :status) 'blocked)
         (magit-dash-chain--set-result plan id :status 'failed
                                       :error (error-message-string err)))
       (signal (car err) (cdr err))))))

(defun magit-dash-chain--land-default (target)
  "Push TARGET's branch, merge its PR with `gh', and fetch the base."
  (let ((dir (plist-get target :worktree))
        (branch (plist-get target :branch))
        (base (plist-get target :base))
        (number (plist-get target :number))
        (slug (plist-get target :slug)))
    (unless number (error "Branch %s has no PR to merge" branch))
    (let ((magit-dash-chain--allow-push t))
      (magit-dash-chain--git-ok dir "push" "origin" branch))
    (cl-flet ((gh (&rest args)
                (let ((res (magit-dash-gh--call-process
                            (append args (and slug (list "-R" slug))) dir)))
                  (unless (zerop (car res))
                    (error "gh %s failed: %s" (car args) (cdr res))))))
      (unless (equal (plist-get target :pr-base) base)
        (gh "pr" "edit" (number-to-string number) "--base" base))
      (gh "pr" "merge" (number-to-string number) "--merge"))
    (magit-dash-chain--git-ok dir "fetch" "origin" base)))

(defun magit-dash-chain--apply-land (plan step target)
  "Run the `land' STEP of PLAN for TARGET, only for an approved plan."
  (let ((id (hitl-plan-step-id step)))
    (unless (or (not (hitl-plan-origin-mcp-p plan)) (hitl-plan-approved-p plan))
      (error "Landing PR #%s requires user approval of this plan" (plist-get target :number)))
    (magit-dash-chain--set-result plan id :status 'running :started (magit-dash-chain--now))
    (condition-case err
        (progn
          (funcall magit-dash-chain-land-function
                   (plist-put (copy-sequence target) :worktree
                              (or (magit-dash-chain--worktree-for-branch
                                   (plist-get target :repo-dir) (plist-get target :branch))
                                  (plist-get target :worktree))))
          (magit-dash-chain--set-result plan id :status 'done :finished (magit-dash-chain--now))
          (setf (hitl-plan-step-note step) "landed"))
      (error
       (magit-dash-chain--set-result plan id :status 'failed :error (error-message-string err))
       (signal (car err) (cdr err))))))

(defun magit-dash-chain--apply ()
  "Apply the current chain plan step (for `chain-step')."
  (let ((plan hitl-plan-current-plan)
        (step hitl-plan-current-step)
        (target hitl-plan-current-target))
    (pcase (plist-get target :kind)
      ('update (magit-dash-chain--apply-update plan step target))
      ('land (magit-dash-chain--apply-land plan step target))
      (kind (error "Unknown chain step kind: %S" kind)))))

(defun magit-dash-chain--driver (plan op steps resolved)
  "Run STEPS of OP in order and stop at the first that does not finish.
Later steps depend on earlier ones, so a failed or blocked step halts
the plan, leaving the rest pending.  RESOLVED maps steps to targets."
  (catch 'halt
    (dolist (step steps)
      (let ((status (hitl-plan-run-step plan step op (cdr (assq step resolved)))))
        (magit-dash-chain--write-record plan)
        (unless (eq status 'done)
          (throw 'halt nil))))))

;;;; Domain and operation

(defun magit-dash-chain--directory ()
  "Return the directory in which chain plans are stored."
  (file-name-as-directory
   (expand-file-name (or magit-dash-chain-plan-directory
                         (expand-file-name "magit-dash-chain/" hitl-plan-directory)))))

(defun magit-dash-chain--format-target (target)
  "Return (FILE . LABEL) columns for TARGET."
  (cons (or (plist-get target :worktree) "") (magit-dash-chain--step-label target)))

(defun magit-dash-chain--visit-target (target &optional other-window)
  "Visit TARGET's worktree in Dired, in another window when OTHER-WINDOW."
  (when-let* ((dir (plist-get target :worktree))
              ((file-directory-p dir)))
    (if other-window (dired-other-window dir) (dired dir))))

(defun magit-dash-chain--sanitize-target (_target)
  "Refuse caller-supplied targets: chain plans come only from discovery."
  (user-error "Chain plans are created with chain_plan, not from explicit targets"))

(hitl-plan-register-domain 'magit-dash-chain
  :label "PR chain"
  :resolve-target #'identity
  :format-target #'magit-dash-chain--format-target
  :visit-target #'magit-dash-chain--visit-target
  :driver #'magit-dash-chain--driver
  :directory #'magit-dash-chain--directory
  :sanitize-target #'magit-dash-chain--sanitize-target)

(hitl-plan-register-op 'magit-dash-chain 'chain-step
  :label "chain step"
  :queueable t
  :apply #'magit-dash-chain--apply
  :describe (lambda (&rest _) "merge parent/base into each chain branch (no rebase)")
  :schema '("none: each target carries its own kind (update or land)"))

;;;; Plan construction

(defun magit-dash-chain--targets (chain land)
  "Return the ordered plan targets for validated CHAIN; add land steps if LAND."
  (let ((base (plist-get chain :base))
        (repo (plist-get chain :repo-dir))
        (slug (plist-get chain :slug))
        targets)
    (dolist (node (plist-get chain :nodes))
      (let* ((parent (plist-get node :parent))
             (common (list :branch (plist-get node :branch) :number (plist-get node :number)
                           :repo-dir repo :slug slug :base base
                           :worktree (plist-get node :worktree)
                           :file (plist-get node :worktree))))
        (push (append (list :kind 'update)
                      (if (and parent (not land))
                          (list :source-kind 'branch :source parent)
                        (list :source-kind 'base :source base :fetch (and land t)))
                      common)
              targets)
        (when land
          (push (append (list :kind 'land :pr-base (plist-get node :pr-base)) common)
                targets))))
    (nreverse targets)))

(cl-defun magit-dash-chain-create-plan (chain &key land origin directory)
  "Create and save a plan for validated CHAIN and return it.
LAND adds explicit push/merge-PR steps.  ORIGIN is `mcp' for plans created
on behalf of an MCP caller (making them subject to approval gating)."
  (let* ((ascii (magit-dash-chain-render-ascii chain))
         (plan (hitl-plan-create
                :domain 'magit-dash-chain
                :directory directory
                :targets (magit-dash-chain--targets chain land)
                :ops (list 'chain-step)
                :description (format "PR chain %s onto %s (merge commits only, no rebase)\n\n%s"
                                     (plist-get chain :root) (plist-get chain :base) ascii)
                :metadata (append (and origin (list :origin origin))
                                  (list :chain (list :repo-dir (plist-get chain :repo-dir)
                                                     :base (plist-get chain :base)
                                                     :root (plist-get chain :root)
                                                     :land (and land t)
                                                     :tree ascii
                                                     :chain-data chain
                                                     :results nil))))))
    (magit-dash-chain--write-record plan)
    plan))

;;;; Resume

(defun magit-dash-chain--plan (plan-or-id)
  "Return the chain plan for PLAN-OR-ID, loading it from disk if needed."
  (let ((plan (if (hitl-plan-p plan-or-id)
                  plan-or-id
                (hitl-plan-get plan-or-id nil 'magit-dash-chain))))
    (unless plan (user-error "Unknown chain plan: %s" plan-or-id))
    (unless (eq (hitl-plan-domain plan) 'magit-dash-chain)
      (user-error "Plan %s is not a PR chain plan" (hitl-plan-id plan)))
    plan))

;;;###autoload
(defun magit-dash-chain-resume (plan-or-id)
  "Retry the failed or blocked steps of chain PLAN-OR-ID and continue the plan.
A step halted by a merge conflict finds the in-progress merge, checks that
the conflicts are resolved (committing the merge when they are staged but
not yet committed), and the plan then proceeds with the remaining steps.
This works from a plan loaded from disk after an Emacs restart.
Return the plan."
  (interactive
   (list (completing-read "Resume chain plan: "
                          (mapcar #'hitl-plan-id
                                  (hitl-plan-list '(draft running paused) nil 'magit-dash-chain)))))
  (let ((plan (magit-dash-chain--plan plan-or-id)))
    (dolist (step (hitl-plan-steps plan))
      (when (eq (hitl-plan-step-status step) 'failed)
        (setf (hitl-plan-step-status step) 'pending
              (hitl-plan-step-message step) nil)))
    (hitl-plan-save plan)
    (hitl-plan-execute plan)
    (magit-dash-chain--write-record plan)
    plan))

;;;; Rollback

(defun magit-dash-chain--rollback-step (plan step)
  "Roll back the update STEP of PLAN; return (STEP-ID ACTION . REASON).
ACTION is `reset', `aborted' or `refused'."
  (let* ((id (hitl-plan-step-id step))
         (res (magit-dash-chain--result plan id))
         (status (plist-get res :status))
         (wt (plist-get res :worktree-path))
         (before (plist-get res :before)))
    (cl-flet ((refuse (why) (list id 'refused why))
              (finish (action)
                (magit-dash-chain--set-result plan id :status 'rolled-back :before nil :sha nil
                                              :rolled-back (magit-dash-chain--now))
                (setf (hitl-plan-step-status step) 'pending
                      (hitl-plan-step-message step) nil)
                (hitl-plan-save plan)
                (list id action nil)))
      (cond
       ((not (memq status '(done blocked))) (refuse "step has not merged anything"))
       ((not before) (refuse "plan has no recorded pre-merge tip (:before); it cannot be rolled back"))
       ((not (and wt (file-directory-p wt))) (refuse (format "worktree %s is gone" wt)))
       ((eq status 'blocked)
        (if (magit-dash-chain--merge-in-progress-p wt)
            (progn (magit-dash-chain--git-ok wt "merge" "--abort")
                   (finish 'aborted))
          (refuse "no merge is in progress")))
       ((equal before (plist-get res :sha)) (refuse "step made no merge commit"))
       ((magit-dash-chain--merge-in-progress-p wt) (refuse "a merge is in progress"))
       ((magit-dash-chain--dirty-p wt) (refuse "worktree has uncommitted changes"))
       ((not (equal (magit-dash-chain--head wt) (plist-get res :sha)))
        (refuse "HEAD is no longer the recorded merge commit"))
       (t (magit-dash-chain--git-ok wt "reset" "--hard" before)
          (finish 'reset))))))

;;;###autoload
(defun magit-dash-chain-rollback (plan-or-id &optional step-id)
  "Undo the merges recorded by chain PLAN-OR-ID, last step first.
With STEP-ID, roll back only that step.  A done step is reset to the tip
recorded before its merge, only when its worktree is clean and HEAD is
still the recorded merge commit.  A blocked step has its merge aborted.
Anything else is refused with the reason; if only some steps could be
undone, a message names the refused ones and the plan is paused.
Return a list of \(STEP-ID ACTION REASON) entries, ACTION being `reset',
`aborted' or `refused'."
  (interactive
   (let ((id (completing-read "Roll back chain plan: "
                              (mapcar #'hitl-plan-id
                                      (hitl-plan-list nil nil 'magit-dash-chain)))))
     (unless (yes-or-no-p (format "Reset the merged worktrees of plan %s? " id))
       (user-error "Rollback cancelled"))
     (list id)))
  (let* ((plan (magit-dash-chain--plan plan-or-id))
         (steps (seq-filter
                 (lambda (s) (and (eq (plist-get (hitl-plan-step-target s) :kind) 'update)
                                  (or (null step-id) (equal step-id (hitl-plan-step-id s)))
                                  (memq (plist-get (magit-dash-chain--result
                                                    plan (hitl-plan-step-id s))
                                                   :status)
                                        '(done blocked))))
                 (reverse (hitl-plan-steps plan))))
         (out (mapcar (lambda (s) (magit-dash-chain--rollback-step plan s)) steps)))
    ;; A rolled-back step is pending again, so a complete plan can re-run.
    ;; A partial rollback (some steps undone, some refused) leaves later
    ;; branches holding merges of states that no longer exist, so say so.
    (let ((undone (seq-filter (lambda (r) (memq (nth 1 r) '(reset aborted))) out))
          (refused (seq-filter (lambda (r) (eq (nth 1 r) 'refused)) out)))
      (when (and undone (memq (hitl-plan-status plan) '(complete paused)))
        (setf (hitl-plan-status plan) 'paused)
        (hitl-plan-save plan))
      (when (and undone refused)
        (message "Chain plan %s partially rolled back: %s undone; refused: %s.  \
Refused branches still contain merges of the undone states"
                 (hitl-plan-id plan)
                 (mapconcat (lambda (r) (format "%s" (nth 0 r))) (reverse undone) ", ")
                 (mapconcat (lambda (r) (format "%s (%s)" (nth 0 r) (nth 2 r))) refused ", "))))
    (magit-dash-chain--write-record plan)
    (when (called-interactively-p 'interactive)
      (message "%s" (if out
                        (mapconcat (lambda (r) (format "step %s: %s%s" (nth 0 r) (nth 1 r)
                                                       (if (nth 2 r) (concat " (" (nth 2 r) ")") "")))
                                   out "; ")
                      "Nothing to roll back")))
    out))

;;;; Interactive entry points

(declare-function hitl-plan-review "hitl-plan-ui")
(declare-function magit-dash-gh-pr-dashboard--entry-at-point "magit-dash-gh-pr")
(declare-function magit-dash-gh-pr-dashboard--find-local-path "magit-dash-gh-pr")
(declare-function magit-dash-mcp--resolve-repo-slug "magit-dash-mcp")
(declare-function magit-dash--repo-at-point "magit-dash")
(declare-function magit-dash-repo-path "magit-dash")

(defun magit-dash-chain--context ()
  "Return (REPO-DIR . ROOT-DEFAULT) from the dashboard entry at point, or nil."
  (cond
   ((and (derived-mode-p 'tabulated-list-mode) (fboundp 'magit-dash-gh-pr-dashboard--entry-at-point)
         (ignore-errors (magit-dash-gh-pr-dashboard--entry-at-point)))
    (let* ((entry (magit-dash-gh-pr-dashboard--entry-at-point))
           (path (magit-dash-gh-pr-dashboard--find-local-path (plist-get entry :repo))))
      (when path (cons path (and (plist-get entry :number)
                                 (number-to-string (plist-get entry :number)))))))
   ((and (derived-mode-p 'magit-dash-mode) (tabulated-list-get-id))
    (cons (magit-dash-repo-path (magit-dash--repo-at-point)) nil))))

;;;###autoload
(defun magit-dash-chain-plan (repo-dir root &optional base land)
  "Plan a sequential merge of the PR chain under ROOT in REPO-DIR.
Interactively, use the PR or repository at point in a magit-dash buffer
\(else prompt), show the validated tree, and open the plan in the hitl
review buffer; press the review buffer's execute key to run it.  With a
prefix argument also add steps that push and merge each PR (LAND).
Never rebases; merge conflicts halt the plan (see `magit-dash-chain-resume')."
  (interactive
   (let* ((ctx (magit-dash-chain--context))
          (dir (or (car ctx)
                   (read-directory-name "Repository: " (magit-dash-chain--toplevel))))
          (root (read-string "Root PR number or branch: " (cdr ctx))))
     (list dir root nil current-prefix-arg)))
  (let* ((slug (magit-dash-mcp--resolve-repo-slug repo-dir repo-dir))
         (chain (magit-dash-chain-validate
                 (plist-put (magit-dash-chain-discover
                             repo-dir root base (magit-dash-chain--fetch-prs slug repo-dir))
                            :slug slug)))
         (errors (plist-get chain :errors)))
    (if errors
        (with-current-buffer (get-buffer-create "*magit-dash-chain*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (magit-dash-chain-render-ascii chain) "\n\nErrors:\n"
                    (mapconcat (lambda (e) (concat "  - " e)) errors "\n") "\n"))
          (special-mode)
          (pop-to-buffer (current-buffer))
          (user-error "Chain is invalid (%d error%s)" (length errors)
                      (if (= 1 (length errors)) "" "s")))
      (let ((plan (magit-dash-chain-create-plan chain :land land)))
        (require 'hitl-plan-ui)
        (hitl-plan-review plan)
        plan))))

(defun magit-dash-chain--toplevel ()
  "Return the current repository root, or `default-directory'."
  (or (and (fboundp 'magit-toplevel) (magit-toplevel)) default-directory))

;;;###autoload
(defun magit-dash-chain-open (plan-or-id)
  "Open chain PLAN-OR-ID (prompting) in the hitl review buffer."
  (interactive
   (list (completing-read "Chain plan: "
                          (mapcar #'hitl-plan-id
                                  (hitl-plan-list '(draft running paused) nil 'magit-dash-chain)))))
  (require 'hitl-plan-ui)
  (hitl-plan-review (magit-dash-chain--plan plan-or-id)))

;;;; MCP-facing entry points (JSON-ready results)

(defun magit-dash-chain--results-json (plan)
  "Return PLAN's recorded per-step results as a JSON-ready vector."
  (vconcat
   (mapcar
    (lambda (step)
      (let ((res (magit-dash-chain--result plan (hitl-plan-step-id step))))
        (list :step (hitl-plan-step-id step)
              :label (magit-dash-chain--step-label (hitl-plan-step-target step))
              :status (symbol-name (hitl-plan-step-status step))
              :result_status (if (plist-get res :status) (symbol-name (plist-get res :status)) :null)
              :sha (or (plist-get res :sha) :null)
              :conflicts (vconcat (plist-get res :conflicts))
              :error (or (plist-get res :error) :null)
              :started (or (plist-get res :started) :null)
              :finished (or (plist-get res :finished) :null))))
    (hitl-plan-steps plan))))

(defun magit-dash-chain-plan-json (repo-dir slug root &optional base land origin)
  "Discover, validate and render the chain under ROOT; create the plan.
Return a JSON-ready plist with the rendered tree.  No plan is created
when validation finds errors.  REPO-DIR and SLUG name the repository."
  (let* ((prs (magit-dash-chain--fetch-prs slug repo-dir))
         (chain (magit-dash-chain-validate
                 (plist-put (magit-dash-chain-discover repo-dir root base prs)
                            :slug slug)))
         (errors (plist-get chain :errors))
         (plan (and (null errors)
                    (magit-dash-chain-create-plan chain :land land :origin origin))))
    (list :valid (if errors :false t)
          :errors (vconcat errors)
          :warnings (vconcat (plist-get chain :warnings))
          :base (plist-get chain :base)
          :root (plist-get chain :root)
          :tree (magit-dash-chain-render-ascii chain)
          :org (magit-dash-chain-render-org chain)
          :plan_id (if plan (hitl-plan-id plan) :null)
          :record_file (if plan (magit-dash-chain-record-file plan) :null)
          :steps (if plan
                     (vconcat (mapcar (lambda (s) (magit-dash-chain--step-label (hitl-plan-step-target s)))
                                      (hitl-plan-steps plan)))
                   []))))

(defun magit-dash-chain-resume-json (plan-id)
  "Resume chain plan PLAN-ID (subject to approval) and return a JSON-ready plist."
  (let ((plan (magit-dash-chain--plan plan-id)))
    (if (not (hitl-plan-approved-p plan))
        (list :status "awaiting_user_confirmation" :plan_id (hitl-plan-id plan))
      (magit-dash-chain-resume plan)
      (list :plan_id (hitl-plan-id plan)
            :status (symbol-name (hitl-plan-status plan))
            :steps (magit-dash-chain--results-json plan)
            :record_file (magit-dash-chain-record-file plan)))))

(provide 'magit-dash-chain)

;;; magit-dash-chain.el ends here
