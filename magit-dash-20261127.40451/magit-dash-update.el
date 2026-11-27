;;; magit-dash-update.el --- Merge the base branch into a topic worktree -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1
;; Keywords: vc, tools, magit

;; This file is not part of GNU Emacs

;;; Commentary:

;; Headless, data-returning steps for bringing one topic branch (usually
;; a linked worktree such as `.wt/pr-42') up to date with its base: merge
;; `origin/<base>' (never rebase), list and resolve conflict hunks,
;; verify, commit.  Pushing is deliberately absent: it needs an explicit
;; user request and is never done here.
;;
;; Where `magit-dash-chain' merges a whole chain of branches under a
;; hitl plan, this module updates one worktree immediately.  It reuses the
;; chain's git runner (which refuses rebase, pull, force and push) and its
;; merge, resume and conflict helpers, so the two paths behave alike.
;;
;; Cost matters: an agent waits on every step.  The common update is a
;; `status', one `fetch' of the base branch, the `merge' and a `rev-parse'
;; (four git processes); conflict hunks are read from the files in Lisp;
;; `magit-dash-update-verify' needs two git calls plus the project's own
;; checks.  Several worktrees of one repository share a single fetch.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-dash-chain)
(require 'magit-dash-worktree)

(defgroup magit-dash-update nil
  "Update a topic worktree from its base branch."
  :group 'magit-dash
  :prefix "magit-dash-update-")

(defcustom magit-dash-update-protected-branches '("main" "master")
  "Branches `magit-dash-update-branch' refuses to merge into."
  :type '(repeat string)
  :group 'magit-dash-update)

(defcustom magit-dash-update-go-env '("GOTOOLCHAIN=local" "GOSUMDB=off")
  "Environment entries for the Go toolchain runs of `magit-dash-update-verify'.
The defaults keep the toolchain offline."
  :type '(repeat string)
  :group 'magit-dash-update)

(defcustom magit-dash-update-go-parallelism 2
  "Value of `-p' for `go vet' and `go test' in `magit-dash-update-verify'."
  :type 'integer
  :group 'magit-dash-update)

(defcustom magit-dash-update-go-batch-size 100
  "Maximum number of Go packages handed to one `go vet' or `go test'."
  :type 'integer
  :group 'magit-dash-update)

(defcustom magit-dash-update-max-failure-lines 60
  "Maximum failure lines `magit-dash-update-verify' returns."
  :type 'integer
  :group 'magit-dash-update)

;;;; Conflict hunks

(defun magit-dash-update--decode (s)
  "Return the byte string S decoded as UTF-8 text."
  (decode-coding-string s 'utf-8 t))

(defun magit-dash-update--parse-hunks ()
  "Return the conflict hunks of the current unibyte buffer.
Each hunk is a plist: :index, :line (of the opening marker), :ours,
:theirs, :base (diff3 style only, else nil), :beg and :end (the buffer
region covering the markers).  Texts are byte strings."
  (save-excursion
    (goto-char (point-min))
    (let (hunks (i 0))
      (while (re-search-forward "^<<<<<<<\\(?: .*\\)?\r?$" nil t)
        (let ((beg (line-beginning-position))
              (line (line-number-at-pos (line-beginning-position)))
              (ours-beg (min (point-max) (1+ (line-end-position))))
              base-beg sep-beg theirs-beg end)
          (forward-line 1)
          (catch 'done
            (while (not (eobp))
              (cond
               ((and (not sep-beg) (looking-at "|||||||\\(?: .*\\)?\r?$"))
                (setq base-beg (line-beginning-position)
                      sep-beg nil))
               ((looking-at "=======\r?$")
                (setq sep-beg (line-beginning-position))
                (setq theirs-beg (min (point-max) (1+ (line-end-position)))))
               ((and sep-beg (looking-at ">>>>>>>\\(?: .*\\)?\r?$"))
                (setq end (min (point-max) (1+ (line-end-position))))
                (throw 'done nil)))
              (forward-line 1)))
          (when (and sep-beg end)
            (let* ((ours-end (or base-beg sep-beg))
                   (base-text-beg (and base-beg (min (point-max) (1+ (save-excursion
                                                                      (goto-char base-beg)
                                                                      (line-end-position))))))
                   (theirs-end (save-excursion (goto-char (- end 1))
                                               (line-beginning-position))))
              (push (list :index i :line line
                          :ours (buffer-substring-no-properties ours-beg ours-end)
                          :theirs (buffer-substring-no-properties theirs-beg theirs-end)
                          :base (and base-text-beg
                                     (buffer-substring-no-properties base-text-beg sep-beg))
                          :beg beg :end end)
                    hunks)
              (cl-incf i)
              (goto-char end)))))
      (nreverse hunks))))

(defun magit-dash-update--file-hunks (dir file)
  "Return the conflict hunks of FILE (relative to DIR), or nil."
  (let ((path (expand-file-name file dir)))
    (when (file-regular-p path)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((coding-system-for-read 'binary))
          (insert-file-contents path))
        (magit-dash-update--parse-hunks)))))

(defun magit-dash-update--conflict-report (dir unmerged &optional include-hunks max-chars)
  "Describe the conflicts in DIR; UNMERGED is `magit-dash-chain--unmerged'.
Return a list of plists (:file :kind :hunks [:conflicts]).  With
INCLUDE-HUNKS, each hunk's text is included until MAX-CHARS (default
6000) of text has been returned; later hunks carry :truncated t."
  (let ((budget (or max-chars 6000)))
    (mapcar
     (lambda (file)
       (let* ((hunks (magit-dash-update--file-hunks dir file))
              (kind (cdr (assoc file unmerged)))
              (entry (list :file file :kind kind :hunks (length hunks))))
         (when include-hunks
           (plist-put
            entry :conflicts
            (mapcar
             (lambda (h)
               (let* ((ours (magit-dash-update--decode (plist-get h :ours)))
                      (theirs (magit-dash-update--decode (plist-get h :theirs)))
                      (base (and (plist-get h :base)
                                 (magit-dash-update--decode (plist-get h :base))))
                      (size (+ (length ours) (length theirs) (length base))))
                 (if (> size budget)
                     (list :index (plist-get h :index) :line (plist-get h :line)
                           :truncated t :size size)
                   (cl-decf budget size)
                   (append (list :index (plist-get h :index) :line (plist-get h :line)
                                 :ours ours :theirs theirs)
                           (and base (list :base base))))))
             hunks)))
         entry))
     (mapcar #'car unmerged))))

;;;; Update

(defun magit-dash-update--common-dir (dir)
  "Return a key shared by every worktree of the repository DIR belongs to."
  (let ((gd (magit-dash-chain--git-dir dir)))
    (if (and gd (string-match "\\`\\(.*\\)/worktrees/[^/]+/?\\'" gd))
        (match-string 1 gd)
      gd)))

(defun magit-dash-update--check-branch (state base)
  "Signal unless STATE's branch may take a merge of BASE; return the branch."
  (let ((branch (plist-get state :branch)))
    (cond ((null branch) (error "Detached HEAD"))
          ((member branch magit-dash-update-protected-branches)
           (error "Refusing to run on %s" branch))
          ((equal branch base)
           (error "%s is the base branch; nothing to update" branch)))
    branch))

(defun magit-dash-update--abort-staged-files (dir state &optional details)
  "Return sorted paths in DIR with resolved, staged, or worktree merge changes.
STATE is `magit-dash-chain--state' for DIR.  Excludes untouched
unmerged paths and files that `git merge' auto-merged without further
edits.  With DETAILS non-nil, return a plist (:files :staged :wt-reset)."
  (let* ((unmerged (plist-get state :unmerged))
         (mhs (let ((r (magit-dash-chain--git dir "rev-parse" "MERGE_HEAD")))
                (and (zerop (car r))
                     (magit-dash-update--lines (cdr r)))))
         (mt (and (= (length mhs) 1)
                  (magit-dash-chain--git
                   dir "-c" "core.quotePath=false"
                   "merge-tree" "--write-tree" "HEAD" (car mhs))))
         (mt-lines (and mt (memq (car mt) '(0 1))
                        (magit-dash-update--lines (cdr mt))))
         (staged
          (if (and mt-lines (string-match-p "\\`[0-9a-f]\\{40,\\}\\'" (car mt-lines)))
              (let* ((tree (car mt-lines))
                     (orig-unmerged
                      (thread-last
                        (cdr mt-lines)
                        (seq-take-while (lambda (line)
                                          (string-match-p "\\`[0-7]+ [0-9a-f]+ [123]\t" line)))
                        (seq-map (lambda (line)
                                   (when (string-match "\t\\(.*\\)\\'" line)
                                     (match-string 1 line))))
                        (delq nil)
                        delete-dups))
                     (resolved (seq-remove (lambda (f) (member f unmerged)) orig-unmerged))
                     (diff-lines
                      (magit-dash-update--lines
                       (magit-dash-chain--git-ok
                        dir "-c" "core.quotePath=false"
                        "diff-index" "--cached" "--name-status" tree))))
                (thread-last
                  diff-lines
                  (seq-map (lambda (line)
                             (when (string-match "\\`\\([^\t]+\\)\t\\(.*\\)\\'" line)
                               (let ((status (match-string 1 line))
                                     (path (match-string 2 line)))
                                 (when (and (not (equal status "U"))
                                            (not (member path unmerged)))
                                   path)))))
                  (delq nil)
                  (append resolved)
                  delete-dups))
            (thread-last
              (magit-dash-chain--git-ok
               dir "-c" "core.quotePath=false"
               "diff" "--cached" "--name-only")
              (magit-dash-update--lines)
              (seq-remove (lambda (f) (member f unmerged)))
              delete-dups)))
         (wt-non-unmerged
          (thread-last
            (magit-dash-chain--git-ok
             dir "-c" "core.quotePath=false"
             "diff-files" "--name-only")
            (magit-dash-update--lines)
            (seq-remove (lambda (f) (member f unmerged)))))
         (wt-unmerged
          (when unmerged
            (let ((stages-by-file nil)
                  (tmp-dir nil))
              (seq-do
               (lambda (line)
                 (when (string-match
                        "\\`[0-7]+ \\([0-9a-f]+\\) \\([123]\\)\t\\(.*\\)\\'" line)
                   (let ((oid (match-string 1 line))
                         (stage (string-to-number (match-string 2 line)))
                         (file (match-string 3 line)))
                     (push (cons stage oid)
                           (alist-get file stages-by-file nil nil #'equal)))))
               (magit-dash-update--lines
                (magit-dash-chain--git-ok
                 dir "-c" "core.quotePath=false" "ls-files" "-u")))
              (unwind-protect
                  (seq-filter
                   (lambda (file)
                     (let* ((stages (cdr (assoc file stages-by-file)))
                            (oid1 (cdr (assq 1 stages)))
                            (oid2 (cdr (assq 2 stages)))
                            (oid3 (cdr (assq 3 stages)))
                            (path (expand-file-name file dir)))
                       (cond
                        ((and oid2 oid3)
                         (if (not (file-regular-p path))
                             t
                           (unless tmp-dir
                             (setq tmp-dir (make-temp-file "magit-dash-mf-" t)))
                           (let ((f1 (expand-file-name "s1" tmp-dir))
                                 (f2 (expand-file-name "s2" tmp-dir))
                                 (f3 (expand-file-name "s3" tmp-dir)))
                             (if oid1
                                 (process-file "git" nil (list :file f1) nil
                                               "-C" dir "cat-file" "blob" oid1)
                               (write-region "" nil f1 nil 'silent))
                             (process-file "git" nil (list :file f2) nil
                                           "-C" dir "cat-file" "blob" oid2)
                             (process-file "git" nil (list :file f3) nil
                                           "-C" dir "cat-file" "blob" oid3)
                             (let ((expected
                                    (with-temp-buffer
                                      (set-buffer-multibyte nil)
                                      (process-file "git" nil t nil "-C" dir
                                                    "merge-file" "-p" f2 f1 f3)
                                      (goto-char (point-min))
                                      (while (re-search-forward
                                              "^\\(<<<<<<<\\||||||||\\|>>>>>>>\\) [^\r\n]*"
                                              nil t)
                                        (replace-match "\\1" t nil))
                                      (buffer-string)))
                                   (actual
                                    (with-temp-buffer
                                      (set-buffer-multibyte nil)
                                      (let ((coding-system-for-read 'binary))
                                        (insert-file-contents path))
                                      (goto-char (point-min))
                                      (while (re-search-forward
                                              "^\\(<<<<<<<\\||||||||\\|>>>>>>>\\) [^\r\n]*"
                                              nil t)
                                        (replace-match "\\1" t nil))
                                      (buffer-string))))
                               (not (equal expected actual))))))
                        (oid2
                         (or (not (or (file-exists-p path) (file-symlink-p path)))
                             (not (equal oid2
                                         (magit-dash-chain--git-ok
                                          dir "hash-object" "--" file)))))
                        (oid3
                         (or (not (or (file-exists-p path) (file-symlink-p path)))
                             (not (equal oid3
                                         (magit-dash-chain--git-ok
                                          dir "hash-object" "--" file)))))
                        (t
                         (or (file-exists-p path) (file-symlink-p path))))))
                   unmerged)
                (when (and tmp-dir (file-directory-p tmp-dir))
                  (delete-directory tmp-dir t))))))
         (all (seq-sort #'string<
                        (delete-dups (append staged wt-non-unmerged wt-unmerged)))))
    (if details
        (list :files all :staged staged :wt-reset wt-non-unmerged)
      all)))

(defun magit-dash-update--stash-merge-work (dir branch files &optional staged-files)
  "Store a git stash entry in DIR for FILES from BRANCH's merge.
STAGED-FILES is the subset of FILES with staged/resolved index changes
\(defaults to all stage-0 entries among FILES).  Uses a temporary index
seeded from HEAD so unmerged paths elsewhere do not block stashing."
  (let* ((head (magit-dash-chain--head dir))
         (stage-map
          (thread-last
            (apply #'magit-dash-chain--git-ok
                   dir "-c" "core.quotePath=false"
                   "ls-files" "--stage" "--" files)
            (magit-dash-update--lines)
            (seq-map (lambda (line)
                       (when (string-match
                              "\\`\\([0-7]+\\) \\([0-9a-f]+\\) 0\t\\(.*\\)\\'"
                              line)
                         (cons (match-string 3 line)
                               (cons (match-string 1 line)
                                     (match-string 2 line))))))
            (delq nil)))
         (idx-targets (or staged-files
                          (seq-filter (lambda (f) (assoc f stage-map)) files)))
         (base-env (if (zerop (car (magit-dash-chain--git dir "var" "GIT_COMMITTER_IDENT")))
                       process-environment
                     (append '("GIT_AUTHOR_NAME=magit-dash"
                               "GIT_AUTHOR_EMAIL=magit-dash@local"
                               "GIT_COMMITTER_NAME=magit-dash"
                               "GIT_COMMITTER_EMAIL=magit-dash@local")
                             process-environment)))
         (tmp-dir nil))
    (unwind-protect
        (progn
          (setq tmp-dir (make-temp-file "magit-dash-stash-" t))
          (let* ((tmp-idx (expand-file-name "index" tmp-dir))
                 (stash-env (cons (concat "GIT_INDEX_FILE=" tmp-idx) base-env))
                 (idx-tree nil)
                 (wt-tree nil))
            (let ((process-environment stash-env)
                  (present (seq-filter (lambda (f) (assoc f stage-map)) idx-targets))
                  (removed (seq-remove (lambda (f) (assoc f stage-map)) idx-targets)))
              (magit-dash-chain--git-ok dir "read-tree" "HEAD")
              (when removed
                (apply #'magit-dash-chain--git-ok
                       dir "rm" "--cached" "-q" "--ignore-unmatch" "--" removed))
              (seq-do
               (lambda (file)
                 (let ((entry (cdr (assoc file stage-map))))
                   (magit-dash-chain--git-ok
                    dir "update-index" "--add" "--cacheinfo"
                    (car entry) (cdr entry) file)))
               present)
              (setq idx-tree (magit-dash-chain--git-ok dir "write-tree"))
              (seq-do
               (lambda (file)
                 (let ((path (expand-file-name file dir)))
                   (cond
                    ((file-symlink-p path)
                     (let ((sym-file (expand-file-name "sym" tmp-dir)))
                       (let ((coding-system-for-write 'no-conversion))
                         (write-region (encode-coding-string (file-symlink-p path) 'utf-8)
                                       nil sym-file nil 'silent))
                       (magit-dash-chain--git-ok
                        dir "update-index" "--add" "--cacheinfo"
                        "120000"
                        (magit-dash-chain--git-ok dir "hash-object" "-w" "--" sym-file)
                        file)))
                    ((file-exists-p path)
                     (magit-dash-chain--git-ok
                      dir "update-index" "--add" "--cacheinfo"
                      (if (file-executable-p path)
                          "100755"
                        "100644")
                      (magit-dash-chain--git-ok dir "hash-object" "-w" "--" file)
                      file))
                    (t
                     (magit-dash-chain--git-ok
                      dir "rm" "--cached" "-q" "--ignore-unmatch" "--" file)))))
               files)
              (setq wt-tree (magit-dash-chain--git-ok dir "write-tree")))
            (let* ((process-environment base-env)
                   (label (or branch "HEAD"))
                   (idx-commit (magit-dash-chain--git-ok
                                dir "commit-tree" idx-tree "-p" head
                                "-m" (format "index on %s: merge-abort resolved work" label)))
                   (wt-commit (magit-dash-chain--git-ok
                               dir "commit-tree" wt-tree "-p" head "-p" idx-commit
                               "-m" (format "WIP on %s: merge-abort resolved work" label))))
              (magit-dash-chain--git-ok
               dir "stash" "store"
               "-m" (format "magit-dash-update: resolved merge work on %s (%s)"
                            label (string-join files ", "))
               wt-commit))))
      (when (and tmp-dir (file-directory-p tmp-dir))
        (delete-directory tmp-dir t)))))

(cl-defun magit-dash-update-branch (dir &key base (fetch t) (merge t) abort
                                        stash force include-hunks max-chars)
  "Merge origin/BASE into the topic branch checked out in worktree DIR.
BASE defaults to the repository's default branch.  FETCH (default t)
fetches just that branch from origin first.  Without MERGE, only report
how far the branch is ahead of and behind the base.  With ABORT, abort
the merge in progress instead; refuses if resolved, staged, or
working-tree merge changes exist unless STASH saves them first, FORCE
discards them, or an interactive caller confirms stashing.  An
in-progress merge of the same base is resumed, so calling again after
resolving conflicts finishes it.

Return a plist: :branch, :base, :head, and for a merge :outcome (`merged',
`up-to-date', `resumed' or `conflict'), :sha or :conflicts (see
`magit-dash-update--conflict-report', with INCLUDE-HUNKS and MAX-CHARS) and
:rerere.  Signals on a detached HEAD, a protected branch, a dirty tree, or
a failed fetch."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (state (magit-dash-chain--state dir)))
    (if abort
        (let* ((branch (plist-get state :branch))
               (info (progn
                       (unless (plist-get state :merging)
                         (error "No merge in progress in %s" dir))
                       (magit-dash-update--abort-staged-files dir state t)))
               (work (plist-get info :files))
               (wt-reset (plist-get info :wt-reset))
               (stashed nil))
          (when work
            (cond
             (stash
              (magit-dash-update--stash-merge-work
               dir branch work (plist-get info :staged))
              (setq stashed t))
             (force nil)
             ((magit-dash-worktree--unattended-p)
              (user-error
               "Refusing to abort merge in %s: resolved or staged changes in %s (stash first or pass :stash t / :force t)"
               dir (string-join work ", ")))
             ((yes-or-no-p
               (format "Merge in %s has resolved or staged changes (%s).  Stash them before aborting? "
                       (abbreviate-file-name dir) (string-join work ", ")))
              (magit-dash-update--stash-merge-work
               dir branch work (plist-get info :staged))
              (setq stashed t))
             (t
              (user-error
               "Merge abort cancelled: resolved or staged changes remain in %s (%s)"
               dir (string-join work ", ")))))
          (when wt-reset
            (apply #'magit-dash-chain--git-ok dir "checkout" "-q" "--" wt-reset))
          (magit-dash-chain--git-ok dir "merge" "--abort")
          (append (list :branch branch :outcome 'aborted)
                  (when stashed
                    (list :stashed t :stashed-files work))))
      (let* ((base (or base (magit-dash-chain--default-base dir)))
             (_ (when (or (string-prefix-p "-" base) (string-match-p "[[:space:]]" base))
                  (error "Invalid base branch name %S" base)))
             (branch (magit-dash-update--check-branch state base))
             (ref (concat "refs/remotes/origin/" base))
             (label (concat "origin/" base)))
        (when (and merge (plist-get state :dirty) (not (plist-get state :merging)))
          (error "Dirty working tree in %s: commit or stash tracked changes first" dir))
        (when fetch
          (magit-dash-chain--git-ok dir "fetch" "origin" base "--quiet"))
        (if (not merge)
            (pcase-let ((`(,ahead ,behind)
                         (split-string (magit-dash-chain--git-ok
                                        dir "rev-list" "--left-right" "--count"
                                        (concat "HEAD..." ref)))))
              (list :branch branch :base base :head (plist-get state :head)
                    :ahead (string-to-number ahead) :behind (string-to-number behind)
                    :merging (plist-get state :merging)
                    :dirty (plist-get state :dirty)))
          (let ((res (magit-dash-chain--update-worktree dir branch ref label state)))
            (if (eq (plist-get res :outcome) 'conflict)
                (let ((now (magit-dash-chain--unmerged dir)))
                  (list :branch branch :base base :outcome 'conflict
                        :conflicts (magit-dash-update--conflict-report
                                    dir now include-hunks max-chars)
                        :rerere (equal "true"
                                       (cdr (magit-dash-chain--git
                                             dir "config" "rerere.enabled")))))
              (list :branch branch :base base
                    :outcome (plist-get res :outcome) :sha (plist-get res :sha)))))))))

(cl-defun magit-dash-update-branches (dirs &rest keys &key (fetch t) &allow-other-keys)
  "Run `magit-dash-update-branch' on each of DIRS with KEYS.
KEYS are the keyword arguments of that function; FETCH applies per
repository.  Worktrees of one repository share a single fetch.  A failure in one
worktree is returned as (:worktree DIR :error MESSAGE) and does not stop
the rest."
  (let (fetched)
    (mapcar
     (lambda (dir)
       (let* ((key (magit-dash-update--common-dir dir))
              (do-fetch (and fetch (not (member key fetched)))))
         (condition-case err
             (let ((res (apply #'magit-dash-update-branch dir
                               :fetch do-fetch (magit-dash-update--plist-delete keys '(:fetch)))))
               (when do-fetch (push key fetched))
               (cons :worktree (cons dir res)))
           (error (list :worktree dir :error (error-message-string err))))))
     dirs)))

(defun magit-dash-update--plist-delete (plist keys)
  "Return PLIST without the entries named in KEYS."
  (let (out)
    (while plist
      (unless (memq (car plist) keys)
        (setq out (append out (list (car plist) (cadr plist)))))
      (setq plist (cddr plist)))
    out))

;;;; Resolve

(defun magit-dash-update--take (hunk take text)
  "Return the replacement string for HUNK under TAKE (and TEXT for `text')."
  (pcase take
    ("ours" (plist-get hunk :ours))
    ("theirs" (plist-get hunk :theirs))
    ("both" (concat (plist-get hunk :ours) (plist-get hunk :theirs)))
    ("both_theirs_first" (concat (plist-get hunk :theirs) (plist-get hunk :ours)))
    ("base" (or (plist-get hunk :base)
                (error "Hunk %d has no base section (not diff3 style)" (plist-get hunk :index))))
    ("text" (let ((s (encode-coding-string (or text (error "take \"text\" needs `text'")) 'utf-8)))
              (if (or (string-empty-p s) (string-suffix-p "\n" s)) s (concat s "\n"))))
    (_ (error "Unknown take %S (use ours, theirs, both, both_theirs_first, base or text)" take))))

(defun magit-dash-update--resolve-file (dir file spec)
  "Apply SPEC to the conflict hunks of FILE in DIR; return a result plist.
SPEC has :take/:text for every hunk, overridden per hunk by :hunks, a
list of plists with :index, :take and :text.  Hunks without a choice are
left in place."
  (let* ((path (expand-file-name file dir))
         (default (plist-get spec :take))
         (per (plist-get spec :hunks))
         (total 0) (resolved 0))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'binary)) (insert-file-contents path))
      (let ((hunks (magit-dash-update--parse-hunks)))
        (setq total (length hunks))
        (dolist (h (reverse hunks))
          (let* ((own (seq-find (lambda (p) (eql (plist-get p :index) (plist-get h :index))) per))
                 (take (or (plist-get own :take) default))
                 (text (if own (plist-get own :text) (plist-get spec :text))))
            (when take
              (let ((new (magit-dash-update--take h take text)))
                (goto-char (plist-get h :beg))
                (delete-region (plist-get h :beg) (plist-get h :end))
                (insert new)
                (cl-incf resolved)))))
        (when (> resolved 0)
          (let ((coding-system-for-write 'binary))
            (write-region (point-min) (point-max) path nil 'silent)))))
    (list :file file :hunks total :resolved resolved :remaining (- total resolved))))

(cl-defun magit-dash-update-resolve (dir specs &optional (stage t))
  "Resolve conflict hunks in worktree DIR according to SPECS.
Each spec is a plist with :file (a conflicted path relative to DIR) and
:take (`ours', `theirs', `both', `both_theirs_first', `base' or `text'
with :text), optionally refined per hunk by :hunks.  Only files git lists
as unmerged are touched.  With STAGE (the default) every fully resolved
file is staged in one `git add'.  Return a list of per-file plists
\(:file :hunks :resolved :remaining :staged), or (:file F :error MSG)."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (state (magit-dash-chain--state dir))
         (unmerged (plist-get state :unmerged))
         (results
          (mapcar
           (lambda (spec)
             (let ((file (plist-get spec :file)))
               (condition-case err
                   (progn
                     (unless (member file unmerged)
                       (error "%s is not an unmerged path" file))
                     (magit-dash-update--resolve-file dir file spec))
                 (error (list :file file :error (error-message-string err))))))
           specs))
         (done (seq-filter (lambda (r) (and (not (plist-get r :error))
                                            (> (plist-get r :hunks) 0)
                                            (zerop (plist-get r :remaining))))
                           results)))
    (when (and stage done)
      (apply #'magit-dash-chain--git-ok dir "add" "--"
             (mapcar (lambda (r) (plist-get r :file)) done)))
    (mapcar (lambda (r) (append r (list :staged (and stage (memq r done) t)))) results)))

;;;; Verify

(defun magit-dash-update--lines (s)
  "Return the non-empty lines of S."
  (split-string s "\n" t))

(defun magit-dash-update--tip (dir state)
  "Return the revision DIR's merge started from, given its git STATE.
That is HEAD while merging, HEAD^1 once the merge commit landed, else HEAD."
  (cond ((plist-get state :merging) "HEAD")
        ((magit-dash-chain--git-p dir "rev-parse" "--verify" "-q" "HEAD^2") "HEAD^1")
        (t "HEAD")))

(defun magit-dash-update--changed-files (dir tip &optional pathspec)
  "Return the files DIR's working tree changed relative to TIP.
That is what the merge brought in plus any conflict resolutions, in one
git process.  PATHSPEC limits the listing."
  (magit-dash-update--lines
   (cdr (apply #'magit-dash-chain--git dir "-c" "core.quotePath=false"
               "diff" "--name-only" tip (and pathspec (list "--" pathspec))))))

(defun magit-dash-update--go-packages (files dir)
  "Return the Go package directories (\"./pkg/x\") of FILES that exist in DIR."
  (thread-last files
               (seq-filter (lambda (f) (string-suffix-p ".go" f)))
               (seq-remove (lambda (f) (string-match-p "testdata\\|tests/e2e" f)))
               (mapcar #'file-name-directory)
               (delq nil)
               delete-dups
               (seq-filter (lambda (d) (file-directory-p (expand-file-name d dir))))
               (mapcar (lambda (d) (concat "./" (directory-file-name d))))))

(defun magit-dash-update-touched-go-packages (dir state &optional base)
  "Return package directories of Go files the base changed in DIR's merge.
DIR is the worktree and STATE its git state.  With no merge to inspect
\(HEAD is not a merge and none is in progress) the range falls back to
HEAD...origin/BASE, so the answer stays right after the merge commit lands."
  (let ((tip (magit-dash-update--tip dir state)))
    (magit-dash-update--go-packages
     (if (and (equal tip "HEAD") (not (plist-get state :merging)))
         (magit-dash-update--lines
          (cdr (magit-dash-chain--git
                dir "diff" "--name-only"
                (format "HEAD...origin/%s" (or base (magit-dash-chain--default-base dir)))
                "--" "*.go")))
       (magit-dash-update--changed-files dir tip "*.go"))
     dir)))

(defun magit-dash-update--go (dir program &rest args)
  "Run PROGRAM ARGS in DIR with the Go environment; return the output lines."
  (let ((default-directory (file-name-as-directory dir))
        (process-environment (append magit-dash-update-go-env process-environment)))
    (with-temp-buffer
      (apply #'process-file program nil t nil args)
      (magit-dash-update--lines (buffer-string)))))

(defun magit-dash-update--go-failures (dir pkgs with-tests changed)
  "Return `go vet' (and with WITH-TESTS `go test') failure lines for PKGS in DIR.
`gofmt -l' also checks the Go files among CHANGED."
  (let (failures
        (p (number-to-string magit-dash-update-go-parallelism)))
    (dolist (batch (seq-split pkgs magit-dash-update-go-batch-size))
      (dolist (cmd (if with-tests '("vet" "test") '("vet")))
        (dolist (l (apply #'magit-dash-update--go dir "go" cmd "-p" p batch))
          (when (and (string-match-p "^\\(FAIL\\|--- \\|panic\\)\\|_test\\.go\\|^vet:" l)
                     (not (string-match-p "build constraints exclude" l)))
            (push l failures)))))
    (when-let* ((files (seq-filter (lambda (f) (and (string-suffix-p ".go" f)
                                                    (file-exists-p (expand-file-name f dir))))
                                   changed))
                (bad (apply #'magit-dash-update--go dir "gofmt" "-l" files)))
      (dolist (f bad) (push (concat "gofmt: " f) failures)))
    (nreverse failures)))

(defun magit-dash-update--commit (dir state merging message)
  "Stage tracked changes in DIR and commit; return the new HEAD SHA or nil.
Finishes an in-progress merge (MERGING) with git's own message; otherwise
commits tracked changes with MESSAGE when STATE (DIR's git state) is dirty."
  (cond
   (merging
    (magit-dash-chain--git-ok dir "add" "-u")
    (magit-dash-chain--git-ok dir "commit" "--no-edit")
    (magit-dash-chain--head dir))
   ((and state (plist-get state :dirty))
    (magit-dash-chain--git-ok dir "add" "-u")
    (magit-dash-chain--git-ok dir "commit" "-m" message)
    (magit-dash-chain--head dir))))

(cl-defun magit-dash-update-verify (dir &key tests commit message base)
  "Check that worktree DIR has no conflict markers and builds, optionally commit.
Only files the merge changed are scanned for markers, and only Go packages
the base changed are vetted, plus `gofmt -l' on changed files.  TESTS
also runs `go test'.  `magit-dash-chain-verify-command' runs too when
set.  With COMMIT and no failures, stage tracked changes and commit: the
merge in progress with git's message, or tracked changes with MESSAGE.
BASE is only needed for the fallback Go range after a fast-forward.

The check costs three git processes (the unmerged list while merging or a
merge probe otherwise, one diff, one scoped grep) plus the Go toolchain.

Return a plist: :ok, :failures (lines), :markers (files still conflicted),
:needs_review (unmerged paths with no markers, e.g. modify/delete or
resolved by rerere), :packages (the vetted Go packages) and, with COMMIT,
:committed and :sha."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (gd (magit-dash-chain--git-dir dir))
         (merging (and gd (file-exists-p (expand-file-name "MERGE_HEAD" gd))))
         (unmerged (and merging (mapcar #'car (magit-dash-chain--unmerged dir))))
         (state (list :merging merging :unmerged unmerged))
         (tip (magit-dash-update--tip dir state))
         (changed (magit-dash-update--changed-files dir tip))
         (scope (delete-dups (append changed unmerged)))
         (markers (and scope (magit-dash-chain--conflict-marker-files dir scope)))
         (review (seq-remove (lambda (f) (member f markers)) unmerged)))
    (if markers
        (list :ok nil :markers markers :needs_review review :failures nil)
      (let* ((go-p (file-exists-p (expand-file-name "go.mod" dir)))
             (pkgs (and go-p
                        (if (or (not (equal tip "HEAD")) (plist-get state :merging))
                            (magit-dash-update--go-packages changed dir)
                          (magit-dash-update-touched-go-packages dir state base))))
             (failures (and go-p (magit-dash-update--go-failures dir pkgs tests changed)))
             (failures (or failures
                           (and magit-dash-chain-verify-command
                                (let ((default-directory dir))
                                  (unless (zerop (call-process-shell-command
                                                  magit-dash-chain-verify-command nil nil))
                                    (list (concat "verify command failed: "
                                                  magit-dash-chain-verify-command)))))))
             (failures (seq-take failures magit-dash-update-max-failure-lines))
             (ok (null failures))
             (sha (and ok commit
                       (magit-dash-update--commit
                        dir (and commit (not merging) (magit-dash-chain--state dir))
                        merging (or message
                                      (format "fix: adapt to changes merged from %s"
                                              (or base "the base branch")))))))
        (append (list :ok ok :failures failures :markers nil :needs_review review
                      :packages pkgs)
                (and commit (list :sha sha :committed (and sha t))))))))

(provide 'magit-dash-update)

;;; magit-dash-update.el ends here
