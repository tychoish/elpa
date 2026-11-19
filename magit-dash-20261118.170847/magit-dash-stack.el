;;; magit-dash-stack.el --- Discover, measure, draw and sync stacks of branches -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (magit "4.0") (hitl "0.1"))
;; Keywords: vc, tools, magit
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; A stack is an ordered set of local topic branches, each built on the
;; previous one.  This module discovers a stack from a branch glob,
;; measures every commit, draws the stack (a `magit-section' buffer, dot
;; and mermaid), detects drift, and brings drifted branches up to date by
;; handing the stack to `magit-dash-chain' as a plan of ordinary merge
;; steps.
;;
;; The module is read-mostly.  It runs only read-only git (through magit)
;; and writes exactly one thing itself: the recorded parent of a branch in
;; git config (`branch.NAME.magit-dash-stack-parent').  Every change to a
;; branch goes through `magit-dash-chain', whose plan the user confirms
;; in the hitl review buffer.  Only the public API of that module is used.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-git)
(require 'magit-section)
(require 'magit-diff)
(require 'magit-log)
(require 'magit-dash-chain)
(require 'magit-dash-worktree)

(declare-function hitl-plan-review "hitl-plan-ui")
(declare-function magit-dash-gh--owner-repo-from-url "magit-dash-gh")

(defconst magit-dash-stack--config-key "magit-dash-stack-parent"
  "Name of the per-branch git config variable holding the recorded parent.")

(defgroup magit-dash-stack nil
  "Stacks of dependent local branches."
  :group 'magit-dash
  :prefix "magit-dash-stack-")

;;;; Git helpers (read-only, through magit)

(defun magit-dash-stack--lines (dir &rest args)
  "Return the output lines of git ARGS run in DIR."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (apply #'magit-git-lines args)))

(defun magit-dash-stack--ok (dir &rest args)
  "Return non-nil when git ARGS succeed in DIR."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (apply #'magit-git-success args)))

(defun magit-dash-stack--ancestor-p (dir ancestor rev)
  "Return non-nil when ANCESTOR is an ancestor of (or equal to) REV in DIR."
  (magit-dash-stack--ok dir "merge-base" "--is-ancestor" ancestor rev))

(defun magit-dash-stack--count (dir range)
  "Return the number of commits in RANGE in DIR."
  (string-to-number (or (car (magit-dash-stack--lines dir "rev-list" "--count" range)) "0")))

;;;; Discovery

(defun magit-dash-stack--first-commit-ref (dir refs)
  "Return the first of REFS that resolves to a commit in DIR, or nil."
  (seq-find (lambda (ref) (magit-dash-stack--ok dir "rev-parse" "--verify" "-q"
                                                (concat ref "^{commit}")))
            refs))

(defun magit-dash-stack--normalize-base (dir base)
  "Return BASE as a plain branch name (no `origin/'), defaulting in DIR."
  (if (and base (not (string-empty-p base)))
      (string-remove-prefix "origin/" base)
    (or (seq-find (lambda (b) (magit-dash-stack--first-commit-ref
                               dir (list (concat "refs/heads/" b))))
                  '("main" "master"))
        "main")))

(defun magit-dash-stack--base-ref (dir base)
  "Return the ref of BASE in DIR, preferring origin's copy, or nil."
  (magit-dash-stack--first-commit-ref
   dir (list (concat "refs/remotes/origin/" base) (concat "refs/heads/" base))))

(defun magit-dash-stack--local-branches (dir)
  "Return an alist (BRANCH . TIP-SHA) of the local branches of DIR."
  (mapcar (lambda (line)
            (let ((parts (split-string line "\t")))
              (cons (string-remove-prefix "refs/heads/" (car parts)) (cadr parts))))
          (magit-dash-stack--lines dir "for-each-ref" "--format=%(refname)%09%(objectname)"
                                   "refs/heads")))

(defun magit-dash-stack--recorded-parents (dir)
  "Return an alist (BRANCH . RECORDED-PARENT) read from git config in DIR."
  (let (out)
    (dolist (line (magit-dash-stack--lines
                   dir "config" "--get-regexp"
                   (concat "^branch\\..*\\." (regexp-quote magit-dash-stack--config-key) "$")))
      (when (string-match (concat "\\`branch\\.\\(.*\\)\\."
                                  (regexp-quote magit-dash-stack--config-key)
                                  " \\(.*\\)\\'")
                          line)
        (push (cons (match-string 1 line) (match-string 2 line)) out)))
    out))

(defun magit-dash-stack--record-parent (dir branch parent)
  "Record PARENT as the parent of BRANCH in the git config of DIR.
This is the only write the stack module makes; it goes through magit's
git wrapper as a plain config assignment."
  (magit-dash-stack--ok dir "config"
                        (format "branch.%s.%s" branch magit-dash-stack--config-key)
                        parent))

(defun magit-dash-stack--ancestry-parent (dir branch branches)
  "Return the nearest branch in BRANCHES that is a proper ancestor of BRANCH.
Use git in DIR.  BRANCHES is an alist (NAME . TIP).  Nil means none: the base."
  (let ((tip (cdr (assoc branch branches)))
        best best-n)
    (dolist (cand branches)
      (unless (or (equal (car cand) branch) (equal (cdr cand) tip))
        (when (magit-dash-stack--ancestor-p dir (cdr cand) tip)
          (let ((n (magit-dash-stack--count dir (format "%s..%s" (cdr cand) tip))))
            (when (or (null best-n) (< n best-n)
                      (and (= n best-n) (string< (car cand) best)))
              (setq best (car cand) best-n n))))))
    best))

(defun magit-dash-stack--parse-commits (lines)
  "Parse `git log --numstat' LINES (see `magit-dash-stack--commits')."
  (let (commits cur)
    (cl-flet ((flush () (when cur (push cur commits))))
      (dolist (line lines)
        (cond
         ((string-prefix-p "\x1e" line)
          (flush)
          (let ((parts (split-string (substring line 1) "\x1f")))
            (setq cur (list :sha (nth 0 parts)
                            :merge (> (length (split-string (or (nth 1 parts) "") " " t)) 1)
                            :subject (or (nth 2 parts) "")
                            :files 0 :added 0 :removed 0 :binary 0))))
         ((and cur (string-match "\\`\\([0-9]+\\|-\\)\t\\([0-9]+\\|-\\)\t" line))
          (let ((a (match-string 1 line)) (r (match-string 2 line)))
            (setq cur (plist-put cur :files (1+ (plist-get cur :files))))
            (if (equal a "-")
                (setq cur (plist-put cur :binary (1+ (plist-get cur :binary))))
              (setq cur (plist-put cur :added (+ (plist-get cur :added) (string-to-number a)))
                    cur (plist-put cur :removed (+ (plist-get cur :removed) (string-to-number r)))))))))
      (flush))
    (nreverse commits)))

(defun magit-dash-stack--commits (dir parent-ref tip)
  "Return the commits in PARENT-REF..TIP in DIR, oldest first, with sizes.
One `git log --numstat' call.  Each commit is a plist (:sha :subject :files
:added :removed :binary :merge).  Binary files count as zero lines and are
tallied in :binary.  A merge commit has no numstat and is a marker row
\(:merge t, all counts zero)."
  (magit-dash-stack--parse-commits
   (magit-dash-stack--lines
    dir "log" "--reverse" "--topo-order" "--numstat"
    "--format=%x1e%H%x1f%P%x1f%s" (format "%s..%s" parent-ref tip))))

(defun magit-dash-stack--order (parents base-children)
  "Return branch names depth-first from BASE-CHILDREN given PARENTS alist."
  (let (out)
    (cl-labels ((walk (b)
                  (push b out)
                  (dolist (k (sort (mapcar #'car (seq-filter (lambda (p) (equal (cdr p) b)) parents))
                                   #'string<))
                    (walk k))))
      (dolist (b (sort (copy-sequence base-children) #'string<)) (walk b)))
    (nreverse out)))

(defun magit-dash-stack--parent-valid-p (parent branch base names)
  "Return non-nil when PARENT can be the parent of BRANCH.
It must be BASE or another of NAMES."
  (and parent (or (equal parent base)
                  (and (member parent names) (not (equal parent branch))))))

(defun magit-dash-stack--select-parent (branch ancestry recorded pr-base base names)
  "Return (PARENT . SOURCE) for BRANCH.
Prefer RECORDED, then PR-BASE, then ANCESTRY; each must be valid against
BASE and NAMES.  SOURCE is `recorded', `pr' or `ancestry'."
  (cond ((magit-dash-stack--parent-valid-p recorded branch base names)
         (cons recorded 'recorded))
        ((magit-dash-stack--parent-valid-p pr-base branch base names)
         (cons pr-base 'pr))
        (t (cons ancestry 'ancestry))))

(defun magit-dash-stack--roots (parents)
  "Return the branch names of PARENTS (an alist) that have no parent."
  (mapcar #'car (seq-filter (lambda (p) (null (cdr p))) parents)))

(defun magit-dash-stack--repair-cycles (parents names)
  "Return (ORDER PARENTS LOST) for the PARENTS alist over NAMES.
Branches unreachable from the base (a recorded-parent cycle) are LOST and
are reattached to the base in the returned PARENTS."
  (let* ((order (magit-dash-stack--order parents (magit-dash-stack--roots parents)))
         (lost (seq-difference names order)))
    (when lost
      (setq parents (mapcar (lambda (p) (if (member (car p) lost) (cons (car p) nil) p))
                            parents)
            order (magit-dash-stack--order parents (magit-dash-stack--roots parents))))
    (list order parents lost)))

(defun magit-dash-stack--build-node (dir branch parent tip base-ref)
  "Return the node plist for BRANCH (TIP) under PARENT in DIR.
Measure against PARENT, or BASE-REF when PARENT is nil.  The caller adds
the :recorded-parent, :observed-parent and :parent-source keys."
  (let* ((pref (if parent (concat "refs/heads/" parent) base-ref))
         (behind (and pref (not (magit-dash-stack--ancestor-p dir pref tip))
                      (magit-dash-stack--count dir (format "%s..%s" tip pref))))
         (commits (and pref (magit-dash-stack--commits dir pref tip))))
    (list :branch branch :parent parent
          :tip tip :worktree (magit-dash-worktree-for-branch dir branch)
          :commits commits :ahead (length commits)
          :drift (and behind (> behind 0) behind))))

(defun magit-dash-stack--tip-problems (branches)
  "Return problems for BRANCHES (alist NAME . TIP) that share a tip."
  (let (seen problems)
    (dolist (b branches)
      (when-let* ((other (rassoc (cdr b) seen)))
        (push (format "Branches %s and %s are at the same commit" (car other) (car b))
              problems))
      (push b seen))
    (nreverse problems)))

(defun magit-dash-stack--node-problems (nodes base)
  "Return problems about the shape of NODES over BASE."
  (let ((bottoms (seq-filter (lambda (n) (null (plist-get n :parent))) nodes))
        problems)
    (when (cdr bottoms)
      (push (format "Several branches sit directly on %s: %s" base
                    (string-join (mapcar (lambda (n) (plist-get n :branch)) bottoms) ", "))
            problems))
    (dolist (n nodes)
      (unless (plist-get n :worktree)
        (push (format "No worktree for %s" (plist-get n :branch)) problems)))
    (nreverse problems)))

;;;###autoload
(defun magit-dash-stack-discover (repo-dir prefix &optional base prs no-record)
  "Discover the stack of branches matching glob PREFIX in REPO-DIR.
BASE is the branch the stack is built on (`origin/main' and `main' are
equivalent; default main or master).  PRS is a list of open PR plists
\(see `magit-dash-chain-fetch-prs') used as a parent source.

The parent of a branch comes, in order, from the recorded parent in git
config, the PR base, and ancestry (the nearest other branch in the set
that is an ancestor).  Missing recorded parents are written unless
NO-RECORD is non-nil.

Return a plist (:repo-dir :base :prefix :nodes :problems).  Nodes are in
depth-first order, each (:branch :parent :recorded-parent :observed-parent
:parent-source :tip :worktree :commits :ahead :drift).  :parent is nil for a
branch directly on the base.  :drift is the number of commits of the
parent (or base) the branch lacks, or nil.  The :branch, :parent and
:worktree keys are the ones `magit-dash-chain' reads."
  (let* ((dir (expand-file-name repo-dir))
         (base (magit-dash-stack--normalize-base dir base))
         (base-ref (magit-dash-stack--base-ref dir base))
         (re (wildcard-to-regexp prefix))
         (branches (seq-filter (lambda (b) (and (string-match-p re (car b))
                                                (not (equal (car b) base))))
                               (magit-dash-stack--local-branches dir)))
         (recorded (magit-dash-stack--recorded-parents dir))
         (names (mapcar #'car branches))
         (problems nil)
         (parents nil)                   ; alist branch -> parent name or nil
         (sources nil)
         (observed nil))
    (unless base-ref
      (push (format "Base branch %s does not resolve locally or on origin" base) problems))
    (unless branches
      (push (format "No local branches match %s" prefix) problems))
    (setq problems (append (reverse (magit-dash-stack--tip-problems branches)) problems))
    (dolist (b names)
      (let* ((anc (magit-dash-stack--ancestry-parent dir b branches))
             (rec (cdr (assoc b recorded)))
             (pr-base (and prs (plist-get (seq-find (lambda (p) (equal (plist-get p :branch) b))
                                                    prs)
                                          :base)))
             (choice (magit-dash-stack--select-parent b anc rec pr-base base names)))
        (when (and rec (not (eq (cdr choice) 'recorded)))
          (push (format "Recorded parent %s of %s is not %s or in the stack; ignored"
                        rec b base)
                problems))
        (push (cons b anc) observed)
        (push (cons b (if (equal (car choice) base) nil (car choice))) parents)
        (push (cons b (cdr choice)) sources)))
    (pcase-let ((`(,order ,fixed ,lost)
                 (magit-dash-stack--repair-cycles (nreverse parents) names)))
      (when lost
        (push (format "Parent cycle among %s; attached to %s" (string-join lost ", ") base)
              problems))
      (let ((nodes (mapcar
                    (lambda (b)
                      (append (magit-dash-stack--build-node
                               dir b (cdr (assoc b fixed)) (cdr (assoc b branches)) base-ref)
                              (list :recorded-parent (cdr (assoc b recorded))
                                    :observed-parent (cdr (assoc b observed))
                                    :parent-source (cdr (assoc b sources)))))
                    order)))
        (setq problems (append (reverse (magit-dash-stack--node-problems nodes base)) problems))
        (unless no-record
          (dolist (n nodes)
            (unless (assoc (plist-get n :branch) recorded)
              (magit-dash-stack--record-parent
               dir (plist-get n :branch) (or (plist-get n :parent) base)))))
        (list :repo-dir dir :base base :prefix prefix :nodes nodes
              :problems (nreverse problems))))))

;;;; Stack queries

(defun magit-dash-stack--node (stack branch)
  "Return the node of BRANCH in STACK."
  (seq-find (lambda (n) (equal (plist-get n :branch) branch)) (plist-get stack :nodes)))

(defun magit-dash-stack--bottoms (stack)
  "Return the nodes of STACK that sit directly on the base."
  (seq-filter (lambda (n) (null (plist-get n :parent))) (plist-get stack :nodes)))

(defun magit-dash-stack--depths (stack)
  "Return a hash table mapping each branch of STACK to its depth.
A bottom is 0.  Nodes are in depth-first order, so one pass suffices."
  (let ((depths (make-hash-table :test #'equal)))
    (dolist (n (plist-get stack :nodes))
      (puthash (plist-get n :branch)
               (if-let* ((p (plist-get n :parent))) (1+ (gethash p depths 0)) 0)
               depths))
    depths))

(defun magit-dash-stack--own-count (node)
  "Return the number of non-merge commits of NODE."
  (length (seq-remove (lambda (c) (plist-get c :merge)) (plist-get node :commits))))

(defun magit-dash-stack--slug (stack)
  "Return a file-name friendly name for STACK from its prefix."
  (let ((s (replace-regexp-in-string
            "\\`-+\\|-+\\'" ""
            (replace-regexp-in-string "[^A-Za-z0-9._]+" "-" (plist-get stack :prefix)))))
    (if (string-empty-p s) "stack" s)))

;;;; Chain adapter

(defun magit-dash-stack--subtree (stack branch)
  "Return the nodes of STACK at and under BRANCH, in stack order."
  (let ((keep (list branch)))
    (seq-filter (lambda (n)
                  (let ((b (plist-get n :branch)))
                    (or (equal b branch)
                        (and (member (plist-get n :parent) keep) (push b keep) t))))
                (plist-get stack :nodes))))

(defun magit-dash-stack-to-chain (stack &optional root)
  "Return STACK (or the subtree under ROOT) in `magit-dash-chain' data form.
Return a plist (:repo-dir :base :root :nodes :problems) whose nodes are
\(:branch :parent) in topological order with no :number.  ROOT is a
branch; it defaults to the only bottom of the stack.  When the stack has
several bottoms and no ROOT is given, the first bottom's subtree is
returned and :problems says so, so validation fails.  A ROOT above the
bottom becomes a chain whose base is its own parent."
  (let* ((bottoms (magit-dash-stack--bottoms stack))
         (root-name (or root (plist-get (car bottoms) :branch)))
         (rnode (and root-name (magit-dash-stack--node stack root-name)))
         (problems nil))
    (when (and root (not rnode))
      (user-error "Branch %s is not in the stack" root))
    (when (and (null root) (cdr bottoms))
      (push (format "Stack has several bottoms (%s); choose one to sync"
                    (string-join (mapcar (lambda (n) (plist-get n :branch)) bottoms) ", "))
            problems))
    (let ((tips (mapcar (lambda (n) (plist-get n :tip))
                        (and rnode (magit-dash-stack--subtree stack root-name)))))
      (when (/= (length tips) (length (delete-dups (copy-sequence tips))))
        (push "Two branches of the stack are at the same commit" problems)))
    (list :repo-dir (plist-get stack :repo-dir)
          :base (or (plist-get rnode :parent) (plist-get stack :base))
          :root root-name
          :nodes (and rnode
                      (mapcar (lambda (n)
                                (list :branch (plist-get n :branch)
                                      :parent (and (not (equal (plist-get n :branch) root-name))
                                                   (plist-get n :parent))))
                              (magit-dash-stack--subtree stack root-name)))
          :problems (nreverse problems))))

;;;; Graph output

(defun magit-dash-stack--commit-word (n)
  "Return \"N commit(s)\"."
  (format "%d commit%s" n (if (= n 1) "" "s")))

(defun magit-dash-stack--short-name (stack branch)
  "Return BRANCH without the literal leading part of STACK's prefix glob."
  (let* ((glob (plist-get stack :prefix))
         (lead (substring glob 0 (or (string-match "[*?[]" glob) (length glob))))
         (rest (string-remove-prefix lead branch)))
    (if (string-empty-p rest) branch rest)))

(defun magit-dash-stack--edge-label (node)
  "Return the \"+N\" edge label for NODE."
  (format "+%d" (magit-dash-stack--own-count node)))

(defun magit-dash-stack--graph-rows (stack)
  "Return one plist (:node :id :parent-id) per node of STACK, for dot and mermaid.
Ids are n0, n1, ...; :parent-id is \"base\" for a branch on the base."
  (let ((ids (make-hash-table :test #'equal))
        (i -1))
    (dolist (n (plist-get stack :nodes))
      (puthash (plist-get n :branch) (format "n%d" (cl-incf i)) ids))
    (mapcar (lambda (n)
              (list :node n
                    :id (gethash (plist-get n :branch) ids)
                    :parent-id (or (and (plist-get n :parent)
                                        (gethash (plist-get n :parent) ids))
                                   "base")))
            (plist-get stack :nodes))))

(defun magit-dash-stack--dot-escape (s)
  "Escape S for a dot double-quoted string."
  (replace-regexp-in-string "\"" "\\\\\"" (replace-regexp-in-string "\\\\" "\\\\\\\\" s)))

(defun magit-dash-stack-dot (stack)
  "Return STACK as a graphviz dot string.
Edges point parent to child, labelled with the child's own commit count;
a drifted edge is red."
  (let ((rows (magit-dash-stack--graph-rows stack))
        (esc #'magit-dash-stack--dot-escape))
    (concat
     (format "// %s stack onto %s. Edges point parent -> child; \"+N\" is the child's own\n"
             (plist-get stack :prefix) (plist-get stack :base))
     "// commits over its parent. Red edges are drifted (the parent has moved on).\n"
     (format "digraph %s {\n" (replace-regexp-in-string "[^A-Za-z0-9_]" "_" (magit-dash-stack--slug stack)))
     "  rankdir=LR;\n"
     "  node [shape=box, fontname=\"monospace\", fontsize=10, style=filled, fillcolor=white];\n"
     "  edge [fontname=\"monospace\", fontsize=9];\n\n"
     (format "  base [label=\"%s\", shape=ellipse];\n" (funcall esc (plist-get stack :base)))
     (mapconcat
      (lambda (row)
        (let ((n (plist-get row :node)))
          (format "  %s [label=\"%s\\n%s\"];\n" (plist-get row :id)
                  (funcall esc (magit-dash-stack--short-name stack (plist-get n :branch)))
                  (magit-dash-stack--commit-word (magit-dash-stack--own-count n)))))
      rows "")
     "\n"
     (mapconcat
      (lambda (row)
        (let ((n (plist-get row :node)))
          (format "  %s -> %s [label=\"%s\"%s];\n"
                  (plist-get row :parent-id) (plist-get row :id)
                  (magit-dash-stack--edge-label n)
                  (if (plist-get n :drift) ", color=red, fontcolor=red" ""))))
      rows "")
     "}\n")))

(defun magit-dash-stack-mermaid (stack)
  "Return STACK as a mermaid flowchart string."
  (let ((rows (magit-dash-stack--graph-rows stack))
        (esc (lambda (s) (replace-regexp-in-string "\"" "#quot;" s))))
    (concat
     "flowchart LR\n"
     (format "  base([\"%s\"])\n" (funcall esc (plist-get stack :base)))
     (mapconcat
      (lambda (row)
        (let ((n (plist-get row :node)))
          (format "  %s[\"%s<br/>%s\"]\n" (plist-get row :id)
                  (funcall esc (magit-dash-stack--short-name stack (plist-get n :branch)))
                  (magit-dash-stack--commit-word (magit-dash-stack--own-count n)))))
      rows "")
     (mapconcat
      (lambda (row)
        (let ((n (plist-get row :node)))
          (format "  %s %s|\"%s%s\"| %s\n"
                  (plist-get row :parent-id)
                  (if (plist-get n :drift) "-.->" "-->")
                  (magit-dash-stack--edge-label n)
                  (if (plist-get n :drift) (format ", behind %d" (plist-get n :drift)) "")
                  (plist-get row :id))))
      rows ""))))

(defun magit-dash-stack--write (text file)
  "Write TEXT to FILE, creating its directory; return FILE."
  (make-directory (file-name-directory (expand-file-name file)) t)
  (with-temp-file file (insert text))
  file)

(defun magit-dash-stack--default-file (stack ext)
  "Return the default output file name for STACK with extension EXT."
  (expand-file-name (format ".wt/%s.%s" (magit-dash-stack--slug stack) ext)
                    (plist-get stack :repo-dir)))

(defun magit-dash-stack--graph-file-args (label ext)
  "Return (STACK FILE) for an interactive graph write; prompt with LABEL.
EXT is the default file extension."
  (let* ((stack (magit-dash-stack--interactive-stack))
         (default (magit-dash-stack--default-file stack ext)))
    (list stack (read-file-name (format "Write %s to: " label) nil default
                                nil (file-name-nondirectory default)))))

(defun magit-dash-stack--write-graph (text file report)
  "Write graph TEXT to FILE, message when REPORT, and return FILE."
  (magit-dash-stack--write text file)
  (when report (message "Wrote %s" file))
  file)

;;;###autoload
(defun magit-dash-stack-write-dot (stack file)
  "Write STACK as a graphviz dot file to FILE and return FILE.
Interactively use the stack of the current buffer (else prompt for one)."
  (interactive (magit-dash-stack--graph-file-args "dot" "dot"))
  (magit-dash-stack--write-graph (magit-dash-stack-dot stack) file
                                (called-interactively-p 'interactive)))

;;;###autoload
(defun magit-dash-stack-write-mermaid (stack file)
  "Write STACK as a mermaid flowchart to FILE and return FILE."
  (interactive (magit-dash-stack--graph-file-args "mermaid" "mmd"))
  (magit-dash-stack--write-graph (magit-dash-stack-mermaid stack) file
                                (called-interactively-p 'interactive)))

;;;###autoload
(defun magit-dash-stack-view-graph (stack)
  "Show STACK's graph: an SVG when `dot' is installed, else the dot text."
  (interactive (list (magit-dash-stack--interactive-stack)))
  (let ((dot (magit-dash-stack-dot stack)))
    (if-let* ((exe (executable-find "dot")))
        (let ((svg (make-temp-file "magit-dash-stack-" nil ".svg")))
          (with-temp-buffer
            (insert dot)
            (unless (zerop (call-process-region (point-min) (point-max) exe t nil nil "-Tsvg" "-o" svg))
              (user-error "The dot command failed")))
          (find-file svg))
      (with-current-buffer (get-buffer-create "*magit-dash-stack-graph*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert dot "\n" (magit-dash-stack-mermaid stack)))
        (goto-char (point-min))
        (pop-to-buffer (current-buffer)))
      (message "graphviz `dot' not found; showing the dot and mermaid text"))))

;;;; Buffer

(defvar-local magit-dash-stack--stack nil "The stack shown in this buffer.")
(defvar-local magit-dash-stack--args nil "(REPO-DIR PREFIX BASE) the buffer was opened with.")

(defvar-keymap magit-dash-stack-mode-map
  :doc "Keymap for `magit-dash-stack-mode'."
  :parent magit-section-mode-map
  "g"   #'magit-dash-stack-refresh
  "RET" #'magit-dash-stack-visit-commit
  "w"   #'magit-dash-stack-visit-worktree
  "l"   #'magit-dash-stack-log
  "d"   #'magit-dash-stack-write-dot
  "m"   #'magit-dash-stack-write-mermaid
  "v"   #'magit-dash-stack-view-graph
  "s"   #'magit-dash-stack-sync)

(define-derived-mode magit-dash-stack-mode magit-section-mode "Stack"
  "Major mode showing a stack of branches with per-commit sizes.

\\{magit-dash-stack-mode-map}"
  :group 'magit-dash-stack
  (setq-local revert-buffer-function (lambda (&rest _) (magit-dash-stack-refresh))))

(defun magit-dash-stack--interactive-stack ()
  "Return the stack of the current buffer, or discover one from prompts."
  (or (and (derived-mode-p 'magit-dash-stack-mode) magit-dash-stack--stack)
      (let* ((repo (or (magit-toplevel) (read-directory-name "Repository: ")))
             (prefix (read-string "Branch glob: "))
             (base (read-string "Base branch (empty for default): ")))
        (magit-dash-stack--discover-with-prs repo prefix (and (not (string-empty-p base)) base)))))

(defun magit-dash-stack--prs (dir)
  "Return open PR plists for the repository in DIR, or nil when unavailable."
  (let* ((default-directory (file-name-as-directory dir))
         (pair (magit-dash-gh--owner-repo-from-url (magit-get "remote" "origin" "url")))
         (slug (and pair (format "%s/%s" (car pair) (cdr pair)))))
    (ignore-errors (magit-dash-chain-fetch-prs slug dir))))

(defun magit-dash-stack--discover-with-prs (repo prefix base)
  "Discover the stack of PREFIX in REPO on BASE, using PR bases when known."
  (magit-dash-stack-discover repo prefix base (magit-dash-stack--prs repo)))

(defun magit-dash-stack--branch-at-point ()
  "Return the branch of the section at point (or its ancestor), or nil."
  (let ((s (magit-current-section)))
    (while (and s (not (eq (oref s type) 'magit-dash-stack-branch)))
      (setq s (oref s parent)))
    (and s (oref s value))))

(defun magit-dash-stack--relative (stack path)
  "Return PATH relative to STACK's repository when beneath it."
  (let ((rel (file-relative-name path (plist-get stack :repo-dir))))
    (if (or (equal rel "..") (string-prefix-p "../" rel)) path rel)))

(defun magit-dash-stack--insert (stack)
  "Insert the sections for STACK."
  (let ((depths (magit-dash-stack--depths stack)))
    (magit-insert-section (magit-dash-stack-root)
			  (magit-insert-heading
			   (format "Stack %s onto %s (%d branches; merge commits shown without size)"
				   (plist-get stack :prefix) (plist-get stack :base)
				   (length (plist-get stack :nodes))))
			  (dolist (p (plist-get stack :problems))
			    (insert (propertize (concat "  ! " p "\n") 'font-lock-face 'warning)))
			  (insert "\n")
			  (dolist (n (plist-get stack :nodes))
			    (let ((indent (make-string (* 2 (gethash (plist-get n :branch) depths 0)) ?\s))
				  (wt (plist-get n :worktree)))
			      (magit-insert-section (magit-dash-stack-branch (plist-get n :branch))
						    (magit-insert-heading
						     (concat indent
							     (propertize (plist-get n :branch) 'font-lock-face 'magit-branch-local)
							     (if wt (concat "  " (magit-dash-stack--relative stack wt)) "  (no worktree)")
							     (if (plist-get n :drift)
								 (propertize (format "  DRIFT (behind %s by %d)"
										     (or (plist-get n :parent) (plist-get stack :base))
										     (plist-get n :drift))
									     'font-lock-face 'error)
							       "")
							     (if (and (plist-get n :recorded-parent)
								      (not (equal (or (plist-get n :recorded-parent) (plist-get stack :base))
										  (or (plist-get n :observed-parent) (plist-get stack :base)))))
								 (format "  [recorded %s, observed %s]" (plist-get n :recorded-parent)
									 (or (plist-get n :observed-parent) (plist-get stack :base)))
							       "")))
						    (dolist (c (plist-get n :commits))
						      (magit-insert-section (magit-dash-stack-commit (plist-get c :sha))
									    (insert indent "  "
										    (propertize (substring (plist-get c :sha) 0 10) 'font-lock-face 'magit-hash)
										    (if (plist-get c :merge)
											(format "  %-18s" "(merge)")
										      (format "  %-6s %-6s %3df%s  "
											      (format "+%d" (plist-get c :added))
											      (format "-%d" (plist-get c :removed))
											      (plist-get c :files)
											      (if (> (plist-get c :binary) 0)
												  (format " %db" (plist-get c :binary)) "")))
										    (plist-get c :subject) "\n")))))))))

(defun magit-dash-stack--render ()
  "Redraw the current stack buffer from `magit-dash-stack--stack'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-dash-stack--insert magit-dash-stack--stack)
    (goto-char (point-min))))

;;;###autoload
(defun magit-dash-stack-open (repo-dir prefix &optional base)
  "Show the stack of branches matching glob PREFIX in REPO-DIR in a buffer.
BASE is the branch the stack is built on.  Return the buffer."
  (interactive
   (let* ((repo (or (magit-toplevel) (read-directory-name "Repository: ")))
          (prefix (read-string "Branch glob (e.g. user/FND-1340-*): "))
          (base (read-string "Base branch (empty for default): ")))
     (list repo prefix (and (not (string-empty-p base)) base))))
  (let* ((stack (magit-dash-stack--discover-with-prs repo-dir prefix base))
         (buf (get-buffer-create (format "*magit-dash-stack: %s*" (string-trim prefix "\\**" "\\**")))))
    (with-current-buffer buf
      (magit-dash-stack-mode)
      (setq default-directory (file-name-as-directory (plist-get stack :repo-dir))
            magit-dash-stack--args (list repo-dir prefix base)
            magit-dash-stack--stack stack)
      (magit-dash-stack--render))
    (when (called-interactively-p 'interactive) (pop-to-buffer buf))
    buf))

(defun magit-dash-stack-refresh ()
  "Rediscover the stack and redraw the buffer."
  (interactive)
  (unless magit-dash-stack--args (user-error "Not in a stack buffer"))
  (let ((line (line-number-at-pos)))
    (setq magit-dash-stack--stack
          (apply #'magit-dash-stack--discover-with-prs magit-dash-stack--args))
    (magit-dash-stack--render)
    (forward-line (1- line))))

(defun magit-dash-stack-visit-commit ()
  "Show the commit at point."
  (interactive)
  (let ((s (magit-current-section)))
    (if (and s (eq (oref s type) 'magit-dash-stack-commit))
        (magit-show-commit (oref s value))
      (user-error "No commit at point"))))

(defun magit-dash-stack-visit-worktree ()
  "Visit the worktree of the branch at point in Dired."
  (interactive)
  (let* ((b (or (magit-dash-stack--branch-at-point) (user-error "No branch at point")))
         (wt (plist-get (magit-dash-stack--node magit-dash-stack--stack b) :worktree)))
    (if (and wt (file-directory-p wt))
        (dired wt)
      (user-error "Branch %s has no worktree" b))))

(defun magit-dash-stack-log ()
  "Open a decorated magit log of the whole stack."
  (interactive)
  (let ((stack (or magit-dash-stack--stack (user-error "Not in a stack buffer"))))
    (magit-log-other (cons (plist-get stack :base)
                           (mapcar (lambda (n) (plist-get n :branch)) (plist-get stack :nodes)))
                     '("-D" "--graph" "--decorate"))))

;;;; Sync

;;;###autoload
(defun magit-dash-stack-sync (stack &optional root fetch)
  "Bring STACK up to date by merging each parent forward, through chain.
Convert STACK to a `magit-dash-chain' (the subtree under branch ROOT when
the stack has several bottoms), validate it, create the plan with origin
`stack', and open it in the hitl review buffer; nothing runs until it is
executed there.  With FETCH (interactively, on confirmation) run
`git fetch origin' first so the base is current.  Never rebases and never
pushes.  Return the plan."
  (interactive
   (let* ((stack (magit-dash-stack--interactive-stack))
          (bottoms (magit-dash-stack--bottoms stack))
          (root (and (cdr bottoms)
                     (completing-read "Sync the subtree under: "
                                      (mapcar (lambda (n) (plist-get n :branch)) bottoms)
                                      nil t)))
          (fetch (y-or-n-p "Run `git fetch origin' first? ")))
     (list stack root fetch)))
  (when (null (cdr (plist-get stack :nodes)))
    (user-error "Nothing to sync: the stack has %d branch%s"
                (length (plist-get stack :nodes))
                (if (= 1 (length (plist-get stack :nodes))) "" "es")))
  (when fetch
    (unless (magit-dash-stack--ok (plist-get stack :repo-dir) "fetch" "origin")
      (user-error "Fetching origin failed"))
    ;; the fetch moved remote-tracking refs: rebuild from current state
    (setq stack (magit-dash-stack--discover-with-prs
                 (plist-get stack :repo-dir) (plist-get stack :prefix)
                 (plist-get stack :base))))
  (let* ((chain (magit-dash-chain-validate (magit-dash-stack-to-chain stack root)))
         (errors (plist-get chain :errors)))
    (when errors
      (user-error "Stack cannot be synced: %s" (string-join errors "; ")))
    (let ((plan (magit-dash-chain-create-plan chain :origin 'stack)))
      (require 'hitl-plan-ui)
      (hitl-plan-review plan)
      plan)))

(provide 'magit-dash-stack)

;;; magit-dash-stack.el ends here
