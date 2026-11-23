;;; diff-review.el --- Review a git diff from a review file -*- lexical-binding: t; -*-

;; Author: Xiaoxing Hu <hi@xiaoxing.dev>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit-section "4.0"))
;; Keywords: vc, tools
;; URL: https://github.com/xiaoxinghu/diff-review

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A review is a file.  Visiting it shows an overview of what a worktree
;; changed since the merge-base with a base revision, the way a pull
;; request page does: the counts, then the changed files as a foldable
;; tree.  Picking a file shows the real file in its own major mode, so
;; every editing command, `xref' and the language server work.  A review
;; with a head revision compares two commits instead; its files are the
;; head's, in a worktree that has the head checked out, else in a
;; throwaway worktree of it under the git directory, read-only, so they
;; are real files in a real tree too.  Overlays add what a diff shows:
;; removed lines inline above where they were, a tint on added lines, and
;; a focus mode that folds the unchanged lines.
;;
;; A sidebar shows the same tree beside the window the review shows in,
;; as its atomic partner: the two split, move and close as one window, so
;; the review can sit in any split the user likes and the tree stays next
;; to it.  Files are kept in the tree's order, directories first, so
;; walking them follows the tree.  The sidebar appears with any window
;; showing the overview or one of its files, goes when the window shows
;; anything else, and `diff-review-toggle-sidebar' turns it off for a
;; review altogether.
;;
;; The review file is JSON and starts with a `format' key, which is how
;; `diff-review-mode' is chosen, whatever the file is called or wherever
;; it lives.  It records what git cannot derive: the base, the viewed
;; marks with the blob hash each file had when it was ticked, so a mark
;; clears when the file changes again.  Everything else comes from git
;; when the file is visited.  Whoever creates the file decides where it
;; goes, see `diff-review-find-file'; `diff-review-branch' puts the review
;; of a branch under the repository's common git directory, where every
;; worktree finds it, and names the branch as the head, so one review
;; follows the branch through its commits.
;;
;;   {"format": "diff-review/1",
;;    "root": "/path/to/worktree/",   optional: the file's repository
;;    "base": "main",                  optional: `diff-review-default-base'
;;    "head": "abc1234",               optional: the worktree
;;    "viewed": {"src/a.ts": "<blob hash>"},
;;    "comments": [...],               optional: see below
;;    "verdict": {"state": "approved", "head": "<full hash>",
;;                "by": "claude", "at": "..."}}   optional: see below
;;
;; Comments are threads on some lines of a file, as on a pull request
;; page, kept in the same file so an agent reads and writes one thing.
;; A thread's first message carries the location; replies carry none.
;; Lines count in the file content "blob" names, so a thread follows its
;; lines when the file changes, through `git diff' of the two blobs, or
;; says it is outdated when they changed.  Both "path" and "lines" absent
;; is a comment on the review as a whole.  Keys nothing here knows are
;; kept as they are, in a comment too.
;;
;;   {"id": "k3f9a1",                 any string unique among the comments
;;    "path": "src/a.ts",             relative to the root, any file under it
;;    "lines": [12, 15],              first and last, inclusive
;;    "blob": "<blob hash>",          optional: the file as it is now
;;    "author": "xiaoxing",
;;    "at": "2026-11-06T12:40:11+13:00",
;;    "body": "Retries forever when the socket is gone.",
;;    "replies": [{"author": "claude", "at": "...", "body": "Capped in 3b1c0e2."}],
;;    "resolved": true}               optional
;;
;; The verdict is the reviewer's last word, "changes" or "approved", on the
;; commit "head" names.  It says whose turn the review is without reading
;; the threads: the author's after changes, the merge's once approved, the
;; reviewer's when there is none or the head has moved past it.
;;
;; The review buffer is an ordinary buffer: hide it, show it, kill it to
;; end the review.  Files keep a pointer back to it.  The files it shows
;; in its window are not recorded in the window's history, so ending the
;; review returns the window to what it showed before.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'diff-mode)
(require 'magit-section)

(defgroup diff-review nil
  "Review a git diff from a review file."
  :group 'tools
  :prefix "diff-review-")

(defconst diff-review-version "0.1.0"
  "The version of diff-review, as in the Version header.")

(defcustom diff-review-default-base #'diff-review-default-branch
  "The base of a new review: a revision, or a function returning one.
The function is called with `default-directory' at the repository's
root.  A review file records the base it got, so changing this does
not move an existing review."
  :type '(choice (string :tag "Revision")
                 (function :tag "Function returning a revision")))

(defcustom diff-review-context 3
  "Unchanged lines kept around each hunk in focus mode."
  :type 'integer)

(defcustom diff-review-inline-removed t
  "Whether to show removed lines inline, above where they were."
  :type 'boolean)

(defcustom diff-review-file-header t
  "The header line of a file under review.
t shows the review's header, nil leaves the buffer's header line alone,
and a function is called in the file's buffer for the string to show.
The header line the buffer had comes back when the review ends."
  :type '(choice (const :tag "The review's header" t)
                 (const :tag "Leave the header line alone" nil)
                 (function :tag "A function returning the header")))

(defcustom diff-review-file-header-keys t
  "Whether the review's file header ends with hints for its keys."
  :type 'boolean)

(defcustom diff-review-sidebar t
  "Whether a review starts with its sidebar shown."
  :type 'boolean)

(defcustom diff-review-sidebar-width 36
  "Width of the sidebar, in columns, when a review starts.
Resizing the sidebar changes its width for the rest of that review."
  :type 'integer)

(defcustom diff-review-sidebar-side 'left
  "Which side of the review's window the sidebar takes."
  :type '(choice (const left) (const right)))

(defcustom diff-review-tint-added t
  "Whether to tint added lines with `diff-review-added'.
Without the tint they keep the fringe bar, which a terminal has not."
  :type 'boolean)

(defcustom diff-review-tint-removed t
  "Whether to tint the removed lines shown inline with `diff-review-removed'.
Without the tint they keep the fringe bar, which a terminal has not."
  :type 'boolean)

(defcustom diff-review-fringe 'left
  "The fringe the hunk bars are drawn in, or nil for no bars."
  :type '(choice (const left) (const right) (const :tag "No bars" nil)))

(defcustom diff-review-hunk-recenter 6
  "The window line a hunk is shown on when moved to, or nil to center it."
  :type '(choice (integer :tag "Line from the top") (const :tag "Center" nil)))

(defcustom diff-review-author nil
  "The name your comments carry, or nil for git's `user.name', else your login."
  :type '(choice (const :tag "Git's user.name" nil) string))

(defface diff-review-added '((t :extend t))
  "Face for added lines, over the syntax faces.
Without a background of its own, the background of `diff-added' is used.")
(defface diff-review-removed '((t :extend t))
  "Face for removed lines, shown inline, over the syntax faces.
Without a background of its own, the background of `diff-removed' is used.")
(defface diff-review-title '((t :inherit bold :height 1.2))
  "Face for the overview's title, the head and the base.")
(defface diff-review-file-added '((t :inherit (bold success)))
  "Face for the status letter of an added file.")
(defface diff-review-file-deleted '((t :inherit (bold error)))
  "Face for the status letter of a deleted file.")
(defface diff-review-file-changed '((t :inherit (bold warning)))
  "Face for the status letter of a modified, renamed or copied file.")
(defface diff-review-unchanged '((t :inherit shadow))
  "Face for the line that stands for folded unchanged lines.")
(defface diff-review-comment '((t :inherit (font-lock-comment-face highlight) :extend t))
  "Face for a comment thread, drawn under the lines it is on.
A review comment is a comment, so it takes the file's comment colour;
the background is what says it is not the file's own.")
(defface diff-review-comment-range '((t :extend t))
  "Face for the lines of an open comment thread, over the syntax faces.
Without a background of its own, it takes `diff-review-comment's.")
(defface diff-review-comment-author '((t :inherit bold))
  "Face for the author of a comment.")

(defconst diff-review-format "diff-review/1"
  "The value of the format key a review file starts with.")

;;;; State

;; In the review buffer.
(defvar-local diff-review--data nil "The review file as parsed: a hash table, written back as is.")
(defvar-local diff-review--root nil "Repository root, with a trailing slash.")
(defvar-local diff-review--base nil "Full SHA the review compares against.")
(defvar-local diff-review--base-name nil "The revision the file names.")
(defvar-local diff-review--head nil "The revision under review, or nil for the worktree.")
(defvar-local diff-review--head-name nil "The branch under review, or the head revision.")
(defvar-local diff-review--tip nil "Full SHA the head named at the last scan, or nil for the worktree.")
(defvar-local diff-review--moved nil "Full SHA the head has moved to since the last scan, or nil.")
(defvar-local diff-review--worktree nil "The throwaway worktree of the tip, once added.")
(defvar-local diff-review--superseded nil
  "The throwaway worktree of the tip before, until a rescan shows the new files.")
(defvar-local diff-review--blobs nil "Alist of a revision buffer's name to the buffer, this review's.")
(defvar-local diff-review--files nil "Vector of file plists, in tree order.")
(defvar-local diff-review--index nil "Index of the file shown, or nil for none.")
(defvar-local diff-review--viewed nil "Hash table of path to the blob hash it was viewed at.")
(defvar-local diff-review--comments nil "Vector of the comments, hash tables as parsed.")
(defvar-local diff-review--sidebar nil "The sidebar buffer, once created.")
(defvar-local diff-review--sidebar-enabled nil "Whether this review shows its sidebar.")
(defvar-local diff-review--sidebar-width nil "The width of this review's sidebar, in columns.")
(defvar-local diff-review--inline-removed nil "Whether this review shows removed lines inline.")
(defvar-local diff-review--threads-open nil "Whether this review draws every thread open.")

;; In a file buffer or the sidebar.
(defvar-local diff-review--review nil "The review buffer this buffer belongs to.")
(defvar-local diff-review--file-path nil "In a file buffer, the path of its file in the review.")
(defvar-local diff-review--hunks nil
  "In a file buffer, its hunks: plists with :line, :count and :removed.")
(defvar-local diff-review--overlays nil "In a file buffer, its review overlays.")
(defvar-local diff-review--texts nil "In a file buffer, its removed lines fontified, once.")
(defvar-local diff-review--focus nil "In a file buffer, whether unchanged lines are folded.")
(defvar-local diff-review--deleted nil "In a buffer showing a deleted file's base version, t.")
(defvar-local diff-review--blob nil "In a buffer showing a revision of a file, what identifies it.")
(defvar-local diff-review--saved nil
  "In a file buffer, the variables the review set: (VAR LOCAL . VALUE) from before.")
(defvar diff-review-file-mode)

(defun diff-review--review-buffer ()
  "The review buffer this buffer belongs to, or nil."
  (cond ((derived-mode-p 'diff-review-mode) (current-buffer))
        ((buffer-live-p diff-review--review) diff-review--review)))

(defmacro diff-review--in-review (&rest body)
  "Run BODY in this buffer's review buffer."
  (declare (indent 0))
  `(with-current-buffer (or (diff-review--review-buffer)
                            (user-error "This buffer's review is closed"))
     ,@body))

;;;; Git

(defun diff-review--git-at (directory &rest args)
  "Run git with ARGS in DIRECTORY and return its output without trailing newlines."
  (let ((default-directory directory))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (error "Git %s: %s" (string-join args " ") (string-trim (buffer-string))))
      (string-trim-right (buffer-string) "\n+"))))

(defun diff-review--git (&rest args)
  "Run git with ARGS at the root and return its output without trailing newlines."
  (apply #'diff-review--git-at diff-review--root args))

(defun diff-review--git-maybe (&rest args)
  "Run git with ARGS in `default-directory': its output, or nil when it fails."
  (with-temp-buffer
    (and (zerop (apply #'process-file "git" nil '(t nil) nil args))
         (string-trim (buffer-string)))))

(defun diff-review--toplevel (directory)
  "The root of the repository holding DIRECTORY, or nil."
  (let ((default-directory directory))
    (when-let* ((top (diff-review--git-maybe "rev-parse" "--show-toplevel")))
      (file-name-as-directory top))))

(defun diff-review--common-dir (root)
  "The git directory the worktrees of the repository at ROOT share."
  (file-name-as-directory
   (file-truename
    (expand-file-name (diff-review--git-at root "rev-parse" "--git-common-dir") root))))

(defun diff-review--worktrees (root)
  "The worktrees of the repository at ROOT, the main one first.
Plists with :path, :head, :branch and :locked, the lock's reason; :branch
is nil at a detached HEAD, and a bare one has no :head."
  (mapcar (lambda (record)
            (let (plist)
              (dolist (line (split-string record "\n" t))
                (pcase (and (string-match "\\`\\([A-Za-z]+\\)\\(?: \\(.*\\)\\)?\\'" line)
                            (list (match-string 1 line) (match-string 2 line)))
                  (`("worktree" ,path)
                   (setq plist (plist-put plist :path (file-name-as-directory path))))
                  (`("HEAD" ,sha) (setq plist (plist-put plist :head sha)))
                  (`("branch" ,ref)
                   (setq plist (plist-put plist :branch (string-remove-prefix "refs/heads/" ref))))
                  (`("locked" ,reason) (setq plist (plist-put plist :locked (or reason ""))))))
              plist))
          (split-string (diff-review--git-at root "worktree" "list" "--porcelain") "\n\n" t)))

(defun diff-review--main-worktree (directory)
  "The main worktree of the repository DIRECTORY belongs to, or nil.
DIRECTORY may be inside the git directory, where a branch's review lives."
  (plist-get (car (ignore-errors (diff-review--worktrees directory))) :path))

(defun diff-review-default-branch ()
  "The default branch of the repository at `default-directory'.
What origin/HEAD points at, else the first branch of `init.defaultBranch',
main and master that exists, else main."
  (or (when-let* ((ref (diff-review--git-maybe
                        "symbolic-ref" "--short" "refs/remotes/origin/HEAD")))
        (string-remove-prefix "origin/" ref))
      (seq-find (lambda (branch)
                  (diff-review--git-maybe "rev-parse" "--verify" "--quiet"
                                          (concat "refs/heads/" branch)))
                (delq nil (list (diff-review--git-maybe "config" "init.defaultBranch")
                                "main" "master")))
      "main"))

(defun diff-review--default-base (root)
  "The base of a new review of the repository at ROOT."
  (if (functionp diff-review-default-base)
      (let ((default-directory root))
        (funcall diff-review-default-base))
    diff-review-default-base))

(defun diff-review--parse-name-status (output)
  "Parse the NUL-separated OUTPUT of `git diff --name-status -M -z'.
Return a list of (PATH STATUS OLD-PATH)."
  (let ((fields (split-string output "\0" t)) result)
    (while fields
      (let ((status (substring (pop fields) 0 1)))
        (if (member status '("R" "C"))
            (let ((old (pop fields)) (new (pop fields)))
              (push (list new status old) result))
          (push (list (pop fields) status nil) result))))
    (nreverse result)))

(defun diff-review--parse-numstat (output)
  "Parse the NUL-separated OUTPUT of `git diff --numstat -M -z'.
Return a hash table of path to (ADDED . REMOVED)."
  (let ((fields (split-string output "\0" t))
        (table (make-hash-table :test #'equal)))
    (while fields
      (pcase-let ((`(,added ,removed ,path) (split-string (pop fields) "\t")))
        (when (string-empty-p path)   ; a rename: old and new follow
          (pop fields)
          (setq path (pop fields)))
        (puthash path (cons (string-to-number added) (string-to-number removed))
                 table)))
    table))

(defun diff-review--tree-less-p (a b)
  "Non-nil when path A comes before path B in a tree, directories first."
  (let ((as (split-string a "/")) (bs (split-string b "/")))
    (while (and (cdr as) (cdr bs) (equal (car as) (car bs)))
      (pop as)
      (pop bs))
    (cond ((and (cdr as) (not (cdr bs))) t)
          ((and (cdr bs) (not (cdr as))) nil)
          (t (string< (car as) (car bs))))))

(defun diff-review--range ()
  "The revisions to give `git diff': the base, and the head's tip when set.
The tip as scanned, not the branch: the files shown are the tip's, and
the branch may have moved on since."
  (delq nil (list diff-review--base diff-review--tip)))

(defun diff-review--hashes (paths &optional rev)
  "The blob hashes of the files at PATHS, at REV or the tip, in one git call."
  (setq rev (or rev diff-review--tip))
  (cond
   ((null paths) nil)
   (rev
    (split-string (apply #'diff-review--git "rev-parse"
                         (mapcar (lambda (p) (concat rev ":" p)) paths))
                  "\n" t))
   (t
    (let ((default-directory diff-review--root))
      (with-temp-buffer
        (insert (string-join paths "\n") "\n")
        (unless (zerop (call-process-region (point-min) (point-max) "git" t t nil
                                            "hash-object" "--stdin-paths"))
          (error "Git hash-object: %s" (string-trim (buffer-string))))
        (split-string (buffer-string) "\n" t))))))

(defun diff-review--scan ()
  "Fill the file vector from git: a handful of calls for any number of files."
  (setq diff-review--head-name (or diff-review--head
                                   (diff-review--git "rev-parse" "--abbrev-ref" "HEAD"))
        diff-review--base
        (condition-case err
            (diff-review--git "merge-base" diff-review--base-name
                              (or diff-review--head "HEAD"))
          (error (user-error "No merge base with base %S: set \"base\" in the review \
file, or `diff-review-default-base' for new reviews (%s)"
                             diff-review--base-name (error-message-string err)))))
  (diff-review--follow-tip)
  (let* ((shown (and diff-review--index   ; keeps its place by path: the index may move
                     (< diff-review--index (length diff-review--files))
                     (plist-get (diff-review--file) :path)))
         (changed (diff-review--parse-name-status
                   (apply #'diff-review--git "diff" "--name-status" "-M" "-z"
                          (diff-review--range))))
         ;; A commented file the diff has not is in the review too, unchanged.
         (commented (diff-review--commented-paths (mapcar #'car changed)))
         (entries (sort (append changed (mapcar (lambda (e) (list (car e) " " nil)) commented))
                        (lambda (a b) (diff-review--tree-less-p (car a) (car b)))))
         (numstat (diff-review--parse-numstat
                   (apply #'diff-review--git "diff" "--numstat" "-M" "-z"
                          (diff-review--range))))
         (deleted (mapcar #'car (seq-filter (lambda (e) (equal (nth 1 e) "D")) changed)))
         (present (mapcar #'car (seq-remove (lambda (e) (equal (nth 1 e) "D")) changed)))
         ;; A deleted file's blob is its base one: every file has the blob it shows.
         (hashes (append (cl-mapcar #'cons present (diff-review--hashes present))
                         (cl-mapcar #'cons deleted
                                    (diff-review--hashes deleted diff-review--base))
                         commented)))
    (setq diff-review--files
          (vconcat
           (mapcar (lambda (entry)
                     (pcase-let ((`(,path ,status ,old) entry))
                       (let ((stat (gethash path numstat)))
                         (list :path path :status status :old old
                               :added (or (car stat) 0) :removed (or (cdr stat) 0)
                               :hash (cdr (assoc path hashes))))))
                   entries)))
    (setq diff-review--index (and shown (diff-review--file-index shown)))))

(defun diff-review--follow-tip ()
  "Note the commit the head names now.
A review of a branch follows the branch: its files are the tip's.  A
throwaway worktree is of one commit, so a moved tip supersedes it: a new
one is added when a file needs it, and this one goes once the rescan has
given its windows the new files, see `diff-review--load'."
  (let ((tip (and diff-review--head
                  (diff-review--git "rev-parse" "--verify"
                                    (concat diff-review--head "^{commit}")))))
    (when (and diff-review--worktree (not (equal tip diff-review--tip)))
      (setq diff-review--superseded diff-review--worktree
            diff-review--worktree nil))
    (setq diff-review--tip tip
          diff-review--moved nil)))

(defun diff-review--commented-paths (listed)
  "The commented paths not among LISTED that the head, or the worktree, has.
As (PATH . HASH); a path git has not is left to the overview's list."
  (delq nil
        (mapcar (lambda (path)
                  (and (not (member path listed))
                       (when-let* ((hash (ignore-errors
                                           (car (diff-review--hashes (list path))))))
                         (cons path hash))))
                (seq-uniq (delq nil (mapcar (lambda (c) (gethash "path" c))
                                            diff-review--comments))))))

(defun diff-review--file (&optional index)
  "The file plist at INDEX, by default the one shown, or nil."
  (and diff-review--files
       (aref diff-review--files (or index diff-review--index))))

;;;; The file

(defun diff-review--read ()
  "Parse the review file in the current buffer into the buffer's state."
  (let* ((data (condition-case err
                   (json-parse-string (buffer-string) :null-object nil :false-object nil)
                 (error (user-error "Not a review file: %s" (error-message-string err)))))
         (root (gethash "root" data))
         (file-root (and buffer-file-name
                         (diff-review--toplevel (file-name-directory buffer-file-name)))))
    (unless (equal (gethash "format" data) diff-review-format)
      (user-error "Not a review file: format is %S" (gethash "format" data)))
    ;; A root that is gone, a removed worktree's, gives way to the main one.
    (setq diff-review--root
          (cond ((and root (file-directory-p (expand-file-name root)))
                 (file-name-as-directory (expand-file-name root)))
                (file-root)
                ((and buffer-file-name
                      (diff-review--main-worktree (file-name-directory buffer-file-name))))
                (t (user-error "The review names no root that exists and is not in a repository"))))
    (puthash "root" diff-review--root data)
    (puthash "base" (or (gethash "base" data) (diff-review--default-base diff-review--root))
             data)
    (unless (hash-table-p (gethash "viewed" data))
      (puthash "viewed" (make-hash-table :test #'equal) data))
    (setq diff-review--data data
          diff-review--base-name (gethash "base" data)
          diff-review--head (gethash "head" data)
          diff-review--viewed (gethash "viewed" data)
          diff-review--comments (let ((comments (gethash "comments" data)))
                                  (if (vectorp comments) comments [])))))

(defun diff-review--write ()
  "Write the buffer's state to its file, leaving the buffer unmodified.
Returns t, as `write-contents-functions' expects."
  (let ((file buffer-file-name)
        ;; Parsed null and false are nil: written back as null, not as {}.
        (json (json-serialize diff-review--data :null-object nil)))   ; here: no data in the temp buffer
    (with-temp-buffer
      (insert json)
      (json-pretty-print-buffer)
      (insert "\n")
      (write-region (point-min) (point-max) file nil 'quiet)))
  (set-visited-file-modtime)
  (set-buffer-modified-p nil)
  t)

;;;; Viewed marks

(defun diff-review--viewed-p (file)
  "Non-nil when FILE was marked viewed at its current contents."
  (equal (gethash (plist-get file :path) diff-review--viewed) (plist-get file :hash)))

(defun diff-review--set-viewed (file flag)
  "Mark FILE viewed at its current contents when FLAG, else unmark it, and write."
  (unless (verify-visited-file-modtime)   ; someone else wrote it: take theirs first
    (diff-review--revert))
  (if flag
      (puthash (plist-get file :path) (plist-get file :hash) diff-review--viewed)
    (remhash (plist-get file :path) diff-review--viewed))
  (diff-review--write))

;;;; Comments

;; A comment is the hash table the file holds, read and written as is.

(defun diff-review--author ()
  "The name a comment written here carries.  The review buffer must be current."
  (or diff-review-author
      (let ((default-directory diff-review--root))
        (diff-review--git-maybe "config" "user.name"))
      user-login-name))

(defun diff-review--comment-id ()
  "A short id no comment of the review has."
  (cl-loop for id = (format "%06x" (random #x1000000))
           unless (diff-review--comment id) return id))

(defun diff-review--comment (id)
  "The comment with ID, or nil."
  (seq-find (lambda (c) (equal (gethash "id" c) id)) diff-review--comments))

(defun diff-review--message (author body &optional table)
  "Put AUTHOR, now and BODY into TABLE, a new hash table by default, and return it."
  (let ((m (or table (make-hash-table :test #'equal))))
    (puthash "author" author m)
    (puthash "at" (format-time-string "%FT%T%:z") m)
    (puthash "body" body m)
    m))

(defun diff-review--comment-lines (comment)
  "COMMENT's lines as (FIRST . LAST), or nil for a comment on the review."
  (let ((lines (gethash "lines" comment)))
    (and (vectorp lines) (= (length lines) 2)
         (cons (aref lines 0) (aref lines 1)))))

(defun diff-review--first-line (comment)
  "The first line of COMMENT's body."
  (car (split-string (or (gethash "body" comment) "") "\n")))

(defun diff-review--file-comments (path)
  "The comments on PATH, in the review's order."
  (seq-filter (lambda (c) (equal (gethash "path" c) path)) diff-review--comments))

(defun diff-review--open-count (comments)
  "How many of COMMENTS are not resolved."
  (seq-count (lambda (c) (not (gethash "resolved" c))) comments))

(defun diff-review--file-index (path)
  "The index of PATH among the review's files, or nil."
  (cl-position path diff-review--files :test (lambda (p f) (equal p (plist-get f :path)))))

(defun diff-review--modify-comments (fn)
  "Call FN to change the comments, then write and redraw.
Someone else's write since the last read is taken first, so an agent
appending beside Emacs loses nothing.  The review buffer must be current."
  (unless (verify-visited-file-modtime)
    (diff-review--revert))
  (funcall fn)
  (puthash "comments" diff-review--comments diff-review--data)
  (diff-review--write)
  (unless (seq-every-p (lambda (c) (or (null (gethash "path" c))   ; a file new to the review
                                       (diff-review--file-index (gethash "path" c))))
                       diff-review--comments)
    (diff-review--scan))
  (diff-review--render-all)
  (diff-review--redecorate-files))

(defun diff-review--add-comment (path lines blob body)
  "Add a thread of BODY on LINES of PATH as BLOB has it, and return it.
Without PATH it is on the review as a whole.  The review buffer must be current."
  (let ((c (make-hash-table :test #'equal)))
    (puthash "id" (diff-review--comment-id) c)
    (when path
      (puthash "path" path c)
      (puthash "lines" (vector (car lines) (cdr lines)) c)
      (when blob (puthash "blob" blob c)))
    (diff-review--message (diff-review--author) body c)
    (puthash "replies" [] c)
    (diff-review--modify-comments
     (lambda () (setq diff-review--comments (vconcat diff-review--comments (vector c)))))
    c))

;;;; The verdict

(defun diff-review--turn ()
  "Whose turn the review is: `reviewer', `author' or `merge'.
A verdict is on the commit it names, so a tip past it, or a verdict
this does not know, leaves the turn the reviewer's.  The tip is the
branch's, moved or not.  The review buffer must be current."
  (let ((verdict (gethash "verdict" diff-review--data)))
    (if (not (and (hash-table-p verdict)
                  (equal (gethash "head" verdict) (or diff-review--moved diff-review--tip))))
        'reviewer
      (pcase (gethash "state" verdict)
        ("changes" 'author)
        ("approved" 'merge)
        (_ 'reviewer)))))

(defun diff-review--verdict-text ()
  "The verdict as the summaries show it, or an empty string.
The review buffer must be current."
  (let* ((verdict (gethash "verdict" diff-review--data))
         (state (and (hash-table-p verdict) (gethash "state" verdict)))
         (label (pcase state ("changes" "changes requested") ("approved" "approved"))))
    (cond ((null label) "")
          ((eq (diff-review--turn) 'reviewer)
           (propertize (concat label ", stale") 'face 'shadow))
          (t (propertize label 'face (if (equal state "approved") 'success 'warning))))))

(defun diff-review--set-verdict (state)
  "Write the verdict STATE on the tip the review shows, and redraw.
The review buffer must be current."
  (let ((tip (or diff-review--tip
                 (user-error "A verdict is on a commit: this review is of the worktree"))))
    (unless (verify-visited-file-modtime) ; someone else wrote it: take theirs first
      (diff-review--revert))
    ;; The same table: keys this does not know stay, as everywhere in the file.
    (let ((verdict (gethash "verdict" diff-review--data)))
      (unless (hash-table-p verdict)
        (setq verdict (make-hash-table :test #'equal)))
      (puthash "state" state verdict)
      (puthash "head" tip verdict)
      (puthash "by" (diff-review--author) verdict)
      (puthash "at" (format-time-string "%FT%T%:z") verdict)
      (puthash "verdict" verdict diff-review--data))
    (diff-review--write)
    (diff-review--render-all)))

(defun diff-review--hunk-field (n)
  "Group N of the hunk header just matched, as a number; 1 when it is absent."
  (if (match-string n) (string-to-number (match-string n)) 1))

(defun diff-review--hunks-between (old new)
  "The hunks from blob OLD to blob NEW, (OLD-START OLD-COUNT NEW-COUNT) each.
The symbol `error' for a blob git has not.  The review buffer must be current."
  (let ((text (condition-case nil (diff-review--git "diff" "-U0" old new) (error nil))))
    (if (null text)
        'error
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (cl-loop while (re-search-forward diff-hunk-header-re-unified nil t)
                 collect (list (diff-review--hunk-field 1) (diff-review--hunk-field 2)
                               (diff-review--hunk-field 4)))))))

(defun diff-review--map-lines (hunks lines)
  "LINES, (FIRST . LAST) in the old blob, as they are in the new one.
HUNKS is between the two.  Nil when a hunk changed lines inside the range:
then the thread is outdated, as a pull request page says."
  (let ((shift 0) (first (car lines)) (last (cdr lines)) stale)
    (pcase-dolist (`(,start ,count ,new-count) hunks)
      (cond
       ;; A pure insertion sits after line START: lines up to it stay.
       ((zerop count)
        (cond ((< start first) (cl-incf shift new-count))
              ((< start last) (setq stale t))))
       ((< (+ start count -1) first) (cl-incf shift (- new-count count)))
       ((<= start last) (setq stale t))))
    (unless stale (cons (+ first shift) (+ last shift)))))

;;;; Rendering

(defun diff-review--glyph (glyph ascii)
  "GLYPH where the frame can draw it, else its stand-in ASCII."
  (if (or (display-graphic-p) (seq-every-p #'char-displayable-p glyph))
      glyph
    ascii))

(defun diff-review--status-face (status)
  "The face for a git STATUS letter."
  (pcase status
    ("A" 'diff-review-file-added)
    ("D" 'diff-review-file-deleted)
    (_ 'diff-review-file-changed)))

(defun diff-review--counts (added removed)
  "+ADDED and −REMOVED, in the diff indicator faces."
  (concat (propertize (format "+%d" added) 'face 'diff-indicator-added) " "
          (propertize (format "%s%d" (diff-review--glyph "−" "-") removed) 'face 'diff-indicator-removed)))

(defun diff-review--sum (files key)
  "The sum of KEY over the plists FILES."
  (apply #'+ (mapcar (lambda (f) (plist-get f key)) files)))

(defun diff-review--summary ()
  "The viewed, line and comment counts of the review, and its verdict."
  (let ((n (length diff-review--comments))
        (open (diff-review--open-count diff-review--comments))
        (verdict (diff-review--verdict-text)))
    (concat
     (format "%d/%d viewed  %s"
             (seq-count #'diff-review--viewed-p diff-review--files)
             (length diff-review--files)
             (diff-review--counts (diff-review--sum diff-review--files :added)
                                  (diff-review--sum diff-review--files :removed)))
     (if (zerop n) ""
       (format "  %d comment%s, %d open" n (if (= n 1) "" "s") open))
     (if (string-empty-p verdict) "" (concat "  " verdict))
     (if (null diff-review--moved) ""
       (concat "  "
               (propertize (format "%s moved to %s," diff-review--head-name
                                   (substring diff-review--moved 0 7))
                           'face 'warning)
               " " (diff-review--key-hints '((diff-review-refresh . "rescan"))))))))

(defun diff-review--comment-mark (path)
  "The mark a list line carries for PATH's open threads, or an empty string."
  (let ((open (diff-review--open-count (diff-review--file-comments path))))
    (if (zerop open) ""
      (propertize (format "%s%d " (diff-review--glyph "◆" "#") open)
                  'face 'diff-review-comment-author))))

(defun diff-review--stats (files &optional mark)
  "The added and removed counts of FILES, after MARK, against the right edge."
  (let ((stats (concat mark (diff-review--counts (diff-review--sum files :added)
                                                 (diff-review--sum files :removed)))))
    (concat (propertize " " 'display `(space :align-to (- right ,(1+ (string-width stats)))))
            stats)))

(defvar-keymap diff-review-file-line-map
  :doc "Keymap on a file's line in a list: a click shows the file."
  "<mouse-2>" #'diff-review-mouse-open)

(defun diff-review--file-line (i indent)
  "The list line of file I, after INDENT."
  (let* ((file (diff-review--file i))
         (path (plist-get file :path))
         (viewed (diff-review--viewed-p file)))
    (propertize (concat indent
                        (propertize (plist-get file :status)
                                    'face (diff-review--status-face (plist-get file :status)))
                        " "
                        (propertize (file-name-nondirectory path)
                                    'face (cond ((eql i diff-review--index) 'bold)
                                                (viewed 'shadow)))
                        (diff-review--stats
                         (list file)
                         (concat (diff-review--comment-mark path)
                                 (and viewed (propertize (concat (diff-review--glyph "✓" "*") " ")
                                                         'face 'success))))
                        "\n")
                'diff-review-index i
                'keymap diff-review-file-line-map
                'follow-link t
                'mouse-face 'highlight
                'help-echo (if (plist-get file :old)
                               (format "%s %s %s"
                                       (plist-get file :old) (diff-review--glyph "→" "->") path)
                             path))))

(defun diff-review--tree ()
  "The files as a tree: an alist of name to a file index or a subtree.
A directory holding only a directory is merged with it, as in a/b."
  (let ((root (list nil)))
    (dotimes (i (length diff-review--files))
      (let ((node root)
            (parts (split-string (plist-get (diff-review--file i) :path) "/")))
        (while (cdr parts)
          (setq node (or (assoc (car parts) (cdr node))
                         (car (setcdr (last node) (list (list (car parts)))))))
          (pop parts))
        (setcdr (last node) (list (cons (car parts) i)))))
    (diff-review--merge-dirs (cdr root))))

(defun diff-review--merge-dirs (tree)
  "TREE with each directory holding only a directory merged with it."
  (mapcar (lambda (entry)
            (let ((name (car entry)) (child (cdr entry)))
              (while (and (consp child) (null (cdr child)) (consp (cdar child)))
                (setq name (concat name "/" (caar child))
                      child (cdar child)))
              (cons name (if (consp child) (diff-review--merge-dirs child) child))))
          tree))

(defun diff-review--tree-files (tree)
  "The file plists under TREE."
  (mapcan (lambda (entry)
            (if (integerp (cdr entry))
                (list (diff-review--file (cdr entry)))
              (diff-review--tree-files (cdr entry))))
          tree))

(defun diff-review--insert-tree (tree indent)
  "Insert TREE as sections, its entries after INDENT."
  (pcase-dolist (`(,name . ,child) tree)
    (if (integerp child)
        (magit-insert-section (diff-review-file child)
          (insert (diff-review--file-line child indent)))
      (magit-insert-section (diff-review-dir name)
        (let ((files (diff-review--tree-files child)))
          (magit-insert-heading
            indent
            (propertize (concat name "/")
                        'face (if (seq-every-p #'diff-review--viewed-p files)
                                  'shadow
                                'font-lock-comment-face))
            (diff-review--stats files)))
        (diff-review--insert-tree child (concat indent "  "))))))

(defun diff-review--insert-files ()
  "Insert the file tree, inside a root section."
  (if (zerop (length diff-review--files))
      (insert (propertize "No changes.\n" 'face 'shadow))
    (diff-review--insert-tree (diff-review--tree) "")))

(defun diff-review--comment-line (comment)
  "The overview line of COMMENT: where it is, who wrote it, how it starts."
  (let* ((path (gethash "path" comment))
         (lines (diff-review--comment-lines comment))
         (resolved (gethash "resolved" comment))
         (replies (length (or (gethash "replies" comment) []))))
    (propertize
     (concat
      "  "
      (propertize (cond ((null path) "review")
                        ((null lines) path)
                        ((= (car lines) (cdr lines)) (format "%s:%d" path (car lines)))
                        (t (format "%s:%d-%d" path (car lines) (cdr lines))))
                  'face (if resolved 'shadow 'default))
      "  "
      (propertize (or (gethash "author" comment) "?") 'face 'diff-review-comment-author)
      "  "
      (propertize (diff-review--first-line comment) 'face (and resolved 'shadow))
      (and (> replies 0) (propertize (format "  +%d" replies) 'face 'shadow))
      (and resolved (propertize (concat "  " (diff-review--glyph "✓" "*")) 'face 'success))
      "\n")
     'diff-review-comment (gethash "id" comment)
     'keymap diff-review-file-line-map
     'follow-link t
     'mouse-face 'highlight)))

(defun diff-review--insert-comments ()
  "Insert the comment threads as a section, when there are any.
Review-wide comments first, then by file in the tree's order, then by line.
A review-wide one has nowhere else to draw, so its whole thread is here,
in a section of its own."
  (when (> (length diff-review--comments) 0)
    (insert "\n")
    (magit-insert-section (diff-review-comments)
      (magit-insert-heading "Comments")
      (let ((key (lambda (c)
                   (let ((path (gethash "path" c)))
                     (list (cond ((null path) -1)
                                 ((diff-review--file-index path))
                                 (t most-positive-fixnum))
                           path
                           (or (car (diff-review--comment-lines c)) 0))))))
        (dolist (entry (sort (mapcar (lambda (c) (cons (funcall key c) c))
                                     (append diff-review--comments nil))
                             (pcase-lambda (`((,ia ,pa ,la) . ,_) `((,ib ,pb ,lb) . ,_))
                               (cond ((/= ia ib) (< ia ib))
                                     ((not (equal pa pb)) (string< pa pb))
                                     (t (< la lb))))))
          (let ((comment (cdr entry)))
            (magit-insert-section (diff-review-comment (gethash "id" comment))
              (magit-insert-heading (diff-review--comment-line comment))
              (unless (gethash "path" comment)
                (insert (diff-review--thread-string comment nil nil nil t))))))))))

(defun diff-review--goto-current ()
  "Put point on the current file's line, or the first file."
  (goto-char (point-min))
  (let ((pos (or (and diff-review--index
                      (text-property-any (point-min) (point-max)
                                         'diff-review-index diff-review--index))
                 (next-single-property-change (point-min) 'diff-review-index))))
    (when pos (goto-char pos)))
  (dolist (window (get-buffer-window-list nil nil t))
    (set-window-point window (point))))

(defun diff-review--render ()
  "Redraw the overview.  The review buffer must be current."
  (let ((inhibit-read-only t)
        (n (seq-count (lambda (f) (not (equal (plist-get f :status) " "))) diff-review--files)))
    (erase-buffer)
    (magit-insert-section (diff-review-root)
      (insert (propertize (format "%s %s %s" diff-review--head-name
                                  (diff-review--glyph "←" "<-") diff-review--base-name)
                          'face 'diff-review-title)
              "\n\n"
              (format "%d file%s changed, " n (if (= n 1) "" "s"))
              (diff-review--summary) "\n"
              (propertize (format "%s\nmerge-base %s\n%s\n"
                                  (abbreviate-file-name diff-review--root)
                                  (substring diff-review--base 0 10)
                                  (abbreviate-file-name buffer-file-name))
                          'face 'shadow)
              "\n")
      (diff-review--insert-files)
      (diff-review--insert-comments))
    (set-buffer-modified-p nil)
    (diff-review--goto-current)))

(defun diff-review--render-all ()
  "Redraw the overview and the sidebar.  The review buffer must be current."
  (diff-review--render)
  (when (buffer-live-p diff-review--sidebar)
    (with-current-buffer diff-review--sidebar (diff-review-sidebar--render)))
  (force-mode-line-update t))

(defun diff-review--file-buffers ()
  "The buffers showing this review's files.  The review buffer must be current."
  (let ((review (current-buffer)))
    (seq-filter (lambda (buffer)
                  (and (buffer-local-value 'diff-review-file-mode buffer)
                       (eq (buffer-local-value 'diff-review--review buffer) review)))
                (buffer-list))))

(defun diff-review--redecorate-files ()
  "Redraw the decorations of this review's open files.
The review buffer must be current."
  (dolist (buffer (diff-review--file-buffers))
    (with-current-buffer buffer (diff-review--decorate))))

(defun diff-review--load ()
  "Read the file, scan git and render.  The buffer holds the file's text."
  (diff-review--read)
  (diff-review--scan)
  (diff-review--render-all)
  (diff-review--refresh-files)
  (when diff-review--superseded              ; its windows show the new tip's files now
    (diff-review--remove-worktree diff-review--superseded)
    (setq diff-review--superseded nil)))

(defun diff-review--check-tip (window-or-frame)
  "Note when the head of the review shown in WINDOW-OR-FRAME has moved on.
On the review buffer's window hooks: a window coming to show the overview,
or selected with it.  A commit on the branch changes no file Emacs
watches, so this is where the review notices one; the notice stays until
the next rescan."
  (let ((window (if (framep window-or-frame)
                    (frame-selected-window window-or-frame)
                  window-or-frame)))
    (when (window-live-p window)
      (with-current-buffer (window-buffer window)
        (when (and (derived-mode-p 'diff-review-mode) diff-review--head)
          (let* ((tip (ignore-errors
                        (diff-review--git "rev-parse" "--verify"
                                          (concat diff-review--head "^{commit}"))))
                 (moved (and tip (not (equal tip diff-review--tip)) tip)))
            (unless (equal moved diff-review--moved)
              (setq diff-review--moved moved)
              (diff-review--render-all))))))))

(defun diff-review--revert (&rest _)
  "Reload the review from its file, as `revert-buffer-function'."
  (let ((inhibit-read-only t))
    (set-visited-file-modtime)          ; the disk copy is what we want: no supersession prompt
    (insert-file-contents buffer-file-name nil nil nil t))
  (diff-review--load))

;;;; Commands on a list, in the overview or the sidebar

(defun diff-review--index-at-point ()
  "The index of the file on this line, or nil."
  (get-text-property (line-beginning-position) 'diff-review-index))

(defun diff-review-open-at-point ()
  "Show the file on this line, or the comment; fold or unfold a directory.
A comment on the review as a whole is drawn here, so it folds too."
  (interactive)
  (let ((i (diff-review--index-at-point))
        (id (get-text-property (line-beginning-position) 'diff-review-comment)))
    (cond (i (diff-review--in-review (diff-review--open i)))
          ((and id (diff-review--in-review (gethash "path" (diff-review--comment id))))
           (diff-review--in-review (diff-review--goto-comment id)))
          (t (magit-section-toggle (magit-current-section))))))

(defun diff-review-mouse-open (event)
  "Show the file clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (diff-review-open-at-point))

(defun diff-review-next-file ()
  "Show the next file."
  (interactive)
  (diff-review--in-review
    (diff-review--open (if diff-review--index
                           (min (1- (length diff-review--files)) (1+ diff-review--index))
                         0))))

(defun diff-review-previous-file ()
  "Show the previous file."
  (interactive)
  (diff-review--in-review
    (diff-review--open (if diff-review--index (max 0 (1- diff-review--index)) 0))))

(defun diff-review-next-unviewed ()
  "Show the next file that is not viewed, wrapping around."
  (interactive)
  (diff-review--in-review
    (let* ((n (length diff-review--files))
           (from (or diff-review--index -1))
           (i (cl-loop for k from 1 to n
                       for j = (mod (+ from k) n)
                       unless (diff-review--viewed-p (diff-review--file j))
                       return j)))
      (if i (diff-review--open i) (message "Every file is viewed")))))

(defun diff-review-toggle-viewed ()
  "Toggle the viewed mark of the file at point, or of the file shown."
  (interactive)
  (let ((i (diff-review--index-at-point)))
    (diff-review--in-review
      (let ((file (or (diff-review--file (or i diff-review--index))
                      (user-error "No file to mark"))))
        (diff-review--set-viewed file (not (diff-review--viewed-p file)))
        (diff-review--render-all)))))

(defun diff-review-refresh ()
  "Reread the file and rescan the diff.
A file that changed since it was viewed loses its mark."
  (interactive)
  (diff-review--in-review (revert-buffer)))

(defun diff-review-approve ()
  "Approve the head as the review shows it, as `diff-review-author'.
The verdict holds until the head moves past that commit."
  (interactive)
  (diff-review--in-review (diff-review--set-verdict "approved")))

(defun diff-review-request-changes ()
  "Ask for changes to the head as the review shows it, as `diff-review-author'.
The threads say which; the verdict holds until the head moves past that commit."
  (interactive)
  (diff-review--in-review (diff-review--set-verdict "changes")))

(defun diff-review-overview ()
  "Show the overview in the main window."
  (interactive)
  (diff-review--show-in-main (or (diff-review--review-buffer)
                                 (user-error "Not in a review"))))

(defun diff-review-quit ()
  "End the review: its buffer, sidebar and decorations go, as does what it showed."
  (interactive)
  (kill-buffer (diff-review--in-review (current-buffer))))

(defun diff-review-toggle-sidebar ()
  "Show or hide this review's sidebar."
  (interactive)
  (when-let* ((main (window-parameter (selected-window) 'diff-review-main)))
    (select-window main))               ; this sidebar may be about to go
  (diff-review--in-review
    (setq diff-review--sidebar-enabled (not diff-review--sidebar-enabled)))
  (mapc #'diff-review--follow (frame-list)))

(defvar-keymap diff-review-raw-mode-map
  :doc "Keymap for `diff-review-raw-mode'."
  "C-c C-c" #'diff-review-toggle-raw)

(define-minor-mode diff-review-raw-mode
  "Minor mode in a review file shown as its JSON.
\\<diff-review-raw-mode-map>\\[diff-review-toggle-raw] turns it back into the review."
  :lighter " Raw")

(defun diff-review-toggle-raw ()
  "Show the review file as its JSON, ending the review, or the JSON as the review.
Like `doc-view-toggle-display': the overview gives way to the file in
the mode its name picks, and back.  Unsaved JSON is saved first, or kept."
  (interactive)
  (if diff-review-raw-mode
      (progn
        (when (buffer-modified-p)
          (if (y-or-n-p "Save the JSON first? ")
              (save-buffer)
            (user-error "The JSON has unsaved changes")))
        (condition-case err
            (diff-review-mode)
          (error (diff-review--raw)       ; not a review: the JSON stays
                 (signal (car err) (cdr err)))))
    (diff-review-overview)
    (diff-review--raw)))

(defun diff-review--raw ()
  "Turn the review in the current buffer into its file's JSON."
  (diff-review--end)
  (kill-local-variable 'revert-buffer-function)
  ;; A revert re-runs `set-auto-mode', which without this match goes by the name.
  (let ((magic-mode-alist (seq-remove (lambda (entry) (eq (cdr entry) #'diff-review-mode))
                                      magic-mode-alist)))
    (revert-buffer t t))
  (diff-review-raw-mode))

(defvar-keymap diff-review-mode-map
  :doc "Keymap for `diff-review-mode', shared with the sidebar."
  :parent magit-section-mode-map
  "RET" #'diff-review-open-at-point
  "n" #'diff-review-next-file
  "p" #'diff-review-previous-file
  "u" #'diff-review-next-unviewed
  "v" #'diff-review-toggle-viewed
  "g" #'diff-review-refresh
  "t" #'diff-review-toggle-sidebar
  "A" #'diff-review-approve
  "C" #'diff-review-request-changes
  "C-c C-c" #'diff-review-toggle-raw
  "q" #'diff-review-quit)

;;;###autoload
(define-derived-mode diff-review-mode magit-section-mode "Review"
  "Major mode for a review file: an overview of what a worktree changed.
\\<diff-review-mode-map>\\[diff-review-open-at-point] shows the file at point in this window,
or folds a directory; a click on a file shows it too.
\\[diff-review-next-file] and \\[diff-review-previous-file] walk the files;
\\[diff-review-next-unviewed] skips to the next unviewed one.
\\[diff-review-toggle-viewed] toggles the viewed mark, \\[diff-review-refresh] rescans
and \\[diff-review-toggle-sidebar] shows or hides the sidebar.
\\[diff-review-approve] approves the head and \\[diff-review-request-changes] asks for changes to it.
\\[diff-review-toggle-raw] shows the file as its JSON, and back.
\\[diff-review-quit] ends the review, as does killing the buffer.

\\{diff-review-mode-map}"
  ;; `setq-local': in Emacs 31 a plain `setq' sets the global default, and
  ;; every file buffer's auto-revert then runs this, leaving it modified.
  (setq-local revert-buffer-function #'diff-review--revert)
  (setq diff-review--sidebar-enabled diff-review-sidebar
        diff-review--sidebar-width diff-review-sidebar-width
        diff-review--inline-removed diff-review-inline-removed)
  (add-hook 'write-contents-functions #'diff-review--write nil t)
  (add-hook 'kill-buffer-hook #'diff-review--close nil t)
  ;; Local: a commit on the branch is noticed when the overview is shown or selected.
  (add-hook 'window-selection-change-functions #'diff-review--check-tip nil t)
  (add-hook 'window-buffer-change-functions #'diff-review--check-tip nil t)
  ;; Global, so they live only while there is a review: see `diff-review--close'.
  (add-hook 'window-buffer-change-functions #'diff-review--follow)
  (add-hook 'find-file-hook #'diff-review--protect)
  (display-line-numbers-mode -1)
  (auto-revert-mode 1)
  (diff-review--load))

;;;###autoload
(add-to-list 'magic-mode-alist
             (cons "\\`\\s-*{\\s-*\"format\"\\s-*:\\s-*\"diff-review/" #'diff-review-mode))

(defun diff-review--leave (buffer leave)
  "Call LEAVE, then show what came before BUFFER in the windows that showed it.
Their dedication is lifted meanwhile, else killing BUFFER deletes a
dedicated one."
  (let* ((windows (get-buffer-window-list buffer nil t))
         (dedicated (mapcar #'window-dedicated-p windows)))
    (dolist (window windows) (set-window-dedicated-p window nil))
    (funcall leave)
    (cl-loop for window in windows
             for was in dedicated
             ;; A killed buffer has left its windows already.
             do (when (eq (window-buffer window) buffer)
                  (switch-to-prev-buffer window))
                (set-window-dedicated-p window was))))

(defun diff-review--close ()
  "End the review being killed, as `diff-review--end' does.
Windows showing the review go back to what they showed before it."
  (diff-review--leave (current-buffer) #'diff-review--end))

(defun diff-review--end ()
  "End the review in the current buffer: its files' decorations and its sidebar.
Windows showing its files go back to what they showed before; buffers
showing a revision of a file are killed, as is the throwaway worktree.
The last review to go takes `diff-review--follow' off the window hook."
  (let ((review (current-buffer)))
    (mapc #'diff-review--drop (diff-review--file-buffers))
    (when (buffer-live-p diff-review--sidebar)
      (kill-buffer diff-review--sidebar))
    (dolist (dir (delq nil (list diff-review--worktree diff-review--superseded)))
      (diff-review--remove-worktree dir))
    (unless (seq-some (lambda (buffer)
                        (and (not (eq buffer review))
                             (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                                      'diff-review-mode)))
                      (buffer-list))
      (remove-hook 'window-buffer-change-functions #'diff-review--follow)
      (remove-hook 'find-file-hook #'diff-review--protect))))

(defun diff-review--protect ()
  "Make a file visited in a review's throwaway worktree read-only.
On `find-file-hook' while a review is open: the worktree goes with its
review, so a file reached there through xref is as read-only as one the
review opened."
  (when (seq-some (lambda (buffer)
                    (when-let* ((dir (buffer-local-value 'diff-review--worktree buffer)))
                      (file-in-directory-p buffer-file-name dir)))
                  (buffer-list))
    (setq buffer-read-only t)))

;;;; The sidebar

(defvar-keymap diff-review-sidebar-mode-map
  :doc "Keymap for `diff-review-sidebar-mode'."
  :parent diff-review-mode-map
  "q" #'diff-review-toggle-sidebar)

(define-derived-mode diff-review-sidebar-mode magit-section-mode "Review-Sidebar"
  "Major mode for a review's sidebar: its changed files as a tree.
The sidebar shows beside the window the review shows in, the two
as one atomic window.  The keys are those of `diff-review-mode';
\\<diff-review-sidebar-mode-map>\\[diff-review-toggle-sidebar] hides the sidebar.

\\{diff-review-sidebar-mode-map}"
  ;; The header line carries the summary; a mode line under the tree adds nothing.
  (setq mode-line-format nil
        header-line-format '(:eval (diff-review-sidebar--header)))
  (add-hook 'kill-buffer-hook #'diff-review-sidebar--close nil t)
  (add-hook 'window-size-change-functions #'diff-review-sidebar--resized nil t)
  (display-line-numbers-mode -1))

(defun diff-review--sidebar-buffer (review)
  "REVIEW's sidebar buffer, created if needed."
  (with-current-buffer review
    (unless (buffer-live-p diff-review--sidebar)
      (let ((sidebar (generate-new-buffer
                      (format " *review sidebar: %s*" (buffer-name review)))))
        (with-current-buffer sidebar
          (diff-review-sidebar-mode)
          (setq diff-review--review review)
          (diff-review-sidebar--render))
        (setq diff-review--sidebar sidebar)))
    diff-review--sidebar))

(defun diff-review-sidebar--header ()
  "The sidebar's header line: the head and the review's summary."
  (diff-review--in-review
    (concat (propertize (format " %s  " diff-review--head-name) 'face 'bold)
            (diff-review--summary))))

(defun diff-review-sidebar--render ()
  "Redraw the sidebar.  The sidebar buffer must be current."
  (let ((inhibit-read-only t)
        (review (diff-review--review-buffer)))
    (erase-buffer)
    (when review
      (let ((moved (not (eql diff-review--index (buffer-local-value 'diff-review--index review)))))
        ;; Local copies of the review's state, for the rendering it shares.
        (dolist (var '(diff-review--files diff-review--index diff-review--viewed
                       diff-review--comments))
          (set (make-local-variable var) (buffer-local-value var review)))
        (magit-insert-section (diff-review-root)
          (diff-review--insert-files))
        (diff-review--goto-current)
        (when (and moved diff-review--index)   ; show the new file, else keep the folds
          (magit-section-reveal (magit-current-section)))))))

;; A sidebar window and the window it is attached to point at each other
;; through window parameters; the link holds while both ends agree.

(defun diff-review--sidebar-window (window)
  "The sidebar window attached to WINDOW, or nil."
  (let ((sidebar (window-parameter window 'diff-review-sidebar)))
    (and (window-live-p sidebar)
         (eq (window-parameter sidebar 'diff-review-main) window)
         sidebar)))

(defun diff-review--attach (window sidebar)
  "Show SIDEBAR beside WINDOW, the two as one atomic window."
  (when-let* ((new (display-buffer-in-atom-window
                    sidebar `((window . ,window)
                              (side . ,diff-review-sidebar-side)
                              (window-width
                               . ,(with-current-buffer sidebar
                                   (diff-review--in-review diff-review--sidebar-width)))
                              ;; Kept through balancing and frame resizes;
                              ;; the user's own resizing still works.
                              (preserve-size . (t . nil))
                              (dedicated . t)))))
    (set-window-parameter new 'diff-review-main window)
    (set-window-parameter window 'diff-review-sidebar new)
    (with-current-buffer sidebar (diff-review-sidebar--render))))

(defun diff-review-sidebar--resized (window)
  "Keep the width WINDOW, a sidebar, was resized to.
The review shows its sidebar at that width from now on, and the width is
preserved again: a resize does not update a preserved width, nor does
`enlarge-window' leave it preserved."
  (when-let* ((review (with-current-buffer (window-buffer window)
                        (diff-review--review-buffer))))
    (with-current-buffer review
      (setq diff-review--sidebar-width (window-total-width window)))
    (window-preserve-size window t t)))

(defun diff-review--detach (sidebar)
  "Delete the sidebar window SIDEBAR, freeing the window it was attached to."
  (let ((main (window-parameter sidebar 'diff-review-main)))
    (let ((ignore-window-parameters t))   ; else the whole atomic window goes
      (delete-window sidebar))
    (when (window-live-p main)
      (set-window-parameter main 'window-atom nil)
      (set-window-parameter main 'diff-review-sidebar nil))))

(defun diff-review-sidebar--close ()
  "Delete the windows of the sidebar buffer being killed."
  (mapc #'diff-review--detach (get-buffer-window-list nil nil t)))

(defun diff-review--follow (&optional frame)
  "Give each window of FRAME the sidebar its buffer's review wants, or none.
Runs after a window of FRAME changes buffer."
  (dolist (window (window-list (if (framep frame) frame (selected-frame)) 'no-minibuffer))
    (when (window-live-p window)        ; not deleted earlier in this loop
      (with-current-buffer (window-buffer window)
        (if (derived-mode-p 'diff-review-sidebar-mode)
            (let ((main (window-parameter window 'diff-review-main)))
              (unless (and (window-live-p main)
                           (eq (diff-review--sidebar-window main) window))
                (diff-review--detach window)))   ; an orphan: restored or left behind
          (let* ((review (diff-review--review-buffer))
                 (wanted (and review
                              (buffer-local-value 'diff-review--sidebar-enabled review)
                              (diff-review--sidebar-buffer review)))
                 (sidebar (diff-review--sidebar-window window)))
            (unless (eq (and sidebar (window-buffer sidebar)) wanted)
              (when sidebar (diff-review--detach sidebar))
              (when wanted (diff-review--attach window wanted)))))))))

;;;; Showing a file

(defun diff-review--main-window ()
  "The window to show the review in.
The selected one, or the one the selected sidebar is attached to."
  (or (window-parameter (selected-window) 'diff-review-main)
      (selected-window)))

(defun diff-review--undedicated (window fn &rest args)
  "Apply FN to ARGS with WINDOW undedicated, then dedicate it as it was."
  (let ((dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window nil)
    (unwind-protect (apply fn args)
      (when (window-live-p window)
        (set-window-dedicated-p window dedicated)))))

(defun diff-review--show-in-main (buffer)
  "Show BUFFER in the review's window and select it.
The window's history is left as it was, so quitting or killing the
review goes back to what the window showed before it.  A window
dedicated to something outside the review is not the review's to
take: then BUFFER goes where `display-buffer' says."
  (let ((window (diff-review--main-window)))
    (if (and (window-dedicated-p window)
             (not (eq (with-current-buffer (window-buffer window) (diff-review--review-buffer))
                      (with-current-buffer buffer (diff-review--review-buffer)))))
        (progn (pop-to-buffer-same-window buffer)
               (selected-window))
      (diff-review--show-quietly window buffer)
      (select-window window)
      window)))

(defun diff-review--show-quietly (window buffer)
  "Show BUFFER in WINDOW, dedicated or not, its history left as it was."
  (let ((prev (window-prev-buffers window))
        (next (window-next-buffers window)))
    (diff-review--undedicated window #'set-window-buffer window buffer)
    (set-window-prev-buffers window prev)
    (set-window-next-buffers window next)))

(defun diff-review--blob-buffer (rev path key &optional deleted)
  "A read-only buffer with PATH as it is at REV; DELETED when the file is gone.
KEY identifies the content, a blob hash or a commit: the buffer is built
once and reused while its KEY holds, so revisits cost no git call and no
mode start, and its overlays are not emptied onto the first line.  It is
named for the head, so one buffer follows the branch, and it is this
review's: another review of the same name and path has its own.  The
review buffer must be current."
  (let* ((name (format "*%s: %s*" (if deleted "deleted" diff-review--head-name) path))
         (buffer (alist-get name diff-review--blobs nil nil #'equal)))
    (unless (buffer-live-p buffer)
      (setq buffer (generate-new-buffer name))
      (setf (alist-get name diff-review--blobs nil nil #'equal) buffer))
    (unless (equal (buffer-local-value 'diff-review--blob buffer) key)
      (let ((contents (diff-review--git "show" (concat rev ":" path)))
            (root diff-review--root))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (delete-all-overlays)
            (erase-buffer)
            (insert contents)
            (let ((buffer-file-name (expand-file-name path root)))
              (set-auto-mode))
            (setq diff-review--deleted deleted
                  diff-review--blob key)
            (read-only-mode 1)))))
    buffer))

(defun diff-review--blob-for (file)
  "The revision buffer of FILE: the head's version, the base's of a deleted one."
  (if (equal (plist-get file :status) "D")
      (diff-review--blob-buffer diff-review--base (plist-get file :path) (plist-get file :hash) t)
    (diff-review--blob-buffer diff-review--tip (plist-get file :path) (plist-get file :hash))))

(defun diff-review--visit (file &optional read-only)
  "Visit FILE as `find-file-noselect' does, READ-ONLY when so: its buffer.
An unmodified buffer the file changed under is reread without a question:
a commit on the branch under review moves its worktree's files, and the
review shows what git has."
  (let ((buffer (find-buffer-visiting file)))
    (when (and buffer (not (buffer-modified-p buffer))
               (not (verify-visited-file-modtime buffer)))
      (with-current-buffer buffer (ignore-errors (revert-buffer t t))))
    (setq buffer (find-file-noselect file))
    (when read-only (with-current-buffer buffer (setq buffer-read-only t)))
    buffer))

(defun diff-review--throwaway-dir ()
  "The folder of the throwaway worktrees, under the common git directory."
  (expand-file-name "review/worktrees/" (diff-review--common-dir diff-review--root)))

(defun diff-review--throwaway-p (worktree)
  "Whether WORKTREE, a plist of `diff-review--worktrees', is a throwaway one."
  (string-prefix-p (diff-review--throwaway-dir) (plist-get worktree :path)))

(defun diff-review--lock-reason ()
  "The lock a throwaway worktree of this Emacs carries: the package, its pid."
  (format "diff-review %d" (emacs-pid)))

(defun diff-review--held-p (worktree)
  "Whether a running Emacs holds WORKTREE: its lock names a live process.
Reviews in another Emacs are not this one's to end; a killed one's are."
  (let ((reason (plist-get worktree :locked)))
    (and reason
         (string-match "\\`diff-review \\([0-9]+\\)\\'" reason)
         (process-attributes (string-to-number (match-string 1 reason))))))

(defun diff-review--remove-worktree-at (worktree)
  "Remove WORKTREE, a plist, locked or not, with its files.
Only what git lists as a worktree goes: a folder under the throwaways'
that is not one, a branch's review file in review/worktrees/..., stays."
  (ignore-errors
    (diff-review--git "worktree" "remove" "--force" "--force"
                      (directory-file-name (plist-get worktree :path)))))

(defun diff-review--remove-worktree (dir)
  "Remove the throwaway worktree DIR, which this Emacs added and locked.
Every buffer visiting a file in it goes first, one reached through xref
too; its windows show what they did before.  One with changes in it, made
past the read-only, on disk or unsaved in a buffer, is kept and named,
for the user to take them from; it stays until this Emacs is gone."
  (let ((buffers (seq-filter (lambda (b) (and (buffer-file-name b)
                                              (file-in-directory-p (buffer-file-name b) dir)))
                             (buffer-list))))
    (if (or (seq-some #'buffer-modified-p buffers)
            (not (equal (ignore-errors (diff-review--git-at dir "status" "--porcelain")) "")))
        (message "Review worktree %s kept: it has changes" (abbreviate-file-name dir))
      (dolist (buffer buffers)
        (diff-review--leave buffer (lambda () (kill-buffer buffer))))
      (when-let* ((worktree (seq-find (lambda (w) (equal (plist-get w :path) dir))
                                      (ignore-errors (diff-review--worktrees diff-review--root)))))
        (when (equal (plist-get worktree :locked) (diff-review--lock-reason))
          (diff-review--remove-worktree-at worktree)
          (ignore-errors (diff-review--git "worktree" "prune")))))))

(defun diff-review--throwaway ()
  "This review's throwaway worktree of the tip, added on first use, or nil.
It is review/worktrees/<hash> under the common git directory, of that one
commit and never moved: a moved tip gets a new one; a worktree lock names
the Emacs that added it.  Ignored files, dependencies among them, are not
in it.  Throwaways no running Emacs holds, a killed one's, go first, and
git's record of a directory that is gone is pruned.  A worktree is one
review's: when a live review, here or in another Emacs, holds the
commit's, this one gets none.  The review buffer must be current."
  (unless (and diff-review--worktree (file-directory-p diff-review--worktree))
    (let ((worktree (file-name-as-directory
                     (expand-file-name diff-review--tip (diff-review--throwaway-dir)))))
      (dolist (old (diff-review--worktrees diff-review--root))
        (when (and (diff-review--throwaway-p old) (not (diff-review--held-p old)))
          (diff-review--remove-worktree-at old)))
      (diff-review--git "worktree" "prune")
      (unless (file-directory-p worktree)
        (make-directory (diff-review--throwaway-dir) t)
        (diff-review--git "worktree" "add" "--quiet" "--detach"
                          (directory-file-name worktree) diff-review--tip)
        (diff-review--git "worktree" "lock" "--reason" (diff-review--lock-reason)
                          (directory-file-name worktree))
        (setq diff-review--worktree worktree))))
  diff-review--worktree)

(defun diff-review--head-buffer (file)
  "A buffer with FILE as it is at the head: a real file where there is one.
A worktree that has the tip checked out, the main one included, with the
file clean, on disk and in any buffer visiting it, gives the file itself,
so xref, eglot and project.el see an ordinary file in its tree.  Without
one the review's throwaway worktree of the tip is added, and is such a
worktree, its files read-only.  A dirty file, there too, would not line
up with the hunks, so then it is the revision buffer; so is a symlink,
whose blob is the link, a file another open review shows, and every
file when another review holds the commit's throwaway."
  (let* ((path (plist-get file :path))
         (review (current-buffer))
         (worktrees (seq-filter (lambda (w) (and (equal (plist-get w :head) diff-review--tip)
                                                 (or (not (diff-review--throwaway-p w))
                                                     (equal (plist-get w :path) diff-review--worktree))))
                                (diff-review--worktrees diff-review--root)))
         (clean-p (lambda (w)
                    (let* ((dir (plist-get w :path))
                           (name (expand-file-name path dir))
                           (buffer (find-buffer-visiting name))
                           (owner (and buffer (buffer-local-value 'diff-review--review buffer))))
                      (and (file-exists-p name)   ; a sparse checkout omits
                           (not (file-symlink-p name))
                           (equal (ignore-errors
                                    (diff-review--git-at dir "status" "--porcelain" "--" path))
                                  "")
                           (not (and buffer (buffer-modified-p buffer)))
                           (or (null owner) (eq owner review) (not (buffer-live-p owner)))))))
         (clean (or (seq-find clean-p worktrees)
                    (and (null worktrees)
                         (when-let* ((added (diff-review--throwaway)))
                           (seq-find clean-p (list (list :path added :head diff-review--tip))))))))
    (if clean
        (diff-review--visit (expand-file-name path (plist-get clean :path))
                            (equal (plist-get clean :path) diff-review--worktree))
      (diff-review--blob-for file))))

(defun diff-review--enter (buffer path)
  "Put BUFFER, showing PATH, under this review: its hunks parsed, decorated.
The review buffer must be current."
  (let ((review (current-buffer)))
    (with-current-buffer buffer
      (setq diff-review--review review
            diff-review--file-path path)
      (diff-review-file-mode 1))))

(defun diff-review--drop (buffer)
  "Take file BUFFER out of the review; its windows show what they did before.
A real file stays, as the user's, and so does a file of the throwaway
worktree with unsaved edits, which keep the worktree too; a revision
buffer, or a throwaway's file as it was, is killed.  The review buffer
must be current."
  (let ((worktree diff-review--worktree))
    (with-current-buffer buffer
      (diff-review--leave
       buffer
       (lambda ()
         (if (and buffer-file-name
                  (or (buffer-modified-p)
                      (not (and worktree (file-in-directory-p buffer-file-name worktree)))))
             (diff-review-file-mode -1)
           (kill-buffer)))))))

(defun diff-review--file-buffer (file)
  "The buffer that shows FILE as the review has it, made or brought up to date.
The review buffer must be current."
  (cond ((equal (plist-get file :status) "D") (diff-review--blob-for file))
        (diff-review--head (diff-review--head-buffer file))
        (t (find-file-noselect (expand-file-name (plist-get file :path) diff-review--root)))))

(defun diff-review--refresh-files ()
  "Bring this review's open files up to date after a scan.
A file the review no longer has leaves it.  Each other file is chosen
again as on opening, so a buffer that no longer shows what the review
has, a worktree's file dirtied or moved on, a file the head deleted,
gives its windows to the one that does; the hunks are parsed again.  The
review buffer must be current."
  (dolist (buffer (diff-review--file-buffers))
    (when (buffer-live-p buffer)        ; not dropped for an earlier one
      (let* ((path (buffer-local-value 'diff-review--file-path buffer))
             (file (and-let* ((i (diff-review--file-index path))) (diff-review--file i))))
        (if (null file)
            (diff-review--drop buffer)
          (let ((shown (diff-review--file-buffer file)))
            (unless (eq shown buffer)
              (dolist (window (get-buffer-window-list buffer nil t))
                (diff-review--show-quietly window shown))
              (diff-review--drop buffer))
            (diff-review--enter shown path)))))))

(defun diff-review--open (i)
  "Show file I in the main window, at its first hunk, and sync the lists.
Returns its buffer.  The review buffer must be current."
  (unless (< -1 i (length diff-review--files))
    (user-error "The review has no files"))
  (setq diff-review--index i)
  (let* ((file (diff-review--file i))
         (path (plist-get file :path))
         (buffer (diff-review--file-buffer file)))
    (dolist (other (diff-review--file-buffers))   ; a file has one buffer in the review
      (when (and (not (eq other buffer))
                 (equal (buffer-local-value 'diff-review--file-path other) path))
        (diff-review--drop other)))
    (diff-review--enter buffer path)
    (diff-review--render-all)
    (diff-review--show-in-main buffer)
    (with-current-buffer buffer          ; the caller may be in the review buffer
      (goto-char (point-min))
      (when diff-review--hunks
        (diff-review--goto-hunk (car diff-review--hunks))))
    buffer))

;;;; The file side

(defun diff-review--parse-hunks (file)
  "Parse `git diff -U0' of FILE into hunk plists, in the review buffer."
  (let ((old (or (plist-get file :old) (plist-get file :path)))
        (default-directory diff-review--root)
        (range (diff-review--range))
        hunks)
    (with-temp-buffer
      (apply #'process-file "git" nil t nil "diff" "-U0" "-M"
             (append range (list "--" old (plist-get file :path))))
      (goto-char (point-min))
      (while (re-search-forward diff-hunk-header-re-unified nil t)
        (let ((line (diff-review--hunk-field 3))   ; the new side
              (count (diff-review--hunk-field 4))
              removed)
          (forward-line 1)
          (while (and (not (eobp)) (looking-at "^\\([-+]\\)\\(.*\\)$"))
            (when (equal (match-string 1) "-")
              (push (match-string 2) removed))
            (forward-line 1))
          (push (list :line line :count count :removed (nreverse removed)) hunks))))
    (nreverse hunks)))

(defun diff-review--line-pos (line)
  "The position where LINE starts, counted in the whole buffer, narrowed or not."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (point))))

(defun diff-review--last-line ()
  "The last line with text in the whole buffer, narrowed or not."
  (save-restriction
    (widen)
    (line-number-at-pos (max (point-min) (1- (point-max))) t)))

(defun diff-review--hunk-line (hunk)
  "The buffer line HUNK starts on; a pure deletion sits before the next line."
  (if (zerop (plist-get hunk :count))
      (1+ (plist-get hunk :line))
    (plist-get hunk :line)))

(when (fboundp 'define-fringe-bitmap)   ; absent from a build without a window system
  (define-fringe-bitmap 'diff-review-bar [224] nil nil '(center repeated)))

(defun diff-review--tint (face source)
  "FACE as an overlay face, with SOURCE's background when FACE sets none."
  (let ((own (face-attribute face :background nil t))
        (borrowed (face-attribute source :background nil t)))
    (if (or (stringp own) (not (stringp borrowed)))
        face
      (list face `(:background ,borrowed :extend t)))))

(defun diff-review--fringe-mark (face)
  "A string that draws a bar in `diff-review-fringe' on its line, in FACE.
Empty without that option or the bitmap.  A text terminal draws nothing for
it, so it is the same string on every frame."
  (if (and diff-review-fringe (fboundp 'fringe-bitmap-p) (fringe-bitmap-p 'diff-review-bar))
      (propertize " " 'display `(,(if (eq diff-review-fringe 'right) 'right-fringe 'left-fringe)
                                 diff-review-bar ,face))
    ""))

(defun diff-review--bars (start lines mark)
  "Put MARK before each of LINES lines from START, in zero-width overlays.
Not `line-prefix': an overlay's would hide the one `org-indent-mode' draws."
  (unless (string-empty-p mark)
    (save-excursion
      (goto-char start)
      (dotimes (_ lines)
        (let ((overlay (make-overlay (point) (point))))
          (overlay-put overlay 'before-string mark)
          (push overlay diff-review--overlays))
        (forward-line 1)))))

(defun diff-review--fontify (lines mode)
  "Copies of LINES with the syntax faces MODE gives them, as one text.
One mode start for all of them: starting a mode is the slow part."
  (condition-case nil
      (if (or (null lines) (eq mode 'fundamental-mode))
          (mapcar #'copy-sequence lines)   ; copies: the hunks' own strings stay bare
        (with-temp-buffer
          (insert (string-join lines "\n"))
          (delay-mode-hooks (funcall mode))
          (font-lock-ensure)
          (split-string (buffer-string) "\n")))
    (error (mapcar #'copy-sequence lines))))

(defun diff-review--removed-string (lines tint)
  "The inline rendering of removed LINES, fontified already: fringe-marked.
TINT, a face or nil, goes over the syntax faces."
  (let ((mark (diff-review--fringe-mark 'diff-indicator-removed)))
    (mapconcat (lambda (text)
                 (when tint (add-face-text-property 0 (length text) tint t text))
                 (concat mark text (propertize "\n" 'face tint)))
               lines)))

(defun diff-review--decorate ()
  "Add the inline removed lines, the added tint, the fringe bars and the folds."
  (diff-review--undecorate)
  (let* ((inline (diff-review--in-review diff-review--inline-removed))
         (added (and diff-review-tint-added (diff-review--tint 'diff-review-added 'diff-added)))
         (removed (and diff-review-tint-removed
                       (diff-review--tint 'diff-review-removed 'diff-removed)))
         (texts (and inline      ; the file's removed lines, fontified together, once
                     (mapcar #'copy-sequence
                             (or diff-review--texts
                                 (setq diff-review--texts
                                       (diff-review--fontify
                                        (seq-mapcat (lambda (hunk) (plist-get hunk :removed))
                                                    diff-review--hunks)
                                        major-mode)))))))
    (diff-review--decorate-comments)   ; first: the folds keep its lines
    (if diff-review--deleted
        (let ((overlay (make-overlay (point-min) (point-max))))
          (overlay-put overlay 'face removed)
          (push overlay diff-review--overlays)
          (diff-review--bars (point-min) (count-lines (point-min) (point-max))
                             (diff-review--fringe-mark 'diff-indicator-removed)))
      (dolist (hunk diff-review--hunks)
        (let* ((line (diff-review--hunk-line hunk))
               (start (diff-review--line-pos line))
               (count (plist-get hunk :count))
               (end (if (zerop count) start
                      (diff-review--line-pos (+ (plist-get hunk :line) count))))
               (overlay (make-overlay start end nil t nil))
               (mark (if (> count 0) (diff-review--fringe-mark 'diff-indicator-added) ""))
               above)
          (overlay-put overlay 'face (and (> count 0) added))
          (when (and inline (plist-get hunk :removed))
            (let ((n (length (plist-get hunk :removed))))
              (setq above (diff-review--removed-string (seq-take texts n) removed))
              (setq texts (nthcdr n texts))))
          ;; The first added line's bar goes after the removed lines, in one
          ;; string: two before-strings at one place have no reliable order.
          (overlay-put overlay 'before-string (concat above mark))
          (push overlay diff-review--overlays)
          (when (> count 1)
            (diff-review--bars (diff-review--line-pos (1+ line)) (1- count) mark))))
      (when diff-review--focus (diff-review--fold)))))

(defun diff-review--fold ()
  "Hide unchanged lines beyond `diff-review-context' of any hunk."
  (let ((last (line-number-at-pos (point-max)))
        (dots (diff-review--glyph "⋯" "..."))
        keep)
    (dolist (hunk diff-review--hunks)
      (push (cons (max 1 (- (diff-review--hunk-line hunk) diff-review-context))
                  (min last (+ (plist-get hunk :line)
                               (max 0 (1- (plist-get hunk :count)))
                               diff-review-context)))
            keep))
    (dolist (overlay (diff-review--thread-overlays))   ; a thread stays in view
      (pcase-let ((`(,first . ,end) (diff-review--thread-span overlay)))
        (push (cons (max 1 (- first diff-review-context)) (min last (+ end diff-review-context)))
              keep)))
    (setq keep (sort keep (lambda (a b) (< (car a) (car b)))))
    (let ((line 1))
      (dolist (range (append keep (list (cons (1+ last) (1+ last)))))
        (when (< line (car range))
          (let ((overlay (make-overlay (diff-review--line-pos line)
                                       (diff-review--line-pos (car range)))))
            (overlay-put overlay 'display
                         (propertize (format "   %s %d unchanged lines %s\n"
                                             dots (- (car range) line) dots)
                                     'face 'diff-review-unchanged))
            (push overlay diff-review--overlays)))
        (setq line (max line (1+ (cdr range))))))))

(defun diff-review--undecorate ()
  "Remove the review overlays from this buffer."
  (mapc #'delete-overlay diff-review--overlays)
  (setq diff-review--overlays nil))

;;;; Comment threads in a file

;; A thread is its range overlay: the comment, its staleness and whether it
;; is drawn open are properties, and the thread text is its after-string.

(defun diff-review--this-index ()
  "The index of this file buffer's file in the review, whichever file it shows."
  (let ((path diff-review--file-path))   ; this buffer's, read before entering the review
    (diff-review--in-review (diff-review--file-index path))))

(defun diff-review--this-file ()
  "The file plist of this file buffer."
  (let ((i (diff-review--this-index)))
    (diff-review--in-review (diff-review--file i))))

(defun diff-review--buffer-blob ()
  "The blob hash of what this file buffer shows: the revision, or its text.
A file buffer's text is hashed as it stands, saved or not, so a comment's
lines and its blob describe the same text.  The blob is written to git's
objects, so the diff that follows a thread once the file changes has
something to start from."
  (or diff-review--blob
      (let ((default-directory (diff-review--in-review diff-review--root))
            (path diff-review--file-path))
        (save-restriction
          (widen)
          (string-trim
           (with-output-to-string
             (unless (zerop (call-process-region (point-min) (point-max) "git" nil
                                                 standard-output nil "hash-object" "-w"
                                                 "--stdin" "--path" path))
               (error "Git hash-object failed"))))))))

(defun diff-review--thread-lines (comment blob)
  "Where COMMENT draws in a buffer showing BLOB: (FIRST LAST STALE).
Lines counted in another blob are mapped through `git diff' of the two;
STALE when a change inside them makes that impossible, and then they
draw where they were.  Lines past the last line with text draw on it, so
the range is never empty: a thread on the empty line a file ends with
hangs under the line before."
  (let* ((lines (diff-review--comment-lines comment))
         (old (gethash "blob" comment))
         (mapped (if (or (null old) (equal old blob))
                     lines
                   (let ((hunks (diff-review--in-review (diff-review--hunks-between old blob))))
                     (and (listp hunks) (diff-review--map-lines hunks lines)))))
         (drawn (or mapped lines))
         (last (diff-review--last-line)))
    (list (min (car drawn) last) (min (cdr drawn) last) (not mapped))))

(defun diff-review--when (at)
  "AT, an RFC 3339 time, as its date and minute; anything else as it is."
  (if (and (stringp at) (string-match "\\`\\([0-9-]+\\)T\\([0-9]+:[0-9]+\\)" at))
      (concat (match-string 1 at) " " (match-string 2 at))
    (or at "")))

(defun diff-review--body-lines (body width)
  "BODY as lines no wider than WIDTH, each paragraph filled; blank lines kept."
  (with-temp-buffer
    (insert (or body ""))
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    (split-string (buffer-string) "\n")))

(defun diff-review--thread-string (comment first last stale open)
  "COMMENT drawn under its lines FIRST to LAST: in full when OPEN, else one line.
Nil lines, for a comment on the review, name none.  STALE adds the outdated
mark.  Every row carries the fringe bar the range has, so range and thread
read as one block.  Bodies fill at the buffer's `fill-column', less the
margin; the keys are hinted where they are bound, in a file."
  (let* ((bar (diff-review--fringe-mark 'diff-review-comment))
         (width (max 20 (- fill-column 4)))
         (who (lambda (m)
                (concat (propertize (or (gethash "author" m) "?") 'face 'diff-review-comment-author)
                        "  " (propertize (diff-review--when (gethash "at" m)) 'face 'shadow))))
         (replies (or (gethash "replies" comment) []))
         (lines
          (list (concat
                 (diff-review--glyph "💬" "#") " " (funcall who comment) "  "
                 (if open
                     (and first
                          (propertize (if (= first last) (format "line %d" first)
                                        (format "lines %d-%d" first last))
                                      'face 'shadow))
                   (concat (truncate-string-to-width (diff-review--first-line comment)
                                                     width nil nil (diff-review--glyph "…" "..."))
                           (and (> (length replies) 0)
                                (propertize (format "  +%d" (length replies)) 'face 'shadow))))
                 (and (gethash "resolved" comment)
                      (propertize (concat "  " (diff-review--glyph "✓" "*") " resolved")
                                  'face 'success))
                 (and stale (propertize "  outdated" 'face 'warning))))))
    (when open
      (dolist (line (diff-review--body-lines (gethash "body" comment) width))
        (push (concat "  " line) lines))
      (seq-doseq (reply replies)
        (push (concat "  " (funcall who reply)) lines)
        (dolist (line (diff-review--body-lines (gethash "body" reply) width))
          (push (concat "    " line) lines)))
      (when (and diff-review-file-header-keys diff-review-file-mode)
        (push (concat "  " (diff-review--key-hints
                            '((diff-review-reply . "reply")
                              (diff-review-toggle-resolved . "resolve")
                              (diff-review-delete-comment . "delete"))))
              lines)))
    (let ((text (concat (mapconcat (lambda (line) (concat bar line)) (nreverse lines) "\n")
                        "\n")))
      (add-face-text-property 0 (length text) 'diff-review-comment t text)
      text)))

(defun diff-review--thread-overlays ()
  "This buffer's thread overlays, by position."
  (sort (seq-filter (lambda (o) (and (overlay-get o 'diff-review-comment) (overlay-buffer o)))
                    diff-review--overlays)
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun diff-review--thread-span (overlay)
  "The lines thread OVERLAY covers, (FIRST . LAST)."
  (cons (line-number-at-pos (overlay-start overlay) t)
        (line-number-at-pos (max (overlay-start overlay) (1- (overlay-end overlay))) t)))

(defun diff-review--redraw-thread (overlay open)
  "Draw thread OVERLAY: in full when OPEN, its lines tinted; else as one line.
A file whose last line has no newline gets one drawn, so the thread
still hangs under that line."
  (pcase-let ((`(,first . ,last) (diff-review--thread-span overlay))
              (ends-a-line (save-excursion (goto-char (overlay-end overlay)) (bolp))))
    (overlay-put overlay 'diff-review-open open)
    (overlay-put overlay 'face
                 (and open (diff-review--tint 'diff-review-comment-range 'diff-review-comment)))
    (overlay-put overlay 'after-string
                 (concat (if ends-a-line "" "\n")
                         (diff-review--thread-string (overlay-get overlay 'diff-review-comment)
                                                     first last
                                                     (overlay-get overlay 'diff-review-stale)
                                                     open)))))

(defun diff-review--decorate-comments ()
  "Draw this file's threads, each under the last line of its range.
The thread is the range overlay's after-string, which Emacs shows before
a before-string at the same place, so the next hunk's inline removed
lines stay below it.  The range gets a fringe bar, no tint: the code
keeps the look it had until the thread opens."
  (let* ((path diff-review--file-path)
         (comments (diff-review--in-review (diff-review--file-comments path)))
         (blob (and comments (diff-review--buffer-blob))))
    (dolist (comment comments)
      (when (diff-review--comment-lines comment)
        (pcase-let* ((`(,first ,last ,stale) (diff-review--thread-lines comment blob))
                     (overlay (make-overlay (diff-review--line-pos first)
                                            (diff-review--line-pos (1+ last)) nil t nil)))
          (overlay-put overlay 'diff-review-comment comment)
          (overlay-put overlay 'diff-review-stale stale)
          (overlay-put overlay 'priority 1)      ; the open tint over the added one
          (push overlay diff-review--overlays)
          (diff-review--bars (diff-review--line-pos first) (1+ (- last first))
                             (diff-review--fringe-mark 'diff-review-comment))
          (diff-review--redraw-thread overlay nil))))
    (diff-review--track-threads)))

(defun diff-review--threads-at-point ()
  "The thread overlays point is at: on their lines, or the line just under them."
  (seq-filter (lambda (o) (overlay-get o 'diff-review-comment))
              (overlays-in (max (point-min) (1- (line-beginning-position)))
                           (min (point-max) (1+ (line-end-position))))))

(defun diff-review--track-threads ()
  "Open the threads point is at and close the ones it left.
On `post-command-hook' in a file under review; nothing elsewhere."
  (when diff-review-file-mode
    (let ((at (diff-review--threads-at-point))
          (all (diff-review--in-review diff-review--threads-open)))
      (dolist (overlay (diff-review--thread-overlays))
        (let ((open (or all (and (memq overlay at) t))))
          (unless (eq open (overlay-get overlay 'diff-review-open))
            (diff-review--redraw-thread overlay open)))))))

(defun diff-review--goto-line (line)
  "Move to LINE, near the top of the window, opening the threads there."
  (goto-char (diff-review--line-pos line))
  (recenter diff-review-hunk-recenter)
  (diff-review--track-threads))

(defun diff-review--goto-thread (comment)
  "Move to where COMMENT is drawn in this buffer."
  (when-let* ((overlay (seq-find (lambda (o)
                                   (equal (gethash "id" (overlay-get o 'diff-review-comment))
                                          (gethash "id" comment)))
                                 (diff-review--thread-overlays))))
    (diff-review--goto-line (car (diff-review--thread-span overlay)))))

(defun diff-review--goto-comment (id)
  "Show comment ID where it is: its file at its lines.
The review buffer must be current."
  (let* ((comment (or (diff-review--comment id) (user-error "No comment %s" id)))
         (path (gethash "path" comment))
         (i (and path (diff-review--file-index path))))
    (cond (i (with-current-buffer (diff-review--open i) (diff-review--goto-thread comment)))
          (t (message "The review has no file %s" (or path "to show this in"))))))

(defun diff-review--region-lines ()
  "The lines the region covers, else the line at point, as (FIRST . LAST).
A region ending at the start of a line stops before it."
  (if (use-region-p)
      (let ((first (line-number-at-pos (region-beginning) t)))
        (cons first (max first (line-number-at-pos (max (region-beginning) (1- (region-end))) t))))
    (let ((line (line-number-at-pos nil t))) (cons line line))))

(defun diff-review--read-body (prompt)
  "Read a comment body in a buffer, with PROMPT; empty is refused."
  (let ((body (string-trim-right (read-string-from-buffer prompt ""))))
    (when (string-empty-p body) (user-error "Nothing to say"))
    body))

(defun diff-review--thread-here ()
  "The comment at point, asked for when there are several; else an error."
  (let ((comments (mapcar (lambda (o) (overlay-get o 'diff-review-comment))
                          (diff-review--threads-at-point))))
    (cond ((null comments) (user-error "No comment on this line"))
          ((null (cdr comments)) (car comments))
          (t (let* ((names (mapcar (lambda (c)   ; the id: two may read the same
                                     (cons (format "%s: %s  [%s]" (gethash "author" c)
                                                   (diff-review--first-line c) (gethash "id" c))
                                           c))
                                   comments))
                    (pick (completing-read "Which thread: " (mapcar #'car names) nil t)))
               (cdr (assoc pick names)))))))

(defun diff-review-comment ()
  "Comment on the region's lines, or the line at point."
  (interactive)
  (unless diff-review-file-mode (user-error "Not in a file under review"))
  (let* ((lines (diff-review--region-lines))
         (path diff-review--file-path)
         (blob (diff-review--buffer-blob))
         (body (diff-review--read-body
                (format "Comment on %s:%d%s" path (car lines)
                        (if (= (car lines) (cdr lines)) "" (format "-%d" (cdr lines)))))))
    (deactivate-mark)
    (diff-review--in-review (diff-review--add-comment path lines blob body))))

(defun diff-review--change-comment (comment fn)
  "Apply FN to the live table of COMMENT, then write.
The table at point may be stale once someone else's write is taken."
  (let ((id (gethash "id" comment)))
    (diff-review--in-review
      (diff-review--modify-comments
       (lambda ()
         (funcall fn (or (diff-review--comment id)
                         (user-error "The comment was deleted meanwhile"))))))))

(defun diff-review-reply ()
  "Reply to the comment thread at point."
  (interactive)
  (let* ((comment (diff-review--thread-here))
         (body (diff-review--read-body (format "Reply to %s" (gethash "author" comment)))))
    (diff-review--change-comment
     comment
     (lambda (c)
       (puthash "replies"
                (vconcat (or (gethash "replies" c) [])
                         (vector (diff-review--message (diff-review--author) body)))
                c)))))

(defun diff-review-toggle-resolved ()
  "Resolve the comment thread at point, or open it again."
  (interactive)
  (diff-review--change-comment
   (diff-review--thread-here)
   (lambda (c) (if (gethash "resolved" c) (remhash "resolved" c) (puthash "resolved" t c)))))

(defun diff-review-toggle-threads ()
  "Draw every thread of the review open, or only the ones at point again."
  (interactive)
  (diff-review--in-review
    (setq diff-review--threads-open (not diff-review--threads-open))
    (dolist (buffer (diff-review--file-buffers))
      (with-current-buffer buffer (diff-review--track-threads)))))

(defun diff-review-delete-comment ()
  "Delete the comment thread at point, replies included."
  (interactive)
  (let ((id (gethash "id" (diff-review--thread-here))))
    (when (y-or-n-p "Delete this comment thread? ")
      (diff-review--in-review
        (diff-review--modify-comments
         (lambda ()
           (setq diff-review--comments
                 (vconcat (seq-remove (lambda (c) (equal (gethash "id" c) id))
                                      diff-review--comments)))))))))

(defun diff-review--file-with-comments (from step)
  "The index of the nearest file with comments from FROM, going STEP, or nil."
  (diff-review--in-review
    (seq-find (lambda (i) (diff-review--file-comments (plist-get (diff-review--file i) :path)))
              (number-sequence (+ from step)
                               (if (> step 0) (1- (length diff-review--files)) 0)
                               step))))

(defun diff-review--walk-comments (step)
  "Go to the thread after point when STEP is 1, before it when -1, across files."
  (let* ((line (line-number-at-pos nil t))
         (forward (> step 0))
         (threads (if forward (diff-review--thread-overlays)
                    (reverse (diff-review--thread-overlays))))
         (next (seq-find (lambda (o)
                           (funcall (if forward #'> #'<) (car (diff-review--thread-span o)) line))
                         threads))
         (i (and (not next)
                 (diff-review--file-with-comments (diff-review--this-index) step))))
    (cond (next (diff-review--goto-line (car (diff-review--thread-span next))))
          (i (with-current-buffer (diff-review--in-review (diff-review--open i))
               (when-let* ((threads (diff-review--thread-overlays)))
                 (diff-review--goto-line
                  (car (diff-review--thread-span (if forward (car threads) (car (last threads)))))))))
          (t (message "No comment %s this one" (if forward "after" "before"))))))

(defun diff-review-next-comment ()
  "Go to the next comment thread, continuing into later files."
  (interactive)
  (diff-review--walk-comments 1))

(defun diff-review-previous-comment ()
  "Go to the previous comment thread, continuing into earlier files."
  (interactive)
  (diff-review--walk-comments -1))

(defun diff-review--hunk-number ()
  "1-based index of the hunk at or before point, or nil."
  (let ((line (line-number-at-pos)) (n 0) found)
    (dolist (hunk diff-review--hunks)
      (cl-incf n)
      (when (<= (diff-review--hunk-line hunk) line) (setq found n)))
    found))

(defun diff-review--key-hints (hints)
  "HINTS, a list of (COMMAND . LABEL), as keys and dimmed labels.
Each key is the one the active maps give, as `substitute-command-keys'
finds it, so the hints follow the user's bindings.  A prefix every key
shares is shown once, in front."
  (let* ((keys (mapcar (lambda (hint) (where-is-internal (car hint) nil t)) hints))
         (prefixes (mapcar (lambda (key) (and (> (length key) 1) (substring key 0 -1)))
                           keys))
         (prefix (and (car prefixes)
                      (seq-every-p (lambda (p) (equal p (car prefixes))) prefixes)
                      (car prefixes)))
         (key-text (lambda (key)
                     (propertize (key-description key)
                                 'face 'help-key-binding 'font-lock-face 'help-key-binding))))
    (concat
     (if prefix (concat (funcall key-text prefix) "  ") "")
     (string-join
      (cl-mapcar (lambda (key hint)
                   (concat (cond ((null key) ; M-x cmd, as substitute-command-keys has it
                                  (substitute-command-keys (format "\\[%s]" (car hint))))
                                 (prefix (funcall key-text (substring key -1)))
                                 (t (funcall key-text key)))
                           (propertize (concat " " (cdr hint)) 'face 'shadow)))
                 keys hints)
      (propertize " · " 'face 'shadow)))))

(defun diff-review--file-header ()
  "The header line of a file: its place, status, path, counts, hunk and keys."
  (let* ((hunk (diff-review--hunk-number))
         (hunks (length diff-review--hunks))
         (focus diff-review--focus)
         (keys (if diff-review-file-header-keys
                   (concat "   " (diff-review--key-hints
                                  '((diff-review-next-hunk . "next hunk")
                                    (diff-review-viewed-and-next . "viewed")
                                    (diff-review-comment . "comment")
                                    (diff-review-toggle-focus . "focus")
                                    (diff-review-overview . "overview"))))
                 "")))
    (let ((file (diff-review--this-file))   ; this buffer's, before entering the review
          (i (diff-review--this-index)))
      (diff-review--in-review
        (concat
         (propertize (format " %d/%d " (1+ i) (length diff-review--files)) 'face 'shadow)
         (propertize (plist-get file :status)
                     'face (diff-review--status-face (plist-get file :status)))
         " "
         (propertize (plist-get file :path) 'face 'bold)
         (if-let* ((old (plist-get file :old)))
             (propertize (format "  (was %s)" old) 'face 'shadow)
           "")
         "   "
         (diff-review--counts (plist-get file :added) (plist-get file :removed))
         (propertize (format "   hunk %s/%d" (or hunk "–") hunks) 'face 'shadow)
         "   "
         (if (diff-review--viewed-p file)
             (propertize (concat (diff-review--glyph "✓" "*") " viewed") 'face 'success)
           (propertize (concat (diff-review--glyph "○" "o") " not viewed") 'face 'shadow))
         (let ((verdict (diff-review--verdict-text)))
           (if (string-empty-p verdict) "" (concat "   " verdict)))
         (propertize (if focus "   focus" "") 'face 'shadow)
         keys)))))

(defun diff-review--goto-hunk (hunk)
  "Move to where HUNK starts, near the top of the window."
  (diff-review--goto-line (diff-review--hunk-line hunk)))

(defun diff-review-next-hunk ()
  "Go to the next hunk, continuing into the next file."
  (interactive)
  (let* ((line (line-number-at-pos))
         (next (seq-find (lambda (h) (> (diff-review--hunk-line h) line))
                         diff-review--hunks)))
    (cond (next (diff-review--goto-hunk next))
          (t (let ((i (diff-review--this-index)))
               (diff-review--in-review
                 (if (< i (1- (length diff-review--files)))
                     (diff-review--open (1+ i))
                   (message "Last hunk of the last file"))))))))

(defun diff-review-previous-hunk ()
  "Go to the previous hunk, continuing into the previous file."
  (interactive)
  (let* ((line (line-number-at-pos))
         (prev (seq-find (lambda (h) (< (diff-review--hunk-line h) line))
                         (reverse diff-review--hunks))))
    (cond (prev (diff-review--goto-hunk prev))
          (t (let ((i (diff-review--this-index)))
               (diff-review--in-review
                 (if (> i 0)
                     (progn
                       (diff-review--open (1- i))
                       (when diff-review--hunks
                         (diff-review--goto-hunk (car (last diff-review--hunks)))))
                   (message "First hunk of the first file"))))))))

(defun diff-review-viewed-and-next ()
  "Mark this file viewed and show the next unviewed one."
  (interactive)
  (diff-review--in-review
    (diff-review--set-viewed (diff-review--this-file) t)
    (diff-review-next-unviewed)))

(defun diff-review-toggle-focus ()
  "Fold or unfold the unchanged lines of this file."
  (interactive)
  (setq diff-review--focus (not diff-review--focus))
  (diff-review--decorate))

(defun diff-review-toggle-inline-removed ()
  "Show or hide the removed lines, in this and later files of the review."
  (interactive)
  (diff-review--in-review
    (setq diff-review--inline-removed (not diff-review--inline-removed)))
  (diff-review--decorate))

(defun diff-review--next-error (n &optional reset)
  "Walk N hunks for `next-error', back when N is negative.
RESET starts from the first hunk of the file."
  ;; `next-error' calls this in the file it last walked from, which a walk
  ;; into the next file leaves behind: walk the file the review shows.
  (let ((review diff-review--review)
        (window (diff-review--main-window)))
    (if (with-current-buffer (window-buffer window)
          (and diff-review-file-mode (eq diff-review--review review)))
        (progn
          (select-window window)
          (when (and reset diff-review--hunks)
            (diff-review--goto-hunk (car diff-review--hunks))
            (setq n (max 0 (1- n))))
          (dotimes (_ (abs n))
            (with-current-buffer (window-buffer)   ; a step into another file shows it there
              (if (< n 0) (diff-review-previous-hunk) (diff-review-next-hunk)))))
      (diff-review--in-review (diff-review--open diff-review--index)))))

(defun diff-review--set (var value)
  "Set VAR to VALUE in this buffer until the review ends."
  (unless (assq var diff-review--saved)    ; on again for the next visit
    (push (cons var (cons (local-variable-p var) (symbol-value var))) diff-review--saved))
  (set (make-local-variable var) value))

(defvar-keymap diff-review-file-command-map
  :doc "The commands of `diff-review-file-mode', under `diff-review-command-prefix'."
  :repeat t
  "n" #'diff-review-next-hunk
  "p" #'diff-review-previous-hunk
  "v" #'diff-review-viewed-and-next
  "d" #'diff-review-toggle-inline-removed
  "z" #'diff-review-toggle-focus
  "c" #'diff-review-comment
  "r" #'diff-review-reply
  "x" #'diff-review-toggle-resolved
  "D" #'diff-review-delete-comment
  "o" #'diff-review-toggle-threads
  "N" #'diff-review-next-comment
  "P" #'diff-review-previous-comment
  "t" #'diff-review-toggle-sidebar
  "l" #'diff-review-overview
  "q" #'diff-review-quit)

(defvar diff-review-file-mode-map (make-sparse-keymap)
  "Keymap for `diff-review-file-mode'.
It holds `diff-review-file-command-map' under `diff-review-command-prefix'.")

;; A minor mode on the file's own buffer: C-c and a letter or a control
;; character belong to the user and the major mode, C-c and punctuation to
;; minor modes, so the commands sit under one such key.
(defcustom diff-review-command-prefix "C-c _"
  "Prefix key for the commands of `diff-review-file-mode'.
Nil binds no keys: bind `diff-review-file-command-map' or the commands
yourself."
  :type '(choice (const :tag "No keys" nil) key)
  :set (lambda (symbol value)
         (set-default symbol value)
         (setcdr diff-review-file-mode-map nil)
         (when value
           (keymap-set diff-review-file-mode-map value diff-review-file-command-map))))

(defun diff-review--before-major-mode ()
  "Turn the review off before a major mode change kills its state.
A revert does that: nothing of the review stays behind in the buffer."
  (diff-review-file-mode -1))

(define-minor-mode diff-review-file-mode
  "Review decorations and keys in a file of a review.
\\<diff-review-file-mode-map>\\[diff-review-next-hunk] and \\[diff-review-previous-hunk] walk the
hunks, across files.  \\[diff-review-viewed-and-next] marks the file viewed and
moves on, \\[diff-review-toggle-focus] folds the unchanged lines,
\\[diff-review-toggle-inline-removed] hides the removed lines,
\\[diff-review-toggle-sidebar] shows or hides the sidebar,
\\[diff-review-overview] goes back to the overview and
\\[diff-review-quit] ends the review.
\\[diff-review-comment] comments on the region or the line, \\[diff-review-reply] replies to the
thread at point, \\[diff-review-toggle-resolved] resolves it, \\[diff-review-delete-comment] deletes it,
\\[diff-review-next-comment] and \\[diff-review-previous-comment] walk the threads, across files,
and \\[diff-review-toggle-threads] opens them all, or only the one at point again.
\\<global-map>\\[next-error] and \\[previous-error] walk the hunks too.  With `repeat-mode'
the prefix is needed once, for a run of these keys.

\\{diff-review-file-mode-map}"
  :lighter " Review"
  (if diff-review-file-mode
      (progn
        (setq diff-review--hunks
              (if diff-review--deleted
                  (list (list :line 1 :count 0 :removed nil))
                (diff-review--in-review (diff-review--parse-hunks (diff-review--this-file))))
              diff-review--texts nil)
        (diff-review--decorate)
        (add-hook 'post-command-hook #'diff-review--track-threads nil t)
        (add-hook 'change-major-mode-hook #'diff-review--before-major-mode nil t)
        (diff-review--set 'next-error-function #'diff-review--next-error)
        (when diff-review-file-header
          (diff-review--set 'header-line-format
                            `(:eval (funcall ',(if (functionp diff-review-file-header)
                                                   diff-review-file-header
                                                 #'diff-review--file-header))))))
    (diff-review--undecorate)
    (remove-hook 'post-command-hook #'diff-review--track-threads t)
    (remove-hook 'change-major-mode-hook #'diff-review--before-major-mode t)
    (pcase-dolist (`(,var ,local . ,value) diff-review--saved)
      (if local (set var value) (kill-local-variable var)))
    (kill-local-variable 'diff-review--saved)
    (setq diff-review--review nil)))

;;;; Evil

;; Normal state shadows n, p and g, so the lists get gj, gk and gr in their
;; place, as evil-collection does, and TAB, which motion state takes for
;; `evil-jump-forward'.  The rest mirror the Emacs keys.  A file under
;; review is being edited: its keys are the prefix, which evil leaves alone.

(defcustom diff-review-evil-keys t
  "Non-nil starts the lists in Evil's normal state, with keys for it.
Set it before Evil or this package loads; nil leaves Evil untouched."
  :type 'boolean)

(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")

(with-eval-after-load 'evil
  (when diff-review-evil-keys
    (dolist (mode '(diff-review-mode diff-review-sidebar-mode))
      (evil-set-initial-state mode 'normal))
    (dolist (map (list diff-review-mode-map diff-review-sidebar-mode-map))
      (evil-define-key* 'normal map
        (kbd "RET") #'diff-review-open-at-point
        (kbd "TAB") #'magit-section-toggle
        "gj" #'diff-review-next-file
        "gk" #'diff-review-previous-file
        "gr" #'diff-review-refresh
        "u" #'diff-review-next-unviewed
        "v" #'diff-review-toggle-viewed
        "t" #'diff-review-toggle-sidebar
        "A" #'diff-review-approve
        "C" #'diff-review-request-changes
        "q" (if (eq map diff-review-sidebar-mode-map)
                #'diff-review-toggle-sidebar
              #'diff-review-quit)))))

;;;; Creating a review

;;;###autoload
(defun diff-review-find-file (file &optional root base head)
  "Visit review FILE, creating it first when it does not exist.
A new file reviews the repository at ROOT, by default the one holding
FILE, against BASE, by default `diff-review-default-base'.  It reviews
revision HEAD when given, else the worktree.  Where FILE lives is the
caller's choice."
  (interactive "FReview file: ")
  (unless (file-exists-p file)
    (make-directory (file-name-directory (expand-file-name file)) t)
    (let ((top (if root
                   (file-name-as-directory (expand-file-name root))
                 (diff-review--toplevel (file-name-directory (expand-file-name file))))))
      (setq base (or base (and top (diff-review--default-base top)))))
    (with-temp-file file
      (insert (json-serialize
               `((format . ,diff-review-format)
                 ,@(and root `((root . ,(file-name-as-directory (expand-file-name root)))))
                 ,@(and base `((base . ,base)))
                 ,@(and head `((head . ,head)))
                 (viewed . ,(make-hash-table)))))
      (json-pretty-print-buffer)
      (insert "\n")))
  (find-file file))

(defun diff-review--root-here ()
  "The root of the repository `default-directory' is in, or an error."
  (or (diff-review--toplevel default-directory)
      (user-error "Not in a git repository")))

(defun diff-review--branch-file (root branch)
  "The review file of BRANCH in the repository at ROOT; a slash makes a folder."
  (expand-file-name (concat branch ".json")
                    (expand-file-name "review/" (diff-review--common-dir root))))

(defun diff-review--range-file (root old new)
  "The review file of the commits OLD to NEW, OLD included, at ROOT.
Named by their abbreviated hashes, beside the branches' files, so every
worktree finds one range's review and no branch's file is taken for it."
  (expand-file-name (apply #'format "%s..%s.json"
                           (mapcar (lambda (rev)
                                     (diff-review--git-at root "rev-parse" "--short"
                                                          (concat rev "^{commit}")))
                                   (list old new)))
                    (expand-file-name "review/" (diff-review--common-dir root))))

(defun diff-review--read-branch ()
  "Read a branch to review: one another worktree has checked out, or any typed."
  (let* ((root (diff-review--root-here))
         (here (file-truename root))
         (branches (delq nil (mapcar (lambda (w)
                                       (and (not (equal (file-truename (plist-get w :path)) here))
                                            (plist-get w :branch)))
                                     (diff-review--worktrees root))))
         (branch (completing-read "Review branch: " branches)))
    (when (string-empty-p branch) (user-error "No branch to review"))
    branch))

;;;###autoload
(defun diff-review-branch (branch)
  "Review BRANCH against `diff-review-default-base', from this repository.
Interactively, pick among the branches the repository's other worktrees
have checked out, or type any.  The review file is review/BRANCH.json
under the common git directory, which every worktree shares and git never
commits, so main's clone and the branch's worktree open the same one: the
branch's pull request, kept through its commits.  The branch is the head,
so the review follows it, and its files are its commits', not a
worktree's uncommitted edits, visited in a worktree that has the branch
checked out."
  (interactive (list (diff-review--read-branch)))
  (let ((root (diff-review--root-here)))
    ;; The main worktree as the root: the file outlives the worktree it was made from.
    (diff-review-find-file (diff-review--branch-file root branch)
                           (or (diff-review--main-worktree root) root) nil branch)))

(provide 'diff-review)
;;; diff-review.el ends here
