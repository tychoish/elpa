;;; magit-dash-open.el --- Interactive git repository picker for magit-dash -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides `magit-dash-open-repo', a context-aware repository picker using
;; `annotated-completing-read'.  Behaviour adapts to the calling context:
;;
;;   - Current directory is a git repo (non-magit buffer):
;;       Only the repo's worktrees and submodules are offered.
;;   - Current buffer is a magit-status buffer:
;;       Scans from the parent of the current repo; also merges `~/' at
;;       depth 1 so sibling repos and home-level repos appear together.
;;   - Otherwise:
;;       Scans subdirectories of `default-directory'.
;;
;; In all cases live magit-status buffers are included in the candidate list
;; and marked "[open]"; selecting one switches to it directly.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)

(require 'magit)
(require 'projectile)

(require 'annotated-completing-read)

;;; Configuration

(defvar magit-dash-open-scan-depth 2
  "How many directory levels to scan for git repositories.
Worktrees and submodules are always enumerated via git regardless of depth.")

;;; Caching

(defvar magit-dash-open--info-cache (make-hash-table :test #'equal)
  "Per-repo git info cache.
Keys are absolute paths.  Values are plists:
  :head-hash        HEAD commit hash at cache-fill time
  :branch           current branch name
  :upstream         configured upstream ref or nil
  :upstream-display short display string for upstream (handles origin/ fallback)
  :ahead            commits ahead of upstream (integer)
  :behind           commits behind upstream (integer)
  :dirty            non-nil when working tree has uncommitted changes")

(defun magit-dash-open-cache-reset ()
  "Clear the git info cache used by `magit-dash-open-repo'."
  (interactive)
  (setq magit-dash-open--info-cache (make-hash-table :test #'equal))
  (message "magit-dash-open-repo: cache cleared"))

;;; Git subprocess helpers

(defun magit-dash-open--run-git (dir &rest args)
  "Run git in DIR with ARGS; return output lines or nil on error."
  (ignore-errors
    (let ((default-directory dir))
      (apply #'magit-git-lines args))))

(defun magit-dash-open--git-p (dir)
  "Return non-nil if DIR is the root of a git repository or worktree."
  (file-exists-p (expand-file-name ".git" dir)))

;;; Git info collection

(defun magit-dash-open--git-info (dir)
  "Return a git info plist for the repo at DIR.
Cached keyed by HEAD hash; stale entries are evicted on the next call.
The plist includes :upstream-display, which handles the origin/BRANCH
fallback so the annotation function never calls git directly."
  (let* ((head-hash (car (magit-dash-open--run-git dir "rev-parse" "HEAD")))
         (cached (map-elt magit-dash-open--info-cache dir)))
    (if (and cached (equal (map-elt cached :head-hash) head-hash))
        cached
      (let* ((branch (or (let ((default-directory dir))
                           (magit-get-current-branch))
                         "HEAD"))
             (upstream-raw (car (magit-dash-open--run-git
                                  dir "rev-parse" "--abbrev-ref"
                                  "--symbolic-full-name" "@{u}")))
             (upstream (when (and upstream-raw (not (string= upstream-raw "@{u}")))
                         upstream-raw))
             (upstream-display
              (or (when upstream
                    (replace-regexp-in-string "^origin/" "" upstream))
                  (when (magit-dash-open--run-git
                          dir "rev-parse" "--verify" "--quiet"
                          (format "origin/%s" branch))
                    branch)))
             (ahead 0)
             (behind 0))
        (when upstream
          (when-let* ((count-line (car (magit-dash-open--run-git
                                         dir "rev-list" "--count" "--left-right"
                                         (format "HEAD...%s" upstream)))))
            (when (string-match "\\([0-9]+\\)\t\\([0-9]+\\)" count-line)
              (setq ahead (string-to-number (match-string 1 count-line))
                    behind (string-to-number (match-string 2 count-line))))))
        (let ((info (list :head-hash head-hash
                          :branch branch
                          :upstream upstream
                          :upstream-display upstream-display
                          :ahead ahead
                          :behind behind
                          :dirty (let ((default-directory dir))
                                   (magit-anything-modified-p)))))
          (setf (map-elt magit-dash-open--info-cache dir) info)
          info)))))

;;; Directory scanning

(defun magit-dash-open--subdirs (dir)
  "Return absolute paths of non-hidden immediate subdirectories of DIR."
  (when (file-accessible-directory-p dir)
    (thread-last (directory-files dir nil "^[^.]" t)
      (seq-map (lambda (name) (expand-file-name name dir)))
      (seq-filter #'file-directory-p))))

(defun magit-dash-open--worktree-paths (dir)
  "Return list of additional worktree absolute paths for git repo at DIR."
  (thread-last (magit-dash-open--run-git dir "worktree" "list" "--porcelain")
               (seq-filter (lambda (l) (string-prefix-p "worktree " l)))
               (cdr)
               (seq-map (lambda (l) (substring l 9)))))

(defun magit-dash-open--submodule-paths (dir)
  "Return list of initialized submodule absolute paths for git repo at DIR."
  (thread-last (magit-dash-open--run-git dir "submodule" "status")
    (seq-filter (lambda (l) (not (string-empty-p l))))
    (seq-map (lambda (line)
               (when (string-match "^[ +U][0-9a-f]+ \\([^ ]+\\)" line)
                 (expand-file-name (match-string 1 line) dir))))
    (seq-remove #'null)))

(defun magit-dash-open--collect-self (dir)
  "Return (path . kind) pairs for worktrees and submodules of git repo at DIR."
  (append
   (seq-map (lambda (p) (cons p 'worktree)) (magit-dash-open--worktree-paths dir))
   (seq-map (lambda (p) (cons p 'submodule)) (magit-dash-open--submodule-paths dir))))

(defun magit-dash-open--collect-deep (root depth)
  "Return (path . kind) pairs for git repos under ROOT up to DEPTH levels.
Kind is one of: `repo', `worktree', `submodule'.
Plain (non-git) directories are traversed for scanning but never returned as
candidates.  Worktrees and submodules are enumerated for every repo found."
  (let* ((subdirs (magit-dash-open--subdirs root))
         (repos (seq-filter #'magit-dash-open--git-p subdirs))
         (plain (seq-remove #'magit-dash-open--git-p subdirs)))
    (append
     (seq-map (lambda (p) (cons p 'repo)) repos)
     (seq-mapcat (lambda (r)
                   (seq-map (lambda (p) (cons p 'worktree))
                            (magit-dash-open--worktree-paths r)))
                 repos)
     (seq-mapcat (lambda (r)
                   (seq-map (lambda (p) (cons p 'submodule))
                            (magit-dash-open--submodule-paths r)))
                 repos)
     (when (> depth 0)
       (seq-mapcat (lambda (d) (magit-dash-open--collect-deep d (1- depth)))
                   plain)))))

;;; Projectile integration

(defun magit-dash-open--projectile-candidates ()
  "Return (path . `repo') pairs from projectile known-projects, limited to git repos."
  (when (and (fboundp 'projectile-load-known-projects)
             (boundp 'projectile-known-projects))
    (ignore-errors (projectile-load-known-projects))
    (thread-last projectile-known-projects
      (seq-map (lambda (p) (directory-file-name (expand-file-name p))))
      (seq-filter #'file-directory-p)
      (seq-filter #'magit-dash-open--git-p)
      (seq-map (lambda (p) (cons p 'repo))))))

;;; Open-buffer detection

(defun magit-dash-open--open-status-buffers ()
  "Return alist of (normalized-path . buffer) for all live magit-status buffers."
  (thread-last (buffer-list)
    (seq-filter (lambda (b) (eq (buffer-local-value 'major-mode b) 'magit-status-mode)))
    (seq-map (lambda (b)
               (cons (file-name-as-directory
                      (expand-file-name
                       (buffer-local-value 'default-directory b)))
                     b)))))

(defun magit-dash-open--find-open-buffer (path open-buffers)
  "Return the live magit-status buffer for PATH from OPEN-BUFFERS, or nil."
  (let ((norm (file-name-as-directory (expand-file-name path))))
    (cdr (seq-find (lambda (pair) (file-equal-p (car pair) norm)) open-buffers))))

;;; Annotation

(defun magit-dash-open--annotation (path kind)
  "Return annotation string for PATH with KIND.
KIND is one of: `repo', `worktree', `submodule', `dir'.
Incorporates cached stats, CI status, and PR counts from `magit-dash-gh--cache'
when available."
  (if (eq kind 'dir)
      "directory"
    (let* ((info (condition-case nil (magit-dash-open--git-info path) (error nil)))
           (cached-stats (when (fboundp 'magit-dash-gh--cache-get)
                           (magit-dash-gh--cache-get path :stats)))
           (cached-ci (when (fboundp 'magit-dash-gh--cache-get)
                        (magit-dash-gh--cache-get path :ci-status)))
           (cached-prs (when (fboundp 'magit-dash-gh--cache-get)
                         (magit-dash-gh--cache-get path :pr-counts)))
           (branch (or (map-elt info :branch)
                       (plist-get cached-stats :branch)
                       "?"))
           (upstream-display (map-elt info :upstream-display))
           (ahead (or (map-elt info :ahead)
                      (plist-get cached-stats :ahead)
                      0))
           (behind (or (map-elt info :behind)
                       (plist-get cached-stats :behind)
                       0))
           (dirty (or (map-elt info :dirty)
                      (plist-get cached-stats :dirty)))
           (kind-str (symbol-name kind))
           (arrow-part (if upstream-display
                           (format "%s → %s" branch upstream-display)
                         branch))
           (count-part (cond
                        ((and (> ahead 0) (> behind 0)) (format "+%d/-%d" ahead behind))
                        ((> ahead 0) (format "+%d" ahead))
                        ((> behind 0) (format "-%d" behind))
                        (t nil)))
           (dirty-part (when dirty "*"))
           (ci-part (when cached-ci
                      (let ((conclusion (plist-get cached-ci :conclusion))
                            (status (plist-get cached-ci :status)))
                        (cond
                         ((equal conclusion "success") "CI:✓")
                         ((equal conclusion "failure") "CI:✗")
                         ((equal status "in_progress") "CI:…")
                         (conclusion (format "CI:%s" conclusion))
                         (t nil)))))
           (pr-part (when (and (consp cached-prs) (> (car cached-prs) 0))
                      (format "%d PR%s" (car cached-prs) (if (= (car cached-prs) 1) "" "s")))))
      (string-join (seq-remove #'null (list kind-str arrow-part count-part dirty-part ci-part pr-part))
                   "  "))))
;;; Entry point

;;;###autoload
(defun magit-dash-open-repo (&optional root)
  "Select a repository and open its magit status buffer.

Context-aware collection:
- In a magit-status buffer: scans from the parent of the current repo and
  merges `~/' at depth 1, so sibling repos and home-level repos appear together.
- In a directory that is itself a git repo: only the repo's worktrees and
  submodules are offered (use a prefix arg to override and scan subdirs).
- Otherwise: scans subdirectories of `default-directory'.

Live magit-status buffers are always included and marked \"[open]\"; selecting
one switches to it directly without reopening it.

With a prefix argument, prompt for ROOT and scan it normally regardless of
the above context rules."
  (interactive
   (list (when current-prefix-arg
           (read-directory-name "Root directory: " default-directory))))
  (let* ((forced-root (when root (expand-file-name root)))
         (in-magit-p (and (not forced-root) (derived-mode-p 'magit-status-mode)))
         (current-dir (expand-file-name default-directory))
         (current-repo-p (and (not forced-root) (magit-dash-open--git-p current-dir)))
         ;; Determine scan root and collect primary entries
         (entries
          (cond
           (forced-root
            (magit-dash-open--collect-deep forced-root magit-dash-open-scan-depth))
           (in-magit-p
            (let* ((parent (file-name-directory (directory-file-name current-dir)))
                   (home (expand-file-name "~/"))
                   (from-parent (magit-dash-open--collect-deep
                                 parent magit-dash-open-scan-depth))
                   (from-home (unless (file-equal-p parent home)
                                (magit-dash-open--collect-deep home 1))))
              (append from-parent from-home)))
           (current-repo-p
            (let ((self (magit-dash-open--collect-self current-dir)))
              ;; Fall back to subdirectory scan if no worktrees or submodules exist
              (or self (magit-dash-open--collect-deep
                        current-dir magit-dash-open-scan-depth))))
           (t (magit-dash-open--collect-deep current-dir magit-dash-open-scan-depth))))
         ;; Always merge projectile and live status buffers
         (projectile-entries (when (and (featurep 'projectile) projectile-mode)
                               (magit-dash-open--projectile-candidates)))
         (open-buffers (magit-dash-open--open-status-buffers))
         ;; Priority: local scan > projectile; open buffers fill gaps
         (path-map (map-into entries '(hash-table :test equal))))
    (map-do (lambda (path kind)
              (unless (map-contains-key path-map path)
                (setf (map-elt path-map path) kind)))
            projectile-entries)
    ;; Ensure every open magit-status buffer appears, even if outside the scan
    (map-do (lambda (norm-path _buf)
              (let ((dir (directory-file-name norm-path)))
                (unless (map-contains-key path-map dir)
                  (setf (map-elt path-map dir) 'repo))))
            open-buffers)
    ;; Ensure registered repositories from magit-dash-repo-list are included
    (when (bound-and-true-p magit-dash-repo-list)
      (dolist (r magit-dash-repo-list)
        (let ((dir (directory-file-name (expand-file-name (magit-dash-repo-path r)))))
          (unless (map-contains-key path-map dir)
            (setf (map-elt path-map dir) (if (magit-dash-repo-worktree r) 'worktree 'repo))))))
    (when (map-empty-p path-map)
      (user-error "No directories found"))
    (when-let* ((full-path (annotated-completing-read
                            (map-into
                             (map-apply
                              (lambda (path kind)
                                (let* ((display (abbreviate-file-name path))
                                       (already-open (magit-dash-open--find-open-buffer path open-buffers))
                                       (base (magit-dash-open--annotation path kind))
                                       (annotation (if already-open (concat base "  [open]") base)))
                                  (cons display (cons annotation path))))
                              path-map)
                             '(hash-table :test equal))
                            :prompt "Open repo: "
                            :require-match t
                            :category 'file)))
      (let ((existing (magit-dash-open--find-open-buffer full-path open-buffers)))
        (if existing
            (switch-to-buffer existing)
          (magit-status-setup-buffer full-path))))))

(provide 'magit-dash-open)
;;; magit-dash-open.el ends here
