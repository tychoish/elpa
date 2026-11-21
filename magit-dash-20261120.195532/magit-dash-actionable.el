;;; magit-dash-actionable.el --- Actions needing attention -*- lexical-binding: t; -*-

;;; Commentary:
;; An overlay and filtering of the `magit-dash-repos' list view:
;; retains full dashboard functionality (status, push, pull, stage,
;; transient menu) while filtering to repositories that need attention
;; (uncommitted changes, unpushed or unpulled commits, and authored PRs
;; needing feedback/fixes), displaying a "Why" column in place of
;; Type and Sync.

;;; Code:

(require 'magit-dash)
(require 'magit-dash-pr-status)
(require 'tabulated-list)
(require 'transient)
(require 'seq)

(declare-function magit-dash-gh-ci--failure-p "magit-dash-gh-ci")

(defvar magit-dash-actionable--why-cache (make-hash-table :test #'equal)
  "Cache mapping repository paths to their why/attention summary strings.")

(defvar magit-dash-actionable--pr-cache (make-hash-table :test #'equal)
  "Cache mapping repository paths to associated PR plists needing attention.")

(defun magit-dash-actionable--repo-entries (repo)
  "Return action rows for REPO based on its cached or collected stats."
  (let* ((stats (magit-dash--get-stats repo))
         (path (magit-dash-repo-path repo))
         (ci (when (fboundp 'magit-dash-gh--cache-get)
               (magit-dash-gh--cache-get path :ci-status)))
         (ci-conclusion (plist-get ci :conclusion))
         (ci-failing (or (and (fboundp 'magit-dash-gh-ci--failure-p)
                              (magit-dash-gh-ci--failure-p ci-conclusion))
                         (member ci-conclusion '("failure" "timed_out" "startup_failure")))))
    (seq-map (lambda (item)
               (list (list :repo repo :kind (car item))
                     (vector (magit-dash-repo-name repo) (cdr item))))
             (delq nil
                   (list (when (> (or (plist-get stats :ahead) 0) 0)
                           (cons 'push (format "%s unpushed commits" (plist-get stats :ahead))))
                         (when (> (or (plist-get stats :behind) 0) 0)
                           (cons 'pull (format "%s unpulled commits" (plist-get stats :behind))))
                         (when (plist-get stats :dirty)
                           (cons 'changes "Uncommitted changes"))
                         (when ci-failing
                           (cons 'ci (format "CI failing (%s)" ci-conclusion))))))))

(defun magit-dash-actionable--partial-marker (pr)
  "Return \" [partial data]\" when PR's nested review/check lists were truncated.
The PR keeps the bucket computed from the data that was fetched; the
marker tells the reader that bucket may be incomplete."
  (if (plist-get pr :truncated)
      " [partial data]"
    ""))

(defun magit-dash-actionable--pr-entries (prs)
  "Return action rows for PRS that need author attention."
  (seq-map (lambda (pr)
             (list (list :pr pr)
                   (vector (format "%s#%s" (plist-get pr :repo) (plist-get pr :number))
                           (format "%s: %s%s" (plist-get pr :bucket) (plist-get pr :title)
                                   (magit-dash-actionable--partial-marker pr)))))
           (seq-filter (lambda (pr) (memq (plist-get pr :bucket) '(fix-ci threads blocked))) prs)))

(defun magit-dash-actionable--format-why (repo)
  "Format the reason why REPO needs attention."
  (or (and (bound-and-true-p magit-dash-actionable--why-cache)
           (gethash (magit-dash-repo-path repo) magit-dash-actionable--why-cache))
      (let* ((stats (magit-dash--get-stats-fast repo))
             (parts nil))
        (when (plist-get stats :dirty)
          (push "Uncommitted changes" parts))
        (when (> (or (plist-get stats :ahead) 0) 0)
          (push (format "%d unpushed" (plist-get stats :ahead)) parts))
        (when (> (or (plist-get stats :behind) 0) 0)
          (push (format "%d unpulled" (plist-get stats :behind)) parts))
        (when (bound-and-true-p magit-dash-actionable--pr-cache)
          (when-let* ((pr (gethash (magit-dash-repo-path repo) magit-dash-actionable--pr-cache)))
            (push (format "PR #%s: %s (%s)%s"
                          (plist-get pr :number)
                          (plist-get pr :title)
                          (or (plist-get pr :bucket) "attention")
                          (magit-dash-actionable--partial-marker pr))
                  parts)))
        (string-join (nreverse parts) ", "))))

(defun magit-dash-actionable--match-pr-to-repo (pr repos)
  "Find repository in REPOS matching PR slug or name, or nil."
  (let ((slug (plist-get pr :repo)))
    (seq-find
     (lambda (r)
       (let* ((name (magit-dash-repo-name r))
              (path (magit-dash-repo-path r))
              (upstream (and (fboundp 'magit-dash-repo-upstream-repo)
                             (magit-dash-repo-upstream-repo r)))
              (repo-field (and (fboundp 'magit-dash-repo-repo)
                               (magit-dash-repo-repo r)))
              (r-slug (or (and (fboundp 'magit-dash-ci--repo-slug)
                               (ignore-errors (magit-dash-ci--repo-slug r)))
                          upstream repo-field)))
         (or (and slug r-slug (string-equal slug r-slug))
             (and slug (string-equal slug name))
             (and slug (string-suffix-p (concat "/" name) slug))
             (and slug (string-equal slug (file-name-nondirectory (directory-file-name path)))))))
     repos)))

(defvar-local magit-dash-actionable--refresh-id 0
  "Counter identifying the latest refresh; stale async results are dropped.")

(defvar-local magit-dash-actionable--cancel nil
  "Function that cancels the in-flight PR collection of the latest refresh.")

(defun magit-dash-actionable--render (prs-raw problems)
  "Render the actionable view from PRS-RAW, noting PROBLEMS in the header.
PROBLEMS is nil or a string describing PR collection failures or progress."
  (clrhash magit-dash-actionable--why-cache)
  (clrhash magit-dash-actionable--pr-cache)
  (let* ((all-repos (or (and (fboundp 'magit-dash--all-repos)
                             (magit-dash--all-repos))
                        magit-dash-repo-list))
         (attention-prs (seq-filter (lambda (pr)
                                      (memq (plist-get pr :bucket) '(fix-ci threads blocked)))
                                    prs-raw))
         (actionable-repos nil))
    ;; 1. Evaluate registered repos for local changes and matched PRs
    (seq-do
     (lambda (repo)
       (let* ((path (magit-dash-repo-path repo))
              (matched-pr (seq-find (lambda (pr)
                                      (magit-dash-actionable--match-pr-to-repo pr (list repo)))
                                    attention-prs))
              (why-parts (append
                          (seq-map (lambda (item) (aref (cadr item) 1))
                                   (magit-dash-actionable--repo-entries repo))
                          (when matched-pr
                            (list (format "PR #%s: %s (%s)%s"
                                          (plist-get matched-pr :number)
                                          (plist-get matched-pr :title)
                                          (plist-get matched-pr :bucket)
                                          (magit-dash-actionable--partial-marker matched-pr)))))))
         (when matched-pr
           (setf (map-elt magit-dash-actionable--pr-cache path) matched-pr))
         (when why-parts
           (setf (map-elt magit-dash-actionable--why-cache path) (string-join why-parts ", "))
           (push repo actionable-repos))))
     all-repos)
    ;; 2. Include any unmatched actionable PRs as synthetic repo entries
    (seq-do
     (lambda (pr)
       (unless (seq-some (lambda (r) (equal (magit-dash-repo-name r) (plist-get pr :repo)))
                         actionable-repos)
         (let ((path (plist-get pr :repo)))
           (setf (map-elt magit-dash-actionable--pr-cache path) pr)
           (setf (map-elt magit-dash-actionable--why-cache path)
                 (format "%s: %s%s" (plist-get pr :bucket) (plist-get pr :title)
                         (magit-dash-actionable--partial-marker pr)))
           (push (magit-dash-repo--make
                  :name (format "%s#%s" path (plist-get pr :number))
                  :path path)
                 actionable-repos))))
     attention-prs)
    (setq actionable-repos (nreverse actionable-repos))
    (setq tabulated-list-format (magit-dash--build-format actionable-repos))
    (tabulated-list-init-header)
    (setq header-line-format (and problems (format "PR status: %s" problems)))
    (setq tabulated-list-entries (seq-map #'magit-dash--build-entry actionable-repos))
    (tabulated-list-print t)))

(defun magit-dash-actionable--async-render (buffer id prs-raw problems)
  "Render PRS-RAW and PROBLEMS in BUFFER if refresh ID is still the latest."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (and (derived-mode-p 'magit-dash-actionable-mode)
                    (= id magit-dash-actionable--refresh-id))))
    (with-current-buffer buffer
      (magit-dash-actionable--render prs-raw problems))))

(defun magit-dash-actionable-refresh ()
  "Refresh the actionable view, filtering to repos and PRs needing attention.
Local results render immediately; PR status is collected asynchronously,
one query per GitHub account, and the view re-renders as each account
answers.  Accounts that fail or time out are named in the header line;
the others still show.  Starting a refresh cancels the previous one.
When invoked outside `magit-dash-actionable-mode', safely redirects to
the `*magit-dash-actions*' buffer if live, preventing accidental corruption
of other buffers."
  (interactive)
  (if (not (derived-mode-p 'magit-dash-actionable-mode))
      (if-let* ((buf (get-buffer "*magit-dash-actions*")))
          (if (and (buffer-live-p buf)
                   (with-current-buffer buf (derived-mode-p 'magit-dash-actionable-mode)))
              (with-current-buffer buf
                (magit-dash-actionable-refresh))
            (when (called-interactively-p 'interactive)
              (user-error "Buffer *magit-dash-actions* is not in magit-dash-actionable-mode")))
        (when (called-interactively-p 'interactive)
          (user-error "No *magit-dash-actions* buffer exists")))
    (let* ((buffer (current-buffer))
           (id (setq magit-dash-actionable--refresh-id (1+ magit-dash-actionable--refresh-id)))
           (partial nil))
      (when magit-dash-actionable--cancel
        (funcall magit-dash-actionable--cancel))
      (magit-dash-actionable--render nil "loading…")
      (setq magit-dash-actionable--cancel
            (magit-dash-pr-status-collect-all-async
             (lambda (prs)
               (magit-dash-actionable--async-render
                buffer id prs (and partial (string-join partial "; "))))
             (lambda (err)
               (magit-dash-actionable--async-render
                buffer id nil (format "unavailable: %s" err)))
             nil
             (lambda (errs)
               (setq partial errs))
             (lambda (prs notes pending)
               (magit-dash-actionable--async-render
                buffer id prs
                (string-join (cons (format "loading %d more account%s…"
                                           pending (if (= pending 1) "" "s"))
                                   notes)
                             "; "))))))))

(defun magit-dash-actionable--current-pr ()
  "Return the PR plist for the item at point, or nil."
  (let ((id (tabulated-list-get-id)))
    (cond
     ((and (listp id) (plist-get id :pr)) (plist-get id :pr))
     ((magit-dash-repo-p id)
      (and (bound-and-true-p magit-dash-actionable--pr-cache)
           (gethash (magit-dash-repo-path id) magit-dash-actionable--pr-cache)))
     (t nil))))

(defun magit-dash-actionable--repo ()
  "Return the repository for the action at point, or signal an error."
  (or (when-let* ((id (tabulated-list-get-id)))
        (cond
         ((magit-dash-repo-p id) id)
         ((and (listp id) (plist-get id :repo)) (plist-get id :repo))))
      (user-error "This row has no local repository")))

(defun magit-dash-actionable--repo-p ()
  "Return non-nil when the row at point has a local repository."
  (when-let* ((id (tabulated-list-get-id)))
    (cond
     ((magit-dash-repo-p id) t)
     ((and (listp id) (plist-get id :repo)) t)
     (t nil))))

(defun magit-dash-actionable--pr-p ()
  "Return non-nil when the row at point has an associated PR."
  (and (magit-dash-actionable--current-pr) t))

(defun magit-dash-actionable-status ()
  "Open Magit status for the repository at point."
  (interactive)
  (magit-status-setup-buffer (magit-dash-repo-path (magit-dash-actionable--repo))))

(defun magit-dash-actionable-pull ()
  "Open Magit's pull command for the repository at point."
  (interactive)
  (let ((default-directory (file-name-as-directory (magit-dash-repo-path (magit-dash-actionable--repo)))))
    (call-interactively #'magit-pull-from-upstream)))

(defun magit-dash-actionable-push ()
  "Open Magit's push command for the repository at point."
  (interactive)
  (let ((default-directory (file-name-as-directory (magit-dash-repo-path (magit-dash-actionable--repo)))))
    (call-interactively #'magit-push-current-to-pushremote)))

(defun magit-dash-actionable-open-pr ()
  "Open the pull request at point in a browser."
  (interactive)
  (if-let* ((pr (magit-dash-actionable--current-pr))
            (url (plist-get pr :url)))
      (browse-url url)
    (user-error "This row has no pull request URL")))

(transient-define-prefix magit-dash-actionable-menu ()
  "Actions for the item at point in the actionable dashboard."
  [["Repository"
    ("s" "Magit status" magit-dash-actionable-status :inapt-if-not magit-dash-actionable--repo-p)
    ("p" "Push" magit-dash-actionable-push :inapt-if-not magit-dash-actionable--repo-p)
    ("u" "Pull" magit-dash-actionable-pull :inapt-if-not magit-dash-actionable--repo-p)
    ("x" "Stage all" magit-dash-stage-all :inapt-if-not magit-dash-actionable--repo-p)
    ("?" "Full menu" magit-dash-menu :inapt-if-not magit-dash-actionable--repo-p)]
   ["Pull request"
    :if magit-dash-actionable--pr-p
    ("o" "Open in browser" magit-dash-actionable-open-pr)]
   ["View"
    ("g" "Refresh" magit-dash-actionable-refresh)
    ("d" "Full dashboard" magit-dash-open)]])

(defvar magit-dash-actionable-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-dash-mode-map)
    (keymap-set map "C-c C-c" #'magit-dash-actionable-menu)
    (keymap-set map "RET" #'magit-dash-actionable-status)
    (keymap-set map "g" #'magit-dash-actionable-refresh)
    map))

(define-derived-mode magit-dash-actionable-mode magit-dash-mode "Dash Actions"
  "Overlay and filtering of `magit-dash-repos' list view for actionable repositories.
Shows repositories needing attention (uncommitted changes, unpushed or unpulled
commits, and pull requests needing author feedback), displaying the Updated timestamp
column and omitting Type, Sync, and Why columns."
  (setq-local magit-dash-columns
              '((org . nil) (name . t) (branch . t) (fetched . nil)
                (updated . t) (ci . t) (status . t)
                (worktree . nil) (sync . nil) (why . nil)
                (cached . nil) (upstream . nil)))
  (setq tabulated-list-format (magit-dash--build-format nil))
  (tabulated-list-init-header))

;;;###autoload
(defun magit-dash-actionable-open ()
  "Open filtered repository view of items needing attention."
  (interactive)
  (let ((buffer (get-buffer-create "*magit-dash-actions*")))
    (with-current-buffer buffer
      (magit-dash-actionable-mode)
      (magit-dash-actionable-refresh))
    (pop-to-buffer buffer)))

(provide 'magit-dash-actionable)
;;; magit-dash-actionable.el ends here
