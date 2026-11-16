;;; diff-review-magit.el --- Reviews from magit -*- lexical-binding: t; -*-

;; Author: Xiaoxing Hu <hi@xiaoxing.dev>
;; URL: https://github.com/xiaoxinghu/diff-review

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Every worktree's branch is a candidate pull request, so magit-status
;; can list them: one row per worktree other than the current one, with
;; its branch, the commits it has ahead of the base and, once the branch
;; has a review, the viewed count, the open threads and the verdict.
;; RET on a row opens the branch's review, creating it, as
;; `diff-review-branch' does.  A worktree at a detached head, or whose
;; branch is gone, has no row.
;;
;; `diff-review-magit-commits' reviews the commits selected in any magit
;; buffer, in a file of their own that every worktree shares.
;;
;; Both are opt-in, behind one switch, which loads no magit:
;;
;;   (diff-review-magit-mode)
;;
;; It adds the section to magit-status and `v' "Review" to the `magit-diff'
;; transient, `d v'.  For one half alone:
;;
;;   (with-eval-after-load 'magit
;;     (add-hook 'magit-status-sections-hook #'diff-review-magit-insert-worktrees t))
;;   (with-eval-after-load 'magit
;;     (transient-append-suffix 'magit-diff 'magit-stash-show
;;       '("v" "Review" diff-review-magit-commits)))

;;; Code:

;; magit-section only: the mode must be safe to turn on before magit loads,
;; and the rest runs in magit's buffers, where magit is loaded.
(require 'magit-section)
(require 'diff-review)

(defvar magit-status-sections-hook)
(declare-function magit-commit-at-point "magit-git" ())
(declare-function transient-append-suffix "transient" (prefix loc suffix &optional keep-other))
(declare-function transient-remove-suffix "transient" (prefix loc))

(defvar-keymap diff-review-magit-branch-section-map
  :doc "Keymap for a branch's row in `diff-review-magit-insert-worktrees'."
  "<remap> <magit-visit-thing>" #'diff-review-magit-visit)

(defclass diff-review-magit-branch-section (magit-section)
  ((keymap :initform 'diff-review-magit-branch-section-map))
  "A worktree's branch, its value the branch name.")

(defun diff-review-magit--branches (root)
  "The branches of the worktrees of the repository at ROOT other than ROOT's.
Only those that exist: a worktree at a detached head, or whose branch was
deleted under it, has none."
  (let ((here (file-truename root))
        (default-directory root))
    (delq nil (mapcar (lambda (worktree)
                        (let ((branch (plist-get worktree :branch)))
                          (and branch
                               (not (equal (file-truename (plist-get worktree :path)) here))
                               (diff-review--git-maybe "rev-parse" "--verify" "--quiet"
                                                       (concat "refs/heads/" branch))
                               branch)))
                      (diff-review--worktrees root)))))

(defun diff-review-magit--review (file)
  "What a row shows of review FILE, as visiting it would find it: a plist.
Read and scanned in a temporary buffer, so nothing is drawn or written."
  (with-temp-buffer
    (insert-file-contents file t)
    (diff-review--read)
    (diff-review--scan)
    (list :base diff-review--base-name
          :viewed (seq-count #'diff-review--viewed-p diff-review--files)
          :files (length diff-review--files)
          :open (diff-review--open-count diff-review--comments)
          :verdict (diff-review--verdict-text))))

(defun diff-review-magit--font-lock (string)
  "STRING with its faces as `font-lock-face', which font-lock leaves alone."
  (let ((s (copy-sequence string)) (i 0))
    (while (< i (length s))
      (let ((next (next-single-property-change i 'face s (length s))))
        (when-let* ((face (get-text-property i 'face s)))
          (put-text-property i next 'font-lock-face face s))
        (setq i next)))
    s))

(defun diff-review-magit--row (root branch)
  "The text after BRANCH's name in its row, in the repository at ROOT."
  (condition-case err
      (let* ((file (diff-review--branch-file root branch))
             (review (and (file-exists-p file) (diff-review-magit--review file)))
             (base (or (plist-get review :base) (diff-review--default-base root)))
             (ahead (string-to-number
                     (diff-review--git-at root "rev-list" "--count" (concat base ".." branch))))
             (open (plist-get review :open))
             (verdict (plist-get review :verdict)))
        (concat
         (format "%d commit%s" ahead (if (= ahead 1) "" "s"))
         (and review (format "  %d/%d viewed" (plist-get review :viewed) (plist-get review :files)))
         (and review (> open 0)
              (propertize (format "  %d open" open) 'font-lock-face 'diff-review-comment-author))
         (and review (not (string-empty-p verdict))
              (concat "  " (diff-review-magit--font-lock verdict)))))
    (error (propertize (error-message-string err) 'font-lock-face 'error))))

;;;###autoload
(defun diff-review-magit-insert-worktrees ()
  "Insert the branches of the other worktrees, each a candidate pull request.
A row shows the branch, its commits ahead of the base and, once it has a
review, the viewed count, the open threads and the verdict.  RET on a row
opens the review, creating it.  For `magit-status-sections-hook':

  (add-hook \\='magit-status-sections-hook
            #\\='diff-review-magit-insert-worktrees t)"
  (when-let* ((root (diff-review--toplevel default-directory))
              (branches (diff-review-magit--branches root)))
    (let ((width (1+ (apply #'max (mapcar #'string-width branches)))))
      (magit-insert-section (diff-review-magit-worktrees)
        (magit-insert-heading t "Worktree branches")
        (dolist (branch branches)
          (magit-insert-section (diff-review-magit-branch-section branch)
            (insert (propertize (string-pad branch width) 'font-lock-face 'magit-branch-local)
                    (diff-review-magit--row root branch)
                    "\n")))
        (insert "\n")))))

;;;###autoload
(defun diff-review-magit-visit ()
  "Open the review of the branch at point, creating it."
  (interactive)
  (diff-review-branch (or (magit-section-value-if 'diff-review-magit-branch-section)
                          (user-error "No worktree branch at point"))))

;;;###autoload
(defun diff-review-magit-commits ()
  "Review the commits the region selects, the oldest included.
Without a region, the commit at point alone.  The review is of a fixed
range, never a branch's: its file is review/OLD..NEW.json under the
common git directory, by the abbreviated hashes, so the same selection
reopens with its marks from any worktree, and a branch's review stays
`diff-review-branch''s."
  (interactive)
  (let* ((root (diff-review--root-here))
         (default-directory root)
         (shas (mapcar (lambda (rev)
                         (diff-review--git-at root "rev-parse" (concat rev "^{commit}")))
                       (or (magit-region-values '(commit branch) t)
                           (list (or (magit-commit-at-point) (user-error "No commit at point"))))))
         (new (car shas))
         (old (car (last shas))))
    ;; By ancestry, not buffer order: a log can be --reverse.
    (when (diff-review--git-maybe "merge-base" "--is-ancestor" new old)
      (cl-rotatef old new))
    ;; Its review would have no base to open with.
    (unless (diff-review--git-maybe "rev-parse" "--verify" "--quiet" (concat old "^"))
      (user-error "The oldest commit selected, %s, has no parent to review against"
                  (diff-review--git-at root "rev-parse" "--short" old)))
    ;; A graph log can select commits from beside the range, or ends on two
    ;; sides of a fork: the review would leave them out.
    (let ((range (split-string (diff-review--git-at root "rev-list" new (concat "^" old "^")))))
      (unless (seq-every-p (lambda (sha) (member sha range)) shas)
        (user-error "The commits selected are not one range of history")))
    (deactivate-mark)
    ;; The main worktree as the root: the file outlives the worktree it was made from.
    (diff-review-find-file (diff-review--range-file root old new)
                           (or (diff-review--main-worktree root) root)
                           (concat old "^") new)))

;;;###autoload
(define-minor-mode diff-review-magit-mode
  "Reviews in magit: the worktrees section in magit-status, and `d v'.
`diff-review-magit-insert-worktrees' joins `magit-status-sections-hook',
and `diff-review-magit-commits' the `magit-diff' transient as `v'
\"Review\".  Safe to turn on before magit loads, which it waits for."
  :global t
  :group 'diff-review
  ;; Not before magit loads: an add-hook would bind the hook first, and
  ;; magit's defcustom would leave out its default sections.  The mode's
  ;; value is read then, so turning it off meanwhile leaves nothing added.
  (with-eval-after-load 'magit (diff-review-magit--sync)))

(defun diff-review-magit--sync ()
  "Add the section and the suffix to magit, or remove them, per the mode."
  (if diff-review-magit-mode
      (progn
        (add-hook 'magit-status-sections-hook #'diff-review-magit-insert-worktrees t)
        (transient-append-suffix 'magit-diff 'magit-stash-show
          '("v" "Review" diff-review-magit-commits)))
    (remove-hook 'magit-status-sections-hook #'diff-review-magit-insert-worktrees)
    (transient-remove-suffix 'magit-diff 'diff-review-magit-commits)))

(provide 'diff-review-magit)
;;; diff-review-magit.el ends here
