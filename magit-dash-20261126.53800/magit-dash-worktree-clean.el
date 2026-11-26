;;; magit-dash-worktree-clean.el --- Plan and execute merged-worktree cleanup via hitl-plan -*- lexical-binding: t -*-

;;; Commentary:

;; Deleting the worktrees of merged (or closed) pull requests used to ask a
;; `y-or-n-p' question per worktree while GitHub was being queried.  This
;; module splits that into two phases, like the ELPAish package install flow:
;;
;; 1. Plan: scan the repository's worktrees, match each to its PR, and turn
;;    every merged or closed one into a step of a `hitl-plan' in the
;;    `magit-dash-worktree-clean' domain.  Worktrees with uncommitted changes
;;    are listed too, but pre-marked `skipped'.
;; 2. Execute: review the plan in the `hitl-plan' buffer (skip steps with `m',
;;    run everything with `x', or one step with `o'), then delete.  Progress
;;    is saved after every step, so `C-g' pauses and `hitl-plan-resume'
;;    continues.
;;
;; Only the worktree is removed; the branch is kept until branch pruning.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'map)
(require 'hitl-plan)
(require 'magit-dash-pr)
(require 'magit-dash-worktree)

(declare-function hitl-plan-review "hitl-plan-ui")
(declare-function magit-dash-pr--fetch-repo-prs "magit-dash-pr")
(declare-function magit-dash-pr--fetch-pr "magit-dash-pr")
(declare-function magit-dash-pr--git-worktrees "magit-dash-pr")
(declare-function magit-dash-pr--match-worktree-by-branch "magit-dash-pr")
(declare-function magit-dash-pr--match-worktree-to-pr "magit-dash-pr")
(declare-function magit-dash-pr--path-equal-p "magit-dash-pr")
(declare-function magit-dash-pr--pr-merged-p "magit-dash-pr")
(declare-function magit-dash-pr--find-existing-worktree "magit-dash-pr")
(declare-function magit-dash-pr--worktree-corroborates-pr-p "magit-dash-pr")
(declare-function magit-dash-pr-delete-worktree "magit-dash-pr")
(declare-function magit-dash-pr-dashboard--current-repo-slug "magit-dash-pr")
(declare-function magit-dash-pr-dashboard--find-local-path "magit-dash-pr")
(declare-function magit-dash-pr-dashboard-refresh "magit-dash-pr")
(declare-function magit-toplevel "magit-git")

(defgroup magit-dash-worktree-clean nil
  "Plan-then-execute cleanup of merged pull request worktrees."
  :group 'magit-dash)

(defcustom magit-dash-worktree-clean-plan-directory nil
  "Directory holding worktree cleanup plans.
When nil, a subdirectory of `hitl-plan-directory'."
  :type '(choice (const :tag "Under hitl-plan-directory" nil) directory)
  :group 'magit-dash-worktree-clean)

;;;; Discovery

(defun magit-dash-worktree-clean--closed-state (pr slug number repo-dir)
  "Return \"merged\", \"closed\" or nil for PR (number NUMBER) in SLUG."
  (cond
   ((magit-dash-pr--pr-merged-p pr slug number repo-dir)
    "merged")
   ((equal (upcase (or (map-elt pr 'state) "")) "CLOSED")
    "closed")))

(defun magit-dash-worktree-clean--match (wt prs slug repo-dir)
  "Return the PR matching worktree WT among PRS, fetching when needed, or nil.
Every match must be corroborated by the worktree's branch; see
`magit-dash-pr--match-worktree-corroborated'."
  (magit-dash-pr--match-worktree-corroborated wt prs slug repo-dir))

(defun magit-dash-worktree-clean--target (wt pr state slug repo-dir)
  "Return the plan target plist for worktree WT, matched to PR in STATE."
  (let ((path (alist-get 'path wt)))
    (list :worktree path
          :repo-dir repo-dir
          :slug slug
          :number (map-elt pr 'number)
          :branch (alist-get 'branch wt)
          :head (alist-get 'head wt)
          :pr-head-oid (map-elt pr 'headRefOid)
          :state state)))

(defun magit-dash-worktree-clean--mark-dirty (targets)
  "Return TARGETS with `:dirty' and `:risks' set.
`:dirty' comes from one concurrent `git status' sweep.  `:risks' lists
`magit-dash-worktree-risks' for each clean worktree: unpublished commits and
ignored files that are not build output."
  (let ((dirty (magit-dash-worktree-dirty-many
                (seq-map (lambda (target) (plist-get target :worktree)) targets))))
    (seq-map (lambda (target)
               (let ((is-dirty (and (cdr (assoc (plist-get target :worktree) dirty)) t)))
                 (thread-first (copy-sequence target)
                               (plist-put :dirty is-dirty)
                               (plist-put :risks
                                          (unless is-dirty
                                            (magit-dash-worktree-risks
                                             (plist-get target :worktree)
                                             (plist-get target :pr-head-oid)))))))
             targets)))

(defun magit-dash-worktree-clean-scan (repo-dir)
  "Return plan targets for the merged or closed PR worktrees of REPO-DIR."
  (let* ((repo-dir (expand-file-name repo-dir))
         (slug (or (ignore-errors
                     (let ((default-directory repo-dir))
                       (magit-dash-pr-dashboard--current-repo-slug)))
                   (file-name-nondirectory (directory-file-name repo-dir))))
         (prs (magit-dash-pr--fetch-repo-prs slug "all" repo-dir)))
    (thread-last
      (magit-dash-pr--git-worktrees repo-dir)
      (seq-filter (lambda (wt)
                    (when-let* ((path (alist-get 'path wt)))
                      (and (file-directory-p path)
                           (not (magit-dash-pr--path-equal-p path repo-dir))))))
      (seq-keep (lambda (wt)
                  (when-let* ((pr (magit-dash-worktree-clean--match wt prs slug repo-dir))
                              (state (magit-dash-worktree-clean--closed-state
                                      pr slug (map-elt pr 'number) repo-dir)))
                    (magit-dash-worktree-clean--target wt pr state slug repo-dir))))
      (magit-dash-worktree-clean--mark-dirty))))

(defun magit-dash-worktree-clean-entry-target (entry)
  "Return a plan target for dashboard PR ENTRY, or nil when it has no worktree.
Open PRs are not candidates, so nil is returned for them too."
  (when-let* ((repo (plist-get entry :repo))
              (local-path (magit-dash-pr-dashboard--find-local-path repo))
              (repo-dir (expand-file-name local-path))
              (number (plist-get entry :number))
              (pr-data (plist-get entry :pr))
              (wt (magit-dash-pr--find-corroborated-worktree repo-dir number pr-data))
              ((file-directory-p (alist-get 'path wt)))
              (state (magit-dash-worktree-clean--closed-state
                      pr-data repo number repo-dir)))
    (car (magit-dash-worktree-clean--mark-dirty
          (list (magit-dash-worktree-clean--target wt pr-data state repo repo-dir))))))

;;;; Domain and operation

(defun magit-dash-worktree-clean--directory ()
  "Return the directory in which cleanup plans are stored."
  (file-name-as-directory
   (expand-file-name (or magit-dash-worktree-clean-plan-directory
                         (expand-file-name "magit-dash-worktree-clean/"
                                           hitl-plan-directory)))))

(defun magit-dash-worktree-clean--format-target (target)
  "Return (FILE . LABEL) columns for TARGET."
  (cons (abbreviate-file-name (plist-get target :worktree))
        (format "PR #%s %s%s%s"
                (or (plist-get target :number) "?")
                (plist-get target :state)
                (if (plist-get target :dirty)
                    ", uncommitted changes"
                  "")
                (if (plist-get target :risks)
                    (format ", NEEDS CONFIRMATION: %s"
                            (string-join (plist-get target :risks) "; "))
                  ""))))

(defun magit-dash-worktree-clean--visit-target (target &optional other-window)
  "Visit TARGET's worktree in Dired, in another window when OTHER-WINDOW."
  (when-let* ((dir (plist-get target :worktree))
              ((file-directory-p dir)))
    (if other-window
        (dired-other-window dir)
      (dired dir))))

(defun magit-dash-worktree-clean--sanitize-target (_target)
  "Refuse caller-supplied targets: cleanup plans only come from discovery."
  (user-error "Worktree cleanup plans are built by scanning, not from explicit targets"))

(defun magit-dash-worktree-clean--after-steps-added (_plan steps)
  "Pre-skip the STEPS whose worktree has uncommitted changes."
  (seq-do (lambda (step)
            (when (plist-get (hitl-plan-step-target step) :dirty)
              (setf (hitl-plan-step-status step) 'skipped
                    (hitl-plan-step-message step) "uncommitted changes")))
          steps))

(defun magit-dash-worktree-clean--verify-target (target)
  "Return nil when TARGET still names the planned worktree, else the reason.
A plan can sit for days.  The path must still be a linked worktree of the
same repository, on the planned `:branch' at the planned `:head', and the
PR must still be merged or closed."
  (let* ((path (plist-get target :worktree))
         (repo-dir (plist-get target :repo-dir))
         (wt (seq-find (lambda (wt)
                         (magit-dash-pr--path-equal-p (alist-get 'path wt) path))
                       (magit-dash-pr--git-worktrees repo-dir)))
         (number (plist-get target :number)))
    (cond
     ((or (null wt) (magit-dash-pr--path-equal-p path repo-dir))
      (format "%s is no longer a linked worktree of %s" path repo-dir))
     ((not (equal (alist-get 'branch wt) (plist-get target :branch)))
      (format "branch changed from %s to %s since the plan"
              (plist-get target :branch) (alist-get 'branch wt)))
     ((not (equal (alist-get 'head wt) (plist-get target :head)))
      "HEAD moved since the plan")
     ((null number)
      "the plan names no PR")
     ((not (magit-dash-worktree-clean--closed-state
            (magit-dash-pr--fetch-pr (plist-get target :slug) number repo-dir)
            (plist-get target :slug) number repo-dir))
      (format "PR #%s is no longer known to be merged or closed" number)))))

(defun magit-dash-worktree-clean--apply ()
  "Delete the current step's worktree after re-verifying the plan target.
A target that no longer matches is refused with the reason, which the plan
records on the step.  A worktree that became dirty is refused too, and one
with unpublished commits or non-build ignored files needs a confirmation."
  (let* ((target hitl-plan-current-target)
         (path (plist-get target :worktree)))
    (when-let* ((problem (magit-dash-worktree-clean--verify-target target)))
      (error "Skipped %s: %s" path problem))
    (when (magit-dash-worktree-uncommitted-changes-p path)
      (error "Worktree %s has uncommitted changes" path))
    (magit-dash-pr-delete-worktree path (plist-get target :repo-dir)
                                   (plist-get target :pr-head-oid))))

(defun magit-dash-worktree-clean--after-apply (_file _steps plan)
  "Refresh a live PR dashboard once cleanup PLAN has run."
  (when (and (hitl-plan-in-domain-p plan 'magit-dash-worktree-clean)
             (get-buffer "*magit-dash-prs*"))
    (with-current-buffer "*magit-dash-prs*"
      (when (derived-mode-p 'magit-dash-pr-dashboard-mode)
        (magit-dash-pr-dashboard-refresh)))))

(hitl-plan-register-domain 'magit-dash-worktree-clean
  :label "Merged worktree cleanup"
  :resolve-target #'identity
  :format-target #'magit-dash-worktree-clean--format-target
  :visit-target #'magit-dash-worktree-clean--visit-target
  :directory #'magit-dash-worktree-clean--directory
  :after-steps-added #'magit-dash-worktree-clean--after-steps-added
  :sanitize-target #'magit-dash-worktree-clean--sanitize-target)

(hitl-plan-register-op 'magit-dash-worktree-clean 'delete-worktree
  :key ?d
  :label "delete worktree"
  :apply #'magit-dash-worktree-clean--apply
  :describe (lambda (&rest _) "remove the worktree (branch kept until pruning)")
  :schema '("none: each target names its own worktree"))

(add-hook 'hitl-plan-after-apply-functions #'magit-dash-worktree-clean--after-apply)

;;;; Entry points

;;;###autoload
(defun magit-dash-worktree-clean-plan-targets (targets &optional description)
  "Create a cleanup plan for TARGETS and open it for review.
Return the plan, or nil (after saying so) when TARGETS is empty."
  (if (null targets)
      (progn
        (message "magit-gh: no merged or closed PR worktrees to clean")
        nil)
    (let ((plan (hitl-plan-create
                 :domain 'magit-dash-worktree-clean
                 :targets targets
                 :ops '(delete-worktree)
                 :description (or description "Delete worktrees of merged or closed PRs"))))
      (require 'hitl-plan-ui)
      (hitl-plan-review plan)
      plan)))

;;;###autoload
(defun magit-dash-worktree-clean-plan (&optional repo-dir)
  "Plan deletion of merged or closed PR worktrees in REPO-DIR and review it.
Execute from the review buffer: `x' runs every pending step, `m' skips the
step at point."
  (interactive)
  (let ((repo-dir (expand-file-name
                   (or repo-dir
                       (magit-dash-worktree-main-dir)
                       (magit-toplevel)
                       default-directory))))
    (message "magit-gh: scanning worktrees in %s..." repo-dir)
    (magit-dash-worktree-clean-plan-targets
     (magit-dash-worktree-clean-scan repo-dir)
     (format "Delete worktrees of merged or closed PRs in %s"
             (abbreviate-file-name repo-dir)))))

(provide 'magit-dash-worktree-clean)
;;; magit-dash-worktree-clean.el ends here
