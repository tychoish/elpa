;;; denote-tree-swap.el --- Resumable swap of two arbitrary Denote sequence subtrees -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (denote-tree "0.1.0") (hitl "0.1.0"))
;; Keywords: docs, denote, convenience, tools
;; URL: https://github.com/tychoish/denote-tree

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Swap any two sequence notes (with their descendants), not only siblings or
;; a parent and child as `denote-tree-swap-with-next' and friends do.
;;
;; The swap is a three-step `hitl-plan': park A at an unused top-level
;; sequence, move B into A's old position, then move A into B's old position.
;; Every step is idempotent and each subtree is renamed descendants-first with
;; the root last, so an interrupted step is recomputed from whatever remains
;; under the old prefix.  The plan is saved after every step, so a swap that
;; is interrupted (or paused after one step) resumes with
;; `hitl-plan-resume'.  Targets are Denote identifiers, which survive the
;; renames, rather than file names.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'denote)
(require 'denote-sequence)
(require 'denote-tree)
(require 'hitl-plan)

(declare-function denote-tree--recursive-reseq-plan "denote-tree")
(declare-function denote-tree--apply-reseq-plan "denote-tree")
(declare-function denote-tree--hierarchy-remap-fold-sequence-many "denote-tree")
(declare-function denote-tree-target-file "denote-tree")
(declare-function hitl-plan-execute-one "hitl-plan" (plan))
(defvar denote-tree-after-reseq-functions)

(defun denote-tree-swap--file-for-id (id)
  "Return the current file for Denote identifier ID, or nil."
  (when (stringp id)
    (denote-get-path-by-id id)))

(defun denote-tree-swap--resolve (target)
  "Resolve swap TARGET (:id :from :seq) to the same plist plus :file, or nil."
  (when-let* ((file (denote-tree-swap--file-for-id (plist-get target :id))))
    (append (list :file file) target)))

(defun denote-tree-swap--format (target)
  "Return (FILE . LABEL) columns describing swap TARGET."
  (cons (or (denote-tree-swap--file-for-id (plist-get target :id))
            (plist-get target :id))
        (format "%s → %s" (plist-get target :from) (plist-get target :seq))))

(defun denote-tree-swap--move (target)
  "Move TARGET's subtree from its :from sequence to its :seq sequence.
Does nothing when the root already sits at :seq.  The plan is rebuilt from
the notes still under :from, and applied descendants-first, so a rerun after
an interruption finishes only the remaining renames."
  (let* ((file (plist-get target :file))
         (from (plist-get target :from))
         (to (plist-get target :seq))
         (current (denote-retrieve-filename-signature file)))
    (cond
     ((equal current to)
      nil)
     ((not (equal current from))
      (error "Note %s is at %s, expected %s or %s" (plist-get target :id) current from to))
     (t
      (let ((plan (denote-tree--recursive-reseq-plan file to)))
        (denote-tree--apply-reseq-plan (reverse plan))
        (denote-tree--hierarchy-remap-fold-sequence-many
         (seq-map (lambda (pair)
                    (cons (denote-retrieve-filename-signature (car pair)) (cdr pair)))
                  plan))
        (run-hook-with-args 'denote-tree-after-reseq-functions plan))))))

(hitl-plan-register-domain 'denote-tree
  :label "Denote tree"
  :resolve-target #'denote-tree-swap--resolve
  :format-target #'denote-tree-swap--format)

(hitl-plan-register-op 'denote-tree 'move
  :label "move subtree"
  :apply (lambda (&rest _) (denote-tree-swap--move hitl-plan-current-target))
  :describe (lambda (&rest _) "move subtree to its target sequence"))

(defun denote-tree-swap--target (file seq to)
  "Return a swap target plist moving FILE (at SEQ) to sequence TO."
  (list :id (denote-retrieve-filename-identifier file) :from seq :seq to))

(defun denote-tree-swap--execute-one (plan)
  "Execute the first step of a newly created swap PLAN and pause it."
  (if (fboundp 'hitl-plan-execute-one)
      (hitl-plan-execute-one plan)
    ;; Older hitl releases provide the step runner but not execute-one.
    (let ((step (car (hitl-plan-steps plan)))
          (inhibit-quit t))
      (setf (hitl-plan-status plan) 'paused)
      (hitl-plan-save plan)
      (hitl-plan-run-step plan step (hitl-plan-step-op step))
      (when quit-flag (setq quit-flag nil))
      (hitl-plan--run-after-apply plan nil)
      (hitl-plan-save plan))))

;;;###autoload
(defun denote-tree-swap-create-plan (file-a file-b)
  "Return a saved, unexecuted swap plan exchanging FILE-A and FILE-B subtrees.
Signal a `user-error' when either note lacks a sequence, both are the same
note, or one is an ancestor of the other.  See `denote-tree-swap'."
  (let ((seq-a (denote-retrieve-filename-signature file-a))
        (seq-b (denote-retrieve-filename-signature file-b)))
    (unless (and seq-a seq-b (denote-sequence-file-p file-a) (denote-sequence-file-p file-b))
      (user-error "Both notes must have a sequence"))
    (when (equal seq-a seq-b)
      (user-error "Cannot swap a note with itself"))
    (when (or (member file-b (denote-sequence-get-all-files-with-prefix seq-a))
              (member file-a (denote-sequence-get-all-files-with-prefix seq-b)))
      (user-error "Cannot swap %s and %s: one contains the other" seq-a seq-b))
    (let ((tmp (denote-sequence-get-new 'parent)))
      (hitl-plan-create
       :domain 'denote-tree
       :description (format "Swap %s and %s" seq-a seq-b)
       :ops '((move))
       :targets (list (denote-tree-swap--target file-a seq-a tmp)
                      (denote-tree-swap--target file-b seq-b seq-a)
                      (denote-tree-swap--target file-a tmp seq-b))))))

;;;###autoload
(defun denote-tree-swap (file-a file-b &optional step)
  "Swap the subtrees rooted at FILE-A and FILE-B, resumably.
The swap runs as a `hitl-plan', so an interruption leaves it paused and
saved; continue with `hitl-plan-resume'.  With prefix argument STEP, run
only the first step and stop, ready to resume with `hitl-plan-resume'."
  (interactive
   (list (denote-tree-target-file "Swap note: " t)
         (denote-tree-target-file "With note: " t)
         current-prefix-arg))
  (let ((plan (denote-tree-swap-create-plan file-a file-b)))
    (if step
        (denote-tree-swap--execute-one plan)
      (hitl-plan-execute plan))
    plan))

(provide 'denote-tree-swap)
;;; denote-tree-swap.el ends here
