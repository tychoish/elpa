;;; agent-shell-queue-train-ui.el --- Focused train views and controls -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused train inspection buffer, review inbox, waiting view, aggregate
;; discovery overview, scoped train controls, circuit-breaker commands,
;; status export, and dry-run reconciliation preview for `agent-shell-queue'.
;;
;; All actions target stable train IDs rather than buffer names.
;; Load-time pure: starts no timers or buffers when required.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'agent-shell-queue-train-contract)
(require 'agent-shell-queue-train)
(require 'agent-shell-queue-dispatcher)

;;; Buffer-Local Stable Identity

(defvar-local agent-shell-queue-train-ui--train-id nil
  "Stable train ID bound to a focused train inspection buffer.")

(define-derived-mode agent-shell-queue-train-view-mode special-mode "ASQ-Train"
  "Major mode for inspecting durable work trains."
  (setq truncate-lines t))

;;; Formatting Helpers

(defun agent-shell-queue-train-ui--remaining-limits (train)
  "Return human-readable remaining budget string for TRAIN."
  (let* ((lim (plist-get train :limits))
         (use (plist-get train :usage))
         (rem-turns (max 0 (- (or (plist-get lim :max-turns) 0)
                              (or (plist-get use :turns) 0))))
         (rem-ret (max 0 (- (or (plist-get lim :max-retries) 0)
                            (or (plist-get use :retries) 0)))))
    (format "turns:%d/%d retries:%d/%d"
            rem-turns (or (plist-get lim :max-turns) 0)
            rem-ret (or (plist-get lim :max-retries) 0))))

(defun agent-shell-queue-train-export-status (train-id)
  "Export a plain-data inspectable status snapshot for TRAIN-ID."
  (when-let* ((train (agent-shell-queue-train-status train-id)))
    (list :id (plist-get train :id)
          :state (plist-get train :state)
          :revision (plist-get train :revision)
          :generation (plist-get train :generation)
          :resource (plist-get train :resource)
          :limits (plist-get train :limits)
          :usage (plist-get train :usage)
          :deadline (plist-get train :deadline)
          :task-count (length (plist-get train :tasks))
          :tasks (plist-get train :tasks)
          :pending-effects (length (plist-get train :pending-effects))
          :reviews (plist-get train :reviews))))

(defun agent-shell-queue-train-reconcile-preview (&optional now)
  "Return a side-effect-free dry-run reconciliation preview at clock NOW."
  (let ((ts (or now (float-time)))
        (active (agent-shell-queue-train-list 'active))
        (due-expire nil)
        (ready-claims nil))
    (dolist (tr active)
      (let ((id (plist-get tr :id))
            (dl (plist-get tr :deadline)))
        (if (and dl (> ts dl))
            (push id due-expire)
          (when-let* ((tk (seq-find (lambda (x)
                                      (eq (agent-shell-queue-train--sym (plist-get x :state))
                                          'ready))
                                    (plist-get tr :tasks))))
            (push (list :train-id id :task-id (plist-get tk :id)
                        :resource (plist-get tr :resource))
                  ready-claims)))))
    (list :dry-run t
          :checked-at ts
          :active-trains (length active)
          :would-expire (nreverse due-expire)
          :would-claim (nreverse ready-claims)
          :next-wakeup-seconds (agent-shell-queue-dispatcher--next-wakeup-delay ts))))

;;; Focused Buffers

(defun agent-shell-queue-train-inspect (train-id)
  "Open or refresh focused inspection buffer for stable TRAIN-ID."
  (interactive
   (list (completing-read "Train ID: "
                          (mapcar (lambda (r) (plist-get r :id))
                                  (agent-shell-queue-train-list))
                          nil t)))
  (let ((train (agent-shell-queue-train-status train-id)))
    (unless train
      (user-error "Train not found: %s" train-id))
    (let ((buf (get-buffer-create (format "*asq-train: %s*" train-id))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (agent-shell-queue-train-view-mode)
          (setq agent-shell-queue-train-ui--train-id train-id)
          (erase-buffer)
          (insert (format "Train: %s  State: %s  Rev: %d  Gen: %d\n"
                          train-id
                          (plist-get train :state)
                          (plist-get train :revision)
                          (plist-get train :generation)))
          (insert (format "Resource: %s\n" (or (plist-get train :resource) "-")))
          (insert (format "Budgets:  %s\n\n" (agent-shell-queue-train-ui--remaining-limits train)))
          (insert "Tasks:\n")
          (dolist (tk (plist-get train :tasks))
            (insert (format "  - %-18s [%-11s] %s\n"
                            (plist-get tk :id)
                            (plist-get tk :state)
                            (or (plist-get tk :objective) ""))))))
      buf)))

(defun agent-shell-queue-train-overview ()
  "Open aggregate discovery buffer listing all trains."
  (interactive)
  (let ((buf (get-buffer-create "*asq-trains*"))
        (trains (agent-shell-queue-train-list)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (agent-shell-queue-train-view-mode)
        (erase-buffer)
        (insert (format "Durable Trains (%d total)\n\n" (length trains)))
        (dolist (tr trains)
          (insert (format "%-20s %-10s rev:%-3d %s  %s\n"
                          (plist-get tr :id)
                          (plist-get tr :state)
                          (plist-get tr :revision)
                          (agent-shell-queue-train-ui--remaining-limits tr)
                          (or (plist-get tr :resource) ""))))))
    buf))

(defun agent-shell-queue-train-review-inbox ()
  "Open focused buffer listing trains with tasks in `needs-human' state."
  (interactive)
  (let ((buf (get-buffer-create "*asq-train-reviews*"))
        (items nil))
    (dolist (tr (agent-shell-queue-train-list))
      (dolist (tk (plist-get tr :tasks))
        (when (eq (agent-shell-queue-train--sym (plist-get tk :state)) 'needs-human)
          (push (cons (plist-get tr :id) tk) items))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (agent-shell-queue-train-view-mode)
        (erase-buffer)
        (insert (format "Train Review Inbox (%d pending)\n\n" (length items)))
        (dolist (pair (nreverse items))
          (insert (format "%-20s task:%-16s %s\n"
                          (car pair)
                          (plist-get (cdr pair) :id)
                          (or (plist-get (cdr pair) :objective) ""))))))
    buf))

(defun agent-shell-queue-train-waiting-view ()
  "Open focused buffer listing tasks currently in `waiting' state."
  (interactive)
  (let ((buf (get-buffer-create "*asq-train-waiting*"))
        (items nil))
    (dolist (tr (agent-shell-queue-train-list))
      (dolist (tk (plist-get tr :tasks))
        (when (eq (agent-shell-queue-train--sym (plist-get tk :state)) 'waiting)
          (push (cons (plist-get tr :id) tk) items))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (agent-shell-queue-train-view-mode)
        (erase-buffer)
        (insert (format "Waiting Train Tasks (%d waiting)\n\n" (length items)))
        (dolist (pair (nreverse items))
          (insert (format "%-20s task:%-16s\n" (car pair) (plist-get (cdr pair) :id))))))
    buf))

;;; Scoped Interactive Commands (Target Stable Train ID)

(defun agent-shell-queue-train-ui--current-id ()
  "Return stable train ID for current focused buffer, or signal `user-error'."
  (or agent-shell-queue-train-ui--train-id
      (user-error "Not in a focused train buffer")))

(defun agent-shell-queue-train-ui-activate ()
  "Activate the focused train using its current revision."
  (interactive)
  (let* ((id (agent-shell-queue-train-ui--current-id))
         (st (agent-shell-queue-train-status id)))
    (agent-shell-queue-train-activate id :expected-revision (plist-get st :revision))
    (agent-shell-queue-train-inspect id)))

(defun agent-shell-queue-train-ui-pause ()
  "Pause the focused train using its current revision."
  (interactive)
  (let* ((id (agent-shell-queue-train-ui--current-id))
         (st (agent-shell-queue-train-status id)))
    (agent-shell-queue-train-pause id :expected-revision (plist-get st :revision))
    (agent-shell-queue-train-inspect id)))

(defun agent-shell-queue-train-ui-resume ()
  "Resume the focused train using its current revision."
  (interactive)
  (let* ((id (agent-shell-queue-train-ui--current-id))
         (st (agent-shell-queue-train-status id)))
    (agent-shell-queue-train-resume id :expected-revision (plist-get st :revision))
    (agent-shell-queue-train-inspect id)))

(defun agent-shell-queue-train-ui-cancel ()
  "Cancel the focused train (after-turn policy)."
  (interactive)
  (let* ((id (agent-shell-queue-train-ui--current-id))
         (st (agent-shell-queue-train-status id)))
    (agent-shell-queue-train-cancel id :expected-revision (plist-get st :revision)
                                    :interrupt 'after-turn)
    (agent-shell-queue-train-inspect id)))

;;; Transient Operator Menu (All Keys Unique Across Groups)

;;;###autoload
(transient-define-prefix agent-shell-queue-train-menu ()
  "Operator controls for durable work trains."
  [["Views"
    ("o" "Overview" agent-shell-queue-train-overview)
    ("i" "Inspect train" agent-shell-queue-train-inspect)
    ("v" "Review inbox" agent-shell-queue-train-review-inbox)
    ("w" "Waiting view" agent-shell-queue-train-waiting-view)]
   ["Focused Train"
    ("a" "Activate" agent-shell-queue-train-ui-activate)
    ("p" "Pause" agent-shell-queue-train-ui-pause)
    ("r" "Resume" agent-shell-queue-train-ui-resume)
    ("k" "Cancel" agent-shell-queue-train-ui-cancel)]
   ["Circuit Breaker"
    ("P" "Pause active set" agent-shell-queue-train-circuit-breaker-pause)
    ("R" "Resume paused set" agent-shell-queue-train-circuit-breaker-resume)]])

(provide 'agent-shell-queue-train-ui)
;;; agent-shell-queue-train-ui.el ends here
