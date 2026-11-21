;;; sprite-desktop.el --- Staged asynchronous desktop restore for sprite -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (seq "2.23"))
;; Keywords: convenience, tools

;;; Commentary:
;; Staged asynchronous desktop restore.  Buffers are collected during
;; `desktop-read' without visiting files, categorized into priority lanes
;; (hot, light, heavy, cold), and materialized at idle time.
;; Visiting any pending file awaits its restoration immediately.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'subr-x)
(require 'desktop)
(require 'generator)
(require 'tabulated-list)
(require 'project)
(require 'sprite)
(require 'sprite-batch)

(defgroup sprite-desktop nil
  "Staged asynchronous desktop restore for sprite."
  :group 'sprite-state
  :prefix "sprite-desktop-")

(defcustom sprite-desktop-eager-count 3
  "Number of desktop entries to restore synchronously at startup."
  :type 'natnum
  :group 'sprite-desktop)

(defcustom sprite-desktop-hot-window 86400.0
  "Seconds within which a visited buffer is hot (default 24h)."
  :type 'number
  :group 'sprite-desktop)

(defcustom sprite-desktop-cold-window 1209600.0
  "Seconds after which an unvisited buffer is considered cold (default 14 days)."
  :type 'number
  :group 'sprite-desktop)

(defcustom sprite-desktop-heavy-ms 50
  "Estimated materialization milliseconds above which a buffer is heavy."
  :type 'natnum
  :group 'sprite-desktop)

(defcustom sprite-desktop-weight-recency 1.0
  "Weight of recency in queue scoring."
  :type 'number
  :group 'sprite-desktop)

(defcustom sprite-desktop-weight-project 1.0
  "Weight of current project match in queue scoring."
  :type 'number
  :group 'sprite-desktop)

(defcustom sprite-desktop-weight-cost 1.0
  "Weight of materialization cost penalty in queue scoring."
  :type 'number
  :group 'sprite-desktop)

(defcustom sprite-desktop-cost-sample-weight 0.3
  "Weight of new samples in exponentially weighted cost average."
  :type 'number
  :group 'sprite-desktop)

(cl-defstruct (sprite-desktop-entry (:copier nil))
  args       ; arguments for `desktop-create-buffer'
  file       ; truename or nil
  name       ; buffer name or nil
  mode       ; major mode symbol
  project    ; project root or nil
  lane       ; now | hot | light | heavy | cold
  score      ; number, higher drains first
  est-ms     ; estimated materialization cost
  state)     ; pending | running | done | failed | dropped

(defvar sprite-desktop--queue nil
  "Alist lane -> list of `sprite-desktop-entry' sorted by score, best first.")

(defvar sprite-desktop--index (make-hash-table :test #'equal)
  "Truename -> `sprite-desktop-entry', for the await path.")

(defvar sprite-desktop-visit-history nil
  "Alist of (TRUENAME . LAST-DISPLAY-FLOAT) persisted in savehist.")

(defvar sprite-desktop-cost-history nil
  "Alist of (MAJOR-MODE . AVG-MS) persisted in savehist.")

(defvar sprite-desktop--total-count 0
  "Total number of staged desktop entries in the current session.")

(defvar sprite-desktop--completed-count 0
  "Number of materialized or resolved desktop entries.")

(defvar sprite-desktop--failed-count 0
  "Number of entries that failed to materialize.")

(defvar sprite-desktop--cancel-fn nil
  "Cancellation function for the active drain batch.")

(defvar sprite-desktop--paused nil
  "Non-nil when background drain is paused.")

(defvar sprite-desktop--last-log-time 0.0
  "Timestamp of last progress log message.")

(defvar savehist-additional-variables)
(defvar eglox-activation-policy)
(defvar desktop-first-buffer nil)
(defvar desktop-buffer-ok-count 0)
(defvar desktop-buffer-fail-count 0)
(declare-function eglox-warm-project "eglox" (buffer))

;;; Persistence & History

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'sprite-desktop-visit-history)
  (add-to-list 'savehist-additional-variables 'sprite-desktop-cost-history))

(defun sprite-desktop--record-cost (mode ms)
  "Update average materialization MS for MODE."
  (when (and mode ms (> ms 0))
    (let* ((existing (alist-get mode sprite-desktop-cost-history))
           (w sprite-desktop-cost-sample-weight)
           (updated (if existing
                        (+ (* w ms) (* (- 1.0 w) existing))
                      (float ms))))
      (setf (alist-get mode sprite-desktop-cost-history) updated))))

(defun sprite-desktop--record-visits ()
  "Update `sprite-desktop-visit-history' from live buffers and trim old entries."
  (let ((now (float-time)))
    (dolist (buf (buffer-list))
      (when-let* ((file (buffer-file-name buf))
                  (truename (file-truename file))
                  (display-time (buffer-local-value 'buffer-display-time buf)))
        (setf (alist-get truename sprite-desktop-visit-history nil nil #'equal)
              (float-time display-time))))
    ;; Trim entries older than 4 * cold window or exceeding 2000
    (let* ((cutoff (- now (* 4 sprite-desktop-cold-window)))
           (trimmed (seq-take
                     (seq-filter (lambda (entry) (>= (cdr entry) cutoff))
                                 sprite-desktop-visit-history)
                     2000)))
      (setq sprite-desktop-visit-history trimmed))))

(add-hook 'desktop-save-hook #'sprite-desktop--record-visits)

;;; Classification and Scoring

(defun sprite-desktop--active-project ()
  "Return the project root of the current buffer or first restored buffer."
  (or (when-let* ((buf (and (bound-and-true-p desktop-first-buffer)
                            (get-buffer desktop-first-buffer)))
                 (file (buffer-file-name buf))
                 (proj (project-current nil (file-name-directory file))))
        (project-root proj))
      (when-let* ((file (or buffer-file-name default-directory))
                 (proj (project-current nil (file-name-directory file))))
        (project-root proj))))

(defun sprite-desktop--hook-count (mode)
  "Return count of hook functions for MODE and its parent modes."
  (let ((count 0)
        (curr mode))
    (while (and curr (symbolp curr))
      (let ((hook-sym (intern-soft (format "%s-hook" curr))))
        (when (and hook-sym (boundp hook-sym))
          (let ((val (symbol-value hook-sym)))
            (setq count (+ count (length (ensure-list (remq t val))))))))
      (setq curr (get curr 'derived-mode-parent)))
    (max 1 count)))

(defun sprite-desktop--estimate-ms (mode)
  "Return estimated materialization ms for MODE."
  (or (and mode (alist-get mode sprite-desktop-cost-history))
      (* 3 (sprite-desktop--hook-count mode))))

(defun sprite-desktop--lsp-applicable-p (mode &optional _file)
  "Return non-nil if an LSP server applies to MODE."
  (or (and (fboundp 'eglot--lookup-mode)
           (cdr (eglot--lookup-mode mode)))
      (and (boundp 'eglot-server-programs)
           (assoc mode eglot-server-programs))))

(defun sprite-desktop--classify (entry)
  "Assign ENTRY to a lane: `hot', `light', `heavy', or `cold'."
  (let* ((file (sprite-desktop-entry-file entry))
         (mode (sprite-desktop-entry-mode entry))
         (est (or (sprite-desktop-entry-est-ms entry) 0))
         (proj (sprite-desktop-entry-project entry))
         (active-proj (sprite-desktop--active-project))
         (last-visit (and file (alist-get file sprite-desktop-visit-history nil nil #'equal)))
         (now (float-time))
         (lsp-p (sprite-desktop--lsp-applicable-p mode file)))
    (cond
     ((or (and proj active-proj (equal proj active-proj))
          (and last-visit (<= (- now last-visit) sprite-desktop-hot-window)))
      'hot)
     ((or (null last-visit)
          (> (- now last-visit) sprite-desktop-cold-window))
      'cold)
     ((or lsp-p (> est sprite-desktop-heavy-ms))
      'heavy)
     (t 'light))))

(defun sprite-desktop--score (entry &optional rank)
  "Calculate score for ENTRY with optional desktop position RANK."
  (let* ((file (sprite-desktop-entry-file entry))
         (proj (sprite-desktop-entry-project entry))
         (active-proj (sprite-desktop--active-project))
         (est (or (sprite-desktop-entry-est-ms entry) 0))
         (last-visit (and file (alist-get file sprite-desktop-visit-history nil nil #'equal)))
         (recency
          (if last-visit
              (let ((hours (/ (max 0.0 (- (float-time) last-visit)) 3600.0)))
                (/ 1.0 (+ 1.0 hours)))
            (if rank (/ 1.0 (+ 1.0 rank)) 0.0)))
         (proj-match (if (and proj active-proj (equal proj active-proj)) 1.0 0.0)))
    (+ (* sprite-desktop-weight-recency recency)
       (* sprite-desktop-weight-project proj-match)
       (- (* sprite-desktop-weight-cost (/ (float est) 100.0))))))

;;; Queue Management

(defun sprite-desktop--entry-less-p (a b)
  "Return non-nil if entry A has higher score than entry B."
  (> (sprite-desktop-entry-score a) (sprite-desktop-entry-score b)))

(defun sprite-desktop-enqueue (args &rest keys)
  "Enqueue a desktop buffer for staged restoration.
ARGS are arguments for `desktop-create-buffer'.  KEYS may specify
overrides for :file, :mode, or :project."
  (let* ((raw-file (or (plist-get keys :file) (nth 1 args)))
         (file (and raw-file (file-truename raw-file)))
         (name (or (plist-get keys :name) (nth 2 args)))
         (mode (or (plist-get keys :mode) (nth 3 args))))
    (unless (or (and file (find-buffer-visiting file))
                (and file (gethash file sprite-desktop--index)))
      (let* ((proj (or (plist-get keys :project)
                       (when (and file (fboundp 'project-current))
                         (when-let* ((pr (project-current nil (file-name-directory file))))
                           (project-root pr)))))
             (est (sprite-desktop--estimate-ms mode))
             (rank (plist-get keys :rank))
             (entry (make-sprite-desktop-entry
                     :args args
                     :file file
                     :name name
                     :mode mode
                     :project proj
                     :est-ms est
                     :state 'pending))
             (lane (sprite-desktop--classify entry))
             (score (sprite-desktop--score entry rank)))
        (setf (sprite-desktop-entry-lane entry) lane
              (sprite-desktop-entry-score entry) score)
        (when file
          (puthash file entry sprite-desktop--index))
        (setf (alist-get lane sprite-desktop--queue)
              (thread-last (cons entry (alist-get lane sprite-desktop--queue))
                (seq-sort (lambda (a b) (> (sprite-desktop-entry-score a)
                                           (sprite-desktop-entry-score b))))))
        (cl-incf sprite-desktop--total-count)
        (sprite-desktop--enable-await)
        (sprite-desktop--update-indicator)
        entry))))

(defun sprite-desktop-bump (entry)
  "Bump ENTRY to the front of the queue to pop next regardless of lane."
  (when entry
    (let ((old-lane (sprite-desktop-entry-lane entry)))
      (setf (alist-get old-lane sprite-desktop--queue)
            (delq entry (alist-get old-lane sprite-desktop--queue))))
    (setf (sprite-desktop-entry-lane entry) 'now)
    (push entry (alist-get 'now sprite-desktop--queue))
    entry))

(defun sprite-desktop--pop ()
  "Pop the highest-priority entry across all lanes."
  (let ((lanes '(now hot light heavy cold))
        popped)
    (while (and lanes (null popped))
      (let* ((lane (pop lanes))
             (entries (alist-get lane sprite-desktop--queue)))
        (when entries
          (setq popped (car entries))
          (setf (alist-get lane sprite-desktop--queue) (cdr entries)))))
    popped))

(defun sprite-desktop--has-pending-p ()
  "Return non-nil if any entries are pending in `sprite-desktop--queue'."
  (seq-some (lambda (lane) (not (null (alist-get lane sprite-desktop--queue))))
            '(now hot light heavy cold)))

(defun sprite-desktop--pending-entries ()
  "Return a flat list of all pending and running entries across lanes."
  (thread-last '(now hot light heavy cold)
    (mapcan (lambda (lane) (copy-sequence (alist-get lane sprite-desktop--queue))))
    (seq-filter (lambda (entry) (memq (sprite-desktop-entry-state entry) '(pending running))))))

(defun sprite-desktop--all-entries ()
  "Return all tracked entries across lanes."
  (thread-last '(now hot light heavy cold)
    (mapcan (lambda (lane) (copy-sequence (alist-get lane sprite-desktop--queue))))))

;;; Materialization & Await

(defun sprite-desktop--materialize (entry)
  "Materialize ENTRY into a live buffer."
  (when entry
    (setf (sprite-desktop-entry-state entry) 'running)
    (when-let* ((file (sprite-desktop-entry-file entry)))
      (remhash file sprite-desktop--index))
    (let* ((args (sprite-desktop-entry-args entry))
           (mode (sprite-desktop-entry-mode entry))
           (start (current-time))
           (buf nil))
      (save-window-excursion
        (condition-case e
            (let ((eglox-activation-policy 'when-visible)
                  (desktop-first-buffer nil)
                  (desktop-buffer-ok-count 0)
                  (desktop-buffer-fail-count 0))
              (setq buf (apply #'desktop-create-buffer args)))
          (error
           (message "[sprite-desktop] Failed to restore %s: %s"
                    (or (sprite-desktop-entry-file entry)
                        (sprite-desktop-entry-name entry)
                        "buffer")
                    (error-message-string e)))))
      (let ((dur-ms (* 1000.0 (float-time (time-since start)))))
        (if (and buf (buffer-live-p buf))
            (progn
              (setf (sprite-desktop-entry-state entry) 'done)
              (bury-buffer buf)
              (sprite-desktop--record-cost mode dur-ms)
              (when (and (bound-and-true-p eglox-warm-projects)
                         (fboundp 'eglox-warm-project))
                (eglox-warm-project buf)))
          (setf (sprite-desktop-entry-state entry) 'failed)
          (cl-incf sprite-desktop--failed-count)))
      (cl-incf sprite-desktop--completed-count)
      (sprite-desktop--log)
      (sprite-desktop--update-indicator)
      (unless (sprite-desktop--has-pending-p)
        (sprite-desktop--disable-await)
        (sprite-desktop--log t))
      buf)))

(defun ad:sprite-desktop-find-file-noselect (orig-fn filename &rest args)
  "Around advice on `find-file-noselect' awaiting pending desktop restoration."
  (if-let* ((filename filename)
            (truename (file-truename filename))
            (entry (gethash truename sprite-desktop--index)))
      (progn
        (let ((lane (sprite-desktop-entry-lane entry)))
          (setf (alist-get lane sprite-desktop--queue)
                (delq entry (alist-get lane sprite-desktop--queue))))
        (sprite-desktop--materialize entry)
        (apply orig-fn filename args))
    (apply orig-fn filename args)))

(defvar sprite-desktop--await-installed nil
  "Non-nil if `find-file-noselect' advice is active.")

(defun sprite-desktop--enable-await ()
  "Install `find-file-noselect' advice if not already installed."
  (unless sprite-desktop--await-installed
    (advice-add 'find-file-noselect :around #'ad:sprite-desktop-find-file-noselect)
    (setq sprite-desktop--await-installed t)))

(defun sprite-desktop--disable-await ()
  "Remove `find-file-noselect' advice."
  (when sprite-desktop--await-installed
    (advice-remove 'find-file-noselect #'ad:sprite-desktop-find-file-noselect)
    (setq sprite-desktop--await-installed nil)))

;;; Drain Engine

(defun sprite-desktop--drain-step ()
  "Drain a single entry from `sprite-desktop--queue'."
  (unless sprite-desktop--paused
    (when-let* ((entry (sprite-desktop--pop)))
      (sprite-desktop--materialize entry))))

(iter-defun sprite-desktop--drain-generator ()
  "Yield generator steps while entries remain in `sprite-desktop--queue'."
  (while (and (not sprite-desktop--paused)
              (sprite-desktop--has-pending-p))
    (iter-yield #'sprite-desktop--drain-step)))

(defun sprite-desktop--start-drain ()
  "Start the batch drain loop using `sprite-run-batch'."
  (when (and (not sprite-desktop--paused)
             (sprite-desktop--has-pending-p)
             (or (null sprite-desktop--cancel-fn)
                 ;; A quit/cancel leaves a stale cancel-fn behind.
                 (not (seq-some
                       (lambda (h) (eq (sprite-batch-handle-cancel-fn h)
                                       sprite-desktop--cancel-fn))
                       (sprite-batch-active-handles)))))
    (setq sprite-desktop--cancel-fn
          (sprite-run-batch
           :operations (sprite-desktop--drain-generator)
           :delay 0.05
           :name "desktop"
           :log-prefix "[desktop]"
           :on-done (lambda ()
                      (setq sprite-desktop--cancel-fn nil)
                      (unless (sprite-desktop--has-pending-p)
                        (sprite-desktop--disable-await)
                        (sprite-desktop--log t)))))))

;;; User Commands

;;;###autoload
(defun sprite-desktop-pause ()
  "Pause background desktop restoration."
  (interactive)
  (setq sprite-desktop--paused t)
  (when sprite-desktop--cancel-fn
    (funcall sprite-desktop--cancel-fn)
    (setq sprite-desktop--cancel-fn nil))
  (message "sprite-desktop: drain paused"))

;;;###autoload
(defun sprite-desktop-resume ()
  "Resume background desktop restoration."
  (interactive)
  (setq sprite-desktop--paused nil)
  (sprite-desktop--start-drain)
  (message "sprite-desktop: drain resumed"))

;;;###autoload
(defun sprite-desktop-restore-all ()
  "Restore all pending desktop buffers immediately."
  (interactive)
  (sprite-desktop-pause)
  (while (sprite-desktop--has-pending-p)
    (when-let* ((entry (sprite-desktop--pop)))
      (sprite-desktop--materialize entry)))
  (message "sprite-desktop: all entries restored"))

;;;###autoload
(defun sprite-desktop-restore-project (project)
  "Restore all pending buffers for PROJECT immediately."
  (interactive (list (completing-read
                      "Project: "
                      (thread-last (sprite-desktop--pending-entries)
                        (mapcar #'sprite-desktop-entry-project)
                        (seq-filter #'identity)
                        (seq-uniq)))))
  (let ((to-restore nil))
    (dolist (lane '(now hot light heavy cold))
      (let ((kept nil))
        (dolist (entry (alist-get lane sprite-desktop--queue))
          (if (equal (sprite-desktop-entry-project entry) project)
              (push entry to-restore)
            (push entry kept)))
        (setf (alist-get lane sprite-desktop--queue) (nreverse kept))))
    (dolist (entry (nreverse to-restore))
      (sprite-desktop--materialize entry))
    (message "sprite-desktop: restored %d buffers for %s" (length to-restore) project)))

;;; Generic Bulk Open API

;;;###autoload
(cl-defun sprite-desktop-open-files (files &key name project)
  "Enqueue FILES for staged background opening.
NAME and PROJECT provide metadata for queuing and prioritization.
Returns immediately.  Opening any listed file later awaits it."
  (dolist (f files)
    (when (and f (file-exists-p f))
      (let* ((truename (file-truename f))
             (buf-name (or name (file-name-nondirectory f)))
             (args (list 208 f buf-name nil nil 1 nil nil nil)))
        (sprite-desktop-enqueue args
                                :file truename
                                :name buf-name
                                :project project))))
  (sprite-desktop--start-drain))

;;; Persistence Round-Trip via desktop-save-hook

(defun sprite-desktop--restore-entry (args)
  "Restore or re-enqueue ARGS from a desktop file saved mid-drain."
  (if (bound-and-true-p sprite-desktop--collecting)
      (sprite-desktop-enqueue args)
    (apply #'desktop-append-buffer-args args)))

(defun sprite-desktop--save-pending ()
  "Insert pending desktop entries into desktop file buffer during save."
  (let ((pending (sprite-desktop--pending-entries)))
    (when pending
      (save-excursion
        (goto-char (point-max))
        (insert "\n;; -----------------------------------------------------------------------\n"
                ";; Pending Staged Desktop Entries\n")
        (dolist (entry pending)
          (let ((print-length nil)
                (print-level nil))
            (insert (format "(sprite-desktop--restore-entry '%S)\n"
                            (sprite-desktop-entry-args entry)))))))))

(add-hook 'desktop-save-hook #'sprite-desktop--save-pending)

;;; Collector & Entry Staging

(defvar sprite-desktop--collecting nil
  "Non-nil during `sprite-desktop-read'.")

(defun sprite-desktop--stage (entries)
  "Stage captured desktop ENTRIES, restoring eager set immediately."
  (setq sprite-desktop--total-count 0
        sprite-desktop--completed-count 0
        sprite-desktop--failed-count 0
        sprite-desktop--paused nil)
  (let ((rank 1))
    ;; Eager synchronous restore
    (dotimes (_ (min sprite-desktop-eager-count (length entries)))
      (when-let* ((args (pop entries)))
        (cl-incf sprite-desktop--total-count)
        (condition-case e
            (let ((buf (apply #'desktop-create-buffer args)))
              (when (and buf (buffer-live-p buf))
                (cl-incf sprite-desktop--completed-count)))
          (error
           (cl-incf sprite-desktop--failed-count)
           (message "[sprite-desktop] Eager restore error: %s"
                    (error-message-string e))))
        (cl-incf rank)))
    ;; Enqueue the remainder
    (dolist (args entries)
      (sprite-desktop-enqueue args :rank rank)
      (cl-incf rank)))
  (sprite-desktop--update-indicator)
  (sprite-desktop--log)
  (sprite-desktop--start-drain))

;;;###autoload
(defun sprite-desktop-read (&optional dirname)
  "Read and stage desktop file in DIRNAME without blocking input."
  (let ((entries nil)
        (sprite-desktop--collecting t))
    (cl-letf (((symbol-function 'desktop-create-buffer)
               (lambda (&rest args) (push args entries) nil))
              ((symbol-function 'desktop-append-buffer-args)
               (lambda (&rest args) (push args entries) nil))
              ((symbol-function 'desktop-lazy-create-buffer)
               (lambda (&rest args) (push args entries) nil)))
      (prog1 (desktop-read dirname)
        (sprite-desktop--stage (nreverse entries))))))

;;; Queue Menu, Progress Logging, & Indicator

(defun sprite-desktop--log (&optional force)
  "Log staged desktop progress to *Messages* at bounded rate."
  (let ((now (float-time)))
    (when (or force (>= (- now sprite-desktop--last-log-time) 10.0))
      (setq sprite-desktop--last-log-time now)
      (let* ((hot-count (length (alist-get 'hot sprite-desktop--queue)))
             (light-count (length (alist-get 'light sprite-desktop--queue)))
             (heavy-count (length (alist-get 'heavy sprite-desktop--queue)))
             (cold-count (length (alist-get 'cold sprite-desktop--queue)))
             (pending (+ hot-count light-count heavy-count cold-count
                         (length (alist-get 'now sprite-desktop--queue)))))
        (if (zerop pending)
            (message "sprite-desktop: restore complete (%d restored, %d failed)"
                     sprite-desktop--completed-count sprite-desktop--failed-count)
          (message "sprite-desktop: %d/%d restored (hot %d, light %d, heavy %d, cold %d pending)%s"
                   sprite-desktop--completed-count
                   sprite-desktop--total-count
                   hot-count light-count heavy-count cold-count
                   (if (> sprite-desktop--failed-count 0)
                       (format ", %d failed" sprite-desktop--failed-count)
                     "")))))))

(defun sprite-desktop--indicator-string ()
  "Generate mode-line indicator string."
  (if (sprite-desktop--has-pending-p)
      (let* ((str (format " ⧗ desktop %d/%d"
                          sprite-desktop--completed-count
                          sprite-desktop--total-count))
             (lsp-q (and (boundp 'eglox-activate-queue)
                         (length eglox-activate-queue))))
        (if (and lsp-q (> lsp-q 0))
            (concat str (format " · lsp %d" lsp-q))
          str))
    ""))

(defun sprite-desktop--update-indicator ()
  "Update `global-mode-string' with restoration indicator."
  (let ((ind (sprite-desktop--indicator-string)))
    (if (string-empty-p ind)
        (setq global-mode-string (remove '(:eval (sprite-desktop--indicator-string))
                                         global-mode-string))
      (unless (member '(:eval (sprite-desktop--indicator-string)) global-mode-string)
        (push '(:eval (sprite-desktop--indicator-string)) global-mode-string)))
    (force-mode-line-update t)))

;;; Tabulated Queue Mode

(defvar-keymap sprite-desktop-queue-mode-map
  :doc "Keymap for `sprite-desktop-queue-mode'."
  :parent tabulated-list-mode-map
  "b"   #'sprite-desktop-queue-bump
  "p"   #'sprite-desktop-queue-restore-project
  "d"   #'sprite-desktop-queue-drop
  "SPC" #'sprite-desktop-queue-toggle-pause
  "g"   #'sprite-desktop-queue-refresh)

(define-derived-mode sprite-desktop-queue-mode tabulated-list-mode "Desktop-Queue"
  "Major mode for inspecting and managing staged desktop entries."
  (setq tabulated-list-format
        [("State" 9 t)
         ("Lane" 7 t)
         ("Score" 7 t)
         ("Est" 7 t)
         ("Project" 18 t)
         ("File" 0 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun sprite-desktop-queue-refresh ()
  "Refresh the *sprite-desktop* buffer."
  (interactive)
  (when (derived-mode-p 'sprite-desktop-queue-mode)
    (setq tabulated-list-entries
          (mapcar (lambda (entry)
                    (let* ((file (or (sprite-desktop-entry-file entry)
                                     (sprite-desktop-entry-name entry)
                                     "unknown"))
                           (proj (or (sprite-desktop-entry-project entry) "-"))
                           (est (format "%dms" (or (sprite-desktop-entry-est-ms entry) 0)))
                           (score (format "%.1f" (or (sprite-desktop-entry-score entry) 0.0)))
                           (lane (symbol-name (or (sprite-desktop-entry-lane entry) 'cold)))
                           (state (symbol-name (or (sprite-desktop-entry-state entry) 'pending))))
                      (list entry (vector state lane score est proj file))))
                  (sprite-desktop--all-entries)))
    (tabulated-list-print t)))

;;;###autoload
(defun sprite-desktop-queue ()
  "Open the staged desktop queue inspection buffer."
  (interactive)
  (let ((buf (get-buffer-create "*sprite-desktop*")))
    (with-current-buffer buf
      (sprite-desktop-queue-mode)
      (sprite-desktop-queue-refresh))
    (pop-to-buffer buf)))

(defun sprite-desktop-queue-bump ()
  "Bump entry at point."
  (interactive)
  (when-let* ((entry (tabulated-list-get-id)))
    (sprite-desktop-bump entry)
    (sprite-desktop-queue-refresh)
    (message "Bumped %s" (or (sprite-desktop-entry-file entry) "entry"))))

(defun sprite-desktop-queue-restore-project ()
  "Restore all entries for project of entry at point."
  (interactive)
  (when-let* ((entry (tabulated-list-get-id))
              (proj (sprite-desktop-entry-project entry)))
    (sprite-desktop-restore-project proj)
    (sprite-desktop-queue-refresh)))

(defun sprite-desktop-queue-drop ()
  "Drop entry at point without restoring."
  (interactive)
  (when-let* ((entry (tabulated-list-get-id)))
    (let ((lane (sprite-desktop-entry-lane entry))
          (file (sprite-desktop-entry-file entry)))
      (setf (alist-get lane sprite-desktop--queue)
            (delq entry (alist-get lane sprite-desktop--queue)))
      (when file (remhash file sprite-desktop--index))
      (setf (sprite-desktop-entry-state entry) 'dropped)
      (sprite-desktop-queue-refresh)
      (message "Dropped %s" (or file "entry")))))

(defun sprite-desktop-queue-toggle-pause ()
  "Toggle pausing or resuming of the desktop drain loop."
  (interactive)
  (if sprite-desktop--paused
      (sprite-desktop-resume)
    (sprite-desktop-pause))
  (sprite-desktop-queue-refresh))

(provide 'sprite-desktop)
;;; sprite-desktop.el ends here
