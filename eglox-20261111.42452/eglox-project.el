;;; eglox-project.el --- Project experience and operations -*- lexical-binding: t; -*-

;;; Commentary:
;; Independent Projectile integration.  Enable `eglox-project-mode' explicitly;
;; loading this module does not enable Projectile or Eglot.  Cache files are
;; local, keyed by the complete project identity, and old in-tree files are left
;; untouched.  Project commands retain their historical names.

;;; Code:
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'project)
(require 'projectile)

(defgroup eglox-project nil "Project experience." :group 'tools)
(defcustom eglox-project-cache-directory
  (locate-user-emacs-file "state/projectile-cache/")
  "Local directory for persistent Projectile caches."
  :type 'directory :group 'eglox-project)
(defcustom eglox-project-mode-hooks '(prog-mode-hook)
  "Hooks on which `eglox-project-mode' queues the current project cache."
  :type '(repeat symbol) :group 'eglox-project)
(defcustom eglox-project-settings
  '((popper-group-function . popper-group-by-projectile)
    (compilation-save-buffers-predicate . project-current)
    (projectile-use-git-grep . t)
    (projectile-enable-caching . persistent)
    (projectile-require-project-root . nil))
  "Alist of `(SYMBOL . VALUE)' settings managed by `eglox-project-mode'."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'eglox-project)

(defvar eglox-project--saved-settings nil
  "Saved prior settings recorded by `eglox-project--apply-settings'.")
(defvar eglox-project--installed-hooks nil)
(defvar eglox-project--owns-projectile nil)
(defvar eglox-project--active nil)
(defvar projectile-projects-cache)
(defvar projectile-enable-caching)
(defvar projectile-project-changed-functions)
(defvar projectile-mode)
(defvar sprite-instance-id)
(declare-function projectile-mode "projectile")
(declare-function projectile-project-root "projectile")
(declare-function projectile-load-project-cache "projectile")
(declare-function projectile-index-project-async "projectile")
(declare-function gen-run-async "gen" (fn sequence &rest args))

(defun eglox-project--cache-file (_original &optional root)
  "Return a local cache filename for ROOT without touching its project tree."
  (when-let* ((root (or root (projectile-project-root))))
    (let* ((identity (directory-file-name (expand-file-name root)))
           (directory (file-name-as-directory (expand-file-name eglox-project-cache-directory))))
      (unless (file-directory-p directory)
        (make-directory directory t)
        (set-file-modes directory #o700))
      (expand-file-name (concat (secure-hash 'sha256 identity) ".eld") directory))))

(defun eglox-project--apply-settings ()
  "Apply `eglox-project-settings' and record prior default values."
  (unless eglox-project--saved-settings
    (seq-do (lambda (entry)
              (let ((symbol (car entry))
                    (value (cdr entry)))
                (push (cons symbol
                            (cons (default-boundp symbol)
                                  (and (default-boundp symbol)
                                       (default-value symbol))))
                      eglox-project--saved-settings)
                (set-default symbol value)))
            eglox-project-settings)))

(defun eglox-project--restore-settings ()
  "Restore settings saved by `eglox-project--apply-settings'."
  (seq-do (lambda (entry)
            (let ((symbol (car entry))
                  (bound (cadr entry))
                  (value (cddr entry)))
              (if bound
                  (set-default symbol value)
                (makunbound symbol))))
          eglox-project--saved-settings)
  (setq eglox-project--saved-settings nil))

;;;###autoload
(defun projectile-save-buffers-in-directory (dir)
  "Save all buffers visiting files within DIR using projectile."
  (let ((default-directory (expand-file-name dir)))
    (projectile-save-project-buffers)))

;;;###autoload
(defun projectile-save-project-buffers-picker ()
  "Interactively pick a project directory from open buffers and save its files."
  (interactive)
  (require 'annotated-completing-read)
  (projectile-save-buffers-in-directory
   (annotated-completing-read
    (thread-last
      (buffer-list)
      (seq-keep #'buffer-file-name)
      (seq-keep (lambda (f)
                  (when-let* ((proj (project-current nil (file-name-directory f))))
                    (project-root proj))))
      (seq-uniq)
      (seq-sort #'string<)
      (seq-map (lambda (dir) (cons dir nil))))
    :prompt "directories =>"
    :category 'file)))

(defun projectile-purge-file-from-cache-debounced (_orig-fn file)
  "Purge FILE from cache with debounced idle flush.
Replaces synchronous serialization with `projectile--schedule-cache-flush'."
  (let* ((project-root (projectile-project-root))
         (project-cache (map-elt projectile-projects-cache project-root)))
    (if (projectile-file-cached-p file project-root)
        (let ((new-cache (remove file project-cache)))
          (setf (map-elt projectile-projects-cache project-root) new-cache)
          (when (projectile-persistent-cache-p)
            (projectile--schedule-cache-flush project-root))
          (projectile--maybe-watch-project project-root new-cache)
          (projectile--message "%s removed from cache" file))
      (user-error "%s is not in the cache" file))))

(cl-defun projectile--run-incrementally (fn items &key label delay on-done (progress-interval 10))
  "Call FN on each of ITEMS cooperatively via `gen-run-async'.
FN returns non-nil when the item counted as handled.  LABEL names the
operation in progress messages.  Calls ON-DONE with the handled count,
or nil if an item fails.  Returns a `gen-run-async' handle for
cancellation, pausing, and progress queries."
  (require 'gen)
  (let ((total (length items))
        (count 0)
        (processed 0)
        (failed nil))
    (gen-run-async
     (lambda (item)
       (when (funcall fn item)
         (setq count (1+ count)))
       (setq processed (1+ processed))
       (when (and progress-interval
                  (or (= processed total)
                      (zerop (mod processed progress-interval))))
         (message "%s [%d/%d]..." label processed total)))
     items
     :name label
     :delay (or delay 0.0)
     :run-after (or delay 0.0)
     :on-done (lambda ()
                (if failed
                    (message "%s: failed after handling %d of %d." label count total)
                  (message "%s: handled %d of %d." label count total))
                (when on-done
                  (funcall on-done (unless failed count))))
     :on-error (lambda (err)
                 (setq failed t)
                 (message "%s: error %S" label err)))))

(defun projectile--set-mode-in-buffer (arg buf)
  "Call `projectile-mode' with ARG in live buffer BUF; return non-nil if it ran."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (projectile-mode arg))
    t))

(cl-defun projectile--toggle-all-buffers (arg &key delay on-done (progress-interval 10))
  "Call `projectile-mode' with ARG across all live buffers incrementally."
  (projectile--run-incrementally
   (apply-partially #'projectile--set-mode-in-buffer arg) (buffer-list)
   :label (if (> arg 0) "Enabling projectile-mode" "Disabling projectile-mode")
   :delay delay :on-done on-done :progress-interval progress-interval))

;;;###autoload
(cl-defun projectile-enable-all-buffers (&key delay on-done (progress-interval 10))
  "Enable `projectile-mode' in all live buffers without blocking."
  (interactive)
  (projectile--toggle-all-buffers 1 :delay delay :on-done on-done :progress-interval progress-interval))

;;;###autoload
(cl-defun projectile-disable-all-buffers (&key delay on-done (progress-interval 10))
  "Disable `projectile-mode' in all live buffers without blocking."
  (interactive)
  (projectile--toggle-all-buffers -1 :delay delay :on-done on-done :progress-interval progress-interval))

;;;###autoload
(cl-defun projectile-load-project-buffers (&optional project &key delay on-done (progress-interval 5))
  "Visit the files of PROJECT (default: current root) without blocking."
  (interactive)
  (let ((root (or project (projectile-project-root))))
    (unless root
      (user-error "Not in a project"))
    (let ((files (projectile-project-files root)))
      (unless files
        (user-error "No files found in project %s" root))
      (projectile--run-incrementally
       (lambda (file)
         (let ((path (expand-file-name file root)))
           (when (and (file-regular-p path)
                      (not (find-buffer-visiting path)))
             (find-file-noselect path)
             t)))
       files
       :label (format "Loading buffers for %s" (projectile-project-name root))
       :delay delay :on-done on-done :progress-interval progress-interval))))

;;;###autoload
(cl-defun projectile-session-restore-async (&optional project &key delay on-done)
  "Restore PROJECT's (default: current) saved session buffers without blocking."
  (interactive)
  (let* ((frame (selected-frame))
         (root (or project (projectile-project-root)))
         (data (and root (projectile-session--read root))))
    (cond
     (data
      (projectile--run-incrementally
       (lambda (saved)
         (buffer-live-p (ignore-errors (projectile-session--recreate-buffer saved))))
       (plist-get data :buffers)
       :label (format "Restoring session for %s" root)
       :delay delay :progress-interval nil
       :on-done (lambda (count)
                  (when-let* ((count)
                              ((frame-live-p frame))
                              (state (plist-get data :window-state)))
                    (condition-case err
                        (window-state-put (projectile-session--sanitize-window-state state)
                                          (frame-root-window frame) 'safe)
                      (error
                       (message "projectile-session: could not restore window layout: %S" err))))
                  (when on-done
                    (funcall on-done count)))))
     ((called-interactively-p 'any)
      (user-error "No saved session for %s" (or root "current project"))))))

(cl-defun projectile--filter-cache-incrementally (pred &optional project
                                                       &key label delay on-done
                                                       (chunk-size 1000)
                                                       (progress-interval 1))
  "Remove paths matching PRED from PROJECT's cache without blocking.
Calls PRED on each cached path.  When PRED returns non-nil, the path is purged.
LABEL names the operation in progress messages.  ON-DONE is called with
the count of purged files, or nil if the purge could not be committed."
  (let* ((root (or project (projectile-project-root)))
         (cache (and root (map-elt projectile-projects-cache root))))
    (unless root
      (user-error "Not in a project"))
    (unless cache
      (user-error "No cache found for project %s" root))
    (let* ((snapshot (copy-sequence cache))
           (chunks (seq-partition snapshot (max 1 chunk-size)))
           (kept-chunks nil)
           (purged-count 0))
      (projectile--run-incrementally
       (lambda (chunk)
         (let ((filtered (seq-remove pred chunk)))
           (setq purged-count (+ purged-count (- (length chunk) (length filtered))))
           (push filtered kept-chunks)
           t))
       chunks
       :label (or label (format "Purging cache for %s" (projectile-project-name root)))
       :delay delay
       :progress-interval progress-interval
       :on-done (lambda (count)
                  (cond
                   ((or (null count)
                        (not (equal snapshot (map-elt projectile-projects-cache root))))
                    (message "Cache purge for %s aborted: %s" root
                             (if count "cache changed during purge" "an item failed"))
                    (when on-done (funcall on-done nil)))
                   ((zerop purged-count)
                    (when on-done (funcall on-done 0)))
                   (t
                    (let ((new-cache (apply #'nconc (nreverse kept-chunks))))
                      (setf (map-elt projectile-projects-cache root) new-cache)
                      (when (projectile-persistent-cache-p)
                        (projectile--schedule-cache-flush root))
                      (projectile--maybe-watch-project root new-cache)
                      (when on-done
                        (funcall on-done purged-count))))))))))

;;;###autoload
(cl-defun projectile-purge-dir-from-cache (dir &key delay on-done (chunk-size 1000) (progress-interval 1))
  "Purge DIR from the current project's cache cooperatively.
ON-DONE receives the purge count, or nil if the operation fails.
Return a `gen-run-async' handle."
  (interactive
   (list (projectile-completing-read
          "Remove directory from cache: "
          (projectile-current-project-dirs)
          :caller 'projectile-read-directory)))
  (let ((root (projectile-project-root)))
    (projectile--filter-cache-incrementally
     (lambda (str) (string-prefix-p dir str))
     root
     :label (format "Purging %s from %s" dir (projectile-project-name root))
     :delay delay :chunk-size chunk-size :progress-interval progress-interval
     :on-done (lambda (count)
                (when count
                  (message "Purged %s from %s (%d files removed)."
                           dir (projectile-project-name root) count))
                (when on-done
                  (funcall on-done count))))))

;;;###autoload
(cl-defun projectile-purge-files-from-cache (files &optional project &key delay on-done (chunk-size 1000) (progress-interval 1))
  "Purge FILES from PROJECT's cache cooperatively.
ON-DONE receives the purge count, or nil if the operation fails.
Return a `gen-run-async' handle."
  (interactive
   (list (completing-read-multiple
          "Remove files from cache (comma-separated): "
          (projectile-current-project-files))))
  (let* ((root (or project (projectile-project-root)))
         (file-set (make-hash-table :test #'equal)))
    (seq-do (lambda (file) (setf (map-elt file-set file) t)) files)
    (projectile--filter-cache-incrementally
     (lambda (f) (map-elt file-set f))
     root
     :label (format "Purging files from %s" (projectile-project-name root))
     :delay delay :chunk-size chunk-size :progress-interval progress-interval
     :on-done (lambda (count)
                (when count
                  (message "Purged %d files from %s." count (projectile-project-name root)))
                (when on-done
                  (funcall on-done count))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projectile Cache Warming

(defconst eglox-projectile-warm-cache-idle-delay 3
  "Seconds of idle time between warming queued Projectile caches.")

(defconst eglox-projectile-warm-cache-chunk-size 3
  "Maximum cold project roots to index per idle timer tick.")

(defvar eglox-projectile-warm-cache-queue nil
  "Project roots queued for background Projectile indexing.")

(defvar eglox-projectile-warm-cache-idle-timer nil
  "Idle timer that drains `eglox-projectile-warm-cache-queue'.")

(defun eglox-projectile-cache-warm-p (root)
  "Return non-nil if ROOT has an in-memory or on-disk Projectile cache."
  (and (bound-and-true-p projectile-enable-caching)
       (or (map-elt projectile-projects-cache root)
           (and (eq projectile-enable-caching 'persistent)
                (projectile-load-project-cache root)))))

(defun eglox-projectile-warm-cache-process-queue ()
  "Index cold projects from `eglox-projectile-warm-cache-queue' in batches.
Cancels `eglox-projectile-warm-cache-idle-timer' when the queue is empty."
  (if (null eglox-projectile-warm-cache-queue)
      (when eglox-projectile-warm-cache-idle-timer
        (cancel-timer eglox-projectile-warm-cache-idle-timer)
        (setq eglox-projectile-warm-cache-idle-timer nil))
    (dotimes (_ eglox-projectile-warm-cache-chunk-size)
      (when-let* ((root (pop eglox-projectile-warm-cache-queue)))
        (unless (eglox-projectile-cache-warm-p root)
          (projectile-index-project-async root))))))

(defun eglox-projectile-warm-cache-enqueue (root)
  "Queue ROOT for background Projectile indexing if not already warm."
  (unless (or (member root eglox-projectile-warm-cache-queue)
              (eglox-projectile-cache-warm-p root))
    (setq eglox-projectile-warm-cache-queue
          (append eglox-projectile-warm-cache-queue (list root))))
  (when (and eglox-projectile-warm-cache-queue
             (not eglox-projectile-warm-cache-idle-timer))
    (setq eglox-projectile-warm-cache-idle-timer
          (run-with-idle-timer eglox-projectile-warm-cache-idle-delay t
                               #'eglox-projectile-warm-cache-process-queue))))

(defun eglox-projectile-warm-cache-for-buffer ()
  "Queue background Projectile indexing for the current buffer's project root."
  (when-let* ((root (and (bound-and-true-p projectile-mode)
                         (projectile-project-root))))
    (eglox-projectile-warm-cache-enqueue root)))

(defun eglox-projectile-warm-cache-on-project-change (_new-root _previous-root)
  "Warm Projectile cache when switching projects."
  (eglox-projectile-warm-cache-for-buffer))

;;;###autoload
(define-minor-mode eglox-project-mode
  "Enable configurable project defaults, cache policy, and mode hooks."
  :global t :group 'eglox-project
  (if eglox-project-mode
      (unless eglox-project--active
        (setq eglox-project--active t)
        (condition-case err
            (progn
              (eglox-project--apply-settings)
              (advice-add 'projectile-project-cache-file :around #'eglox-project--cache-file)
              (advice-add 'projectile-purge-file-from-cache :around #'projectile-purge-file-from-cache-debounced)
              (add-hook 'projectile-project-changed-functions #'eglox-projectile-warm-cache-on-project-change)
              (setq eglox-project--installed-hooks (copy-sequence eglox-project-mode-hooks))
              (seq-do (lambda (hook) (add-hook hook #'eglox-projectile-warm-cache-for-buffer))
                      eglox-project--installed-hooks)
              (unless (bound-and-true-p projectile-mode)
                (setq eglox-project--owns-projectile t)
                (projectile-mode 1)))
          (error
           (eglox-project-mode -1)
           (signal (car err) (cdr err)))))
    (remove-hook 'projectile-project-changed-functions #'eglox-projectile-warm-cache-on-project-change)
    (when (timerp eglox-projectile-warm-cache-idle-timer)
      (cancel-timer eglox-projectile-warm-cache-idle-timer)
      (setq eglox-projectile-warm-cache-idle-timer nil))
    (setq eglox-projectile-warm-cache-queue nil)
    (advice-remove 'projectile-project-cache-file #'eglox-project--cache-file)
    (advice-remove 'projectile-purge-file-from-cache #'projectile-purge-file-from-cache-debounced)
    (seq-do (lambda (hook) (remove-hook hook #'eglox-projectile-warm-cache-for-buffer)) eglox-project--installed-hooks)
    (setq eglox-project--installed-hooks nil)
    (when eglox-project--owns-projectile
      (when (bound-and-true-p projectile-mode)
        (projectile-mode -1))
      (setq eglox-project--owns-projectile nil))
    (eglox-project--restore-settings)
    (setq eglox-project--active nil)))

(provide 'eglox-project)
;;; eglox-project.el ends here
