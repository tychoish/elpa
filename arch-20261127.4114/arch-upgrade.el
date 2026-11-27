;;; arch-upgrade.el --- On-demand package upgrade groups -*- lexical-binding: t; -*-

;;; Commentary:
;; Configured package groups with a serial, asynchronous progress driver.
;; Full system upgrades remain available through `arch-upgrade-all'.

;;; Code:

(require 'arch)

(defcustom arch-upgrade-packages-list nil
  "Package names to upgrade with `arch-upgrade-packages'.
Packages must already be installed on the selected host.  AUR packages
use `arch-aur-install-method', including ABS rebuilds for development packages."
  :type '(repeat string)
  :group 'arch)

(defcustom arch-upgrade-package-groups nil
  "Alist of named package groups, such as ((daily . (\"foo\" \"bar\"))).
Each group is upgraded on demand with `arch-upgrade-group' on the selected host."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'arch)

(cl-defstruct (arch-upgrade--item (:constructor arch-upgrade--make-item))
  "Progress for one package upgrade."
  name (state 'pending) (message ""))

(defvar-local arch-upgrade--entries nil "Package upgrade items in execution order.")
(defvar-local arch-upgrade--backend nil "Backend captured for this upgrade session.")
(defvar-local arch-upgrade--aur-names nil "AUR package names in this session.")
(defvar-local arch-upgrade--active nil "Active or queued package item.")
(defvar-local arch-upgrade--paused nil "Whether to wait before the next package.")
(defvar-local arch-upgrade--cancelled nil "Whether this session was cancelled.")
(defvar-local arch-upgrade--callback nil "Callback identifying this session's worker.")
(defvar-local arch-upgrade--worker nil "Worker output buffer for this session.")

(defun arch-upgrade--names (packages)
  "Validate PACKAGES and return unique string names in their original order."
  (thread-last (flatten-tree packages)
               (seq-map (lambda (name)
                          (let ((name (if (symbolp name) (symbol-name name) name)))
                            (unless (and (stringp name)
                                         (string-match-p "\\`[[:alnum:]@+_.][[:alnum:]@+_.:-]*\\'" name))
                              (user-error "Invalid package name: %S" name))
                            name)))
               (seq-uniq)))

;;;###autoload
(defun arch-register-upgrade-package (&rest packages)
  "Append unique PACKAGES to `arch-upgrade-packages-list'.
Accept strings, symbols, or lists of either.  Return the resulting list."
  (setq arch-upgrade-packages-list
        (arch-upgrade--names (append arch-upgrade-packages-list packages))))

(defun arch-upgrade--refresh ()
  "Refresh the current progress buffer without moving the reader."
  (setq tabulated-list-entries
        (seq-map (lambda (item)
                   (list (arch-upgrade--item-name item)
                         (vector (arch-upgrade--item-name item)
                                 (symbol-name (arch-upgrade--item-state item))
                                 (arch-upgrade--item-message item))))
                 arch-upgrade--entries))
  (setq header-line-format
        (format "%s | %s | %d/%d done | 1 worker | %s"
                (or arch-host "local") arch-default-backend
                (seq-count (lambda (item) (eq (arch-upgrade--item-state item) 'done))
                           arch-upgrade--entries)
                (length arch-upgrade--entries)
                (cond (arch-upgrade--cancelled "Cancelled")
                      (arch-upgrade--paused "Paused: c continue, R retry")
                      (arch-upgrade--active "Working (RET: output)")
                      (t "Finished"))))
  (tabulated-list-print t))

(defun arch-upgrade--finish (buffer item process)
  "Update ITEM in BUFFER after PROCESS finishes, then run the next item."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq arch-upgrade--active nil)
      (cond
       (arch-upgrade--cancelled
        (setf (arch-upgrade--item-state item) 'cancelled))
       ((and process (eq (process-status process) 'exit)
             (zerop (process-exit-status process)))
        (setf (arch-upgrade--item-state item) 'done)
        (setf (arch-upgrade--item-message item) ""))
       (t
        (setq arch-upgrade--paused t)
        (setf (arch-upgrade--item-state item) 'failed)
        (setf (arch-upgrade--item-message item)
              (if process
                  (format "%s: %d" (process-status process) (process-exit-status process))
                "Could not start worker"))))
      (arch-upgrade--refresh)
      (arch-upgrade--step buffer))))

(defun arch-upgrade--step (buffer)
  "Start the next pending package in progress BUFFER when ready."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (or arch-upgrade--active arch-upgrade--paused arch-upgrade--cancelled)
        (when-let* ((item (seq-find (lambda (entry) (eq (arch-upgrade--item-state entry) 'pending))
                                    arch-upgrade--entries)))
          (setq arch-upgrade--active item)
          (setf (arch-upgrade--item-state item) 'active)
          (setf (arch-upgrade--item-message item) "Queued/running")
          (setq arch-upgrade--callback
                (lambda (process) (arch-upgrade--finish buffer item process)))
          (arch-upgrade--refresh)
          (condition-case err
              (let ((arch--operation-callback arch-upgrade--callback)
                    (name (arch-upgrade--item-name item)))
                (if (member name arch-upgrade--aur-names)
                    (pcase arch-aur-install-method
                      ('abs
                       (let ((arch--aur-operation 'upgrade))
                         (funcall (or (arch-backend-abs-install-fn (arch--aur-backend))
                                      (user-error "Backend does not support ABS")) name)))
                      ('helper
                       (funcall (or (arch-backend-aur-batch-fn (arch--aur-backend))
                                    (user-error "Backend does not support AUR helper upgrades"))
                                (list name) 'upgrade))
                      (_ (user-error "Invalid AUR install method")))
                  (funcall (or (arch-backend-upgrade-fn arch-upgrade--backend)
                               (user-error "Backend does not support upgrades")) name)))
            (error
             (setq arch-upgrade--active nil)
             (setq arch-upgrade--paused t)
             (setf (arch-upgrade--item-state item) 'failed)
             (setf (arch-upgrade--item-message item) (error-message-string err))
             (arch-upgrade--refresh))))))))

(defun arch-upgrade-toggle-pause ()
  "Toggle waiting before the next package; active work can finish."
  (interactive)
  (setq arch-upgrade--paused (not arch-upgrade--paused))
  (arch-upgrade--refresh)
  (arch-upgrade--step (current-buffer)))

(defun arch-upgrade-continue ()
  "Continue pending packages after a pause or failure."
  (interactive)
  (when arch-upgrade--cancelled (user-error "Session cancelled; retry a package with R"))
  (setq arch-upgrade--paused nil)
  (arch-upgrade--refresh)
  (arch-upgrade--step (current-buffer)))

(defun arch-upgrade-retry ()
  "Retry the failed or cancelled package at point."
  (interactive)
  (when arch-upgrade--active (user-error "Wait for the active package to finish"))
  (let ((item (seq-find (lambda (entry) (equal (arch-upgrade--item-name entry)
                                               (tabulated-list-get-id)))
                        arch-upgrade--entries)))
    (unless (and item (memq (arch-upgrade--item-state item) '(failed cancelled)))
      (user-error "Select a failed or cancelled package"))
    (setf (arch-upgrade--item-state item) 'pending)
    (setf (arch-upgrade--item-message item) "")
    (setq arch-upgrade--cancelled nil)
    (arch-upgrade-continue)))

(defun arch-upgrade-cancel ()
  "Cancel pending work and interrupt this session's active worker."
  (interactive)
  (setq arch-upgrade--cancelled t)
  (setq arch--worker-queue
        (seq-remove (lambda (operation) (eq (nth 4 operation) arch-upgrade--callback))
                    arch--worker-queue))
  (seq-do (lambda (item)
            (when (memq (arch-upgrade--item-state item) '(pending active))
              (setf (arch-upgrade--item-state item) 'cancelled)))
          arch-upgrade--entries)
  (if-let* ((buffer (and (buffer-live-p arch-upgrade--worker) arch-upgrade--worker))
            (process (get-buffer-process buffer))
            ((eq (process-get process 'arch-callback) arch-upgrade--callback))
            ((process-live-p process)))
      (interrupt-process process)
    (setq arch-upgrade--active nil))
  (arch-upgrade--refresh))

(defun arch-upgrade-show-output ()
  "Display this session's worker output."
  (interactive)
  (unless (buffer-live-p arch-upgrade--worker) (user-error "Worker buffer no longer exists"))
  (display-buffer arch-upgrade--worker))

(defun arch-upgrade--kill-query ()
  "Ask before killing the progress buffer of an active session."
  (if arch-upgrade--active
      (when (yes-or-no-p "Cancel this package upgrade session? ")
        (arch-upgrade-cancel)
        t)
    t))

(defvar-keymap arch-upgrade-progress-mode-map
  :parent tabulated-list-mode-map
  "p" #'arch-upgrade-toggle-pause
  "SPC" #'arch-upgrade-toggle-pause
  "c" #'arch-upgrade-continue
  "R" #'arch-upgrade-retry
  "k" #'arch-upgrade-cancel
  "RET" #'arch-upgrade-show-output
  "q" #'quit-window)

(define-derived-mode arch-upgrade-progress-mode tabulated-list-mode "Arch-Upgrade"
  "Progress for serial upgrades; p pauses, c continues, R retries, k cancels."
  (setq tabulated-list-format [("Package" 32 t) ("State" 12 t) ("Detail" 0 nil)])
  (tabulated-list-init-header)
  (add-hook 'kill-buffer-query-functions #'arch-upgrade--kill-query nil t))

;;;###autoload
(defun arch-upgrade-packages (&optional packages)
  "Upgrade installed PACKAGES, defaulting to `arch-upgrade-packages-list'.
Confirm once, then execute asynchronously with per-package progress.
Return the progress buffer, or nil when confirmation is declined."
  (interactive)
  (let* ((names (or (arch-upgrade--names (or packages arch-upgrade-packages-list))
                    (user-error "No upgrade packages configured")))
         (backend (arch--default-backend))
         (installed (funcall (arch-backend-list-fn backend)))
         (missing (seq-remove (lambda (name)
                                (seq-find (lambda (pkg) (equal (arch-pkg-name pkg) name)) installed))
                              names))
         (host arch-host)
         (backend-name arch-default-backend)
         (aur-backend arch-aur-backend)
         (aur-method arch-aur-install-method)
         (foreign (when-let* ((fn (arch-backend-foreign-fn backend))) (funcall fn)))
         (buffer (get-buffer (arch--buffer-name "*arch-upgrade-progress*"))))
    (when missing (user-error "Packages not installed on %s: %s" (or host "local") (string-join missing ", ")))
    (when (and buffer
               (with-current-buffer buffer
                 (or arch-upgrade--active
                     (and (not arch-upgrade--cancelled)
                          (seq-some (lambda (item) (eq (arch-upgrade--item-state item) 'pending))
                                    arch-upgrade--entries)))))
      (user-error "An upgrade session is already active for this host"))
    (when (yes-or-no-p (format "Upgrade %d packages on %s? " (length names) (or host "local")))
      (setq buffer (get-buffer-create (arch--buffer-name "*arch-upgrade-progress*")))
      (with-current-buffer buffer
        (arch-upgrade-progress-mode)
        (arch--set-buffer-context host backend-name)
        (setq-local arch-aur-backend aur-backend)
        (setq-local arch-aur-install-method aur-method)
        (setq arch-upgrade--backend backend)
        (setq arch-upgrade--entries (seq-map (lambda (name) (arch-upgrade--make-item :name name)) names))
        (setq arch-upgrade--aur-names
              (seq-filter (lambda (name)
                            (or (map-elt foreign name)
                                (seq-some (lambda (pkg) (and (equal (arch-pkg-name pkg) name)
                                                             (arch-pkg-aur-p pkg))) installed))) names))
        (setq arch-upgrade--worker (arch--worker-buffer))
        (arch-upgrade--refresh))
      (pop-to-buffer buffer)
      (arch-upgrade--step buffer)
      buffer)))

;;;###autoload
(defun arch-upgrade-group (group)
  "Upgrade the configured package GROUP on the selected host."
  (interactive
   (list (intern (completing-read "Upgrade group: "
                                  (seq-map (lambda (entry) (symbol-name (car entry)))
                                           arch-upgrade-package-groups) nil t))))
  (arch-upgrade-packages
   (or (cdr (assq group arch-upgrade-package-groups))
       (user-error "Unknown or empty package group: %s" group))))

(provide 'arch-upgrade)
;;; arch-upgrade.el ends here
