;;; sprite-state.el --- Opt-in instance state and desktop lifecycle -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/tychoish/sprite
;; Keywords: convenience

;;; Commentary:
;; Configure selected state paths and file locks early with
;; `sprite-state-configure'.  Independently opt into desktop persistence with
;; `sprite-state-desktop-enabled' and enable `sprite-state-global-mode' at the
;; desired restore point.  No optional package is required by these mappings.

;;; Code:

(require 'sprite)
(require 'seq)

(defgroup sprite-state nil
  "Opt-in state persistence for sprite instances."
  :group 'convenience)

(defcustom sprite-state-mappings nil
  "Alist mapping state variable symbols to instance-scoped relative names.
Call `sprite-state-configure' before the relevant packages initialize.
No mappings are enabled by default; selecting one does not load its package."
  :type '(alist :key-type symbol :value-type string)
  :group 'sprite-state)

(defcustom sprite-state-locks-enabled nil
  "When non-nil, `sprite-state-configure' sets up instance file locks."
  :type 'boolean
  :group 'sprite-state)

(defcustom sprite-state-auto-save-enabled nil
  "When non-nil, `sprite-state-configure' sets up instance auto-save.
This also sets up instance backup paths."
  :type 'boolean
  :group 'sprite-state)

(defcustom sprite-state-desktop-enabled nil
  "When non-nil, `sprite-state-global-mode' persists named instance desktops.
Set this before enabling the mode.  Solo instances never restore or save."
  :type 'boolean
  :group 'sprite-state)

(defcustom sprite-state-desktop-deferred nil
  "When non-nil, start desktop explicitly with `sprite-state-start-desktop'.
The state mode may then start history before restoring the desktop."
  :type 'boolean :group 'sprite-state)

(defcustom sprite-state-desktop-idle-interval 120
  "Idle seconds between desktop save checks.
Set before enabling `sprite-state-global-mode'; changes take effect when
it is next enabled."
  :type 'natnum
  :group 'sprite-state)

(defcustom sprite-state-savehist-enabled nil
  "When non-nil, manage Savehist while the state mode is enabled."
  :type 'boolean :group 'sprite-state)

(defcustom sprite-state-recentf-enabled nil
  "When non-nil, manage Recentf while the state mode is enabled."
  :type 'boolean :group 'sprite-state)

(defcustom sprite-state-settings
  '((recentf-auto-cleanup . never)
    (recentf-keep file-remote-p file-readable-p)
    (recentf-max-menu-items . 100)
    (bookmark-save-flag . 1)
    (create-lockfiles . nil))
  "Alist of `(SYMBOL . VALUE)' settings managed by `sprite-state-global-mode'."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'sprite-state)

(defcustom sprite-state-desktop-settings
  '((desktop-save . t)
    (desktop-restore-frames . nil)
    (desktop-restore-in-current-display . nil)
    (desktop-restore-eager . t))
  "Alist of `(SYMBOL . VALUE)' settings managed by `sprite-state-start-desktop'."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'sprite-state)

(defvar sprite-state--saved-settings nil
  "Saved prior settings recorded by `sprite-state--apply-settings'.")
(defvar sprite-state--owned-modes nil)
(defvar sprite-state-global-mode)

(defun sprite-state--apply-settings (&optional settings)
  "Apply SETTINGS (default `sprite-state-settings').
Record prior default values."
  (seq-do (lambda (entry)
            (let ((symbol (car entry))
                  (value (cdr entry)))
              (unless (assq symbol sprite-state--saved-settings)
                (push (cons symbol
                            (cons (default-boundp symbol)
                                  (and (default-boundp symbol)
                                       (default-value symbol))))
                      sprite-state--saved-settings))
              (set-default symbol value)))
          (or settings sprite-state-settings)))

(defun sprite-state--restore-settings ()
  "Restore settings saved by `sprite-state--apply-settings'."
  (seq-do (lambda (entry)
            (let ((symbol (car entry))
                  (bound (cadr entry))
                  (value (cddr entry)))
              (if bound
                  (set-default symbol value)
                (makunbound symbol))))
          sprite-state--saved-settings)
  (setq sprite-state--saved-settings nil))

(defun sprite-state--stop-owned-modes (&optional save)
  "Stop owned persistence modes, optionally SAVE history first.
Return the first error after attempting every cleanup operation."
  (let (failure)
    (when (and save (memq 'savehist-mode sprite-state--owned-modes)
               (bound-and-true-p savehist-mode))
      (condition-case err (savehist-save)
        (error (setq failure err))))
    (seq-do (lambda (mode)
              (condition-case err (funcall mode -1)
                (error (unless failure (setq failure err)))))
            sprite-state--owned-modes)
    (setq sprite-state--owned-modes nil)
    failure))

(defun sprite-state--start-mode (feature mode)
  "Load FEATURE and enable MODE if it is not already active."
  (require feature)
  (unless (symbol-value mode)
    ;; Record ownership first so partial startup can be rolled back.
    (push mode sprite-state--owned-modes)
    (funcall mode 1)))

(defvar sprite-state--desktop-timer nil)
(defvar sprite-state--last-save-time nil)
(defvar sprite-state--desktop-directory nil)
(defvar sprite-state--active nil)
(defvar sprite-state--restored-file nil)
(defvar lock-file-name-transforms)
(defvar auto-save-file-name-transforms)
(defvar backup-directory-alist)
(defvar desktop-base-file-name)
(declare-function savehist-save "savehist" (&optional auto-save))
(declare-function desktop-read "desktop" (dirname))
(declare-function desktop-save "desktop" (dirname &optional release))

;;;###autoload
(defun sprite-state-configure ()
  "Apply selected instance state mappings and optional lock policy.
Call before consumers initialize.  This does not load optional packages."
  (seq-do (lambda (mapping)
            (set-default (car mapping) (sprite-state-path (cdr mapping))))
          sprite-state-mappings)
  (when sprite-state-locks-enabled
    (let ((path (if (equal (sprite-instance-name) "solo")
                    (file-name-concat
                     (seq-find #'file-exists-p
                               (list (format "/run/user/%d" (user-uid))
                                     "/var/tmp" (temporary-file-directory)))
                     (format "emacs-%d" (emacs-pid)))
                  (sprite-state-path "locks/"))))
      (make-directory path t)
      (set-file-modes path #o700)
      (setq lock-file-name-transforms
            `(("\\`/.*/\\([^/]+\\)\\'" ,(concat (file-name-as-directory path) "\\1") t)))))
  (when sprite-state-auto-save-enabled
    (sprite-state-setup-auto-save)))

;;;###autoload
(defun sprite-state-setup-auto-save ()
  "Configure auto-save and backup directory paths for this instance."
  (let ((path (sprite-state-path "backup/")))
    (setq auto-save-file-name-transforms `((".*" ,path t)))
    (add-to-list 'backup-directory-alist (cons "." path))
    (unless (file-exists-p path)
      (make-directory path t))
    (set-file-modes path #o700)))

(defun sprite-state--desktop-save ()
  "Save the desktop probabilistically, or after 150 seconds."
  (when (or (> 40 (random 100))
            (< 150 (float-time (time-since sprite-state--last-save-time))))
    (desktop-save sprite-state--desktop-directory)
    (setq sprite-state--restored-file
          (expand-file-name desktop-base-file-name sprite-state--desktop-directory)
          sprite-state--last-save-time (current-time))))

;;;###autoload
(defun sprite-state-start-desktop ()
  "Start desktop persistence after the state mode is enabled.
This may be called later in startup after setting
`sprite-state-desktop-enabled'; repeated calls do not add timers."
  (when (and sprite-state-global-mode sprite-state-desktop-enabled
             (not sprite-state--desktop-timer)
             (not (equal (sprite-instance-name) "solo")))
    (require 'desktop)
    (setq sprite-state--desktop-directory
          (file-name-directory (sprite-state-path "desktop.el")))
    (make-directory sprite-state--desktop-directory t)
    (sprite-state--apply-settings
     (append sprite-state-desktop-settings
             `((desktop-base-file-name . ,(sprite-state-file-prefix "desktop.el"))
               (desktop-base-lock-name . ,(sprite-state-file-prefix
                                           (format "desktop-%d.lock" (emacs-pid))))
               (desktop-path . ,(list sprite-state--desktop-directory
                                      user-emacs-directory
                                      (expand-file-name "~/")))
               (desktop-load-locked-desktop . ,(and (daemonp) t)))))
    (setq sprite-state--last-save-time (current-time))
    (let ((file (expand-file-name desktop-base-file-name
                                  sprite-state--desktop-directory)))
      (when (and (file-exists-p file)
                 (not (equal file sprite-state--restored-file)))
        (unless (eq t (desktop-read sprite-state--desktop-directory))
          (error "Unable to read desktop in %s" sprite-state--desktop-directory))
        (setq sprite-state--restored-file file)))
    (setq sprite-state--desktop-timer
          (run-with-idle-timer sprite-state-desktop-idle-interval t
                               #'sprite-state--desktop-save))))

;;;###autoload
(define-minor-mode sprite-state-global-mode
  "Manage the opt-in state lifecycle for this instance.
When `sprite-state-desktop-enabled' is non-nil, named instances restore
once per process and save on a repeating idle timer.  Re-enabling after
disable does not restore the same desktop again.  Disabling cancels the
timer, but cannot undo restored buffers or safely revert changed state
variables.  This mode is
independent of `sprite-mode'."
  :global t
  :group 'sprite-state
  (cond
   ((not sprite-state-global-mode)
    (let ((failure (sprite-state--stop-owned-modes t)))
      (when sprite-state--desktop-timer
        (cancel-timer sprite-state--desktop-timer)
        (setq sprite-state--desktop-timer nil))
      (sprite-state--restore-settings)
      (setq sprite-state--active nil)
      (when failure (signal (car failure) (cdr failure)))))
   ((not sprite-state--active)
    (condition-case err
        (progn
          (sprite-state-configure)
          (sprite-state--apply-settings)
          (when sprite-state-savehist-enabled
            (sprite-state--start-mode 'savehist 'savehist-mode))
          (let ((inhibit-message t)
                (message-log-max nil))
            (sprite-state--start-mode 'recentf 'recentf-mode))
          (unless sprite-state-desktop-deferred
            (sprite-state-start-desktop))
          (setq sprite-state--active t))
      (error (sprite-state--stop-owned-modes)
             (when sprite-state--desktop-timer
               (cancel-timer sprite-state--desktop-timer)
               (setq sprite-state--desktop-timer nil))
             (sprite-state--restore-settings)
             (setq sprite-state-global-mode nil)
             (signal (car err) (cdr err)))))))

(provide 'sprite-state)
;;; sprite-state.el ends here
