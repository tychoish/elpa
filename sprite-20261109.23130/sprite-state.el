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

(defcustom sprite-state-desktop-idle-interval 120
  "Idle seconds between desktop save checks.
Set before enabling `sprite-state-global-mode'; changes take effect when
it is next enabled."
  :type 'natnum
  :group 'sprite-state)

(defvar sprite-state--desktop-timer nil)
(defvar sprite-state--last-save-time nil)
(defvar sprite-state--desktop-directory nil)
(defvar sprite-state--active nil)
(defvar sprite-state--restored-file nil)
(defvar lock-file-name-transforms)
(defvar auto-save-file-name-transforms)
(defvar backup-directory-alist)
(defvar desktop-base-file-name)
(defvar desktop-base-lock-name)
(defvar desktop-path)
(defvar desktop-save)
(defvar desktop-restore-frames)
(defvar desktop-restore-in-current-display)
(defvar desktop-restore-eager)
(defvar desktop-load-locked-desktop)
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
    (when sprite-state--desktop-timer
      (cancel-timer sprite-state--desktop-timer)
      (setq sprite-state--desktop-timer nil))
    (setq sprite-state--active nil))
   ((not sprite-state--active)
    (condition-case err
        (progn
          (when (and sprite-state-desktop-enabled
                     (not (equal (sprite-instance-name) "solo")))
            (require 'desktop)
            (setq sprite-state--desktop-directory
                  (file-name-directory (sprite-state-path "desktop.el")))
            (make-directory sprite-state--desktop-directory t)
            (setq desktop-base-file-name (sprite-state-file-prefix "desktop.el")
                  desktop-base-lock-name (sprite-state-file-prefix
                                          (format "desktop-%d.lock" (emacs-pid)))
                  desktop-path (list sprite-state--desktop-directory
                                     user-emacs-directory (expand-file-name "~/"))
                  desktop-save t
                  desktop-restore-frames nil
                  desktop-restore-in-current-display nil
                  desktop-restore-eager t
                  desktop-load-locked-desktop (and (daemonp) t)
                  sprite-state--last-save-time (current-time))
            (let ((file (expand-file-name desktop-base-file-name
                                          sprite-state--desktop-directory)))
              (when (and (file-exists-p file)
                         (not (equal file sprite-state--restored-file)))
                (unless (eq t (desktop-read sprite-state--desktop-directory))
                  (error "Unable to read desktop in %s" sprite-state--desktop-directory))
                (setq sprite-state--restored-file file)))
            (setq sprite-state--desktop-timer
                  (run-with-idle-timer sprite-state-desktop-idle-interval t
                                       #'sprite-state--desktop-save)))
          (setq sprite-state--active t))
      (error (setq sprite-state-global-mode nil)
             (signal (car err) (cdr err)))))))

(provide 'sprite-state)
;;; sprite-state.el ends here
