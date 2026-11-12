;;; cmdr-frame.el --- Frame and display management minor mode -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: convenience, frames
;; URL: https://github.com/tychoish/cmdr.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Opt-in global minor mode for frame transparency, contextual menubars,
;; buffer navigation predicates, read-only window reuse, and terminal display
;; defaults.

;;; Code:

(require 'seq)
(require 'cmdr-ops)

(defgroup cmdr-frame nil
  "Frame and display configuration for cmdr."
  :group 'convenience)

(defcustom cmdr-frame-opacity 95
  "Default frame opacity percentage when graphical."
  :type 'integer
  :group 'cmdr-frame)

(defcustom cmdr-frame-contextual-menubar (eq system-type 'darwin)
  "When non-nil, show menubar on graphical frames and hide on TTY frames."
  :type 'boolean
  :group 'cmdr-frame)

(defcustom cmdr-frame-tty-transparent t
  "When non-nil, unset default face background/foreground on TTY frames."
  :type 'boolean
  :group 'cmdr-frame)

(defcustom cmdr-frame-buffer-predicate t
  "When non-nil, install unified buffer predicate on frames."
  :type 'boolean
  :group 'cmdr-frame)

(defcustom cmdr-frame-reuse-readonly-windows t
  "When non-nil, configure `display-buffer-alist' to reuse read-only file windows."
  :type 'boolean
  :group 'cmdr-frame)

;;; Buffer home frame and frame buffer predicate

(defvar-local cmdr-frame--buffer-home-frame nil
  "Frame where this buffer was first shown; nil means no frame restriction.")

;;;###autoload
(defun cmdr-frame-record-home-frame ()
  "Record the selected frame as this buffer's home frame (first visit only)."
  (unless cmdr-frame--buffer-home-frame
    (setq-local cmdr-frame--buffer-home-frame (selected-frame))))

(defun cmdr-frame-buffer-predicate (buf)
  "Unified predicate controlling next-buffer and switch-to-buffer candidates."
  (let* ((current (current-buffer))
         (current-file (buffer-file-name current))
         (current-readonly (buffer-local-value 'buffer-read-only current)))
    (with-current-buffer buf
      (and
       ;; Rule 1: frame-sticky buffers stay on their home frame
       (or (null cmdr-frame--buffer-home-frame)
           (eq cmdr-frame--buffer-home-frame (selected-frame)))
       ;; Rules 2 & 3: file-based navigation restrictions
       (cond
        ;; Not in a file buffer -- no restriction
        ((null current-file) t)
        ;; In a read-only file (reference buffer) -- only writable files
        (current-readonly (and (buffer-file-name) (not buffer-read-only)))
        ;; In a writable file -- only file buffers (any)
        (t (buffer-file-name)))))))

(defun cmdr-frame-install-buffer-predicate (frame)
  "Install the unified buffer predicate on FRAME."
  (set-frame-parameter frame 'buffer-predicate #'cmdr-frame-buffer-predicate))

;;; Read-only window reuse display policy

(defun cmdr-frame-readonly-file-buffer-p (buf _action)
  "Return non-nil if BUF is a read-only file-visiting buffer."
  (with-current-buffer buf
    (and buffer-read-only (buffer-file-name))))

(defun cmdr-frame-reuse-readonly-file-window (buffer _alist)
  "Action: display BUFFER in an existing read-only file window if one exists."
  (when-let* ((win (seq-find
                    (lambda (w)
                      (and (not (eq w (selected-window)))
                           (with-current-buffer (window-buffer w)
                             (and buffer-read-only (buffer-file-name)))))
                    (window-list nil 'nomini))))
    (set-window-buffer win buffer)
    win))

(defconst cmdr-frame-readonly-display-buffer-entry
  '(cmdr-frame-readonly-file-buffer-p
    (cmdr-frame-reuse-readonly-file-window
     display-buffer-use-some-window))
  "`display-buffer-alist' entry for read-only file buffers.")

;;; Frame setup and theme hooks

(defun cmdr-frame--setup-frame (frame)
  "Apply frame settings to FRAME."
  (when (and cmdr-frame-opacity (display-graphic-p frame))
    (ensure-frame-opacity cmdr-frame-opacity frame))
  (when cmdr-frame-contextual-menubar
    (contextual-menubar frame))
  (when cmdr-frame-tty-transparent
    (frame-unset-background-for-tty frame))
  (when cmdr-frame-buffer-predicate
    (cmdr-frame-install-buffer-predicate frame)))

(defun cmdr-frame--reapply-after-theme (&rest _theme)
  "Re-apply frame display and opacity settings after a theme is loaded."
  (when cmdr-frame-tty-transparent
    (seq-do #'frame-unset-background-for-tty (frame-list)))
  (when cmdr-frame-opacity
    (dolist (f (frame-list))
      (when (display-graphic-p f)
        (ensure-frame-opacity cmdr-frame-opacity f)))))


;;;###autoload
(define-minor-mode cmdr-frame-mode
  "Global minor mode managing frame transparency, buffer navigation, and display."
  :global t
  :group 'cmdr-frame
  (if cmdr-frame-mode
      (progn
        (when cmdr-frame-opacity
          (ensure-frame-opacity cmdr-frame-opacity))
        (when cmdr-frame-reuse-readonly-windows
          (add-to-list 'display-buffer-alist cmdr-frame-readonly-display-buffer-entry))
        (add-hook 'after-make-frame-functions #'cmdr-frame--setup-frame)
        (add-hook 'enable-theme-functions #'cmdr-frame--reapply-after-theme)
        (seq-do #'cmdr-frame--setup-frame (frame-list)))
    (remove-hook 'after-make-frame-functions #'cmdr-frame--setup-frame)
    (remove-hook 'enable-theme-functions #'cmdr-frame--reapply-after-theme)
    (setq display-buffer-alist
          (delete cmdr-frame-readonly-display-buffer-entry display-buffer-alist))))

(provide 'cmdr-frame)
;;; cmdr-frame.el ends here
