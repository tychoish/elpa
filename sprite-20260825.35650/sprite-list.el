;;; sprite-list.el --- Tabulated overview buffer for sprite.el -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (seq "2.24") (transient "0.4"))
;; Keywords: tools, daemon, processes

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The tabulated overview buffer for sprite.el -- split out of sprite.el so
;; that the core instance-id/state-path machinery (needed early and often)
;; doesn't drag in `tabulated-list' and `transient' just to support this one
;; interactive management screen.
;;
;; Entry point: `sprite-list'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)
(require 'sprite)

(declare-function annotated-completing-read "annotated-completing-read")

(defun sprite--query-buffer-count (full-name)
  "Try to get the buffer count from sprite FULL-NAME.  Returns integer or nil."
  (when-let* ((n (with-sprite full-name (length (buffer-list)) :no-log t))
              ((numberp n)))
    n))

(defun sprite--for-current-parent-p (s)
  "Return t if sprite S belongs to the current parent instance."
  (equal (sprite-parent s) (sprite-instance-name)))

(defun sprite--build-list-entry (s)
  "Build a `tabulated-list' entry for sprite struct S."
  (let* ((provisional (sprite--provisional-p s))
         (name (sprite-name s))
         (uptime (when (sprite-start-time s)
                   (float-time (time-since (sprite-start-time s)))))
         (last-seen (when (sprite-last-contact s)
                      (float-time (time-since (sprite-last-contact s)))))
         (buffers (if provisional "—" (or (sprite--query-buffer-count name) "?")))
         (spawned-by (or (sprite-spawned-by s) "")))
    (list s
          (vector
           (if (sprite-idx s) (number-to-string (sprite-idx s)) "?")
           (cond ((eq (sprite-startup s) 'sync) "S")
                 ((eq (sprite-startup s) 'idle) "I")
                 (t " "))
           (if provisional (sprite-unique-name s) name)
           (if provisional
               (format "(pending %s)" (sprite-startup s))
             (sprite--format-uptime uptime))
           (if (stringp buffers) buffers
             (number-to-string buffers))
           (sprite--format-uptime last-seen)
           spawned-by))))

(defun sprite--build-parent-entry ()
  "Build a `tabulated-list' entry representing the current parent instance."
  (let* ((name (sprite-instance-name))
         (uptime (when (boundp 'before-init-time)
                   (float-time (time-since before-init-time))))
         (buffers (length (buffer-list))))
    (list (sprite--make :name name
                        :idx nil
                        :parent nil
                        :unique-name name)
          (vector
           "-"
           " "
           (propertize name 'face 'bold)
           (sprite--format-uptime uptime)
           (number-to-string buffers)
           "(self)"
           "system"))))

(defconst sprite--list-buffer-name "*sprite-list*"
  "Name of the sprite overview buffer.")

(defvar sprite-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "c") #'sprite-list-create)
    (define-key map (kbd "d") #'sprite-list-decommission)
    (define-key map (kbd "r") #'sprite-list-restart)
    (define-key map (kbd "s") #'sprite-list-stop)
    (define-key map (kbd "p") #'sprite-list-check)
    (define-key map (kbd "o") #'sprite-list-open-log)
    (define-key map (kbd "f") #'sprite-list-open-frame)
    (define-key map (kbd "g") #'sprite-list-refresh)
    (define-key map (kbd "i") #'sprite-list-info)
    (define-key map (kbd "?") #'sprite-list-menu)
    (define-key map (kbd "m") #'sprite-list-menu)
    map)
  "Keymap for `sprite-list-mode'.")

(define-derived-mode sprite-list-mode tabulated-list-mode "sprite"
  "Major mode for viewing and managing sprite daemons.

\\{sprite-list-mode-map}"
  (setq tabulated-list-format
        (vector
         '("#"          4  t)
         '("Def"        4  nil)
         '("Name"      30  t)
         '("Uptime"    10  nil)
         '("Buffers"    7  nil)
         '("Last Seen" 12  nil)
         '("Spawned By" 20 nil)))
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header))

(defun sprite-list-refresh ()
  "Refresh the sprite overview buffer."
  (interactive)
  (sprite--discover-and-sync-registry)
  (setq tabulated-list-entries
        (cons (sprite--build-parent-entry)
              (thread-last (sprite--registry-all)
                (seq-filter (lambda (s)
                              (and (sprite--for-current-parent-p s)
                                   (sprite--live-p s))))
                (seq-map #'sprite--build-list-entry))))
  (tabulated-list-print t))

(defvar sprite-info-map (make-sparse-keymap)
  "Keymap for sprite info help buffers.  Inherits `help-mode-map' once loaded.
Add context-specific bindings here.")

(with-eval-after-load 'help-mode
  (set-keymap-parent sprite-info-map help-mode-map))

(defun sprite-list-info ()
  "Show a help-window detail buffer for the sprite at point."
  (interactive)
  (when-let* ((s (tabulated-list-get-id))
              (buf-name (format "*Sprite Info: %s*" (sprite-name s))))
    (with-help-window buf-name
      (princ (format "Sprite: %s\n\n" (sprite-name s)))
      (princ (format "  Parent:       %s\n" (or (sprite-parent s) "(root)")))
      (princ (format "  Unique name:  %s\n" (or (sprite-unique-name s) "")))
      (princ (format "  Index:        %s\n"
                     (if (sprite-idx s) (number-to-string (sprite-idx s)) "(unspawned)")))
      (princ (format "  Startup:      %s\n" (or (sprite-startup s) "(none)")))
      (princ (format "  PID:          %s\n" (or (sprite-pid s) "—")))
      (princ (format "  State dir:    %s\n" (or (sprite-state-dir s) "—")))
      (princ (format "  Start time:   %s\n"
                     (if (sprite-start-time s)
                         (format-time-string "%F %T" (sprite-start-time s))
                       "—")))
      (princ (format "  Last contact: %s\n"
                     (if (sprite-last-contact s)
                         (format-time-string "%F %T" (sprite-last-contact s))
                       "—")))
      (princ (format "  Status:       %s\n" (or (sprite-running-status s) "unknown")))
      (princ (format "  Spawned by:   %s\n" (or (sprite-spawned-by s) "—")))
      (princ (format "  Backend:      %s\n" sprite-communication-backend)))
    (when-let* ((buf (get-buffer buf-name)))
      (with-current-buffer buf
        (use-local-map sprite-info-map)))))

;;;###autoload
(defun sprite-list ()
  "Open the sprite overview buffer."
  (interactive)
  (with-current-buffer (get-buffer-create sprite--list-buffer-name)
    (unless (derived-mode-p 'sprite-list-mode)
      (sprite-list-mode))
    (sprite-list-refresh)
    (pop-to-buffer (current-buffer))))

(defun sprite-list--sprite-at-point-p ()
  "Return non-nil if there is a managed sprite at point (not the parent row)."
  (when-let* ((s (tabulated-list-get-id)))
    (not (null (sprite-idx s)))))

(defun sprite-list--log-exists-at-point-p ()
  "Return non-nil if a log buffer exists for the sprite at point."
  (when-let* ((s (tabulated-list-get-id)))
    (get-buffer (sprite--log-buffer-name (sprite-name s)))))

(defun sprite-list--known-dead-p ()
  "Return non-nil if the sprite at point has been checked and is not running."
  (when-let* ((s (tabulated-list-get-id))
              ((sprite-idx s)))
    (eq (sprite-running-status s) 'dead)))

(defun sprite-list--can-open-frame-p ()
  "Return non-nil if a frame can be opened for the sprite at point.
True when point is on a sprite row that is not known to be dead."
  (and (sprite-list--sprite-at-point-p)
       (not (sprite-list--known-dead-p))))

(defun sprite-list--sprite-at-point ()
  "Return the managed sprite struct at point, or signal `user-error'.
Signals an error if point is on the parent-instance row."
  (if-let* ((s (tabulated-list-get-id))
            ((sprite-idx s)))
    s
    (user-error "No sprite at point")))

(defun sprite-list-create ()
  "Create a new sprite from the overview buffer."
  (interactive)
  (call-interactively #'sprite-create)
  (sprite-list-refresh))

(defun sprite-list-decommission ()
  "Decommission the sprite at point."
  (interactive)
  (when-let* ((name (sprite-name (sprite-list--sprite-at-point)))
              ((yes-or-no-p (format "Decommission sprite %s? " name))))
    (sprite-decommission name)
    (sprite-list-refresh)))

(defun sprite-list-restart ()
  "Restart the sprite at point."
  (interactive)
  (sprite-restart (sprite-name (sprite-list--sprite-at-point)))
  (sprite-list-refresh))

(defun sprite-list-stop ()
  "Stop the sprite at point."
  (interactive)
  (sprite-stop (sprite-name (sprite-list--sprite-at-point)))
  (sprite-list-refresh))

(defun sprite-list-open-log ()
  "Open the communication log for the sprite at point."
  (interactive)
  (sprite-open-log (sprite-name (sprite-list--sprite-at-point))))

(defun sprite-list-check ()
  "Verify whether the sprite at point is reachable via emacsclient.
Updates the sprite's running status and refreshes the list."
  (interactive)
  (let* ((s (sprite-list--sprite-at-point))
         (name (sprite-name s))
         (running (sprite--running-p name)))
    (setf (sprite-running-status s) (if running 'running 'dead))
    (sprite-list-refresh)
    (message "Sprite %s: %s" name (if running "running" "not running"))))

(defun sprite-list-open-frame ()
  "Open a new Emacs frame connected to the sprite at point."
  (interactive)
  (sprite-open-frame (sprite-list--sprite-at-point)))

;;;###autoload
(transient-define-prefix sprite-list-menu ()
  "Actions for the sprite overview buffer."
  [["Sprite"
    ("c" "Create"       sprite-list-create)
    ("d" "Decommission" sprite-list-decommission
     :inapt-if-not sprite-list--sprite-at-point-p)
    ("r" "Restart"      sprite-list-restart
     :inapt-if-not sprite-list--sprite-at-point-p)
    ("s" "Stop"         sprite-list-stop
     :inapt-if-not sprite-list--sprite-at-point-p)
    ("p" "Check/ping"   sprite-list-check
     :inapt-if-not sprite-list--sprite-at-point-p)]
   ["Navigate"
    ("o" "Open log"     sprite-list-open-log
     :inapt-if-not sprite-list--log-exists-at-point-p)
    ("f" "New frame"    sprite-list-open-frame
     :inapt-if-not sprite-list--can-open-frame-p)
    ("g" "Refresh"      sprite-list-refresh)
    ("q" "Quit"         quit-window)]])

(provide 'sprite-list)
;;; sprite-list.el ends here
