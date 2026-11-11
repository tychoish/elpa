;;; sprite.el --- Core instance identity and path resolution for sprite -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (seq "2.24"))
;; URL: https://github.com/tychoish/sprite
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
;; sprite.el provides core instance identity and state path resolution for
;; Emacs instances and subordinate "sprite" daemons.  It is designed to be
;; loaded early during Emacs startup (e.g. in early-init.el).
;;
;; For daemon management, evaluation, and lifecycle commands, see
;; `sprite-daemon' (autoloaded as needed).
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'generator)

(declare-function gen-p "gen" (sequence))
(declare-function gen--next "gen" (sequence))

;; server.el variables; server.el is loaded by any running daemon but may be
;; absent in batch/test contexts.
(defvar server-use-tcp)
(defvar server-socket-dir)

;;;; Instance identity and state paths

(defconst sprite--conf-state-directory "state"
  "Name of the state subdirectory under `user-emacs-directory'.")

(defconst sprite--state-subdir "sprite"
  "Subdirectory name under the state path where sprite state lives.")

(defvar sprite-instance-id nil
  "Name of the running Emacs instance.
Set by `sprite-set-up-instance-name' at startup.")

(defvar sprite--system-name-cached nil
  "Cached value of `system-name' for use in system contexts.")

(defvar sprite-cli-instance-id nil
  "CLI-specified instance name; set from --id command-line arguments.")

(defun sprite-resolve-instance-id ()
  "Return the current Emacs instance ID.
Resolution order: daemon name, `sprite-cli-instance-id',
`sprite-instance-id', then \"solo\"."
  (let ((daemon (daemonp)))
    (or (when (eq daemon t) "primary")
        (when (stringp daemon)
          (if (bound-and-true-p server-use-tcp)
              (setenv "EMACS_SERVER_FILE" daemon)
            (when (bound-and-true-p server-socket-dir)
              (setenv "EMACS_SOCKET_FILE" (expand-file-name daemon server-socket-dir))))
          daemon)
        sprite-cli-instance-id
        sprite-instance-id
        "solo")))

(defun sprite-instance-name ()
  "Return the current Emacs instance name, initialising it if needed.
Caches the result in `sprite-instance-id'.  This is the preferred public
accessor; call `sprite-resolve-instance-id' only when the raw resolution
chain must be re-evaluated."
  (unless sprite-instance-id
    (setq sprite-instance-id (sprite-resolve-instance-id)))
  sprite-instance-id)

(defun sprite-system-name ()
  (or sprite--system-name-cached (setq sprite--system-name-cached (system-name))))

(defun sprite-conf-host-and-instance ()
  "Return (HOSTNAME INSTANCE-ID) for state-path construction."
  (list (if (eq system-type 'darwin)
            (car (string-split (sprite-system-name) "\\."))
          (sprite-system-name))
        (sprite-instance-name)))

(defun sprite-state-file-prefix (name)
  "Return the instance-scoped filename prefix for NAME.
Produces HOSTNAME-INSTANCE-NAME[-USERNAME] where USERNAME is included
only when running as root or under a symlinked `user-emacs-directory'."
  (let* ((host-and-instance (sprite-conf-host-and-instance))
         (host (nth 0 host-and-instance))
         (instance (nth 1 host-and-instance)))
    (string-join
     (seq-filter #'identity
                 (list host instance name
                       (when (or (equal "root" user-login-name)
                                 (file-symlink-p user-emacs-directory))
                         user-login-name)))
     "-")))

;;;###autoload
(defun sprite-state-path (name)
  "Return full state-directory path for NAME, scoped to host and instance."
  (file-name-concat
   user-emacs-directory
   sprite--conf-state-directory
   (sprite-state-file-prefix name)))

;;;; Identity

(defun sprite--format-full-name (parent idx unique-name)
  "Format PARENT, IDX, and UNIQUE-NAME into a sprite full name."
  (format "%s.%d.%s" parent idx unique-name))

(defun sprite--parse-full-name (full-name)
  "Parse FULL-NAME into (PARENT IDX UNIQUE-NAME) or nil if malformed.
All three segments must be non-empty and contain no dots."
  (when-let* ((parts (string-split full-name "\\."))
              ((= (length parts) 3))
              ((seq-every-p (lambda (p) (not (string-empty-p p))) parts))
              ((string-match-p "^[0-9]+$" (nth 1 parts))))
    (list (nth 0 parts)
          (string-to-number (nth 1 parts))
          (nth 2 parts))))

(defun sprite--full-name-p (name)
  "Return t if NAME matches sprite full pattern <parent>.<idx>.<unique>."
  (and (stringp name)
       (string-match-p "^[^.]+\\.[0-9]+\\.[^.]+$" name)
       t))

(defun sprite--parent-letter (parent-id)
  "Return the first character of PARENT-ID as a single-char string."
  (substring parent-id 0 1))

(defun sprite--mode-line-id (full-name)
  "Return a mode-line display string for FULL-NAME.
For sprite names, abbreviates the parent to its first letter.
For top-level names, returns FULL-NAME unchanged."
  (if-let* ((parts (sprite--parse-full-name full-name)))
    (format "%s.%d.%s"
            (sprite--parent-letter (car parts))
            (cadr parts)
            (caddr parts))
    full-name))

(defun sprite--mode-line-string ()
  "Return mode-line display string for the current Emacs instance.
Always returns a non-nil string: abbreviated for sprite instances,
the raw instance id for top-level instances."
  (sprite--mode-line-id (sprite-instance-name)))

(cl-defun sprite-state-directory (&key full-name)
  "Return state directory for sprite FULL-NAME, or sprite root if nil."
  (let ((base (file-name-as-directory (sprite-state-path sprite--state-subdir))))
    (if full-name
        (file-name-concat base full-name)
      base)))

(cl-defun sprite-conf-state-path (name &key full-name)
  "Return path for NAME within FULL-NAME's state directory.
If FULL-NAME is nil, returns path under the sprite root."
  (file-name-concat (sprite-state-directory :full-name full-name) name))


;;;###autoload
(defcustom sprite-startup-operations nil
  "List of operations run in sequence during startup."
  :type '(repeat function)
  :group 'sprite)

;;;###autoload
(defcustom sprite-first-frame-operations nil
  "List of operations run after the first frame is created."
  :type '(repeat function)
  :group 'sprite)

;;;###autoload
(defcustom sprite-slow-op-reporting (and (bound-and-true-p init-file-debug) t)
  "When non-nil, log timing statistics for startup operations."
  :type 'boolean
  :group 'sprite)

;;;###autoload
(defcustom sprite-slow-op-threshold 0.00001
  "Threshold in seconds for logging startup operation durations."
  :type 'number
  :group 'sprite)

;;;###autoload
(defmacro with-sprite-slow-op-timer (name &rest body)
  "Execute BODY, logging duration for NAME when reporting is enabled.
Reporting is enabled when `sprite-slow-op-reporting' or `init-file-debug'
is non-nil, and logged if duration exceeds `sprite-slow-op-threshold'."
  (declare (indent 1) (debug (form body)))
  `(if (not (or (bound-and-true-p sprite-slow-op-reporting)
                (bound-and-true-p init-file-debug)))
       (progn ,@body)
     (let* ((start (current-time))
            (res (let ((inhibit-message t)) ,@body))
            (duration (float-time (time-since start)))
            (threshold (if (boundp 'sprite-slow-op-threshold)
                           sprite-slow-op-threshold
                         0.00001)))
       (when (> duration threshold)
         (let ((inhibit-message t))
           (message "[op]: <sprite> %s: %.06fs" ,name duration)))
       res)))

(defun sprite--execute-operation (fn &optional args frame log-prefix buffer)
  "Run operation FN with ARGS in FRAME and BUFFER context, reporting errors."
  (sprite-batch--execute-operation fn args frame buffer log-prefix))

(defun sprite--execute-startup-operation (fn &optional frame)
  "Run startup operation FN in FRAME, reporting errors."
  (sprite-batch--execute-operation fn '(1) frame nil "[startup]"))

(declare-function annotated-completing-read "annotated-completing-read")

(defun sprite--annotated-or-plain-read (prompt candidates &optional default)
  "Read a candidate string from CANDIDATES, an alist of (NAME . ANNOTATION).
Uses `annotated-completing-read', showing each ANNOTATION, when that
package is loaded; otherwise falls back to plain `completing-read' over
the candidate names, since `annotated-completing-read' is an optional
dependency.  Specify the text of the PROMPT; DEFAULT is the candidate
chosen on an empty answer."
  (if (fboundp 'annotated-completing-read)
      (apply #'annotated-completing-read candidates :prompt prompt :require-match t
             (and default (list :default default)))
    (completing-read prompt (seq-map #'car candidates) nil t nil nil default)))

(defun sprite--make-iterator (sequence)
  "Normalize SEQUENCE into (TOTAL-COUNT . NEXT-FN).
NEXT-FN is a function of zero arguments returning (IDX . ITEM) or nil.
SEQUENCE may be nil, a vector, a gen.el stream, a generator iterator,
or a list."
  (let ((idx 0))
    (cond
     ((null sequence)
      (cons 0 (lambda () nil)))
     ((vectorp sequence)
      (let ((total (length sequence)))
        (cons total
              (lambda ()
                (when (< idx total)
                  (let ((curr idx))
                    (setq idx (1+ idx))
                    (cons curr (aref sequence curr))))))))
     ;; gen struct duck-typing (gen-p or has gen--next)
     ((and (featurep 'gen) (fboundp 'gen-p) (funcall 'gen-p sequence))
      (cons nil
            (lambda ()
              (let ((v (funcall 'gen--next sequence)))
                (when v
                  (let ((curr idx))
                    (setq idx (1+ idx))
                    (cons curr (car v))))))))
     ;; Check `functionp' before `listp': in Emacs 29 closures are lists, so
     ;; the list branch must exclude functions to avoid treating generator
     ;; iterators as lists.
     ((and (listp sequence) (not (functionp sequence)))
      (let ((total (length sequence))
            (remaining sequence))
        (cons total
              (lambda ()
                (when remaining
                  (let ((curr idx)
                        (item (pop remaining)))
                    (setq idx (1+ idx))
                    (cons curr item)))))))
     ;; generator.el iterators (functions/closures) and anything else
     ;; that `iter-next' accepts.
     (t
      (cons nil
            (lambda ()
              (condition-case nil
                  (let ((item (iter-next sequence))
                        (curr idx))
                    (setq idx (1+ idx))
                    (cons curr item))
                (iter-end-of-sequence nil))))))))

(provide 'sprite)
(require 'sprite-batch)
;;; sprite.el ends here
