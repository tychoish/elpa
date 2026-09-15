;;; sprite-heartbeat.el --- Heartbeat macros for sprite IPC -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/tychoish/sprite
;; Keywords: tools, daemon, processes
;;; Commentary:
;; Provides a two-sided macro pair for tagging sprite-to-parent calls with
;; the sender's identity so the parent can track last-contact times.
;;
;; Sprite side: `sprite-heartbeat-send' wraps an outgoing `emacsclient' call to the
;; parent, prepending a heartbeat registration so the parent knows which
;; sprite is calling.
;;
;; Parent side: `sprite-heartbeat-defhandler' defines a function whose first argument
;; is the calling sprite's full name; `last-contact' is updated before the
;; body runs.

;;; Code:

(require 'sprite)

;;;; Parent-side primitives

(defun sprite-heartbeat--record (sprite-name)
  "Update the last-contact timestamp for SPRITE-NAME in the registry."
  (when-let* ((s (sprite--registry-get sprite-name)))
    (setf (sprite-last-contact s) (current-time))))

(defmacro sprite-heartbeat-defhandler (name args &rest body)
  "Define a function NAME callable by sprites.
ARGS must begin with a symbol that receives the calling sprite's full name;
`last-contact' is updated for that sprite before BODY runs.

Example:
  (sprite-heartbeat-defhandler handle-status (caller status)
    (message \"%s reported: %s\" caller status))"
  (declare (indent defun))
  (unless (consp args)
    (error "sprite-heartbeat-defhandler: ARGS must be a list with at least one element"))
  `(defun ,name ,args
     (sprite-heartbeat--record ,(car args))
     ,@body))

;;;; Sprite-side primitives

(defun sprite-heartbeat-parent-socket-name ()
  "Return the socket name of this sprite's parent instance.
Derived from the leading component of `sprite-instance-name'.
Falls back to the instance name itself when called on the root instance."
  (if-let* ((parts (sprite--parse-full-name (sprite-instance-name))))
    (car parts)
    (sprite-instance-name)))

(defmacro sprite-heartbeat-send (form)
  "Evaluate FORM in the parent instance, tagging this sprite as the sender.
The parent records a heartbeat for this sprite before evaluating FORM.
Returns the result of FORM as read from the parent."
  (let ((gself (make-symbol "self"))
        (gparent (make-symbol "parent")))
    `(let* ((,gself (sprite-instance-name))
            (,gparent (sprite-heartbeat-parent-socket-name)))
       (sprite--call-and-read ,gparent
                              `(progn
                                 (sprite-heartbeat--record ,,gself)
                                 ,',form)))))

(provide 'sprite-heartbeat)
;;; sprite-heartbeat.el ends here
