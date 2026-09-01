;;; sprite-heartbeat.el --- Heartbeat macros for sprite IPC -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a two-sided macro pair for tagging sprite-to-parent calls with
;; the sender's identity so the parent can track last-contact times.
;;
;; Sprite side: `sprite-send' wraps an outgoing `emacsclient' call to the
;; parent, prepending a heartbeat registration so the parent knows which
;; sprite is calling.
;;
;; Parent side: `sprite-defhandler' defines a function whose first argument
;; is the calling sprite's full name; `last-contact' is updated before the
;; body runs.

;;; Code:

(require 'sprite)

;;;; Parent-side primitives

(defun sprite--record-heartbeat (sprite-name)
  "Update the last-contact timestamp for SPRITE-NAME in the registry."
  (when-let* ((s (sprite--registry-get sprite-name)))
    (setf (sprite-last-contact s) (current-time))))

(defmacro sprite-defhandler (name args &rest body)
  "Define a function NAME callable by sprites.
ARGS must begin with a symbol that receives the calling sprite's full name;
`last-contact' is updated for that sprite before BODY runs.

Example:
  (sprite-defhandler handle-status (caller status)
    (message \"%s reported: %s\" caller status))"
  (declare (indent defun))
  (unless (consp args)
    (error "sprite-defhandler: ARGS must be a list with at least one element"))
  `(defun ,name ,args
     (sprite--record-heartbeat ,(car args))
     ,@body))

;;;; Sprite-side primitives

(defun sprite-parent-socket-name ()
  "Return the socket name of this sprite's parent instance.
Derived from the leading component of `sprite-instance-name'.
Falls back to the instance name itself when called on the root instance."
  (if-let* ((parts (sprite--parse-full-name (sprite-instance-name))))
    (car parts)
    (sprite-instance-name)))

(defmacro sprite-send (form)
  "Evaluate FORM in the parent instance, tagging this sprite as the sender.
The parent records a heartbeat for this sprite before evaluating FORM.
Returns the result of FORM as read from the parent."
  (let ((gself (make-symbol "self"))
        (gparent (make-symbol "parent")))
    `(let* ((,gself (sprite-instance-name))
            (,gparent (sprite-parent-socket-name)))
       (sprite--call-and-read ,gparent
                              `(progn
                                 (sprite--record-heartbeat ,,gself)
                                 ,',form)))))

(provide 'sprite-heartbeat)
;;; sprite-heartbeat.el ends here
