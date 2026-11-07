;;; cmdr-keys.el --- Annotated key-binding helpers -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/cmdr.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Root map, prefix annotation and full-sequence binding helpers.  Built-ins only.

;;; Code:

(require 'map)
(require 'subr-x)

;; Prefix labels are stored in the binding itself as the native
;; (LABEL . KEYMAP) menu-item form.  `lookup-key' and `keymap-lookup' strip
;; the label and return KEYMAP, while Help and which-key read the label.
;; Nothing here touches `global-map' or maps owned by other packages unless
;; the caller passes them in explicitly.

(defvar-keymap cmdr-mode-map
  :doc "Root key map attached by `cmdr-mode' (the cmdr prefix tree).")

(defun cmdr-keys--events (seq)
  "Return the vector of events for key sequence SEQ, or signal `user-error'."
  (unless (and (stringp seq) (not (string-empty-p seq)) (key-valid-p seq))
    (user-error "Malformed key sequence: %S" seq))
  (key-parse seq))

(defun cmdr-keys--descend (map events seq)
  "Return the prefix keymap reached from MAP by EVENTS, creating it as needed.
SEQ is the full sequence, used only for error messages.  Signal `user-error'
when an event is already bound to something that is not a prefix."
  (seq-reduce
   (lambda (current event)
     (let ((binding (lookup-key current (vector event))))
       (cond
        ((keymapp binding)
         binding)
        ((null binding)
         (let ((child (make-sparse-keymap)))
           (define-key current (vector event) child)
           child))
        (t
         (user-error "Key %s in %s is bound to a non-prefix command"
                     (key-description (vector event)) seq)))))
   events
   map))

(defun cmdr-keys-prefix (map seq label)
  "Create or label a sparse prefix keymap at key sequence SEQ in MAP.
Intermediate prefixes are created.  LABEL is stored in the binding as the
native (LABEL . KEYMAP) form.  An existing prefix keymap is reused and only
relabelled.  Signal `user-error' for a malformed SEQ, a non-string LABEL, or
when any key along SEQ is already bound to a non-prefix command.  Return the
prefix keymap."
  (unless (stringp label)
    (user-error "Prefix label must be a string: %S" label))
  (let* ((events (cmdr-keys--events seq))
         (last-event (vector (aref events (1- (length events)))))
         (parent (cmdr-keys--descend map (seq-take events (1- (length events))) seq))
         (child (cmdr-keys--descend parent (seq-drop events (1- (length events))) seq))
         (raw (assq (aref last-event 0) (cdr parent))))
    (unless (equal (car-safe (cdr-safe raw)) label)
      (define-key parent last-event (cons label child)))
    child))

(defun cmdr-keys-set (map seq command)
  "Bind full key sequence SEQ to COMMAND in MAP, creating intermediate prefixes.
Signal `user-error' for a malformed SEQ, when an intermediate key is bound to
a non-prefix command, or when SEQ itself is currently a prefix."
  (let* ((events (cmdr-keys--events seq))
         (parent (cmdr-keys--descend map (seq-take events (1- (length events))) seq)))
    (when (keymapp (lookup-key parent (seq-drop events (1- (length events)))))
      (user-error "Key sequence %s is a prefix in the map" seq))
    (keymap-set map seq command)))

(provide 'cmdr-keys)
;;; cmdr-keys.el ends here
