;;; cmdr-macros.el --- Self-contained toggle, command-generator and hook macros -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/cmdr.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Independent macros; no dependency on xtd-macro or any configuration.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defun cmdr--resolve-hooks (hook)
  "Resolve HOOK to a list of hook variable symbols.
HOOK may be a symbol, a list of symbols, or the sentinel
`after-first-frame-created', which resolves to `server-after-make-frame-hook'
in a daemon and `window-setup-hook' otherwise."
  (let ((hook (if (eq hook 'after-first-frame-created)
                  (if (daemonp)
                      'server-after-make-frame-hook
                    'window-setup-hook)
                hook)))
    (cond
     ((symbolp hook)
      (list hook))
     ((and (listp hook) (seq-every-p #'symbolp hook))
      hook)
     (t
      (user-error "Must have a symbol, list of symbols, or `after-first-frame-created' for hook: %S" hook)))))

(eval-and-compile
  (defun cmdr--build-symbol-name (&rest parts)
    "Join PARTS into a single hyphen-separated string suitable for a symbol name."
    (replace-regexp-in-string
     "-\\{3,\\}" "-"
     (mapconcat #'identity
                (thread-last
                  parts
                  (seq-filter #'stringp)
                  (seq-map (lambda (elem) (replace-regexp-in-string "[ \t\n\r]+" " " elem)))
                  (seq-map #'string-trim)
                  (seq-map (lambda (elem) (replace-regexp-in-string "[=+_'\"\\/ ]+" "-" elem)))
                  (seq-remove #'string-empty-p))
                "-"))))

(cl-defmacro add-lazy-init (&key name operation form (delay 1))
  "Run FORM or OPERATION on an idle timer after DELAY seconds.
NAME identifies the operation.  FORM takes precedence over OPERATION when
both are supplied.  Callers that want timing wrap FORM themselves."
  (unless (and name (or form operation))
    (user-error "The add-lazy-init macro requires :name and :form or :operation"))
  `(run-with-idle-timer
    ,delay nil
    (lambda ()
      ,(or form `(funcall ,operation)))))

(cl-defmacro add-one-shot-hook
    (&key name hook function result body form operation
          (args nil) (local nil) (persist nil) (count 1) (depth 0)
          (make-unique nil) (cleanup nil) (idle-timer nil))
  "Register a self-removing hook function named NAME on HOOK.
The function to run is taken from FORM, BODY, RESULT, OPERATION, or
FUNCTION, in that precedence order (see the source for details).  It
removes itself after COUNT runs unless PERSIST is non-nil.  LOCAL makes the
registration buffer-local, IDLE-TIMER defers the work to an idle timer, and
CLEANUP or MAKE-UNIQUE also unintern the generated function."
  (unless hook
    (user-error "The add-one-shot-hook macro requires :hook"))
  (let* ((unique-tag (or (when make-unique (gensym "hook-"))
                         (make-symbol "hook")))
         (count-tag (cond
                     (persist "perpetual")
                     ((not (numberp count))
                      (user-error "Must specify hook limited count as a number %S" count))
                     ((eq count 1) "one-shot")
                     (t (format "run-%d-times" count))))
         (cleanup-symbol (intern (cmdr--build-symbol-name "one-shot" count-tag name (symbol-name unique-tag))))
         (hook-form (if (symbolp hook) `',hook hook))
         (hook-display (cond
                        ((and (consp hook) (eq (car hook) 'quote))
                         (cadr hook))
                        ((or (symbolp hook) (seq-every-p #'symbolp hook))
                         hook)
                        (t "a computed hook expression")))
         (call-args (seq-remove (lambda (arg) (memq arg '(&optional &rest))) args))
         (resolved-form
          (or (cond
               (form
                form)
               (body
                (macroexp-progn body))
               (result
                (eval result))
               (operation
                (if args
                    `(funcall ,operation ,@call-args)
                  `(funcall ,operation)))
               ((and function (symbolp function))
                (if args
                    `(funcall ',function ,@call-args)
                  `(funcall ',function)))
               ((memq (car-safe function) '(function quote))
                `(funcall ,function ,@call-args))
               ((listp function)
                function))
              (user-error "Could not resolve the hook function from input for %s" name)))
         (cleanup-expr
          (if (or make-unique cleanup)
              `(unintern ',cleanup-symbol obarray)
            t))
         (run-count-var (intern (concat (symbol-name cleanup-symbol) "--run-count")))
         (bookkeeping
          `((cl-incf ,run-count-var)
            (when (and (not ,persist) (>= ,run-count-var ,count))
              (seq-do (lambda (h) (remove-hook h ',cleanup-symbol ,local))
                      (cmdr--resolve-hooks ,hook-form))
              ,cleanup-expr))))

    `(progn
       (defvar ,run-count-var 0
         ,(format "Times the one-shot hook function `%s' has run." cleanup-symbol))

       (defun ,cleanup-symbol ,args
         ,(format "Self-removing hook function for `%s'.
Registered on %S.
Generated by `add-one-shot-hook'; removes itself from the hook after
%s." name hook-display (if (eq count 1) "one run" (format "%d runs" count)))
         ,@(if idle-timer
               `((run-with-idle-timer ,idle-timer nil (lambda () ,resolved-form))
                 ,@bookkeeping)
             `(,resolved-form
               ,@bookkeeping)))

       (seq-do (lambda (h) (add-hook h ',cleanup-symbol ,depth ,local))
               (cmdr--resolve-hooks ,hook-form)))))

(cl-defmacro create-toggle-functions (value &key short-name local keymap key)
  "Define turn-on, turn-off, and toggle interactive commands for variable VALUE.
Use SHORT-NAME to override the generated name.  LOCAL makes the commands use
`setq-local'.  Optionally bind the toggle to KEY in KEYMAP."
  (let* ((name (or short-name (symbol-name value)))
         (suffix (when local "local"))
         (ops (list
               `(,(intern (cmdr--build-symbol-name "turn-on" name suffix)) t)
               `(,(intern (cmdr--build-symbol-name "turn-off" name suffix)) nil)
               `(,(intern (cmdr--build-symbol-name "toggle" name suffix)) (not ,value))))
         (setter (if local 'setq-local 'setq)))

    (when (and keymap (not key))
      (user-error "Must define both keymap and a key"))

    `(progn
       ,@(seq-map (lambda (op)
                    `(defun ,(car op) ()
                       (interactive)
                       (,setter ,value ,(cadr op))))
                  ops)
       ,(when keymap
          `(keymap-set ,keymap ,key #',(car (nth 2 ops)))))))

(cl-defmacro make-read-extended-command-for-prefix (prefix &key bind-map bind-key key-alias)
  "Define a command running `execute-extended-command' filtered to PREFIX.
Only commands whose names begin with PREFIX are offered for completion.
Optionally bind the command to BIND-KEY in BIND-MAP (default `global-map')
labeled KEY-ALIAS."
  (declare (indent defun))
  (unless (setq prefix (when-let* ((_ prefix)
                                   (trimmed (string-trim prefix))
                                   (_ (not (string-empty-p trimmed))))
                         trimmed))
    (user-error "Cannot build predicate function for an empty prefix"))

  (let* ((predicate-symbol (intern (format "read-extended-command-for-%s-prefix-p" prefix)))
         (user-command-symbol (intern (format "execute-extended-%s-command" prefix))))
    `(prog1
         (defun ,user-command-symbol ()
           ,(format "Read an extended command beginning with prefix `%s'." prefix)
           (interactive)
           (let ((read-extended-command-predicate #',predicate-symbol))
             (with-suppressed-warnings ((interactive-only execute-extended-command))
               (execute-extended-command nil))))

       (defun ,predicate-symbol (command _)
         ,(format "Match commands beginning with prefix `%s'." prefix)
         (string-prefix-p ,prefix (symbol-name command)))
       ,(when bind-key
          `(keymap-set ,(or bind-map 'global-map) ,bind-key
                       (cons ,(or key-alias (format "%s-commands" prefix))
                             #',user-command-symbol))))))

(defun cmdr--deferred-keymap-activate (map key package keymap-symbol name stand-in)
  "Load PACKAGE for the deferred binding of KEY in MAP, then replay the keys.
KEYMAP-SYMBOL must hold a keymap once PACKAGE loads; otherwise signal a
`user-error' and leave STAND-IN installed so a later use can retry.  KEY
is only rebound, labeled NAME, if it is still bound to STAND-IN."
  (require package)
  (let ((real-map (and (boundp keymap-symbol) (symbol-value keymap-symbol))))
    (unless (keymapp real-map)
      (user-error "Package `%s' did not define `%s' as a keymap" package keymap-symbol))
    (when (eq (lookup-key map (key-parse key)) stand-in)
      (keymap-set map key (cons name real-map))))
  (setq unread-command-events
        (append (listify-key-sequence (this-command-keys-vector))
                unread-command-events)))

(defmacro cmdr-bind-deferred-keymap (map key package keymap-symbol name)
  "Bind KEY in MAP to a labeled stand-in that loads PACKAGE on first use.
All arguments are evaluated exactly once, when the binding is installed;
PACKAGE and KEYMAP-SYMBOL evaluate to symbols and PACKAGE is not loaded
at expansion or install time.  KEY is the full key sequence relative to
MAP.  NAME is the label stored in the binding.

On first use the stand-in requires PACKAGE, checks that KEYMAP-SYMBOL's
value is a keymap, rebinds KEY to that keymap (keeping NAME) only if KEY
is still bound to the stand-in, and replays the invoking key sequence so
it falls through to the real map.  If loading or validation fails, the
stand-in stays installed, nothing is replayed, and a later use retries.
Returns the stand-in command."
  (let ((map-var (gensym "map"))
        (key-var (gensym "key"))
        (package-var (gensym "package"))
        (symbol-var (gensym "keymap-symbol"))
        (name-var (gensym "name"))
        (stand-in (gensym "stand-in")))
    `(let* ((,map-var ,map)
            (,key-var ,key)
            (,package-var ,package)
            (,symbol-var ,keymap-symbol)
            (,name-var ,name)
            (,stand-in nil))
       (setq ,stand-in
             (lambda ()
               (interactive)
               (cmdr--deferred-keymap-activate
                ,map-var ,key-var ,package-var ,symbol-var ,name-var ,stand-in)))
       (keymap-set ,map-var ,key-var (cons ,name-var ,stand-in))
       ,stand-in)))

(provide 'cmdr-macros)
;;; cmdr-macros.el ends here
