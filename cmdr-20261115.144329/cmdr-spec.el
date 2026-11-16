;;; cmdr-spec.el --- Declarative binding and command specs with discovery -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/cmdr.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;; A spec is data: prefixes, key bindings, deferred keymaps and registry
;; commands for one feature, plus an optional probe naming the command(s)
;; whose presence makes the spec applicable: a function symbol must be
;; `fboundp' (autoloads count) and a string must name a library that
;; `locate-library' finds.  `cmdr-spec-discover' installs every applicable
;; spec, so nothing is required just to be bound; a spec may name autoloads
;; libraries to `load' for packages that live outside `package.el'.  Specs that are not installed are recorded,
;; with the reason, in `cmdr-spec-skipped'; see `cmdr-spec-report'.
;;
;; Registry commands are handed to `cmdr-register-command' only once
;; `cmdr' itself is loaded, so installing specs never loads Transient.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'cmdr-keys)
(require 'cmdr-macros)

(declare-function cmdr-register-command "cmdr")
(declare-function cmdr-transient-insert-suffix-once "cmdr")

(defgroup cmdr-spec nil
  "Declarative cmdr specs."
  :group 'convenience)

(defcustom cmdr-disabled-specs nil
  "Names of specs that discovery must not install."
  :type '(repeat symbol))

(defcustom cmdr-opt-in-specs nil
  "Names of opt-in specs (those defined with :opt-in) discovery installs."
  :type '(repeat symbol))

(defcustom cmdr-spec-libraries '(cmdr-defaults cmdr-ecosystem cmdr-integrations)
  "Libraries of specs that `cmdr-spec-discover' loads.
These contain only `cmdr-register-spec' forms and require nothing else."
  :type '(repeat symbol))

(cl-defstruct (cmdr-spec (:copier nil))
  "One feature's prefixes, bindings and registry commands."
  name doc probe autoloads opt-in prefixes bindings deferred-keymaps commands keymap-bindings transient-suffixes)

(cl-defstruct (cmdr-spec-command (:copier nil))
  "A registry command carried by a spec until `cmdr' loads.
Mirrors the arguments of `cmdr-register-command'."
  category command description transient-key display-name row visible inapt
  exclude-from-transient)

(defvar cmdr-spec-table nil
  "List of `cmdr-spec' structs in definition order.")

(defvar cmdr-spec-skipped nil
  "Alist of (NAME . REASON) for specs the last discovery did not install.")

(defvar cmdr-spec--pending nil
  "Specs whose commands wait for `cmdr' to load.")

(defvar cmdr-spec--pending-transient-suffixes nil
  "List of (PREFIX LOC SUFFIX) entries waiting for `transient--layout'.")

(cl-defmacro cmdr-spec-command (&key category command description transient-key display-name row if ifapt exclude-from-transient)
  "Construct a `cmdr-spec-command'; symbol arguments are quoted data.
The keywords are those of `cmdr-register-command'.  IF and IFAPT are
function symbols or lambda forms."
  `(make-cmdr-spec-command :category ',category :command ',command
                           :description ,description
                           :transient-key ,transient-key
                           :display-name ,display-name :row ',row
                           :visible ,(when if `(function ,if))
                           :inapt ,(when ifapt `(function ,ifapt))
                           :exclude-from-transient ',exclude-from-transient))

(cl-defmacro cmdr-register-spec (&key name doc probe autoloads opt-in prefixes bindings deferred-keymaps commands keymap-bindings transient-suffixes)
  "Register or replace the spec NAME, a required symbol.
Every argument is quoted data.
PROBE is a function symbol (must be `fboundp') or library name string
(must be found by `locate-library'), or a list of them, all of which must hold.
AUTOLOADS is a library name or list of names that installing loads when
`locate-library' finds them.
OPT-IN non-nil installs only when NAME is in `cmdr-opt-in-specs'.
PREFIXES is a list of (KEY . LABEL), BINDINGS a list of (KEY . COMMAND),
DEFERRED-KEYMAPS a list of (KEY PACKAGE KEYMAP-SYMBOL LABEL).  COMMANDS is a
list of `cmdr-spec-command' forms and is the one argument that is evaluated.
KEYMAP-BINDINGS is a list of (PACKAGE KEYMAP-SYMBOL . SPEC) defining bindings
in external keymaps immediately if available, or via mode hooks.
SPEC may include :hook to specify custom activation hook(s).
TRANSIENT-SUFFIXES is a list of (PACKAGE PREFIX LOC SUFFIX) or
(PREFIX LOC SUFFIX) entries passed to `cmdr-transient-insert-suffix-once'."
  (declare (indent 0))
  (unless (and name (symbolp name))
    (user-error "cmdr-register-spec requires a symbol :name, got %S" name))
  `(cmdr-spec--put (make-cmdr-spec :name ',name :doc ,doc :probe ',probe
                                   :autoloads ',autoloads
                                   :opt-in ',opt-in :prefixes ',prefixes
                                   :bindings ',bindings
                                   :deferred-keymaps ',deferred-keymaps
                                   :commands (list ,@commands)
                                   :keymap-bindings ',keymap-bindings
                                   :transient-suffixes ',transient-suffixes)))

(defun cmdr-spec--put (spec)
  "Add SPEC to `cmdr-spec-table', replacing a same-named spec in place."
  (setq cmdr-spec-table
        (if (seq-find (lambda (s) (eq (cmdr-spec-name s) (cmdr-spec-name spec))) cmdr-spec-table)
            (seq-map (lambda (s)
                       (if (eq (cmdr-spec-name s) (cmdr-spec-name spec))
                           spec
                         s))
                     cmdr-spec-table)
          (append cmdr-spec-table (list spec))))
  spec)

(defun cmdr-spec--normalize-command (cmd)
  "Normalize CMD for `cmdr-keys-set'."
  (cond
   ((and (consp cmd) (eq (car cmd) 'quote))
    (cadr cmd))
   ((and (consp cmd) (eq (car cmd) 'cons))
    (eval cmd t))
   ((and (consp cmd) (consp (cdr cmd)) (null (cddr cmd)) (stringp (car cmd)))
    (cons (car cmd) (if (and (consp (cadr cmd)) (eq (car (cadr cmd)) 'quote))
                        (cadr (cadr cmd))
                      (cadr cmd))))
   (t cmd)))

(defun cmdr-spec-get (name)
  "Return the spec called NAME or signal `user-error'."
  (or (seq-find (lambda (s) (eq (cmdr-spec-name s) name)) cmdr-spec-table)
      (user-error "No cmdr spec named %S" name)))

(defun cmdr-spec--probe-missing-p (probe)
  "Return non-nil when PROBE, a function symbol or library string, is absent."
  (if (stringp probe)
      (not (locate-library probe))
    (not (fboundp probe))))

(defun cmdr-spec--missing-probe (spec)
  "Return the first probe of SPEC that is absent, or nil."
  (seq-find #'cmdr-spec--probe-missing-p (ensure-list (cmdr-spec-probe spec))))

(defun cmdr-spec--skip-reason (spec)
  "Return a string saying why discovery skips SPEC, or nil to install it."
  (cond
   ((memq (cmdr-spec-name spec) cmdr-disabled-specs)
    "disabled")
   ((and (cmdr-spec-opt-in spec) (not (memq (cmdr-spec-name spec) cmdr-opt-in-specs)))
    "opt-in")
   ((cmdr-spec--missing-probe spec)
    (if (stringp (cmdr-spec--missing-probe spec))
        (format "library %s not found" (cmdr-spec--missing-probe spec))
      (format "%s is not defined" (cmdr-spec--missing-probe spec))))))

(defun cmdr-spec--record-failure (spec what err)
  "Warn about and record the failure ERR of WHAT for SPEC."
  (let ((message (format "%s failed: %s" what (error-message-string err))))
    (display-warning 'cmdr (format "spec %s: %s" (cmdr-spec-name spec) message))
    (setq cmdr-spec-skipped
          (append cmdr-spec-skipped (list (cons (cmdr-spec-name spec) message))))))

(defun cmdr-spec-flush-commands ()
  "Register the commands of every pending spec with `cmdr'.
A spec whose commands fail to register is recorded in `cmdr-spec-skipped'
and does not stop the others."
  (let ((specs (reverse (prog1 cmdr-spec--pending
                          (setq cmdr-spec--pending nil)))))
    (seq-do (lambda (spec)
              (condition-case err
                  (seq-do (lambda (command)
                            (cmdr-register-command
                             :category (cmdr-spec-command-category command)
                             :command (cmdr-spec-command-command command)
                             :description (cmdr-spec-command-description command)
                             :transient-key (cmdr-spec-command-transient-key command)
                             :display-name (cmdr-spec-command-display-name command)
                             :row (or (cmdr-spec-command-row command) 1)
                             :if (cmdr-spec-command-visible command)
                             :ifapt (cmdr-spec-command-inapt command)
                             :exclude-from-transient (cmdr-spec-command-exclude-from-transient command)))
                          (cmdr-spec-commands spec))
                (error (cmdr-spec--record-failure spec "registering commands" err))))
            specs)))

(defun cmdr-spec--apply-keymap-bindings (keymap-symbol prefixes bindings)
  "Apply PREFIXES and BINDINGS to KEYMAP-SYMBOL if it is bound to a keymap.
Return non-nil if applied, nil otherwise."
  (let ((map (if (symbolp keymap-symbol)
                 (and (boundp keymap-symbol) (symbol-value keymap-symbol))
               keymap-symbol)))
    (when (keymapp map)
      (seq-do (pcase-lambda (`(,key . ,rest-prefix))
                (let ((label (if (consp rest-prefix) (car rest-prefix) rest-prefix))
                      (var (when (consp rest-prefix) (cadr rest-prefix))))
                  (let ((child (cmdr-keys-prefix map key label)))
                    (when var
                      (set var child)))))
              prefixes)
      (seq-do (pcase-lambda (`(,key . ,command))
                (cmdr-keys-set map key (cmdr-spec--normalize-command command)))
              bindings)
      t)))

(defun cmdr-spec--flush-transient-suffixes (&rest _)
  "Insert any pending transient suffixes whose prefix layout is now defined."
  (setq cmdr-spec--pending-transient-suffixes
        (seq-remove (pcase-lambda (`(,prefix ,loc ,suffix))
                      (when (get prefix 'transient--layout)
                        (unless (fboundp 'cmdr-transient-insert-suffix-once)
                          (require 'cmdr))
                        (cmdr-transient-insert-suffix-once prefix loc suffix)
                        t))
                    cmdr-spec--pending-transient-suffixes))
  (unless cmdr-spec--pending-transient-suffixes
    (remove-hook 'after-load-functions #'cmdr-spec--flush-transient-suffixes)))

(defun cmdr-spec--install-transient-suffix (entry)
  "Install or defer transient suffix ENTRY.
ENTRY is either (PREFIX LOC SUFFIX) or (PACKAGE PREFIX LOC SUFFIX)."
  (pcase-let ((`(,prefix ,loc ,suffix)
               (if (= (length entry) 4) (cdr entry) entry)))
    (if (get prefix 'transient--layout)
        (progn
          (unless (fboundp 'cmdr-transient-insert-suffix-once)
            (require 'cmdr))
          (cmdr-transient-insert-suffix-once prefix loc suffix))
      (let ((norm (list prefix loc suffix)))
        (unless (member norm cmdr-spec--pending-transient-suffixes)
          (push norm cmdr-spec--pending-transient-suffixes))
        (add-hook 'after-load-functions #'cmdr-spec--flush-transient-suffixes)))))

;;;###autoload
(defun cmdr-spec-install (name)
  "Install the spec called NAME into `cmdr-mode-map', ignoring its probe.
Installing again is idempotent."
  (interactive (list (intern (completing-read "Spec: " (seq-map #'cmdr-spec-name cmdr-spec-table) nil t))))
  (let ((spec (cmdr-spec-get name)))
    (seq-do (lambda (library)
              (when (locate-library library)
                (load library nil t)))
            (ensure-list (cmdr-spec-autoloads spec)))
    (seq-do (pcase-lambda (`(,key . ,label))
              (cmdr-keys-prefix cmdr-mode-map key label))
            (cmdr-spec-prefixes spec))
    (seq-do (pcase-lambda (`(,key . ,command))
              (cmdr-keys-set cmdr-mode-map key command))
            (cmdr-spec-bindings spec))
    (seq-do (pcase-lambda (`(,key ,package ,keymap-symbol ,label))
              (unless (keymap-lookup cmdr-mode-map key)
                (cmdr-bind-deferred-keymap cmdr-mode-map key package keymap-symbol label)))
            (cmdr-spec-deferred-keymaps spec))
    (seq-do (pcase-lambda (`(,_package ,keymap-symbol . ,rest))
              (let* ((hook-opt (when (keywordp (car-safe rest)) (plist-get rest :hook)))
                     (prefixes (when (keywordp (car-safe rest)) (plist-get rest :prefixes)))
                     (bindings (cond
                                ((and (keywordp (car-safe rest)) (plist-member rest :bindings))
                                 (plist-get rest :bindings))
                                ((keywordp (car-safe rest))
                                 (let ((rem rest)
                                       (shorthand nil))
                                   (while rem
                                     (if (keywordp (car rem))
                                         (setq rem (cddr rem))
                                       (push (car rem) shorthand)
                                       (setq rem (cdr rem))))
                                   (nreverse shorthand)))
                                (t rest))))
                (unless (cmdr-spec--apply-keymap-bindings keymap-symbol prefixes bindings)
                  (let* ((hooks (ensure-list
                                 (or hook-opt
                                     (let ((name (symbol-name keymap-symbol)))
                                       (cond
                                        ((string-suffix-p "-global-map" name)
                                         (list (intern (format "%s-global-mode-hook" (string-remove-suffix "-global-map" name)))
                                               (intern (format "%s-mode-hook" (string-remove-suffix "-global-map" name)))))
                                        ((string-suffix-p "-mode-map" name)
                                         (list (intern (format "%s-mode-hook" (string-remove-suffix "-mode-map" name)))))
                                        ((string-suffix-p "-map" name)
                                         (list (intern (format "%s-hook" (string-remove-suffix "-map" name)))))
                                        (t
                                         (list (intern (format "%s-hook" name)))))))))
                         (installer nil))
                    (setq installer
                          (lambda (&rest _)
                            (when (cmdr-spec--apply-keymap-bindings keymap-symbol prefixes bindings)
                              (dolist (h hooks)
                                (remove-hook h installer)))))
                    (dolist (h hooks)
                      (add-hook h installer))))))
            (ignore-errors (cmdr-spec-keymap-bindings spec)))
    (seq-do #'cmdr-spec--install-transient-suffix
            (cmdr-spec-transient-suffixes spec))
    (when (cmdr-spec-commands spec)
      (setq cmdr-spec--pending (cons spec (delq spec cmdr-spec--pending)))
      (when (featurep 'cmdr)
        (cmdr-spec-flush-commands)))
    spec))

;;;###autoload
(defun cmdr-spec-discover ()
  "Install every applicable spec and record the rest in `cmdr-spec-skipped'.
Safe to call again, for instance after installing a package."
  (interactive)
  (seq-do #'require cmdr-spec-libraries)
  (setq cmdr-spec-skipped nil)
  (seq-do (lambda (spec)
            (if-let* ((reason (cmdr-spec--skip-reason spec)))
                (setq cmdr-spec-skipped
                      (append cmdr-spec-skipped (list (cons (cmdr-spec-name spec) reason))))
              (condition-case err
                  (cmdr-spec-install (cmdr-spec-name spec))
                (error (cmdr-spec--record-failure spec "install" err)))))
          cmdr-spec-table)
  cmdr-spec-skipped)

(defun cmdr-spec--key-key (key)
  "Return the normalized description of key sequence string KEY."
  (key-description (key-parse key)))

(defun cmdr-spec-validate (&optional specs)
  "Return a list of conflict descriptions between SPECS, default `cmdr-spec-table'.
Detects a prefix given different labels, a key bound to different
commands, and a key that is a command in one place and a prefix in
another.  Specs are compared whether or not their probes pass.  Called
interactively, show the result."
  (interactive)
  (let ((labels nil)
        (bound nil)
        (conflicts nil))
    (seq-do
     (lambda (spec)
       (let ((name (cmdr-spec-name spec)))
         (seq-do (pcase-lambda (`(,key . ,label))
                   (let ((key (cmdr-spec--key-key key)))
                     (if-let* ((old (map-elt labels key))
                               ((not (equal (cdr old) label))))
                         (push (format "prefix %s labelled %S by %s but %S by %s"
                                       key (cdr old) (car old) label name)
                               conflicts)
                       (setf (map-elt labels key) (cons name label)))))
                 (append (cmdr-spec-prefixes spec)
                         (seq-map (pcase-lambda (`(,key ,_package ,_symbol ,label))
                                    (cons key label))
                                  (cmdr-spec-deferred-keymaps spec))))
         (seq-do (pcase-lambda (`(,key . ,command))
                   (let ((key (cmdr-spec--key-key key)))
                     (if-let* ((old (map-elt bound key))
                               ((not (eq (cdr old) command))))
                         (push (format "key %s bound to %s by %s but %s by %s"
                                       key (cdr old) (car old) command name)
                               conflicts)
                       (setf (map-elt bound key) (cons name command)))))
                 (cmdr-spec-bindings spec))))
     (or specs cmdr-spec-table))
    (map-do (lambda (key old)
              (when-let* ((other (map-elt labels key)))
                (push (format "key %s is a command (%s, by %s) and a prefix (by %s)"
                              key (cdr old) (car old) (car other))
                      conflicts))
              (seq-do (lambda (other-key)
                        (when (string-prefix-p (concat key " ") other-key)
                          (push (format "key %s is a command (by %s) but a prefix of %s"
                                        key (car old) other-key)
                                conflicts)))
                      (append (map-keys bound) (map-keys labels))))
            bound)
    (setq conflicts (nreverse conflicts))
    (when (called-interactively-p 'any)
      (message "%s" (if conflicts
                        (string-join conflicts "\n")
                      "No cmdr spec conflicts")))
    conflicts))

(defun cmdr-spec-report ()
  "Show which specs the last discovery skipped, and why."
  (interactive)
  (with-current-buffer (get-buffer-create "*cmdr-specs*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%d specs, %d skipped\n\n" (length cmdr-spec-table) (length cmdr-spec-skipped)))
      (seq-do (pcase-lambda (`(,name . ,reason))
                (insert (format "%-24s %s\n" name reason)))
              cmdr-spec-skipped))
    (special-mode)
    (pop-to-buffer (current-buffer))))

(provide 'cmdr-spec)
;;; cmdr-spec.el ends here
