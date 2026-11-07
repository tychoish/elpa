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
  name doc probe autoloads opt-in prefixes bindings deferred-keymaps commands)

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

(cl-defmacro cmdr-register-spec (&key name doc probe autoloads opt-in prefixes bindings deferred-keymaps commands)
  "Register or replace the spec NAME, a required symbol.
Every argument is quoted data.
PROBE is a function symbol (must be `fboundp') or library name string
(must be found by `locate-library'), or a list of them, all of which must hold.
AUTOLOADS is a library name or list of names that installing loads when
`locate-library' finds them.
OPT-IN non-nil installs only when NAME is in `cmdr-opt-in-specs'.
PREFIXES is a list of (KEY . LABEL), BINDINGS a list of (KEY . COMMAND),
DEFERRED-KEYMAPS a list of (KEY PACKAGE KEYMAP-SYMBOL LABEL).  COMMANDS is a
list of `cmdr-spec-command' forms and is the one argument that is evaluated."
  (declare (indent 0))
  (unless (and name (symbolp name))
    (user-error "cmdr-register-spec requires a symbol :name, got %S" name))
  `(cmdr-spec--put (make-cmdr-spec :name ',name :doc ,doc :probe ',probe
                                   :autoloads ',autoloads
                                   :opt-in ',opt-in :prefixes ',prefixes
                                   :bindings ',bindings
                                   :deferred-keymaps ',deferred-keymaps
                                   :commands (list ,@commands))))

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
