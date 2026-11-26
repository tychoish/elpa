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

(declare-function annotated-completing-read "annotated-completing-read")
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
  name doc probe autoloads opt-in prefixes bindings deferred-keymaps commands keymap-bindings transient-suffixes settings feature install-hook activate-hook)

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

(defvar cmdr-spec--pending-by-feature (make-hash-table :test #'eq)
  "Map features to names of specs waiting for them.")

(defvar cmdr-spec--dispatching nil
  "Non-nil while the after-load dispatcher is running.")

(defvar cmdr-spec--dispatch-again nil
  "Non-nil when a nested load requires another dispatch pass.")

(defvar cmdr-spec--activating (make-hash-table :test #'eq)
  "Names of specs currently running activation hooks.")

(defvar cmdr-spec--installed (make-hash-table :test #'eq)
  "Names of specs installed in this session.")

(defvar cmdr-spec--loaded-autoloads (make-hash-table :test #'eq)
  "Map spec names to autoload libraries successfully loaded for them.")

(defvar cmdr-spec--activated (make-hash-table :test #'eq)
  "Names of specs whose activation hooks have run.")

(defvar cmdr-spec--pending-resources (make-hash-table :test #'eq)
  "Map spec names to resources or activation retries awaiting a later load.")

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

(defun cmdr-spec--install-hook-name (name)
  "Return the install hook variable symbol for spec NAME."
  (intern (format "cmdr-spec-%s-install-hook" name)))

(defun cmdr-spec--activate-hook-name (name)
  "Return the activate hook variable symbol for spec NAME."
  (intern (format "cmdr-spec-%s-activate-hook" name)))

(defun cmdr-spec--define-hooks (name)
  "Ensure install and activate hook variables are defined for spec NAME."
  (let ((inst-hook (cmdr-spec--install-hook-name name))
        (act-hook (cmdr-spec--activate-hook-name name)))
    (unless (boundp inst-hook)
      (set inst-hook nil)
      (put inst-hook 'custom-type 'hook)
      (put inst-hook 'variable-documentation
           (format "Hook run immediately when cmdr spec `%s' is installed." name)))
    (unless (boundp act-hook)
      (set act-hook nil)
      (put act-hook 'custom-type 'hook)
      (put act-hook 'variable-documentation
           (format "Hook run when the feature for cmdr spec `%s' is loaded/activated." name)))))

(defun cmdr-spec--hook-function-symbol-p (x)
  "Return non-nil when X is a named function reference."
  (or (and (symbolp x) x)
      (and (consp x)
           (memq (car x) '(function quote))
           (symbolp (cadr x))
           (cadr x))))

(defun cmdr-spec--normalize-hook-function (form)
  "Normalize FORM to a named function symbol or form."
  (cond
   ((and (consp form) (eq (car form) 'function) (symbolp (cadr form)))
    form)
   ((and (consp form) (eq (car form) 'quote) (symbolp (cadr form)))
    `(function ,(cadr form)))
   ((and (symbolp form) form)
    `(function ,form))
   (t
    (user-error "Hook function must be a named function symbol, got %S" form))))

(defun cmdr-spec--normalize-hook-arg (arg)
  "Normalize ARG into an expression returning a list of functions."
  (cond
   ((null arg) nil)
   ((and (consp arg)
         (not (memq (car arg) '(function quote lambda)))
         (seq-every-p #'cmdr-spec--hook-function-symbol-p arg))
    `(list ,@(mapcar #'cmdr-spec--normalize-hook-function arg)))
   (t
    `(list ,(cmdr-spec--normalize-hook-function arg)))))

(defun cmdr-spec--register-hooks (name install-functions activate-functions)
  "Define and attach INSTALL-FUNCTIONS and ACTIVATE-FUNCTIONS for spec NAME."
  (cmdr-spec--define-hooks name)
  (dolist (fn install-functions)
    (add-hook (cmdr-spec--install-hook-name name) fn))
  (dolist (fn activate-functions)
    (add-hook (cmdr-spec--activate-hook-name name) fn)))

(cl-defmacro cmdr-register-spec (&key name doc probe feature autoloads opt-in prefixes bindings deferred-keymaps commands keymap-bindings transient-suffixes settings install-hook activate-hook)
  "Register or replace the spec NAME, a required symbol.
Every argument is quoted data, except COMMANDS, INSTALL-HOOK, and ACTIVATE-HOOK.
PROBE is a function symbol (must be `fboundp') or library name string
(must be found by `locate-library'), or a list of them, all of which must hold.
FEATURE is the feature symbol whose loading triggers ACTIVATE-HOOK; defaults
to PROBE if a library, or NAME.
AUTOLOADS is a library name or list of names that installing loads when
`locate-library' finds them.
OPT-IN non-nil installs only when NAME is in `cmdr-opt-in-specs'.
PREFIXES is a list of (KEY . LABEL), BINDINGS a list of (KEY . COMMAND),
DEFERRED-KEYMAPS a list of (KEY PACKAGE KEYMAP-SYMBOL LABEL).  COMMANDS is a
list of `cmdr-spec-command' forms.
KEYMAP-BINDINGS is a list of (PACKAGE KEYMAP-SYMBOL . SPEC) defining bindings
in external keymaps when PACKAGE is loaded.  SPEC may contain :prefixes
and :bindings options, or direct (KEY . COMMAND) bindings.
TRANSIENT-SUFFIXES is a list of (PACKAGE PREFIX LOC SUFFIX) or
(PREFIX LOC SUFFIX) entries passed to `cmdr-transient-insert-suffix-once'.
SETTINGS is an alist of (SYMBOL . VALUE) settings applied upon installation.
INSTALL-HOOK is a named function or list of named functions run when the
spec is installed.  ACTIVATE-HOOK has the same form and runs when
FEATURE is loaded."
  (declare (indent 0))
  (unless (and name (symbolp name))
    (user-error "cmdr-register-spec requires a symbol :name, got %S" name))
  (dolist (entry keymap-bindings)
    (let ((options (cddr entry)))
      (while options
        (if (keywordp (car options))
            (progn
              (unless (memq (car options) '(:prefixes :bindings))
                (user-error "Unknown keymap option %S" (car options)))
              (unless (cdr options)
                (user-error "Missing value for keymap option %S" (car options)))
              (setq options (cddr options)))
          (pop options)))))
  `(let* ((inst-funcs ,(cmdr-spec--normalize-hook-arg install-hook))
          (act-funcs ,(cmdr-spec--normalize-hook-arg activate-hook))
          (spec (make-cmdr-spec :name ',name :doc ,doc :probe ',probe
                                :feature ',(if (and (consp feature) (eq (car feature) 'quote))
                                               (cadr feature)
                                             feature)
                                :autoloads ',autoloads
                                :opt-in ',opt-in :prefixes ',prefixes
                                :bindings ',bindings
                                :deferred-keymaps ',deferred-keymaps
                                :commands (list ,@commands)
                                :keymap-bindings ',keymap-bindings
                                :transient-suffixes ',transient-suffixes
                                :settings ',settings
                                :install-hook inst-funcs
                                :activate-hook act-funcs)))
     (cmdr-spec--put spec)
     (cmdr-spec--define-hooks ',name)
     (when inst-funcs
       (cmdr-spec--register-hooks ',name inst-funcs nil))
     (when act-funcs
       (cmdr-spec--register-hooks ',name nil act-funcs))
     spec))

(defun cmdr-spec--put (spec)
  "Add SPEC to `cmdr-spec-table', replacing a same-named spec in place."
  (let ((old (seq-find (lambda (s) (eq (cmdr-spec-name s) (cmdr-spec-name spec))) cmdr-spec-table)))
    (when old
      (let ((inst-hook (cmdr-spec--install-hook-name (cmdr-spec-name old)))
            (act-hook (cmdr-spec--activate-hook-name (cmdr-spec-name old))))
        (dolist (fn (ignore-errors (cmdr-spec-install-hook old)))
          (remove-hook inst-hook fn))
        (dolist (fn (ignore-errors (cmdr-spec-activate-hook old)))
          (remove-hook act-hook fn)))))
  (map-delete cmdr-spec--activated (cmdr-spec-name spec))
  (map-delete cmdr-spec--pending-resources (cmdr-spec-name spec))
  (dolist (feature (map-keys cmdr-spec--pending-by-feature))
    (let ((remaining (delq (cmdr-spec-name spec)
                           (map-elt cmdr-spec--pending-by-feature feature))))
      (if remaining
          (map-put! cmdr-spec--pending-by-feature feature remaining)
        (map-delete cmdr-spec--pending-by-feature feature))))
  (setq cmdr-spec--pending
        (seq-remove (lambda (old) (eq (cmdr-spec-name old) (cmdr-spec-name spec)))
                    cmdr-spec--pending))
  (when (and (map-empty-p cmdr-spec--pending-by-feature)
             (map-empty-p cmdr-spec--pending-resources))
    (remove-hook 'after-load-functions #'cmdr-spec--after-load))
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

(defun cmdr-spec--activate-keymap-bindings (spec)
  "Apply available external keymaps in SPEC; defer the rest by feature."
  (let (missing)
    (dolist (entry (cmdr-spec-keymap-bindings spec))
      (pcase-let* ((`(,feature ,keymap . ,rest) entry)
                   (prefixes (plist-get rest :prefixes))
                   (bindings (if (keywordp (car-safe rest))
				 (or (plist-get rest :bindings)
                                     (let ((items rest) result)
                                       (while items
					 (if (keywordp (car items))
                                             (setq items (cddr items))
                                           (push (pop items) result)))
                                       (nreverse result)))
                               rest)))
	(unless (cmdr-spec--apply-keymap-bindings keymap prefixes bindings)
          (if (featurep feature)
              (push entry missing)
            (cmdr-spec--queue-feature feature (cmdr-spec-name spec))))))
    missing))

(defun cmdr-spec--activate-transient-suffixes (spec)
  "Apply transient suffixes in SPEC or retain them until their layout exists."
  (let ((name (cmdr-spec-name spec)) pending)
    (dolist (entry (cmdr-spec-transient-suffixes spec))
      (pcase-let* ((feature (and (= (length entry) 4) (car entry)))
                   (`(,prefix ,loc ,suffix) (if feature (cdr entry) entry)))
        (cond
         ((and feature (not (featurep feature)))
          (cmdr-spec--queue-feature feature name))
         ((and (get prefix 'transient--layout) (featurep 'cmdr))
          (cmdr-transient-insert-suffix-once prefix loc suffix))
         (t (push entry pending)))))
    (when (and pending (not (featurep 'cmdr)))
      (cmdr-spec--queue-feature 'cmdr name))
    (if pending
        (progn
          (map-put! cmdr-spec--pending-resources name (nreverse pending))
          (add-hook 'after-load-functions #'cmdr-spec--after-load))
      (map-delete cmdr-spec--pending-resources name))))

(defcustom cmdr-spec-merge-variables '(popper-reference-buffers)
  "Variables whose setting values are merged as collections via `add-to-list'.
Other variables are set via `set-default'."
  :type '(repeat symbol)
  :group 'cmdr-spec)

(defvar cmdr-spec--saved-settings nil
  "Alist of (SYMBOL . (BOUNDP . VALUE)) saved by `cmdr-spec--apply-settings'.")

(defun cmdr-spec--apply-settings (settings)
  "Apply SETTINGS alist, recording prior values for reversal."
  (seq-do (pcase-lambda (`(,sym . ,val))
            (unless (assq sym cmdr-spec--saved-settings)
              (let ((prior (and (boundp sym) (symbol-value sym))))
                (push (cons sym (if (boundp sym)
                                    (cons t (if (consp prior) (copy-sequence prior) prior))
                                  (cons nil nil)))
                      cmdr-spec--saved-settings)))
            (if (memq sym cmdr-spec-merge-variables)
                (if (and (boundp sym) (consp (symbol-value sym)))
                    (dolist (item val)
                      (add-to-list sym item))
                  (set-default sym (copy-sequence val)))
              (set-default sym val)))
          settings))

(defun cmdr-spec--revert-settings ()
  "Restore variables saved by `cmdr-spec--apply-settings'."
  (seq-do (lambda (entry)
            (let ((sym (car entry))
                  (status (cdr entry)))
              (if (car status)
                  (set-default sym (cdr status))
                (makunbound sym))))
          cmdr-spec--saved-settings)
  (setq cmdr-spec--saved-settings nil))

(defun cmdr-spec--resolve-feature (spec)
  "Resolve the feature symbol for SPEC."
  (or (cmdr-spec-feature spec)
      (let ((probe (cmdr-spec-probe spec))
            (name (cmdr-spec-name spec)))
        (cond
         ((stringp probe) (intern probe))
         ((and (consp probe) (stringp (car probe))) (intern (car probe)))
         ((or (null probe) (not (symbolp probe)) (eq probe name)) name)
         ((and (fboundp probe) (autoloadp (symbol-function probe)))
          (if (equal (file-name-base (cadr (symbol-function probe)))
                     (symbol-name probe))
              probe
            name))
         ((locate-library (symbol-name probe)) probe)
         (t name)))))

(defun cmdr-spec--queue-feature (feature name)
  "Wait for FEATURE on behalf of spec NAME."
  (unless (or (null feature) (eq feature 'none))
    (map-put! cmdr-spec--pending-by-feature feature
              (cons name (delq name (map-elt cmdr-spec--pending-by-feature feature))))
    (add-hook 'after-load-functions #'cmdr-spec--after-load)))

(defun cmdr-spec--activate-spec (name &optional force resolved-feature)
  "Apply resources and activate NAME once, or again when FORCE is non-nil.
Use RESOLVED-FEATURE when the caller has already resolved it."
  (unless (map-elt cmdr-spec--activating name)
    (map-put! cmdr-spec--activating name t)
    (unwind-protect
        (let* ((spec (cmdr-spec-get name))
               (feature (or resolved-feature (cmdr-spec--resolve-feature spec))))
          (let ((missing (cmdr-spec--activate-keymap-bindings spec)))
            (cmdr-spec--activate-transient-suffixes spec)
            (when missing
              (map-put! cmdr-spec--pending-resources name t)
              (add-hook 'after-load-functions #'cmdr-spec--after-load)))
          (when (and (not (eq feature 'none)) (featurep feature)
                     (or force (not (map-elt cmdr-spec--activated name))))
            (run-hooks (cmdr-spec--activate-hook-name name))
            (map-put! cmdr-spec--activated name t)))
      (map-delete cmdr-spec--activating name)
      (when (and cmdr-spec--dispatch-again
                 (not cmdr-spec--dispatching)
                 (map-empty-p cmdr-spec--activating))
        (cmdr-spec--after-load nil)))))

(defun cmdr-spec--after-load (_file)
  "Dispatch pending spec work after a library loads."
  (if (or cmdr-spec--dispatching (not (map-empty-p cmdr-spec--activating)))
      (setq cmdr-spec--dispatch-again t)
    (let ((cmdr-spec--dispatching t)
          (cmdr-spec--dispatch-again t)
          failed)
      (while cmdr-spec--dispatch-again
        (setq cmdr-spec--dispatch-again nil)
        (let (names)
          (dolist (feature (map-keys cmdr-spec--pending-by-feature))
            (when (featurep feature)
              (setq names (append (map-elt cmdr-spec--pending-by-feature feature) names))
              (map-delete cmdr-spec--pending-by-feature feature)
              (when (and (eq feature 'cmdr) cmdr-spec--pending)
                (cmdr-spec-flush-commands))))
          (dolist (name (delete-dups (append names (map-keys cmdr-spec--pending-resources))))
            (unless (memq name failed)
              (condition-case err
                  (cmdr-spec--activate-spec name)
                (error
                 (push name failed)
                 (map-put! cmdr-spec--pending-resources name t)
                 (cmdr-spec--record-failure (cmdr-spec-get name) "activation" err)))))))
      (when (and (map-empty-p cmdr-spec--pending-by-feature)
                 (map-empty-p cmdr-spec--pending-resources))
        (remove-hook 'after-load-functions #'cmdr-spec--after-load)))
    (setq cmdr-spec--dispatch-again nil)))

;;;###autoload
(defun cmdr-spec-reload (&optional name)
  "Reinstall and reactivate installed spec NAME, or all installed specs if nil."
  (interactive
   (progn
     (require 'annotated-completing-read)
     (let* ((candidates
             (cons (cons "All installed specs" "Reload every installed spec")
                   (seq-map (lambda (spec)
                              (cons (symbol-name (cmdr-spec-name spec))
                                    (or (cmdr-spec-doc spec) "")))
                            cmdr-spec-table)))
            (choice (annotated-completing-read
                     candidates :prompt "Reload spec: " :require-match t
                     :category 'cmdr-spec :or-nil t)))
       (unless choice (user-error "No spec selected"))
       (list (unless (equal choice "All installed specs") (intern choice))))))
  (dolist (spec (if name (list (cmdr-spec-get name))
                  (seq-filter (lambda (item)
                                (map-elt cmdr-spec--installed (cmdr-spec-name item)))
                              cmdr-spec-table)))
    (map-delete cmdr-spec--activated (cmdr-spec-name spec))
    (map-delete cmdr-spec--loaded-autoloads (cmdr-spec-name spec))
    (cmdr-spec-install (cmdr-spec-name spec))))

;;;###autoload
(defun cmdr-spec-install (name)
  "Install the spec called NAME into `cmdr-mode-map', ignoring its probe.
Installing again is idempotent."
  (interactive (list (intern (completing-read "Spec: " (seq-map #'cmdr-spec-name cmdr-spec-table) nil t))))
  (let ((spec (cmdr-spec-get name)))
    (seq-do (lambda (library)
              (unless (member library (map-elt cmdr-spec--loaded-autoloads name))
                (when (locate-library library)
                  (load library nil t)
                  (map-put! cmdr-spec--loaded-autoloads name
                            (cons library (map-elt cmdr-spec--loaded-autoloads name))))))
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
      (if (featurep 'cmdr)
          (cmdr-spec-flush-commands)
        (cmdr-spec--queue-feature 'cmdr name)))
    (when (cmdr-spec-settings spec)
      (cmdr-spec--apply-settings (cmdr-spec-settings spec)))
    (run-hooks (cmdr-spec--install-hook-name (cmdr-spec-name spec)))
    (let ((feature (if (map-elt cmdr-spec--activated name)
                       'none
                     (cmdr-spec--resolve-feature spec))))
      (when (and (or (cmdr-spec-feature spec)
                     (cmdr-spec-probe spec)
                     (symbol-value (cmdr-spec--activate-hook-name name)))
                 (not (map-elt cmdr-spec--activated name))
                 (not (eq feature 'none)) (not (featurep feature)))
        (cmdr-spec--queue-feature feature name))
      (cmdr-spec--activate-spec name nil feature))
    (map-put! cmdr-spec--installed name t)
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
