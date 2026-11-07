;;; cmdr.el --- Command registry with Transient and ACR frontends -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4") (transient "0.9.3") (annotated-completing-read "0.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/cmdr.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Registry of categorized commands, exposed as `cmdr-menu' (Transient) and
;; `cmdr-dispatch' (annotated completing-read).
;;
;; Register commands with `cmdr-register-command'.  Entries are identified
;; by (CATEGORY . COMMAND); registering the same pair again replaces the
;; entry in place.  Categories keep their insertion order.  The registry is
;; never reset by loading this library; call `cmdr-reset-command-table'
;; explicitly.
;;
;; Both frontends honor the same contract:
;;
;; - :if is a visibility predicate; a nil result hides the entry.
;; - :ifapt is an availability predicate; a non-nil result marks the entry
;;   unavailable.  Unavailable entries are shown (grayed out in the menu,
;;   annotated in `cmdr-dispatch') but invoking them signals `user-error'.
;; - Predicates run in the buffer that was current when the frontend was
;;   invoked, and are checked again immediately before the command runs.
;; - The command runs through `call-interactively' with the prefix argument
;;   that was given to the frontend.
;;
;; The Transient menu is built each time `cmdr-menu' is invoked, so later
;; registrations appear without reloading anything.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'annotated-completing-read)

;;;; Registry

(defvar cmdr-command-table nil
  "Alist mapping category symbols to lists of `cmdr-command' structs.
Categories appear in registration order.  Populated by
`cmdr-register-command' and cleared only by `cmdr-reset-command-table'.")

(defconst cmdr--max-row 3
  "Highest row number `cmdr-menu' lays out.")

(defconst cmdr--reserved-transient-keys '("C-g" "C-h" "C-q" "C-x" "C-z")
  "Keys Transient binds in every menu; registrations may not use them.")

(define-error 'cmdr-predicate-error "cmdr predicate failed")

(cl-defstruct (cmdr-command
               (:copier nil)
               (:constructor make-cmdr-command
                             (&key command description category transient-key display-name row visible inapt exclude-from-transient
                                   &aux (category (or category (cmdr--derive-category command)))
                                   (display-name (or display-name
                                                     (when command (symbol-name command))))
                                   (row (or row 1)))))
  "A command entry in the cmdr registry."
  (command nil :type symbol :documentation "Interactive command symbol to invoke.")
  (description nil :type (or null string) :documentation "Short label shown in menus and annotations.")
  (category nil :type symbol :documentation "Grouping key.")
  (transient-key nil :type (or null string) :documentation "Key string for the Transient suffix; nil for completion-only entries.")
  (display-name nil :type (or null string) :documentation "Candidate string shown in `cmdr-dispatch'.")
  (row 1 :type integer :documentation "Row of the Transient layout.")
  (visible nil :documentation "Predicate (:if): nil result hides the entry.")
  (inapt nil :documentation "Predicate (:ifapt): non-nil result makes the entry unavailable.")
  (exclude-from-transient nil :documentation "When non-nil the entry is omitted from `cmdr-menu'."))

(defun cmdr--derive-category (command)
  "Derive a category symbol from COMMAND's defining file, or \\='cmdr."
  (or (when-let* ((file (and command (symbol-file command 'defun))))
        (intern (file-name-base file)))
      'cmdr))

(defun cmdr--flat-entries ()
  "Return a flat list of (CATEGORY . CMDR-COMMAND) pairs from the table."
  (seq-mapcat (lambda (bucket)
                (seq-map (lambda (cmd) (cons (car bucket) cmd))
                         (cdr bucket)))
              cmdr-command-table))

(defun cmdr--key-events (key)
  "Return the normalized list of events for Transient KEY.
Signals `user-error' when KEY is not a non-empty string."
  (let ((events (when (and (stringp key) (not (string-empty-p key)))
                  (listify-key-sequence (kbd key)))))
    (unless events
      (user-error "Invalid Transient key %S" key))
    events))

(defun cmdr--key-prefix-p (a b)
  "Return non-nil when event lists A and B are equal or one prefixes the other."
  (let ((n (min (length a) (length b))))
    (equal (seq-take a n) (seq-take b n))))

(defun cmdr--validate-entry (entry)
  "Signal `user-error' unless ENTRY is consistent with the current table.
Other entries sharing ENTRY's identity are ignored, since they are
about to be replaced."
  (let* ((cat (cmdr-command-category entry))
         (command (cmdr-command-command entry))
         (key (cmdr-command-transient-key entry))
         (name (cmdr-command-display-name entry))
         (row (cmdr-command-row entry))
         (others (seq-remove (lambda (pair)
                               (and (eq cat (car pair))
                                    (eq command (cmdr-command-command (cdr pair)))))
                             (cmdr--flat-entries))))
    (unless (and (integerp row) (<= 1 row cmdr--max-row))
      (user-error "Invalid row %S for %S: expected an integer from 1 to %d"
                  row command cmdr--max-row))
    (when-let* ((clash (seq-find (lambda (pair)
                                   (and (eq cat (car pair))
                                        (/= row (cmdr-command-row (cdr pair)))))
                                 others)))
      (user-error "Row %S for %S conflicts with row %S of %S in category %S"
                  row command (cmdr-command-row (cdr clash))
                  (cmdr-command-command (cdr clash)) cat))
    (when-let* ((clash (seq-find (lambda (pair)
                                   (equal name (cmdr-command-display-name (cdr pair))))
                                 others)))
      (user-error "Display name %S for %S is already used by %S in category %S"
                  name command (cmdr-command-command (cdr clash)) (car clash)))
    (when key
      (let ((events (cmdr--key-events key)))
        (when-let* ((reserved (seq-find (lambda (it)
                                          (cmdr--key-prefix-p events (cmdr--key-events it)))
                                        cmdr--reserved-transient-keys)))
          (user-error "Transient key %S for %S collides with reserved key %S"
                      key command reserved))
        (when-let* ((clash (seq-find (lambda (pair)
                                       (when-let* ((other (cmdr-command-transient-key (cdr pair))))
                                         (cmdr--key-prefix-p events (cmdr--key-events other))))
                                     others)))
          (user-error "Transient key %S for %S collides with key %S of %S in category %S"
                      key command (cmdr-command-transient-key (cdr clash))
                      (cmdr-command-command (cdr clash)) (car clash)))))))

(defun cmdr--table-with (table entry)
  "Return TABLE with ENTRY added, or replacing the entry of the same identity."
  (let ((cat (cmdr-command-category entry))
        (command (cmdr-command-command entry)))
    (if (assq cat table)
        (seq-map (lambda (bucket)
                   (if (eq cat (car bucket))
                       (cons cat
                             (if (seq-find (lambda (it) (eq command (cmdr-command-command it)))
                                           (cdr bucket))
                                 (seq-map (lambda (it)
                                            (if (eq command (cmdr-command-command it))
                                                entry
                                              it))
                                          (cdr bucket))
                               (append (cdr bucket) (list entry))))
                     bucket))
                 table)
      (append table (list (cons cat (list entry)))))))

;;;###autoload
(cl-defun cmdr-register-command (&key category command description transient-key display-name (row 1) if ifapt exclude-from-transient)
  "Add or replace the entry for CATEGORY and COMMAND in `cmdr-command-table'.

An entry is identified by (CATEGORY . COMMAND); registering it again
replaces the entry and keeps its position.  CATEGORY defaults to the
name of the file defining COMMAND, or `cmdr'.

DESCRIPTION is the label shown in the menu and the annotation shown by
`cmdr-dispatch'.  TRANSIENT-KEY is the key string of the menu suffix; an
entry without one is completion-only and reserves no key.  DISPLAY-NAME
is the `cmdr-dispatch' candidate and defaults to the name of COMMAND.

ROW (1 to `cmdr--max-row', default 1) places the category in the menu
layout; every entry of a category must use the same row.

IF is a visibility predicate: a nil result hides the entry from both
frontends.  IFAPT is an availability predicate: a non-nil result makes
the entry unavailable, so it is shown but cannot be invoked.  Both are
called with no arguments in the buffer that was current when the
frontend was invoked.

EXCLUDE-FROM-TRANSIENT omits the entry from `cmdr-menu' while keeping it
in `cmdr-dispatch'.

Signals `user-error', leaving the table unchanged, when the entry has an
invalid command, row or key, when its row differs from its category's,
when its key equals or prefixes another entry's key (keys are compared
after normalization and need not be shown in the menu), or when its
display name is already in use."
  (unless (and command (symbolp command))
    (user-error "Invalid command %S: expected a symbol" command))
  (let ((entry (make-cmdr-command :command command
                                  :description description
                                  :category category
                                  :transient-key transient-key
                                  :display-name display-name
                                  :row row
                                  :visible if
                                  :inapt ifapt
                                  :exclude-from-transient exclude-from-transient)))
    (cmdr--validate-entry entry)
    (setq cmdr-command-table (cmdr--table-with cmdr-command-table entry))
    entry))

(defun cmdr-reset-command-table ()
  "Remove every registration from `cmdr-command-table'."
  (interactive)
  (setq cmdr-command-table nil))

(defun cmdr--ensure-registered ()
  "Signal `user-error' when nothing has been registered."
  (unless cmdr-command-table
    (user-error "No commands registered; add some with `cmdr-register-command'")))

;;;; Predicates and invocation

(defun cmdr--call-predicate (entry slot pred origin)
  "Call PRED for ENTRY's SLOT in buffer ORIGIN and return its value.
Errors are re-signaled as `cmdr-predicate-error' naming the command, its
category, SLOT and the original error; `quit' is not intercepted."
  (with-current-buffer (if (buffer-live-p origin)
                           origin
                         (current-buffer))
    (condition-case err
        (funcall pred)
      (error (signal 'cmdr-predicate-error
                     (list (cmdr-command-command entry)
                           (cmdr-command-category entry)
                           slot
                           err))))))

(defun cmdr--entry-visible-p (entry &optional origin)
  "Return non-nil when ENTRY's :if predicate allows it, checked in ORIGIN."
  (if-let* ((pred (cmdr-command-visible entry)))
      (cmdr--call-predicate entry :if pred origin)
    t))

(defun cmdr--entry-inapt-p (entry &optional origin)
  "Return non-nil when ENTRY's :ifapt predicate marks it unavailable in ORIGIN."
  (when-let* ((pred (cmdr-command-inapt entry)))
    (cmdr--call-predicate entry :ifapt pred origin)))

(defun cmdr--entry-available-p (entry &optional origin)
  "Return non-nil when ENTRY is visible and not inapt, checked in ORIGIN."
  (and (cmdr--entry-visible-p entry origin)
       (not (cmdr--entry-inapt-p entry origin))))

(defun cmdr--check-invocable (entry origin)
  "Signal `user-error' unless ENTRY can run now, checked in ORIGIN."
  (let ((command (cmdr-command-command entry))
        (cat (cmdr-command-category entry)))
    (cond
     ((not (cmdr--entry-visible-p entry origin))
      (user-error "Command %S (category %S) is not visible" command cat))
     ((cmdr--entry-inapt-p entry origin)
      (user-error "Command %S (category %S) is unavailable" command cat))
     ((not (fboundp command))
      (user-error "Command %S registered in category %S is not defined" command cat))
     ((not (commandp command))
      (user-error "Command %S registered in category %S is not interactive" command cat)))))

(defun cmdr--invoke (entry origin prefix)
  "Run ENTRY's command interactively in ORIGIN with prefix argument PREFIX.
Visibility and availability are checked again first."
  (with-current-buffer (if (buffer-live-p origin)
                           origin
                         (current-buffer))
    (cmdr--check-invocable entry origin)
    (let ((current-prefix-arg prefix))
      (call-interactively (cmdr-command-command entry)))))

;;;; Transient menu

(defun cmdr--bucket-to-column (bucket origin)
  "Convert BUCKET (CATEGORY . ENTRIES) to a Transient column vector.
Entries that are excluded or have no key are omitted.  Predicates and
the pre-invocation check run in buffer ORIGIN."
  (apply #'vector
         (symbol-name (car bucket))
         (thread-last (cdr bucket)
           (seq-remove #'cmdr-command-exclude-from-transient)
           (seq-filter #'cmdr-command-transient-key)
           (seq-map (lambda (entry)
                      (append (list (cmdr-command-transient-key entry)
                                    (or (cmdr-command-description entry)
                                        (symbol-name (cmdr-command-command entry)))
                                    (cmdr-command-command entry))
                              (when (cmdr-command-visible entry)
                                (list :if (lambda () (cmdr--entry-visible-p entry origin))))
                              (when (cmdr-command-inapt entry)
                                (list :inapt-if (lambda () (cmdr--entry-inapt-p entry origin))))
                              (list :advice* (lambda (fn &rest args)
                                               (cmdr--check-invocable entry origin)
                                               (apply fn args)))))))))

(defun cmdr--setup-row-children (row)
  "Build Transient columns for the categories placed in ROW."
  (let ((origin (current-buffer)))
    (transient-parse-suffixes
     'cmdr-menu
     (thread-last cmdr-command-table
       (seq-filter (lambda (bucket)
                     (= row (cmdr-command-row (cadr bucket)))))
       (seq-map (lambda (bucket)
                  (cmdr--bucket-to-column bucket origin)))
       (seq-filter (lambda (column)
                     (> (length column) 1)))))))

;;;###autoload (autoload 'cmdr-menu "cmdr" nil t)
(transient-define-prefix cmdr-menu ()
  "Categorized menu of every registered command.
The layout is rebuilt on each invocation from `cmdr-command-table'."
  [:class transient-columns
   :setup-children (lambda (_) (cmdr--setup-row-children 1))]
  [:class transient-columns
   :setup-children (lambda (_) (cmdr--setup-row-children 2))]
  [:class transient-columns
   :setup-children (lambda (_) (cmdr--setup-row-children 3))]
  (interactive)
  (cmdr--ensure-registered)
  (transient-setup 'cmdr-menu))

;;;###autoload
(defun cmdr-transient-insert-suffix-once (prefix loc suffix)
  "Insert SUFFIX at LOC in transient PREFIX unless already present.
No-op if PREFIX has no `transient--layout' defined yet, or if a suffix
with the same key already exists in PREFIX."
  (when (get prefix 'transient--layout)
    (let ((key (if (consp suffix)
                   (car suffix)
                 suffix)))
      (unless (ignore-errors (transient-get-suffix prefix key))
        (transient-insert-suffix prefix loc suffix)))))

;;;; Completing-read frontend

(defun cmdr--candidates (origin)
  "Return an alist of (DISPLAY-NAME . (CATEGORY . ENTRY)) for visible entries.
Visibility is evaluated in buffer ORIGIN."
  (thread-last (cmdr--flat-entries)
    (seq-filter (lambda (pair)
                  (cmdr--entry-visible-p (cdr pair) origin)))
    (seq-map (lambda (pair)
               (cons (cmdr-command-display-name (cdr pair)) pair)))))

;;;###autoload
(defun cmdr-dispatch ()
  "Select a registered command by name and run it.
Candidates are annotated with their description and grouped by category.
Unavailable commands are listed, marked as such, and refuse to run."
  (interactive)
  (cmdr--ensure-registered)
  (let* ((origin (current-buffer))
         (prefix current-prefix-arg)
         (candidates (cmdr--candidates origin)))
    (unless candidates
      (user-error "No registered commands are currently visible"))
    (let ((label (annotated-completing-read
                  (seq-map (lambda (candidate)
                             (cons (car candidate)
                                   (concat (or (cmdr-command-description (cddr candidate)) "")
                                           (when (cmdr--entry-inapt-p (cddr candidate) origin)
                                             " (unavailable)"))))
                           candidates)
                  :prompt "cmdr:"
                  :require-match t
                  :category 'cmdr-dispatch
                  :group-name (lambda (name)
                                (when-let* ((candidate (assoc name candidates)))
                                  (symbol-name (cadr candidate)))))))
      (when (and (stringp label) (not (string-empty-p label)))
        (if-let* ((candidate (assoc label candidates)))
            (cmdr--invoke (cddr candidate) origin prefix)
          (user-error "No visible cmdr command named %S" label))))))

(provide 'cmdr)
;;; cmdr.el ends here
