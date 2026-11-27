;;; orgx-capture.el --- Org-capture template DSL and registration -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, capture, convenience

;;; Commentary:
;; Capture template registration and builder DSL for `orgx'.  Defines flat
;; note, journal, and task template families, project-file registration
;; macros, customizable standard template sets, and soft-loaded hooks for
;; Denote and agent-shell-queue.
;;
;; The interactive `orgx-capture' command lives in `orgx.el'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'subr-x)
(require 'annotated-completing-read)

(eval-when-compile
  (require 'org))

(declare-function denote-org-capture "denote")
(declare-function denote-last-path "denote")
(declare-function denote-journal-capture-entry-for-date "denote-journal-capture")
(declare-function denote-journal-capture-entry-today "denote-journal-capture")
(declare-function denote-journal-capture-timestamp "denote-journal-capture")
(declare-function agent-shell-queue-capture-from-context "agent-shell-queue")
(declare-function org-agenda-files "org")

(defvar org-capture-templates)
(defvar org-capture-plist)
(defvar org-stored-links)
(defvar org-directory)

(defgroup orgx-capture nil
  "Capture template DSL and registration for `orgx'."
  :group 'org-capture
  :prefix "orgx-capture-")

(defcustom orgx-capture-default-notes-file "records.org"
  "Default file name for general note capture templates."
  :type 'string
  :group 'orgx-capture)

(defcustom orgx-capture-default-journal-file "journal.org"
  "Default file name for general journal capture templates."
  :type 'string
  :group 'orgx-capture)

(defcustom orgx-capture-default-tasks-file "planner.org"
  "Default file name for primary task capture templates."
  :type 'string
  :group 'orgx-capture)

(defcustom orgx-capture-default-agent-file "agentic.org"
  "Default file name for agent task and journal capture templates."
  :type 'string
  :group 'orgx-capture)

(defcustom orgx-capture-default-agent-tasks-headline "Tasks"
  "Headline under which agent tasks are captured.
When nil or empty, captures target the file directly without a parent headline."
  :type '(choice (string :tag "Parent headline")
                 (const :tag "File only (no parent headline)" nil))
  :group 'orgx-capture)

;;; External package integrations

(defun orgx-capture-setup-asq-template ()
  "Register `agent-shell-queue' template in `org-capture-templates'."
  (when (and (boundp 'org-capture-templates)
             (fboundp 'agent-shell-queue-capture-from-context))
    (add-to-list 'org-capture-templates
                 '("q" "agent queue item" plain
                   (function ignore)
                   ""
                   :immediate-finish t
                   :before-finalize (lambda ()
                                      (call-interactively
                                       #'agent-shell-queue-capture-from-context))))))


;;; Reset and helper functions

(defun orgx-capture--select-template ()
  "Interactively select an `org-capture-templates' key.
Uses `annotated-completing-read', grouping candidates primarily by
target file.  Templates with no file target (e.g. function-based
integration targets) fall back to their key-prefix's root description
when even that is unknown."
  (let* ((key-table (make-hash-table :test #'equal))
         (annotation-table (make-hash-table :test #'equal))
         (group-table (make-hash-table :test #'equal))
         (prefix-map (map-into
                      (seq-map (lambda (it) (cons (nth 0 it) (nth 1 it)))
                               (seq-filter (lambda (it) (<= (length it) 4))
                                           org-capture-templates))
                      '(hash-table :test equal))))
    (seq-do
     (lambda (template)
       (let* ((key-char (nth 0 template))
              (description (nth 1 template))
              (target-loc (cadr (nth 3 template)))
              (target-file (if (stringp target-loc) (file-name-nondirectory target-loc) ""))
              (content (nth 4 template))
              (raw (if (stringp content) (string-replace "\n" " " content) ""))
              (preview (if (> (length raw) 32) (concat (substring raw 0 29) "...") raw))
              (group (cond
                      ((not (string-empty-p target-file)) target-file)
                      ((map-elt prefix-map (substring key-char 0 1)))
                      (t "Other"))))
         (setf (map-elt key-table description) key-char)
         (setf (map-elt annotation-table description)
               (format "[%s] <%s> '%s'" key-char target-file preview))
         (setf (map-elt group-table description) group)))
     (seq-filter (lambda (it) (< 4 (length it))) org-capture-templates))
    (map-elt key-table
             (annotated-completing-read
              annotation-table
              :prompt "org-capture => "
              :category 'org-capture
              :require-match nil
              :group-name (lambda (candidate)
                            (map-elt group-table candidate ""))))))

;;;###autoload
(defun orgx-capture (&optional goto keys)
  "Launch Org capture using `orgx-capture' configured templates.
GOTO is passed directly to `org-capture'.  When KEYS is nil (the
interactive case), a template is selected via `annotated-completing-read'
through `orgx-capture--select-template' instead of Org's own key-menu;
passing KEYS explicitly dispatches straight to that template, as with
plain `org-capture'."
  (interactive "P")
  (unless (and (boundp 'org-capture-templates) org-capture-templates)
    (orgx-capture-setup-standard-templates))
  (org-capture goto (or keys (orgx-capture--select-template))))

;;;###autoload
(defun orgx-capture-reset-templates ()
  "Reset `org-capture-templates' to the standard base categories."
  (interactive)
  (setq org-capture-templates '(("t" "tasks")
                                ("j" "journal")
                                ("n" "notes")
                                ("a" "agent"))))

(defun orgx-capture--read-title ()
  "Read a title string interactively during `org-capture' template expansion."
  (read-string "title => "))

(defun orgx-capture--prompt-link ()
  "Interactively select or create an Org link.
If `org-stored-links' is non-nil, offers candidates via ACR or
`completing-read'.  Otherwise prompts for a link target and optional
description."
  (let* ((candidates (seq-map (lambda (item)
                                (let ((link (car item))
                                      (desc (cadr item)))
                                  (cons (if (and desc (not (string-empty-p desc)))
                                            (format "%s (%s)" desc link)
                                          link)
                                        item)))
                              (bound-and-true-p org-stored-links)))
         (choice (cond
                  ((and candidates (fboundp 'annotated-completing-read))
                   (annotated-completing-read
                    candidates
                    :prompt "Select stored org-link (or type new): "
                    :require-match nil))
                  (candidates
                   (completing-read "Select stored org-link (or type new): "
                                    candidates nil nil))
                  (t
                   (read-string "org-link (URL/file/path) => ")))))
    (if-let* ((found (assoc choice candidates)))
        (let ((link (cadr found))
              (desc (caddr found)))
          (if (and desc (not (string-empty-p desc)))
              (format "[[%s][%s]]" link desc)
            (format "[[%s]]" link)))
      (if (string-empty-p choice)
          ""
        (let ((desc (read-string "link description (optional) => ")))
          (if (not (string-empty-p desc))
              (format "[[%s][%s]]" choice desc)
            (format "[[%s]]" choice)))))))

(defun orgx-capture--get-link ()
  "Return an Org link string `[[link][description]]` for capture expansion.
First checks if an annotation link was captured or stored in `org-stored-links'.
If unavailable, interactively prompts the user to select or create a link."
  (let ((annotation (when (boundp 'org-capture-plist)
                      (plist-get org-capture-plist :annotation))))
    (cond
     ((and (stringp annotation) (not (string-empty-p annotation)))
      annotation)
     ((bound-and-true-p org-stored-links)
      (let ((latest (car org-stored-links)))
        (if (cadr latest)
            (format "[[%s][%s]]" (car latest) (cadr latest))
          (format "[[%s]]" (car latest)))))
     (t
      (orgx-capture--prompt-link)))))

;;; Flat template generator engine

(cl-defun orgx-capture--add-flat-templates
    (&key kind char name path (key "") target body-fn first-sub default-subs
          (prepend t) (time-prompt-suffix nil))
  "Register a flat set of capture templates of KIND for NAME at PATH.

CHAR is the single-letter kind prefix (\"t\", \"j\", \"n\").  KEY is
the project prefix; an empty string disables the hierarchical
\"<key><suffix>\" and shortcut \"<char><key>\" entries.

TARGET is the `org-capture' target form.  BODY-FN is called with
two arguments: the per-template body anchor string and a boolean flag
indicating whether the link is placed directly on the heading line.
It returns the template body string.

FIRST-SUB is (ANCHOR . DESCRIPTION) for the \"<char><key>\" shortcut,
added only when KEY is non-empty.

DEFAULT-SUBS is a list of (SUFFIX ANCHOR DESCRIPTION &rest EXTRA-PROPS);
each becomes a \"<key><suffix>\" template.

PREPEND becomes the templates' :prepend value.  When a key sequence
ends with TIME-PROMPT-SUFFIX, the template is marked :time-prompt t."
  (when (string-equal char key)
    (user-error "Cannot define %s %s org-capture-templates with key `%s'" kind name char))

  (let (specs append-item)
    (unless (string-equal "" key)
      (setq append-item t)
      (setf (alist-get (concat key char) org-capture-templates nil nil #'equal)
            (list (format "%s %s <%s>" name kind (file-name-nondirectory path))))
      (push (list (concat char key) (car first-sub) (cdr first-sub)) specs)
      (seq-do
       (lambda (sub)
         (let ((suffix (car sub)))
           (when (and (string-prefix-p char suffix)
                      (or (string-suffix-p "q" suffix)
                          (string-suffix-p "b" suffix)
                          (string-suffix-p "h" suffix)))
             (let ((shortcut-key (concat (substring suffix 0 (length char)) key (substring suffix (length char)))))
               (push (cons shortcut-key (cdr sub)) specs)))))
       default-subs))
    (seq-do
     (lambda (entry)
       (let* ((key-sequence (nth 0 entry))
              (template-anchor (nth 1 entry))
              (description (nth 2 entry))
              (extra-props (nthcdr 3 entry))
              (heading-anchor-p (plist-get extra-props :heading-anchor))
              (template (append (list key-sequence
                                      (format "%s (%s; %s)" kind name description)
                                      'entry target
                                      (funcall body-fn template-anchor heading-anchor-p)
                                      :prepend prepend
                                      :kill-buffer t
                                      :empty-lines 0)
                                extra-props)))
         (when (and time-prompt-suffix
                    (string-suffix-p time-prompt-suffix key-sequence))
           (setq template (append template (list :time-prompt t))))
         (setq org-capture-templates
               (seq-remove (lambda (it) (equal (car it) key-sequence))
                           org-capture-templates))
         (add-to-list 'org-capture-templates template append-item)))
     (append (nreverse specs)
             (seq-map (lambda (it) (cons (concat key (car it)) (cdr it)))
                      default-subs)))))

;;; Public template builder entrypoints

;;;###autoload
(cl-defun orgx-capture-add-journal-templates
    (&key name path (key "")
          (headline (unless (equal key "") "Journal")))
  "Register a full set of journal capture templates for NAME at PATH.
KEY is the project key prefix.
HEADLINE is the parent heading; nil or empty uses a root datetree."
  (orgx-capture--add-flat-templates
   :kind "journal" :char "j" :name name :path path :key key
   :target (if (and headline (not (string-empty-p headline)))
               (list 'file+olp+datetree path headline)
             (list 'file+olp+datetree path))
   :body-fn (lambda (anchor &optional heading-p)
              (cond
               (heading-p
                (concat "* " anchor " <%<%Y-%m-%d %H:%M>>%?"))
               ((string-empty-p anchor)
                "* %(orgx-capture--read-title) <%<%Y-%m-%d %H:%M>>%?")
               (t
                (concat "* %(orgx-capture--read-title) <%<%Y-%m-%d %H:%M>>\n" anchor "%?"))))
   :first-sub (cons "" "<today>")
   :default-subs '(("jj" "" "<today>")
                   ("jp" "" "<date prompt>")
                   ("jx" "%x" "X11 buffer")
                   ("jlb" "%(orgx-capture--get-link)" "org-link (body)")
                   ("jlh" "%(orgx-capture--get-link)" "org-link (heading)" :heading-anchor t)
                   ("jlq" "%(orgx-capture--prompt-link)" "quick org-link (heading)" :heading-anchor t :immediate-finish t)
                   ("jk" "%c" "emacs kill-ring"))
   :prepend nil
   :time-prompt-suffix "jp"))

;;;###autoload
(cl-defun orgx-capture-add-task-templates (&key name path (key "") (headline "Tasks"))
  "Register a full set of task capture templates for NAME at PATH.
When KEY is provided, prefix capture template keys with KEY.
When HEADLINE is nil or empty, templates target PATH directly as a file.
Otherwise, templates target the headline named HEADLINE in PATH."
  (let ((target (if (and headline (not (string-empty-p headline)))
                    (list 'file+headline path headline)
                  (list 'file path))))
    (orgx-capture--add-flat-templates
     :kind "tasks" :char "t" :name name :path path :key key
     :target target
     :body-fn (lambda (anchor &optional heading-p)
                (cond
                 (heading-p
                  (concat "* TODO " anchor "%?"))
                 ((string-empty-p anchor)
                  "* TODO %(orgx-capture--read-title)%?")
                 (t
                  (concat "* TODO %(orgx-capture--read-title)\n" anchor "%?"))))
     :first-sub (cons "%i" "selection")
     :default-subs '(("tt" "%i" "selection")
                     ("tq" "" "quick task" :immediate-finish t)
                     ("tx" "%x" "X11 buffer")
                     ("tlb" "%(orgx-capture--get-link)" "org-link (body)")
                     ("tlh" "%(orgx-capture--get-link)" "org-link (heading)" :heading-anchor t)
                     ("tlq" "%(orgx-capture--prompt-link)" "quick org-link task" :heading-anchor t :immediate-finish t)
                     ("tk" "%c" "emacs kill-ring")))))

;;;###autoload
(cl-defun orgx-capture-add-note-templates
    (&key name path (key "") (headline "Inbox") (kind "notes") (char "n"))
  "Register a full set of note capture templates for NAME at PATH.
KEY is the project key prefix.
HEADLINE is the parent heading; nil or empty captures at file level.
KIND and CHAR select the family label and key prefix (also used for inbox)."
  (orgx-capture--add-flat-templates
   :kind kind :char char :name name :path path :key key
   :target (if (and headline (not (string-empty-p headline)))
               (list 'file+headline path headline)
             (list 'file path))
   :body-fn (lambda (anchor &optional heading-p)
              (cond
               (heading-p
                (concat "* " anchor "%?"))
               ((string-empty-p anchor)
                "* %(orgx-capture--read-title)%?")
               (t
                (concat "* %(orgx-capture--read-title)\n" anchor "%?"))))
   :first-sub (cons "%i" "selection")
   :default-subs
   (seq-map (lambda (entry)
              (cons (if (equal (car entry) "nn")
                        (concat char char)
                      (concat char (substring (car entry) 1)))
                    (cdr entry)))
            '(("nn" "%i" "selection")
              ("nx" "%x" "X11 buffer")
              ("nlb" "%(orgx-capture--get-link)" "org-link (body)")
              ("nlh" "%(orgx-capture--get-link)" "org-link (heading)" :heading-anchor t)
              ("nlq" "%(orgx-capture--prompt-link)" "quick org-link (heading)" :heading-anchor t :immediate-finish t)
              ("nk" "%c" "emacs kill-ring")))))

;;; Registration helpers

;;;###autoload
(cl-defun orgx-capture-register-file
    (&key path name (key "") agenda (templates '(notes journal tasks)))
  "Register PATH with selected stock capture TEMPLATES.
Relative paths resolve against `org-directory'.  NAME labels the file;
KEY prefixes template keys.  AGENDA adds the file to
variable `org-agenda-files'.
TEMPLATES is a list of notes, inbox, tasks, journal, or datetree symbols,
or lists such as (tasks :headline nil) and (journal :headline \"Log\").
Nil or empty headlines capture at file level (a root datetree for journal).
Defaults are Inbox for notes/inbox, Tasks for tasks, and Journal for a
journal with a nonempty KEY.  Inbox uses the i key family; datetree is an
alias for journal.  Existing conventional `org-capture-templates' remain
available; register them directly as usual.  Reusing a generated key
replaces that entry, making repeated registrations safe."
  (unless (and (stringp path) (not (string-empty-p path))
               (stringp key) (or (null name) (stringp name)))
    (user-error "Capture registration needs a path and string name/key"))
  (seq-do
   (lambda (spec)
     (let ((kind (if (consp spec) (car spec) spec))
           (options (when (consp spec) (cdr spec))))
       (unless (and (memq kind '(notes inbox tasks journal datetree))
                    (or (null options)
                        (and (= (length options) 2)
                             (eq (car options) :headline)
                             (or (null (cadr options))
                                 (stringp (cadr options))))))
         (user-error "Invalid stock capture template: %S" spec))
       (when (equal key (pcase kind
                          ('tasks "t")
                          ((or 'journal 'datetree) "j")
                          ('notes "n")
                          ('inbox "i")))
         (user-error "Capture key conflicts with family: %S" kind))))
   templates)
  (require 'org-capture)
  (setq path (expand-file-name path org-directory))
  (setq name (or name (file-name-base path)))
  (when agenda
    (add-to-list 'org-agenda-files path))
  (unless (string-empty-p key)
    (setf (alist-get key org-capture-templates nil nil #'equal)
          (list (format "%s (project; %s)" name (file-name-nondirectory path)))))
  (seq-do
   (lambda (spec)
     (let* ((kind (if (consp spec) (car spec) spec))
            (options (when (consp spec) (cdr spec)))
            (args (append (list :name name :path path :key key) options)))
       (pcase kind
         ('tasks (apply #'orgx-capture-add-task-templates args))
         ((or 'journal 'datetree)
          (apply #'orgx-capture-add-journal-templates args))
         ('notes (apply #'orgx-capture-add-note-templates args))
         ('inbox (apply #'orgx-capture-add-note-templates
                        :kind "inbox" :char "i" args)))))
   templates))

;;;###autoload
(defmacro orgx-capture-add-project-file-templates (&rest args)
  "Register project file capture templates for PATH.
ARGS are passed to `orgx-capture--add-project-file-templates'.
If `org' is loaded, configures templates immediately; otherwise hooks
into `org-load-hook'."
  `(if (featurep 'org)
       (orgx-capture--add-project-file-templates ,@args)
     (add-hook 'org-load-hook
               (lambda () (orgx-capture--add-project-file-templates ,@args)))))

(cl-defun orgx-capture--add-project-file-templates
    (&key (name "planner") path (key "") agenda
          (templates '(notes journal tasks)))
  "Register project PATH, NAME, KEY, AGENDA and selected TEMPLATES.
Compatibility entry point for `orgx-capture-add-project-file-templates'.
See `orgx-capture-register-file' for stock template specifications."
  (orgx-capture-register-file
   :name name :path (or path (concat (subst-char-in-string ?\s ?- (downcase name)) ".org"))
   :key key :agenda agenda :templates templates))

;;;###autoload
(defun orgx-capture-setup-standard-templates ()
  "Install standard orgx capture templates using customizable default files."
  (interactive)
  (require 'org-capture)
  (unless (boundp 'org-capture-templates)
    (setq org-capture-templates nil))
  (seq-do (lambda (header) (add-to-list 'org-capture-templates header t))
          '(("t" "tasks") ("j" "journal") ("n" "notes") ("a" "agent")))
  (orgx-capture-add-note-templates
   :name "notes"
   :path orgx-capture-default-notes-file)
  (orgx-capture-add-journal-templates
   :name "diary"
   :path orgx-capture-default-journal-file)
  (orgx-capture-add-task-templates
   :name "prime"
   :path orgx-capture-default-tasks-file)
  (orgx-capture-add-task-templates
   :name "agentic"
   :path orgx-capture-default-agent-file
   :key "a"
   :headline orgx-capture-default-agent-tasks-headline)
  (orgx-capture-add-journal-templates
   :name "agentic"
   :path orgx-capture-default-agent-file
   :key "a")
  (orgx-capture-setup-asq-template)
  (when (fboundp 'orgx-denote-setup-capture-templates)
    (orgx-denote-setup-capture-templates)))

(provide 'orgx-capture)
;;; orgx-capture.el ends here
