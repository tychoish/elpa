;;; orgx-capture-shell.el --- Non-modal capture shell and REPL for Org mode -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, capture, convenience, comm

;;; Commentary:
;; A non-modal comint REPL and capture shell for Org mode.
;; Provides continuous, rapid logging into Org headings without interrupting
;; window configurations or entering modal capture buffers.
;;
;; Features:
;; - Comint-based REPL with customizable prompt and completion
;; - Cascading target resolution via `orgx-capture-shell-target-directory-alist'
;; - Dedicated bottom popup window (15 lines) with toggle command
;; - Slash commands (/target, /template, /form, /denote, /undo, /recent, /help)
;; - Capture form: end a line with `orgx-capture-shell-form-trigger' or press
;;   C-RET to be prompted for title, template, target and body
;; - Undo stack for rolling back captured entries

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'button)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'transient)

(defvar org-capture-templates)
(defvar org-directory)

(declare-function denote-directory-files "denote")
(declare-function denote-retrieve-filename-title "denote")

(defgroup orgx-capture-shell nil
  "Continuous capture shell and REPL for Org mode."
  :group 'orgx
  :prefix "orgx-capture-shell-")

(defcustom orgx-capture-shell-buffer-name "*orgx-capture-shell*"
  "Name of the orgx capture shell buffer."
  :type 'string
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-default-file "agentic.org"
  "Default target Org file when no directory-specific target matches."
  :type 'string
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-default-headline "Tasks"
  "Default target headline within the target file."
  :type 'string
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-target-directory-alist nil
  "Alist mapping directory path patterns to default (FILE . HEADLINE) targets.
Each entry is of the form (DIRECTORY-PATTERN . (FILE . HEADLINE)) or
\(DIRECTORY-PATTERN . FILE).
When capturing, if `default-directory' matches DIRECTORY-PATTERN,
that file and headline are used as the default target."
  :type '(alist :key-type (choice string regexp)
                :value-type (choice (cons string string) string))
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-popup-height 15
  "Height in lines for the bottom popup window."
  :type 'integer
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-window-rule t
  "When non-nil, automatically register display-buffer rule for bottom popup."
  :type 'boolean
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-form-trigger ";"
  "String that, when ending a line of input, opens the capture form.
The trigger is stripped and the rest of the line becomes the initial title.
Set to nil to disable; \\`C-RET' always opens the form."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-form-style 'prompt
  "How the capture form collects values.
`prompt' asks for each field in the minibuffer; `transient' opens a
transient menu with editable fields."
  :type '(choice (const prompt) (const transient))
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-headline-separator "/"
  "Separator for nested outline paths in a target headline (e.g. \"Projects/Tasks\")."
  :type 'string
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-prepend 'follow
  "Where new entries go within the target section.
`follow' uses the `:prepend' setting of the matching `org-capture-templates'
entry (see `orgx-capture-shell--resolve-prepend'); t always inserts at the top
of the section, before its first child; nil appends.  Either way an entry
never lands after a trailing child tagged with `org-archive-tag'."
  :type '(choice (const :tag "Follow the capture template" follow)
                 (const :tag "Always prepend" t)
                 (const :tag "Always append" nil))
  :group 'orgx-capture-shell)

(defconst orgx-capture-shell-templates '(task note journal plain)
  "Recognized capture templates.")

(defconst orgx-capture-shell--prompt-regexp
  (rx bol (any "●✓✗") " [" (*? nonl) "] ❯ ")
  "Regexp matching the shell prompt.")

;;; State variables (buffer-local)

(defvar-local orgx-capture-shell-current-file nil
  "Current target Org file for this shell buffer.")

(defvar-local orgx-capture-shell-current-headline nil
  "Current target headline within `orgx-capture-shell-current-file'.")

(defvar-local orgx-capture-shell-current-template 'task
  "Current active template type: \\='task, \\='note, \\='journal, or \\='plain.")

(defcustom orgx-capture-shell-undo-ring-max 50
  "Maximum number of captured items to retain in `orgx-capture-shell-undo-ring'."
  :type 'integer
  :group 'orgx-capture-shell)

(defvar orgx-capture-shell-undo-ring nil
  "Global list of recently captured items: ((FILE POSITION TEXT)...).")

;;; Target resolution cascade

(defun orgx-capture-shell--resolve-target (&optional dir)
  "Resolve the default (FILE . HEADLINE) target for DIR or `default-directory'."
  (let* ((target-dir (file-name-as-directory (expand-file-name (or dir default-directory))))
         (matched nil))
    (dolist (entry orgx-capture-shell-target-directory-alist)
      (when (and (not matched)
                 (let ((pat (car entry)))
                   (if (string-prefix-p "^" pat)
                       (string-match-p pat target-dir)
                     (string-prefix-p (expand-file-name pat) target-dir))))
        (setq matched (cdr entry))))
    (cond
     ((consp matched)
      (cons (expand-file-name (car matched)) (cdr matched)))
     ((stringp matched)
      (cons (expand-file-name matched) orgx-capture-shell-default-headline))
     (t
      (let ((base-file (if (boundp 'org-directory)
                           (expand-file-name orgx-capture-shell-default-file org-directory)
                         (expand-file-name orgx-capture-shell-default-file))))
        (cons base-file orgx-capture-shell-default-headline))))))

;;; Popup window management

(defun orgx-capture-shell-register-window-rule ()
  "Register the 15-line bottom popup rule in `display-buffer-alist'."
  (interactive)
  (let ((rule `(,(regexp-quote orgx-capture-shell-buffer-name)
                (display-buffer-reuse-window display-buffer-at-bottom)
                (window-height . ,orgx-capture-shell-popup-height)
                (reusable-frames . visible))))
    (unless (assoc (car rule) display-buffer-alist)
      (add-to-list 'display-buffer-alist rule))))


;;; Core capture engine

(defconst orgx-capture-shell--heading-line-regexp "\\`\\(\\*+\\) "
  "Regexp matching an Org heading line, capturing the stars.")

(defun orgx-capture-shell--format-entry (text template)
  "Format capture TEXT according to TEMPLATE (\\='task, \\='note, \\='journal, \\='plain)."
  (let ((trimmed (string-trim text)))
    (if (string-match-p orgx-capture-shell--heading-line-regexp trimmed)
        trimmed
      (pcase template
        ('task
         (format "* TODO %s" trimmed))
        ('journal
         (let* ((parts (split-string trimmed "\n"))
                (title (car parts))
                (body (cdr parts)))
           (concat (format "* %s <%s>" title
                           (format-time-string "%Y-%m-%d %H:%M"))
                   (if body (concat "\n" (string-join body "\n")) ""))))
        (_
         (format "* %s" trimmed))))))

(defun orgx-capture-shell--relevel (text level)
  "Shift the headings in TEXT so the shallowest one is at LEVEL."
  (let* ((lines (split-string text "\n"))
         (shallowest (cl-loop for l in lines
                              when (string-match orgx-capture-shell--heading-line-regexp l)
                              minimize (length (match-string 1 l)))))
    (if (or (null shallowest) (= shallowest level))
        text
      (let ((delta (- level shallowest)))
        (mapconcat
         (lambda (l)
           (if (string-match orgx-capture-shell--heading-line-regexp l)
               (concat (make-string (max 1 (+ (length (match-string 1 l)) delta)) ?*)
                       (substring l (match-end 1)))
             l))
         lines "\n")))))

(defun orgx-capture-shell--headline-path (headline)
  "Split HEADLINE into a list of outline path segments."
  (and headline
       (split-string headline (regexp-quote orgx-capture-shell-headline-separator)
                     t "[ \t]+")))

(defconst orgx-capture-shell--template-kinds
  '((task . "tasks") (note . "notes") (journal . "journal"))
  "Map shell templates to the kind word starting capture template descriptions.")

(defconst orgx-capture-shell--builtin-prepend
  '((task . t) (note . t) (journal . nil) (plain . t))
  "Prepend defaults per shell template, mirroring `orgx-capture'.")

(defun orgx-capture-shell--template-file-p (tpl file)
  "Return non-nil if the capture template TPL targets FILE."
  (let ((target (nth 3 tpl)))
    (and file (consp target) (stringp (nth 1 target))
         (string-equal (expand-file-name (nth 1 target)
                                         (and (boundp 'org-directory) org-directory))
                       (expand-file-name file)))))

(defun orgx-capture-shell--resolve-prepend (template &optional file)
  "Return non-nil if entries for shell TEMPLATE should be prepended.
Follows `orgx-capture-shell-prepend'.  In `follow' mode the `:prepend' of
the `org-capture-templates' entry of the same kind is used, preferring one
whose target is FILE; absent one, the builtin default for TEMPLATE."
  (if (not (eq orgx-capture-shell-prepend 'follow))
      orgx-capture-shell-prepend
    (let* ((kind (alist-get template orgx-capture-shell--template-kinds))
           (candidates
            (and kind (boundp 'org-capture-templates)
                 (seq-filter (lambda (tpl)
                               (and (consp tpl) (>= (length tpl) 5)
                                    (stringp (nth 1 tpl))
                                    (string-prefix-p kind (nth 1 tpl))))
                             org-capture-templates)))
           (tpl (or (seq-find (lambda (c) (orgx-capture-shell--template-file-p c file))
                              candidates)
                    (car candidates))))
      (if tpl
          (and (plist-get (nthcdr 5 tpl) :prepend) t)
        (alist-get template orgx-capture-shell--builtin-prepend t)))))

(defun orgx-capture-shell--insertion-point (parent level end &optional prepend)
  "Return where a new child of the section at LEVEL belongs.
PARENT is the position of the section's heading, or nil for the whole
buffer; END is the end of the section.  With PREPEND the position is before
the first child; otherwise at the end, skipping trailing children tagged
`org-archive-tag'."
  (save-excursion
    (let ((child-level (1+ level))
          (start (if parent
                     (progn (goto-char parent) (line-end-position))
                   (point-min)))
          children)
      (goto-char start)
      (if prepend
          (if (re-search-forward "^\\*+ " end t) (match-beginning 0) end)
        (while (re-search-forward "^\\(\\*+\\) " end t)
          (when (= (length (match-string 1)) child-level)
            (push (cons (match-beginning 0)
                        (and (member org-archive-tag (org-get-tags nil t)) t))
                  children)))
        (let ((pos end))
          (while (and children (cdar children))
            (setq pos (caar children))
            (pop children))
          pos)))))

(defun orgx-capture-shell--locate (headline &optional prepend)
  "Find, creating as needed, the insertion point for entries under HEADLINE.
HEADLINE is an outline path; each segment is searched among the
descendants of the previous one.  Missing segments are created with the
correct nesting.  Returns (LEVEL . POSITION) where LEVEL is the level of the
parent heading (0 at top level) and POSITION is where the entry goes, per
`orgx-capture-shell--insertion-point' and PREPEND.
Must be called with the buffer widened."
  (let ((level 0)
        (beg (point-min))
        (end (point-max)))
    (dolist (seg (orgx-capture-shell--headline-path headline))
      (let ((found nil))
        (goto-char beg)
        (while (and (not found)
                    (re-search-forward "^\\(\\*+\\) " end t))
          (when (and (> (length (match-string 1)) level)
                     (string-equal (org-get-heading t t t t) seg))
            (setq found (match-beginning 0))))
        (if found
            (progn
              (goto-char found)
              (setq level (length (progn (looking-at "\\*+") (match-string 0))))
              (setq beg found)
              (org-end-of-subtree t t)
              (setq end (point)))
          (goto-char end)
          (unless (bolp) (insert "\n"))
          (setq beg (point))
          (insert (make-string (1+ level) ?*) " " seg "\n")
          (setq level (1+ level))
          (setq end (point)))))
    (cons level (orgx-capture-shell--insertion-point
                 (and (> level 0) beg) level end prepend))))

(defun orgx-capture-shell--insert-entry (file headline entry-text &optional prepend)
  "Insert ENTRY-TEXT under HEADLINE in FILE, at the top if PREPEND.
HEADLINE may be a nested outline path.  Headings in ENTRY-TEXT are
re-leveled to be children of the target.  Returns a record
\(FILE MARKER-POS ENTRY-TEXT) for the undo ring."
  (let* ((expanded (expand-file-name file))
         (dir (file-name-directory expanded)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t))
    (let ((buf (or (get-file-buffer expanded)
                   (find-file-noselect expanded t))))
      (with-current-buffer buf
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (let ((rec (save-excursion
                     (org-with-wide-buffer
                      (let* ((ctx (orgx-capture-shell--locate headline prepend))
                             (text (orgx-capture-shell--relevel entry-text (1+ (car ctx)))))
                        (goto-char (cdr ctx))
                        (unless (bolp) (insert "\n"))
                        (let ((pos (point)))
                          (insert text "\n")
                          (save-buffer)
                          (list expanded pos text (copy-marker pos t))))))))
          (push rec orgx-capture-shell-undo-ring)
          (when (> (length orgx-capture-shell-undo-ring) orgx-capture-shell-undo-ring-max)
            (setq orgx-capture-shell-undo-ring
                  (seq-take orgx-capture-shell-undo-ring orgx-capture-shell-undo-ring-max)))
          rec)))))

(defun orgx-capture-shell--buffer-headline-paths ()
  "Return outline paths of all headings in the current buffer."
  (let (stack paths)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward "^\\(\\*+\\) " nil t)
       (let ((lvl (length (match-string 1)))
             (title (org-get-heading t t t t)))
         (while (and stack (>= (caar stack) lvl)) (pop stack))
         (push (cons lvl title) stack)
         (push (mapconcat #'cdr (reverse stack)
                          orgx-capture-shell-headline-separator)
               paths))))
    (nreverse paths)))

(defun orgx-capture-shell--headline-paths (file)
  "Return outline paths of the headings in FILE, or nil if it doesn't exist."
  (when (and file (file-exists-p file))
    (if-let* ((buf (get-file-buffer (expand-file-name file))))
        (with-current-buffer buf (orgx-capture-shell--buffer-headline-paths))
      (with-temp-buffer
        (insert-file-contents file)
        (delay-mode-hooks (org-mode))
        (orgx-capture-shell--buffer-headline-paths)))))

(defvar orgx-capture-shell--status nil
  "Dynamically bound status of the current shell input, when processing one.")

(cl-defun orgx-capture-shell--capture (text &key (template orgx-capture-shell-current-template)
                                            (file orgx-capture-shell-current-file)
                                            (headline orgx-capture-shell-current-headline)
                                            body)
  "Capture TEXT (plus optional BODY) using TEMPLATE under HEADLINE in FILE.
Missing FILE falls back to the resolved default target.  Returns a
description string."
  (unless file
    (let ((res (orgx-capture-shell--resolve-target)))
      (setq file (car res))
      (when (or (null headline) (string-empty-p headline))
        (setq headline (cdr res)))))
  (let* ((entry (orgx-capture-shell--format-entry text template))
         (entry (if (and body (not (string-empty-p (string-trim body))))
                    (concat entry "\n" (string-trim-right body))
                  entry)))
    (orgx-capture-shell--insert-entry
     file headline entry (orgx-capture-shell--resolve-prepend template file))
    (when orgx-capture-shell--status
      (setq orgx-capture-shell--status 'success))
    (format "Captured -> [%s%s] %s"
            (file-name-nondirectory file)
            (if (and headline (not (string-empty-p headline)))
                (format ":%s" headline)
              "")
            entry)))

(defun orgx-capture-shell--undo-last ()
  "Undo the most recently captured entry from `orgx-capture-shell-undo-ring'."
  (interactive)
  (if-let* ((rec (pop orgx-capture-shell-undo-ring)))
      (let* ((file (nth 0 rec))
             (pos (nth 1 rec))
             (text (nth 2 rec))
             (buf (or (get-file-buffer file)
                      (find-file-noselect file t))))
        (with-current-buffer buf
          (save-excursion
            (org-with-wide-buffer
             (goto-char (min pos (point-max)))
             ;; Search forward/backward around pos for text
             (let ((len (+ (length text) 1)))
               (if (search-forward text (+ (point) (* len 2)) t)
                   (delete-region (match-beginning 0) (min (point-max) (+ (match-end 0) 1)))
                 (goto-char (max (point-min) (- pos (* len 2))))
                 (when (search-forward text (+ pos len) t)
                   (delete-region (match-beginning 0) (min (point-max) (+ (match-end 0) 1))))))
             (save-buffer))))
        (format "Undid capture: \"%s\" from %s" (string-trim text) (file-name-nondirectory file)))
    "Undo ring is empty."))

;;; Capture form

(defvar orgx-capture-shell--force-form nil
  "When non-nil, the input sender treats the current input as a form request.")

(defvar orgx-capture-shell--form-origin nil
  "Shell buffer that opened the current transient capture form, if any.")

(defvar orgx-capture-shell--form-prompt-marker nil
  "Marker at the end of the prompt associated with the transient form.")

(defvar orgx-capture-shell--form nil
  "Plist of values (:title :body :template :file :headline) for the transient form.")

(defun orgx-capture-shell--form-defaults ()
  "Return a plist (:file :headline :template) of defaults for a capture form.
Values come from the shell buffer when it exists, else from target resolution."
  (let ((read-defaults
         (lambda ()
           (if orgx-capture-shell-current-file
               (list :file orgx-capture-shell-current-file
                     :headline orgx-capture-shell-current-headline
                     :template orgx-capture-shell-current-template)
             (let ((res (orgx-capture-shell--resolve-target)))
               (list :file (car res)
                     :headline (cdr res)
                     :template orgx-capture-shell-current-template))))))
    (if-let* ((buf (get-buffer orgx-capture-shell-buffer-name)))
        (with-current-buffer buf (funcall read-defaults))
      (funcall read-defaults))))

(defun orgx-capture-shell--read-template (default)
  "Prompt for a template, offering DEFAULT (a symbol)."
  (intern (completing-read "Template: "
                           (mapcar #'symbol-name orgx-capture-shell-templates)
                           nil t nil nil (symbol-name default))))

(defun orgx-capture-shell--read-file (default)
  "Prompt for a target file, offering DEFAULT."
  (expand-file-name
   (read-file-name "Target file: " nil default nil (and default (abbreviate-file-name default)))))

(defun orgx-capture-shell--read-headline (file default)
  "Prompt for a headline path in FILE, offering DEFAULT."
  (completing-read "Headline (path, empty for top): "
                   (orgx-capture-shell--headline-paths file)
                   nil nil default))

(defun orgx-capture-shell--form-read (initial defaults)
  "Prompt for each form field, starting with title INITIAL, using DEFAULTS.
Returns a form plist, or nil when the title is empty."
  (let ((title (string-trim (read-string "Title: " initial))))
    (unless (string-empty-p title)
      (let* ((template (orgx-capture-shell--read-template (plist-get defaults :template)))
             (file (orgx-capture-shell--read-file (plist-get defaults :file)))
             (headline (orgx-capture-shell--read-headline file (plist-get defaults :headline)))
             (body (read-string "Body (optional): ")))
        (list :title title :template template :file file
              :headline headline :body body)))))

(defun orgx-capture-shell--form-commit (form)
  "Capture the values in FORM and return a description string."
  (let ((title (string-trim (or (plist-get form :title) ""))))
    (if (string-empty-p title)
        "Capture cancelled (empty title)."
      (orgx-capture-shell--capture title
                                   :template (plist-get form :template)
                                   :file (plist-get form :file)
                                   :headline (plist-get form :headline)
                                   :body (plist-get form :body)))))

(defun orgx-capture-shell--run-form (&optional initial)
  "Collect a capture form seeded with title INITIAL.
Uses `orgx-capture-shell-form-style'.
Returns a message string."
  (let ((defaults (orgx-capture-shell--form-defaults)))
    (pcase orgx-capture-shell-form-style
      ('transient
       (setq orgx-capture-shell--form-origin
             (and (derived-mode-p 'orgx-capture-shell-mode) (current-buffer)))
       (setq orgx-capture-shell--form (plist-put (copy-sequence defaults) :title (or initial "")))
       (orgx-capture-shell-form-transient)
       "Capture form opened.")
      (_
       (condition-case nil
           (if-let* ((form (orgx-capture-shell--form-read (or initial "") defaults)))
               (orgx-capture-shell--form-commit form)
             "Capture cancelled (empty title).")
         (quit "Capture cancelled."))))))

;;;###autoload
(defun orgx-capture-shell-form (&optional initial)
  "Capture a task via the form, seeded with title INITIAL."
  (interactive)
  (message "%s" (orgx-capture-shell--run-form initial)))

(defun orgx-capture-shell-form-set-title ()
  "Set the title in the transient capture form."
  (interactive)
  (setq orgx-capture-shell--form
        (plist-put orgx-capture-shell--form :title
                   (read-string "Title: " (plist-get orgx-capture-shell--form :title)))))

(defun orgx-capture-shell-form-set-body ()
  "Set the body in the transient capture form."
  (interactive)
  (setq orgx-capture-shell--form
        (plist-put orgx-capture-shell--form :body
                   (read-string "Body: " (plist-get orgx-capture-shell--form :body)))))

(defun orgx-capture-shell-form-set-template ()
  "Set the template in the transient capture form."
  (interactive)
  (setq orgx-capture-shell--form
        (plist-put orgx-capture-shell--form :template
                   (orgx-capture-shell--read-template
                    (plist-get orgx-capture-shell--form :template)))))

(defun orgx-capture-shell-form-set-file ()
  "Set the target file in the transient capture form."
  (interactive)
  (setq orgx-capture-shell--form
        (plist-put orgx-capture-shell--form :file
                   (orgx-capture-shell--read-file
                    (plist-get orgx-capture-shell--form :file)))))

(defun orgx-capture-shell-form-set-headline ()
  "Set the target headline in the transient capture form."
  (interactive)
  (setq orgx-capture-shell--form
        (plist-put orgx-capture-shell--form :headline
                   (orgx-capture-shell--read-headline
                    (plist-get orgx-capture-shell--form :file)
                    (plist-get orgx-capture-shell--form :headline)))))

(defun orgx-capture-shell-form-commit ()
  "Capture the transient form's values and update its shell prompt."
  (interactive)
  (let ((orgx-capture-shell--status 'ready)
        (origin orgx-capture-shell--form-origin))
    (unwind-protect
        (condition-case err
            (message "%s" (orgx-capture-shell--form-commit orgx-capture-shell--form))
          (error
           (setq orgx-capture-shell--status 'error)
           (message "%s" (error-message-string err))))
      (when (and (buffer-live-p origin)
                 (markerp orgx-capture-shell--form-prompt-marker)
                 (eq (marker-buffer orgx-capture-shell--form-prompt-marker) origin))
        (with-current-buffer origin
          (save-excursion
            (goto-char orgx-capture-shell--form-prompt-marker)
            (let ((inhibit-field-text-motion t))
              (beginning-of-line))
            (when (and (looking-at orgx-capture-shell--prompt-regexp)
                       (= (match-end 0) orgx-capture-shell--form-prompt-marker))
              (let ((inhibit-read-only t))
                (delete-region (point) (1+ (point)))
                (insert (substring (orgx-capture-shell--prompt-string
                                    orgx-capture-shell--status) 0 1)))))))
      (setq orgx-capture-shell--form nil)
      (setq orgx-capture-shell--form-origin nil)
      (when (markerp orgx-capture-shell--form-prompt-marker)
        (set-marker orgx-capture-shell--form-prompt-marker nil))
      (setq orgx-capture-shell--form-prompt-marker nil))))

(defun orgx-capture-shell--form-field (key label)
  "Return a description function showing LABEL and the form value at KEY."
  (lambda ()
    (let ((v (plist-get orgx-capture-shell--form key)))
      (format "%s: %s" label
              (propertize (format "%s" (if (member v '(nil "")) "-" v))
                          'face 'transient-value)))))

(transient-define-prefix orgx-capture-shell-form-transient ()
  "Transient capture form."
  ["Capture form"
   ("t" orgx-capture-shell-form-set-title :transient t
    :description (lambda () (funcall (orgx-capture-shell--form-field :title "Title"))))
   ("b" orgx-capture-shell-form-set-body :transient t
    :description (lambda () (funcall (orgx-capture-shell--form-field :body "Body"))))
   ("m" orgx-capture-shell-form-set-template :transient t
    :description (lambda () (funcall (orgx-capture-shell--form-field :template "Template"))))
   ("f" orgx-capture-shell-form-set-file :transient t
    :description (lambda () (funcall (orgx-capture-shell--form-field :file "File"))))
   ("h" orgx-capture-shell-form-set-headline :transient t
    :description (lambda () (funcall (orgx-capture-shell--form-field :headline "Headline"))))]
  ["Finish"
   ("c" "Capture" orgx-capture-shell-form-commit)
   ("q" "Cancel" transient-quit-one)])

;;; Recent captures

(defun orgx-capture-shell--visit-recent (button)
  "Visit the captured entry described by BUTTON."
  (let* ((record (button-get button 'orgx-capture-record))
         (marker (nth 3 record))
         (text (nth 2 record))
         (buffer (find-file-noselect (nth 0 record)))
         (position
          (with-current-buffer buffer
            (save-restriction
              (widen)
              (save-excursion
                (cond
                 ((and (markerp marker) (eq (marker-buffer marker) buffer))
                  (goto-char marker)
                  (unless (looking-at-p (regexp-quote text))
                    (user-error "Capture no longer found in %s" (nth 0 record)))
                  (point))
                 ((progn (goto-char (point-min)) (search-forward text nil t))
                  (match-beginning 0))
                 (t (user-error "Capture no longer found in %s" (nth 0 record)))))))))
    (pop-to-buffer buffer)
    (unless (<= (point-min) position (point-max))
      (widen))
    (goto-char position)
    (when (derived-mode-p 'org-mode)
      (org-fold-show-context))))

(defun orgx-capture-shell--recent-list ()
  "Return the recent captures as clickable buttons."
  (with-temp-buffer
    (seq-do
     (lambda (record)
       (insert "- ")
       (insert-text-button
        (format "[%s] %s" (file-name-nondirectory (nth 0 record))
                (string-trim (nth 2 record)))
        'action #'orgx-capture-shell--visit-recent
        'follow-link t 'orgx-capture-record record)
       (insert "\n"))
     orgx-capture-shell-undo-ring)
    (string-trim-right (buffer-string))))

;;; Slash command dispatcher

(defun orgx-capture-shell--prompt-string (&optional status)
  "Return the target and template prompt for STATUS.
STATUS is `ready', `success', or `error'; it defaults to `ready'."
  (let* ((indicator (pcase status
                      ('success (propertize "✓" 'face 'success))
                      ('error (propertize "✗" 'face 'error))
                      (_ (propertize "●" 'face 'font-lock-keyword-face))))
         (file-name (if orgx-capture-shell-current-file
                        (file-name-nondirectory orgx-capture-shell-current-file)
                      "none"))
         (headline orgx-capture-shell-current-headline))
    (format "%s [%s%s · %s] ❯ " indicator file-name
            (if (and headline (not (string-empty-p headline)))
                (format " : %s" headline)
              "")
            orgx-capture-shell-current-template)))

(defun orgx-capture-shell--execute-slash-command (cmd-line)
  "Execute a slash command CMD-LINE (e.g. \"/target foo.org:Tasks\")."
  (let* ((parts (split-string (string-remove-prefix "/" cmd-line) "[ \t]+" t))
         (verb (downcase (or (car parts) "")))
         (args (string-join (cdr parts) " ")))
    (pcase verb
      ((or "target" "t")
       (if (string-empty-p args)
           (format "Current target: %s [%s]"
                   (or orgx-capture-shell-current-file "none")
                   (or orgx-capture-shell-current-headline "none"))
         (let* ((colon (string-search ":" args))
                (f (if colon (substring args 0 colon) args))
                (h (and colon (substring args (1+ colon)))))
           (setq orgx-capture-shell-current-file (expand-file-name f))
           (when (and h (not (string-empty-p h)))
             (setq orgx-capture-shell-current-headline h))
           (format "Target set to %s [%s]"
                   (file-name-nondirectory orgx-capture-shell-current-file)
                   (or orgx-capture-shell-current-headline "none")))))
      ((or "template" "tpl")
       (if (string-empty-p args)
           (format "Current template: %s" orgx-capture-shell-current-template)
         (let ((sym (intern (downcase args))))
           (if (memq sym orgx-capture-shell-templates)
               (progn
                 (setq orgx-capture-shell-current-template sym)
                 (format "Template switched to: %s" sym))
             (when orgx-capture-shell--status
               (setq orgx-capture-shell--status 'error))
             (format "Unknown template: %s (choose task, note, journal, plain)" args)))))
      ("form"
       (orgx-capture-shell--run-form args))
      ("denote"
       (if (fboundp 'denote-directory-files)
           (let* ((files (denote-directory-files))
                  (choice (completing-read "Select denote note: "
                                           (mapcar #'file-name-nondirectory files)
                                           nil t)))
             (when-let* ((full (seq-find (lambda (f) (string-equal (file-name-nondirectory f) choice)) files)))
               (setq orgx-capture-shell-current-file full)
               (setq orgx-capture-shell-current-headline nil)
               (format "Target set to denote note: %s" choice)))
         (when orgx-capture-shell--status
           (setq orgx-capture-shell--status 'error))
         "Denote is not available."))
      ("undo"
       (unless orgx-capture-shell-undo-ring
         (when orgx-capture-shell--status
           (setq orgx-capture-shell--status 'error)))
       (orgx-capture-shell--undo-last))
      ((or "recent" "list")
       (if orgx-capture-shell-undo-ring
           (orgx-capture-shell--recent-list)
         "No recent captures."))
      ("clear"
       (comint-clear-buffer)
       "")
      ((or "help" "h" "?")
       (mapconcat #'identity
                  '("Available slash commands:"
                    "  /target [file[:headline]]  - Switch capture target file and headline"
                    "  /template [task|note|journal|plain] - Switch capture formatting template"
                    "  /form [title]             - Capture via a form (also: end a line with the form trigger, or C-RET)"
                    "  /denote                   - Pick a denote note as capture target"
                    "  /undo                     - Undo the most recently captured item"
                    "  /recent, /list            - Display recently captured items"
                    "  /clear                    - Clear the shell buffer"
                    "  /help                     - Display this help message")
                  "\n"))
      (_
       (when orgx-capture-shell--status
         (setq orgx-capture-shell--status 'error))
       (format "Unknown command: /%s (type /help for available commands)" verb)))))

;;; Comint input filter and handler

(defun orgx-capture-shell--form-triggered-p (input)
  "Return non-nil when INPUT ends with a single `orgx-capture-shell-form-trigger'.
A doubled trigger is an escape: it captures INPUT with one trigger removed."
  (and orgx-capture-shell-form-trigger
       (not (string-empty-p orgx-capture-shell-form-trigger))
       (string-suffix-p orgx-capture-shell-form-trigger input)
       (not (string-suffix-p (concat orgx-capture-shell-form-trigger
                                     orgx-capture-shell-form-trigger)
                             input))))

(defun orgx-capture-shell--unescape-trigger (input)
  "Drop one trigger from INPUT if it ends with a doubled trigger."
  (let ((trigger orgx-capture-shell-form-trigger))
    (if (and trigger (not (string-empty-p trigger))
             (string-suffix-p (concat trigger trigger) input))
        (string-remove-suffix trigger input)
      input)))

(defun orgx-capture-shell-input-sender (proc input)
  "Send and process INPUT from the comint prompt via PROC.
A semicolon followed by body text separates the title from the body.
Use C-j to insert a literal newline before submitting with RET."
  (let* ((trimmed (string-trim input))
         (force orgx-capture-shell--force-form)
         (orgx-capture-shell--status 'ready)
         (previous-form orgx-capture-shell--form)
         (msg
          (condition-case err
              (cond
               ((string-prefix-p "/" trimmed)
		(orgx-capture-shell--execute-slash-command trimmed))
               ((or force (and (not (string-empty-p trimmed))
                               (orgx-capture-shell--form-triggered-p trimmed)))
		(orgx-capture-shell--run-form
		 (string-trim (string-remove-suffix
                               (or orgx-capture-shell-form-trigger "") trimmed))))
               ((not (string-empty-p trimmed))
		(unless orgx-capture-shell-current-file
		  (let ((res (orgx-capture-shell--resolve-target)))
                    (setq orgx-capture-shell-current-file (car res))
                    (setq orgx-capture-shell-current-headline (cdr res))))
		(orgx-capture-shell--capture
		 (let ((text (orgx-capture-shell--unescape-trigger trimmed)))
		   (if (string-match ";[ \t]*\\([^\n]+\\)" text)
                       (concat (substring text 0 (match-beginning 0)) "\n"
                               (substring text (match-beginning 1)))
                     text)))))
            (error
             (setq orgx-capture-shell--status 'error)
             (error-message-string err)))))
    (when (and msg (not (string-empty-p msg)))
      (comint-output-filter proc (concat msg "\n")))
    (comint-output-filter proc (orgx-capture-shell--prompt-string orgx-capture-shell--status))
    (when (and (eq orgx-capture-shell--form-origin (current-buffer))
               (not (eq previous-form orgx-capture-shell--form)))
      (setq orgx-capture-shell--form-prompt-marker (copy-marker (process-mark proc))))))

(defun orgx-capture-shell-send-form ()
  "Send the current input to the capture form, regardless of the trigger."
  (interactive)
  (let ((orgx-capture-shell--force-form t))
    (comint-send-input)))

;;; Completion at point

(defconst orgx-capture-shell-commands
  '("/target" "/template" "/form" "/denote" "/undo" "/recent" "/list" "/clear" "/help")
  "List of recognized slash commands in orgx-capture-shell.")

(defun orgx-capture-shell--input-start ()
  "Return the buffer position where user input begins on the current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at orgx-capture-shell--prompt-regexp)
        (match-end 0)
      (point))))

(defun orgx-capture-shell-completion-at-point ()
  "Completion at point function for orgx-capture-shell slash commands.
Completes command names, `/template' names, and `/target' files and headlines."
  (let* ((start (orgx-capture-shell--input-start))
         (line (buffer-substring-no-properties start (point))))
    (cond
     ((string-match "\\`/\\S-*\\'" line)
      (list start (point) orgx-capture-shell-commands :exclusive 'no))
     ((string-match "\\`/\\(?:template\\|tpl\\)[ \t]+\\(\\S-*\\)\\'" line)
      (list (+ start (match-beginning 1)) (point)
            (mapcar #'symbol-name orgx-capture-shell-templates)
            :exclusive 'no))
     ((string-match "\\`/\\(?:target\\|t\\)[ \t]+\\(.*\\)\\'" line)
      (let* ((arg (match-string 1 line))
             (arg-start (+ start (match-beginning 1)))
             (colon (string-search ":" arg)))
        (if colon
            (list (+ arg-start colon 1) (point)
                  (orgx-capture-shell--headline-paths
                   (expand-file-name (substring arg 0 colon)))
                  :exclusive 'no)
          (list arg-start (point) #'completion-file-name-table
                :exclusive 'no)))))))

;;; Major Mode Definition

(defvar-keymap orgx-capture-shell-mode-map
  :doc "Keymap for `orgx-capture-shell-mode'."
  :parent comint-mode-map
  "C-c C-u" #'orgx-capture-shell--undo-last
  "C-c C-k" #'comint-kill-input
  "C-j" #'newline
  "C-<return>" #'orgx-capture-shell-send-form)

(define-derived-mode orgx-capture-shell-mode comint-mode "orgx-shell"
  "Major mode for the interactive `orgx-capture-shell' REPL.
\\{orgx-capture-shell-mode-map}"
  (setq comint-prompt-regexp orgx-capture-shell--prompt-regexp)
  (setq comint-use-prompt-regexp t)
  (setq comint-input-sender #'orgx-capture-shell-input-sender)
  (setq-local fill-column 80)
  (add-hook 'completion-at-point-functions #'orgx-capture-shell-completion-at-point nil t)

  ;; Resolve initial target
  (let ((target (orgx-capture-shell--resolve-target)))
    (setq orgx-capture-shell-current-file (car target))
    (setq orgx-capture-shell-current-headline (cdr target))))

;;; Interactive Entrypoints

(defun orgx-capture-shell--ensure-process (&optional buf)
  "Ensure a running dummy comint process exists in BUF."
  (let ((b (or buf (current-buffer))))
    (unless (comint-check-proc b)
      (let ((proc (start-process "orgx-capture-shell" b "cat")))
        (set-process-query-on-exit-flag proc nil)
        proc))))

;;;###autoload
(defun orgx-capture-shell ()
  "Start or switch to the `orgx-capture-shell' REPL."
  (interactive)
  (when orgx-capture-shell-window-rule
    (orgx-capture-shell-register-window-rule))
  (let ((buf (get-buffer-create orgx-capture-shell-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'orgx-capture-shell-mode)
        (orgx-capture-shell-mode))
      (orgx-capture-shell--ensure-process buf)
      (goto-char (point-max))
      (when (= (point-min) (point-max))
        (comint-output-filter
         (get-buffer-process buf)
         (concat "Orgx Capture Shell (type /help for commands)\n"
                 (orgx-capture-shell--prompt-string)))))
    (pop-to-buffer buf)
    buf))

;;;###autoload
(defun orgx-capture-shell-toggle ()
  "Toggle visibility of the `orgx-capture-shell' popup window.
If the shell window is visible and active, hide it and return to the previous
window.  If it is hidden or not active, display it in the bottom popup window."
  (interactive)
  (let* ((buf (get-buffer orgx-capture-shell-buffer-name))
         (win (and buf (get-buffer-window buf))))
    (if (and win (eq win (selected-window)))
        (delete-window win)
      (orgx-capture-shell))))


;;; Interactive Modality Commands & Transient Menu

(defun orgx-capture-shell-set-target-file (file &optional headline)
  "Set the capture target to FILE and optional HEADLINE."
  (interactive "fOrg file: \nsHeadline (optional): ")
  (setq orgx-capture-shell-current-file (expand-file-name file))
  (setq orgx-capture-shell-current-headline (and (not (string-empty-p headline)) headline))
  (message "Target set to %s%s"
           (file-name-nondirectory orgx-capture-shell-current-file)
           (if orgx-capture-shell-current-headline
               (format ":%s" orgx-capture-shell-current-headline)
             "")))

(defun orgx-capture-shell-set-template (template)
  "Set the active capture TEMPLATE."
  (interactive
   (list (intern (completing-read "Template: " (mapcar #'symbol-name orgx-capture-shell-templates) nil t))))
  (setq orgx-capture-shell-current-template template)
  (message "Template set to %s" template))

(defun orgx-capture-shell-set-denote-target ()
  "Prompt for a denote note and set it as capture target."
  (interactive)
  (if (fboundp 'denote-directory-files)
      (let* ((files (denote-directory-files))
             (choice (completing-read "Select denote note: "
                                      (mapcar #'file-name-nondirectory files)
                                      nil t)))
        (when-let* ((full (seq-find (lambda (f) (string-equal (file-name-nondirectory f) choice)) files)))
          (setq orgx-capture-shell-current-file full)
          (setq orgx-capture-shell-current-headline nil)
          (message "Target set to denote note: %s" choice)))
    (user-error "Denote is not available")))

;;;###autoload (autoload 'orgx-capture-shell-transient "orgx-capture-shell" nil t)
(defun orgx-capture-shell-undo-command ()
  "Undo the last capture performed in the shell."
  (interactive)
  (message "%s" (orgx-capture-shell--undo-last)))

(defun orgx-capture-shell-visit-target-command ()
  "Visit the current capture target file in a buffer."
  (interactive)
  (when orgx-capture-shell-current-file
    (find-file orgx-capture-shell-current-file)))

;;;###autoload (autoload 'orgx-capture-shell-transient "orgx-capture-shell" nil t)
(transient-define-prefix orgx-capture-shell-transient ()
  "Transient menu for orgx capture shell operations."
  ["Orgx Capture Shell"
   ["Actions"
    ("s" "Launch / Focus Shell" orgx-capture-shell)
    ("t" "Toggle Popup Window" orgx-capture-shell-toggle)
    ("F" "Capture Form" orgx-capture-shell-form)
    ("u" "Undo Last Capture" orgx-capture-shell-undo-command)
    ("o" "Visit Current Target" orgx-capture-shell-visit-target-command)]
   ["Switch Modality"
    ("f" "Set Target File" orgx-capture-shell-set-target-file)
    ("m" "Set Template" orgx-capture-shell-set-template)
    ("d" "Set Denote Note" orgx-capture-shell-set-denote-target)]])

(provide 'orgx-capture-shell)
;;; orgx-capture-shell.el ends here
