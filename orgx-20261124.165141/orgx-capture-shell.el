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
(require 'pcomplete)
(require 'annotated-completing-read)
(require 'button)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'org)
(require 'transient)

(defvar org-capture-templates)
(defvar org-directory)

(declare-function denote-directory-files "denote")
(declare-function denote-retrieve-filename-title "denote")
(declare-function cape-dabbrev "cape")
(declare-function cape-dict "cape")
(declare-function cape-file "cape")
(declare-function tempel-complete "tempel")
(declare-function flyspell-mode "flyspell")
(declare-function corfu-mode "corfu")

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
  "When non-nil, automatically register `display-buffer' rule for bottom popup."
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

(defcustom orgx-capture-shell-sync-target-on-template t
  "Non-nil means track the target file of the activated capture template.
When non-nil, activating a template with a concrete target file
automatically updates `orgx-capture-shell-current-file' and
`orgx-capture-shell-current-headline'."
  :type 'boolean
  :group 'orgx-capture-shell)

(defconst orgx-capture-shell-templates '(task note journal plain)
  "Recognized capture templates.")

(defconst orgx-capture-shell--prompt-regexp
  (rx bol (in "●✓✗") " [" (*? nonl) "] ❯ ")
  "Regexp matching the shell prompt.")

;;; State variables (buffer-local)

(defvar-local orgx-capture-shell-current-file nil
  "Current target Org file for this shell buffer.")

(defvar-local orgx-capture-shell-current-headline nil
  "Current target headline within `orgx-capture-shell-current-file'.")

(defvar-local orgx-capture-shell-current-template 'task
  "Current active template: a built-in symbol (`task', `note', `journal', `plain')
or an `org-capture-templates' key string (e.g. \"tq\", \"tt\", \"atq\").")

(defcustom orgx-capture-shell-undo-ring-max 50
  "Maximum number of captured items to retain in `orgx-capture-shell-undo-ring'."
  :type 'integer
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-show-welcome nil
  "When non-nil, display welcome banner when initializing `orgx-capture-shell'."
  :type 'boolean
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-hide-slash-commands nil
  "When non-nil, hide slash command completion and disallow execution.
Users can access commands via `orgx-capture-shell-transient'
or `orgx-capture-shell-command-menu'."
  :type 'boolean
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-enable-flyspell t
  "When non-nil, enable `flyspell-mode' in `orgx-capture-shell-mode'."
  :type 'boolean
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-enable-corfu t
  "When non-nil, enable `corfu-mode' in `orgx-capture-shell-mode'."
  :type 'boolean
  :group 'orgx-capture-shell)

(defcustom orgx-capture-shell-enable-abbrev t
  "When non-nil, enable `abbrev-mode' in `orgx-capture-shell-mode'."
  :type 'boolean
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

(defun orgx-capture-shell--set-target (file &optional headline keep-headline)
  "Set target FILE and optional HEADLINE, updating `default-directory'.
If KEEP-HEADLINE is non-nil and HEADLINE is nil, keep the existing headline."
  (when file
    (let* ((exp (expand-file-name file))
           (dir (file-name-directory exp)))
      (setq orgx-capture-shell-current-file exp)
      (unless (and keep-headline (null headline))
        (setq orgx-capture-shell-current-headline
              (and headline (not (string-empty-p (string-trim headline))) headline)))
      (when (and dir (file-directory-p dir))
        (setq default-directory (file-name-as-directory dir))))))

(defvar orgx-capture-shell--buffer nil
  "The current capture shell buffer, or nil if it has been killed.")

(defun orgx-capture-shell--find-buffer ()
  "Return the existing capture shell buffer, if any."
  (when (buffer-live-p orgx-capture-shell--buffer)
    orgx-capture-shell--buffer))

(defun orgx-capture-shell--rename-buffer ()
  "Update the shell buffer name to show its current target and template."
  (when (derived-mode-p 'orgx-capture-shell-mode)
    (rename-buffer
     (format "%s [%s · %s]" orgx-capture-shell-buffer-name
             (if orgx-capture-shell-current-file
                 (file-name-nondirectory orgx-capture-shell-current-file)
               "none")
             orgx-capture-shell-current-template)
     t)))

;;; Popup window management

(declare-function popper-popup-p "popper" (buffer))

(defun orgx-capture-shell--display-below-other (buffer _alist)
  "Display BUFFER below the other window when it shows an agenda or popup.
Only split when the frame has exactly two side-by-side ordinary windows."
  (let* ((windows (window-list nil 'no-minibuffer))
         (selected (selected-window))
         (other (and (= (length windows) 2)
                     (seq-find (lambda (window) (not (eq window selected))) windows))))
    (when (and other
               (/= (car (window-edges other)) (car (window-edges selected)))
               (or (with-current-buffer (window-buffer other)
                     (derived-mode-p 'org-agenda-mode))
                   (and (fboundp 'popper-popup-p)
                        (popper-popup-p (window-buffer other))))
               (let ((split-height-threshold 0))
                 (window-splittable-p other)))
      (let ((window (split-window
                     other (- (min orgx-capture-shell-popup-height
                                   (- (window-total-height other) window-min-height)))
                     'below)))
        (set-window-buffer window buffer)
        window))))

(defun orgx-capture-shell-register-window-rule ()
  "Register the 15-line bottom popup rule in `display-buffer-alist'."
  (interactive)
  (let ((rule `(,(regexp-quote orgx-capture-shell-buffer-name)
                (display-buffer-reuse-window orgx-capture-shell--display-below-other
                 display-buffer-at-bottom)
                (window-height . ,orgx-capture-shell-popup-height)
                (reusable-frames . visible))))
    (setq display-buffer-alist
          (cons rule (seq-remove (lambda (entry) (equal (car entry) (car rule)))
                                 display-buffer-alist)))))


;;; Core capture engine

(defconst orgx-capture-shell--heading-line-regexp "\\`\\(\\*+\\) "
  "Regexp matching an Org heading line, capturing the stars.")

(defun orgx-capture-shell--find-template (key-or-sym)
  "Return the `org-capture-templates' entry matching KEY-OR-SYM, or nil."
  (when (and key-or-sym (boundp 'org-capture-templates))
    (let ((key (if (symbolp key-or-sym) (symbol-name key-or-sym) key-or-sym)))
      (seq-find (lambda (entry)
                  (and (consp entry) (>= (length entry) 5)
                       (string-equal (car entry) key)))
                org-capture-templates))))

(defun orgx-capture-shell--template-keys ()
  "Return a list of all known template key strings and symbols."
  (let ((keys (mapcar #'symbol-name orgx-capture-shell-templates)))
    (when (boundp 'org-capture-templates)
      (dolist (tpl org-capture-templates)
        (when (and (consp tpl) (>= (length tpl) 5) (stringp (car tpl)))
          (push (car tpl) keys))))
    (nreverse (delete-dups keys))))

(defun orgx-capture-shell--available-templates ()
  "Return an alist of available templates for selection.
Each element is of the form: (LABEL VALUE ANNOTATION GROUP)."
  (let* ((prefix-map (and (boundp 'org-capture-templates)
                          (map-into
                           (seq-map (lambda (it) (cons (nth 0 it) (nth 1 it)))
                                    (seq-filter (lambda (it) (<= (length it) 4))
                                                org-capture-templates))
                           '(hash-table :test equal))))
         items)
    (dolist (sym orgx-capture-shell-templates)
      (let ((name (symbol-name sym)))
        (push (list name
                    sym
                    (format "[builtin] Generic %s template" name)
                    "Generic")
              items)))
    (when (boundp 'org-capture-templates)
      (dolist (tpl (seq-filter (lambda (it) (< 4 (length it))) org-capture-templates))
        (let* ((key-char (nth 0 tpl))
               (description (nth 1 tpl))
               (target (nth 3 tpl))
               (target-loc (and (consp target) (cadr target)))
               (target-file (if (stringp target-loc) (file-name-nondirectory target-loc) ""))
               (content (nth 4 tpl))
               (raw (if (stringp content) (string-replace "\n" " " content) ""))
               (preview (if (> (length raw) 32) (concat (substring raw 0 29) "...") raw))
               (group (cond
                       ((not (string-empty-p target-file)) target-file)
                       ((and prefix-map (map-elt prefix-map (substring key-char 0 1))))
                       (t "Other")))
               (label (format "%s (%s)" description key-char)))
          (push (list label
                      key-char
                      (format "[%s] <%s> '%s'" key-char target-file preview)
                      group)
                items))))
    (nreverse items)))

(defun orgx-capture-shell--interpolate-template (template-str text)
  "Interpolate TEXT into Org capture TEMPLATE-STR non-modally."
  (let* ((res template-str)
         (title (string-trim text))
         (title-inserted nil))
    ;; 1. Expand title prompts if present
    (when (string-match-p "%(orgx-capture--read-title)" res)
      (setq res (replace-regexp-in-string "%(orgx-capture--read-title)" (lambda (_) title) res t t))
      (setq title-inserted t))
    (when (string-match-p "%\\^{[^}]*}" res)
      (setq res (replace-regexp-in-string "%\\^{[^}]*}" (lambda (_) title) res t t))
      (setq title-inserted t))
    ;; 2. If title wasn't consumed by explicit prompt, place it at %?
    (if title-inserted
        (setq res (replace-regexp-in-string "%\\?" "" res t t))
      (if (string-match-p "%\\?" res)
          (setq res (replace-regexp-in-string "%\\?" (lambda (_) title) res t t))
        (unless (string-empty-p title)
          (setq res (concat res " " title)))))
    ;; 3. Expand %<FORMAT> timestamps
    (while (string-match "%<\\([^>]+\\)>" res)
      (let ((fmt (match-string 1 res)))
        (setq res (replace-match (format-time-string fmt) t t res))))
    ;; 4. Expand standard timestamp macros
    (setq res (replace-regexp-in-string "%T" (lambda (_) (format-time-string "<%Y-%m-%d %a %H:%M>")) res t t))
    (setq res (replace-regexp-in-string "%t" (lambda (_) (format-time-string "<%Y-%m-%d %a>")) res t t))
    (setq res (replace-regexp-in-string "%U" (lambda (_) (format-time-string "[%Y-%m-%d %a %H:%M]")) res t t))
    (setq res (replace-regexp-in-string "%u" (lambda (_) (format-time-string "[%Y-%m-%d %a]")) res t t))
    ;; 5. Expand link placeholders
    (let ((link (if (bound-and-true-p org-stored-links)
                    (let ((latest (car org-stored-links)))
                      (if (cadr latest)
                          (format "[[%s][%s]]" (car latest) (cadr latest))
                        (format "[[%s]]" (car latest))))
                  "")))
      (setq res (replace-regexp-in-string "%(orgx-capture--get-link)" (lambda (_) link) res t t))
      (setq res (replace-regexp-in-string "%(orgx-capture--prompt-link)" (lambda (_) link) res t t))
      (setq res (replace-regexp-in-string "%a" (lambda (_) link) res t t)))
    ;; 6. Expand clipboard / region / kill-ring placeholders
    (setq res (replace-regexp-in-string "%i" "" res t t))
    (setq res (replace-regexp-in-string "%x" "" res t t))
    (setq res (replace-regexp-in-string "%c" (lambda (_) (or (and (fboundp 'current-kill) (ignore-errors (current-kill 0 t))) "")) res t t))
    ;; 7. Clean up
    (string-trim-right res)))

(defun orgx-capture-shell--format-entry (text template)
  "Format capture TEXT according to TEMPLATE (symbol or key string)."
  (let ((trimmed (string-trim text)))
    (if (string-match-p orgx-capture-shell--heading-line-regexp trimmed)
        trimmed
      (if-let* ((entry (orgx-capture-shell--find-template template)))
          (let ((template-body (nth 4 entry)))
            (cond
             ((stringp template-body)
              (orgx-capture-shell--interpolate-template template-body trimmed))
             ((functionp template-body)
              (funcall template-body))
             (t
              (format "* %s" trimmed))))
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
           (format "* %s" trimmed)))))))

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
    (if-let* ((exact (orgx-capture-shell--find-template template)))
        (let ((plist (nthcdr 5 exact)))
          (if (plist-member plist :prepend)
              (and (plist-get plist :prepend) t)
            t))
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
          (alist-get template orgx-capture-shell--builtin-prepend t))))))

(defun orgx-capture-shell--insertion-point (parent level end &optional prepend)
  "Return where a new child of the section at LEVEL belongs.
PARENT is the position of the section's heading, or nil for the whole
buffer; END is the end of the section.  With PREPEND the position is before
the first child; otherwise at the end, skipping trailing children tagged
`org-archive-tag'."
  (save-excursion
    (let ((child-level (1+ level))
          (start (if parent
                     (progn (goto-char parent) (forward-line 1) (point))
                   (point-min)))
          children)
      (goto-char start)
      (if prepend
          (let ((continue (if (org-at-heading-p) t (outline-next-heading))))
            (if (and continue (< (point) end))
                (point)
              end))
        (let ((continue (if (org-at-heading-p) t (outline-next-heading))))
          (while (and continue (< (point) end))
            (when (= (org-current-level) child-level)
              (push (cons (point)
                          (and (member org-archive-tag (org-get-tags nil t)) t))
                    children))
            (setq continue (outline-next-heading))))
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
        (let ((continue (if (org-at-heading-p) t (outline-next-heading))))
          (while (and (not found) continue (< (point) end))
            (when (and (> (org-current-level) level)
                       (string-equal (org-get-heading t t t t) seg))
              (setq found (point)))
            (setq continue (outline-next-heading))))
        (if found
            (progn
              (goto-char found)
              (setq level (org-current-level))
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
  (org-with-wide-buffer
   (org-map-entries
    (lambda ()
      (mapconcat #'identity (org-get-outline-path t)
                 orgx-capture-shell-headline-separator)))))

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
    (let* ((trimmed (string-trim entry))
           (first-line (car (split-string trimmed "\n" t)))
           (preview (cond
                     ((> (length first-line) 60)
                      (concat (substring first-line 0 57) "..."))
                     ((string-match-p "\n" trimmed)
                      (concat first-line "..."))
                     (t first-line))))
      (format "Captured -> [%s%s] %s"
              (file-name-nondirectory file)
              (if (and headline (not (string-empty-p headline)))
                  (format ":%s" headline)
                "")
              preview))))

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
    (if-let* ((buf (orgx-capture-shell--find-buffer)))
        (with-current-buffer buf (funcall read-defaults))
      (funcall read-defaults))))

(defun orgx-capture-shell--read-template (default)
  "Prompt for a template, offering DEFAULT (a symbol or string)."
  (let* ((available (orgx-capture-shell--available-templates))
         (default-str (cond
                       ((symbolp default) (symbol-name default))
                       ((stringp default) default)
                       (t "task")))
         (choice-table (make-hash-table :test #'equal))
         (candidates nil)
         (group-table (make-hash-table :test #'equal)))
    (dolist (item available)
      (let ((label (nth 0 item))
            (val (nth 1 item))
            (ann (nth 2 item))
            (grp (nth 3 item)))
        (setf (map-elt choice-table label) val)
        (let ((raw (if (symbolp val) (symbol-name val) val)))
          (unless (map-elt choice-table raw)
            (setf (map-elt choice-table raw) val)))
        (push (cons label ann) candidates)
        (setf (map-elt group-table label) grp)))
    (setq candidates (nreverse candidates))
    (let ((chosen
           (if (fboundp 'annotated-completing-read)
               (annotated-completing-read
                candidates
                :prompt "Template: "
                :require-match nil
                :default default-str
                :group-name (lambda (cand) (map-elt group-table cand "")))
             (completing-read (format "Template (default %s): " default-str)
                              candidates
                              nil nil nil nil default-str))))
      (or (map-elt choice-table chosen)
          (map-elt choice-table (string-trim chosen))
          (intern-soft chosen)
          chosen))))

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
       (with-suppressed-warnings ((interactive-only orgx-capture-shell-form-transient))
         (orgx-capture-shell-form-transient))
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
  "Return the recent captures as clickable buttons.
Multi-line entries display only their first line with an ellipsis."
  (with-temp-buffer
    (seq-do
     (lambda (record)
       (let* ((raw-text (string-trim (nth 2 record)))
              (first-line (car (split-string raw-text "\n" t)))
              (display-text (if (string-match-p "\n" raw-text)
                                (format "%s..." first-line)
                              first-line)))
         (insert "- ")
         (insert-text-button
          (format "[%s] %s" (file-name-nondirectory (nth 0 record))
                  display-text)
          'action #'orgx-capture-shell--visit-recent
          'follow-link t 'orgx-capture-record record)
         (insert "\n")))
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
         (headline orgx-capture-shell-current-headline)
         (prompt (format "%s [%s%s · %s] ❯ " indicator file-name
                         (if (and headline (not (string-empty-p headline)))
                             (format " : %s" headline)
                           "")
                         (if (symbolp orgx-capture-shell-current-template)
                             (symbol-name orgx-capture-shell-current-template)
                           orgx-capture-shell-current-template))))
    (propertize prompt
                'read-only t
                'front-sticky '(read-only)
                'rear-nonsticky '(read-only face))))

(defun orgx-capture-shell--execute-slash-command (cmd-line)
  "Execute a slash command CMD-LINE (e.g. \"/target foo.org:Tasks\")."
  (if orgx-capture-shell-hide-slash-commands
      (progn
        (when orgx-capture-shell--status (setq orgx-capture-shell--status 'error))
        "Slash commands are disabled (use transient menu 'C-c m' or command menu 'C-c C-o').")
    (let* ((parts (split-string (string-remove-prefix "/" cmd-line) "[ \t]+" t))
           (verb (downcase (or (car parts) "")))
           (args (string-join (cdr parts) " ")))
      (pcase verb
      ((or "target" "t")
       (if (string-empty-p (string-trim args))
           (format "Current target: %s [%s]"
                   (or orgx-capture-shell-current-file "none")
                   (or orgx-capture-shell-current-headline "none"))
         (let* ((colon (string-search ":" args))
                (f (string-trim (if colon (substring args 0 colon) args)))
                (h (and colon (string-trim (substring args (1+ colon)))))
                (exp (expand-file-name f)))
           (cond
            ((string-empty-p f)
             (when orgx-capture-shell--status (setq orgx-capture-shell--status 'error))
             "Error: Target file cannot be empty")
            ((not (file-exists-p exp))
             (when orgx-capture-shell--status (setq orgx-capture-shell--status 'error))
             (format "Error: Target file does not exist: %s" f))
            ((file-directory-p exp)
             (when orgx-capture-shell--status (setq orgx-capture-shell--status 'error))
             (format "Error: Target cannot be a directory: %s" f))
            ((not (string-suffix-p ".org" exp))
             (when orgx-capture-shell--status (setq orgx-capture-shell--status 'error))
             (format "Error: Target file must be an Org file (.org): %s" f))
            (t
             (orgx-capture-shell--set-target exp (and h (not (string-empty-p h)) h) t)
             (format "Target set to %s [%s]"
                     (file-name-nondirectory orgx-capture-shell-current-file)
                     (or orgx-capture-shell-current-headline "none")))))))
      ((or "template" "tpl")
       (let ((trimmed (string-trim args)))
         (if (string-empty-p trimmed)
             (format "Current template: %s" orgx-capture-shell-current-template)
           (condition-case nil
               (progn
                 (orgx-capture-shell-set-template trimmed)
                 (format "Template switched to: %s" orgx-capture-shell-current-template))
             (user-error
              (when orgx-capture-shell--status
                (setq orgx-capture-shell--status 'error))
              (format "Unknown template: %s (choose %s)"
                      trimmed
                      (string-join (orgx-capture-shell--template-keys) ", ")))))))
      ("form"
       (orgx-capture-shell--run-form args))
      ("denote"
       (if (fboundp 'denote-directory-files)
           (let* ((files (denote-directory-files))
                  (choice (completing-read "Select denote note: "
                                           (mapcar #'file-name-nondirectory files)
                                           nil t)))
             (when-let* ((full (seq-find (lambda (f) (string-equal (file-name-nondirectory f) choice)) files)))
               (orgx-capture-shell--set-target full nil)
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
       (format "Unknown command: /%s (type /help for available commands)" verb))))))

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
Use \\`C-j' to insert a literal newline before submitting with RET."
  (let* ((trimmed (string-trim input))
         (force orgx-capture-shell--force-form)
         (orgx-capture-shell--status 'ready)
         (previous-form orgx-capture-shell--form)
         (msg
          (condition-case err
              (cond
               ((string-prefix-p "/" trimmed)
                (if orgx-capture-shell-hide-slash-commands
                    (progn
                      (when orgx-capture-shell--status
                        (setq orgx-capture-shell--status 'error))
                      "Slash commands are disabled (use transient menu 'C-c m' or command menu 'C-c C-o').")
                  (orgx-capture-shell--execute-slash-command trimmed)))
               ((or force (and (not (string-empty-p trimmed))
                               (orgx-capture-shell--form-triggered-p trimmed)))
		(orgx-capture-shell--run-form
		 (string-trim (string-remove-suffix
                               (or orgx-capture-shell-form-trigger "") trimmed))))
               ((not (string-empty-p trimmed))
		(unless orgx-capture-shell-current-file
		  (let ((res (orgx-capture-shell--resolve-target)))
                    (orgx-capture-shell--set-target (car res) (cdr res))))
		(orgx-capture-shell--capture
		 (let ((text (orgx-capture-shell--unescape-trigger trimmed)))
		   (if (string-match ";[ \t]*\\([^\n]+\\)" text)
                       (concat (substring text 0 (match-beginning 0)) "\n"
                               (substring text (match-beginning 1)))
                     text)))))
            (error
             (setq orgx-capture-shell--status 'error)
             (error-message-string err)))))
    (orgx-capture-shell--rename-buffer)
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
  "List of recognized slash commands in `orgx-capture-shell'.")

(defun orgx-capture-shell--input-start ()
  "Return the buffer position where user input begins on the current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at orgx-capture-shell--prompt-regexp)
        (match-end 0)
      (point))))

(defun orgx-capture-shell-completion-at-point ()
  "Completion at point function for `orgx-capture-shell' slash commands.
Completes command names, `/template' names, and `/target' files and headlines."
  (unless orgx-capture-shell-hide-slash-commands
    (let* ((start (orgx-capture-shell--input-start))
           (line (buffer-substring-no-properties start (point))))
      (cond
       ((string-match "\\`/\\S-*\\'" line)
        (list start (point) orgx-capture-shell-commands :exclusive 'no))
       ((string-match "\\`/\\(?:template\\|tpl\\)[ \t]+\\(\\S-*\\)\\'" line)
        (list (+ start (match-beginning 1)) (point)
              (orgx-capture-shell--template-keys)
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
                  :exclusive 'no))))))))

(defvar-local orgx-capture-shell--completion-buffer nil
  "Private Org buffer used to provide Org completions.")

(defvar-local orgx-capture-shell--completion-context-cache nil
  "Cached target file context for Org completion.")

(defun orgx-capture-shell--completion-context (file source)
  "Return Org completion variables for FILE, optionally visited in SOURCE."
  (let* ((attributes (and file (not source) (file-attributes file)))
         (key (list file source (and source (buffer-chars-modified-tick source))
                    (and attributes (file-attribute-modification-time attributes))
                    (and attributes (file-attribute-size attributes)))))
    (unless (equal key (car orgx-capture-shell--completion-context-cache))
      (let ((vars '(org-todo-keywords-1 org-todo-regexp org-current-tag-alist
                    org-link-abbrev-alist-local org-file-tags)))
        (setq orgx-capture-shell--completion-context-cache
              (cons key
                    (if (and source (with-current-buffer source (derived-mode-p 'org-mode)))
                        (seq-map (lambda (var) (cons var (buffer-local-value var source))) vars)
                      (with-temp-buffer
                        (when file
                          (setq default-directory (file-name-directory file))
                          (if source
                              (insert-buffer-substring source)
                            (when (file-readable-p file) (insert-file-contents file))))
                        (delay-mode-hooks (org-mode))
                        (seq-map (lambda (var) (cons var (symbol-value var))) vars)))))))
    (cdr orgx-capture-shell--completion-context-cache)))

(defun orgx-capture-shell--kill-completion-buffer ()
  "Kill the private completion buffer along with its shell."
  (when (buffer-live-p orgx-capture-shell--completion-buffer)
    (kill-buffer orgx-capture-shell--completion-buffer)))

(defun orgx-capture-shell-org-completion-at-point ()
  "Complete Org syntax in the current input using Org's completion table."
  (when-let* ((process (get-buffer-process (current-buffer)))
              (start (marker-position (process-mark process)))
              ((>= (point) start)))
    (let ((input (buffer-substring-no-properties start (point-max)))
          (offset (- (point) start))
          (target orgx-capture-shell-current-file)
          (directory default-directory)
          (source (and orgx-capture-shell-current-file
                       (get-file-buffer orgx-capture-shell-current-file))))
      (unless (buffer-live-p orgx-capture-shell--completion-buffer)
        (setq orgx-capture-shell--completion-buffer
              (generate-new-buffer " *orgx-capture-completion*"))
        (with-current-buffer orgx-capture-shell--completion-buffer
          (delay-mode-hooks (org-mode))))
      (let ((shadow orgx-capture-shell--completion-buffer)
            (context (orgx-capture-shell--completion-context target source)))
        (with-current-buffer shadow
          (setq-local default-directory
                      (if target (file-name-directory target) directory))
          (seq-do (lambda (entry)
                    (set (make-local-variable (car entry)) (cdr entry)))
                  context)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert input))
          (goto-char (+ (point-min) offset))
          (when-let* ((completion
                       (or (pcomplete-completions-at-point)
                           (save-excursion
                             (when (re-search-backward "\\[\\[file:\\([^]]*\\)\\="
                                                       (line-beginning-position) t)
                               (list (match-beginning 1) (match-end 1)
                                     #'completion-file-name-table))))))
            (let ((table (nth 2 completion))
                  (props (copy-sequence (nthcdr 3 completion)))
                  (generation (buffer-chars-modified-tick))
                  (termination pcomplete-termination-string))
              (seq-do
               (lambda (property)
                 (when-let* ((fn (plist-get props property)))
                   (setq props
                         (plist-put props property
                                    (lambda (&rest args)
                                      (with-current-buffer shadow
                                        (apply fn args)))))))
               '(:predicate :annotation-function :company-docsig :company-kind))
              (when-let* ((exit (plist-get props :exit-function)))
                (setq props
                      (plist-put props :exit-function
                                 (lambda (string status)
                                   (let ((pcomplete-termination-string termination))
                                     (funcall exit string status))))))
              (setq props (plist-put props :exclusive 'no))
              (append
               (list (+ start (1- (nth 0 completion)))
                     (+ start (1- (nth 1 completion)))
                     (lambda (string predicate action)
                       (when (and (buffer-live-p shadow)
                                  (= generation (buffer-chars-modified-tick shadow)))
                         (with-current-buffer shadow
                           (complete-with-action action table string predicate)))))
               props))))))))

;;; Major Mode Definition

(declare-function annotated-completing-read "annotated-completing-read")

(defun orgx-capture-shell-command-menu ()
  "Prompt for an action using `annotated-completing-read' or transient."
  (interactive)
  (let* ((actions
          '(("Transient Menu"  . ("Open full interactive transient menu" . transient))
            ("Set Target File" . ("Select target Org file and headline" . target))
            ("Set Template"    . ("Select capture template type" . template))
            ("Set Denote"      . ("Pick a denote note as capture target" . denote))
            ("Capture Form"    . ("Open multi-field capture form" . form))
            ("Undo Capture"    . ("Undo last capture" . undo))
            ("Recent Captures" . ("Display recent captures" . recent))
            ("Clear Buffer"    . ("Clear shell buffer" . clear))))
         (table (mapcar (lambda (a) (cons (car a) (cadr a))) actions))
         (choice (if (fboundp 'annotated-completing-read)
                     (annotated-completing-read table :prompt "Shell command: " :require-match t)
                   (completing-read "Shell command: " (mapcar #'car actions) nil t)))
         (action (when choice (cddr (assoc choice actions)))))
    (pcase action
      ('transient
       (with-suppressed-warnings ((interactive-only orgx-capture-shell-transient))
         (orgx-capture-shell-transient)))
      ('target (call-interactively #'orgx-capture-shell-set-target-file))
      ('template (call-interactively #'orgx-capture-shell-set-template))
      ('denote (call-interactively #'orgx-capture-shell-set-denote-target))
      ('form (call-interactively #'orgx-capture-shell-form))
      ('undo (call-interactively #'orgx-capture-shell-undo-command))
      ('recent (message "%s" (orgx-capture-shell--recent-list)))
      ('clear (let ((inhibit-read-only t))
                (erase-buffer)
                (comint-output-filter (get-buffer-process (current-buffer))
                                      (orgx-capture-shell--prompt-string)))))))

(defun orgx-capture-shell-slash ()
  "Self-insert `/' or open command menu if slash commands are hidden.
Only triggers command menu when point is at prompt start."
  (interactive)
  (if (and orgx-capture-shell-hide-slash-commands
           (= (point) (orgx-capture-shell--input-start)))
      (orgx-capture-shell-command-menu)
    (self-insert-command 1 ?/)))

(defvar-keymap orgx-capture-shell-mode-map
  :doc "Keymap for `orgx-capture-shell-mode'."
  :parent comint-mode-map
  "C-c C-u" #'orgx-capture-shell--undo-last
  "C-c C-k" #'comint-kill-input
  "C-c C-c" #'comint-send-input
  "C-c C-o" #'orgx-capture-shell-command-menu
  "C-c C-r" #'orgx-capture-shell-restart
  "C-c m"   #'orgx-capture-shell-transient
  "C-j" #'newline
  "C-<return>" #'orgx-capture-shell-send-form
  "/" #'orgx-capture-shell-slash)

(defun orgx-capture-shell--flyspell-verify ()
  "Verification predicate for `flyspell-mode' in `orgx-capture-shell-mode'.
Returns non-nil if the word at point should be spellchecked.
Ignores prompts, slash commands, and short syntax."
  (save-excursion
    (let* ((proc (get-buffer-process (current-buffer)))
           (pmark (and proc (process-mark proc)))
           (word-beg (progn (backward-word 1) (point))))
      (and
       (or (not pmark) (>= word-beg (marker-position pmark)))
       (not (eq (char-before word-beg) ?/))
       (not (looking-at-p "/[a-zA-Z0-9_-]+"))
       (save-excursion
         (skip-chars-backward "^ \t\n\r")
         (not (eq (char-after (point)) ?/)))))))

(put 'orgx-capture-shell-mode 'flyspell-mode-predicate #'orgx-capture-shell--flyspell-verify)

(define-derived-mode orgx-capture-shell-mode comint-mode "orgx-shell"
  "Major mode for the interactive `orgx-capture-shell' REPL.
\\{orgx-capture-shell-mode-map}"
  (setq comint-prompt-regexp orgx-capture-shell--prompt-regexp)
  (setq comint-use-prompt-regexp t)
  (setq-local comint-prompt-read-only t)
  (setq comint-input-sender #'orgx-capture-shell-input-sender)
  (setq-local fill-column 80)
  (visual-line-mode 1)
  (add-hook 'completion-at-point-functions #'orgx-capture-shell-org-completion-at-point nil t)
  (add-hook 'completion-at-point-functions #'orgx-capture-shell-completion-at-point nil t)
  (when (fboundp 'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-dabbrev 80 t))
  (let ((dict-fn (or (and (fboundp 'cape-dict) #'cape-dict)
                     (and (fboundp 'cape-dictionary) #'cape-dictionary))))
    (when dict-fn
      (add-hook 'completion-at-point-functions dict-fn 85 t)))
  (when (fboundp 'tempel-complete)
    (add-hook 'completion-at-point-functions #'tempel-complete 90 t))
  (when (fboundp 'cape-file)
    (add-hook 'completion-at-point-functions #'cape-file 95 t))
  (add-hook 'kill-buffer-hook #'orgx-capture-shell--kill-completion-buffer nil t)
  (add-hook 'change-major-mode-hook #'orgx-capture-shell--kill-completion-buffer nil t)

  ;; Autocomplete, spellcheck, and abbrevs
  (when (and orgx-capture-shell-enable-abbrev (fboundp 'abbrev-mode))
    (abbrev-mode 1))
  (when (and orgx-capture-shell-enable-corfu (fboundp 'corfu-mode))
    (setq-local corfu-auto t)
    (setq-local corfu-auto-prefix 1)
    (corfu-mode 1))
  (when (and orgx-capture-shell-enable-flyspell (fboundp 'flyspell-mode))
    (setq-local flyspell-generic-check-word-predicate
                #'orgx-capture-shell--flyspell-verify)
    (flyspell-mode 1))

  ;; Resolve initial target
  (let ((target (orgx-capture-shell--resolve-target)))
    (orgx-capture-shell--set-target (car target) (cdr target))))

;;; Interactive Entrypoints

(defvar-local orgx-capture-shell--saved-input-marker nil
  "Input boundary retained when the shell helper exits.")

(defun orgx-capture-shell--process-sentinel (process _event)
  "Save PROCESS input boundary when the helper exits."
  (when (and (not (process-live-p process)) (buffer-live-p (process-buffer process)))
    (with-current-buffer (process-buffer process)
      (setq orgx-capture-shell--saved-input-marker
            (copy-marker (process-mark process))))))

(defun orgx-capture-shell--ensure-process (&optional buf)
  "Ensure a running dummy comint process exists in BUF."
  (let ((b (or buf (current-buffer))))
    (unless (comint-check-proc b)
      (let ((proc (start-process "orgx-capture-shell" b "cat")))
        (set-process-query-on-exit-flag proc nil)
        (set-process-sentinel proc #'orgx-capture-shell--process-sentinel)
        proc))))

(defun orgx-capture-shell-restart ()
  "Restart the shell helper, preserving the current input."
  (interactive)
  (unless (derived-mode-p 'orgx-capture-shell-mode)
    (user-error "Not in an orgx capture shell"))
  (let* ((old-process (get-buffer-process (current-buffer)))
         (input-start (or (and old-process (marker-position (process-mark old-process)))
                          (and (markerp orgx-capture-shell--saved-input-marker)
                               (marker-position orgx-capture-shell--saved-input-marker)))))
    (when old-process
      (delete-process old-process))
    (let ((process (orgx-capture-shell--ensure-process)))
      (if (and input-start (> (point-max) (point-min)))
          (set-marker (process-mark process) input-start)
        (set-marker (process-mark process) (point-max))
        (comint-output-filter process (orgx-capture-shell--prompt-string))))))

;;;###autoload
(defun orgx-capture-shell ()
  "Start or switch to the `orgx-capture-shell' REPL."
  (interactive)
  (when orgx-capture-shell-window-rule
    (orgx-capture-shell-register-window-rule))
  (let ((buf (or (orgx-capture-shell--find-buffer)
                 (get-buffer-create orgx-capture-shell-buffer-name))))
    (setq orgx-capture-shell--buffer buf)
    (with-current-buffer buf
      (unless (derived-mode-p 'orgx-capture-shell-mode)
        (orgx-capture-shell-mode))
      (if (and (not (comint-check-proc buf))
               orgx-capture-shell--saved-input-marker)
          (orgx-capture-shell-restart)
        (orgx-capture-shell--ensure-process buf))
      (orgx-capture-shell--rename-buffer)
      (goto-char (point-max))
      (when (= (point-min) (point-max))
        (comint-output-filter
         (get-buffer-process buf)
         (concat (when orgx-capture-shell-show-welcome
                   "Orgx Capture Shell (type /help for commands)\n")
                 (orgx-capture-shell--prompt-string)))))
    (pop-to-buffer buf)
    buf))

;;;###autoload
(defun orgx-capture-shell-toggle ()
  "Toggle visibility of the `orgx-capture-shell' popup window.
If the shell window is visible and active, hide it and return to the previous
window.  If it is hidden or not active, display it in the bottom popup window."
  (interactive)
  (let* ((buf (orgx-capture-shell--find-buffer))
         (win (and buf (get-buffer-window buf))))
    (if (and win (eq win (selected-window)))
        (delete-window win)
      (orgx-capture-shell))))


;;; Interactive Modality Commands & Transient Menu

(defun orgx-capture-shell-set-target-file (file &optional headline)
  "Set the capture target to FILE and optional HEADLINE."
  (interactive
   (let* ((files (delete-dups
                  (delq nil
                        (cons orgx-capture-shell-current-file
                              (seq-map (lambda (buffer)
                                         (with-current-buffer buffer
                                           (when (and buffer-file-name
                                                      (derived-mode-p 'org-mode))
                                             buffer-file-name)))
                                       (buffer-list))))))
          (choice (annotated-completing-read
                   (cons '("Browse..." . "Choose another file")
                         (seq-map (lambda (file) (cons file "Open Org file")) files))
                   :prompt "Org file: " :require-match t))
          (f (if (equal choice "Browse...")
                 (read-file-name "Org file: " nil nil t nil
                                 (lambda (name)
                                   (or (file-directory-p name)
                                       (string-suffix-p ".org" name))))
               choice))
          (h (completing-read "Headline (optional): "
                              (when (and f (file-regular-p f))
                                (orgx-capture-shell--headline-paths f))
                              nil nil)))
     (list f h)))
  (unless (and file (stringp file) (not (string-empty-p (string-trim file))))
    (user-error "Target file must be specified"))
  (let ((exp (expand-file-name file)))
    (unless (file-exists-p exp)
      (user-error "Target file does not exist: %s" exp))
    (when (file-directory-p exp)
      (user-error "Target file cannot be a directory: %s" exp))
    (unless (string-suffix-p ".org" exp)
      (user-error "Target file must be an Org file (.org): %s" exp))
    (orgx-capture-shell--set-target exp headline)
    (orgx-capture-shell--rename-buffer)
    (message "Target set to %s%s"
             (file-name-nondirectory orgx-capture-shell-current-file)
             (if orgx-capture-shell-current-headline
                 (format ":%s" orgx-capture-shell-current-headline)
               ""))))

(defun orgx-capture-shell-set-template (template)
  "Set the active capture TEMPLATE (symbol or key string).
When TEMPLATE specifies a target file in `org-capture-templates' and
`orgx-capture-shell-sync-target-on-template' is non-nil, update
`orgx-capture-shell-current-file' and `orgx-capture-shell-current-headline'."
  (interactive
   (list (orgx-capture-shell--read-template orgx-capture-shell-current-template)))
  (let* ((str (if (symbolp template) (symbol-name template) (string-trim template)))
         (sym (intern-soft str))
         (canonical (cond
                     ((memq sym orgx-capture-shell-templates) sym)
                     ((orgx-capture-shell--find-template str) str)
                     ((orgx-capture-shell--find-template sym) (symbol-name sym))
                     (t (user-error "Invalid template: %s (choose from %s)"
                                    template
                                    (string-join (orgx-capture-shell--template-keys) ", "))))))
    (setq orgx-capture-shell-current-template canonical)
    (when orgx-capture-shell-sync-target-on-template
      (when-let* ((entry (orgx-capture-shell--find-template canonical))
                  (target (nth 3 entry)))
        (when (consp target)
          (let* ((type (car target))
                 (raw-file (cadr target))
                 (file (and (stringp raw-file)
                            (expand-file-name raw-file (or (bound-and-true-p org-directory)
                                                           default-directory))))
                 (headline (pcase type
                             ('file+headline (nth 2 target))
                             ('file+olp (string-join (nthcdr 2 target)
                                                    orgx-capture-shell-headline-separator))
                             ('file+olp+datetree (nth 2 target))
                             (_ nil))))
            (when file
              (orgx-capture-shell--set-target file headline t))))))
    (orgx-capture-shell--rename-buffer)
    (message "Template set to %s%s"
             canonical
             (if orgx-capture-shell-current-file
                 (format " [target: %s%s]"
                         (file-name-nondirectory orgx-capture-shell-current-file)
                         (if orgx-capture-shell-current-headline
                             (format ":%s" orgx-capture-shell-current-headline)
                           ""))
               ""))))

(defun orgx-capture-shell-set-denote-target ()
  "Prompt for a denote note and set it as capture target."
  (interactive)
  (if (fboundp 'denote-directory-files)
      (let* ((files (denote-directory-files))
             (choice (completing-read "Select denote note: "
                                      (mapcar #'file-name-nondirectory files)
                                      nil t)))
        (when-let* ((full (seq-find (lambda (f) (string-equal (file-name-nondirectory f) choice)) files)))
          (orgx-capture-shell--set-target full nil)
          (orgx-capture-shell--rename-buffer)
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

;;;###autoload
(defun orgx-capture-shell-execute-command ()
  "Run `execute-extended-command' filtered to `orgx-capture-shell' commands."
  (interactive)
  (let ((read-extended-command-predicate
         (lambda (sym _)
           (string-prefix-p "orgx-capture-shell" (symbol-name sym)))))
    (with-suppressed-warnings ((interactive-only execute-extended-command))
      (execute-extended-command nil))))

;;;###autoload (autoload 'orgx-capture-shell-transient "orgx-capture-shell" nil t)
(transient-define-prefix orgx-capture-shell-transient ()
  "Transient menu for orgx capture shell operations."
  ["Orgx Capture Shell"
   ["Actions"
    ("s" "Launch / Focus Shell" orgx-capture-shell)
    ("t" "Toggle Popup Window" orgx-capture-shell-toggle)
    ("F" "Capture Form" orgx-capture-shell-form)
    ("u" "Undo Last Capture" orgx-capture-shell-undo-command)
    ("r" "Restart Helper" orgx-capture-shell-restart)
    ("o" "Visit Current Target" orgx-capture-shell-visit-target-command)
    ("x" "Execute Shell Command" orgx-capture-shell-execute-command)]
   ["Switch Modality"
    ("f" "Set Target File" orgx-capture-shell-set-target-file)
    ("m" "Set Template" orgx-capture-shell-set-template)
    ("d" "Set Denote Note" orgx-capture-shell-set-denote-target)]])

(provide 'orgx-capture-shell)
;;; orgx-capture-shell.el ends here
