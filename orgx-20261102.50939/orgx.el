;;; orgx.el --- Personal org-mode extensions and navigation -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, convenience

;;; Commentary:
;; Core Org mode extensions:
;; - Date formatting and timestamp utilities
;; - Agenda file management, reload, and scoping commands
;; - Custom speed commands and headline navigation
;; - Archive management and completed task refiling
;; - Minor mode (`orgx-mode') with unified keymap under `C-c o'
;; - Global keymap (`orgx-global-map')

;;; Code:

(eval-when-compile
  (require 'org-macs)
  (require 'org-element))

(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'outline)
(require 'org)
(require 'org-fold)
(require 'org-capture)
(require 'org-refile)
(require 'org-agenda)
(require 'org-archive)
(require 'ol)

(declare-function org-cycle-force-archived "org-cycle")
(declare-function whitespace-cleanup "whitespace")
(declare-function org-babel-execute:go "ob-go")
(declare-function org-babel-execute:emacs-lisp "ob-emacs-lisp")
(declare-function org-babel-execute:js "ob-js")
(declare-function org-annotate-file "org-annotate-file")
(declare-function org-id-store-link "org-id")
(declare-function consult-org-heading "consult")
(declare-function agent-shell-queue-org-refile-from-heading "agent-shell-queue-org")
(declare-function orgx-denote-setup-agenda-commands "orgx-denote")

(declare-function orgx-capture "orgx-capture")
(declare-function orgx-capture-reset-templates "orgx-capture")
(declare-function orgx-capture-add-project-file-templates "orgx-capture")
(declare-function orgx-capture-setup-standard-templates "orgx-capture")
(declare-function orgx-capture-shell "orgx-capture-shell")
(declare-function orgx-capture-shell-toggle "orgx-capture-shell")
(declare-function orgx-capture-shell-transient "orgx-capture-shell")
(declare-function orgx-denote-migrate-subtree "orgx-denote")
(declare-function orgx-denote-agenda-todos "orgx-denote")
(declare-function orgx-denote-agenda-questions "orgx-denote")
(declare-function orgx-denote-agenda-composite "orgx-denote")
(declare-function orgx-denote-agenda-completed-plans "orgx-denote")

(defvar orgx-agenda-minor-mode)
(defvar org-state)
(defvar org-last-state)

(defgroup orgx nil
  "Org-mode personal extensions."
  :group 'org)

(defcustom orgx-project-tags '("project" "task" "epic" "plan" "scope" "tracker")
  "Tags that identify projects for skip functions."
  :type '(repeat string)
  :group 'orgx)

;;; Date formatting

(defconst orgx-date-spec-datetime "<%Y-%02m-%02d %02H:%02M:%02S %Z>")
(defconst orgx-date-spec-date "<%Y-%02m-%02d>")

(cl-defun orgx-date-now (&key short)
  "Format current time into an Org active timestamp.
When SHORT is non-nil, format as date-only."
  (interactive)
  (format-time-string (if short
                          orgx-date-spec-date
                        orgx-date-spec-datetime)
                      (current-time)))

;;; TOC priority preservation (for toc-org)

(defvar orgx--toc-priorities nil)

(defun orgx--toc-collect-priorities ()
  "Collect priority markers from all headings in the current buffer."
  (let (alist)
    (save-excursion
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (when-let* ((priority (nth 3 (org-heading-components)))
                     (title (org-get-heading t t t t)))
           (push (cons title priority) alist)))))
    (nreverse alist)))

(defun orgx--toc-bind-priorities (fn &rest args)
  "Around advice for `toc-org-insert-toc': collect buffer priorities."
  (let ((orgx--toc-priorities (orgx--toc-collect-priorities)))
    (apply fn args)))

(defun orgx--toc-hrefify-priority (fn str &optional hash)
  "Around advice for `toc-org-hrefify-gh': prepend heading priority."
  (if-let* ((cell (assoc str orgx--toc-priorities)))
      (progn
        (setq orgx--toc-priorities (delq cell orgx--toc-priorities))
        (funcall fn (format "[#%c] %s" (cdr cell) str) hash))
    (funcall fn str hash)))


(declare-function revbufs "revbufs")

(defun orgx--background-revbufs ()
  "Run `revbufs' without disturbing current window layout."
  (when (fboundp 'revbufs)
    (save-window-excursion (revbufs))))

(defun orgx--use-speed-commands ()
  "Speed command predicate: cursor at beginning of outline heading."
  (and (looking-at org-outline-regexp) (looking-back "^\\**" nil)))

;;; Agenda file management

(defvar local-notes-directory nil
  "Directory where notes (org, roam, deft, etc.) are stored.")

(defvar user-org-directories nil
  "Additional directories that may contain org files.")

(defvar deft-directory nil
  "Directory for deft notes.")

(defvar denote-directory nil
  "Directory for denote notes.")

(defvar org-annotate-file-storage-file nil
  "File for org-annotate-file storage.")

;;;###autoload
(defun orgx-set-notes-directory (&optional path)
  "Set the notes directory to PATH and update Org paths.
If PATH is nil, use `local-notes-directory'.
Updates `local-notes-directory', `org-directory', `org-agenda-files',
`org-annotate-file-storage-file', `deft-directory', and `denote-directory'."
  (interactive
   (list (read-directory-name "Notes directory: " (or local-notes-directory "~/notes"))))
  (when path
    (setq local-notes-directory (expand-file-name path)))

  (unless local-notes-directory
    (error "Must define `local-notes-directory'"))

  (setq org-directory (file-name-concat local-notes-directory "org"))
  (setq org-agenda-files (thread-last (list org-directory user-org-directories)
                                      (flatten-tree)
                                      (seq-map #'expand-file-name)
                                      (seq-filter #'identity)
                                      (seq-map #'string-trim)
                                      (seq-remove #'string-empty-p)
                                      (seq-uniq)))
  (setq org-annotate-file-storage-file (file-name-concat org-directory "records.org"))
  (setq org-default-notes-file (file-name-concat org-directory "records.org"))
  (setq org-archive-location (file-name-concat org-directory "archive/%s::datetree/"))
  (setq deft-directory (file-name-concat local-notes-directory "deft"))
  (setq denote-directory (file-name-concat local-notes-directory "denote"))
  local-notes-directory)

;;;###autoload
(defalias 'bootstrap-set-notes-directory #'orgx-set-notes-directory)


;;;###autoload
(defun orgx-agenda-files-open ()
  "Open all files listed in `org-agenda-files'."
  (interactive)
  (when (boundp 'org-agenda-files)
    (seq-do (lambda (f)
              (when (file-exists-p f)
                (find-file-noselect f)))
            (org-agenda-files))))

;;;###autoload
(defun orgx-agenda-files-reload ()
  "Revert all buffers visiting `org-agenda-files' to match disk."
  (interactive)
  (when (boundp 'org-agenda-files)
    (seq-do (lambda (f)
              (when-let* ((buf (get-file-buffer f)))
                (with-current-buffer buf
                  (revert-buffer :ignore-auto :noconfirm))))
            (org-agenda-files))))

(defun orgx--ad-org-agenda-redo (orig-fun &rest args)
  "Advice to background-revert buffers before re-running agenda view."
  (orgx--background-revbufs)
  (apply orig-fun args))


;;; Task status and archiving

;;;###autoload
(defun orgx-mark-done-and-archive ()
  "Mark current heading DONE and execute `org-archive-default-command'."
  (interactive)
  (org-todo "DONE")
  (call-interactively org-archive-default-command))

(defun orgx-set-weekday-of-timestamp ()
  "Add day of the week to an active timestamp at point."
  (interactive)
  (let ((time (org-time-string-to-time (thing-at-point 'line t))))
    (when time
      (delete-region (line-beginning-position) (line-end-position))
      (insert (format-time-string "<%Y-%m-%d %a>" time)))))

(defun orgx-skip-child-of-project-tag ()
  "Skip children of headings tagged with any element of `orgx-project-tags'."
  (save-restriction
    (widen)
    (let (skip)
      (while (and (not skip) (org-up-heading-safe))
        (when (seq-intersection orgx-project-tags (org-get-tags nil t))
          (setq skip (save-excursion (org-end-of-subtree t) (point)))))
      skip)))

(defun orgx-done-state-match ()
  "Return an `org-map-entries' match string for all completed todo states."
  (let ((done-states (or (and (boundp 'org-done-keywords) org-done-keywords)
                         '("DONE"))))
    (concat "/" (string-join done-states "|"))))

(defun orgx-archive-completed-tasks (archive-fn label)
  "Archive completed tasks using ARCHIVE-FN, logging progress with LABEL."
  (let ((count 0)
        (matcher (orgx-done-state-match)))
    (org-map-entries
     (lambda ()
       (setq org-map-continue-from (point))
       (funcall archive-fn)
       (setq count (1+ count)))
     matcher
     nil)
    (message "Archived %d tasks to %s." count label)))

;;;###autoload
(defun orgx-archive-done-tasks-to-archive-sibling ()
  "Archive all DONE tasks in the current buffer to their archive sibling."
  (interactive)
  (orgx-archive-completed-tasks #'org-archive-to-archive-sibling "archive sibling"))

;;;###autoload
(defun orgx-archive-done-tasks-to-archive-file ()
  "Archive all DONE tasks in the current buffer to its default archive file."
  (interactive)
  (orgx-archive-completed-tasks #'org-archive-subtree "archive file"))

;;; Skip functions

(defun orgx-skip-unless-untagged ()
  "Skip headings that have tags (including inherited tags)."
  (when (org-get-tags)
    (or (outline-next-heading) (point-max))))

(defun orgx-skip-unless-open-question ()
  "Skip headings in agenda views that are not open human questions."
  (if (and (fboundp 'orgx-denote-file-completed-p)
           (bound-and-true-p orgx-denote-agenda-suppress-completed)
           (orgx-denote-file-completed-p (buffer-file-name)))
      (point-max)
    (let* ((hl (org-get-heading t t t t))
           (todo (org-get-todo-state))
           (local-tags (org-get-tags nil t))
           (is-answered (or (equal todo "ANSWERED")
                            (equal todo "DONE")
                            (and hl (string-prefix-p "ANSWERED " hl))))
           (is-metadata (member hl '("Context" "Response" "LOGBOOK" "PROPERTIES")))
           (is-sub-under-question (and (not (member "question" local-tags))
                                       (org-up-heading-safe)
                                       (member "question" (org-get-tags nil t)))))
      (if (or is-answered is-metadata is-sub-under-question)
          (save-excursion (org-end-of-subtree t) (point))
        nil))))

;;; Agenda view scoping

;;;###autoload
(defun orgx-agenda-untagged-in-file (file &optional tag todo-only inherited)
  "Show untagged tasks in FILE.
When optional TAG is supplied, search for that tag instead."
  (interactive "fFile: ")
  (let ((org-agenda-files (list file)))
    (if tag
        (org-tags-view todo-only (format "%s%s" (if inherited "" "+") tag))
      (let ((org-agenda-skip-function #'orgx-skip-unless-untagged))
        (org-tags-view t "+TODO=\"TODO\"|+TODO=\"INPROGRESS\"")))))

;;;###autoload
(defun orgx-agenda-for-file (file)
  "Show full todo agenda restricted to FILE."
  (interactive "fFile: ")
  (let ((org-agenda-files (list file)))
    (org-todo-list)))

;;; Human question workflow

;;;###autoload
(defun orgx-insert-question (question &optional context)
  "Insert an actionable human QUESTION heading with optional CONTEXT."
  (interactive "sQuestion: \nsContext (leave blank to skip): ")
  (org-insert-heading-respect-content)
  (insert (concat "QUESTION " question))
  (org-toggle-tag "question" 'on)
  (org-id-get-create)
  (let ((stars (make-string (1+ (org-current-level)) ?*)))
    (unless (or (null context) (string-empty-p context))
      (org-end-of-subtree)
      (insert (concat "\n" stars " Context\n" context)))
    (org-end-of-subtree)
    (insert (concat "\n" stars " Response\n"))))

(defun orgx-enforce-question-answered ()
  "Block closing a :question:-tagged heading with any DONE state but ANSWERED."
  (when (and (member org-state org-done-keywords)
             (not (string= org-state "ANSWERED"))
             (member "question" (org-get-tags)))
    (org-todo org-last-state)
    (user-error "Headings tagged :question: can only be closed with ANSWERED, not %s" org-state)))

;;; Agenda view dispatcher

(defconst orgx-agenda-builtin-views
  '(("a" "Agenda (week/day)")
    ("t" "All TODOs")
    ("m" "Match tags / props / todo")
    ("s" "Search keywords"))
  "Standard built-in views for `orgx-agenda-view'.")

(defun orgx--agenda-view-candidate (entry width)
  "Format label for ENTRY with key left-padded to WIDTH."
  (concat (string-pad (car entry) width) ": " (cadr entry)))

(defun orgx--agenda-view-settings (entry)
  "Return SETTINGS alist for a custom-command ENTRY."
  (when (>= (length entry) 3)
    (if (listp (nth 2 entry))
        (when (>= (length entry) 4) (nth 3 entry))
      (when (>= (length entry) 5) (nth 4 entry)))))

(defun orgx--agenda-view-annotation (entry)
  "Return annotation description for ENTRY."
  (if (< (length entry) 3)
      "built-in"
    (let* ((type (nth 2 entry))
           (match (when (>= (length entry) 4) (nth 3 entry)))
           (match-label
            (cond
             ((and (symbolp match) (functionp match)) (symbol-name match))
             ((and (stringp match) (not (string-empty-p match))) match)))
           (settings (orgx--agenda-view-settings entry))
           (header (cadr (assq 'org-agenda-overriding-header settings)))
           (files-p (assq 'org-agenda-files settings)))
      (string-join
       (seq-filter #'identity
                   (list (unless (listp type) (symbol-name type))
                         match-label
                         (when files-p "custom files")
                         (when (and header (not (equal header (cadr entry))))
                           header)))
       "  "))))

(defun orgx--agenda-view-group (entry)
  "Group label for ENTRY."
  (cond
   ((< (length entry) 3) "Built-in")
   ((assq 'org-agenda-files (orgx--agenda-view-settings entry)) "File-scoped")
   (t (format "Global: %s" (capitalize (format "%s" (nth 2 entry)))))))

;;;###autoload
(defun orgx-agenda-view ()
  "Select and dispatch an `org-agenda' view via completing-read."
  (interactive)
  (let* ((custom (or (and (boundp 'org-agenda-custom-commands) org-agenda-custom-commands) nil))
         (entries (append orgx-agenda-builtin-views custom))
         (max-key-len (apply #'max (mapcar (lambda (e) (length (car e))) entries)))
         (table (make-hash-table :test 'equal))
         candidates)
    (dolist (entry entries)
      (let ((cand (orgx--agenda-view-candidate entry max-key-len)))
        (puthash cand entry table)
        (push cand candidates)))
    (setq candidates (nreverse candidates))
    (let* ((collection
            (lambda (str pred action)
              (if (eq action 'metadata)
                  `(metadata
                    (category . org-agenda-view)
                    (annotation-function
                     . ,(lambda (c)
                          (when-let* ((entry (gethash c table)))
                            (concat "   " (orgx--agenda-view-annotation entry)))))
                    (group-function
                     . ,(lambda (c transform)
                          (if transform c
                            (when-let* ((entry (gethash c table)))
                              (orgx--agenda-view-group entry))))))
                (complete-with-action action candidates str pred))))
           (choice (completing-read "Agenda view: " collection nil t)))
      (when-let* ((entry (gethash choice table)))
        (org-agenda nil (car entry))))))

;;; Heading search and navigation

;;;###autoload
(defun orgx-agenda-switch-buffer ()
  "Switch to an agenda buffer or other Org file using completing-read."
  (interactive)
  (let* ((agenda-bufs (seq-filter (lambda (b)
                                    (with-current-buffer b
                                      (derived-mode-p 'org-mode 'org-agenda-mode)))
                                  (buffer-list)))
         (names (mapcar #'buffer-name agenda-bufs)))
    (if names
        (switch-to-buffer (completing-read "Org buffer: " names nil t))
      (message "No active Org or Agenda buffers."))))

(defun orgx--heading-context (point)
  "Return path of ancestor headings leading to POINT."
  (save-excursion
    (goto-char point)
    (let (path)
      (while (org-up-heading-safe)
        (push (org-get-heading t t t t) path))
      path)))

(defun orgx--format-heading-candidate (heading path file max-file-len max-path-len)
  "Format candidate line aligning FILE, PATH, and HEADING."
  (let ((file-part (string-pad (file-name-sans-extension file) max-file-len))
        (path-part (string-pad (if path (string-join path " > ") "") max-path-len)))
    (format "%s  %s  %s" file-part path-part heading)))

;;;###autoload
(defun orgx-find-heading ()
  "Jump to any heading across all `org-agenda-files' using completing-read."
  (interactive)
  (let (targets max-file-len max-path-len)
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (let* ((heading (org-get-heading t t t t))
                    (path (orgx--heading-context (point)))
                    (pos (point))
                    (fname (file-name-nondirectory file)))
               (push (list fname path heading pos file) targets)
               (setq max-file-len (max (or max-file-len 0) (length (file-name-sans-extension fname))))
               (when path
                 (setq max-path-len (max (or max-path-len 0) (length (string-join path " > ")))))))))))
    (setq targets (nreverse targets))
    (let* ((candidates
            (mapcar (lambda (tgt)
                      (cons (orgx--format-heading-candidate (nth 2 tgt) (nth 1 tgt) (nth 0 tgt)
                                                            (or max-file-len 0) (or max-path-len 0))
                            tgt))
                    targets))
           (choice (completing-read "Heading: " (mapcar #'car candidates) nil t))
           (selected (cdr (assoc choice candidates))))
      (when selected
        (find-file (nth 4 selected))
        (goto-char (nth 3 selected))
        (org-fold-show-context 'agenda)))))

;;; Link handling

(defun orgx-link-github-line-support (orig-fn path in-emacs)
  "Support GitHub-style line fragments (#L123 or #L123-L145) in file links."
  (if (string-match "\\`\\(.*?\\)#L\\([0-9]+\\)\\(?:-L[0-9]+\\)?\\(?:\\(::.*\\)\\)?\\'" path)
      (let* ((real-path (match-string 1 path))
             (line (string-to-number (match-string 2 path)))
             (extra-opt (match-string 3 path))
             (new-path (if extra-opt (concat real-path extra-opt)
                         (format "%s::%d" real-path line))))
        (funcall orig-fn new-path in-emacs))
    (funcall orig-fn path in-emacs)))

(defun orgx-open-custom-id-slug-fallback (path)
  "Open custom ID link fallback."
  (when (string-prefix-p "#" path)
    (let ((custom-id (substring path 1)))
      (org-link-search (concat "#" custom-id))
      (org-fold-show-context)
      t)))


;;; Auxiliary packages and export tools

(defvar orgx--auxiliary-packages
  '(org-contrib toc-org ox-gist ox-hugo ox-rst ox-leanpub ob-mermaid)
  "Supporting org packages used with orgx.")

(autoload 'org-annotate-file "org-annotate-file" nil t)
(autoload 'org-gist-export-to-gist "ox-gist" nil t)
(autoload 'toc-org-insert-toc "toc-org" nil t)
(autoload 'org-leanpub-book-export-markdown "ox-leanpub" nil t)

(defvar org-rst-headline-underline-characters)

(defun orgx-gist-export-private ()
  "Export current Org buffer or region to a private Gist."
  (interactive)
  (org-gist-export-to-gist nil 'open))

(defun orgx-gist-export-public ()
  "Export current Org buffer or region to a public Gist."
  (interactive)
  (org-gist-export-to-gist 'public))

(defalias 'org-gist-export-private-gist #'orgx-gist-export-private)
(defalias 'org-gist-export-public-gist #'orgx-gist-export-public)

(defvar-keymap orgx-gist-map
  :name "orgx-gist"
  :doc "Gist export commands under C-c o g (orgx-mode)."
  "p" #'orgx-gist-export-private
  "g" #'orgx-gist-export-public)

;;; Keymaps and minor modes

(defvar-keymap orgx-capture-map
  :name "orgx-capture"
  :doc "Capture commands under C-c o c (orgx-mode).")

(keymap-set orgx-capture-map "c" #'orgx-capture)
(keymap-set orgx-capture-map "m" #'org-capture)
(keymap-set orgx-capture-map "p" #'org-capture-goto-last-stored)
(keymap-set orgx-capture-map "l" #'org-capture-goto-last-stored)
(keymap-set orgx-capture-map "t" #'orgx-capture-shell-toggle)
(keymap-set orgx-capture-map "r" #'org-capture-refile)
(keymap-set orgx-capture-map "w" #'org-capture-refile)
(keymap-set orgx-capture-map "s" #'orgx-capture-shell)
(keymap-set orgx-capture-map "C-t" #'orgx-capture-shell-transient)

(defvar-keymap orgx-archive-map
  :name "orgx-archive"
  :doc "Archive commands under C-c o C-f (orgx-mode).")

(keymap-set orgx-archive-map "e" #'org-cycle-force-archived)
(keymap-set orgx-archive-map "t" #'org-archive-set-tag)
(keymap-set orgx-archive-map "s" (cons "current→sibling" #'org-archive-to-archive-sibling))
(keymap-set orgx-archive-map "d" (cons "done+archive" #'orgx-mark-done-and-archive))

(defvar-keymap orgx-link-map
  :name "org-link"
  :doc "Org link commands under C-c o l (orgx-mode)."
  "s" #'org-store-link
  "a" #'org-id-store-link
  "i" #'org-insert-link
  "n" #'org-annotate-file)

(defvar-keymap orgx-command-map
  :name "org"
  :doc "Command keymap for orgx commands mounted under C-c o.")

(keymap-set orgx-command-map "4" #'org-agenda)
(keymap-set orgx-command-map "a" #'orgx-agenda-view)
(keymap-set orgx-command-map "c" orgx-capture-map)
(keymap-set orgx-command-map "d" #'orgx-date-now)
(keymap-set orgx-command-map "f" #'orgx-agenda-files-open)
(keymap-set orgx-command-map "C-f" orgx-archive-map)
(keymap-set orgx-command-map "g" orgx-gist-map)
(keymap-set orgx-command-map "h" #'consult-org-heading)
(keymap-set orgx-command-map "j" #'orgx-capture)
(keymap-set orgx-command-map "k" #'org-capture)
(keymap-set orgx-command-map "l" orgx-link-map)
(keymap-set orgx-command-map "n" #'org-narrow-to-subtree)
(keymap-set orgx-command-map "o" #'orgx-agenda-files-open)
(keymap-set orgx-command-map "p" #'org-insert-property-drawer)
(keymap-set orgx-command-map "q" #'agent-shell-queue-org-refile-from-heading)
(keymap-set orgx-command-map "r" #'orgx-agenda-files-reload)
(keymap-set orgx-command-map "s" #'org-save-all-org-buffers)
(keymap-set orgx-command-map "C-s" #'orgx-capture-shell-toggle)
(keymap-set orgx-command-map "C-t" #'orgx-capture-shell-transient)
(keymap-set orgx-command-map "t" #'org-set-tags-command)
(keymap-set orgx-command-map "u" (cons "untagged-in-file" #'orgx-agenda-untagged-in-file))
(keymap-set orgx-command-map "w" #'org-refile)
(keymap-set orgx-command-map "/" #'orgx-agenda-for-file)

(defvar-keymap orgx-mode-map
  :doc "Buffer-local minor mode keymap activating commands under C-c o."
  "C-c o" orgx-command-map)

(defvar-keymap orgx-global-mode-map
  :doc "Global minor mode keymap activating commands under C-c o."
  "C-c o" orgx-command-map)

(defvar orgx-mode--managed-settings
  '((fill-column . 72)
    (org-return-follows-link . t)
    (org-use-speed-commands . orgx--use-speed-commands)
    (org-startup-folded . content)
    (org-startup-indented . nil)
    (org-log-into-drawer . t)
    (org-reverse-note-order . t)
    (org-fontify-done-headline . t)
    (org-provide-todo-statistics . t)
    (org-blank-before-new-entry . ((heading . nil) (plain-list-item . nil)))
    (org-enforce-todo-dependencies . t)
    (org-enforce-todo-checkbox-dependencies . t)
    (org-fast-tag-selection-include-todo . t)
    (org-use-fast-todo-selection . auto)
    (org-use-fast-tag-selection . auto)
    (org-tags-column . -70)
    (org-tags-exclude-from-inheritance . ("project"))
    (org-track-ordered-property-with-tag . t)
    (org-footnote-auto-label . nil)
    (org-footnote-define-inline . nil)
    (org-footnote-section . nil))
  "Alist of (SYMBOL . VALUE) buffer-local settings managed by `orgx-mode'.")

(defvar-local orgx-mode--saved-settings nil
  "Alist of (SYMBOL . (LOCAL-P . VALUE)) saved before `orgx-mode' was enabled.")

(defun orgx-mode--apply-settings ()
  "Save previous buffer-local values and apply `orgx-mode--managed-settings'."
  (setq-local orgx-mode--saved-settings nil)
  (dolist (pair orgx-mode--managed-settings)
    (let ((sym (car pair))
          (val (cdr pair)))
      (push (cons sym (if (local-variable-p sym)
                          (cons t (buffer-local-value sym (current-buffer)))
                        (cons nil nil)))
            orgx-mode--saved-settings)
      (set (make-local-variable sym) val))))

(defun orgx-mode--revert-settings ()
  "Restore buffer-local variables to their state prior to `orgx-mode'."
  (dolist (entry orgx-mode--saved-settings)
    (let ((sym (car entry))
          (status (cdr entry)))
      (if (car status)
          (set (make-local-variable sym) (cdr status))
        (kill-local-variable sym))))
  (setq-local orgx-mode--saved-settings nil))

(defun orgx--set-up-buffer ()
  "Configure buffer settings for `orgx-mode'."
  (orgx-mode--apply-settings)
  (add-hook 'org-ctrl-c-ctrl-c-hook #'orgx-set-weekday-of-timestamp nil t)
  (add-hook 'org-after-todo-state-change-hook #'orgx-enforce-question-answered nil t)
  (add-hook 'org-shiftup-final-hook #'windmove-up nil t)
  (add-hook 'org-shiftleft-final-hook #'windmove-left nil t)
  (add-hook 'org-shiftdown-final-hook #'windmove-down nil t)
  (add-hook 'org-shiftright-final-hook #'windmove-right nil t)
  (add-hook 'before-save-hook #'whitespace-cleanup nil t))

(defun orgx--tear-down-buffer ()
  "Tear down buffer settings for `orgx-mode'."
  (orgx-mode--revert-settings)
  (remove-hook 'org-ctrl-c-ctrl-c-hook #'orgx-set-weekday-of-timestamp t)
  (remove-hook 'org-after-todo-state-change-hook #'orgx-enforce-question-answered t)
  (remove-hook 'org-shiftup-final-hook #'windmove-up t)
  (remove-hook 'org-shiftleft-final-hook #'windmove-left t)
  (remove-hook 'org-shiftdown-final-hook #'windmove-down t)
  (remove-hook 'org-shiftright-final-hook #'windmove-right t)
  (remove-hook 'before-save-hook #'whitespace-cleanup t))

;;;###autoload
(defalias 'orgx-minor-mode #'orgx-mode)

(defvar orgx--setup-done nil
  "Non-nil if `orgx-setup' has already run.")

(defcustom orgx-setup-hook nil
  "Hook run after `orgx-setup' completes."
  :type 'hook
  :group 'orgx)

(define-minor-mode orgx-mode
  "Personal org-mode keybindings and buffer setup.
Activates `orgx-command-map' under C-c o."
  :lighter " ox"
  :keymap orgx-mode-map
  (if orgx-mode
      (progn
        (unless orgx--setup-done
          (orgx-setup))
        (orgx--set-up-buffer))
    (orgx--tear-down-buffer)))

;;;###autoload
(defun orgx-mode-turn-on ()
  "Enable `orgx-mode' in the current buffer."
  (orgx-mode 1))

;;; orgx-agenda-minor-mode

(defvar-keymap orgx-agenda-minor-mode-map
  :doc "Keymap for `orgx-agenda-minor-mode'.")

(keymap-set orgx-agenda-minor-mode-map "C-l" #'org-agenda-open-link)
(keymap-set orgx-agenda-minor-mode-map "M-c" #'org-agenda-goto-calendar)
(keymap-set orgx-agenda-minor-mode-map "/" #'orgx-agenda-for-file)
(keymap-set orgx-agenda-minor-mode-map "C-e" #'orgx-denote-migrate-subtree)
(keymap-set orgx-agenda-minor-mode-map "C-b" #'orgx-agenda-switch-buffer)
(keymap-set orgx-agenda-minor-mode-map "M-a" #'orgx-agenda-view)
(keymap-set orgx-agenda-minor-mode-map "r" #'org-agenda-refile)

;;;###autoload
(define-minor-mode orgx-agenda-minor-mode
  "Personal org-agenda keybindings and setup."
  :lighter " oxa"
  :keymap orgx-agenda-minor-mode-map
  (when orgx-agenda-minor-mode
    (orgx--background-revbufs)))

;;;###autoload
(defun orgx-agenda-minor-mode-turn-on ()
  "Enable `orgx-agenda-minor-mode' in the current buffer."
  (orgx-agenda-minor-mode 1))


(defun orgx-color (name &optional fallback)
  "Resolve theme color for NAME, falling back to FALLBACK."
  (if (fboundp 'modus-themes-get-color-value)
      (or (funcall 'modus-themes-get-color-value name) fallback)
    fallback))

(defvar orgx-todo-keywords
  '((sequence "TODO(t)" "|" "DONE(d!)")
    (sequence "QUESTION(q)" "|" "ANSWERED(a@)")
    (sequence "INPROGRESS" "|" "COMPLETE")
    (sequence "CONSIDER(c)" "MUST(m)" "|" "ACKNOWLEDGED(k@)" "IGNORED(u)" "REJECTED(r)" "DISMISSED(p)")
    (sequence "BACKLOG(b)" "SCHEDULED(s)" "|" "GONEAWAY(g@)" "INCOMPLETE(i@)")
    (sequence "BLOCKED(o)" "PAUSED(h)" "|" "SKIPPED(j@)" "CANCELED(x@)" "INCOMPLETE(i@)" "ABANDONED(l@)"))
  "Default todo keywords for orgx GTD workflow.")

(defun orgx--default-todo-faces ()
  "Generate default keyword faces for orgx workflow."
  (let ((red (orgx-color 'red "red"))
        (dim (orgx-color 'fg-dim "dim gray"))
        (cyan (orgx-color 'cyan "cyan"))
        (yellow (orgx-color 'yellow "yellow"))
        (yellow-warmer (orgx-color 'yellow-warmer "gold"))
        (indigo (orgx-color 'indigo "indigo"))
        (blue (orgx-color 'blue "blue"))
        (cyan-warmer (orgx-color 'cyan-warmer "dark cyan"))
        (rust (orgx-color 'rust "dark orange"))
        (magenta (orgx-color 'magenta "magenta"))
        (blue-warmer (orgx-color 'blue-warmer "deep sky blue")))
    `(("TODO" . (:foreground ,red :weight bold))
      ("DONE" . (:foreground ,dim))
      ("QUESTION" . (:foreground ,cyan :weight bold))
      ("ANSWERED" . (:foreground ,dim))
      ("CONSIDER" . (:foreground ,yellow :weight bold))
      ("MUST" . (:foreground ,yellow-warmer :weight bold))
      ("ACKNOWLEDGED" . (:foreground ,dim))
      ("IGNORED" . (:foreground ,dim))
      ("REJECTED" . (:foreground ,dim))
      ("DISMISSED" . (:foreground ,dim))
      ("BACKLOG" . (:foreground ,indigo :weight bold))
      ("INPROGRESS" . (:foreground ,blue :weight bold))
      ("SCHEDULED" . (:foreground ,cyan-warmer :weight bold))
      ("GONEAWAY" . (:foreground ,dim))
      ("INCOMPLETE" . (:foreground ,dim))
      ("BLOCKED" . (:foreground ,rust :weight bold))
      ("PAUSED" . (:foreground ,magenta :weight bold))
      ("SKIPPED" . (:foreground ,dim))
      ("CANCELED" . (:foreground ,dim))
      ("ABANDONED" . (:foreground ,dim))
      ("PROJECT" . (:foreground ,blue-warmer :weight bold)))))

(defvar orgx-tag-alist
  '((:startgroup . nil)
    ("inbox" . ?i)
    ("active" . ?s)
    ("backlog" . ?b)
    ("canceled" . ?c)
    (:endgroup . nil)
    (:startgroup . nil)
    ("@agent" . ?a)
    ("@personal" . ?p)
    ("@work" . ?w)
    (:endgroup . nil)
    (:startgroup . nil)
    ("question" . ?q)
    (:endgroup . nil))
  "Default tag alist for orgx GTD workflow.")

(defvar orgx-agenda-default-commands
  `(("b" "Backlog" tags "+backlog|+inbox-ITEM=\"Inbox\"|TODO=BLOCKED"
     ((org-agenda-skip-function-global nil)))
    ("u" "Untagged TODOs (local)" todo ""
     ((org-agenda-skip-function #'orgx-skip-unless-untagged)
      (org-agenda-overriding-header "TODOs with no local tags")))
    ("h" "Untagged headings (local)" tags "LEVEL>=1-TODO={.+}"
     ((org-agenda-skip-function #'orgx-skip-unless-untagged)
      (org-agenda-overriding-header "Headings with no local tags")))
    ("i" . "including inherited")
    ("iu" "Untagged TODOs (incl. inherited)" todo ""
     ((org-agenda-skip-function #'orgx-skip-unless-untagged)
      (org-agenda-overriding-header "TODOs with no local or inherited tags")
      (orgx-agenda-include-inherited-tags t)))
    ("ih" "Untagged headings (incl. inherited)" tags "LEVEL>=1-TODO={.+}"
     ((org-agenda-skip-function #'orgx-skip-unless-untagged)
      (org-agenda-overriding-header "Headings with no local or inherited tags")
      (orgx-agenda-include-inherited-tags t))))
  "Default agenda custom commands for orgx.")

(defvar orgx-default-modules
  '(org-capture
    org-datetree
    org-annotate-file
    org-depend
    org-habit)
  "Default Org modules loaded by orgx.")

(defun orgx-babel-setup ()
  "Configure supported Org Babel languages."
  (require 'ob)
  (unless (fboundp 'org-babel-execute:golang)
    (defalias 'org-babel-execute:golang #'org-babel-execute:go))
  (provide 'ob-golang)
  (unless (fboundp 'org-babel-execute:elisp)
    (defalias 'org-babel-execute:elisp #'org-babel-execute:emacs-lisp))
  (provide 'ob-elisp)
  (unless (fboundp 'org-babel-execute:javascript)
    (defalias 'org-babel-execute:javascript #'org-babel-execute:js))
  (provide 'ob-javascript)
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((python . t)
             (emacs-lisp . t)
             (dot . t)
             (js . t)
             (go . t)
             (rust . t)
             (golang . t)
             (elisp . t)
             (javascript . t))))
  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("golang" . go))
    (add-to-list 'org-src-lang-modes '("elisp" . emacs-lisp))
    (add-to-list 'org-src-lang-modes '("javascript" . js))
    (add-to-list 'org-src-lang-modes '("rust" . rust))))

(defvar orgx-global--managed-settings
  `((org-rst-headline-underline-characters . ,(list ?= ?- ?~ ?' ?^ ?`))
    (org-modules . ,orgx-default-modules)
    (org-todo-keywords . ,orgx-todo-keywords)
    (org-tag-alist . ,orgx-tag-alist)
    (org-todo-keyword-faces . ,(orgx--default-todo-faces))
    (org-agenda-custom-commands . ,orgx-agenda-default-commands)
    (org-replace-disputed-keys . t)
    (org-outline-path-complete-in-steps . nil)
    (org-refile-allow-creating-parent-nodes . confirm)
    (org-refile-use-outline-path . file)
    (org-archive-default-command . org-archive-to-archive-sibling)
    (org-blank-before-new-entry . ((heading . nil) (plain-list-item . nil)))
    (org-refile-targets . ((org-agenda-files :maxlevel . 4)))
    (org-agenda-sticky . t)
    (org-agenda-include-diary . nil)
    (org-agenda-block-separator . nil)
    (org-agenda-columns-add-appointments-to-effort-sum . t)
    (org-agenda-compact-blocks . t)
    (org-agenda-default-appointment-duration . 60)
    (org-agenda-inhibit-startup . nil)
    (org-agenda-mouse-1-follows-link . t)
    (org-agenda-use-time-grid . t)
    (org-agenda-skip-deadline-if-done . nil)
    (org-agenda-skip-scheduled-if-deadline-is-shown . nil)
    (org-agenda-skip-scheduled-if-done . t)
    (org-agenda-skip-unavailable-files . t)
    (org-agenda-skip-timestamp-if-done . t)
    (org-agenda-todo-ignore-deadlines . t)
    (org-agenda-todo-ignore-scheduled . t)
    (org-agenda-start-on-weekday . nil)
    (org-agenda-skip-function-global . orgx-skip-child-of-project-tag))
  "Alist of (SYMBOL . VALUE) settings managed globally by `orgx-global-mode'.")

(defvar orgx-global--saved-settings nil
  "Alist of (SYMBOL . (BOUNDP . VALUE)) saved before enabling.")

(defun orgx-global--apply-settings ()
  "Save previous values and apply `orgx-global--managed-settings'."
  (setq orgx-global--saved-settings nil)
  (dolist (pair orgx-global--managed-settings)
    (let ((sym (car pair))
          (val (cdr pair)))
      (push (cons sym (if (boundp sym) (cons t (symbol-value sym)) (cons nil nil)))
            orgx-global--saved-settings)
      (set sym val))))

(defun orgx-global--revert-settings ()
  "Restore variables to their state prior to `orgx-global-mode'."
  (dolist (entry orgx-global--saved-settings)
    (let ((sym (car entry))
          (status (cdr entry)))
      (if (car status)
          (set sym (cdr status))
        (makunbound sym))))
  (setq orgx-global--saved-settings nil))

(defun orgx--enable-advice ()
  "Enable orgx advice and link hooks."
  (advice-add 'toc-org-insert-toc :around #'orgx--toc-bind-priorities)
  (advice-add 'toc-org-hrefify-gh :around #'orgx--toc-hrefify-priority)
  (advice-add 'org-agenda-redo :around #'orgx--ad-org-agenda-redo)
  (advice-add 'org-link-open-as-file :around #'orgx-link-github-line-support)
  (add-hook 'org-open-link-functions #'orgx-open-custom-id-slug-fallback))

(defun orgx--disable-advice ()
  "Disable orgx advice and link hooks."
  (advice-remove 'toc-org-insert-toc #'orgx--toc-bind-priorities)
  (advice-remove 'toc-org-hrefify-gh #'orgx--toc-hrefify-priority)
  (advice-remove 'org-agenda-redo #'orgx--ad-org-agenda-redo)
  (advice-remove 'org-link-open-as-file #'orgx-link-github-line-support)
  (remove-hook 'org-open-link-functions #'orgx-open-custom-id-slug-fallback))

;;;###autoload
(defun orgx-setup ()
  "Centrally initialize and wire orgx features, templates, and agenda integration."
  (interactive)
  (orgx-global--apply-settings)
  (orgx--enable-advice)
  (org-load-modules-maybe t)
  (orgx-babel-setup)
  (add-hook 'org-mode-hook #'orgx-mode-turn-on)
  (add-hook 'org-capture-mode-hook #'orgx-mode-turn-on)
  (add-hook 'org-agenda-mode-hook #'orgx-agenda-minor-mode-turn-on)
  (orgx-capture-setup-standard-templates)
  (orgx-denote-setup-agenda-commands)
  (setq orgx--setup-done t)
  (run-hooks 'orgx-setup-hook))

;;;###autoload
(define-minor-mode orgx-global-mode
  "Global mode for orgx.
When enabled, runs `orgx-setup', activates `orgx-global-mode-map'
\(with `C-c o'), and automatically enables `orgx-mode' in `org-mode'
and `org-capture-mode' buffers, and `orgx-agenda-minor-mode' in
`org-agenda-mode' buffers.
When disabled, reverts managed configuration settings and removes hooks."
  :global t
  :keymap orgx-global-mode-map
  :group 'orgx
  (if orgx-global-mode
      (progn
        (orgx-setup)
        (orgx--enable-advice)
        (add-hook 'org-mode-hook #'orgx-mode-turn-on)
        (add-hook 'org-capture-mode-hook #'orgx-mode-turn-on)
        (add-hook 'org-agenda-mode-hook #'orgx-agenda-minor-mode-turn-on))
    (orgx--disable-advice)
    (orgx-global--revert-settings)
    (remove-hook 'org-mode-hook #'orgx-mode-turn-on)
    (remove-hook 'org-capture-mode-hook #'orgx-mode-turn-on)
    (remove-hook 'org-agenda-mode-hook #'orgx-agenda-minor-mode-turn-on)
    (setq orgx--setup-done nil)))

;;;###autoload
(defalias 'orgx-global-minor-mode #'orgx-global-mode)

(provide 'orgx)
;;; orgx.el ends here
