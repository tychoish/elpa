;;; orgx.el --- Personal org-mode extensions and navigation -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (annotated-completing-read "0.1") (mcpkit "0.1.0") (hitl "0.1.0"))
;; Keywords: org, convenience

;;; Commentary:
;; Core Org mode extensions:
;; - Date formatting and timestamp utilities
;; - Agenda file management, reload, and scoping commands
;; - Custom speed commands and headline navigation
;; - Archive management and completed task refiling
;; - Minor mode (`orgx-mode') with unified keymap under `C-c o'
;; - Global keymap (`orgx-global-map')
;;
;; Bulk plans are `hitl-plan' plans (the `hitl' package).  `agent-shell-queue'
;; is an optional dependency, used only by `orgx-bulk-asq'.

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
(require 'annotated-completing-read)
(require 'org-refile)
(require 'orgx-refile-agent)
(require 'org-agenda)
(require 'org-archive)
(require 'ob)
(require 'org-src)
(require 'ol)

(declare-function org-cycle-force-archived "org-cycle")
(declare-function org-table--shrink-columns "org-table")
(declare-function org-table-shrink "org-table")
(declare-function org-table-expand "org-table")
(declare-function org-table-map-tables "org-table")
(declare-function org-table-to-lisp "org-table")
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
(declare-function orgx-bulk-todo-state "orgx-bulk")
(declare-function orgx-agenda-bulk-action "orgx-bulk-agenda")
(declare-function orgx-bulk-resume "orgx-bulk-buffer")

(defvar orgx-agenda-minor-mode)
(defvar org-state)
(defvar org-last-state)

(defgroup orgx nil
  "Org-mode personal extensions."
  :group 'org)

(defcustom orgx-project-tags '("PROJECT" "EPIC" "project" "task" "epic" "plan" "scope" "tracker")
  "Tags that identify projects for skip functions.
Matching against a heading's tags is case-insensitive, so casing here
is cosmetic; both conventions are listed since real notes use either."
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

(defvar denote-directory)

(defvar org-annotate-file-storage-file nil
  "File for org-annotate-file storage.")

;;;###autoload
(defun orgx-set-notes-directory (&optional path)
  "Set the notes directory to PATH and update Org paths.
If PATH is nil, use `local-notes-directory'.
Updates `local-notes-directory', `org-directory',
`org-annotate-file-storage-file', `deft-directory', and `denote-directory'."
  (interactive
   (list (read-directory-name "Notes directory: " (or local-notes-directory "~/notes"))))
  (when path
    (setq local-notes-directory (expand-file-name path)))

  (unless local-notes-directory
    (error "Must define `local-notes-directory'"))

  (setq org-directory (file-name-concat local-notes-directory "org"))
  (setq org-annotate-file-storage-file (file-name-concat org-directory "records.org"))
  (setq org-default-notes-file (file-name-concat org-directory "records.org"))
  (setq org-archive-location (file-name-concat org-directory "archive/%s::datetree/"))
  (setq deft-directory (file-name-concat local-notes-directory "deft"))
  (setq denote-directory (file-name-concat local-notes-directory "denote"))
  local-notes-directory)

;;;###autoload
(defun orgx-add-notes-to-agenda ()
  "Add configured Org directories to `org-agenda-files'."
  (if (stringp org-agenda-files)
      (display-warning 'orgx "Cannot add directories: org-agenda-files names a file" :warning)
    (seq-do (lambda (directory)
              (when (and (stringp directory)
                         (not (string-empty-p (string-trim directory))))
                (let ((path (directory-file-name
                             (expand-file-name (string-trim directory)))))
                  (unless (seq-some (lambda (entry)
                                      (and (stringp entry)
                                           (equal path (directory-file-name
                                                        (expand-file-name entry)))))
                                    org-agenda-files)
                    (add-to-list 'org-agenda-files path t)))))
            (flatten-tree (list (bound-and-true-p org-directory)
                                user-org-directories)))))


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
  (orgx-refresh-todo-faces)
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
  "Skip children of headings tagged with any element of `orgx-project-tags'.
Tag comparison is case-insensitive."
  (save-restriction
    (widen)
    (let (skip)
      (while (and (not skip) (org-up-heading-safe))
        (when (seq-intersection orgx-project-tags (org-get-tags nil t) #'string-equal-ignore-case)
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
  "Archive completed direct children, or siblings when there are no children.
Only headings at the selected level are processed; descendants of those
headings move with their parent and are not archived independently."
  (interactive)
  (let (targets parent)
    (save-excursion
      (save-restriction
        (cond
         ((org-before-first-heading-p))
         (t
          (org-back-to-heading t)
          (if (or (save-excursion (org-goto-first-child))
                  (org-up-heading-safe))
              (progn
                (setq parent (point))
                (org-narrow-to-subtree))
            nil)))
        (org-map-entries
         (lambda ()
           (when (and (equal parent (save-excursion
                                      (when (org-up-heading-safe) (point))))
                      (not (member org-archive-tag (org-get-tags nil t))))
             (push (copy-marker (point)) targets)))
         (orgx-done-state-match) nil)))
    (seq-do (lambda (marker)
              (save-excursion
                (goto-char marker)
                (let ((transient-mark-mode nil))
                  (org-archive-to-archive-sibling)))
              (set-marker marker nil))
            targets)
    (message "Archived %d tasks to archive sibling." (length targets))))

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
  "Select an org-agenda view via annotated completing read.
Includes both the standard built-in views and any entries in
`org-agenda-custom-commands'.  Each candidate is labeled with its
dispatch key (e.g. \"da: Denote Agenda ALL\") and annotated with its
command type, match string or filter function, overriding header, and
file scope.  Candidates are grouped by file scope via
`orgx--agenda-view-group'."
  (interactive)
  (let* ((customs (seq-filter (lambda (e) (and (proper-list-p e) (stringp (cadr e))))
                              (or (and (boundp 'org-agenda-custom-commands) org-agenda-custom-commands) nil)))
         (all (append orgx-agenda-builtin-views customs))
         (key-width (apply #'max (seq-map (lambda (e) (length (car e))) all)))
         (acr-table (seq-map (lambda (e)
                                (cons (orgx--agenda-view-candidate e key-width)
                                      (cons (orgx--agenda-view-annotation e) e)))
                              all))
         (entry (annotated-completing-read
                 acr-table
                 :prompt "Agenda view: "
                 :require-match t
                 :category 'org-agenda-view
                 :group-name (lambda (candidate)
                               (when-let* ((e (cddr (assoc candidate acr-table))))
                                 (orgx--agenda-view-group e))))))
    (org-agenda nil (car entry))))

;;; Heading search and navigation

;;;###autoload
(defun orgx-agenda-switch-buffer ()
  "Switch to an Org or Org Agenda buffer via `annotated-completing-read'.
Lists every buffer in `org-mode' or `org-agenda-mode', annotated with
whether it is currently visible in a window and its major mode."
  (interactive)
  (let ((buffers (seq-filter (lambda (buf)
                               (with-current-buffer buf
                                 (derived-mode-p 'org-mode 'org-agenda-mode)))
                             (buffer-list))))
    (unless buffers
      (user-error "No active Org or Agenda buffers"))
    (switch-to-buffer
     (annotated-completing-read
      (seq-map (lambda (buf)
                 (cons (buffer-name buf)
                       (format "%s, %s"
                               (if (get-buffer-window buf) "visible" "not visible")
                               (with-current-buffer buf (symbol-name major-mode)))))
               buffers)
      :prompt "Switch to Org buffer: "
      :category 'buffer
      :require-match t))))

;;;###autoload
(defun orgx-find-heading ()
  "Jump to any org heading via `org-refile-get-targets'.
Candidates come from `annotated-completing-read'.  Scope and depth
follow `org-refile-targets', annotated with the target's file and
buffer position."
  (interactive)
  (let* ((targets (org-refile-get-targets))
         (choice (annotated-completing-read
                  (seq-map (lambda (target)
                             (cons (car target)
                                   (format "%s:%d" (nth 1 target) (nth 3 target))))
                           targets)
                  :prompt "Heading: "
                  :require-match t)))
    (when-let* ((target (seq-find (lambda (tgt) (equal (car tgt) choice)) targets)))
      (find-file (nth 1 target))
      (goto-char (nth 3 target))
      (org-fold-show-context 'agenda))))

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
(keymap-set orgx-command-map "b" #'orgx-bulk-todo-state)
(keymap-set orgx-command-map "B" #'orgx-bulk-resume)
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

(defcustom orgx-use-visual-line t
  "When non-nil, `orgx-mode' enables `visual-line-mode' in its buffer."
  :type 'boolean
  :group 'orgx)

(defvar-local orgx--visual-line-enabled nil
  "Non-nil if `orgx-mode' turned on `visual-line-mode' in this buffer.")

(defun orgx--enable-visual-line ()
  "Enable `visual-line-mode' per `orgx-use-visual-line'."
  (when (and orgx-use-visual-line (not visual-line-mode))
    (visual-line-mode 1)
    (setq orgx--visual-line-enabled t)))

(defun orgx--disable-visual-line ()
  "Disable `visual-line-mode' if `orgx-mode' enabled it."
  (when orgx--visual-line-enabled
    (visual-line-mode -1)
    (setq orgx--visual-line-enabled nil)))

;;; Table shrinking

(defcustom orgx-shrink-tables t
  "When non-nil, `orgx-mode' shrinks wide columns of Org tables on enable.
Shrinking only changes how tables are displayed, never the buffer text."
  :type 'boolean
  :group 'orgx)

(defcustom orgx-table-shrink-width 40
  "Display width of shrunk Org table columns in `orgx-mode' buffers.
Columns whose content is wider than this are shrunk to it.  Columns with
an explicit width cookie keep the cookie's width."
  :type 'integer
  :group 'orgx)

(defvar-local orgx--tables-shrunk nil
  "Non-nil if `orgx-mode' shrank tables in this buffer.")

(defun orgx--ad-table-shrink-field (args)
  "Filter ARGS of `org-table--shrink-field' to apply `orgx-table-shrink-width'.
Org shrinks columns without a width cookie to a single character, including
when it re-shrinks after `org-table-align'; in `orgx-mode' they use the
configured width instead."
  (if (and (bound-and-true-p orgx-mode)
           (eql (car args) 0)
           (integerp orgx-table-shrink-width)
           (> orgx-table-shrink-width 0))
      (cons orgx-table-shrink-width (cdr args))
    args))

(defun orgx--table-wide-columns (width)
  "Return the numbers of columns of the table at point wider than WIDTH.
Columns with a width cookie are excluded."
  (let ((rows (seq-filter #'listp (org-table-to-lisp)))
        (maxima nil)
        (cookies nil))
    (dolist (row rows)
      (cl-loop for cell in row
               for i from 1
               do (let ((text (string-trim cell)))
                    (if (string-match-p "\\`<[lrc]?[0-9]+>\\'" text)
                        (cl-pushnew i cookies)
                      (setf (alist-get i maxima 0) (max (alist-get i maxima 0)
                                                        (string-width text)))))))
    (sort (cl-loop for (i . w) in maxima
                   when (and (> w width) (not (memq i cookies)))
                   collect i)
          #'<)))

(defun orgx--shrink-table-at-point ()
  "Shrink the wide columns of the Org table at point (display only)."
  (let ((begin (org-table-begin))
        (end (org-table-end)))
    (org-table-shrink begin end)
    (when-let* ((cols (orgx--table-wide-columns orgx-table-shrink-width)))
      (org-table--shrink-columns cols begin end))))

;;;###autoload
(defun orgx-table-shrink-wide-columns ()
  "Shrink wide columns of all Org tables in the buffer.
Only the display changes.  Widths are `orgx-table-shrink-width'."
  (interactive)
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'org-table--shrink-columns))
    (let ((inhibit-message t))
      ;; Older Org takes only (F &optional QUIET).
      (if (let ((max (cdr (func-arity #'org-table-map-tables))))
            (or (eq max 'many) (>= max 3)))
          (org-table-map-tables #'orgx--shrink-table-at-point t 'org)
        (org-table-map-tables #'orgx--shrink-table-at-point t)))
    (setq orgx--tables-shrunk t)))

(defun orgx--enable-table-shrink ()
  "Shrink tables per `orgx-shrink-tables'."
  (when (and orgx-shrink-tables (derived-mode-p 'org-mode))
    (orgx-table-shrink-wide-columns)))

(defun orgx--disable-table-shrink ()
  "Expand tables shrunk by `orgx-mode'."
  (when orgx--tables-shrunk
    (org-table-expand (point-min) (point-max))
    (setq orgx--tables-shrunk nil)))

(defun orgx--set-up-buffer ()
  "Configure buffer settings for `orgx-mode'."
  (orgx-mode--apply-settings)
  (orgx--enable-visual-line)
  (orgx--enable-table-shrink)
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
  (orgx--disable-visual-line)
  (orgx--disable-table-shrink)
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
        (orgx-refresh-todo-faces)
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
(keymap-set orgx-agenda-minor-mode-map "B" #'orgx-agenda-bulk-action)

;;;###autoload
(define-minor-mode orgx-agenda-minor-mode
  "Personal org-agenda keybindings and setup."
  :lighter " oxa"
  :keymap orgx-agenda-minor-mode-map
  (when orgx-agenda-minor-mode
    (orgx-refresh-todo-faces)
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

;;;###autoload
(defun orgx-refresh-todo-faces ()
  "Refresh `org-todo-keyword-faces' from active theme palette."
  (interactive)
  (setq org-todo-keyword-faces (orgx--default-todo-faces)))

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
  ;; Aliases use the canonical backends and have no corresponding ob-* files.
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append (seq-remove (lambda (language)
                         (memq (car language) '(golang elisp javascript)))
                       org-babel-load-languages)
           '((python . t)
             (emacs-lisp . t)
             (dot . t)
             (js . t)
             (go . t)
             (rust . t))))
  (unless (fboundp 'org-babel-execute:golang)
    (defalias 'org-babel-execute:golang #'org-babel-execute:go))
  (unless (fboundp 'org-babel-execute:elisp)
    (defalias 'org-babel-execute:elisp #'org-babel-execute:emacs-lisp))
  (unless (fboundp 'org-babel-execute:javascript)
    (defalias 'org-babel-execute:javascript #'org-babel-execute:js))
  (add-to-list 'org-src-lang-modes '("golang" . go))
  (add-to-list 'org-src-lang-modes '("elisp" . emacs-lisp))
  (add-to-list 'org-src-lang-modes '("javascript" . js))
  (add-to-list 'org-src-lang-modes '("rust" . rust)))

(defvar orgx-global--managed-settings
  `((org-rst-headline-underline-characters . ,(list ?= ?- ?~ ?' ?^ ?`))
    (org-modules . ,orgx-default-modules)
    (org-todo-keywords . ,orgx-todo-keywords)
    (org-tag-alist . ,orgx-tag-alist)
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
      (set sym val)))
  (push (cons 'org-todo-keyword-faces
              (if (boundp 'org-todo-keyword-faces)
                  (cons t (symbol-value 'org-todo-keyword-faces))
                (cons nil nil)))
        orgx-global--saved-settings)
  (orgx-refresh-todo-faces))

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
  (advice-add 'org-table--shrink-field :filter-args #'orgx--ad-table-shrink-field)
  (add-hook 'org-open-link-functions #'orgx-open-custom-id-slug-fallback)
  (orgx-refile-agent-enable))

(defun orgx--disable-advice ()
  "Disable orgx advice and link hooks."
  (advice-remove 'toc-org-insert-toc #'orgx--toc-bind-priorities)
  (advice-remove 'toc-org-hrefify-gh #'orgx--toc-hrefify-priority)
  (advice-remove 'org-agenda-redo #'orgx--ad-org-agenda-redo)
  (advice-remove 'org-link-open-as-file #'orgx-link-github-line-support)
  (advice-remove 'org-table--shrink-field #'orgx--ad-table-shrink-field)
  (remove-hook 'org-open-link-functions #'orgx-open-custom-id-slug-fallback)
  (orgx-refile-agent-disable))

;;;###autoload
(defun orgx-setup ()
  "Centrally initialize and wire orgx features, templates, and agenda integration."
  (interactive)
  (orgx-global--apply-settings)
  (orgx-add-notes-to-agenda)
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

;;; Generic entry utilities

(defun orgx-entry-find-by-property (property value)
  "Return a marker at the first heading whose PROPERTY equals VALUE, or nil.
PROPERTY is compared as a string against the entry's own (non-inherited)
value, so CUSTOM_ID and any other property work alike.  The search covers
the whole accessible buffer (widened); point is not moved."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let (found)
     (while (and (not found)
                 (re-search-forward org-heading-regexp nil t))
       (when (equal (org-entry-get (point) property) value)
         (setq found (copy-marker (line-beginning-position)))))
     found)))

(defmacro orgx--with-entry (pos &rest body)
  "Run BODY with point on the heading of the entry at POS (default point).
Point is restored afterwards.  The buffer is widened for the duration."
  (declare (indent 1))
  `(save-excursion
     (org-with-wide-buffer
      (when ,pos (goto-char ,pos))
      (org-back-to-heading t)
      ,@body)))

;; Org special properties computed by Org rather than drawer entries.
(defconst orgx--non-drawer-properties
  '("TODO" "ITEM" "ALLTAGS" "TAGS" "PRIORITY" "CLOSED" "SCHEDULED" "DEADLINE"
    "TIMESTAMP" "TIMESTAMP_IA" "FILE" "BLOCKED" "CLOCKSUM" "CLOCKSUM_T"
    "CATEGORY_ALL")
  "Property names `orgx-entry-set-property' refuses to write.")

(defun orgx-entry-set-property (property value &optional pos)
  "Set PROPERTY to VALUE on the entry at POS (default point).
Signal a `user-error' when VALUE contains a newline or carriage return,
when PROPERTY is not a plain name, or when PROPERTY is an Org special
property that is not stored in the PROPERTIES drawer."
  (unless (and (stringp property)
               (string-match-p "\\`[A-Za-z0-9_-]+\\'" property))
    (user-error "Invalid property name: %S" property))
  (when (member (upcase property) orgx--non-drawer-properties)
    (user-error "Not a drawer property: %s" property))
  (unless (and (stringp value) (not (string-match-p "[\n\r]" value)))
    (user-error "Property value must be a single-line string"))
  (orgx--with-entry pos
    (org-entry-put (point) property value)))

(defun orgx-entry-set-todo (state &optional pos)
  "Set the TODO keyword of the entry at POS (default point) to STATE.
STATE must be in `org-todo-keywords-1', or the empty string to clear the
keyword.  Org's normal logging and CLOSED behavior applies."
  (unless (and (stringp state)
               (or (string-empty-p state) (member state org-todo-keywords-1)))
    (error "Unknown TODO state: %s" state))
  (orgx--with-entry pos
    (org-todo (if (string-empty-p state) 'none state))))

(defun orgx--entry-body-end ()
  "Return the position where the body of the entry at point ends.
That is just before the next heading of any level, or the end of the buffer.
Point must be on a heading; point is preserved."
  (save-excursion
    (end-of-line)
    (if (outline-next-heading) (point) (point-max))))

(defun orgx--entry-find-child (title)
  "Return the position of a direct child heading of the entry at point named TITLE."
  (save-excursion
    (let ((level (org-current-level))
          (end (save-excursion (org-end-of-subtree t t)))
          found)
      (while (and (not found)
                  (outline-next-heading)
                  (< (point) end))
        (when (and (= (org-current-level) (1+ level))
                   (equal (org-get-heading t t t t) title))
          (setq found (line-beginning-position))))
      found)))

(defun orgx--entry-ensure-child (title)
  "Return the position of the child heading TITLE of the entry at point.
Create it, one level deeper, at the end of the subtree when missing."
  (or (orgx--entry-find-child title)
      (let ((level (org-current-level)))
        (save-excursion
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (let ((start (point)))
            (insert (make-string (1+ level) ?*) " " title "\n")
            start)))))

(defun orgx-entry-append-note (note &optional pos target-section)
  "Append NOTE as \"- [TIMESTAMP] NOTE\" to the body of the entry at POS.
The note goes after the entry's drawers (PROPERTIES, LOGBOOK) and before
its first child.  With TARGET-SECTION, the note goes in the child heading
of that title instead, which is created when missing."
  (unless (and (stringp note) (not (string-empty-p (string-trim note))))
    (error "Note is empty"))
  (orgx--with-entry pos
    (when target-section
      (goto-char (orgx--entry-ensure-child target-section)))
    (let* ((head (point))
           (end (orgx--entry-body-end))
           (text (concat "- " (format-time-string "[%Y-%m-%d %a %H:%M]") " "
                         (replace-regexp-in-string "\r\n\\|\r\\|\n" "\n  " (string-trim-right note) t t)
                         "\n")))
      (goto-char end)
      (when (and (eobp) (not (bolp))) (insert "\n"))
      ;; Sit after existing body text but before trailing blank lines.
      (re-search-backward "[^ \t\n]" head t)
      (end-of-line)
      (if (eobp) (insert "\n") (forward-char 1))
      (insert text))))

(defun orgx-entry-update-in-file (file match-property match-value fn)
  "Visit FILE, call FN at the heading whose MATCH-PROPERTY is MATCH-VALUE, save.
FN is called with the heading's position.  Prompts are suppressed and the
buffer is saved afterwards.  Signal an error if no heading matches.  Return
a plist (:file FILE :heading TITLE)."
  (let* ((file (expand-file-name file))
         (buf (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                         ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (find-file-noselect file))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode) (org-mode))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (let* ((inhibit-read-only t)
               (marker (or (orgx-entry-find-by-property match-property match-value)
                           (error "No heading in %s with %s = %s"
                                  (file-name-nondirectory file)
                                  match-property match-value)))
               (title (org-with-wide-buffer
                       (goto-char marker)
                       (org-get-heading t t t t))))
          (unwind-protect (funcall fn marker)
            (set-marker marker nil))
          (save-buffer)
          (list :file file :heading title))))))

(provide 'orgx)
;;; orgx.el ends here
