;;; orgx-denote.el --- Denote note-taking integration for orgx -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, denote, convenience

;;; Commentary:
;; Denote integration for orgx:
;; - File discovery and filtering for active, completed, and plan notes
;; - Sequence-number (signature) sorting and category formatting in org-agenda
;; - Dedicated Denote agenda views (todos, questions, composite, completed plans)
;; - Subtree extraction to new Denote notes (`orgx-denote-migrate-subtree')
;; - Standard Denote capture templates

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'outline)
(require 'org)
(require 'org-agenda)

(declare-function denote-directories "denote")
(declare-function denote-retrieve-filename-keywords-as-list "denote")
(declare-function denote-retrieve-filename-signature "denote")
(declare-function denote-org-capture "denote")
(declare-function denote-org-extract-org-subtree "denote")
(declare-function denote-format-link "denote")
(declare-function denote-journal-capture-entry-today "denote-journal-capture")
(declare-function denote-journal-capture-entry-for-date "denote-journal-capture")
(declare-function denote-journal-capture-timestamp "denote-journal-capture")

(declare-function orgx-skip-unless-open-question "orgx")

(defvar org-agenda-custom-commands)
(defvar denote-last-path)
(defvar org-done-keywords)
(defvar org-state)
(defvar org-last-state)

(defgroup orgx-denote nil
  "Denote integrations for orgx."
  :group 'orgx)

(defcustom orgx-denote-agenda-suppress-completed t
  "Suppress items from closed or completed notes in denote composite agenda."
  :type 'boolean
  :group 'orgx-denote)

(defconst orgx-denote-agenda-category-width 8
  "Max width in characters for `orgx-denote-agenda-category'.")

;;; File discovery and classification

(defun orgx-denote-files ()
  "Return every .org file across all directories in `denote-directories'.
Computed fresh on each call so newly added notes are picked up."
  (when (fboundp 'denote-directories)
    (thread-last
      (denote-directories)
      (seq-mapcat (lambda (dir) (directory-files-recursively dir "\\.org\\'"))))))

(defun orgx-denote-file-completed-p (file)
  "Return non-nil if FILE represents a closed or completed denote note.
Checks filename keywords for \"x\" or \"done\", and note frontmatter for
execution_status/status of complete/executed/closed or tags :x:/:done:."
  (when (and file (file-exists-p file))
    (let ((kws (if (fboundp 'denote-retrieve-filename-keywords-as-list)
                   (denote-retrieve-filename-keywords-as-list file)
                 (when (string-match "__\\([a-zA-Z0-9_]+\\)\\." (file-name-nondirectory file))
                   (split-string (match-string 1 (file-name-nondirectory file)) "_")))))
      (or (member "x" kws)
          (member "done" kws)
          (let ((buf (get-file-buffer file)))
            (if buf
                (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (or (re-search-forward "^#\\+\\(?:execution_status\\|status\\):[ \t]*\\(?:complete\\|executed\\|closed\\)" 1024 t)
                        (re-search-forward "^#\\+filetags:.*:\\(?:x\\|done\\):" 1024 t))))
              (with-temp-buffer
                (insert-file-contents file nil 0 1024)
                (goto-char (point-min))
                (or (re-search-forward "^#\\+\\(?:execution_status\\|status\\):[ \t]*\\(?:complete\\|executed\\|closed\\)" nil t)
                    (re-search-forward "^#\\+filetags:.*:\\(?:x\\|done\\):" nil t)))))))))

(defun orgx-denote-plan-file-p (file)
  "Return non-nil if FILE represents a plan or tracker note."
  (when (and file (file-exists-p file))
    (let ((kws (if (fboundp 'denote-retrieve-filename-keywords-as-list)
                   (denote-retrieve-filename-keywords-as-list file)
                 (when (string-match "__\\([a-zA-Z0-9_]+\\)\\." (file-name-nondirectory file))
                   (split-string (match-string 1 (file-name-nondirectory file)) "_"))))
          (base (file-name-nondirectory file)))
      (or (member "plan" kws)
          (member "tracker" kws)
          (string-match-p "_plan" base)
          (string-match-p "_tracker" base)
          (let ((buf (get-file-buffer file)))
            (if buf
                (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "^#\\+filetags:.*:\\(?:plan\\|tracker\\):" 1024 t)))
              (with-temp-buffer
                (insert-file-contents file nil 0 1024)
                (goto-char (point-min))
                (re-search-forward "^#\\+filetags:.*:\\(?:plan\\|tracker\\):" nil t))))))))

(defun orgx-denote-active-files ()
  "Return all denote files that are not closed, completed, or tagged x."
  (seq-remove #'orgx-denote-file-completed-p (orgx-denote-files)))

(defun orgx-denote-composite-files ()
  "Return files to include in the denote composite agenda view.
When `orgx-denote-agenda-suppress-completed' is non-nil,
excludes completed notes."
  (if orgx-denote-agenda-suppress-completed
      (orgx-denote-active-files)
    (orgx-denote-files)))

(defun orgx-denote-completed-plan-files ()
  "Return all denote files that are completed plans or trackers."
  (seq-filter (lambda (f)
                (and (orgx-denote-plan-file-p f)
                     (orgx-denote-file-completed-p f)))
              (orgx-denote-files)))

(defun orgx-denote-skip-if-completed-note ()
  "Skip the current heading if visiting a closed/completed denote note."
  (when (and orgx-denote-agenda-suppress-completed
             (orgx-denote-file-completed-p (buffer-file-name)))
    (or (outline-next-heading) (point-max))))

;;; Agenda settings and signature sorting

(defun orgx-denote-agenda-category ()
  "Return sequence-number label for the denote agenda prefix."
  (let* ((file (buffer-file-name))
         (seq (or (and file (fboundp 'denote-retrieve-filename-signature)
                       (denote-retrieve-filename-signature file)) "")))
    (string-pad (string-limit seq orgx-denote-agenda-category-width nil "…")
                orgx-denote-agenda-category-width)))

(defun orgx-denote-agenda-cmp-signature (a b)
  "Order agenda lines A and B by the denote signature of their source file."
  (let* ((file-a (get-text-property 0 'org-category a))
         (file-b (get-text-property 0 'org-category b))
         (sig-a (or (and file-a (fboundp 'denote-retrieve-filename-signature)
                         (denote-retrieve-filename-signature file-a)) ""))
         (sig-b (or (and file-b (fboundp 'denote-retrieve-filename-signature)
                         (denote-retrieve-filename-signature file-b)) "")))
    (cond ((string< sig-a sig-b) -1)
          ((string< sig-b sig-a) 1)
          (t 0))))

(defun orgx-denote-agenda-settings (header &optional files-form)
  "Return shared `org-agenda-custom-commands' settings for a denote view.
Scopes to FILES-FORM (defaults to `(orgx-denote-files)'), sorts by denote
signature, and formats the sequence category."
  `((org-agenda-files ,(or files-form '(orgx-denote-files)))
    (org-agenda-skip-function-global nil)
    (org-agenda-overriding-header ,header)
    (org-agenda-sorting-strategy '(user-defined-up))
    (org-agenda-cmp-user-defined #'orgx-denote-agenda-cmp-signature)
    (org-agenda-prefix-format '((todo . " %i %-8(orgx-denote-agenda-category) ")
                                (tags . " %i %-8(orgx-denote-agenda-category) ")))))

;;;###autoload
(defun orgx-denote-agenda-todos ()
  "Show all TODO items across the denote note tree."
  (interactive)
  (org-tags-view t "+TODO=\"TODO\"|+TODO=\"INPROGRESS\"-question"))

;;;###autoload
(defun orgx-denote-agenda-questions ()
  "Show all human questions across the denote tree."
  (interactive)
  (org-tags-view t "+question|+TODO=\"QUESTION\""))

;;;###autoload
(defun orgx-denote-agenda-composite ()
  "Show composite denote agenda, suppressing closed/completed notes."
  (interactive)
  (let ((orgx-denote-agenda-suppress-completed t))
    (org-tags-view t "+TODO=\"TODO\"|+TODO=\"INPROGRESS\"-question")))

;;;###autoload
(defun orgx-denote-agenda-completed-plans ()
  "Show all items from completed plans across the denote tree."
  (interactive)
  (let ((org-agenda-files (orgx-denote-completed-plan-files)))
    (org-tags-view t "+TODO=\"DONE\"|+TODO=\"CANCELLED\"")))

(defconst orgx-denote-agenda-commands
  `(("d" . "denote database files")
    ("da" "Denote Agenda" todo ""
     ,(orgx-denote-agenda-settings "Denote Agenda: All"))
    ("dt" "Denote Agenda Tasks (without agent or questions)"
     ((tags-todo "-agent-question-TODO=\"QUESTION\""
                 ((org-agenda-overriding-header "Denote Agenda"))))
     ,(orgx-denote-agenda-settings "Denote Agenda: Tasks"))
    ("dn" "Denote Agenda (without agent)"
     ((tags-todo "-agent"
                 ((org-agenda-overriding-header "Denote (non) Agent Tasks"))))
     ,(orgx-denote-agenda-settings "Denote Agenda: Non-agent tasks"))
    ("dg" "Denote Agenda (agent only)"
     ((tags-todo "+agent-question-TODO=\"QUESTION\""
                 ((org-agenda-overriding-header "Denote Agent Tasks"))))
     ,(orgx-denote-agenda-settings "Denote Agenda: Agent tasks"))
    ("dq" "Human Questions"
     ((tags "+question|TODO=\"QUESTION\""
            ((org-agenda-skip-function #'orgx-skip-unless-open-question)
             (org-agenda-overriding-header "Human Questions"))))
     ,(orgx-denote-agenda-settings nil))
    ("dc" "Denote Composite Agenda"
     ((tags "+question|TODO=\"QUESTION\""
            ((org-agenda-skip-function #'orgx-skip-unless-open-question)
             (org-agenda-overriding-header "Human questions")))
      (tags-todo "TODO=\"CONSIDER\"|TODO=\"MUST\""
                 ((org-agenda-overriding-header "Pending Considerations")))
      (tags-todo "TODO=\"INPROGRESS\"|TODO=\"PAUSED\"|TODO=\"SCHEDULED\"|TODO=\"BLOCKED\""
                 ((org-agenda-overriding-header "Active Tasks")))
      (tags-todo "-agent-question-TODO=\"QUESTION\""
                 ((org-agenda-overriding-header "Tasks")))
      (tags-todo "+agent-question-TODO=\"QUESTION\"-TODO=\"CONSIDER\"-TODO=\"MUST\"-TODO=\"INPROGRESS\"-TODO=\"PAUSED\"-TODO=\"SCHEDULED\"-TODO=\"BLOCKED\""
                 ((org-agenda-overriding-header "Agent tasks"))))
     ,(orgx-denote-agenda-settings nil '(orgx-denote-composite-files)))
    ("dx" "Completed Plans"
     ((tags "+question|TODO=\"QUESTION\""
            ((org-agenda-skip-function #'orgx-skip-unless-open-question)
             (org-agenda-overriding-header "Open Questions in Completed Plans")))
      (tags-todo "TODO=\"INPROGRESS\"|TODO=\"PAUSED\"|TODO=\"SCHEDULED\"|TODO=\"BLOCKED\""
                 ((org-agenda-overriding-header "Active/Incomplete Tasks in Completed Plans")))
      (tags-todo "-agent-question-TODO=\"QUESTION\""
                 ((org-agenda-overriding-header "Tasks in Completed Plans")))
      (tags-todo "+agent-question-TODO=\"QUESTION\""
                 ((org-agenda-overriding-header "Agent Tasks in Completed Plans")))
      (tags "TODO=\"DONE\"|TODO=\"ANSWERED\"|TODO=\"COMPLETE\"|TODO=\"CANCELED\"|TODO=\"ABANDONED\""
            ((org-agenda-overriding-header "Completed Tasks in Completed Plans"))))
     ,(orgx-denote-agenda-settings "Completed Plans" '(orgx-denote-completed-plan-files))))
  "List of Denote agenda commands for `org-agenda-custom-commands'.")

;;;###autoload
(defun orgx-denote-setup-agenda-commands ()
  "Register Denote custom commands into `org-agenda-custom-commands'."
  (interactive)
  (when (boundp 'org-agenda-custom-commands)
    (dolist (cmd orgx-denote-agenda-commands)
      (setq org-agenda-custom-commands
            (cons cmd (seq-remove (lambda (c) (equal (car c) (car cmd)))
                                  org-agenda-custom-commands))))))


;;; Subtree migration

(defun orgx-denote--parse-heading-date (heading)
  "Return an Emacs time value for the first org timestamp in HEADING, or nil."
  (when (string-match org-ts-regexp-both heading)
    (condition-case nil
        (org-time-string-to-time (match-string 0 heading))
      (error nil))))

;;;###autoload
(defun orgx-denote-migrate-subtree ()
  "Extract the current Org subtree to a new denote note.
Replaces the original heading with a denote link to the newly created note.
Preserves timestamps in CREATED property if present."
  (interactive)
  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-goto))
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be in an org-mode or org-agenda-mode buffer"))
  (let* ((source-buf (current-buffer))
         (insert-marker (copy-marker (org-entry-beginning-position)))
         (level (org-current-level))
         (heading (org-get-heading t t t t))
         (heading-date (orgx-denote--parse-heading-date heading)))
    (when (and heading-date
               (not (or (org-entry-get nil "DATE")
                        (org-entry-get nil "CREATED")
                        (org-entry-get nil "CLOSED"))))
      (org-set-property "CREATED"
                        (format-time-string "[%Y-%m-%d %a %H:%M]" heading-date)))
    (when-let* ((path (and (fboundp 'denote-org-extract-org-subtree)
                           (denote-org-extract-org-subtree))))
      (with-current-buffer source-buf
        (save-excursion
          (goto-char insert-marker)
          (insert (concat (make-string level ?*)
                          " "
                          (if (fboundp 'denote-format-link)
                              (denote-format-link path heading 'org nil)
                            (format "[[file:%s][%s]]" path heading))
                          "\n"))
          (goto-char insert-marker)
          (org-toggle-tag "denoted" 'on)))
      (set-marker insert-marker nil))))

;;; Capture template integration

;;;###autoload
(defun orgx-denote-setup-capture-templates ()
  "Register Denote templates into `org-capture-templates'."
  (interactive)
  (when (boundp 'org-capture-templates)
    (when (fboundp 'denote-org-capture)
      (add-to-list 'org-capture-templates
                   '("dn" "denote note" plain
                     (file denote-last-path)
                     #'denote-org-capture
                     :no-save t
                     :immediate-finish nil
                     :kill-buffer t
                     :jump-to-captured t)))
    (when (fboundp 'denote-journal-capture-entry-today)
      (add-to-list 'org-capture-templates
                   '("dj" "denote journal (today)" entry
                     (file+olp denote-journal-capture-entry-today "Journal")
                     "* %(denote-journal-capture-timestamp) %^{Entry}\n%?"
                     :no-save t
                     :kill-buffer t
                     :jump-to-captured t))
      (add-to-list 'org-capture-templates
                   '("dJ" "denote journal (date)" entry
                     (file+olp denote-journal-capture-entry-for-date "Journal")
                     "* %(denote-journal-capture-timestamp) %^{Entry}\n%?"
                     :no-save t
                     :kill-buffer t
                     :jump-to-captured t)))))

;; Wire capture templates into org-capture-mode-hook
(add-hook 'org-capture-mode-hook #'orgx-denote-setup-capture-templates)

(provide 'orgx-denote)
;;; orgx-denote.el ends here
