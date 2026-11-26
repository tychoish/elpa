;;; orgx-denote.el --- Denote note-taking integration for orgx -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, denote, convenience

;;; Commentary:
;; Denote integration for orgx:
;; - File discovery and filtering for active, completed, and plan notes
;; - Sequence-number (signature) sorting and category formatting in org-agenda
;; - Dedicated Denote agenda views (todos, questions, composite, completed plans)
;; - Standard Denote capture templates
;; - Capture of a linked heading per note with open TODOs
;;   (`orgx-denote-capture-open-items')

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'outline)
(require 'org)
(require 'org-element)
(require 'org-agenda)
(require 'cl-lib)

(declare-function org-element--property "org-element-ast")
(autoload 'denote-directories "denote")
(declare-function denote-retrieve-filename-keywords-as-list "denote")
(declare-function denote-retrieve-filename-signature "denote")
(declare-function denote-org-capture "denote")
(declare-function denote-org-extract-org-subtree "denote")
(declare-function denote-format-link "denote")
(declare-function denote-journal-capture-entry-today "denote-journal-capture")
(declare-function denote-journal-capture-entry-for-date "denote-journal-capture")
(declare-function denote-journal-capture-timestamp "denote-journal-capture")

(declare-function orgx-skip-unless-open-question "orgx")
(declare-function gen-mapc-async "gen")
(declare-function hitl-plan-restricted-p "hitl-plan")

(defvar org-agenda-custom-commands)
(defvar denote-last-path)
(defvar org-done-keywords)
(defvar org-todo-keywords-1)
(defvar org-capture-templates)
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

;;; Denote link storage

(declare-function denote-file-is-in-denote-directory-p "denote")
(declare-function denote-file-has-denoted-filename-p "denote")
(declare-function denote-retrieve-filename-identifier "denote")
(declare-function denote-get-link-description "denote")
(defvar org-store-link-plist)

(defun orgx-denote--store-non-org-link (original &rest args)
  "Call ORIGINAL with ARGS, avoiding Org parsing in non-Org Denote files."
  (if (derived-mode-p 'org-mode)
      (apply original args)
    (when-let* ((file (buffer-file-name))
                ((file-regular-p file))
                ((denote-file-is-in-denote-directory-p file))
                ((denote-file-has-denoted-filename-p file))
                (identifier (denote-retrieve-filename-identifier file))
                (description (denote-get-link-description file)))
      (org-link-store-props :type "denote" :description description
                            :link (concat "denote:" identifier))
      org-store-link-plist)))

(advice-add 'denote-link-ol-store :around #'orgx-denote--store-non-org-link)

;;; File discovery and classification

(defun orgx-denote-files ()
  "Return every .org file across all directories in `denote-directories'.
Computed fresh on each call so newly added notes are picked up."
  (when (fboundp 'denote-directories)
    (thread-last
      (denote-directories)
      (seq-mapcat (lambda (dir) (directory-files-recursively dir "\\.org\\'"))))))

(defun orgx-denote--buffer-completed-p ()
  "Return non-nil if current buffer's frontmatter indicates a completed note."
  (let* ((kws (org-collect-keywords '("EXECUTION_STATUS" "STATUS" "FILETAGS")))
         (status (or (car (alist-get "EXECUTION_STATUS" kws nil nil #'string-equal))
                     (car (alist-get "STATUS" kws nil nil #'string-equal))))
         (filetags (alist-get "FILETAGS" kws nil nil #'string-equal)))
    (or (and status (member (downcase (string-trim status))
                            '("complete" "executed" "closed")))
        (and filetags
             (seq-some (lambda (raw)
                         (let ((tags (split-string raw "[: \t]+" t)))
                           (or (member "x" tags) (member "done" tags))))
                       filetags)))))

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
          (if-let* ((buf (get-file-buffer file)))
              (with-current-buffer buf
                (unless (derived-mode-p 'org-mode)
                  (org-mode))
                (orgx-denote--buffer-completed-p))
            (with-temp-buffer
              (insert-file-contents file nil 0 1024)
              (delay-mode-hooks (org-mode))
              (orgx-denote--buffer-completed-p)))))))

(defun orgx-denote--buffer-plan-p ()
  "Return non-nil if current buffer is tagged as a plan or tracker."
  (when-let* ((filetags (alist-get "FILETAGS" (org-collect-keywords '("FILETAGS"))
                                   nil nil #'string-equal)))
    (seq-some (lambda (raw)
                (let ((tags (split-string raw "[: \t]+" t)))
                  (or (member "plan" tags) (member "tracker" tags))))
              filetags)))

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
          (if-let* ((buf (get-file-buffer file)))
              (with-current-buffer buf
                (unless (derived-mode-p 'org-mode)
                  (org-mode))
                (orgx-denote--buffer-plan-p))
            (with-temp-buffer
              (insert-file-contents file nil 0 1024)
              (delay-mode-hooks (org-mode))
              (orgx-denote--buffer-plan-p)))))))

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
    (string-pad (truncate-string-to-width seq orgx-denote-agenda-category-width nil nil "…")
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
HEADER is the overriding agenda header.
Scopes to FILES-FORM (defaults to `(orgx-denote-files)'), sorts by denote
signature, and formats the sequence category."
  `((org-agenda-files ,(or files-form '(orgx-denote-files)))
    (org-agenda-skip-function-global nil)
    (org-agenda-overriding-header ,header)
    (org-agenda-sorting-strategy '(user-defined-up))
    (org-agenda-cmp-user-defined #'orgx-denote-agenda-cmp-signature)
    (org-agenda-prefix-format '((todo . " %i %-8(orgx-denote-agenda-category) ")
                                (tags . " %i %-8(orgx-denote-agenda-category) ")))))

;;; Buffer warming and asynchronous agenda execution

(defcustom orgx-denote-agenda-warm-chunk-size 1
  "Number of denote buffers to open per timer tick during background warming."
  :type 'natnum
  :group 'orgx-denote)

(defcustom orgx-denote-agenda-warm-delay 0.005
  "Delay in seconds between buffer warming chunks."
  :type 'number
  :group 'orgx-denote)

(defun orgx-denote--file-warmed-p (file)
  "Return non-nil if FILE is visited in a live buffer derived from `org-mode'."
  (when-let* ((buf (get-file-buffer file)))
    (and (buffer-live-p buf)
         (with-current-buffer buf
           (derived-mode-p 'org-mode)))))

(defun orgx-denote--visit-file (file)
  "Safely visit FILE in the background and bury its buffer."
  (when (and file (stringp file) (file-exists-p file))
    (let ((buf (get-file-buffer file)))
      (if (buffer-live-p buf)
          (with-current-buffer buf
            (unless (derived-mode-p 'org-mode)
              (org-mode)))
        (let ((enable-local-variables (if (and (fboundp 'hitl-plan-restricted-p)
                                               (hitl-plan-restricted-p))
                                          nil
                                        enable-local-variables))
              (enable-dir-local-variables (if (and (fboundp 'hitl-plan-restricted-p)
                                                   (hitl-plan-restricted-p))
                                              nil
                                            enable-dir-local-variables)))
          (when-let* ((new-buf (find-file-noselect file)))
            (when (buffer-live-p new-buf)
              (with-current-buffer new-buf
                (unless (derived-mode-p 'org-mode)
                  (org-mode)))
              (bury-buffer new-buf))))))))

;;;###autoload
(cl-defun orgx-denote-warm-buffers-and-run (files on-done &key label
                                                  (chunk-size orgx-denote-agenda-warm-chunk-size)
                                                  (delay orgx-denote-agenda-warm-delay))
  "Warm unvisited FILES in yielding batches, then call ON-DONE.
LABEL names the batch job.
CHUNK-SIZE is the number of files per batch and DELAY the pause
between batches.
Batches run through `gen-mapc-async'.  If all FILES are already open in
buffers in `org-mode' (or in noninteractive batch mode), calls ON-DONE
immediately."
  (let* ((all (seq-filter #'stringp (mapcar #'expand-file-name files)))
         (cold (seq-filter (lambda (f) (not (orgx-denote--file-warmed-p f))) all)))
    (if (or noninteractive (null cold))
        (progn
          (when (and noninteractive cold)
            (mapc #'orgx-denote--visit-file cold))
          (when on-done (funcall on-done)))
      (message "Warming %d Denote file%s for %s..."
               (length cold) (if (= (length cold) 1) "" "s") (or label "agenda"))
      (if (fboundp 'gen-mapc-async)
          (gen-mapc-async
           #'orgx-denote--visit-file
           cold
           :chunk-size (max 1 (or chunk-size 5))
           :idle nil
           :delay (or delay 0.02)
           :name (format "orgx-denote-warm%s" (if label (format ":%s" label) ""))
           :on-progress (lambda (done total)
                          (when (or (= done total) (= (% done 25) 0))
                            (message "Warming Denote files for %s: %d/%d..."
                                     (or label "agenda") done total)))
           :on-done (lambda ()
                      (message "Denote files ready. Rendering %s..." (or label "agenda"))
                      (when on-done
                        (condition-case err
                            (funcall on-done)
                          (error
                           (display-warning 'orgx-denote
                                            (format "Error rendering %s: %s"
                                                    (or label "agenda")
                                                    (error-message-string err))
                                            :error))))))
        ;; Fallback when gen.el is not loaded
        (letrec ((rem cold)
                 (total (length cold))
                 (done-count 0)
                 (step
                  (lambda ()
                    (let ((count 0)
                          (limit (max 1 (or chunk-size 5))))
                      (while (and rem (< count limit))
                        (condition-case err
                            (orgx-denote--visit-file (pop rem))
                          (error (display-warning 'orgx-denote
                                                  (error-message-string err) :warning)))
                        (cl-incf count)
                        (cl-incf done-count))
                      (message "Warming Denote files for %s: %d/%d..."
                               (or label "agenda") done-count total)
                      (if (null rem)
                          (progn
                            (message "Denote files ready. Rendering %s..." (or label "agenda"))
                            (when on-done
                              (condition-case err
                                  (funcall on-done)
                                (error
                                 (display-warning 'orgx-denote
                                                  (format "Error rendering %s: %s"
                                                          (or label "agenda")
                                                          (error-message-string err))
                                                  :error)))))
                        (run-at-time (or delay 0.02) nil step))))))
          (run-at-time (or delay 0.02) nil step))))))

;;;###autoload
(defun orgx-denote-agenda-todos ()
  "Show all TODO items across the denote note tree."
  (interactive)
  (let ((files (orgx-denote-files)))
    (orgx-denote-warm-buffers-and-run
     files
     (lambda ()
       (let ((org-agenda-files files))
         (org-tags-view t "+TODO=\"TODO\"|+TODO=\"INPROGRESS\"-question")))
     :label "todos")))

;;;###autoload
(defun orgx-denote-agenda-questions ()
  "Show all human questions across the denote tree."
  (interactive)
  (let ((files (orgx-denote-files)))
    (orgx-denote-warm-buffers-and-run
     files
     (lambda ()
       (let ((org-agenda-files files))
         (org-tags-view t "+question|+TODO=\"QUESTION\"")))
     :label "questions")))

;;;###autoload
(defun orgx-denote-agenda-composite ()
  "Show composite denote agenda, suppressing closed/completed notes."
  (interactive)
  (let ((files (orgx-denote-composite-files)))
    (orgx-denote-warm-buffers-and-run
     files
     (lambda ()
       (let ((orgx-denote-agenda-suppress-completed t)
             (org-agenda-files files))
         (org-tags-view t "+TODO=\"TODO\"|+TODO=\"INPROGRESS\"-question")))
     :label "composite")))

;;;###autoload
(defun orgx-denote-agenda-completed-plans ()
  "Show all items from completed plans across the denote tree."
  (interactive)
  (let ((files (orgx-denote-completed-plan-files)))
    (orgx-denote-warm-buffers-and-run
     files
     (lambda ()
       (let ((org-agenda-files files))
         (org-tags-view t "+TODO=\"DONE\"|+TODO=\"CANCELLED\"")))
     :label "completed-plans")))

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

(defun orgx-denote--extract-command-files (entry)
  "Extract the list of files specified by custom command ENTRY, if any."
  (let* ((settings (if (listp (nth 2 entry))
                       (nth 3 entry)
                     (nth 4 entry)))
         (files-binding (assq 'org-agenda-files settings)))
    (when files-binding
      (condition-case nil
          (eval (cadr files-binding) t)
        (error nil)))))

(defun orgx-denote--agenda-dispatcher-advice (orig-fn &optional arg keys restriction)
  "Warm custom agenda files before calling ORIG-FN with ARG, KEYS, RESTRICTION."
  (let* ((key-str (and keys (format "%s" keys))))
    (if key-str
        (let* ((custom (assoc key-str (and (boundp 'org-agenda-custom-commands) org-agenda-custom-commands)))
               (files (and (proper-list-p custom)
                           custom (orgx-denote--extract-command-files custom)))
               (cold (and (consp files)
                          (seq-filter (lambda (f) (and (stringp f) (not (orgx-denote--file-warmed-p f)))) files))))
          (if cold
              (let ((origin (current-buffer))
                    (origin-window (selected-window))
                    (commands org-agenda-custom-commands)
                    (agenda-files org-agenda-files))
                (orgx-denote-warm-buffers-and-run
                 files
                 (lambda ()
                   (if (buffer-live-p origin)
                       (let ((org-agenda-custom-commands commands)
                             (org-agenda-files agenda-files))
                         (if (window-live-p origin-window)
                             (with-selected-window origin-window
                               (with-current-buffer origin
                                 (funcall orig-fn arg keys restriction)))
                           (with-current-buffer origin
                             (funcall orig-fn arg keys restriction))))
                     (display-warning 'orgx-denote
                                      "Agenda cancelled: the originating buffer was killed"
                                      :warning)))
                 :label key-str))
            (funcall orig-fn arg keys restriction)))
      (funcall orig-fn arg keys restriction))))

(advice-add 'org-agenda :around #'orgx-denote--agenda-dispatcher-advice)

(defun orgx-denote--ensure-agenda-file-buffer-mode (buf)
  "Ensure buffer BUF returned by `org-get-agenda-file-buffer' is in `org-mode'."
  (when (and (buffer-live-p buf)
             (when-let* ((file (buffer-file-name buf)))
               (string-match-p "\\.org\\'" file))
             (with-current-buffer buf (not (derived-mode-p 'org-mode))))
    (let ((enable-local-variables (if (and (fboundp 'hitl-plan-restricted-p)
                                           (hitl-plan-restricted-p))
                                      nil
                                    enable-local-variables))
          (enable-dir-local-variables (if (and (fboundp 'hitl-plan-restricted-p)
                                               (hitl-plan-restricted-p))
                                          nil
                                        enable-dir-local-variables)))
      (with-current-buffer buf
        (org-mode))))
  buf)

(advice-add 'org-get-agenda-file-buffer :filter-return #'orgx-denote--ensure-agenda-file-buffer-mode)


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

;;; Open-items capture

(defcustom orgx-denote-capture-dedupe 'prompt
  "How `orgx-denote-capture-open-items' treats notes already linked at the target.
`prompt' asks each time, `skip' drops them silently, and `include' keeps
them.  A note is already linked when a child heading of the target
contains a `denote:ID' link to it."
  :type '(choice (const prompt) (const skip) (const include))
  :group 'orgx-denote)

(defun orgx-denote--file-id (file)
  "Return the denote identifier of FILE, or nil if its name has none."
  (let ((name (file-name-nondirectory file)))
    (when (string-match "\\`\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)" name)
      (match-string 1 name))))

(defun orgx-denote--file-title (file)
  "Return the `#+title:' of FILE, falling back to its base name."
  (with-temp-buffer
    (insert-file-contents file nil 0 2048)
    (org-mode)
    (or (car (alist-get "TITLE" (org-collect-keywords '("TITLE"))
                        nil nil #'string-equal))
        (file-name-base file))))

(defun orgx-denote--has-open-items-p (file keywords)
  "Return non-nil if FILE has a heading whose TODO keyword is in KEYWORDS."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((org-todo-keywords (list (cons 'sequence (append keywords '("|" "DONE"))))))
      (org-mode)
      (catch 'found
        (org-map-entries
         (lambda ()
           (when-let* ((state (org-get-todo-state)))
             (when (member state keywords)
               (throw 'found t)))))
        nil))))

(defun orgx-denote--open-note-files (files tags keywords)
  "Return the denote notes in FILES that fit TAGS and KEYWORDS.
A note needs every tag in TAGS and a heading in one of KEYWORDS."
  (seq-filter (lambda (file)
                (and (orgx-denote--file-id file)
                     (seq-every-p (lambda (tag)
                                    (member tag (denote-retrieve-filename-keywords-as-list file)))
                                  tags)
                     (orgx-denote--has-open-items-p file keywords)))
              files))

(defun orgx-denote--peer-linked-ids ()
  "Return denote IDs linked from headings that would peer new children at point.
When point is on a heading these are its direct children; otherwise
they are the top-level headings of the buffer."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((heading-p (org-at-heading-p))
             (level (if heading-p (1+ (org-outline-level)) 1))
             (end (if heading-p (save-excursion (org-end-of-subtree t t)) (point-max)))
             (ids nil)
             (continue (if heading-p
                           (progn (forward-line 1)
                                  (if (org-at-heading-p) t (outline-next-heading)))
                         (goto-char (point-min))
                         (if (org-at-heading-p) t (outline-next-heading)))))
        (while (and continue (< (point) end))
          (when (= (org-current-level) level)
            (when-let* ((prop-id (org-entry-get (point) "DENOTE_ID")))
              (push prop-id ids))
            (let* ((title (org-get-heading t t t t))
                   (links (org-element-map
                              (org-element-parse-secondary-string title '(link))
                              'link #'identity)))
              (dolist (l links)
                (let ((raw (or (org-element-property :path l)
                               (org-element-property :raw-link l))))
                  (when (string-match "\\(?:denote:\\)?\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)" raw)
                    (push (match-string 1 raw) ids))))))
          (setq continue (outline-next-heading)))
        (delete-dups (nreverse ids))))))

(defun orgx-denote--existing-done-or-archived-ids ()
  "Return denote IDs of any headings in current buffer that are DONE or archived."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((ids nil)
            (done-kws (or (bound-and-true-p org-done-keywords) '("DONE" "CANCELLED"))))
        (while (not (eobp))
          (when (org-at-heading-p)
            (let* ((done-p (or (org-entry-is-done-p)
                               (member (org-get-todo-state) done-kws)))
                   (archived-p (or (and (fboundp 'org-in-archived-heading-p)
                                        (org-in-archived-heading-p))
                                   (member org-archive-tag (org-get-tags))))
                   (id (org-entry-get (point) "DENOTE_ID")))
              (unless id
                (let ((title (org-get-heading t t t t)))
                  (when (and title (string-match "\\(?:denote:\\)?\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)" title))
                    (setq id (match-string 1 title)))))
              (when (and id (or done-p archived-p))
                (push id ids))))
          (unless (outline-next-heading)
            (goto-char (point-max))))
        (delete-dups (nreverse ids))))))

(defun orgx-denote--target-marker (target)
  "Return a marker at the heading of refile TARGET, or at the start of its file."
  (with-current-buffer (find-file-noselect (nth 1 target))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (or (nth 3 target) (point-min)))
        (point-marker)))))

(defun orgx-denote--unlinked-files (files linked)
  "Return FILES to capture, given the denote IDs LINKED already under the target.
Notes in LINKED are dropped, kept, or chosen per `orgx-denote-capture-dedupe'."
  (let ((dups (seq-filter (lambda (file) (member (orgx-denote--file-id file) linked))
                          files)))
    (pcase (if dups orgx-denote-capture-dedupe 'include)
      ('include files)
      ('skip (seq-difference files dups))
      (_ (pcase (completing-read
                 (format "%d note(s) already linked here: " (length dups))
                 '("skip" "include" "choose") nil t nil nil "skip")
           ("include" files)
           ("choose"
            (let ((readd (completing-read-multiple
                          "Add anyway: " (seq-map #'file-name-nondirectory dups))))
              (seq-difference
               files
               (seq-remove (lambda (file) (member (file-name-nondirectory file) readd))
                           dups))))
           (_ (seq-difference files dups)))))))

(defun orgx-denote--link-headings (files)
  "Return org text with one top-level `denote:' link heading per file in FILES.
Brackets are stripped from titles and `%' is escaped for `org-capture'.
Sets the DENOTE_ID property for idempotency."
  (string-join
   (seq-map (lambda (file)
              (let ((id (orgx-denote--file-id file))
                    (title (thread-last
                             (orgx-denote--file-title file)
                             (replace-regexp-in-string "[][]" "")
                             (replace-regexp-in-string "%" "\\\\%"))))
                (format "* [[denote:%s][%s]]\n:PROPERTIES:\n:DENOTE_ID: %s\n:END:"
                        id title id)))
            files)
   "\n"))

;;;###autoload
(defun orgx-denote-capture-open-items (&optional files)
  "Capture one linked heading per denote note that has open TODO items.
FILES defaults to every active note, filtered by tags prompted for.
When FILES is given, as from a `denote-dash' view, it is used as is.
Prompts for which TODO keywords count as open, then for a target
heading via `org-refile'.  Notes already linked from a child of the
target are handled per `orgx-denote-capture-dedupe'.  The headings land
in an `org-capture' buffer under the target."
  (interactive)
  (let* ((candidates (or files (orgx-denote-active-files)))
         (tags (unless files
                 (completing-read-multiple
                  "Tags (empty for any): "
                  (delete-dups (seq-mapcat #'denote-retrieve-filename-keywords-as-list candidates)))))
         (known (with-temp-buffer
                  (org-mode)
                  (seq-remove (lambda (kw) (member kw org-done-keywords)) org-todo-keywords-1)))
         (keywords (or (seq-remove #'string-empty-p
                                   (completing-read-multiple "TODO keywords (empty for all open): " known nil t))
                       known))
         (open-files (orgx-denote--open-note-files candidates tags keywords)))
    (when (null open-files)
      (user-error "No notes with open items matching %s" (string-join keywords ", ")))
    (let* ((marker (orgx-denote--target-marker
                    (org-refile-get-location "Capture open denote items under")))
           (target-buf (marker-buffer marker))
           (done-or-archived-ids
            (with-current-buffer target-buf
              (orgx-denote--existing-done-or-archived-ids)))
           (active-files (seq-remove (lambda (file)
                                       (member (orgx-denote--file-id file)
                                               done-or-archived-ids))
                                     open-files))
           (kept (orgx-denote--unlinked-files
                  active-files
                  (with-current-buffer target-buf
                    (save-excursion
                      (goto-char marker)
                      (orgx-denote--peer-linked-ids))))))
      (when (null kept)
        (user-error "Every matching note is already linked or done/archived under the target"))
      (let ((org-capture-templates
             `(("o" "denote open items" entry
                (function ,(lambda ()
                             (set-buffer (marker-buffer marker))
                             (goto-char marker)))
                ,(orgx-denote--link-headings kept)
                :empty-lines 0))))
        (org-capture nil "o")))))

(autoload 'orgx-refile-denote "orgx-refile-denote" nil t)

;;;###autoload
(defalias 'orgx-denote-refile #'orgx-refile-denote)

(defalias 'org-refile-to-denote #'orgx-refile-denote)
(defalias 'denote-org-refile-to-denote #'orgx-refile-denote)
(defalias 'orgx-refile-to-denote #'orgx-refile-denote)

(provide 'orgx-denote)
;;; orgx-denote.el ends here
