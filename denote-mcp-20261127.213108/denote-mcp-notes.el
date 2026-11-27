;;; denote-mcp-notes.el --- Plan, report and tracker notes for denote-mcp -*- lexical-binding: t; -*-

;; Author: Sam Kleinman <sam@tychoish.com>
;; Maintainer: Sam Kleinman <sam@tychoish.com>
;; Keywords: tools, mcp, denote, notes

;;; Commentary:
;;
;; Typed agent notes -- plans, reports and trackers -- on the `denote'
;; service.  `denote-mcp-notes-create' backs the `type' argument of
;; `denote_create_note'; this file registers the lifecycle tools that work
;; on plans and trackers alike: `denote_update_note', `denote_next_tasks'
;; and `denote_close_note'.
;;
;; The mutation helpers live in denote-mcp-common.el (frontmatter, drawers,
;; questions), denote-mcp-plan.el (plans) and denote-mcp-tracker.el
;; (trackers).  A plan or tracker also gets a standing TODO in agentic.org
;; when `orgx-mcp' is available.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'denote)
(require 'mcpkit)
(require 'denote-mcp)
(require 'denote-mcp-common)
(require 'denote-mcp-plan)
(require 'denote-mcp-tracker)

(declare-function orgx-mcp-capture-run "orgx-mcp" (target heading &optional body-str clock-in-p))
(declare-function orgx-mcp-capture-target "orgx-mcp" (keys))

(defconst denote-mcp-notes-types
  '((plan :tags ("agent" "plan") :template plan :capture t)
    (report :tags ("agent" "report") :template nil :capture nil)
    (tracker :tags ("agent" "tracker") :template tracker :capture t))
  "Typed notes: tags, denote template symbol, and whether to capture a TODO.")

(defun denote-mcp-notes--capture (id title)
  "Capture a TODO linking denote ID (titled TITLE) to agentic.org.
Return t, or an error string when capture is unavailable or fails."
  (if (not (and (require 'orgx-mcp nil t) (fboundp 'orgx-mcp-capture-run)))
      "orgx-mcp is not available"
    (condition-case err
        (progn (orgx-mcp-capture-run (orgx-mcp-capture-target '("att" "at"))
                                     (format "TODO [[denote:%s][%s]]" id title))
               t)
      (error (error-message-string err)))))

;;;###autoload
(defun denote-mcp-notes-create (args)
  "Create a typed note from the `denote_create_note' tool ARGS plist.
ARGS has :type (plan, report or tracker), :title and optional :content,
:file_type and :sequence_or_parent (a parent signature gets its next
child, an unused signature is assigned as given)."
  (let* ((type (intern (plist-get args :type)))
         (spec (or (alist-get type denote-mcp-notes-types)
                   (user-error "Unknown note type %S (plan, report or tracker)" type)))
         (tags (plist-get spec :tags))
         (title (plist-get args :title))
         (content (plist-get args :content))
         (ft-str (plist-get args :file_type))
         (ft (if (and ft-str (string-prefix-p "markdown" ft-str)) 'markdown-yaml 'org))
         (denote-directory (denote-mcp-get-directory))
         (captured nil) file-path id)
    (denote-mcp-mutating
      (save-window-excursion
        (denote title tags ft nil nil (plist-get spec :template))
        (when (eq ft 'org) (denote-mcp-common--normalize-frontmatter))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (when content (insert content))
        (when (eq ft 'org) (denote-mcp-common--normalize-property-drawers))
        (save-buffer)
        (setq file-path (denote-mcp-common--place-in-sequence
                         (buffer-file-name) (plist-get args :sequence_or_parent) tags))
        (setq denote-mcp-common--last-created-file file-path
              denote-mcp-tracker--last-created-file
              (if (eq type 'tracker) file-path denote-mcp-tracker--last-created-file)
              id (and (string-match "\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)"
                                    (file-name-nondirectory file-path))
                      (match-string 1 (file-name-nondirectory file-path))))
        (when (plist-get spec :capture)
          (setq captured (denote-mcp-notes--capture (or id (file-name-base file-path)) title)))))
    (append (list :status "ok" :path file-path :id (or id "") :title title :type (symbol-name type))
            (when (plist-get spec :capture)
              (list :todo_captured (if (eq captured t) t :json-false)
                    :todo_error (if (stringp captured) captured :null))))))

(defun denote-mcp-notes--file (target)
  "Resolve TARGET (path, denote ID or slug) to an existing note file."
  (or (denote-mcp--find-file target)
      (user-error "Note not found: %s" target)))

(defun denote-mcp-notes--add-tasks (file tasks)
  "Add tracker TASKS (plists) to FILE; return the generated :ID: strings."
  (mapcar
   (lambda (task)
     (let ((id (denote-mcp-tracker-next-id file (plist-get task :id_hint)))
           (severity (plist-get task :severity)))
       (denote-mcp-tracker-add-task
        file (plist-get task :category) id (plist-get task :title)
        (or (plist-get task :status) "TODO") (or (plist-get task :priority) "B")
        (when severity (list (cons "SEVERITY" severity)))
        (plist-get task :body))
       id))
   tasks))

(mcpkit-register-tool 'denote_update_note 'denote
  :description "Apply any combination of tracker task additions, task status changes, result journal entries, notes, decisions, question answers and an execution status to a plan OR tracker note in ONE call. Additions run first and return their generated :ID:s. Status changes re-derive a plan's Execution Plan table from the headings. Returns remaining open task counts; when every task of a plan is done and mark_executed is true the note is tagged x (renamed) automatically. Trackers are never auto-tagged."
  :input-schema '(:type "object"
                  :properties (:note (:type "string" :description "Plan or tracker file path, denote ID, or slug")
                               :add_tasks (:type "array" :description "Tracker only: tasks to add"
                                           :items (:type "object"
                                                   :properties (:category (:type "string" :description "Category heading under * Tasks (created if absent)")
                                                                :id_hint (:type "string" :description "Free text slugified into a unique :ID:")
                                                                :title (:type "string")
                                                                :status (:type "string" :description "TODO keyword, default TODO")
                                                                :priority (:type "string" :description "A, B or C; default B")
                                                                :severity (:type "string")
                                                                :body (:type "string"))
                                                   :required ["category" "id_hint" "title"]))
                               :results (:type "array" :description "Tracker only: journal entries under a task"
                                         :items (:type "object"
                                                 :properties (:id (:type "string" :description "Task :ID:")
                                                              :text (:type "string")
                                                              :label (:type "string" :description "Result (default) or Status"))
                                                 :required ["id" "text"]))
                               :transitions (:type "array" :description "Task status changes"
                                             :items (:type "object"
                                                     :properties (:id (:type "string" :description "Task :CUSTOM_ID: (plans) or :ID: (trackers)")
                                                                  :status (:type "string" :description "TODO keyword: INPROGRESS, DONE, BLOCKED, CANCELED, ..."))
                                                     :required ["id" "status"]))
                               :notes (:type "array" :description "Paragraphs to add under a heading"
                                       :items (:type "object"
                                               :properties (:heading (:type "string" :description "Regexp matching the heading line, e.g. \"^\\\\*\\\\* Assumptions\"")
                                                            :text (:type "string"))
                                               :required ["heading" "text"]))
                               :decisions (:type "array" :description "Design decisions to record"
                                           :items (:type "object"
                                                   :properties (:title (:type "string")
                                                                :text (:type "string")
                                                                :status (:type "string" :description "Default ACCEPTED")
                                                                :under (:type "string" :description "Heading regexp; default is the * Decisions section"))
                                                   :required ["title" "text"]))
                               :answers (:type "array" :description "Questions to answer"
                                         :items (:type "object"
                                                 :properties (:question_title (:type "string")
                                                              :answer_text (:type "string")
                                                              :new_state (:type "string" :description "Default ANSWERED"))
                                                 :required ["question_title" "answer_text"]))
                               :execution_status (:type "string" :description "proposed, in-progress, complete, blocked or abandoned. Marking complete needs user confirmation first; prefer denote_close_note.")
                               :mark_executed (:type "boolean" :description "Tag x when all plan tasks are done (default false)"))
                  :required ["note"])
  (let* ((file (denote-mcp-notes--file (plist-get args :note)))
         (added (denote-mcp-notes--add-tasks file (denote-mcp--list (plist-get args :add_tasks)))))
    (dolist (r (denote-mcp--list (plist-get args :results)))
      (denote-mcp-tracker-append-result file (plist-get r :id) (plist-get r :text) (plist-get r :label)))
    (let ((result
           (denote-mcp-plan-update
            file
            :transitions (mapcar (lambda (tr) (cons (plist-get tr :id) (plist-get tr :status)))
                                 (denote-mcp--list (plist-get args :transitions)))
            :notes (mapcar (lambda (n) (cons (plist-get n :heading) (plist-get n :text)))
                           (denote-mcp--list (plist-get args :notes)))
            :decisions (mapcar (lambda (d) (list (plist-get d :title) (plist-get d :text)
                                                 (plist-get d :status) (plist-get d :under)))
                               (denote-mcp--list (plist-get args :decisions)))
            :answers (mapcar (lambda (a) (list (plist-get a :question_title)
                                               (plist-get a :answer_text)
                                               (plist-get a :new_state)))
                             (denote-mcp--list (plist-get args :answers)))
            :execution-status (plist-get args :execution_status)
            :mark-executed (eq (plist-get args :mark_executed) t))))
      (list :status "ok"
            :file (plist-get result :file)
            :added_ids (vconcat added)
            :applied (plist-get result :applied)
            :total (plist-get result :total)
            :open (plist-get result :open)
            :all_done (if (plist-get result :all-done) t :json-false)
            :marked_executed (if (plist-get result :marked) t :json-false)))))

(mcpkit-register-tool 'denote_next_tasks 'denote
  :description "Read a plan's Execution Plan: tasks that are ready now (not done, all dependencies done), the rounds (parallel groups) and status counts. Statuses come from the task headings."
  :input-schema '(:type "object"
                  :properties (:note (:type "string" :description "Plan file path, denote ID, or slug"))
                  :required ["note"])
  (let* ((file (denote-mcp-notes--file (plist-get args :note)))
         (r (denote-mcp-plan-next-tasks file)))
    (list :status "ok" :file file
          :ready (vconcat (plist-get r :ready))
          :rounds (vconcat (mapcar (lambda (rd) (list :round (plist-get rd :round)
                                                      :ids (vconcat (plist-get rd :ids))))
                                   (plist-get r :rounds)))
          :counts (mapcar (lambda (c) (list :status (car c) :n (cdr c)))
                          (plist-get r :counts)))))

(mcpkit-register-tool 'denote_close_note 'denote
  :description "Close a plan or tracker note: answer remaining questions, set execution_status complete, close its agentic.org TODO, tag it x (renames the file). Irreversible-ish and user-visible: only call after the user explicitly confirmed completion, and pass confirmed true."
  :input-schema '(:type "object"
                  :properties (:note (:type "string" :description "Plan or tracker file path, denote ID, or slug")
                               :confirmed (:type "boolean" :description "Must be true: the user confirmed this closeout"))
                  :required ["note" "confirmed"])
  (unless (eq (plist-get args :confirmed) t)
    (user-error "Closing a note needs explicit user confirmation; pass confirmed true only after asking"))
  (let ((new (denote-mcp-plan-close-plan (denote-mcp-notes--file (plist-get args :note)))))
    (list :status "ok" :file new)))

(provide 'denote-mcp-notes)
;;; denote-mcp-notes.el ends here
