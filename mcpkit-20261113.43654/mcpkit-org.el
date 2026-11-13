;;; mcpkit-org.el --- Org manipulation service for mcpkit -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0"))

;;; Commentary:
;; Structured Org insertion for MCP clients and other Elisp callers.
;;; Code:

(require 'cl-lib)
(require 'map)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)
(require 'mcpkit)

(defvar mcpkit-org-service
  (or (mcpkit-get-service 'org)
      (mcpkit-define-service 'org :description "Org document manipulation"))
  "The Org MCP service.")

(mcpkit-register-autostart 'org :on-collision 'namespace)

(cl-defun mcpkit-org-append-entry (file-or-buffer content &key target-heading custom-id
                                                 (position "end_of_subtree")
                                                 headline-title todo-state priority properties)
  "Insert CONTENT in Org FILE-OR-BUFFER, optionally beneath a target.
TARGET-HEADING and CUSTOM-ID select one unique headline.  POSITION is
`end_of_subtree', `beginning_of_subtree', or `as_last_child'.  HEADLINE-TITLE
creates a child heading with optional TODO-STATE, PRIORITY and PROPERTIES.
A file-visiting buffer is saved; another buffer remains modified."
  (unless (and (stringp file-or-buffer) (stringp content))
    (user-error "A target and content are required"))
  (when (and target-heading custom-id)
    (user-error "Specify either target_heading or custom_id"))
  (unless (member position '("end_of_subtree" "beginning_of_subtree" "as_last_child"))
    (user-error "Invalid insertion position: %s" position))
  (when (and (equal position "as_last_child") (not headline-title))
    (user-error "as_last_child requires headline_title"))
  (when (and (or todo-state priority properties) (not headline-title))
    (user-error "Heading metadata requires headline_title"))
  (when (and headline-title (or (string-empty-p headline-title)
                                (string-match-p "[\n\r]" headline-title)))
    (user-error "Invalid headline title"))
  (when (and todo-state (not (string-match-p "\\`[[:alnum:]_-]+\\'" todo-state)))
    (user-error "Invalid TODO state"))
  (when (and priority (not (string-match-p "\\`[A-Za-z0-9]\\'" priority)))
    (user-error "Invalid priority"))
  (let ((enable-local-variables nil)
        (enable-local-eval nil)
        (large-file-warning-threshold nil)
        (coding-system-for-read nil))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (user-error "Operation requires confirmation")))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _) (user-error "Operation requires confirmation"))))
      (with-current-buffer (or (get-buffer file-or-buffer)
                             (find-file-noselect (expand-file-name file-or-buffer)))
      (unless (derived-mode-p 'org-mode)
        (user-error "Target is not an Org buffer"))
      (org-with-wide-buffer
       (let ((matches nil)
             (case-fold-search t)
             (level 1))
         (when (or target-heading custom-id)
           (goto-char (point-min))
           (org-map-entries
            (lambda ()
              (when (if custom-id
                        (equal (org-entry-get nil "CUSTOM_ID") custom-id)
                      (string-equal-ignore-case (nth 4 (org-heading-components)) target-heading))
                (push (point) matches))) nil nil)
           (unless (= (length matches) 1)
             (user-error "Target headline must match exactly once (found %d)" (length matches)))
           (goto-char (car matches))
           (setq level (1+ (org-current-level))))
         (atomic-change-group
           (cond
            ((not matches) (goto-char (point-max)))
            ((and (equal position "beginning_of_subtree") (not headline-title))
             (org-end-of-meta-data t))
            ((or (not headline-title) (equal position "beginning_of_subtree"))
             ;; Insert after the parent's own body and before its first child.
             (let ((end (save-excursion (org-end-of-subtree t t) (point))))
               (outline-next-heading)
               (when (> (point) end) (goto-char end))))
            (t (org-end-of-subtree t t)))
           (unless (bolp) (insert "\n"))
           (when headline-title
             (insert (make-string level ?*) " "
                     (if todo-state (concat todo-state " ") "")
                     (if priority (format "[#%s] " priority) "")
                     headline-title "\n")
             (forward-line -1)
             (when properties
               (map-do (lambda (key value)
                         (unless (and (stringp value)
                                      (string-match-p "\\`[[:alnum:]_@#%]+\\'"
                                                      (if (keywordp key) (substring (symbol-name key) 1) key))
                                      (not (string-match-p "[\n\r]" value)))
                           (user-error "Invalid property"))
                         (org-entry-put nil (if (keywordp key) (substring (symbol-name key) 1) key) value))
                       properties))
             (org-end-of-meta-data t))
           (insert content)
           (unless (bolp) (insert "\n"))
           (org-element-cache-reset)
           (when buffer-file-name (save-buffer)))
         (list :status "success" :buffer (buffer-name)
               :file (or buffer-file-name :json-null) :level (if headline-title level :json-null))))))))

(mcpkit-register-tool 'org_append_entry 'org
  :description "Append content or a child headline to an Org document."
  :input-schema '(:type "object"
                  :properties (:file_or_buffer (:type "string")
                               :content (:type "string")
                               :target_heading (:type "string")
                               :custom_id (:type "string")
                               :position (:type "string" :enum ["end_of_subtree" "beginning_of_subtree" "as_last_child"])
                               :headline_title (:type "string")
                               :todo_state (:type "string")
                               :priority (:type "string")
                               :properties (:type "object"))
                  :required ["file_or_buffer" "content"])
  (mcpkit-org-append-entry (plist-get args :file_or_buffer) (plist-get args :content)
                           :target-heading (plist-get args :target_heading)
                           :custom-id (plist-get args :custom_id)
                           :position (or (plist-get args :position) "end_of_subtree")
                           :headline-title (plist-get args :headline_title)
                           :todo-state (plist-get args :todo_state)
                           :priority (plist-get args :priority)
                           :properties (plist-get args :properties)))

(provide 'mcpkit-org)
;;; mcpkit-org.el ends here
