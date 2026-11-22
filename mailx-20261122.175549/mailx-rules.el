;;; mailx-rules.el --- Declarative mail classification rules -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: mail, convenience
;; URL: https://github.com/tychoish/mailx

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Portable rules and per-maildir storage for mail classification.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(defgroup mailx-rules nil
  "Declarative mail classification rules."
  :group 'mailx
  :prefix "mailx-")

(defcustom mailx-rules-filename "tools/mailx-rules.eld"
  "Path relative to mail root for rule storage."
  :type 'string
  :group 'mailx-rules)

(defcustom mailx-trash-folder "/trash"
  "Default maildir folder for junk/trash."
  :type 'string
  :group 'mailx-rules)

(defconst mailx-attention-tiers '(reply read cursory bulk junk)
  "Supported attention tiers in descending order of attention.")

(cl-defstruct (mailx-rule (:constructor make-mailx-rule))
  "A declarative mail classification rule."
  id match folder tier tags expire-days (source 'manual) created)

(defvar mailx--rules-cache (make-hash-table :test #'equal)
  "Cached rules, keyed by canonical mail root.")

(defun mailx-rules-file (root)
  "Return the rule store path for mail ROOT."
  (expand-file-name mailx-rules-filename root))

(defun mailx--rule-plist (rule)
  "Convert RULE to its persistent property list."
  (list :id (mailx-rule-id rule) :match (mailx-rule-match rule)
        :folder (mailx-rule-folder rule) :tier (mailx-rule-tier rule)
        :tags (mailx-rule-tags rule) :expire-days (mailx-rule-expire-days rule)
        :source (mailx-rule-source rule) :created (mailx-rule-created rule)))

(defun mailx--validate-rule (rule)
  "Validate RULE and return it, signalling an error for malformed data."
  (unless (and (mailx-rule-p rule) (symbolp (mailx-rule-id rule))
               (mailx-rule-id rule) (consp (mailx-rule-match rule))
               (or (null (mailx-rule-folder rule))
                   (and (stringp (mailx-rule-folder rule))
                        (string-prefix-p "/" (mailx-rule-folder rule))
                        (not (string-match-p "[\0-\37\177]" (mailx-rule-folder rule)))
                        (not (member ".." (split-string (mailx-rule-folder rule) "/" t)))))
               (or (null (mailx-rule-tier rule))
                   (memq (mailx-rule-tier rule) mailx-attention-tiers))
               (seq-every-p (lambda (tag)
                              (and (stringp tag)
                                   (string-match-p "\\`[^[:space:][:cntrl:],]+\\'" tag)))
                            (mailx-rule-tags rule))
               (or (null (mailx-rule-expire-days rule))
                   (and (integerp (mailx-rule-expire-days rule))
                        (> (mailx-rule-expire-days rule) 0))))
    (error "Invalid mailx rule: %S" rule))
  ;; Validation also checks the shape and regex of the condition.
  (mailx-eval-condition (mailx-rule-match rule) nil)
  rule)

;;;###autoload
(defun mailx-rules-load (root &optional refresh)
  "Load rules for ROOT, reusing unchanged data unless REFRESH is non-nil."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (file (mailx-rules-file root))
         (cached (map-elt mailx--rules-cache root)))
    (with-temp-buffer
      (when (file-exists-p file) (insert-file-contents file))
      ;; Hash content rather than relying on timestamps: editors and sync tools
      ;; can preserve the old mtime even when the contents change.
      (let ((digest (if (file-exists-p file)
                        (secure-hash 'sha256 (current-buffer))
                      'absent)))
        (if (and (not refresh) cached (equal digest (car cached)))
            (cdr cached)
          (goto-char (point-min))
          (let ((rules
                 (unless (eq digest 'absent)
                   (let ((data (read (current-buffer))))
                     (unless (listp data) (error "Invalid rule store"))
                     (seq-map (lambda (entry)
                                (mailx--validate-rule (apply #'make-mailx-rule entry)))
                              data)))))
            (setf (map-elt mailx--rules-cache root) (cons digest rules))
            rules))))))

;;;###autoload
(defun mailx-rules-save (root rules)
  "Validate and atomically save RULES under ROOT."
  (seq-do #'mailx--validate-rule rules)
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (file (mailx-rules-file root)))
    (make-directory (file-name-directory file) t)
    (let ((tmp (make-temp-file (concat file "."))))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (let ((print-length nil) (print-level nil))
                (prin1 (seq-map #'mailx--rule-plist rules) (current-buffer))
                (insert "\n")))
            (rename-file tmp file t)
            (map-delete mailx--rules-cache root))
        (when (file-exists-p tmp) (delete-file tmp))))))

(defun mailx--field-strings (value)
  "Extract searchable strings from message field VALUE."
  (cond
   ((stringp value) (list value))
   ((and (listp value) (keywordp (car value)))
    (delq nil (list (map-elt value :name) (map-elt value :email))))
   ((and (consp value) (or (null (car value)) (stringp (car value)))
         (stringp (cdr value)))
    (delq nil (list (car value) (cdr value))))
   ((listp value) (seq-mapcat #'mailx--field-strings value))
   (t nil)))

(defun mailx-eval-condition (condition msg)
  "Evaluate declarative CONDITION against message property list MSG."
  (unless (consp condition) (error "Invalid condition: %S" condition))
  (let ((op (car condition)) (args (cdr condition)))
    (pcase op
      ((or :and :or)
       (unless args (error "Empty boolean condition"))
       ;; Evaluate all operands, including those after a decisive operand, so
       ;; malformed rules never become valid merely because of short circuiting.
       (let ((values (seq-map (lambda (arg) (mailx-eval-condition arg msg)) args)))
         (if (eq op :and) (seq-every-p #'identity values) (seq-some #'identity values))))
      (:not
       (unless (= (length args) 1) (error "Invalid :not condition"))
       (not (mailx-eval-condition (car args) msg)))
      (:has-unsubscribe
       (unless (null args) (error "Invalid :has-unsubscribe condition"))
       (or (map-elt msg :list) (map-elt msg :list-unsubscribe)))
      ((or :from :to :delivered-to :list :subject :maildir :tag)
       (unless (and (= (length args) 1) (stringp (car args)))
         (error "Invalid %s condition" op))
       (let* ((pattern (car args))
              (fields (pcase op
                        (:to '(:to :cc))
                        (:delivered-to '(:delivered-to :x-forwarded-to))
                        (:tag '(:tags))
                        (_ (list op))))
              (case-fold-search t))
         (unless (eq op :tag) (string-match-p pattern ""))
         (seq-some (lambda (field)
                     (seq-some (lambda (value)
                                 (if (eq op :tag)
                                     (equal pattern value)
                                   (string-match-p pattern value)))
                               (mailx--field-strings (map-elt msg field))))
                   fields)))
      (_ (error "Unknown mailx condition: %S" op)))))

(defun mailx-classify-by-rules (msg rules)
  "Return first of RULES matching MSG, or nil."
  (seq-find (lambda (rule) (mailx-eval-condition (mailx-rule-match rule) msg)) rules))

(defun mailx--mu-find-output (root query)
  "Return mu sexp output for QUERY under ROOT, rejecting query errors."
  (let ((stderr-file (make-temp-file "mailx-mu-stderr-"))
        (process-environment (cons "LC_ALL=C" process-environment)))
    (unwind-protect
        (with-temp-buffer
          (let* ((status (process-file "mu" nil (list (current-buffer) stderr-file) nil
                                       "find" "--muhome" (expand-file-name ".mu" root)
                                       "--format=sexp" query))
                 (output (buffer-string))
                 (diagnostic (with-temp-buffer
                               (insert-file-contents stderr-file)
                               (string-trim (buffer-string)))))
            (cond
             ((equal status 0) output)
             ((and (equal status 2) (string-empty-p output)
                   (equal diagnostic "no matches for search expression")) "")
             (t (error "mu find failed (%s): %s" status diagnostic)))))
      (delete-file stderr-file))))

(defun mailx-test-rule (root rule)
  "Count matches for RULE in ROOT's mu database using local evaluation.
Return a plist with :count and :subjects.  Some raw headers are not
included in mu's sexp output; refuse conditions requiring them."
  (mailx--validate-rule rule)
  (let ((condition (mailx-rule-match rule)))
    (when (seq-some (lambda (op) (memq op (flatten-tree condition)))
                    '(:delivered-to :has-unsubscribe))
      (error "Cannot preview conditions requiring unindexed headers")))
  (with-temp-buffer
    (insert (mailx--mu-find-output root ""))
    (let ((count 0)
          (subjects nil))
      (goto-char (point-min))
      (while (progn (skip-chars-forward " \t\r\n") (not (eobp)))
        (let ((msg (read (current-buffer))))
          (when (mailx-eval-condition (mailx-rule-match rule) msg)
            (setq count (1+ count))
            (when (< (length subjects) 5)
              (push (or (map-elt msg :subject) "") subjects)))))
      (list :count count :subjects (nreverse subjects)))))

(defun mailx-condition-to-mu-query (condition)
  "Compile CONDITION to a mu query string, rejecting unsupported translations."
  (unless (consp condition)
    (error "Invalid condition: %S" condition))
  (let ((op (car condition))
        (args (cdr condition)))
    (pcase op
      ((or :and :or)
       (unless args (error "Empty boolean condition: %S" condition))
       (let ((subqueries (seq-map #'mailx-condition-to-mu-query args))
             (sep (if (eq op :and) " AND " " OR ")))
         (concat "(" (mapconcat #'identity subqueries sep) ")")))
      (:not
       (unless (= (length args) 1) (error "Invalid :not condition: %S" condition))
       (concat "NOT (" (mailx-condition-to-mu-query (car args)) ")"))
      ((or :from :to :list :subject :maildir :tag)
       (unless (and (= (length args) 1) (stringp (car args)))
         (error "Invalid %s condition: %S" op condition))
       (let ((term (car args)))
         ;; Term must be a safe plain term without regex metacharacters
         (unless (string-match-p "\\`[[:alnum:]@._+-]+\\'" term)
           (error "Condition cannot be translated safely to a mu query: %S" condition))
         (pcase op
           (:from (format "from:%s" term))
           (:to (format "(to:%s OR cc:%s)" term term))
           (:list (format "list:%s" term))
           (:subject (format "subject:%s" term))
           (:maildir (format "maildir:%s" term))
           (:tag (format "tag:%s" term)))))
      (_ (error "Unsupported mu query condition: %S" condition)))))

(defun mailx--condition-to-procmail (condition)
  "Convert CONDITION to a list of procmail condition strings.
The returned strings omit the leading `* `.
Signal an error if CONDITION cannot be safely translated to procmail."
  (unless (consp condition)
    (error "Invalid condition: %S" condition))
  (let ((op (car condition))
        (args (cdr condition)))
    (when (and (car args) (stringp (car args))
               (string-match-p "[\r\n]" (car args)))
      (error "Condition contains unsafe newline for procmail export: %S" condition))
    (pcase op
      (:and
       (unless args (error "Empty boolean condition"))
       (seq-mapcat #'mailx--condition-to-procmail args))
      (:not
       (unless (= (length args) 1) (error "Invalid :not condition"))
       (let ((sub (mailx--condition-to-procmail (car args))))
         (unless (= (length sub) 1)
           (error "Cannot invert compound condition in procmail: %S" condition))
         (list (concat "! " (car sub)))))
      (:from
       (unless (= (length args) 1) (error "Invalid :from condition"))
       (list (format "^From:.*%s" (car args))))
      (:to
       (unless (= (length args) 1) (error "Invalid :to condition"))
       (list (format "^TO_%s" (car args))))
      (:delivered-to
       (unless (= (length args) 1) (error "Invalid :delivered-to condition"))
       (list (format "^(Delivered-To|X-Forwarded-To):.*%s" (car args))))
      (:list
       (unless (= (length args) 1) (error "Invalid :list condition"))
       (list (format "^(List-Id|X-Mailing-List):.*%s" (car args))))
      (:subject
       (unless (= (length args) 1) (error "Invalid :subject condition"))
       (list (format "^Subject:.*%s" (car args))))
      (:has-unsubscribe
       (list "^List-(Unsubscribe|Id):.*"))
      ((or :maildir :tag)
       (error "Condition cannot be evaluated at delivery time: %S" condition))
      (_ (error "Unsupported procmail condition: %S" condition)))))

(defun mailx--rule-to-procmail (rule &optional mail-dir-var)
  "Convert a single RULE to a procmail recipe block.
MAIL-DIR-VAR is the procmail variable holding the maildir root, default \"$MAIL\"."
  (let* ((conditions (mailx--condition-to-procmail (mailx-rule-match rule)))
         (folder (mailx-rule-folder rule))
         (tier (mailx-rule-tier rule))
         (tags (mailx-rule-tags rule))
         (var (or mail-dir-var "$MAIL"))
         (cond-lines (mapconcat (lambda (c) (format "* %s\n" c)) conditions ""))
         (labels (append (when tier (list (format "tier:%s" tier))) tags)))
    (cond
     ((or (equal folder mailx-trash-folder) (and (null folder) (eq tier 'junk)))
      (format ":0:\n%s/dev/null\n" cond-lines))
     ((and folder (not (string-empty-p folder)))
      (let ((dest (format "%s%s/" var folder)))
        (if labels
            (format ":0\n%s{\n  :0 fhw\n  | formail -I \"X-Label: %s\"\n\n  :0:\n  %s\n}\n"
                    cond-lines
                    (mapconcat #'identity labels " ")
                    dest)
          (format ":0:\n%s%s\n" cond-lines dest))))
     (labels
      (format ":0 fhw\n%s| formail -I \"X-Label: %s\"\n"
              cond-lines
              (mapconcat #'identity labels " ")))
     (t (error "Rule has neither destination folder nor tags: %S" rule)))))

;;;###autoload
(defun mailx-export-procmail (rules &optional mail-dir-var)
  "Export RULES as a string of procmail recipes.
Reject export if RULES is empty or if any rule cannot be safely translated.
MAIL-DIR-VAR is the variable for the mail directory (default \"$MAIL\")."
  (unless (and (consp rules) (seq-every-p #'mailx-rule-p rules))
    (error "RULES must be a non-empty list of mailx-rule structs"))
  (mapconcat (lambda (rule) (mailx--rule-to-procmail rule mail-dir-var)) rules "\n"))

(defun mailx--condition-to-fdm (condition)
  "Convert CONDITION to an fdm match expression string.
Signal an error if CONDITION cannot be safely translated to fdm."
  (unless (consp condition)
    (error "Invalid condition: %S" condition))
  (let ((op (car condition))
        (args (cdr condition)))
    (when (and (car args) (stringp (car args))
               (string-match-p "[\"\r\n]" (car args)))
      (error "Condition contains unsafe character for fdm export: %S" condition))
    (pcase op
      (:and
       (unless args (error "Empty boolean condition"))
       (mapconcat #'mailx--condition-to-fdm args " and "))
      (:or
       (unless args (error "Empty boolean condition"))
       (concat "(" (mapconcat #'mailx--condition-to-fdm args " or ") ")"))
      (:not
       (unless (= (length args) 1) (error "Invalid :not condition"))
       (concat "not " (mailx--condition-to-fdm (car args))))
      (:from
       (unless (= (length args) 1) (error "Invalid :from condition"))
       (format "match \"^From:.*%s\" in headers" (car args)))
      (:to
       (unless (= (length args) 1) (error "Invalid :to condition"))
       (format "match \"^(To|Cc):.*%s\" in headers" (car args)))
      (:delivered-to
       (unless (= (length args) 1) (error "Invalid :delivered-to condition"))
       (format "match \"^(Delivered-To|X-Forwarded-To):.*%s\" in headers" (car args)))
      (:list
       (unless (= (length args) 1) (error "Invalid :list condition"))
       (format "match \"^(List-Id|X-Mailing-List):.*%s\" in headers" (car args)))
      (:subject
       (unless (= (length args) 1) (error "Invalid :subject condition"))
       (format "match \"^Subject:.*%s\" in headers" (car args)))
      (:has-unsubscribe
       "match \"^List-(Unsubscribe|Id):.*\" in headers")
      ((or :maildir :tag)
       (error "Condition cannot be evaluated at delivery time: %S" condition))
      (_ (error "Unsupported fdm condition: %S" condition)))))

(defun mailx--rule-to-fdm (rule &optional mail-dir)
  "Convert RULE to fdm action and match definitions.
MAIL-DIR is the path prefix in fdm, default \"%h/mail\"."
  (let* ((match-str (mailx--condition-to-fdm (mailx-rule-match rule)))
         (folder (mailx-rule-folder rule))
         (tier (mailx-rule-tier rule))
         (tags (mailx-rule-tags rule))
         (mdir (or mail-dir "%h/mail"))
         (id-str (symbol-name (mailx-rule-id rule)))
         (labels (append (when tier (list (format "tier:%s" tier))) tags)))
    (cond
     ((or (equal folder mailx-trash-folder) (and (null folder) (eq tier 'junk)))
      (format "%s action drop\n" match-str))
     ((and folder (not (string-empty-p folder)))
      (let ((act-name (format "act-%s" id-str))
            (dest (format "%s%s" mdir folder)))
        (if labels
            (format "action \"%s\" {\n  add-header \"X-Label\" \"%s\"\n  maildir \"%s\"\n}\n%s action \"%s\"\n"
                    act-name
                    (mapconcat #'identity labels " ")
                    dest
                    match-str
                    act-name)
          (format "action \"%s\" maildir \"%s\"\n%s action \"%s\"\n"
                  act-name
                  dest
                  match-str
                  act-name))))
     (t (error "Rule has neither destination folder nor tags: %S" rule)))))

;;;###autoload
(defun mailx-export-fdm (rules &optional mail-dir)
  "Export RULES as an fdm configuration string.
Reject export if RULES is empty or if any rule cannot be safely translated."
  (unless (and (consp rules) (seq-every-p #'mailx-rule-p rules))
    (error "RULES must be a non-empty list of mailx-rule structs"))
  (mapconcat (lambda (rule) (mailx--rule-to-fdm rule mail-dir)) rules "\n"))

;;;###autoload
(defun mailx-rules-export-all (root)
  "Export all rules in ROOT to procmail and fdm config files.
Writes to <root>/tools/mailx-generated.procmailrc and <root>/tools/fdm.conf."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (rules (mailx-rules-load root))
         (tools-dir (expand-file-name "tools" root))
         (procmail-file (expand-file-name "mailx-generated.procmailrc" tools-dir))
         (fdm-file (expand-file-name "fdm.conf" tools-dir)))
    (make-directory tools-dir t)
    (when rules
      (with-temp-file procmail-file
        (insert "# mailx-generated.procmailrc --- generated by mailx. DO NOT EDIT.\n\n")
        (insert (mailx-export-procmail rules))
        (insert "\n"))
      (with-temp-file fdm-file
        (insert "# fdm.conf --- generated by mailx. DO NOT EDIT.\n\n")
        (insert (mailx-export-fdm rules))
        (insert "\n")))
    (list :procmail procmail-file :fdm fdm-file :count (length rules))))

(defcustom mailx-import-folder-tier-map
  '(("/archive" . (read nil))
    ("/trash" . (junk nil)))
  "Default mapping from folder to (TIER TAGS) for imported rules."
  :type '(alist :key-type string :value-type (list symbol (repeat symbol)))
  :group 'mailx-rules)

(defun mailx--sanitize-import-pattern (pattern)
  "Sanitize PATTERN from procmail regex to valid Emacs regexp."
  (let ((pat (string-trim pattern)))
    (setq pat (string-trim pat "[\"']+" "[\"']+"))
    (while (string-match-p "\\`\\*+" pat)
      (setq pat (replace-regexp-in-string "\\`\\*+" ".*" pat)))
    (condition-case nil
        (progn (string-match-p pat "") pat)
      (error
       (regexp-quote pat)))))

(defun mailx--parse-procmail-condition (line)
  "Parse a single procmail condition LINE into a mailx condition."
  (let ((trimmed (string-trim line)))
    (if (string-prefix-p "! " trimmed)
        (list :not (mailx--parse-procmail-condition (substring trimmed 2)))
      (cond
       ((string-match "\\`\\(?:\\^\\)?From:?[.*]*\\(.*\\)\\'" trimmed)
        (list :from (mailx--sanitize-import-pattern (match-string 1 trimmed))))
       ((string-match "\\`\\(?:\\^\\)?from:?[.*]*\\(.*\\)\\'" trimmed)
        (list :from (mailx--sanitize-import-pattern (match-string 1 trimmed))))
       ((string-match "\\`\\(?:\\^\\)?TO_?[ \t]*\\(.*\\)\\'" trimmed)
        (list :to (mailx--sanitize-import-pattern (match-string 1 trimmed))))
       ((string-match "\\`\\(?:\\^\\)?To:?[.*]*\\(.*\\)\\'" trimmed)
        (list :to (mailx--sanitize-import-pattern (match-string 1 trimmed))))
       ((string-match "\\`\\(?:\\^\\)?Subject:?[.*]*\\(.*\\)\\'" trimmed)
        (list :subject (mailx--sanitize-import-pattern (match-string 1 trimmed))))
       ((string-match "\\`\\(?:\\^\\)?Reply-[tT]o:?[.*]*\\(.*\\)\\'" trimmed)
        (list :from (mailx--sanitize-import-pattern (match-string 1 trimmed))))
       ((string-match "\\`\\(?:\\^\\)?\\(?:List-Id\\|X-Mailing-List\\):?[.*]*\\(.*\\)\\'" trimmed)
        (list :list (mailx--sanitize-import-pattern (match-string 1 trimmed))))
       (t
        (list :from (mailx--sanitize-import-pattern trimmed)))))))

;;;###autoload
(defun mailx-import-procmail-file (file &optional tier-map)
  "Import procmail recipes from FILE into a list of `mailx-rule' structs.
TIER-MAP is an optional alist mapping folder to (TIER TAGS)."
  (unless (file-exists-p file)
    (error "File does not exist: %s" file))
  (let* ((map (or tier-map mailx-import-folder-tier-map))
         (base (file-name-sans-extension (file-name-nondirectory file)))
         (rules nil)
         (index 0))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^:0.*$" nil t)
        (let ((conds nil)
              (dest nil))
          (forward-line 1)
          (while (and (not (eobp)) (looking-at "^\\*\\s-*\\(.*\\)$"))
            (push (mailx--parse-procmail-condition (match-string 1)) conds)
            (forward-line 1))
          (while (and (not (eobp)) (looking-at "^\\s-*\\(?:#.*\\)?$"))
            (forward-line 1))
          (when (and (not (eobp)) (looking-at "^\\([^#\n{].*\\)$"))
            (setq dest (string-trim (match-string 1))))
          (when (and conds dest)
            (setq conds (nreverse conds))
            (let* ((condition (if (= (length conds) 1) (car conds) (cons :and conds)))
                   (folder (cond
                            ((string-prefix-p "/dev/null" dest) mailx-trash-folder)
                            ((string-match "\\$MAIL/\\([^/]+\\)" dest)
                             (format "/%s" (downcase (match-string 1 dest))))
                            (t nil)))
                   (tier-info (when folder (map-elt map folder)))
                   (tier (if (equal folder mailx-trash-folder) 'junk (or (car tier-info) 'read)))
                   (tags (cadr tier-info))
                   (rule-id (intern (format "%s-%d" base (setq index (1+ index))))))
              (when folder
                (condition-case _err
                    (let ((rule (make-mailx-rule :id rule-id
                                                 :match condition
                                                 :folder (unless (equal folder mailx-trash-folder) folder)
                                                 :tier tier
                                                 :tags tags
                                                 :source 'imported
                                                 :created (format-time-string "%Y-%m-%d"))))
                      (mailx--validate-rule rule)
                      (push rule rules))
                  (error nil))))))))
    (nreverse rules)))

;;;###autoload
(defun mailx-import-procmail-directory (dir &optional files tier-map)
  "Import all procmail filter files in DIR.
FILES is an optional list of relative filenames to import in order.
If nil, imports all files matching `filter-*' in DIR."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (file-list (or files
                        (sort (file-expand-wildcards (expand-file-name "filter-*" dir))
                              #'string-lessp)))
         (all-rules nil)
         (seen-ids (make-hash-table :test #'eq)))
    (seq-do
     (lambda (f)
       (let* ((filepath (if (file-name-absolute-p f) f (expand-file-name f dir)))
              (rules (when (file-exists-p filepath)
                       (mailx-import-procmail-file filepath tier-map))))
         (seq-do
          (lambda (rule)
            (let ((id (mailx-rule-id rule)))
              (when (map-elt seen-ids id)
                (let ((counter 1)
                      (new-id id))
                  (while (map-elt seen-ids new-id)
                    (setq new-id (intern (format "%s-%d" id counter)))
                    (setq counter (1+ counter)))
                  (setf (mailx-rule-id rule) new-id)))
              (setf (map-elt seen-ids (mailx-rule-id rule)) t)
              (push rule all-rules)))
          rules)))
     file-list)
    (nreverse all-rules)))

(provide 'mailx-rules)
;;; mailx-rules.el ends here
