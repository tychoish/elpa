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

(declare-function mailx-current-maildir "mailx" ())
(declare-function mu4e-message-at-point "mu4e-message" (&optional noerror))

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
    (if (mailx-rule-p rule)
        (error "Invalid mailx rule %s: check id, folder (leading slash, no ..), tier, tags and expire-days"
               (mailx-rule-id rule))
      (error "Invalid mailx rule: %S" rule)))
  ;; Validation also checks the shape and regex of the condition.
  (mailx-eval-condition (mailx-rule-match rule) nil)
  rule)

(defun mailx-regexp-risky-p (pattern)
  "Return non-nil if PATTERN has a quantified group containing a quantifier.
This is a heuristic for patterns such as \\(a*\\)* or \\(a+\\)+ that can
backtrack catastrophically and freeze Emacs.  It is not a guarantee: other
slow patterns exist and no time limit is enforced."
  (let ((i 0)
        (n (length pattern))
        (stack (list nil))
        (risky nil))
    (while (and (< i n) (not risky))
      (let ((c (aref pattern i)))
        (cond
         ((and (eq c ?\\) (< (1+ i) n))
          (let ((d (aref pattern (1+ i))))
            (setq i (+ i 2))
            (cond
             ((eq d ?\() (push nil stack))
             ((and (eq d ?\)) (cdr stack))
              (let ((inner (pop stack))
                    (quantified (or (and (< i n) (memq (aref pattern i) '(?* ?+)))
                                    (string-prefix-p "\\{" (substring pattern i)))))
                (when (and quantified inner) (setq risky t))
                (when (or quantified inner) (setcar stack t))))
             ((eq d ?{) (setcar stack t)))))
         ((eq c ?\[)
          (setq i (1+ i))
          (when (and (< i n) (eq (aref pattern i) ?^)) (setq i (1+ i)))
          (when (and (< i n) (eq (aref pattern i) ?\])) (setq i (1+ i)))
          (while (and (< i n) (not (eq (aref pattern i) ?\])))
            (if (and (eq (aref pattern i) ?\[) (< (1+ i) n) (eq (aref pattern (1+ i)) ?:)
                     (string-match ":\\]" pattern i))
                (setq i (match-end 0))
              (setq i (1+ i))))
          (setq i (1+ i)))
         (t
          (when (memq c '(?* ?+)) (setcar stack t))
          (setq i (1+ i))))))
    risky))

(defun mailx--condition-regexps (condition)
  "Return the regexp strings used by CONDITION."
  (pcase condition
    (`(,(or :and :or :not) . ,args) (seq-mapcat #'mailx--condition-regexps args))
    (`(,(or :from :to :delivered-to :list :subject :maildir) ,(and (pred stringp) pat))
     (list pat))
    (_ nil)))

(defun mailx-check-rule-regexps (condition)
  "Signal a `user-error' if CONDITION contains a risky regexp.
Use for new rules; see `mailx-regexp-risky-p' for the heuristic."
  (seq-do (lambda (pattern)
            (when (mailx-regexp-risky-p pattern)
              (user-error "Regexp %S may backtrack catastrophically (nested quantifier)"
                          pattern)))
          (mailx--condition-regexps condition))
  nil)

(defun mailx--parse-rules-buffer (file)
  "Parse and validate the rules in the current buffer, read from FILE.
Signal a `user-error' naming FILE, the entry index and rule id on failure."
  (let ((data (condition-case err
                  (prog1 (read (current-buffer))
                    (skip-chars-forward " \t\r\n")
                    (unless (eobp)
                      (error "unexpected data after the rule list")))
                (error (user-error "Cannot read rules file %s: %s"
                                   file (error-message-string err)))))
        (seen (make-hash-table :test #'eq))
        (index -1))
    (unless (listp data)
      (user-error "Invalid rules file %s: not a list of rules" file))
    (seq-map
     (lambda (entry)
       (setq index (1+ index))
       (let* ((id (and (consp entry) (plist-get entry :id)))
              (rule (condition-case err
                        (mailx--validate-rule (apply #'make-mailx-rule entry))
                      (error (user-error "Invalid rule in %s (index %d%s): %s"
                                         file index
                                         (if id (format ", id %s" id) "")
                                         (error-message-string err)))))
              (first (map-elt seen (mailx-rule-id rule))))
         (when first
           (user-error "Duplicate rule id %s in %s (indexes %d and %d)"
                       (mailx-rule-id rule) file first index))
         (setf (map-elt seen (mailx-rule-id rule)) index)
         ;; Existing rules must keep loading: only warn.
         (seq-do (lambda (pattern)
                   (when (mailx-regexp-risky-p pattern)
                     (display-warning
                      'mailx
                      (format "Rule %s in %s (index %d) has a regexp that may backtrack catastrophically: %S"
                              (mailx-rule-id rule) file index pattern))))
                 (mailx--condition-regexps (mailx-rule-match rule)))
         rule))
     data)))

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
                   (mailx--parse-rules-buffer file))))
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
  "Evaluate declarative CONDITION against message property list MSG.
Patterns are Emacs regexps matched case-insensitively.  Emacs cannot
interrupt a regexp match, so a pattern like \\(a*\\)*b can freeze Emacs;
`mailx-regexp-risky-p' is a heuristic check for the common nested-quantifier
form and is not a guarantee."
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

;;;###autoload
(defun mailx-rules-find-file (&optional root)
  "Visit the rules file for mail ROOT, default `mailx-current-maildir'."
  (interactive)
  (find-file (mailx-rules-file (or root (mailx-current-maildir)))))

(defun mailx-explain-rules-text (root msg)
  "Return a description of the rules in ROOT that match message plist MSG.
Name the winning rule (first match) and every later, shadowed matching rule."
  (let ((matches (seq-filter (lambda (rule)
                               (mailx-eval-condition (mailx-rule-match rule) msg))
                             (mailx-rules-load root))))
    (if (null matches)
        "No rule matches this message.\n"
      (let ((describe (lambda (rule)
                        (format "  %s  match %S  folder %s  tier %s  tags %S\n"
                                (mailx-rule-id rule) (mailx-rule-match rule)
                                (or (mailx-rule-folder rule) "-")
                                (or (mailx-rule-tier rule) "-")
                                (mailx-rule-tags rule)))))
        (concat "Classified by (first match wins):\n"
                (funcall describe (car matches))
                (when (cdr matches)
                  (concat "Shadowed (later rules that also match):\n"
                          (mapconcat describe (cdr matches) ""))))))))

;;;###autoload
(defun mailx-explain-rules (&optional msg)
  "Show the rule classifying MSG and the later rules it shadows.\nMSG defaults to the mu4e message at point."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)
                  (user-error "No message at point")))
         (root (mailx-current-maildir))
         (text (mailx-explain-rules-text root msg)))
    (with-help-window "*mailx rules*"
      (princ text))))

(defun mailx-classify-by-rules (msg rules)
  "Return first of RULES matching MSG, or nil."
  (seq-find (lambda (rule) (mailx-eval-condition (mailx-rule-match rule) msg)) rules))

(defun mailx--mu-find-output (root query &optional limit)
  "Return mu sexp output for QUERY under ROOT, rejecting query errors.
LIMIT, when non-nil, caps the number of results."
  (let ((stderr-file (make-temp-file "mailx-mu-stderr-"))
        (process-environment (cons "LC_ALL=C" process-environment)))
    (unwind-protect
        (with-temp-buffer
          (let* ((status (apply #'process-file "mu" nil (list (current-buffer) stderr-file) nil
                                "find" "--muhome" (expand-file-name ".mu" root)
                                "--format=sexp"
                                (append (when limit (list (format "--maxnum=%d" limit)))
                                        (list query))))
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
Writes to <root>/tools/mailx-generated.procmailrc and <root>/tools/fdm.conf.
Both outputs are computed in memory first.  A rule a target cannot express
is skipped for that target; the result plist's :skipped key lists
\(ID . REASON) pairs.  A target with no exportable rule is not written.
Files are replaced atomically.  Returns (:procmail FILE :fdm FILE :count N
:skipped LIST)."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (rules (mailx-rules-load root))
         (tools-dir (expand-file-name "tools" root))
         (procmail-file (expand-file-name "mailx-generated.procmailrc" tools-dir))
         (fdm-file (expand-file-name "fdm.conf" tools-dir))
         (skipped nil))
    (make-directory tools-dir t)
    (when rules
      (let* ((convert
              (lambda (target fn)
                (seq-keep
                 (lambda (rule)
                   (condition-case err
                       (funcall fn rule)
                     (error
                      (push (cons (mailx-rule-id rule)
                                  (format "%s: %s" target (error-message-string err)))
                            skipped)
                      nil)))
                 rules)))
             (procmail (funcall convert "procmail" #'mailx--rule-to-procmail))
             (fdm (funcall convert "fdm" #'mailx--rule-to-fdm)))
        (seq-do
         (lambda (job)
           (when (cadr job)
             (let ((tmp (make-temp-file (concat (car job) "."))))
               (unwind-protect
                   (progn
                     (with-temp-file tmp
                       (insert (nth 2 job) (mapconcat #'identity (cadr job) "\n") "\n"))
                     (rename-file tmp (car job) t))
                 (when (file-exists-p tmp) (delete-file tmp))))))
         (list (list procmail-file procmail
                     "# mailx-generated.procmailrc --- generated by mailx. DO NOT EDIT.\n\n")
               (list fdm-file fdm
                     "# fdm.conf --- generated by mailx. DO NOT EDIT.\n\n")))))
    (list :procmail procmail-file :fdm fdm-file :count (length rules)
          :skipped (nreverse skipped))))

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
