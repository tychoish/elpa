;;; magit-dash-review-hitl.el --- HITL staged review plan gatekeeping for magit-dash -*- lexical-binding: t; -*-

;; Author: sam kleinman
;; Maintainer: tychoish
;; Version: 0.1
;; Keywords: vc, git, review, hitl, agent

;;; Commentary:

;; Registers the bidirectional `code-review' domain in `hitl-plan' so
;; human diff-review findings can be selectively staged and dispatched to
;; `agent-shell' (via ASW / ASQ), and autonomous agent review findings
;; created over MCP can be triaged before being recorded into
;; `<git-common-dir>/review/<target>.json'.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'hitl-plan)
(require 'magit-dash-review)

;; External optional declarations
(defvar diff-review-file-mode)
(defvar diff-review--files)

(declare-function diff-review-find-file "diff-review" (file &optional base root head))
(declare-function diff-review--file-index "diff-review" (path))
(declare-function diff-review--open "diff-review" (i &optional other-window))
(declare-function diff-review--goto-line "diff-review" (line))
(declare-function hitl-plan-review "hitl-plan-ui" (plan &optional select))

(defvar magit-dash-review-hitl--batch-findings nil
  "Accumulator for findings collected during a `send-to-agent' batch driver run.")

(defvar magit-dash-review-hitl--batch-json-updates nil
  "Alist of (REVIEW-FILE . UPDATES-LIST) accumulated during a batch driver run.")

(defvar magit-dash-review-hitl-dispatch-function nil
  "Optional override function `(lambda (plan findings))' for `send-to-agent' batch dispatch.")

;;; Target Sanitization and Resolution

(defun magit-dash-review-hitl--plist-get-any (plist &rest keys)
  "Return the first non-nil value in PLIST for any of KEYS."
  (seq-some (lambda (k) (plist-get plist k)) keys))

(defun magit-dash-review-hitl--has-dotdot-p (path)
  "Return non-nil if PATH contains a `..' directory traversal segment."
  (and (stringp path)
       (member ".." (split-string path "[/\\]" t))))

(defun magit-dash-review-hitl-sanitize-target (target)
  "Validate and normalize a `code-review' domain TARGET plist.
Accepts both kebab-case (:review-file, :end-line) and MCP snake_case
(:review_file, :end_line) keys.  Signals an error if paths contain `..',
use remote TRAMP syntax, or point outside a `review/*.json' review file."
  (unless (and (listp target) (keywordp (car target)))
    (error "Target must be a keyword plist: %S" target))
  (let* ((id (magit-dash-review-hitl--plist-get-any target :id))
         (review-file (magit-dash-review-hitl--plist-get-any target :review-file :review_file))
         (worktree (magit-dash-review-hitl--plist-get-any target :worktree))
         (path (magit-dash-review-hitl--plist-get-any target :path :file))
         (line (magit-dash-review-hitl--plist-get-any target :line))
         (end-line (magit-dash-review-hitl--plist-get-any target :end-line :end_line))
         (severity (magit-dash-review-hitl--plist-get-any target :severity))
         (author (magit-dash-review-hitl--plist-get-any target :author))
         (body (magit-dash-review-hitl--plist-get-any target :body))
         (snippet (magit-dash-review-hitl--plist-get-any target :snippet))
         (hunk (magit-dash-review-hitl--plist-get-any target :hunk)))
    (unless (and (stringp review-file) (not (string-empty-p review-file)))
      (error "Target requires a `:review-file' string"))
    (when (file-remote-p review-file)
      (error "Remote `:review-file' paths are not allowed: %s" review-file))
    (when (magit-dash-review-hitl--has-dotdot-p review-file)
      (error "Directory traversal `..' is not allowed in `:review-file': %s" review-file))
    (let ((expanded-rf (expand-file-name review-file)))
      (unless (and (string-suffix-p ".json" expanded-rf)
                   (string-match-p "/review/[^/]+\\.json\\'" expanded-rf))
        (error "`:review-file' must reside in a `review/*.json' directory: %s" review-file))
      (setq review-file expanded-rf))
    (when worktree
      (unless (stringp worktree)
        (error "`:worktree' must be a string: %S" worktree))
      (when (file-remote-p worktree)
        (error "Remote `:worktree' paths are not allowed: %s" worktree))
      (when (magit-dash-review-hitl--has-dotdot-p worktree)
        (error "Directory traversal `..' is not allowed in `:worktree': %s" worktree))
      (setq worktree (file-name-as-directory (expand-file-name worktree))))
    (when path
      (unless (stringp path)
        (error "`:path' must be a string: %S" path))
      (when (or (file-remote-p path) (file-name-absolute-p path))
        (error "`:path' must be repository-relative: %s" path))
      (when (magit-dash-review-hitl--has-dotdot-p path)
        (error "Directory traversal `..' is not allowed in `:path': %s" path)))
    (when line
      (unless (and (integerp line) (> line 0))
        (error "`:line' must be a positive integer: %S" line)))
    (when end-line
      (unless (and (integerp end-line) (> end-line 0))
        (error "`:end-line' must be a positive integer: %S" end-line))
      (when (and line (< end-line line))
        (error "`:end-line' (%d) cannot be less than `:line' (%d)" end-line line)))
    (when body
      (unless (stringp body)
        (error "`:body' must be a string: %S" body)))
    (list :id id
          :review-file review-file
          :worktree worktree
          :path path
          :line line
          :end-line (or end-line line)
          :severity (or severity "medium")
          :author (or author "reviewer")
          :body (or body "")
          :snippet snippet
          :hunk hunk)))

(defun magit-dash-review-hitl-resolve-target (target)
  "Resolve a `code-review' domain TARGET plist.
Returns the normalized target plist when `:review-file' is valid, or nil."
  (condition-case nil
      (let* ((clean (magit-dash-review-hitl-sanitize-target target))
             (rf (plist-get clean :review-file)))
        (when (and rf
                   (or (file-exists-p rf)
                       (file-directory-p (file-name-directory (directory-file-name (file-name-directory rf))))))
          clean))
    (error nil)))

(defun magit-dash-review-hitl-capture-target ()
  "Capture a `code-review' finding target plist from the current buffer."
  (let* ((ctx (magit-dash-review-current-hunk))
         (rf (or (plist-get ctx :review-file)
                 (magit-dash-review--current-review-file))))
    (unless rf
      (user-error "Current buffer is not associated with a review JSON file"))
    (list :id (plist-get ctx :comment-id)
          :review-file (expand-file-name rf)
          :worktree (plist-get ctx :root)
          :path (plist-get ctx :path)
          :line (plist-get ctx :line)
          :end-line (plist-get ctx :end-line)
          :hunk (plist-get ctx :hunk)
          :author user-login-name
          :body "")))

(defun magit-dash-review-hitl-format-target (target)
  "Format TARGET as (LOCATION-COLUMN . BODY-PREVIEW) for `hitl-plan-ui'."
  (let* ((path (plist-get target :path))
         (line (plist-get target :line))
         (end-line (plist-get target :end-line))
         (severity (plist-get target :severity))
         (author (plist-get target :author))
         (body (or (plist-get target :body) ""))
         (first-line (car (split-string body "\n")))
         (loc (cond
               ((and path line end-line (/= line end-line))
                (format "%s:%d-%d" path line end-line))
               ((and path line)
                (format "%s:%d" path line))
               (path path)
               (t "(review)")))
         (prefix (string-join
                  (delq nil
                        (list (and severity (not (string-empty-p severity))
                                   (format "[%s]" severity))
                              (and author (not (string-empty-p author))
                                   (format "%s:" author))))
                  " "))
         (label (string-trim
                 (if (string-empty-p prefix)
                     first-line
                   (format "%s %s" prefix first-line)))))
    (cons loc label)))

(defun magit-dash-review-hitl-visit-target (target &optional other-window)
  "Visit TARGET in `diff-review-file-mode', or fall back to its worktree file."
  (let* ((rf (plist-get target :review-file))
         (path (plist-get target :path))
         (line (plist-get target :line))
         (worktree (plist-get target :worktree)))
    (cond
     ((and rf (file-exists-p rf) (fboundp 'diff-review-find-file))
      (let ((buf (diff-review-find-file rf)))
        (if other-window
            (switch-to-buffer-other-window buf)
          (switch-to-buffer buf))
        (when (and path (fboundp 'diff-review--file-index) (fboundp 'diff-review--open))
          (with-current-buffer buf
            (when-let* ((idx (diff-review--file-index path))
                        (fbuf (diff-review--open idx other-window)))
              (with-current-buffer fbuf
                (when (and line (fboundp 'diff-review--goto-line))
                  (diff-review--goto-line line))))))))
     ((and path worktree (file-exists-p (expand-file-name path worktree)))
      (let ((fbuf (find-file-noselect (expand-file-name path worktree))))
        (if other-window
            (switch-to-buffer-other-window fbuf)
          (switch-to-buffer fbuf))
        (when line
          (goto-char (point-min))
          (forward-line (1- line)))))
     (rf
      (let ((buf (find-file-noselect rf)))
        (if other-window
            (switch-to-buffer-other-window buf)
          (switch-to-buffer buf)))))))

(defun magit-dash-review-hitl-accept-buffer-p (buf)
  "Return non-nil when BUF is eligible for `code-review' plan queueing."
  (and (buffer-live-p buf)
       (with-current-buffer buf
         (or (derived-mode-p 'agent-shell-mode
                             'diff-review-mode
                             'code-review-mode)
             (bound-and-true-p diff-review-file-mode)))))

(defun magit-dash-review-hitl-build-batch (scope)
  "Build finding targets from SCOPE's `:review-file' for `hitl-plan-create'."
  (let ((rf (magit-dash-review-hitl--plist-get-any scope :review-file :review_file)))
    (unless (and rf (file-readable-p rf))
      (error "Scope requires a readable `:review-file': %S" scope))
    (let* ((findings (magit-dash-review-extract-findings rf t))
           (targets (mapcar
                     (lambda (f)
                       (magit-dash-review-hitl-sanitize-target
                        (list :id (plist-get f :id)
                              :review-file (plist-get f :review-file)
                              :worktree (plist-get f :worktree)
                              :path (plist-get f :path)
                              :line (plist-get f :line)
                              :end-line (plist-get f :end-line)
                              :author (plist-get f :author)
                              :body (plist-get f :body)
                              :hunk (plist-get f :hunk))))
                     findings)))
      (cons nil targets))))

;;; Batch Driver Execution

(defun magit-dash-review-hitl--flush-json-updates ()
  "Apply accumulated `magit-dash-review-hitl--batch-json-updates' to disk."
  (pcase-dolist (`(,review-file . ,updates) (nreverse magit-dash-review-hitl--batch-json-updates))
    (let* ((data (if (file-exists-p review-file)
                     (magit-dash-review--read-json-file review-file)
                   (let ((ht (make-hash-table :test #'equal)))
                     (setf (map-elt ht "format") "diff-review/1")
                     (setf (map-elt ht "viewed") (make-hash-table :test #'equal))
                     (setf (map-elt ht "comments") [])
                     ht)))
           (comments (append (or (map-elt data "comments") []) nil)))
      (dolist (upd (nreverse updates))
        (pcase (plist-get upd :type)
          ('comment
           (let* ((new-c (plist-get upd :comment))
                  (cid (map-elt new-c "id"))
                  (existing (and cid (seq-find (lambda (c) (equal (map-elt c "id") cid))
                                               comments))))
             (if existing
                 (map-do (lambda (k v) (setf (map-elt existing k) v)) new-c)
               (setq comments (append comments (list new-c))))))
          ('verdict
           (setf (map-elt data "verdict") (plist-get upd :verdict)))))
      (setf (map-elt data "comments") (vconcat comments))
      (magit-dash-review--write-json-file review-file data)
      (when-let* ((live-buf (find-buffer-visiting review-file)))
        (with-current-buffer live-buf
          (revert-buffer t t t))))))

(defun magit-dash-review-hitl--dispatch-findings (plan findings)
  "Dispatch approved FINDINGS from PLAN to the target worktree's agent session."
  (cond
   ((functionp magit-dash-review-hitl-dispatch-function)
    (funcall magit-dash-review-hitl-dispatch-function plan findings))
   ((fboundp 'magit-dash-review-send-findings-to-agent)
    (funcall 'magit-dash-review-send-findings-to-agent findings
             (plist-get (hitl-plan-scope plan) :review-file)))
   (t
    (message "magit-dash-review-hitl: collected %d finding(s) for agent dispatch"
             (length findings)))))

(defun magit-dash-review-hitl-driver (plan op _pending-steps resolved-alist)
  "Batch `:driver' for the `code-review' domain in `hitl-plan'.
Executes each step via `hitl-plan-run-step', then flushes any accumulated
JSON updates or outbound `send-to-agent' findings in a single batch."
  (let ((magit-dash-review-hitl--batch-findings nil)
        (magit-dash-review-hitl--batch-json-updates nil))
    (pcase-dolist (`(,step . ,resolved) resolved-alist)
      (hitl-plan-run-step plan step op resolved))
    (when magit-dash-review-hitl--batch-json-updates
      (magit-dash-review-hitl--flush-json-updates))
    (when magit-dash-review-hitl--batch-findings
      (magit-dash-review-hitl--dispatch-findings
       plan (nreverse magit-dash-review-hitl--batch-findings)))))

;;; Domain Registration

(hitl-plan-register-domain 'code-review
  :label "Code review findings"
  :resolve-target #'magit-dash-review-hitl-resolve-target
  :capture-target #'magit-dash-review-hitl-capture-target
  :format-target #'magit-dash-review-hitl-format-target
  :visit-target #'magit-dash-review-hitl-visit-target
  :build-batch #'magit-dash-review-hitl-build-batch
  :driver #'magit-dash-review-hitl-driver
  :accept-buffer #'magit-dash-review-hitl-accept-buffer-p
  :sanitize-target #'magit-dash-review-hitl-sanitize-target)

;;; Operations

(defun magit-dash-review-hitl--effective-body (target step plan)
  "Return the effective comment body for TARGET, honoring STEP or PLAN notes."
  (let ((step-note (and step (hitl-plan-step-note step)))
        (plan-note (and plan (eq (hitl-plan-log-notes plan) 'one)
                        (hitl-plan-note plan))))
    (cond
     ((and (stringp step-note) (not (string-empty-p (string-trim step-note))))
      step-note)
     ((and (stringp plan-note) (not (string-empty-p (string-trim plan-note))))
      plan-note)
     (t (or (plist-get target :body) "")))))

(defun magit-dash-review-hitl--target-to-comment-ht (target status-str body)
  "Construct a diff-review/1 comment hash table from TARGET with STATUS-STR and BODY."
  (let* ((ht (make-hash-table :test #'equal))
         (path (plist-get target :path))
         (line (plist-get target :line))
         (end-line (or (plist-get target :end-line) line))
         (worktree (plist-get target :worktree))
         (id (or (plist-get target :id)
                 (magit-dash-review--make-comment-id
                  (format "%s:%s:%s:%s" (or path "") (or line 0) body (float-time)))))
         (blob (and path worktree (file-directory-p worktree)
                    (magit-dash-review--resolve-blob worktree "HEAD" path))))
    (setf (map-elt ht "id") id)
    (setf (map-elt ht "origin") "hitl")
    (when status-str
      (setf (map-elt ht "status") status-str))
    (when (plist-get target :severity)
      (setf (map-elt ht "severity") (plist-get target :severity)))
    (when path
      (setf (map-elt ht "path") path))
    (when line
      (setf (map-elt ht "lines") (vector line (max line end-line))))
    (when blob
      (setf (map-elt ht "blob") blob))
    (setf (map-elt ht "author") (or (plist-get target :author) "agent-shell"))
    (setf (map-elt ht "at") (format-time-string "%FT%T%:z"))
    (setf (map-elt ht "body") body)
    (setf (map-elt ht "resolved") :false)
    (setf (map-elt ht "replies") [])
    ht))

(defun magit-dash-review-hitl--queue-json-update (review-file update-plist)
  "Queue UPDATE-PLIST for REVIEW-FILE in `magit-dash-review-hitl--batch-json-updates'."
  (let ((cell (assoc review-file magit-dash-review-hitl--batch-json-updates)))
    (if cell
        (push update-plist (cdr cell))
      (push (cons review-file (list update-plist))
            magit-dash-review-hitl--batch-json-updates))))

(defun magit-dash-review-hitl--apply-send-to-agent ()
  "Operation `:apply' handler for `send-to-agent'."
  (let* ((target hitl-plan-current-target)
         (body (magit-dash-review-hitl--effective-body
                target hitl-plan-current-step hitl-plan-current-plan))
         (finding (plist-put (copy-sequence target) :body body)))
    (push finding magit-dash-review-hitl--batch-findings)))

(defun magit-dash-review-hitl--apply-record-finding (&optional status-override)
  "Operation `:apply' handler for `record-finding' (or `draft-finding')."
  (let* ((target hitl-plan-current-target)
         (rf (plist-get target :review-file))
         (body (magit-dash-review-hitl--effective-body
                target hitl-plan-current-step hitl-plan-current-plan))
         (comment-ht (magit-dash-review-hitl--target-to-comment-ht
                      target (or status-override "accepted") body)))
    (magit-dash-review-hitl--queue-json-update
     rf (list :type 'comment :comment comment-ht))))

(defun magit-dash-review-hitl--apply-draft-finding ()
  "Operation `:apply' handler for `draft-finding'."
  (magit-dash-review-hitl--apply-record-finding "draft"))

(defun magit-dash-review-hitl--validate-verdict (&optional state &rest _)
  "Validate STATE argument for `apply-verdict'."
  (let ((s (or state "approved")))
    (unless (member s '("approved" "changes" "comment"))
      (format "Verdict state must be \"approved\", \"changes\", or \"comment\", got: %S" s))))

(defun magit-dash-review-hitl--verdict-from-json (obj)
  "Extract `(STATE BY HEAD)' argument list from JSON OBJ for `apply-verdict'."
  (let ((get-key (lambda (k)
                   (let ((s (symbol-name k)))
                     (cond
                      ((hash-table-p obj) (or (map-elt obj s) (map-elt obj k)))
                      ((and (consp obj) (keywordp (car obj)))
                       (plist-get obj (intern (concat ":" s))))
                      ((consp obj)
                       (or (map-elt obj k) (map-elt obj s))))))))
    (list (or (funcall get-key 'state) "approved")
          (funcall get-key 'by)
          (funcall get-key 'head))))

(defun magit-dash-review-hitl--apply-verdict (&optional state by head)
  "Operation `:apply' handler for `apply-verdict'."
  (let* ((target hitl-plan-current-target)
         (rf (plist-get target :review-file))
         (v-ht (make-hash-table :test #'equal)))
    (setf (map-elt v-ht "state") (or state "approved"))
    (setf (map-elt v-ht "by") (or by (plist-get target :author) user-login-name))
    (setf (map-elt v-ht "at") (format-time-string "%FT%T%:z"))
    (when head
      (setf (map-elt v-ht "head") head))
    (magit-dash-review-hitl--queue-json-update
     rf (list :type 'verdict :verdict v-ht))))

(hitl-plan-register-op 'code-review 'send-to-agent
  :key ?a
  :label "Send finding to agent-shell"
  :queueable t
  :apply #'magit-dash-review-hitl--apply-send-to-agent
  :describe (lambda (&rest _) "send-to-agent"))

(hitl-plan-register-op 'code-review 'record-finding
  :key ?r
  :label "Record accepted finding in review JSON"
  :queueable t
  :apply #'magit-dash-review-hitl--apply-record-finding
  :describe (lambda (&rest _) "record-finding"))

(hitl-plan-register-op 'code-review 'draft-finding
  :key ?d
  :label "Record draft finding in review JSON"
  :queueable t
  :apply #'magit-dash-review-hitl--apply-draft-finding
  :describe (lambda (&rest _) "draft-finding"))

(hitl-plan-register-op 'code-review 'apply-verdict
  :key ?v
  :label "Apply review verdict"
  :queueable t
  :order-last t
  :read-args (lambda ()
               (list (completing-read "Verdict state: "
                                      '("approved" "changes" "comment")
                                      nil t nil nil "approved")))
  :validate #'magit-dash-review-hitl--validate-verdict
  :from-json #'magit-dash-review-hitl--verdict-from-json
  :apply #'magit-dash-review-hitl--apply-verdict
  :describe (lambda (&optional state &rest _)
              (format "apply-verdict (%s)" (or state "approved"))))

;;; Interactive Staging Entrypoint

;;;###autoload
(defun magit-dash-review-stage-hitl (&optional review-file op-name)
  "Stage unresolved threads from REVIEW-FILE into a `code-review' `hitl-plan'.
OP-NAME defaults to `send-to-agent'.  Opens `hitl-plan-review' when called
interactively and returns the created `hitl-plan' struct."
  (interactive
   (list (or (magit-dash-review--current-review-file)
             (read-file-name "Review JSON file: " nil nil t))
         'send-to-agent))
  (let* ((file (expand-file-name (or review-file
                                     (magit-dash-review--current-review-file)
                                     (user-error "No active review JSON file"))))
         (op (or op-name 'send-to-agent))
         (findings (magit-dash-review-extract-findings file t)))
    (unless findings
      (user-error "No unresolved review findings in %s" file))
    (let* ((worktree (plist-get (car findings) :worktree))
           (targets (mapcar
                     (lambda (f)
                       (magit-dash-review-hitl-sanitize-target
                        (list :id (plist-get f :id)
                              :review-file file
                              :worktree (plist-get f :worktree)
                              :path (plist-get f :path)
                              :line (plist-get f :line)
                              :end-line (plist-get f :end-line)
                              :author (plist-get f :author)
                              :body (plist-get f :body)
                              :hunk (plist-get f :hunk))))
                     findings))
           (plan (hitl-plan-create
                  :domain 'code-review
                  :description (format "Review findings (%s)"
                                       (file-name-nondirectory file))
                  :scope (list :review-file file :worktree worktree)
                  :targets targets
                  :ops (list (if (consp op) op (list :name op))))))
      (when (and (called-interactively-p 'any)
                 (fboundp 'hitl-plan-review))
        (hitl-plan-review plan t))
      plan)))

(provide 'magit-dash-review-hitl)
;;; magit-dash-review-hitl.el ends here
