;;; mailx-bulk.el --- Reviewable mail targets for HITL plans -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (hitl "0.1.0"))
;; Keywords: mail, convenience
;; URL: https://github.com/tychoish/mailx

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Explicit registration of a mail target domain.  Target resolution queries
;; the selected database by Message-ID, rejecting missing or ambiguous matches.
;; Message mutation requires transport-specific coordination and is not
;; registered by this module.

;;; Code:

(require 'hitl-plan)
(require 'mailx-classify)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function hitl-plan-review "hitl-plan-ui" (plan-or-id &optional display))

(defgroup mailx-bulk nil
  "Bulk triage operations and HITL plan integration for mailx."
  :group 'mailx
  :prefix "mailx-")

(defcustom mailx-plans-directory "tools/plans"
  "Directory name for reviewable HITL plans relative to the maildir."
  :type 'string
  :group 'mailx-bulk)

(defcustom mailx-unsubscribed-filename "tools/mailx-unsubscribed.eld"
  "Path relative to mail root for unsubscribed senders cache."
  :type 'string
  :group 'mailx-bulk)

(defcustom mailx-bulk-mu4e-timeout 5
  "Seconds to wait for the mu4e server to apply a change to its database."
  :type 'number
  :group 'mailx-bulk)

(declare-function mailx-current-maildir "mailx" ())
(declare-function mu4e-running-p "mu4e-server" ())
(declare-function mu4e-root-maildir "mu4e-helpers" ())
(declare-function mu4e--server-move "mu4e-server" (docid-or-msgid &optional maildir flags no-view))
(declare-function mu4e--server-add "mu4e-server" (path))
(declare-function mailx-ensure-context "mailx-org" (name))
(declare-function mailx-server-current-home "mailx-server" ())
(declare-function mailx-rules-load "mailx-rules" (root &optional refresh))
(declare-function mailx-rules-save "mailx-rules" (root rules))
(declare-function make-mailx-rule "mailx-rules" (&rest plist))
(declare-function mu4e-context-name "mu4e-context" (context))
(declare-function mu4e-view-message-with-message-id "mu4e-view" (message-id))
(declare-function mu4e-message-at-point "mu4e-message" (&optional no-error))

(defvar hitl-plan-current-target)
(defvar mu4e-change-filenames-when-moving)
(defvar mu4e-update-func)
(defvar mu4e-error-func)
(defvar mailx-archive-folder)
(defvar mailx-trash-folder)

(defun mailx-bulk-message-id-valid-p (id)
  "Return non-nil if ID can be safely quoted into a mu msgid: query.
ID must be a non-empty string without control characters or whitespace."
  (and (stringp id)
       (not (string-empty-p id))
       (not (string-match-p "[[:cntrl:][:space:]]" id))))

(defun mailx-bulk--msgid-query (id)
  "Return a mu query matching Message-ID ID exactly, quoted for mu."
  (format "msgid:\"%s\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" id)))

(defun mailx-bulk--resolve-target (target)
  "Resolve TARGET by its :root and :message-id, or return nil.
Only a single exact Message-ID match inside ROOT is accepted.  Never trust a
stored path alone: Maildir files can be renamed or replaced after staging."
  (let ((root (map-elt target :root))
        (id (map-elt target :message-id)))
    (if (eq (map-elt target :kind) 'rule)
        (when (and (stringp root) (file-directory-p root))
          target)
      (when (and (stringp root) (file-directory-p root)
                 (mailx-bulk-message-id-valid-p id))
        (let ((matches (seq-filter
                        (lambda (message)
                          (and (equal id (map-elt message :message-id))
                               (stringp (map-elt message :path))
                               (file-regular-p (map-elt message :path))
                               (file-in-directory-p (file-truename (map-elt message :path))
                                                    (file-truename root))))
                        (mailx--mu-find root (mailx-bulk--msgid-query id)))))
          (when (= (length matches) 1)
            (append (car matches) (list :root root :context (map-elt target :context)))))))))

(defun mailx-bulk--require-resolved (target)
  "Resolve TARGET like `mailx-bulk--resolve-target', or signal an error.
Apply functions use this so hitl marks the step failed instead of done when
the message changed or vanished after the plan was reviewed."
  (or (and (not (eq (map-elt target :kind) 'rule))
           (mailx-bulk--resolve-target target))
      (error "Mail target no longer resolves: %s"
             (or (map-elt target :subject) (map-elt target :message-id) "unknown message"))))

(defun mailx-bulk--format-target (target)
  "Return display columns for TARGET."
  (cons (or (map-elt target :context) "")
        (or (map-elt target :subject) (map-elt target :message-id) "")))

(defun mailx-bulk--visit-target (target &optional _other-window)
  "Revalidate and visit TARGET in its context."
  (let ((context (map-elt target :context))
        (root (map-elt target :root)))
    (unless (and (stringp context) (not (string-empty-p context))
                 (mailx-bulk--resolve-target target))
      (user-error "Mail target is missing, ambiguous, or invalid"))
    (require 'mailx-org)
    (mailx-ensure-context context)
    (unless (and (mailx-server-current-home)
                 (equal (file-truename (mailx-server-current-home))
                        (file-truename (expand-file-name ".mu" root))))
      (user-error "Mail context uses a different database"))
    ;; Repeat resolution after switching; mail may have changed during it.
    (unless (mailx-bulk--resolve-target target)
      (user-error "Mail target changed during context switch"))
    (mu4e-view-message-with-message-id (map-elt target :message-id))
    t))

(defun mailx-bulk-plan-directory ()
  "Return per-maildir plan storage directory."
  (expand-file-name mailx-plans-directory (mailx-current-maildir)))

;;; mu4e backend
;; When a mu4e server is running for the plan's root, changes go through the
;; server so its database stays current and Maildir base names are kept.
;; Otherwise the filesystem code below runs and asserts exclusive access.

(defun mailx-bulk--mu4e-active-p (root)
  "Return non-nil when a running mu4e server is serving ROOT."
  (and (fboundp 'mu4e-running-p)
       (fboundp 'mu4e-root-maildir)
       (fboundp 'mailx-server-current-home)
       (mu4e-running-p)
       (when-let* ((home (mailx-server-current-home)))
         (equal (file-truename home)
                (file-truename (expand-file-name ".mu" root))))
       (equal (file-truename (mu4e-root-maildir)) (file-truename root))))

(defun mailx-bulk--mu4e-call (function &rest args)
  "Call mu4e server FUNCTION with ARGS, or signal if mu4e is too old."
  (unless (fboundp function)
    (error "This mu4e does not provide %s; update mu4e or stop its server" function))
  (apply function args))

(defun mailx-bulk--mu4e-confirm (message-id predicate what thunk)
  "Run THUNK, then wait for the mu4e server to confirm the change to MESSAGE-ID.
THUNK issues the asynchronous server commands.  Confirmation is an :update
from the server whose message satisfies PREDICATE; a server error aborts
the wait.  Wait at most `mailx-bulk-mu4e-timeout' seconds and signal an
error mentioning WHAT on failure or timeout.  The server's own replies are
used because a separate `mu find' only sees committed changes, which the
server batches."
  (let* ((done nil)
         (failure nil)
         (update-handler (and (boundp 'mu4e-update-func) mu4e-update-func))
         (mu4e-update-func
          (lambda (message &rest rest)
            (when (and (equal message-id (plist-get message :message-id))
                       (funcall predicate message))
              (setq done t))
            (when update-handler
              (apply update-handler message rest))))
         (mu4e-error-func
          (lambda (code text &rest _rest)
            (setq failure (format "%s %s" code text))))
         (process (and (boundp 'mu4e--server-process)
                       (processp (symbol-value 'mu4e--server-process))
                       (symbol-value 'mu4e--server-process)))
         (deadline (+ (float-time) mailx-bulk-mu4e-timeout)))
    (funcall thunk)
    (while (and (not done) (not failure) (< (float-time) deadline))
      (accept-process-output process 0.1))
    (when failure
      (error "mu4e failed %s for %s: %s" what message-id failure))
    (unless done
      (error "mu4e did not confirm %s for %s within %ss"
             what message-id mailx-bulk-mu4e-timeout))))

(defun mailx-bulk--mu4e-folder (folder)
  "Return FOLDER validated and with a leading slash, as mu4e expects."
  (mailx-bulk-check-folder (if (string-prefix-p "/" folder) folder (concat "/" folder))))

(defun mailx-bulk--mu4e-move (resolved folder)
  "Move RESOLVED to FOLDER through mu4e, keeping its base name."
  (let ((id (map-elt resolved :message-id))
        (mu4e-change-filenames-when-moving nil))
    (unless (equal folder (map-elt resolved :maildir))
      (mailx-bulk--mu4e-confirm
       id (lambda (message) (equal folder (map-elt message :maildir)))
       (format "move to %s" folder)
       (lambda () (mailx-bulk--mu4e-call 'mu4e--server-move id folder nil t))))))

(defun mailx-bulk--mu4e-classify (resolved tier tags folder)
  "Stamp TIER and TAGS on RESOLVED, then move it to FOLDER through mu4e.
Stamp in place, tell mu4e to re-read the file, and move it if FOLDER
differs, waiting until the server reports the new folder and tier tag."
  (let* ((folder (mailx-bulk--mu4e-folder folder))
         (path (map-elt resolved :path))
         (id (map-elt resolved :message-id))
         (tier-tag (concat "tier:" (symbol-name tier)))
         (mu4e-change-filenames-when-moving nil))
    (unless (and (fboundp 'mu4e--server-add) (fboundp 'mu4e--server-move))
      (error "This mu4e lacks the server commands needed to apply changes"))
    (mailx-bulk--mu4e-confirm
     id (lambda (message)
          (and (equal folder (map-elt message :maildir))
               (member tier-tag (map-elt message :tags))))
     (format "tier %s in %s" tier folder)
     (lambda ()
       (mailx-stamp-in-place path tier tags)
       (mailx-bulk--mu4e-call 'mu4e--server-add path)
       (unless (equal folder (map-elt resolved :maildir))
         (mailx-bulk--mu4e-call 'mu4e--server-move id folder nil t))))))

(defun mailx-bulk--mark-read-apply ()
  "Apply mark-read to the current target."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (resolved (mailx-bulk--require-resolved target))
              (path (map-elt resolved :path))
              ((file-exists-p path)))
    (if (mailx-bulk--mu4e-active-p (map-elt target :root))
        (let ((id (map-elt resolved :message-id))
              (mu4e-change-filenames-when-moving nil))
          (mailx-bulk--mu4e-confirm
           id (lambda (message) (memq 'seen (map-elt message :flags)))
           "mark read"
           (lambda () (mailx-bulk--mu4e-call 'mu4e--server-move id nil "+S-N"))))
      (let* ((dir (file-name-directory path))
             (parent (file-name-nondirectory (directory-file-name dir)))
             (grandparent (file-name-directory (directory-file-name dir)))
             (base (file-name-nondirectory path)))
        (cond
         ((string-match "\\(:2,[A-Za-z]*\\)\\'" base)
          (let ((flags (match-string 1 base)))
            (unless (string-match-p "S" flags)
              (let ((new-path (expand-file-name (concat (substring base 0 (match-beginning 1))
                                                        flags "S") dir)))
                (rename-file path new-path t)))))
         ((equal parent "new")
          (let* ((cur-dir (expand-file-name "cur" grandparent))
                 (new-path (expand-file-name (concat base ":2,S") cur-dir)))
            (make-directory cur-dir t)
            (rename-file path new-path t))))))))

(defun mailx-bulk-check-folder (folder)
  "Return FOLDER if it is a safe Maildir folder name, else signal `user-error'.
FOLDER must be a non-empty string with a leading slash, no control
characters, and no empty or `..' components."
  (unless (and (stringp folder) (not (string-empty-p folder)))
    (user-error "Folder must be a non-empty string"))
  (unless (string-prefix-p "/" folder)
    (user-error "Folder %S must start with /" folder))
  (when (string-match-p "[[:cntrl:]]" folder)
    (user-error "Folder %S contains control characters" folder))
  (when (seq-some (lambda (part) (member part '("" "..")))
                  (cdr (split-string (string-remove-suffix "/" folder) "/")))
    (user-error "Folder %S has an empty or .. component" folder))
  folder)

(defun mailx-bulk--refile-apply (folder)
  "Apply refile to FOLDER on the current target.
Never overwrites an existing destination file."
  (mailx-bulk-check-folder folder)
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (root (map-elt target :root))
              (resolved (mailx-bulk--require-resolved target))
              (path (map-elt resolved :path))
              ((file-exists-p path)))
    (if (mailx-bulk--mu4e-active-p root)
        (mailx-bulk--mu4e-move resolved folder)
      (let* ((dest-dir (expand-file-name (concat (string-remove-prefix "/" folder) "/cur") root))
             (new-path (expand-file-name (file-name-nondirectory path) dest-dir)))
        (make-directory dest-dir t)
        (rename-file path new-path nil)))))

(defun mailx-bulk--tag-apply (tier tags)
  "Apply TIER and TAGS on the current target."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (root (map-elt target :root))
              (resolved (mailx-bulk--require-resolved target))
              (path (map-elt resolved :path))
              (folder (or (map-elt resolved :maildir) (if (boundp 'mailx-archive-folder) mailx-archive-folder "/archive"))))
    (if (mailx-bulk--mu4e-active-p root)
        (mailx-bulk--mu4e-classify resolved tier tags folder)
      (mailx-stamp-and-move path root folder tier tags 'exclusive-access))))

(defun mailx-bulk--classify-apply (tier tags folder)
  "Stamp TIER and TAGS on the current target and move it to FOLDER.
With a running mu4e server for the root the change goes through it.
Otherwise applying an approved plan is where exclusive Maildir access is
asserted; the reviewer is expected to have stopped other writers."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (root (map-elt target :root))
              (resolved (mailx-bulk--require-resolved target))
              (path (map-elt resolved :path)))
    (if (mailx-bulk--mu4e-active-p root)
        (mailx-bulk--mu4e-classify resolved tier tags folder)
      (mailx-stamp-and-move path root folder tier tags 'exclusive-access))))

(defun mailx-bulk--unsubscribe-apply ()
  "Record unsubscribed sender from current target."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (root (map-elt target :root))
              (resolved (mailx-bulk--require-resolved target))
              (sender (car (mailx--sender-addresses (map-elt resolved :from))))
              (file (expand-file-name mailx-unsubscribed-filename root)))
    (make-directory (file-name-directory file) t)
    (let ((existing (when (file-exists-p file)
                      (with-temp-buffer
                        (insert-file-contents file)
                        (condition-case nil (read (current-buffer)) (error nil))))))
      (unless (member sender existing)
        (with-temp-file file
          (prin1 (sort (cons sender existing) #'string-lessp) (current-buffer))
          (insert "\n"))))))

(defun mailx-bulk--block-sender-apply ()
  "Create an exact-address junk rule (once) and trash the current target."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (root (map-elt target :root))
              (resolved (mailx-bulk--require-resolved target))
              (sender (car (mailx--sender-addresses (map-elt resolved :from)))))
    (let* ((rules (mailx-rules-load root))
           (rule-id (intern (format "block-%s" (replace-regexp-in-string "[^[:alnum:]]" "-" sender))))
           (trash (if (boundp 'mailx-trash-folder) mailx-trash-folder "/trash")))
      (unless (seq-some (lambda (rule) (eq (mailx-rule-id rule) rule-id)) rules)
        (mailx-rules-save
         root
         (append rules
                 (list (make-mailx-rule :id rule-id
                                        :match (list :from (format "\\`%s\\'" (regexp-quote sender)))
                                        :folder trash
                                        :tier 'junk
                                        :source 'manual
                                        :created (format-time-string "%Y-%m-%d"))))))
      (when-let* ((path (map-elt resolved :path))
                  ((file-exists-p path)))
        (mailx-bulk--refile-apply trash)))))

(defun mailx-bulk--expire-apply (days)
  "Expire message if older than DAYS."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (resolved (mailx-bulk--require-resolved target))
              (path (map-elt resolved :path))
              ((file-exists-p path)))
    (let* ((mtime (file-attribute-modification-time (file-attributes path)))
           (age-days (/ (float-time (time-subtract (current-time) mtime)) 86400.0)))
      (when (> age-days days)
        (mailx-bulk--refile-apply (if (boundp 'mailx-archive-folder) mailx-archive-folder "/archive"))))))

(defun mailx-bulk--promote-rule-apply (rule)
  "Persist proposed RULE into active root."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (root (map-elt target :root)))
    (let ((rules (mailx-rules-load root)))
      (when (seq-some (lambda (r) (eq (mailx-rule-id r) (mailx-rule-id rule))) rules)
        (error "Rule ID already exists: %s" (mailx-rule-id rule)))
      (mailx-rules-save root (append rules (list rule))))))

(defun mailx-bulk--register-ops ()
  "Register bulk triage operations under the `mailx' domain."
  (hitl-plan-register-op 'mailx 'mark-read
    :key ?r :label "Mark read" :queueable t
    :apply #'mailx-bulk--mark-read-apply
    :describe (lambda () "Mark read"))
  (hitl-plan-register-op 'mailx 'refile
    :key ?m :label "Refile"
    :read-args (lambda () (list (mailx-bulk-check-folder
                                 (read-string "Destination folder: " (if (boundp 'mailx-archive-folder) mailx-archive-folder "/archive")))))
    :apply #'mailx-bulk--refile-apply
    :describe (lambda (f) (format "Refile to %s" f)))
  (hitl-plan-register-op 'mailx 'tag
    :key ?t :label "Tag"
    :read-args (lambda () (list (intern (read-string "Tier: " "read"))
                                (split-string (read-string "Tags: ") " " t)))
    :apply #'mailx-bulk--tag-apply
    :describe (lambda (tier tags) (format "Tag %s [%s]" tier (string-join tags ", "))))
  (hitl-plan-register-op 'mailx 'classify
    :key ?c :label "Classify"
    :read-args (lambda () (list (intern (read-string "Tier: " "read"))
                                (split-string (read-string "Tags: ") " " t)
                                (read-string "Destination folder: "
                                             (if (boundp 'mailx-archive-folder) mailx-archive-folder "/archive"))))
    :apply #'mailx-bulk--classify-apply
    :describe (lambda (tier tags folder)
                (format "Classify %s [%s] -> %s" tier (string-join tags ", ") folder)))
  (hitl-plan-register-op 'mailx 'unsubscribe
    :key ?u :label "Unsubscribe" :queueable t
    :apply #'mailx-bulk--unsubscribe-apply
    :describe (lambda () "Record unsubscribed sender"))
  (hitl-plan-register-op 'mailx 'block-sender
    :key ?b :label "Block sender (exact address)" :queueable t
    :apply #'mailx-bulk--block-sender-apply
    :describe (lambda () "Block exact sender address and trash"))
  (hitl-plan-register-op 'mailx 'expire-older
    :key ?e :label "Expire older"
    :read-args (lambda () (list (read-number "Days: " 30)))
    :apply #'mailx-bulk--expire-apply
    :describe (lambda (d) (format "Expire older than %d days" d)))
  (hitl-plan-register-op 'mailx 'promote-rule
    :key ?p :label "Promote rule"
    :apply #'mailx-bulk--promote-rule-apply
    :describe (lambda (r) (format "Promote rule %s" (mailx-rule-id r)))))

;;;###autoload
(defun mailx-bulk-setup ()
  "Register the `mailx' domain and bulk operations on hitl-plan."
  (let ((domain (hitl-plan-register-domain 'mailx
                  :label "mailx"
                  :resolve-target #'mailx-bulk--resolve-target
                  :format-target #'mailx-bulk--format-target
                  :visit-target #'mailx-bulk--visit-target
                  :directory #'mailx-bulk-plan-directory
                  :unresolved-message
                  "Unsupported, missing or ambiguous Message-ID: target no longer resolves")))
    (mailx-bulk--register-ops)
    domain))

;;;###autoload
(defun mailx-bulk-ensure-domain ()
  "Register the `mailx' hitl-plan domain unless it is already registered.
Call before creating a plan; nothing else registers the domain."
  (unless (hitl-plan-get-domain 'mailx)
    (mailx-bulk-setup)))

;;;###autoload
(defun mailx-bulk-stage-current-view (&optional title)
  "Stage a hitl-plan for all messages in the active mu4e headers view."
  (interactive)
  (unless (derived-mode-p 'mu4e-headers-mode)
    (user-error "Not in a mu4e headers buffer"))
  (let* ((root (mailx-current-maildir))
         (context (when (fboundp 'mu4e-context-current)
                    (when-let* ((ctx (mu4e-context-current)))
                      (mu4e-context-name ctx))))
         (targets nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((msg (mu4e-message-at-point t))
                    (id (plist-get msg :message-id)))
          (push (list :root root
                      :context context
                      :message-id id
                      :subject (plist-get msg :subject)
                      :path (plist-get msg :path))
                targets))
        (forward-line 1)))
    (unless targets
      (user-error "No messages found in current view"))
    (setq targets (nreverse targets))
    (mailx-bulk-ensure-domain)
    (let ((plan (hitl-plan-create :domain 'mailx
                                  :description (or title "mailx-triage")
                                  :targets targets)))
      (hitl-plan-review plan))))

;;;###autoload
(defun mailx-bulk-mark-view-read ()
  "Stage mark-read for all messages in current view."
  (interactive)
  (let* ((root (mailx-current-maildir))
         (context (when (fboundp 'mu4e-context-current)
                    (when-let* ((ctx (mu4e-context-current)))
                      (mu4e-context-name ctx))))
         (targets nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((msg (mu4e-message-at-point t))
                    (id (plist-get msg :message-id)))
          (push (list :root root :context context :message-id id :path (plist-get msg :path))
                targets))
        (forward-line 1)))
    (when targets
      (mailx-bulk-ensure-domain)
      (let ((plan (hitl-plan-create :domain 'mailx
                                    :description "mailx-mark-read"
                                    :targets (nreverse targets)
                                    :ops '((mark-read)))))
        (hitl-plan-review plan)))))

(provide 'mailx-bulk)
;;; mailx-bulk.el ends here
