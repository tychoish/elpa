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

(declare-function mailx-current-maildir "mailx" ())
(declare-function mailx-ensure-context "mailx-org" (name))
(declare-function mailx-server-current-home "mailx-server" ())
(declare-function mailx-rules-load "mailx-rules" (root &optional refresh))
(declare-function mailx-rules-save "mailx-rules" (root rules))
(declare-function make-mailx-rule "mailx-rules" (&rest plist))
(declare-function mu4e-context-name "mu4e-context" (context))
(declare-function mu4e-view-message-with-message-id "mu4e-view" (message-id))
(declare-function mu4e-message-at-point "mu4e-message" (&optional no-error))

(defvar hitl-plan-current-target)
(defvar mailx-archive-folder)
(defvar mailx-trash-folder)

(defun mailx-bulk--resolve-target (target)
  "Resolve TARGET by its :root and :message-id, or return nil.
Only a single exact Message-ID match inside ROOT is accepted.  Never trust a
stored path alone: Maildir files can be renamed or replaced after staging."
  (let ((root (map-elt target :root))
        (id (map-elt target :message-id)))
    ;; Query syntax characters are not accepted; this is deliberately more
    ;; restrictive than RFC Message-ID syntax to prevent query injection.
    (when (and (stringp root) (file-directory-p root)
               (stringp id)
               (string-match-p "\\`[[:alnum:]@._+%=-]+\\'" id))
      (let ((matches (seq-filter
                      (lambda (message)
                        (and (equal id (map-elt message :message-id))
                             (stringp (map-elt message :path))
                             (file-regular-p (map-elt message :path))
                             (file-in-directory-p (file-truename (map-elt message :path))
                                                  (file-truename root))))
                      (mailx--mu-find root (concat "msgid:" id)))))
        (when (= (length matches) 1)
          (append (car matches) (list :root root :context (map-elt target :context))))))))

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

(defun mailx-bulk--mark-read-apply ()
  "Apply mark-read to the current target."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (resolved (mailx-bulk--resolve-target target))
              (path (map-elt resolved :path))
              ((file-exists-p path)))
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
          (rename-file path new-path t)))))))

(defun mailx-bulk--refile-apply (folder)
  "Apply refile to FOLDER on the current target."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (root (map-elt target :root))
              (resolved (mailx-bulk--resolve-target target))
              (path (map-elt resolved :path))
              ((file-exists-p path)))
    (let* ((dest-dir (expand-file-name (concat (string-remove-prefix "/" folder) "/cur") root))
           (new-path (expand-file-name (file-name-nondirectory path) dest-dir)))
      (make-directory dest-dir t)
      (rename-file path new-path t))))

(defun mailx-bulk--tag-apply (tier tags)
  "Apply TIER and TAGS on the current target."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (root (map-elt target :root))
              (resolved (mailx-bulk--resolve-target target))
              (path (map-elt resolved :path))
              (folder (or (map-elt resolved :maildir) (if (boundp 'mailx-archive-folder) mailx-archive-folder "/archive"))))
    (mailx-stamp-and-move path root folder tier tags 'exclusive-access)))

(defun mailx-bulk--unsubscribe-apply ()
  "Record unsubscribed sender from current target."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (root (map-elt target :root))
              (resolved (mailx-bulk--resolve-target target))
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
  "Create a junk rule and trash the current target."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (root (map-elt target :root))
              (resolved (mailx-bulk--resolve-target target))
              (sender (car (mailx--sender-addresses (map-elt resolved :from))))
              (domain (and sender (cadr (split-string sender "@")))))
    (let* ((rules (mailx-rules-load root))
           (rule-id (intern (format "block-%s" (replace-regexp-in-string "[^[:alnum:]]" "-" domain))))
           (trash (if (boundp 'mailx-trash-folder) mailx-trash-folder "/trash"))
           (rule (make-mailx-rule :id rule-id
                                  :match (list :from (format "@%s\\'" (regexp-quote domain)))
                                  :folder trash
                                  :tier 'junk
                                  :source 'manual
                                  :created (format-time-string "%Y-%m-%d"))))
      (mailx-rules-save root (append rules (list rule)))
      (when-let* ((path (map-elt resolved :path))
                  ((file-exists-p path)))
        (mailx-bulk--refile-apply trash)))))

(defun mailx-bulk--expire-apply (days)
  "Expire message if older than DAYS."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (resolved (mailx-bulk--resolve-target target))
              (path (map-elt resolved :path))
              ((file-exists-p path)))
    (let* ((mtime (file-attribute-modification-time (file-attributes path)))
           (age-days (/ (float-time (time-subtract (current-time) mtime)) 86400.0)))
      (when (> age-days days)
        (mailx-bulk--refile-apply (if (boundp 'mailx-archive-folder) mailx-archive-folder "/archive"))))))

(defun mailx-bulk--promote-rule-apply (rule)
  "Persist proposed RULE into active root."
  (when-let* ((target (and (boundp 'hitl-plan-current-target) hitl-plan-current-target))
              (root (map-elt target :root))
              (rules (mailx-rules-load root)))
    (mailx-rules-save root (append rules (list rule)))))

(defun mailx-bulk--register-ops ()
  "Register bulk triage operations under the `mailx' domain."
  (hitl-plan-register-op 'mailx 'mark-read
    :key ?r :label "Mark read" :queueable t
    :apply #'mailx-bulk--mark-read-apply
    :describe (lambda () "Mark read"))
  (hitl-plan-register-op 'mailx 'refile
    :key ?m :label "Refile"
    :read-args (lambda () (list (read-string "Destination folder: " (if (boundp 'mailx-archive-folder) mailx-archive-folder "/archive"))))
    :apply #'mailx-bulk--refile-apply
    :describe (lambda (f) (format "Refile to %s" f)))
  (hitl-plan-register-op 'mailx 'tag
    :key ?t :label "Tag"
    :read-args (lambda () (list (intern (read-string "Tier: " "read"))
                                (split-string (read-string "Tags: ") " " t)))
    :apply #'mailx-bulk--tag-apply
    :describe (lambda (tier tags) (format "Tag %s [%s]" tier (string-join tags ", "))))
  (hitl-plan-register-op 'mailx 'unsubscribe
    :key ?u :label "Unsubscribe" :queueable t
    :apply #'mailx-bulk--unsubscribe-apply
    :describe (lambda () "Record unsubscribed sender"))
  (hitl-plan-register-op 'mailx 'block-sender
    :key ?b :label "Block sender" :queueable t
    :apply #'mailx-bulk--block-sender-apply
    :describe (lambda () "Block sender and trash"))
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
                  :directory #'mailx-bulk-plan-directory)))
    (mailx-bulk--register-ops)
    domain))

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
    (let ((plan (hitl-plan-create :domain 'mailx
                                  :title (or title "mailx-triage")
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
      (let ((plan (hitl-plan-create :domain 'mailx
                                    :title "mailx-mark-read"
                                    :targets (nreverse targets)
                                    :ops '((mark-read)))))
        (hitl-plan-review plan)))))

(provide 'mailx-bulk)
;;; mailx-bulk.el ends here
