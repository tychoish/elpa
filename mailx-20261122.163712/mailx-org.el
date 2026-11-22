;;; mailx-org.el --- Context-aware links to mail -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: mail, convenience
;; URL: https://github.com/tychoish/mailx

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Register Org links that restore the mail context before opening a message
;; or search.  Call `mailx-org-setup' when this integration is wanted.

;;; Code:

(require 'org)
(require 'url-util)
(require 'mailx-server)

(declare-function mu4e-context-current "mu4e-context")
(declare-function mu4e-context-name "mu4e-context")
(declare-function mu4e-context-switch "mu4e-context")
(declare-function mu4e-message-at-point "mu4e-message")
(declare-function mu4e-search "mu4e-search")
(declare-function mu4e-view-message-with-message-id "mu4e-view")

(defvar mailx-prepare-context-hook nil
  "Functions called after loading mailx and before selecting a context.")

;;;###autoload
(defun mailx-ensure-context (name)
  "Switch to mail context NAME and wait for its server to be ready."
  (when mailx--transitioning
    (user-error "Wait for the mail database switch to finish"))
  ;; Loading mailx activates the configured contexts; loading mu4e defines
  ;; its context API without starting a server in the wrong database.
  (require 'mailx)
  (run-hooks 'mailx-prepare-context-hook)
  (require 'mu4e)
  (unless (and (mu4e-context-current)
               (equal name (mu4e-context-name (mu4e-context-current))))
    (mailx-server--ensure-no-drafts)
    (mu4e-context-switch nil name))
  (mailx-server-ensure-ready))

(defun mailx-org-follow-link (path _argument)
  "Open PATH, a context-qualified query or message ID."
  (unless (string-match "\\`\\([^:]+\\):\\(query\\|msgid\\):\\(.+\\)\\'" path)
    (user-error "Invalid mailx link: %s" path))
  (let ((context (match-string 1 path))
        (kind (match-string 2 path))
        (value (match-string 3 path)))
    (setq context (url-unhex-string context))
    (setq value (url-unhex-string value))
    (mailx-ensure-context context)
    (pcase kind
      ("query" (mu4e-search value))
      ("msgid" (mu4e-view-message-with-message-id value)))))

(defun mailx-org-store-link ()
  "Store a context-qualified link to the message at point, if any."
  (when (derived-mode-p 'mu4e-headers-mode 'mu4e-view-mode)
    (when-let* ((msg (mu4e-message-at-point t))
                (context (mu4e-context-current))
                (id (plist-get msg :message-id)))
      (org-link-store-props
       :type "mailx"
       :link (format "mailx:%s:msgid:%s"
                     (url-hexify-string (mu4e-context-name context))
                     (url-hexify-string id))
       :description (or (plist-get msg :subject) id)))))

(defvar org-capture-templates nil)
(defvar org-note-abort)

(defgroup mailx-org nil
  "Org integration for mailx."
  :group 'mail)

(defcustom mailx-org-capture-template
  '("m" "Mail task" entry (file+headline "planner.org" "Tasks")
    "* TODO %a\n%i%?" :prepend t :kill-buffer t)
  "Org capture template registered by `mailx-org-setup'."
  :type 'sexp
  :group 'mailx-org)

(defcustom mailx-org-capture-template-key "m"
  "Org capture template key to use for `mailx-capture-task'.
If nil, `org-capture' will prompt for a template."
  :type '(choice (const :tag "Prompt" nil) string)
  :group 'mailx-org)

(defcustom mailx-sync-process-buffer "*mailx-sync*"
  "Buffer name for background mail sync and classification."
  :type 'string
  :group 'mailx-org)

;;;###autoload
(defun mailx-org-setup ()
  "Register context-aware mail links and capture template with Org."
  (org-link-set-parameters "mailx" :follow #'mailx-org-follow-link
                           :store #'mailx-org-store-link)
  (when (and (boundp 'org-capture-templates) mailx-org-capture-template)
    (let ((key (car mailx-org-capture-template)))
      (setq org-capture-templates
            (cons mailx-org-capture-template
                  (seq-remove (lambda (el) (equal (car el) key))
                              org-capture-templates))))))

;;;###autoload
(defun mailx-org-teardown ()
  "Unregister context-aware mail links and capture template from Org."
  (org-link-set-parameters "mailx" :follow nil :store nil)
  (when (and (boundp 'org-capture-templates) mailx-org-capture-template)
    (let ((key (car mailx-org-capture-template)))
      (setq org-capture-templates
            (seq-remove (lambda (el) (equal (car el) key))
                        org-capture-templates)))))

(declare-function mailx-current-maildir "mailx" ())
(declare-function mailx-refile-folder "mailx" (msg))
(declare-function mailx-stamp-and-move "mailx-classify" (path root target-folder tier &optional tags coordination))
(declare-function mailx-classify-batch "mailx-classify" (root &optional options))
(declare-function org-capture "org-capture" (&optional goto keys))

;;;###autoload
(defun mailx-capture-task (&optional key)
  "Capture an Org task from the current mail message.
Refile the message upon finalization.
KEY is the optional `org-capture' template key."
  (interactive)
  (unless (derived-mode-p 'mu4e-headers-mode 'mu4e-view-mode)
    (user-error "Must be called from a mu4e headers or view buffer"))
  (when-let* ((msg (mu4e-message-at-point t))
              (path (plist-get msg :path)))
    (mailx-org-store-link)
    (let ((captured-path path)
          (captured-msg msg))
      (letrec ((finalize-hook
                (lambda ()
                  (remove-hook 'org-capture-after-finalize-hook finalize-hook)
                  (unless (bound-and-true-p org-note-abort)
                    (when (and captured-path (file-exists-p captured-path))
                      (require 'mailx)
                      (let* ((folder (mailx-refile-folder captured-msg))
                             (root (mailx-current-maildir)))
                        (condition-case nil
                            (mailx-stamp-and-move captured-path root folder 'read nil 'exclusive-access)
                          (error nil))))))))
        (add-hook 'org-capture-after-finalize-hook finalize-hook)
        (org-capture nil (or key mailx-org-capture-template-key))))))

;;;###autoload
(defun mailx-sync-and-classify-async (&optional root sync-command callback)
  "Synchronize mail and run batch classification in the background.
ROOT is the maildir root, default `(mailx-current-maildir)'.
SYNC-COMMAND is an optional shell command or list of strings to run
before classification.
CALLBACK is called with the process status when complete."
  (interactive)
  (let* ((mail-root (or root (mailx-current-maildir)))
         (cmd (or sync-command
                  (when (boundp 'mu4e-get-mail-command) mu4e-get-mail-command)))
         (buf (get-buffer-create mailx-sync-process-buffer)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n[%s] Starting mail sync and classify for %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S") mail-root)))
    (if cmd
        (let* ((proc-cmd (if (listp cmd) cmd (list shell-file-name shell-command-switch cmd)))
               (proc (make-process
                      :name "mailx-sync"
                      :buffer buf
                      :command proc-cmd
                      :sentinel
                      (lambda (p event)
                        (when (memq (process-status p) '(exit signal))
                          (let ((ok (and (eq (process-status p) 'exit)
                                         (zerop (process-exit-status p)))))
                            (with-current-buffer (process-buffer p)
                              (insert (format "[%s] Sync %s: %s\n"
                                              (format-time-string "%Y-%m-%d %H:%M:%S")
                                              (if ok "finished successfully" "failed")
                                              (string-trim event))))
                            (if ok
                                (progn
                                  (require 'mailx-classify)
                                  (let ((res (condition-case err
                                                 (mailx-classify-batch mail-root '(:dry-run nil :update-index t))
                                               (error (list :error (error-message-string err))))))
                                    (with-current-buffer (process-buffer p)
                                      (insert (format "[%s] Classification finished: %S\n"
                                                      (format-time-string "%Y-%m-%d %H:%M:%S") res)))))
                              (with-current-buffer (process-buffer p)
                                (insert (format "[%s] Skipping classification due to sync error.\n"
                                                (format-time-string "%Y-%m-%d %H:%M:%S")))))
                            (when callback (funcall callback p event))))))))
          proc)
      (run-with-idle-timer 0 nil
                           (lambda ()
                             (require 'mailx-classify)
                             (mailx-classify-batch mail-root '(:dry-run nil :update-index t))
                             (when callback (funcall callback nil "finished")))))))

(provide 'mailx-org)
;;; mailx-org.el ends here
