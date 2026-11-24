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
  ;; Loading mu4e defines its context API (and `mu4e-contexts') without
  ;; starting a server in the wrong database; hooks may rely on both.
  (require 'mailx)
  (require 'mu4e)
  (run-hooks 'mailx-prepare-context-hook)
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
(declare-function org-capture "org-capture" (&optional goto keys))

(defvar mailx-org--pending-capture nil
  "Plist with :path and :msg of the message being captured, or nil.")

(defun mailx-org--finalize-capture ()
  "Refile the captured message once `org-capture' finishes.
Run from `org-capture-after-finalize-hook'; removes itself.  The move needs
delivery and sync stopped, so ask once and skip it when not confirmed."
  (remove-hook 'org-capture-after-finalize-hook #'mailx-org--finalize-capture)
  (let ((path (plist-get mailx-org--pending-capture :path))
        (msg (plist-get mailx-org--pending-capture :msg)))
    (setq mailx-org--pending-capture nil)
    (when (and path
               (not (bound-and-true-p org-note-abort))
               (file-exists-p path))
      (require 'mailx)
      (if (y-or-n-p "Mail delivery and sync are stopped, so the message can be moved? ")
          (condition-case err
              (mailx-stamp-and-move path (mailx-current-maildir) (mailx-refile-folder msg)
                                    'read nil 'exclusive-access)
            (error (message "mailx: could not refile captured message: %s"
                            (error-message-string err))))
        (message "mailx: captured message not moved; stop mail delivery and sync to refile it")))))

;;;###autoload
(defun mailx-capture-task (&optional key)
  "Capture an Org task from the current mail message.
Refile the message upon finalization, after confirming that mail delivery
and sync are stopped.
KEY is the optional `org-capture' template key."
  (interactive)
  (unless (derived-mode-p 'mu4e-headers-mode 'mu4e-view-mode)
    (user-error "Must be called from a mu4e headers or view buffer"))
  (let* ((msg (mu4e-message-at-point t))
         (path (plist-get msg :path)))
    (if path
        (progn
          (mailx-org-store-link)
          (setq mailx-org--pending-capture (list :path path :msg msg))
          (add-hook 'org-capture-after-finalize-hook #'mailx-org--finalize-capture)
          (org-capture nil (or key mailx-org-capture-template-key)))
      (message "mailx: no message at point; nothing captured"))))

(provide 'mailx-org)
;;; mailx-org.el ends here
