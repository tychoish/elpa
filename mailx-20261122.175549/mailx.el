;;; mailx.el --- Mail routing, classification, attention views, and triage -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (hitl "0.1.0") (mcpkit "0.1.0") (annotated-completing-read "0.1.0"))
;; Keywords: mail, convenience
;; URL: https://github.com/tychoish/mailx

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Declarative mail routing, multi-tier attention views, 3-stage
;; cascading classification, and human-in-the-loop bulk triage for mu4e.

;;; Code:

(require 'map)
(require 'seq)
(require 'cl-lib)
(require 'mailx-server)
(require 'mailx-rules)
(require 'mailx-views)

(autoload 'mu4e "mu4e" nil t)
(autoload 'mu4e-update-index "mu4e" nil t)
(autoload 'mu4e-update-mail-and-index "mu4e" nil t)
(autoload 'make-mu4e-context "mu4e-context")
(autoload 'mu4e-context-switch "mu4e-context" nil t)
(autoload 'mu4e-compose-context-switch "mu4e-compose" nil t)

(declare-function annotated-completing-read "annotated-completing-read")
(declare-function message-field-value "message" (header &optional first))
(declare-function mu4e-compose-reply "mu4e-compose")
(declare-function mu4e-running-p "mu4e-server")
(declare-function mu4e-headers-mark-for-read "mu4e-headers")
(declare-function mu4e-headers-mark-for-unread "mu4e-headers")
(declare-function mu4e-mark-resolve-deferred-marks "mu4e-mark")
(declare-function mu4e-message-field "mu4e-message")
(declare-function mu4e-fetch-field "mu4e-message")
(declare-function mu4e-message-at-point "mu4e-message")
(declare-function mu4e-contact-email "mu4e-contacts")
(declare-function mu4e-contact-name "mu4e-contacts")

(declare-function mailx-org-setup "mailx-org")
(declare-function mailx-org-teardown "mailx-org")
(declare-function cmdr-frame-record-home-frame "cmdr-frame")

(defgroup mailx nil
  "Declarative mail routing, classification, and attention views."
  :group 'mail
  :prefix "mailx-")

(defcustom mailx-default-maildir "~/mail"
  "Default maildir root directory."
  :type 'directory
  :group 'mailx)

(defcustom mailx-archive-folder "/archive"
  "Default maildir folder for archived messages."
  :type 'string
  :group 'mailx)

(defcustom mailx-drafts-folder "drafts"
  "Default folder name for drafts relative to the maildir."
  :type 'string
  :group 'mailx)

(defcustom mailx-signatures-dir "tools/signatures"
  "Default directory name for signatures relative to the maildir."
  :type 'string
  :group 'mailx)

(defcustom mailx-citation-line-format "On %A, %B %d %Y, %T, %N wrote:\n"
  "Format string for message citation lines."
  :type 'string
  :group 'mailx)

(defcustom mailx-header-separator "--------------------------"
  "Separator line between mail headers and body."
  :type 'string
  :group 'mailx)

(defvar mm-discouraged-alternatives)
(defvar mml-secure-openpgp-sign-with-sender)
(defvar mu4e-headers-actions)
(defvar mu4e-view-actions)
(defvar mu4e-refile-folder)
(defvar mu4e-bookmarks)
(defvar mu4e-compose-complete-only-after)
(defvar mu4e--header-separator)
(defvar mu4e-search-include-related)
(defvar mu4e-search-results-limit)
(defvar mu4e-compose-context-policy)
(defvar mu4e--context-current)
(defvar mu4e-get-mail-command)
(defvar message-directory)
(defvar message-auto-save-directory)
(defvar message-signature-directory)
(defvar message-signature-file)
(defvar message-signature)
(defvar message-sendmail-extra-arguments)
(defvar mail-host-address)
(defvar message-send-mail-function)
(defvar message-fill-column)
(defvar compose-mail-user-agent-warnings)
(defvar mail-specify-envelope-from)
(defvar mail-user-agent)
(defvar mail-header-separator)
(defvar message-citation-line-function)
(defvar mailx--composing nil
  "Non-nil while mu4e creates a draft.")

(defcustom mailx-compose-complete-years 10
  "Number of years back to include contacts for address completion.
When non-nil, sets `mu4e-compose-complete-only-after' to that many years
prior to current date."
  :type '(choice (integer :tag "Years") (const :tag "Disabled" nil))
  :group 'mailx)

(defun mailx-compose-complete-after-date (&optional years)
  "Return ISO date string YEARS before now.
YEARS defaults to `mailx-compose-complete-years'."
  (when-let* ((y (or years mailx-compose-complete-years)))
    (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time (* 365 y))))))

(defun mailx-current-maildir ()
  "Return the active maildir root directory."
  (or (and (boundp 'message-directory) message-directory
           (file-directory-p message-directory)
           message-directory)
      (when-let* ((home (mailx-server-current-home)))
        (file-name-directory (directory-file-name home)))
      (expand-file-name mailx-default-maildir)))

(defconst mailx--refile-matchable-fields
  '((:from . address)
    (:to . address)
    (:subject . string)
    (:maildir . string)
    (:list . string))
  "Alist of mu4e message fields available for refile rule generation.")

(defvaralias 'mailx--refile-rules 'mailx-refile-rules
  "Alias for `mailx-refile-rules'.")

;;;###autoload
(defcustom mailx-refile-rules nil
  "Functions of one argument MSG, tried in order by `mailx-refile-folder'."
  :type '(repeat function)
  :group 'mailx)

(defcustom mailx-global-managed-settings
  `((mail-specify-envelope-from . t)
    (mail-user-agent . mu4e-user-agent)
    (mail-header-separator . ,mailx-header-separator)
    (message-citation-line-format . ,mailx-citation-line-format)
    (message-citation-line-function . message-insert-formatted-citation-line)
    (message-send-mail-function . message-send-mail-with-sendmail)
    (message-fill-column . 80)
    (mml-secure-openpgp-sign-with-sender . t)
    (compose-mail-user-agent-warnings . nil)
    (mu4e-refile-folder . mailx-refile-folder)
    (mu4e-compose-context-policy . ask)
    (mu4e-compose-complete-only-after . ,(mailx-compose-complete-after-date))
    (mu4e-search-include-related . nil)
    (mu4e-search-results-limit . 1000))
  "Alist of (SYMBOL . VALUE) settings managed globally by `mailx-global-mode'."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'mailx)

(defvar mailx-global--saved-settings nil
  "Alist of (SYMBOL . (BOUNDP . VALUE)) saved before enabling `mailx-global-mode'.")

(defun mailx-global--apply-settings ()
  "Save previous values and apply `mailx-global-managed-settings'."
  (setq mailx-global--saved-settings nil)
  (dolist (pair mailx-global-managed-settings)
    (let ((sym (car pair))
          (val (cdr pair)))
      (push (cons sym (if (boundp sym) (cons t (symbol-value sym)) (cons nil nil)))
            mailx-global--saved-settings)
      (set sym val))))

(defun mailx-global--revert-settings ()
  "Restore variables to their state prior to `mailx-global-mode'."
  (dolist (entry mailx-global--saved-settings)
    (let ((sym (car entry))
          (status (cdr entry)))
      (if (car status)
          (set sym (cdr status))
        (makunbound sym))))
  (setq mailx-global--saved-settings nil))

(defun mailx-global--enable-advice ()
  "Enable advice managed by `mailx-global-mode'."
  (advice-add 'basic-save-buffer :before #'mailx--ad-guard-save)
  (advice-add 'mu4e--draft :before #'mailx-global--enable-mu4e-draft)
  (advice-add 'mu4e--delimit-headers :before #'mailx-global--enable-mu4e-draft)
  (advice-add 'mu4e--draft :around #'mailx--ad-draft)
  (advice-add 'mu4e--draft :before #'mailx--ad-start-for-draft '((depth . -50))))

(defun mailx-global--disable-advice ()
  "Disable advice managed by `mailx-global-mode'."
  (advice-remove 'basic-save-buffer #'mailx--ad-guard-save)
  (advice-remove 'mu4e--draft #'mailx-global--enable-mu4e-draft)
  (advice-remove 'mu4e--delimit-headers #'mailx-global--enable-mu4e-draft)
  (advice-remove 'mu4e--draft #'mailx--ad-draft)
  (advice-remove 'mu4e--draft #'mailx--ad-start-for-draft))

(defconst mailx-frame-home-hooks
  '(mu4e-main-mode-hook
    mu4e-headers-mode-hook
    mu4e-view-mode-hook
    mu4e-compose-mode-hook)
  "Hooks where `cmdr-frame-record-home-frame' is installed if available.")

(defun mailx-global--maybe-enable-home-frames (&rest _)
  "Install home-frame hooks once cmdr makes its recorder available."
  (when (fboundp 'cmdr-frame-record-home-frame)
    (seq-do (lambda (hook) (add-hook hook #'cmdr-frame-record-home-frame))
            mailx-frame-home-hooks)
    (remove-hook 'after-load-functions #'mailx-global--maybe-enable-home-frames)))

(defun mailx-global--enable-hooks ()
  "Add hooks managed by `mailx-global-mode'."
  (add-hook 'mu4e-compose-mode-hook #'mailx-set-up-message-mode-buffer)
  (add-hook 'mu4e-compose-mode-hook #'whitespace-cleanup)
  (add-hook 'cmdr-spec-mail-activate-hook #'mailx-global--enable-mu4e)
  (add-hook 'cmdr-spec-mail-activate-hook #'mailx-global--enable-message)
  (dolist (hook '(mu4e-main-mode-hook mu4e-headers-mode-hook mu4e-view-mode-hook))
    (add-hook hook #'mailx-global--enable-mu4e)
    (add-hook hook #'mailx-global--enable-message))
  (add-hook 'message-mode-hook #'mailx-global--enable-message)
  (if (fboundp 'cmdr-frame-record-home-frame)
      (mailx-global--maybe-enable-home-frames)
    (add-hook 'after-load-functions #'mailx-global--maybe-enable-home-frames)))

(defun mailx-global--disable-hooks ()
  "Remove hooks managed by `mailx-global-mode'."
  (remove-hook 'mu4e-compose-mode-hook #'mailx-set-up-message-mode-buffer)
  (remove-hook 'mu4e-compose-mode-hook #'whitespace-cleanup)
  (remove-hook 'cmdr-spec-mail-activate-hook #'mailx-global--enable-mu4e)
  (remove-hook 'cmdr-spec-mail-activate-hook #'mailx-global--enable-message)
  (dolist (hook '(mu4e-main-mode-hook mu4e-headers-mode-hook mu4e-view-mode-hook))
    (remove-hook hook #'mailx-global--enable-mu4e)
    (remove-hook hook #'mailx-global--enable-message))
  (remove-hook 'message-mode-hook #'mailx-global--enable-message)
  (remove-hook 'after-load-functions #'mailx-global--maybe-enable-home-frames)
  (seq-do (lambda (hook) (remove-hook hook 'cmdr-frame-record-home-frame))
          mailx-frame-home-hooks))

(defconst mailx--display-buffer-entry
  '(mailx--buffer-p (display-buffer-reuse-window) (reusable-frames . t))
  "Entry added to `display-buffer-alist' by `mailx-global-mode'.")

(defun mailx-global--enable-display-buffer ()
  "Register mailx buffer handling in `display-buffer-alist'."
  (add-to-list 'display-buffer-alist mailx--display-buffer-entry))

(defun mailx-global--disable-display-buffer ()
  "Remove mailx buffer handling from `display-buffer-alist'."
  (setq display-buffer-alist
        (delete mailx--display-buffer-entry display-buffer-alist)))

(defconst mailx--mu4e-headers-actions
  '(("generate refile rule" . mailx-generate-refile-rule)))

(defconst mailx--mu4e-view-actions
  '(("unsubscribe" . mailx-unsubscribe)
    ("generate refile rule" . mailx-generate-refile-rule)))

(defvar mailx-global--mu4e-active nil)

(defun mailx-global--enable-mu4e ()
  "Apply mailx configuration to mu4e."
  (when (and (featurep 'mu4e) (not mailx-global--mu4e-active))
    (mailx-views-setup)
    (setq mailx-global--mu4e-active t)
    (when (boundp 'mu4e-headers-actions)
      (dolist (action mailx--mu4e-headers-actions)
        (add-to-list 'mu4e-headers-actions action t)))
    (when (boundp 'mu4e-view-actions)
      (dolist (action mailx--mu4e-view-actions)
        (add-to-list 'mu4e-view-actions action t)))))

(defun mailx-global--disable-mu4e ()
  "Remove mailx configuration from mu4e."
  (when mailx-global--mu4e-active
    (mailx-views-teardown)
    (setq mailx-global--mu4e-active nil)
    (when (boundp 'mu4e-headers-actions)
      (dolist (action mailx--mu4e-headers-actions)
        (setq mu4e-headers-actions (delete action mu4e-headers-actions))))
    (when (boundp 'mu4e-view-actions)
      (dolist (action mailx--mu4e-view-actions)
        (setq mu4e-view-actions (delete action mu4e-view-actions))))))

(defvar mailx-global--saved-header-separator nil
  "Saved `mu4e--header-separator' value prior to `mailx-global-mode'.")

(defvar mailx-global--header-separator-saved-p nil)

(defun mailx-global--enable-mu4e-draft (&rest _args)
  "Apply mailx configuration to mu4e-draft."
  (when (boundp 'mu4e--header-separator)
    (unless mailx-global--header-separator-saved-p
      (setq mailx-global--saved-header-separator mu4e--header-separator
            mailx-global--header-separator-saved-p t))
    (setq mu4e--header-separator
          (propertize (if (boundp 'mail-header-separator)
                          mail-header-separator
                        mailx-header-separator)
                      'read-only t 'intangible t))))

(defun mailx-global--disable-mu4e-draft ()
  "Remove mailx configuration from mu4e-draft."
  (when (boundp 'mu4e--header-separator)
    (when mailx-global--header-separator-saved-p
      (setq mu4e--header-separator mailx-global--saved-header-separator
            mailx-global--saved-header-separator nil
            mailx-global--header-separator-saved-p nil))))

(defvar mailx-global--saved-message-cite-style nil
  "Saved `message-cite-style' value prior to `mailx-global-mode'.")

(defvar mailx-global--message-cite-style-saved-p nil)
(defvar mailx-global--added-discouraged-alternatives nil)

(defun mailx-global--enable-message ()
  "Apply mailx configuration to message."
  (when (boundp 'mm-discouraged-alternatives)
    (dolist (type '("text/richtext" "text/html"))
      (unless (member type mm-discouraged-alternatives)
        (push type mm-discouraged-alternatives)
        (push type mailx-global--added-discouraged-alternatives))))
  (when (and (display-graphic-p) (facep 'message-separator))
    (set-face-attribute 'message-separator nil :background (face-attribute 'default :background nil)))
  (when (boundp 'message-cite-style)
    (unless mailx-global--message-cite-style-saved-p
      (setq mailx-global--saved-message-cite-style message-cite-style
            mailx-global--message-cite-style-saved-p t))
    (when (boundp 'message-cite-style-gmail)
      (setq message-cite-style
            (seq-remove (lambda (binding)
                          (memq (car binding) '(message-citation-line-function
                                                message-citation-line-format)))
                        message-cite-style-gmail)))))

(defun mailx-global--disable-message ()
  "Remove mailx configuration from message."
  (when (boundp 'mm-discouraged-alternatives)
    (dolist (type mailx-global--added-discouraged-alternatives)
      (setq mm-discouraged-alternatives (delete type mm-discouraged-alternatives))))
  (setq mailx-global--added-discouraged-alternatives nil)
  (when (boundp 'message-cite-style)
    (when mailx-global--message-cite-style-saved-p
      (setq message-cite-style mailx-global--saved-message-cite-style
            mailx-global--saved-message-cite-style nil
            mailx-global--message-cite-style-saved-p nil))))

(defvar org-mode-hook)

(defun mailx-global--enable-org (&rest _)
  "Apply mailx configuration to Org, including on first capture."
  (require 'mailx-org)
  (mailx-org-setup)
  (remove-hook 'org-mode-hook #'mailx-global--enable-org)
  (advice-remove 'org-capture #'mailx-global--enable-org)
  (advice-remove 'orgx-capture #'mailx-global--enable-org))

(defun mailx-global--disable-org ()
  "Remove mailx configuration from Org."
  (remove-hook 'org-mode-hook #'mailx-global--enable-org)
  (advice-remove 'org-capture #'mailx-global--enable-org)
  (advice-remove 'orgx-capture #'mailx-global--enable-org)
  (when (featurep 'mailx-org)
    (mailx-org-teardown)))

;;;###autoload
(define-minor-mode mailx-global-mode
  "Global minor mode for mailx routing, attention views, and triage."
  :global t
  :group 'mailx
  (if mailx-global-mode
      (progn
        (mailx-global--apply-settings)
        (mailx-global--enable-advice)
        (mailx-global--enable-hooks)
        (mailx-global--enable-display-buffer)
        (when (featurep 'mu4e) (mailx-global--enable-mu4e))
        (when (featurep 'mu4e-draft) (mailx-global--enable-mu4e-draft))
        (when (featurep 'message) (mailx-global--enable-message))
        (add-hook 'org-mode-hook #'mailx-global--enable-org)
        (advice-add 'org-capture :before #'mailx-global--enable-org)
        (advice-add 'orgx-capture :before #'mailx-global--enable-org)
        (when (featurep 'org)
          (mailx-global--enable-org)))
    (mailx-global--revert-settings)
    (mailx-global--disable-advice)
    (mailx-global--disable-hooks)
    (mailx-global--disable-display-buffer)
    (when (featurep 'mu4e) (mailx-global--disable-mu4e))
    (when (featurep 'mu4e-draft) (mailx-global--disable-mu4e-draft))
    (when (featurep 'message) (mailx-global--disable-message))
    (mailx-global--disable-org)))

;;;###autoload
(defun mailx-setup ()
  "Enable `mailx-global-mode'."
  (interactive)
  (mailx-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Management and Supporting Code

(defun mailx-set-up-message-mode-buffer ()
  "Configure local settings for mu4e composition."
  (setq-local use-hard-newlines t)
  (setq-local make-backup-files nil)
  (if (fboundp 'turn-off-hard-wrap)
      (turn-off-hard-wrap)
    (auto-fill-mode -1)
    (visual-line-mode 1))
  (seq-do #'make-local-variable
          '(mu4e--context-current
            user-mail-address
            user-full-name
            message-directory
            message-auto-save-directory
            message-signature-directory
            message-signature-file
            message-signature
            mail-host-address
            message-sendmail-extra-arguments
            mu4e-get-mail-command))
  (add-hook 'message-send-hook #'mailx--guard-draft-database nil t)
  (add-hook 'message-send-hook #'mailx--guard-draft-identity nil t))

;;;###autoload
(defun mailx-add-refile-rule (rule)
  "Register RULE, a function of one argument MSG, as a refile rule.
RULE should return a maildir folder string, or nil to fall through to the
next registered rule."
  (add-to-list 'mailx-refile-rules rule t))

;;;###autoload
(defun mailx-refile-folder (msg)
  "Return refile folder for MSG by trying each registered rule."
  (or (when-let* ((root (and (boundp 'message-directory) message-directory))
                  (rule (seq-find (lambda (candidate)
                                    (and (mailx-rule-folder candidate)
                                         (mailx-eval-condition
                                          (mailx-rule-match candidate) msg)))
                                  (mailx-rules-load root))))
        (mailx-rule-folder rule))
      (seq-some (lambda (rule) (funcall rule msg)) mailx-refile-rules)
      mailx-archive-folder))

(defun mailx--field-display-value (msg field)
  "Return a human-readable string for FIELD value in MSG."
  (let ((val (mu4e-message-field msg field)))
    (cond
     ((null val) "(none)")
     ((and (listp val) (consp (car val)))
      (mapconcat (lambda (ct)
                   (or (mu4e-contact-name ct)
                       (mu4e-contact-email ct)
                       "?"))
                 val ", "))
     ((stringp val) val)
     (t (format "%S" val)))))

;;;###autoload
(defun mailx-generate-refile-rule (&optional msg)
  "Create a declarative refile rule from MSG or the current message.
Preview matching messages before confirming persistence in the current maildir."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)
                  (user-error "No message at point")))
         (root (or (and (boundp 'message-directory) message-directory)
                   (user-error "No current mail directory")))
         (candidates
          (seq-map (lambda (entry)
                     (cons (symbol-name (car entry))
                           (mailx--field-display-value msg (car entry))))
                   mailx--refile-matchable-fields))
         (field-name (annotated-completing-read
                      candidates :prompt "Match field: " :require-match t))
         (field (intern field-name))
         (regex (read-string
                 (format "Regex (current: %s): "
                         (mailx--field-display-value msg field))))
         (folder (read-string "Refile to folder: " mailx-archive-folder))
         (tier-name (annotated-completing-read
                     (seq-map (lambda (tier) (cons (symbol-name tier) nil))
                              mailx-attention-tiers)
                     :prompt "Attention tier: " :require-match t))
         (id-name (read-string "Rule ID (letters, digits, hyphens): ")))
    (unless (and (assoc field mailx--refile-matchable-fields)
                 (member tier-name (seq-map #'symbol-name mailx-attention-tiers)))
      (user-error "Invalid field or attention tier"))
    (unless (string-match-p "\\`[[:alnum:]][[:alnum:]-]*\\'" id-name)
      (user-error "Invalid rule ID"))
    (unless (and (string-prefix-p "/" folder)
                 (not (member ".." (split-string folder "/" t))))
      (user-error "Invalid maildir folder"))
    (condition-case nil
        (string-match-p regex "")
      (invalid-regexp (user-error "Invalid regular expression")))
    (let* ((id (intern id-name))
           (rules (mailx-rules-load root))
           (rule (make-mailx-rule :id id :match (list field regex)
                                  :folder folder :tier (intern tier-name)
                                  :source 'manual
                                  :created (format-time-string "%Y-%m-%d"))))
      (when (seq-some (lambda (existing) (eq id (mailx-rule-id existing))) rules)
        (user-error "Rule ID already exists: %s" id-name))
      (let ((preview (mailx-test-rule root rule)))
        (when (yes-or-no-p
               (format "Rule matches %d messages%s; save? "
                       (map-elt preview :count)
                       (if-let* ((subjects (map-elt preview :subjects)))
                           (format " (e.g. %s)" (mapconcat #'identity subjects "; "))
                         "")))
          (mailx-rules-save root (append rules (list rule)))
          (message "Saved mailx rule %s" id-name)
          rule)))))

;;;###autoload
(defun mailx-compose-reply ()
  "Ask whether to reply-to-all or not."
  (interactive)
  (mu4e-compose-reply (yes-or-no-p "Reply to all?")))

;;;###autoload
(defun mailx-unsubscribe (msg)
  "Unsubscribe from a mailing list using the List-Unsubscribe header in MSG.
For mailto: URIs, opens a compose buffer pre-filled with the unsubscribe
address, subject, and body.  For https: URIs, opens the URL in a browser."
  (interactive (list (mu4e-message-at-point)))
  (let* ((header (or (mu4e-fetch-field msg "List-Unsubscribe")
                     (user-error "No List-Unsubscribe header found")))
         (uris (let (result)
                 (with-temp-buffer
                   (insert header)
                   (goto-char (point-min))
                   (while (re-search-forward "<\\([^>]+\\)>" nil t)
                     (push (match-string 1) result)))
                 (nreverse result))))
    (unless uris
      (user-error "No unsubscribe URIs in List-Unsubscribe header"))
    (let ((uri (if (cdr uris)
                   (completing-read "Unsubscribe via: " uris nil t)
                 (car uris))))
      (cond
       ((string-prefix-p "mailto:" uri)
        (require 'url-mailto)
        (url-mailto (url-generic-parse-url uri)))
       ((string-match-p "\\`https?://" uri)
        (browse-url uri))
       (t
        (user-error "Unrecognized URI scheme in List-Unsubscribe: %s" uri))))))

;; display-buffer: mu4e buffers prefer the frame they were first shown on
(defun mailx--buffer-p (buf _action)
  "Return non-nil if BUF uses a mu4e major mode."
  (with-current-buffer buf
    (derived-mode-p 'mu4e-main-mode 'mu4e-headers-mode
                    'mu4e-view-mode 'mu4e-compose-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contexts and database switching

(defvar mailx-prepare-context-hook nil
  "Hook run before selecting a mail context.")

;;;###autoload
(defun mailx-select-account ()
  "Select a mu4e context, updating the current draft when appropriate."
  (interactive)
  (run-hooks 'mailx-prepare-context-hook)
  (if (derived-mode-p 'mu4e-compose-mode)
      (call-interactively #'mu4e-compose-context-switch)
    (call-interactively #'mu4e-context-switch)))

(defun mailx--guard-draft-database (&rest _args)
  "Refuse draft operations outside the active mu database's mail root."
  (let* ((home (mailx-server-current-home))
         (root (and home (file-name-directory (directory-file-name home)))))
    (unless (and root buffer-file-name (mu4e-running-p)
                 (file-in-directory-p buffer-file-name root))
      (user-error "Switch to this draft's database before saving or sending"))))

(defun mailx--ad-guard-save (&rest _args)
  "Guard saving mu4e drafts.
`before-save-hook' errors are demoted by `basic-save-buffer', so this
must be advice to be able to abort the save."
  (when (derived-mode-p 'mu4e-compose-mode)
    (mailx--guard-draft-database)))

(defun mailx--guard-draft-identity (&rest _args)
  "Refuse sending when the From header disagrees with the selected identity."
  (require 'mail-extr)
  (let* ((from (message-field-value "From"))
         (address (and from (cadr (mail-extract-address-components from)))))
    (unless (and address user-mail-address
                 (equal (downcase address) (downcase user-mail-address)))
      (user-error "Select the context matching this draft's From before sending"))))

(defun mailx--ad-start-for-draft (&rest _args)
  "Start the default database and await its readiness before composing."
  (when mailx--transitioning
    (user-error "Wait for the mail database switch before composing"))
  (mailx-server-ensure-ready))

(defun mailx--ad-draft (original &rest args)
  "Call ORIGINAL with ARGS, isolating the draft's selected identity."
  (let ((mailx--composing t)
        (mu4e--context-current mu4e--context-current)
        (user-mail-address user-mail-address)
        (user-full-name user-full-name)
        (message-signature-directory message-signature-directory)
        (message-signature-file message-signature-file)
        (message-signature message-signature)
        (mail-host-address mail-host-address)
        (message-sendmail-extra-arguments message-sendmail-extra-arguments))
    (apply original args)))

;;;###autoload
(cl-defun mailx-make-context (&key name full-name address maildir command)
  "Build a mu4e context NAME for ADDRESS and FULL-NAME.
MAILDIR is the account's mail root and COMMAND fetches its mail."
  (let ((maildir (expand-file-name maildir)))
    (make-mu4e-context
     :name name
     :enter-func (lambda ()
                   (unless (or mailx--composing
                               (derived-mode-p 'mu4e-compose-mode))
                     (mailx-server-switch-database
                      (file-name-concat maildir ".mu"))
                     (setq message-directory maildir)
                     (setq message-auto-save-directory (file-name-concat maildir mailx-drafts-folder))
                     (setq mu4e-get-mail-command command)))
     :match-func
     (lambda (msg)
       (when msg
         (seq-some (lambda (contact)
                     (when-let* ((email (mu4e-contact-email contact)))
                       (equal (downcase email) (downcase address))))
                   (append (mu4e-message-field msg :to)
                           (mu4e-message-field msg :cc)))))
     :vars `((user-mail-address . ,address)
             (user-full-name . ,full-name)
             (message-signature-directory . ,(file-name-concat maildir mailx-signatures-dir))
             (message-signature-file . ,address)
             (message-signature . t)
             (mail-host-address . ,(replace-regexp-in-string ".*@" "" address))
             (message-sendmail-extra-arguments . ,(list "-a" address))))))

(provide 'mailx)
;;; mailx.el ends here
