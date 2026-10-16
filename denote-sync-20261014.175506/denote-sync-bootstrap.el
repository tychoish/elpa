;;; denote-sync-bootstrap.el --- Onboarding and setup wizard for denote-sync -*- lexical-binding: t; -*-

;; Author: Sam Kleinman <sam@tychoish.com>
;; Maintainer: Sam Kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (denote-sync "0.2.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/denote-sync

;;; Commentary:
;; Interactive setup wizard for denote-sync Google Docs synchronization.
;; Guides users through gog CLI discovery, credentials registration,
;; file keyring configuration, OAuth authorization with sensitive-scope
;; minimization (--drive-scope=file), and configuration snippet generation
;; for ~/.emacs.d/user/ (e.g. personal.el).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'denote-sync-gdocs)

(defgroup denote-sync-bootstrap nil
  "Onboarding and setup wizard for `denote-sync'."
  :group 'denote-sync
  :prefix "denote-sync-bootstrap-")

(defcustom denote-sync-bootstrap-preferred-config-file
  (expand-file-name "~/.emacs.d/user/personal.el")
  "Target configuration file path for user-specific credentials."
  :type 'file
  :group 'denote-sync-bootstrap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verification and helper functions

(defun denote-sync-bootstrap-check-gog ()
  "Check whether `gog' executable is available in `exec-path'.
Returns the executable path, or signals a user-error with installation instructions."
  (let ((path (executable-find denote-sync-gdocs-gog-executable)))
    (unless path
      (user-error
       "The `gog' CLI was not found in PATH.\nInstall with:\n  go install github.com/openclaw/gogcli/cmd/gog@latest\n  sudo ln -sf ~/go/bin/gog /usr/local/bin/gog"))
    path))

(defun denote-sync-bootstrap-configure-credentials (&optional client-json-path client-id client-secret)
  "Register OAuth client credentials with `gog'.
CLIENT-JSON-PATH is a path to a downloaded Google credentials JSON file.
Alternatively, CLIENT-ID and CLIENT-SECRET can be passed directly."
  (denote-sync-bootstrap-check-gog)
  (let ((temp-json nil)
        (path client-json-path))
    (unwind-protect
        (progn
          (when (and (not path) (or client-id denote-sync-gdocs-client-id))
            (let* ((cid (or client-id denote-sync-gdocs-client-id))
                   (sec (or client-secret denote-sync-gdocs-client-secret ""))
                   (payload (list :installed
                                  (list :client_id cid
                                        :client_secret sec
                                        :auth_uri "https://accounts.google.com/o/oauth2/auth"
                                        :token_uri "https://oauth2.googleapis.com/token"))))
              (setq temp-json (make-temp-file "gog-creds-" nil ".json"))
              (with-temp-file temp-json
                (insert (json-serialize payload)))
              (setq path temp-json)))
          (when path
            (unless (file-exists-p path)
              (user-error "Credentials file does not exist: %s" path))
            (let ((exit-code (call-process denote-sync-gdocs-gog-executable
                                           nil nil nil
                                           "auth" "credentials" "set"
                                           (expand-file-name path) "--insecure")))
              (unless (zerop exit-code)
                (user-error "Failed to register credentials with gog (exit %d)" exit-code))
              (message "Successfully registered OAuth credentials with gog."))))
      (when (and temp-json (file-exists-p temp-json))
        (delete-file temp-json)))))

(defun denote-sync-bootstrap-configure-keyring ()
  "Switch `gog' keyring backend to encrypted file storage."
  (denote-sync-bootstrap-check-gog)
  (let ((exit-code (call-process denote-sync-gdocs-gog-executable
                                 nil nil nil
                                 "auth" "keyring" "file")))
    (unless (zerop exit-code)
      (user-error "Failed to set gog keyring to file backend (exit %d)" exit-code))
    (message "Configured gog keyring backend: file (~/.local/share/gogcli/keyring).")))

(defun denote-sync-bootstrap-doctor ()
  "Run `gog auth doctor --json' and return parsed health diagnostic alist."
  (denote-sync-bootstrap-check-gog)
  (let* ((stdout-buf (generate-new-buffer " *gog-doctor-out*"))
         (exit-code (call-process denote-sync-gdocs-gog-executable
                                  nil (list stdout-buf nil) nil
                                  "auth" "doctor" "--json"))
         (output (with-current-buffer stdout-buf (buffer-string))))
    (kill-buffer stdout-buf)
    (condition-case nil
        (json-parse-string output :object-type 'alist :array-type 'list)
      (error
       (list (cons 'raw-exit exit-code)
             (cons 'raw-output output))))))

(defun denote-sync-bootstrap-format-user-config (account keyring-password &optional target-file)
  "Format an Emacs Lisp configuration snippet for user configuration.
ACCOUNT is the Google email address.
KEYRING-PASSWORD is the keyring passphrase.
TARGET-FILE is the suggested destination path (defaults to
`denote-sync-bootstrap-preferred-config-file')."
  (let ((target (or target-file denote-sync-bootstrap-preferred-config-file)))
    (format
     ";; ====================================================================\n;; denote-sync Google Docs Configuration\n;; Destination: %s\n;; Generated on: %s\n;; ====================================================================\n\n(setenv \"GOG_KEYRING_PASSWORD\" %S)\n\n(setq denote-sync-gdocs-default-account %S)\n\n;; Scope minimization: use Sensitive Scope (drive.file) rather than Restricted Scope\n(setq denote-sync-gdocs-drive-scope \"file\")\n\n;; Default parent targets for note synchronization\n(add-to-list 'denote-sync-parent-registry\n             '(\"Google Docs\" . (google-docs . (%S . nil))))\n"
     target
     (format-time-string "%Y-%m-%d %T")
     keyring-password
     account
     account)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive Wizard

;;;###autoload
(defun denote-sync-bootstrap ()
  "Run the interactive onboarding wizard for denote-sync Google Docs."
  (interactive)
  (message "Starting denote-sync Google Docs setup wizard...")
  (denote-sync-bootstrap-check-gog)

  ;; Step 1: Credentials
  (let ((creds-choice (read-char-choice
                       "Credentials: [1] Provide OAuth client JSON file  [2] Enter Client ID & Secret  [3] Skip/Keep existing: "
                       '(?1 ?2 ?3))))
    (pcase creds-choice
      (?1
       (let ((file (read-file-name "Path to downloaded Google OAuth client JSON file: ")))
         (denote-sync-bootstrap-configure-credentials file)))
      (?2
       (let ((cid (read-string "Google OAuth Client ID: "))
             (sec (read-passwd "Google OAuth Client Secret: ")))
         (denote-sync-bootstrap-configure-credentials nil cid sec)))
      (?3
       (message "Keeping existing gog credentials configuration."))))

  ;; Step 2: Keyring backend
  (denote-sync-bootstrap-configure-keyring)

  ;; Step 3: Keyring passphrase
  (let* ((env-pass (getenv "GOG_KEYRING_PASSWORD"))
         (passphrase (if (and env-pass (not (string-empty-p env-pass)))
                         env-pass
                       (read-passwd "Enter passphrase for local encrypted keyring: "))))
    (when (string-empty-p passphrase)
      (user-error "Keyring passphrase cannot be empty"))
    (setenv "GOG_KEYRING_PASSWORD" passphrase)

    ;; Step 4: Account authorization
    (let ((email (read-string "Google Account email to authorize: ")))
      (when (string-empty-p email)
        (user-error "Email address cannot be empty"))
      (denote-sync-gdocs-add-account email)

      ;; Step 5: Format configuration snippet
      (let* ((snippet (denote-sync-bootstrap-format-user-config email passphrase))
             (buf (get-buffer-create "*denote-sync-config*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert snippet)
          (emacs-lisp-mode)
          (goto-char (point-min)))
        (display-buffer buf)
        (kill-new snippet)

        ;; Offer to append to ~/.emacs.d/user/personal.el if writable directory exists
        (let ((user-file denote-sync-bootstrap-preferred-config-file))
          (when (and (file-writable-p (file-name-directory user-file))
                     (y-or-n-p (format "Append configuration snippet to %s? " user-file)))
            (with-temp-buffer
              (insert "\n" snippet)
              (append-to-file (point-min) (point-max) user-file))
            (message "Appended denote-sync configuration to %s" user-file)))

        (message "Setup complete! Configuration snippet copied to kill-ring and displayed in *denote-sync-config*.")))))

(provide 'denote-sync-bootstrap)
;;; denote-sync-bootstrap.el ends here
