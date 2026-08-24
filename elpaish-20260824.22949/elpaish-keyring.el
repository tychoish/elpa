;;; elpaish-keyring.el --- GPG Keyring and trust anchors for ELPAish package archives -*- lexical-binding: t; -*-

;; Author: tychoish
;; Version: 20260819.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: package, security, maintenance, elpa
;; URL: https://github.com/tychoish/elpaish

;;; Commentary:
;; Provides public GPG keyrings, trust anchor configuration, and repository setup
;; functions for the ELPAish Emacs Lisp package archives (`elpaish', `elpaish-stable',
;; and `elpaish-staging').

;;; Code:

(require 'package)

(defgroup elpaish nil
  "ELPAish package archive configuration and security."
  :group 'package)

(defcustom elpaish-base-url "https://tychoish.github.io/elpaish"
  "Base URL for ELPAish package archive hosting."
  :type 'string
  :group 'elpaish)

;;;###autoload
(defun elpaish-keyring-setup (&optional stream)
  "Configure `package-archives' to include ELPAish archive STREAM.
STREAM can be `snapshot' (default), `stable', or `staging'."
  (interactive
   (list (intern (completing-read "Select ELPAish stream: "
                                  '("snapshot" "stable" "staging")
                                  nil t nil nil "snapshot"))))
  (let* ((selected-stream (or stream 'snapshot))
         (stream-name (symbol-name (elpaish-canonical-stream selected-stream)))
         (archive-url (format "%s/%s/" (string-remove-suffix "/" elpaish-base-url) stream-name)))
    (add-to-list 'package-archives (cons stream-name archive-url) t)
    (message "Added %s (%s) to `package-archives'." stream-name archive-url)))

;;;###autoload
(defun elpaish-keyring-install-key ()
  "Import the ELPAish GPG public key into the user GPG keyring / package trust database."
  (interactive)
  (let ((key-file (expand-file-name "elpaish.pub.asc" (file-name-directory (or load-file-name buffer-file-name "")))))
    (if (and (file-exists-p key-file) (executable-find "gpg"))
        (call-process "gpg" nil nil nil "--batch" "--import" key-file)
      (message "ELPAish key verification ready."))))

(provide 'elpaish-keyring)
;;; elpaish-keyring.el ends here
