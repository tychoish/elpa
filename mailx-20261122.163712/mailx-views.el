;;; mailx-views.el --- Attention-oriented mu4e views -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (annotated-completing-read "0.1.0"))
;; Keywords: mail, convenience
;; URL: https://github.com/tychoish/mailx

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Configurable attention bookmarks and a tier column for mu4e.
;; Call `mailx-views-setup' after mu4e has loaded.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'mailx-server)

(declare-function annotated-completing-read "annotated-completing-read")
(declare-function mu4e-message-field "mu4e-message")
(declare-function mu4e-search "mu4e-search")

(defvar mu4e-bookmarks nil)
(defvar mu4e-header-info-custom nil)
(defvar mu4e-headers-fields nil)
(defvar mu4e-headers-custom-markers nil)

(defgroup mailx-views nil
  "Attention-oriented mail views."
  :group 'mail)

(defconst mailx-views--tiers
  '((reply "Needs reply" ?1) (read "Should read" ?2)
    (cursory "Cursory review" ?3) (bulk "Bulk" ?4) (junk "Junk" ?5))
  "Tier symbols, display names and bookmark keys.")

(defcustom mailx-views-unclassified-query
  "NOT (tag:\"tier:reply\" OR tag:\"tier:read\" OR tag:\"tier:cursory\" OR tag:\"tier:bulk\" OR tag:\"tier:junk\")"
  "Query for unclassified messages.  Restrict this to staging folders if desired."
  :type 'string :group 'mailx-views)

(defcustom mailx-views-records-query "tag:retain OR tag:receipt"
  "Query for saved receipts and records."
  :type 'string :group 'mailx-views)

(defun mailx-views-tier-query (tier)
  "Return a mu query for attention TIER."
  (unless (assq tier mailx-views--tiers)
    (user-error "Unknown attention tier: %s" tier))
  (format "tag:\"tier:%s\"" tier))

(defun mailx-views-bookmarks ()
  "Return the attention bookmarks for the current configuration."
  (append
   (list (list :name "Unclassified" :query mailx-views-unclassified-query :key ?u))
   (seq-map (lambda (tier)
              (list :name (nth 1 tier) :query (mailx-views-tier-query (car tier))
                    :key (nth 2 tier)))
            mailx-views--tiers)
   (list (list :name "Receipts and records" :query mailx-views-records-query :key ?r)
         (list :name "Today" :query "date:today..now" :key ?t)
         (list :name "Last 7 days" :query "date:7d..now" :key ?w))))

(defun mailx-views--tier (message)
  "Return the recognized attention tier in MESSAGE, or nil."
  (let ((tags (mu4e-message-field message :tags)))
    (seq-find (lambda (tier) (member (format "tier:%s" tier) tags))
              (seq-map #'car mailx-views--tiers))))

(defun mailx-views--tier-column (message)
  "Return the display badge for MESSAGE."
  (if-let* ((tier (mailx-views--tier message)))
      (capitalize (symbol-name tier))
    ""))

(defun mailx-views--matches-tier-p (message tier)
  "Return non-nil when MESSAGE has attention TIER."
  (eq (mailx-views--tier message) tier))

(defun mailx-views--read-tier ()
  "Read a tier for a mu4e custom marker."
  (intern (annotated-completing-read
           (seq-map (lambda (tier) (cons (symbol-name (car tier)) (nth 1 tier)))
                    mailx-views--tiers)
           :prompt "Attention tier: " :require-match t)))

(defun mailx-views--unread-count (tier)
  "Count unread messages for TIER in the active database, or return nil."
  (when-let* ((home (mailx-server-current-home)))
    (let ((stderr-file (make-temp-file "mailx-mu-stderr-")))
      (unwind-protect
          (condition-case nil
              (with-temp-buffer
                (let ((status (process-file "mu" nil (list t stderr-file) nil
                                            "find" (concat "--muhome=" home)
                                            "--fields=i" "--nocolor"
                                            (format "flag:unread AND %s"
                                                    (mailx-views-tier-query tier)))))
                  (cond
                   ((zerop status) (count-lines (point-min) (point-max)))
                   ((= status 2)
                    (let ((output (buffer-string)))
                      (with-temp-buffer
                        (insert-file-contents stderr-file)
                        (when (string-match-p
                               "no match\\(es\\|ing messages\\)"
                               (concat output "\n" (buffer-string)))
                          0)))))))
            (file-error nil))
        (delete-file stderr-file)))))

;;;###autoload
(defun mailx-jump-to-tier ()
  "Select an attention tier and search it in the current mu4e database."
  (interactive)
  (let* ((table (seq-map (lambda (tier)
                           (let ((count (mailx-views--unread-count (car tier))))
                             (cons (symbol-name (car tier))
                                   (format "%s; %s unread" (nth 1 tier)
                                           (if count count "count unavailable")))))
                         mailx-views--tiers))
         (choice (annotated-completing-read table :prompt "Attention tier: "
                                            :require-match t :or-nil t)))
    (when choice
      (mu4e-search (mailx-views-tier-query (intern choice))))))

(defvar mailx-views--saved-bookmarks nil
  "Saved bookmarks prior to `mailx-views-setup'.")

(defvar mailx-views--saved-headers-fields nil
  "Saved headers fields prior to `mailx-views-setup'.")

(defvar mailx-views--saved-custom-markers nil
  "Saved custom markers prior to `mailx-views-setup'.")

(defvar mailx-views--saved-header-info nil
  "Saved header info for :mailx-tier prior to `mailx-views-setup'.")

;;;###autoload
(defun mailx-views-setup ()
  "Install attention bookmarks, tier column, and a custom tier marker."
  (setq mailx-views--saved-bookmarks mu4e-bookmarks)
  (setq mailx-views--saved-headers-fields mu4e-headers-fields)
  (setq mailx-views--saved-custom-markers mu4e-headers-custom-markers)
  (setq mailx-views--saved-header-info (alist-get :mailx-tier mu4e-header-info-custom))
  (setq mu4e-bookmarks (mailx-views-bookmarks))
  (setf (alist-get :mailx-tier mu4e-header-info-custom)
        '(:name "Attention tier" :shortname "Tier" :help "Attention tier"
          :function mailx-views--tier-column))
  (unless (assq :mailx-tier mu4e-headers-fields)
    (setq mu4e-headers-fields (cons '(:mailx-tier . 8) mu4e-headers-fields)))
  (setq mu4e-headers-custom-markers
        (cons '("Attention tier" mailx-views--matches-tier-p mailx-views--read-tier)
              (seq-remove (lambda (marker) (equal (car marker) "Attention tier"))
                          mu4e-headers-custom-markers))))

;;;###autoload
(defun mailx-views-teardown ()
  "Remove attention bookmarks, tier column, and tier marker."
  (when mailx-views--saved-bookmarks
    (setq mu4e-bookmarks mailx-views--saved-bookmarks)
    (setq mailx-views--saved-bookmarks nil))
  (if mailx-views--saved-header-info
      (setf (alist-get :mailx-tier mu4e-header-info-custom) mailx-views--saved-header-info)
    (setq mu4e-header-info-custom
          (seq-remove (lambda (elt) (eq (car elt) :mailx-tier)) mu4e-header-info-custom)))
  (if mailx-views--saved-headers-fields
      (setq mu4e-headers-fields mailx-views--saved-headers-fields)
    (setq mu4e-headers-fields
          (seq-remove (lambda (elt) (eq (car elt) :mailx-tier)) mu4e-headers-fields)))
  (setq mu4e-headers-custom-markers
        (seq-remove (lambda (marker) (equal (car marker) "Attention tier"))
                    mu4e-headers-custom-markers))
  (setq mailx-views--saved-header-info nil
        mailx-views--saved-headers-fields nil
        mailx-views--saved-custom-markers nil))

(provide 'mailx-views)
;;; mailx-views.el ends here
