;;; mailx-review.el --- Review and edit proposed mail classifications -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (hitl "0.1.0") (annotated-completing-read "0.1.0"))
;; Keywords: mail, convenience
;; URL: https://github.com/tychoish/mailx

;; This file is not part of GNU Emacs.

;;; Commentary:
;; `mailx-review' classifies unclassified mail without changing any of it and
;; lists each proposal in a tabulated buffer.  Proposals can be edited,
;; accepted, skipped, or promoted to a persistent rule.  Nothing is moved from
;; this buffer: accepted entries become a `mailx' hitl-plan that a human
;; reviews and applies, which is where exclusive Maildir access is asserted.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'mailx)
(require 'mailx-classify)
(require 'mailx-rules)
(require 'mailx-bulk)

(declare-function annotated-completing-read "annotated-completing-read")
(declare-function hitl-plan-id "hitl-plan" (plan))
(declare-function hitl-plan-get "hitl-plan" (id &optional directory domain))
(declare-function hitl-plan-review "hitl-plan-ui" (plan-or-id &optional display))
(declare-function mu4e-context-current "mu4e-context")
(declare-function mu4e-context-name "mu4e-context")

(defgroup mailx-review nil
  "Review buffer for proposed mail classifications."
  :group 'mailx
  :prefix "mailx-review-")

(defcustom mailx-review-limit 100
  "Maximum number of messages `mailx-review' classifies at once."
  :type 'integer
  :group 'mailx-review)

(defvar-local mailx-review--root nil
  "Mail root whose proposals the current review buffer shows.")

(defvar-local mailx-review--notices nil
  "Load notices for the current review buffer, a list of (SHORT . LONG).
SHORT appears in the mode line, LONG is messaged once when loading.")

(defvar-local mailx-review--entries nil
  "Review entries, in display order.
Each entry is a plist with :message :tier :folder :tags :source :status.
:status is one of `pending', `accepted', `skipped' or `planned'.
A planned entry also has :plan, the id of the hitl plan holding it.")

;;; Entries

(defun mailx-review--entry (message tier folder tags source)
  "Return a pending review entry for MESSAGE and its proposal.
TIER, FOLDER and TAGS are the proposed classification; SOURCE says where
the proposal came from."
  (list :message message :tier tier :folder folder :tags tags
        :source source :status 'pending))

(defun mailx-review--result-entry (result)
  "Return a review entry for a `mailx-classify-batch' RESULT."
  (let ((rule (map-elt (map-elt result :classification) :rule)))
    (mailx-review--entry (map-elt result :message)
                         (map-elt result :tier)
                         (map-elt result :folder)
                         (map-elt result :tags)
                         (cond
                          (rule (format "rule:%s" (mailx-rule-id rule)))
                          (t "heuristic")))))

(defun mailx-review--unresolved-entry (message)
  "Return an empty-proposal entry for MESSAGE, which no stage classified."
  (mailx-review--entry message nil (map-elt message :maildir) nil "none"))

(defun mailx-review--entry-id (entry)
  "Return the identifier of ENTRY, its message path."
  (map-elt (map-elt entry :message) :path))

(defun mailx-review--entry-at-point ()
  "Return the entry on the current line."
  (or (when-let* ((id (tabulated-list-get-id)))
        (seq-find (lambda (entry) (equal (mailx-review--entry-id entry) id))
                  mailx-review--entries))
      (user-error "No message on this line")))

(defun mailx-review--targets ()
  "Return the entries in the active region, else the one at point."
  (cond
   ((use-region-p)
    (let ((end (region-end))
          (ids nil))
      (save-excursion
        (goto-char (region-beginning))
        (while (and (< (point) end) (not (eobp)))
          (when-let* ((id (tabulated-list-get-id)))
            (push id ids))
          (forward-line 1)))
      (or (seq-filter (lambda (entry) (member (mailx-review--entry-id entry) ids))
                      mailx-review--entries)
          (user-error "No messages in region"))))
   (t (list (mailx-review--entry-at-point)))))

;;; Display

(defun mailx-review--row (entry)
  "Return the `tabulated-list-entries' row for ENTRY."
  (let ((message (map-elt entry :message))
        (tier (map-elt entry :tier)))
    (list (mailx-review--entry-id entry)
          (vector (symbol-name (map-elt entry :status))
                  (if tier (symbol-name tier) "-")
                  (or (map-elt entry :folder) "-")
                  (string-join (map-elt entry :tags) ",")
                  (map-elt entry :source)
                  (or (car (mailx--sender-addresses (map-elt message :from))) "")
                  (or (map-elt message :subject) "")))))

(defun mailx-review--render ()
  "Redraw the review table, keeping point."
  (setq tabulated-list-entries (seq-map #'mailx-review--row mailx-review--entries))
  (tabulated-list-print t))

(defconst mailx-review--stale-hint
  "run `mu4e-update-index' and press g to reload"
  "What to do when the mu index lists files that no longer exist.")

(defun mailx-review--load ()
  "Classify unclassified mail without changing it and fill the buffer.
Messages whose file is gone (the mu index is stale) are dropped and counted."
  (let* ((result (mailx-classify-batch mailx-review--root
                                       (list :dry-run t :limit mailx-review-limit)))
         (stale 0)
         (live (lambda (message)
                 (or (file-exists-p (map-elt message :path))
                     (progn (setq stale (1+ stale)) nil)))))
    (setq mailx-review--entries
          (append (seq-map #'mailx-review--result-entry
                           (seq-filter (lambda (res) (funcall live (map-elt res :message)))
                                       (map-elt result :results)))
                  (seq-map #'mailx-review--unresolved-entry
                           (seq-filter live (map-elt result :unresolved)))))
    (mailx-review--render)
    (let ((correspondents (map-elt result :correspondents)))
      (setq mailx-review--notices
            (delq nil
                  (list
                   (cond
                    ((eq correspondents 'skipped-no-sent-query)
                     (cons "no sent query"
                           "mailx-sent-query is unset, so known correspondents are not detected and nothing is proposed as reply; set it to a mu query matching mail you sent"))
                    ((eq (car-safe correspondents) 'error)
                     (cons "correspondents failed"
                           (format "Refreshing correspondents failed: %s" (cdr correspondents)))))
                   (when (> stale 0)
                     (cons (format "%d stale" stale)
                           (format "Dropped %d messages whose files no longer exist; %s"
                                   stale mailx-review--stale-hint)))
                   (when (map-elt result :truncated)
                     (cons (format "first %d" (length mailx-review--entries))
                           (format "Showing the first %d messages and more may remain; raise `mailx-review-limit' or press g after applying"
                                   (length mailx-review--entries))))))))
    (setq mode-line-process
          (when mailx-review--notices
            (format " [%s]" (string-join (seq-map #'car mailx-review--notices) "; "))))
    (force-mode-line-update)
    (when mailx-review--notices
      (message "mailx: %s" (string-join (seq-map #'cdr mailx-review--notices) "; ")))
    result))

(defun mailx-review--revert (&optional _ignore-auto _noconfirm)
  "Reclassify, asking first when that would discard review decisions."
  (when (and (seq-some (lambda (entry) (not (eq (map-elt entry :status) 'pending)))
                       mailx-review--entries)
             (not (yes-or-no-p "Discard accept/skip decisions and reclassify? ")))
    (user-error "Reclassification cancelled"))
  (mailx-review--load))

;;; Editing

(defun mailx-review--check-editable (entries)
  "Signal a `user-error' if any of ENTRIES is already in a plan."
  (when (seq-some (lambda (entry) (eq (map-elt entry :status) 'planned)) entries)
    (user-error "Entry is already in a plan")))

(defun mailx-review-edit (entries prop value)
  "Set PROP of ENTRIES to VALUE after validating it.
PROP is :tier, :folder or :tags.  An edited accepted entry returns to
pending so every accepted entry matches what was reviewed."
  (mailx-review--check-editable entries)
  (pcase prop
    (:folder (mailx-bulk-check-folder value))
    (:tier (unless (memq value mailx-attention-tiers)
             (user-error "Unknown attention tier: %S" value)))
    (:tags (mailx--validated-classification
            (list :tier (car mailx-attention-tiers) :tags value)))
    (_ (error "Cannot edit %S" prop)))
  (seq-do (lambda (entry)
            (map-put! entry prop value)
            (when (eq (map-elt entry :status) 'accepted)
              (map-put! entry :status 'pending)))
          entries)
  entries)

(defun mailx-review--read-tier ()
  "Read an attention tier."
  (intern (annotated-completing-read
           (seq-map (lambda (tier) (cons (symbol-name tier) nil)) mailx-attention-tiers)
           :prompt "Attention tier: " :require-match t)))

(defun mailx-review-set-tier ()
  "Set the proposed tier of the entry at point, or of the entries in the region."
  (interactive)
  (mailx-review-edit (mailx-review--targets) :tier (mailx-review--read-tier))
  (mailx-review--render))

(defun mailx-review-set-folder ()
  "Set the proposed folder of the entry at point, or of the entries in the region."
  (interactive)
  (let ((entries (mailx-review--targets)))
    (mailx-review-edit entries :folder
                       (read-string "Folder: " (map-elt (car entries) :folder)))
    (mailx-review--render)))

(defun mailx-review-set-tags ()
  "Set the proposed tags of the entry at point, or of the entries in the region."
  (interactive)
  (let ((entries (mailx-review--targets)))
    (mailx-review-edit entries :tags
                       (split-string
                        (read-string "Tags (space separated): "
                                     (string-join (map-elt (car entries) :tags) " "))
                        " " t))
    (mailx-review--render)))

;;; Accepting

(defun mailx-review-mark (entries status)
  "Set the review STATUS of ENTRIES to `accepted' or `skipped'.
Accepting requires every entry to have both a tier and a folder."
  (mailx-review--check-editable entries)
  (when (and (eq status 'accepted)
             (seq-some (lambda (entry)
                         (not (and (map-elt entry :tier) (map-elt entry :folder))))
                       entries))
    (user-error "Set a tier and folder before accepting"))
  (seq-do (lambda (entry) (map-put! entry :status status)) entries)
  entries)

(defun mailx-review-accept ()
  "Accept the proposal at point, or in the region, and move on."
  (interactive)
  (mailx-review-mark (mailx-review--targets) 'accepted)
  (mailx-review--render)
  (forward-line 1))

(defun mailx-review-skip ()
  "Skip the entry at point, or in the region, and move on."
  (interactive)
  (mailx-review-mark (mailx-review--targets) 'skipped)
  (mailx-review--render)
  (forward-line 1))

(defun mailx-review-accept-all ()
  "Accept every pending entry that already has a complete proposal."
  (interactive)
  (let ((ready (seq-filter (lambda (entry)
                             (and (eq (map-elt entry :status) 'pending)
                                  (map-elt entry :tier)
                                  (map-elt entry :folder)))
                           mailx-review--entries)))
    (unless ready
      (user-error "No pending entries have a complete proposal"))
    (mailx-review-mark ready 'accepted)
    (mailx-review--render)
    (message "Accepted %d entries" (length ready))))

;;; Applying and promoting

(defun mailx-review--target (entry root)
  "Return the hitl target for ENTRY under ROOT."
  (let ((message (map-elt entry :message)))
    (unless (map-elt message :message-id)
      (user-error "Message has no Message-ID: %s" (or (map-elt message :subject) "")))
    (list :root root
          :context (when (fboundp 'mu4e-context-current)
                     (when-let* ((context (mu4e-context-current)))
                       (mu4e-context-name context)))
          :message-id (map-elt message :message-id)
          :subject (map-elt message :subject)
          :path (map-elt message :path))))

(defun mailx-review-create-plan (entries root)
  "Create a hitl plan applying the classification of each of ENTRIES under ROOT.
Entries sharing a tier, folder and tags share one `classify' operation."
  (let ((groups (seq-group-by (lambda (entry)
                                (list (map-elt entry :tier)
                                      (map-elt entry :tags)
                                      (map-elt entry :folder)))
                              entries)))
    (mailx-bulk-ensure-domain)
    (let ((plan (hitl-plan-create :domain 'mailx :description "mailx-review")))
      (seq-do (lambda (group)
                (hitl-plan-add-op plan
                                  (cons 'classify (car group))
                                  (seq-map (lambda (entry) (mailx-review--target entry root))
                                           (cdr group))))
              groups)
      plan)))

(defun mailx-review-apply ()
  "Turn the accepted entries into a hitl plan and open it for review."
  (interactive)
  (let ((accepted (seq-filter (lambda (entry) (eq (map-elt entry :status) 'accepted))
                              mailx-review--entries)))
    (unless accepted
      (user-error "No accepted entries"))
    (let* ((plan (mailx-review-create-plan accepted mailx-review--root))
           (id (hitl-plan-id plan)))
      (seq-do (lambda (entry)
                (map-put! entry :status 'planned)
                (map-put! entry :plan id))
              accepted)
      (mailx-review--render)
      (message "Plan %s created with %d entries; press o to reopen it%s"
               id (length accepted)
               (if (mailx-bulk--mu4e-active-p mailx-review--root)
                   ""
                 (format "; applying moves files directly, afterwards %s"
                         mailx-review--stale-hint)))
      (hitl-plan-review plan))))

(defun mailx-review-reopen-plan ()
  "Reopen the hitl plan of the planned entry at point for review."
  (interactive)
  (let ((id (map-elt (mailx-review--entry-at-point) :plan)))
    (unless id
      (user-error "Entry is not in a plan"))
    (hitl-plan-review (or (hitl-plan-get id nil 'mailx)
                          (user-error "Plan %s no longer exists" id)))))

(defun mailx-review-promote-entry (entry root &optional scope)
  "Offer to save a rule reproducing ENTRY's proposal under ROOT.
SCOPE is passed to `mailx-classify-propose-rule'.  Return the saved rule,
or nil when declined.  The confirmation shows how many messages the rule
would catch and names an existing rule that already matches this message,
since the first matching rule wins and the new one is appended last."
  (unless (map-elt entry :tier)
    (user-error "Set a tier before promoting to a rule"))
  (let* ((rule (mailx-classify-propose-rule
                (map-elt entry :message)
                (list :tier (map-elt entry :tier)
                      :folder (map-elt entry :folder)
                      :tags (map-elt entry :tags))
                scope))
         (existing (mailx-rules-load root))
         (shadow (mailx-classify-by-rules (map-elt entry :message) existing)))
    (when (seq-some (lambda (other) (eq (mailx-rule-id other) (mailx-rule-id rule)))
                    existing)
      (user-error "Rule ID already exists: %s" (mailx-rule-id rule)))
    (condition-case err
        (mailx--validate-rule rule)
      (error (user-error "Cannot save rule %s: %s" (mailx-rule-id rule)
                         (error-message-string err))))
    (let ((preview (mailx-test-rule root rule)))
      (when (yes-or-no-p
             (format "Rule %s matches %d messages%s%s; save? "
                     (mailx-rule-id rule)
                     (map-elt preview :count)
                     (if-let* ((subjects (map-elt preview :subjects)))
                         (format " (e.g. %s)" (string-join subjects "; "))
                       "")
                     (if shadow
                         (format ", but is shadowed by rule %s for this message"
                                 (mailx-rule-id shadow))
                       "")))
        (mailx-rules-save root (append existing (list rule)))
        rule))))

(defun mailx-review-promote (&optional scope)
  "Save a rule reproducing the proposal at point.
The rule matches the message's list, else its sender's address or domain
\(see `mailx-classify-propose-rule').  With a prefix argument, choose
SCOPE: address, domain or list."
  (interactive
   (list (when current-prefix-arg
           (intern (completing-read "Rule scope: " '("address" "domain" "list") nil t)))))
  (when-let* ((rule (mailx-review-promote-entry (mailx-review--entry-at-point)
                                                mailx-review--root scope)))
    (message "Saved mailx rule %s; press g to reclassify the other entries"
             (mailx-rule-id rule))))

(defun mailx-review-view ()
  "Show the raw message file at point."
  (interactive)
  (let ((path (mailx-review--entry-id (mailx-review--entry-at-point))))
    (unless (file-exists-p path)
      (user-error "File is gone and the mu index may be stale; %s" mailx-review--stale-hint))
    (view-file-other-window path)))

;;; Mode

(defvar-keymap mailx-review-mode-map
  :doc "Keymap for `mailx-review-mode'."
  :parent tabulated-list-mode-map
  "n" #'next-line
  "p" #'previous-line
  "a" #'mailx-review-accept
  "s" #'mailx-review-skip
  "*" #'mailx-review-accept-all
  "t" #'mailx-review-set-tier
  "f" #'mailx-review-set-folder
  "l" #'mailx-review-set-tags
  "r" #'mailx-review-promote
  "x" #'mailx-review-apply
  "o" #'mailx-review-reopen-plan
  "v" #'mailx-review-view)

(define-derived-mode mailx-review-mode tabulated-list-mode "Mailx-Review"
  "Major mode for reviewing proposed mail classifications.
Nothing is moved from this buffer; accepted entries become a hitl plan."
  (setq tabulated-list-format [("State" 8 t)
                               ("Tier" 8 t)
                               ("Folder" 18 t)
                               ("Tags" 16 nil)
                               ("Source" 14 t)
                               ("From" 26 t)
                               ("Subject" 0 t)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header)
  (setq-local revert-buffer-function #'mailx-review--revert))

;;;###autoload
(defun mailx-review (&optional root)
  "Review proposed classifications for unclassified mail under ROOT.
ROOT defaults to the active maildir.  No message is changed."
  (interactive)
  (let* ((root (file-name-as-directory (expand-file-name (or root (mailx-current-maildir)))))
         (buffer (get-buffer-create (format "*mailx-review: %s*" (abbreviate-file-name root)))))
    (with-current-buffer buffer
      (mailx-review-mode)
      (setq mailx-review--root root)
      (mailx-review--load))
    (pop-to-buffer buffer)))

(provide 'mailx-review)
;;; mailx-review.el ends here
