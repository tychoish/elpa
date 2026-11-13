;;; orgx-refile-agent.el --- Refile Org subtrees to agent-shell sessions -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, convenience

;;; Commentary:
;; Adds agent-shell targets to `org-refile' (and so `org-agenda-refile').
;; The refile prompt lists one entry per live agent-shell session plus an
;; unassigned-queue entry.  Choosing one opens an agent-shell-queue capture
;; buffer seeded with the subtree, instead of moving it to an Org file.  What
;; happens to the original heading is `orgx-refile-agent-original-action'.
;;
;; Works from `org-agenda-refile' too.  Enable with `orgx-refile-agent-enable' (done by `orgx-setup').  The extra
;; targets are only added while `org-refile' itself runs, so other callers of
;; `org-refile-get-location' (bulk refile, denote capture) are unaffected.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-refile)
(require 'org-agenda)

(declare-function agent-shell-buffers "agent-shell")
(declare-function org-agenda-redo "org-agenda")
(declare-function org-get-at-bol "org-macs")
(defvar org-refile-keep)
(declare-function agent-shell-queue--open-capture "agent-shell-queue-ui")

(defgroup orgx-refile-agent nil
  "Refile Org subtrees to agent-shell sessions."
  :group 'orgx
  :prefix "orgx-refile-agent-")

(defcustom orgx-refile-agent-prefix "agent-shell:"
  "Prefix of the agent-shell entries in the refile target list."
  :type 'string
  :group 'orgx-refile-agent)

(defcustom orgx-refile-agent-original-action 'ask
  "What to do with the original heading after refiling it to an agent.
`ask' prompts each time; `keep' leaves it; `remove' cuts the subtree (it
stays in the kill ring); `mark' sets it to `orgx-refile-agent-mark-state'."
  :type '(choice (const ask) (const keep) (const remove) (const mark))
  :group 'orgx-refile-agent)

(defcustom orgx-refile-agent-mark-state "INPROGRESS"
  "TODO keyword set on the original heading by the `mark' action."
  :type 'string
  :group 'orgx-refile-agent)

(defconst orgx-refile-agent--file "agent-shell"
  "Pseudo file name carried by agent-shell refile targets.")

(defvar orgx-refile-agent--active nil
  "Non-nil while `org-refile' is running and agent targets should be offered.")

(defvar orgx-refile-agent--marker nil
  "Marker of the heading being refiled from the agenda, or nil.")

(defvar orgx-refile-agent--entries nil
  "Alist (NAME . BUFFER) of the agent targets offered by the current refile.
BUFFER is nil for the unassigned queue.")

(defun orgx-refile-agent--entry-alist ()
  "Return an alist (NAME . BUFFER) of agent-shell refile targets."
  (cons (cons (format "%s (unassigned queue)" orgx-refile-agent-prefix) nil)
        (when (fboundp 'agent-shell-buffers)
          (mapcar (lambda (buf)
                    (cons (format "%s %s" orgx-refile-agent-prefix (buffer-name buf)) buf))
                  (agent-shell-buffers)))))

(defun orgx-refile-agent--lookup (name)
  "Return the (NAME . BUFFER) entry in `orgx-refile-agent--entries' for NAME.
NAME may carry the suffixes `org-refile-get-location' adds."
  (let ((bare (thread-last name
                           (replace-regexp-in-string
                            (format " (%s)\\'" (regexp-quote orgx-refile-agent--file)) "")
                           (replace-regexp-in-string "/\\'" ""))))
    (assoc bare orgx-refile-agent--entries)))

(defun orgx-refile-agent--read-action ()
  "Return the action for the original heading, prompting for `ask'.
Always `keep' when refiling as a copy (`org-refile-keep')."
  (cond
   ((bound-and-true-p org-refile-keep) 'keep)
   ((eq orgx-refile-agent-original-action 'ask)
    (intern (completing-read
             "Original heading: "
             '("keep" "remove" "mark")
             nil t nil nil "keep")))
   (t orgx-refile-agent-original-action)))

(defun orgx-refile-agent--apply-action (action)
  "Apply ACTION (`keep', `remove', or `mark') to the heading at point."
  (pcase action
    ('remove (org-cut-subtree))
    ('mark (when (member orgx-refile-agent-mark-state org-todo-keywords-1)
             (org-todo orgx-refile-agent-mark-state)))))

(defun orgx-refile-agent-send (buffer)
  "Open a queue capture buffer for BUFFER seeded with the subtree at point.
BUFFER is an agent-shell buffer, or nil for the unassigned queue.  Then
apply `orgx-refile-agent-original-action' to the original heading."
  (unless (fboundp 'agent-shell-queue--open-capture)
    (user-error "agent-shell-queue is not available"))
  (org-back-to-heading t)
  (let* ((origin (current-buffer))
         (text (buffer-substring-no-properties
                (point) (save-excursion (org-end-of-subtree t) (point))))
         (action (orgx-refile-agent--read-action)))
    (agent-shell-queue--open-capture buffer origin text)
    (with-current-buffer origin
      (orgx-refile-agent--apply-action action))))

(defun orgx-refile-agent--ad-get-targets (targets)
  "Filter-return advice for `org-refile-get-targets': append agent TARGETS."
  (if (not orgx-refile-agent--active)
      targets
    (setq orgx-refile-agent--entries (orgx-refile-agent--entry-alist))
    (append targets
            (mapcar (lambda (e) (list (car e) orgx-refile-agent--file nil nil))
                    orgx-refile-agent--entries))))

(defun orgx-refile-agent--ad-get-location (location)
  "Filter-return advice for `org-refile-get-location'.
When LOCATION is an agent target, send the subtree and abort the refile."
  (if-let* (orgx-refile-agent--active
            ((equal (nth 1 location) orgx-refile-agent--file))
            (entry (orgx-refile-agent--lookup (car location))))
      (progn (if (markerp orgx-refile-agent--marker)
                 (org-with-point-at orgx-refile-agent--marker
                   (orgx-refile-agent-send (cdr entry)))
               (orgx-refile-agent-send (cdr entry)))
             (throw 'orgx-refile-agent-handled 'handled))
    location))

(defun orgx-refile-agent--call (fn)
  "Call FN; return `handled' if an agent target was chosen, else nil.
When handled, `org-refile-history' is restored so the agent entry doesn't
become the default for the next refile."
  (let ((saved org-refile-history))
    (when (eq (catch 'orgx-refile-agent-handled (funcall fn) nil) 'handled)
      (setq org-refile-history saved)
      'handled)))

(defun orgx-refile-agent--ad-refile (orig &optional arg default-buffer rfloc msg)
  "Around advice for `org-refile': offer agent targets when prompting.
ORIG is called with ARG, DEFAULT-BUFFER, RFLOC and MSG.  Targets are only
offered when the location is read interactively, and not for a jump or
with an active region (which may span several subtrees)."
  (let ((orgx-refile-agent--active (and (null rfloc)
                                        (not (equal arg '(4)))
                                        (not (org-region-active-p))))
        (orgx-refile-agent--marker nil))
    (orgx-refile-agent--call (lambda () (funcall orig arg default-buffer rfloc msg)))
    nil))

(defun orgx-refile-agent--ad-agenda-refile (orig &optional goto rfloc no-update)
  "Around advice for `org-agenda-refile': offer agent targets.
ORIG is called with GOTO, RFLOC and NO-UPDATE.  The agenda reads the refile
location itself, so the heading is found through its marker."
  (let ((orgx-refile-agent--active (and (null rfloc) (null goto)))
        (orgx-refile-agent--marker (org-get-at-bol 'org-hd-marker)))
    (when (and (orgx-refile-agent--call
                (lambda () (funcall orig goto rfloc no-update)))
               (not no-update))
      (org-agenda-redo))
    nil))

;;;###autoload
(defun orgx-refile-agent-enable ()
  "Offer agent-shell targets in `org-refile'."
  (advice-add 'org-refile :around #'orgx-refile-agent--ad-refile)
  (advice-add 'org-agenda-refile :around #'orgx-refile-agent--ad-agenda-refile)
  (advice-add 'org-refile-get-targets :filter-return #'orgx-refile-agent--ad-get-targets)
  (advice-add 'org-refile-get-location :filter-return #'orgx-refile-agent--ad-get-location))

;;;###autoload
(defun orgx-refile-agent-disable ()
  "Stop offering agent-shell targets in `org-refile'."
  (advice-remove 'org-refile #'orgx-refile-agent--ad-refile)
  (advice-remove 'org-agenda-refile #'orgx-refile-agent--ad-agenda-refile)
  (advice-remove 'org-refile-get-targets #'orgx-refile-agent--ad-get-targets)
  (advice-remove 'org-refile-get-location #'orgx-refile-agent--ad-get-location))

(provide 'orgx-refile-agent)
;;; orgx-refile-agent.el ends here
