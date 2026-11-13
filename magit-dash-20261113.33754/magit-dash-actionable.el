;;; magit-dash-actionable.el --- Actions needing attention -*- lexical-binding: t; -*-

;;; Commentary:
;; A combined view of local repository changes and open PRs needing attention.

;;; Code:

(require 'magit-dash)
(require 'magit-dash-gh-pr-status)
(require 'tabulated-list)
(require 'transient)
(require 'seq)

(defun magit-dash-actionable--repo-entries (repo)
  "Return action rows for REPO based on its cached or collected stats."
  (let ((stats (magit-dash--get-stats repo)))
    (seq-map (lambda (item)
               (list (list :repo repo :kind (car item))
                     (vector (magit-dash-repo-name repo) (cdr item))))
             (delq nil
                   (list (when (> (or (plist-get stats :ahead) 0) 0)
                           (cons 'push (format "%s unpushed commits" (plist-get stats :ahead))))
                         (when (> (or (plist-get stats :behind) 0) 0)
                           (cons 'pull (format "%s unpulled commits" (plist-get stats :behind))))
                         (when (plist-get stats :dirty)
                           (cons 'changes "Uncommitted changes")))))))

(defun magit-dash-actionable--pr-entries (prs)
  "Return action rows for PRS that need author attention."
  (seq-map (lambda (pr)
             (list (list :pr pr)
                   (vector (format "%s#%s" (plist-get pr :repo) (plist-get pr :number))
                           (format "%s: %s" (plist-get pr :bucket) (plist-get pr :title)))))
           (seq-filter (lambda (pr) (memq (plist-get pr :bucket) '(fix-ci threads blocked))) prs)))

(defun magit-dash-actionable-refresh ()
  "Refresh local changes and open pull requests needing attention."
  (interactive)
  (setq header-line-format nil)
  (let ((entries (seq-mapcat #'magit-dash-actionable--repo-entries magit-dash-repo-list)))
    (condition-case err
        (setq entries (append entries (magit-dash-actionable--pr-entries
                                       (magit-dash-gh-pr-status-collect-all))))
      (error (setq header-line-format
                   (format "PR status unavailable: %s" (error-message-string err)))))
    (setq tabulated-list-entries entries)
    (tabulated-list-print t)))

(defun magit-dash-actionable--repo ()
  "Return the repository for the action at point, or signal an error."
  (or (plist-get (tabulated-list-get-id) :repo)
      (user-error "This row has no local repository")))

(defun magit-dash-actionable--repo-p ()
  "Return non-nil when the row at point has a local repository."
  (and (plist-get (tabulated-list-get-id) :repo) t))

(defun magit-dash-actionable--pr-p ()
  "Return non-nil when the row at point has a pull request."
  (and (plist-get (tabulated-list-get-id) :pr) t))

(defun magit-dash-actionable-status ()
  "Open Magit status for the repository at point."
  (interactive)
  (magit-status-setup-buffer (magit-dash-repo-path (magit-dash-actionable--repo))))

(defun magit-dash-actionable-pull ()
  "Open Magit's pull command for the repository at point."
  (interactive)
  (let ((default-directory (file-name-as-directory (magit-dash-repo-path (magit-dash-actionable--repo)))))
    (call-interactively #'magit-pull-from-upstream)))

(defun magit-dash-actionable-push ()
  "Open Magit's push command for the repository at point."
  (interactive)
  (let ((default-directory (file-name-as-directory (magit-dash-repo-path (magit-dash-actionable--repo)))))
    (call-interactively #'magit-push-current-to-pushremote)))

(defun magit-dash-actionable-open-pr ()
  "Open the pull request at point in a browser."
  (interactive)
  (if-let* ((url (plist-get (plist-get (tabulated-list-get-id) :pr) :url)))
      (browse-url url)
    (user-error "This row has no pull request URL")))

(transient-define-prefix magit-dash-actionable-menu ()
  "Actions for the item at point."
  [["Repository"
    ("s" "Magit status" magit-dash-actionable-status :inapt-if-not magit-dash-actionable--repo-p)
    ("p" "Push" magit-dash-actionable-push :inapt-if-not magit-dash-actionable--repo-p)
    ("u" "Pull" magit-dash-actionable-pull :inapt-if-not magit-dash-actionable--repo-p)]
   ["Pull request"
    ("o" "Open in browser" magit-dash-actionable-open-pr :inapt-if-not magit-dash-actionable--pr-p)]])

(defvar magit-dash-actionable-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (keymap-set map "C-c C-c" #'magit-dash-actionable-menu)
    (keymap-set map "RET" #'magit-dash-actionable-menu)
    (keymap-set map "g" #'magit-dash-actionable-refresh)
    map))

(define-derived-mode magit-dash-actionable-mode tabulated-list-mode "Dash Actions"
  "List repository and pull request actions needing attention."
  (setq tabulated-list-format [("Repository / PR" 36 t) ("Needs attention" 60 t)])
  (tabulated-list-init-header))

;;;###autoload
(defun magit-dash-actionable-open ()
  "Open a list of repository and PR actions needing attention."
  (interactive)
  (let ((buffer (get-buffer-create "*magit-dash-actions*")))
    (with-current-buffer buffer
      (magit-dash-actionable-mode)
      (magit-dash-actionable-refresh))
    (pop-to-buffer buffer)))

(provide 'magit-dash-actionable)
;;; magit-dash-actionable.el ends here
