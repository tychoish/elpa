;;; magit-dash-pr-status-org.el --- Sync open PR status to an Org file -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (magit-dash "0.1") (org "9.0"))
;; Keywords: vc, tools, magit, github, org
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; Synchronizes pull request status collected by `magit-dash-pr-status'
;; into an Org file (summary table, task headings, STEP and LAST_SEEN
;; properties, and activity action items).
;;
;; Provides:
;;   M-x magit-dash-pr-status-sync (runs asynchronously by default)
;;   M-x magit-dash-pr-status-start-timer
;;   M-x magit-dash-pr-status-stop-timer

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'magit-dash-pr)
(require 'magit-dash-pr-status)
(require 'orgx nil t)

(defvar org-directory)

(defgroup magit-dash-pr-status-org nil
  "Org synchronizer for open PR status."
  :group 'magit-dash-pr-status
  :prefix "magit-dash-pr-status-")

(defcustom magit-dash-pr-status-file
  (expand-file-name "pr-review-status.org"
                    (if (boundp 'org-directory) org-directory "~/org"))
  "Org file kept in sync with the open PR list by `magit-dash-pr-status-sync'."
  :type 'file
  :group 'magit-dash-pr-status-org)

(defcustom magit-dash-pr-status-timer-interval 3600
  "Seconds between automatic `magit-dash-pr-status-sync' runs."
  :type 'natnum
  :group 'magit-dash-pr-status-org)

(defcustom magit-dash-pr-status-ci-preflight
  '(("casaphq/go-monorepo" :dir "~/src/go-monorepo"))
  "Repositories whose failing CI is downloaded for fix-ci action items.
An alist of (SLUG . PLIST) where `:dir' is the local checkout where
CI logs and triage artifacts are saved.
Repositories not listed get a fix-ci action without downloaded logs."
  :type '(alist :key-type string :value-type plist)
  :group 'magit-dash-pr-status-org)

(defcustom magit-dash-pr-status-preflight-function nil
  "Optional function called as (FUNC CTX CALLBACK) to fetch CI logs for preflights."
  :type '(choice (const :tag "None" nil) function)
  :group 'magit-dash-pr-status-org)

(defcustom magit-dash-pr-status-failure-warn-threshold 3
  "Consecutive collection failures after which a warning replaces a message."
  :type 'natnum
  :group 'magit-dash-pr-status-org)

(defvar magit-dash-pr-status-org--fingerprint nil
  "Fingerprint of the PR set last synced to the Org file (in memory only).")

(defvar magit-dash-pr-status-org--last-count 0
  "Number of PRs in the last committed sync (in memory only).")

(defvar magit-dash-pr-status-org--failures 0
  "Consecutive PR collection failures in `magit-dash-pr-status-sync'.")

(defvar magit-dash-pr-status-org--timer nil
  "Timer driving periodic PR status sync, or nil when stopped.")

(defvar magit-dash-pr-status-org--syncing nil
  "Non-nil while an asynchronous sync is in progress.")

(defvar magit-dash-pr-status-org--preflight-requests nil
  "Preflight requests (REPO NUMBER FINGERPRINT) collected during a sync.")

(defvar magit-dash-pr-status-org--preflight-procs (make-hash-table :test #'equal)
  "Map \"repo#number\" to t while a preflight of that PR is running.")

;;;; String & Org Sanitization Helpers

(defun magit-dash-pr-status-org--pr-field (pr key)
  "Return tracking KEY of PR."
  (plist-get (plist-get pr :tracking) key))

(defun magit-dash-pr-status-org--pr-step (pr)
  "Return the STEP string (the bucket name) for PR."
  (magit-dash-pr-status-org--line (or (plist-get pr :bucket) "unknown")))

(defun magit-dash-pr-status-org--line (value)
  "Return VALUE as a single-line string safe to embed in Org.
Control characters (including newlines) collapse to single spaces."
  (string-trim
   (replace-regexp-in-string "[[:cntrl:]]+" " "
                             (format "%s" (if (null value) "" value)))))

(defun magit-dash-pr-status-org--pr-heading-text (pr)
  "Return the sanitized task heading text (without stars or keyword) for PR."
  (let ((text (magit-dash-pr-status-org--line
               (format "%s: %s" (plist-get pr :number)
                       (or (magit-dash-pr-status-org--pr-field pr :action-summary)
                           (plist-get pr :title) "")))))
    ;; A trailing :word: would be parsed as heading tags.
    (string-trim-right
     (replace-regexp-in-string "[ \t]+:[[:alnum:]_@#%:]+:\\'" "" text))))

(defun magit-dash-pr-status-org--body-line (value)
  "Return VALUE as a safe Org list item line, or nil if empty.
The line has no trailing newline."
  (let ((l (magit-dash-pr-status-org--line value)))
    (unless (string-empty-p l) (concat "- " l))))

(defun magit-dash-pr-status-org--cell (value)
  "Return VALUE formatted as a single-line Org table cell string."
  (replace-regexp-in-string "|" "/" (magit-dash-pr-status-org--line value)))

(defun magit-dash-pr-status-org--find-heading (title)
  "Return position of the level-1 heading titled TITLE, or nil.
Tolerates a TODO keyword, priority cookie and tags, and ignores
text inside example/src/export/comment blocks."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((re (concat "^\\* +\\(?:[A-Z]+ +\\)?\\(?:\\[#[A-Z0-9]+\\] +\\)?"
                     (regexp-quote title)
                     "\\(?:[ \t]+:[[:alnum:]_@#%:]+:\\)?[ \t]*$"))
         found)
     (while (and (not found) (re-search-forward re nil t))
       (unless (org-in-block-p '("example" "src" "export" "comment"
                                 "verse" "quote" "center"))
         (setq found (line-beginning-position))))
     found)))

(defun magit-dash-pr-status-org--ensure-skeleton ()
  "Ensure the current Org buffer has the title, filetags, Summary and Tasks."
  (org-with-wide-buffer
   (when (zerop (buffer-size))
     (insert "#+title: Open PR status (@me)\n#+filetags: :agent:prs:\n\n"))
   (dolist (h '("Summary" "Tasks"))
     (unless (magit-dash-pr-status-org--find-heading h)
       (goto-char (point-max))
       (unless (or (bobp) (bolp)) (insert "\n"))
       (insert (format "* %s\n" h))))))

(defun magit-dash-pr-status-org--task-entries ()
  "Return an alist ((REPO . PR) . MARKER) of entries with REPO and PR properties."
  (let (res)
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (let ((repo (org-entry-get nil "REPO"))
              (pr (org-entry-get nil "PR")))
          (when (and repo pr)
            (push (cons (cons repo pr) (copy-marker (point) t)) res))))
      t))
    (nreverse res)))

(defun magit-dash-pr-status-org--set-prop (prop value pos)
  "Set PROP to single-line VALUE on the entry at POS."
  (let ((value (magit-dash-pr-status-org--line value)))
    (if (fboundp 'orgx-entry-set-property)
        (orgx-entry-set-property prop value pos)
      (org-with-point-at pos (org-entry-put nil prop value)))))

(defun magit-dash-pr-status-org--write-summary (prs)
  "Replace the body of the Summary section with a refresh line and table of PRS."
  (org-with-wide-buffer
   (goto-char (magit-dash-pr-status-org--find-heading "Summary"))
   (forward-line 1)
   (unless (bolp) (insert "\n"))
   (let ((start (point))
         (end (save-excursion
                (cond ((looking-at org-outline-regexp-bol) (point))
                      ((outline-next-heading) (point))
                      (t (point-max))))))
     (delete-region start end)
     (insert (format "Last refreshed: %s\n\n" (format-time-string "[%F %a %R]")))
     (let ((table-start (point)))
       (insert "| PR | Repo | Branch | Approvals | CI failing | Unresolved | Bucket |\n|-\n")
       (dolist (pr prs)
         (insert (format "| %s |\n"
                          (mapconcat
                           #'magit-dash-pr-status-org--cell
                           (list (plist-get pr :number) (plist-get pr :repo)
                                 (plist-get pr :branch)
                                 (or (plist-get pr :approvals) 0)
                                 (length (magit-dash-pr-status-org--pr-field
                                          pr :failing-checks))
                                 (or (plist-get pr :threads-open) 0)
                                 (plist-get pr :bucket))
                           " | "))))
       (save-excursion (goto-char table-start) (org-table-align)))
     (insert "\n"))))

(defun magit-dash-pr-status-org--open-keyword ()
  "Return the keyword for an open task, or nil when none is defined."
  (if (member "TODO" org-not-done-keywords)
      "TODO"
    (car org-not-done-keywords)))

(defun magit-dash-pr-status-org--set-todo (marker state)
  "Set the TODO state of the entry at MARKER to STATE (\"\" clears it)."
  (let ((org-log-done nil)
        (org-todo-log-states nil)
        (org-enforce-todo-dependencies nil)
        (org-enforce-todo-checkbox-dependencies nil)
        (org-after-todo-state-change-hook nil)
        (org-blocker-hook nil))
    (cl-letf (((symbol-function 'org-add-log-setup) #'ignore)
              ((symbol-function 'org-add-log-note) #'ignore))
      (org-with-point-at marker
        (org-back-to-heading t)
        (org-todo (if (string-empty-p state) 'none state))))))

(defun magit-dash-pr-status-org--failing-alist (pr)
  "Return ((NAME . URL-or-nil) ...) of the failing checks of PR, sorted by name."
  (let ((full (plist-get pr :failing))
        (names (magit-dash-pr-status-org--pr-field pr :failing-checks)))
    (sort (if full
              (mapcar (lambda (f) (if (consp f) (cons (car f) (cdr f)) (cons f nil)))
                      full)
            (mapcar (lambda (n) (cons n nil)) names))
          (lambda (a b) (string< (car a) (car b))))))

(defun magit-dash-pr-status-org--plural (n word)
  "Return \"N WORD\" with WORD pluralised with an s unless N is 1."
  (format "%d %s%s" n word (if (= n 1) "" "s")))

(defun magit-dash-pr-status-org--pr-actions (pr)
  "Return the outstanding activities of PR as a list of (KIND TEXT . FAILING)."
  (let* ((failing (magit-dash-pr-status-org--failing-alist pr))
         (koalaty (seq-filter (lambda (f) (string-match-p "koalaty" (car f))) failing))
         (tests (seq-remove (lambda (f) (member f koalaty)) failing))
         (threads (or (plist-get pr :threads-open) 0))
         (changes (or (plist-get pr :changes-requested) 0))
         (missing (- (or (plist-get pr :required) 1) (or (plist-get pr :approvals) 0)))
         (base (or (plist-get pr :base) "base"))
         (merge-state (plist-get pr :merge-state))
         acts)
    (when tests
      (push `(fix-ci ,(format "fix failing tests: %s" (mapconcat #'car tests ", "))
                     . ,tests)
            acts))
    (when koalaty
      (push '(koalaty "raise koalaty rating") acts))
    (when (equal merge-state "DIRTY")
      (push (list 'conflicts (format "fix merge conflicts with %s" base)) acts))
    (when (equal merge-state "BEHIND")
      (push (list 'behind (format "update branch from %s" base)) acts))
    (when (> threads 0)
      (push (list 'threads (format "respond to %s"
                                   (magit-dash-pr-status-org--plural
                                    threads "open review thread")))
            acts))
    (when (> changes 0)
      (push (list 'changes (format "address changes requested by %s"
                                   (magit-dash-pr-status-org--plural
                                    changes "reviewer")))
            acts))
    (when (> missing 0)
      (push (list 'reviews (format "find %s" (magit-dash-pr-status-org--plural
                                              missing "more review")))
            acts))
    (when (plist-get pr :draft)
      (push '(draft "mark ready for review") acts))
    (when (eq (plist-get pr :bucket) 'ready)
      (push '(merge "merge") acts))
    (nreverse acts)))

(defun magit-dash-pr-status-org--task-open-p (marker)
  "Return non-nil when the task at MARKER is open (not in a done state)."
  (let ((state (org-with-point-at marker (org-get-todo-state))))
    (if state
        (member state org-not-done-keywords)
      (null org-not-done-keywords))))

(defun magit-dash-pr-status-org--set-heading-text (marker text)
  "Replace the heading text of the entry at MARKER with TEXT."
  (org-with-point-at marker
    (org-back-to-heading t)
    (when (looking-at org-complex-heading-regexp)
      (unless (equal (match-string 4) text)
        (replace-match text t t nil 4)))))

(defun magit-dash-pr-status-org--action-children (marker)
  "Return ((KIND . MARKER) ...) of the action items under the entry at MARKER."
  (org-with-point-at marker
    (org-back-to-heading t)
    (let ((level (org-current-level))
          (end (save-excursion (org-end-of-subtree t t) (point)))
          res)
      (save-excursion
        (while (and (outline-next-heading) (< (point) end))
          (when-let* (((= (org-current-level) (1+ level)))
                      (kind (org-entry-get nil "ACTION")))
            (push (cons kind (copy-marker (point) t)) res))))
      (nreverse res))))

(defun magit-dash-pr-status-org--append-action (marker kind text)
  "Insert an open action item KIND with TEXT as the last child of MARKER's entry.
Return a marker at the new heading."
  (org-with-point-at marker
    (org-back-to-heading t)
    (let ((stars (make-string (1+ (org-current-level)) ?*))
          (kw (magit-dash-pr-status-org--open-keyword))
          start)
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (setq start (point))
      (insert (format "%s %s%s\n:PROPERTIES:\n:ACTION: %s\n:END:\n"
                      stars (if kw (concat kw " ") "") text kind))
      (copy-marker start t))))

(defun magit-dash-pr-status-org--set-body-line (marker prefix text)
  "Set the body line \"- PREFIX: TEXT\" of the entry at MARKER."
  (org-with-point-at marker
    (org-back-to-heading t)
    (let* ((end (save-excursion (outline-next-heading) (point)))
           (re (format "^- %s:.*\n" (regexp-quote prefix)))
           (line (and text (format "- %s: %s\n" prefix text))))
      (if (save-excursion (re-search-forward re end t))
          (replace-match (or line "") t t)
        (when line
          (org-end-of-meta-data nil)
          (insert line))))))

(defun magit-dash-pr-status-org--ci-fingerprint (failing)
  "Return a fingerprint of the failing-check alist FAILING."
  (md5 (mapconcat (lambda (f) (format "%s %s" (car f) (or (cdr f) ""))) failing "\n")))

(defun magit-dash-pr-status-org--ci-run-id (failing)
  "Return the GitHub Actions run id found in the URLs of FAILING, or nil."
  (seq-some (lambda (f)
              (and (cdr f) (string-match "/runs/\\([0-9]+\\)" (cdr f))
                   (match-string 1 (cdr f))))
            failing))

(defun magit-dash-pr-status-org--preflight-config (pr)
  "Return the preflight plist configured for the repository of PR, or nil."
  (cdr (assoc (plist-get pr :repo) magit-dash-pr-status-ci-preflight)))

(defun magit-dash-pr-status-org--org-link (target label)
  "Return an Org link to TARGET described by LABEL, or LABEL without TARGET."
  (if target
      (format "[[%s][%s]]" target (replace-regexp-in-string "[][]" "" label))
    label))

(defun magit-dash-pr-status-org--sync-ci-action (marker pr failing)
  "Refresh the failing-test links, staleness and preflight of the fix-ci item."
  (let* ((fp (magit-dash-pr-status-org--ci-fingerprint failing))
         (old (org-with-point-at marker (org-entry-get nil "CI_FINGERPRINT")))
         (fetched (org-with-point-at marker (org-entry-get nil "LOGS_FETCHED")))
         (logged (org-with-point-at marker (org-entry-get nil "LOGS_FINGERPRINT")))
         (run-id (magit-dash-pr-status-org--ci-run-id failing))
         (number (plist-get pr :number)))
    (magit-dash-pr-status-org--set-prop "CI_FINGERPRINT" fp marker)
    (magit-dash-pr-status-org--set-body-line
     marker "Failing"
     (mapconcat (lambda (f)
                  (magit-dash-pr-status-org--org-link
                   (cdr f) (magit-dash-pr-status-org--line (car f))))
                failing ", "))
    (when run-id (magit-dash-pr-status-org--set-prop "RUN_ID" run-id marker))
    (let ((stale (and fetched (not (equal logged fp)))))
      (when (and fetched old (not (equal old fp)) (not stale))
        (setq stale t))
      (if stale
          (magit-dash-pr-status-org--set-prop "STALE" "t" marker)
        (org-with-point-at marker (org-entry-delete nil "STALE")))
      (when (and (or stale (not fetched)) run-id
                 (magit-dash-pr-status-org--preflight-config pr))
        (push (list (plist-get pr :repo) number fp run-id (plist-get pr :branch))
              magit-dash-pr-status-org--preflight-requests)))))

(defun magit-dash-pr-status-org--sync-actions (marker pr)
  "Reconcile the action item children of the PR task at MARKER with PR."
  (let ((existing (magit-dash-pr-status-org--action-children marker))
        (wanted (magit-dash-pr-status-org--pr-actions pr)))
    (dolist (act wanted)
      (let* ((kind (symbol-name (car act)))
              (text (magit-dash-pr-status-org--line (cadr act)))
              (hit (assoc kind existing))
              (m (if hit
                     (cdr hit)
                   (magit-dash-pr-status-org--append-action marker kind text))))
        (when hit
          (magit-dash-pr-status-org--set-heading-text m text)
          (unless (magit-dash-pr-status-org--task-open-p m)
            (magit-dash-pr-status-org--set-todo
             m (or (magit-dash-pr-status-org--open-keyword) ""))
            (org-with-point-at m (org-entry-delete nil "CLOSED"))))
        (when (eq (car act) 'fix-ci)
          (magit-dash-pr-status-org--sync-ci-action m pr (cddr act)))
        (unless hit (set-marker m nil))))
    (dolist (e existing)
      (when (and (not (assq (intern (car e)) wanted))
                 (magit-dash-pr-status-org--task-open-p (cdr e)))
        (magit-dash-pr-status-org--close-task (cdr e))))
    (dolist (e existing) (set-marker (cdr e) nil))))

(defun magit-dash-pr-status-org--append-task (pr)
  "Append a new task entry for PR under the Tasks heading, with its actions."
  (org-with-wide-buffer
   (goto-char (magit-dash-pr-status-org--find-heading "Tasks"))
   (let* ((stars (make-string (1+ (org-current-level)) ?*))
          (kw (magit-dash-pr-status-org--open-keyword))
          (l #'magit-dash-pr-status-org--line)
          start)
     (org-end-of-subtree t t)
     (unless (bolp) (insert "\n"))
     (setq start (copy-marker (point)))
     (insert
      (format "%s %s%s\n:PROPERTIES:\n:REPO: %s\n:PR: %s\n:STEP: %s\n:LAST_SEEN: %s\n:BRANCH: %s\n:URL: %s\n:END:\n%s"
              stars (if kw (concat kw " ") "")
              (magit-dash-pr-status-org--pr-heading-text pr)
              (funcall l (plist-get pr :repo)) (funcall l (plist-get pr :number))
              (magit-dash-pr-status-org--pr-step pr)
              (funcall l (magit-dash-pr-status-org--pr-field pr :last-seen))
              (funcall l (plist-get pr :branch)) (funcall l (plist-get pr :url))
              (mapconcat (lambda (s) (concat s "\n"))
                         (delq nil
                               (list (magit-dash-pr-status-org--body-line
                                      (plist-get pr :title))
                                     (magit-dash-pr-status-org--body-line
                                      (magit-dash-pr-status-org--pr-field
                                       pr :delta-reason))))
                         "")))
     (set-marker-insertion-type start t)
     (magit-dash-pr-status-org--sync-actions start pr)
     (set-marker start nil))))

(defun magit-dash-pr-status-org--record-preflight (file repo number fp ok detail)
  "Record a finished fix-ci preflight for REPO#NUMBER in FILE."
  (magit-dash-pr-status-org--edit-file
   file
   (lambda ()
     (when-let* ((parent (cdr (assoc (cons repo (format "%s" number))
                                     (magit-dash-pr-status-org--task-entries))))
                 (child (cdr (assoc "fix-ci"
                                    (magit-dash-pr-status-org--action-children
                                     parent)))))
       (if (not ok)
           (magit-dash-pr-status-org--set-prop "PREFLIGHT_ERROR" detail child)
         (let* ((root (file-name-as-directory
                       (expand-file-name
                        (plist-get (cdr (assoc repo magit-dash-pr-status-ci-preflight))
                                   :dir))))
                (dir (expand-file-name (or (plist-get detail :ci-dir) "") root))
                (index (expand-file-name (or (plist-get detail :ci-index-file) "") root)))
           (org-with-point-at child (org-entry-delete nil "PREFLIGHT_ERROR"))
           (magit-dash-pr-status-org--set-prop
            "LOGS_FETCHED" (format-time-string "[%F %a %R]") child)
           (magit-dash-pr-status-org--set-prop "LOGS_FINGERPRINT" fp child)
           (magit-dash-pr-status-org--set-body-line
            child "Logs"
            (format "[[file:%s][%s]], [[file:%s][triage index]]"
                    dir (abbreviate-file-name dir) index))
           (when (equal fp (org-with-point-at child
                             (org-entry-get nil "CI_FINGERPRINT")))
             (org-with-point-at child (org-entry-delete nil "STALE")))))))))

(defun magit-dash-pr-status-org--start-preflight (file request)
  "Run the preflight for REQUEST, recording the result into FILE."
  (pcase-let* ((`(,repo ,number ,fp ,run-id ,branch) request)
               (cfg (cdr (assoc repo magit-dash-pr-status-ci-preflight)))
               (key (format "%s#%s" repo number))
               (fn (or magit-dash-pr-status-preflight-function
                       (and (fboundp 'agent-shell-workflow-library--fix-ci-pre-op)
                            #'agent-shell-workflow-library--fix-ci-pre-op))))
    (when (and cfg fn (not (gethash key magit-dash-pr-status-org--preflight-procs)))
      (puthash key t magit-dash-pr-status-org--preflight-procs)
      (let ((done (lambda (ok detail)
                    (remhash key magit-dash-pr-status-org--preflight-procs)
                    (condition-case err
                        (magit-dash-pr-status-org--record-preflight
                         file repo number fp ok detail)
                      (error (message "magit-dash-pr-status: preflight for %s not recorded: %s"
                                      key (error-message-string err)))))))
        (condition-case err
            (funcall fn
                     (list :args (list :repo repo :branch branch :run-id (string-to-number run-id)
                                       :root (plist-get cfg :dir)))
                     (lambda (ctx) (funcall done t ctx)))
          (error (funcall done nil (magit-dash-pr-status-org--line
                                    (error-message-string err)))))))))

(defun magit-dash-pr-status-org--start-preflights (file)
  "Start every preflight queued by the sync that just wrote FILE."
  (dolist (req (nreverse (delete-dups magit-dash-pr-status-org--preflight-requests)))
    (magit-dash-pr-status-org--start-preflight file req))
  (setq magit-dash-pr-status-org--preflight-requests nil))

(defun magit-dash-pr-status-org--update-task (marker pr)
  "Update STEP and LAST_SEEN (and the heading on step change) of PR at MARKER."
  (let ((step (magit-dash-pr-status-org--pr-step pr))
        (state (org-with-point-at marker (org-get-todo-state))))
    (unless (equal step (org-with-point-at marker (org-entry-get nil "STEP")))
      (magit-dash-pr-status-org--set-prop "STEP" step marker)
      (magit-dash-pr-status-org--set-heading-text
       marker (magit-dash-pr-status-org--pr-heading-text pr)))
    (when (and state (member state org-done-keywords))
      (magit-dash-pr-status-org--set-todo
       marker (or (magit-dash-pr-status-org--open-keyword) "")))
    (magit-dash-pr-status-org--set-prop
     "LAST_SEEN"
     (magit-dash-pr-status-org--pr-field pr :last-seen) marker)
    (magit-dash-pr-status-org--sync-actions marker pr)))

(defun magit-dash-pr-status-org--close-task (marker)
  "Mark the task at MARKER DONE and set CLOSED, without prompts or logging."
  (magit-dash-pr-status-org--set-todo
   marker (or (car org-done-keywords) "DONE"))
  (org-with-point-at marker
    (org-back-to-heading t)
    (unless (org-entry-get nil "CLOSED")
      (org-add-planning-info 'closed (org-current-effective-time)))))

(defun magit-dash-pr-status-org--close-with-actions (marker)
  "Close the PR task at MARKER and every open action item under it."
  (dolist (c (magit-dash-pr-status-org--action-children marker))
    (when (magit-dash-pr-status-org--task-open-p (cdr c))
      (magit-dash-pr-status-org--close-task (cdr c)))
    (set-marker (cdr c) nil))
  (magit-dash-pr-status-org--close-task marker))

(defun magit-dash-pr-status-org--close-allowed-p (prs)
  "Return non-nil when PRS is complete enough to close vanished tasks.
An empty or truncated (>= max-prs) list is never trusted."
  (and prs
       (< (length prs)
          (or (bound-and-true-p magit-dash-pr-status-max-prs) 100))))

(defun magit-dash-pr-status-org--sync-buffer (prs)
  "Apply PRS to the current Org buffer."
  (magit-dash-pr-status-org--ensure-skeleton)
  (magit-dash-pr-status-org--write-summary prs)
  (let ((entries (magit-dash-pr-status-org--task-entries))
        (seen nil))
    (dolist (pr prs)
      (let* ((key (cons (magit-dash-pr-status-org--line (plist-get pr :repo))
                        (magit-dash-pr-status-org--line (plist-get pr :number))))
             (hit (assoc key entries)))
        (push key seen)
        (if hit
            (magit-dash-pr-status-org--update-task (cdr hit) pr)
          (magit-dash-pr-status-org--append-task pr))))
    (when (magit-dash-pr-status-org--close-allowed-p prs)
      (dolist (e entries)
        (unless (member (car e) seen)
          (when (magit-dash-pr-status-org--task-open-p (cdr e))
            (magit-dash-pr-status-org--close-with-actions (cdr e))))))
    (dolist (e entries) (set-marker (cdr e) nil))))

(defun magit-dash-pr-status-org--edit-file (file fn)
  "Call FN in an Org buffer holding FILE's content, then write the result back atomically."
  (let* ((file (expand-file-name file))
         (target (file-truename file))
         (visiting (or (find-buffer-visiting file) (find-buffer-visiting target)))
         (dir (file-name-directory target))
         content mode tmp)
    (when (and visiting (buffer-modified-p visiting))
      (user-error "Buffer %s has unsaved changes; save or revert before syncing PR status"
                  (buffer-name visiting)))
    (make-directory dir t)
    (setq mode (and (file-exists-p target) (file-modes target)))
    (with-temp-buffer
      (let ((inhibit-read-only t)
            (inhibit-message t))
        (when (file-exists-p target) (insert-file-contents target))
        (delay-mode-hooks (org-mode))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (funcall fn))
        (setq content (buffer-string))))
    (setq tmp (make-temp-file (expand-file-name ".pr-status-" dir)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region content nil tmp nil 'silent))
          (when mode (set-file-modes tmp mode))
          (rename-file tmp target t)
          (setq tmp nil))
      (when (and tmp (file-exists-p tmp)) (delete-file tmp)))
    (when (and visiting (buffer-live-p visiting))
      (with-current-buffer visiting
        (let ((inhibit-read-only t))
          (revert-buffer t t t))))
    file))

;;;###autoload
(defun magit-dash-pr-status-sync-file (prs &optional file)
  "Non-destructively sync the PR status Org FILE with PRS.
PRS are PR plists carrying a `:tracking' plist (see
`magit-dash-pr-attach-tracking').  FILE defaults to
`magit-dash-pr-status-file'.  Signals an error when a buffer visiting
FILE has unsaved changes.  Return FILE."
  (let* ((file (expand-file-name (or file magit-dash-pr-status-file))))
    (magit-dash-pr-status-org--edit-file
     file
     (lambda () (magit-dash-pr-status-org--sync-buffer prs)))
    (magit-dash-pr-status-org--start-preflights file)
    file))

;;;; Sync Dispatch & Timer

(defun magit-dash-pr-status-org--note-failure (reason)
  "Record a consecutive collection failure and report REASON (message or warning)."
  (cl-incf magit-dash-pr-status-org--failures)
  (if (>= magit-dash-pr-status-org--failures
          magit-dash-pr-status-failure-warn-threshold)
      (display-warning
       'magit-dash
       (format "pr-status-sync: %d consecutive collection failures: %s"
               magit-dash-pr-status-org--failures reason)
       :warning)
    (message "magit-dash: PR status collection failed: %s" reason)))

(defun magit-dash-pr-status-org--apply-sync (collected &optional file callback)
  "Apply COLLECTED PR state to FILE, commit tracking, and invoke CALLBACK."
  (setq magit-dash-pr-status-org--failures 0)
  (let* ((target-file (or file magit-dash-pr-status-file))
         (prs (mapcar #'magit-dash-pr-attach-tracking collected))
         (fingerprint (magit-dash-pr-tracking-fingerprint prs)))
    (cond
     ((and (null prs)
           magit-dash-pr-status-org--fingerprint
           (> magit-dash-pr-status-org--last-count 0))
      (message "magit-dash: PR collection returned nothing after a non-empty sync; status file left untouched")
      (when callback (funcall callback nil 'empty-after-nonempty)))

     ((equal fingerprint magit-dash-pr-status-org--fingerprint)
      (message "magit-dash: PR status unchanged (%d PRs); file untouched" (length prs))
      (when callback (funcall callback nil 'unchanged)))

     (t
      (condition-case err
          (let ((res (magit-dash-pr-status-sync-file prs target-file)))
            (magit-dash-pr-commit-tracking prs)
            (setq magit-dash-pr-status-org--fingerprint fingerprint
                  magit-dash-pr-status-org--last-count (length prs))
            (message "magit-dash: PR status synced (%d PRs) -> %s" (length prs) res)
            (when callback (funcall callback res t)))
        (error
         (magit-dash-pr-status-org--note-failure (error-message-string err))
         (when callback (funcall callback nil err))))))))

;;;###autoload
(defun magit-dash-pr-status-sync (&optional sync file callback)
  "Collect PR state and sync `magit-dash-pr-status-file'.
When SYNC is `async', runs asynchronously.
When SYNC is otherwise non-nil (or non-interactive/batch by default),
runs synchronously.  In interactive sessions with SYNC nil, runs
asynchronously in the background without blocking Emacs.
CALLBACK, when supplied, is called as (CALLBACK FILE-OR-NIL STATUS)."
  (interactive "P")
  (if (and (not (eq sync 'async))
           (or sync noninteractive))
      (condition-case err
          (let ((collected (magit-dash-pr-status-collect-all)))
            (magit-dash-pr-status-org--apply-sync collected file callback))
        (error
         (magit-dash-pr-status-org--note-failure (error-message-string err))
         (when callback (funcall callback nil err))))
    (if magit-dash-pr-status-org--syncing
        (message "magit-dash: PR status sync already in progress")
      (setq magit-dash-pr-status-org--syncing t)
      (message "magit-dash: collecting PR status in background...")
      (magit-dash-pr-status-collect-all-async
       (lambda (collected)
         (setq magit-dash-pr-status-org--syncing nil)
         (magit-dash-pr-status-org--apply-sync collected file callback))
       (lambda (err)
         (setq magit-dash-pr-status-org--syncing nil)
         (magit-dash-pr-status-org--note-failure err)
         (when callback (funcall callback nil err)))))))

;;;###autoload
(defalias 'magit-dash-pr-status-sync-now #'magit-dash-pr-status-sync)

;;;###autoload
(defun magit-dash-pr-status-start-timer (&optional interval)
  "Start (or restart) the periodic PR status sync timer.
INTERVAL is the seconds between runs (default `magit-dash-pr-status-timer-interval')."
  (interactive)
  (magit-dash-pr-status-stop-timer)
  (let ((secs (or interval magit-dash-pr-status-timer-interval)))
    (setq magit-dash-pr-status-org--timer
          (run-at-time secs secs
                       (lambda ()
                         (condition-case err
                             (magit-dash-pr-status-sync)
                           (error (message "magit-dash-pr-status timer: %s"
                                           (error-message-string err)))))))
    (message "magit-dash: PR status sync timer started (%ds interval)" secs)))

;;;###autoload
(defun magit-dash-pr-status-stop-timer ()
  "Stop the periodic PR status sync timer.  Safe when not running."
  (interactive)
  (when (timerp magit-dash-pr-status-org--timer)
    (cancel-timer magit-dash-pr-status-org--timer)
    (setq magit-dash-pr-status-org--timer nil)))

(provide 'magit-dash-pr-status-org)
;;; magit-dash-pr-status-org.el ends here
