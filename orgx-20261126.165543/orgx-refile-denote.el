;;; orgx-refile-denote.el --- Refile Org subtrees to Denote notes -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, denote, convenience

;;; Commentary:
;; Adds Denote targets to `org-refile' (and `org-agenda-refile').
;; The refile prompt offers entries for selecting an existing Denote note,
;; creating a new Denote note from the subtree, or choosing from active notes.
;; Once a note is chosen, refiling can target a specific heading within that note
;; or the note's top level.
;;
;; Also provides the interactive command `orgx-refile-denote' (with aliases
;; `orgx-denote-refile', `org-refile-to-denote', and `denote-org-refile-to-denote')
;; to refile directly to a Denote note from Org buffers and agenda.
;;
;; Enable with `orgx-refile-denote-enable' (done by `orgx-setup' and `orgx-global-mode').

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-refile)
(require 'org-agenda)

(autoload 'denote-directories "denote")
(autoload 'denote-directory-files "denote")
(autoload 'denote-file-prompt "denote")
(declare-function denote-retrieve-filename-keywords-as-list "denote")
(declare-function denote-get-file-name-relative-to-denote-directory "denote")
(declare-function denote-convert-subtree "denote-convert")
(declare-function denote-org-extract-org-subtree "denote-org")
(declare-function org-agenda-redo "org-agenda")
(declare-function org-get-at-bol "org-macs")

(defvar org-refile-history)
(defvar org-heading-regexp)

(defgroup orgx-refile-denote nil
  "Refile Org subtrees to Denote notes."
  :group 'orgx
  :prefix "orgx-refile-denote-")

(defcustom orgx-refile-denote-prefix "denote:"
  "Prefix of the Denote entries in the refile target list."
  :type 'string
  :group 'orgx-refile-denote)

(defcustom orgx-refile-denote-target-mode 'both
  "Which Denote targets to offer in `org-refile'.
`both' offers `(choose note)', `(new note)', and active note files.
`prompt-only' offers only `(choose note)' and `(new note)'.
`notes-only' offers active note files and `(new note)'."
  :type '(choice (const :tag "Both prompt actions and note files" both)
                 (const :tag "Prompt actions only" prompt-only)
                 (const :tag "Note files only" notes-only))
  :group 'orgx-refile-denote)

(defcustom orgx-refile-denote-prompt-heading t
  "When non-nil, prompt for a heading in the chosen Denote note.
Selecting `(top level)' or providing an empty input refiles to the top level."
  :type 'boolean
  :group 'orgx-refile-denote)

(defconst orgx-refile-denote--file "denote"
  "Pseudo file name carried by Denote refile targets.")

(defvar orgx-refile-denote--active nil
  "Non-nil while `org-refile' is running and Denote targets should be offered.")

(defvar orgx-refile-denote--marker nil
  "Marker of the heading being refiled from the agenda, or nil.")

(defvar orgx-refile-denote--entries nil
  "Alist (NAME . TARGET-VAL) of the Denote targets offered by current refile.")

(defun orgx-refile-denote--entry-alist ()
  "Return an alist (NAME . TARGET-VAL) of Denote refile targets."
  (let* ((prefix (concat orgx-refile-denote-prefix " "))
         (prompt-entries (list (cons (concat prefix "(choose note)") :choose)
                               (cons (concat prefix "(new note)") :new))))
    (cond
     ((eq orgx-refile-denote-target-mode 'prompt-only)
      prompt-entries)
     (t
      (let* ((roots (and (fboundp 'denote-directories) (denote-directories)))
             (files (when roots
                      (seq-filter (lambda (f) (string-suffix-p ".org" f))
                                  (denote-directory-files nil nil :text-only nil roots))))
             (note-entries
              (when files
                (thread-last files
                  (seq-filter
                   (lambda (f)
                     (let ((kws (when (fboundp 'denote-retrieve-filename-keywords-as-list)
                                  (denote-retrieve-filename-keywords-as-list f))))
                       (not (or (member "x" kws) (member "done" kws))))))
                  (seq-map
                   (lambda (f)
                     (let ((rel (if (fboundp 'denote-get-file-name-relative-to-denote-directory)
                                    (denote-get-file-name-relative-to-denote-directory f)
                                  (file-name-nondirectory f))))
                       (cons (concat prefix rel) f))))))))
        (if (eq orgx-refile-denote-target-mode 'notes-only)
            (cons (cons (concat prefix "(new note)") :new) note-entries)
          (append prompt-entries note-entries)))))))

(defun orgx-refile-denote--lookup (name)
  "Return the (NAME . TARGET-VAL) entry in `orgx-refile-denote--entries' for NAME.
NAME may carry suffixes added by `org-refile-get-location'."
  (let ((bare (thread-last name
                (replace-regexp-in-string
                 (format " (%s)\\'" (regexp-quote orgx-refile-denote--file)) "")
                (replace-regexp-in-string "/\\'" ""))))
    (assoc bare orgx-refile-denote--entries)))

(defun orgx-refile-denote--headings-alist (file)
  "Return an alist of (OUTLINE-PATH . POSITION) for headings in FILE."
  (with-current-buffer (find-file-noselect file 'nowarn)
    (unless (derived-mode-p 'org-mode)
      (org-mode))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let (headings)
          (while (re-search-forward org-heading-regexp nil t)
            (push (cons (string-join (org-get-outline-path t) "/")
                        (line-beginning-position))
                  headings))
          (nreverse headings))))))

(defun orgx-refile-denote--read-heading (file)
  "Prompt for a heading in FILE, returning its buffer position, or nil for top level."
  (if (not orgx-refile-denote-prompt-heading)
      nil
    (if-let* ((headings (orgx-refile-denote--headings-alist file)))
        (let* ((top-choice "(top level)")
               (candidates (cons top-choice (seq-map #'car headings)))
               (choice (completing-read
                        (format "Heading in %s (RET for top level): "
                                (file-name-nondirectory file))
                        candidates nil t nil nil top-choice)))
          (if (or (string-empty-p choice) (equal choice top-choice))
              nil
            (alist-get choice headings nil nil #'equal)))
      nil)))

(defun orgx-refile-denote-to-new-note ()
  "Extract the current Org subtree to a new Denote note."
  (cond
   ((fboundp 'denote-convert-subtree)
    (denote-convert-subtree))
   ((fboundp 'denote-org-extract-org-subtree)
    (denote-org-extract-org-subtree))
   (t
    (user-error "Neither `denote-convert-subtree' nor `denote-org-extract-org-subtree' is available"))))

(defun orgx-refile-denote--ad-get-targets (targets)
  "Filter-return advice for `org-refile-get-targets': append Denote TARGETS."
  (if (not orgx-refile-denote--active)
      targets
    (setq orgx-refile-denote--entries (orgx-refile-denote--entry-alist))
    (append targets
            (seq-map (lambda (e) (list (car e) orgx-refile-denote--file nil nil))
                     orgx-refile-denote--entries))))

(defun orgx-refile-denote--ad-get-location (location)
  "Filter-return advice for `org-refile-get-location'.
When LOCATION is a Denote target, resolve note/heading and return a concrete refile location."
  (if-let* (orgx-refile-denote--active
            ((equal (nth 1 location) orgx-refile-denote--file))
            (entry (orgx-refile-denote--lookup (car location))))
      (let ((val (cdr entry)))
        (cond
         ((eq val :new)
          (if (markerp orgx-refile-denote--marker)
              (org-with-point-at orgx-refile-denote--marker
                (orgx-refile-denote-to-new-note))
            (orgx-refile-denote-to-new-note))
          (throw 'orgx-refile-denote-handled 'handled))
         (t
          (let* ((target-file (if (eq val :choose)
                                  (denote-file-prompt "\\.org\\'" "Refile to Denote note: ")
                                val))
                 (pos (orgx-refile-denote--read-heading target-file)))
            (list (file-name-nondirectory target-file) target-file nil pos)))))
    location))

(defun orgx-refile-denote--call (fn)
  "Call FN; return `handled' if a new note target was chosen, else result of FN."
  (let ((saved org-refile-history)
        (result (catch 'orgx-refile-denote-handled
                  (funcall fn))))
    (if (eq result 'handled)
        (progn
          (setq org-refile-history saved)
          'handled)
      result)))

(defun orgx-refile-denote--ad-refile (orig &optional arg default-buffer rfloc msg)
  "Around advice for `org-refile': offer Denote targets when prompting.
ORIG is called with ARG, DEFAULT-BUFFER, RFLOC and MSG."
  (let ((orgx-refile-denote--active (null rfloc))
        (orgx-refile-denote--marker nil))
    (orgx-refile-denote--call (lambda () (funcall orig arg default-buffer rfloc msg)))))

(defun orgx-refile-denote--ad-agenda-refile (orig &optional goto rfloc no-update)
  "Around advice for `org-agenda-refile': offer Denote targets.
ORIG is called with GOTO, RFLOC and NO-UPDATE."
  (let ((orgx-refile-denote--active (and (null rfloc) (null goto)))
        (orgx-refile-denote--marker (org-get-at-bol 'org-hd-marker)))
    (let ((res (orgx-refile-denote--call
                (lambda () (funcall orig goto rfloc no-update)))))
      (when (and (eq res 'handled) (not no-update))
        (org-agenda-redo))
      res)))

;;;###autoload
(defun orgx-refile-denote (&optional arg)
  "Refile the Org subtree at point to a Denote note.
Works in `org-mode' and `org-agenda-mode' buffers.  With prefix ARG (e.g. 3),
copy the subtree instead of moving it."
  (interactive "P")
  (if (derived-mode-p 'org-agenda-mode)
      (let ((marker (or (org-get-at-bol 'org-hd-marker)
                        (user-error "No headline at point in agenda"))))
        (org-with-point-at marker
          (orgx-refile-denote arg))
        (org-agenda-redo))
    (unless (derived-mode-p 'org-mode)
      (user-error "Must be in an org-mode or org-agenda-mode buffer"))
    (let* ((target-file (denote-file-prompt "\\.org\\'" "Refile to Denote note: "))
           (pos (orgx-refile-denote--read-heading target-file))
           (rfloc (list (file-name-nondirectory target-file) target-file nil pos)))
      (org-refile arg nil rfloc))))

;;;###autoload
(defalias 'orgx-denote-refile #'orgx-refile-denote)

;;;###autoload
(defalias 'org-refile-to-denote #'orgx-refile-denote)

;;;###autoload
(defalias 'denote-org-refile-to-denote #'orgx-refile-denote)

;;;###autoload
(defalias 'orgx-refile-to-denote #'orgx-refile-denote)

;;;###autoload
(defun orgx-refile-denote-enable ()
  "Offer Denote targets in `org-refile'."
  (advice-add 'org-refile :around #'orgx-refile-denote--ad-refile)
  (advice-add 'org-agenda-refile :around #'orgx-refile-denote--ad-agenda-refile)
  (advice-add 'org-refile-get-targets :filter-return #'orgx-refile-denote--ad-get-targets)
  (advice-add 'org-refile-get-location :filter-return #'orgx-refile-denote--ad-get-location))

;;;###autoload
(defun orgx-refile-denote-disable ()
  "Stop offering Denote targets in `org-refile'."
  (advice-remove 'org-refile #'orgx-refile-denote--ad-refile)
  (advice-remove 'org-agenda-refile #'orgx-refile-denote--ad-agenda-refile)
  (advice-remove 'org-refile-get-targets #'orgx-refile-denote--ad-get-targets)
  (advice-remove 'org-refile-get-location #'orgx-refile-denote--ad-get-location))

(provide 'orgx-refile-denote)
;;; orgx-refile-denote.el ends here
