;;; magit-dash-pr-cache.el --- Persistent per-PR state cache for magit-dash -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1.0
;; Keywords: vc, tools, github
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; Stores one plist per pull request at
;; <state>/magit-dash-pr/<owner>/<repo>/<number>.eld (owner and repo
;; lowercased in the path only; records keep the original-case `:repo'),
;; where <state> comes from `sprite-state-path'.  Writes are
;; atomic (temp file, then rename), touch exactly one file, and are
;; skipped when nothing but the timestamp would change and the record is
;; still young.  Records older than `magit-dash-pr-cache-stale-minutes'
;; are stale: callers dim them with `magit-dash-stale' but never expire
;; them.  An unreadable, malformed or unwritable record is skipped with a
;; logged message and never signals.
;;
;; Record keys: :version :repo :number :url :fetched-at :field-times
;; :updated-at :head-sha :payload :status and :gone.
;;
;; Freshness.  `:fetched-at' is the time of the last write.  Because a
;; sparse discovery payload is merged over the cached one, old values of
;; the volatile fields (statusCheckRollup, reviewDecision, mergeable,
;; headRefOid) can survive a write; `:field-times' records, per volatile
;; field, when it was last supplied by a fetch.  `magit-dash-pr-cache-
;; effective-fetched-at' is the oldest of `:fetched-at' and those times,
;; and `magit-dash-pr-cache-stale-p' uses it, so a merged old CI value
;; never looks fresh.  A `:fetched-at' more than five minutes in the
;; future (clock skew) counts as stale, checked on `:fetched-at' itself so
;; an older field time cannot hide it.  A write from cross-repo (search)
;; discovery, which never supplies those fields, preserves existing
;; `:field-times' for the ones it merely keeps (so
;; `magit-dash-pr-cache-field-stale-p' reflects their real age) while
;; marking `:search-refreshed' so a PR first cached from a per-repo view
;; is not dimmed forever by search refreshes.
;;
;; Ordering.  A payload whose `updatedAt' is older than the cached one is
;; never merged in (no rollback); only the status may be refreshed.  A
;; newer payload lacking `headRefOid' clears the cached head sha.
;;
;; Ghosts.  `magit-dash-pr-cache-mark-gone' flags records that a complete
;; discovery no longer returns; open-state listings skip them and any later
;; write of the PR revives it, even one whose `updatedAt' is older (the
;; payload is then not merged, only the flag cleared).
;;
;; Case and migration.  Repo names are case-insensitive: records are filed
;; under lowercase directories, and legacy mixed-case directories are
;; migrated (renamed, or merged keeping the greater `:updated-at', then
;; `:fetched-at') on first access of a repo in a session (memoized, see
;; `magit-dash-pr-cache-reset-migrated'); every listing migrates its scope
;; again and dedupes by (lowercase repo, number).  A malformed legacy
;; record is moved aside as <number>.eld.bad and logged once.  A legacy
;; directory created by another Emacs mid-session is picked up by the next
;; listing, not by single-record reads.  A non-finite `:fetched-at' makes
;; a record invalid.
;;
;; Known limitations: ghost eviction needs every discovered row to parse
;; (any unusable row suppresses it); records are validated by shape, not
;; against GitHub; the `:url' is checked only for a matching pull request
;; number, since a renamed or transferred repo legitimately has another
;; name in it; cross-repo (search) discovery, filtered discovery,
;; truncated discovery and `hide failing' never mark ghosts, so such PRs
;; linger until a per-repo unfiltered discovery sees them missing;
;; dimming granularity is the whole row (while
;; `magit-dash-pr-cache-field-stale-p' tracks kept volatile fields
;; individually); the skip-rewrite optimisation lets a re-fetch of
;; identical data refresh `:fetched-at' only once per third of the stale
;; threshold; concurrent Emacs instances re-check `:updated-at' on disk
;; before renaming temp files, though non-`:updated-at' fields can still
;; race last-writer-wins.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'sprite)

(declare-function hitl-plan-get-domain "hitl-plan" (name))
(declare-function hitl-plan-register-domain "hitl-plan" (name &rest args))
(declare-function hitl-plan-register-op "hitl-plan" (domain name &rest args))
(declare-function hitl-plan-create "hitl-plan" (&rest args))
(declare-function hitl-plan-review "hitl-plan-ui" (plan-or-id &optional display))
(declare-function hitl-plan-skip "hitl-plan" (&optional reason))
(defvar hitl-plan-directory)
(defvar hitl-plan-current-target)

(defconst magit-dash-pr-cache-version 1
  "Schema version stamped into every cache record.")

(defconst magit-dash-pr-cache-minimum-stale-minutes 10
  "Lower bound for `magit-dash-pr-cache-stale-minutes'.")

(defun magit-dash-pr-cache--set-stale-minutes (symbol value)
  "Set SYMBOL to VALUE, clamping anything below the minimum.
A non-number VALUE falls back to 30 with a message."
  (cond
   ((numberp value)
    (set-default symbol (max magit-dash-pr-cache-minimum-stale-minutes value)))
   (t
    (message "magit-dash-pr-cache: %S is not a number, using 30" value)
    (set-default symbol 30))))

(defcustom magit-dash-pr-cache-stale-minutes 30
  "Minutes after which a cached PR record is considered stale.
Values below 10 are clamped to 10."
  :type 'number
  :set #'magit-dash-pr-cache--set-stale-minutes
  :group 'magit-dash)

(defcustom magit-dash-pr-cache-prune-days nil
  "When non-nil, only prune cache records older than this many days.
When nil, all gone, closed, or merged records are prune candidates."
  :type '(choice (const :tag "All gone/closed/merged records" nil) number)
  :group 'magit-dash)

(defface magit-dash-stale
  '((t :inherit shadow))
  "Face for rows whose cached data is older than the stale threshold."
  :group 'magit-dash)

(defun magit-dash-pr-cache-dim-cells (cells)
  "Return a copy of vector CELLS with `magit-dash-stale' added to strings."
  (seq-into
   (seq-map (lambda (cell)
              (if (stringp cell)
                  (let ((copy (copy-sequence cell)))
                    (add-face-text-property 0 (length copy) 'magit-dash-stale t copy)
                    copy)
                cell))
            cells)
   'vector))

(defun magit-dash-pr-cache-stale-seconds ()
  "Return the stale threshold in seconds."
  (* 60 (max magit-dash-pr-cache-minimum-stale-minutes
             (if (numberp magit-dash-pr-cache-stale-minutes)
                 magit-dash-pr-cache-stale-minutes
               30))))

(defconst magit-dash-pr-cache--volatile-keys
  '(statusCheckRollup reviewDecision mergeable headRefOid)
  "Payload keys that go stale quickly and are timestamped individually.")

(defconst magit-dash-pr-cache--future-skew 300
  "Seconds a `:fetched-at' may lie in the future before it counts as stale.")

(defun magit-dash-pr-cache--finite-p (value)
  "Return non-nil when VALUE is a finite number (not NaN or infinite)."
  (and (numberp value)
       (= value value)
       (< (abs value) 1.0e+INF)))

(defun magit-dash-pr-cache-effective-fetched-at (record)
  "Return the oldest freshness time of RECORD, or nil when it has none.
That is the minimum of `:fetched-at' and every `:field-times' entry,
unless `:search-refreshed' is non-nil (see `magit-dash-pr-cache-put'),
in which case `:fetched-at' is returned so kept volatile fields do not
dim the entire row.  A non-finite `:fetched-at' counts as none;
non-finite field times are ignored."
  (let ((fetched (plist-get record :fetched-at)))
    (when (magit-dash-pr-cache--finite-p fetched)
      (if (plist-get record :search-refreshed)
          fetched
        (apply #'min fetched
               (seq-filter #'magit-dash-pr-cache--finite-p
                           (seq-map #'cdr (plist-get record :field-times))))))))

(defun magit-dash-pr-cache-field-fetched-at (record field)
  "Return the freshness timestamp of volatile FIELD in RECORD, or nil."
  (let ((ts (or (alist-get field (plist-get record :field-times))
                (and (not (plist-get record :search-refreshed))
                     (plist-get record :fetched-at)))))
    (when (magit-dash-pr-cache--finite-p ts)
      ts)))

(defun magit-dash-pr-cache-field-stale-p (record field &optional now)
  "Return non-nil when volatile FIELD in RECORD is stale at NOW."
  (let ((fetched (magit-dash-pr-cache-field-fetched-at record field))
        (now (or now (float-time))))
    (or (not fetched)
        (magit-dash-pr-cache-future-p record now)
        (> (- now fetched) (magit-dash-pr-cache-stale-seconds)))))

(defun magit-dash-pr-cache-future-p (record &optional now)
  "Return non-nil when RECORD's `:fetched-at' lies beyond the clock skew at NOW."
  (let ((fetched (plist-get record :fetched-at)))
    (and (magit-dash-pr-cache--finite-p fetched)
         (> fetched (+ (or now (float-time)) magit-dash-pr-cache--future-skew)))))

(defun magit-dash-pr-cache-stale-p (record &optional now)
  "Return non-nil when RECORD is older than the stale threshold at NOW.
NOW defaults to the current `float-time'.  Age is that of the oldest
volatile field (see `magit-dash-pr-cache-effective-fetched-at').  A
record without `:fetched-at', or with one stamped in the future (checked
on `:fetched-at' alone, not the minimum), is stale."
  (let ((fetched (magit-dash-pr-cache-effective-fetched-at record))
        (now (or now (float-time))))
    (or (not fetched)
        (magit-dash-pr-cache-future-p record now)
        (> (- now fetched) (magit-dash-pr-cache-stale-seconds)))))

(defun magit-dash-pr-cache--root ()
  "Return the cache root directory."
  (file-name-as-directory (sprite-state-path "magit-dash-pr/")))

(defun magit-dash-pr-cache--repo-dir (repo)
  "Return the directory holding REPO's records, or nil when REPO is malformed.
REPO must be OWNER/NAME of letters, digits, underscore, dot and dash,
with no \".\" or \"..\" component.  Case is folded."
  (when (and (stringp repo)
             (string-match-p "\\`[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+\\'" repo)
             (not (seq-some (lambda (part) (member part '("." "..")))
                            (split-string repo "/"))))
    (file-name-as-directory
     (expand-file-name (downcase repo) (magit-dash-pr-cache--root)))))

(defun magit-dash-pr-cache--case-subdirs (dir name)
  "Return the subdirectories of DIR whose name equals NAME ignoring case."
  (seq-filter (lambda (sub)
                (equal (downcase (file-name-nondirectory sub)) (downcase name)))
              (magit-dash-pr-cache--subdirs dir)))

(defun magit-dash-pr-cache--legacy-dirs (repo)
  "Return existing mixed-case directories of REPO other than the lowercase one."
  (when-let* ((canon (magit-dash-pr-cache--repo-dir repo)))
    (let ((parts (split-string repo "/")))
      (seq-remove
       (lambda (dir)
         (or (equal (file-name-as-directory dir) canon)
             (file-equal-p dir canon)))
       (seq-mapcat (lambda (owner-dir)
                     (magit-dash-pr-cache--case-subdirs owner-dir (cadr parts)))
                   (magit-dash-pr-cache--case-subdirs
                    (magit-dash-pr-cache--root) (car parts)))))))

(defun magit-dash-pr-cache--better-p (a b)
  "Return non-nil when record A should replace record B.
The greater `:updated-at' wins (a record lacking one loses to one that
has it); equal or absent `:updated-at' falls back to the greater
`:fetched-at'."
  (let ((ua (plist-get a :updated-at))
        (ub (plist-get b :updated-at)))
    (cond
     ((and (stringp ua) (stringp ub) (not (string= ua ub)))
      (string> ua ub))
     ((and (stringp ua) (not (stringp ub))) t)
     ((and (stringp ub) (not (stringp ua))) nil)
     (t
      (> (or (plist-get a :fetched-at) 0)
         (or (plist-get b :fetched-at) 0))))))

(defvar magit-dash-pr-cache--migrated nil
  "Hash table of (ROOT . LOWERCASE-REPO) keys already migrated this session.")

(defun magit-dash-pr-cache-reset-migrated ()
  "Forget which repos were migrated, so the next access scans again.
Meant for tests and for picking up legacy directories created by another
Emacs instance mid-session."
  (setq magit-dash-pr-cache--migrated nil))

(defun magit-dash-pr-cache--merge-record (src canon)
  "Merge the legacy record file SRC into the canonical directory CANON."
  (let* ((dest (expand-file-name (file-name-nondirectory src) canon))
         (theirs (magit-dash-pr-cache--read-file src))
         (ours (and (file-exists-p dest)
                    (magit-dash-pr-cache--read-file dest))))
    (cond
     ((null theirs)
      (message "magit-dash-pr-cache: moving malformed legacy record %s aside" src)
      (rename-file src (expand-file-name
                        (concat (file-name-nondirectory src) ".bad") canon)
                   t))
     ((not (file-exists-p dest))
      (rename-file src dest))
     ((null ours)
      (message "magit-dash-pr-cache: canonical record %s is unreadable; replacing it with %s"
               dest src)
      (rename-file dest (concat dest ".bad") t)
      (rename-file src dest))
     ((magit-dash-pr-cache--better-p theirs ours)
      (rename-file src dest t))
     (t (delete-file src)))))

(defun magit-dash-pr-cache--migrate-legacy (repo)
  "Move records of mixed-case directories of REPO into the lowercase one.
When no lowercase directory exists the legacy one is renamed; otherwise
each record is merged in (see `magit-dash-pr-cache--better-p'), a
malformed one is moved aside as <number>.eld.bad, and the emptied legacy
directory is removed, so a bad record is not retried or logged again.
Failures are logged and never signal.  Marks REPO as migrated."
  (let ((canon (magit-dash-pr-cache--repo-dir repo)))
    (when canon
      (unless magit-dash-pr-cache--migrated
        (setq magit-dash-pr-cache--migrated (make-hash-table :test #'equal)))
      (setf (map-elt magit-dash-pr-cache--migrated
                     (cons (magit-dash-pr-cache--root) (downcase repo)))
            t)
      (seq-do
       (lambda (legacy)
         (condition-case err
             (unless (file-equal-p legacy canon)
               (when (not (file-directory-p canon))
                 (make-directory (file-name-directory (directory-file-name canon)) t)
                 (condition-case nil
                     (rename-file (directory-file-name legacy)
                                  (directory-file-name canon))
                   (file-already-exists nil)))
               (when (and (file-directory-p legacy)
                          (not (file-equal-p legacy canon)))
                 (seq-do (lambda (src)
                           (magit-dash-pr-cache--merge-record src canon))
                         (directory-files legacy t "\\.eld\\'"))
                 (ignore-errors (delete-directory legacy)))
               (ignore-errors (delete-directory
                               (file-name-directory (directory-file-name legacy)))))
           (error
            (message "magit-dash-pr-cache: could not migrate %s: %s"
                     legacy (error-message-string err)))))
       (magit-dash-pr-cache--legacy-dirs repo)))))

(defun magit-dash-pr-cache--ensure-migrated (repo)
  "Migrate REPO's legacy directories unless already done this session.
Once per repo, so reads and writes do not walk the cache root."
  (unless (and magit-dash-pr-cache--migrated
               (map-elt magit-dash-pr-cache--migrated
                        (cons (magit-dash-pr-cache--root) (downcase repo))))
    (magit-dash-pr-cache--migrate-legacy repo)))

(defun magit-dash-pr-cache--migrate-scope (org)
  "Migrate every mixed-case repo directory, limited to ORG (any case) if non-nil."
  (seq-do
   (lambda (owner-dir)
     (let ((owner (file-name-nondirectory owner-dir)))
       (seq-do
        (lambda (repo-dir)
          (let ((name (file-name-nondirectory repo-dir)))
            (when (or (not (equal owner (downcase owner)))
                      (not (equal name (downcase name))))
              (magit-dash-pr-cache--migrate-legacy (concat owner "/" name)))))
        (magit-dash-pr-cache--subdirs owner-dir))))
   (if org
       (magit-dash-pr-cache--case-subdirs (magit-dash-pr-cache--root) org)
     (magit-dash-pr-cache--subdirs (magit-dash-pr-cache--root)))))

(defun magit-dash-pr-cache--file (repo number)
  "Return the file for REPO and NUMBER, or nil when either is malformed.
Migrates legacy mixed-case directories of REPO first, once per session
\(see `magit-dash-pr-cache-reset-migrated')."
  (when-let* (((integerp number))
              (dir (magit-dash-pr-cache--repo-dir repo)))
    (magit-dash-pr-cache--ensure-migrated repo)
    (expand-file-name (format "%s.eld" number) dir)))

(defun magit-dash-pr-cache--null-to-nil (value)
  "Return VALUE with every `:null' replaced by nil.
Recurses on the car only and iterates along lists, so a very long list
\(a large `statusCheckRollup') cannot exhaust the Lisp stack."
  (cond
   ((eq value :null) nil)
   ((consp value)
    (let ((out nil)
          (tail value))
      (while (consp tail)
        (push (magit-dash-pr-cache--null-to-nil (car tail)) out)
        (setq tail (cdr tail)))
      (nconc (nreverse out) (magit-dash-pr-cache--null-to-nil tail))))
   (t value)))

(defun magit-dash-pr-cache--url-repo (url)
  "Return the lowercase OWNER/NAME of the GitHub pull request URL, or nil."
  (when (and (stringp url)
             (string-match "\\`https?://[^/]+/\\([^/]+/[^/]+\\)/pull/[0-9]+" url))
    (downcase (match-string 1 url))))

(defun magit-dash-pr-cache--url-number (url)
  "Return the pull request number in the GitHub pull request URL, or nil."
  (when (and (stringp url)
             (string-match "\\`https?://[^/]+/[^/]+/[^/]+/pull/\\([0-9]+\\)" url))
    (string-to-number (match-string 1 url))))

(defvar magit-dash-pr-cache--renames-logged nil
  "Hash table of lowercase repos whose url/record name mismatch was logged.")

(defun magit-dash-pr-cache--note-rename (repo url)
  "Log once per REPO that URL names a different repo (rename or transfer)."
  (when-let* ((url-repo (magit-dash-pr-cache--url-repo url))
              ((not (equal url-repo (downcase repo)))))
    (unless magit-dash-pr-cache--renames-logged
      (setq magit-dash-pr-cache--renames-logged (make-hash-table :test #'equal)))
    (unless (map-elt magit-dash-pr-cache--renames-logged (downcase repo))
      (setf (map-elt magit-dash-pr-cache--renames-logged (downcase repo)) t)
      (message "magit-dash-pr-cache: %s is cached with a url naming %s (renamed?)"
               repo url-repo))))

(defun magit-dash-pr-cache--problem (record)
  "Return a string describing why RECORD is invalid, or nil when it is sound."
  (let ((payload (plist-get record :payload))
        (fetched (plist-get record :fetched-at))
        (version (or (plist-get record :version) 1))
        (url-number (magit-dash-pr-cache--url-number (plist-get record :url))))
    (cond
     ((not (and (proper-list-p record) (plistp record))) "not a plist")
     ((not (eql version magit-dash-pr-cache-version))
      (format "unknown version %S" version))
     ((not (magit-dash-pr-cache--repo-dir (plist-get record :repo))) "bad :repo")
     ((not (integerp (plist-get record :number))) "bad :number")
     ((not (or (null payload)
               (and (proper-list-p payload) (seq-every-p #'consp payload))))
      "bad :payload")
     ((not (or (null fetched) (magit-dash-pr-cache--finite-p fetched)))
      "bad :fetched-at")
     ((not (let ((times (plist-get record :field-times)))
             (and (proper-list-p times)
                  (seq-every-p (lambda (cell)
                                 (and (consp cell) (symbolp (car cell))
                                      (magit-dash-pr-cache--finite-p (cdr cell))))
                               times))))
      "bad :field-times")
     ((and url-number (not (eql url-number (plist-get record :number))))
      (format "url number %s differs from record" url-number)))))

(defun magit-dash-pr-cache--read-file (file)
  "Return the validated record stored in FILE, or nil with a message.
A malformed `:status' is dropped (with a message) but keeps the record."
  (condition-case err
      (let ((record (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8-unix))
                        (insert-file-contents file))
                      (read (current-buffer)))))
        (cond
         ((magit-dash-pr-cache--problem record)
          (message "magit-dash-pr-cache: skipping %s: %s"
                   file (magit-dash-pr-cache--problem record))
          nil)
         ((let ((status (plist-get record :status)))
            (not (or (null status) (and (proper-list-p status) (plistp status)))))
          (message "magit-dash-pr-cache: dropping malformed :status in %s" file)
          (plist-put record :status nil))
         (t record)))
    (error
     (message "magit-dash-pr-cache: skipping unreadable %s: %s"
              file (error-message-string err))
     nil)))

(defun magit-dash-pr-cache-get (repo number)
  "Return the cached record for REPO and NUMBER, or nil."
  (when-let* ((file (magit-dash-pr-cache--file repo number))
              ((file-readable-p file)))
    (magit-dash-pr-cache--read-file file)))

(defun magit-dash-pr-cache--subdirs (dir)
  "Return the subdirectories of DIR, or nil when DIR does not exist."
  (when (file-directory-p dir)
    (seq-filter #'file-directory-p (directory-files dir t "\\`[^.]"))))

(defun magit-dash-pr-cache--sweep-orphans (dir)
  "Delete leftover temp files in DIR older than a day."
  (seq-do (lambda (tmp)
            (when-let* ((attrs (file-attributes tmp))
                        ((> (- (float-time) (float-time (file-attribute-modification-time attrs)))
                            86400)))
              (ignore-errors (delete-file tmp))))
          (directory-files dir t "\\`\\.pr-.*\\.tmp\\'")))

(defun magit-dash-pr-cache--dedupe (records)
  "Return RECORDS with one per (lowercase repo, number), the best.
See `magit-dash-pr-cache--better-p'."
  (let ((best (make-hash-table :test #'equal))
        (order nil))
    (seq-do (lambda (record)
              (let* ((key (cons (downcase (plist-get record :repo))
                                (plist-get record :number)))
                     (kept (map-elt best key)))
                (cond
                 ((null kept)
                  (push key order)
                  (setf (map-elt best key) record))
                 ((magit-dash-pr-cache--better-p record kept)
                  (setf (map-elt best key) record)))))
            records)
    (seq-map (lambda (key)
               (map-elt best key))
             (nreverse order))))

(defun magit-dash-pr-cache-list (&optional repo org)
  "Return the cached records for REPO, ORG's repos, or every repo.
Only the directories of the requested scope are read.  Never signals; an
unscannable directory yields nil."
  (let ((dirs (condition-case err
                  (cond
                   (repo
                    (magit-dash-pr-cache--migrate-legacy repo)
                    (when-let* ((dir (magit-dash-pr-cache--repo-dir repo))
                                ((file-directory-p dir)))
                      (list dir)))
                   ((and (stringp org)
                         (string-match-p "\\`[A-Za-z0-9_.-]+\\'" org)
                         (not (member org '("." ".."))))
                    (magit-dash-pr-cache--migrate-scope org)
                    (magit-dash-pr-cache--subdirs
                     (expand-file-name (downcase org) (magit-dash-pr-cache--root))))
                   (org nil)
                   (t
                    (magit-dash-pr-cache--migrate-scope nil)
                    (seq-mapcat #'magit-dash-pr-cache--subdirs
                                (magit-dash-pr-cache--subdirs
                                 (magit-dash-pr-cache--root)))))
                (error
                 (message "magit-dash-pr-cache: could not scan cache: %s"
                          (error-message-string err))
                 nil))))
    (magit-dash-pr-cache--dedupe
     (seq-keep #'magit-dash-pr-cache--read-file
              (seq-mapcat (lambda (dir)
                            (condition-case err
                                (progn
                                  (magit-dash-pr-cache--sweep-orphans dir)
                                  (directory-files dir t "\\.eld\\'"))
                              (error
                               (message "magit-dash-pr-cache: could not scan %s: %s"
                                        dir (error-message-string err))
                               nil)))
                          dirs)))))

(defun magit-dash-pr-cache--write (file record &optional guard-newer)
  "Atomically write RECORD to FILE.
When GUARD-NEWER is non-nil, re-read FILE right before renaming and
refuse to overwrite a record whose `:updated-at' is strictly newer;
return `(:written . RECORD)' or `(:conflict . ON-DISK)'."
  (let ((dir (file-name-directory file)))
    (make-directory dir t)
    (let ((tmp (make-temp-file (expand-file-name ".pr-" dir) nil ".tmp")))
      (condition-case err
          (progn
            (let ((print-length nil)
                  (print-level nil)
                  (print-circle nil)
                  (coding-system-for-write 'utf-8-unix))
              (with-temp-file tmp
                (prin1 record (current-buffer))
                (insert "\n")))
            (if guard-newer
                (let ((on-disk (and (file-readable-p file)
                                    (magit-dash-pr-cache--read-file file))))
                  (if (and on-disk
                           (magit-dash-pr-cache--newer-p
                            (plist-get on-disk :updated-at)
                            (plist-get record :updated-at)))
                      (progn
                        (ignore-errors (delete-file tmp))
                        (cons :conflict on-disk))
                    (rename-file tmp file t)
                    (cons :written record)))
              (rename-file tmp file t)
              record))
        (error
         (ignore-errors (delete-file tmp))
         (signal (car err) (cdr err)))))))

(defun magit-dash-pr-cache-merge-payload (fresh old)
  "Return FRESH alist extended with the keys of OLD that FRESH lacks.
Keys are deduplicated by `car'; the first (fresh) value always wins, so
repeated merges never grow the alist."
  (let ((unique (seq-uniq fresh (lambda (a b) (equal (car a) (car b))))))
    (append unique
            (seq-uniq (seq-remove (lambda (cell) (assoc (car cell) unique)) old)
                      (lambda (a b) (equal (car a) (car b)))))))

(defun magit-dash-pr-cache--newer-p (a b)
  "Return non-nil when timestamp string A is later than string B."
  (and (stringp a) (stringp b) (string> a b)))

(defun magit-dash-pr-cache--skippable-p (new prior)
  "Return non-nil when writing NEW over PRIOR would only bump the timestamp.
That requires identical content and a PRIOR younger than a third of the
stale threshold."
  (and prior
       (equal (plist-get new :payload) (plist-get prior :payload))
       (equal (plist-get new :status) (plist-get prior :status))
       (equal (plist-get new :url) (plist-get prior :url))
       (eq (plist-get new :gone) (plist-get prior :gone))
       (eq (not (not (plist-get new :search-refreshed)))
           (not (not (plist-get prior :search-refreshed))))
       (when-let* ((fetched (magit-dash-pr-cache-effective-fetched-at prior)))
         (< 0 (- (float-time) fetched) (/ (magit-dash-pr-cache-stale-seconds) 3)))))

(defun magit-dash-pr-cache--handle-stale-write (file repo number existing status)
  "Apply non-rollback update for REPO/NUMBER against newer EXISTING record.
Clears `:gone' when flagged and applies STATUS when non-nil."
  (cond
   ((plist-get existing :gone)
    (let ((revived (plist-put (copy-sequence existing) :gone nil)))
      (when status
        (setq revived (plist-put revived :status status)))
      (magit-dash-pr-cache--write file revived)
      revived))
   (status
    (magit-dash-pr-cache-put-status
     repo number status (plist-get existing :payload) existing))))

(defun magit-dash-pr-cache-put (repo number payload &optional status prior search-p)
  "Store PAYLOAD (a `gh pr view' alist) for REPO and NUMBER.
PAYLOAD is merged over the cached payload (fresh keys win).  Keep the
existing `:status' when STATUS is nil.  PRIOR, when non-nil, is the
already-read previous record and is checked against disk if newer data
was written concurrently.  REPO's case is kept in the record.  SEARCH-P
non-nil says PAYLOAD comes from cross-repo search, which preserves kept
`:field-times' while marking `:search-refreshed' (see Commentary).

A PAYLOAD whose `updatedAt' is older than the cached one is dropped (no
rollback; STATUS alone is applied, and a `:gone' flag is cleared).  A
payload whose `url' names another pull request number is refused; a
different repo name in it (rename, transfer) is accepted and logged once
per repo.  Touch exactly one file.  Return the record, or nil when it
could not be written."
  (condition-case err
      (let ((file (magit-dash-pr-cache--file repo number)))
        (cond
         ((not file)
          (message "magit-dash-pr-cache: not caching %S#%S: malformed repo or number"
                   repo number)
          nil)
         ((let ((url-number (magit-dash-pr-cache--url-number (map-elt payload 'url))))
            (and url-number (not (eql url-number number))))
          (message "magit-dash-pr-cache: not caching %s#%s: url names #%s"
                   repo number (magit-dash-pr-cache--url-number (map-elt payload 'url)))
          nil)
         (t
          (magit-dash-pr-cache--note-rename repo (map-elt payload 'url))
          (let* ((disk (and prior
                            (file-readable-p file)
                            (magit-dash-pr-cache--read-file file)))
                 (prior (cond
                         ((and prior disk (magit-dash-pr-cache--better-p disk prior))
                          disk)
                         (prior prior)
                         (t (magit-dash-pr-cache-get repo number))))
                 (fresh (magit-dash-pr-cache--null-to-nil payload))
                 (old (plist-get prior :payload))
                 (stale-write (magit-dash-pr-cache--newer-p
                               (map-elt old 'updatedAt) (map-elt fresh 'updatedAt))))
            (if stale-write
                (magit-dash-pr-cache--handle-stale-write file repo number prior status)
              (let* ((now (float-time))
                     (newer (magit-dash-pr-cache--newer-p
                             (map-elt fresh 'updatedAt) (map-elt old 'updatedAt)))
                     (merged (magit-dash-pr-cache-merge-payload
                              fresh
                              (if (and newer (not (assq 'headRefOid fresh)))
                                  (assq-delete-all 'headRefOid (copy-sequence old))
                                old)))
                     (old-times (plist-get prior :field-times))
                     (record (list :version magit-dash-pr-cache-version
                                   :repo repo
                                   :number number
                                   :url (map-elt merged 'url)
                                   :fetched-at now
                                   :field-times
                                   (seq-keep
                                    (lambda (key)
                                      (cond
                                       ((assq key fresh) (cons key now))
                                       ((assq key merged)
                                        (when-let* ((ts (or (alist-get key old-times)
                                                            (plist-get prior :fetched-at))))
                                          (cons key ts)))))
                                    magit-dash-pr-cache--volatile-keys)
                                   :search-refreshed (and search-p t)
                                   :updated-at (map-elt merged 'updatedAt)
                                   :head-sha (map-elt merged 'headRefOid)
                                   :payload merged
                                   :status (or status (plist-get prior :status)))))
                (cond
                 ((magit-dash-pr-cache--skippable-p record prior)
                  prior)
                 (t
                  (let ((result (magit-dash-pr-cache--write file record t)))
                    (if (eq (car result) :conflict)
                        (magit-dash-pr-cache--handle-stale-write
                         file repo number (cdr result) status)
                      (cdr result)))))))))))
    (error
     (message "magit-dash-pr-cache: could not write %s#%s: %s"
              repo number (error-message-string err))
     nil)))

(defun magit-dash-pr-cache-put-status (repo number status payload &optional prior)
  "Store STATUS for REPO and NUMBER without refreshing the record's age.
When a previous record exists (PRIOR, or read from disk) only its
`:status' is replaced; `:fetched-at', `:field-times', `:payload',
`:updated-at' and `:head-sha' are preserved so stale rows stay stale.
Without one, write PAYLOAD with a fresh stamp.  Return the record, or
nil when it could not be written."
  (condition-case err
      (when-let* ((file (magit-dash-pr-cache--file repo number)))
        (let ((disk (and prior
                         (file-readable-p file)
                         (magit-dash-pr-cache--read-file file))))
          (if-let* ((old (cond
                          ((and prior disk (magit-dash-pr-cache--better-p disk prior))
                           disk)
                          (prior prior)
                          (t (magit-dash-pr-cache-get repo number)))))
              (let ((record (plist-put (copy-sequence old) :status status)))
                (magit-dash-pr-cache--write file record)
                record)
            (magit-dash-pr-cache-put repo number payload status))))
    (error
     (message "magit-dash-pr-cache: could not write %s#%s: %s"
              repo number (error-message-string err))
     nil)))

(defun magit-dash-pr-cache-delete (repo number)
  "Delete the cached record for REPO and NUMBER; return non-nil if one existed."
  (condition-case err
      (when-let* ((file (magit-dash-pr-cache--file repo number))
                  ((file-exists-p file)))
        (delete-file file)
        t)
    (error
     (message "magit-dash-pr-cache: could not delete %s#%s: %s"
              repo number (error-message-string err))
     nil)))

(defun magit-dash-pr-cache-mark-gone (repo number)
  "Flag the record for REPO and NUMBER `:gone'; return the record or nil.
Nothing is written when there is no record or it is already flagged."
  (condition-case err
      (when-let* ((file (magit-dash-pr-cache--file repo number))
                  (record (magit-dash-pr-cache-get repo number)))
        (if (plist-get record :gone)
            record
          (let ((gone (plist-put (copy-sequence record) :gone t)))
            (magit-dash-pr-cache--write file gone)
            gone)))
    (error
     (message "magit-dash-pr-cache: could not mark %s#%s gone: %s"
              repo number (error-message-string err))
     nil)))

(defun magit-dash-pr-cache--prune-reason (record)
  "Return why RECORD is prunable (\"gone\", \"merged\", or \"closed\"), or nil."
  (let* ((state (map-elt (plist-get record :payload) 'state))
         (upper (and (stringp state) (upcase state))))
    (cond
     ((plist-get record :gone) "gone")
     ((equal upper "MERGED") "merged")
     ((equal upper "CLOSED") "closed"))))

(defun magit-dash-pr-cache-prunable-records (&optional repo days)
  "Return cached PR records eligible for pruning.
A record is prunable when it is flagged `:gone' or its payload `state'
is \"MERGED\" or \"CLOSED\" (case-insensitive).  REPO limits the scan to
that owner/name repository.  DAYS (defaulting to
`magit-dash-pr-cache-prune-days'), when a positive number, further limits
results to records older than DAYS days."
  (let* ((max-days (if (numberp days)
                       days
                     magit-dash-pr-cache-prune-days))
         (cutoff (when (and (numberp max-days) (> max-days 0))
                   (- (float-time) (* max-days 86400)))))
    (seq-filter
     (lambda (record)
       (and (magit-dash-pr-cache--prune-reason record)
            (or (null cutoff)
                (let ((fetched (magit-dash-pr-cache-effective-fetched-at record)))
                  (or (null fetched) (< fetched cutoff))))))
     (magit-dash-pr-cache-list repo))))

(defun magit-dash-pr-cache--plan-directory ()
  "Return the directory holding PR cache prune plans."
  (file-name-as-directory
   (expand-file-name "magit-dash-pr-cache/"
                     (or (bound-and-true-p hitl-plan-directory)
                         (sprite-state-path "hitl-plans/")))))

(defun magit-dash-pr-cache--format-prune-target (target)
  "Return (KEY . LABEL) columns for prune TARGET."
  (let ((repo (or (plist-get target :repo) "?"))
        (number (or (plist-get target :number) "?"))
        (title (plist-get target :title))
        (reason (or (plist-get target :reason) "prunable")))
    (cons (format "%s#%s" repo number)
          (if (and (stringp title) (not (string-empty-p title)))
              (format "%s (%s)" title reason)
            (format "(%s)" reason)))))

(defun magit-dash-pr-cache--visit-prune-target (target &optional other-window)
  "Open TARGET's cache file read-only, or in another window if OTHER-WINDOW."
  (when-let* ((repo (plist-get target :repo))
              (number (plist-get target :number))
              (file (magit-dash-pr-cache--file repo number))
              ((file-exists-p file)))
    (if other-window
        (view-file-other-window file)
      (view-file file))))

(defun magit-dash-pr-cache--sanitize-prune-target (target)
  "Validate prune TARGET plist and return a canonical copy."
  (let ((repo (and (consp target) (plist-get target :repo)))
        (number (and (consp target) (plist-get target :number))))
    (unless (and (magit-dash-pr-cache--repo-dir repo) (integerp number))
      (user-error "Invalid PR cache prune target: %S" target))
    (let ((record (magit-dash-pr-cache-get repo number)))
      (unless record
        (user-error "Cached record %s#%s no longer exists" repo number))
      (list :repo (plist-get record :repo)
            :number number
            :title (or (map-elt (plist-get record :payload) 'title) "")
            :reason (or (magit-dash-pr-cache--prune-reason record) "prunable")))))

(defun magit-dash-pr-cache--apply-prune ()
  "Delete the cached PR record named by `hitl-plan-current-target'."
  (let* ((target hitl-plan-current-target)
         (repo (plist-get target :repo))
         (number (plist-get target :number))
         (file (magit-dash-pr-cache--file repo number))
         (record (magit-dash-pr-cache-get repo number))
         (skip-reason
          (cond
           ((null record) "already deleted")
           ((not (magit-dash-pr-cache--prune-reason record))
            "no longer gone or closed"))))
    (cond
     ((and skip-reason (fboundp 'hitl-plan-skip))
      (hitl-plan-skip skip-reason))
     (skip-reason
      (error "%s" skip-reason))
     ((and (not (magit-dash-pr-cache-delete repo number))
           file
           (file-exists-p file))
      (error "Could not delete cached PR record %s#%s (%s)" repo number file)))))

;;;###autoload
(defun magit-dash-pr-cache-prune (&optional repo days)
  "Prune cached PR records that are `:gone', closed, or merged.
When REPO (OWNER/NAME) is non-nil, limit to that repository.  DAYS
overrides `magit-dash-pr-cache-prune-days'.  Uses `hitl-plan' for
step-by-step review when available, otherwise falls back to `y-or-n-p'."
  (interactive
   (list (when current-prefix-arg
           (let ((slug (string-trim (read-string "Repo (owner/name, empty for all): "))))
             (unless (string-empty-p slug)
               slug)))))
  (let ((records (magit-dash-pr-cache-prunable-records repo days)))
    (if (null records)
        (progn
          (message "magit-dash-pr-cache: no gone or closed PR records to prune")
          nil)
      (let ((targets
             (seq-map
              (lambda (record)
                (list :repo (plist-get record :repo)
                      :number (plist-get record :number)
                      :title (or (map-elt (plist-get record :payload) 'title) "")
                      :reason (magit-dash-pr-cache--prune-reason record)))
              records)))
        (if (and (fboundp 'hitl-plan-create)
                 (fboundp 'hitl-plan-get-domain)
                 (fboundp 'hitl-plan-register-domain)
                 (fboundp 'hitl-plan-register-op)
                 (fboundp 'hitl-plan-review))
            (progn
              (unless (hitl-plan-get-domain 'magit-dash-pr-cache)
                (hitl-plan-register-domain 'magit-dash-pr-cache
                  :label "PR cache prune"
                  :resolve-target #'identity
                  :format-target #'magit-dash-pr-cache--format-prune-target
                  :visit-target #'magit-dash-pr-cache--visit-prune-target
                  :directory #'magit-dash-pr-cache--plan-directory
                  :sanitize-target #'magit-dash-pr-cache--sanitize-prune-target)
                (hitl-plan-register-op 'magit-dash-pr-cache 'prune
                  :key ?d
                  :label "delete cache file"
                  :apply #'magit-dash-pr-cache--apply-prune
                  :describe (lambda (&rest _) "delete cached .eld record")
                  :schema '("none: each target names its own cache record")))
              (let ((plan (hitl-plan-create
                           :domain 'magit-dash-pr-cache
                           :targets targets
                           :ops '(prune)
                           :description (if repo
                                            (format "Prune gone/closed PR cache for %s" repo)
                                          "Prune gone/closed PR cache"))))
                (hitl-plan-review plan)
                plan))
          (let* ((count (length targets))
                 (preview (string-join
                           (seq-map (lambda (target)
                                      (format "%s#%s (%s)"
                                              (plist-get target :repo)
                                              (plist-get target :number)
                                              (plist-get target :reason)))
                                    (seq-take targets 5))
                           ", "))
                 (summary (if (> count 5)
                              (format "%s, ... (+%d more)" preview (- count 5))
                            preview)))
            (when (y-or-n-p (format "Prune %d cached PR record(s) [%s]? "
                                    count summary))
              (let ((deleted
                     (seq-filter
                      (lambda (target)
                        (magit-dash-pr-cache-delete
                         (plist-get target :repo)
                         (plist-get target :number)))
                      targets)))
                (message "magit-dash-pr-cache: pruned %d of %d record(s)"
                         (length deleted) count)
                (length deleted)))))))))

(provide 'magit-dash-pr-cache)
;;; magit-dash-pr-cache.el ends here
