;;; code-review-api-analysis.el --- Public analysis accessor for a PR -*- lexical-binding: t; -*-

;;; Commentary:

;; A non-interactive, buffer-free accessor over the analysis the
;; review render computes: `code-review-api-analysis' returns, for one
;; stored PR, a nested alist
;;
;;   ((PATH . ((HUNK-KEY . (:rank N :tags (TAG...) :criteria (ID...)))
;;             ...))
;;    ...)
;;
;; It reuses the engines (phase 15 delicacy, phase 17 test impact,
;; phase 14 heat cache, phase 23/29 criteria) and never reimplements
;; them.  No network: the worktree is only looked up, never fetched
;; or created.
;;
;; HUNK KEY: the raw `@@ ranges @@' MIDDLE text of the hunk header,
;; e.g. "-1,3 +1,4" (`code-review-analysis--split-hunks' :ranges).  For
;; ordinary `@@' headers it is identical to the washer's hunk `ranges'
;; slot and the phase 13 viewed-table key.  It is NOT a zero-based
;; index.  Combined-diff (`@@@') hunks are omitted, and a file with no
;; parseable hunk (binary, pure rename, mode change) appears as
;; (PATH), i.e. with an empty hunk list.
;;
;; RANK: a dense 1-based ordering of ALL the PR's hunks, riskiest
;; first: hunks with a phase 15 delicacy entry first by score
;; (descending), the rest in diff order; ties keep diff order.  It
;; depends on engine settings (`code-review-analysis-delicacy-report-min',
;; `code-review-analysis-max-hunk-entries') and on which engines ran.
;;
;; TAGS (in this order, each only when it applies):
;;   "CI-GAME"   the hunk's file has a phase 17 CI-gaming finding
;;               (pure diff text: works without a worktree)
;;   "RISK"      delicacy score at or over the delicacy threshold
;;   "INCIDENT"  the delicacy entry counts registered incidents
;;   "NO-TEST"   a changed definition no test references
;;   heat bucket (e.g. "HOT"/"WARM"/"COLD") from the phase 14 cache,
;;               ONLY once a render has harvested history for this
;;               PR and diff (never harvests here)
;;
;; CRITERIA: ids of criteria covering the hunk's path, or its changed
;; definitions (covers-fn), in id order.
;;
;; Without a worktree only CI-GAME (and ranks by diff order) are
;; available; `code-review-api-analysis-available-p' tells a caller
;; whether the worktree engines can run, so "engines did not run" is
;; distinguishable from "no findings".

;;; Code:

(require 'seq)
(require 'map)
(require 'subr-x)
(require 'closql)
(require 'emacsql)
(require 'code-review-db)
(require 'code-review-diff)
(require 'code-review-analysis)
(require 'code-review-testimpact)
(require 'code-review-criteria)
(require 'code-review-repo)

(defvar code-review-history--order-table)
(defvar code-review-github-base-url)
(defvar code-review-gitlab-base-url)

(defun code-review-api-analysis--find-pr (repo number)
  "The newest stored PR row for REPO (\"owner/name\") and NUMBER, or nil.
Unlike `code-review-db-search' the row need not be saved."
  (let* ((parts (split-string repo "/" t))
         (owner (car parts))
         (name (string-join (cdr parts) "/"))
         (db (code-review-db)))
    (when (and owner (not (string-empty-p name)))
      (seq-some
       (lambda (candidate)
         (when-let* ((rows (emacsql db [:select [id] :from pullreq
                                        :where (and (= owner $s1)
                                                    (= repo $s2)
                                                    (= number $s3))
                                        :order-by [(desc rowid)]]
                                    owner candidate (format "%s" number))))
           (closql-get db (caar rows) 'code-review-db-pullreq)))
       (delete-dups (list name (replace-regexp-in-string "/" "%2F" name)))))))

(defun code-review-api-analysis--worktree (pr)
  "An EXISTING review worktree of PR, or nil.  Lookup only, no network."
  (when-let* ((forge (code-review-repo--forge pr))
              (host (if (eq forge 'github)
                        code-review-github-base-url
                      code-review-gitlab-base-url))
              (dir (code-review-repo--find-existing
                    host (oref pr owner)
                    (replace-regexp-in-string "%2F" "/" (oref pr repo))))
              (wt (code-review-repo--worktree-dir dir (oref pr number))))
    (when (file-exists-p (expand-file-name ".git" wt))
      wt)))

;;;###autoload
(defun code-review-api-analysis-available-p (repo number)
  "Non-nil when the worktree engines can run for PR NUMBER of REPO.
That is: the PR and its diff are stored and an existing review
worktree is found.  When nil, `code-review-api-analysis' still
returns keys, diff-order ranks and CI-GAME tags, but no NO-TEST,
RISK, INCIDENT or criteria data (\"engines did not run\", not \"no
findings\").  Cheap: no engine runs, no network."
  (when-let* ((pr (code-review-api-analysis--find-pr repo number))
              ((stringp (oref pr raw-diff))))
    (and (code-review-api-analysis--worktree pr) t)))

;;;###autoload
(defun code-review-api-analysis (repo number)
  "Analysis of PR NUMBER in REPO (\"owner/name\") as a nested alist.
Return ((PATH . ((HUNK-KEY . (:rank N :tags TAGS :criteria IDS)) ...))
...), in diff order; nil when the PR or its diff is not stored.  See
the Commentary of this file for the hunk key, rank, tag and criteria
conventions.

SYNCHRONOUS and potentially SLOW: with a worktree it runs the
analysis and test-impact engines, which do worktree-wide `git grep'
and `git blame' (seconds on big repositories), and it WRITES the
shared per-(PR, diff) analysis and test-impact caches (including a
cached nil when an engine fails; the engines log failures).  Never
call it on the UI thread or in a live daemon without expecting a
stall.  The worktree is the one found by lookup only (no explicit
override, so the shared caches cannot be poisoned by a different
checkout).  Results can differ across calls: ranks depend on
analysis settings, and the heat tag appears only after a render has
harvested history.  See `code-review-api-analysis-available-p'."
  (when-let* ((pr (code-review-api-analysis--find-pr repo number))
              (diff (oref pr raw-diff))
              ((stringp diff)))
    (let* ((wt (code-review-api-analysis--worktree pr))
           (blocks (code-review--diff--split-by-files diff))
           (analysis (and wt
                          (with-temp-buffer
                            (setq-local code-review-repo-worktree wt)
                            (let ((code-review-db--pullreq-id (oref pr id)))
                              (list (code-review-analysis-run)
                                    (code-review-testimpact-run))))))
           ;; the CI scan is pure diff text: available without a worktree
           (tres (or (cadr analysis)
                     (list :ci (code-review-testimpact--scan-ci blocks))))
           (delicate (map-into
                      (seq-map (lambda (e)
                                 (cons (cons (plist-get e :path)
                                             (plist-get e :ranges))
                                       e))
                               (plist-get (car analysis) :hunks))
                      '(hash-table :test equal)))
           (heat (when-let* ((id (oref pr id))
                             ((boundp 'code-review-history--order-table))
                             (entries (map-elt code-review-history--order-table
                                               (concat id "|" (md5 diff)))))
                   (seq-map (lambda (e)
                              (cons (plist-get e :path) (plist-get e :bucket)))
                            entries)))
           (reqs (and wt (code-review-criteria--criteria wt)))
           (per-file (seq-map (lambda (blk)
                                (cons (car blk)
                                      (code-review-analysis--split-hunks
                                       (cdr blk))))
                              blocks))
           ;; one entry per hunk, flat in diff order: (:key :tags :criteria :score)
           (entries
            (vconcat
             (seq-mapcat
             (lambda (f)
               (seq-map
                (lambda (hunk)
                  (let* ((path (car f))
                         (ranges (plist-get hunk :ranges))
                         (d (map-elt delicate (cons path ranges)))
                         (score (plist-get d :score))
                         (bucket (map-elt heat path)))
                    (list :key ranges
                          :score score
                          :tags
                          (delq nil
                                (list (code-review-testimpact--file-tag tres path)
                                      (and score
                                           (>= score code-review-analysis-delicacy-threshold)
                                           "RISK")
                                      (and (> (or (plist-get d :incidents) 0) 0)
                                           "INCIDENT")
                                      (and (code-review-testimpact--hunk-tag
                                            tres path ranges)
                                           "NO-TEST")
                                      (and (stringp bucket) bucket)))
                          :criteria
                          (seq-map
                           (lambda (m) (plist-get (plist-get m :req) :id))
                           (code-review-criteria--matching
                            reqs (list path)
                            (seq-map #'car (code-review-analysis--definitions-in
                                            path (plist-get hunk :added))))))))
                (cdr f)))
             per-file)))
           ;; dense ranks by flat index (stable sort: ties keep diff order)
           (ranks (make-vector (length entries) 0))
           (offset 0))
      (seq-do-indexed
       (lambda (idx pos) (aset ranks idx (1+ pos)))
       (sort (number-sequence 0 (1- (length entries)))
             (lambda (a b)
               (> (or (plist-get (aref entries a) :score) -1.0)
                  (or (plist-get (aref entries b) :score) -1.0)))))
      (seq-map
       (lambda (f)
         (let ((base offset))
           (setq offset (+ offset (length (cdr f))))
           (cons (car f)
                 (seq-map-indexed
                  (lambda (_hunk i)
                    (let ((e (aref entries (+ base i))))
                      (cons (plist-get e :key)
                            (list :rank (aref ranks (+ base i))
                                  :tags (plist-get e :tags)
                                  :criteria (plist-get e :criteria)))))
                  (cdr f)))))
       per-file))))

(provide 'code-review-api-analysis)
;;; code-review-api-analysis.el ends here
