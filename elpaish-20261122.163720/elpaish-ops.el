;;; elpaish-ops.el --- Status, rebuild and upgrade operations for ELPAish -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: tools, package, elpa

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Programmatic, structured operations over the ELPAish registry and the
;; locally installed packages.  Each returns a plist whose collections are
;; vectors, so results serialize directly with `json-serialize'; they back
;; the MCP tools, which `builder' hosts in `builder-elpaish'.
;;
;; - `elpaish-ops-status': per-recipe commit delta against the last build.
;; - `elpaish-ops-rebuild': rebuild one package, all of them, or only those
;;   behind their git HEAD.
;; - `elpaish-ops-upgrade': upgrade installed packages, one, several or all.
;; - `elpaish-ops-workflow-run': dispatch a GitHub Actions workflow.
;; - `elpaish-ops-recipe-snippet': format a registration form.
;;
;; Status is designed to be cheap: it never touches the network (a recipe
;; with no local checkout is reported as uncloned rather than cloned), reads
;; HEAD from the repository files instead of spawning git, and runs the
;; remaining per-recipe `git rev-list' counts concurrently.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'package)
(require 'seq)
(require 'subr-x)
(require 'elpaish)

(declare-function elpaish-install--process-package "elpaish-install")
(declare-function elpaish-install--package-up-to-date-p "elpaish-install")
(declare-function elpaish-install--do-packages-async "elpaish-install")
(declare-function elpaish-load-packages "elpaish-recipes")
(declare-function magit-dash-gh-workflow-run "magit-dash-gh-actions")

(defgroup elpaish-ops nil
  "Programmatic ELPAish status, rebuild and upgrade operations."
  :group 'elpaish
  :prefix "elpaish-ops-")

(defcustom elpaish-ops-after-upgrade-hook nil
  "Hook run after packages were installed or upgraded.
Use it to flush a package quickstart file or similar.  When this hook is
nil and `package-quickstart' is non-nil, `package-quickstart-refresh'
runs instead."
  :type 'hook
  :group 'elpaish-ops)

(defcustom elpaish-ops-github-owner nil
  "GitHub owner used by `elpaish-ops-recipe-snippet' for default URLs.
Nil means the owner part of `elpaish-github-repo-slug'."
  :type '(choice (const :tag "Owner of elpaish-github-repo-slug" nil) string)
  :group 'elpaish-ops)

(defcustom elpaish-ops-workflow "pages.yml"
  "Workflow `elpaish-ops-workflow-run' dispatches by default."
  :type 'string
  :group 'elpaish-ops)

(defun elpaish-ops--symbol (x)
  "Return X as a symbol."
  (if (stringp x) (intern x) x))

(defun elpaish-ops--name (x)
  "Return X as a string."
  (if (symbolp x) (symbol-name x) x))

;;;; Status

(defun elpaish-ops--local-repo (recipe)
  "Return RECIPE's existing local checkout, or nil.  Never uses the network.
`elpaish--resolve-repo-path' clones or fetches remote recipes, which is
wrong for a status query."
  (or (map-elt elpaish--resolved-repo-path-cache recipe)
      (let ((target (elpaish-recipe-repository-path recipe)))
        (if (and (stringp target)
                 (not (elpaish--remote-url-p target))
                 (file-directory-p (expand-file-name target)))
            (expand-file-name target)
          (let ((clone (expand-file-name (elpaish-recipe-name recipe) elpaish-work-dir)))
            (and (file-exists-p (expand-file-name ".git" clone)) clone))))))

(defun elpaish-ops--git-dir (repo)
  "Return the git directory of REPO (following a worktree's .git file), or nil."
  (let ((dot-git (expand-file-name ".git" repo)))
    (cond ((file-directory-p dot-git) dot-git)
          ((file-regular-p dot-git)
           (with-temp-buffer
             (insert-file-contents dot-git)
             (when (looking-at "gitdir: \\(.+\\)")
               (expand-file-name (match-string 1) repo)))))))

(defun elpaish-ops--read-line (file)
  "Return the trimmed first line of FILE, or nil if unreadable."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 512)
      (string-trim (buffer-substring (point-min) (line-end-position))))))

(defun elpaish-ops--head-hash (repo)
  "Return the full HEAD hash of REPO without spawning git, or nil."
  (when-let* ((git-dir (elpaish-ops--git-dir repo))
              (head (elpaish-ops--read-line (expand-file-name "HEAD" git-dir))))
    (let ((common (if-let* ((c (elpaish-ops--read-line (expand-file-name "commondir" git-dir))))
                      (expand-file-name c git-dir)
                    git-dir)))
      (cond
       ((string-match-p "\\`[0-9a-f]\\{40,64\\}\\'" head) head)
       ((string-match "\\`ref: \\(.+\\)\\'" head)
        (let ((ref (match-string 1 head)))
          (or (elpaish-ops--read-line (expand-file-name ref git-dir))
              (elpaish-ops--read-line (expand-file-name ref common))
              (when (file-readable-p (expand-file-name "packed-refs" common))
                (with-temp-buffer
                  (insert-file-contents (expand-file-name "packed-refs" common))
                  (when (re-search-forward
                         (concat "^\\([0-9a-f]+\\) " (regexp-quote ref) "$") nil t)
                    (match-string 1)))))))))))

(defun elpaish-ops--current-hash (repo source-dir-rel)
  "Return REPO's current hash, scoped to SOURCE-DIR-REL when non-nil."
  (or (and (null source-dir-rel) (elpaish-ops--head-hash repo))
      (elpaish--current-hash repo source-dir-rel)))

(defun elpaish-ops--git-parallel (jobs)
  "Run git JOBS concurrently and return an alist of (KEY . OUTPUT).
Each job is (KEY DIRECTORY ARG...).  OUTPUT is the trimmed stdout, or nil
when git failed.  At most `elpaish-parallel-jobs' run at once."
  (let ((pending jobs)
        (running 0)
        (limit (max 1 (bound-and-true-p elpaish-parallel-jobs)))
        (deadline (+ (float-time) 60))
        results)
    (cl-labels
        ((start ()
           (while (and pending (< running limit))
             (pcase-let* ((`(,key ,dir . ,args) (pop pending))
                          (buf (generate-new-buffer " *elpaish-git*"))
                          (default-directory (file-name-as-directory dir)))
               (cl-incf running)
               (make-process
                :name "elpaish-git" :buffer buf :noquery t :connection-type 'pipe
                :command (cons "git" args)
                :sentinel (lambda (proc _event)
                            (when (memq (process-status proc) '(exit signal))
                              (push (cons key (and (zerop (process-exit-status proc))
                                                   (string-trim
                                                    (with-current-buffer buf (buffer-string)))))
                                    results)
                              (kill-buffer buf)
                              (cl-decf running)
                              (start))))))))
      (start)
      (while (and (or pending (> running 0)) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    results))

(defun elpaish-ops-status-entries (&optional resolver)
  "Return one status entry per registered recipe, sorted by name.
Each entry is (RECIPE REPO CURRENT-HASH DELTA) where REPO is nil when the
recipe has no checkout and DELTA is \"New\", a commit count string, or
\"Uncloned\".  RESOLVER is a function from a recipe to its repository
directory; it defaults to a local-only lookup that never uses the network,
`elpaish--resolve-repo-path' clones and fetches."
  (let* ((resolver (or resolver #'elpaish-ops--local-repo))
         (recipes (seq-sort-by #'elpaish-recipe-name #'string<
                               (hash-table-values elpaish-registry)))
         (entries (mapcar
                   (lambda (recipe)
                     (let* ((repo (let ((dir (funcall resolver recipe)))
                                    (and dir (file-exists-p dir) dir)))
                            (rel (and repo (elpaish--recipe-source-dir-relative recipe))))
                       (list recipe repo
                             (and repo (elpaish-ops--current-hash repo rel))
                             (cond ((null repo) "Uncloned")
                                   ((seq-contains-p '(nil "" "uncommitted")
                                                    (elpaish-recipe-built-hash recipe))
                                    "New"))
                             rel)))
                   recipes))
         (counts (elpaish-ops--git-parallel
                  (delq nil
                        (mapcar
                         (lambda (entry)
                           (pcase-let ((`(,recipe ,repo ,_hash ,delta ,rel) entry))
                             (unless delta
                               `(,(elpaish-recipe-name recipe) ,repo "rev-list" "--count"
                                 ,(concat (elpaish-recipe-built-hash recipe) "..HEAD")
                                 ,@(and rel (list "--" rel))))))
                         entries)))))
    (mapcar (lambda (entry)
              (pcase-let ((`(,recipe ,repo ,hash ,delta ,_rel) entry))
                (list recipe repo hash
                      (or delta
                          (alist-get (elpaish-recipe-name recipe) counts nil nil #'equal)
                          "0"))))
            entries)))

(defun elpaish-ops--behind-p (delta)
  "Return non-nil if DELTA (see `elpaish-ops-status-entries') means behind HEAD."
  (or (equal delta "New") (> (string-to-number delta) 0)))

(defun elpaish-ops--record (entry)
  "Return the status plist for ENTRY from `elpaish-ops-status-entries'."
  (pcase-let ((`(,recipe ,repo ,hash ,delta) entry))
    (list :package (elpaish-recipe-name recipe)
          :repository (if repo (abbreviate-file-name repo) "")
          :exists (if repo t :json-false)
          :current_hash (if hash (substring hash 0 (min 7 (length hash))) "N/A")
          :built_hash (or (elpaish-recipe-built-hash recipe) "")
          :delta (format "+%s" delta)
          :behind (if (elpaish-ops--behind-p delta) t :json-false)
          :snapshot_version (or (elpaish-recipe-built-version-snapshot recipe) "—")
          :stable_version (or (elpaish-recipe-built-version-stable recipe) "—")
          :staging_version (or (elpaish-recipe-built-version-staging recipe) "—"))))

(defun elpaish-ops--ensure-recipes ()
  "Load the registered recipes if none are loaded yet."
  (when (zerop (hash-table-count elpaish-registry))
    (require 'elpaish-recipes)
    (elpaish-load-packages)))

;;;###autoload
(cl-defun elpaish-ops-status (&key packages behind-only)
  "Return structured status for the registered ELPAish recipes.
PACKAGES limits the report to those names; BEHIND-ONLY keeps only recipes
whose git HEAD is ahead of their last build.  Returns a plist:
:total_packages, :behind_count (both over the selection before
BEHIND-ONLY), :packages (vector of plists with :package :repository
:exists :current_hash :built_hash :delta :behind and the built versions)
and :elapsed_ms.  Never touches the network."
  (elpaish-ops--ensure-recipes)
  (let* ((start (float-time))
         (names (mapcar #'elpaish-ops--name packages))
         (entries (seq-filter (lambda (entry)
                                (or (null names)
                                    (member (elpaish-recipe-name (car entry)) names)))
                              (elpaish-ops-status-entries)))
         (behind (seq-filter (lambda (entry) (elpaish-ops--behind-p (nth 3 entry))) entries)))
    (list :total_packages (length entries)
          :behind_count (length behind)
          :packages (vconcat (mapcar #'elpaish-ops--record (if behind-only behind entries)))
          :elapsed_ms (round (* 1000 (- (float-time) start))))))

;;;; Rebuild

(defun elpaish-ops--condition-case-error (name thunk)
  "Call THUNK; return nil on success or a plist describing its error for NAME."
  (condition-case err
      (progn (funcall thunk) nil)
    (error (list :package name :error (error-message-string err)))))

;;;###autoload
(cl-defun elpaish-ops-rebuild (&key package (stream 'snapshot) behind-only)
  "Rebuild ELPAish package archives.
PACKAGE (string or symbol) rebuilds one recipe with `elpaish-build-single'.
BEHIND-ONLY rebuilds every recipe whose git HEAD is ahead of its archive,
found with the same status pass as `elpaish-ops-status', and refreshes the
indexes once.  Otherwise everything is rebuilt with `elpaish-build-all'.
STREAM is `snapshot', `stable', `staging' or `all'.  Returns a plist
with :status (\"ok\", \"partial_error\" or \"error\"), :stream, :rebuilt
\(vector of names), :errors (vector of plists) and :count."
  (elpaish-ops--ensure-recipes)
  (let* ((stream-sym (elpaish-ops--symbol (or stream 'snapshot)))
         (name (and package (not (equal (elpaish-ops--name package) "all"))
                    (elpaish-ops--name package)))
         rebuilt errors)
    (cond
     (name
      (let ((recipe (or (gethash name elpaish-registry)
                        (user-error "ELPAish recipe `%s' not found" name))))
        (if-let* ((err (elpaish-ops--condition-case-error
                        name (lambda () (elpaish-build-single recipe stream-sym)))))
            (push err errors)
          (push name rebuilt))))
     (behind-only
      (dolist (entry (seq-filter (lambda (e) (elpaish-ops--behind-p (nth 3 e)))
                                 (elpaish-ops-status-entries)))
        (let* ((recipe (car entry))
               (recipe-name (elpaish-recipe-name recipe)))
          (if-let* ((err (elpaish-ops--condition-case-error
                          recipe-name (lambda () (elpaish-build-package recipe stream-sym)))))
              (push err errors)
            (push recipe-name rebuilt))))
      (when rebuilt
        (elpaish-generate-archive-contents stream-sym)
        (when (fboundp 'elpaish-generate-stream-index)
          (elpaish-generate-stream-index stream-sym))))
     (t
      (if-let* ((err (elpaish-ops--condition-case-error
                      "all" (lambda () (elpaish-build-all stream-sym)))))
          (push err errors)
        (setq rebuilt (nreverse (sort (hash-table-keys elpaish-registry) #'string<))))))
    (let ((rebuilt (nreverse rebuilt))
          (errors (nreverse errors)))
      (list :status (cond ((and errors (null rebuilt)) "error")
                          (errors "partial_error")
                          (t "ok"))
            :stream (symbol-name stream-sym)
            :rebuilt (vconcat rebuilt)
            :errors (vconcat errors)
            :count (length rebuilt)))))

;;;; Upgrade

(defun elpaish-ops--installed-version (pkg)
  "Return the installed version string of PKG, or nil."
  (when-let* ((desc (cadr (assq pkg package-alist))))
    (package-version-join (package-desc-version desc))))

(defun elpaish-ops--ensure-archives (refresh)
  "Initialize package.el and refresh archives once when REFRESH or empty."
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  (when (or refresh (null package-archive-contents))
    (package-refresh-contents)))

(defun elpaish-ops--upgrade-one (pkg reinstall)
  "Upgrade or reinstall PKG; return a result plist with :package and :status."
  (let ((old (elpaish-ops--installed-version pkg)))
    (condition-case err
        (if reinstall
            (progn (package-reinstall pkg)
                   (list :package (symbol-name pkg) :status "reinstalled"
                         :from old :to (elpaish-ops--installed-version pkg)))
          (let ((res (elpaish-install--process-package pkg 'upgrade nil)))
            (list :package (symbol-name pkg)
                  :status (pcase (plist-get res :status)
                            (:upgraded "upgraded")
                            (:installed "installed")
                            (:up-to-date "up-to-date")
                            (_ "error"))
                  :from (plist-get res :from) :to (plist-get res :to)
                  :error (plist-get res :error))))
      (error (list :package (symbol-name pkg) :status "error"
                   :error (error-message-string err))))))

(defun elpaish-ops--upgrade-candidates ()
  "Return installed packages that have a newer version in the archives."
  (require 'elpaish-install)
  (seq-remove (lambda (pkg) (elpaish-install--package-up-to-date-p pkg 'upgrade))
              (mapcar #'car package-alist)))

;;;###autoload
(cl-defun elpaish-ops-upgrade (&key packages all refresh reinstall async pool-size)
  "Upgrade installed PACKAGES (names), or every upgradable package with ALL.
Archives are refreshed at most once (when REFRESH is non-nil or none are
loaded), and only packages with a newer archive version are touched.
REINSTALL forces a reinstall of the named packages.  With ASYNC, hand the
work to a background pool of POOL-SIZE workers and return immediately.
Returns a plist: :status (\"ok\", \"partial_error\" or \"started\"),
:upgraded (vector of plists with :package :from :to), :up_to_date (count),
:failed (vector of plists with :package :error) and :elapsed_ms."
  (require 'elpaish-install)
  (let* ((start (float-time))
         (names (mapcar #'elpaish-ops--symbol packages))
         (targets (progn (elpaish-ops--ensure-archives refresh)
                         (if (and names (not all))
                             names
                           (elpaish-ops--upgrade-candidates)))))
    (if async
        (progn (elpaish-install--do-packages-async
                targets :action 'upgrade :refresh nil :pool-size (or pool-size 4))
               (list :status "started" :packages (vconcat (mapcar #'symbol-name targets))))
      (let (upgraded failed (current 0))
        (dolist (pkg targets)
          (let ((res (elpaish-ops--upgrade-one pkg reinstall)))
            (pcase (plist-get res :status)
              ((or "upgraded" "installed" "reinstalled") (push res upgraded))
              ("up-to-date" (cl-incf current))
              (_ (push res failed)))))
        (when upgraded
          (if elpaish-ops-after-upgrade-hook
              (run-hooks 'elpaish-ops-after-upgrade-hook)
            (when (and (bound-and-true-p package-quickstart)
                       (fboundp 'package-quickstart-refresh))
              (package-quickstart-refresh))))
        (list :status (if failed "partial_error" "ok")
              :upgraded (vconcat (nreverse upgraded))
              :up_to_date current
              :failed (vconcat (nreverse failed))
              :elapsed_ms (round (* 1000 (- (float-time) start))))))))

;;;; Workflows

(defun elpaish-ops--magit-dash-workflow-p ()
  "Return non-nil if magit-dash can dispatch workflows."
  (and (require 'magit-dash-gh-actions nil t)
       (fboundp 'magit-dash-gh-workflow-run)))

(defun elpaish-ops--gh (&rest args)
  "Run the gh CLI with ARGS; return (EXIT-CODE . OUTPUT), or nil if absent."
  (when (executable-find "gh")
    (with-temp-buffer
      (let ((exit (apply #'call-process "gh" nil t nil args)))
        (cons exit (string-trim (buffer-string)))))))

;;;###autoload
(defun elpaish-ops-workflow-run (&optional workflow repo ref)
  "Dispatch GitHub Actions WORKFLOW on REPO at REF.
WORKFLOW defaults to `elpaish-ops-workflow', REPO to
`elpaish-github-repo-slug' and REF to \"main\".  Uses magit-dash when
available and the gh CLI otherwise.  Returns a plist with :status,
:method, :workflow, :repo and :ref (or :error)."
  (let ((workflow (or workflow elpaish-ops-workflow))
        (repo (or repo elpaish-github-repo-slug))
        (ref (or ref "main")))
    (if (elpaish-ops--magit-dash-workflow-p)
        (progn
          (magit-dash-gh-workflow-run workflow repo ref nil nil)
          (list :status "ok" :method "magit-dash" :workflow workflow :repo repo :ref ref))
      (if-let* ((result (elpaish-ops--gh "workflow" "run" workflow "--ref" ref "-R" repo)))
          (if (zerop (car result))
              (list :status "ok" :method "gh-cli" :workflow workflow :repo repo :ref ref)
            (list :status "error" :method "gh-cli" :exit_code (car result) :error (cdr result)))
        (list :status "error" :error "Neither magit-dash nor the gh CLI is available")))))

;;;; Recipe snippets

;;;###autoload
(cl-defun elpaish-ops-recipe-snippet (package &key summary requires keywords url owner)
  "Return the `elpaish-register-package' form registering PACKAGE.
SUMMARY, REQUIRES (a list of package symbols) and KEYWORDS fill the
recipe.  The repository is OWNER/PACKAGE on GitHub, or URL when given;
OWNER defaults to `elpaish-ops-github-owner' or the owner of
`elpaish-github-repo-slug'."
  (let* ((name (elpaish-ops--name package))
         (owner (or owner elpaish-ops-github-owner
                    (car (split-string elpaish-github-repo-slug "/"))))
         (home (or url (format "https://github.com/%s/%s" owner name))))
    (format "(elpaish-register-package\n '%s\n (elpaish-recipe-path \"%s.git\")\n :branch \"main\"\n :test-dir \"test\"\n :requires '%S\n :summary \"%s\"\n :url \"%s\"\n :keywords '%S)\n"
            name home requires (or summary (format "%s package" name)) home
            (or keywords '("tools" "convenience")))))

(provide 'elpaish-ops)

;; Local Variables:
;; package-lint-main-file: "elpaish.el"
;; End:
;;; elpaish-ops.el ends here
