;;; elpaish.el --- ELPA package repository builder and CI automation toolkit -*- lexical-binding: t; -*-

;; Author: tychoish
;; Version: 0.1.0
;; URL: https://github.com/tychoish/elpaish
;; Keywords: maint, tools, local, package, elpa
;; Package-Requires: ((emacs "28.1") (annotated-completing-read "0.1.0") (htmlize "1.34") (map "3.0") (modus-themes "4.0.0") (seq "2.0") (web-server "0.1.2") (transient "0.3.0"))

;;; Commentary:
;; ELPAish is a toolkit for building (and a prototype application of) an
;; Emacs Lisp package archive/repository hosted on GitHub Pages or web servers.
;;
;; Release Streams:
;; - `elpaish`: Primary snapshot archive tracking the TIP of the default branch
;;   with pure date-based version strings (YYYYMMDD.HHMMSS).
;; - `elpaish-stable`: Official releases built strictly from clean semver Git tags
;;   (vX.Y.Z -> X.Y.Z). Repositories lacking clean tags are omitted entirely.
;; - `elpaish-staging`: Pre-release builds and release candidates derived from
;;   non-stable Git tags (e.g. -rc, -pre, -beta) and `git describe` versions.
;;
;; Features:
;; - Pure Emacs Lisp orchestration without external build tool dependencies.
;; - In-memory version header injection and dynamic <pkg>-pkg.el tarball generation
;;   without mutating or dirtying upstream Git trees.
;; - Subkey GPG signing pipeline supporting headless CI with loopback pinentry.
;; - Full GPG key lifecycle tooling, automated secret synchronization via GitHub CLI
;;   (`gh secret set`), and emergency revocation publishing.
;; - Preflight package validation gates (check-parens, checkdoc, package-lint,
;;   isolated byte-compilation, ERT tests) with automatic package quarantine.
;; - Built-in local HTTP preview server for testing in isolated `emacs -Q` sessions.
;; - Interactive `tabulated-list-mode` status UI (`*elpaish-status*`).

;;; Code:

(require 'cl-lib)
(require 'epg)
(require 'map)
(require 'package)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'timer)
(require 'compile)
(require 'web-server)

(require 'annotated-completing-read)
(require 'htmlize)
(require 'transient)

(require 'elpaish-check)
(require 'elpaish-install)
(require 'elpaish-keyring)


(defgroup elpaish nil
  "ELPA package repository builder."
  :group 'development)

(defcustom elpaish-base-url "https://tychoish.github.io/elpaish"
  "Base URL for the ELPAish package repository site."
  :type 'string
  :group 'elpaish)
(defcustom elpaish-output-dir (expand-file-name "public/" default-directory)
  "Root directory where package archives, keys, and indexes are written."
  :type 'directory
  :group 'elpaish)

(defcustom elpaish-work-dir (expand-file-name "repos/" default-directory)
  "Directory where remote Git repositories are cloned."
  :type 'directory
  :group 'elpaish)

(defcustom elpaish-sign-packages nil
  "When non-nil, sign generated package archives and `archive-contents' with GPG."
  :type 'boolean
  :group 'elpaish)

(defcustom elpaish-gpg-key nil
  "GPG key ID, fingerprint, or email used to sign packages.
If nil, checks `ELPAISH_KEY_ID' or `ELPAISH_GPG_KEY' environment variables,
falling back to the first available secret key in the GPG keyring."
  :type '(choice (const :tag "Default / Environment Key" nil)
                 (string :tag "Key ID or Fingerprint"))
  :group 'elpaish)

(defcustom elpaish-gpg-passphrase nil
  "Optional passphrase for GPG signing key (or from `ELPAISH_GPG_PASSPHRASE')."
  :type '(choice (const :tag "None / GPG Agent" nil)
                 (string :tag "Passphrase"))
  :group 'elpaish)

(defcustom elpaish-release-mode 'all
  "Default release mode / track for building packages.
Can be `all' (builds elpaish, elpaish-stable, and elpaish-staging),
`elpaish' (snapshot date versions), `elpaish-stable' (semver tags only),
or `elpaish-staging' (pre-release tags and git describe)."
  :type '(choice (const :tag "All Tracks (elpaish, stable, staging)" all)
                 (const :tag "elpaish (Snapshot date versions)" elpaish)
                 (const :tag "elpaish-stable (Clean semver tags only)" elpaish-stable)
                 (const :tag "elpaish-staging (Pre-release & describe)" elpaish-staging))
  :group 'elpaish)

(defcustom elpaish-run-preflight t
  "When non-nil, execute preflight quality gates before building packages."
  :type 'boolean
  :group 'elpaish)

(defcustom elpaish-default-branch "main"
  "Default Git branch to track for recipes that do not specify one."
  :type 'string
  :group 'elpaish)

(defcustom elpaish-github-repo-slug "tychoish/elpaish"
  "Default \"owner/repo\" slug used by `elpaish-rotate-keys' for `gh secret set'."
  :type 'string
  :group 'elpaish)

(defconst elpaish-streams '(snapshot stable staging)
  "List of supported package archive release streams.")

;;; Registry Data Structure

(cl-defstruct (elpaish-recipe (:constructor elpaish-recipe-create))
  "Structure representing an ELPA package build recipe."
  (name nil :type string :documentation "Package name.")
  (repository-path nil :type string :documentation "Local directory path or Git URL.")
  (branch "main" :type string :documentation "Git branch to track.")
  (files '("*.el") :type list :documentation "List of file patterns to include.")
  (exclude-files nil :type list :documentation "List of file patterns to exclude.")
  (source-directory-path "." :type string :documentation "Subdirectory within REPOSITORY-PATH holding the package source.")
  (test-directory-path nil :type (choice null string) :documentation "Optional custom test directory path.")
  (preflight-skip nil :type (choice boolean list) :documentation "Checks to skip in preflight.")
  (disabled-streams nil :type list :documentation "List of stream symbols where this package is suppressed/disabled.")
  (external-p nil :type boolean :documentation "Non-nil if package is externally maintained.")
  (summary nil :type (choice null string) :documentation "Package summary description.")
  (url nil :type (choice null string) :documentation "Upstream homepage or repository URL.")
  (doc nil :type (choice null string) :documentation "Documentation URL or path.")
  (keywords nil :type list :documentation "List of keywords.")
  (requires nil :type list :documentation "Declared dependencies ((dep min-ver) ...).")
  (built-version-snapshot nil :type (choice null string) :documentation "Last built version for snapshot stream.")
  (built-version-stable nil :type (choice null string) :documentation "Last built version for stable stream.")
  (built-version-staging nil :type (choice null string) :documentation "Last built version for staging stream.")
  (built-hash nil :type (choice null string) :documentation "Git commit hash when last built.")
  (built-type 'single :type symbol :documentation "Package archive type (\\='single or \\='tar)."))


(defvar elpaish-registry (make-hash-table :test 'equal)
  "Registry storing package recipes keyed by package name string.")

(defvar elpaish-timer nil
  "Timer object for scheduled repository auto-rebuilds.")
(defvar elpaish-server-process nil
  "Server object or process handle for local preview HTTP server.")

(defvar elpaish--quarantined-packages nil
  "Names of packages quarantined by preflight during the current build run.
Dynamically bound by `elpaish-build-all' and `elpaish-build-single' so
callers can detect and fail on preflight quarantines instead of silently
publishing an incomplete archive.")

;; Compatibility accessors for single built-version references
(defun elpaish-recipe-built-version (recipe)
  "Return most recent built version for RECIPE across streams."
  (or (elpaish-recipe-built-version-snapshot recipe)
      (elpaish-recipe-built-version-stable recipe)
      (elpaish-recipe-built-version-staging recipe)))

(gv-define-setter elpaish-recipe-built-version (val recipe)
  `(setf (elpaish-recipe-built-version-snapshot ,recipe) ,val))

;;;###autoload
(cl-defun elpaish-register-package (name
				    repository-path
				    &key branch
				    (files '("*.el"))
                                    exclude-files
                                    exclude
                                    source-directory-path
                                    test-directory-path
                                    source-dir
                                    test-dir
				    preflight-skip
                                    external
                                    external-p
				    summary
				    url
				    doc
                                    keywords
				    requires
				    disabled-streams
                                    suppress-streams
                                    docs-url
                                    &allow-other-keys)
  "Register package NAME with REPOSITORY-PATH (local directory or Git URL).
BRANCH defaults to `elpaish-default-branch' and FILES to \\='(\"*.el\").
SOURCE-DIRECTORY-PATH (or SOURCE-DIR) is the subdirectory within
REPOSITORY-PATH holding the package (default \".\"), for monorepos or
nested folders.
TEST-DIRECTORY-PATH (or TEST-DIR) is an optional custom test directory path.
EXCLUDE-FILES (or EXCLUDE) is an optional list of file patterns to exclude.
PREFLIGHT-SKIP is t (or \\='t) to skip all checks, or a list of check
symbols to bypass during preflight.
DISABLED-STREAMS (or SUPPRESS-STREAMS) is a list of suppressed stream symbols.
DOC (or DOCS-URL) is an optional URL or path to documentation.
SUMMARY, URL, KEYWORDS, and REQUIRES provide package metadata."
  (let* ((raw-name (if (symbolp name) (symbol-name name) (string-trim name)))
         (name-str (string-remove-suffix ".el" raw-name))
         (effective-source (or source-directory-path source-dir "."))
         (effective-test (or test-directory-path test-dir))
         (raw-disabled (append (when (listp disabled-streams) disabled-streams)
                               (when (symbolp disabled-streams) (list disabled-streams))
                               (when (listp suppress-streams) suppress-streams)
                               (when (symbolp suppress-streams) (list suppress-streams))))
         (dis-streams (seq-uniq (mapcar #'elpaish-canonical-stream (delq nil raw-disabled))))
         (repo-target (if (and (stringp repository-path)
                               (not (string-match-p "\\`https?://" repository-path))
                               (not (string-match-p "\\`git@" repository-path)))
                          (expand-file-name repository-path)
                        repository-path))
         (recipe (elpaish-recipe-create
                  :name name-str
                  :repository-path repo-target
                  :branch (or branch elpaish-default-branch)
                  :files (or files '("*.el"))
                  :exclude-files (or exclude-files exclude)
                  :source-directory-path effective-source
                  :test-directory-path effective-test
                  :preflight-skip preflight-skip
                  :external-p (and (or external external-p) t)
                  :disabled-streams dis-streams
                  :summary (or summary "No description")
                  :url url
                  :doc (or doc docs-url)
                  :keywords (or keywords '("tools"))
                  :requires requires
                  :built-version-snapshot nil
                  :built-version-stable nil
                  :built-version-staging nil
                  :built-hash nil
                  :built-type 'single)))
    (puthash name-str recipe elpaish-registry)
    recipe))

(defun elpaish-recipe-disabled-for-stream-p (recipe stream)
  "Return non-nil if RECIPE is disabled/suppressed on STREAM."
  (let ((canon (elpaish-canonical-stream stream))
        (dis (elpaish-recipe-disabled-streams recipe)))
    (and dis (memq canon (mapcar #'elpaish-canonical-stream dis)))))

(defun elpaish-clear-registry ()
  "Clear all registered recipes from the registry."
  (interactive)
  (clrhash elpaish-registry))

;;; Stream & Directory Resolution

(defun elpaish-canonical-stream (stream)
  "Return canonical stream symbol for STREAM.
STREAM can be \\='snapshot, \\='stable, \\='staging, or \\='all."
  (pcase stream
    ((or 'snapshot 'elpaish 'unstable) 'snapshot)
    ((or 'stable 'elpaish-stable 'release) 'stable)
    ((or 'staging 'elpaish-staging 'pre 'beta 'rc) 'staging)
    ('all 'all)
    (_ 'snapshot)))

(defun elpaish-stream-dir (stream &optional root-directory-path)
  "Return destination directory for STREAM under ROOT-DIRECTORY-PATH.
ROOT-DIRECTORY-PATH defaults to `elpaish-output-dir'."
  (let ((base (file-name-as-directory (or root-directory-path elpaish-output-dir)))
        (canon (elpaish-canonical-stream stream)))
    (if (eq canon 'all)
        base
      (expand-file-name (symbol-name canon) base))))


(defun elpaish-recipe-version-for-stream (recipe stream)
  "Return stored built version string for RECIPE on STREAM."
  (pcase (elpaish-canonical-stream stream)
    ('snapshot (elpaish-recipe-built-version-snapshot recipe))
    ('stable (elpaish-recipe-built-version-stable recipe))
    ('staging (elpaish-recipe-built-version-staging recipe))
    (_ (elpaish-recipe-built-version recipe))))

(gv-define-setter elpaish-recipe-version-for-stream (val recipe stream)
  `(pcase (elpaish-canonical-stream ,stream)
     ('snapshot (setf (elpaish-recipe-built-version-snapshot ,recipe) ,val))
     ('stable (setf (elpaish-recipe-built-version-stable ,recipe) ,val))
     ('staging (setf (elpaish-recipe-built-version-staging ,recipe) ,val))
     (_ (setf (elpaish-recipe-built-version-snapshot ,recipe) ,val))))


(defvar elpaish--resolved-repo-path-cache (make-hash-table :test 'eq)
  "Cache of RECIPE -> resolved local repo directory for current build run.
Avoids redundant `git fetch'/clone operations when the same recipe's path
is resolved more than once (its own preflight/build, plus as a sibling
`load-path' entry for every other recipe's preflight check).")
(defun elpaish--resolve-repo-path (recipe)
  "Return working directory for RECIPE, cloning or fetching if remote Git URL."
  (or (map-elt elpaish--resolved-repo-path-cache recipe)
      (setf (map-elt elpaish--resolved-repo-path-cache recipe)
            (elpaish--resolve-repo-path-1 recipe))))

(defun elpaish--resolve-repo-path-1 (recipe)
  "Uncached implementation of `elpaish--resolve-repo-path' for RECIPE."
  (let ((repo-target (elpaish-recipe-repository-path recipe)))
    (if (and (stringp repo-target)
             (not (string-match-p "\\`https?://" repo-target))
             (not (string-match-p "\\`git@" repo-target))
             (file-directory-p (expand-file-name repo-target)))
        (expand-file-name repo-target)
      ;; Remote Git repository target
      (let* ((name (elpaish-recipe-name recipe))
             (branch (or (elpaish-recipe-branch recipe) elpaish-default-branch))
             (pkg-dir (expand-file-name name elpaish-work-dir)))
        (make-directory elpaish-work-dir t)
        (if (file-exists-p (expand-file-name ".git" pkg-dir))
            (let ((default-directory pkg-dir))
              (call-process "git" nil nil nil "fetch" "origin")
              (call-process "git" nil nil nil "checkout" branch)
              (call-process "git" nil nil nil "reset" "--hard" (concat "origin/" branch)))
          (call-process "git" nil nil nil "clone" "--branch" branch repo-target pkg-dir))
        pkg-dir))))

(defun elpaish--recipe-source-dir-relative (recipe)
  "Return RECIPE's `:source-dir' relative path, or nil when it is the repo root."
  (let ((source-dir (or (elpaish-recipe-source-directory-path recipe) ".")))
    (unless (string= source-dir ".")
      (string-remove-suffix "/" source-dir))))

(defun elpaish--recipe-source-path (recipe)
  "Return the absolute source directory for RECIPE, honoring `:source-dir'."
  (let ((repo-dir (elpaish--resolve-repo-path recipe))
        (rel (elpaish--recipe-source-dir-relative recipe)))
    (if rel
        (expand-file-name rel repo-dir)
      repo-dir)))

(defun elpaish--git-string (dir &rest args)
  "Execute Git command ARGS in DIR silently into a temporary buffer.
Return trimmed stdout string on exit 0, or nil on failure."
  (let ((default-directory (if (and dir (file-directory-p dir))
                               (file-name-as-directory (expand-file-name dir))
                             default-directory)))
    (with-temp-buffer
      (let ((exit (apply #'call-process "git" nil (list t nil) nil args)))
        (when (and (numberp exit) (zerop exit))
          (string-trim (buffer-string)))))))

(defun elpaish--git-lines (dir &rest args)
  "Execute Git command ARGS in DIR silently.
Return list of non-empty output lines on success, or nil on failure."
  (when-let* ((str (apply #'elpaish--git-string dir args)))
    (split-string str "\n" t)))

(defun elpaish--current-hash (repo-dir &optional source-dir-rel)
  "Get current HEAD hash in REPO-DIR, optionally scoped to SOURCE-DIR-REL."
  (or (if (and source-dir-rel (file-directory-p (expand-file-name ".git" repo-dir)))
          (elpaish--git-string repo-dir "log" "-1" "--format=%H" "--" source-dir-rel)
        (elpaish--git-string repo-dir "rev-parse" "HEAD"))
      "uncommitted"))

(defun elpaish--commit-delta (repo-dir built-hash &optional source-dir-rel)
  "Calculate commit count between BUILT-HASH and HEAD in REPO-DIR.
When SOURCE-DIR-REL is non-nil, scope the count to that subtree."
  (if (seq-contains-p '(nil "" "uncommitted") built-hash)
      "New"
    (or (if source-dir-rel
            (elpaish--git-string repo-dir "rev-list" "--count" (concat built-hash "..HEAD") "--" source-dir-rel)
          (elpaish--git-string repo-dir "rev-list" "--count" (concat built-hash "..HEAD")))
        "0")))

;;; Release Stream Version Derivation Engine

(defun elpaish--get-snapshot-version (repo-dir &optional source-dir-rel)
  "Return pure UTC date version string (YYYYMMDD.HHMMSS) for REPO-DIR.
When SOURCE-DIR-REL is non-nil, scope the Git log query to that subtree so
that unrelated monorepo packages do not bump each other's snapshot version.
Derives deterministic UTC date strings from Git commit timestamps."
  (or (and (file-directory-p (expand-file-name ".git" repo-dir))
           (when-let* ((epoch-str (if source-dir-rel
                                      (elpaish--git-string repo-dir "log" "-1" "--format=%ct" "--" source-dir-rel)
                                    (elpaish--git-string repo-dir "log" "-1" "--format=%ct"))))
             (unless (string-empty-p (string-trim epoch-str))
               (format-time-string "%Y%m%d.%H%M%S" (seconds-to-time (string-to-number epoch-str)) t))))
      (format-time-string "%Y%m%d.%H%M%S" nil t)))
(defun elpaish--stable-tag-p (tag)
  "Return non-nil if TAG is a clean semver release tag (excluding pre-releases)."
  (and (stringp tag)
       (string-match-p "\\`v?[0-9]+\\.[0-9]+\\(?:\\.[0-9]+\\)*\\'" tag)
       (not (string-match-p "[-._]\\(?:rc\\|pre\\|beta\\|alpha\\|dev\\|preview\\)" tag))))

(defun elpaish--clean-semver-string (tag)
  "Strip leading \\='v\\=' from TAG."
  (if (string-prefix-p "v" tag)
      (substring tag 1)
    tag))

(defun elpaish--get-stable-version (repo-dir)
  "Return highest clean stable semver tag version in REPO-DIR, or nil if none."
  (when (file-directory-p (expand-file-name ".git" repo-dir))
    (let* ((all-tags (or (elpaish--git-lines repo-dir "tag" "-l" "--sort=-v:refname") nil))
           (stable-tags (seq-filter #'elpaish--stable-tag-p all-tags)))
      (when stable-tags
        (elpaish--clean-semver-string (car stable-tags))))))
(defun elpaish--normalize-staging-version (raw-ver)
  "Parse and normalize raw Git version string RAW-VER to clean semver format."
  (let ((clean (if (string-prefix-p "v" raw-ver) (substring raw-ver 1) raw-ver)))
    ;; Handle git-describe format: 1.2.0-4-gabcdef -> 1.2.0.4
    (if (string-match "\\`\\([0-9]+\\(?:\\.[0-9]+\\)*\\)[-. ]+\\([0-9]+\\)[-. ]+g[0-9a-fA-F]+\\'" clean)
        (format "%s.%s" (match-string 1 clean) (match-string 2 clean))
      ;; Replace hyphens with dots
      (setq clean (replace-regexp-in-string "-+" "." clean))
      ;; Clean up double dots or dotted pre-release: 1.2.0.rc.1 -> 1.2.0.rc1
      (setq clean (replace-regexp-in-string "\\.\\(rc\\|pre\\|beta\\|alpha\\)\\." ".\\1" clean))
      ;; Validate with version-to-list
      (condition-case nil
          (progn
            (ignore (version-to-list clean))
            clean)
        (error
         (let ((nums (seq-filter (lambda (s) (string-match-p "\\`[0-9]+\\'" s))
                                 (split-string clean "[^0-9a-zA-Z]+" t))))
           (if nums
               (string-join nums ".")
             (format-time-string "%Y%m%d.%H%M%S" nil t))))))))

(cl-defun elpaish--get-staging-version (repo-dir)
  "Return pre-release or git-describe version string for REPO-DIR."
  (let ((default-directory repo-dir))
    (unless (file-directory-p (expand-file-name ".git" repo-dir))
      (cl-return-from elpaish--get-staging-version
        (format "0.0.0.%s" (elpaish--get-snapshot-version repo-dir))))
    ;; 1. Check if any pre-release tags exist
    (let* ((all-tags (or (elpaish--git-lines repo-dir "tag" "-l" "--sort=-v:refname") nil))
           (pre-tags (seq-filter (lambda (tg)
                                   (and (string-match-p "\\`v?[0-9]" tg)
                                        (string-match-p "[-._]\\(?:rc\\|pre\\|beta\\|alpha\\)" tg)))
                                 all-tags)))
      (if pre-tags
          (elpaish--normalize-staging-version (car pre-tags))
        ;; 2. Fall back to git describe or commit count
        (let ((desc (or (elpaish--git-string repo-dir "describe" "--tags" "--always" "--long")
                        (elpaish--git-string repo-dir "describe" "--always"))))
          (cond
           ((and desc (string-match "\\`v?\\([0-9]+\\.[0-9]+\\(?:\\.[0-9]+\\)*\\)-\\([0-9]+\\)-g\\([0-9a-fA-F]+\\)\\'" desc))
            (let ((tag-part (match-string 1 desc))
                  (commits-ahead (match-string 2 desc)))
              (if (string= commits-ahead "0")
                  (elpaish--clean-semver-string tag-part)
                (format "%s.%s" tag-part commits-ahead))))
           ((and desc (string-match "\\`v?\\([0-9]+\\.[0-9]+\\(?:\\.[0-9]+\\)*\\)\\'" desc))
            (match-string 1 desc))
           (t
            (let ((count (or (elpaish--git-string repo-dir "rev-list" "--count" "HEAD") "1")))
              (format "0.0.0.%s" count)))))))))

(defun elpaish-derive-version (recipe &optional stream)
  "Derive package version string for RECIPE on STREAM.
STREAM can be \\='snapshot, \\='stable, or \\='staging.  Return
normalized version string, or nil for \\='stable if no tag is present."
  (let* ((repo-dir (elpaish--resolve-repo-path recipe))
         (source-dir-rel (elpaish--recipe-source-dir-relative recipe))
         (canon (elpaish-canonical-stream (or stream 'snapshot)))
         (raw-ver (pcase canon
                    ('snapshot
                     (elpaish--get-snapshot-version repo-dir source-dir-rel))
                    ('stable
                     (elpaish--get-stable-version repo-dir))
                    ('staging
                     (elpaish--get-staging-version repo-dir))
                    (_
                     (elpaish--get-snapshot-version repo-dir)))))
    (when raw-ver
      (condition-case nil
          (package-version-join (version-to-list raw-ver))
        (error raw-ver)))))
;;; In-Memory Version Header Injection & Packaging

(defun elpaish--inject-version-header (version-str)
  "Ensure current buffer has a `;; Version: VERSION-STR' header line."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^;;\\s-*\\(?:Package-\\)?Version:\\s-*.*$" nil t)
        (replace-match (format ";; Version: %s" version-str))
      (goto-char (point-min))
      (if (re-search-forward "^;;\\s-*Author:" nil t)
          (beginning-of-line)
        (forward-line 1))
      (insert (format ";; Version: %s\n" version-str)))))

(defun elpaish--sibling-main-files (recipe)
  "Return other recipes' main-file basenames sharing RECIPE source directory.
Lets a package's default `:files' pattern (e.g. \"*.el\") safely glob its
whole source directory without also sweeping up a sibling package's entry
point when two recipes are registered against the same directory."
  (when recipe
    (let ((own-path (elpaish--recipe-source-path recipe)))
      (delq nil
            (mapcar (lambda (other)
                      (when (and (not (eq other recipe))
                                 (equal (elpaish--recipe-source-path other) own-path))
                        (let ((oname (elpaish-recipe-name other)))
                          (if (string-suffix-p ".el" oname) oname (concat oname ".el")))))
                    (hash-table-values elpaish-registry))))))

(defconst elpaish-bundled-doc-patterns
  '("README" "README.*" "readme" "readme.*"
    "LICENSE" "LICENSE.*" "license" "license.*"
    "COPYING" "COPYING.*" "copying" "copying.*"
    "UNLICENSE" "UNLICENSE.*")
  "Wildcard patterns for documentation and licensing files bundled into packages.")

(defun elpaish--collect-files (repo-dir patterns &optional pkg-name recipe)
  "Collect relative file paths in REPO-DIR matching PATTERNS.
Also includes README and LICENSE files.  Excludes tests and generated
descriptor files.  PKG-NAME overrides base name detection.
When RECIPE is given, also excludes any sibling recipe's main file that
shares RECIPE's source directory, other packages' descriptor files,
and any patterns in RECIPE's `:exclude-files'."
  (let* ((default-directory repo-dir)
         (name-str (and pkg-name (if (symbolp pkg-name) (symbol-name pkg-name) pkg-name)))
         (sibling-mains (elpaish--sibling-main-files recipe))
         (user-patterns (or patterns '("*.el")))
         (exclude-patterns (when recipe (elpaish-recipe-exclude-files recipe)))
         (explicit-files
          (seq-mapcat
           (lambda (pat)
             (if (string-match-p "[*?]" pat)
                 (file-expand-wildcards pat)
               (when (file-exists-p pat) (list pat))))
           user-patterns))
         (doc-files
          (seq-mapcat #'file-expand-wildcards elpaish-bundled-doc-patterns)))
    (thread-last (append explicit-files doc-files)
      (seq-filter #'file-regular-p)
      (seq-remove (lambda (f)
                    (let ((base (file-name-nondirectory f)))
                      (or (string-match-p "\\`\\.#" base)
                          (string-suffix-p ".elc" base)
                          (string-suffix-p "-autoloads.el" base)
                          ;; Exclude own generated pkg.el if not explicitly requested
                          (and name-str (string= base (format "%s-pkg.el" name-str))
                               (not (member base user-patterns)))
                          ;; Exclude any other package's -pkg.el descriptor (unless it is this package's own code file <name>.el)
                          (and (string-suffix-p "-pkg.el" base)
                               (not (and name-str (string= base (format "%s.el" name-str))))
                               (not (and name-str (string= base (format "%s-pkg.el" name-str)))))
                          (string-match-p "\\`tests/" f)
                          (string-prefix-p "test-" base)
                          (string-suffix-p "-test.el" base)
                          (string-suffix-p "-tests.el" base)
                          (member base sibling-mains)
                          ;; Exclude explicit exclude patterns
                          (and exclude-patterns
                               (seq-some (lambda (epat)
                                           (or (string= f epat)
                                               (string= base epat)
                                               (string-match-p (wildcard-to-regexp epat) f)
                                               (string-match-p (wildcard-to-regexp epat) base)))
                                         exclude-patterns))))))
      (seq-uniq))))
(cl-defun elpaish--generate-pkg-file (dest-file name &key version summary reqs url keywords)
  "Write `<pkg>-pkg.el' descriptor for NAME at DEST-FILE.
VERSION, SUMMARY, REQS, URL, and KEYWORDS provide the descriptor metadata."
  (with-temp-file dest-file
    (insert ";; -*- no-byte-compile: t -*-\n")
    (let ((req-forms (mapcar (lambda (r)
                               (let ((dep (car r))
                                     (ver (cadr r)))
                                 (list dep (if (stringp ver) ver (package-version-join ver)))))
                             reqs))
          (extra-kws (append (when url `(:url ,url))
                             (when keywords `(:keywords ,@keywords)))))
      (insert (format "(define-package %S %S %S\n  '%S\n"
                      name version (or summary "No description") req-forms))
      (when extra-kws
        (insert (format "  %s" (mapconcat (lambda (x) (format "%S" x)) extra-kws " "))))
      (insert ")\n"))))

(cl-defun elpaish--create-tar-package (repo-dir dest-file pkg-name-ver name files
                                                 &key version summary reqs url keywords)
  "Create a tar package at DEST-FILE for FILES in REPO-DIR named PKG-NAME-VER.
NAME is the package's base name.  VERSION, SUMMARY, REQS, URL, and KEYWORDS
are forwarded to `elpaish--generate-pkg-file' for the bundled descriptor."
  (let* ((temp-dir (make-temp-file "elpaish-pkg-" t))
         (pkg-subdir (expand-file-name pkg-name-ver temp-dir)))
    (unwind-protect
        (progn
          (make-directory pkg-subdir t)
          (dolist (rel-file files)
            (let ((dst (expand-file-name rel-file pkg-subdir)))
              (make-directory (file-name-directory dst) t)
              (copy-file (expand-file-name rel-file repo-dir) dst t)))
          ;; Generate <pkg>-pkg.el inside tarball root
          (elpaish--generate-pkg-file (expand-file-name (format "%s-pkg.el" name) pkg-subdir) name
                                       :version version :summary summary :reqs reqs
                                       :url url :keywords keywords)
          (let ((default-directory temp-dir))
            (call-process "tar" nil nil nil "-cf" dest-file pkg-name-ver)))
      (delete-directory temp-dir t))))
;;; Dependency Verification

;;;###autoload
(defun elpaish-dependency-valid-p (dep)
  "Return non-nil if package dependency DEP is builtin, in MELPA, or in `elpaish-registry'."
  (let* ((sym (cond ((symbolp dep) dep)
                    ((stringp dep) (intern dep))
                    ((consp dep) (if (symbolp (car dep)) (car dep) (intern (car dep))))
                    (t nil))))
    (when sym
      (or (eq sym 'emacs)
          (package-built-in-p sym)
          (package-installed-p sym)
          (and (fboundp 'locate-library) (locate-library (symbol-name sym)))
          (and (boundp 'elpaish-registry)
               (hash-table-p elpaish-registry)
               (or (gethash (symbol-name sym) elpaish-registry)
                   (gethash (concat (symbol-name sym) ".el") elpaish-registry)))
          (and (bound-and-true-p package-archive-contents)
               (assq sym package-archive-contents))
          ;; Known external/MELPA package dependencies fallback
          (memq sym '(agent-shell alert request transient magit projectile htmlize web-server
                                 modus-themes compat package-lint async dash s f yaml markdown-mode
                                 ht kv llama tempel consult embark marshal))))))

(defun elpaish--recipe-provided-features (recipe)
  "Return list of feature/package symbols provided internally by RECIPE."
  (when (and (fboundp 'elpaish-recipe-p) (elpaish-recipe-p recipe))
    (let ((name (elpaish-recipe-name recipe))
          (files (elpaish-recipe-files recipe))
          (provided nil))
      (push (intern name) provided)
      (dolist (f (or files (list (concat name ".el"))))
        (when (string-suffix-p ".el" f)
          (let ((feat (string-remove-suffix ".el" (file-name-nondirectory f))))
            (push (intern feat) provided))))
      (delete-dups provided))))

;;;###autoload
(defun elpaish-check-recipe-dependencies (recipe)
  "Check that all dependencies of RECIPE are builtin, in MELPA, in ELPAish, or self-provided.
Returns a list of invalid dependency symbols if any are invalid, or nil if all are valid."
  (let* ((recipe-obj (cond ((and (fboundp 'elpaish-recipe-p) (elpaish-recipe-p recipe)) recipe)
                           ((stringp recipe) (and (boundp 'elpaish-registry) (gethash recipe elpaish-registry)))
                           ((symbolp recipe) (and (boundp 'elpaish-registry) (gethash (symbol-name recipe) elpaish-registry)))
                           (t nil)))
         (provided (when recipe-obj (elpaish--recipe-provided-features recipe-obj)))
         (reqs (when recipe-obj
                 (append (when (fboundp 'elpaish-recipe-requires)
                           (elpaish-recipe-requires recipe-obj))
                         (when-let* ((repo-dir (and (fboundp 'elpaish--recipe-source-path)
                                                    (file-directory-p (elpaish--recipe-source-path recipe-obj))
                                                    (elpaish--recipe-source-path recipe-obj)))
                                     (name (elpaish-recipe-name recipe-obj))
                                     (main-file (expand-file-name
                                                 (if (string-suffix-p ".el" name) name (concat name ".el"))
                                                 repo-dir)))
                           (when (and (file-exists-p main-file) (fboundp 'elpaish-install--extract-header-requires))
                             (elpaish-install--extract-header-requires main-file))))))
         (invalid nil))
    (dolist (req reqs)
      (let ((dep-sym (if (consp req) (car req) req)))
        (unless (or (memq dep-sym provided)
                    (elpaish-dependency-valid-p dep-sym))
          (push dep-sym invalid))))
    (nreverse (delete-dups invalid))))
;;;###autoload
(defun elpaish-validate-all-dependencies ()
  "Validate that all dependencies of all registered packages are builtin, in MELPA, or in ELPAish.
Returns an alist of (package . invalid-deps) for any packages with invalid dependencies."
  (interactive)
  (let ((invalid-map nil))
    (dolist (recipe (hash-table-values elpaish-registry))
      (when-let* ((invalid (elpaish-check-recipe-dependencies recipe)))
        (push (cons (elpaish-recipe-name recipe) invalid) invalid-map)))
    (if invalid-map
        (progn
          (message "Invalid dependencies found: %S" invalid-map)
          invalid-map)
      (when (called-interactively-p 'any)
        (message "✓ All registered package dependencies are valid (builtin, MELPA, or ELPAish)."))
      nil)))

(defvar elpaish--preflight-cache (make-hash-table :test 'equal)
  "Cache of recipe name -> preflight result (t or nil) for current build run.
Avoids re-running preflight test suites redundantly across multiple streams.")

(defun elpaish-preflight-package (recipe)
  "Execute preflight quality gates on RECIPE.
Returns t if checks pass, nil if quarantined."
  (if (not elpaish-run-preflight)
      t
    (let* ((name (elpaish-recipe-name recipe))
           (cached (gethash name elpaish--preflight-cache 'not-found)))
      (if (not (eq cached 'not-found))
          cached
        (let ((result (elpaish-preflight-package-1 recipe)))
          (puthash name result elpaish--preflight-cache)
          result)))))

(defun elpaish-preflight-package-1 (recipe)
  "Internal uncached preflight execution on RECIPE."
  (let ((skip (elpaish-recipe-preflight-skip recipe)))
    (if (or (eq skip t) (eq skip 't) (and (listp skip) (memq 'all skip)))
        t
      (if-let* ((invalid-deps (elpaish-check-recipe-dependencies recipe)))
          (progn
            (message "PREFLIGHT QUARANTINE for %s: invalid dependency (%s) — must be builtin, in MELPA, or registered in ELPAish"
                     (elpaish-recipe-name recipe)
                     (mapconcat #'symbol-name invalid-deps ", "))
            nil)
        (elpaish-install-ensure-package-dependencies recipe)
        (unless (featurep 'elpaish-check)
          (require 'elpaish-check nil t))
        (if (fboundp 'elpaish-check-package)
            (let* ((repo-dir (elpaish--recipe-source-path recipe))
                   (name (elpaish-recipe-name recipe))
                   (main-file (if (string-suffix-p ".el" name) name (concat name ".el")))
                   (sibling-dirs (thread-last (hash-table-values elpaish-registry)
                                   (seq-remove (lambda (r) (eq r recipe)))
                                   (seq-map #'elpaish--recipe-source-path)))
                   (tdir (elpaish-recipe-test-directory-path recipe))
                   (res (elpaish-check-package repo-dir
                                               :main-file (and (file-exists-p (expand-file-name main-file repo-dir)) main-file)
                                               :test-dir tdir
                                               :skip-checks skip
                                               :extra-load-path sibling-dirs))
                   (passed (plist-get res :passed))
                   (errs (plist-get res :errors)))
              (unless passed
                (message "PREFLIGHT QUARANTINE for %s: %d error(s)"
                         name (length errs))
                (dolist (e errs)
                  (message "   - %s" e)))
              passed)
          t)))))

;;; GPG Package & Archive Signing Pipeline

(defun elpaish--detect-secret-key-id ()
  "Return the key ID or fingerprint of the first secret signing key in GPG keyring."
  (when (executable-find "gpg")
    (with-temp-buffer
      (when (zerop (call-process "gpg" nil t nil "--list-secret-keys" "--with-colons"))
        (goto-char (point-min))
        (let (fpr)
          (while (and (not fpr) (re-search-forward "^fpr:::::::::+\\([0-9A-Fa-f]+\\):" nil t))
            (setq fpr (match-string 1)))
          fpr)))))

(defun elpaish--get-signing-key ()
  "Resolve active GPG key ID from custom var, environment, or secret keyring."
  (or (and elpaish-gpg-key (not (string-prefix-p "-----BEGIN" elpaish-gpg-key)) elpaish-gpg-key)
      (let ((k (getenv "ELPAISH_KEY_ID")))
        (and k (not (string-prefix-p "-----BEGIN" k)) (not (string-empty-p k)) k))
      (let ((k (getenv "ELPAISH_GPG_KEY")))
        (and k (not (string-prefix-p "-----BEGIN" k)) (not (string-empty-p k)) k))
      (elpaish--detect-secret-key-id)
      nil))

(defun elpaish--get-signing-passphrase ()
  "Resolve GPG passphrase from custom var or environment."
  (or elpaish-gpg-passphrase
      (getenv "ELPAISH_GPG_PASSPHRASE")
      ""))

(defun elpaish--sign-with-gpg-cli (file sig-file key-id passphrase)
  "Sign FILE generating detached signature SIG-FILE using `gpg' CLI in batch mode.
KEY-ID is the signing key.  PASSPHRASE is the optional passphrase."
  (let* ((pass (or passphrase (elpaish--get-signing-passphrase)))
         (args (list "--batch" "--yes" "--detach-sign" "--pinentry-mode" "loopback"))
         (args (if (and key-id (not (string-prefix-p "-----BEGIN" key-id)))
                   (append args (list "--default-key" key-id))
                 args))
         (args (append args (list "--passphrase-fd" "0" "--output" sig-file file))))
    (with-temp-buffer
      (insert pass "\n")
      (apply #'call-process-region (point-min) (point-max) "gpg" t t nil args))))

(defun elpaish--sign-file (file)
  "Generate a detached GPG signature `FILE.sig' for FILE if signing is enabled.
Strictly non-interactive: executes in batch mode via loopback without prompting.
Signals an error if signing is enabled but the signature cannot be produced,
so a misconfigured key or passphrase fails the build instead of silently
publishing an archive without signatures."
  (when elpaish-sign-packages
    (let ((key-id (elpaish--get-signing-key))
          (passphrase (elpaish--get-signing-passphrase))
          (sig-file (concat file ".sig")))
      (when (file-exists-p sig-file)
        (delete-file sig-file))
      (unless (executable-find "gpg")
        (error "elpaish-sign-packages is enabled but `gpg' was not found in PATH"))
      (let ((exit-code (elpaish--sign-with-gpg-cli file sig-file key-id passphrase)))
        (if (and (numberp exit-code) (zerop exit-code) (file-exists-p sig-file))
            (message "Signed %s -> %s"
                     (file-name-nondirectory file)
                     (file-name-nondirectory sig-file))
          (error "Failed to sign %s (gpg exit code %S)"
                 (file-name-nondirectory file) exit-code))))))

;;;###autoload
(cl-defun elpaish-sign-file-headless (file &key key-id passphrase output-file)
  "Sign FILE headlessly generating detached signature for KEY-ID.
Passphrase is provided non-interactively via PASSPHRASE argument,
`elpaish-gpg-passphrase', or `ELPAISH_GPG_PASSPHRASE' environment variable.
Never triggers interactive terminal or GUI pinentry dialogs.
OUTPUT-FILE defaults to FILE.sig.  Return the signature path, or nil."
  (let* ((sig-file (or output-file (concat file ".sig")))
         (resolved-key (or key-id (elpaish--get-signing-key)))
         (resolved-pass (or passphrase (elpaish--get-signing-passphrase))))
    (when (file-exists-p sig-file)
      (delete-file sig-file))
    (if (not (executable-find "gpg"))
        (progn
          (message "GPG binary not found in PATH")
          nil)
      (let ((exit-code (elpaish--sign-with-gpg-cli file sig-file resolved-key resolved-pass)))
        (if (and (numberp exit-code) (zerop exit-code) (file-exists-p sig-file))
            (progn
              (message "Headless signature created: %s" sig-file)
              sig-file)
          (message "Headless signing failed for %s (exit code %S)" file exit-code)
          nil)))))
(defun elpaish--ensure-gpg-agent-loopback ()
  "Ensure gpg-agent is configured to allow loopback pinentry for batch signing."
  (when (executable-find "gpg")
    (let* ((gnupg-dir (expand-file-name ".gnupg" (or (getenv "GNUPGHOME") (getenv "HOME"))))
           (agent-conf (expand-file-name "gpg-agent.conf" gnupg-dir)))
      (make-directory gnupg-dir t)
      (set-file-modes gnupg-dir #o700)
      (unless (and (file-exists-p agent-conf)
                   (with-temp-buffer
                     (insert-file-contents agent-conf)
                     (search-forward "allow-loopback-pinentry" nil t)))
        (with-temp-file agent-conf
          (when (file-exists-p agent-conf)
            (insert-file-contents agent-conf))
          (goto-char (point-max))
          (insert "\nallow-loopback-pinentry\n"))
        (set-file-modes agent-conf #o600)
        (when (executable-find "gpgconf")
          (call-process "gpgconf" nil nil nil "--kill" "gpg-agent"))
        (when (executable-find "gpg-connect-agent")
          (call-process "gpg-connect-agent" nil nil nil "reloadagent" "/bye"))))))

;;;###autoload
(defun elpaish-init-signing-from-env ()
  "Initialize GPG signing from `ELPAISH_SIGNING_KEY' environment variable.
Imports key armor, configures loopback pinentry, detects secret key ID,
and enables signing.  Return the detected signing key ID or nil."
  (let ((key-armor (getenv "ELPAISH_SIGNING_KEY"))
        (passphrase (or (getenv "ELPAISH_GPG_PASSPHRASE") "")))
    (when (and key-armor (not (string-empty-p key-armor)) (executable-find "gpg"))
      (elpaish--ensure-gpg-agent-loopback)
      (with-temp-buffer
        (insert key-armor)
        (call-process-region (point-min) (point-max) "gpg" nil nil nil "--batch" "--import"))
      (setq elpaish-sign-packages t)
      (setq elpaish-gpg-passphrase passphrase)
      (setq elpaish-gpg-key (elpaish--detect-secret-key-id))
      (when elpaish-gpg-key
        (message "[elpaish] GPG signing initialized for key %s" elpaish-gpg-key))
      elpaish-gpg-key)))

(defun elpaish-setup-signing ()
  "Interactive wizard to guide the user through selecting a GPG signing key."
  (interactive)
  (unless (executable-find "gpg")
    (user-error "GPG executable not found in system PATH"))
  (let* ((keys (or (epg-list-keys (epg-make-context 'OpenPGP) "" t)
                   (user-error "No secret GPG keys found.  Please generate a GPG key first using `gpg --full-generate-key'")))
         (table (thread-last keys
                  (seq-map (lambda (k)
                             (cons (epg-sub-key-id (car (epg-key-sub-key-list k)))
                                   (epg-user-id-string (car (epg-key-user-id-list k))))))))
         (key-id (annotated-completing-read table
                                            :prompt "Select GPG key for signing ELPA packages: "
                                            :require-match t)))
    (setq elpaish-gpg-key key-id
          elpaish-sign-packages t)
    (message "GPG package signing enabled! Selected Key ID: %s." key-id)))

;;; Key Lifecycle, Subkey Rotation & Secret Sync

(defun elpaish-export-keyring (&optional output-dir key-id)
  "Export binary `elpaish-keyring.gpg' and armored keys for KEY-ID to OUTPUT-DIR."
  (let* ((target-dir (or output-dir elpaish-output-dir))
         (key (or key-id (elpaish--get-signing-key) ""))
         (gpg-bin (executable-find "gpg"))
         (rev-file (expand-file-name "elpaish.rev.asc" target-dir)))
    (make-directory target-dir t)
    ;; Ensure revocation certificate file exists (empty placeholder if no revocation cert is published yet)
    (unless (file-exists-p rev-file)
      (with-temp-file rev-file
        (insert "")))
    (when gpg-bin
      (let ((binary-ring (expand-file-name "elpaish-keyring.gpg" target-dir))
            (armor-pub (expand-file-name "elpaish.pub.asc" target-dir)))
        (call-process gpg-bin nil nil nil "--batch" "--yes" "--output" binary-ring "--export" key)
        (call-process gpg-bin nil nil nil "--batch" "--yes" "--armor" "--output" armor-pub "--export" key)
        (message "Exported public keyrings to %s and %s" binary-ring armor-pub)))))

;;;###autoload
(cl-defun elpaish-rotate-keys (&key master-key-id repo-slug (output-dir elpaish-output-dir))
  "Rotate GPG signing subkey [S] for MASTER-KEY-ID and export to OUTPUT-DIR.
Syncs with GitHub secrets on REPO-SLUG."
  (unless (executable-find "gpg")
    (user-error "GPG binary not found in PATH"))
  (let* ((master (or master-key-id
                     (if (called-interactively-p 'interactive)
                         (read-string "Primary / Master GPG Key ID or Fingerprint: " (elpaish--get-signing-key))
                       (elpaish--get-signing-key))
                     (user-error "No master key ID specified")))
         (target-repo (or repo-slug elpaish-github-repo-slug))
         (gpg-bin (executable-find "gpg"))
         (gh-bin (executable-find "gh")))

    (message "Generating new 1-year signing subkey for %s..." master)
    (call-process gpg-bin nil nil nil "--batch" "--quick-add-key" master "ed25519" "sign" "1y")
    (elpaish-export-keyring output-dir master)

    (if (not gh-bin)
        (message "GitHub CLI `gh' not found; skipped automated secret sync.")
      (with-temp-buffer
        (let ((export-code (call-process gpg-bin nil t nil "--batch" "--armor" "--export-secret-subkeys" master)))
          (if (not (zerop export-code))
              (message "Warning: Failed to export secret subkeys for GitHub secret sync.")
            (let ((secret-str (buffer-string)))
              (with-temp-buffer
                (insert secret-str)
                (let ((gh-code (call-process-region (point-min) (point-max) gh-bin nil nil nil
                                                    "secret" "set" "ELPAISH_SIGNING_KEY" "-R" target-repo)))
                  (if (zerop gh-code)
                      (message "Successfully synchronized ELPAISH_SIGNING_KEY secret to %s!" target-repo)
                    (message "Warning: `gh secret set` failed with exit code %d" gh-code)))))))))

    (message "Key rotation complete for %s." master)))

;;;###autoload
(defun elpaish-revoke-key (key-id &optional output-dir)
  "Publish revocation certificate for KEY-ID to `elpaish.rev.asc' in OUTPUT-DIR."
  (interactive "sKey ID or Fingerprint to revoke: ")
  (unless (executable-find "gpg")
    (user-error "GPG binary not found in PATH"))
  (unless (and key-id (not (string-empty-p key-id)))
    (user-error "No key ID provided for revocation"))
  (let* ((target-dir (or output-dir elpaish-output-dir))
         (rev-file (expand-file-name "elpaish.rev.asc" target-dir))
         (gpg-bin (executable-find "gpg")))
    (make-directory target-dir t)
    (call-process gpg-bin nil nil nil "--batch" "--yes" "--armor" "--output" rev-file "--gen-revoke" key-id)
    (elpaish-export-keyring target-dir)
    (message "Published revocation certificate to %s" rev-file)))

;;; Package Build Engine

(defun elpaish-read-archive-contents (target-dir)
  "Read and parse `archive-contents' from TARGET-DIR if it exists.
Returns a hash table of package-name -> entry-vector."
  (let ((ac-file (expand-file-name "archive-contents" target-dir))
        (tbl (make-hash-table :test 'equal)))
    (when (file-exists-p ac-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents ac-file)
            (let ((data (read (current-buffer))))
              (when (and (listp data) (eq (car data) 1))
                (dolist (entry (cdr data))
                  (when (and (consp entry) (symbolp (car entry)) (vectorp (cdr entry)))
                    (puthash (symbol-name (car entry)) (cdr entry) tbl))))))
        (error nil)))
    tbl))
(defun elpaish--extract-buffer-metadata (recipe)
  "Extract package metadata from current buffer or fallback to RECIPE defaults.
Returns a plist with :summary, :reqs, :url, and :keywords."
  (let ((pkg-info (condition-case nil (package-buffer-info) (error nil))))
    (if pkg-info
        (list :summary (or (package-desc-summary pkg-info)
                           (and recipe (elpaish-recipe-summary recipe))
                           "No description")
              :reqs (or (package-desc-reqs pkg-info)
                        (and recipe (elpaish-recipe-requires recipe)))
              :url (or (cdr (assoc :url (package-desc-extras pkg-info)))
                       (and recipe (elpaish-recipe-url recipe)))
              :keywords (or (cdr (assoc :keywords (package-desc-extras pkg-info)))
                            (and recipe (elpaish-recipe-keywords recipe))
                            '("tools")))
      ;; Fallback: try parsing a (define-package ...) form if present in buffer
      (save-excursion
        (goto-char (point-min))
        (let ((form (condition-case nil (read (current-buffer)) (error nil))))
          (if (and (listp form) (eq (car form) 'define-package))
              (let* ((summary (nth 3 form))
                     (reqs (condition-case nil (eval (nth 4 form)) (error nil)))
                     (extras (nthcdr 5 form))
                     (url (plist-get extras :url))
                     (raw-kws (plist-get extras :keywords))
                     (keywords (if (and (listp raw-kws) (eq (car raw-kws) 'quote))
                                   (cadr raw-kws)
                                 raw-kws)))
                (list :summary (or summary (and recipe (elpaish-recipe-summary recipe)) "No description")
                      :reqs (or reqs (and recipe (elpaish-recipe-requires recipe)))
                      :url (or url (and recipe (elpaish-recipe-url recipe)))
                      :keywords (or keywords (and recipe (elpaish-recipe-keywords recipe)) '("tools"))))
            ;; Ultimate fallback: recipe defaults
            (list :summary (or (and recipe (elpaish-recipe-summary recipe)) "No description")
                  :reqs (and recipe (elpaish-recipe-requires recipe))
                  :url (and recipe (elpaish-recipe-url recipe))
                  :keywords (or (and recipe (elpaish-recipe-keywords recipe)) '("tools")))))))))

(defcustom elpaish-build-buffer-name "*elpaish-build*"
  "Buffer name used for ELPAish package build and compilation output."
  :type 'string
  :group 'elpaish)
(defun elpaish--write-sha256-file (file-path)
  "Generate a `.sha256' checksum file for FILE-PATH."
  (when (and file-path (file-exists-p file-path))
    (let ((hash (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally file-path)
                  (secure-hash 'sha256 (current-buffer))))
          (base (file-name-nondirectory file-path))
          (sha-file (concat file-path ".sha256")))
      (with-temp-file sha-file
        (insert (format "%s  %s\n" hash base)))
      sha-file)))

(defun elpaish--log (format-string &rest args)
  "Log message using FORMAT-STRING and ARGS to build buffer and `message'."
  (let ((msg (apply #'format format-string args))
        (time-str (format-time-string "%Y-%m-%d %H:%M:%S")))
    (when-let* ((buf (get-buffer elpaish-build-buffer-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "[%s] %s\n" time-str msg))
          (dolist (win (get-buffer-window-list buf nil t))
            (set-window-point win (point-max))))))
    (message "%s" msg)))

(defvar elpaish-build-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-mode-map)
    (define-key map (kbd "g") #'elpaish-build-recompile)
    (define-key map (kbd "r") #'elpaish-build-recompile)
    map)
  "Keymap for `elpaish-build-mode'.")

;;;###autoload
(defun elpaish-build-recompile (&optional _ignore-auto _noconfirm)
  "Re-run `elpaish-build-all' from the build compilation buffer."
  (interactive)
  (elpaish-build-all))

;;;###autoload
(define-derived-mode elpaish-build-mode compilation-mode "ELPAish-Build"
  "Major mode for ELPAish package build and compilation logs.
\\{elpaish-build-mode-map}"
  (setq-local revert-buffer-function #'elpaish-build-recompile))

(defmacro elpaish-with-build-buffer (title &rest body)
  "Execute BODY, directing build logs and results into `elpaish-build-buffer-name'.
TITLE is a string describing the build operation."
  (declare (indent 1))
  `(let* ((buf (get-buffer-create elpaish-build-buffer-name))
          (start-time (current-time))
          (op-title ,title))
     (with-current-buffer buf
       (let ((inhibit-read-only t))
         (erase-buffer)
         (unless (eq major-mode 'elpaish-build-mode)
           (elpaish-build-mode))
         (setq-local default-directory (expand-file-name elpaish-output-dir))
         (insert (propertize (format "=== ELPAish Build: %s ===\nStarted: %s\n\n"
                                     op-title
                                     (format-time-string "%Y-%m-%d %H:%M:%S" start-time))
                             'face 'bold))))
     (when (and (not noninteractive) (called-interactively-p 'any))
       (display-buffer buf))
     (unwind-protect
         (prog1 (progn ,@body)
           (with-current-buffer buf
             (let ((inhibit-read-only t)
                   (elapsed (float-time (time-subtract (current-time) start-time))))
               (goto-char (point-max))
               (insert (propertize (format "\n=== Build Finished in %.2fs ===\n" elapsed)
                                   'face 'bold-italic)))))
       nil)))

(cl-defun elpaish-build-package (recipe &optional stream output-directory-path)
  "Build, package, sign, and record status for RECIPE on STREAM.
STREAM is `snapshot', `stable', or `staging'.
OUTPUT-DIRECTORY-PATH defaults to stream directory under `elpaish-output-dir'.
Always regenerates the artifact, even when the source commit is unchanged
since the last build — elpaish's own packaging logic (file collection,
descriptor generation, signing, etc.) can change between builds
independently of the source repository, so a cached artifact from a prior
build's code cannot be trusted to reflect the current implementation.
Version numbers still track the source's last commit (see
`elpaish-derive-version'), not the time of this build."
  (let* ((effective-stream (elpaish-canonical-stream (or stream elpaish-release-mode)))
         (target-dir (or output-directory-path (elpaish-stream-dir effective-stream)))
         (repo-dir (elpaish--recipe-source-path recipe))
         (source-dir-rel (elpaish--recipe-source-dir-relative recipe))
         (name (elpaish-recipe-name recipe))
         (main-file-name
          (cond
           ((and (elpaish-recipe-files recipe)
                 (member (format "%s-pkg.el" name) (elpaish-recipe-files recipe))
                 (not (member (format "%s.el" name) (elpaish-recipe-files recipe))))
            (format "%s-pkg.el" name))
           ((string-suffix-p ".el" name) name)
           (t (concat name ".el"))))
         (main-file (expand-file-name main-file-name repo-dir))
         (current-commit (elpaish--current-hash repo-dir source-dir-rel)))

    ;; Check if package is disabled for this stream
    (when (elpaish-recipe-disabled-for-stream-p recipe effective-stream)
      (elpaish--log "Omitting %s from %s: Package is suppressed for this stream." name effective-stream)
      (cl-return-from elpaish-build-package nil))

    (unless (and main-file (file-exists-p main-file))
      (error "Main file %s not found in %s" main-file-name repo-dir))
    (unless (elpaish-preflight-package recipe)
      (elpaish--log "Skipping %s due to preflight quarantine." name)
      (push name elpaish--quarantined-packages)
      (cl-return-from elpaish-build-package nil))

    ;; 2. Derive Version
    (let ((version-str (elpaish-derive-version recipe effective-stream)))
      (unless version-str
        (when (eq effective-stream 'stable)
          (elpaish--log "Omitting %s from stable: No clean semver Git tag." name))
        (cl-return-from elpaish-build-package nil))
      ;; 3. Build artifact
      (make-directory target-dir t)
      (let* ((files (elpaish--collect-files repo-dir (elpaish-recipe-files recipe) name recipe))
             (is-tar (or (> (length files) 1) (string= main-file-name (format "%s-pkg.el" name))))
             (pkg-type (if is-tar 'tar 'single))
             (pkg-name-ver (format "%s-%s" name version-str))
             (dest-file (expand-file-name (format "%s.%s" pkg-name-ver (if is-tar "tar" "el"))
                                          target-dir))
             meta summary reqs url keywords)
        (with-temp-buffer
          (insert-file-contents main-file)
          (unless (string= main-file-name (format "%s-pkg.el" name))
            (elpaish--inject-version-header version-str))
          (setq meta (elpaish--extract-buffer-metadata recipe))
          (setq summary (plist-get meta :summary)
                reqs (plist-get meta :reqs)
                url (plist-get meta :url)
                keywords (plist-get meta :keywords))
          (if is-tar
              (elpaish--create-tar-package repo-dir dest-file pkg-name-ver name files
                                            :version version-str :summary summary
                                            :reqs reqs :url url :keywords keywords)
            (write-region (point-min) (point-max) dest-file nil 'silent)))

        ;; 4. Sign artifact and write SHA256 checksum file
        (elpaish--sign-file dest-file)
        (elpaish--write-sha256-file dest-file)
        ;; 5. Update recipe metadata & status
        (setf (elpaish-recipe-version-for-stream recipe effective-stream) version-str)

        (setf (elpaish-recipe-built-type recipe) pkg-type
              (elpaish-recipe-summary recipe) summary
              (elpaish-recipe-url recipe) url
              (elpaish-recipe-keywords recipe) keywords
              (elpaish-recipe-requires recipe) reqs
              (elpaish-recipe-built-hash recipe) current-commit)

        (elpaish--log "Successfully built %s version %s on %s" name version-str effective-stream)
        dest-file))))

;;; Archive Contents Generation

(defun elpaish-generate-archive-contents (&optional stream output-directory-path)
  "Generate `archive-contents' and its signature for STREAM.
OUTPUT-DIRECTORY-PATH defaults to stream destination directory."
  (let* ((effective-stream (elpaish-canonical-stream (or stream elpaish-release-mode)))
         (target-dir (or output-directory-path (elpaish-stream-dir effective-stream)))
         (archive-file (expand-file-name "archive-contents" target-dir))
         (recipes (hash-table-values elpaish-registry))
         (entries
          (delq nil
                (mapcar
                 (lambda (recipe)
                   (unless (elpaish-recipe-disabled-for-stream-p recipe effective-stream)
                     (when-let* ((ver-str (elpaish-recipe-version-for-stream recipe effective-stream)))
                       (let* ((name (intern (elpaish-recipe-name recipe)))
                              (ver (version-to-list ver-str))
                              (summary (or (elpaish-recipe-summary recipe) "No description"))
                              (pkg-type (or (elpaish-recipe-built-type recipe) 'single))
                              (reqs (elpaish-recipe-requires recipe))
                              (url (elpaish-recipe-url recipe))
                              (doc (elpaish-recipe-doc recipe))
                              (keywords (elpaish-recipe-keywords recipe))
                              (commit (elpaish-recipe-built-hash recipe))
                              (extras (delq nil
                                            (list (when url (cons :url url))
                                                  (when doc (cons :doc doc))
                                                  (when commit (cons :commit commit))
                                                  (when keywords (cons :keywords keywords))))))
                         `(,name . [,ver ,reqs ,summary ,pkg-type ,extras])))))
                 recipes))))
    (make-directory target-dir t)
    (with-temp-file archive-file
      (insert ";; -*- no-byte-compile: t -*-\n")
      (pp `(1 ,@entries) (current-buffer)))
    (elpaish--sign-file archive-file)
    (elpaish--write-sha256-file archive-file)
    archive-file))

(declare-function elpaish-generate-stream-index "elpaish-website" (&optional stream output-dir title))
(declare-function elpaish-generate-top-index "elpaish-website" (&optional output-dir))

;;; Build Orchestration

;;;###autoload
(cl-defun elpaish-build-all (&optional mode output-directory-path)
  "Build registered packages, generate indexes, and sign archives.
MODE can be `all', `snapshot', `stable', or `staging'.
Defaults to `elpaish-release-mode'.  OUTPUT-DIRECTORY-PATH defaults to
`elpaish-output-dir'.  Every registered package is rebuilt unconditionally
on every call; see `elpaish-build-package'."
  (interactive)
  (clrhash elpaish--resolved-repo-path-cache)
  (clrhash elpaish--preflight-cache)
  (let* ((effective-mode (or mode elpaish-release-mode))
         (target-root (or output-directory-path elpaish-output-dir))
         (streams (if (eq effective-mode 'all)
                      elpaish-streams
                    (list (elpaish-canonical-stream effective-mode))))
         (server-was-running (elpaish-server-running-p))
         (preview-port (and server-was-running (elpaish--server-port elpaish-server-process)))
         (elpaish--quarantined-packages nil))
    (elpaish-with-build-buffer (format "Build All (%s)" effective-mode)
      (make-directory target-root t)
      (dolist (stream streams)
        (let ((stream-dir (elpaish-stream-dir stream target-root)))
          (make-directory stream-dir t)
          (dolist (recipe (hash-table-values elpaish-registry))
            (elpaish-build-package recipe stream stream-dir))
          (elpaish-generate-archive-contents stream stream-dir)
          (elpaish-generate-stream-index stream stream-dir)))

      ;; Generate top-level landing page and public keyrings
      (elpaish-generate-top-index target-root)
      (elpaish-export-keyring target-root)

      ;; If preview server was running before build, ensure it remains active
      (when server-was-running
        (unless (elpaish-server-running-p)
          (elpaish-serve-local preview-port target-root)))

      (elpaish--log "ELPAish repository successfully generated at %s" target-root)
      (when (eq major-mode 'elpaish-status-mode)
        (elpaish-status-refresh))
      ;; Preflight quarantines are otherwise silent: the archive still builds
      ;; and this function still returns normally, so a caller that only
      ;; checks for an unhandled error (e.g. the CI batch runner) would see a
      ;; false success despite missing packages.
      (when elpaish--quarantined-packages
        (let ((names (delete-dups (nreverse elpaish--quarantined-packages))))
          (elpaish--log "Quarantined %d package(s) during preflight: %s"
                        (length names) (string-join names ", "))
          (error "ELPAish build quarantined %d package(s) during preflight: %s"
                 (length names) (string-join names ", ")))))))

;;;###autoload
(defun elpaish-build-single (recipe-or-name &optional stream output-directory-path)
  "Interactively build and sign a single package RECIPE-OR-NAME for STREAM.
OUTPUT-DIRECTORY-PATH overrides destination directory."
  (interactive
   (let* ((names (sort (hash-table-keys elpaish-registry) #'string<))
          (table (mapcar (lambda (n)
                           (let* ((r (gethash n elpaish-registry))
                                  (summary (if r (elpaish-recipe-summary r) "")))
                             (cons n summary)))
                         names))
          (picked (annotated-completing-read table :prompt "Build ELPAish package: " :require-match t))
          (stream-choice (annotated-completing-read
                          '(("all" . "Build across all release streams (snapshot, stable, staging)")
                            ("snapshot" . "Snapshot / development release stream")
                            ("stable" . "Clean semver release stream")
                            ("staging" . "Pre-release / RC release stream"))
                          :prompt "Release stream: " :default "all" :require-match t)))
     (list (gethash picked elpaish-registry) (intern stream-choice))))
  (let* ((recipe (cond
                  ((elpaish-recipe-p recipe-or-name) recipe-or-name)
                  ((symbolp recipe-or-name) (gethash (symbol-name recipe-or-name) elpaish-registry))
                  ((stringp recipe-or-name) (gethash recipe-or-name elpaish-registry))
                  (t nil)))
         (effective-stream (or stream 'all))
         (target-root (or output-directory-path elpaish-output-dir))
         (server-was-running (elpaish-server-running-p))
         (preview-port (and server-was-running (elpaish--server-port elpaish-server-process)))
         (elpaish--quarantined-packages nil))
    (unless recipe
      (user-error "Recipe %s not found in registry" recipe-or-name))
    (elpaish-with-build-buffer (format "%s (%s)" (elpaish-recipe-name recipe) effective-stream)
      (let ((streams (if (eq effective-stream 'all)
                         elpaish-streams
                       (list (elpaish-canonical-stream effective-stream)))))
        (dolist (st streams)
          (let ((stream-dir (elpaish-stream-dir st target-root)))
            (make-directory stream-dir t)
            (elpaish-build-package recipe st stream-dir)
            (elpaish-generate-archive-contents st stream-dir)
            (elpaish-generate-stream-index st stream-dir)))
        (elpaish-generate-top-index target-root)

        ;; If preview server was running before build, ensure it remains active
        (when server-was-running
          (unless (elpaish-server-running-p)
            (elpaish-serve-local preview-port target-root)))

        (when (eq major-mode 'elpaish-status-mode)
          (elpaish-status-refresh))
        (elpaish--log "Single build complete for %s on %s."
                      (elpaish-recipe-name recipe) effective-stream)
        (when elpaish--quarantined-packages
          (elpaish--log "Quarantined during preflight: %s" (elpaish-recipe-name recipe))
          (error "ELPAish build of %s quarantined during preflight" (elpaish-recipe-name recipe)))))))

;;;###autoload
(defun elpaish-preflight-single (recipe-or-name)
  "Run preflight quality gates on a single package RECIPE-OR-NAME."
  (interactive
   (let* ((names (sort (hash-table-keys elpaish-registry) #'string<))
          (table (mapcar (lambda (n)
                           (let* ((r (gethash n elpaish-registry))
                                  (summary (if r (elpaish-recipe-summary r) "")))
                             (cons n (if (string-empty-p summary) "registered recipe" summary))))
                         names))
          (picked (annotated-completing-read table :prompt "Preflight package: " :require-match t)))
     (list (gethash picked elpaish-registry))))
  (let ((recipe (cond
                 ((elpaish-recipe-p recipe-or-name) recipe-or-name)
                 ((symbolp recipe-or-name) (gethash (symbol-name recipe-or-name) elpaish-registry))
                 ((stringp recipe-or-name) (gethash recipe-or-name elpaish-registry))
                 (t nil))))
    (unless recipe
      (user-error "Recipe not found"))
    (if (elpaish-preflight-package recipe)
        (message "✓ Preflight passed for %s" (elpaish-recipe-name recipe))
      (message "✗ Preflight failed for %s" (elpaish-recipe-name recipe)))))

;;;###autoload
(defun elpaish-view-build-log ()
  "Switch to the ELPAish build compilation log buffer."
  (interactive)
  (let ((buf (get-buffer-create elpaish-build-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'compilation-mode)
        (elpaish-build-mode)))
    (display-buffer buf)))
;;; Local Preview HTTP Server

(defcustom elpaish-preview-server-port 9001
  "Default TCP port for `elpaish-serve-local' when none is specified."
  :type 'natnum
  :group 'elpaish)

(defconst elpaish--mime-types
  '((".html" . "text/html; charset=utf-8")
    (".el" . "text/plain; charset=utf-8")
    (".sig" . "application/pgp-signature")
    (".asc" . "application/pgp-keys")
    (".gpg" . "application/pgp-keys")
    (".tar" . "application/x-tar"))
  "File suffix to MIME content-type mapping for `elpaish--http-mime-type'.")

(defun elpaish--http-mime-type (path)
  "Return MIME content-type string for PATH."
  (or (cdr (seq-find (lambda (entry) (string-suffix-p (car entry) path)) elpaish--mime-types))
      "text/plain"))

(defun elpaish--server-port (&optional server)
  "Return active port of SERVER or `elpaish-server-process'."
  (let ((s (or server elpaish-server-process)))
    (cond
     ((null s) nil)
     ((and (fboundp 'ws-port) (ws-port s))
      (ws-port s))
     ((and (fboundp 'ws-process) (ws-process s) (process-live-p (ws-process s)))
      (process-contact (ws-process s) :service))
     ((processp s)
      (and (process-live-p s) (process-contact s :service)))
     (t nil))))

(defun elpaish-server-running-p (&optional server)
  "Return non-nil if SERVER or `elpaish-server-process' is currently running."
  (let ((s (or server elpaish-server-process)))
    (cond
     ((null s) nil)
     ((and (fboundp 'ws-process) (ignore-errors (ws-process s)))
      (let ((proc (ws-process s)))
        (and (processp proc) (process-live-p proc))))
     ((processp s)
      (process-live-p s))
     (t nil))))

(defun elpaish--http-resolve-path (req-path doc-root)
  "Resolve HTTP REQ-PATH against DOC-ROOT, preventing directory traversal.
Return resolved file path if valid and within DOC-ROOT, else nil."
  (when (stringp req-path)
    (let* ((clean-path (string-remove-prefix "/" req-path))
           (rel-path (if (or (string-empty-p clean-path) (string-suffix-p "/" clean-path))
                         (concat clean-path "index.html")
                       clean-path))
           (expanded (expand-file-name rel-path doc-root))
           (canon-root (file-name-as-directory (expand-file-name doc-root))))
      (when (string-prefix-p canon-root (expand-file-name expanded))
        expanded))))

(defun elpaish--handle-ws-request (request doc-root)
  "Handler for `web-server' REQUEST serving files from DOC-ROOT."
  (with-slots (process headers) request
    (let* ((get-path (cdr (assoc :GET headers)))
           (head-path (cdr (assoc :HEAD headers)))
           (req-path (or get-path head-path)))
      (if (not req-path)
          (progn
            (ws-response-header process 400
                                (cons "Content-Type" "text/plain; charset=utf-8")
                                (cons "Content-Length" "11"))
            (process-send-string process "Bad Request")
            (throw 'close-connection nil))
        (let ((file-path (elpaish--http-resolve-path req-path doc-root)))
          (if (and file-path (file-exists-p file-path) (file-regular-p file-path))
              (let ((mime (elpaish--http-mime-type file-path))
                    (size (file-attribute-size (file-attributes file-path))))
                (if head-path
                    (progn
                      (ws-response-header process 200
                                          (cons "Content-Type" mime)
                                          (cons "Content-Length" (number-to-string size)))
                      (throw 'close-connection nil))
                  (ws-send-file process file-path mime)))
            (ws-response-header process 404
                                (cons "Content-Type" "text/plain; charset=utf-8")
                                (cons "Content-Length" "9"))
            (process-send-string process "Not Found")
            (throw 'close-connection nil)))))))

(defun elpaish--handle-http-request (request-str doc-root)
  "Process HTTP REQUEST-STR for document root DOC-ROOT.
Returns a cons cell (HEADERS . BODY-BYTES)."
  (if (string-match "\\`\\(GET\\|HEAD\\)[ \t]+\\([^ \t\r\n?]+\\)" request-str)
      (let* ((method (match-string 1 request-str))
             (req-path (match-string 2 request-str))
             (file-path (elpaish--http-resolve-path req-path doc-root)))
        (if (and file-path (file-exists-p file-path) (file-regular-p file-path))
            (let* ((content (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally file-path)
                              (buffer-string)))
                   (mime (elpaish--http-mime-type file-path))
                   (headers (format "HTTP/1.1 200 OK\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n"
                                    mime (length content))))
              (cons headers (if (string= method "HEAD") "" content)))
          (cons "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 9\r\nConnection: close\r\n\r\n"
                "Not Found")))
    (cons "HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 11\r\nConnection: close\r\n\r\n"
          "Bad Request")))

;;;###autoload
(defun elpaish-serve-local (&optional port output-dir)
  "Start local HTTP server serving OUTPUT-DIR (defaults to public/) at PORT.
PORT defaults to `elpaish-preview-server-port'."
  (interactive "P")
  (elpaish-stop-server)
  (let* ((preview-port (or port elpaish-preview-server-port))
         (doc-root (file-name-as-directory (or output-dir elpaish-output-dir))))
    (unless (file-directory-p doc-root)
      (make-directory doc-root t))
    (setq elpaish-server-process
          (ws-start
           (lambda (req)
             (elpaish--handle-ws-request req doc-root))
           preview-port))
    (message "ELPAish preview server running at http://127.0.0.1:%d/ (root: %s)" preview-port doc-root)))

;;;###autoload
(defun elpaish-stop-server ()
  "Stop local preview HTTP server if active."
  (interactive)
  (when elpaish-server-process
    (cond
     ((and (fboundp 'ws-stop)
           (or (and (boundp 'ws-server) (cl-typep elpaish-server-process 'ws-server))
               (and (fboundp 'ws-process) (ignore-errors (ws-process elpaish-server-process)))))
      (ws-stop elpaish-server-process))
     ((processp elpaish-server-process)
      (when (process-live-p elpaish-server-process)
        (delete-process elpaish-server-process))))
    (setq elpaish-server-process nil)
    (message "ELPAish preview server stopped.")))

;;; Automated Rebuild Timer

(defconst elpaish-auto-build-intervals
  '("1 min" "5 mins" "10 mins" "30 mins" "1 hour" "2 hours" "4 hours" "8 hours" "12 hours")
  "Preset interval options for `elpaish-start-auto-build'.")

;;;###autoload
(defun elpaish-start-auto-build (interval &optional idle)
  "Start scheduled background rebuilds of the ELPA repository.
INTERVAL can be seconds or a time string (e.g. \"1 hour\").
If IDLE is non-nil, run rebuilds when Emacs is idle for INTERVAL."
  (interactive
   (list (annotated-completing-read
          (thread-last elpaish-auto-build-intervals
            (seq-map (lambda (i) (cons i (format "Rebuild every %s" i)))))
          :prompt "Auto-build interval: "
          :require-match nil
          :default "1 hour")
         current-prefix-arg))
  (elpaish-stop-auto-build)
  (let* ((secs (cond
                ((numberp interval) interval)
                ((and (stringp interval) (string-match-p "^[0-9]+$" interval))
                 (string-to-number interval))
                ((stringp interval)
                 (or (timer-duration interval) (string-to-number interval)))
                (t (error "Invalid interval: %S" interval)))))
    (setq elpaish-timer
          (if idle
              (run-with-idle-timer secs t #'elpaish-build-all)
            (run-at-time secs secs #'elpaish-build-all)))
    (message "ELPA auto-build started (%s, running every %s)."
             (if idle "when idle" "periodic")
             interval)))

;;;###autoload
(defun elpaish-stop-auto-build ()
  "Stop the periodic background rebuild timer if active."
  (interactive)
  (when (timerp elpaish-timer)
    (cancel-timer elpaish-timer)
    (setq elpaish-timer nil)
    (message "ELPA auto-build timer stopped.")))

(defvar elpaish-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'elpaish-status-refresh)
    (define-key map (kbd "b") #'elpaish-status-build-at-point)
    (define-key map (kbd "B") #'elpaish-build-single)
    (define-key map (kbd "a") #'elpaish-build-all)
    (define-key map (kbd "p") #'elpaish-status-preflight-at-point)
    (define-key map (kbd "P") #'elpaish-status-preflight-all)
    (define-key map (kbd "l") #'elpaish-view-build-log)
    (define-key map (kbd "s") #'elpaish-setup-signing)
    (define-key map (kbd "r") #'elpaish-rotate-keys)
    (define-key map (kbd "w") #'elpaish-serve-local)
    (define-key map (kbd "?") #'elpaish-menu)
    (define-key map (kbd "m") #'elpaish-menu)
    map)
  "Keymap for `elpaish-status-mode'.")

(define-derived-mode elpaish-status-mode tabulated-list-mode "ELPAish-Builder"
  "Major mode for inspecting and managing ELPAish package tracks."
  (setq tabulated-list-format
        [("Package Name" 24 t)
         ("Path / Repository" 30 t)
         ("Snapshot" 18 t)
         ("Stable" 10 t)
         ("Staging" 14 t)
         ("Hash" 9 nil)
         ("Delta" 10 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun elpaish-status-refresh ()
  "Refresh the package registry status table."
  (interactive)
  (setq tabulated-list-entries
        (thread-last (hash-table-values elpaish-registry)
          (seq-map (lambda (recipe)
                     (let* ((name (elpaish-recipe-name recipe))
                            (repo-dir (elpaish--resolve-repo-path recipe))
                            (exists (file-exists-p repo-dir))
                            (curr-hash (if exists (substring (elpaish--current-hash repo-dir) 0 7) "N/A"))
                            (delta (if exists
                                       (elpaish--commit-delta repo-dir (elpaish-recipe-built-hash recipe)
                                                               (elpaish--recipe-source-dir-relative recipe))
                                     "Uncloned"))
                            (snap-ver (or (elpaish-recipe-built-version-snapshot recipe)
                                          "—"))
                            (stab-ver (or (elpaish-recipe-built-version-stable recipe) "—"))
                            (stage-ver (or (elpaish-recipe-built-version-staging recipe) "—")))
                       (list name
                             (vector name
                                     (elpaish-recipe-repository-path recipe)
                                     snap-ver
                                     stab-ver
                                     stage-ver
                                     curr-hash
                                     (format "+%s" delta))))))))
  (tabulated-list-print t))
;;;###autoload
(defun elpaish-status-build-at-point ()
  "Build the package at point across all release streams."
  (interactive)
  (if-let* ((name (tabulated-list-get-id))
            (recipe (gethash name elpaish-registry)))
      (progn
        (dolist (stream elpaish-streams)
          (elpaish-build-package recipe stream)
          (elpaish-generate-archive-contents stream)
          (elpaish-generate-stream-index stream))
        (elpaish-generate-top-index)
        (elpaish-status-refresh))
    (user-error "No recipe at point")))

;;;###autoload
(defun elpaish-status-preflight-at-point ()
  "Execute preflight check for package at point."
  (interactive)
  (if-let* ((name (tabulated-list-get-id))
            (recipe (gethash name elpaish-registry)))
      (if (elpaish-preflight-package recipe)
          (message "✓ Preflight checks passed for %s" name)
        (message "✗ Preflight checks failed for %s" name))
    (user-error "No recipe found at point")))

;;;###autoload
(defun elpaish-status-preflight-all ()
  "Execute preflight check suite for all registered packages."
  (interactive)
  (let ((passed-count 0)
        (failed-count 0))
    (dolist (recipe (hash-table-values elpaish-registry))
      (if (elpaish-preflight-package recipe)
          (cl-incf passed-count)
        (cl-incf failed-count)))
    (message "Preflight results: %d passed, %d quarantined." passed-count failed-count)))

;;;###autoload
(defun elpaish-status ()
  "Open the ELPAish Builder management buffer."
  (interactive)
  (let ((buf (get-buffer-create "*elpaish-status*")))
    (with-current-buffer buf
      (elpaish-status-mode)
      (elpaish-status-refresh))
    (switch-to-buffer buf)))

;;;###autoload
(defun elpaish-run-checks (&optional dir)
  "Execute package quality check suite for current repository or DIR."
  (interactive)
  (require 'elpaish-check)
  (elpaish-check-all dir))


;;;###autoload
(defun elpaish-view-check-log (&optional dir)
  "Switch to the ELPAish check compilation log buffer for DIR."
  (interactive)
  (require 'elpaish-check)
  (let ((buf (get-buffer-create (elpaish-check--get-buffer-name dir))))
    (with-current-buffer buf
      (unless (derived-mode-p 'compilation-mode)
        (elpaish-check-mode)))
    (display-buffer buf)))

(declare-function elpaish-load-packages "elpaish-recipes" (&optional files))

;;;###autoload
(transient-define-prefix elpaish-menu ()
  "Transient dispatch menu for ELPAish package repository management."
  [:description "ELPAish Repository Builder"
   ["Build Actions"
    ("b" "Build single package" elpaish-build-single)
    ("a" "Build all packages (all streams)" (lambda () (interactive) (elpaish-build-all 'all)))
    ("rd" "Build snapshot stream" (lambda () (interactive) (elpaish-build-all 'snapshot)))
    ("rlts" "Build stable stream" (lambda () (interactive) (elpaish-build-all 'stable)))
    ("rrc" "Build staging stream" (lambda () (interactive) (elpaish-build-all 'staging)))]
   ["Quality & Inspection"
    ("po" "Preflight single package" elpaish-preflight-single)
    ("pa" "Preflight all packages" elpaish-status-preflight-all)
    ("v" "Status overview" elpaish-status)
    ("c" "Run quality check suite" elpaish-run-checks)
    ("l" "View build log" elpaish-view-build-log)
    ("cl" "View check log" elpaish-view-check-log)]
   ["Website & Preview"
    ("g" "Rebuild website only" elpaish-build-website)
    ("wo" "Start local preview server" elpaish-serve-local)
    ("wc" "Stop preview server" elpaish-stop-server)
    ("rr" "Reload package recipes" (lambda () (interactive) (elpaish-load-packages) (message "Reloaded package recipes.")))]
   ["Keyring & Operations"
    ("k" "Export GPG keyring" elpaish-export-keyring)
    ("TKO" "Rotate GPG signing keys" elpaish-rotate-keys)]])

(provide 'elpaish)
;;; elpaish.el ends here
