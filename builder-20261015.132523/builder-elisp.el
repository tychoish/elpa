;;; builder-elisp.el --- Emacs Lisp package management, test, and reload -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (f "0.20.0") (xtdlib "0.1.0"))
;; Keywords: tools, lisp, elpa

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tools for developing, testing, linting, packaging, and in-session reloading
;; of Emacs Lisp packages.

;;; Code:

(require 'compile)
(require 'package)
(require 'lisp-mnt)
(require 'ert)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'cl-lib)
(require 'f)
(require 'xtd-f)
(require 'xtd-project)
(require 'builder-core)

(declare-function lm-header "lisp-mnt")
(declare-function package-build-archive "package-build")
(defvar package-build-working-dir)
(defvar package-build-archive-dir)
(defvar package-build-recipes-dir)
(defvar package-build-verbose)
(defvar package-build-releases)

(defgroup builder-elisp nil
  "Emacs Lisp development and package tools for builder."
  :group 'builder
  :prefix "builder-elisp-")

(defun builder-elisp-package--name (root)
  "Return the package name for the elisp project at ROOT.
Strips a trailing \".el\" from the directory name so a project in
\"xtdlib.el/\" packages as \"xtdlib\"."
  (let ((basename (file-name-nondirectory (directory-file-name root))))
    (if (string-suffix-p ".el" basename)
        (string-remove-suffix ".el" basename)
      basename)))

(defvar builder-elisp-package-path-resolvers
  (list (lambda (root &optional _name) root)
        (lambda (root &optional _name) (f-join root "pkg"))
        (lambda (root &optional _name) (f-join root "lisp"))
        (lambda (root &optional _name) (f-join root "src")))
  "List of functions to resolve candidate elisp package paths for ROOT.
Each function is called with (ROOT &optional NAME), where ROOT is the
directory of the repository or project and NAME is the string package name.
A resolver may return a directory path, a regular file path, a list of
paths, or nil if not applicable.")

(defun builder-register-elisp-package-path-resolver (fn &optional append)
  "Register FN as an elisp package path resolver.
FN is added to `builder-elisp-package-path-resolvers'.
If APPEND is non-nil, add FN to the end of the list."
  (if append
      (setq builder-elisp-package-path-resolvers
            (append builder-elisp-package-path-resolvers (list fn)))
    (add-to-list 'builder-elisp-package-path-resolvers fn)))

(defun builder-elisp-package-resolve-paths (root &optional name)
  "Return existing candidate package paths for ROOT.
Evaluates `builder-elisp-package-path-resolvers' with ROOT and package NAME,
returning a deduplicated list of existing directories or files."
  (let* ((pkg-name (or name (builder-elisp-package--name root)))
         (raw-paths (seq-mapcat
                     (lambda (fn)
                       (let ((res (funcall fn root pkg-name)))
                         (cond
                          ((null res) nil)
                          ((listp res) res)
                          (t (list res)))))
                     builder-elisp-package-path-resolvers)))
    (seq-uniq (seq-filter (lambda (p)
                            (or (file-directory-p p)
                                (file-regular-p p)))
                          raw-paths))))

(defun builder-elisp-package-directories (root &optional name)
  "Return existing package directories for ROOT resolved by `builder-elisp-package-path-resolvers'."
  (seq-filter #'file-directory-p (builder-elisp-package-resolve-paths root name)))

(defun builder-elisp-package--main-file (root)
  "Return the absolute path to the main .el file under ROOT, or nil.
Resolves candidate paths via `builder-elisp-package-path-resolvers'."
  (let* ((name (builder-elisp-package--name root))
         (target (concat name ".el")))
    (cl-some (lambda (path)
               (cond
                ((file-regular-p path)
                 (when (string-equal (file-name-nondirectory path) target)
                   path))
                ((file-directory-p path)
                 (let ((candidate (f-join path target)))
                   (when (file-regular-p candidate)
                     candidate)))))
             (builder-elisp-package-resolve-paths root name))))

(defun builder-elisp-package-p (&optional root)
  "Return non-nil if ROOT looks like an elisp package.
ROOT defaults to `approximate-project-root'."
  (and (builder-elisp-package--main-file (or root (approximate-project-root))) t))

(defun builder-elisp-package--pkg-file (root)
  "Return absolute path to <NAME>-pkg.el under ROOT, or nil if absent.
Resolves candidate paths via `builder-elisp-package-path-resolvers'."
  (let* ((name (builder-elisp-package--name root))
         (target (concat name "-pkg.el")))
    (cl-some (lambda (path)
               (cond
                ((file-regular-p path)
                 (when (string-equal (file-name-nondirectory path) target)
                   path))
                ((file-directory-p path)
                 (let ((candidate (f-join path target)))
                   (when (file-regular-p candidate)
                     candidate)))))
             (builder-elisp-package-resolve-paths root name))))

(defun builder-elisp-package--source-files (root)
  "Return the list of top-level .el source files under ROOT.
Excludes test/ (not at top level), the generated <NAME>-pkg.el package
descriptor, and any <NAME>-autoloads.el file — none of which should be
byte-compiled as ordinary sources."
  (when (file-directory-p root)
    (let* ((name (builder-elisp-package--name root))
           (skip (list (concat name "-pkg.el")
                       (concat name "-autoloads.el")))
           (dirs (builder-elisp-package-directories root name))
           (entries (seq-mapcat (lambda (d) (f-entries d #'file-regular-p)) dirs)))
      (seq-remove (lambda (it)
                    (or (member (file-name-nondirectory it) skip)
                        (string-suffix-p "-pkg.el" it)
                        (string-suffix-p "-autoloads.el" it)))
                  (seq-filter (lambda (it) (string-equal (file-name-extension it) "el"))
                              entries)))))

(defun builder-elisp-package--test-files (root)
  "Return the list of test files under ROOT (test/, tests/, or root directory)."
  (let ((dirs (seq-filter #'file-directory-p
                          (list (f-join root "test")
                                (f-join root "tests")
                                root)))
        (files nil))
    (dolist (dir dirs)
      (dolist (it (f-entries dir #'file-regular-p))
        (when (string-equal (file-name-extension it) "el")
          (let ((name (file-name-nondirectory it)))
            (when (or (string-prefix-p "test-" name)
                      (string-suffix-p "-test.el" name)
                      (string-suffix-p "-tests.el" name)
                      (not (f-equal-p dir root)))
              (push it files))))))
    (seq-uniq (nreverse files))))
(defun builder-elisp-package--read-pkg-deps (pkg-file)
  "Return dep symbols from a multi-file package's <NAME>-pkg.el descriptor.
Reads the `define-package' form's fourth argument (the requires list).
The pseudo-package `emacs' is omitted."
  (with-temp-buffer
    (insert-file-contents pkg-file)
    (goto-char (point-min))
    (let ((form (ignore-errors (read (current-buffer)))))
      (when (and (consp form) (eq (car form) 'define-package))
        (let ((reqs (nth 4 form)))
          (when (and (consp reqs) (eq (car reqs) 'quote))
            (setq reqs (cadr reqs)))
          (seq-remove (lambda (it) (eq it 'emacs))
			  (seq-map #'car reqs)))))))

(defun builder-elisp-package--read-deps (file-or-root)
  "Return list of dep package symbols for the package at FILE-OR-ROOT.
If FILE-OR-ROOT is a directory, prefer <NAME>-pkg.el's `define-package'
requires and fall back to the main file's `Package-Requires' header.
If it is a file, read `Package-Requires' from that file. The pseudo-
package `emacs' is omitted, since it is not installable."
  (require 'lisp-mnt)
  (cond
   ((and file-or-root (file-directory-p file-or-root))
    (or (when-let* ((pkg (builder-elisp-package--pkg-file file-or-root)))
          (builder-elisp-package--read-pkg-deps pkg))
        (when-let* ((main (builder-elisp-package--main-file file-or-root)))
          (builder-elisp-package--read-deps main))))
   ((and file-or-root (file-regular-p file-or-root))
    (with-temp-buffer
      (insert-file-contents file-or-root)
      (when-let* ((line (lm-header "Package-Requires")))
        (seq-remove (lambda (it) (eq it 'emacs))
			(seq-map #'car (read line))))))))

(defun builder-elisp-package--require-deps (deps)
  "Require each symbol in DEPS, signaling a `user-error' if any cannot load.
Use this from contexts that assume the user's `load-path' already provides them
\(direct M-x invocation, or batch with `-L' flags pointing into ~/.emacs.d/elpa)."
  (dolist (it deps)
    (unless (require it nil t)
      (user-error "elisp-package dependency not on load-path: %s" it))))

;;;###autoload
(defun builder-elisp-package-test ()
  "Run ert tests for the elisp package at `approximate-project-root`.
Loads package sources and any test/test-*.el or test/*-test.el files,
then runs ert. In batch mode exits with the test status; otherwise
opens the *ert* selector."
  (interactive)
  (require 'ert)
  (let* ((root (file-name-as-directory (approximate-project-root)))
         (main-file (builder-elisp-package--main-file root))
         (test-files (builder-elisp-package--test-files root)))
    (unless (or main-file test-files)
      (user-error "No test files or package found at %s" root))
    (builder-elisp-package--require-deps (builder-elisp-package--read-deps root))
    (let* ((test-dirs (seq-filter #'file-directory-p
                                  (list (f-join root "test") (f-join root "tests"))))
           (load-path (append (cons root test-dirs) load-path)))
      (when main-file
        (load main-file nil t)
        (dolist (it (builder-elisp-package--source-files root))
          (unless (f-equal-p it main-file)
            (load it nil t))))
      (dolist (it test-files)
        (load it nil t)))
    (if noninteractive
        (ert-run-tests-batch-and-exit)
      (ert t))))


(defun builder--ert-test-names ()
  "Return the sorted names (strings) of every currently defined ert test."
  (thread-last (ert-select-tests t t)
               (seq-map (lambda (test) (symbol-name (ert-test-name test))))
               (seq-sort #'string<)))

(defconst builder--ert-prefix-separator-regexp "[-/:]+"
  "Regexp for a run of separator characters used to derive ert test-name prefixes.")

(defun builder--ert-test-name-prefixes (name)
  "Return every separator-terminated prefix of ert test NAME."
  (let (prefixes (start 0))
    (while (string-match builder--ert-prefix-separator-regexp name start)
      (setq start (match-end 0))
      (push (substring name 0 start) prefixes))
    (nreverse prefixes)))

(defun builder--ert-known-prefixes ()
  "Return each prefix shared by two or more currently defined ert test names.
Derived generically from whichever separator convention (\"/\", \"-\", \"--\")
a given test suite happens to use, rather than hardcoding module names."
  (let ((counts (make-hash-table :test #'equal)))
    (seq-do (lambda (name)
              (seq-do (lambda (prefix) (cl-incf (map-elt counts prefix 0)))
                      (builder--ert-test-name-prefixes name)))
            (builder--ert-test-names))
    (thread-last (map-keys counts)
                 (seq-filter (lambda (prefix) (> (map-elt counts prefix) 1)))
                 (seq-sort #'string<))))

(defun builder--ert-prefix-candidates (&optional suffix)
  "Return an ACR table of known ert test-name prefixes.
Each candidate is annotated with how many currently loaded tests it
matches, and has SUFFIX (e.g. \"*\" for a glob prompt) appended after
counting so the annotation still reflects the bare prefix's matches."
  (let ((names (builder--ert-test-names)))
    (seq-map (lambda (prefix)
               (cons (concat prefix suffix)
                     (format "%d tests"
                             (length (seq-filter (lambda (name) (string-prefix-p prefix name))
                                                  names)))))
             (builder--ert-known-prefixes))))

(defun builder--ert-run-tests-matching (predicate description)
  "Run every currently loaded ert test whose name satisfies PREDICATE.
DESCRIPTION appears in the `user-error' raised when nothing matches."
  (require 'ert)
  (let ((matching (seq-filter predicate (builder--ert-test-names))))
    (unless matching
      (user-error "No ert tests match %s" description))
    (ert (cons 'member (seq-map #'intern matching)))))

;;;###autoload
(defun builder-run-ert-tests-with-prefix (prefix)
  "Run every currently loaded ert test whose name starts with PREFIX.
PREFIX is read via `annotated-completing-read', offering every prefix
shared by two or more loaded test names (see `builder--ert-known-prefixes')
as a suggestion, annotated with its match count. Any string works —
matching is a plain `string-prefix-p' test, not restricted to what's listed."
  (interactive
   (list (annotated-completing-read
          (builder--ert-prefix-candidates)
          :prompt "Test prefix: "
          :category 'ert-test-prefix)))
  (builder--ert-run-tests-matching
   (lambda (name) (string-prefix-p prefix name))
   (format "prefix %S" prefix)))

;;;###autoload
(defun builder-run-ert-tests-with-glob (pattern)
  "Run every currently loaded ert test whose name matches glob PATTERN.
PATTERN is read via `annotated-completing-read' the same way as
`builder-run-ert-tests-with-prefix' (candidates are the known prefixes
with a trailing \"*\"), but matching translates PATTERN through
`wildcard-to-regexp' first, so \"*\" and \"?\" work as wildcards
instead of literal characters."
  (interactive
   (list (annotated-completing-read
          (builder--ert-prefix-candidates "*")
          :prompt "Test name glob: "
          :category 'ert-test-glob)))
  (let ((regexp (wildcard-to-regexp pattern)))
    (builder--ert-run-tests-matching
     (lambda (name) (string-match-p regexp name))
     (format "glob %S" pattern))))

;;;###autoload
(defun builder-elisp-package-compile ()
  "Byte-compile every top-level .el source file in the elisp package at point.
In batch mode exits with status 1 if any file fails to compile."
  (interactive)
  (let* ((root      (file-name-as-directory (approximate-project-root)))
         (main-file (builder-elisp-package--main-file root)))
    (unless main-file
      (user-error "no <%s>.el at %s" (builder-elisp-package--name root) root))
    (builder-elisp-package--require-deps (builder-elisp-package--read-deps root))
    (let* ((load-path (cons root load-path))
           (failed 0))
      (dolist (it (builder-elisp-package--source-files root))
        (unless (byte-compile-file it)
          (cl-incf failed)))
      (when noninteractive
        (kill-emacs (if (zerop failed) 0 1)))
      (zerop failed))))

;;;###autoload
(defun builder-elisp-package-lint ()
  "Run linters (byte-compile, checkdoc, package-lint) for the package at point.
In batch mode exits with status 1 if any file fails linting or compilation."
  (interactive)
  (let* ((root      (file-name-as-directory (approximate-project-root)))
         (main-file (builder-elisp-package--main-file root))
         (sources   (builder-elisp-package--source-files root))
         (failed 0))
    (unless main-file
      (user-error "no <%s>.el at %s" (builder-elisp-package--name root) root))
    (builder-elisp-package--require-deps (builder-elisp-package--read-deps root))
    (let* ((load-path (cons root load-path))
           (byte-compile-error-on-warn t))
      ;; 1. Byte-compilation
      (dolist (it sources)
        (message "[builder-lint] byte-compiling %s..." (file-name-nondirectory it))
        (unless (byte-compile-file it)
          (cl-incf failed))
        (let ((elc (concat (file-name-sans-extension it) ".elc")))
          (when (file-exists-p elc)
            (delete-file elc))))
      ;; 2. Checkdoc
      (require 'checkdoc)
      (dolist (it sources)
        (message "[builder-lint] checkdoc %s..." (file-name-nondirectory it))
        (with-current-buffer (find-file-noselect it)
          (let ((checkdoc-diagnostic-buffer "*checkdoc-diagnostics*"))
            (checkdoc-current-buffer t))))
      ;; 3. Package-lint (when available)
      (when (require 'package-lint nil t)
        (dolist (it sources)
          (message "[builder-lint] package-lint %s..." (file-name-nondirectory it))
          (with-current-buffer (find-file-noselect it)
            (let ((errs (package-lint-buffer)))
              (when errs
                (dolist (e errs)
                  (message "[builder-lint] %s:%s:%s [%s]: %s"
                           (file-name-nondirectory it)
                           (nth 0 e) (nth 1 e) (nth 2 e) (nth 3 e)))
                (cl-incf failed (length (seq-filter (lambda (e) (eq (nth 2 e) 'error)) errs))))))))
      (message "[builder-lint] completed with %d error(s)" failed)
      (when noninteractive
        (kill-emacs (if (zerop failed) 0 1)))
      (zerop failed))))

;;;###autoload
(defun builder-elisp-package-build ()
  "Build an installable .tar via `package-build' for the elisp package at point.
The recipe is generated at build time and uses a file:// URL pointing at the
local repository, so the package contents reflect HEAD; uncommitted changes
are not included. The tarball lands in <root>/build/packages/."
  (interactive)
  (require 'package-build)
  (require 'package-recipe)
  (let* ((root      (file-name-as-directory (approximate-project-root)))
         (name      (builder-elisp-package--name root))
         (main-file (builder-elisp-package--main-file root)))
    (unless main-file
      (user-error "no <%s>.el at %s" name root))
    (unless (file-directory-p (f-join root ".git"))
      (user-error "package build requires a git repository at %s" root))
    (unless (zerop (call-process "git" nil nil nil "-C" root
                                 "rev-parse" "--verify" "HEAD"))
      (user-error "package build requires at least one commit in %s" root))
    (let* ((build-root  (f-join root "build"))
           (working-dir (f-join build-root "working"))
           (archive-dir (f-join build-root "packages"))
           (recipes-dir (f-join build-root "recipes"))
           (recipe-path (f-join recipes-dir name)))
      (dolist (it (list build-root working-dir archive-dir recipes-dir))
        (make-directory it t))
      (with-temp-file recipe-path
        (let ((print-quoted t))
          (prin1 `(,(intern name)
                   :fetcher git
                   :url ,(concat "file://" (directory-file-name root))
                   :files ("*.el"))
                 (current-buffer)))
        (insert "\n"))
      (let ((package-build-working-dir (file-name-as-directory working-dir))
            (package-build-archive-dir (file-name-as-directory archive-dir))
            (package-build-recipes-dir (file-name-as-directory recipes-dir))
            (package-build-verbose t)
            (package-build-releases nil))
        (package-build-archive name))
      (when noninteractive (kill-emacs 0)))))

;;;###autoload
(defun builder-elisp-package-clean ()
  "Remove build artifacts (build/, .cache/, *.elc) for the elisp package at point."
  (interactive)
  (let ((root (file-name-as-directory (approximate-project-root))))
    (dolist (it (list (f-join root "build") (f-join root ".cache")))
      (when (file-directory-p it)
        (delete-directory it t)
        (message "removed %s" it)))
    (dolist (it (seq-filter (lambda (it) (string-equal (file-name-extension it) "elc"))
			  (f-entries root #'file-regular-p t)))
      (delete-file it)
      (message "removed %s" it))
    (when noninteractive (kill-emacs 0))))

;;;###autoload
(defun builder-elisp-package-reload (&optional directory)
  "Reload the Emacs Lisp package in DIRECTORY into the current Emacs session.
DIRECTORY defaults to `approximate-project-root' or `default-directory'.
Adds package directories to `load-path', loads declared dependencies,
reloads all package source files into the running Emacs process, and provides
the package feature."
  (interactive
   (list (if current-prefix-arg
             (read-directory-name "Reload package in directory: ")
           (or (approximate-project-root) default-directory))))
  (let* ((root (file-name-as-directory (expand-file-name (or directory (approximate-project-root) default-directory))))
         (name (builder-elisp-package--name root))
         (main-file (builder-elisp-package--main-file root))
         (source-files (builder-elisp-package--source-files root))
         (dirs (builder-elisp-package-directories root name)))
    (unless (file-directory-p root)
      (user-error "Directory does not exist: %s" root))
    (unless (or main-file source-files)
      (when-let* ((scripts-dir (f-join root "scripts"))
                  (_ (file-directory-p scripts-dir)))
        (setq source-files (seq-filter (lambda (it) (string-equal (file-name-extension it) "el"))
                                       (f-entries scripts-dir #'file-regular-p)))))
    (unless (or main-file source-files)
      (user-error "No Emacs Lisp package or source files found in %s" root))
    (dolist (d (or dirs (list root)))
      (add-to-list 'load-path (file-name-as-directory d)))
    (when-let* ((deps (builder-elisp-package--read-deps root)))
      (builder-elisp-package--require-deps deps))
    (let* ((files (seq-uniq
                   (if main-file
                       (cons main-file (seq-remove (lambda (f) (f-equal-p f main-file)) source-files))
                     source-files)))
           (load-prefer-newer t)
           (count 0))
      (dolist (file files)
        (load (file-name-sans-extension file) nil t)
        (cl-incf count))
      (dolist (al (list (f-join root (concat name "-autoloads.el"))
                        (f-join root "loaddefs.el")))
        (when (file-regular-p al)
          (load (file-name-sans-extension al) nil t)))
      (when name
        (provide (intern name)))
      (when (fboundp 'builder-clear-all-caches)
        (builder-clear-all-caches))
      (message "Reloaded %s (%d file%s) into current Emacs session"
               name count (if (= count 1) "" "s"))
      count)))

;; package dependency analysis — builds on the private `package--get-deps'
;; (transitive closure) and `package-desc-reqs' (direct deps from local
;; metadata) primitives in package.el.  `package--removable-packages' is a
;; near-cousin but is hard-wired to `package-selected-packages'; these
;; helpers accept an arbitrary root list.

(defun builder-package--direct-deps (package)
  "Return the list of direct dependency symbols declared by PACKAGE.
Reads `Package-Requires' via `package-desc-reqs' on the entry in
`package-alist'; filters the `emacs' pseudo-dependency.  Returns nil
when PACKAGE is not installed."
  (when-let* ((desc (cadr (assq package package-alist))))
    (seq-remove (lambda (it) (eq it 'emacs))
		 (seq-map #'car (package-desc-reqs desc)))))

(defun builder-package-deps-map (packages)
  "Return a hash-table mapping each symbol in PACKAGES to its direct deps.
Values are lists of dependency symbols read from local package
metadata.  Packages not present in `package-alist' map to nil."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (pkg packages table)
      (puthash pkg (builder-package--direct-deps pkg) table))))

(defun builder-package-deps-closure (packages)
  "Return the de-duplicated transitive closure of PACKAGES.
Includes the roots themselves even when they have no entry in
`package-alist' (e.g. packages loaded via `:load-path').
`package--get-deps' filters such roots out of its result because it
keys off `package-alist'; we union the raw roots back in so closure
membership is meaningful for non-package.el installs."
  (seq-uniq (append (copy-sequence packages)
                    (package--get-deps (copy-sequence packages)))))

(defvar builder-package-installed-exclude '(archives gnupg)
  "Directory-name symbols under `package-user-dir' to ignore.
These are subdirectories created by `package.el' that are not
packages (archive metadata, gpg keyring).  Consumed by
`builder-package--installed-on-disk'.")

(defun builder-package--installed-on-disk ()
  "Return package-name symbols rooted in `package-user-dir'.
Reads the directory directly so packages without a `package-alist'
entry are still surfaced.  A trailing `-<digits-and-dots>' version
suffix (as written by package.el) is stripped so the result is
comparable with `package-alist' keys."
  (when (file-directory-p package-user-dir)
    (thread-last (directory-files package-user-dir nil "\\`[^.]")
      (seq-filter (lambda (it) (file-directory-p (expand-file-name it package-user-dir))))
      (seq-map (lambda (it) (replace-regexp-in-string "-[0-9.]+\\'" "" it)))
      (seq-map #'intern)
      (seq-remove (lambda (it) (memq it builder-package-installed-exclude)))
      seq-uniq)))

(defun builder-package--declared-use-packages (&optional dir)
  "Return symbols declared via `use-package' in init source files.
Walks .el files under DIR (defaults to `user-emacs-directory'/lisp) by
`read'ing top-level forms and collecting the NAME from every
(use-package NAME ...) form, including those nested inside top-level
`progn'/`with-eval-after-load'/`eval-when-compile' wrappers.  Used to
augment `builder-package-unused' so packages loaded via `:load-path'
are not misclassified as unused."
  (let* ((dir (or dir (expand-file-name "lisp" user-emacs-directory)))
         (declared '())
         (walk nil))
    (setq walk
          (lambda (form)
            (when (consp form)
              (cond
               ((and (eq (car form) 'use-package) (symbolp (cadr form)))
                (push (cadr form) declared))
               ((memq (car form) '(progn prog1 prog2 eval-and-compile
                                        eval-when-compile with-eval-after-load
                                        when unless if))
                (dolist (sub (cdr form)) (funcall walk sub)))))))
    (when (file-directory-p dir)
      (dolist (file (directory-files-recursively dir "\\.el\\'"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case nil
              (while t (funcall walk (read (current-buffer))))
            (end-of-file nil)
            (invalid-read-syntax nil)))))
    (seq-uniq declared)))

;;;###autoload
(defun builder-package-unused (packages)
  "Return packages on disk under `package-user-dir' not reachable from PACKAGES.
The reachable set is `builder-package-deps-closure' of PACKAGES *plus*
every name declared via `use-package' in user init files (see
`builder-package--declared-use-packages') — this keeps `:load-path'
installs out of the unused list.  The installed set comes from
`builder-package--installed-on-disk'.  Called interactively, defaults
PACKAGES to `package-selected-packages' and echoes the result."
  (interactive (list package-selected-packages))
  (let* ((roots (seq-uniq (append packages
                                  (builder-package--declared-use-packages))))
         (needed (builder-package-deps-closure roots))
         (unused (seq-sort (lambda (a b) (string-lessp (symbol-name a)
						    (symbol-name b)))
			  (seq-remove (lambda (it) (memq it needed))
				      (builder-package--installed-on-disk)))))
    (when (called-interactively-p 'interactive)
      (if unused
          (message "%d unused under %s: %s"
                   (length unused) package-user-dir
                   (mapconcat #'symbol-name unused " "))
        (message "no unused packages under %s" package-user-dir)))
    unused))

(provide 'builder-elisp)
;;; builder-elisp.el ends here
