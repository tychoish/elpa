;;; builder-scaffold.el --- Scaffold Elisp package repositories and CI -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Keywords: tools, lisp, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Deterministic, side-effect-contained scaffolding for Emacs Lisp package
;; repositories, exposed as the `elisp_package_spinout' and `elisp_ci_setup'
;; MCP tools by `builder-mcp':
;;
;; - `builder-scaffold-package': .gitignore, a starter <pkg>.el, a starter
;;   test file and a CI workflow.
;; - `builder-scaffold-ci': generate .github/workflows/ci.yml, inferring
;;   dependencies from the Package-Requires header.
;;
;; Both share one workflow generator, `builder-scaffold-ci-workflow'.
;; Nothing here runs git or touches the network, writes never replace an
;; existing file unless asked, and every writer takes DRY-RUN.  Results
;; are plists whose collections are vectors, so they serialize directly
;; with `json-serialize'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lisp-mnt)

(defgroup builder-scaffold nil
  "Scaffold Elisp package repositories and CI workflows."
  :group 'builder
  :prefix "builder-scaffold-")

(defcustom builder-scaffold-author nil
  "Author name for generated package headers.
Nil means the variable `user-full-name'."
  :type '(choice (const :tag "user-full-name" nil) string)
  :group 'builder-scaffold)

(defcustom builder-scaffold-email nil
  "Author email for generated package headers.
Nil means the variable `user-mail-address'."
  :type '(choice (const :tag "user-mail-address" nil) string)
  :group 'builder-scaffold)

(defcustom builder-scaffold-github-owner nil
  "GitHub user or organization used for default package URLs.
Nil means the function `user-login-name'."
  :type '(choice (const :tag "user-login-name" nil) string)
  :group 'builder-scaffold)

(defcustom builder-scaffold-ci-action "tychoish/elisp-ci"
  "GitHub repository providing the composite CI actions.
The workflow uses ACTION (tests), ACTION/lint and ACTION/coverage."
  :type 'string
  :group 'builder-scaffold)

(defcustom builder-scaffold-ci-action-ref "main"
  "Git ref of `builder-scaffold-ci-action' that generated workflows use."
  :type 'string
  :group 'builder-scaffold)

(defcustom builder-scaffold-ci-emacs-versions '("29.4" "30.2")
  "Emacs versions in the generated CI matrix."
  :type '(repeat string)
  :group 'builder-scaffold)

(defconst builder-scaffold-builtin-packages
  '(emacs cl-lib subr-x seq pcase bytecomp ert map project)
  "Packages built into current Emacs that CI need not install.")

;;;; Helpers

(defun builder-scaffold--string (x)
  "Return X as a string (symbols are converted)."
  (if (symbolp x) (symbol-name x) x))

(defun builder-scaffold--read-file (file)
  "Return the contents of FILE, or nil if it does not exist."
  (when (file-exists-p file)
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

(defun builder-scaffold--write-file (file text)
  "Write TEXT to FILE, creating parent directories."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun builder-scaffold--owner ()
  "Return the GitHub owner for default URLs."
  (or builder-scaffold-github-owner (user-login-name)))

(defun builder-scaffold--files (directory)
  "Return top-level .el files in DIRECTORY."
  (directory-files directory t "\\`[^.#].*\\.el\\'"))

(defun builder-scaffold-package-requires (file)
  "Return the external dependency names in FILE's Package-Requires header.
Built-in packages (see `builder-scaffold-builtin-packages') are omitted."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (when-let* ((header (lm-header-multiline "Package-Requires"))
                  (requires (ignore-errors (read (mapconcat #'identity header " ")))))
        (seq-remove (lambda (name) (memq name builder-scaffold-builtin-packages))
                    (mapcar (lambda (item) (if (consp item) (car item) item))
                            requires))))))

(defun builder-scaffold--infer-dependencies (directory)
  "Return dependency names declared by the .el files in DIRECTORY."
  (delete-dups (mapcan #'builder-scaffold-package-requires
                       (builder-scaffold--files directory))))

;;;; CI workflow

(cl-defun builder-scaffold-ci-workflow (&key dependencies (with-lint t)
                                             (with-coverage t) (with-codecov t))
  "Return the text of a GitHub Actions CI workflow.
DEPENDENCIES is a list of package names (symbols or strings) the actions
install.  WITH-LINT, WITH-COVERAGE and WITH-CODECOV toggle steps; the
Codecov upload needs coverage."
  (let* ((deps (string-join (mapcar #'builder-scaffold--string dependencies) " "))
         (action builder-scaffold-ci-action)
         (ref builder-scaffold-ci-action-ref)
         (with (if (string-empty-p deps)
                   ""
                 (format "        with:\n          dependencies: \"%s\"\n" deps)))
         (coverage-with (format "        with:\n%s%s"
                                (if (string-empty-p deps)
                                    ""
                                  (format "          dependencies: \"%s\"\n" deps))
                                "          report-format: \"lcov\"\n          output-file: \"coverage/lcov.info\"\n")))
    (concat
     "name: CI\n\n"
     "on:\n  push:\n    branches: [main]\n  pull_request:\n\n"
     "concurrency:\n  group: ${{ github.workflow }}-${{ github.ref }}\n  cancel-in-progress: true\n\n"
     "jobs:\n  test:\n    runs-on: ubuntu-latest\n"
     "    container: silex/emacs:${{ matrix.emacs_version }}-ci\n"
     "    timeout-minutes: 5\n    strategy:\n      fail-fast: false\n      matrix:\n        emacs_version:\n"
     (mapconcat (lambda (v) (format "          - \"%s\"\n" v))
                builder-scaffold-ci-emacs-versions "")
     "\n    steps:\n      - uses: actions/checkout@v5\n\n"
     (when with-lint
       (format "      - name: Lint Source Code\n        uses: %s/lint@%s\n%s\n" action ref with))
     (format "      - name: Run ERT Tests\n        uses: %s@%s\n%s" action ref with)
     (when with-coverage
       (concat
        (format "\n      - name: Measure Code Coverage\n        uses: %s/coverage@%s\n%s" action ref coverage-with)
        (when with-codecov
          (concat "\n      - name: Upload Coverage to Codecov\n"
                  "        uses: codecov/codecov-action@v5\n"
                  "        if: ${{ !cancelled() }}\n"
                  "        with:\n          files: coverage/lcov.info\n          fail_ci_if_error: false\n")))))))

(cl-defun builder-scaffold-ci (directory &key dependencies (with-lint t)
                                         (with-coverage t) (with-codecov t)
                                         overwrite dry-run)
  "Generate .github/workflows/ci.yml for the package in DIRECTORY.
DEPENDENCIES default to the Package-Requires headers in DIRECTORY.
WITH-LINT, WITH-COVERAGE and WITH-CODECOV toggle workflow steps.  An
existing workflow is only replaced when OVERWRITE is non-nil; with
DRY-RUN nothing is written.  Returns a plist: :status (\"written\",
\"dry-run\" or \"exists\"), :path, :dependencies (vector of strings),
:workflow (generated text) and :existing (current text when present)."
  (let* ((dir (file-name-as-directory (expand-file-name directory)))
         (path (expand-file-name ".github/workflows/ci.yml" dir))
         (deps (mapcar #'builder-scaffold--string
                       (or dependencies (builder-scaffold--infer-dependencies dir))))
         (current (builder-scaffold--read-file path))
         (workflow (builder-scaffold-ci-workflow
                    :dependencies deps :with-lint with-lint
                    :with-coverage with-coverage :with-codecov with-codecov))
         (status (cond (dry-run "dry-run")
                       ((and current (not overwrite)) "exists")
                       (t (builder-scaffold--write-file path workflow) "written"))))
    (append (list :status status :path path
                  :dependencies (vconcat deps) :workflow workflow)
            (when current (list :existing current)))))

;;;; Package repository

(defun builder-scaffold--gitignore ()
  "Return the starter .gitignore text."
  "*.elc\n*-autoloads.el\n.deps/\n*~\n#*#\n.projectile-cache.eld\n")

(defun builder-scaffold--main-file (pkg summary url)
  "Return the starter <PKG>.el text with SUMMARY and URL in its header."
  (let ((author (or builder-scaffold-author user-full-name))
        (email (or builder-scaffold-email user-mail-address)))
    (format (concat ";;; %1$s.el --- %2$s -*- lexical-binding: t; -*-\n\n"
                    ";; Author: %3$s <%4$s>\n;; Maintainer: %3$s <%4$s>\n"
                    ";; Version: 0.1.0\n;; Package-Requires: ((emacs \"29.1\"))\n"
                    ";; URL: %5$s\n\n;; This file is not part of GNU Emacs.\n\n"
                    ";;; Commentary:\n;;\n;; Description and primary usage.\n\n"
                    ";;; Code:\n\n(provide '%1$s)\n;;; %1$s.el ends here\n")
            pkg (or summary "One line description") author email
            (or url (format "https://github.com/%s/%s" (builder-scaffold--owner) pkg)))))

(defun builder-scaffold--test-file (pkg)
  "Return the starter test/test-<PKG>.el text."
  (format (concat ";;; test-%1$s.el --- Tests for %1$s -*- lexical-binding: t; -*-\n\n"
                  ";;; Code:\n\n(require 'ert)\n(require '%1$s)\n\n"
                  "(ert-deftest %1$s-test-loads ()\n  \"The package provides its feature.\"\n"
                  "  (should (featurep '%1$s)))\n\n"
                  "(provide 'test-%1$s)\n;;; test-%1$s.el ends here\n")
          pkg))

(cl-defun builder-scaffold-package (package &key directory summary url
                                            dependencies dry-run)
  "Scaffold a repository for PACKAGE in DIRECTORY (default ~/src/PACKAGE).
Creates .gitignore, <PACKAGE>.el, test/test-<PACKAGE>.el and a CI
workflow, skipping any file that already exists.  SUMMARY and URL feed
the starter header; DEPENDENCIES go into the CI workflow.  With DRY-RUN,
write nothing and report what would be created.  Returns a plist with
:package :directory :dry_run :created (vector of paths) and :existing
\(vector of paths).  Never runs git or touches the network."
  (let* ((pkg (builder-scaffold--string package))
         (dir (file-name-as-directory
               (expand-file-name (or directory (format "~/src/%s" pkg)))))
         (ci (expand-file-name ".github/workflows/ci.yml" dir))
         (files `((,(expand-file-name ".gitignore" dir) . ,(builder-scaffold--gitignore))
                  (,(expand-file-name (format "%s.el" pkg) dir)
                   . ,(builder-scaffold--main-file pkg summary url))
                  (,(expand-file-name (format "test/test-%s.el" pkg) dir)
                   . ,(builder-scaffold--test-file pkg))
                  (,ci . ,(builder-scaffold-ci-workflow
                           :dependencies dependencies))))
         (existing (seq-filter #'file-exists-p (mapcar #'car files)))
         (created (seq-remove (lambda (p) (member p existing)) (mapcar #'car files))))
    (unless dry-run
      (dolist (file files)
        (unless (member (car file) existing)
          (builder-scaffold--write-file (car file) (cdr file)))))
    (list :package pkg
          :directory dir
          :dry_run (if dry-run t :json-false)
          :created (vconcat created)
          :existing (vconcat existing))))

(provide 'builder-scaffold)
;;; builder-scaffold.el ends here
