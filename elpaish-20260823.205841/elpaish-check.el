;;; elpaish-check.el --- Quality validation and preflight checks for Emacs packages -*- lexical-binding: t -*-

;; Author: tychoish
;; Keywords: tools, lisp, test, lint, maintenance
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Provides comprehensive, isolated preflight quality validation for Emacs
;; Lisp packages.  Runs check-parens, checkdoc, package-lint, byte-compilation,
;; and ERT test suites, returning structured diagnostic results.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'subr-x)

(defgroup elpaish-check nil
  "Preflight checks for Emacs Lisp packages."
  :group 'development)

(defun elpaish-check--find-package-files (dir)
  "Find main package .el files in DIR."
  (let* ((el-files (if (file-directory-p dir)
                       (directory-files dir t "\\.el\\'")
                     nil))
         (dir-name (file-name-nondirectory (directory-file-name dir)))
         (candidates
          (seq-remove (lambda (f)
                        (let ((base (file-name-nondirectory f)))
                          (or (string-prefix-p "." base)
                              (string-prefix-p "#" base)
                              (string-prefix-p ".#" base)
                              (string-prefix-p "test-" base)
                              (string-suffix-p "-test.el" base)
                              (string-suffix-p "-tests.el" base)
                              (string-suffix-p "-spec.el" base)
                              (string= base "run-checks.el")
                              (string= base "elpaish-check.el")
                              (string-suffix-p "-autoloads.el" base)
                              ;; Exclude generated <pkg-dir>-pkg.el descriptor if main <pkg-dir>.el exists
                              (and (string= base (format "%s-pkg.el" dir-name))
                                   (file-exists-p (expand-file-name (format "%s.el" dir-name) dir))))))
                      el-files)))
    candidates))

(defun elpaish-check--find-test-files (dir &optional custom-test-dir)
  "Find ERT test files in DIR or CUSTOM-TEST-DIR."
  (let ((test-subdir (or (and custom-test-dir (expand-file-name custom-test-dir dir))
                         (let ((t1 (expand-file-name "test" dir))
                               (t2 (expand-file-name "tests" dir)))
                           (cond ((file-directory-p t1) t1)
                                 ((file-directory-p t2) t2)
                                 (t nil))))))
    (if (and test-subdir (file-directory-p test-subdir))
        (directory-files test-subdir t "\\.el\\'")
      (let ((el-files (if (file-directory-p dir)
                          (directory-files dir t "\\.el\\'")
                        nil)))
        (seq-filter (lambda (f)
                      (let ((base (file-name-nondirectory f)))
                        (or (string-prefix-p "test-" base)
                            (string-suffix-p "-test.el" base)
                            (string-suffix-p "-tests.el" base)
                            (string-suffix-p "-spec.el" base)
                            (string-match-p "\\`test" base)
                            (string-match-p "spec" base))))
                    el-files)))))

(defun elpaish-check--check-parens (pkg-files verbose)
  "Run check-parens on PKG-FILES. Returns list of error strings."
  (when verbose (message "[elpaish-check] 1. Running check-parens..."))
  (let ((errs nil))
    (dolist (f pkg-files)
      (with-temp-buffer
        (insert-file-contents f)
        (emacs-lisp-mode)
        (condition-case err
            (check-parens)
          (error
           (push (format "check-parens (%s): %s"
                         (file-name-nondirectory f)
                         (error-message-string err))
                 errs)))))
    (nreverse errs)))

(defun elpaish-check--checkdoc (pkg-file file-name verbose)
  "Run checkdoc on PKG-FILE. Returns cons (ERRORS . WARNINGS)."
  (when verbose (message "[elpaish-check] 2. Running checkdoc..."))
  (let ((errs nil)
        (warns nil))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents pkg-file)
          (emacs-lisp-mode)
          (setq-local checkdoc-create-error-function
                      (lambda (text start end &optional unfixable)
                        (push (format "checkdoc (%s): %s" file-name text) warns)
                        nil))
          (checkdoc-current-buffer t))
      (error
       (push (format "checkdoc error (%s): %s" file-name (error-message-string err)) errs)))
    (cons (nreverse errs) (nreverse warns))))

(defun elpaish-check--package-lint (pkg-file verbose)
  "Run package-lint on PKG-FILE. Returns cons (ERRORS . WARNINGS)."
  (when verbose (message "[elpaish-check] 3. Running package-lint..."))
  (let ((errs nil)
        (warns nil))
    (condition-case err
        (if (not (fboundp 'package-lint-buffer))
            (when verbose
              (message "[elpaish-check] package-lint not installed, skipping."))
          (with-temp-buffer
            (insert-file-contents pkg-file)
            (emacs-lisp-mode)
            (let ((lint-res (package-lint-buffer)))
              (dolist (item lint-res)
                (let* ((line (nth 0 item))
                       (col (nth 1 item))
                       (type (nth 2 item))
                       (msg (nth 3 item))
                       (formatted (format "package-lint [%s:%d:%d]: %s" type line col msg)))
                  (if (eq type 'error)
                      (push formatted errs)
                    (push formatted warns)))))))
      (error
       (push (format "package-lint execution error: %s" (error-message-string err)) errs)))
    (cons (nreverse errs) (nreverse warns))))

(defun elpaish-check--byte-compile (pkg-files verbose extra-load-path)
  "Byte-compile PKG-FILES with EXTRA-LOAD-PATH. Returns cons (ERRORS . WARNINGS)."
  (when verbose (message "[elpaish-check] 4. Running byte-compilation..."))
  (let ((errs nil)
        (warns nil)
        (byte-compile-log-buffer (generate-new-buffer " *elpaish-check-compile-log*")))
    (unwind-protect
        (progn
          (let ((byte-compile-log-buffer byte-compile-log-buffer)
                (byte-compile-dest-file-function (lambda (_) (make-temp-file "elpaish-check-elc-")))
                (load-path (append extra-load-path load-path)))
            (dolist (f pkg-files)
              (byte-compile-file f)))
          (let ((compile-output (with-current-buffer byte-compile-log-buffer (buffer-string))))
            (when (string-match-p "Error:" compile-output)
              (push (format "byte-compile errors:\n%s" compile-output) errs))
            (when (and (string-match-p "Warning:" compile-output) (not (string-match-p "Error:" compile-output)))
              (push (format "byte-compile warnings:\n%s" compile-output) warns))))
      (when (buffer-live-p byte-compile-log-buffer)
        (kill-buffer byte-compile-log-buffer)))
    (cons (nreverse errs) (nreverse warns))))

(defun elpaish-check--ert (test-files pkg-name verbose extra-load-path)
  "Run ERT on TEST-FILES for PKG-NAME with EXTRA-LOAD-PATH silently without popping up buffers.
Returns list of error strings."
  (when verbose (message "[elpaish-check] 5. Running ERT tests (%d files)..." (length test-files)))
  (let ((errs nil)
        (orig-registry (when (boundp 'ert--test-registry)
                         (copy-hash-table ert--test-registry))))
    (dolist (tf test-files)
      (condition-case err
          (let ((load-path (append extra-load-path load-path)))
            (load tf nil t)
            (let* ((selector (format "\\`%s" (regexp-quote pkg-name)))
                   (stats (ert-run-tests (ert-select-tests selector t)
                                         (lambda (_event-type &rest _args) nil)))
                   (failed (if stats (ert-stats-completed-unexpected stats) 0)))
              (when (> failed 0)
                (push (format "ERT: %d test(s) failed in %s" failed (file-name-nondirectory tf)) errs))))
        (error
         (push (format "ERT execution error in %s: %s"
                       (file-name-nondirectory tf)
                       (error-message-string err))
               errs))))
    (when (and orig-registry (boundp 'ert--test-registry))
      (setq ert--test-registry orig-registry))
    (nreverse errs)))
;;;###autoload
(cl-defun elpaish-check-package (&optional dir &key main-file test-dir skip-checks verbose
                                           extra-load-path)
  "Run preflight quality checks for package located at DIR.
MAIN-FILE explicitly overrides main file detection.
TEST-DIR explicitly specifies directory containing ERT test files.
SKIP-CHECKS is a list of check symbols to bypass, or t to skip all.
Supported check symbols: `parens', `checkdoc', `package-lint', `byte-compile', `ert'.
EXTRA-LOAD-PATH is a list of directories added to `load-path' during byte-compilation and tests.
VERBOSE prints progress messages to `message` buffer."
  (let* ((package-dir (expand-file-name (or dir default-directory)))
         (pkg-files (if main-file
                        (list (expand-file-name main-file package-dir))
                      (elpaish-check--find-package-files package-dir)))
         (pkg-file (car pkg-files))
         (test-files (elpaish-check--find-test-files package-dir test-dir))
         (skip-list (if (listp skip-checks) skip-checks (if skip-checks '(all) nil)))
         (pkg-name-str (or (and pkg-file (file-name-sans-extension (file-name-nondirectory pkg-file)))
                           (file-name-nondirectory (directory-file-name package-dir))))
         (pkg-load-dirs (delete-dups (cons package-dir extra-load-path)))
         (all-errors nil)
         (all-warnings nil))

    (unless (or (memq 'all skip-list) (null pkg-files))
      ;; 1. Check Parens
      (unless (memq 'parens skip-list)
        (let ((errs (elpaish-check--check-parens pkg-files verbose)))
          (setq all-errors (append all-errors errs))))

      ;; 2. Checkdoc
      (unless (or (memq 'checkdoc skip-list) (null pkg-file))
        (let ((res (elpaish-check--checkdoc pkg-file (file-name-nondirectory pkg-file) verbose)))
          (setq all-errors (append all-errors (car res)))
          (setq all-warnings (append all-warnings (cdr res)))))

      ;; 3. Package Lint
      (unless (or (memq 'package-lint skip-list) (null pkg-file))
        (let ((res (elpaish-check--package-lint pkg-file verbose)))
          (setq all-errors (append all-errors (car res)))
          (setq all-warnings (append all-warnings (cdr res)))))

      ;; 4. Byte Compile
      (unless (memq 'byte-compile skip-list)
        (let ((res (elpaish-check--byte-compile pkg-files verbose pkg-load-dirs)))
          (setq all-errors (append all-errors (car res)))
          (setq all-warnings (append all-warnings (cdr res)))))

      ;; 5. ERT Test Suite
      (unless (or (memq 'ert skip-list) (null test-files))
        (let ((errs (elpaish-check--ert test-files pkg-name-str verbose pkg-load-dirs)))
          (setq all-errors (append all-errors errs)))))

    (let* ((passed (null all-errors))
           (result (list :passed passed
                         :package pkg-name-str
                         :errors all-errors
                         :warnings all-warnings)))
      (if passed
          (when verbose
            (message "[elpaish-check] ✓ Preflight passed for %s (%d warning(s))"
                     pkg-name-str (length all-warnings)))
        (message "[elpaish-check] ✗ Preflight FAILED for %s (%d error(s), %d warning(s))"
                 pkg-name-str (length all-errors) (length all-warnings))
        (dolist (e all-errors)
          (message "  - ERROR: %s" e)))
      result)))

;;;###autoload
(defun elpaish-check-all ()
  "Run preflight quality checks for current repository."
  (interactive)
  (let* ((res (elpaish-check-package default-directory :verbose t))
         (passed (plist-get res :passed))
         (pkg (plist-get res :package))
         (errs (plist-get res :errors))
         (warns (plist-get res :warnings)))
    (if passed
        (message "Preflight check PASSED for %s (%d warnings)" pkg (length warns))
      (message "Preflight check FAILED for %s (%d errors):\n%s"
               pkg
               (length errs)
               (mapconcat (lambda (e) (format "  * %s" e)) errs "\n")))
    passed))

;; Backwards compatibility aliases
(defalias 'run-checks-package #'elpaish-check-package)
(defalias 'run-checks--find-package-files #'elpaish-check--find-package-files)
(defalias 'run-checks--find-test-files #'elpaish-check--find-test-files)
(defalias 'run-checks--check-parens #'elpaish-check--check-parens)
(defalias 'run-checks--checkdoc #'elpaish-check--checkdoc)
(defalias 'run-checks--package-lint #'elpaish-check--package-lint)
(defalias 'run-checks--byte-compile #'elpaish-check--byte-compile)
(defalias 'run-checks--ert #'elpaish-check--ert)
(defalias 'acr-run-all-checks #'elpaish-check-all)

(provide 'elpaish-check)
(provide 'run-checks)
;;; elpaish-check.el ends here
