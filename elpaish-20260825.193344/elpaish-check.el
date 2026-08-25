;;; elpaish-check.el --- Quality validation and preflight checks -*- lexical-binding: t -*-

;; Author: tychoish
;; Version: 0.1.0
;; Keywords: tools, lisp, test, lint, maintenance
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/tychoish/elpaish

;;; Commentary:
;; Provides comprehensive, isolated preflight quality validation for Emacs
;; Lisp packages.  Runs check-parens, checkdoc, package-lint, byte-compilation,
;; and ERT test suites, returning structured diagnostic results.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'ert)
(require 'seq)
(require 'subr-x)

(declare-function package-lint-buffer "package-lint")
(defvar ert--test-registry)

(defgroup elpaish-check nil
  "Preflight checks for Emacs Lisp packages."
  :group 'development)

(defun elpaish-check-buffer-name (&optional dir)
  "Return check compilation buffer name for DIR.
Formatted as `*<project-name>-checks*' based on `project-current' and `project-name'."
  (let* ((target-dir (expand-file-name (or dir default-directory)))
         (proj (and (fboundp 'project-current) (project-current nil target-dir)))
         (pname (or (and proj (fboundp 'project-name) (project-name proj))
                    (file-name-nondirectory (directory-file-name target-dir)))))
    (format "*%s-checks*" pname)))

(defcustom elpaish-check-buffer-name #'elpaish-check-buffer-name
  "Buffer name or function used for ELPAish package quality check output.
Defaults to `function:elpaish-check-buffer-name', formatting the buffer name as
`*<project-name>-checks*' based on `project-current' and `project-name'."
  :type '(choice (function-item :tag "Default (*<project-name>-checks*)" elpaish-check-buffer-name)
                 (string :tag "Fixed Buffer Name")
                 (function :tag "Custom Function"))
  :group 'elpaish-check)
(defun elpaish-check--get-buffer-name (&optional dir)
  "Resolve check compilation buffer name for DIR."
  (cond
   ((stringp elpaish-check-buffer-name) elpaish-check-buffer-name)
   ((functionp elpaish-check-buffer-name) (funcall elpaish-check-buffer-name dir))
   (t (elpaish-check-buffer-name dir))))

(defvar elpaish-check-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-mode-map)
    (define-key map (kbd "g") #'elpaish-check-recompile)
    (define-key map (kbd "r") #'elpaish-check-recompile)
    map)
  "Keymap for `elpaish-check-mode'.")

;;;###autoload
(defun elpaish-check-recompile (&optional _ignore-auto _noconfirm)
  "Recompile and re-run package quality check suite from the compilation buffer."
  (interactive)
  (elpaish-check-all default-directory))

;;;###autoload
(define-derived-mode elpaish-check-mode compilation-mode "ELPAish-Check"
  "Major mode for ELPAish package quality check and preflight logs.
\\{elpaish-check-mode-map}"
  (setq-local revert-buffer-function #'elpaish-check-recompile))

(defmacro elpaish-check-with-buffer (title &optional dir &rest body)
  "Execute BODY, directing check logs into check compilation buffer.
TITLE is a string describing the check operation.
DIR is the target directory, defaulting to `default-directory'."
  (declare (indent 2))
  `(let* ((target-dir (expand-file-name (or ,dir default-directory)))
          (buf-name (elpaish-check--get-buffer-name target-dir))
          (buf (get-buffer-create buf-name))
          (start-time (current-time))
          (op-title ,title))
     (with-current-buffer buf
       (let ((inhibit-read-only t))
         (erase-buffer)
         (unless (eq major-mode 'elpaish-check-mode)
           (elpaish-check-mode))
         (setq-local compilation-num-errors-found 0)
         (setq-local compilation-num-warnings-found 0)
         (setq-local default-directory target-dir)
         (insert (propertize (format "=== ELPAish Checks: %s ===\nDirectory: %s\nStarted: %s\n\n"
                                     op-title
                                     target-dir
                                     (format-time-string "%Y-%m-%d %H:%M:%S" start-time))
                             'face 'bold))))
     (when (and (not noninteractive) (called-interactively-p 'any))
       (display-buffer buf))
     (let ((res nil))
       (unwind-protect
           (progn
             (setq res (progn ,@body))
             (let ((passed (if (listp res) (plist-get res :passed) res)))
               (with-current-buffer buf
                 (let ((inhibit-read-only t)
                       (elapsed (float-time (time-subtract (current-time) start-time))))
                   (goto-char (point-max))
                   (insert (propertize
                            (format "\n=== Checks %s in %.2fs ===\n\n"
                                    (if passed "PASSED" "FAILED")
                                    elapsed)
                            'face (if passed 'bold 'compilation-error)))
                   (insert (if passed
                               (format "Compilation finished at %s\n"
                                       (format-time-string "%Y-%m-%d %H:%M:%S"))
                             (format "Compilation exited abnormally with code 1 at %s\n"
                                     (format-time-string "%Y-%m-%d %H:%M:%S"))))
                   (compilation-parse-errors (point-min) (point-max))
                   (run-hook-with-args 'compilation-finish-functions buf
                                       (if passed "finished\n" "exited abnormally with code 1\n")))))
             res)
         nil))))

(defun elpaish-check--log (verbose format-string &rest args)
  "Log formatted message using FORMAT-STRING and ARGS.
Writes to check buffer if active, and to `message' if VERBOSE."
  (let ((msg (apply #'format format-string args))
        (buf-name (elpaish-check--get-buffer-name)))
    (when-let* ((buf (get-buffer buf-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert msg "\n")
          (dolist (win (get-buffer-window-list buf nil t))
            (set-window-point win (point-max))))))
    (when verbose
      (message "%s" msg))))

(defun elpaish-check--find-package-files (dir)
  "Find main package .el files in DIR."
  (let* ((top-files (if (file-directory-p dir)
                        (directory-files dir t "\\.el\\'")
                      nil))
         (sub-dirs '("pkg" "lisp" "src"))
         (sub-files (seq-mapcat (lambda (sub)
                                  (let ((sdir (expand-file-name sub dir)))
                                    (if (file-directory-p sdir)
                                        (directory-files sdir t "\\.el\\'")
                                      nil)))
                                sub-dirs))
         (el-files (delete-dups (append top-files sub-files)))
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
                              (string-suffix-p "-autoloads.el" base)
                              ;; Exclude any -pkg.el descriptor unless it is this package's own code file <dir-name>.el
                              (and (string-suffix-p "-pkg.el" base)
                                   (not (string= base (format "%s.el" dir-name))))
                              (and (string= base "packages.el")
                                   (> (length el-files) 1)))))
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
  "Run `check-parens' on PKG-FILES.  VERBOSE enables logging.
Return list of error strings."
  (elpaish-check--log verbose "[elpaish-check] 1. Running check-parens (%d file(s))..." (length pkg-files))
  (let ((errs nil))
    (dolist (f pkg-files)
      (with-temp-buffer
        (insert-file-contents f)
        (emacs-lisp-mode)
        (condition-case err
            (check-parens)
          (error
           (let ((line (line-number-at-pos (point)))
                 (col (1+ (current-column)))
                 (rel (file-relative-name f default-directory)))
             (push (format "%s:%d:%d: error: check-parens: %s"
                           rel line col
                           (error-message-string err))
                   errs))))))
    (nreverse errs)))

(defun elpaish-check--checkdoc (pkg-file file-name verbose)
  "Run `checkdoc' on PKG-FILE with name FILE-NAME.  VERBOSE enables logging.
Return cons (ERRORS . WARNINGS)."
  (elpaish-check--log verbose "[elpaish-check] 2. Running checkdoc (%s)..." file-name)
  (let ((errs nil)
        (warns nil)
        (rel (file-relative-name pkg-file default-directory)))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents pkg-file)
          (emacs-lisp-mode)
          (setq-local checkdoc-spellcheck-documentation-flag nil)
          (setq-local checkdoc-create-error-function
                      (lambda (text start _end &optional _unfixable)
                        (let ((line (line-number-at-pos start))
                              (col (save-excursion (goto-char start) (1+ (current-column)))))
                          (push (format "%s:%d:%d: warning: checkdoc: %s" rel line col text) warns))
                        nil))
          (checkdoc-current-buffer t))
      (error
       (push (format "%s:1:1: error: checkdoc: %s" rel (error-message-string err)) errs)))
    (cons (nreverse errs) (nreverse warns))))

(defun elpaish-check--package-lint (pkg-file verbose)
  "Run `package-lint' on PKG-FILE.  VERBOSE enables logging.
Return cons (ERRORS . WARNINGS)."
  (elpaish-check--log verbose "[elpaish-check] 3. Running package-lint (%s)..."
                      (file-name-nondirectory pkg-file))
  (let ((errs nil)
        (warns nil)
        (rel (file-relative-name pkg-file default-directory)))
    (condition-case err
        (if (not (fboundp 'package-lint-buffer))
            (elpaish-check--log verbose "   - package-lint not installed, skipping.")
          (with-temp-buffer
            (insert-file-contents pkg-file)
            (emacs-lisp-mode)
            (let ((lint-res (package-lint-buffer)))
              (dolist (item lint-res)
                (let* ((line (nth 0 item))
                       (col (nth 1 item))
                       (type (nth 2 item))
                       (msg (nth 3 item))
                       (formatted (format "%s:%d:%d: %s: package-lint: %s" rel line col type msg)))
                  (if (eq type 'error)
                      (push formatted errs)
                    (push formatted warns)))))))
      (error
       (push (format "%s:1:1: error: package-lint execution error: %s" rel (error-message-string err)) errs)))
    (cons (nreverse errs) (nreverse warns))))

(defun elpaish-check--byte-compile (pkg-files verbose extra-load-path)
  "Byte-compile PKG-FILES with EXTRA-LOAD-PATH.  VERBOSE enables logging.
Return cons (ERRORS . WARNINGS)."
  (elpaish-check--log verbose "[elpaish-check] 4. Running byte-compilation (%d file(s))..." (length pkg-files))
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
          (let* ((raw-output (with-current-buffer byte-compile-log-buffer (buffer-string)))
                 (compile-output (replace-regexp-in-string "[\f]" "" raw-output)))
            (when (string-match-p "Error:" compile-output)
              (push (string-trim compile-output) errs))
            (when (and (string-match-p "Warning:" compile-output) (not (string-match-p "Error:" compile-output)))
              (push (string-trim compile-output) warns))))
      (when (buffer-live-p byte-compile-log-buffer)
        (kill-buffer byte-compile-log-buffer)))
    (cons (nreverse errs) (nreverse warns))))

(defun elpaish-check--ert (test-files pkg-name verbose extra-load-path &optional pkg-files)
  "Run ERT on TEST-FILES for PKG-NAME with EXTRA-LOAD-PATH silently.
PKG-FILES are loaded before running test files to ensure fresh definitions.
VERBOSE enables logging.  Return list of error strings."
  (elpaish-check--log verbose "[elpaish-check] 5. Running ERT tests (%d file(s))..." (length test-files))
  (let ((errs nil)
        (orig-registry (when (boundp 'ert--test-registry)
                         (copy-hash-table ert--test-registry)))
        (ert--test-registry (make-hash-table :test 'equal)))
    (unwind-protect
        (let ((load-prefer-newer t)
              (load-path (append extra-load-path load-path)))
          (dolist (pf pkg-files)
            (when (file-exists-p pf)
              (condition-case nil
                  (load pf nil t)
                (error nil))))
          ;; Load all test files
          (dolist (tf test-files)
            (condition-case err
                (load tf nil t)
              (error
               (push (format "%s:1:1: error: ERT load error: %s"
                             (file-relative-name tf default-directory)
                             (error-message-string err))
                     errs))))
          ;; Run ERT test suite once across the package
          (let* ((selector (format "\\`%s" (regexp-quote pkg-name)))
                 (stats (ert-run-tests selector (lambda (_event-type &rest _args) nil)))
                 (failed (if stats (ert-stats-completed-unexpected stats) 0)))
            (when (> failed 0)
              (let* ((tests (and stats (ert--stats-tests stats)))
                     (results (and stats (ert--stats-test-results stats)))
                     (len (if tests (length tests) 0)))
                (dotimes (i len)
                  (let* ((tst (aref tests i))
                         (res (aref results i)))
                    (when (ert-test-failed-p res)
                      (let* ((t-file (or (ert-test-file-name tst) (car test-files)))
                             (tf-rel (file-relative-name t-file default-directory)))
                        (push (format "%s:1:1: error: ERT test '%s' failed: %s"
                                      tf-rel
                                      (ert-test-name tst)
                                      (ert-test-failed-condition res))
                              errs)))))))))
      (when (and orig-registry (boundp 'ert--test-registry))
        (setq ert--test-registry orig-registry)))
    (nreverse errs)))

;;;###autoload
(cl-defun elpaish-check-package (&optional dir &key main-file test-dir skip-checks verbose
                                           extra-load-path)
  "Execute preflight quality check suite for package located at DIR.
MAIN-FILE explicitly overrides main file detection.
TEST-DIR explicitly specifies directory containing ERT test files.
SKIP-CHECKS is a list of check symbols to bypass, or t to skip all.
Supported check symbols: `parens', `checkdoc', `package-lint',
`byte-compile', `ert'.
VERBOSE is toggles extra output.
EXTRA-LOAD-PATH is a list of directories added to `load-path' during
byte-compilation and tests."
  (let* ((package-dir (expand-file-name (or dir default-directory)))
         (default-directory package-dir)
         (load-prefer-newer t)
         (dir-pkg-name (file-name-nondirectory (directory-file-name package-dir)))
         (pkg-files (if main-file
                        (list (expand-file-name main-file package-dir))
                      (elpaish-check--find-package-files package-dir)))
         (pkg-file (or (and main-file (expand-file-name main-file package-dir))
                       (seq-find (lambda (f)
                                   (string= (file-name-sans-extension (file-name-nondirectory f))
                                            dir-pkg-name))
                                 pkg-files)
                       (car pkg-files)))
         (pkg-name-str (or (and pkg-file (file-name-sans-extension (file-name-nondirectory pkg-file)))
                           dir-pkg-name))
         (recipe-skip (and (boundp 'elpaish-registry)
                           (hash-table-p elpaish-registry)
                           (when-let* ((rec (gethash pkg-name-str elpaish-registry)))
                             (elpaish-recipe-preflight-skip rec))))
         (effective-skip (or skip-checks recipe-skip))
         (skip-list (if (listp effective-skip) effective-skip (if effective-skip '(all) nil)))
         (test-files (elpaish-check--find-test-files package-dir test-dir))
         (pkg-load-dirs (delete-dups (cons package-dir extra-load-path)))
         (all-errors nil)
         (all-warnings nil))
    (unless (or (memq 'all skip-list) (null pkg-files))
      (when (fboundp 'elpaish-install-ensure-package-dependencies)
        (elpaish-install-ensure-package-dependencies package-dir))
      ;; 1. Check Parens
      (unless (memq 'parens skip-list)
        (let ((errs (elpaish-check--check-parens pkg-files verbose)))
          (if errs
              (dolist (e errs)
                (elpaish-check--log verbose "%s" e))
            (elpaish-check--log verbose "   ✓ check-parens passed (%d file(s))" (length pkg-files)))
          (setq all-errors (append all-errors errs))))

      ;; 2. Checkdoc
      (unless (or (memq 'checkdoc skip-list) (null pkg-file))
        (let ((res (elpaish-check--checkdoc pkg-file (file-name-nondirectory pkg-file) verbose)))
          (if (or (car res) (cdr res))
              (progn
                (dolist (e (car res))
                  (elpaish-check--log verbose "%s" e))
                (dolist (w (cdr res))
                  (elpaish-check--log verbose "%s" w)))
            (elpaish-check--log verbose "   ✓ checkdoc passed"))
          (setq all-errors (append all-errors (car res)))
          (setq all-warnings (append all-warnings (cdr res)))))

      ;; 3. Package Lint
      (unless (or (memq 'package-lint skip-list) (null pkg-file))
        (let ((res (elpaish-check--package-lint pkg-file verbose)))
          (if (or (car res) (cdr res))
              (progn
                (dolist (e (car res))
                  (elpaish-check--log verbose "%s" e))
                (dolist (w (cdr res))
                  (elpaish-check--log verbose "%s" w)))
            (when (fboundp 'package-lint-buffer)
              (elpaish-check--log verbose "   ✓ package-lint passed")))
          (setq all-errors (append all-errors (car res)))
          (setq all-warnings (append all-warnings (cdr res)))))

      ;; 4. Byte Compile
      (unless (memq 'byte-compile skip-list)
        (let ((res (elpaish-check--byte-compile pkg-files verbose pkg-load-dirs)))
          (if (or (car res) (cdr res))
              (progn
                (dolist (e (car res))
                  (elpaish-check--log verbose "%s" e))
                (dolist (w (cdr res))
                  (elpaish-check--log verbose "%s" w)))
            (elpaish-check--log verbose "   ✓ byte-compilation passed (%d file(s))" (length pkg-files)))
          (setq all-errors (append all-errors (car res)))
          (setq all-warnings (append all-warnings (cdr res)))))

      ;; 5. ERT Test Suite
      (unless (or (memq 'ert skip-list) (null test-files))
        (let ((errs (elpaish-check--ert test-files pkg-name-str verbose pkg-load-dirs pkg-files)))
          (if errs
              (dolist (e errs)
                (elpaish-check--log verbose "%s" e))
            (elpaish-check--log verbose "   ✓ ERT test suite passed (%d file(s))" (length test-files)))
          (setq all-errors (append all-errors errs)))))

    (let* ((passed (null all-errors))
           (result (list :passed passed
                         :package pkg-name-str
                         :errors all-errors
                         :warnings all-warnings)))
      (if passed
          (elpaish-check--log verbose "[elpaish-check] ✓ Preflight passed for %s" pkg-name-str)
        (elpaish-check--log verbose "[elpaish-check] ✗ Preflight FAILED for %s (%d error(s))"
                            pkg-name-str (length all-errors)))
      result)))

;;;###autoload
(defun elpaish-check-all (&optional dir)
  "Execute preflight quality check suite for DIR (defaults to `default-directory').
Pipes all output into `function:elpaish-check-buffer-name' compilation buffer."
  (interactive)
  (let* ((target-dir (expand-file-name (or dir default-directory)))
         (pkg-name-str (file-name-nondirectory (directory-file-name target-dir))))
    (elpaish-check-with-buffer pkg-name-str target-dir
      (let* ((res (elpaish-check-package target-dir :verbose t))
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
        passed))))

;;; Project Builder Integration

;;;###autoload
(defun elpaish-check-project-has-el-files-p (&optional dir)
  "Return non-nil when DIR or its project root has Emacs Lisp (.el) files."
  (let* ((target-dir (expand-file-name (or dir default-directory)))
         (proj (and (fboundp 'project-current) (project-current nil target-dir)))
         (proj-root (if proj
                        (if (fboundp 'project-root)
                            (project-root proj)
                          (with-no-warnings (cdr proj)))
                      target-dir)))
    (or (and (file-directory-p target-dir)
             (directory-files target-dir nil "\\.el\\'"))
        (and (file-directory-p proj-root)
             (or (directory-files proj-root nil "\\.el\\'")
                 (directory-files-recursively proj-root "\\.el\\'" nil
                                              (lambda (d)
                                                (not (string-prefix-p "." (file-name-nondirectory d))))))))))

;;;###autoload
(defun elpaish-check-setup-compile-command (&optional dir)
  "Set buffer-local `compile-command' to run `elpaish-run-checks' for DIR.
Only applies when the current buffer or DIR belongs to a project with .el files."
  (interactive)
  (let* ((target-dir (expand-file-name (or dir default-directory)))
         (proj (and (fboundp 'project-current) (project-current nil target-dir)))
         (root-dir (if proj
                       (if (fboundp 'project-root)
                           (project-root proj)
                         (with-no-warnings (cdr proj)))
                     target-dir)))
    (when (or (derived-mode-p 'emacs-lisp-mode)
              (elpaish-check-project-has-el-files-p root-dir))
      (setq-local compile-command
                  (format "emacsclient --eval \"(elpaish-run-checks %S)\""
                          (directory-file-name root-dir))))))
;;;###autoload
(defun elpaish-check-maybe-setup-builder ()
  "Configure `compile-command' to run `elpaish-run-checks' when opening .el files."
  (when (and buffer-file-name
             (or (string-suffix-p ".el" buffer-file-name)
                 (derived-mode-p 'emacs-lisp-mode)
                 (elpaish-check-project-has-el-files-p)))
    (elpaish-check-setup-compile-command)))

;;;###autoload
(defun elpaish-check-enable-builder ()
  "Enable `elpaish-run-checks' as the default build command for projects with .el files."
  (interactive)
  (add-hook 'emacs-lisp-mode-hook #'elpaish-check-setup-compile-command)
  (add-hook 'find-file-hook #'elpaish-check-maybe-setup-builder)
  (message "ELPAish builder command enabled for Emacs Lisp files."))

(provide 'elpaish-check)

;; Local Variables:
;; package-lint-main-file: "pkg/elpaish.el"
;; End:
;;; elpaish-check.el ends here

