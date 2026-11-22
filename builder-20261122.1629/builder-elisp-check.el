;;; builder-elisp-check.el --- Structured, cached checks for Elisp packages -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Keywords: tools, lisp, convenience

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; `builder-elisp-check-package' runs check-parens, byte-compile, checkdoc,
;; package-lint, relint and builder's static analysis over a package and
;; returns structured findings (tool, file, line, severity, message)
;; instead of printing to the echo area, so agents and scripts can consume
;; the result directly.  It is the engine behind the `elisp_preflight'
;; MCP tool.
;;
;; Tuned for latency, since it runs in a loop while editing:
;;
;; - Everything runs in-process.  An `emacs --batch' child costs ~0.6s
;;   before it checks anything; only `:isolated' pays that.
;; - Each source file is read once into one scratch buffer shared by all
;;   buffer-based tools, without visiting the file (no mode hooks, no
;;   file-local-variable prompts, no buffer left behind).
;; - Findings are cached per file and tool, keyed on the file's mtime and
;;   size, so re-running after editing one file only re-checks that file.
;;   Byte-compile findings can depend on other files (macros, requires),
;;   so their key covers every source file in the package.
;; - package-lint and relint are required only when selected and present.
;; - Native compilation of loaded dependencies is disabled while
;;   byte-compiling.
;;
;; Nothing here leaves `.elc' files behind: the compiler writes to a
;; temporary file that is deleted afterwards.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'f)
(require 'builder-elisp)

(defvar checkdoc-diagnostic-buffer)
(defvar checkdoc-autofix-flag)
(defvar checkdoc-force-docstrings-flag)
(defvar checkdoc-spellcheck-documentation-flag)
(defvar package-lint-main-file)
(defvar native-comp-jit-compilation)
(declare-function relint-buffer "relint")
(declare-function package-lint-buffer "package-lint")

(defgroup builder-elisp-check nil
  "Structured checks for Elisp packages."
  :group 'builder-elisp
  :prefix "builder-elisp-check-")

(defconst builder-elisp-check-tools
  '(parens compile checkdoc package-lint relint static)
  "Checks run by `builder-elisp-check-package', in order.")

(defcustom builder-elisp-check-use-cache t
  "Non-nil means reuse findings for files unchanged since the last check."
  :type 'boolean
  :group 'builder-elisp-check)

(defcustom builder-elisp-check-spellcheck nil
  "Non-nil means checkdoc also spell-checks documentation (slow)."
  :type 'boolean
  :group 'builder-elisp-check)

(defvar builder-elisp-check--cache (make-hash-table :test #'equal)
  "Map (TOOL FILE) to (STAMP . FINDINGS) for `builder-elisp-check-package'.")

(defun builder-elisp-check-clear-cache ()
  "Forget all cached findings."
  (interactive)
  (clrhash builder-elisp-check--cache))

;;;; Findings

(defun builder-elisp-check--finding (tool file line severity message)
  "Return a finding plist for TOOL at FILE:LINE with SEVERITY and MESSAGE.
LINE is 0 when unknown."
  (list :tool (symbol-name tool)
        :file file
        :line (or line 0)
        :severity severity
        :message (string-trim (or message ""))))

(defun builder-elisp-check--error-p (finding)
  "Return non-nil if FINDING has error severity."
  (equal (plist-get finding :severity) "error"))

;;;; Source discovery and cache keys

(defun builder-elisp-check--sources (root)
  "Return the package source files under ROOT.
Honors `builder-elisp-package-path-resolvers' (pkg/, lisp/, src/ layouts)
and leaves out test files that sit next to the sources."
  (seq-remove
   (lambda (file)
     (let ((name (file-name-nondirectory file)))
       (or (string-prefix-p "test-" name)
           (string-match-p "-tests?\\.el\\'" name))))
   (builder-elisp-package--source-files root)))

(defun builder-elisp-check--stamp (file)
  "Return a cheap change stamp (mtime and size) for FILE."
  (when-let* ((attrs (file-attributes file)))
    (list (file-attribute-modification-time attrs)
          (file-attribute-size attrs))))

(defun builder-elisp-check--cached (key stamp fn)
  "Return FN's findings, cached under KEY while STAMP is unchanged.
Return (FINDINGS . HIT-P)."
  (let ((entry (and builder-elisp-check-use-cache
                    (gethash key builder-elisp-check--cache))))
    (if (and entry (equal (car entry) stamp))
        (cons (cdr entry) t)
      (let ((findings (funcall fn)))
        (when builder-elisp-check-use-cache
          (puthash key (cons stamp findings) builder-elisp-check--cache))
        (cons findings nil)))))

;;;; Checks.  Each returns a list of findings.

(defun builder-elisp-check--parens (file)
  "Return `check-parens' findings for the current buffer, which visits FILE."
  (save-excursion
    (goto-char (point-min))
    (condition-case err
        (progn (check-parens) nil)
      (error
       (list (builder-elisp-check--finding
              'parens file (line-number-at-pos (point)) "error"
              (error-message-string err)))))))

(defconst builder-elisp-check--compile-regexp
  "^\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\): \\(Warning\\|Error\\): \\(.*\\)$"
  "Match a positioned diagnostic in the byte-compile log.")

(defun builder-elisp-check--compile (file root)
  "Return byte-compile findings for FILE, resolving requires against ROOT.
The compiler writes to a temporary file that is deleted afterwards."
  (let* ((tmp (make-temp-file "builder-check-" nil ".elc"))
         (log "*builder-check-compile-log*")
         (byte-compile-log-buffer log)
         (byte-compile-dest-file-function (lambda (_) tmp))
         (byte-compile-warnings
          (if (memq 'docstrings-wide (bound-and-true-p byte-compile-warning-types))
              (cons 'docstrings-wide builder-elisp-byte-compile-warnings)
            builder-elisp-byte-compile-warnings))
         (native-comp-jit-compilation nil)
         (load-path (append (builder-elisp-package-directories root) (list root) load-path))
         findings ok)
    (when (get-buffer log) (kill-buffer log))
    (unwind-protect
        (progn
          (setq ok (let ((inhibit-message t)) (byte-compile-file file)))
          (when (get-buffer log)
            (with-current-buffer log
              (goto-char (point-min))
              (while (re-search-forward builder-elisp-check--compile-regexp nil t)
                (push (builder-elisp-check--finding
                       'compile file (string-to-number (match-string 2))
                       (if (equal (match-string 4) "Error") "error" "warning")
                       (match-string 5))
                      findings))))
          (unless (or ok (seq-some #'builder-elisp-check--error-p findings))
            (push (builder-elisp-check--finding
                   'compile file 0 "error" "byte-compile-file failed")
                  findings)))
      (when (file-exists-p tmp) (delete-file tmp))
      (when (get-buffer log) (kill-buffer log)))
    (nreverse findings)))

(defconst builder-elisp-check--checkdoc-regexp
  "^\\(?:.+?\\):\\([0-9]+\\): \\(.*\\)$"
  "Match a checkdoc diagnostic line.")

(defun builder-elisp-check--checkdoc (file)
  "Return checkdoc findings for the current buffer, which visits FILE."
  (require 'checkdoc)
  (let ((diag "*builder-check-checkdoc*") findings)
    (when (get-buffer diag) (kill-buffer diag))
    (let ((checkdoc-diagnostic-buffer diag)
          (checkdoc-autofix-flag 'never)
          (checkdoc-force-docstrings-flag builder-elisp-checkdoc-force-docstrings-flag)
          (checkdoc-spellcheck-documentation-flag builder-elisp-check-spellcheck)
          (inhibit-message t))
      (save-excursion (checkdoc-current-buffer t)))
    (when (get-buffer diag)
      (with-current-buffer diag
        (goto-char (point-min))
        (while (re-search-forward builder-elisp-check--checkdoc-regexp nil t)
          (push (builder-elisp-check--finding
                 'checkdoc file (string-to-number (match-string 1)) "warning"
                 (match-string 2))
                findings)))
      (kill-buffer diag))
    (nreverse findings)))

(defun builder-elisp-check--package-lint (file main)
  "Return package-lint findings for the current buffer, which visits FILE.
Secondary files are linted against MAIN, the package's main file."
  (let ((package-lint-main-file (and main (not (f-equal-p main file)) main)))
    (mapcar (lambda (e)
              (builder-elisp-check--finding
               'package-lint file (nth 0 e)
               (if (eq (nth 2 e) 'error) "error" "warning")
               (nth 3 e)))
            (package-lint-buffer))))

(defun builder-elisp-check--relint (file)
  "Return relint findings for the current buffer, which visits FILE.
relint reports (MESSAGE POS END-POS ...); POS is a buffer position."
  (mapcar (lambda (e)
            (builder-elisp-check--finding
             'relint file
             (and (integerp (nth 1 e)) (line-number-at-pos (nth 1 e)))
             "warning" (nth 0 e)))
          (relint-buffer (current-buffer))))

(defun builder-elisp-check--static (file)
  "Return builder static-analysis findings for the current buffer (FILE)."
  (mapcar (lambda (msg)
            (builder-elisp-check--finding 'static file 0 "error" msg))
          (builder-elisp-lint-file (current-buffer))))

(defun builder-elisp-check--unavailable (tool)
  "Return nil if TOOL can run, else a string saying why it cannot."
  (pcase tool
    ('package-lint (unless (require 'package-lint nil t) "package-lint not installed"))
    ('relint (unless (require 'relint nil t) "relint not installed"))
    (_ nil)))

(defun builder-elisp-check--file-findings (file root main tools)
  "Return (FINDINGS . HITS) for FILE under ROOT, running TOOLS.
MAIN is the package's main file.  Compile findings are cached against
every source file in ROOT; the rest against FILE alone."
  (let ((stamp (builder-elisp-check--stamp file))
        (hits 0)
        findings
        (source nil))
    (cl-flet* ((buffer-run
                 (tool fn)
                 (let* ((r (builder-elisp-check--cached
                            (list tool file) stamp
                            (lambda ()
                              (unless source
                                (setq source (generate-new-buffer " *builder-check-source*"))
                                (with-current-buffer source
                                  (insert-file-contents file)
                                  (setq buffer-file-name file)
                                  (delay-mode-hooks (emacs-lisp-mode))
                                  (set-buffer-modified-p nil)))
                              (with-current-buffer source (funcall fn))))))
                   (when (cdr r) (cl-incf hits))
                   (car r))))
      (unwind-protect
          (let ((paren-errors (and (memq 'parens tools)
                                   (buffer-run 'parens (lambda () (builder-elisp-check--parens file))))))
            (setq findings paren-errors)
            ;; Unbalanced parens make every other check noise.
            (unless paren-errors
              (dolist (tool tools)
                (setq findings
                      (append
                       findings
                       (pcase tool
                         ('compile
                          (let ((r (builder-elisp-check--cached
                                    (list 'compile file)
                                    (mapcar #'builder-elisp-check--stamp
                                            (builder-elisp-check--sources root))
                                    (lambda () (builder-elisp-check--compile file root)))))
                            (when (cdr r) (cl-incf hits))
                            (car r)))
                         ('checkdoc (buffer-run 'checkdoc (lambda () (builder-elisp-check--checkdoc file))))
                         ('package-lint (buffer-run 'package-lint (lambda () (builder-elisp-check--package-lint file main))))
                         ('relint (buffer-run 'relint (lambda () (builder-elisp-check--relint file))))
                         ('static (buffer-run 'static (lambda () (builder-elisp-check--static file))))
                         (_ nil)))))))
        (when (buffer-live-p source) (kill-buffer source))))
    (cons findings hits)))

;;;; Entry point

;;;###autoload
(cl-defun builder-elisp-check-package (&optional root &key files tools)
  "Check the Elisp package at ROOT and return structured findings.
ROOT defaults to `default-directory'.  FILES overrides the discovered
source list (absolute or relative to ROOT).  TOOLS is a list of symbols
from `builder-elisp-check-tools' (default all).  Tools that are not
installed are listed in :skipped rather than failing the run.

Returns a plist: :status (\"ok\" or \"failed\"), :root, :files, :errors,
:warnings, :findings (vector of plists with :tool :file :line :severity
:message), :skipped (vector of strings), :cached (checks answered from
the cache) and :elapsed_ms.  Only errors fail the run."
  (let* ((start (float-time))
         (root (file-name-as-directory (expand-file-name (or root default-directory))))
         (main (builder-elisp-package--main-file root))
         (files (mapcar (lambda (f) (expand-file-name f root))
                        (or files (builder-elisp-check--sources root))))
         (tools (or tools builder-elisp-check-tools))
         (skipped nil)
         (active (seq-filter
                  (lambda (tool)
                    (if-let* ((why (builder-elisp-check--unavailable tool)))
                        (progn (push (format "%s: %s" tool why) skipped) nil)
                      t))
                  tools))
         (hits 0)
         findings)
    (dolist (file files)
      (pcase-let ((`(,found . ,n) (builder-elisp-check--file-findings file root main active)))
        (setq findings (append findings found))
        (cl-incf hits n)))
    (when (memq 'static active)
      (dolist (finding (builder-elisp-static-findings root nil))
        (setq findings
              (append findings
                      (list (builder-elisp-check--finding
                             'static root 0 "error" (plist-get finding :message)))))))
    (let ((errors (seq-count #'builder-elisp-check--error-p findings)))
      (list :status (if (zerop errors) "ok" "failed")
            :root root
            :files (length files)
            :errors errors
            :warnings (- (length findings) errors)
            :findings (vconcat findings)
            :skipped (vconcat (nreverse skipped))
            :cached hits
            :elapsed_ms (round (* 1000 (- (float-time) start)))))))

(provide 'builder-elisp-check)
;;; builder-elisp-check.el ends here
