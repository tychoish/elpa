;;; builder-isolate-elisp.el --- Isolated subprocess and daemon check runners -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (f "0.20.0"))
;; Keywords: tools, compilation

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Runs byte-compile, load, and test checks in isolated subprocesses (`emacs --batch')
;; or persistent daemons (via `sprite').  Subprocess checks catch missing `require'
;; statements; daemon checks catch session state pollution.

;;; Code:

(require 'compile)
(require 'ert)
(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'f)
(require 'builder-core)
(require 'builder-elisp)

(declare-function alert "alert" (message &rest args))
(declare-function async-start-process "async" (name program finish-func &rest program-args))
(declare-function comp-el-to-eln-filename "comp-run" (filename &optional base-dir))
(declare-function sprite-name "sprite-daemon")
(defvar treesit-auto-install-grammar)
(declare-function sprite-get-or-create-next "sprite-daemon")
(declare-function sprite-direct-open "sprite-direct")
(declare-function sprite-direct-eval-non-blocking "sprite-direct")
(declare-function sprite-direct-promise-then "sprite-direct")

(defgroup builder-isolate-elisp nil
  "Isolated check and execution runners."
  :group 'builder
  :prefix "builder-isolate-")


(defun builder-run-ci-tests-docker ()
  "Run CI tests for the current project in a Docker container.
Detects whether a Cask file is present in the project root to
select the appropriate silex/emacs image and test invocation.
With Cask: uses the \"-ci-cask\" image variant and runs
\"cask install && cask exec ert-runner test/test-*.el\".
Without Cask: uses the \"-ci\" image variant matching .github/workflows/test.yml
and runs emacs in --fg-daemon mode with builder-emacs-conf-run-ci-tests."
  (interactive)
  (let* ((root (file-name-as-directory (approximate-project-root)))
         (project-name (file-name-nondirectory (directory-file-name root)))
         (has-cask (file-regular-p (f-join root "Cask")))
         (image (if has-cask
                    "silex/emacs:30.2-ci-cask"
                  "silex/emacs:30.2-ci"))
         (test-command (if has-cask
                           "cask install && cask exec ert-runner test/test-*.el"
                         "ln -sf /workspace ~/.emacs.d && find ~/.emacs.d -name '*.elc' -delete && emacs --fg-daemon --eval '(builder-emacs-conf-run-ci-tests)'"))
         (command (format "docker run --rm -v %s:/workspace -w /workspace %s bash -c %s"
                          (shell-quote-argument (directory-file-name root))
                          image
                          (shell-quote-argument test-command)))
         (buf-name (format "*%s-docker-ci*" project-name))
         (default-directory root))
    (compilation-start
     command
     'compilation-mode
     (compile-buffer-name buf-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs config -- CI tests, byte-compile checks, native-compile warm-up

;;;###autoload
(defun builder-emacs-conf-run-ci-tests (&optional timeout)
  "Discover and run all ERT tests under test/, then exit.
Intended for CI invocations via --fg-daemon --eval, where `daemonp'
returns t for the unnamed daemon.  Named persistent daemons (started via
--daemon=NAME, e.g. the tychoish/work/hud sprites) and plain interactive
sessions report other `daemonp' values, so this refuses to run --
and, critically, refuses to call `kill-emacs' -- anywhere else. Never
invoke this against a live daemon via emacsclient; use
`builder-emacs-conf-run-ci-tests-isolated' instead.
Installs a TIMEOUT-second kill guard (default 240) before running."
  (unless (eq (daemonp) t)
    (user-error "builder-emacs-conf-run-ci-tests: refusing to run outside an unnamed --fg-daemon CI process (daemonp = %S); this function calls `kill-emacs' unconditionally" (daemonp)))
  (let ((test-dir (expand-file-name "test" user-emacs-directory))
        (noninteractive t)
        ;; Never block CI on a tree-sitter grammar install prompt (Emacs 31+).
        (treesit-auto-install-grammar 'never))
    (add-to-list 'load-path test-dir)
    (load (expand-file-name "test-helper" test-dir) nil t)
    (run-with-timer (or timeout 240) nil (lambda () (kill-emacs 1)))
    (condition-case err
        (seq-do (lambda (file) (load file nil t))
                (directory-files test-dir t "\\`\\(?:test-.*\\|.*-test\\)\\.el\\'"))
      (error
       (message "builder-emacs-conf-run-ci-tests: error loading test files: %S" err)
       (kill-emacs 1)))
    ;; ert-run-tests-batch-and-exit requires noninteractive=t (--batch only).
    ;; In --fg-daemon mode we call ert-run-tests-batch directly and kill-emacs
    ;; ourselves based on the result.
    (let ((stats (ert-run-tests-batch t)))
      (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))))

;;;###autoload
(defun builder-emacs-conf-run-ci-tests-isolated ()
  "Run the CI test suite in a throwaway `emacs --fg-daemon' subprocess.
Mirrors the invocation in .github/workflows/test.yml.  Safe to call via
emacsclient against a live daemon: this only spawns the subprocess and
returns, so `builder-emacs-conf-run-ci-tests' -- which calls
`kill-emacs' unconditionally -- runs in its own disposable process
instead of the caller. For agent skills via emacsclient:
  emacsclient --eval \\='(builder-emacs-conf-run-ci-tests-isolated)\\='"
  (interactive)
  (let* ((root (expand-file-name user-emacs-directory))
         (default-directory root)
         (command "emacs --fg-daemon --eval '(builder-emacs-conf-run-ci-tests)'"))
    (compilation-start
     command
     'compilation-mode
     (compile-buffer-name "*dot-emacs-ci-tests*"))))

;;;###autoload
(defun builder-emacs-conf-clean-elc (&optional directory)
  "Delete all compiled .elc files under DIRECTORY (defaults to `user-emacs-directory`).
Matching the CI cleanup step in .github/workflows/test.yml."
  (interactive)
  (let* ((dir (expand-file-name (or directory user-emacs-directory)))
         (files (directory-files-recursively dir "\\.elc\\'")))
    (seq-do #'delete-file files)
    (message "builder: deleted %d .elc file(s) in %s" (length files) dir)))
;; Isolated checks: byte-compile / load / run tests somewhere other than
;; the calling process, so its already-loaded packages, advice, or
;; interactively-defined state can't mask a real problem.  Two
;; non-overlapping transports; do not layer one on the other:
;;
;;   - subprocess (`emacs --batch', below): a throwaway process with only
;;     lisp/ on `load-path'.  Catches a missing `require' -- there is no
;;     "something else already loaded it" to fall back on.  A missing
;;     `require' is still only a byte-compiler *warning*, so always check
;;     the log, not just the returned boolean.
;;
;;   - sprite (further below): a persistent, already-running daemon with
;;     the same init.el and loaded state as this session, so it does
;;     *not* catch a missing `require'.  What it catches is this session
;;     itself being the problem -- stale advice, a redefined function,
;;     leftover buffer-local state from an in-progress task.

(defun builder-emacs-conf--do-byte-compile-check (file)
  "Byte-compile FILE; return (cons SUCCESS-P LOG).
Strips a `no-byte-compile: t' local variable from a scratch copy first,
so files like `bootstrap.el' that opt out of the normal build are still
validated here.  LOG is only the portion of `*Compile-Log*' this call
appended, so repeated calls in one process (as sprite checks do) don't
return a prior call's leftovers.  Never signals; safe in any process."
  (let* ((scratch (make-temp-file "builder-emacs-conf-byte-compile-" nil ".el"))
         (elc (concat (file-name-sans-extension scratch) ".elc"))
         (log-buf (get-buffer "*Compile-Log*"))
         (log-start (when log-buf (with-current-buffer log-buf (point-max)))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (when (re-search-forward "-\\*-.*-\\*-" (line-end-position) t)
              (replace-match
               (replace-regexp-in-string
                "[ \t]*;[ \t]*no-byte-compile:[ \t]*t[ \t]*" ""
                (match-string 0))
               t t))
            (write-region (point-min) (point-max) scratch nil 'silent))
          (let* ((byte-compile-warnings (if (boundp 'builder-elisp-byte-compile-warnings)
                                            builder-elisp-byte-compile-warnings
                                          byte-compile-warnings))
                 (success (and (byte-compile-file scratch) t))
                 (log-buf (get-buffer "*Compile-Log*")))
            (cons success
                  (when log-buf
                    (with-current-buffer log-buf
                      (buffer-substring-no-properties (or log-start (point-min)) (point-max)))))))
      (when (file-exists-p elc)
        (delete-file elc))
      (delete-file scratch))))

(defun builder-emacs-conf--do-load-check (file)
  "Load FILE; return (cons SUCCESS-P MESSAGE), MESSAGE nil on success.
Never signals, unlike plain `load' -- both transports below need a value
back across a process/wire boundary, not a propagated backtrace."
  (condition-case err
      (progn (load file nil t) (cons t nil))
    (error (cons nil (error-message-string err)))))

(defun builder-emacs-conf--do-elisp-package-test-check (root)
  "Run ERT tests for the elisp package at ROOT; return (cons SUCCESS-P LOG).
Loads the package the same way `builder-elisp-package-test' does, then
runs only the tests those files just defined -- never
`ert-run-tests-batch-and-exit' (calls `kill-emacs', fatal to a shared
sprite daemon) and never \"every test the process knows about\" (would
pull in unrelated suites in a reused sprite)."
  (require 'ert)
  (pcase-let ((`(,root ,_name ,main-file) (builder-elisp-package--resolve root 'error)))
    (builder-elisp-package--require-deps (builder-elisp-package--read-deps root))
    (let* ((test-dir (f-join root "test"))
           (load-path (append (seq-filter #'identity (list root (and (file-directory-p test-dir) test-dir))) load-path))
           (known-before (ert-select-tests t t)))
      (load main-file nil t)
      (seq-do (lambda (it) (unless (f-equal-p it main-file) (load it nil t)))
              (builder-elisp-package--source-files root))
      (seq-do (lambda (it) (load it nil t)) (builder-elisp-package--test-files root))
      (let* ((new-tests (seq-difference (ert-select-tests t t) known-before))
             (stats (ert-run-tests-batch (and new-tests (cons 'member new-tests)))))
        (cons (zerop (ert-stats-completed-unexpected stats))
              (format "%d/%d tests passed%s"
                      (- (ert-stats-completed stats) (ert-stats-completed-unexpected stats))
                      (ert-stats-total stats)
                      (if (zerop (ert-stats-completed-unexpected stats))
                          ""
                        (format ", %d unexpected" (ert-stats-completed-unexpected stats)))))))))

(defun builder-emacs-conf--read-result-file (result-file fallback-string)
  "Read a (SUCCESS-P . LOG) cons written by `prin1' to RESULT-FILE.
`make-temp-file' creates RESULT-FILE empty, so existence alone can't
distinguish a written result from a child that crashed first -- checks
for actual content, falling back to (cons nil FALLBACK-STRING)."
  (if (> (file-attribute-size (file-attributes result-file)) 0)
      (with-temp-buffer
        (insert-file-contents result-file)
        (read (current-buffer)))
    (cons nil fallback-string)))

(defun builder-emacs-conf--report-log (buffer-name output)
  "Replace the contents of BUFFER-NAME with OUTPUT, creating it if needed.
Leaves an isolated check's log inspectable under a well-known buffer
name the way `*Compile-Log*' would be for an in-process compile; clears
`inhibit-read-only' since that buffer is normally read-only."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (or output "")))))

(defun builder-emacs-conf--builder-library-dir ()
  "Return directory containing `builder' if resolvable on `load-path'."
  (let ((lib (locate-library "builder")))
    (and lib (file-name-directory lib))))

;;;; async.el transport -- throwaway `emacs --batch' subprocess

(defun builder-emacs-conf--isolated-batch-eval (form)
  "Evaluate FORM in an isolated `emacs --batch' subprocess; return its value.
The child has lisp/ on `load-path' and `builder' loaded, but none of the
calling session's other packages, advice, or interactive state.  The
return value crosses via a scratch result file, not stdout/stderr (which
byte-compiler warnings also write to).  Returns (cons nil PROCESS-OUTPUT)
if the child never wrote a result -- e.g. FORM signaled outside its own
`condition-case', as the `do-*' checks above are careful not to."
  (let* ((result-file (make-temp-file "builder-isolated-result-"))
         (buf (generate-new-buffer " *builder-isolated-batch*"))
         (builder-dir (builder-emacs-conf--builder-library-dir)))
    (unwind-protect
        (progn
          (apply #'call-process "emacs" nil buf nil
                 (append (list "--batch" "-L" (expand-file-name "lisp" user-emacs-directory))
                         (when builder-dir (list "-L" builder-dir))
                         (when (and (boundp 'package-user-dir) package-user-dir)
                           (list "--eval" (format "(setq package-user-dir %S)" (expand-file-name package-user-dir))))
                         (list "-f" "package-initialize" "--eval" "(require 'builder)"
                               "--eval" (format "(with-temp-file %S (prin1 %S (current-buffer)))"
                                               result-file form))))
          (builder-emacs-conf--read-result-file
           result-file (with-current-buffer buf (buffer-string))))
      (kill-buffer buf)
      (when (file-exists-p result-file)
        (delete-file result-file)))))

;;;; Fast subprocess transport -- no `package-initialize'

(defun builder-emacs-conf--emacs-program ()
  "Return the path of the running Emacs executable, or \"emacs\"."
  (let ((exe (expand-file-name invocation-name invocation-directory)))
    (if (file-executable-p exe) exe "emacs")))

(defun builder-emacs-conf--fast-batch-eval (form)
  "Evaluate FORM in a minimal `emacs -Q --batch' subprocess; return its value.
Unlike `builder-emacs-conf--isolated-batch-eval', the child does not run
`package-initialize' (about 0.5s of startup): it is handed this session's
`load-path' instead, so every installed package can be required but none
is activated.  `builder-isolate-elisp' is loaded first, so FORM may call
the `builder-emacs-conf--do-*' functions.  The value crosses via a scratch
file; if the child never wrote one, return (:status \"error\" :error OUTPUT)."
  (let* ((result-file (make-temp-file "builder-fast-result-"))
         (buf (generate-new-buffer " *builder-fast-batch*")))
    (unwind-protect
        (progn
          (call-process (builder-emacs-conf--emacs-program) nil buf nil
                        "--batch" "-Q"
                        "--eval" (format "(setq load-path '%S)" load-path)
                        "--eval" "(require 'builder-isolate-elisp)"
                        "--eval" (format "(with-temp-file %S (prin1 %S (current-buffer)))"
                                         result-file form))
          (if (> (file-attribute-size (file-attributes result-file)) 0)
              (with-temp-buffer
                (insert-file-contents result-file)
                (read (current-buffer)))
            (list :status "error"
                  :error (with-current-buffer buf (string-trim (buffer-string))))))
      (kill-buffer buf)
      (when (file-exists-p result-file)
        (delete-file result-file)))))

(defun builder-emacs-conf--ert-result-condition (result)
  "Return a short string describing why ERT RESULT did not pass."
  (let ((text (if (ert-test-result-with-condition-p result)
                  (format "%S" (ert-test-result-with-condition-condition result))
                (format "%s" (type-of result)))))
    (if (> (length text) 400) (concat (substring text 0 400) "...") text)))

(defun builder-emacs-conf--do-package-test-report (root selector prefix glob)
  "Run ERT tests for the elisp package at ROOT and return a result plist.
Loads the package as `builder-emacs-conf--do-elisp-package-test-check'
does, then runs only the tests those files defined, narrowed by SELECTOR
\(a regexp), PREFIX (a name prefix) and GLOB (a wildcard pattern) when
non-nil.  Returns :status, :total, :passed, :failed, :skipped and
:failing (a vector of plists with :name and :condition).  Never signals."
  ;; `ert-run-tests' must run outside any `condition-case': before Emacs 30
  ;; ERT detects failures through the debugger, which an enclosing handler
  ;; for `error' would pre-empt, turning a failing test into an escaped signal.
  (let ((tests (condition-case err
                   (progn
                     (require 'ert)
                     (pcase-let ((`(,root ,_name ,main-file) (builder-elisp-package--resolve root 'error)))
                       (builder-elisp-package--require-deps (builder-elisp-package--read-deps root))
                       (let* ((test-dir (f-join root "test"))
                              (load-path (append (seq-filter #'identity
                                                             (list root (and (file-directory-p test-dir) test-dir)))
                                                 load-path))
                              (known-before (ert-select-tests t t))
                              (glob-re (and glob (wildcard-to-regexp glob))))
                         (load main-file nil t)
                         (seq-do (lambda (it) (unless (f-equal-p it main-file) (load it nil t)))
                                 (builder-elisp-package--source-files root))
                         (seq-do (lambda (it) (load it nil t)) (builder-elisp-package--test-files root))
                         (seq-filter
                          (lambda (test)
                            (let ((name (symbol-name (ert-test-name test))))
                              (and (or (null selector) (string-match-p selector name))
                                   (or (null prefix) (string-prefix-p prefix name))
                                   (or (null glob-re) (string-match-p glob-re name)))))
                          (seq-difference (ert-select-tests t t) known-before)))))
                 (error (error-message-string err)))))
    (if (stringp tests)
        (list :status "error" :error tests)
      (let ((stats (ert-run-tests (cons 'member tests) #'ignore))
            (failing nil))
        (cl-loop for test across (ert--stats-tests stats)
                 for result across (ert--stats-test-results stats)
                 unless (or (null result) (ert-test-result-expected-p test result))
                 do (push (list :name (symbol-name (ert-test-name test))
                                :condition (builder-emacs-conf--ert-result-condition result))
                          failing))
        (list :status (if (and tests (zerop (ert-stats-completed-unexpected stats))) "ok" "failed")
              :total (ert-stats-total stats)
              :passed (ert-stats-completed-expected stats)
              :failed (ert-stats-completed-unexpected stats)
              :skipped (ert-stats-skipped stats)
              :failing (vconcat (nreverse failing))
              :error (and (null tests) "no tests matched"))))))

;;;###autoload
(cl-defun builder-emacs-conf-package-test-report (&optional root &key selector prefix glob)
  "Run the elisp package at ROOT's ERT tests in a fast subprocess.
Returns a plist: :status (\"ok\", \"failed\" or \"error\"), :root,
:total, :passed, :failed, :skipped, :failing (vector of :name and
:condition plists), :error and :elapsed_ms.  SELECTOR is a test-name
regexp, PREFIX a name prefix and GLOB a wildcard pattern; all narrow the
run.  ROOT defaults to `approximate-project-root'.  The child starts
without `package-initialize', see `builder-emacs-conf--fast-batch-eval'."
  (let* ((root (file-name-as-directory
                (expand-file-name (or root (approximate-project-root)))))
         (start (float-time))
         (result (builder-emacs-conf--fast-batch-eval
                  (list 'builder-emacs-conf--do-package-test-report
                        root selector prefix glob))))
    (append (list :root root) result
            (list :elapsed_ms (round (* 1000 (- (float-time) start)))))))

;;;###autoload
(defun builder-emacs-conf-async-byte-compile-check (&optional file)
  "Byte-compile FILE in an isolated subprocess without blocking Emacs.
FILE defaults to the current buffer's file, resolved relative to
`user-emacs-directory'.  Use this over
`builder-emacs-conf-byte-compile-and-delete-artifact' for interactive
use, where blocking on the subprocess would freeze Emacs.  Reports
pass/fail with `alert' and leaves the compile log in `*Compile-Log*',
displaying that buffer on failure."
  (interactive (list (buffer-file-name)))
  (let* ((expanded (expand-file-name file user-emacs-directory))
         (result-file (make-temp-file "builder-isolated-result-"))
         (builder-dir (builder-emacs-conf--builder-library-dir)))
    (apply #'async-start-process
           "builder-async-byte-compile" "emacs"
           (lambda (proc)
             (let* ((result (builder-emacs-conf--read-result-file
                             result-file (with-current-buffer (process-buffer proc) (buffer-string))))
                    (success (car result)))
               (when (file-exists-p result-file)
                 (delete-file result-file))
               (builder-emacs-conf--report-log "*Compile-Log*" (cdr result))
               (alert (format "isolated byte-compile %s" (if success "passed" "FAILED"))
                      :title (format "builder-emacs-conf-async-byte-compile-check: %s" file))
               (if success
                   (kill-buffer (process-buffer proc))
                 (display-buffer "*Compile-Log*"))))
           (append (list "--batch" "-L" (expand-file-name "lisp" user-emacs-directory))
                   (when builder-dir (list "-L" builder-dir))
                   (when (and (boundp 'package-user-dir) package-user-dir)
                     (list "--eval" (format "(setq package-user-dir %S)" (expand-file-name package-user-dir))))
                   (list "-f" "package-initialize" "--eval" "(require 'builder)"
                         "--eval" (format "(with-temp-file %S (prin1 (builder-emacs-conf--do-byte-compile-check %S) (current-buffer)))"
                                          result-file expanded))))))

;;;; sprite transport -- persistent daemon, reached over sprite-direct

(cl-defun builder-emacs-conf-sprite-eval (form &key sprite callback)
  "Evaluate FORM in a persistent sprite; return a `sprite-direct-promise'.
Gets or creates a sprite via `sprite-get-or-create-next' (or uses SPRITE,
a `sprite' struct, when given).  Use this over the subprocess transport
above to rule out this session's own state -- stale advice, redefined
functions -- as the cause of a failure; it will not catch a missing
`require', since the sprite already has whatever this session has.  FORM
runs after `(require \\='builder)', so the `builder-emacs-conf--do-*'
checks can be called directly.  Never blocks; with CALLBACK, it's wired
via `sprite-direct-promise-then' and called with (STATE VALUE) once the
sprite responds -- without one, poll the promise or call
`sprite-direct-promise-wait'."
  (require 'sprite)
  (require 'sprite-daemon)
  (require 'sprite-direct)
  (let* ((target (sprite-name (or sprite (sprite-get-or-create-next))))
         (conn (sprite-direct-open target))
         (promise (sprite-direct-eval-non-blocking conn `(progn (require 'builder) ,form))))
    (when callback
      (sprite-direct-promise-then promise callback))
    promise))

(defun builder-emacs-conf--sprite-check-callback (buffer-name label)
  "Return a `sprite-direct-promise' callback for a builder isolated check.
Populates BUFFER-NAME with the log/message half of the check's (SUCCESS-P
. LOG) result and reports pass/fail with `alert', titled LABEL."
  (lambda (state value)
    (builder-emacs-conf--report-log buffer-name (cdr value))
    (alert (format "sprite check %s" (if (and (eq state :resolved) (car value)) "passed" "FAILED"))
           :title label)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; native compilation & vendored package maintenance

(defcustom builder--native-compile-chunk-size 25
  "Number of files to check for staleness per idle tick during incremental native compilation."
  :type 'integer
  :group 'builder-isolate-elisp)

(defcustom builder--native-compile-idle-delay 10
  "Idle seconds between successive chunks in `builder--native-compile-process-queue'.
Intentionally short (10s) for foreground incremental native-compilation queue
pickup after edits, unlike 300s+ background health-check sweeps."
  :type 'number
  :group 'builder-isolate-elisp)

(defvar builder--native-compile-queue nil
  "Remaining .el files still to be checked/compiled by `builder--native-compile-process-queue'.")

(defvar builder--native-compile-idle-timer nil
  "Idle timer driving `builder--native-compile-process-queue'.")

(defun builder--native-compile-stale-p (file)
  "Return non-nil if FILE has no up-to-date native-compiled .eln."
  (let ((eln (ignore-errors (comp-el-to-eln-filename file))))
    (or (not eln)
        (not (file-exists-p eln))
        (file-newer-than-file-p file eln))))

(defun builder--native-compile-process-queue ()
  "Check/dispatch the next chunk of `builder--native-compile-queue'.
Runs a bounded chunk per idle tick rather than staleness-checking and
dispatching the whole lisp/+elpa/ file list in one synchronous burst.
Only stale files (no up-to-date .eln) are queued for compilation.
Cancels itself and prunes the eln cache once the queue drains."
  (if (null builder--native-compile-queue)
      (when builder--native-compile-idle-timer
        (cancel-timer builder--native-compile-idle-timer)
        (setq builder--native-compile-idle-timer nil)
        (condition-case err
            (when (fboundp 'native-compile-prune-cache)
              (native-compile-prune-cache))
          (error (message "native-compile-prune-cache error: %s"
                          (error-message-string err)))))
    (let (chunk)
      (dotimes (_ builder--native-compile-chunk-size)
        (when builder--native-compile-queue
          (push (pop builder--native-compile-queue) chunk)))
      (when-let* ((stale (seq-filter #'builder--native-compile-stale-p chunk)))
        (native-compile-async stale)))))

(defun builder--emacs-conf-user-files ()
  "Return list of user configuration `.el' files in root, lisp/, and user/."
  (let* ((root-files (directory-files user-emacs-directory t "\\`[^.#].*\\.el\\'"))
         (lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (user-dir (expand-file-name "user" user-emacs-directory)))
    (seq-filter
     #'file-regular-p
     (append root-files
             (when (file-directory-p lisp-dir)
               (directory-files-recursively lisp-dir "\\`[^.#].*\\.el\\'"))
             (when (file-directory-p user-dir)
               (directory-files-recursively user-dir "\\`[^.#].*\\.el\\'"))))))

(defun builder--emacs-conf-candidate-files ()
  "Return list of candidate .el files across user Emacs configuration and packages."
  (let ((user-files (builder--emacs-conf-user-files))
        (elpa-dir (expand-file-name "elpa" user-emacs-directory)))
    (append user-files
            (when (file-directory-p elpa-dir)
              (seq-filter #'file-regular-p
                          (directory-files-recursively elpa-dir "\\`[^.#].*\\.el\\'"))))))

;;;###autoload
(defun builder-emacs-conf-native-compile-all ()
  "Incrementally native-compile stale .el files in configuration and packages.
Collects candidate files, then hands them to
`builder--native-compile-process-queue' to work through in bounded,
staleness-checked chunks on an idle timer. Runs once on an idle
timer after startup; also callable interactively for a fresh sweep."
  (interactive)
  (if (not (or (string-match "NATIVE_COMP" system-configuration-features)
               (fboundp 'native-compile-async)))
      (message "Native compilation not available in this build")
    (let ((files (builder--emacs-conf-candidate-files)))
      (setq builder--native-compile-queue files)
      (unless builder--native-compile-idle-timer
        (setq builder--native-compile-idle-timer
              (run-with-idle-timer builder--native-compile-idle-delay t
                                   #'builder--native-compile-process-queue)))
      (when (called-interactively-p 'interactive)
        (message "[builder]: queued %d .el files for incremental native compilation staleness check"
                 (length files))))))

(defun builder-native-compile-wait (&optional timeout)
  "Wait synchronously until all asynchronous native compilation tasks complete.
TIMEOUT is the maximum number of seconds to wait (defaults to 300)."
  (let ((start (current-time))
        (max-wait (or timeout 300)))
    (while (and (or (bound-and-true-p comp-files-queue)
                    (and (fboundp 'comp--async-runnings)
                         (> (comp--async-runnings) 0)))
                (< (float-time (time-since start)) max-wait))
      (sit-for 0.5))))

(defvar bootstrap-vendored-packages)

;;;###autoload
(defun builder-emacs-conf-recompile-all (&optional force wait)
  "Byte-recompile user configuration and native-compile all stale files.
When FORCE is non-nil (with prefix argument `\\[universal-argument]`),
force recompilation of all files even if `.elc' is up to date.
When WAIT is non-nil (with double prefix argument `\\[universal-argument] \\[universal-argument]`),
block until all background native compilation subprocesses finish.
Returns the list of files queued for native compilation."
  (interactive
   (list (and current-prefix-arg (>= (prefix-numeric-value current-prefix-arg) 4))
         (and current-prefix-arg (>= (prefix-numeric-value current-prefix-arg) 16))))
  (let* ((user-files (builder--emacs-conf-user-files))
         (recompiled (seq-keep (lambda (f)
                                 (unless (eq 'no-byte-compile
                                             (byte-recompile-file f force 0))
                                   f))
                               user-files))
         (all-candidates (builder--emacs-conf-candidate-files))
         (stale (when (and (string-match-p "NATIVE_COMP" system-configuration-features)
                           (fboundp 'native-compile-async))
                  (seq-filter #'builder--native-compile-stale-p all-candidates)))
         (to-native (delete-dups (append recompiled stale))))
    (when to-native
      (native-compile-async to-native))
    (message "[builder]: %d config files recompiled, %d queued for native compilation%s"
             (length recompiled)
             (length to-native)
             (if (and (fboundp 'comp--async-runnings) (> (comp--async-runnings) 0))
                 (format " (%d workers running)" (comp--async-runnings))
               ""))
    (when wait
      (message "[builder]: awaiting native compilation completion...")
      (builder-native-compile-wait)
      (when (fboundp 'native-compile-prune-cache)
        (native-compile-prune-cache))
      (message "[builder]: all byte and native compilation complete."))
    to-native))

;;;###autoload
(defalias 'builder-recompile-all #'builder-emacs-conf-recompile-all)

;;;###autoload
(defun builder-emacs-conf-byte-recompile-directory ()
  "Recompile all `.el' files in `user-emacs-directory' and its direct subdirectories.
With a prefix argument, force recompilation of every file regardless of timestamps.
Returns the list of files that were recompiled."
  (interactive)
  (let ((recompiled (thread-last (cons user-emacs-directory
                                       (seq-filter #'file-directory-p
                                                   (directory-files user-emacs-directory t "^[^.]")))
                                 (seq-mapcat (lambda (dir) (directory-files dir t "\\.el\\'")))
                                 (seq-filter #'file-regular-p)
                                 (seq-keep (lambda (f)
                                             (unless (eq 'no-byte-compile
                                                         (byte-recompile-file f current-prefix-arg))
                                               f))))))
    (when (and recompiled
               (string-match-p "NATIVE_COMP" system-configuration-features)
               (fboundp 'native-compile-async))
      (native-compile-async recompiled))
    recompiled))

;;;###autoload
(defun builder-emacs-conf-recompile-vendored-packages ()
  "Recompile all `.el' files in each `bootstrap-vendored-packages' checkout.
Only scans each package's top-level directory, not its test/ subdirectory
\(matching `builder-emacs-conf-byte-recompile-directory''s scope\). Useful
after `git pull'-ing one of these external/ checkouts, since
`bootstrap-package' only installs/compiles a package the first time it
sees the checkout and never notices later updates on its own.
With a prefix argument, force recompilation of every file regardless of
timestamps. Returns the list of files that were recompiled."
  (interactive)
  (thread-last bootstrap-vendored-packages
               (seq-map (lambda (spec) (expand-file-name (nth 1 spec) user-emacs-directory)))
               (seq-filter #'file-directory-p)
               (seq-mapcat (lambda (dir) (directory-files dir t "\\.el\\'")))
               (seq-filter #'file-regular-p)
               (seq-keep (lambda (f)
                           (unless (eq 'no-byte-compile
                                       (byte-recompile-file f current-prefix-arg))
                             f)))))

;;;###autoload
(defun builder-emacs-conf-generate-loaddefs (&optional package)
  "Regenerate the `*-autoloads.el' file for a vendored package's checkout.
PACKAGE is a symbol key into `bootstrap-vendored-packages'; interactively
prompts among them, or regenerates every checkout with a prefix argument.

`bootstrap-package' only calls `package-vc-install-from-checkout' (which
generates the autoloads file) the first time it sees a checkout — edits
made to an `external/' package afterward, including new, renamed, or
moved `;;;###autoload' cookies, never refresh that file on their own (see
`builder-emacs-conf-recompile-vendored-packages' for the equivalent
byte-compile staleness). Regeneration runs in a fresh batch Emacs
subprocess so the result reflects only what is on disk, not whatever
this session already has loaded. Returns the list of package symbols
regenerated."
  (interactive
   (list (unless current-prefix-arg
           (intern (completing-read "Regenerate loaddefs for: "
                                    (seq-map (lambda (it) (symbol-name (car it)))
                                             bootstrap-vendored-packages)
                                    nil t)))))
  (let* ((specs (seq-filter (lambda (spec) (or (null package) (eq (car spec) package)))
                            bootstrap-vendored-packages))
         (regenerated
          (seq-keep
           (lambda (spec)
             (when (file-directory-p (expand-file-name (nth 1 spec) user-emacs-directory))
               (let* ((dir (expand-file-name (nth 1 spec) user-emacs-directory))
                      (output (expand-file-name (format "%s-autoloads.el" (car spec)) dir))
                      (out-buf (generate-new-buffer " *loaddefs-generate*"))
                      (status (unwind-protect
                                  (call-process "emacs" nil out-buf nil
                                                "--batch" "--eval"
                                                (format "(loaddefs-generate %S %S)" dir output))
                                (kill-buffer out-buf))))
                 (if (zerop status)
                     (car spec)
                   (message "builder: loaddefs-generate failed for %s" (car spec))
                   nil))))
           specs)))
    (when (called-interactively-p 'interactive)
      (message "builder: regenerated loaddefs for %s"
               (if regenerated (mapconcat #'symbol-name regenerated ", ") "none")))
    regenerated))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unified check dispatch table & modern aliases

(defconst builder-isolated-check-kinds
  '((byte-compile . (builder-emacs-conf--do-byte-compile-check . "*Compile-Log*"))
    (load . (builder-emacs-conf--do-load-check . "*builder-load-check-log*"))
    (package-test . (builder-emacs-conf--do-elisp-package-test-check . "*builder-test-check-log*")))
  "Supported check kinds for `builder-isolated-check'.
Maps each KIND symbol to (DO-FN . LOG-BUFFER-NAME).")

(defun builder-emacs-conf--resolve-check-target (kind target)
  "Resolve TARGET path for KIND (`byte-compile', `load', or `package-test')."
  (if (eq kind 'package-test)
      (expand-file-name (or target (approximate-project-root)))
    (expand-file-name target user-emacs-directory)))

;;;###autoload
(cl-defun builder-isolated-check (kind target &optional (transport 'subprocess) &key callback sprite)
  "Run isolated check of KIND on TARGET using TRANSPORT.
KIND is one of `byte-compile', `load', or `package-test'.
TRANSPORT is either `subprocess' (default) or `sprite'."
  (let ((spec (alist-get kind builder-isolated-check-kinds)))
    (unless spec
      (user-error "Unknown check kind: %s" kind))
    (let ((do-fn (car spec))
          (log-buf (cdr spec))
          (expanded (builder-emacs-conf--resolve-check-target kind target)))
      (pcase transport
        ('subprocess
         (let ((result (builder-emacs-conf--isolated-batch-eval (list do-fn expanded))))
           (builder-emacs-conf--report-log log-buf (cdr result))
           (car result)))
        ('sprite
         (builder-emacs-conf-sprite-eval
          (list do-fn expanded)
          :sprite sprite
          :callback (or callback
                        (builder-emacs-conf--sprite-check-callback
                         log-buf
                         (if (eq kind 'package-test) expanded target)))))
        (_ (user-error "Unknown transport: %s" transport))))))

;;;###autoload
(defun builder-emacs-conf-byte-compile-and-delete-artifact (file)
  "Byte-compile FILE for error checking in an isolated subprocess.
FILE is resolved relative to `user-emacs-directory'.  Use this instead of
plain `byte-compile-file'; see the isolation note above
`builder-emacs-conf--do-byte-compile-check' for what that catches.

Returns t if compilation produced no errors; check `*Compile-Log*'
regardless, since a missing `require' shows up there as a warning
\(\"is not known to be defined\"), not necessarily a failure. For agent
skills via emacsclient:
  emacsclient --eval \\='(builder-emacs-conf-byte-compile-and-delete-artifact \"lisp/foo.el\")\\='"
  (builder-isolated-check 'byte-compile file 'subprocess))

;;;###autoload
(defun builder-emacs-conf-load-check (file)
  "Load FILE in an isolated subprocess to check it loads cleanly.
FILE is resolved relative to `user-emacs-directory'.  Use this instead of
plain `load'; see the isolation note above
`builder-emacs-conf--do-byte-compile-check' for what that catches.  A
missing `require' surfaces here as a real void-function/void-variable
error instead of silently succeeding.

Returns t if FILE loaded without error; any error is left in
`*builder-load-check-log*'. For agent skills via emacsclient:
  emacsclient --eval \\='(builder-emacs-conf-load-check \"lisp/foo.el\")\\='"
  (builder-isolated-check 'load file 'subprocess))

;;;###autoload
(defun builder-emacs-conf-elisp-package-test-isolated (&optional root)
  "Run ERT tests for the elisp package at ROOT in an isolated subprocess.
ROOT defaults to `approximate-project-root'.  Use this instead of
`builder-elisp-package-test'; see the isolation note above
`builder-emacs-conf--do-byte-compile-check' for what that catches.

Returns t if all tests passed; the ERT summary is left in
`*builder-test-check-log*'. For agent skills via emacsclient:
  emacsclient --eval \\='(builder-emacs-conf-elisp-package-test-isolated \"external/foo\")\\='"
  (builder-isolated-check 'package-test root 'subprocess))

;;;###autoload
(cl-defun builder-emacs-conf-sprite-byte-compile-check (file &key sprite callback)
  "Byte-compile FILE in a persistent sprite instead of the calling process.
FILE is resolved relative to `user-emacs-directory'.  Use to rule out
this session's own state as the cause of a compile failure; see
`builder-emacs-conf-sprite-eval'.  Returns a `sprite-direct-promise'
immediately; without CALLBACK, pass/fail is reported with `alert' and
the log is left in `*Compile-Log*' once the sprite responds."
  (interactive (list (buffer-file-name)))
  (builder-isolated-check 'byte-compile file 'sprite :sprite sprite :callback callback))

;;;###autoload
(cl-defun builder-emacs-conf-sprite-load-check (file &key sprite callback)
  "Load FILE in a persistent sprite instead of the calling process.
FILE is resolved relative to `user-emacs-directory'.  Use to rule out
this session's own state as the cause of a load failure; see
`builder-emacs-conf-sprite-eval'.  Returns a `sprite-direct-promise'
immediately; without CALLBACK, pass/fail is reported with `alert' and
the result is left in `*builder-load-check-log*' once the sprite
responds."
  (interactive (list (buffer-file-name)))
  (builder-isolated-check 'load file 'sprite :sprite sprite :callback callback))

;;;###autoload
(cl-defun builder-emacs-conf-sprite-test-check (&optional root &key sprite callback)
  "Run ERT tests for the elisp package at ROOT in a persistent sprite.
ROOT defaults to `approximate-project-root'.  Use to rule out this
session's own state as the cause of a test failure; see
`builder-emacs-conf-sprite-eval'.  Returns a `sprite-direct-promise'
immediately; without CALLBACK, pass/fail is reported with `alert' and
the ERT summary is left in `*builder-test-check-log*' once the sprite
responds."
  (interactive (list (approximate-project-root)))
  (builder-isolated-check 'package-test root 'sprite :sprite sprite :callback callback))

(defalias 'builder-isolate--do-byte-compile-check #'builder-emacs-conf--do-byte-compile-check)
(defalias 'builder-isolate--do-load-check #'builder-emacs-conf--do-load-check)
(defalias 'builder-isolate--do-elisp-package-test-check #'builder-emacs-conf--do-elisp-package-test-check)
(defalias 'builder-isolate--read-result-file #'builder-emacs-conf--read-result-file)
(defalias 'builder-isolate--report-log #'builder-emacs-conf--report-log)
(defalias 'builder-isolate--batch-eval #'builder-emacs-conf--isolated-batch-eval)
(defalias 'builder-isolate-byte-compile-and-delete-artifact #'builder-emacs-conf-byte-compile-and-delete-artifact)
(defalias 'builder-isolate-load-check #'builder-emacs-conf-load-check)
(defalias 'builder-isolate-elisp-package-test-isolated #'builder-emacs-conf-elisp-package-test-isolated)
(defalias 'builder-isolate-async-byte-compile-check #'builder-emacs-conf-async-byte-compile-check)
(defalias 'builder-isolate-sprite-eval #'builder-emacs-conf-sprite-eval)
(defalias 'builder-isolate-sprite-byte-compile-check #'builder-emacs-conf-sprite-byte-compile-check)
(defalias 'builder-isolate-sprite-load-check #'builder-emacs-conf-sprite-load-check)
(defalias 'builder-isolate-sprite-test-check #'builder-emacs-conf-sprite-test-check)

(provide 'builder-isolate-elisp)
;;; builder-isolate-elisp.el ends here
