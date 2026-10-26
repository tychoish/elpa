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

(declare-function comp-el-to-eln-filename "comp-run" (filename &optional base-dir))
(declare-function sprite-name "sprite-daemon")
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
        (noninteractive t))
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
          (let* ((success (and (byte-compile-file scratch) t))
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
  (let* ((root (file-name-as-directory root))
         (main-file (builder-elisp-package--main-file root)))
    (unless main-file
      (error "no <%s>.el at %s" (builder-elisp-package--name root) root))
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
                         (list "-f" "package-initialize" "--eval" "(require 'builder)"
                               "--eval" (format "(with-temp-file %S (prin1 %S (current-buffer)))"
                                               result-file form))))
          (builder-emacs-conf--read-result-file
           result-file (with-current-buffer buf (buffer-string))))
      (kill-buffer buf)
      (when (file-exists-p result-file)
        (delete-file result-file)))))

;;;###autoload
(defun builder-emacs-conf-byte-compile-and-delete-artifact (file)
  "Byte-compile FILE for error checking in an isolated subprocess.
FILE is resolved relative to `user-emacs-directory'.  Use this instead of
plain `byte-compile-file'; see the isolation note above
`builder-emacs-conf--do-byte-compile-check' for what that catches.

Returns t if compilation produced no errors; check `*Compile-Log*'
regardless, since a missing `require' shows up there as a warning
(\"is not known to be defined\"), not necessarily a failure. For agent
skills via emacsclient:
  emacsclient --eval \\='(builder-emacs-conf-byte-compile-and-delete-artifact \"lisp/foo.el\")\\='"
  (let* ((expanded (expand-file-name file user-emacs-directory))
         (result (builder-emacs-conf--isolated-batch-eval
                  `(builder-emacs-conf--do-byte-compile-check ,expanded))))
    (builder-emacs-conf--report-log "*Compile-Log*" (cdr result))
    (car result)))

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
  (let* ((expanded (expand-file-name file user-emacs-directory))
         (result (builder-emacs-conf--isolated-batch-eval
                  `(builder-emacs-conf--do-load-check ,expanded))))
    (builder-emacs-conf--report-log "*builder-load-check-log*" (cdr result))
    (car result)))

;;;###autoload
(defun builder-emacs-conf-elisp-package-test-isolated (&optional root)
  "Run ERT tests for the elisp package at ROOT in an isolated subprocess.
ROOT defaults to `approximate-project-root'.  Use this instead of
`builder-elisp-package-test'; see the isolation note above
`builder-emacs-conf--do-byte-compile-check' for what that catches.

Returns t if all tests passed; the ERT summary is left in
`*builder-test-check-log*'. For agent skills via emacsclient:
  emacsclient --eval \\='(builder-emacs-conf-elisp-package-test-isolated \"external/foo\")\\='"
  (let* ((root (expand-file-name (or root (approximate-project-root))))
         (result (builder-emacs-conf--isolated-batch-eval
                  `(builder-emacs-conf--do-elisp-package-test-check ,root))))
    (builder-emacs-conf--report-log "*builder-test-check-log*" (cdr result))
    (car result)))

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
         (result-file (make-temp-file "builder-isolated-result-")))
    (let ((builder-dir (builder-emacs-conf--builder-library-dir)))
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
                     (list "-f" "package-initialize" "--eval" "(require 'builder)"
                           "--eval" (format "(with-temp-file %S (prin1 (builder-emacs-conf--do-byte-compile-check %S) (current-buffer)))"
                                            result-file expanded)))))))

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

;;;###autoload
(cl-defun builder-emacs-conf-sprite-byte-compile-check (file &key sprite callback)
  "Byte-compile FILE in a persistent sprite instead of the calling process.
FILE is resolved relative to `user-emacs-directory'.  Use to rule out
this session's own state as the cause of a compile failure; see
`builder-emacs-conf-sprite-eval'.  Returns a `sprite-direct-promise'
immediately; without CALLBACK, pass/fail is reported with `alert' and
the log is left in `*Compile-Log*' once the sprite responds."
  (interactive (list (buffer-file-name)))
  (let ((expanded (expand-file-name file user-emacs-directory)))
    (builder-emacs-conf-sprite-eval
     `(builder-emacs-conf--do-byte-compile-check ,expanded)
     :sprite sprite
     :callback (or callback
                   (builder-emacs-conf--sprite-check-callback "*Compile-Log*" file)))))

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
  (let ((expanded (expand-file-name file user-emacs-directory)))
    (builder-emacs-conf-sprite-eval
     `(builder-emacs-conf--do-load-check ,expanded)
     :sprite sprite
     :callback (or callback
                   (builder-emacs-conf--sprite-check-callback "*builder-load-check-log*" file)))))

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
  (let ((root (expand-file-name (or root (approximate-project-root)))))
    (builder-emacs-conf-sprite-eval
     `(builder-emacs-conf--do-elisp-package-test-check ,root)
     :sprite sprite
     :callback (or callback
                   (builder-emacs-conf--sprite-check-callback "*builder-test-check-log*" root)))))

;; Expose the isolated checks above as compile-buffer candidates, dispatched
;; via emacsclient against this running session so `builder-emacs-conf-*'
;; still does its own subprocess/sprite isolation internally.


(defconst builder--native-compile-chunk-size 25
  "Number of files to check for staleness per idle tick during incremental native compilation.")

(defconst builder--native-compile-idle-delay 10
  "Idle seconds between successive chunks in `builder--native-compile-process-queue'.")

(defvar builder--native-compile-queue nil
  "Remaining .el files still to be checked/compiled by `builder--native-compile-process-queue'.")

(defvar builder--native-compile-idle-timer nil
  "Idle timer driving `builder--native-compile-process-queue'.")

(declare-function comp-el-to-eln-filename "comp-run" (filename &optional base-dir))

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

;;;###autoload
(defun builder-emacs-conf-native-compile-all ()
  "Incrementally native-compile stale .el files in lisp/ and elpa/.
Collects candidate files, then hands them to
`builder--native-compile-process-queue' to work through in bounded,
staleness-checked chunks on an idle timer. Runs once on a 60-second idle
timer after startup; also callable interactively for a fresh sweep."
  (interactive)
  (if (not (or (string-match "NATIVE_COMP" system-configuration-features)
	       (fboundp 'native-compile-async)))
      (message "Native compilation not available in this build")
    (let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
           (elpa-dir (expand-file-name "elpa" user-emacs-directory))
           (files (seq-filter
                   #'file-regular-p
                   (append
                    (when (file-directory-p lisp-dir)
		      (directory-files-recursively lisp-dir "\\.el\\'"))
                    (when (file-directory-p elpa-dir)
		      (directory-files-recursively elpa-dir "\\.el\\'"))))))
      (setq builder--native-compile-queue files)
      (unless builder--native-compile-idle-timer
        (setq builder--native-compile-idle-timer
              (run-with-idle-timer builder--native-compile-idle-delay t
                                    #'builder--native-compile-process-queue)))
      (message "builder-emacs-conf-native-compile-all: queued %d .el files for incremental staleness check"
               (length files)))))

(defvar bootstrap-vendored-packages nil)

;;;###autoload
(defun builder-emacs-conf-byte-recompile-directory ()
  "Recompile all `.el' files in `user-emacs-directory' and its direct subdirectories.
With a prefix argument, force recompilation of every file regardless of timestamps.
Returns the list of files that were recompiled."
  (interactive)
  (thread-last (cons user-emacs-directory
                     (seq-filter #'file-directory-p
                                 (directory-files user-emacs-directory t "^[^.]")))
               (seq-mapcat (lambda (dir) (directory-files dir t "\\.el\\'")))
               (seq-filter #'file-regular-p)
               (seq-keep (lambda (f)
                           (unless (eq 'no-byte-compile
                                       (byte-recompile-file f current-prefix-arg))
                             f)))))

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

(defconst builder-isolated-check-kinds
  '((byte-compile . "*Compile-Log*")
    (load         . "*builder-load-check-log*")
    (package-test . "*builder-test-check-log*"))
  "Supported check kinds for `builder-isolated-check'.")

;;;###autoload
(cl-defun builder-isolated-check (kind target &optional (transport 'subprocess) &key callback sprite)
  "Run isolated check of KIND on TARGET using TRANSPORT.
KIND is one of `byte-compile', `load', or `package-test'.
TRANSPORT is either `subprocess' (default) or `sprite'."
  (unless (assq kind builder-isolated-check-kinds)
    (user-error "Unknown check kind: %s" kind))
  (pcase transport
    ('subprocess
     (pcase kind
       ('byte-compile (builder-isolate-byte-compile-and-delete-artifact target))
       ('load         (builder-isolate-load-check target))
       ('package-test (builder-isolate-elisp-package-test-isolated target))))
    ('sprite
     (pcase kind
       ('byte-compile (builder-isolate-sprite-byte-compile-check target :sprite sprite :callback callback))
       ('load         (builder-isolate-sprite-load-check target :sprite sprite :callback callback))
       ('package-test (builder-isolate-sprite-test-check target :sprite sprite :callback callback))))
    (_ (user-error "Unknown transport: %s" transport))))

(provide 'builder-isolate-elisp)
;;; builder-isolate-elisp.el ends here
