;;; elpaish-install.el --- Package bootstrapping and upgrade utilities -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maint, tools, package
;; URL: https://github.com/tychoish/elpaish

;;; Commentary:
;; Client-side helper module for installing groups of packages, bootstrapping
;; configurations, and upgrading installed packages with cooperative non-blocking
;; yielding and continue-on-error semantics.

;;; Code:

(require 'cl-lib)
(require 'package)
(require 'gen nil t)

(declare-function elpaish-recipe-p "elpaish" (object))
(declare-function elpaish-recipe-name "elpaish" (recipe))
(declare-function elpaish-recipe-requires "elpaish" (recipe))
(declare-function elpaish--recipe-source-path "elpaish" (recipe))
(declare-function sprite-future-then "sprite" (future callback))
(declare-function gen-run-async "gen" (fn sequence &rest args))
(declare-function package--reload-previously-loaded "package" (pkg-desc &optional warn))

(defgroup elpaish-install nil
  "Package bootstrapping and upgrade utilities."
  :group 'package
  :prefix "elpaish-install-")

(define-obsolete-variable-alias
  'elpaish-install-bootstrap-packages
  'elpaish-install-bootstrap-packages-list
  "0.2.0")

(defcustom elpaish-install-bootstrap-packages-list
  '(compat web-server htmlize annotated-completing-read transient package-lint)
  "List of package symbols to install or upgrade during bootstrapping.
Used as default target package list for `elpaish-install-bootstrap-packages'
when no explicit packages are specified."
  :type '(repeat symbol)
  :group 'elpaish-install)

(defcustom elpaish-install-concurrency 1
  "Maximum concurrent package operations when running asynchronously.
Defaults to 1 because in-session package installation modifies global
`load-path' and `package-alist' state."
  :type 'integer
  :group 'elpaish-install)

(defcustom elpaish-install-yield-delay 0.005
  "Delay in seconds between package operations when yielding to the event loop.
Allows the Emacs event loop to remain responsive and process user input."
  :type 'number
  :group 'elpaish-install)

(defcustom elpaish-install-package-updated-functions nil
  "Hook run when a package is installed or upgraded by `elpaish-install'.
Each function is called with three arguments: PKG (symbol), STATUS
  (\\='installed or \\='upgraded), and RES (the result plist)."
  :type 'hook
  :group 'elpaish-install)

(defcustom elpaish-install-bootstrap-finished-functions nil
  "Hook run when package bootstrap or installation completes.
Each function is called with two arguments: RES-LIST (the list of package
result plists) and ACTION (symbol, \\='upgrade or \\='install)."
  :type 'hook
  :group 'elpaish-install)

(defvar elpaish-install--active-cancel-handle nil
  "Cancellation handle function for active async package installation.")

;;;###autoload
(defun elpaish-install-register-bootstrap-package (&rest pkgs)
  "Append unique package symbols from PKGS to bootstrap package list.
PKGS can be package symbols or lists of package symbols.  Updates
`elpaish-install-bootstrap-packages-list' and returns it."
  (let ((new-pkgs nil))
    (dolist (p (flatten-tree pkgs))
      (when (and (symbolp p)
                 p
                 (not (memq p elpaish-install-bootstrap-packages-list))
                 (not (memq p new-pkgs)))
        (push p new-pkgs)))
    (when new-pkgs
      (setq elpaish-install-bootstrap-packages-list
            (append elpaish-install-bootstrap-packages-list (nreverse new-pkgs)))))
  elpaish-install-bootstrap-packages-list)

(define-obsolete-function-alias
  'elpaish-install-register-bootstrap-packages
  #'elpaish-install-register-bootstrap-package
  "0.2.0")

(define-obsolete-function-alias
  'elpaish-install-add-bootstrap-packages
  #'elpaish-install-register-bootstrap-package
  "0.2.0")

(defun elpaish-install--extract-header-requires (file)
  "Extract package requirements list from FILE header using `package-buffer-info'."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (when-let* ((pkg-info (condition-case nil (package-buffer-info) (error nil))))
        (package-desc-reqs pkg-info)))))

;;;###autoload
(defun elpaish-install-ensure-package-dependencies (dir-or-recipe)
  "Ensure dependencies in Package-Requires of DIR-OR-RECIPE are installed.
DIR-OR-RECIPE can be an `elpaish-recipe' struct, a directory string, or a file
string containing package sources.  Extracts dependencies from package header
and invokes `package-install' for any uninstalled dependency."
  (unless package-archives
    (user-error "Cannot ensure package dependencies: `package-archives' is empty"))
  (unless (bound-and-true-p package-archive-contents)
    (package-initialize))
  (let* ((recipe (if (and (fboundp 'elpaish-recipe-p) (elpaish-recipe-p dir-or-recipe))
                     dir-or-recipe
                   nil))
         (reqs (cond
                ((and recipe (elpaish-recipe-requires recipe))
                 (elpaish-recipe-requires recipe))
                (t
                 (let* ((is-file (and (stringp dir-or-recipe) (file-regular-p dir-or-recipe)))
                        (source-dir (if recipe
                                        (elpaish--recipe-source-path recipe)
                                      (if is-file
                                          (file-name-directory (expand-file-name dir-or-recipe))
                                        (file-name-as-directory (expand-file-name dir-or-recipe)))))
                        (name (if recipe
                                  (elpaish-recipe-name recipe)
                                (if is-file
                                    (file-name-base dir-or-recipe)
                                  (file-name-nondirectory (directory-file-name source-dir)))))
                        (main-file (if is-file
                                       (expand-file-name dir-or-recipe)
                                     (expand-file-name (format "%s.el" name) source-dir))))
                   (elpaish-install--extract-header-requires main-file))))))
    (when reqs
      (let ((missing nil))
        (dolist (req reqs)
          (let ((dep-pkg (if (consp req) (car req) req)))
            (unless (or (eq dep-pkg 'emacs)
                        (package-installed-p dep-pkg)
                        ;; Skip packages that are part of the local ELPAish repository
                        (and (boundp 'elpaish-registry)
                             (hash-table-p elpaish-registry)
                             (gethash (symbol-name dep-pkg) elpaish-registry)))
              (push dep-pkg missing))))
        (when missing
          (message "Refreshing package archive contents to install missing dependencies: %S" missing)
          (package-refresh-contents)
          (dolist (dep-pkg missing)
            (message "Installing missing package dependency: %s" dep-pkg)
            (condition-case err
                (package-install dep-pkg)
              (error
               (message "Warning: Failed to install package dependency %s: %s"
                        dep-pkg (error-message-string err))))))))))

;;;; Core Single Package Operation

(defun elpaish-install--process-package (pkg &optional action refresh)
  "Perform single package PKG operation for ACTION (\\='upgrade or \\='install).
ACTION defaults to \\='upgrade, which installs PKG if missing or upgrades it
if an update is available.  If REFRESH is non-nil, refresh archive contents."
  (unless (bound-and-true-p package-archive-contents)
    (package-initialize))
  (when refresh
    (package-refresh-contents))
  (let ((act (or action 'upgrade)))
    (condition-case err
        (pcase act
          ('install
           (cond
            ((package-installed-p pkg)
             (list :status :already-installed :pkg pkg))
            (t
             (package-install pkg)
             (when (fboundp 'package--reload-previously-loaded)
               (when-let* ((desc (cadr (assq pkg package-alist))))
                 (condition-case nil
                     (package--reload-previously-loaded desc)
                   (error nil))))
             (let ((res (list :status :installed :pkg pkg)))
               (run-hook-with-args 'elpaish-install-package-updated-functions
                                   pkg 'installed res)
               res))))
          (_
           (cond
            ((not (package-installed-p pkg))
             (package-install pkg)
             (when (fboundp 'package--reload-previously-loaded)
               (when-let* ((desc (cadr (assq pkg package-alist))))
                 (condition-case nil
                     (package--reload-previously-loaded desc)
                   (error nil))))
             (let ((res (list :status :installed :pkg pkg)))
               (run-hook-with-args 'elpaish-install-package-updated-functions
                                   pkg 'installed res)
               res))
            ((fboundp 'package-upgrade)
             (let* ((installed-desc (cadr (assq pkg package-alist)))
                    (inst-ver (and installed-desc (package-desc-version installed-desc))))
               (condition-case sub-err
                   (progn
                     (package-upgrade pkg)
                     (let* ((new-desc (cadr (assq pkg package-alist)))
                            (new-ver (and new-desc (package-desc-version new-desc)))
                            (from-str (and inst-ver (package-version-join inst-ver)))
                            (to-str (and new-ver (package-version-join new-ver)))
                            (res (list :status :upgraded :pkg pkg :from from-str :to to-str)))
                       (run-hook-with-args 'elpaish-install-package-updated-functions
                                           pkg 'upgraded res)
                       res))
                 (user-error
                  (list :status :up-to-date :pkg pkg))
                 (error
                  (signal (car sub-err) (cdr sub-err))))))
            (t
             (let* ((installed-desc (cadr (assq pkg package-alist)))
                    (archive-desc (cadr (assq pkg package-archive-contents)))
                    (inst-ver (and installed-desc (package-desc-version installed-desc)))
                    (arch-ver (and archive-desc (package-desc-version archive-desc))))
               (cond
                ((and inst-ver arch-ver (version-list-< inst-ver arch-ver))
                 (package-install pkg)
                 (when (fboundp 'package--reload-previously-loaded)
                   (when-let* ((new-desc (cadr (assq pkg package-alist))))
                     (condition-case nil
                         (package--reload-previously-loaded new-desc)
                       (error nil))))
                 (let* ((from-str (package-version-join inst-ver))
                        (to-str (package-version-join arch-ver))
                        (res (list :status :upgraded :pkg pkg :from from-str :to to-str)))
                   (run-hook-with-args 'elpaish-install-package-updated-functions
                                       pkg 'upgraded res)
                   res))
                (t
                 (list :status :up-to-date :pkg pkg))))))))
      (error
       (list :status :failed :pkg pkg :error (error-message-string err))))))

;;;; Summary and Completion Helpers

(defun elpaish-install--report-summary (res-list action)
  "Display summary message for RES-LIST and ACTION."
  (let ((installed nil)
        (upgraded nil)
        (up-to-date 0)
        (failed nil))
    (dolist (res res-list)
      (let ((p (plist-get res :pkg)))
        (pcase (plist-get res :status)
          (':installed (push p installed))
          (':upgraded  (push p upgraded))
          (':up-to-date (cl-incf up-to-date))
          (':already-installed (cl-incf up-to-date))
          (':failed    (push (cons p (plist-get res :error)) failed)))))
    (let ((parts nil))
      (when upgraded
        (push (format "%d upgraded (%s)"
                      (length upgraded)
                      (mapconcat #'symbol-name (nreverse upgraded) ", "))
              parts))
      (when installed
        (push (format "%d installed (%s)"
                      (length installed)
                      (mapconcat #'symbol-name (nreverse installed) ", "))
              parts))
      (when (> up-to-date 0)
        (push (format "%d up to date" up-to-date) parts))
      (when failed
        (push (format "%d failed (%s)"
                      (length failed)
                      (mapconcat (lambda (f) (format "%s" (car f))) (nreverse failed) ", "))
              parts))
      (cond
       ((and (null upgraded) (null installed) (null failed))
        (message "[elpaish] %s complete: all %d package(s) up to date."
                 (if (eq action 'install) "Installation" "Bootstrap")
                 up-to-date))
       (t
        (message "[elpaish] %s complete: %s."
                 (if (eq action 'install) "Installation" "Bootstrap")
                 (string-join (nreverse parts) ", ")))))))

(defun elpaish-install--remote-eval-form (pkg action parent-user-dir parent-archives refresh)
  "Generate Lisp form for remote sprite execution of PKG with ACTION.
ACTION is \\='install or \\='upgrade.
PARENT-USER-DIR and PARENT-ARCHIVES configure the remote package environment.
If REFRESH is non-nil, refreshes package contents remotely."
  `(progn
     (require 'package)
     (let ((package-user-dir ,parent-user-dir)
           (package-archives ',parent-archives))
       (require 'elpaish-install)
       (elpaish-install--process-package ',pkg ',action ,refresh))))

(defun elpaish-install--on-async-complete (res-list action callback)
  "Handle completion of async package operations RES-LIST for ACTION.
Reloads package contents in the main Emacs instance, activates updated packages,
and invokes CALLBACK."
  (condition-case nil
      (if (fboundp 'package-read-all-archive-contents)
          (package-read-all-archive-contents)
        (package-initialize))
    (error nil))
  (dolist (res res-list)
    (when (memq (plist-get res :status) '(:installed :upgraded))
      (when-let* ((pkg (plist-get res :pkg)))
        (condition-case nil
            (progn
              (package-activate pkg)
              (when (fboundp 'package--reload-previously-loaded)
                (when-let* ((desc (cadr (assq pkg package-alist))))
                  (package--reload-previously-loaded desc))))
          (error nil))
        (pcase (plist-get res :status)
          (:installed
           (message "[elpaish] Installed and activated %s" pkg))
          (:upgraded
           (message "[elpaish] Upgraded and reloaded %s" pkg)))
        (run-hook-with-args 'elpaish-install-package-updated-functions
                            pkg
                            (if (eq (plist-get res :status) :installed) 'installed 'upgraded)
                            res))))
  (elpaish-install--report-summary res-list action)
  (run-hook-with-args 'elpaish-install-bootstrap-finished-functions res-list action)
  (when callback (funcall callback res-list)))

;;;; Batch Execution

(cl-defun elpaish-install--do-packages (pkgs &key (action 'upgrade) refresh)
  "Perform package operation ACTION for PKGS synchronously.
If REFRESH is non-nil, call `package-refresh-contents' first."
  (let ((target-pkgs (cond
                      ((eq action 'install) (seq-remove #'package-installed-p pkgs))
                      (t pkgs))))
    (cond
     ((and (eq action 'install) (null target-pkgs))
      nil)
     (t
      (unless (bound-and-true-p package-archive-contents)
        (package-initialize))
      (when (or refresh (eq action 'upgrade) (null package-archive-contents))
        (message "[elpaish] Refreshing package archive contents...")
        (package-refresh-contents))
      (let* ((results nil)
             (total (length target-pkgs))
             (idx 0))
        (dolist (pkg target-pkgs)
          (cl-incf idx)
          (let ((res (elpaish-install--process-package pkg action nil)))
            (push res results)
            (pcase (plist-get res :status)
              (:installed
               (message "[elpaish] [%d/%d] Installed %s (activated)" idx total pkg))
              (:upgraded
               (if (and (plist-get res :from) (plist-get res :to))
                   (message "[elpaish] [%d/%d] Upgraded %s from %s to %s (reloaded)"
                            idx total pkg (plist-get res :from) (plist-get res :to))
                 (message "[elpaish] [%d/%d] Upgraded %s (reloaded)" idx total pkg)))
              (:failed
               (message "[elpaish] [%d/%d] Failed %s: %s"
                        idx total pkg (plist-get res :error)))
              (_
               (message "[elpaish] [%d/%d] %s is up to date" idx total pkg)))))
        (let ((final-res (nreverse results)))
          (elpaish-install--report-summary final-res action)
          (run-hook-with-args 'elpaish-install-bootstrap-finished-functions
                              final-res action)
          final-res))))))

(defun elpaish-install--do-install-packages (pkgs refresh)
  "Perform actual installation sequence for PKGS synchronously.
If REFRESH is non-nil, call `package-refresh-contents' first."
  (elpaish-install--do-packages pkgs :action 'install :refresh refresh))

(defun elpaish-install--do-upgrade-packages (pkgs)
  "Perform actual package upgrade sequence for PKGS synchronously."
  (elpaish-install--do-packages pkgs :action 'upgrade :refresh t))

(cl-defun elpaish-install--do-packages-async (pkgs &key (action 'upgrade) refresh concurrency delay pool-size callback)
  "Perform package operation ACTION for PKGS asynchronously.
By default, uses `gen-run-async' yielding to the Emacs event loop.
If POOL-SIZE is non-nil and `sprite-pool-mapcar' is available, uses
out-of-process sprite pool.  Otherwise, falls back to `run-at-time'.
Calls CALLBACK when complete and returns a cancellation handle function."
  (let ((target-pkgs (cond
                      ((eq action 'install) (seq-remove #'package-installed-p pkgs))
                      (t pkgs))))
    (cond
     ((and (eq action 'install) (null target-pkgs))
      (when callback (funcall callback nil))
      (lambda (&optional _) nil))
     ((and pool-size
           (require 'sprite nil t)
           (require 'sprite-fleet nil t)
           (fboundp 'sprite-pool-mapcar))
      (let ((parent-user-dir package-user-dir)
            (parent-archives package-archives))
        (message "[elpaish] %s %d package(s) asynchronously using sprite pool..."
                 (if (eq action 'install) "Installing" "Bootstrapping")
                 (length target-pkgs))
        (let ((future (sprite-pool-mapcar
                       (lambda (pkg)
                         (elpaish-install--remote-eval-form pkg action parent-user-dir parent-archives refresh))
                       target-pkgs
                       :pool-size pool-size
                       :async t)))
          (sprite-future-then
           future
           (lambda (res-list)
             (elpaish-install--on-async-complete res-list action callback))))
        (lambda (&optional _action) nil)))
     ((fboundp 'gen-run-async)
      (unless (bound-and-true-p package-archive-contents)
        (package-initialize))
      (when (or refresh (eq action 'upgrade) (null package-archive-contents))
        (message "[elpaish] Refreshing package archive contents...")
        (package-refresh-contents))
      (let* ((results nil)
             (total (length target-pkgs))
             (idx 0))
        (message "[elpaish] %s %d package(s) asynchronously..."
                 (if (eq action 'install) "Installing" "Bootstrapping")
                 total)
        (let ((runner
               (gen-run-async
                (lambda (pkg done-cb)
                  (cl-incf idx)
                  (let ((res (elpaish-install--process-package pkg action nil)))
                    (push res results)
                    (pcase (plist-get res :status)
                      (:installed
                       (message "[elpaish] [%d/%d] Installed %s (activated)" idx total pkg))
                      (:upgraded
                       (if (and (plist-get res :from) (plist-get res :to))
                           (message "[elpaish] [%d/%d] Upgraded %s from %s to %s (reloaded)"
                                    idx total pkg (plist-get res :from) (plist-get res :to))
                         (message "[elpaish] [%d/%d] Upgraded %s (reloaded)" idx total pkg)))
                      (:failed
                       (message "[elpaish] [%d/%d] Failed %s: %s"
                                idx total pkg (plist-get res :error)))
                      (_
                       (message "[elpaish] [%d/%d] %s is up to date" idx total pkg)))
                    (funcall done-cb)))
                target-pkgs
                :concurrency (or concurrency elpaish-install-concurrency 1)
                :delay (or delay elpaish-install-yield-delay 0.0)
                :on-done
                (lambda ()
                  (setq elpaish-install--active-cancel-handle nil)
                  (let ((final-res (nreverse results)))
                    (elpaish-install--report-summary final-res action)
                    (run-hook-with-args 'elpaish-install-bootstrap-finished-functions
                                        final-res action)
                    (when callback
                      (funcall callback final-res))))
                :on-error
                (lambda (err)
                  (message "[elpaish] Error during package bootstrapping: %S" err)))))
          (setq elpaish-install--active-cancel-handle runner)
          runner)))
     (t
      ;; Fallback when gen-run-async is not available
      (run-at-time 0 nil
                   (lambda ()
                     (let ((res (elpaish-install--do-packages target-pkgs :action action :refresh refresh)))
                       (when callback (funcall callback res)))))
      (lambda (&optional _) nil)))))

;;;###autoload
(defun elpaish-install-cancel ()
  "Cancel any in-flight asynchronous package installation or bootstrap operations."
  (interactive)
  (when (functionp elpaish-install--active-cancel-handle)
    (ignore-errors (funcall elpaish-install--active-cancel-handle :cancel)))
  (setq elpaish-install--active-cancel-handle nil))

;;;; Public Commands and Collapsed Dispatcher

;;;###autoload
(cl-defun elpaish-install-bootstrap-packages (&rest args)
  "Install or upgrade bootstrap packages with non-blocking cooperative yielding.
Packages are installed if missing, or upgraded if newer versions exist.
By default, execution is asynchronous and yielding using `gen.el'.
When called interactively with a prefix argument (e.g. \\[universal-argument]),
execution is synchronous.

ARGS can contain package symbols, lists of package symbols, or keyword options:
  :sync BOOL        - When non-nil, execute synchronously.
  :async BOOL       - When non-nil (default t), execute asynchronously.
  :refresh BOOL     - When non-nil (default t), refresh package archives.
  :action ACTION    - Operation action: \\='upgrade (default) or \\='install.
  :concurrency N    - Concurrent jobs for `gen-run-async' (default 1).
  :delay SECS       - Yield delay in seconds
                      (default `elpaish-install-yield-delay').
  :callback FN      - Function called with result list on completion.
  :pool-size N      - When specified and sprite is available, use sprite pool.

When no packages are specified in ARGS, defaults to
`elpaish-install-bootstrap-packages-list'."
  (interactive "P")
  (let* ((interactive-p (called-interactively-p 'any))
         (prefix-arg-val (when interactive-p (car args)))
         (remaining (if interactive-p nil args))
         (pkgs nil)
         (refresh t)
         (sync (not (null prefix-arg-val)))
         (async :default)
         (action 'upgrade)
         (concurrency nil)
         (delay nil)
         (pool-size nil)
         (callback nil))
    (while remaining
      (let ((arg (pop remaining)))
        (cond
         ((eq arg :sync)       (setq sync (pop remaining)))
         ((eq arg :async)      (setq async (pop remaining)))
         ((eq arg :refresh)    (setq refresh (pop remaining)))
         ((eq arg :action)     (setq action (pop remaining)))
         ((eq arg :concurrency)(setq concurrency (pop remaining)))
         ((eq arg :delay)      (setq delay (pop remaining)))
         ((eq arg :pool-size)  (setq pool-size (pop remaining)))
         ((or (eq arg :callback) (eq arg :on-done))
          (setq callback (pop remaining)))
         ((listp arg)
          (dolist (p (flatten-tree arg))
            (when (symbolp p) (push p pkgs))))
         ((symbolp arg)
          (push arg pkgs)))))
    (let* ((target-pkgs (or (delete-dups (nreverse pkgs))
                            elpaish-install-bootstrap-packages-list))
           (is-sync (or sync (eq async nil))))
      (cond
       (is-sync
        (let ((res (elpaish-install--do-packages target-pkgs :action action :refresh refresh)))
          (when callback (funcall callback res))
          res))
       (t
        (elpaish-install--do-packages-async
         target-pkgs
         :action action
         :refresh refresh
         :concurrency concurrency
         :delay delay
         :pool-size pool-size
         :callback callback))))))

;;;###autoload
(defalias 'elpaish-install-packages #'elpaish-install-bootstrap-packages)

(define-obsolete-function-alias
  'elpaish-install-upgrade-packages
  #'elpaish-install-bootstrap-packages
  "0.2.0")

(define-obsolete-function-alias
  'elpaish-upgrade-packages
  #'elpaish-install-bootstrap-packages
  "0.2.0")

;;;###autoload
(cl-defun elpaish-install-packages-async (pkgs &key refresh pool-size callback)
  "Install PKGS asynchronously out-of-process using a sprite pool if available.
If REFRESH is non-nil, refresh archive contents.  POOL-SIZE is the sprite pool
concurrency.  Calls CALLBACK when complete."
  (elpaish-install-bootstrap-packages pkgs :action 'install :refresh refresh :pool-size (or pool-size 2) :callback callback :async t))

(define-obsolete-function-alias
  'elpaish-install-upgrade-packages-async
  #'elpaish-install-packages-async
  "0.2.0")

(provide 'elpaish-install)

;; Local Variables:
;; package-lint-main-file: "pkg/elpaish.el"
;; End:

;;; elpaish-install.el ends here
