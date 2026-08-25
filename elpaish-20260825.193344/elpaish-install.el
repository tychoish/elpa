;;; elpaish-install.el --- Package bootstrapping and upgrade utilities -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maint, tools, package
;; URL: https://github.com/tychoish/elpaish

;;; Commentary:
;; Client-side helper module for installing groups of packages, bootstrapping
;; configurations, and upgrading installed packages with continue-on-error semantics.

;;; Code:

(require 'cl-lib)
(require 'package)

(defgroup elpaish-install nil
  "Package bootstrapping and upgrade utilities."
  :group 'package
  :prefix "elpaish-install-")

(defcustom elpaish-install-bootstrap-packages
  '(web-server htmlize annotated-completing-read transient package-lint)
  "List of package symbols to install or upgrade during bootstrapping.
Used as default target package list for `elpaish-install-packages'
and `elpaish-install-upgrade-packages' when no explicit packages are specified."
  :type '(repeat symbol)
  :group 'elpaish-install)

;;;###autoload
(defun elpaish-install-add-bootstrap-packages (&rest pkgs)
  "Append unique package symbols from PKGS to `elpaish-install-bootstrap-packages'.
PKGS can be package symbols or lists of package symbols."
  (interactive)
  (let ((new-pkgs nil))
    (dolist (arg pkgs)
      (cond
       ((listp arg)
        (dolist (p arg)
          (when (and (symbolp p)
                     (not (memq p elpaish-install-bootstrap-packages))
                     (not (memq p new-pkgs)))
            (push p new-pkgs))))
       ((symbolp arg)
        (when (and (not (memq arg elpaish-install-bootstrap-packages))
                   (not (memq arg new-pkgs)))
          (push arg new-pkgs)))))
    (when new-pkgs
      (setq elpaish-install-bootstrap-packages
            (append elpaish-install-bootstrap-packages (nreverse new-pkgs)))))
  elpaish-install-bootstrap-packages)
(defun elpaish-install--extract-header-requires (file)
  "Extract package requirements list from FILE header using `package-buffer-info'."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (when-let* ((pkg-info (condition-case nil (package-buffer-info) (error nil))))
        (package-desc-reqs pkg-info)))))

;;;###autoload
(defun elpaish-install-ensure-package-dependencies (dir-or-recipe)
  "Extract package dependencies from DIR-OR-RECIPE and install any missing ones.
DIR-OR-RECIPE can be a directory path string or an `elpaish-recipe' struct."
  (let* ((recipe (when (and (fboundp 'elpaish-recipe-p) (elpaish-recipe-p dir-or-recipe))
                   dir-or-recipe))
         (repo-dir (cond
                    (recipe (if (fboundp 'elpaish--recipe-source-path)
                                (elpaish--recipe-source-path recipe)
                              default-directory))
                    ((stringp dir-or-recipe) (expand-file-name dir-or-recipe))
                    (t default-directory)))
         (name (cond
                (recipe (elpaish-recipe-name recipe))
                ((file-directory-p repo-dir) (file-name-nondirectory (directory-file-name repo-dir)))
                (t "package")))
         (main-file (cond
                     ((file-regular-p repo-dir) repo-dir)
                     ((file-directory-p repo-dir)
                      (let ((el-file (expand-file-name (concat name ".el") repo-dir))
                            (pkg-file (expand-file-name (concat name "-pkg.el") repo-dir)))
                        (cond
                         ((file-exists-p el-file) el-file)
                         ((file-exists-p pkg-file) pkg-file)
                         (t (car (directory-files repo-dir t "\\.el\\'"))))))
                     (t nil)))
         (header-reqs (and main-file (elpaish-install--extract-header-requires main-file)))
         (recipe-reqs (when (and recipe (fboundp 'elpaish-recipe-requires))
                        (elpaish-recipe-requires recipe)))
         (reqs (append header-reqs recipe-reqs)))

    (let ((missing nil))
      (dolist (req reqs)
        (let ((dep-pkg (if (consp req) (car req) req)))
          (when (and (symbolp dep-pkg)
                     (not (eq dep-pkg 'emacs))
                     (not (package-installed-p dep-pkg)))
            (push dep-pkg missing))))
      (when missing
        (setq missing (nreverse (delete-dups missing)))
        (unless (and (boundp 'package-archives) package-archives)
          (user-error "Cannot install missing dependencies (%s) for %s: `package-archives' is not configured"
                      (mapconcat #'symbol-name missing ", ") name))
        (unless (bound-and-true-p package-archive-contents)
          (package-initialize))
        (when (or (not (bound-and-true-p package-archive-contents))
                  (seq-some (lambda (p) (not (assq p package-archive-contents))) missing))
          (package-refresh-contents))
        (message "Installing implicit dependencies for %s: %s" name missing)
        (dolist (pkg missing)
          (unless (package-installed-p pkg)
            (condition-case err
                (package-install pkg)
              (error
               (message "Warning: Implicit dependency %s installation skipped or failed: %s"
                        pkg (error-message-string err))))))))))

;;;; Core Single Package Operation

(defun elpaish-install--process-package (pkg action &optional refresh)
  "Perform single package PKG installation or upgrade for ACTION ('install or 'upgrade).
If REFRESH is non-nil, call `package-refresh-contents' first."
  (unless (bound-and-true-p package-archive-contents)
    (package-initialize))
  (when refresh
    (package-refresh-contents))
  (condition-case err
      (pcase action
        ('install
         (if (package-installed-p pkg)
             (progn
               (message "Package %s is already installed." pkg)
               (list :status :already-installed :pkg pkg))
           (message "Installing package %s..." pkg)
           (package-install pkg)
           (message "Successfully installed %s." pkg)
           (list :status :installed :pkg pkg)))
        ('upgrade
         (cond
          ((not (package-installed-p pkg))
           (message "Package %s is not installed; installing..." pkg)
           (package-install pkg)
           (list :status :upgraded :pkg pkg))
          ((fboundp 'package-upgrade)
           (condition-case sub-err
               (progn
                 (package-upgrade pkg)
                 (message "Successfully upgraded %s." pkg)
                 (list :status :upgraded :pkg pkg))
             (user-error
              (message "Package %s is up to date." pkg)
              (list :status :up-to-date :pkg pkg))
             (error
              (signal (car sub-err) (cdr sub-err)))))
          (t
           (let* ((installed-desc (cadr (assq pkg package-alist)))
                  (archive-desc (cadr (assq pkg package-archive-contents)))
                  (inst-ver (and installed-desc (package-desc-version installed-desc)))
                  (arch-ver (and archive-desc (package-desc-version archive-desc))))
             (if (and inst-ver arch-ver (version-list-< inst-ver arch-ver))
                 (progn
                   (package-install pkg)
                   (message "Successfully upgraded %s from %s to %s."
                            pkg (package-version-join inst-ver) (package-version-join arch-ver))
                   (list :status :upgraded :pkg pkg))
               (message "Package %s is up to date." pkg)
               (list :status :up-to-date :pkg pkg)))))))
    (error
     (message "Failed to %s %s: %s" action pkg (error-message-string err))
     (list :status :failed :pkg pkg :error (error-message-string err)))))

;;;; Remote Form and Completion Helpers

(defun elpaish-install--remote-eval-form (pkg action parent-user-dir parent-archives refresh)
  "Generate Lisp form for remote sprite execution of PKG with ACTION ('install or 'upgrade).
PARENT-USER-DIR and PARENT-ARCHIVES configure the remote package environment.
If REFRESH is non-nil, refreshes package contents remotely."
  `(progn
     (require 'package)
     (require 'elpaish-install)
     (setq package-user-dir ,parent-user-dir)
     (setq package-archives ',parent-archives)
     (elpaish-install--process-package ',pkg ',action ,refresh)))

(defun elpaish-install--on-async-complete (res-list action callback)
  "Handle completion of async package operations RES-LIST for ACTION ('install or 'upgrade).
Reloads package contents in the main Emacs instance and invokes CALLBACK."
  (condition-case nil
      (if (fboundp 'package-read-all-archive-contents)
          (package-read-all-archive-contents)
        (package-initialize))
    (error nil))
  (let ((installed 0) (upgraded 0) (up-to-date 0) (failed 0))
    (dolist (res res-list)
      (pcase (plist-get res :status)
        (':installed (cl-incf installed))
        (':upgraded  (cl-incf upgraded))
        (':up-to-date (cl-incf up-to-date))
        (':failed    (cl-incf failed))))
    (if (eq action 'upgrade)
        (message "Async package upgrade complete: %d upgraded, %d up to date, %d failed."
                 upgraded up-to-date failed)
      (message "Async package installation complete: %d installed, %d failed."
               installed failed)))
  (when callback (funcall callback res-list)))

;;;; Unified Batch Execution

(cl-defun elpaish-install--do-packages (pkgs &key (action 'install) refresh)
  "Perform package operation ACTION ('install or 'upgrade) for PKGS synchronously.
If REFRESH is non-nil, call `package-refresh-contents' first."
  (unless (bound-and-true-p package-archive-contents)
    (package-initialize))
  (when (or refresh (eq action 'upgrade))
    (message "Refreshing package archive contents...")
    (package-refresh-contents))
  (let ((installed-count 0)
        (upgraded-count 0)
        (up-to-date-count 0)
        (failed-count 0)
        (results nil))
    (dolist (pkg pkgs)
      (let ((res (elpaish-install--process-package pkg action nil)))
        (push res results)
        (pcase (plist-get res :status)
          (':installed (cl-incf installed-count))
          (':upgraded  (cl-incf upgraded-count))
          (':up-to-date (cl-incf up-to-date-count))
          (':failed    (cl-incf failed-count)))))
    (if (eq action 'upgrade)
        (message "Package upgrade complete: %d upgraded, %d up to date, %d failed."
                 upgraded-count up-to-date-count failed-count)
      (message "Package installation complete: %d installed, %d failed."
               installed-count failed-count))
    (nreverse results)))

(defun elpaish-install--do-install-packages (pkgs refresh)
  "Perform actual installation sequence for PKGS.
If REFRESH is non-nil, call `package-refresh-contents' first."
  (elpaish-install--do-packages pkgs :action 'install :refresh refresh))

(defun elpaish-install--do-upgrade-packages (pkgs)
  "Perform actual package upgrade sequence for PKGS."
  (elpaish-install--do-packages pkgs :action 'upgrade :refresh t))

(cl-defun elpaish-install--do-packages-async (pkgs &key (action 'install) refresh pool-size callback)
  "Perform package operation ACTION ('install or 'upgrade) for PKGS asynchronously out-of-process.
Uses a sprite pool if available, falling back to a background timer otherwise.
Calls CALLBACK when complete."
  (if (and (require 'sprite nil t)
           (require 'sprite-fleet nil t)
           (fboundp 'sprite-pool-mapcar))
      (let* ((parent-user-dir package-user-dir)
             (parent-archives package-archives)
             (target-pkgs (if (eq action 'install)
                              (seq-remove #'package-installed-p pkgs)
                            pkgs)))
        (if (null target-pkgs)
            (progn
              (message "All specified packages are already installed.")
              (when callback (funcall callback nil)))
          (message "%s %d package(s) asynchronously using sprite pool..."
                   (if (eq action 'upgrade) "Upgrading" "Installing")
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
               (elpaish-install--on-async-complete res-list action callback))))))
    (run-at-time 0 nil
                 (lambda ()
                   (let ((res (elpaish-install--do-packages pkgs :action action :refresh refresh)))
                     (when callback (funcall callback res)))))))

;;;###autoload
(cl-defun elpaish-install-packages-async (pkgs &key refresh pool-size callback)
  "Install PKGS asynchronously out-of-process using a sprite pool if available.
If sprite is not available, falls back to a background timer.
Calls CALLBACK when installation finishes."
  (elpaish-install--do-packages-async pkgs :action 'install :refresh refresh :pool-size pool-size :callback callback))

;;;###autoload
(cl-defun elpaish-install-upgrade-packages-async (pkgs &key pool-size callback)
  "Upgrade PKGS asynchronously out-of-process using a sprite pool if available.
If sprite is not available, falls back to a background timer.
Calls CALLBACK when upgrade finishes."
  (elpaish-install--do-packages-async pkgs :action 'upgrade :refresh t :pool-size pool-size :callback callback))

;;;; Public Command Dispatchers

(defun elpaish-install--execute (args action)
  "Parse ARGS and execute package operation for ACTION ('install or 'upgrade)."
  (let ((pkgs nil)
        (refresh nil)
        (async nil)
        (pool-size nil)
        (callback nil))
    (while args
      (let ((arg (pop args)))
        (cond
         ((eq arg :refresh)   (setq refresh (pop args)))
         ((eq arg :async)     (setq async (pop args)))
         ((eq arg :pool-size) (setq pool-size (pop args)))
         ((eq arg :callback)  (setq callback (pop args)))
         ((listp arg)         (dolist (p arg) (push p pkgs)))
         ((symbolp arg)       (push arg pkgs)))))
    (let ((target-pkgs (or (nreverse pkgs) elpaish-install-bootstrap-packages)))
      (if async
          (elpaish-install--do-packages-async target-pkgs :action action :refresh refresh :pool-size pool-size :callback callback)
        (elpaish-install--do-packages target-pkgs :action action :refresh (if (eq action 'upgrade) t refresh))))))

;;;###autoload
(cl-defun elpaish-install-packages (&rest args)
  "Install a group of packages with continue-on-error semantics.
ARGS can contain package symbols, lists of symbols, or keyword options:
  :refresh T    - Call `package-refresh-contents' before installing.
  :async T      - Perform installation asynchronously (uses sprite pool if available).
  :pool-size N  - Number of sprite workers to use for async parallel installation.
  :callback FN  - Function to call when async installation finishes.

When no packages are specified in ARGS, defaults to `elpaish-install-bootstrap-packages'."
  (interactive)
  (elpaish-install--execute args 'install))

;;;###autoload
(cl-defun elpaish-install-upgrade-packages (&rest args)
  "Upgrade a group of packages with continue-on-error semantics.
Runs `package-refresh-contents' before checking for upgrades.
It is not an error if nothing is upgradable.

ARGS can contain package symbols, lists of symbols, or keyword options:
  :async T      - Perform upgrading asynchronously (uses sprite pool if available).
  :pool-size N  - Number of sprite workers to use for async parallel upgrade.
  :callback FN  - Function to call when async upgrade finishes.

When no packages are specified in ARGS, defaults to `elpaish-install-bootstrap-packages'."
  (interactive)
  (elpaish-install--execute args 'upgrade))

(provide 'elpaish-install)

;; Local Variables:
;; package-lint-main-file: "pkg/elpaish.el"
;; End:
;;; elpaish-install.el ends here
