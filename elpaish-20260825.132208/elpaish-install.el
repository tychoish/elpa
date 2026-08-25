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

(defvaralias 'elpaish-bootstrap-packages 'elpaish-install-bootstrap-packages)

(defcustom elpaish-install-bootstrap-packages nil
  "List of package symbols to install or upgrade during bootstrapping.
Used as default target package list for `elpaish-install-packages'
and `elpaish-install-upgrade-packages' when no explicit packages are specified."
  :type '(repeat symbol)
  :group 'elpaish-install)

(defun elpaish-install--do-install-packages (pkgs refresh)
  "Perform actual installation sequence for PKGS.
If REFRESH is non-nil, call `package-refresh-contents' first."
  (unless (bound-and-true-p package-archive-contents)
    (package-initialize))
  (when refresh
    (package-refresh-contents))
  (let ((installed-count 0)
        (failed-count 0))
    (dolist (pkg pkgs)
      (if (package-installed-p pkg)
          (message "Package %s is already installed." pkg)
        (condition-case err
            (progn
              (message "Installing package %s..." pkg)
              (package-install pkg)
              (cl-incf installed-count)
              (message "Successfully installed %s." pkg))
          (error
           (cl-incf failed-count)
           (message "Failed to install %s: %s" pkg (error-message-string err))))))
    (message "Package installation complete: %d installed, %d failed."
             installed-count failed-count)))

;;;###autoload
(cl-defun elpaish-install-packages (&rest args)
  "Install a group of packages with continue-on-error semantics.
ARGS can contain package symbols, lists of symbols, or keyword options:
  :refresh T    - Call `package-refresh-contents' before installing.
  :async T      - Perform installation asynchronously in a background timer.

When no packages are specified in ARGS, defaults to `elpaish-install-bootstrap-packages'."
  (interactive)
  (let ((pkgs nil)
        (refresh nil)
        (async nil))
    (while args
      (let ((arg (pop args)))
        (cond
         ((eq arg :refresh) (setq refresh (pop args)))
         ((eq arg :async)   (setq async (pop args)))
         ((listp arg)       (dolist (p arg) (push p pkgs)))
         ((symbolp arg)     (push arg pkgs)))))
    (let ((target-pkgs (or (nreverse pkgs) elpaish-install-bootstrap-packages)))
      (if async
          (run-at-time 0 nil (lambda () (elpaish-install--do-install-packages target-pkgs refresh)))
        (elpaish-install--do-install-packages target-pkgs refresh)))))

(defun elpaish-install--do-upgrade-packages (pkgs)
  "Perform actual package upgrade sequence for PKGS."
  (unless (bound-and-true-p package-archive-contents)
    (package-initialize))
  (message "Refreshing package archive contents...")
  (package-refresh-contents)
  (let ((upgraded-count 0)
        (failed-count 0)
        (up-to-date-count 0))
    (dolist (pkg pkgs)
      (condition-case err
          (cond
           ((not (package-installed-p pkg))
            (message "Package %s is not installed; installing..." pkg)
            (package-install pkg)
            (cl-incf upgraded-count))
           ((fboundp 'package-upgrade)
            (condition-case sub-err
                (progn
                  (package-upgrade pkg)
                  (cl-incf upgraded-count)
                  (message "Successfully upgraded %s." pkg))
              (user-error
               (cl-incf up-to-date-count)
               (message "Package %s is up to date." pkg))
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
                    (cl-incf upgraded-count)
                    (message "Successfully upgraded %s from %s to %s."
                             pkg (package-version-join inst-ver) (package-version-join arch-ver)))
                (cl-incf up-to-date-count)
                (message "Package %s is up to date." pkg)))))
        (error
         (cl-incf failed-count)
         (message "Failed to upgrade %s: %s" pkg (error-message-string err)))))
    (message "Package upgrade complete: %d upgraded, %d up to date, %d failed."
             upgraded-count up-to-date-count failed-count)))

;;;###autoload
(cl-defun elpaish-install-upgrade-packages (&rest args)
  "Upgrade a group of packages with continue-on-error semantics.
Runs `package-refresh-contents' before checking for upgrades.
It is not an error if nothing is upgradable.

ARGS can contain package symbols, lists of symbols, or keyword options:
  :async T      - Perform upgrading asynchronously in a background timer.

When no packages are specified in ARGS, defaults to `elpaish-install-bootstrap-packages'."
  (interactive)
  (let ((pkgs nil)
        (async nil))
    (while args
      (let ((arg (pop args)))
        (cond
         ((eq arg :async) (setq async (pop args)))
         ((listp arg)     (dolist (p arg) (push p pkgs)))
         ((symbolp arg)   (push arg pkgs)))))
    (let ((target-pkgs (or (nreverse pkgs) elpaish-install-bootstrap-packages)))
      (if async
          (run-at-time 0 nil (lambda () (elpaish-install--do-upgrade-packages target-pkgs)))
        (elpaish-install--do-upgrade-packages target-pkgs)))))

(defalias 'elpaish-upgrade-packages 'elpaish-install-upgrade-packages)

(provide 'elpaish-install)

;; Local Variables:
;; package-lint-main-file: "pkg/elpaish.el"
;; End:
;;; elpaish-install.el ends here
