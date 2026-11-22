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
(require 'map)
(require 'package)
(require 'elpaish-async)
(require 'logx)
(require 'gen nil t)

(declare-function elpaish-recipe-p "elpaish" (object))
(declare-function elpaish-recipe-name "elpaish" (recipe))
(declare-function elpaish-recipe-requires "elpaish" (recipe))
(declare-function elpaish-recipe-summary "elpaish" (recipe))
(declare-function copy-elpaish-recipe "elpaish" (recipe))
(declare-function elpaish-recipe-name-from-url "elpaish-recipes" (url))
(declare-function elpaish-register-package "elpaish" (name repository-path &rest options))
(declare-function elpaish-build-package "elpaish" (recipe &optional stream output-directory-path))
(declare-function elpaish-async-build-package-in-sprite "elpaish-async" (recipe &rest args))
(declare-function elpaish--recipe-source-path "elpaish" (recipe))
(declare-function sprite-future-p "sprite" (object))
(declare-function sprite-future-then "sprite" (future callback &optional on-reject))
(declare-function sprite-future-cancel "sprite" (future &optional reason))
(declare-function gen-run-async "gen" (fn sequence &rest args))
(declare-function package--reload-previously-loaded "package" (pkg-desc &optional warn))
(declare-function package--description-file "package" (dir))
(declare-function package-quickstart-refresh "package" ())
(declare-function elpaish--git-string "elpaish" (dir &rest args))
(declare-function elpaish--git-lines "elpaish" (dir &rest args))
(declare-function elpaish--resolve-repo-path "elpaish" (recipe))
(declare-function elpaish-load-packages "elpaish-recipes" (&optional files))
(declare-function annotated-completing-read "annotated-completing-read" (table &rest args))

(defvar elpaish-work-dir)
(defvar elpaish-sign-packages)
(defvar elpaish-registry)
(defvar elpaish-release-mode)
(defvar elpaish--resolved-repo-path-cache)
(defvar package-quickstart-file)
(defvar package-user-dir)
(defvar warning-inhibit-types)
(defvar warning-suppress-types)
(defvar warning-suppress-log-types)

(defgroup elpaish-install nil
  "Package bootstrapping and upgrade utilities."
  :group 'package
  :prefix "elpaish-install-")

(defcustom elpaish-install-packages-list
  '(compat web-server htmlize annotated-completing-read logx transient package-lint elpaish-keyring elpaish)
  "List of package symbols to ensure are installed on the system.
Used as default target package list for `elpaish-install-packages'
when no explicit packages are specified."
  :type '(repeat symbol)
  :group 'elpaish-install)

(defcustom elpaish-install-upgrade-packages-list nil
  "List of package symbols to upgrade on demand.
Packages in this list must be a strict subset of
`elpaish-install-packages-list'.  Used as default target package list
for `elpaish-install-upgrade-packages'."
  :type '(repeat symbol)
  :group 'elpaish-install)

(defcustom elpaish-install-package-groups nil
  "Alist mapping package group symbols to group specifications.
Each entry has the form `(GROUP-NAME :policy POLICY :packages (PKG ...))'
or simple alist form `(GROUP-NAME . (PKG1 PKG2 ...))'."
  :type '(alist :key-type symbol :value-type sexp)
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

(defcustom elpaish-install-verbose nil
  "When non-nil, log every package checked during installation and bootstrap.
When nil, only log installed, upgraded, and failed packages, plus the
final summary."
  :type 'boolean
  :group 'elpaish-install)

(defcustom elpaish-install-confirm-intermediate nil
  "When non-nil, ask for intermediate confirmation before package work items.
Defaults to nil (off).  Can also be toggled interactively in
`elpaish-install-progress-mode' with `elpaish-install-progress-toggle-confirm'."
  :type 'boolean
  :group 'elpaish-install)

(defface elpaish-install-up-to-date-face
  '((t :inherit shadow))
  "Face used in the progress buffer to gray out up-to-date packages."
  :group 'elpaish-install)

(defcustom elpaish-install-cleanup-legacy nil
  "When non-nil, clean up older, duplicate versions of upgraded packages.
Defaults to nil (off).  Can also be toggled interactively in
`elpaish-install-progress-mode' with `elpaish-install-progress-toggle-cleanup'."
  :type 'boolean
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
(defun elpaish-install-register-package (&rest pkgs)
  "Append unique package symbols from PKGS to `elpaish-install-packages-list'.
PKGS can be package symbols or lists of package symbols.  Updates
`elpaish-install-packages-list' and returns it."
  (let ((new-pkgs nil))
    (dolist (p (flatten-tree pkgs))
      (when (and (symbolp p)
                 p
                 (not (memq p elpaish-install-packages-list))
                 (not (memq p new-pkgs)))
        (push p new-pkgs)))
    (when new-pkgs
      (setq elpaish-install-packages-list
            (append elpaish-install-packages-list (nreverse new-pkgs)))))
  elpaish-install-packages-list)

;;;###autoload
(defun elpaish-install-register-upgrade-package (&rest pkgs)
  "Append unique package symbols from PKGS to upgrade list.
PKGS can be package symbols or lists of package symbols.  Packages
are also ensured to be in `elpaish-install-packages-list' so that
the upgrade list is a strict subset of the installed packages list.
Returns `elpaish-install-upgrade-packages-list'."
  (let ((new-pkgs nil))
    (dolist (p (flatten-tree pkgs))
      (when (and (symbolp p) p)
        ;; Enforce strict subset: package must be in List 1
        (unless (memq p elpaish-install-packages-list)
          (elpaish-install-register-package p))
        (when (and (not (memq p elpaish-install-upgrade-packages-list))
                   (not (memq p new-pkgs)))
          (push p new-pkgs))))
    (when new-pkgs
      (setq elpaish-install-upgrade-packages-list
            (append elpaish-install-upgrade-packages-list (nreverse new-pkgs)))))
  elpaish-install-upgrade-packages-list)

(defun elpaish-install--group-policy (group-entry)
  "Return policy (`upgrade' or `install') for GROUP-ENTRY."
  (cond
   ((and (cdr group-entry) (keywordp (cadr group-entry)))
    (or (plist-get (cdr group-entry) :policy) 'upgrade))
   ((memq (car group-entry) '(install install-only))
    'install)
   (t 'upgrade)))

(defun elpaish-install--group-packages-list (group-entry)
  "Return list of package symbols from GROUP-ENTRY."
  (let ((raw (if (and (cdr group-entry) (keywordp (cadr group-entry)))
                 (plist-get (cdr group-entry) :packages)
               (cdr group-entry))))
    (seq-filter (lambda (p) (and (symbolp p) p))
                (delete-dups (flatten-tree raw)))))

;;;###autoload
(defun elpaish-install-get-group-packages (group)
  "Return list of package symbols registered under GROUP symbol."
  (when-let* ((entry (assq group elpaish-install-package-groups)))
    (elpaish-install--group-packages-list entry)))

;;;###autoload
(defun elpaish-install-register-group (group pkgs &optional policy)
  "Register package symbols PKGS under group symbol GROUP with POLICY.
POLICY can be `upgrade' (default) or `install' (install-only).
If GROUP is `install' or `install-only' and POLICY is nil, POLICY defaults
to `install'.
Packages are registered into `elpaish-install-packages-list' and, if POLICY
is `upgrade', into `elpaish-install-upgrade-packages-list'.
Returns the updated `elpaish-install-package-groups'."
  (unless (symbolp group)
    (user-error "GROUP must be a symbol, got %S" group))
  (let* ((actual-policy (or policy (if (memq group '(install install-only)) 'install 'upgrade)))
         (clean-pkgs (seq-filter (lambda (p) (and (symbolp p) p))
                                 (delete-dups (flatten-tree pkgs))))
         (existing (assq group elpaish-install-package-groups))
         (prior-pkgs (when existing (elpaish-install--group-packages-list existing)))
         (combined (delete-dups (append prior-pkgs clean-pkgs)))
         (spec (list :policy actual-policy :packages combined)))
    (if existing
        (setcdr existing spec)
      (push (cons group spec) elpaish-install-package-groups))
    (if (eq actual-policy 'upgrade)
        (apply #'elpaish-install-register-upgrade-package clean-pkgs)
      (apply #'elpaish-install-register-package clean-pkgs)))
  elpaish-install-package-groups)

;;;###autoload
(defun elpaish-install-register-groups (groups)
  "Register multiple package GROUPS.
GROUPS is an alist where each entry is `(GROUP-NAME . (PKG1 PKG2 ...))'
or `(GROUP-NAME :policy POLICY :packages (PKG1 ...))'.
Returns `elpaish-install-package-groups'."
  (dolist (entry groups)
    (cond
     ((and (consp entry) (keywordp (cadr entry)))
      (elpaish-install-register-group
       (car entry)
       (plist-get (cdr entry) :packages)
       (plist-get (cdr entry) :policy)))
     ((consp entry)
      (elpaish-install-register-group
       (car entry)
       (cdr entry)
       (if (memq (car entry) '(install install-only)) 'install 'upgrade)))))
  elpaish-install-package-groups)

(defun elpaish-install--get-upgrade-packages ()
  "Return packages from upgrade list that are in `elpaish-install-packages-list'."
  (seq-filter (lambda (p) (memq p elpaish-install-packages-list))
              elpaish-install-upgrade-packages-list))

(defun elpaish-install--extract-header-requires (file)
  "Extract package requirements list from FILE header using `package-buffer-info'."
  (require 'package)
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
          (logx 'info :msg (format "Refreshing package archive contents to install missing dependencies: %S" missing)
                :missing missing)
          (package-refresh-contents)
          (dolist (dep-pkg missing)
            (logx 'info :msg (format "Installing missing package dependency: %s" dep-pkg)
                  :pkg dep-pkg)
            (condition-case err
                (package-install dep-pkg)
              (error
               (logx 'warn :msg (format "Warning: Failed to install package dependency %s: %s"
                                        dep-pkg (error-message-string err))
                     :pkg dep-pkg :error (error-message-string err))))))))))

(defun elpaish-install--resolve-repository-recipe (pkg)
  "Return `elpaish-recipe' for PKG (symbol or string) from `elpaish-registry'."
  (when (and (boundp 'elpaish-registry)
             (hash-table-p elpaish-registry))
    (let ((name-str (if (symbolp pkg) (symbol-name pkg) pkg))
          (name-sym (if (symbolp pkg) pkg (intern pkg))))
      (or (map-elt elpaish-registry name-str)
          (map-elt elpaish-registry name-sym)))))

(defun elpaish-install--process-repository-package (pkg &optional stream)
  "Build and install single repository package PKG for STREAM.
Returns a result plist with `:status', `:pkg', `:from', `:to', `:desc',
or `:error' on failure."
  (require 'elpaish)
  (require 'elpaish-recipes)
  (require 'package)
  (unless (and (boundp 'elpaish-registry)
               (hash-table-p elpaish-registry)
               (> (hash-table-count elpaish-registry) 0))
    (elpaish-load-packages))
  (let* ((pkg-sym (if (symbolp pkg) pkg (intern (format "%s" pkg))))
         (pkg-name (symbol-name pkg-sym))
         (recipe (elpaish-install--resolve-repository-recipe pkg))
         (effective-stream (or stream 'snapshot))
         (prev-desc (and (boundp 'package-alist) (cadr (assq pkg-sym package-alist))))
         (prev-ver (and prev-desc (package-version-join (package-desc-version prev-desc)))))
    (if (null recipe)
        (progn
          (logx 'warn :msg (format "[elpaish] Skipping %s: no recipe found in repository" pkg-name)
                :pkg pkg-name)
          (list :status :failed :pkg pkg :error "No recipe found in repository"))
      (let* ((temp-dir (make-temp-file "elpaish-install-repo-" t))
             (build-dir (expand-file-name "build" temp-dir))
             (elpaish-sign-packages nil))
        (unwind-protect
            (condition-case err
                (let ((artifact (elpaish-build-package recipe effective-stream build-dir)))
                  (if (not (and artifact (file-exists-p artifact)))
                      (let ((err-msg (or (and (boundp 'elpaish--last-build-error) elpaish--last-build-error)
                                         "Build produced no artifact")))
                        (logx 'error :msg (format "[elpaish] Failed to build %s: %s" pkg-name err-msg)
                              :pkg pkg-name :error err-msg)
                        (list :status :failed :pkg pkg :error err-msg))
                    (let* ((built-desc (package-install-file artifact))
                           (desc (or (seq-find (lambda (candidate)
                                                 (equal (package-desc-version candidate)
                                                        (package-desc-version built-desc)))
                                               (cdr (assq (package-desc-name built-desc) package-alist)))
                                     built-desc))
                           (new-ver (and desc (package-version-join (package-desc-version desc))))
                           (status (if prev-ver :upgraded :installed))
                           (res (list :status status
                                      :pkg pkg
                                      :from prev-ver
                                      :to new-ver
                                      :desc desc)))
                      (when (and desc (fboundp 'package--reload-previously-loaded))
                        (condition-case nil
                            (package--reload-previously-loaded desc)
                          (error nil)))
                      (logx 'info :msg (format "[elpaish] Installed %s (%s)"
                                               pkg-name (or new-ver ""))
                            :pkg pkg-name :version new-ver)
                      (run-hook-with-args 'elpaish-install-package-updated-functions
                                          pkg-sym
                                          (if prev-ver 'upgraded 'installed)
                                          res)
                      res)))
              (error
               (logx 'error :msg (format "[elpaish] Failed %s: %s"
                                         pkg-name (error-message-string err))
                     :pkg pkg-name :error (error-message-string err))
               (list :status :failed :pkg pkg :error (error-message-string err))))
          (when (file-directory-p temp-dir)
            (delete-directory temp-dir t)))))))

;;;###autoload
(defun elpaish-install-repository (&optional repository &rest options)
  "Build REPOSITORY and install its package in the current Emacs session.
REPOSITORY is a local directory, Git URL, or `elpaish-recipe'.  When
REPOSITORY is omitted, nil, empty, or one of the scope symbols/keywords
\\='installed, \\='upgrade, or \\='all, delegates to
`elpaish-install-repository-packages' to build and install packages defined in
the current ELPAish repository.

For single repository strings, OPTIONS are keyword arguments accepted by
`elpaish-register-package'; :name overrides the name derived from the
repository path.  For example:

  (elpaish-install-repository \"https://example.org/foo.git\"
                              :name \"foo\" :branch \"main\")

A recipe object already contains its options and does not accept OPTIONS.
The build uses the snapshot stream and a temporary output directory.  Remote
repositories are cloned into a temporary checkout; local directories are used
as-is, so :branch applies only to remote repositories.  Artifacts are not
signed.  Dependencies are installed by package.el.
Return the installed `package-desc', or signal an error if the build fails.

To install packages defined in the current ELPAish repository (with ACR menu
selection across installed, upgrade, or all), see
`elpaish-install-repository-packages'."
  (interactive
   (list (let ((input (read-string "Repository directory, Git URL, or scope (installed/upgrade/all, default installed): ")))
           (if (string-empty-p (string-trim input))
               'installed
             input))))
  (cond
   ((or (null repository)
        (and (stringp repository) (string-empty-p (string-trim repository)))
        (memq repository '(installed upgrade all :installed :upgrade :all)))
    (let ((scope (cond
                  ((memq repository '(:installed installed)) 'installed)
                  ((memq repository '(:upgrade upgrade)) 'upgrade)
                  ((memq repository '(:all all)) 'all)
                  (t 'installed))))
      (apply #'elpaish-install-repository-packages scope options)))
   (t
    (require 'elpaish-recipes)
    (when (and (elpaish-recipe-p repository) options)
      (error "Recipe objects cannot be combined with registration options"))
    (unless (or (elpaish-recipe-p repository) (stringp repository))
      (signal 'wrong-type-argument (list 'string-or-recipe-p repository)))
    (unless (and (zerop (% (length options) 2))
                 (cl-loop for (key _value) on options by #'cddr always (keywordp key)))
      (error "Registration options must be keyword/value pairs"))
    (let* ((temp-dir (make-temp-file "elpaish-install-" t))
           (elpaish-work-dir (expand-file-name "repos" temp-dir))
           (elpaish-sign-packages nil)
           (elpaish-registry (make-hash-table :test 'equal))
           (elpaish--resolved-repo-path-cache (make-hash-table :test 'eq)))
      (unwind-protect
          (let* ((recipe
                  (if (elpaish-recipe-p repository)
                      (copy-elpaish-recipe repository)
                    (let ((name (or (plist-get options :name)
                                    (elpaish-recipe-name-from-url
                                     (directory-file-name repository))))
                          (args nil))
                      (while options
                        (let ((key (pop options)) (value (pop options)))
                          (unless (eq key :name)
                            (setq args (append args (list key value))))))
                      (apply #'elpaish-register-package name repository args))))
                 (artifact (elpaish-build-package recipe 'snapshot
                                                  (expand-file-name "build" temp-dir))))
            (unless artifact
              (error "Could not build package %s" (elpaish-recipe-name recipe)))
            (let* ((built-desc (package-install-file artifact))
                   (desc (or (seq-find (lambda (candidate)
                                         (equal (package-desc-version candidate)
                                                (package-desc-version built-desc)))
                                       (cdr (assq (package-desc-name built-desc) package-alist)))
                             built-desc)))
              (when (and desc (fboundp 'package--reload-previously-loaded))
                (package--reload-previously-loaded desc))
              desc))
        (delete-directory temp-dir t))))))

;;;###autoload
(cl-defun elpaish-install-repository-packages (&optional scope &key stream sync async delay confirm cleanup callback)
  "Build and install packages from the local ELPAish repository into Emacs.
Unlike `elpaish-install-packages' (which installs pre-built archives from
`package-archives') and `elpaish-install-repository' (which builds a single
package from an external Git URL or local directory), this operation builds
packages directly from the recipe definitions registered in `elpaish-registry'
and installs them into `package-user-dir' using `package-install-file',
reporting progress through the `*ELPAish Upgrade Progress*' interface.

SCOPE specifies which subset of repository packages to install:
  \\='installed (default) - Only packages in `elpaish-install-packages-list'
                         that have recipes in the repository.
  \\='upgrade            - Only packages in
                         `elpaish-install-upgrade-packages-list' that have
                         recipes in the repository.
  \\='all                - All packages defined in the ELPAish repository
                         (`elpaish-registry').
  A list of symbols/strings - The specified package recipes.
  A single symbol/string   - The specified single package recipe.

When called interactively, prompts with an ACR (`annotated-completing-read')
menu displaying candidate package counts for each scope
(installed, upgrade, all) to resolve ambiguity over which packages to install,
and runs asynchronously with cooperative yielding in
`*ELPAish Upgrade Progress*' (or synchronously when called with a prefix
argument \\[universal-argument]).

STREAM is the release mode stream to build (defaults to \\='snapshot).
SYNC forces synchronous execution; ASYNC forces asynchronous execution.
DELAY overrides `elpaish-install-yield-delay'; CONFIRM enables intermediate
confirmation prompts before each work item.  CLEANUP enables cleaning stale
duplicate versions.  CALLBACK is invoked with the list of installed
`package-desc' objects on completion.
Returns list of installed `package-desc' objects when synchronous, or a
cancellation handle function when asynchronous."
  (interactive
   (list
    (progn
      (require 'elpaish)
      (require 'elpaish-recipes)
      (unless (and (boundp 'elpaish-registry)
                   (hash-table-p elpaish-registry)
                   (> (hash-table-count elpaish-registry) 0))
        (elpaish-load-packages))
      (let* ((installed-pkgs
              (seq-filter (lambda (p) (elpaish-install--resolve-repository-recipe p))
                          elpaish-install-packages-list))
             (upgrade-pkgs
              (seq-filter (lambda (p) (elpaish-install--resolve-repository-recipe p))
                          (or (elpaish-install--get-upgrade-packages)
                              elpaish-install-upgrade-packages-list)))
             (all-count (if (and (boundp 'elpaish-registry) (hash-table-p elpaish-registry))
                            (hash-table-count elpaish-registry)
                          0))
             (table `(("installed" . ,(format "Packages registered for installation (%d package%s)"
                                              (length installed-pkgs)
                                              (if (= (length installed-pkgs) 1) "" "s")))
                      ("upgrade"   . ,(format "Packages registered for upgrade (%d package%s)"
                                              (length upgrade-pkgs)
                                              (if (= (length upgrade-pkgs) 1) "" "s")))
                      ("all"       . ,(format "All packages defined in repository (%d package%s)"
                                              all-count
                                              (if (= all-count 1) "" "s")))))
             (prompt "Install repository packages: ")
             (choice (if (fboundp 'annotated-completing-read)
                         (annotated-completing-read table
                                                    :prompt prompt
                                                    :default "installed"
                                                    :require-match t)
                       (completing-read prompt table nil t nil nil "installed"))))
        (intern (downcase (string-trim choice)))))
    :sync (not (null current-prefix-arg))
    :async (null current-prefix-arg)))
  (require 'elpaish)
  (require 'elpaish-recipes)
  (require 'package)
  (unless (and (boundp 'elpaish-registry)
               (hash-table-p elpaish-registry)
               (> (hash-table-count elpaish-registry) 0))
    (elpaish-load-packages))
  (let* ((scope-sym (cond
                     ((null scope) 'installed)
                     ((stringp scope) (intern (downcase (string-trim scope))))
                     ((keywordp scope) (intern (substring (symbol-name scope) 1)))
                     ((symbolp scope) (intern (downcase (symbol-name scope))))
                     (t scope)))
         (raw-targets
          (cond
           ((listp scope)
            scope)
           ((eq scope-sym 'all)
            (seq-map #'intern (map-keys elpaish-registry)))
           ((eq scope-sym 'upgrade)
            (seq-filter (lambda (p) (elpaish-install--resolve-repository-recipe p))
                        (or (elpaish-install--get-upgrade-packages)
                            elpaish-install-upgrade-packages-list)))
           ((eq scope-sym 'installed)
            (seq-filter (lambda (p) (elpaish-install--resolve-repository-recipe p))
                        elpaish-install-packages-list))
           ((symbolp scope-sym)
            (list scope-sym))
           (t
            (seq-filter (lambda (p) (elpaish-install--resolve-repository-recipe p))
                        elpaish-install-packages-list))))
         (target-pkgs (seq-map (lambda (p) (if (symbolp p) p (intern (format "%s" p))))
                               raw-targets))
         (effective-stream (or stream 'snapshot))
         (is-sync (cond
                   (sync t)
                   (async nil)
                   (t (and noninteractive t)))))
    (if (null target-pkgs)
        (progn
          (logx 'warn :msg (format "No repository package recipes found matching scope '%s'." scope-sym)
                :scope scope-sym)
          (when callback (funcall callback nil))
          nil)
      (when (and (not is-sync) elpaish-install--active-cancel-handle)
        (user-error "An ELPAish package operation is already running"))
      (let* ((extract-descs
              (lambda (res-list)
                (delq nil (seq-map (lambda (r) (plist-get r :desc)) res-list))))
             (wrapped-cb
              (when callback
                (lambda (res-list)
                  (funcall callback (funcall extract-descs res-list)))))
             (run-res
              (elpaish-install--upgrade-run
               target-pkgs nil is-sync delay wrapped-cb 'repo-install
               :stream effective-stream
               :confirm confirm
               :cleanup cleanup)))
        (if is-sync
            (funcall extract-descs run-res)
          run-res)))))

;;;###autoload
(cl-defun elpaish-install-repository-package (&optional package &key stream sync async delay confirm cleanup callback)
  "Build and install a PACKAGE from the local ELPAish repository into Emacs.
PACKAGE can be a package symbol, string name, recipe, or a list of packages.
When called interactively, prompts with an ACR (`annotated-completing-read')
interface allowing selection of a package from repository definitions (or
multiple packages when called with a prefix argument) and runs asynchronously
with cooperative yielding in `*ELPAish Upgrade Progress*'.  With a prefix
argument, runs synchronously.

STREAM is the release mode stream to build (defaults to \\='snapshot).
SYNC forces synchronous execution; ASYNC forces asynchronous execution.
DELAY overrides `elpaish-install-yield-delay'; CONFIRM enables intermediate
confirmation prompts.  CLEANUP enables removing older duplicate versions.
CALLBACK is an optional function invoked with the installed `package-desc'
object (or list of objects) upon completion.

Returns the installed `package-desc' object (or list of `package-desc' objects
if multiple packages were selected or passed) when synchronous, or a
cancellation handle function when asynchronous."
  (interactive
   (list
    (progn
      (require 'elpaish)
      (require 'elpaish-recipes)
      (unless (and (boundp 'elpaish-registry)
                   (hash-table-p elpaish-registry)
                   (> (hash-table-count elpaish-registry) 0))
        (elpaish-load-packages))
      (let* ((names (sort (map-keys elpaish-registry) #'string<))
             (table (seq-map (lambda (n)
                               (let* ((r (map-elt elpaish-registry n))
                                      (summary (if r (elpaish-recipe-summary r) "")))
                                 (cons n (if (string-empty-p summary) "registered recipe" summary))))
                             names))
             (multi current-prefix-arg)
             (prompt (if multi "Install repository package(s): " "Install repository package: "))
             (choice (if (fboundp 'annotated-completing-read)
                         (annotated-completing-read table
                                                    :prompt prompt
                                                    :require-match t
                                                    :multiple (and multi t))
                       (if multi
                           (completing-read-multiple prompt table nil t)
                         (completing-read prompt table nil t)))))
        (if (listp choice)
            (seq-map (lambda (c) (intern (string-trim c))) choice)
          (intern (string-trim choice)))))
    :sync (not (null current-prefix-arg))
    :async (null current-prefix-arg)))
  (unless package
    (user-error "No package specified"))
  (let* ((is-list (listp package))
         (targets (if is-list package (list package)))
         (is-sync (cond
                   (sync t)
                   (async nil)
                   (t (and noninteractive t))))
         (wrapped-cb (when callback
                       (lambda (descs)
                         (funcall callback (if is-list descs (car descs))))))
         (res (elpaish-install-repository-packages
               targets
               :stream stream
               :sync is-sync
               :async (not is-sync)
               :delay delay
               :confirm confirm
               :cleanup cleanup
               :callback wrapped-cb)))
    (if is-sync
        (if is-list
            res
          (or (car res)
              (error "Failed to install repository package: %s" package)))
      res)))

;;;; Core Single Package Operation

(defun elpaish-install--process-package (pkg &optional action refresh)
  "Perform single package PKG operation for ACTION (\\='upgrade or \\='install).
ACTION defaults to \\='upgrade, which installs PKG if missing or upgrades it
if an update is available.  If REFRESH is non-nil, refresh archive contents."
  (require 'package)
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

(defun elpaish-install--format-duration (seconds)
  "Format duration SECONDS as a concise human-readable string."
  (let ((secs (max 0.0 (or seconds 0.0))))
    (if (>= secs 60.0)
        (format "%dm %.1fs" (/ (truncate secs) 60) (mod secs 60.0))
      (format "%.2fs" secs))))

(defun elpaish-install--report-summary (res-list action &optional stats)
  "Display summary message for RES-LIST, ACTION, and optional timing STATS."
  (let ((installed nil)
        (upgraded nil)
        (up-to-date 0)
        (failed nil)
        (op-label (pcase action
                    ('install "Installation")
                    ('repo-install "Repository installation")
                    (_ "Bootstrap")))
        (timing-suffix
         (if (and stats (plist-get stats :wall-time))
             (format " [wall: %s, actual: %s, wait: %s]"
                     (elpaish-install--format-duration (plist-get stats :wall-time))
                     (elpaish-install--format-duration (plist-get stats :actual-time))
                     (elpaish-install--format-duration (plist-get stats :wait-time)))
           "")))
    (seq-do
     (lambda (res)
       (let ((p (plist-get res :pkg)))
         (pcase (plist-get res :status)
           (':installed (push p installed))
           (':upgraded (push p upgraded))
           (':up-to-date (cl-incf up-to-date))
           (':already-installed (cl-incf up-to-date))
           (':failed (push (cons p (plist-get res :error)) failed)))))
     res-list)
    (let ((parts nil))
      (when upgraded
        (push (format "%d upgraded (%s)"
                      (length upgraded)
                      (mapconcat (lambda (x) (format "%s" x)) (nreverse upgraded) ", "))
              parts))
      (when installed
        (push (format "%d installed (%s)"
                      (length installed)
                      (mapconcat (lambda (x) (format "%s" x)) (nreverse installed) ", "))
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
        (logx 'info :msg (format "[elpaish] %s complete: all %d package(s) up to date.%s"
                                 op-label
                                 up-to-date
                                 timing-suffix)
              :action action :status :all-up-to-date :count up-to-date
              :wall-time (plist-get stats :wall-time)
              :actual-time (plist-get stats :actual-time)
              :wait-time (plist-get stats :wait-time)))
       (t
        (let ((msg (format "[elpaish] %s complete: %s.%s"
                           op-label
                           (string-join (nreverse parts) ", ")
                           timing-suffix)))
          (if failed
              (logx 'warn :msg msg
                    :action action :installed installed :upgraded upgraded
                    :failed failed :up-to-date up-to-date
                    :wall-time (plist-get stats :wall-time)
                    :actual-time (plist-get stats :actual-time)
                    :wait-time (plist-get stats :wait-time))
            (logx 'info :msg msg
                  :action action :installed installed :upgraded upgraded
                  :failed failed :up-to-date up-to-date
                  :wall-time (plist-get stats :wall-time)
                  :actual-time (plist-get stats :actual-time)
                  :wait-time (plist-get stats :wait-time)))))))))

(defun elpaish-install--remote-eval-form (pkg action parent-user-dir parent-archives refresh)
  "Generate Lisp form for remote sprite execution of PKG with ACTION.
ACTION is \\='install or \\='upgrade.
PARENT-USER-DIR and PARENT-ARCHIVES configure the remote package environment.
If REFRESH is non-nil, refreshes package contents remotely."
  `(progn
     (eval-when-compile (require 'package))
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
           (logx 'info :msg (format "[elpaish] Installed and activated %s" pkg)
                 :pkg pkg :action 'install))
          (:upgraded
           (logx 'info :msg (format "[elpaish] Upgraded and reloaded %s" pkg)
                 :pkg pkg :action 'upgrade)))
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
        (logx 'info :msg "[elpaish] Refreshing package archive contents...")
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
               (logx 'info :msg (format "[elpaish] [%d/%d] Installed %s (activated)" idx total pkg)
                     :index idx :total total :pkg pkg :status :installed))
              (:upgraded
               (if (and (plist-get res :from) (plist-get res :to))
                   (logx 'info :msg (format "[elpaish] [%d/%d] Upgraded %s from %s to %s (reloaded)"
                                            idx total pkg (plist-get res :from) (plist-get res :to))
                         :index idx :total total :pkg pkg :from (plist-get res :from) :to (plist-get res :to) :status :upgraded)
                 (logx 'info :msg (format "[elpaish] [%d/%d] Upgraded %s (reloaded)" idx total pkg)
                       :index idx :total total :pkg pkg :status :upgraded)))
              (:failed
               (logx 'error :msg (format "[elpaish] [%d/%d] Failed %s: %s"
                                        idx total pkg (plist-get res :error))
                     :index idx :total total :pkg pkg :error (plist-get res :error) :status :failed))
              (_
               (when elpaish-install-verbose
                 (logx 'info :msg (format "[elpaish] [%d/%d] %s is up to date" idx total pkg)
                       :index idx :total total :pkg pkg :status :up-to-date))))))
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
        (logx 'info :msg (format "[elpaish] %s %d package(s) asynchronously using sprite pool..."
                                 (if (eq action 'install) "Installing" "Bootstrapping")
                                 (length target-pkgs))
              :action action :count (length target-pkgs) :backend 'sprite-pool)
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
        (logx 'info :msg "[elpaish] Refreshing package archive contents...")
        (package-refresh-contents))
      (let* ((results nil)
             (total (length target-pkgs))
             (idx 0))
        (logx 'info :msg (format "[elpaish] %s %d package(s) asynchronously..."
                                 (if (eq action 'install) "Installing" "Bootstrapping")
                                 total)
              :action action :total total)
        (let ((runner
               (gen-run-async
                (lambda (pkg done-cb)
                  (cl-incf idx)
                  (let ((res (elpaish-install--process-package pkg action nil)))
                    (push res results)
                    (pcase (plist-get res :status)
                      (:installed
                       (logx 'info :msg (format "[elpaish] [%d/%d] Installed %s (activated)" idx total pkg)
                             :index idx :total total :pkg pkg :status :installed))
                      (:upgraded
                       (if (and (plist-get res :from) (plist-get res :to))
                           (logx 'info :msg (format "[elpaish] [%d/%d] Upgraded %s from %s to %s (reloaded)"
                                                    idx total pkg (plist-get res :from) (plist-get res :to))
                                 :index idx :total total :pkg pkg :from (plist-get res :from) :to (plist-get res :to) :status :upgraded)
                         (logx 'info :msg (format "[elpaish] [%d/%d] Upgraded %s (reloaded)" idx total pkg)
                               :index idx :total total :pkg pkg :status :upgraded)))
                      (:failed
                       (logx 'error :msg (format "[elpaish] [%d/%d] Failed %s: %s"
                                                idx total pkg (plist-get res :error))
                             :index idx :total total :pkg pkg :error (plist-get res :error) :status :failed))
                      (_
                       (when elpaish-install-verbose
                         (logx 'info :msg (format "[elpaish] [%d/%d] %s is up to date" idx total pkg)
                               :index idx :total total :pkg pkg :status :up-to-date))))
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
                  (logx 'error :msg (format "[elpaish] Error during package bootstrapping: %S" err)
                        :error err)))))
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
  (let ((handle elpaish-install--active-cancel-handle))
    (setq elpaish-install--active-cancel-handle nil)
    (when (functionp handle)
      (ignore-errors (funcall handle :cancel)))))

;;;; Public Commands and Dispatchers

;;;###autoload
(cl-defun elpaish-install-packages (&rest args)
  "Ensure packages are installed on the system, blocking until complete.
Only refreshes package archives if at least one package is missing.
Packages in ARGS (or `elpaish-install-packages-list' if no packages specified)
are registered into `elpaish-install-packages-list'.

This installation operation is synchronous (blocking) by default, matching the
semantics of `use-package <name> :ensure t :defer t' during startup.

ARGS can contain package symbols, lists of package symbols, or keyword options:
  :refresh BOOL     - When non-nil, force package archive refresh even if
                      all packages are already installed.
  :sync BOOL        - When non-nil, execute synchronously (default t).
  :async BOOL       - When non-nil, execute asynchronously.
  :action ACTION    - Operation action.  If \='upgrade, delegates to
                      `elpaish-install-upgrade-packages'."
  (interactive)
  (let* ((interactive-p (called-interactively-p 'any))
         (remaining (if interactive-p nil args))
         (pkgs nil)
         (force-refresh nil)
         (action 'install)
         (sync nil)
         (async nil)
         (concurrency nil)
         (delay nil)
         (pool-size nil)
         (callback nil))
    (while remaining
      (let ((arg (pop remaining)))
        (cond
         ((eq arg :refresh)     (setq force-refresh (pop remaining)))
         ((eq arg :action)      (setq action (pop remaining)))
         ((eq arg :sync)        (setq sync (pop remaining)))
         ((eq arg :async)       (setq async (pop remaining)))
         ((eq arg :concurrency) (setq concurrency (pop remaining)))
         ((eq arg :delay)       (setq delay (pop remaining)))
         ((eq arg :pool-size)   (setq pool-size (pop remaining)))
         ((or (eq arg :callback) (eq arg :on-done))
          (setq callback (pop remaining)))
         ((listp arg)
          (dolist (p (flatten-tree arg))
            (when (symbolp p) (push p pkgs))))
         ((symbolp arg)
          (push arg pkgs)))))
    (setq pkgs (delete-dups (nreverse pkgs)))
    (cond
     ((eq action 'upgrade)
      (apply #'elpaish-install-upgrade-packages
             (append pkgs
                     (list :refresh force-refresh :sync sync :async async
                           :concurrency concurrency :delay delay
                           :pool-size pool-size :callback callback))))
     (t
      (when pkgs
        (apply #'elpaish-install-register-package pkgs))
      (let* ((target-pkgs (or pkgs elpaish-install-packages-list))
             (missing (seq-remove #'package-installed-p target-pkgs))
             (is-async (and async (not sync))))
        (cond
         ((and (null missing) (not force-refresh))
          (when interactive-p
            (logx 'info :msg (format "[elpaish] All %d package(s) are already installed."
                                     (length target-pkgs))
                  :count (length target-pkgs)))
          (when callback (funcall callback nil))
          nil)
         (is-async
          (elpaish-install--do-packages-async
           (if force-refresh target-pkgs missing)
           :action 'install
           :refresh force-refresh
           :concurrency concurrency
           :delay delay
           :pool-size pool-size
           :callback callback))
         ((and interactive-p (not noninteractive))
          (let ((to-install (if force-refresh target-pkgs missing)))
            (elpaish-install--upgrade-run to-install force-refresh sync delay callback 'install)))
         (t
          (unless (bound-and-true-p package-archive-contents)
            (package-initialize))
          (when (or force-refresh missing (null package-archive-contents))
            (logx 'info :msg "[elpaish] Refreshing package archive contents...")
            (package-refresh-contents))
          (let* ((to-install (if force-refresh target-pkgs missing))
                 (results nil)
                 (total (length to-install))
                 (idx 0))
            (dolist (pkg to-install)
              (cl-incf idx)
              (let ((res (elpaish-install--process-package pkg 'install nil)))
                (push res results)
                (pcase (plist-get res :status)
                  (:installed
                   (logx 'info :msg (format "[elpaish] [%d/%d] Installed %s (activated)" idx total pkg)
                         :index idx :total total :pkg pkg :status :installed))
                  (:failed
                   (logx 'error :msg (format "[elpaish] [%d/%d] Failed %s: %s"
                                            idx total pkg (plist-get res :error))
                         :index idx :total total :pkg pkg :error (plist-get res :error) :status :failed))
                  (_
                   (when elpaish-install-verbose
                     (logx 'info :msg (format "[elpaish] [%d/%d] %s is up to date" idx total pkg)
                           :index idx :total total :pkg pkg :status :up-to-date))))))
            (let ((final-res (nreverse results)))
              (elpaish-install--report-summary final-res 'install)
              (run-hook-with-args 'elpaish-install-bootstrap-finished-functions
                                  final-res 'install)
              (when callback (funcall callback final-res))
              final-res)))))))))

(defun elpaish-install--installed-version (pkg)
  "Return the installed version of PKG as a string, or a placeholder."
  (if-let* ((desc (cadr (assq pkg package-alist))))
      (package-version-join (package-desc-version desc))
    "not installed"))

(defun elpaish-install--archive-version (pkg)
  "Return available archive version string for PKG, or nil if unknown."
  (when-let* ((entry (and (boundp 'package-archive-contents)
                          (listp package-archive-contents)
                          (assq pkg package-archive-contents)))
              (desc (cadr entry))
              (ver (and (package-desc-p desc) (package-desc-version desc))))
    (package-version-join ver)))

(defun elpaish-install--version-generation-time (version)
  "Extract generation timestamp string from snapshot VERSION if date-based.
VERSION may be a version string (e.g. \"20261118.134022\" or \"20261118.1309\")
or a version list.  Returns \"YYYY-MM-DD HH:MM\" or nil."
  (let ((ver-str (cond
                  ((stringp version) version)
                  ((and (listp version) version)
                   (condition-case nil
                       (package-version-join version)
                     (error nil)))
                  (t nil))))
    (when (and ver-str
               (string-match
                "\\`\\([12][0-9]\\{3\\}\\)\\([01][0-9]\\)\\([0-3][0-9]\\)\\.\\([0-9]+\\)\\'"
                ver-str))
      (let* ((year (string-to-number (match-string 1 ver-str)))
             (month (string-to-number (match-string 2 ver-str)))
             (day (string-to-number (match-string 3 ver-str)))
             (time-str (match-string 4 ver-str))
             (time-num (string-to-number time-str))
             (len (length time-str))
             (hour (cond
                    ((= len 6) (string-to-number (substring time-str 0 2)))
                    ((= len 4) (string-to-number (substring time-str 0 2)))
                    ((> time-num 2400) (/ time-num 10000))
                    (t (/ time-num 100))))
             (min (cond
                   ((= len 6) (string-to-number (substring time-str 2 4)))
                   ((= len 4) (string-to-number (substring time-str 2 4)))
                   ((> time-num 2400) (% (/ time-num 100) 100))
                   (t (% time-num 100)))))
        (when (and (<= 1 month 12)
                   (<= 1 day 31)
                   (<= 0 hour 23)
                   (<= 0 min 59))
          (format "%04d-%02d-%02d %02d:%02d" year month day hour min))))))

(defun elpaish-install--package-previous-timestamps (pkg &optional version-str)
  "Return plist of previous version and timestamps for PKG.
Inspects `package-alist' and `package-user-dir' using VERSION-STR if given."
  (let* ((desc (and (boundp 'package-alist) (cadr (assq pkg package-alist))))
         (ver (or version-str
                  (and (package-desc-p desc)
                       (package-version-join (package-desc-version desc)))
                  (elpaish-install--installed-version pkg)))
         (installed-p (and (stringp ver) (not (string= ver "not installed"))))
         (inst-dir (and installed-p
                        (or (and (package-desc-p desc)
                                 (stringp (package-desc-dir desc))
                                 (file-directory-p (package-desc-dir desc))
                                 (package-desc-dir desc))
                            (elpaish-install--package-install-dir pkg ver))))
         (raw-ver (when (and inst-dir
                             (string-match (format "\\`%s-\\(.+\\)\\'"
                                                   (regexp-quote (symbol-name pkg)))
                                           (file-name-nondirectory
                                            (directory-file-name inst-dir))))
                    (match-string 1 (file-name-nondirectory
                                     (directory-file-name inst-dir)))))
         (pkg-file (and inst-dir
                        (let ((p1 (expand-file-name (format "%s-pkg.el" pkg) inst-dir))
                              (p2 (expand-file-name (format "%s.el" pkg) inst-dir)))
                          (cond
                           ((file-exists-p p1) p1)
                           ((file-exists-p p2) p2)
                           (t nil)))))
         (autoloads-file (and inst-dir
                              (let ((af (expand-file-name (format "%s-autoloads.el" pkg) inst-dir)))
                                (when (file-exists-p af) af))))
         (prev-gen (when installed-p
                     (or (elpaish-install--version-generation-time (or raw-ver ver))
                         (when pkg-file
                           (format-time-string
                            "%Y-%m-%d %H:%M"
                            (file-attribute-modification-time
                             (file-attributes pkg-file)))))))
         (prev-local (when inst-dir
                       (let ((target (or autoloads-file inst-dir)))
                         (when (file-exists-p target)
                           (format-time-string
                            "%Y-%m-%d %H:%M"
                            (file-attribute-modification-time
                             (file-attributes target))))))))
    (list :prev-version (when installed-p ver)
          :prev-generated prev-gen
          :prev-local-update prev-local)))

(defun elpaish-install--package-up-to-date-p (pkg &optional action)
  "Return non-nil when PKG is already up to date for ACTION."
  (let ((act (or action 'upgrade)))
    (cond
     ((eq act 'install)
      (package-installed-p pkg))
     ((eq act 'repo-install)
      nil)
     (t
      (when (package-installed-p pkg)
        (let* ((inst-desc (and (boundp 'package-alist)
                               (cadr (assq pkg package-alist))))
               (arch-entry (and (boundp 'package-archive-contents)
                                (listp package-archive-contents)
                                (assq pkg package-archive-contents)))
               (arch-desc (cadr arch-entry))
               (inst-ver (and (package-desc-p inst-desc)
                              (package-desc-version inst-desc)))
               (arch-ver (and (package-desc-p arch-desc)
                              (package-desc-version arch-desc))))
          (cond
           ((and inst-ver arch-ver)
            (not (version-list-< inst-ver arch-ver)))
           ((and inst-ver
                 (bound-and-true-p package-archive-contents)
                 (seq-some (lambda (e) (and (consp e) (package-desc-p (cadr e))))
                           package-archive-contents))
            t)
           (t nil))))))))

(defun elpaish-install--reorder-entries (entries)
  "Return ENTRIES reordered with up-to-date packages first and work items at end."
  (let ((up-to-date nil)
        (work-items nil))
    (seq-do
     (lambda (entry)
       (if (plist-get (nth 4 entry) :up-to-date)
           (push entry up-to-date)
         (push entry work-items)))
     entries)
    (append (nreverse up-to-date) (nreverse work-items))))

(defvar-local elpaish-install--progress-last-active nil
  "Most recently displayed active stage or package.")

(defvar-local elpaish-install--progress-context nil
  "Context plist for restarting or continuing the current progress operation.")

(defvar-local elpaish-install--progress-step-fn nil
  "Step function for resuming progress execution.")

(defvar-local elpaish-install--progress-control-fn nil
  "Control dispatcher function for mutating active progress execution state.")

(defun elpaish-install--icon-string (marker)
  "Return propertized MARKER with visual icon display property.
Preserves underlying string text so ERT string matching remains intact."
  (let ((icon (pcase marker
                ("[x]" "✓ ")
                ("[!]" "✗ ")
                ("[...]" "⏳ ")
                ("[ ]" "○ ")
                ("[-]" "⊘ ")
                (_ nil))))
    (if icon
        (propertize marker 'display icon)
      marker)))

(defun elpaish-install--archives-summary (&optional initial-times refresh-state active-archive)
  "Format string summarizing package archives.
Include status icons and old -> new timestamps.
INITIAL-TIMES is an alist mapping archive names to timestamps before refresh.
REFRESH-STATE indicates refresh stage ([ ], [...], [x], [-]).  ACTIVE-ARCHIVE
indicates which archive is actively downloading."
  (if (and (boundp 'package-archives) package-archives)
      (let ((lines nil))
        (seq-do
         (lambda (archive)
           (let* ((name (car archive))
                  (archive-dir (and (boundp 'package-user-dir)
                                    (expand-file-name (format "archives/%s" name) package-user-dir)))
                  (content-file (and archive-dir (expand-file-name "archive-contents" archive-dir)))
                  (cur-mtime (when (and content-file (file-exists-p content-file))
                               (format-time-string "%Y-%m-%d %H:%M"
                                                   (file-attribute-modification-time
                                                    (file-attributes content-file)))))
                  (old-mtime (or (cdr (assoc name initial-times)) cur-mtime "not updated"))
                  (new-mtime (or cur-mtime "not updated"))
                  (is-active (equal active-archive name))
                  (marker (cond
                           (is-active "[...]")
                           ((string= refresh-state "[-]") "[-]")
                           ((member refresh-state '("[x]" "[-]")) "[x]")
                           ((string= refresh-state "[...]") "[ ]")
                           (t "[ ]")))
                  (icon (elpaish-install--icon-string marker))
                  (text (cond
                         (is-active
                          (format "  %s %-10s Downloading archive: %s" icon name name))
                         ((string= refresh-state "[-]")
                          (format "  %s %-10s %s" icon name old-mtime))
                         ((or (not initial-times) (equal old-mtime new-mtime))
                          (format "  %s %-10s %s -> %s" icon name old-mtime new-mtime))
                         (t
                          (format "  %s %-10s %s -> %s" icon name old-mtime new-mtime)))))
             (push text lines)))
         package-archives)
        (concat "Package Archives:\n" (mapconcat #'identity (nreverse lines) "\n") "\n"))
    ""))

(defun elpaish-install--insert-version-link (pkg ver)
  "Insert VER as a Dired button for PKG if installed directory exists.
Otherwise, insert VER as plain text."
  (if (and ver (stringp ver) (not (string= ver "not installed")))
      (if-let* ((dir (elpaish-install--package-install-dir pkg ver)))
          (insert-button ver
                         'action (lambda (_) (dired dir))
                         'help-echo (format "Open %s in Dired" dir)
                         'follow-link t)
        (insert ver))
    (insert (or ver ""))))

(defun elpaish-install--package-source (pkg)
  "Return repository source archive string for PKG if known."
  (if-let* ((desc (and (boundp 'package-alist) (cadr (assq pkg package-alist))))
            (arch (and (package-desc-p desc) (package-desc-archive desc))))
      arch
    (if-let* ((entry (and (boundp 'package-archive-contents) (assq pkg package-archive-contents)))
              (desc (cadr entry))
              (arch (and (package-desc-p desc) (package-desc-archive desc))))
        arch
      (if (and (boundp 'elpaish-registry)
               (hash-table-p elpaish-registry)
               (or (map-elt elpaish-registry (symbol-name pkg))
                   (map-elt elpaish-registry pkg)))
          "elpaish"
        nil))))

(defun elpaish-install--package-install-dir (pkg version)
  "Return installed directory path for PKG and VERSION if it exists."
  (when (and (boundp 'package-user-dir)
             (stringp version)
             (not (string= version "not installed")))
    (let ((dir (expand-file-name (format "%s-%s" pkg version) package-user-dir)))
      (when (file-directory-p dir) dir))))

(defvar elpaish-install-progress-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'elpaish-install-progress-restart)
    (define-key map (kbd "c") #'elpaish-install-progress-continue)
    (define-key map (kbd "r") #'elpaish-install-progress-continue)
    (define-key map (kbd "p") #'elpaish-install-progress-toggle-pause)
    (define-key map (kbd "SPC") #'elpaish-install-progress-toggle-pause)
    (define-key map (kbd "P") #'elpaish-install-progress-pause)
    (define-key map (kbd "+") #'elpaish-install-progress-slow-down)
    (define-key map (kbd "=") #'elpaish-install-progress-slow-down)
    (define-key map (kbd ">") #'elpaish-install-progress-slow-down)
    (define-key map (kbd "s") #'elpaish-install-progress-slow-down)
    (define-key map (kbd "-") #'elpaish-install-progress-speed-up)
    (define-key map (kbd "<") #'elpaish-install-progress-speed-up)
    (define-key map (kbd "d") #'elpaish-install-progress-set-delay)
    (define-key map (kbd "i") #'elpaish-install-progress-toggle-confirm)
    (define-key map (kbd "C") #'elpaish-install-progress-toggle-confirm)
    (define-key map (kbd "l") #'elpaish-install-progress-toggle-cleanup)
    (define-key map (kbd "L") #'elpaish-install-progress-toggle-cleanup)
    (define-key map (kbd "C-g") #'elpaish-install-progress-soft-quit)
    (define-key map (kbd "q") #'elpaish-install-progress-quit)
    (define-key map (kbd "x") #'elpaish-install-progress-cancel)
    (define-key map (kbd "k") #'elpaish-install-progress-cancel)
    (define-key map (kbd "m") #'elpaish-install-progress-menu)
    (define-key map (kbd "?") #'elpaish-install-progress-menu)
    map)
  "Keymap for `elpaish-install-progress-mode'.")

(define-derived-mode elpaish-install-progress-mode special-mode "ELPAish"
  "Major mode for ELPAish package installation and upgrade progress."
  (setq buffer-undo-list t)
  (setq-local revert-buffer-function (lambda (_ignore-auto _noconfirm) (elpaish-install-progress-restart)))
  (setq-local mode-line-format
              '(" " mode-line-buffer-identification
                "  [" (:eval (or (and (boundp 'elpaish-install--progress-context)
                                      (plist-get elpaish-install--progress-context :global-state))
                                 "ELPAish"))
                "] "
                mode-line-end-spaces))
  (visual-line-mode 1))

(defun elpaish-install-progress-soft-quit ()
  "Dismiss transient input without canceling the package operation."
  (interactive)
  (message "Operation continues; use x to cancel."))

(defun elpaish-install--progress-query-kill ()
  "Buffer query function to prevent accidental exit during active upgrade."
  (if (and (bound-and-true-p elpaish-install--active-cancel-handle)
           (plist-get elpaish-install--progress-context :running))
      (if (yes-or-no-p "An ELPAish package operation is in progress. Really abort? ")
          (progn
            (elpaish-install-cancel)
            t)
        nil)
    t))

(defun elpaish-install-progress-quit ()
  "Quit progress window, querying if operation is active."
  (interactive)
  (if (and (bound-and-true-p elpaish-install--active-cancel-handle)
           (plist-get elpaish-install--progress-context :running))
      (when (yes-or-no-p "An ELPAish package operation is in progress. Really abort? ")
        (elpaish-install-cancel)
        (quit-window))
    (quit-window)))

(defun elpaish-install-progress-cancel ()
  "Cancel running ELPAish package operation."
  (interactive)
  (if (and (bound-and-true-p elpaish-install--active-cancel-handle)
           (plist-get elpaish-install--progress-context :running))
      (when (yes-or-no-p "Cancel current ELPAish package operation? ")
        (elpaish-install-cancel)
        (logx 'info :msg "[elpaish] Operation canceled."))
    (logx 'warn :msg "[elpaish] No active package operation to cancel.")))

(defun elpaish-install-progress-restart (&optional _ignore-auto _noconfirm)
  "Restart package operation using stored context."
  (interactive)
  (if-let* ((ctx elpaish-install--progress-context))
      (let ((pkgs (plist-get ctx :pkgs))
            (refresh (plist-get ctx :refresh))
            (sync (plist-get ctx :sync))
            (delay (plist-get ctx :delay))
            (callback (plist-get ctx :callback))
            (action (plist-get ctx :action))
            (stream (plist-get ctx :stream))
            (confirm (plist-get ctx :confirm))
            (cleanup (plist-get ctx :cleanup)))
        (when (and (bound-and-true-p elpaish-install--active-cancel-handle)
                   (plist-get ctx :running))
          (elpaish-install-cancel))
        (elpaish-install--upgrade-run pkgs refresh sync delay callback action
                                      :stream stream :confirm confirm :cleanup cleanup))
    (user-error "No previous package operation context found to restart")))

(defun elpaish-install-progress-pause ()
  "Pause the active ELPAish package operation."
  (interactive)
  (if (plist-get elpaish-install--progress-context :running)
      (progn
        (setf (plist-get elpaish-install--progress-context :paused) t)
        (when (functionp elpaish-install--progress-control-fn)
          (funcall elpaish-install--progress-control-fn :pause))
        (logx 'info :msg "[elpaish] Package operation paused."))
    (logx 'warn :msg "[elpaish] No active package operation to pause.")))

(defun elpaish-install-progress-continue ()
  "Resume or step current package operation immediately."
  (interactive)
  (if (and (or elpaish-install--progress-control-fn
               elpaish-install--progress-step-fn)
           (plist-get elpaish-install--progress-context :running))
      (progn
        (setf (plist-get elpaish-install--progress-context :paused) nil)
        (logx 'info :msg "[elpaish] Continuing package operation...")
        (if (functionp elpaish-install--progress-control-fn)
            (funcall elpaish-install--progress-control-fn :resume)
          (funcall elpaish-install--progress-step-fn)))
    (logx 'warn :msg "[elpaish] Operation is not paused or already finished.")))

(defun elpaish-install-progress-toggle-pause ()
  "Toggle pause and resume on the active ELPAish package operation."
  (interactive)
  (if (not (plist-get elpaish-install--progress-context :running))
      (logx 'warn :msg "[elpaish] No active package operation.")
    (if (plist-get elpaish-install--progress-context :paused)
        (elpaish-install-progress-continue)
      (elpaish-install-progress-pause))))

(defun elpaish-install-progress-set-delay (delay)
  "Set inter-step yield DELAY (in seconds) for the current package operation."
  (interactive
   (list (read-number "Yield delay between steps (seconds): "
                      (or (plist-get elpaish-install--progress-context :delay)
                          elpaish-install-yield-delay
                          0.005))))
  (let ((new-delay (max 0.0 (float delay))))
    (when elpaish-install--progress-context
      (setf (plist-get elpaish-install--progress-context :delay) new-delay))
    (when (functionp elpaish-install--progress-control-fn)
      (funcall elpaish-install--progress-control-fn :set-delay new-delay))
    (logx 'info :msg (format "[elpaish] Set inter-step delay to %.3fs." new-delay)
          :delay new-delay)
    new-delay))

(defun elpaish-install-progress-slow-down (&optional factor)
  "Increase yield delay between package steps to slow down execution.
Optional FACTOR multiplies the current delay (defaults to 2.0, minimum 0.1s)."
  (interactive)
  (let* ((cur (or (plist-get elpaish-install--progress-context :delay)
                  elpaish-install-yield-delay
                  0.005))
         (mult (if (numberp factor) (max 1.1 (float factor)) 2.0))
         (new-delay (max 0.1 (* cur mult))))
    (elpaish-install-progress-set-delay new-delay)))

(defun elpaish-install-progress-speed-up (&optional factor)
  "Decrease yield delay between package steps to speed up execution.
Optional FACTOR divides the current delay (defaults to 2.0)."
  (interactive)
  (let* ((cur (or (plist-get elpaish-install--progress-context :delay)
                  elpaish-install-yield-delay
                  0.005))
         (div (if (numberp factor) (max 1.1 (float factor)) 2.0))
         (min-delay (min 0.005 (or elpaish-install-yield-delay 0.005)))
         (new-delay (max min-delay (/ (float cur) div))))
    (elpaish-install-progress-set-delay new-delay)))

(defun elpaish-install-progress-toggle-confirm ()
  "Toggle intermediate confirmation prompts for the current operation."
  (interactive)
  (let ((new-val (not (plist-get elpaish-install--progress-context :confirm))))
    (when elpaish-install--progress-context
      (setf (plist-get elpaish-install--progress-context :confirm) new-val))
    (when (functionp elpaish-install--progress-control-fn)
      (funcall elpaish-install--progress-control-fn :set-confirm new-val))
    (logx 'info :msg (format "[elpaish] Intermediate confirmation %s."
                             (if new-val "enabled" "disabled"))
          :confirm new-val)
    new-val))

(defun elpaish-install-progress-toggle-cleanup ()
  "Toggle cleanup of legacy/stale package versions for the current operation."
  (interactive)
  (let ((new-val (not (plist-get elpaish-install--progress-context :cleanup))))
    (when elpaish-install--progress-context
      (setf (plist-get elpaish-install--progress-context :cleanup) new-val))
    (when (functionp elpaish-install--progress-control-fn)
      (funcall elpaish-install--progress-control-fn :set-cleanup new-val))
    (logx 'info :msg (format "[elpaish] Cleanup legacy packages %s."
                             (if new-val "enabled" "disabled"))
          :cleanup new-val)
    new-val))

(defun elpaish-install-progress-menu ()
  "Action menu for ELPAish package progress buffer."
  (interactive)
  (let* ((actions '(("Pause / Resume operation" . elpaish-install-progress-toggle-pause)
                    ("Continue / Step operation" . elpaish-install-progress-continue)
                    ("Pause operation" . elpaish-install-progress-pause)
                    ("Slow down execution" . elpaish-install-progress-slow-down)
                    ("Speed up execution" . elpaish-install-progress-speed-up)
                    ("Set yield delay" . elpaish-install-progress-set-delay)
                    ("Toggle intermediate confirmation" . elpaish-install-progress-toggle-confirm)
                    ("Toggle cleanup legacy packages" . elpaish-install-progress-toggle-cleanup)
                    ("Restart operation" . elpaish-install-progress-restart)
                    ("Cancel operation" . elpaish-install-progress-cancel)
                    ("Install repository package" . elpaish-install-repository-package)
                    ("Install repository packages" . elpaish-install-repository-packages)
                    ("Clean stale packages" . elpaish-install-clean-stale-packages)
                    ("Rebuild local package" . elpaish-install-rebuild-local-package)
                    ("Quit progress window" . elpaish-install-progress-quit)))
         (choice (if (fboundp 'annotated-completing-read)
                     (annotated-completing-read actions :prompt "ELPAish Action: ")
                   (completing-read "ELPAish Action: " actions nil t))))
    (when-let* ((cmd (map-elt actions choice)))
      (call-interactively cmd))))

(defun elpaish-install--upgrade-progress (buffer entries refresh-state quickstart-state &optional global-state active-pkg action stats cleanup-state)
  "Render upgrade ENTRIES and stage states in BUFFER.
GLOBAL-STATE indicates engine status (Working, Yielding, Paused, Completed,
Canceled).  ACTIVE-PKG is the package currently being processed.  ACTION is
\\='upgrade, \\='install, or \\='repo-install.  STATS is an optional timing
and options plist.  CLEANUP-STATE is the stale package cleanup status."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (active-pos nil)
             (win (get-buffer-window buffer t))
             (old-point (and win (window-point win)))
             (active-key (or (plist-get elpaish-install--progress-context :active-archive)
                             active-pkg
                             (and (string= refresh-state "[...]") 'refresh)
                             (and (string= quickstart-state "[...]") 'quickstart)))
             (op-title (pcase action
                         ('install "installation")
                         ('repo-install "repository installation")
                         (_ "upgrade")))
             (status-lbl (or global-state "Working"))
             (ctx elpaish-install--progress-context)
             (total-count (length entries))
             (utd-count (length (seq-filter (lambda (e) (plist-get (nth 4 e) :up-to-date))
                                            entries)))
             (work-entries (seq-remove (lambda (e) (plist-get (nth 4 e) :up-to-date))
                                       entries))
             (work-total (length work-entries))
             (work-done (length (seq-filter (lambda (e)
                                              (member (nth 1 e) '("[x]" "[!]" "[-]")))
                                            work-entries)))
             (wall-time (or (plist-get stats :wall-time)
                            (plist-get ctx :wall-time)
                            0.0))
             (actual-time (or (plist-get stats :actual-time)
                              (plist-get ctx :actual-time)
                              0.0))
             (wait-time (or (plist-get stats :wait-time)
                            (plist-get ctx :wait-time)
                            0.0))
             (cur-delay (or (plist-get stats :delay)
                            (plist-get ctx :delay)
                            elpaish-install-yield-delay
                            0.005))
             (cur-confirm (if (plist-member stats :confirm)
                              (plist-get stats :confirm)
                            (plist-get ctx :confirm)))
             (cur-cleanup (if (plist-member stats :cleanup)
                              (plist-get stats :cleanup)
                            (plist-get ctx :cleanup))))
        (setq-local header-line-format nil)
        (erase-buffer)
        (insert (propertize (format "ELPAish package %s [%s]\n" op-title status-lbl)
                            'face '(:inherit header-line :weight bold)))
        (insert (format "Duration: %s wall (%s actual, %s waiting)\n"
                        (elpaish-install--format-duration wall-time)
                        (elpaish-install--format-duration actual-time)
                        (elpaish-install--format-duration wait-time)))
        (insert (format "Packages: %d total (%d current, %d work item%s: %d done, %d pending)\n"
                        total-count utd-count work-total (if (= work-total 1) "" "s")
                        work-done (max 0 (- work-total work-done))))
        (insert (propertize (format "Controls: delay %ss | confirm %s | cleanup %s  ([p/SPC] pause, [c] continue, [+/-/d] delay, [i] confirm, [l] cleanup, [g] restart, [x] cancel, [?] menu)\n\n"
                                    cur-delay
                                    (if cur-confirm "on" "off")
                                    (if cur-cleanup "on" "off"))
                            'face 'shadow))
        (unless (eq action 'repo-install)
          (let ((arch-start (point)))
            (insert (elpaish-install--archives-summary
                     (plist-get ctx :initial-archives)
                     refresh-state
                     (plist-get ctx :active-archive)))
            (when-let* ((active-arch (plist-get ctx :active-archive)))
              (save-excursion
                (goto-char arch-start)
                (when (search-forward (format "Downloading archive: %s" active-arch) nil t)
                  (let ((line-beg (line-beginning-position))
                        (line-end (line-end-position)))
                    (setq active-pos line-beg)
                    (add-face-text-property line-beg line-end 'highlight))))))
          (insert "\n"))
        (seq-do
         (lambda (entry)
           (let* ((pkg (car entry))
                  (marker (nth 1 entry))
                  (ver (nth 2 entry))
                  (detail (nth 3 entry))
                  (meta (nth 4 entry))
                  (up-to-date-p (plist-get meta :up-to-date))
                  (prev-ver (plist-get meta :prev-version))
                  (to-ver (or (plist-get meta :to) (plist-get meta :target-version)))
                  (pkg-dur (plist-get meta :duration))
                  (src (elpaish-install--package-source pkg))
                  (line-beg (point)))
             (insert (format "%s %s: " (elpaish-install--icon-string marker) pkg))
             (elpaish-install--insert-version-link pkg ver)
             (when src
               (insert (format " [%s]" src)))
             (if up-to-date-p
                 (insert " — current")
               (when detail
                 (insert (format " — %s" detail))))
             (let ((has-paren nil))
               (when pkg-dur
                 (insert (format " (time: %s" (elpaish-install--format-duration pkg-dur)))
                 (setq has-paren t))
               (unless up-to-date-p
                 (when prev-ver
                   (if has-paren (insert ", ") (insert " ("))
                   (insert "prev: ")
                   (elpaish-install--insert-version-link pkg prev-ver)
                   (setq has-paren t))
                 (when (and to-ver (not (equal to-ver prev-ver)))
                   (if has-paren (insert ", ") (insert " ("))
                   (insert "to: ")
                   (elpaish-install--insert-version-link pkg to-ver)
                   (setq has-paren t)))
               (when has-paren
                 (insert ")")))
             (let ((line-end (point)))
               (insert "\n")
               (when up-to-date-p
                 (add-face-text-property line-beg line-end 'elpaish-install-up-to-date-face)
                 (put-text-property line-beg line-end 'elpaish-up-to-date t))
               (when (or (string= marker "[...]") (eq pkg active-pkg))
                 (setq active-pos line-beg)
                 (put-text-property line-beg (point) 'face 'highlight)))))
         entries)
        (let ((q-beg (point)))
          (insert (format "\n%s Rebuild package quickstart\n"
                          (elpaish-install--icon-string quickstart-state)))
          (when (string= quickstart-state "[...]")
            (setq active-pos q-beg)
            (put-text-property q-beg (point) 'face 'highlight)))
        (let* ((cl-marker (or (and (stringp cleanup-state) (car (split-string cleanup-state)))
                              (if cur-cleanup "[x]" "[-]")))
               (cl-label (if (and (stringp cleanup-state) (string-match-p "legacy package" cleanup-state))
                             (format " (%s)" (string-trim (substring cleanup-state 3)))
                           (if cur-cleanup " (enabled)" " (disabled)"))))
          (insert (format "%s Clean up legacy packages%s\n"
                          (elpaish-install--icon-string cl-marker)
                          cl-label)))
        ;; Auto-scroll window to follow current activity
        (when win
          (if (and active-pos (not (equal active-key elpaish-install--progress-last-active)))
              (progn
                (set-window-point win active-pos)
                (unless (pos-visible-in-window-p active-pos win)
                  (set-window-start win active-pos)))
            (when old-point (set-window-point win (min old-point (point-max))))))
        (setq elpaish-install--progress-last-active active-key)))
    (redisplay)))

(cl-defun elpaish-install--upgrade-run (pkgs refresh sync delay callback &optional action &key stream confirm paused cleanup)
  "Upgrade or install PKGS, updating a progress buffer throughout the operation.
REFRESH controls archive refresh, SYNC runs immediately, DELAY sets the
pause between asynchronous steps, and CALLBACK receives the results.
ACTION is \\='upgrade (default), \\='install, or \\='repo-install.
STREAM specifies the release stream for \\='repo-install.  CONFIRM enables
intermediate confirmation prompts (default
`elpaish-install-confirm-intermediate').
PAUSED starts the operation in paused state when non-nil.
CLEANUP enables removing older duplicate package versions after changes."
  (let* ((buffer (get-buffer-create "*ELPAish Upgrade Progress*"))
         (act (or action 'upgrade))
         (cur-delay (or delay elpaish-install-yield-delay))
         (cur-confirm (if confirm t elpaish-install-confirm-intermediate))
         (cur-paused paused)
         (cur-cleanup (if (not (null cleanup)) cleanup elpaish-install-cleanup-legacy))
         (cleanup-state "[ ]")
         (entries (seq-map (lambda (pkg)
                             (let* ((ver (elpaish-install--installed-version pkg))
                                    (ts (elpaish-install--package-previous-timestamps pkg ver)))
                               (list pkg "[ ]" ver nil
                                     (append (list :work-item t :up-to-date nil) ts))))
                           pkgs))
         (remaining (copy-sequence pkgs))
         (refresh-state (if refresh "[ ]" "[-]"))
         (quickstart-state "[ ]")
         (global-state (if cur-paused "Paused" "Working"))
         (stage 'refresh)
         (results nil)
         (timer nil)
         (heartbeat nil)
         (step nil)
         (current-future nil)
         (start-time (float-time))
         (end-time nil)
         (actual-time 0.0)
         (work-start nil)
         (confirm-wait-time 0.0)
         (captured-archives (copy-alist package-archives))
         (captured-priorities (and (boundp 'package-archive-priorities)
                                   (copy-alist package-archive-priorities)))
         (initial-archives
          (when (and (boundp 'package-archives) package-archives)
            (seq-map
             (lambda (archive)
               (let* ((name (car archive))
                      (archive-dir (and (boundp 'package-user-dir)
                                        (expand-file-name (format "archives/%s" name) package-user-dir)))
                      (content-file (and archive-dir (expand-file-name "archive-contents" archive-dir)))
                      (mtime (when (and content-file (file-exists-p content-file))
                               (format-time-string "%Y-%m-%d %H:%M"
                                                   (file-attribute-modification-time
                                                    (file-attributes content-file))))))
                 (cons name (or mtime "not updated"))))
             package-archives)))
         (compute-stats
          (lambda ()
            (let* ((now (or end-time (float-time)))
                   (wall (max 0.0 (- now start-time)))
                   (live-actual (+ actual-time (if work-start (- now work-start) 0.0)))
                   (wait (max confirm-wait-time (- wall live-actual))))
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (setf (plist-get elpaish-install--progress-context :wall-time) wall)
                  (setf (plist-get elpaish-install--progress-context :actual-time) live-actual)
                  (setf (plist-get elpaish-install--progress-context :wait-time) wait)
                  (setf (plist-get elpaish-install--progress-context :delay) cur-delay)
                  (setf (plist-get elpaish-install--progress-context :confirm) cur-confirm)
                  (setf (plist-get elpaish-install--progress-context :paused) cur-paused)
                  (setf (plist-get elpaish-install--progress-context :cleanup) cur-cleanup)))
              (list :wall-time wall
                    :actual-time live-actual
                    :wait-time wait
                    :delay cur-delay
                    :confirm cur-confirm
                    :paused cur-paused
                    :cleanup cur-cleanup))))
         (render (lambda (&optional cur-pkg)
                   (elpaish-install--upgrade-progress
                    buffer entries refresh-state quickstart-state
                    global-state cur-pkg act (funcall compute-stats) cleanup-state))))
    (with-current-buffer buffer
      (elpaish-install-progress-mode)
      (setq-local elpaish-install--progress-context
                  (list :pkgs pkgs :refresh refresh :sync sync :delay cur-delay
                        :callback callback :action act :stream stream
                        :confirm cur-confirm :paused cur-paused :cleanup cur-cleanup
                        :initial-archives initial-archives
                        :start-time start-time :wall-time 0.0
                        :actual-time 0.0 :wait-time 0.0 :running t))
      (add-hook 'kill-buffer-query-functions #'elpaish-install--progress-query-kill nil t))
    (funcall render)
    (display-buffer buffer)
    (setq step
          (lambda ()
            (let ((package-archives captured-archives)
                  (package-archive-priorities captured-priorities)
                  (is-async-item nil))
              (condition-case err
                  (pcase stage
                    ('refresh
                     (setq global-state "Working")
                     (setq refresh-state (if refresh "[...]" "[-]"))
                     (funcall render)
                     (let ((t0 (float-time)))
                       (setq work-start t0)
                       (unless (or (eq act 'repo-install)
                                   (bound-and-true-p package-archive-contents))
                         (package-initialize))
                       (when refresh
                         (let ((advice
                                (lambda (original archive file &rest args)
                                  (when (equal file "archive-contents")
                                    (with-current-buffer buffer
                                      (setf (plist-get elpaish-install--progress-context
                                                       :active-archive) (car archive)))
                                    (funcall render))
                                  (unwind-protect
                                      (apply original archive file args)
                                    (with-current-buffer buffer
                                      (setf (plist-get elpaish-install--progress-context
                                                       :active-archive) nil))
                                    (funcall render)))))
                           (advice-add 'package--download-one-archive :around advice)
                           (unwind-protect (let ((inhibit-quit t) (quit-flag nil))
                                             (package-refresh-contents))
                             (advice-remove 'package--download-one-archive advice))))
                       ;; First iterate over packages, strike through up-to-date ones,
                       ;; and move everything needing upgrade to the end of the list.
                       (seq-do
                        (lambda (entry)
                          (let* ((pkg (car entry))
                                 (ver (elpaish-install--installed-version pkg))
                                 (ts (elpaish-install--package-previous-timestamps pkg ver))
                                 (arch-ver (elpaish-install--archive-version pkg))
                                 (utd (elpaish-install--package-up-to-date-p pkg act))
                                 (meta (append (list :up-to-date (and utd t)
                                                     :work-item (not utd)
                                                     :target-version arch-ver)
                                               ts)))
                            (setf (nth 2 entry) ver)
                            (setf (nth 4 entry) meta)
                            (when utd
                              (let ((st (if (eq act 'install) :already-installed :up-to-date)))
                                (setf (nth 1 entry) "[x]")
                                (setf (nth 3 entry) "current")
                                (push (append (list :status st :pkg pkg) ts) results)))))
                        entries)
                       (setq entries (elpaish-install--reorder-entries entries))
                       (setq remaining
                             (thread-last entries
                               (seq-filter (lambda (e) (plist-get (nth 4 e) :work-item)))
                               (seq-map #'car)))
                       (setq actual-time (+ actual-time (- (float-time) t0)))
                       (setq work-start nil))
                     (setq refresh-state (if refresh "[x]" "[-]"))
                     (setq stage 'packages)
                     (setq global-state (if cur-delay "Yielding" "Working")))
                    ('packages
                     (if remaining
                         (let* ((next-pkg (car remaining))
                                (proceed t))
                           (when cur-confirm
                             (setq global-state "Waiting for confirmation")
                             (funcall render next-pkg)
                             (let ((wait-t0 (float-time)))
                               (setq proceed
                                     (y-or-n-p
                                      (format "Proceed with %s of %s (%d remaining)? "
                                              (pcase act
                                                ('install "installation")
                                                ('repo-install "repository installation")
                                                (_ "upgrade"))
                                              next-pkg
                                              (length remaining))))
                               (setq confirm-wait-time
                                     (+ confirm-wait-time (- (float-time) wait-t0)))))
                           (if (not proceed)
                               (progn
                                 (setq cur-paused t)
                                 (setq global-state "Paused")
                                 (logx 'info :msg (format "[elpaish] Paused before %s." next-pkg)
                                       :pkg next-pkg))
                             (let* ((pkg (pop remaining))
                                    (entry (assq pkg entries))
                                    (pkg-sym (if (symbolp pkg) pkg (intern (format "%s" pkg))))
                                    (pkg-name (symbol-name pkg-sym))
                                    (t0 (float-time))
                                    (_start (setq work-start t0))
                                    (finish-item
                                     (lambda (raw-res &optional schedule-next)
                                       (let* ((pkg-dur (- (float-time) t0))
                                              (meta (nth 4 entry))
                                              (status (plist-get raw-res :status))
                                              (utd-status (memq status '(:up-to-date :already-installed)))
                                              (version (or (plist-get raw-res :to)
                                                           (elpaish-install--installed-version pkg)))
                                              (res (append raw-res
                                                           (list :duration pkg-dur
                                                                 :prev-version (plist-get meta :prev-version)
                                                                 :prev-generated (plist-get meta :prev-generated)
                                                                 :prev-local-update (plist-get meta :prev-local-update)))))
                                         (setq actual-time (+ actual-time pkg-dur)
                                               work-start nil)
                                         (push res results)
                                         (setf (plist-get meta :duration) pkg-dur)
                                         (when utd-status
                                           (setf (plist-get meta :up-to-date) t)
                                           (setf (plist-get meta :work-item) nil))
                                         (setf (nth 1 entry) (if (eq status :failed) "[!]" "[x]"))
                                         (setf (nth 2 entry) version)
                                         (setf (nth 3 entry) (if (eq status :failed)
                                                                 (plist-get res :error)
                                                               (if utd-status "current" (substring (symbol-name status) 1))))
                                         (setf (nth 4 entry) meta)
                                         (when utd-status
                                           (setq entries (elpaish-install--reorder-entries entries)))
                                         (setq global-state (if cur-paused
                                                                "Paused"
                                                              (if (and remaining cur-delay) "Yielding" "Working")))
                                         (funcall render pkg)
                                         (when (and schedule-next (not (eq stage 'done)) (not cur-paused))
                                           (setq timer (run-at-time (or cur-delay elpaish-install-yield-delay) nil step)))))))
                               (if (and (not sync)
                                        (eq act 'repo-install)
                                        (fboundp 'elpaish-async-build-package-in-sprite))
                                   (progn
                                     (setq global-state "Compiling in sprite")
                                     (setf (nth 1 entry) "[...]")
                                     (funcall render pkg)
                                     (unless (and (boundp 'elpaish-registry)
                                                  (hash-table-p elpaish-registry)
                                                  (> (hash-table-count elpaish-registry) 0))
                                       (elpaish-load-packages))
                                     (let ((recipe (elpaish-install--resolve-repository-recipe pkg)))
                                       (if (null recipe)
                                           (progn
                                             (logx 'warn :msg (format "[elpaish] Skipping %s: no recipe found in repository" pkg-name)
                                                   :pkg pkg-name)
                                             (funcall finish-item (list :status :failed :pkg pkg :error "No recipe found in repository")))
                                         (let ((fut (condition-case dispatch-err
                                                        (elpaish-async-build-package-in-sprite (or recipe pkg) :stream stream)
                                                      (error
                                                       (list :status :failed :pkg pkg :error (error-message-string dispatch-err))))))
                                           (if (and (fboundp 'sprite-future-p) (funcall 'sprite-future-p fut))
                                               (progn
                                                 (setq is-async-item t)
                                                 (setq current-future fut)
                                                 (funcall 'sprite-future-then
                                                  fut
                                                  (lambda (build-res)
                                                    (setq current-future nil)
                                                    (unless (eq stage 'done)
                                                      (let ((raw-res
                                                             (condition-case install-err
                                                                 (if (eq (plist-get build-res :status) :ok)
                                                                     (progn
                                                                       (setq global-state "Installing in host")
                                                                       (funcall render pkg)
                                                                       (let* ((artifact (plist-get build-res :artifact))
                                                                              (prev-desc (and (boundp 'package-alist)
                                                                                              (cadr (assq pkg-sym package-alist))))
                                                                              (prev-ver (and prev-desc
                                                                                             (package-version-join (package-desc-version prev-desc)))))
                                                                         (unwind-protect
                                                                             (let* ((built-desc (package-install-file artifact))
                                                                                    (desc (or (seq-find
                                                                                               (lambda (candidate)
                                                                                                 (equal (package-desc-version candidate)
                                                                                                        (package-desc-version built-desc)))
                                                                                               (cdr (assq (package-desc-name built-desc) package-alist)))
                                                                                              built-desc))
                                                                                    (new-ver (and desc (package-version-join (package-desc-version desc))))
                                                                                    (status (if prev-ver :upgraded :installed))
                                                                                    (res (list :status status
                                                                                               :pkg pkg
                                                                                               :from prev-ver
                                                                                               :to new-ver
                                                                                               :desc desc)))
                                                                               (when (and desc (fboundp 'package--reload-previously-loaded))
                                                                                 (condition-case nil
                                                                                     (package--reload-previously-loaded desc)
                                                                                   (error nil)))
                                                                               (logx 'info :msg (format "[elpaish] Installed %s (%s)"
                                                                                                        pkg-name (or new-ver ""))
                                                                                     :pkg pkg-name :version new-ver)
                                                                               (run-hook-with-args 'elpaish-install-package-updated-functions
                                                                                                   pkg-sym
                                                                                                   (if prev-ver 'upgraded 'installed)
                                                                                                   res)
                                                                               res)
                                                                           (when (and artifact (file-exists-p artifact))
                                                                             (let ((parent-dir (file-name-directory artifact)))
                                                                               (if (and parent-dir (file-directory-p parent-dir)
                                                                                        (string-match-p "elpaish-sprite-build-" parent-dir))
                                                                                   (delete-directory parent-dir t)
                                                                                 (delete-file artifact)))))))
                                                                   (let ((err-msg (or (plist-get build-res :error) "Build failed in sprite worker")))
                                                                     (logx 'error :msg (format "[elpaish] Failed to build %s: %s" pkg err-msg)
                                                                           :pkg pkg :error err-msg)
                                                                     (list :status :failed :pkg pkg :error err-msg :stage (plist-get build-res :stage))))
                                                               (error
                                                                (logx 'error :msg (format "[elpaish] Failed to install %s: %s"
                                                                                          pkg (error-message-string install-err))
                                                                      :pkg pkg :error (error-message-string install-err))
                                                                (list :status :failed :pkg pkg :error (error-message-string install-err))))))
                                                        (funcall finish-item raw-res t))))
                                                  (lambda (reject-val)
                                                    (setq current-future nil)
                                                    (unless (eq stage 'done)
                                                      (let* ((err-msg (if (consp reject-val)
                                                                          (format "%s" (cdr reject-val))
                                                                        (error-message-string reject-val)))
                                                             (raw-res (list :status :failed :pkg pkg :error err-msg :stage :sprite-dispatch)))
                                                        (logx 'error :msg (format "[elpaish] Sprite build rejected for %s: %s" pkg err-msg)
                                                              :pkg pkg :error err-msg)
                                                        (funcall finish-item raw-res t))))))
                                             (funcall finish-item fut))))))
                                 (setq global-state "Working")
                                 (setf (nth 1 entry) "[...]")
                                 (funcall render pkg)
                                 (let ((raw-res (let ((inhibit-quit t) (quit-flag nil))
                                                  (if (eq act 'repo-install)
                                                      (elpaish-install--process-repository-package pkg stream)
                                                    (elpaish-install--process-package pkg act nil)))))
                                   (funcall finish-item raw-res))))))
                       (setq stage 'quickstart)
                       (setq global-state "Working")))
                    ('quickstart
                     (setq quickstart-state "[...]")
                     (setq global-state "Working")
                     (funcall render)
                     (let* ((t0 (float-time))
                            (final-results (nreverse results))
                            (changed (seq-some (lambda (res)
                                                 (memq (plist-get res :status) '(:installed :upgraded)))
                                               final-results))
                            (quickstart-enabled
                             (memq #'elpaish-install--on-bootstrap-finished
                                   elpaish-install-bootstrap-finished-functions))
                            (elpaish-install-bootstrap-finished-functions
                             (remove #'elpaish-install--on-bootstrap-finished
                                     elpaish-install-bootstrap-finished-functions)))
                       (when (and changed quickstart-enabled)
                         (elpaish-install-flush-package-quickstart t))
                       (if cur-cleanup
                           (if changed
                               (condition-case clean-err
                                   (let ((deleted (elpaish-install-clean-stale-packages t)))
                                     (setq cleanup-state (format "[x] %d legacy package%s cleaned"
                                                                 deleted (if (= deleted 1) "" "s"))))
                                 (error
                                  (setq cleanup-state (format "[!] %s" (error-message-string clean-err)))))
                             (setq cleanup-state "[x] 0 legacy packages cleaned"))
                         (setq cleanup-state "[-]"))
                       (setq actual-time (+ actual-time (- (float-time) t0)))
                       (setq end-time (float-time))
                       (when (timerp heartbeat) (cancel-timer heartbeat))
                       (setq quickstart-state (if (and changed quickstart-enabled) "[x]" "[-]"))
                       (setq results final-results)
                       (setq stage 'done)
                       (setq global-state "Completed")
                       (setq elpaish-install--active-cancel-handle nil)
                       (let ((final-stats (funcall compute-stats)))
                         (with-current-buffer buffer
                           (setf (plist-get elpaish-install--progress-context :running) nil)
                           (setf (plist-get elpaish-install--progress-context :end-time) end-time))
                         (elpaish-install--report-summary results act final-stats)
                         (run-hook-with-args 'elpaish-install-bootstrap-finished-functions results act)
                         (when callback (funcall callback results))))))
                (quit
                 (setq cur-paused t global-state "Paused" work-start nil)
                 (message "Operation paused; press c to continue."))
                (error
                 (when (timerp heartbeat) (cancel-timer heartbeat))
                 (setq end-time (float-time))
                 (pcase stage
                   ('refresh (setq refresh-state (format "[!] %s" (error-message-string err))))
                   ('quickstart (setq quickstart-state (format "[!] %s" (error-message-string err)))))
                 (unless (eq stage 'done)
                   (setq elpaish-install--active-cancel-handle nil)
                   (with-current-buffer buffer
                     (setf (plist-get elpaish-install--progress-context :running) nil)))
                 (setq stage 'done)
                 (setq global-state "Failed")
                 (logx 'error :msg (format "[elpaish] %s failed: %s" act (error-message-string err))
                       :action act :error (error-message-string err))
                 (funcall render)
                 (when sync (signal (car err) (cdr err)))))
              (funcall render)
              (if (or (eq stage 'done) cur-paused is-async-item)
                  nil
                (unless sync
                  (setq timer (run-at-time (or cur-delay elpaish-install-yield-delay) nil step)))))))
    (with-current-buffer buffer
      (setq-local elpaish-install--progress-step-fn step)
      (setq-local elpaish-install--progress-control-fn
                  (lambda (cmd &optional arg)
                    (pcase cmd
                      (:pause
                       (when (timerp timer)
                         (cancel-timer timer)
                         (setq timer nil))
                       (setq cur-paused t)
                       (setq global-state "Paused")
                       (funcall render))
                      (:resume
                       (when (timerp timer)
                         (cancel-timer timer)
                         (setq timer nil))
                       (setq cur-paused nil)
                       (setq global-state "Working")
                       (funcall step))
                      (:set-delay
                       (setq cur-delay (max 0.0 (float arg)))
                       (when (and (timerp timer) (not cur-paused) (not (eq stage 'done)))
                         (cancel-timer timer)
                         (setq timer (run-at-time cur-delay nil step)))
                       (funcall render))
                      (:set-confirm
                       (setq cur-confirm (and arg t))
                       (funcall render))
                      (:set-cleanup
                       (setq cur-cleanup (and arg t))
                       (funcall render))
                      (:render
                       (funcall render))))))
    (if sync
        (progn
          (while (and (not (eq stage 'done)) (not cur-paused))
            (funcall step))
          results)
      (unless cur-paused
        (setq timer (run-at-time 0 nil step)))
      (unless (or sync noninteractive)
        (setq heartbeat
              (run-at-time 1 1 (lambda ()
                                 (if (and (buffer-live-p buffer)
                                          (buffer-local-value 'elpaish-install--progress-context buffer)
                                          (plist-get (buffer-local-value 'elpaish-install--progress-context buffer) :running))
                                     (funcall render)
                                   (when (timerp heartbeat) (cancel-timer heartbeat)))))))
      (let ((cancel (lambda (&optional _)
                      (when (timerp timer) (cancel-timer timer))
                      (when (timerp heartbeat) (cancel-timer heartbeat))
                      (when (and current-future (fboundp 'sprite-future-cancel))
                        (funcall 'sprite-future-cancel current-future :user-cancel))
                      (setq current-future nil)
                      (setq end-time (float-time))
                      (setq remaining nil)
                      (setq stage 'done)
                      (setq cur-paused nil)
                      (setq global-state "Canceled")
                      (setq elpaish-install--active-cancel-handle nil)
                      (with-current-buffer buffer
                        (setf (plist-get elpaish-install--progress-context :running) nil)
                        (setf (plist-get elpaish-install--progress-context :end-time) end-time))
                      (funcall render)
                      (logx 'info :msg "[elpaish] Package operation canceled."))))
        (setq elpaish-install--active-cancel-handle cancel)
        cancel))))

;;;###autoload
(cl-defun elpaish-install-upgrade-packages (&rest args)
  "Upgrade on-demand packages to their latest versions.
Upgrades packages configured in `elpaish-install-upgrade-packages-list',
which must be a strict subset of `elpaish-install-packages-list'.
The upgrade operation is lazy and runs in the background asynchronously
with cooperative yielding.  When called interactively with a prefix argument
\(e.g. \\[universal-argument]), execution is synchronous.

ARGS can contain package symbols, lists of package symbols, or keyword options:
  :sync BOOL        - When non-nil, execute synchronously.
  :async BOOL       - When non-nil (default t), execute asynchronously.
  :refresh BOOL     - When non-nil (default t), refresh package archives.
  :concurrency N    - Accepted for compatibility; upgrades run sequentially.
  :delay SECS       - Yield delay in seconds
                      (default `elpaish-install-yield-delay').
  :confirm BOOL     - When non-nil, ask for intermediate confirmation before
                      each package work item (default
                      `elpaish-install-confirm-intermediate').
  :cleanup BOOL     - When non-nil, clean up legacy package directories on
                      completion (default `elpaish-install-cleanup-legacy').
  :paused BOOL      - When non-nil, start in paused state.
  :callback FN      - Function called with result list on completion.
  :pool-size N      - Accepted for compatibility; upgrades run in this Emacs.

When no packages are specified in ARGS, defaults to
`elpaish-install-upgrade-packages-list'."
  (interactive "P")
  (let* ((interactive-p (called-interactively-p 'any))
         (prefix-arg-val (when interactive-p (car args)))
         (remaining (if interactive-p nil args))
         (pkgs nil)
         (group nil)
         (refresh t)
         (sync (not (null prefix-arg-val)))
         (async :default)
         (concurrency nil)
         (delay nil)
         (confirm elpaish-install-confirm-intermediate)
         (cleanup elpaish-install-cleanup-legacy)
         (paused nil)
         (pool-size nil)
         (callback nil))
    (while remaining
      (let ((arg (pop remaining)))
        (cond
         ((eq arg :group) (setq group (pop remaining)))
         ((eq arg :sync) (setq sync (pop remaining)))
         ((eq arg :async) (setq async (pop remaining)))
         ((eq arg :refresh) (setq refresh (pop remaining)))
         ((memq arg '(:action :status))
          (pop remaining))
         ((eq arg :concurrency) (setq concurrency (pop remaining)))
         ((eq arg :delay) (setq delay (pop remaining)))
         ((or (eq arg :confirm) (eq arg :confirm-intermediate))
          (setq confirm (pop remaining)))
         ((eq arg :cleanup)
          (setq cleanup (pop remaining)))
         ((eq arg :paused) (setq paused (pop remaining)))
         ((eq arg :pool-size) (setq pool-size (pop remaining)))
         ((or (eq arg :callback) (eq arg :on-done))
          (setq callback (pop remaining)))
         ((listp arg)
          (seq-do (lambda (p) (when (symbolp p) (push p pkgs)))
                  (flatten-tree arg)))
         ((symbolp arg)
          (push arg pkgs)))))
    (setq pkgs (delete-dups (nreverse pkgs)))
    (when group
      (let ((group-pkgs (elpaish-install-get-group-packages group)))
        (unless group-pkgs
          (user-error "Group `%s' not found or has no registered packages" group))
        (setq pkgs (delete-dups (append pkgs group-pkgs)))))
    (when elpaish-install--active-cancel-handle
      (user-error "An ELPAish package operation is already running"))
    (when pkgs
      (apply #'elpaish-install-register-upgrade-package pkgs))
    (let* ((raw-targets (or pkgs (elpaish-install--get-upgrade-packages)))
           (target-pkgs (seq-filter (lambda (p) (memq p elpaish-install-packages-list))
                                    raw-targets))
           (is-sync (or sync (eq async nil))))
      (cond
       ((null target-pkgs)
        (logx 'warn :msg "[elpaish] No packages configured in `elpaish-install-upgrade-packages-list' to upgrade.")
        (when callback (funcall callback nil))
        nil)
       (t
        (ignore concurrency pool-size)
        (elpaish-install--upgrade-run target-pkgs refresh is-sync delay callback
                                      'upgrade :confirm confirm :paused paused :cleanup cleanup))))))

;;;###autoload
(defun elpaish-install-upgrade-group (group &optional sync)
  "Upgrade all packages belonging to GROUP.
Interactively, prompt for GROUP among `elpaish-install-package-groups'
with completion.  With prefix argument SYNC, run synchronously."
  (interactive
   (let* ((groups (or elpaish-install-package-groups
                      (user-error "No package groups registered")))
          (table (mapcar (lambda (entry)
                           (let* ((g (car entry))
                                  (pol (elpaish-install--group-policy entry))
                                  (pkgs (elpaish-install--group-packages-list entry)))
                             (cons (symbol-name g)
                                   (format "%d packages [%s]" (length pkgs) pol))))
                         groups))
          (choice (if (fboundp 'annotated-completing-read)
                      (annotated-completing-read table :prompt "Upgrade package group: " :require-match t)
                    (completing-read "Upgrade package group: " table nil t))))
     (list (intern choice) current-prefix-arg)))
  (let ((pkgs (elpaish-install-get-group-packages group)))
    (unless pkgs
      (user-error "Group `%s' has no registered packages" group))
    (elpaish-install-upgrade-packages pkgs :sync (not (null sync)))))

;;;###autoload
(cl-defun elpaish-install-bootstrap-packages (&rest args)
  "Install or upgrade bootstrap packages (legacy dispatcher).
When :action is \\='install, delegates to `elpaish-install-packages'.
Otherwise, installs missing packages and upgrades existing ones.

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
`elpaish-install-packages-list'."
  (interactive "P")
  (let* ((interactive-p (called-interactively-p 'any))
         (prefix-arg-val (when interactive-p (car args)))
         (remaining (if interactive-p nil args))
         (action 'upgrade))
    (while remaining
      (let ((arg (pop remaining)))
        (when (eq arg :action)
          (setq action (pop remaining)))))
    (cond
     ((eq action 'install)
      (apply #'elpaish-install-packages args))
     (t
      (let* ((remaining (if interactive-p nil args))
             (pkgs nil)
             (refresh t)
             (sync (not (null prefix-arg-val)))
             (async :default)
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
             ((eq arg :action)     (pop remaining))
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
                                elpaish-install-packages-list))
               (is-sync (or sync (eq async nil))))
          (cond
           (is-sync
            (let ((res (elpaish-install--do-packages target-pkgs :action 'upgrade :refresh refresh)))
              (when callback (funcall callback res))
              res))
           (t
            (elpaish-install--do-packages-async
             target-pkgs
             :action 'upgrade
             :refresh refresh
             :concurrency concurrency
             :delay delay
             :pool-size pool-size
             :callback callback)))))))))

;;;###autoload
(cl-defun elpaish-install-packages-async (pkgs &key refresh pool-size callback)
  "Install PKGS asynchronously out-of-process using a sprite pool if available.
If REFRESH is non-nil, refresh archive contents.  POOL-SIZE is the sprite pool
concurrency.  Calls CALLBACK when complete."
  (elpaish-install-packages pkgs :action 'install :refresh refresh :pool-size (or pool-size 2) :callback callback :async t))

;;;; Package Maintenance Tools

;;;###autoload
(defun elpaish-install-delete-package-artifacts (pkg)
  "Delete installed directory and artifacts for PKG in `package-user-dir'.
PKG can be a string or symbol. Returns list of deleted file paths."
  (let* ((pkg-sym (if (stringp pkg) (intern pkg) pkg))
         (pkg-name (symbol-name pkg-sym))
         (prefix (regexp-quote (concat pkg-name "-")))
         (deleted nil))
    (when (and (boundp 'package-user-dir)
               (file-directory-p package-user-dir))
      (seq-do (lambda (entry)
                (let ((full-path (expand-file-name entry package-user-dir)))
                  (when (and (string-prefix-p (concat pkg-name "-") entry)
                             (string-match-p (format "\\`%s[0-9]" prefix) entry))
                    (if (file-directory-p full-path)
                        (delete-directory full-path t)
                      (delete-file full-path))
                    (push full-path deleted))))
              (directory-files package-user-dir nil "\\`[^.]" t)))
    (when (boundp 'package-alist)
      (setq package-alist (assq-delete-all pkg-sym package-alist)))
    (nreverse deleted)))

;;;###autoload
(defun elpaish-install-rebuild-local-package (pkg &optional stream)
  "Rebuild PKG from its local Git checkout and reinstall into `package-user-dir'.
Refuses if the local repository has uncommitted changes, has unpushed commits,
or has no remote tracking branch. Deletes prior versions of PKG and its signed
artifacts from `package-user-dir' before installing."
  (interactive
   (list (let* ((pkgs (progn
                        (require 'elpaish)
                        (require 'elpaish-recipes)
                        (unless (> (hash-table-count elpaish-registry) 0)
                          (elpaish-load-packages))
                        (delete-dups
                         (append (map-keys elpaish-registry)
                                 (mapcar #'symbol-name elpaish-install-packages-list)))))
                (prompt "Rebuild local package: "))
           (intern (if (fboundp 'annotated-completing-read)
                       (annotated-completing-read pkgs :prompt prompt)
                     (completing-read prompt pkgs nil t))))))
  (require 'elpaish)
  (require 'elpaish-recipes)
  (require 'package)
  (unless (> (hash-table-count elpaish-registry) 0)
    (elpaish-load-packages))
  (let* ((pkg-sym (if (stringp pkg) (intern pkg) pkg))
         (pkg-name (symbol-name pkg-sym))
         (recipe (or (map-elt elpaish-registry pkg-name)
                     (map-elt elpaish-registry pkg-sym)
                     (user-error "No ELPAish recipe found for %s" pkg-name)))
         (repo-dir (elpaish--resolve-repo-path recipe))
         (effective-stream (or stream (and (boundp 'elpaish-release-mode) elpaish-release-mode) 'snapshot)))
    (unless (and repo-dir (file-directory-p repo-dir))
      (user-error "Local checkout directory not found for %s: %s" pkg-name repo-dir))
    (let ((git-dir (expand-file-name ".git" repo-dir)))
      (unless (or (file-directory-p git-dir) (file-regular-p git-dir))
        (user-error "Local directory is not a Git repository: %s" repo-dir)))
    ;; Check Git state:
    ;; 1. Dirty check
    (let ((dirty (elpaish--git-string repo-dir "status" "--porcelain")))
      (when (and dirty (not (string-empty-p (string-trim dirty))))
        (user-error "Cannot rebuild %s: repository at %s has uncommitted changes" pkg-name repo-dir)))
    ;; 2. Remote tracking check
    (let ((upstream (elpaish--git-string repo-dir "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}")))
      (when (or (null upstream) (string-empty-p (string-trim upstream)))
        (user-error "Cannot rebuild %s: current branch in %s has no remote tracking branch" pkg-name repo-dir)))
    ;; 3. Unpushed commits check
    (let ((unpushed (elpaish--git-lines repo-dir "rev-list" "@{u}..HEAD")))
      (when (and unpushed (not (seq-empty-p unpushed)))
        (user-error "Cannot rebuild %s: local repository in %s has unpushed commits" pkg-name repo-dir)))
    ;; Clean previous installations and signed artifacts from package-user-dir
    (elpaish-install-delete-package-artifacts pkg-name)
    ;; Build the package artifact
    (let ((dest-file (elpaish-build-package recipe effective-stream)))
      (unless (and dest-file (file-exists-p dest-file))
        (error "Failed to build package artifact for %s" pkg-name))
      ;; Install the built package
      (package-install-file dest-file)
      (elpaish-install-schedule-quickstart-refresh)
      (logx 'info :msg (format "[elpaish] Successfully rebuilt and reinstalled %s from %s" pkg-name repo-dir)
            :pkg pkg-name :dir repo-dir)
      dest-file)))

;;;###autoload
(defun elpaish-install-clean-stale-packages (&optional no-prompt)
  "Find and remove older, duplicate versions of packages in `package-user-dir'.
For every package with multiple installed versions, keeps the newest version
and prompts (yes/no/all/cancel) to delete each older version and its archives.
When NO-PROMPT is non-nil, automatically deletes all stale versions."
  (interactive)
  (require 'package)
  (unless (and (boundp 'package-user-dir)
               (file-directory-p package-user-dir))
    (user-error "Package directory %s does not exist" package-user-dir))
  (let* ((entries (directory-files package-user-dir nil "\\`[^.]" t))
         (groups (make-hash-table :test #'equal))
         (stale-entries nil)
         (deleted-count 0)
         (delete-all no-prompt))
    (seq-do
     (lambda (entry)
       (let ((full-path (expand-file-name entry package-user-dir)))
         (when (file-directory-p full-path)
           (unless (member entry '("archives" "gnupg"))
             (let ((desc (condition-case nil
                             (package-load-descriptor full-path)
                           (error nil))))
               (if desc
                   (let* ((name (symbol-name (package-desc-name desc)))
                          (ver (package-desc-version desc))
                          (item (list :name name :version ver :dir entry :path full-path)))
                     (setf (map-elt groups name) (cons item (map-elt groups name))))
                 (when (string-match "\\`\\(.*?\\)-\\([0-9]+\\(?:\\.[0-9]+\\)*\\(?:\\.[0-9]+\\)?\\)\\'" entry)
                   (let* ((name (match-string 1 entry))
                          (ver-str (match-string 2 entry))
                          (ver (condition-case nil (version-to-list ver-str) (error '(0))))
                          (item (list :name name :version ver :dir entry :path full-path)))
                     (setf (map-elt groups name) (cons item (map-elt groups name)))))))))))
     entries)
    (map-do
     (lambda (_name items)
       (when (> (length items) 1)
         (let ((sorted (sort items (lambda (a b)
                                     (version-list-< (plist-get b :version)
                                                     (plist-get a :version))))))
           (seq-do (lambda (stale) (push stale stale-entries)) (cdr sorted)))))
     groups)
    (if (null stale-entries)
        (progn
          (logx 'info :msg (format "[elpaish] No stale package versions found in %s" package-user-dir)
                :dir package-user-dir)
          0)
      (catch 'cancel
        (seq-do
         (lambda (item)
           (let* ((dir-name (plist-get item :dir))
                  (full-dir (plist-get item :path))
                  (prefix (expand-file-name dir-name package-user-dir))
                  (associated (seq-filter #'file-exists-p
                                          (list (concat prefix ".tar")
                                                (concat prefix ".tar.sig")
                                                (concat prefix ".signed")
                                                (concat prefix ".el"))))
                  (should-delete nil))
             (if delete-all
                 (setq should-delete t)
               (let ((answer (read-answer
                              (format "Delete stale package %s? " dir-name)
                              '(("yes" ?y "delete this package")
                                ("no" ?n "skip this package")
                                ("all" ?! "delete all remaining stale packages")
                                ("cancel" ?q "abort remaining deletions")))))
                 (pcase answer
                   ("yes" (setq should-delete t))
                   ("no" (setq should-delete nil))
                   ("all" (setq delete-all t) (setq should-delete t))
                   ("cancel" (throw 'cancel nil)))))
             (when should-delete
               (delete-directory full-dir t)
               (seq-do #'delete-file associated)
               (cl-incf deleted-count)
               (logx 'info :msg (format "[elpaish] Deleted stale package %s" dir-name)
                     :dir dir-name))))
         (nreverse stale-entries)))
      (when (> deleted-count 0)
        (elpaish-install-schedule-quickstart-refresh))
      (logx 'info :msg (format "[elpaish] Finished cleaning: deleted %d stale package version(s)" deleted-count)
            :count deleted-count)
      deleted-count)))

;;;; Quickstart management and optimization

(defvar elpaish-install--quickstart-refresh-timer nil
  "Timer used to debounce calls to `package-quickstart-refresh'.")

(defcustom elpaish-install-quickstart-refresh-delay 1.0
  "Idle time in seconds to wait before regenerating `package-quickstart-file'."
  :type 'number
  :group 'elpaish-install)

;;;###autoload
(defun elpaish-install-sync-package-load-paths ()
  "Prune stale package directories from `load-path'.
Also ensures current ELPA directories and autoloads are loaded."
  (let ((elpa-dir (expand-file-name "elpa" user-emacs-directory)))
    (when (file-directory-p elpa-dir)
      (let ((newest-dirs (make-hash-table :test #'equal))
            (stale-dirs (make-hash-table :test #'equal)))
        (setf (map-elt stale-dirs (directory-file-name (expand-file-name "archives" elpa-dir))) t)
        (setf (map-elt stale-dirs (directory-file-name (expand-file-name "gnupg" elpa-dir))) t)
        (dolist (dir (directory-files elpa-dir t "\\`[^.]" t))
          (when (file-directory-p dir)
            (let ((base (file-name-nondirectory dir)))
              (unless (member base '("archives" "gnupg"))
                (if (string-match "\\`\\(.*?\\)-\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\'" base)
                    (let* ((name (match-string 1 base))
                           (ver (condition-case nil (version-to-list (match-string 2 base)) (error '(0))))
                           (prev (map-elt newest-dirs name)))
                      (if (or (null prev) (version-list-< (car prev) ver))
                          (progn
                            (when prev
                              (setf (map-elt stale-dirs (cdr prev)) t))
                            (setf (map-elt newest-dirs name) (cons ver dir)))
                        (setf (map-elt stale-dirs dir) t)))
                  (setf (map-elt newest-dirs base) (cons '(0) dir)))))))
        (setq load-path
              (seq-filter (lambda (p)
                            (and (file-directory-p p)
                                 (not (map-elt stale-dirs (directory-file-name (expand-file-name p))))))
                          load-path))
        (map-do
         (lambda (pkg-name entry)
           (let* ((dir (cdr entry))
                  (autoloads-file (expand-file-name (concat pkg-name "-autoloads") dir)))
             (add-to-list 'load-path dir)
             (when (or (file-exists-p (concat autoloads-file ".el"))
                       (file-exists-p (concat autoloads-file ".elc")))
               (load autoloads-file t t))))
         newest-dirs)))))

;;;###autoload
(defun elpaish-install-optimize-quickstart-file (file)
  "Transform FILE (package-quickstart.el) into a streamlined format.
Consolidates `load-path' additions into a static pre-expanded list,
strips docstrings, removes definition prefixes, strips unused let* scopes,
removes autoload feature provisions, and eliminates extraneous whitespace."
  (when (file-exists-p file)
    (with-temp-buffer
      (delay-mode-hooks (emacs-lisp-mode))
      (insert-file-contents file)
      (let ((dirs nil))
        ;; 1. Collect directories and delete individual (add-to-list 'load-path ...) forms
        (goto-char (point-min))
        (while (re-search-forward "(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))" nil t)
          (let ((match-beg (match-beginning 0))
                (match-end (match-end 0)))
            (save-excursion
              (when (re-search-backward "load-file-name \"\\([^\"]+\\)\"" nil t)
                (push (file-name-directory (match-string 1)) dirs)))
            (delete-region match-beg match-end)))

        ;; 2. Strip definition prefixes (register-definition-prefixes ...)
        (goto-char (point-min))
        (while (re-search-forward "(register-definition-prefixes" nil t)
          (backward-up-list 1)
          (let ((beg (point)))
            (forward-sexp 1)
            (when (looking-at "\n") (forward-char 1))
            (delete-region beg (point))))

        ;; 3. Strip docstrings from autoload definitions:
        ;; Match (autoload ... \"file\" \"docstring...\") and replace \"docstring...\" with nil
        (goto-char (point-min))
        (while (re-search-forward "(autoload[ \t\n\r]+['#][^ \t\n\r]+[ \t\n\r]+\"[^\"]+\"[ \t\n\r]+\"" nil t)
          (backward-char 1)
          (let ((beg (point)))
            (forward-sexp 1)
            (delete-region beg (point))
            (insert "nil")))

        ;; 4. Strip (provide '...-autoloads) forms
        (goto-char (point-min))
        (while (re-search-forward "(provide '[^)]+-autoloads)" nil t)
          (delete-region (match-beginning 0) (match-end 0)))

        ;; 5. Replace (let* ((load-file-name ...)(load-true-file-name load-file-name)) with (progn
        (goto-char (point-min))
        (while (re-search-forward "(let\\*\\s-*((\\s-*load-file-name\\s-+\"[^\"]+\"\\s-*)\\s-*(\\s-*load-true-file-name\\s-+load-file-name\\s-*))" nil t)
          (replace-match "(progn"))

        ;; 6. Clean up form-feeds and collapse blank lines
        (goto-char (point-min))
        (while (re-search-forward "[\x0c\r]" nil t)
          (replace-match ""))
        (goto-char (point-min))
        (while (re-search-forward "\n[ \t]*\n[ \t]*\n+" nil t)
          (replace-match "\n\n"))

        ;; 7. Insert pre-expanded, deduplicated static load-path list at the top
        (goto-char (point-min))
        (forward-line 3)
        (let ((clean-dirs (delete-dups (mapcar #'expand-file-name (mapcar #'directory-file-name (nreverse dirs))))))
          (insert "(setq load-path\n      (append '")
          (prin1 clean-dirs (current-buffer))
          (insert "\n              load-path))\n\n"))

        ;; Save the optimized file
        (write-region (point-min) (point-max) file nil 'silent)))))

(defun elpaish-install--package-quickstart-refresh-optimize (orig-fn &rest args)
  "Optimize quickstart file format and compile bytecode."
  (require 'bytecomp)
  (let ((real-byte-compile (symbol-function 'byte-compile-file)))
    (cl-letf (((symbol-function 'byte-compile-file)
               (lambda (file)
                 (elpaish-install-optimize-quickstart-file file)
                 (with-temp-buffer
                   (let ((byte-compile-log-buffer (buffer-name))
                         (display-buffer-alist
                          (cons (cons (regexp-quote (buffer-name))
                                      '(display-buffer-no-window))
                                display-buffer-alist))
                         (warning-inhibit-types (cons '(bytecomp) (and (boundp 'warning-inhibit-types) warning-inhibit-types)))
                         (warning-suppress-types (cons '(bytecomp) (and (boundp 'warning-suppress-types) warning-suppress-types)))
                         (warning-suppress-log-types (cons '(bytecomp) (and (boundp 'warning-suppress-log-types) warning-suppress-log-types)))
                         (inhibit-message t)
                         (message-log-max nil))
                     (funcall real-byte-compile file)))
                 file)))
      (apply orig-fn args))))

;;;###autoload
(defun elpaish-install-package-quickstart-stale-p ()
  "Return non-nil when quickstart file or its .elc is missing or stale."
  (let ((elc (concat (file-name-sans-extension package-quickstart-file) ".elc")))
    (or (not (file-exists-p package-quickstart-file))
        (not (file-exists-p elc))
        (time-less-p (file-attribute-modification-time (file-attributes elc))
                     (file-attribute-modification-time (file-attributes package-quickstart-file)))
        (and (file-directory-p package-user-dir)
             (let ((quickstart-mtime (file-attribute-modification-time
                                      (file-attributes elc))))
               (seq-some (lambda (pkg-dir)
                           (and (file-directory-p pkg-dir)
                                (file-exists-p (expand-file-name (package--description-file pkg-dir) pkg-dir))
                                (time-less-p quickstart-mtime
                                             (file-attribute-modification-time (file-attributes pkg-dir)))))
                         (directory-files package-user-dir t "\\`[^.]" t)))))))

;;;###autoload
(defun elpaish-install-refresh-package-quickstart (&optional force)
  "Regenerate `package-quickstart-file' and clear the debounce timer.
When FORCE is nil, do nothing unless quickstart file is stale."
  (interactive "P")
  (setq elpaish-install--quickstart-refresh-timer nil)
  (when (or force (elpaish-install-package-quickstart-stale-p))
    (logx 'info :msg (format "[elpaish] %s; refreshing package quickstart"
                             (if force "Package changes detected or refresh requested"
                               "Package quickstart is stale"))
          :reason (if force "Package changes detected or refresh requested"
                    "Package quickstart is stale"))
    (require 'package)
    (elpaish-install-sync-package-load-paths)
    (package-quickstart-refresh)
    (when (and (fboundp 'native-comp-available-p)
               (native-comp-available-p)
               (boundp 'package-quickstart-file)
               (file-exists-p package-quickstart-file))
      (if (fboundp 'native-compile-async)
          (native-compile-async package-quickstart-file)
        (when (fboundp 'native-compile)
          (native-compile package-quickstart-file))))))

;;;###autoload
(defun elpaish-install-flush-package-quickstart (&optional force)
  "Ensure any pending `package-quickstart-refresh' runs immediately.
When FORCE is non-nil, force refresh even if no debounce timer was pending."
  (when (or force (timerp elpaish-install--quickstart-refresh-timer))
    (when (timerp elpaish-install--quickstart-refresh-timer)
      (cancel-timer elpaish-install--quickstart-refresh-timer)
      (setq elpaish-install--quickstart-refresh-timer nil))
    (elpaish-install-refresh-package-quickstart t)))

;;;###autoload
(defun elpaish-install-schedule-quickstart-refresh (&rest _)
  "Debounce regeneration of `package-quickstart-file' after package operations."
  (if noninteractive
      (elpaish-install-refresh-package-quickstart t)
    (when (timerp elpaish-install--quickstart-refresh-timer)
      (cancel-timer elpaish-install--quickstart-refresh-timer))
    (setq elpaish-install--quickstart-refresh-timer
          (run-with-idle-timer elpaish-install-quickstart-refresh-delay
                               nil
                               (lambda () (elpaish-install-refresh-package-quickstart t))))))

(defun elpaish-install--on-bootstrap-finished (results _action)
  "Schedule quickstart refresh when RESULTS include changes."
  (when (seq-some (lambda (result)
                    (memq (plist-get result :status) '(:installed :upgraded)))
                  results)
    (elpaish-install-schedule-quickstart-refresh)))

(advice-add 'package-quickstart-refresh :around #'elpaish-install--package-quickstart-refresh-optimize)

(add-hook 'elpaish-install-bootstrap-finished-functions
          #'elpaish-install--on-bootstrap-finished)

(provide 'elpaish-install)

;; Local Variables:
;; package-lint-main-file: "elpaish.el"
;; End:

;;; elpaish-install.el ends here
