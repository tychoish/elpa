;;; arch-elpa.el --- Package management UI for Emacs Lisp packages -*- lexical-binding: t; -*-
;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (annotated-completing-read "0.1.0") (yaml "0.5.0") (compat "30.0.0.0"))
;; Keywords: tools, package, arch, elpa
;; URL: https://github.com/tychoish/arch.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;; A tabulated-list package manager for ELPA/MELPA packages, mirroring the
;; browsing/search/install/upgrade UX of `arch.el' (persistent package list,
;; ACR-driven search and selection, marks for batch operations) but built
;; directly on `package.el'.  Unlike `arch.el', installs and removals run
;; synchronously in this Emacs process, so there is no compile/progress
;; buffer machinery here; package detail views delegate to the built-in
;; `describe-package' rather than reimplementing one.

;;; Code:

(require 'package)
(require 'seq)
(require 'map)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)
(require 'annotated-completing-read)
(require 'arch)
(require 'compat)

;;; Package info helpers

(defun arch-elpa--ensure-initialized ()
  "Ensure `package-alist' and `package-archive-contents' are populated.
This config sets `package-enable-at-startup' to nil and boots via
`package-quickstart' instead, so packages are on `load-path' and active
from startup but `package.el's own bookkeeping tables are never populated
until something asks for them — mirrors what `package-install' does
internally via `package--archives-initialize'."
  (unless package--initialized
    (package-initialize t))
  (unless package-archive-contents
    (condition-case err
        (package-refresh-contents)
      (error (message "arch-elpa: could not refresh archive contents: %s"
                      (error-message-string err))))))

(defun arch-elpa--installed-desc (name)
  "Return the active installed `package-desc' for NAME, or nil."
  (car (map-elt package-alist name)))

(defun arch-elpa--available-desc (name)
  "Return best available `package-desc' for NAME from archive, or nil."
  (car (map-elt package-archive-contents name)))

(defun arch-elpa--upgradeable-p (name)
  "Return non-nil if NAME has an installed version older than available."
  (when-let* ((installed (arch-elpa--installed-desc name))
              (available (arch-elpa--available-desc name)))
    (version-list-< (package-desc-version installed) (package-desc-version available))))

(defun arch-elpa--all-names ()
  "Return a sorted list of all known package name symbols.
Union of installed packages and packages available from configured archives."
  (arch-elpa--ensure-initialized)
  (thread-last (append (map-keys package-alist) (map-keys package-archive-contents))
    (delete-dups)
    (seq-sort #'string-lessp)))

(cl-defstruct (arch-elpa-pkg (:constructor arch-elpa-pkg--make) (:copier nil))
  "Represents an Emacs Lisp package known to `package.el'."
  name          ; symbol
  version       ; string, "-" when unknown
  archive       ; string, e.g. "melpa", "gnu", "builtin", "unknown"
  summary       ; string
  installed-p   ; boolean
  upgradeable-p ; boolean
  built-in-p)   ; boolean — no installed or available desc, but Emacs ships it

(defun arch-elpa--pkg-for-name (name)
  "Build an `arch-elpa-pkg' struct for package NAME symbol."
  (let* ((installed (arch-elpa--installed-desc name))
         (available (arch-elpa--available-desc name))
         (desc (or installed available))
         (archive (cond
                   ((and desc (package-desc-archive desc)))
                   ((package-built-in-p name) "builtin")
                   (t "unknown"))))
    (arch-elpa-pkg--make
     :name name
     :version (if desc (package-version-join (package-desc-version desc)) "-")
     :archive archive
     :summary (or (and desc (package-desc-summary desc)) "")
     :installed-p (and installed t)
     :upgradeable-p (arch-elpa--upgradeable-p name)
     :built-in-p (and (not installed) (not available) (package-built-in-p name) t))))

;;; Tabulated list view

(defconst arch-elpa--list-buffer-name "*arch-elpa-packages*"
  "Name of the arch-elpa package list buffer.")

(defvar-local arch-elpa--marked (make-hash-table :test #'equal)
  "Hash table of marked package name symbols in the current arch-elpa list buffer.")

(defvar-local arch-elpa--filter nil
  "Current filter predicate (arch-elpa-pkg → bool), or nil for no filter.")

(defvar-local arch-elpa--all-entries nil
  "Full unfiltered tabulated-list entries for the current arch-elpa list buffer.")

(defvar-local arch-elpa--list-wide t
  "When non-nil, show all known packages; when nil, installed packages only.")

(defun arch-elpa--pkg-status (pkg)
  "Return a propertized status string for PKG."
  (cond
   ((arch-elpa-pkg-built-in-p pkg) (propertize "built-in" 'face 'arch-face-available))
   ((arch-elpa-pkg-installed-p pkg) (propertize "installed" 'face 'arch-face-installed))
   (t (propertize "avail" 'face 'arch-face-available))))

(defun arch-elpa--build-entry (pkg)
  "Build a tabulated-list entry for `arch-elpa-pkg' PKG."
  (list pkg
        (vector
         (propertize (symbol-name (arch-elpa-pkg-name pkg))
                     'face (if (map-elt arch-elpa--marked (arch-elpa-pkg-name pkg))
                               'arch-face-pkg-link-marked
                             'arch-face-pkg-link))
         (arch-elpa-pkg-archive pkg)
         (arch-elpa--pkg-status pkg)
         (if (arch-elpa-pkg-upgradeable-p pkg)
             (propertize (arch-elpa-pkg-version pkg) 'face 'arch-face-version-old)
           (arch-elpa-pkg-version pkg))
         (arch-elpa-pkg-summary pkg))))

(defvar-keymap arch-elpa-list-mode-map
  :doc "Keymap for `arch-elpa-list-mode'."
  :parent tabulated-list-mode-map
  "i"     #'arch-elpa-list-install
  "a"     #'arch-elpa-list-actions
  "r"     #'arch-elpa-list-remove
  "u"     #'arch-elpa-list-upgrade
  "U"     #'arch-elpa-list-upgrade-all
  "v i"   #'arch-elpa-list-view-installed
  "v u"   #'arch-elpa-list-view-upgradeable
  "f"     #'arch-elpa-list-filter
  "F"     #'arch-elpa-list-filter-clear
  "w"     #'arch-elpa-list-toggle-wide
  "s"     #'arch-elpa-search
  "/"     #'arch-elpa-list-find
  "SPC"   #'arch-elpa-list-toggle-mark
  "m"     #'arch-elpa-list-mark
  "M"     #'arch-elpa-list-unmark-all
  "DEL"   #'arch-elpa-list-unmark
  "D"     #'arch-elpa-search-deps-of-dependents
  "v g"   #'arch-elpa-list-view-deps-of-dependents
  "v d g" #'arch-elpa-list-view-deps-of-dependents
  "g"     #'arch-elpa-list-refresh
  "C"     #'arch-elpa-list-refresh-contents
  "RET"   #'arch-elpa-list-show-info
  "?"     #'arch-elpa-list-menu)

(define-derived-mode arch-elpa-list-mode tabulated-list-mode "arch-elpa"
  "Major mode for browsing and managing Emacs Lisp packages via `package.el'.

Columns: Name | Archive | Status | Version | Summary
  Status: installed / avail / built-in
  Version: shown in bold red when an upgrade is available

Marks: SPC toggles, m marks, DEL unmarks, M unmarks all.
Marked names appear bold.
Filter: f to set, F to clear.  a for package actions menu.
Wide mode: w toggles all-packages view (installed-only vs installed+available).

\\{arch-elpa-list-mode-map}"
  (setq tabulated-list-format
        (vector
         '("Name"     30 t)
         '("Archive"  10 t)
         '("Status"    9 t)
         '("Version"  14 t)
         '("Summary"  44 nil)))
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header))

(defun arch-elpa-list-toggle-wide ()
  "Toggle between installed-only and installed+available package view."
  (interactive)
  (setq arch-elpa--list-wide (not arch-elpa--list-wide))
  (arch-elpa-list-refresh)
  (message "arch-elpa: %s" (if arch-elpa--list-wide "showing all packages" "showing installed only")))

(defun arch-elpa-list-refresh ()
  "Refresh the arch-elpa package list buffer from current package.el state."
  (interactive)
  (arch-depgraph-clear 'elpa)
  (arch-elpa--ensure-initialized)
  (let* ((names (if arch-elpa--list-wide (arch-elpa--all-names) (map-keys package-alist)))
         (pkgs (seq-map #'arch-elpa--pkg-for-name names)))
    (setq arch-elpa--all-entries (seq-map #'arch-elpa--build-entry pkgs))
    (setq tabulated-list-entries
          (if arch-elpa--filter
              (seq-filter (lambda (e) (funcall arch-elpa--filter (car e))) arch-elpa--all-entries)
            arch-elpa--all-entries))
    (tabulated-list-print t)))

(defun arch-elpa-list-refresh-contents ()
  "Refresh archive contents from configured archives, then rebuild the list."
  (interactive)
  (message "arch-elpa: refreshing archive contents...")
  (package-refresh-contents)
  (arch-elpa-list-refresh)
  (message "arch-elpa: archive contents refreshed"))

;;; Dependency analysis & driver graph

(defun arch-elpa--installed-third-party-names ()
  "Return list of symbols for third-party packages in `package-user-dir'."
  (arch-elpa--ensure-initialized)
  (let ((user-dir (and package-user-dir (file-name-as-directory (expand-file-name package-user-dir))))
        (names nil))
    (dolist (entry package-alist)
      (let* ((name (car entry))
             (desc (cadr entry))
             (dir (and desc (package-desc-dir desc))))
        (when (or (null user-dir)
                  (null dir)
                  (file-in-directory-p dir user-dir)
                  (string-prefix-p user-dir (expand-file-name dir)))
          (push name names))))
    (sort (delete-dups names) (lambda (a b) (string-lessp (symbol-name a) (symbol-name b))))))

(defun arch-elpa--package-direct-deps (name)
  "Return direct dependency package symbols for package NAME."
  (when-let* ((desc (or (arch-elpa--installed-desc name)
                        (arch-elpa--available-desc name)))
              (reqs (package-desc-reqs desc)))
    (thread-last reqs
      (mapcar #'car)
      (seq-remove (lambda (req) (eq req 'emacs)))
      (delete-dups))))

(defun arch-elpa-depgraph (&optional force)
  "Return the cached `arch-depgraph' for installed ELPA packages.
When FORCE is non-nil, rebuild the graph from current `package.el' state."
  (arch-depgraph-get
   'elpa
   (lambda ()
     (arch-elpa--ensure-initialized)
     (let ((pkgs (arch-elpa--installed-third-party-names)))
       (arch-depgraph-build 'elpa pkgs #'arch-elpa--package-direct-deps)))
   force))

;;;###autoload
(defun arch-elpa-package-dependencies (&optional pkg-name force)
  "Find installed packages and list their dependencies and dependents.
When PKG-NAME is non-nil (symbol or string), return a plist of dependency
analysis for that package:
  (:package               PKG-NAME (symbol)
   :installed-p           boolean
   :dependencies          direct dependencies (list of symbols)
   :dependents            direct dependents (list of symbols)
   :transitive-deps       all recursive dependencies (list of symbols)
   :transitive-dependents all recursive dependents (list of symbols)
   :deps-of-dependents    dependencies of all packages that depend on it
   :zero-dependencies-p   boolean
   :zero-dependents-p     boolean)

When PKG-NAME is nil, return an alist mapping each installed package symbol
to its dependency analysis plist.

When called interactively, prompt for a package using ACR and display its
dependency summary in the echo area (or show info with prefix argument)."
  (interactive
   (list (when current-prefix-arg
           (intern (arch-elpa--select-package
                    "Package dependencies: "
                    (seq-map #'arch-elpa--pkg-for-name
                             (arch-elpa--installed-third-party-names)))))))
  (let* ((graph (arch-elpa-depgraph force)))
    (if pkg-name
        (let* ((sym (if (stringp pkg-name) (intern pkg-name) pkg-name))
               (installed-p (and (arch-elpa--installed-desc sym) t))
               (deps (arch-depgraph-dependencies graph sym))
               (dependents (arch-depgraph-dependents graph sym))
               (trans-deps (arch-depgraph-dependencies graph sym t))
               (trans-dependents (arch-depgraph-dependents graph sym t))
               (deps-of-deps (arch-depgraph-deps-of-dependents graph sym))
               (result
                (list :package sym
                      :installed-p installed-p
                      :dependencies deps
                      :dependents dependents
                      :transitive-deps trans-deps
                      :transitive-dependents trans-dependents
                      :deps-of-dependents deps-of-deps
                      :zero-dependencies-p (null deps)
                      :zero-dependents-p (null dependents))))
          (when (called-interactively-p 'interactive)
            (message "%s: %d dependencies, %d dependents, %d deps-of-dependents"
                     sym (length deps) (length dependents) (length deps-of-deps)))
          result)
      (if (called-interactively-p 'interactive)
          (let ((choice (intern (arch-elpa--select-package
                                 "Package dependencies: "
                                 (seq-map #'arch-elpa--pkg-for-name
                                          (arch-elpa--installed-third-party-names))))))
            (arch-elpa-show-info choice))
        (mapcar (lambda (name)
                  (cons name (arch-elpa-package-dependencies name force)))
                (arch-depgraph-nodes graph))))))

;;; Filter predicates

(defun arch-elpa--filter-upgradeable-p (pkg) (arch-elpa-pkg-upgradeable-p pkg))
(defun arch-elpa--filter-installed-p (pkg)   (arch-elpa-pkg-installed-p pkg))
(defun arch-elpa--filter-available-p (pkg)   (not (arch-elpa-pkg-installed-p pkg)))
(defun arch-elpa--filter-built-in-p (pkg)    (arch-elpa-pkg-built-in-p pkg))

(defun arch-elpa--filter-zero-dependencies-p (pkg)
  "Return non-nil if PKG is installed and has zero dependencies."
  (let* ((name (arch-elpa-pkg-name pkg))
         (graph (arch-elpa-depgraph)))
    (and (arch-elpa-pkg-installed-p pkg)
         (null (arch-depgraph-dependencies graph name)))))

(defun arch-elpa--filter-zero-dependents-p (pkg)
  "Return non-nil if PKG is installed and has zero dependents."
  (let* ((name (arch-elpa-pkg-name pkg))
         (graph (arch-elpa-depgraph)))
    (and (arch-elpa-pkg-installed-p pkg)
         (null (arch-depgraph-dependents graph name)))))

(defconst arch-elpa--filter-options
  '(("upgradeable"       arch-elpa--filter-upgradeable-p       "has an available upgrade")
    ("installed"         arch-elpa--filter-installed-p         "currently installed")
    ("available"         arch-elpa--filter-available-p         "available but not installed")
    ("built-in"          arch-elpa--filter-built-in-p          "ships with Emacs, no package.el entry")
    ("zero-dependencies" arch-elpa--filter-zero-dependencies-p "installed with no dependencies (leaf)")
    ("zero-dependents"   arch-elpa--filter-zero-dependents-p   "installed with no dependents (top-level)"))
  "Alist of (label predicate-symbol description) for arch-elpa-list filters.")

(defun arch-elpa-list-filter ()
  "Set a filter on the arch-elpa package list via ACR."
  (interactive)
  (let ((choice (annotated-completing-read
                 (seq-map (lambda (opt) (cons (car opt) (caddr opt))) arch-elpa--filter-options)
                 :prompt "Filter packages: "
                 :require-match t
                 :category 'arch-elpa-filter)))
    (setq arch-elpa--filter (symbol-function (cadr (assoc choice arch-elpa--filter-options))))
    (setq tabulated-list-entries
          (seq-filter (lambda (e) (funcall arch-elpa--filter (car e))) arch-elpa--all-entries))
    (tabulated-list-print t)
    (message "arch-elpa: filter: %s" choice)))

(defun arch-elpa-list-filter-clear ()
  "Remove the active filter and show all packages."
  (interactive)
  (setq arch-elpa--filter nil)
  (setq tabulated-list-entries arch-elpa--all-entries)
  (tabulated-list-print t)
  (message "arch-elpa: filter cleared"))

(defun arch-elpa-list-set-filter (predicate label)
  "Filter the arch-elpa list buffer with PREDICATE labeled LABEL."
  (setq arch-elpa--filter predicate)
  (setq tabulated-list-entries
        (seq-filter (lambda (e) (funcall arch-elpa--filter (car e))) arch-elpa--all-entries))
  (tabulated-list-print t)
  (message "arch-elpa: filter: %s" label))

(defun arch-elpa-list-view-installed ()
  "Show installed packages in the current list."
  (interactive)
  (arch-elpa-list-set-filter #'arch-elpa--filter-installed-p "installed"))

(defun arch-elpa-list-view-upgradeable ()
  "Show upgradeable packages in the current list."
  (interactive)
  (arch-elpa-list-set-filter #'arch-elpa--filter-upgradeable-p "upgradeable"))

(defun arch-elpa-list-view-zero-dependencies ()
  "Filter the list to installed packages with zero dependencies."
  (interactive)
  (arch-elpa-list-set-filter #'arch-elpa--filter-zero-dependencies-p "zero dependencies"))

(defun arch-elpa-list-view-zero-dependents ()
  "Filter the list to installed packages with zero dependents."
  (interactive)
  (arch-elpa-list-set-filter #'arch-elpa--filter-zero-dependents-p "zero dependents"))

(defun arch-elpa-list-view-deps-of-dependents (&optional target-pkg)
  "Filter the list to show dependencies of dependents of TARGET-PKG."
  (interactive
   (list (if (arch-elpa-list--pkg-at-point-p)
             (arch-elpa-pkg-name (arch-elpa--pkg-at-point))
           (intern (arch-elpa--select-package "Package: ")))))
  (let* ((sym (if (stringp target-pkg) (intern target-pkg) target-pkg))
         (graph (arch-elpa-depgraph))
         (deps-of-deps (arch-depgraph-deps-of-dependents graph sym))
         (pred (lambda (pkg) (memq (arch-elpa-pkg-name pkg) deps-of-deps))))
    (arch-elpa-list-set-filter pred (format "deps of dependents of %s (%d)" sym (length deps-of-deps)))))

;;; Package actions

(defun arch-elpa--pkg-at-point ()
  "Return arch-elpa-pkg at point or signal a user error."
  (or (tabulated-list-get-id)
      (user-error "No package at point")))

(defun arch-elpa-list--pkg-at-point-p ()
  "Return non-nil if there is a package at point."
  (tabulated-list-get-id))

(defun arch-elpa--select-package (prompt &optional pkgs)
  "ACR-select an elpa package with PROMPT; return selected name or nil.
PKGS restricts the candidate set; defaults to all known packages."
  (let* ((pkgs (or pkgs (seq-map #'arch-elpa--pkg-for-name (arch-elpa--all-names))))
         (index (map-into (seq-map (lambda (p) (cons (symbol-name (arch-elpa-pkg-name p)) p)) pkgs)
                          '(hash-table :test equal))))
    (annotated-completing-read
     (seq-map (lambda (pkg)
                (cons (symbol-name (arch-elpa-pkg-name pkg))
                      (format "[%s] %s %s"
                              (arch-elpa-pkg-archive pkg)
                              (arch-elpa-pkg-version pkg)
                              (arch-elpa-pkg-summary pkg))))
              pkgs)
     :prompt prompt
     :require-match t
     :category 'arch-elpa-package
     :group-name (lambda (name) (arch-elpa-pkg-archive (map-elt index name))))))

;;;###autoload
(defun arch-elpa-search ()
  "ACR-search all known elisp packages and show info for the selection."
  (interactive)
  (when-let* ((name (arch-elpa--select-package "Search elisp packages: ")))
    (arch-elpa-show-info (intern name))))

;;;###autoload
(defun arch-elpa-search-deps-of-dependents (&optional target-pkg)
  "ACR-search dependencies of dependents for TARGET-PKG and show info.
If TARGET-PKG is nil, prompt with ACR."
  (interactive
   (list (if (and (derived-mode-p 'arch-elpa-list-mode) (arch-elpa-list--pkg-at-point-p))
             (arch-elpa-pkg-name (arch-elpa--pkg-at-point))
           (intern (arch-elpa--select-package "Dependencies of dependents for: ")))))
  (let* ((sym (if (stringp target-pkg) (intern target-pkg) target-pkg))
         (graph (arch-elpa-depgraph))
         (deps-of-deps (arch-depgraph-deps-of-dependents graph sym)))
    (if (null deps-of-deps)
        (message "arch-elpa: no dependencies of dependents found for %s" sym)
      (let* ((dependents (arch-depgraph-dependents graph sym))
             (table
              (mapcar
               (lambda (dep-sym)
                 (let* ((pkg (arch-elpa--pkg-for-name dep-sym))
                        (why (seq-filter (lambda (d)
                                           (memq dep-sym (arch-depgraph-dependencies graph d)))
                                         dependents))
                        (why-str (mapconcat #'symbol-name why ", ")))
                   (cons (symbol-name dep-sym)
                         (format "[%s] %s | required by: %s"
                                 (arch-elpa-pkg-archive pkg)
                                 (arch-elpa-pkg-version pkg)
                                 why-str))))
               deps-of-deps))
             (selected (annotated-completing-read
                        table
                        :prompt (format "Deps of dependents of %s: " sym)
                        :require-match t
                        :category 'arch-elpa-package)))
        (when selected
          (arch-elpa-show-info (intern selected)))))))

;;;###autoload
(defun arch-elpa-install ()
  "ACR-select and install an elisp package via `package-install'."
  (interactive)
  (let* ((pkgs (seq-remove #'arch-elpa-pkg-installed-p
                          (seq-map #'arch-elpa--pkg-for-name (arch-elpa--all-names))))
         (name (arch-elpa--select-package "Install elisp package: " pkgs)))
    (when name
      (package-install (intern name))
      (message "arch-elpa: installed %s" name))))

(defun arch-elpa-list-install ()
  "Install the package at point via `package-install'."
  (interactive)
  (let* ((pkg (arch-elpa--pkg-at-point))
         (name (arch-elpa-pkg-name pkg)))
    (when (arch-elpa-pkg-installed-p pkg)
      (user-error "%s is already installed; use `arch-elpa-list-upgrade' instead" name))
    (package-install name)
    (arch-elpa-list-refresh)
    (message "arch-elpa: installed %s" name)))

(defun arch-elpa-list-remove ()
  "Remove the installed package at point via `package-delete'."
  (interactive)
  (let* ((pkg (arch-elpa--pkg-at-point))
         (name (arch-elpa-pkg-name pkg))
         (desc (or (arch-elpa--installed-desc name)
                  (user-error "%s is not installed" name))))
    (when (yes-or-no-p (format "Remove %s? " name))
      (package-delete desc)
      (arch-elpa-list-refresh)
      (message "arch-elpa: removed %s" name))))

(defun arch-elpa-list-upgrade ()
  "Upgrade the package at point via `package-upgrade'."
  (interactive)
  (let* ((pkg (arch-elpa--pkg-at-point))
         (name (arch-elpa-pkg-name pkg)))
    (unless (arch-elpa-pkg-upgradeable-p pkg)
      (user-error "%s has no available upgrade" name))
    (package-upgrade name)
    (arch-elpa-list-refresh)
    (message "arch-elpa: upgraded %s" name)))

;;;###autoload
(defun arch-elpa-list-upgrade-all ()
  "Upgrade all packages with an available upgrade via `package-upgrade-all'."
  (interactive)
  (when (yes-or-no-p "Upgrade all elisp packages? ")
    (package-upgrade-all)
    (when (derived-mode-p 'arch-elpa-list-mode)
      (arch-elpa-list-refresh))
    (message "arch-elpa: upgrade complete")))

;;; Package info view

(defconst arch-elpa--info-buffer-prefix "*arch-elpa-info<"
  "Prefix for arch-elpa info buffer names.")

(defun arch-elpa--info-buffer-name (pkg-name)
  "Return the buffer name for displaying PKG-NAME info."
  (format "*arch-elpa-info<%s>*" pkg-name))

(defvar-local arch-elpa--info-package nil
  "Package symbol displayed in the current arch-elpa-info buffer.")

(defvar-keymap arch-elpa-info-mode-map
  :doc "Keymap for `arch-elpa-info-mode'."
  :parent help-mode-map
  "q" #'quit-window
  "d" #'arch-elpa-info-describe-package
  "D" #'arch-elpa-info-search-deps-of-dependents
  "i" #'arch-elpa-info-install
  "r" #'arch-elpa-info-remove
  "u" #'arch-elpa-info-upgrade
  "s" #'arch-elpa-search
  "l" #'arch-elpa-list
  "?" #'arch-elpa-info-menu)

(define-derived-mode arch-elpa-info-mode special-mode "arch-elpa-info"
  "Major mode for viewing detailed package information in arch-elpa.

\\{arch-elpa-info-mode-map}"
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (when arch-elpa--info-package
                  (arch-elpa-show-info arch-elpa--info-package)))))

(defun arch-elpa-info-describe-package ()
  "Show built-in `describe-package' for the current package."
  (interactive)
  (when arch-elpa--info-package
    (describe-package arch-elpa--info-package)))

(defun arch-elpa-info-search-deps-of-dependents ()
  "Search dependencies of dependents for the current package."
  (interactive)
  (when arch-elpa--info-package
    (arch-elpa-search-deps-of-dependents arch-elpa--info-package)))

(defun arch-elpa-info-install ()
  "Install the current package via `package-install'."
  (interactive)
  (when arch-elpa--info-package
    (package-install arch-elpa--info-package)
    (arch-depgraph-clear 'elpa)
    (arch-elpa-show-info arch-elpa--info-package)
    (message "arch-elpa: installed %s" arch-elpa--info-package)))

(defun arch-elpa-info-remove ()
  "Remove the current package via `package-delete'."
  (interactive)
  (when arch-elpa--info-package
    (if-let* ((desc (arch-elpa--installed-desc arch-elpa--info-package)))
        (when (yes-or-no-p (format "Remove %s? " arch-elpa--info-package))
          (package-delete desc)
          (arch-depgraph-clear 'elpa)
          (arch-elpa-show-info arch-elpa--info-package)
          (message "arch-elpa: removed %s" arch-elpa--info-package))
      (user-error "%s is not installed" arch-elpa--info-package))))

(defun arch-elpa-info-upgrade ()
  "Upgrade the current package via `package-upgrade'."
  (interactive)
  (when arch-elpa--info-package
    (if (arch-elpa--upgradeable-p arch-elpa--info-package)
        (progn
          (package-upgrade arch-elpa--info-package)
          (arch-depgraph-clear 'elpa)
          (arch-elpa-show-info arch-elpa--info-package)
          (message "arch-elpa: upgraded %s" arch-elpa--info-package))
      (user-error "%s has no available upgrade" arch-elpa--info-package))))

(transient-define-prefix arch-elpa-info-menu ()
  "Actions for the arch-elpa info buffer."
  [["Actions"
    ("d"  "Describe (help)"     arch-elpa-info-describe-package)
    ("D"  "Deps of dependents"  arch-elpa-info-search-deps-of-dependents)
    ("i"  "Install"             arch-elpa-info-install)
    ("r"  "Remove"              arch-elpa-info-remove)
    ("u"  "Upgrade"             arch-elpa-info-upgrade)]
   ["Navigation"
    ("s"  "Search packages"     arch-elpa-search)
    ("l"  "Package list"        arch-elpa-list)
    ("q"  "Quit"                quit-window)]])

(defun arch-elpa--render-pkg-button (pkg-sym)
  "Insert a clickable button for PKG-SYM that opens its info buffer."
  (let ((name-str (symbol-name pkg-sym)))
    (insert-text-button name-str
                        'action (lambda (_b) (arch-elpa-show-info pkg-sym))
                        'follow-link t
                        'face 'arch-face-pkg-link
                        'help-echo (format "mouse-1: show info for %s" name-str))))

(defun arch-elpa--insert-pkg-list (key pkg-symbols)
  "Insert YAML KEY with clickable buttons for each symbol in PKG-SYMBOLS."
  (insert (arch--yaml-key-str key) "\n")
  (if (null pkg-symbols)
      (insert "  - None\n")
    (dolist (sym pkg-symbols)
      (insert "  - ")
      (arch-elpa--render-pkg-button sym)
      (insert "\n"))))

;;;###autoload
(defun arch-elpa-show-info (pkg-name)
  "Display detailed info for PKG-NAME in an arch-elpa info buffer."
  (interactive
   (list (intern (arch-elpa--select-package "Package info: "))))
  (arch-elpa--ensure-initialized)
  (let* ((sym (if (stringp pkg-name) (intern pkg-name) pkg-name))
         (pkg (arch-elpa--pkg-for-name sym))
         (desc (or (arch-elpa--installed-desc sym)
                   (arch-elpa--available-desc sym)))
         (dep-info (arch-elpa-package-dependencies sym))
         (buf-name (arch-elpa--info-buffer-name sym)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (arch-elpa-info-mode)
        (setq arch-elpa--info-package sym)
        (arch--yaml-insert-pair "name" (symbol-name sym))
        (arch--yaml-insert-pair "version" (arch-elpa-pkg-version pkg))
        (arch--yaml-insert-pair "archive" (arch-elpa-pkg-archive pkg))
        (arch--yaml-insert-pair "status" (if (arch-elpa-pkg-installed-p pkg) "installed" "available"))
        (when-let* ((summary (arch-elpa-pkg-summary pkg))
                    ((not (string-empty-p summary))))
          (arch--yaml-insert-pair "summary" summary))
        (when-let* ((desc desc)
                    (extras (package-desc-extras desc))
                    (url (or (alist-get :url extras) (alist-get 'url extras))))
          (arch--yaml-insert-url "url" (format "%s" url)))
        (when-let* ((inst-desc (arch-elpa--installed-desc sym))
                    (dir (package-desc-dir inst-desc)))
          (arch--yaml-insert-pair "directory" dir))
        (arch-elpa--insert-pkg-list "depends-on" (plist-get dep-info :dependencies))
        (arch-elpa--insert-pkg-list "required-by" (plist-get dep-info :dependents))
        (arch-elpa--insert-pkg-list "deps-of-dependents" (plist-get dep-info :deps-of-dependents))
        (arch--yaml-insert-pair "zero-dependencies" (if (plist-get dep-info :zero-dependencies-p) "true" "false"))
        (arch--yaml-insert-pair "zero-dependents" (if (plist-get dep-info :zero-dependents-p) "true" "false"))
        (goto-char (point-min)))
      (pop-to-buffer (current-buffer)))))

(defun arch-elpa-list-show-info ()
  "Show package info for the entry at point via `arch-elpa-show-info'."
  (interactive)
  (arch-elpa-show-info (arch-elpa-pkg-name (arch-elpa--pkg-at-point))))

(defun arch-elpa-list-find ()
  "Jump to a package in the current list using ACR."
  (interactive)
  (let* ((pkg-index (map-into
                     (seq-map (lambda (e) (cons (symbol-name (arch-elpa-pkg-name (car e))) (car e)))
                              tabulated-list-entries)
                     '(hash-table :test equal)))
         (candidates
          (seq-map (lambda (e)
                     (let* ((pkg (car e))
                            (name (symbol-name (arch-elpa-pkg-name pkg))))
                       (cons name
                             (format "[%s] %s %s"
                                     (arch-elpa-pkg-archive pkg)
                                     (arch-elpa-pkg-version pkg)
                                     (arch-elpa-pkg-summary pkg)))))
                   tabulated-list-entries))
         (name (annotated-completing-read
                candidates
                :prompt "Jump to package: "
                :require-match t
                :category 'arch-elpa-package
                :group-name (lambda (n) (arch-elpa-pkg-archive (map-elt pkg-index n))))))
    (when name
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (equal (when-let* ((pkg (tabulated-list-get-id)))
                                (symbol-name (arch-elpa-pkg-name pkg)))
                              name)))
        (forward-line 1)))))

;;; Marks & batch operations

(defun arch-elpa-list-toggle-mark ()
  "Toggle mark on the package at point without moving point."
  (interactive)
  (when-let* ((pkg (tabulated-list-get-id))
              (name (arch-elpa-pkg-name pkg)))
    (if (map-elt arch-elpa--marked name)
        (map-delete arch-elpa--marked name)
      (setf (map-elt arch-elpa--marked name) t))
    (tabulated-list-set-col 0
      (propertize (symbol-name name)
                  'face (if (map-elt arch-elpa--marked name)
                            'arch-face-pkg-link-marked
                          'arch-face-pkg-link))
      t)))

(defun arch-elpa-list-mark ()
  "Mark the package at point and advance to the next line."
  (interactive)
  (when-let* ((pkg (tabulated-list-get-id)))
    (setf (map-elt arch-elpa--marked (arch-elpa-pkg-name pkg)) t)
    (tabulated-list-set-col 0
      (propertize (symbol-name (arch-elpa-pkg-name pkg)) 'face 'arch-face-pkg-link-marked)
      t)
    (forward-line 1)))

(defun arch-elpa-list-unmark ()
  "Unmark the package at point and move to the previous line."
  (interactive)
  (when-let* ((pkg (tabulated-list-get-id)))
    (map-delete arch-elpa--marked (arch-elpa-pkg-name pkg))
    (tabulated-list-set-col 0
      (propertize (symbol-name (arch-elpa-pkg-name pkg)) 'face 'arch-face-pkg-link)
      t)
    (forward-line -1)))

(defun arch-elpa-list-unmark-all ()
  "Unmark all packages in the list."
  (interactive)
  (clrhash arch-elpa--marked)
  (tabulated-list-print t))

(defun arch-elpa-list-install-marked ()
  "Install all marked packages via `package-install'."
  (interactive)
  (let ((names (or (map-keys arch-elpa--marked) (user-error "No packages marked"))))
    (when (yes-or-no-p (format "Install %d marked packages? " (length names)))
      (seq-do #'package-install names)
      (clrhash arch-elpa--marked)
      (arch-elpa-list-refresh))))

(defun arch-elpa-list-remove-marked ()
  "Remove all marked, installed packages via `package-delete'."
  (interactive)
  (let ((names (or (map-keys arch-elpa--marked) (user-error "No packages marked"))))
    (when (yes-or-no-p (format "Remove %d marked packages? " (length names)))
      (seq-do (lambda (name)
                (when-let* ((desc (arch-elpa--installed-desc name)))
                  (package-delete desc)))
              names)
      (clrhash arch-elpa--marked)
      (arch-elpa-list-refresh))))

(defun arch-elpa-list-actions ()
  "ACR interface of actions for the package at point."
  (interactive)
  (let* ((pkg (arch-elpa--pkg-at-point))
         (name (arch-elpa-pkg-name pkg))
         (actions
          (list
           (cons "show info"          (cons #'arch-elpa-list-show-info           "Display package details"))
           (cons "describe (help)"    (cons (lambda () (describe-package name)) "Display standard help buffer"))
           (cons "search deps of deps" (cons (lambda () (arch-elpa-search-deps-of-dependents name)) "ACR search deps of dependents"))
           (cons "view deps of deps"  (cons (lambda () (arch-elpa-list-view-deps-of-dependents name)) "Filter list to deps of dependents"))
           (cons "install"            (cons #'arch-elpa-list-install             "Install this package"))
           (cons "remove"             (cons #'arch-elpa-list-remove              "Remove this package"))
           (cons "upgrade"            (cons #'arch-elpa-list-upgrade             "Upgrade to latest version"))
           (cons "mark"               (cons #'arch-elpa-list-mark                "Add to marked set"))
           (cons "unmark"             (cons #'arch-elpa-list-unmark              "Remove from marked set"))))
         (choice (annotated-completing-read
                  (seq-map (lambda (a) (cons (car a) (cddr a))) actions)
                  :prompt (format "[%s]: " name)
                  :require-match t
                  :category 'arch-elpa-action)))
    (when-let* ((entry (assoc choice actions)))
      (funcall (cadr entry)))))

;;; Transient menus

(transient-define-prefix arch-elpa-list-menu ()
  "Actions for the arch-elpa package list buffer."
  [["Package"
    ("a"   "Actions menu"    arch-elpa-list-actions
     :inapt-if-not arch-elpa-list--pkg-at-point-p)
    ("i"   "Install"         arch-elpa-list-install
     :inapt-if-not arch-elpa-list--pkg-at-point-p)
    ("r"   "Remove"          arch-elpa-list-remove
     :inapt-if-not arch-elpa-list--pkg-at-point-p)
    ("u"   "Upgrade"         arch-elpa-list-upgrade
     :inapt-if-not arch-elpa-list--pkg-at-point-p)
    ("U"   "Upgrade all"     arch-elpa-list-upgrade-all)]
   ["Marked"
    ("SPC" "Toggle mark"     arch-elpa-list-toggle-mark)
    ("m"   "Mark"            arch-elpa-list-mark)
    ("DEL" "Unmark"          arch-elpa-list-unmark)
    ("M"   "Unmark all"      arch-elpa-list-unmark-all)
    ("xi"  "Install marked"  arch-elpa-list-install-marked)
    ("xr"  "Remove marked"   arch-elpa-list-remove-marked)]
   ["View & Filter"
    ("RET" "Package info"    arch-elpa-list-show-info
     :inapt-if-not arch-elpa-list--pkg-at-point-p)
    ("/"   "Find in list"    arch-elpa-list-find)
    ("w"   "Toggle wide"     arch-elpa-list-toggle-wide)
    ("f"   "Filter"          arch-elpa-list-filter)
    ("vi"  "Installed"       arch-elpa-list-view-installed)
    ("vu"  "Upgradeable"     arch-elpa-list-view-upgradeable)
    ("0d"  "Zero deps view"  arch-elpa-list-view-zero-dependencies)
    ("0r"  "Zero dependents view" arch-elpa-list-view-zero-dependents)
    ("D"   "Deps of dependents" arch-elpa-search-deps-of-dependents)
    ("vg"  "View deps of dependents" arch-elpa-list-view-deps-of-dependents)
    ("vdg" "View deps of dependents (alias)" arch-elpa-list-view-deps-of-dependents)
    ("xc"  "Clear filter"    arch-elpa-list-filter-clear)
    ("s"   "Search"          arch-elpa-search)
    ("g"   "Refresh"         arch-elpa-list-refresh)
    ("C"   "Refresh archives" arch-elpa-list-refresh-contents)]])

;;; Top-level commands

;;;###autoload
(defun arch-elpa-list ()
  "Open the arch-elpa package list."
  (interactive)
  (let ((buf (get-buffer-create arch-elpa--list-buffer-name)))
    (with-current-buffer buf
      (arch-elpa-list-mode)
      (arch-elpa-list-refresh))
    (pop-to-buffer buf)))

(provide 'arch-elpa)
;;; arch-elpa.el ends here
