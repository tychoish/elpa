;;; arch-debian.el --- Debian package backend for arch -*- lexical-binding: t; -*-

;;; Commentary:
;; apt-get and dpkg-query operations using arch's local or TRAMP worker.
;; Select the "apt" backend with `arch-select-host'.

;;; Code:

(require 'arch)

(defun arch-debian--query (&rest args)
  "Run query ARGS with stable English output on the selected host."
  (or (arch--run-sync args t)
      (error "Debian query failed: %s" (string-join args " "))))

(defun arch-debian--parse-installed (output)
  "Parse tab-separated dpkg-query OUTPUT into installed packages."
  (thread-last (split-string output "\n" t)
               (seq-map (lambda (line) (split-string line "\t")))
               (seq-filter (lambda (fields) (and (equal (car fields) "installed")
                                                 (>= (length fields) 4))))
               (seq-map (lambda (fields)
                          (arch-pkg--make :name (nth 1 fields) :version (nth 2 fields)
                                          :description (nth 3 fields) :repo "apt"
                                          :installed-p t)))))

(defun arch-debian--list ()
  "Return installed Debian packages, including architecture qualifiers."
  (arch-debian--parse-installed
   (arch-debian--installed-output)))

(defun arch-debian--installed-output ()
  "Query installed package metadata and dependency fields."
  (arch-debian--query "dpkg-query" "--show"
                      "--showformat=${db:Status-Status}\t${binary:Package}\t${Version}\t${binary:Summary}\t${Depends}, ${Pre-Depends}\n"))

(defun arch-debian--parse-control (output)
  "Parse Debian control OUTPUT into package metadata plists."
  (seq-map
   (lambda (record)
     (let (metadata)
       (seq-do
        (lambda (line)
          (when (string-match "\\`\\([^ :]+\\):[ \t]*\\(.*\\)\\'" line)
            (when-let* ((key (cdr (assoc (match-string 1 line)
                                         '(("Package" . name) ("Version" . version)
                                           ("Description" . description)
                                           ("Description-en" . description)
                                           ("Homepage" . url) ("Depends" . depends-on)
                                           ("Installed-Size" . installed-size))))))
              (setq metadata (plist-put metadata key (match-string 2 line))))))
        (split-string record "\n" t))
       (plist-put metadata 'repository "apt")))
   (split-string output "\n\n+" t)))

(defun arch-debian--list-all ()
  "Return packages in the configured apt indexes and installed packages."
  (let ((installed (map-into
                    (seq-map (lambda (pkg) (cons (arch-pkg-name pkg) pkg))
                             (arch-debian--list))
                    '(hash-table :test equal)))
        (available (make-hash-table :test #'equal)))
    (seq-do
     (lambda (metadata)
       (when-let* ((name (plist-get metadata 'name)))
         (setf (map-elt available name)
               (or (map-elt installed name)
                   (arch-pkg--make :name name :repo "apt"
                                   :version (plist-get metadata 'version)
                                   :description (plist-get metadata 'description))))))
     (arch-debian--parse-control (arch-debian--query "apt-cache" "dumpavail")))
    (map-do (lambda (name pkg) (setf (map-elt available name) pkg)) installed)
    (map-values available)))

(defun arch-debian--search (query)
  "Return Debian packages matching apt-cache search QUERY."
  (let ((installed (map-into (seq-map (lambda (pkg) (cons (arch-pkg-name pkg) pkg))
                                      (arch-debian--list))
                             '(hash-table :test equal))))
    (thread-last (split-string (arch-debian--query "apt-cache" "search" "--" query) "\n" t)
                 (seq-filter (lambda (line) (string-match-p " - " line)))
                 (seq-map (lambda (line)
                            (string-match "\\`\\([^ ]+\\) - \\(.*\\)\\'" line)
                            (or (map-elt installed (match-string 1 line))
                                (arch-pkg--make :name (match-string 1 line) :repo "apt"
                                                :description (match-string 2 line))))))))

(defun arch-debian--info (name)
  "Return Debian metadata for package NAME."
  (car (arch-debian--parse-control
        (arch-debian--query "apt-cache" "--no-all-versions" "show" "--" name))))

(defun arch-debian--files (name)
  "Return installed files for NAME, or nil if not installed."
  (thread-last (split-string
                (or (arch--run-sync (list "dpkg-query" "--listfiles" name) t) "") "\n" t)
               (seq-filter (lambda (file) (string-prefix-p "/" file)))))

(defun arch-debian--upgradeable ()
  "Return installed packages updated by a simulated full apt upgrade."
  (map-into
   (thread-last (split-string (arch-debian--query "apt-get" "--simulate" "dist-upgrade") "\n" t)
     (seq-filter (lambda (line) (string-match-p "\\`Inst [^ ]+ \\[" line)))
     (seq-map (lambda (line) (cadr (split-string line " " t))))
     (arch-debian--normalize-names (arch-debian--list))
     (seq-map (lambda (name) (cons name t))))
   '(hash-table :test equal)))

(defun arch-debian--normalize-names (packages names)
  "Match apt NAMES against dpkg PACKAGES, preserving native multiarch names."
  (when names
    (let ((native (string-trim (arch-debian--query "dpkg" "--print-architecture")))
          (installed (seq-map #'arch-pkg-name packages)))
      (seq-map (lambda (name)
                 (let ((qualified (concat name ":" native)))
                   (if (and (not (member name installed)) (member qualified installed))
                       qualified
                     name)))
               names))))

(defun arch-debian--populate-cache ()
  "Populate metadata and manual-install flags for installed Debian packages."
  (let* ((output (arch-debian--installed-output))
         (packages (arch-debian--parse-installed output))
         (manual (split-string (arch-debian--query "apt-mark" "showmanual") "\n" t))
         (orphans (thread-last (split-string (arch-debian--query "apt-get" "--simulate" "autoremove") "\n" t)
                      (seq-filter (lambda (line) (string-prefix-p "Remv " line)))
                      (seq-map (lambda (line) (cadr (split-string line " " t))))
                      (arch-debian--normalize-names packages)))
         (required (thread-last (split-string output "\n" t)
                                (seq-map (lambda (line) (nth 4 (split-string line "\t"))))
                                (seq-mapcat (lambda (deps) (split-string (or deps "") "[,|]" t "[ \t]+")))
                                (seq-map (lambda (dep) (car (split-string dep "[ (:]+" t)))))))
    (seq-do
     (lambda (pkg)
       (let ((name (arch-pkg-name pkg)))
         (arch--cache-put
          (arch-pkg-name pkg)
          (list 'name (arch-pkg-name pkg) 'version (arch-pkg-version pkg)
                'summary-only t
                'description (arch-pkg-description pkg) 'repository "apt"
                'orphan-p (and (member name orphans) t)
                'required-by (if (or (member name required)
                                     (member (car (split-string name ":")) required))
                                 "Debian package dependency" "None")
                'install-reason (if (or (member (arch-pkg-name pkg) manual)
                                        (member (car (split-string (arch-pkg-name pkg) ":")) manual))
                                    "Explicitly installed" "Installed as a dependency")))))
     packages)))

(defun arch-debian--command (&rest args)
  "Build a noninteractive apt-get command with ARGS."
  (append '("sudo" "env" "DEBIAN_FRONTEND=noninteractive" "LC_ALL=C"
            "apt-get" "--yes" "-o" "Dpkg::Use-Pty=0") args))

(defun arch-debian--install (name)
  "Install Debian package NAME."
  (arch--worker-run (arch-debian--command "install" "--" name) 'install name))

(defun arch-debian--install-batch (names)
  "Install Debian packages NAMES together."
  (arch--worker-run (apply #'arch-debian--command "install" "--" names) 'install names))

(defun arch-debian--remove (name)
  "Remove Debian package NAME."
  (arch--worker-run (arch-debian--command "remove" "--" name) 'remove name))

(defun arch-debian--upgrade (name)
  "Upgrade installed Debian package NAME."
  (arch--worker-run (arch-debian--command "--only-upgrade" "install" "--" name) 'upgrade name))

(defun arch-debian--sync (&optional _force)
  "Refresh apt indexes; ignore _FORCE since apt always refreshes them."
  (arch--worker-run (arch-debian--command "update")))

(defun arch-debian--upgrade-all ()
  "Refresh apt indexes, then perform a full distribution upgrade."
  (arch--worker-run
   (list "sh" "-c"
         (concat (mapconcat #'shell-quote-argument (arch-debian--command "update") " ")
                 " && "
                 (mapconcat #'shell-quote-argument (arch-debian--command "dist-upgrade") " ")))
   'upgrade-all))

(arch-register-backend
 (arch-backend--make
  :name "apt" :label "Debian (apt)"
  :search-fn #'arch-debian--search :info-fn #'arch-debian--info
  :files-fn #'arch-debian--files :list-fn #'arch-debian--list
  :list-all-fn #'arch-debian--list-all
  :foreign-fn (lambda () (make-hash-table :test #'equal))
  :upgradeable-fn #'arch-debian--upgradeable
  :populate-cache-fn #'arch-debian--populate-cache
  :install-methods '((direct . arch-debian--install))
  :default-install-method 'direct
  :install-batch-fn #'arch-debian--install-batch
  :remove-fn #'arch-debian--remove :upgrade-fn #'arch-debian--upgrade
  :upgrade-all-fn #'arch-debian--upgrade-all :sync-fn #'arch-debian--sync))

(provide 'arch-debian)
;;; arch-debian.el ends here
