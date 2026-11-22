;;; builder-elpaish.el --- MCP tools for ELPAish, hosted by builder -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0"))
;; Keywords: tools, package, elpa, mcp

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Registers the `elpaish' MCP service over `elpaish-ops':
;;
;; - `elpaish_status': per-recipe commit delta against the last build.
;; - `elpaish_rebuild': rebuild archives (one, all, or only those behind).
;; - `elpaish_upgrade': upgrade installed packages (one, several or all).
;; - `elpaish_workflow_dispatch': dispatch the repository's GitHub workflow.
;; - `elpaish_recipe': format an `elpaish-register-package' form.
;;
;; ELPAish is an optional dependency: nothing here loads it until a tool is
;; called, and the tools register only when `elpaish-ops' is installed.
;; `builder-mcp' requires this file, so it loads with the rest of the
;; builder service.  Building, linting and testing a package are `builder'
;; tools (`elisp_preflight', `builder_run_tests', `builder_package_build').

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mcpkit)

(declare-function elpaish-ops-status "elpaish-ops")
(declare-function elpaish-ops-rebuild "elpaish-ops")
(declare-function elpaish-ops-upgrade "elpaish-ops")
(declare-function elpaish-ops-workflow-run "elpaish-ops")
(declare-function elpaish-ops-recipe-snippet "elpaish-ops")

(defun builder-elpaish--require-ops ()
  "Load `elpaish-ops', signalling a clear error when ELPAish is missing."
  (unless (require 'elpaish-ops nil t)
    (user-error "ELPAish (elpaish-ops) is not installed")))

(defun builder-elpaish--list (value)
  "Return JSON array VALUE (vector or list) as a list, or nil."
  (cond ((vectorp value) (append value nil))
        ((listp value) value)))

(defun builder-elpaish--true (args key)
  "Return non-nil when ARGS has boolean KEY set to JSON true."
  (eq (plist-get args key) t))

(defcustom builder-elpaish-port 8768
  "Default TCP port for the elpaish MCP service."
  :type 'integer
  :group 'builder)

(defvar builder-elpaish-service nil
  "The `elpaish' `mcpkit-service', or nil when ELPAish is not installed.")

(when (locate-library "elpaish-ops")
  (setq builder-elpaish-service
        (or (mcpkit-get-service 'elpaish)
            (mcpkit-define-service 'elpaish
              :port builder-elpaish-port
              :description "ELPAish status, rebuild and package upgrade tools")))

  (mcpkit-register-tool 'elpaish_status 'elpaish
    :description "Report, per registered ELPAish recipe, how many commits its git HEAD is ahead of the last build (delta, behind). Local only: never clones or fetches, so a recipe without a checkout is reported as uncloned. Use behind_only to list just the packages needing a rebuild."
    :input-schema '(:type "object"
                    :properties (:packages (:type "array" :items (:type "string")
                                            :description "Limit the report to these package names")
                                 :behind_only (:type "boolean"
                                               :description "Return only packages behind git HEAD"))
                    :required [])
    (builder-elpaish--require-ops)
    (elpaish-ops-status :packages (builder-elpaish--list (plist-get args :packages))
                        :behind-only (builder-elpaish--true args :behind_only)))

  (mcpkit-register-tool 'elpaish_rebuild 'elpaish
    :description "Rebuild ELPAish package archives locally (archive-contents, checksums, signatures). Rebuilds one package, only the packages behind git HEAD, or everything when neither package nor behind_only is given."
    :input-schema '(:type "object"
                    :properties (:package (:type "string" :description "Package to rebuild; omit or \"all\" for every package")
                                 :stream (:type "string" :enum ["snapshot" "stable" "staging" "all"]
                                          :description "Release stream (default snapshot)")
                                 :behind_only (:type "boolean"
                                               :description "Rebuild only packages that have commits ahead of the archive"))
                    :required [])
    (builder-elpaish--require-ops)
    (elpaish-ops-rebuild :package (plist-get args :package)
                         :stream (intern (or (plist-get args :stream) "snapshot"))
                         :behind-only (builder-elpaish--true args :behind_only)))

  (mcpkit-register-tool 'elpaish_upgrade 'elpaish
    :description "Upgrade installed Emacs packages. Name them in packages, or set all to upgrade everything with a newer archive version; archives are refreshed at most once and only packages with newer versions are touched. reinstall forces a reinstall of the named packages. async hands the work to a background pool and returns immediately."
    :input-schema '(:type "object"
                    :properties (:packages (:type "array" :items (:type "string")
                                            :description "Packages to upgrade")
                                 :all (:type "boolean" :description "Upgrade every upgradable package (default when packages is omitted)")
                                 :refresh (:type "boolean" :description "Refresh package archives first")
                                 :reinstall (:type "boolean" :description "Reinstall the named packages even if current")
                                 :async (:type "boolean" :description "Run in a background worker pool and return immediately")
                                 :pool_size (:type "integer" :description "Worker pool size for async (default 4)"))
                    :required [])
    (builder-elpaish--require-ops)
    (let ((packages (builder-elpaish--list (plist-get args :packages))))
      (elpaish-ops-upgrade :packages packages
                           :all (or (builder-elpaish--true args :all) (null packages))
                           :refresh (builder-elpaish--true args :refresh)
                           :reinstall (builder-elpaish--true args :reinstall)
                           :async (builder-elpaish--true args :async)
                           :pool-size (plist-get args :pool_size))))

  (mcpkit-register-tool 'elpaish_workflow_dispatch 'elpaish
    :description "Dispatch a GitHub Actions workflow of the ELPAish repository (default pages.yml on elpaish-github-repo-slug), via magit-dash or the gh CLI."
    :input-schema '(:type "object"
                    :properties (:workflow (:type "string" :description "Workflow filename or name (default pages.yml)")
                                 :repo (:type "string" :description "owner/repo slug (default the ELPAish repository)")
                                 :ref (:type "string" :description "Git ref (default main)"))
                    :required [])
    (builder-elpaish--require-ops)
    (elpaish-ops-workflow-run (plist-get args :workflow)
                              (plist-get args :repo)
                              (plist-get args :ref)))

  (mcpkit-register-tool 'elpaish_recipe 'elpaish
    :description "Format the elpaish-register-package form that registers a package, for pasting into packages.el."
    :input-schema '(:type "object"
                    :properties (:package (:type "string" :description "Package name")
                                 :summary (:type "string" :description "One-line description")
                                 :requires (:type "array" :items (:type "string")
                                            :description "Package dependencies")
                                 :keywords (:type "array" :items (:type "string")
                                            :description "Package keywords")
                                 :url (:type "string" :description "Repository URL (default GitHub owner/package)")
                                 :owner (:type "string" :description "GitHub owner for the default URL"))
                    :required ["package"])
    (builder-elpaish--require-ops)
    (let ((pkg (plist-get args :package)))
      (unless (and (stringp pkg) (not (string-empty-p pkg)))
        (user-error "Package is required"))
      (list :snippet (elpaish-ops-recipe-snippet
                      pkg
                      :summary (plist-get args :summary)
                      :requires (mapcar #'intern (builder-elpaish--list (plist-get args :requires)))
                      :keywords (builder-elpaish--list (plist-get args :keywords))
                      :url (plist-get args :url)
                      :owner (plist-get args :owner))))))

;;;###autoload
(defun builder-elpaish-start (&optional port)
  "Start or merge the `elpaish' MCP service on PORT.
When PORT is nil and a shared server is already active, attach to it;
otherwise fall back to `builder-elpaish-port'."
  (interactive "P")
  (unless builder-elpaish-service
    (user-error "ELPAish (elpaish-ops) is not installed"))
  (if (and (null port) (fboundp 'mcpkit-active-port) (mcpkit-active-port))
      (mcpkit-start-service 'elpaish)
    (mcpkit-start-service 'elpaish :port (or port builder-elpaish-port))))

(provide 'builder-elpaish)
;;; builder-elpaish.el ends here
