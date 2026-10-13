;;; gptel-pup.el --- gptel backend for DataDog Pup / Bits AI -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.0"))
;; Keywords: tools, llm, gptel
;; URL: https://github.com/tychoish/gptel-pup

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Wraps DataDog's Pup CLI (https://github.com/DataDog/pup) so its
;; `pup acp serve' command can be driven from gptel.
;;
;; `pup acp serve' starts a local HTTP server that proxies to Datadog Bits
;; AI, exposing both the Agent Communication Protocol
;; (https://agentcommunicationprotocol.dev/, unrelated to Zed's Agent
;; Client Protocol despite the shared acronym) and an OpenAI-compatible
;; `/chat/completions' endpoint.  gptel-pup manages the `pup' subprocess
;; and registers a `gptel-backend' against the OpenAI-compatible endpoint,
;; so Bits AI shows up as an ordinary gptel backend/model.
;;
;; Usage:
;;
;;   (require 'gptel-pup)
;;   (gptel-pup-start)      ; launches `pup acp serve' and waits for it
;;   (gptel-pup-start-chat) ; opens a gptel buffer using the Pup backend
;;
;; Authentication is handled entirely by Pup itself (`pup auth login');
;; gptel-pup never sees or stores Datadog credentials.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'json)
(require 'gptel)
(require 'gptel-openai)

(defgroup gptel-pup nil
  "gptel integration for DataDog's Pup CLI."
  :group 'gptel)

(defcustom gptel-pup-executable "pup"
  "Name or path of the `pup' executable.

If this is a bare command name (no directory component) and it cannot
be found on `exec-path', `gptel-pup-ensure-executable' looks for it
under `gptel-pup-install-directory' before giving up."
  :type 'string
  :group 'gptel-pup)

(defcustom gptel-pup-install-directory
  (expand-file-name "gptel-pup" user-emacs-directory)
  "Directory `gptel-pup-install' downloads the `pup' binary into."
  :type 'directory
  :group 'gptel-pup)

(defcustom gptel-pup-host "127.0.0.1"
  "Host `pup acp serve' binds to."
  :type 'string
  :group 'gptel-pup)

(defcustom gptel-pup-port 9099
  "Port `pup acp serve' binds to."
  :type 'integer
  :group 'gptel-pup)

(defcustom gptel-pup-agent-id nil
  "Datadog Bits AI agent ID to target.

When nil, `pup acp serve' auto-discovers the first available agent."
  :type '(choice (const nil) string)
  :group 'gptel-pup)

(defcustom gptel-pup-site nil
  "Datadog site to pass as DD_SITE to the `pup' process, e.g. \"datadoghq.eu\".

When nil, `pup' uses its own default/stored session site."
  :type '(choice (const nil) string)
  :group 'gptel-pup)

(defcustom gptel-pup-org nil
  "Named Datadog org/session to pass as DD_ORG to the `pup' process.

See \"Multiple sites and orgs\" in the Pup CLI documentation."
  :type '(choice (const nil) string)
  :group 'gptel-pup)

(defcustom gptel-pup-extra-serve-args nil
  "Extra command-line arguments appended to \"pup acp serve\"."
  :type '(repeat string)
  :group 'gptel-pup)

(defcustom gptel-pup-startup-timeout 15
  "Seconds to wait for `pup acp serve' to come up before giving up."
  :type 'number
  :group 'gptel-pup)

(defcustom gptel-pup-models '(datadog-ai)
  "Model names exposed by the Pup OpenAI-compatible backend.

Pup's `/chat/completions' endpoint proxies to a single Bits AI agent
regardless of the requested model name, so this mostly exists to give
the model a label in gptel's UI."
  :type '(repeat symbol)
  :group 'gptel-pup)

(defvar gptel-pup--process nil
  "The running \"pup acp serve\" process, or nil.")

(defvar gptel-pup-backend nil
  "The `gptel-backend' registered for Pup, once started.")

(defconst gptel-pup--github-repo "DataDog/pup"
  "GitHub repository Pup release binaries are downloaded from.")

;;; Executable discovery & installation

(defun gptel-pup--install-path ()
  "Return the path `gptel-pup-install' places the `pup' binary at."
  (expand-file-name "pup" gptel-pup-install-directory))

(defun gptel-pup-ensure-executable (&optional interactive)
  "Return a path to the `pup' executable, or signal an error.

Checks `gptel-pup-executable' on `exec-path' first, then falls back
to a previous `gptel-pup-install' download.  When INTERACTIVE is
non-nil and neither is found, offer to run `gptel-pup-install'."
  (or (executable-find gptel-pup-executable)
      (let ((installed (gptel-pup--install-path)))
        (when (file-executable-p installed)
          installed))
      (if (and interactive
               (y-or-n-p (format "`%s' not found; download it now? "
                                  gptel-pup-executable)))
          (gptel-pup-install)
        (user-error "Pup executable not found; see `gptel-pup-install'"))))

(defun gptel-pup--platform-asset-name (version)
  "Return the release asset filename for VERSION on the current platform."
  (let ((os (pcase system-type
              ('darwin "Darwin")
              ((or 'gnu/linux 'gnu 'gnu/kfreebsd) "Linux")
              (_ (user-error "Unsupported platform: %s" system-type))))
        (arch (if (string-match-p "\\`\\(aarch64\\|arm64\\)" system-configuration)
                  "arm64"
                "x86_64")))
    (format "pup_%s_%s_%s.tar.gz" version os arch)))

(defun gptel-pup--latest-release ()
  "Return (VERSION . ASSET-URL) for the latest Pup GitHub release."
  (let* ((url (format "https://api.github.com/repos/%s/releases/latest"
                      gptel-pup--github-repo))
         (json-object-type 'plist)
         (release (with-current-buffer (url-retrieve-synchronously url t t 30)
                    (goto-char (point-min))
                    (re-search-forward "^$")
                    (prog1 (json-read)
                      (kill-buffer))))
         (tag (plist-get release :tag_name))
         (version (string-remove-prefix "v" tag))
         (asset-name (gptel-pup--platform-asset-name version))
         (assets (append (plist-get release :assets) nil))
         (asset (cl-find-if (lambda (a) (equal (plist-get a :name) asset-name))
                            assets)))
    (unless asset
      (user-error "No Pup release asset named `%s' in latest release" asset-name))
    (cons version (plist-get asset :browser_download_url))))

;;;###autoload
(defun gptel-pup-install (&optional force)
  "Download the latest `pup' release binary into `gptel-pup-install-directory'.

Skips the download when an executable already exists there, unless
FORCE is non-nil.  Returns the path to the installed binary."
  (interactive "P")
  (let ((target (gptel-pup--install-path)))
    (if (and (file-executable-p target) (not force))
        target
      (make-directory gptel-pup-install-directory t)
      (pcase-let* ((`(,version . ,asset-url) (gptel-pup--latest-release))
                   (archive (expand-file-name (format "pup-%s.tar.gz" version)
                                               (temporary-file-directory))))
        (url-copy-file asset-url archive t)
        (unless (zerop (call-process "tar" nil nil nil "xzf" archive
                                      "-C" gptel-pup-install-directory "pup"))
          (error "Failed to extract `pup' from %s" archive))
        (delete-file archive)
        (set-file-modes target #o755)
        (message "Installed pup %s to %s" version target)
        target))))

;;; Process management

(defun gptel-pup-running-p ()
  "Return non-nil when a `pup acp serve' process is alive."
  (and gptel-pup--process (process-live-p gptel-pup--process)))

(defun gptel-pup--base-url ()
  "Return the base URL of the local Pup ACP server."
  (format "http://%s:%d" gptel-pup-host gptel-pup-port))

(defun gptel-pup--serve-args ()
  "Return the argument list for \"pup acp serve\"."
  (append (list "acp" "serve"
                "--host" gptel-pup-host
                "--port" (number-to-string gptel-pup-port))
          (when gptel-pup-agent-id (list "--agent-id" gptel-pup-agent-id))
          gptel-pup-extra-serve-args))

(defun gptel-pup--environment ()
  "Return the process environment for the `pup' subprocess."
  (append (when gptel-pup-site (list (format "DD_SITE=%s" gptel-pup-site)))
          (when gptel-pup-org (list (format "DD_ORG=%s" gptel-pup-org)))
          process-environment))

(defun gptel-pup--wait-for-server ()
  "Block until the Pup server answers or `gptel-pup-startup-timeout' elapses."
  (let ((deadline (+ (float-time) gptel-pup-startup-timeout))
        (url (concat (gptel-pup--base-url) "/agent.json")))
    (while (and (gptel-pup-running-p)
                (< (float-time) deadline)
                (not (ignore-errors
                       (let ((buf (url-retrieve-synchronously url t t 2)))
                         (prog1 buf (when buf (kill-buffer buf)))))))
      (sit-for 0.3))
    (unless (and (gptel-pup-running-p) (< (float-time) deadline))
      (user-error "Timed out waiting for `pup acp serve' to start; see %s"
                  (process-buffer gptel-pup--process)))))

(defun gptel-pup--sentinel (process event)
  "Log unexpected exits of PROCESS via EVENT."
  (unless (process-live-p process)
    (message "gptel-pup: `pup acp serve' exited: %s" (string-trim event))))

;;;###autoload
(defun gptel-pup-start (&optional force)
  "Start `pup acp serve' and register its gptel backend.

Reuses an already-running server unless FORCE is non-nil.  Returns
the registered `gptel-backend'."
  (interactive "P")
  (when (and force (gptel-pup-running-p))
    (gptel-pup-stop))
  (unless (gptel-pup-running-p)
    (let ((executable (gptel-pup-ensure-executable t)))
      (setq gptel-pup--process
            (let ((process-environment (gptel-pup--environment)))
              (make-process
               :name "gptel-pup"
               :buffer (generate-new-buffer " *gptel-pup*")
               :command (cons executable (gptel-pup--serve-args))
               :noquery t
               :sentinel #'gptel-pup--sentinel)))
      (gptel-pup--wait-for-server)))
  (gptel-pup-register-backend))

;;;###autoload
(defun gptel-pup-stop ()
  "Stop the running `pup acp serve' process, if any."
  (interactive)
  (when (gptel-pup-running-p)
    (delete-process gptel-pup--process))
  (setq gptel-pup--process nil))

;;; gptel backend

;;;###autoload
(defun gptel-pup-register-backend ()
  "Register (or refresh) the gptel backend for the local Pup server.

Uses Pup's OpenAI-compatible `/chat/completions' endpoint.  No API
key is sent: authentication is handled out-of-band by `pup auth
login', and the server only listens on `gptel-pup-host'."
  (setq gptel-pup-backend
        (gptel-make-openai "pup"
          :host (format "%s:%d" gptel-pup-host gptel-pup-port)
          :protocol "http"
          :endpoint "/chat/completions"
          :header (lambda (_info) nil)
          :models gptel-pup-models
          :stream t)))

;;;###autoload
(defun gptel-pup-start-chat ()
  "Start (or reuse) the Pup server and open a gptel chat using it."
  (interactive)
  (gptel-pup-start)
  (with-current-buffer (gptel "*pup*")
    (setq-local gptel-backend gptel-pup-backend
                gptel-model (car gptel-pup-models))
    (current-buffer)))

(provide 'gptel-pup)

;;; gptel-pup.el ends here
