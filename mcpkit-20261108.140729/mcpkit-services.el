;;; mcpkit-services.el --- Port discovery and service startup for mcpkit -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0") (gen "0.1.0"))
;; Keywords: comm, tools, mcp
;; URL: https://github.com/tychoish/mcpkit.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Stable per-instance port selection and service initialization on top of
;; mcpkit.el.  The port is derived from the instance name so it survives
;; restarts, and is probed upward when another process already holds it.
;; `mcpkit-init-services' loads `mcpkit-service-modules' (asynchronously
;; unless asked otherwise) and starts every registered service.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'mcpkit)
(require 'gen)

(defcustom mcpkit-instance-name-function #'system-name
  "Function of no arguments returning this instance's name, or nil.
The name seeds `mcpkit-instance-port'; a nil result falls back to \"solo\"."
  :type 'function
  :group 'mcpkit)

(defcustom mcpkit-instance-port-base 20000
  "Lowest port `mcpkit-instance-port' derives from an instance name."
  :type 'integer
  :group 'mcpkit)

(defcustom mcpkit-instance-port-range 10000
  "Number of ports above `mcpkit-instance-port-base' names hash into."
  :type 'integer
  :group 'mcpkit)

;; `mcpkit-default-port' defaults to a fixed integer, so it cannot signal
;; "derive one"; this option does.
(defcustom mcpkit-services-port 'instance
  "Port `mcpkit-init-services' starts services on.
An integer is used as is; `instance' derives a stable port from the instance
name with `mcpkit-instance-port'."
  :type '(choice (const :tag "Derive from instance name" instance)
                 (integer :tag "Port number"))
  :group 'mcpkit)

(defun mcpkit-port-available-p (port)
  "Return non-nil if local TCP PORT is available to bind."
  (condition-case nil
      (progn
        (delete-process
         (make-network-process :name " mcpkit-port-probe"
                               :server t
                               :host 'local
                               :service port
                               :noquery t
                               :family 'ipv4))
        t)
    (file-error nil)))

(defun mcpkit-find-available-port (base-port &optional max-tries)
  "Return the first available port from BASE-PORT, probing MAX-TRIES ports.
MAX-TRIES defaults to 50.  If none is free, ask the OS for a free port."
  (let ((port base-port)
        (tries (or max-tries 50)))
    (while (and (> tries 0) (not (mcpkit-port-available-p port)))
      (setq port (1+ port))
      (setq tries (1- tries)))
    (if (> tries 0)
        port
      (mcpkit-allocate-free-port))))

(defun mcpkit-instance-port ()
  "Return a TCP port derived from the instance name, stable across restarts.
The preferred port hashes `mcpkit-instance-name-function''s result into
`mcpkit-instance-port-range' ports above `mcpkit-instance-port-base'.  If it
is already bound, sequential probing assigns distinct, deterministic ports so
concurrent instances never collide."
  (let ((name (or (funcall mcpkit-instance-name-function) "solo")))
    (mcpkit-find-available-port
     (+ mcpkit-instance-port-base
        (mod (abs (sxhash-equal name)) mcpkit-instance-port-range))
     50)))

(defun mcpkit--require-module (module)
  "Require MODULE, logging instead of signaling when it cannot be loaded."
  (condition-case err
      (require module)
    (error (message "[mcpkit]: skipping %s: %s" module (error-message-string err)))))

(defun mcpkit--register-emacs-editing-tools ()
  "Register editing tools on the `emacs' service when both are available."
  (when (and (fboundp 'mcpkit-editing-register-tools)
             (mcpkit-get-service 'emacs))
    (mcpkit-editing-register-tools 'emacs)))

(cl-defun mcpkit-load-services-async (&key on-done (delay 0.01))
  "Require each of `mcpkit-service-modules' in turn, yielding DELAY between them.
Register editing tools afterwards, then call ON-DONE.  Return the `gen'
runner handle, which cancels pending loads when called."
  (gen-run-async
   (lambda (module done-cb)
     (mcpkit--require-module module)
     (funcall done-cb))
   mcpkit-service-modules
   :concurrency 1
   :delay delay
   :on-done (lambda ()
              (mcpkit--register-emacs-editing-tools)
              (when on-done
                (funcall on-done)))
   :on-error (lambda (err)
               (message "[mcpkit]: load error: %S" err))))

;;;###autoload
(defun mcpkit-start-services-on-instance-port ()
  "Start all services on the running port, `mcpkit-services-port',
or a derived one."
  (mcpkit-start-all-services
   (or (mcpkit-active-port)
       (and (integerp mcpkit-services-port) mcpkit-services-port)
       (mcpkit-instance-port))))

;;;###autoload
(defun mcpkit-init-services-sync ()
  "Load `mcpkit-service-modules' and start all services now, unless running."
  (interactive)
  (unless (mcpkit-active-port)
    (seq-do #'mcpkit--require-module mcpkit-service-modules)
    (mcpkit--register-emacs-editing-tools)
    (mcpkit-start-services-on-instance-port)))

;;;###autoload
(defun mcpkit-init-services ()
  "Load `mcpkit-service-modules' and start all services, unless running.
Load asynchronously and start afterwards, or do everything synchronously when
Emacs is `noninteractive'.  See `mcpkit-init-services-sync'."
  (interactive)
  (cond
   ((mcpkit-active-port) nil)
   (noninteractive (mcpkit-init-services-sync))
   (t (mcpkit-load-services-async
       :on-done #'mcpkit-start-services-on-instance-port))))

(provide 'mcpkit-services)
;;; mcpkit-services.el ends here
