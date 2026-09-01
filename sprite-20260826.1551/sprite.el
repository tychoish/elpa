;;; sprite.el --- Orchestrate daemon instances and async programming model -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (seq "2.24"))
;; URL: https://github.com/tychoish/sprite
;; Keywords: tools, daemon, processes

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; sprite.el manages "sprite" Emacs daemons that are children of a
;; parent Emacs instance.  Each sprite runs as a named emacs --daemon
;; subprocess and is addressable via emacsclient.
;;
;; Sprite names follow the pattern "<parent>.<idx>.<unique-name>", where
;; idx is the lowest integer not already in use for that parent.
;;
;; Entry points:
;;   `sprite-create'            — spawn a new sprite daemon
;;   `sprite-open-frame'        — open a frame connected to a sprite
;;   `with-sprite'               — evaluate a form in a sprite
;;   `sprite-get-next'          — get the next available sprite
;;   `sprite-get-or-create-next' — get or create a sprite
;;
;; `sprite-list' (the tabulated overview buffer) lives in sprite-list.el,
;; loaded on demand, since it depends on tabulated-list and transient.
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'sprite-direct)

(declare-function annotated-completing-read "annotated-completing-read")

;; server.el variables; server.el is loaded by any running daemon but may be
;; absent in batch/test contexts.
(defvar server-use-tcp)
(defvar server-auth-dir)

;;;; Instance identity and state paths

(defconst sprite--conf-state-directory "state"
  "Name of the state subdirectory under `user-emacs-directory'.")

(defconst sprite--state-subdir "sprite"
  "Subdirectory name under the state path where sprite state lives.")

(defvar sprite-instance-id nil
  "Name of the running Emacs instance.
Set by `sprite-set-up-instance-name' at startup.")

(defvar sprite--system-name-cached nil
  "Cached value of the output of `funcion:system-name' for use in system management contexts.")

(defvar sprite-cli-instance-id nil
  "CLI-specified instance name; set from --id command-line arguments.")

(defun sprite-resolve-instance-id ()
  "Return the current Emacs instance ID.
Resolution order: daemon name, `sprite-cli-instance-id',
`sprite-instance-id', then \"solo\"."
  (let ((daemon (daemonp)))
    (or (when (eq daemon t) "primary")
        (when (stringp daemon)
	  (if server-use-tcp
	      (setenv "EMACS_SERVER_FILE" daemon)
	    (setenv "EMACS_SOCKET_FILE" (expand-file-name daemon server-socket-dir)))
          daemon)
        sprite-cli-instance-id
        sprite-instance-id
        "solo")))

(defun sprite-instance-name ()
  "Return the current Emacs instance name, initialising it if needed.
Caches the result in `sprite-instance-id'.  This is the preferred public
accessor; call `sprite-resolve-instance-id' only when the raw resolution
chain must be re-evaluated."
  (unless sprite-instance-id
    (setq sprite-instance-id (sprite-resolve-instance-id)))
  sprite-instance-id)

(defun sprite-system-name ()
  (or sprite--system-name-cached (setq sprite--system-name-cached (system-name))))

(defun sprite-conf-host-and-instance ()
  "Return (HOSTNAME INSTANCE-ID) for state-path construction."
  (list (if (eq system-type 'darwin)
            (car (string-split (sprite-system-name) "\\."))
          (sprite-system-name))
        (sprite-instance-name)))

(defun sprite-state-file-prefix (name)
  "Return the instance-scoped filename prefix for NAME.
Produces HOSTNAME-INSTANCE-NAME[-USERNAME] where USERNAME is included
only when running as root or under a symlinked `user-emacs-directory'."
  (let* ((host-and-instance (sprite-conf-host-and-instance))
         (host (nth 0 host-and-instance))
         (instance (nth 1 host-and-instance)))
    (string-join
     (seq-filter #'identity
                 (list host instance name
                       (when (or (equal "root" user-login-name)
                                 (file-symlink-p user-emacs-directory))
                         user-login-name)))
     "-")))

;;;autoload
(defun sprite-state-path (name)
  "Return the full state-directory path for NAME, scoped to this host and instance."
  (file-name-concat
   user-emacs-directory
   sprite--conf-state-directory
   (sprite-state-file-prefix name)))

;;;; Identity

(defun sprite-cli-resolve-id ()
  "Parse --id=NAME or --id NAME from `command-line-functions' context.
Sets `sprite-cli-instance-id' when found."
  (cond
   ((string-equal "--id" argi)
    (setq sprite-cli-instance-id (pop argv)))
   ((and (> (length argi) 5)
         (or (string-prefix-p "--id=" argi)
             (string-prefix-p "--id " argi)))
    (setq sprite-cli-instance-id (substring argi 5)))))

(add-to-list 'command-line-functions #'sprite-cli-resolve-id)

(defun sprite--format-full-name (parent idx unique-name)
  "Format PARENT, IDX, and UNIQUE-NAME into a sprite full name."
  (format "%s.%d.%s" parent idx unique-name))

(defun sprite--parse-full-name (full-name)
  "Parse FULL-NAME into (PARENT IDX UNIQUE-NAME) or nil if malformed.
All three segments must be non-empty and contain no dots."
  (when-let* ((parts (string-split full-name "\\."))
              ((= (length parts) 3))
              ((seq-every-p (lambda (p) (not (string-empty-p p))) parts))
              ((string-match-p "^[0-9]+$" (nth 1 parts))))
    (list (nth 0 parts)
          (string-to-number (nth 1 parts))
          (nth 2 parts))))

(defun sprite--full-name-p (name)
  "Return t if NAME matches the sprite full-name pattern <parent>.<idx>.<unique>."
  (and (stringp name)
       (string-match-p "^[^.]+\\.[0-9]+\\.[^.]+$" name)
       t))

(defun sprite--parent-letter (parent-id)
  "Return the first character of PARENT-ID as a single-char string."
  (substring parent-id 0 1))

(defun sprite--mode-line-id (full-name)
  "Return a mode-line display string for FULL-NAME.
For sprite names, abbreviates the parent to its first letter.
For top-level names, returns FULL-NAME unchanged."
  (if-let* ((parts (sprite--parse-full-name full-name)))
    (format "%s.%d.%s"
            (sprite--parent-letter (car parts))
            (cadr parts)
            (caddr parts))
    full-name))

(defun sprite--mode-line-string ()
  "Return mode-line display string for the current Emacs instance.
Always returns a non-nil string: abbreviated for sprite instances,
the raw instance id for top-level instances."
  (sprite--mode-line-id (sprite-instance-name)))

(cl-defun sprite-state-directory (&key full-name)
  "Return the state directory for sprite instance FULL-NAME, or the sprite root if nil."
  (let ((base (file-name-as-directory (sprite-state-path sprite--state-subdir))))
    (if full-name
        (file-name-concat base full-name)
      base)))

(cl-defun sprite-conf-state-path (name &key full-name)
  "Return path for NAME within FULL-NAME's state directory.
If FULL-NAME is nil, returns path under the sprite root."
  (file-name-concat (sprite-state-directory :full-name full-name) name))
(defun sprite--live-p (s)
  "Return non-nil when S should appear in the overview list.
Provisional (unspawned) definitions are always live.
Full entries are live when not decommissioned."
  (or (sprite--provisional-p s)
      (not (sprite--decommissioned-p (sprite-name s)))))

;;;; Struct and registry

(cl-defstruct (sprite (:constructor sprite--make) (:copier nil))
  "Registry entry for a managed sprite daemon."
  name
  idx
  parent
  unique-name
  pid
  state-dir
  spawned-by
  start-time
  last-contact
  running-status
  startup)  ; nil | 'sync | 'idle — non-nil marks a declared definition

(defvar sprite--registry (make-hash-table :test #'equal)
  "In-memory registry mapping full-name strings to `sprite' structs.")

(defun sprite--registry-get (name)
  "Return the sprite struct for NAME, or nil."
  (map-elt sprite--registry name))

(defun sprite--registry-put (s)
  "Store sprite struct S in the registry under its name."
  (setf (map-elt sprite--registry (sprite-name s)) s))

(defun sprite--registry-remove (name)
  "Remove the registry entry for NAME."
  (map-delete sprite--registry name))

(defun sprite--registry-all ()
  "Return a list of all sprite structs in the registry."
  (map-values sprite--registry))

(defvar sprite--registry-saved nil
  "Serialized registry for savehist persistence.
A list of plists; populated by `sprite--registry-serialize'.")

(defun sprite--struct-to-plist (s)
  "Convert sprite struct S to a plist for serialization."
  (list :name (sprite-name s)
        :idx (sprite-idx s)
        :parent (sprite-parent s)
        :unique-name (sprite-unique-name s)
        :pid (sprite-pid s)
        :state-dir (sprite-state-dir s)
        :spawned-by (sprite-spawned-by s)
        :start-time (sprite-start-time s)
        :last-contact (sprite-last-contact s)
        :startup (sprite-startup s)))

(defun sprite--plist-to-struct (plist)
  "Reconstruct a sprite struct from PLIST."
  (sprite--make
   :name (plist-get plist :name)
   :idx (plist-get plist :idx)
   :parent (plist-get plist :parent)
   :unique-name (plist-get plist :unique-name)
   :pid (plist-get plist :pid)
   :state-dir (plist-get plist :state-dir)
   :spawned-by (plist-get plist :spawned-by)
   :start-time (plist-get plist :start-time)
   :last-contact (plist-get plist :last-contact)
   :startup (plist-get plist :startup)))

(defun sprite--registry-serialize ()
  "Serialize the registry to `sprite--registry-saved' for savehist."
  (setq sprite--registry-saved
        (seq-map #'sprite--struct-to-plist (sprite--registry-all))))

(defun sprite--registry-deserialize ()
  "Restore the registry hash table from `sprite--registry-saved'."
  (setq sprite--registry (make-hash-table :test #'equal))
  (seq-do (lambda (plist)
            (sprite--registry-put (sprite--plist-to-struct plist)))
          (or sprite--registry-saved nil)))

(defun sprite--discover-state-dirs ()
  "Return full-name strings for all sprite state directories on disk."
  (when (file-directory-p (sprite-state-directory))
    (thread-last (directory-files (sprite-state-directory) nil nil t)
      (seq-filter #'sprite--full-name-p))))

(defun sprite--decommissioned-p (full-name)
  "Return t if the DECOMMISSIONED marker file exists for FULL-NAME."
  (file-exists-p
   (file-name-concat (sprite-state-directory :full-name full-name) "DECOMMISSIONED")))

(defun sprite--write-decommissioned-file (full-name)
  "Write the DECOMMISSIONED marker file for FULL-NAME, returning t on success."
  (condition-case _
      (progn
        (write-region "" nil
                      (file-name-concat (sprite-state-directory :full-name full-name)
                                        "DECOMMISSIONED"))
        t)
    (error nil)))

(defun sprite--allocated-indices (parent)
  "Return list of idx integers in use for PARENT, from disk state directories."
  (thread-last (sprite--discover-state-dirs)
    (seq-map #'sprite--parse-full-name)
    (seq-filter (lambda (parts)
                  (and parts
                       (equal parent (car parts))
                       (not (sprite--decommissioned-p
                              (sprite--format-full-name
                               (car parts) (cadr parts) (caddr parts)))))))
    (seq-map #'cadr)))

(defun sprite--next-idx (parent)
  "Return the lowest non-negative integer index not in use for PARENT."
  (let ((used (sprite--allocated-indices parent))
        (n 0))
    (while (member n used)
      (setq n (1+ n)))
    n))

(defun sprite--make-from-state-dir (full-name)
  "Create a minimal sprite struct from a discovered state directory name FULL-NAME."
  (if-let* ((parts (sprite--parse-full-name full-name)))
    (let ((parent (nth 0 parts))
          (idx (nth 1 parts))
          (unique-name (nth 2 parts)))
      (sprite--make :name full-name
                    :idx idx
                    :parent parent
                    :unique-name unique-name
                    :state-dir (sprite-state-directory :full-name full-name)))
    (error "Cannot parse sprite name: %s" full-name)))

(defun sprite--discover-and-sync-registry ()
  "Reconcile the registry with the filesystem.
Adds discovered sprite not in the registry.  Does not remove entries."
  (seq-do (lambda (name)
            (unless (sprite--registry-get name)
              (sprite--registry-put (sprite--make-from-state-dir name))))
          (sprite--discover-state-dirs)))

;;;; Definitions — declared sub-sprite instances

(defun sprite--provisional-p (s)
  "Return non-nil when S is a declared definition not yet spawned.
Provisional entries carry a `startup' value but have no numeric index."
  (and (sprite-startup s) (null (sprite-idx s))))

(cl-defun sprite-define (&key unique-name (parent (sprite-instance-name)) (startup 'idle))
  "Declare that a sprite named UNIQUE-NAME should exist under PARENT.
STARTUP controls creation when absent:
  `sync'  — create immediately when `sprite-defs-activate' runs.
  `idle'  — create on the next Emacs idle period after activation.
Definitions persist across sessions via savehist.  Returns the
provisional sprite struct."
  (unless unique-name
    (user-error "sprite-define: :unique-name is required"))
  (let* ((key (format "%s.%s" parent unique-name))
         (s (or (sprite--registry-get key)
                (sprite--make :name key :parent parent :unique-name unique-name))))
    (setf (sprite-startup s) startup)
    (sprite--registry-put s)
    s))

(defun sprite-defs-activate ()
  "Reconcile declared definitions with the running instance.
For each provisional entry whose :parent matches `sprite-instance-name':
  - If a real sprite with the same (parent, unique-name) is already in
    the registry, promote it (copy startup, remove provisional entry).
  - If absent and startup is `sync', create the sprite immediately.
  - If absent and startup is `idle', schedule via the session idle hook."
  (let ((self (sprite-instance-name)))
    (seq-do
     (lambda (s)
       (when (and (sprite--provisional-p s) (equal (sprite-parent s) self))
         (let ((live (seq-find (lambda (r)
                                 (and (sprite--full-name-p (sprite-name r))
                                      (equal (sprite-parent r) (sprite-parent s))
                                      (equal (sprite-unique-name r) (sprite-unique-name s))))
                               (sprite--registry-all))))
           (cond
            (live
             (setf (sprite-startup live) (sprite-startup s))
             (sprite--registry-remove (sprite-name s)))
            ((eq (sprite-startup s) 'sync)
             (let ((new-s (sprite-create (sprite-unique-name s))))
               (setf (sprite-startup new-s) (sprite-startup s))
               (sprite--registry-put new-s)
               (sprite--registry-remove (sprite-name s))))
            (t
             (sprite-session-add-on-idle #'sprite-defs-activate-idle-check))))))
     (sprite--registry-all))))

(defun sprite-defs-activate-idle-check ()
  "Idle hook: activate pending definitions; deregister when none remain."
  (sprite-defs-activate)
  (unless (seq-some #'sprite--provisional-p (sprite--registry-all))
    (sprite-session-remove-on-idle #'sprite-defs-activate-idle-check)))

;;;; Communication and eval

(defconst sprite--log-time-format "%H:%M:%S"
  "Format string for timestamps in sprite log buffers.")

(defun sprite--log-buffer-name (name)
  "Return the name of the log buffer for sprite NAME."
  (format "*sprite:%s*" name))

(defun sprite--log (name direction content)
  "Log a message to the sprite NAME log buffer.
DIRECTION is the symbol `sent' or `received'.  CONTENT is a string."
  (with-current-buffer (get-buffer-create (sprite--log-buffer-name name))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize (format "%s" direction) 'face 'bold))
      (insert (format " [%s]: %s\n"
                      (format-time-string sprite--log-time-format)
                      content)))))

(cl-defun sprite--call (name form &key buffer)
  "The single site where emacsclient is invoked for eval-based communication.
Evaluates FORM in sprite NAME; captures output in BUFFER when non-nil.
Returns the emacsclient exit code."
  (call-process "emacsclient" nil buffer nil
                "--socket-name" name "--eval" (format "%S" form)))

(defun sprite--call-and-read-emacsclient (name form)
  "Invoke emacsclient against NAME evaluating FORM; return the read result.
Returns the read Lisp value on success, nil if the call fails or output is
unreadable.  Process errors propagate to the caller."
  (with-temp-buffer
    (when (= 0 (sprite--call name form :buffer (current-buffer)))
      (condition-case _ (read (buffer-string))
        (error nil)))))
(defun sprite--call-and-read-direct (name form)
  "Evaluate FORM in sprite NAME using the direct-socket backend."
  (sprite-direct-call-and-read (sprite--direct-target name) form))

(defun sprite--call-and-read (name form)
  "Evaluate FORM in sprite NAME using `sprite-communication-backend'.
Falls back to the emacsclient backend once when the direct backend
fails and `sprite-communication-fallback' is non-nil."
  (pcase sprite-communication-backend
    ('direct
     (or (sprite--call-and-read-direct name form)
         (when sprite-communication-fallback
           (sprite--call-and-read-emacsclient name form))))
    (_ (sprite--call-and-read-emacsclient name form))))

(cl-defmacro with-sprite (name form &key no-log)
  "Evaluate FORM in sprite NAME via emacsclient.
Returns the read result, or nil if the connection fails.
Unless NO-LOG is non-nil, logs the exchange and updates last-contact."
  (declare (indent 1))
  (let ((gname (make-symbol "name"))
        (gresult (make-symbol "result")))
    `(let ((,gname ,name))
       ,(unless no-log `(sprite--log ,gname 'sent (format "%S" ',form)))
       (let ((,gresult
              (condition-case ,(if no-log '_ 'err)
                  (sprite--call-and-read ,gname ',form)
                (error
                 ,(unless no-log
                    `(sprite--log ,gname 'error (error-message-string err)))
                 nil))))
         ,(unless no-log
            `(when ,gresult
               (sprite--log ,gname 'received (format "%S" ,gresult))
               (when-let* ((s (sprite--registry-get ,gname)))
                 (setf (sprite-last-contact s) (current-time)))))
         ,gresult))))

(defun sprite-open-log (name)
  "Switch to the communication log buffer for sprite NAME."
  (interactive (list (completing-read "Sprite log: "
                                      (seq-map #'sprite-name (sprite--registry-all)))))
  (pop-to-buffer (sprite--log-buffer-name name)))

;;;; Lifecycle

(defun sprite--resolve-name (name)
  "Resolve NAME to a full sprite name.
NAME may be a full name (returned as-is) or a unique-name matched in the
registry.  Signals `user-error' if NAME cannot be resolved."
  (cond
   ((sprite--registry-get name) name)
   (t
    (if-let* ((found (seq-find (lambda (s)
                                 (equal name (sprite-unique-name s)))
                               (sprite--registry-all))))
      (sprite-name found)
      (user-error "No sprite found with name: %s" name)))))

(defun sprite--ensure-state-dir (full-name)
  "Create the state directory for FULL-NAME if it does not exist.
Returns the state directory path."
  (let ((dir (sprite-state-directory :full-name full-name)))
    (make-directory dir t)
    dir))

(defun sprite--spawn (full-name)
  "Low-level: start an Emacs daemon named FULL-NAME.
Returns the sprite struct with `start-time' set.  Updates the registry."
  (let* ((s (or (sprite--registry-get full-name)
                (sprite--make-from-state-dir full-name)))
         (log-buf (get-buffer-create (sprite--log-buffer-name full-name)))
         (proc (start-process "sprite-daemon" log-buf
                              (expand-file-name invocation-name invocation-directory)
                              (concat "--daemon=" full-name))))
    (setf (sprite-pid s) (process-id proc))
    (setf (sprite-start-time s) (current-time))
    (sprite--registry-put s)
    s))

(defun sprite--running-p (full-name)
  "Return t if the sprite FULL-NAME appears to be live.
Checks whether the log buffer's associated process is still alive with a
matching PID first; falls back to an emacsclient ping."
  (or (when-let* ((buf (get-buffer (sprite--log-buffer-name full-name)))
                  (proc (get-buffer-process buf))
                  ((process-live-p proc))
                  (s (sprite--registry-get full-name))
                  (pid (sprite-pid s))
                  ((= pid (process-id proc))))
        t)
      (not (null (with-sprite full-name t :no-log t)))))

(cl-defun sprite--wait-for-server (full-name &key timeout-secs)
  "Poll until sprite FULL-NAME's server is accepting connections.
Times out after TIMEOUT-SECS seconds (default 10).  Returns t on success."
  (let ((deadline (time-add (current-time) (or timeout-secs 10)))
        ready)
    (while (and (not ready) (time-less-p (current-time) deadline))
      (when (with-sprite full-name t :no-log t)
        (setq ready t))
      (unless ready
        (sleep-for 0.5)))
    ready))

;;;###autoload
(cl-defun sprite-create (unique-name &key timeout)
  "Spawn a new sprite daemon with UNIQUE-NAME under the current instance.
The current instance's ID becomes the parent.
Returns the new sprite struct.

Times out after TIMEOUT seconds."
  (interactive "sSprite unique name: ")
  (let* ((parent (sprite-instance-name))
         (idx (sprite--next-idx parent))
         (full-name (sprite--format-full-name parent idx unique-name))
         (wait-timeout (or timeout sprite-startup-timeout)))
    (when (sprite--decommissioned-p full-name)
      (user-error "Sprite %s is decommissioned and cannot be restarted" full-name))
    (sprite--ensure-state-dir full-name)
    (let ((s (sprite--spawn full-name)))
      (setf (sprite-spawned-by s)
            (buffer-name (current-buffer)))
      (sprite--registry-put s)
      (unless (sprite--wait-for-server full-name :timeout-secs wait-timeout)
        (user-error "Sprite %s did not become ready within %ds"
                    full-name wait-timeout))
      s)))

(defun sprite--annotated-or-plain-read (prompt candidates)
  "Read a candidate string from CANDIDATES, an alist of (NAME . ANNOTATION).
Uses `annotated-completing-read', showing each ANNOTATION, when that
package is loaded; otherwise falls back to plain `completing-read' over
the candidate names, since `annotated-completing-read' is an optional
dependency.  Specify the text of the PROMPT."
  (if (fboundp 'annotated-completing-read)
      (annotated-completing-read candidates :prompt prompt :require-match t)
    (completing-read prompt (seq-map #'car candidates) nil t)))

(defun sprite--completing-read (prompt)
  "Read a sprite name from the registry using annotated completion.
Each candidate is annotated with its index and uptime.  Specify the text of the PROMPT."
  (sprite--annotated-or-plain-read
   prompt
   (seq-map (lambda (s)
              (cons (sprite-name s)
                    (format "idx:%d uptime:%s"
                            (sprite-idx s)
                            (if (sprite-start-time s)
                                (sprite--format-uptime (float-time (time-since (sprite-start-time s))))
                              "?"))))
            (sprite--registry-all))))

(defun sprite-stop (name)
  "Send (kill-emacs) to sprite NAME via emacsclient."
  (interactive (list (sprite--completing-read "Stop sprite: ")))
  (with-sprite (sprite--resolve-name name) (kill-emacs)))
(defun sprite-restart (name)
  "Restart sprite NAME: stop it then create a new daemon with the same unique-name."
  (interactive (list (sprite--completing-read "Restart sprite: ")))
  (let* ((full-name (sprite--resolve-name name))
         (s (sprite--registry-get full-name)))
    (when (sprite--decommissioned-p full-name)
      (user-error "Cannot restart decommissioned sprite: %s" full-name))
    (sprite-stop full-name)
    (sprite--ensure-state-dir full-name)
    (sprite--spawn full-name)
    (when s
      (setf (sprite-start-time (sprite--registry-get full-name)) (current-time)))))

(defun sprite-decommission (name)
  "Decommission sprite NAME: stop it and write the DECOMMISSIONED marker."
  (interactive (list (sprite--completing-read "Decommission sprite: ")))
  (let ((full-name (sprite--resolve-name name)))
    (condition-case _ (sprite-stop full-name) (error nil))
    (sprite--ensure-state-dir full-name)
    (unless (sprite--write-decommissioned-file full-name)
      (error "Failed to write DECOMMISSIONED file for %s" full-name))
    (when-let* ((s (sprite--registry-get full-name)))
      (setf (sprite-last-contact s) (current-time)))))

;;;; Communication

(defcustom sprite-communication-backend 'emacsclient
  "Transport used for sprite eval calls.
`emacsclient' shells out to the emacsclient binary (default, matches
current behaviour).  `direct' speaks the server wire protocol over a
socket via sprite-direct.el, with no subprocess."
  :type '(choice (const emacsclient) (const direct))
  :group 'sprite)

(defcustom sprite-communication-fallback nil
  "When non-nil, retry with the other backend after a failed direct call.
Only applies when `sprite-communication-backend' is `direct': a failed
socket connection retries once via emacsclient before giving up."
  :type 'boolean
  :group 'sprite)

(defun sprite--direct-target (full-name)
  "Return the sprite-direct TARGET string for FULL-NAME.
When the sprite runs with `server-use-tcp', reads its TCP server file
via `sprite-direct-read-tcp-server-file' if present; otherwise returns
FULL-NAME as a Unix socket name."
  (if (and (bound-and-true-p server-use-tcp)
           (bound-and-true-p server-auth-dir))
      (or (sprite-direct-read-tcp-server-file
           (expand-file-name full-name server-auth-dir))
          full-name)
    full-name))

;;;; Uptime formatting and remote frames
;;
;; The tabulated overview buffer (`sprite-list', `sprite-list-mode', etc.)
;; lives in sprite-list.el, loaded on demand -- it depends on tabulated-list
;; and transient, neither of which the rest of this file needs.

(defun sprite--format-uptime (seconds)
  "Format SECONDS as a human-readable uptime string.
Returns \"?\" when SECONDS is nil."
  (cond
   ((null seconds) "?")
   ((< seconds 60) (format "%ds" (round seconds)))
   ((< seconds 3600) (format "%dm" (round (/ seconds 60))))
   ((< seconds 86400) (format "%dh" (round (/ seconds 3600))))
   (t (format "%dd" (round (/ seconds 86400))))))

(defun sprite--format-duration (seconds)
  "Format SECONDS as a compact duration string, or \"?\" when nil."
  (cond
   ((null seconds)    "?")
   ((< seconds 60)   (format "%ds" seconds))
   ((< seconds 3600) (format "%dm" (/ seconds 60)))
   (t                (format "%dh" (/ seconds 3600)))))

;;;###autoload
(defun sprite-open-frame (sprite)
  "Open a new Emacs frame connected to SPRITE.
When called interactively, select from accessible sprites via
`annotated-completing-read' (when available), annotated with status and
uptime."
  (interactive
   (list (let* ((sprites (sprite-resolve-list))
                (name (sprite--annotated-or-plain-read
                       "sprite:"
                       (seq-map (lambda (s)
                                  (cons (sprite-name s)
                                        (format "%s  up:%s"
                                                (or (sprite-running-status s) "?")
                                                (sprite--format-duration
                                                 (when-let* ((st (sprite-start-time s)))
                                                   (floor (float-time
                                                           (time-since st))))))))
                                sprites))))
           (seq-find (lambda (s) (equal name (sprite-name s))) sprites))))
  (unless sprite
    (user-error "No sprite selected"))
  (let ((name (sprite-name sprite)))
    (with-environment-variables (("DISPLAY" (or (getenv "DISPLAY") ":0"))
                                 ("TERM" nil))
      (start-process
       (format "sprite-frame-%s" name)
       (get-buffer-create (sprite--log-buffer-name name))
       "emacsclient" "--no-wait" "--create-frame" "--server-file" name))))

;;;; Fleet API

(defcustom sprite-active-threshold 30
  "Seconds since last-contact after which a sprite is considered available.
A sprite is \"active\" if it was contacted within this many seconds."
  :type 'integer
  :group 'sprite)

(defcustom sprite-max-count 4
  "Maximum number of sprite `sprite-get-or-create-next' will spawn."
  :type 'integer
  :group 'sprite)

(defcustom sprite-startup-timeout 30
  "Seconds to wait for a newly spawned sprite server to accept connections."
  :type 'integer
  :group 'sprite)


(defun sprite-worker-p ()
  "Return t if this instance is itself a sprite (has a parent)."
  (sprite--full-name-p (sprite-instance-name)))

(defun sprite-resolve-list ()
  "Return list of sprite structs accessible to this instance.
Includes direct children.  If this instance is itself a sprite (worker),
also includes sibling sprite from the same parent."
  (let ((own-id (sprite-instance-name)))
    (if (sprite-worker-p)
      (let ((parts (sprite--parse-full-name own-id)))
        (thread-last (sprite--registry-all)
          (seq-filter (lambda (s)
                        (and (sprite--live-p s)
                             (or (equal own-id (sprite-parent s))
                                 (equal (car parts) (sprite-parent s))))))))
      (thread-last (sprite--registry-all)
        (seq-filter (lambda (s)
                      (and (equal own-id (sprite-parent s))
                           (sprite--live-p s))))))))

(defun sprite-controller-p ()
  "Return t if this instance has at least one non-decommissioned sprite."
  (not (null (thread-last (sprite--registry-all)
               (seq-filter (lambda (s)
                              (and (equal (sprite-instance-name) (sprite-parent s))
                                   (sprite--live-p s))))))))

(defun sprite-available-p ()
  "Return t if any accessible sprite are configured."
  (not (null (sprite-resolve-list))))

(defun sprite--available-p (s)
  "Return t if sprite S has not been contacted recently."
  (or (null (sprite-last-contact s))
      (> (float-time (time-since (sprite-last-contact s)))
         sprite-active-threshold)))

(defun sprite--usable-p (s)
  "Return non-nil if sprite struct S is running and available."
  (and (sprite--running-p (sprite-name s))
       (sprite--available-p s)))

(defun sprite--target-name (s)
  "Return target name string for sprite S (struct or string)."
  (if (stringp s) s (sprite-name s)))

 ;;;###autoload
 (defun sprite-get-next ()
   "Return the struct of the next available sprite, or nil.
 \"Available\" means running and not recently contacted."
   (seq-find #'sprite--usable-p (sprite-resolve-list)))

 ;;;###autoload
 (cl-defun sprite-get-or-create-next (&key timeout)
   "Return the next available sprite, creating one if none are free.
 Times out after TIMEOUT seconds.  Signals `user-error' if
 `sprite-max-count' would be exceeded."
   (or (sprite-get-next)
       (let ((active-count (length (sprite-resolve-list))))
         (if (>= active-count sprite-max-count)
             (user-error "All %d sprite are busy and max-count (%d) reached"
                         active-count sprite-max-count)
           (sprite-create (format "worker-%d" active-count) :timeout timeout)))))

 ;;;###autoload
 (cl-defun sprite-get-or-create-fleet (count &key timeout)
   "Return a list of up to COUNT available or newly created sprite structs.
 Reuses available sprites first, spawning new ones up to `sprite-max-count'
 or COUNT."
   (let* ((available (seq-take (seq-filter #'sprite--usable-p (sprite-resolve-list)) count))
          (needed (- count (length available)))
          (active-count (length (sprite-resolve-list)))
          (created-list nil)
          (created nil))
     (while (and (> needed 0) (< active-count sprite-max-count))
       (setq created (ignore-errors
                       (sprite-create (format "worker-%d" active-count) :timeout timeout)))
       (if created
           (progn
             (push created created-list)
             (cl-decf needed)
             (cl-incf active-count))
         (setq needed 0)))
     (nconc available (nreverse created-list))))


;;;; Global minor mode

(defvar savehist-additional-variables)

(defvar-keymap sprite-mode-command-map
  "c" #'sprite-create
  "l" #'sprite-list
  "o" #'sprite-open-frame
  "n" #'sprite-get-or-create-next)

(defcustom sprite-mode-map-prefix nil
  "Cons storing `sprite-mode-command-map' is bound as a nested prefix keymap.

Applied when `sprite-mode' is enabled and removed when it is
disabled.  Set to nil to skip binding it into any parent keymap."
  :type '(choice (const :tag "Do not bind" nil)
                 (cons (string :tag "Key") (symbol :tag "Keymap")))
  :group 'sprite)

(defun sprite-mode--bind-map-prefix ()
  "Bind `sprite-mode-command-map' per `sprite-mode-map-prefix'."
  (when sprite-mode-map-prefix
    (keymap-set (symbol-value (cdr sprite-mode-map-prefix))
                (car sprite-mode-map-prefix)
                (cons "sprite" sprite-mode-command-map))))

(defun sprite-mode--unbind-map-prefix ()
  "Remove the binding installed by `sprite-mode--bind-map-prefix'."
  (when sprite-mode-map-prefix
    (keymap-unset (symbol-value (cdr sprite-mode-map-prefix))
                  (car sprite-mode-map-prefix) t)))

(defun sprite-mode--enable ()
  "Wire the sprite registry into `savehist' persistence and populate it.
Registers `sprite--registry-saved' with `savehist-additional-variables' and
hooks the serialize/deserialize/activate/discovery steps onto `savehist' so
the registry survives across sessions, then scans the filesystem for
sprites already running to populate the in-memory registry immediately."
  (add-to-list 'savehist-additional-variables 'sprite--registry-saved)
  (add-hook 'savehist-save-hook #'sprite--registry-serialize)
  ;; Explicit depths, not call order: `sprite-defs-activate' and
  ;; `sprite--discover-and-sync-registry' need the registry already
  ;; populated, so `sprite--registry-deserialize' must run first.
  (add-hook 'savehist-mode-hook #'sprite--registry-deserialize -10)
  (add-hook 'savehist-mode-hook #'sprite-defs-activate)
  (add-hook 'savehist-mode-hook #'sprite--discover-and-sync-registry 10)
  ;; `savehist-mode-hook' only fires on the transition to enabled; if
  ;; `savehist-mode' is already on by the time `sprite-mode' is turned on,
  ;; run the same reconciliation steps directly.
  (when (bound-and-true-p savehist-mode)
    (sprite--registry-deserialize)
    (sprite-defs-activate))
  (sprite--discover-and-sync-registry)
  (sprite-mode--bind-map-prefix))

(defun sprite-mode--disable ()
  "Undo `sprite-mode--enable': remove sprite's `savehist' hooks and variable."
  (setq savehist-additional-variables
        (delq 'sprite--registry-saved savehist-additional-variables))
  (remove-hook 'savehist-save-hook #'sprite--registry-serialize)
  (remove-hook 'savehist-mode-hook #'sprite--registry-deserialize)
  (remove-hook 'savehist-mode-hook #'sprite-defs-activate)
  (remove-hook 'savehist-mode-hook #'sprite--discover-and-sync-registry)
  (sprite-mode--unbind-map-prefix))

(defvar-keymap sprite-mode-map
  :doc "Empty by design -- `sprite-mode' ships no default bindings of its
own. See `sprite-mode-command-map' for commands, nested per
`sprite-mode-map-prefix'; bind directly into this map for anything that
should be active unconditionally whenever `sprite-mode' is on.")

;;;###autoload
(define-minor-mode sprite-mode
  "Global minor mode for managing sprite (subordinate Emacs daemon) state.
Enabling wires the registry into `savehist' persistence and scans the
filesystem for already-running sprites to populate the registry;
disabling removes the `savehist' hooks and variable registration.
See `sprite-mode-map' and `sprite-mode-command-map'."
  :global t
  :group 'sprite
  :keymap sprite-mode-map
  (if sprite-mode
      (sprite-mode--enable)
    (sprite-mode--disable)))

(provide 'sprite)
;;; sprite.el ends here
