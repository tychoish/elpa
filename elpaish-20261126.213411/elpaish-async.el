;;; elpaish-async.el --- Concurrency and parallelism delegation shim -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Tychoish <tycho@tychoish.com>
;; Keywords: convenience, tools

;;; Commentary:

;; Ultra-minimal delegation shim providing `elpaish-async-mapcar'.
;; When `sprite-fleet' is available and `elpaish-parallel-jobs' > 1,
;; package builds are dispatched across persistent worker daemons.
;; Otherwise, execution cleanly falls back to synchronous `mapcar'.

;;; Code:

(require 'cl-lib)
(require 'map)

(declare-function sprite-fleet-mapcar "sprite-fleet" (fn-or-form items &rest args))
(declare-function sprite-future-then "sprite-future" (future on-resolve &optional on-reject))
(declare-function sprite-future--make "sprite-future" (&rest args))
(declare-function sprite-future--settle "sprite-future" (future state value &optional before-callbacks))
(declare-function sprite-get-or-create-fleet "sprite-daemon" (count &rest args))
(declare-function sprite--target-name "sprite-daemon" (s))
(declare-function elpaish-recipe-p "elpaish" (object))
(declare-function elpaish-recipe-name "elpaish" (recipe))
(declare-function elpaish-build-package "elpaish" (recipe &optional stream output-directory-path))
(declare-function elpaish-preflight-package "elpaish" (recipe))
(declare-function elpaish--recipe-source-path "elpaish" (recipe))
(declare-function elpaish-install--resolve-repository-recipe "elpaish-install" (pkg))
(defvar elpaish-registry)

(defgroup elpaish-async nil
  "Concurrency and parallel execution options for ELPAish."
  :group 'elpaish
  :prefix "elpaish-")

(defcustom elpaish-parallel-jobs (min 8 (num-processors))
  "Maximum concurrent package build jobs."
  :type 'integer
  :group 'elpaish-async)

(defcustom elpaish-build-timeout 300
  "Timeout in seconds for building a single package."
  :type 'integer
  :group 'elpaish-async)

(defcustom elpaish-async-local-fallback 'ask
  "Whether to build in this Emacs when no sprite is available.
`ask' prompts (and fails the build when running non-interactively),
`always' builds locally without asking, and `never' fails the build.
Choosing \"always\" at the prompt sets this to `always' for the session."
  :type '(choice (const :tag "Ask first" ask)
                 (const :tag "Always build locally" always)
                 (const :tag "Never build locally" never))
  :group 'elpaish-async)

(defun elpaish-async-error-p (result)
  "Return non-nil if RESULT represents an error from `elpaish-async-mapcar'."
  (and (consp result) (plist-get result :error)))

(defun elpaish-async--sprite-fleet-supported-p ()
  "Return non-nil if `sprite-fleet' is loaded and supports modern combinator keys."
  (and (> elpaish-parallel-jobs 1)
       (require 'sprite-fleet nil t)
       (fboundp 'sprite-fleet-mapcar)
       (condition-case nil
           (let ((arglist (help-function-arglist 'sprite-fleet-mapcar)))
             (and (memq '&key arglist)
                  (let ((keys (cdr (memq '&key arglist))))
                    (and (or (memq 'order keys) (assq 'order keys))
                         (or (memq 'error-policy keys) (assq 'error-policy keys))))))
         (error nil))))

(defun elpaish-async-mapcar (fn items)
  "Map FN over ITEMS, delegating to `sprite-fleet' if available.
When `elpaish-parallel-jobs' is greater than 1 and modern `sprite-fleet'
is available, delegate to `sprite-fleet-mapcar' with `:order \\='preserve'
and `:error-policy :collect-error'.  Otherwise, execute synchronously
using `mapcar', wrapping any signaled errors as `(:error ERR :target local)'
to preserve uniform error collection semantics."
  (cond
   ((elpaish-async--sprite-fleet-supported-p)
    (sprite-fleet-mapcar fn items
                         :fleet-size elpaish-parallel-jobs
                         :timeout elpaish-build-timeout
                         :order 'preserve
                         :error-policy :collect-error))
   (t
    (mapcar (lambda (item)
              (condition-case err
                  (funcall fn item)
                (error (list :error err :target 'local))))
            items))))

(defvar elpaish-async--worker-loaded-files nil
  "Alist mapping library name to (FILE . MTIME) loaded in a sprite worker.
Used to reload ELPAish libraries automatically when a persistent sprite
daemon outlives a package upgrade or source edit.")

(defun elpaish-async--resolve-elpaish-dir ()
  "Resolve the directory containing `elpaish.el' for sprite workers.
Prefers `locate-library' for `elpaish', then the directory of
`elpaish-async' if it contains `elpaish', then the registered `elpaish'
recipe's source path, and finally `pkg/' under `default-directory'."
  (let ((lib (locate-library "elpaish")))
    (cond
     (lib (file-name-directory lib))
     ((let* ((async-lib (locate-library "elpaish-async"))
             (dir (and async-lib (file-name-directory async-lib))))
        (and dir
             (or (file-exists-p (expand-file-name "elpaish.el" dir))
                 (file-exists-p (expand-file-name "elpaish.elc" dir)))
             dir)))
     ((and (boundp 'elpaish-registry)
           (hash-table-p elpaish-registry)
           (fboundp 'elpaish--recipe-source-path))
      (let* ((rec (map-elt elpaish-registry "elpaish"))
             (src (and rec (elpaish--recipe-source-path rec))))
        (and src (file-exists-p (expand-file-name "elpaish.el" src)) src)))
     (t (expand-file-name "pkg/" default-directory)))))

;;;###autoload
(defun elpaish-async-acquire-sprite-targets (count)
  "Acquire up to COUNT distinct sprite target names for parallel builds.
Delegates to `sprite-get-or-create-fleet' when available and returns a
deduplicated list of sprite target name strings, or nil when no sprites
can be acquired."
  (when (and (integerp count)
             (> count 0)
             (fboundp 'sprite-get-or-create-fleet))
    (let ((sprites (condition-case nil
                       (sprite-get-or-create-fleet count)
                     (error nil))))
      (seq-uniq
       (delq nil
             (seq-map (lambda (s)
                        (cond
                         ((stringp s) s)
                         ((fboundp 'sprite--target-name) (sprite--target-name s))
                         (t nil)))
                      sprites))))))

(defun elpaish-async--build-package-form (recipe-spec stream staging-dir load-path-dirs elpaish-dir &optional registry-recipes)
  "Construct Lisp form to preflight and build RECIPE-SPEC in a sprite.
STREAM is the target release stream, STAGING-DIR receives the built artifact,
LOAD-PATH-DIRS augments worker `load-path', ELPAISH-DIR specifies the
directory of ELPAish source/bytecode to reload if changed, and optional
REGISTRY-RECIPES synchronizes sibling recipes into the worker registry."
  `(progn
     (dolist (p ',load-path-dirs)
       (add-to-list 'load-path p))
     (when (and ,elpaish-dir (file-directory-p ,elpaish-dir))
       (let ((dir (file-name-as-directory ,elpaish-dir)))
         (setq load-path (cons dir (delete dir load-path)))
         (unless (boundp 'elpaish-async--worker-loaded-files)
           (set 'elpaish-async--worker-loaded-files nil))
         (dolist (lib '("elpaish" "elpaish-recipes" "elpaish-check"))
           (let* ((el-file (expand-file-name (concat lib ".el") dir))
                  (elc-file (expand-file-name (concat lib ".elc") dir))
                  (has-el (file-exists-p el-file))
                  (has-elc (file-exists-p elc-file))
                  (target (cond
                           ((and has-el has-elc)
                            (if (file-newer-than-file-p el-file elc-file) el-file elc-file))
                           (has-elc elc-file)
                           (has-el el-file)
                           (t nil))))
             (when target
               (let* ((attrs (file-attributes target))
                      (sig (cons target (and attrs (file-attribute-modification-time attrs))))
                      (prev (cdr (assoc lib (symbol-value 'elpaish-async--worker-loaded-files)))))
                 (when (or (not (featurep (intern lib)))
                           (not (equal sig prev)))
                   (load target nil t)
                   (set 'elpaish-async--worker-loaded-files
                        (cons (cons lib sig)
                              (assoc-delete-all lib (symbol-value 'elpaish-async--worker-loaded-files)))))))))))
     (require 'map)
     (require 'elpaish)
     (require 'elpaish-recipes)
     (require 'elpaish-check nil t)
     (when ',registry-recipes
       (unless (and (boundp 'elpaish-registry) (hash-table-p elpaish-registry))
         (setq elpaish-registry (make-hash-table :test #'equal)))
       (dolist (r ',registry-recipes)
         (when (elpaish-recipe-p r)
           (setf (map-elt elpaish-registry (elpaish-recipe-name r)) r))))
     (let* ((recipe-or-name ',recipe-spec)
            (effective-stream ',stream)
            (output-dir ,staging-dir)
            (elpaish-sign-packages nil))
       (unless (and (boundp 'elpaish-registry)
                    (hash-table-p elpaish-registry)
                    (> (hash-table-count elpaish-registry) 0))
         (elpaish-load-packages))
       (let* ((rec (cond
                    ((elpaish-recipe-p recipe-or-name) recipe-or-name)
                    ((fboundp 'elpaish-install--resolve-repository-recipe)
                     (elpaish-install--resolve-repository-recipe recipe-or-name))
                    ((and (boundp 'elpaish-registry) (hash-table-p elpaish-registry))
                     (or (map-elt elpaish-registry (format "%s" recipe-or-name))
                         (map-elt elpaish-registry (if (symbolp recipe-or-name)
                                                       recipe-or-name
                                                     (intern recipe-or-name)))))))
              (pkg-name (if rec (elpaish-recipe-name rec) (format "%s" recipe-or-name))))
         (if (null rec)
             (list :status :failed
                   :pkg (if (symbolp recipe-or-name) recipe-or-name (intern (format "%s" recipe-or-name)))
                   :stage :recipe-resolution
                   :error (format "No recipe found in repository for %s" pkg-name)
                   :log (format "Recipe resolution failed for %s" pkg-name))
           (when (and (boundp 'elpaish--preflight-cache)
                      (hash-table-p elpaish--preflight-cache))
             (map-delete elpaish--preflight-cache pkg-name))
           (when (and (boundp 'elpaish--preflight-diagnostics)
                      (hash-table-p elpaish--preflight-diagnostics))
             (map-delete elpaish--preflight-diagnostics pkg-name))
           (when (boundp 'elpaish--last-build-error)
             (setq elpaish--last-build-error nil))
           (condition-case err
               (let ((artifact (elpaish-build-package rec effective-stream output-dir)))
                 (if (and artifact (file-exists-p artifact))
                     (list :status :ok
                           :pkg (intern pkg-name)
                           :artifact artifact
                           :diagnostics (and (boundp 'elpaish--preflight-diagnostics)
                                             (map-elt elpaish--preflight-diagnostics pkg-name)))
                   (let* ((diag (and (boundp 'elpaish--preflight-diagnostics)
                                     (map-elt elpaish--preflight-diagnostics pkg-name)))
                          (last-err (or (and (boundp 'elpaish--last-build-error) elpaish--last-build-error)
                                        "Build produced no artifact"))
                          (stage (cond
                                  ((and diag (listp diag)) :preflight)
                                  ((and (boundp 'elpaish--last-build-error) elpaish--last-build-error) :build)
                                  (t :preflight))))
                     (list :status :failed
                           :pkg (intern pkg-name)
                           :stage stage
                           :error last-err
                           :log (format "Build failed for %s: %s" pkg-name last-err)))))
             (error
              (list :status :failed
                    :pkg (intern pkg-name)
                    :stage :build
                    :error (error-message-string err)
                    :log (format "Exception during build of %s: %s" pkg-name (error-message-string err))))))))))

;;;###autoload
(cl-defun elpaish-async-build-package-in-sprite (recipe &key stream staging-dir timeout sprite)
  "Build RECIPE asynchronously inside an isolated worker sprite daemon.
Returns a `sprite-future' resolving to a structured result plist:
  (:status :ok :artifact PATH :diagnostics DIAG :pkg PKG)
or on failure:
  (:status :failed :stage STAGE :error ERR :log LOG :pkg PKG).

STREAM specifies the release stream (defaults to \\='snapshot).
STAGING-DIR specifies the target directory for the built package artifact.
TIMEOUT overrides `elpaish-build-timeout'.
SPRITE, when non-nil, pins execution to that target sprite name or struct.
When no sprite is available the fleet rejects the dispatch; per
`elpaish-async-local-fallback' the build then runs in this Emacs, blocking
until it finishes, or fails."
  (let* ((effective-stream (or stream 'snapshot))
         (effective-timeout (or timeout elpaish-build-timeout))
         (target-dir (or staging-dir (make-temp-file "elpaish-sprite-build-" t)))
         (elpaish-dir (elpaish-async--resolve-elpaish-dir))
         (registry-recipes (when (and (boundp 'elpaish-registry)
                                      (hash-table-p elpaish-registry)
                                      (> (hash-table-count elpaish-registry) 0))
                             (map-values elpaish-registry)))
         (form (elpaish-async--build-package-form
                recipe effective-stream target-dir load-path elpaish-dir registry-recipes)))
    (cond
     ((and (require 'sprite nil t)
           (require 'sprite-fleet nil t)
           (require 'sprite-future nil t)
           (fboundp 'sprite-fleet-mapcar)
           (fboundp 'sprite-future-then))
      (let ((fleet-future
             (apply #'sprite-fleet-mapcar
                    form
                    (list t)
                    :fleet-size 1
                    :timeout effective-timeout
                    :async t
                    :order 'preserve
                    :error-policy :collect-error
                    (when sprite
                      (list :sprites (list sprite))))))
        (sprite-future-then
         fleet-future
         (lambda (results)
           (let ((res (car results)))
             (cond
              ((and (listp res) (plist-member res :status)) res)
              ((elpaish-async-error-p res)
               (let* ((raw-err (plist-get res :error))
                      (err-msg (cond
                                ((stringp raw-err) raw-err)
                                ((and (consp raw-err) (symbolp (car raw-err))
                                      (get (car raw-err) 'error-conditions))
                                 (error-message-string raw-err))
                                (t (format "%S" raw-err)))))
                 (list :status :failed
                       :pkg (cond
                             ((elpaish-recipe-p recipe) (intern (elpaish-recipe-name recipe)))
                             ((symbolp recipe) recipe)
                             (t (intern (format "%s" recipe))))
                       :stage :sprite-dispatch
                       :error err-msg
                       :log (format "Sprite dispatch failure: %s" err-msg))))
              ((listp res) res)
              (t
               (list :status :failed
                     :pkg (cond
                           ((elpaish-recipe-p recipe) (intern (elpaish-recipe-name recipe)))
                           ((symbolp recipe) recipe)
                           (t (intern (format "%s" recipe))))
                     :stage :unknown
                     :error "Invalid response from sprite worker"
                     :log (format "%S" res))))))
         (lambda (reject-val)
           (let ((pkg (cond
                       ((elpaish-recipe-p recipe) (intern (elpaish-recipe-name recipe)))
                       ((symbolp recipe) recipe)
                       (t (intern (format "%s" recipe)))))
                 (reason (if (stringp reject-val) reject-val (format "%S" reject-val))))
             (if (pcase elpaish-async-local-fallback
                   ('always t)
                   ('never nil)
                   (_ (and (not noninteractive)
                           (pcase (car (read-multiple-choice
                                        (format "No sprite available to build %s (%s).  Build in this Emacs (blocks until done)? "
                                                pkg reason)
                                        '((?y "yes" "build in this Emacs")
                                          (?n "no" "fail this build")
                                          (?a "always" "build locally without asking this session"))))
                             (?y t)
                             (?a (setq elpaish-async-local-fallback 'always) t)
                             (_ nil)))))
                 (progn
                   (message "[elpaish] No sprite available for %s (%s); building in this Emacs" pkg reason)
                   (condition-case err
                       (eval form t)
                     (error
                      (list :status :failed :pkg pkg :stage :local-fallback
                            :error (error-message-string err)
                            :log (format "Local build fallback failure: %s"
                                         (error-message-string err))))))
               (list :status :failed :pkg pkg :stage :sprite-dispatch
                     :error (format "No sprite available (%s); local build not permitted" reason)
                     :log (format "Sprite dispatch failure: %s" reason))))))))
     ((and (fboundp 'sprite-future--make)
           (fboundp 'sprite-future--settle))
      (let* ((res (eval form t))
             (fut (sprite-future--make :state :pending)))
        (sprite-future--settle fut :resolved res)
        fut))
     (t
      (eval form t)))))

(provide 'elpaish-async)

;; Local Variables:
;; package-lint-main-file: "elpaish.el"
;; End:
;;; elpaish-async.el ends here
