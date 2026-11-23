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
(declare-function elpaish-recipe-p "elpaish" (object))
(declare-function elpaish-recipe-name "elpaish" (recipe))
(declare-function elpaish-build-package "elpaish" (recipe &optional stream output-directory-path))
(declare-function elpaish-preflight-package "elpaish" (recipe))
(declare-function elpaish-install--resolve-repository-recipe "elpaish-install" (pkg))

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

(defun elpaish-async--build-package-form (recipe-spec stream staging-dir load-path-dirs elpaish-dir)
  "Construct Lisp form to preflight and build RECIPE-SPEC in a sprite."
  `(progn
     (dolist (p ',load-path-dirs)
       (add-to-list 'load-path p))
     (when ,elpaish-dir
       (add-to-list 'load-path ,elpaish-dir))
     (require 'map)
     (require 'elpaish)
     (require 'elpaish-recipes)
     (require 'elpaish-check nil t)
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
                                  ((and diag (listp diag)) (car diag))
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
(cl-defun elpaish-async-build-package-in-sprite (recipe &key stream staging-dir timeout)
  "Build RECIPE asynchronously inside an isolated worker sprite daemon.
Returns a `sprite-future' resolving to a structured result plist:
  (:status :ok :artifact PATH :diagnostics DIAG :pkg PKG)
or on failure:
  (:status :failed :stage STAGE :error ERR :log LOG :pkg PKG).

STREAM specifies the release stream (defaults to \\='snapshot).
STAGING-DIR specifies the target directory for the built package artifact.
TIMEOUT overrides `elpaish-build-timeout'.
When no sprite is available the fleet rejects the dispatch; per
`elpaish-async-local-fallback' the build then runs in this Emacs, blocking
until it finishes, or fails."
  (let* ((effective-stream (or stream 'snapshot))
         (effective-timeout (or timeout elpaish-build-timeout))
         (target-dir (or staging-dir (make-temp-file "elpaish-sprite-build-" t)))
         (elpaish-lib (locate-library "elpaish"))
         (elpaish-dir (if elpaish-lib
                          (file-name-directory elpaish-lib)
                        (expand-file-name "pkg/" default-directory)))
         (form (elpaish-async--build-package-form recipe effective-stream target-dir load-path elpaish-dir)))
    (cond
     ((and (require 'sprite nil t)
           (require 'sprite-fleet nil t)
           (require 'sprite-future nil t)
           (fboundp 'sprite-fleet-mapcar)
           (fboundp 'sprite-future-then))
      (let ((fleet-future
             (sprite-fleet-mapcar
              form
              (list t)
              :fleet-size 1
              :timeout effective-timeout
              :async t
              :order 'preserve
              :error-policy :collect-error)))
        (sprite-future-then
         fleet-future
         (lambda (results)
           (let ((res (car results)))
             (cond
              ((elpaish-async-error-p res)
               (let ((err-msg (error-message-string (plist-get res :error))))
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
