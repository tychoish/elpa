;;; elpaish-async.el --- Concurrency and parallelism delegation shim -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Tychoish <tycho@tychoish.com>
;; Keywords: convenience, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Ultra-minimal delegation shim providing `elpaish-async-mapcar'.
;; When `sprite-fleet' is available and `elpaish-parallel-jobs' > 1,
;; package builds are dispatched across persistent worker daemons.
;; Otherwise, execution cleanly falls back to synchronous `mapcar'.

;;; Code:

(require 'cl-lib)

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

(provide 'elpaish-async)
;;; elpaish-async.el ends here
