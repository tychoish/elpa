;;; hitl-cursor.el --- External polling cursors and serialization for HITL -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Keywords: tools, convenience, agent, ai
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides cursor-based iteration over pending questions for external
;; processes (CLI tools, daemons, background workers) and serialization
;; helpers for plist and JSON data interchange.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(declare-function hitl-get "hitl")
(declare-function hitl-answer "hitl")
(declare-function hitl-list-pending "hitl")
(declare-function hitl-list-all "hitl")
(declare-function hitl-clear-store "hitl")
(declare-function hitl--normalize-kind "hitl")
(declare-function hitl-question--make "hitl")
(declare-function hitl-question-id "hitl")
(declare-function hitl-question-prompt "hitl")
(declare-function hitl-question-kind "hitl")
(declare-function hitl-question-options "hitl")
(declare-function hitl-question-default-value "hitl")
(declare-function hitl-question-status "hitl")
(declare-function hitl-question-response "hitl")
(declare-function hitl-question-target "hitl")
(declare-function hitl-question-directory "hitl")
(declare-function hitl-question-created-at "hitl")
(declare-function hitl-question-answered-at "hitl")
(declare-function hitl-question-timeout "hitl")
(declare-function hitl-question-metadata "hitl")

(defvar hitl--store)
(defvar hitl--cursors (make-hash-table :test #'equal)
  "Hash table mapping cursor ID strings to last-seen question ID strings.")

;;; Cursor Protocol

;;;###autoload
(defun hitl-cursor-next (&optional cursor-id target-filter)
  "Return the next pending `hitl-question' for CURSOR-ID and advance position.
If CURSOR-ID is nil, defaults to \"default\".
When TARGET-FILTER is non-nil, only questions matching TARGET-FILTER
are returned.  Advances the cursor position to the returned question."
  (let* ((cid (or cursor-id "default"))
         (last-id (gethash cid hitl--cursors))
         (pending (hitl-list-pending target-filter))
         (next-q (cond
                  ((null pending) nil)
                  ((null last-id) (car pending))
                  (t
                   (let ((tail (member (hitl-get last-id) pending)))
                     (if (and tail (cdr tail))
                         (cadr tail)
                       (car pending)))))))
    (when next-q
      (puthash cid (hitl-question-id next-q) hitl--cursors))
    next-q))

;;;###autoload
(defun hitl-cursor-peek (&optional cursor-id target-filter)
  "Inspect the next pending `hitl-question' for CURSOR-ID without advancing.
If CURSOR-ID is nil, defaults to \"default\"."
  (let* ((cid (or cursor-id "default"))
         (last-id (gethash cid hitl--cursors))
         (pending (hitl-list-pending target-filter)))
    (cond
     ((null pending) nil)
     ((null last-id) (car pending))
     (t
      (let ((tail (member (hitl-get last-id) pending)))
        (if (and tail (cdr tail))
            (cadr tail)
          (car pending)))))))

;;;###autoload
(defun hitl-cursor-reset (&optional cursor-id)
  "Reset cursor CURSOR-ID to the beginning."
  (remhash (or cursor-id "default") hitl--cursors))

;;;###autoload
(defun hitl-cursor-answer (id response)
  "Submit an answer RESPONSE for question ID via cursor protocol.
Returns the updated `hitl-question' struct."
  (hitl-answer id response))

;;; Serialization Helpers (Plist & JSON)

;;;###autoload
(defun hitl-question-to-plist (q)
  "Serialize `hitl-question' struct Q to a property list."
  (list :id (hitl-question-id q)
        :prompt (hitl-question-prompt q)
        :kind (hitl-question-kind q)
        :options (hitl-question-options q)
        :default-value (hitl-question-default-value q)
        :status (hitl-question-status q)
        :response (hitl-question-response q)
        :target (let ((tgt (hitl-question-target q)))
                  (if (bufferp tgt) (buffer-name tgt) tgt))
        :directory (hitl-question-directory q)
        :created-at (hitl-question-created-at q)
        :answered-at (hitl-question-answered-at q)
        :timeout (hitl-question-timeout q)
        :metadata (hitl-question-metadata q)))

;;;###autoload
(defun hitl-question-from-plist (plist)
  "Deserialize PLIST to a `hitl-question' struct."
  (hitl-question--make
   :id (plist-get plist :id)
   :prompt (plist-get plist :prompt)
   :kind (hitl--normalize-kind (plist-get plist :kind))
   :options (plist-get plist :options)
   :default-value (plist-get plist :default-value)
   :status (let ((st (plist-get plist :status)))
             (if (stringp st) (intern st) st))
   :response (plist-get plist :response)
   :target (plist-get plist :target)
   :directory (plist-get plist :directory)
   :created-at (plist-get plist :created-at)
   :answered-at (plist-get plist :answered-at)
   :timeout (plist-get plist :timeout)
   :metadata (plist-get plist :metadata)))

;;;###autoload
(defun hitl-serialize-store ()
  "Serialize the entire question store into a list of plists."
  (mapcar #'hitl-question-to-plist (hitl-list-all)))

;;;###autoload
(defun hitl-deserialize-store (data)
  "Populate the question store from a list of question plists DATA."
  (hitl-clear-store)
  (dolist (item data)
    (let ((q (hitl-question-from-plist item)))
      (puthash (hitl-question-id q) q hitl--store))))

;;;###autoload
(defun hitl-question-to-json (q)
  "Serialize `hitl-question' struct Q to a JSON string."
  (let* ((plist (hitl-question-to-plist q))
         (json-encoding-pretty-print nil))
    (json-encode plist)))

;;;###autoload
(defun hitl-question-from-json (json-string)
  "Deserialize JSON-STRING to a `hitl-question' struct."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (plist (json-read-from-string json-string)))
    (hitl-question-from-plist plist)))

(provide 'hitl-cursor)
;;; hitl-cursor.el ends here
