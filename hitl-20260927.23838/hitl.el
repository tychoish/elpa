;;; hitl.el --- Universal Human-in-the-Loop Engine -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, agent, ai
;; URL: https://github.com/tychoish/hitl

;; This file is not part of GNU Emacs.

;;; Commentary:

;; hitl is a universal, package-agnostic Human-in-the-Loop engine for Emacs.
;; It provides structured questioning, approval gates, cursor-driven queue
;; polling for external processes, and rich interactive completion widgets.
;; Any AI agent or autonomous tool can use Emacs as a human approval barrier
;; without coupling to a specific queue or runner implementation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup hitl nil
  "Universal Human-in-the-Loop interaction engine."
  :group 'tools)

;;; Core Data Structure

(cl-defstruct (hitl-question
               (:constructor hitl-question--make)
               (:copier nil))
  "Represents an interactive question awaiting human response."
  id              ; String UUID/identifier
  prompt          ; String prompt text shown to user
  kind            ; Keyword/symbol: :boolean, :confirm, :single-choice, :multi-choice, :text, :file, :number
  options         ; List of choices (strings, or alist of (value . annotation/label))
  default-value   ; Default choice or string
  status          ; Symbol: 'pending, 'answered, 'cancelled, 'expired
  response        ; Human response payload
  target          ; Target identifier/context (string, symbol, or buffer)
  directory       ; Target default-directory string
  created-at      ; Float timestamp
  answered-at     ; Float timestamp or nil
  timeout         ; Optional timeout in seconds
  callback        ; Optional callback function: (lambda (response &optional question))
  metadata)       ; Extra key-value metadata plist

;;; Memory Store & Hooks

(defvar hitl--store (make-hash-table :test #'equal)
  "Hash table mapping question ID strings to `hitl-question' structs.")

(defvar hitl-on-question-created-functions nil
  "Hook run with (QUESTION) when a new question is registered.")

(defvar hitl-on-question-answered-functions nil
  "Hook run with (QUESTION RESPONSE) when a question is answered.")

(defvar hitl-on-question-cancelled-functions nil
  "Hook run with (QUESTION REASON) when a question is cancelled.")

(defvar hitl-on-question-expired-functions nil
  "Hook run with (QUESTION) when a question expires.")

;;; ID Generation & Helpers

;;;###autoload
(defun hitl-generate-id ()
  "Generate a unique question ID string."
  (format "hitl-%s-%04x"
          (format-time-string "%s")
          (random #xffff)))

;;;###autoload
(defun hitl--normalize-kind (kind)
  "Normalize KIND string, symbol or keyword to keyword symbol."
  (cond
   ((null kind) :single-choice)
   ((keywordp kind) kind)
   ((symbolp kind) (intern (format ":%s" (symbol-name kind))))
   ((stringp kind)
    (let ((trimmed (string-remove-prefix ":" kind)))
      (intern (format ":%s" trimmed))))
   (t :text)))

;;; Lifecycle API

;;;###autoload
(cl-defun hitl-ask
    (&key prompt (kind :single-choice) options default-value target
          directory timeout callback metadata id)
  "Register a new `hitl-question'.
PROMPT is the prompt string displayed to the user.
KIND specifies the input widget: `:boolean', `:confirm', `:single-choice',
`:multi-choice', `:text', `:file', or `:number'.
OPTIONS is a list of candidate strings or alist.
DEFAULT-VALUE is the preselected fallback value.
TARGET is an optional target identifier (buffer, shell name, or agent id).
DIRECTORY is an optional working directory string.
TIMEOUT is an optional lifespan in seconds.
CALLBACK is an optional function invoked with (RESPONSE QUESTION) upon answer.
METADATA is an optional plist of extra context.
ID optionally overrides the generated question ID.
Returns the created `hitl-question' struct."
  (unless prompt
    (error "Prompt string is required for hitl-ask"))
  (let* ((qid (or id (hitl-generate-id)))
         (norm-kind (hitl--normalize-kind kind))
         (dir (or directory
                  (when (bufferp target)
                    (buffer-local-value 'default-directory target))
                  default-directory))
         (q (hitl-question--make
             :id qid
             :prompt prompt
             :kind norm-kind
             :options options
             :default-value default-value
             :status 'pending
             :response nil
             :target target
             :directory dir
             :created-at (float-time)
             :answered-at nil
             :timeout timeout
             :callback callback
             :metadata metadata)))
    (puthash qid q hitl--store)
    (run-hook-with-args 'hitl-on-question-created-functions q)
    q))

;;;###autoload
(defun hitl-get (id)
  "Retrieve `hitl-question' by ID, or nil if not found."
  (gethash id hitl--store))

;;;###autoload
(defun hitl-list-pending (&optional target-filter)
  "List all pending `hitl-question' structs in chronological order.
When TARGET-FILTER is non-nil, only questions matching TARGET-FILTER
are returned."
  (hitl-check-expirations)
  (let ((items nil))
    (maphash
     (lambda (_id q)
       (when (and (eq (hitl-question-status q) 'pending)
                  (or (null target-filter)
                      (equal (hitl-question-target q) target-filter)))
         (push q items)))
     hitl--store)
    (sort items (lambda (a b) (< (hitl-question-created-at a)
                                 (hitl-question-created-at b))))))

;;;###autoload
(defun hitl-list-all ()
  "List all registered `hitl-question' structs in chronological order."
  (let ((items nil))
    (maphash (lambda (_id q) (push q items)) hitl--store)
    (sort items (lambda (a b) (< (hitl-question-created-at a)
                                 (hitl-question-created-at b))))))

;;;###autoload
(defun hitl-answer (id response)
  "Mark question ID as answered with RESPONSE and invoke callback.
Returns the updated `hitl-question' struct."
  (let ((q (hitl-get id)))
    (unless q
      (user-error "HITL question `%s' not found" id))
    (unless (eq (hitl-question-status q) 'pending)
      (user-error "HITL question `%s' is not pending (status: %s)"
                  id (hitl-question-status q)))
    (setf (hitl-question-status q) 'answered)
    (setf (hitl-question-response q) response)
    (setf (hitl-question-answered-at q) (float-time))
    (when-let* ((cb (hitl-question-callback q)))
      (condition-case err
          (if (functionp cb)
              (funcall cb response q))
        (error (message "HITL callback error: %S" err))))
    (run-hook-with-args 'hitl-on-question-answered-functions q response)
    q))

;;;###autoload
(defun hitl-cancel (id &optional reason)
  "Mark question ID as cancelled with optional REASON string.
Returns the updated `hitl-question' struct."
  (let ((q (hitl-get id)))
    (when q
      (setf (hitl-question-status q) 'cancelled)
      (when reason
        (setf (hitl-question-response q) (format "Cancelled: %s" reason)))
      (run-hook-with-args 'hitl-on-question-cancelled-functions q reason)
      q)))

;;;###autoload
(defun hitl-expire (id)
  "Mark question ID as expired.
Returns the updated `hitl-question' struct."
  (let ((q (hitl-get id)))
    (when (and q (eq (hitl-question-status q) 'pending))
      (setf (hitl-question-status q) 'expired)
      (setf (hitl-question-response q) "Expired")
      (run-hook-with-args 'hitl-on-question-expired-functions q)
      q)))

;;;###autoload
(defun hitl-check-expirations ()
  "Check and expire questions that have exceeded their timeout."
  (let ((now (float-time))
        (expired nil))
    (maphash
     (lambda (id q)
       (when (and (eq (hitl-question-status q) 'pending)
                  (hitl-question-timeout q)
                  (> now (+ (hitl-question-created-at q) (hitl-question-timeout q))))
         (push id expired)))
     hitl--store)
    (dolist (id expired)
      (hitl-expire id))
    expired))

;;;###autoload
(defun hitl-clear-store ()
  "Clear all questions from the in-memory store."
  (clrhash hitl--store))

(require 'hitl-prompt)
(require 'hitl-cursor)
(require 'hitl-ui)

(provide 'hitl)
;;; hitl.el ends here
