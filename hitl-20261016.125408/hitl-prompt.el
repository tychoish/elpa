;;; hitl-prompt.el --- Minibuffer prompters for HITL questions -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Keywords: tools, convenience, agent, ai
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides interactive minibuffer prompting widgets for `hitl.el',
;; supporting boolean, confirm, single-choice, multi-choice, text, file,
;; and numeric question kinds.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'annotated-completing-read nil t)

(declare-function hitl-get "hitl")
(declare-function hitl-answer "hitl")
(declare-function hitl-list-pending "hitl")
(declare-function hitl-question-id "hitl")
(declare-function hitl-question-prompt "hitl")
(declare-function hitl-question-kind "hitl")
(declare-function hitl-question-options "hitl")
(declare-function hitl-question-default-value "hitl")

;;;###autoload
(defun hitl-prompt-question (q)
  "Prompt the user interactively for `hitl-question' Q and return the response."
  (let* ((kind (hitl-question-kind q))
         (prompt (format "[HITL] %s " (hitl-question-prompt q)))
         (options (hitl-question-options q))
         (def (hitl-question-default-value q)))
    (pcase kind
      ((or :boolean 'boolean)
       (y-or-n-p prompt))
      ((or :confirm 'confirm)
       (let* ((expected (or def "yes"))
              (confirm-prompt (format "%s(type '%s' to confirm): " prompt expected))
              (val (read-string confirm-prompt)))
         (equal (string-trim val) expected)))
      ((or :single-choice 'single-choice)
       (if (and (fboundp 'annotated-completing-read) options)
           (annotated-completing-read options :prompt prompt :default def)
         (completing-read prompt options nil t nil nil def)))
      ((or :multi-choice 'multi-choice)
       (if (and (fboundp 'annotated-completing-read) options)
           (annotated-completing-read options :prompt prompt :multiple t :default def)
         (completing-read-multiple prompt options nil t nil nil def)))
      ((or :file 'file)
       (read-file-name prompt (or def default-directory)))
      ((or :number 'number)
       (read-number prompt (if (numberp def) def 0)))
      (_
       (read-string prompt def)))))

;;;###autoload
(defun hitl-prompt (&optional question-id)
  "Interactively prompt user to answer a pending HITL question.
If QUESTION-ID is provided, answer that specific question;
otherwise select from all pending questions."
  (interactive)
  (let* ((pending (unless question-id (hitl-list-pending)))
         (q (cond
             (question-id (hitl-get question-id))
             ((null pending) (user-error "No pending HITL questions"))
             ((= (length pending) 1) (car pending))
             (t
              (let* ((table (mapcar (lambda (item)
                                      (cons (format "[%s] %s"
                                                    (hitl-question-id item)
                                                    (hitl-question-prompt item))
                                            item))
                                    pending))
                     (choice (completing-read "Select question to answer: " table nil t)))
                (cdr (assoc choice table)))))))
    (when q
      (let ((resp (hitl-prompt-question q)))
        (hitl-answer (hitl-question-id q) resp)
        (message "HITL Question %s answered." (hitl-question-id q))))))

(provide 'hitl-prompt)
;;; hitl-prompt.el ends here
