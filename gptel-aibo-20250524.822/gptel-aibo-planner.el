;;; gptel-aibo-planner.el --- Action executor -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Sun Yi Ming
;;
;; Author: Sun Yi Ming <dolmens@gmail.com>
;; Keywords: emacs tools editing gptel ai assistant code-completion productivity

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Action executor for gptel-aibo

;;; Code:

(declare-function gptel-aibo-make-action-org-parser "gptel-aibo-action-org-parser")
(declare-function gptel-aibo-make-action-parser "gptel-aibo-action-parser")

(require 'gptel-aibo-action)

(defun gptel-aibo-apply-last-suggestions ()
  "Parse and apply the last LLM response in current buffer.

This function searches backward from the end of buffer for the last
GPT response (marked with ''gptel ''response text property), extracts
its content, and applies any valid operations found in the response.

See `gptel-aibo--apply-suggestions' for implementation details."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if-let* ((prop (text-property-search-backward 'gptel 'response t))
              (beg (prop-match-beginning prop))
              (end (prop-match-end prop)))
        (gptel-aibo--apply-suggestions-in-region beg end)
      (message "No response found."))))

(defun gptel-aibo--apply-suggestions-in-region (beg end &optional ignore-empty)
  "Parse and apply GPT suggestions in region between BEG and END.

If IGNORE-EMPTY is non-nil, suppress the empty response message.
See `gptel-aibo--apply-suggestions' for implementation details."
  (if (> end beg)
      (save-excursion
        (goto-char beg)
        (let ((working-buffer
               (and (text-property-search-backward 'gptaiu)
                    (get-text-property (point) 'gptaiu))))
          (cond
           ((not working-buffer)
            (message "The original request is missing."))
           ((not (bufferp working-buffer))
            (message "The request's working-buffer appears to be invalid."))
           ((and (not gptel-aibo--working-project)
                 (not (buffer-live-p working-buffer)))
            (message "The request's working-buffer has been closed."))
           (t
            (let ((response (buffer-substring-no-properties beg end)))
              (gptel-aibo--apply-suggestions
               response
               (when (buffer-live-p working-buffer)
                 working-buffer)))))))
    (unless ignore-empty
      (message "Empty response."))))

(defun gptel-aibo--apply-suggestions
    (response &optional working-buffer ignore-empty)
  "Parse the RESPONSE for OP commands and apply the actions.

Optional WORKING-BUFFER specifies the current buffer where operations are
applied. If IGNORE-EMPTY is non-nil, suppress the empty response message."
  (setq gptel-aibo--delete-confirmation nil)
  (let* ((parser (cond
                  ((derived-mode-p 'org-mode)
                   (require 'gptel-aibo-action-org-parser)
                   (gptel-aibo-make-action-org-parser))
                  (t
                   (require 'gptel-aibo-action-parser)
                   (gptel-aibo-make-action-parser))))
         (parse-result (gptel-aibo-parse-action parser response)))
    (cond
     ((eq (car parse-result) 'error)
      (message "Error parsing suggestions: %s" (cadr parse-result)))
     ((null parse-result)
      (unless ignore-empty
        (message "No operations found in response")))
     (t
      (with-current-buffer (or working-buffer (current-buffer))
        (condition-case err
            (progn
              (message "Applying OPs[dry-run]...")
              (dolist (op parse-result)
                (gptel-aibo-execute op 'dry-run))
              (message "Applying OPs...")
              (dolist (op parse-result)
                (gptel-aibo-execute op))
              (message "All operations applied successfully."))
          (error
           (message "Error applying OPs: %s" (error-message-string err)))))))))

(provide 'gptel-aibo-planner)
;;; gptel-aibo-planner.el ends here
