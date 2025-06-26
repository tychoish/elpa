;;; gptel-aibo.el --- An AI Writing Assistant -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Sun Yi Ming
;;
;; Author: Sun Yi Ming <dolmens@gmail.com>
;; Maintainer: Sun Yi Ming <dolmens@gmail.com>
;; Created: January 09, 2025
;; Modified: January 09, 2025
;; Package-Version: 20250605.859
;; Package-Revision: 36a5b96332c8
;; Keywords: emacs tools editing gptel ai assistant code-completion productivity
;; Homepage: https://github.com/dolmens/gptel-aibo
;; Package-Requires: ((emacs "27.1") (gptel "0.9.7"))

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

;;  gptel-aibo is an AI-powered writing assistant that helps users create and
;;  manage content in Emacs, including programs, documents, papers, and novels.
;;  It provides various operations like buffer modifications, file creation and
;;  deletion.

;;; Code:

(defvar-local gptel-aibo--old-directives nil)

(defvar-local gptel-aibo--old-system-message nil)

(defvar-local gptel-aibo--old-use-context nil)

(defvar-local gptel-aibo--from-gptel-mode nil
  "If this from `gptel-mode'.")

(require 'gptel)
(require 'gptel-context)
(require 'gptel-aibo-context)
(require 'gptel-aibo-planner)
(require 'gptel-aibo-summon)
(require 'gptel-aibo-face)

(defcustom gptel-aibo-default-mode nil
  "Default major mode for `gptel-aibo' console buffers.
If nil, use `gptel-default-mode' instead.
Should be a function that turns on a major mode."
  :type '(choice (const nil) function)
  :group 'gptel-aibo)

(defcustom gptel-aibo-prompt-prefix-alist
  '((markdown-mode . "\\> ")
    (text-mode . "\\> "))
  "String used as a prefix to the query being sent to the LLM.

This is meant for the user to distinguish between queries and
responses, and is removed from the query before it is sent.

This is an alist mapping major modes to the prefix strings.  This
is only inserted in `gptel-aibo' console buffers."
  :type '(alist :key-type symbol :value-type string)
  :group 'gptel-aibo)

(defvar gptel-aibo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'gptel-aibo-send)
    (define-key map (kbd "C-c <return>") #'gptel-aibo-send)
    (define-key map (kbd "C-c !") #'gptel-aibo-apply-last-suggestions)
    map)
  "Keymap for `gptel-aibo-mode'.")

;;;###autoload
(define-minor-mode gptel-aibo-mode
  "Minor mode for `gptel-aibo' interacting with LLMs."
  :lighter " GPTel-Aibo"
  :keymap gptel-aibo-mode-map
  (if gptel-aibo-mode
      (progn
        (if gptel-mode
            (setq gptel-aibo--from-gptel-mode gptel-mode)
          (gptel-mode 1))
        (setq gptel-aibo--old-directives gptel-directives)
        (setq-local gptel-directives (cons `(Aibo . ,gptel-aibo--system-message)
                                           gptel-directives))
        (setq gptel-aibo--old-system-message gptel--system-message)
        (setq-local gptel--system-message gptel-aibo--system-message)
        (setq gptel-aibo--old-use-context gptel-use-context)
        (setq-local gptel-use-context 'user)
        (when gptel-use-header-line
          (setq header-line-format
                (cons '(:eval (concat
                               (propertize " " 'display '(space :align-to 0))
                               "<"
                               (buffer-name gptel-aibo--working-buffer)
                               ">"))
                      (cdr header-line-format))))
        (add-hook 'window-selection-change-functions
                  #'gptel-aibo--window-selection-change nil t)
        (font-lock-add-keywords nil (gptel-aibo-op-generate-keywords))
        (message "gptel-aibo-mode enabled"))
    (remove-hook 'window-selection-change-functions
                 #'gptel-aibo--window-selection-change)
    (font-lock-remove-keywords nil (gptel-aibo-op-generate-keywords))
    (setq-local gptel-directives gptel-aibo--old-directives)
    (setq-local gptel--system-message gptel-aibo--old-system-message)
    (setq-local gptel-use-context gptel-aibo--old-use-context)
    (unless gptel-aibo--from-gptel-mode
      (gptel-mode -1))
    (message "gptel-aibo-mode disabled")))

(defun gptel-aibo--transform-add-context (callback fsm)
  "Transform context use buffer in FSM, call CALLBACK in the last."
  (when gptel-use-context
    (let ((data-buffer (plist-get (gptel-fsm-info fsm) :data))
          (console-buffer (plist-get (gptel-fsm-info fsm) :buffer)))
      (with-current-buffer data-buffer
        (thread-last (gptel-context--collect)
                     (gptel-aibo-context-string console-buffer)
                     (gptel-context--wrap-in-buffer)))))
  (funcall callback))

(defun gptel-aibo-context-string (console-buffer context-alist)
  "Format context string from CONTEXT-ALIST with CONSOLE-BUFFER."
  (let* ((header "---

Request context:

**Note**: This context reflects the user's **most recent** working state. If
there is a conflict with inferred content, the context takes precedence, as
previous suggested actions may not have been executed and the user may have made
changes outside this conversation.

")
         (working-context (gptel-aibo-context-info
                           (buffer-local-value
                            'gptel-aibo--working-buffer
                            console-buffer)))
         (other-context (gptel-context--string context-alist)))
    (concat header
            working-context
            (when (and other-context (not (string-empty-p other-context)))
              (concat "\n\nOther " other-context)))))


(defun gptel-aibo--window-selection-change (window)
  "Handle window selection change to update working buffer and project.

WINDOW is the newly selected window."
  (when (eq window (selected-window))
    (if gptel-aibo--trigger-buffer
        (progn
          (unless (eq gptel-aibo--working-buffer gptel-aibo--trigger-buffer)
            (setq gptel-aibo--working-buffer gptel-aibo--trigger-buffer))
          (setq gptel-aibo--trigger-buffer nil))
      (if-let* ((windows (cdr (window-list)))
                (sorted-windows
                 (sort windows
                       (lambda (w1 w2)
                         (> (window-use-time w1)
                            (window-use-time w2)))))
                (top-win
                 (if (not gptel-aibo--working-project)
                     (car sorted-windows)
                   (seq-find
                    (lambda (win)
                      (when-let* ((buf (window-buffer win))
                                  (project (with-current-buffer buf
                                             (project-current)))
                                  (project-root-dir
                                   (gptel-aibo--project-root project)))
                        (equal gptel-aibo--working-project
                               project-root-dir)))
                    sorted-windows)))
                (working-buffer (window-buffer top-win)))
          (unless (eq gptel-aibo--working-buffer working-buffer)
            (setq gptel-aibo--working-buffer working-buffer))))))

(defun gptel-aibo--get-console ()
  "Retrieve a console matching current buffer."
  (if-let ((project (project-current)))
      (gptel-aibo--get-project-console project)
    (get-buffer-create "*gptel-aibo*")))

(defun gptel-aibo--get-project-console (project)
  "Retrieve a console matching the PROJECT."
  (let*  ((project-root-dir (gptel-aibo--project-root project))
          (project-readable-name (gptel-aibo--project-name project))
          (base-name (format "*gptel-aibo: %s*" project-readable-name)))
    (if-let* ((buffer-candidate (get-buffer base-name))
              ((equal (buffer-local-value 'gptel-aibo--working-project
                                          buffer-candidate)
                      project-root-dir)))
        buffer-candidate
      (gptel-aibo--get-project-match-console base-name project-root-dir))))

(defun gptel-aibo--get-project-match-console (base-name project-root-dir)
  "Retrieve a buffer matching BASE-NAME and PROJECT-ROOT-DIR."
  (let ((name-pattern (format "^%s<\\([0-9]+\\)>$" (regexp-quote base-name))))
    (if-let ((matching-buffer
              (seq-find
               (lambda (buffer)
                 (and (string-match name-pattern (buffer-name buffer))
                      (equal (buffer-local-value 'gptel-aibo--working-project
                                                 buffer)
                             project-root-dir)))
               (buffer-list))))
        matching-buffer
      (generate-new-buffer base-name))))

;;;###autoload
(defun gptel-aibo (&optional buffer)
  "Open or initialize a GPTEL-AIBO console buffer.

If called interactively with a prefix argument, prompt for a project-specific
console buffer or create a new one. Sets up the buffer to use `gptel-aibo-mode`
and handles project-specific configurations if applicable. Displays the console
buffer after initialization.

Optional argument BUFFER specifies the name of the buffer to manage."
  (interactive
   (list
    (and current-prefix-arg
         (let ((project-root-dir (when-let ((project (project-current)))
                                   (gptel-aibo--project-root project))))
           (read-buffer
            "Create or choose gptel-aibo console: "
            nil nil
            (lambda (b)
              (and-let*
                  ((buf (get-buffer (or (car-safe b) b)))
                   ((buffer-local-value 'gptel-aibo-mode buf))
                   ((equal
                     (buffer-local-value 'gptel-aibo--working-project buf)
                     project-root-dir))))))))))
  (let ((trigger-buffer (current-buffer))
        (console-buffer
         (if buffer (get-buffer-create buffer)
           (gptel-aibo--get-console))))
    (with-current-buffer console-buffer
      (let ((console-mode (or gptel-aibo-default-mode gptel-default-mode)))
        (cond ;Set major mode
         ((eq major-mode console-mode))
         ((eq console-mode 'text-mode)
          (text-mode)
          (visual-line-mode 1))
         (t (funcall console-mode))))
      (unless (local-variable-p 'gptel-prompt-prefix-alist)
        (setq-local gptel-prompt-prefix-alist (gptel-aibo--merge-alists
                                               gptel-aibo-prompt-prefix-alist
                                               gptel-prompt-prefix-alist)))
      (when-let* (((not gptel-aibo--working-project))
                  (current-project (project-current))
                  (project-root-dir (gptel-aibo--project-root current-project)))
        (setq gptel-aibo--working-project project-root-dir))
      (unless gptel-aibo-mode (gptel-aibo-mode 1))
      (unless (local-variable-p 'gptel-aibo--console)
        (setq-local gptel-aibo--console t)
        (if (bobp) (insert (gptel-prompt-prefix-string)))
        (when (and (bound-and-true-p evil-local-mode)
                   (fboundp 'evil-insert-state))
          (evil-insert-state)))
      (when (called-interactively-p 'any)
        (setq gptel-aibo--trigger-buffer trigger-buffer)
        (display-buffer (current-buffer) gptel-display-buffer-action)))))

(defun gptel-aibo--merge-alists (a1 a2)
  "Merge two alists A1 and A2.
Keys from A1 take precedence and appear first in the result.
Returns a new alist containing all unique keys."
  (let* ((a2-filtered (cl-remove-if (lambda (pair)
                                      (assoc (car pair) a1))
                                    a2)))
    (append a1 a2-filtered)))

(defcustom gptel-aibo-auto-apply nil
  "If non-nil, automatically apply LLM-returned suggestions."
  :type 'boolean
  :group 'gptel-aibo)

(defun gptel-aibo--auto-apply (beg end)
  "Automatically apply suggestions in the region (BEG END) if it's non-empty."
  (when (> end beg)
    (gptel-aibo--apply-suggestions-in-region beg end 'ignore)))

;;;###autoload
(defun gptel-aibo-send ()
  "Send the current context and request to GPT for processing."
  (interactive)

  ;; HACK: gptel requires a non-empty context alist for context wrapping.
  (unless gptel-context--alist
    (setq gptel-context--alist (list (cons gptel-aibo--working-buffer nil))))

  (if gptel-aibo-auto-apply
      (add-hook 'gptel-post-response-functions
                #'gptel-aibo--auto-apply nil t)
    (remove-hook 'gptel-post-response-functions
                 #'gptel-aibo--auto-apply t))

  (if (save-excursion
        (when (re-search-backward "\\S-" nil t)
          (forward-char)
          (let ((end (point))
                (beg))
            (unless (text-property-search-backward 'gptel 'response)
              (goto-char (point-min)))
            (re-search-forward "\\S-" nil t)
            (backward-char)
            (setq beg (point))
            (add-text-properties beg end `(gptaiu ,gptel-aibo--working-buffer)))
          t))
      (let ((gptel-prompt-transform-functions
             '(gptel--transform-apply-preset gptel-aibo--transform-add-context)))
        (gptel-send))
    (message "Empty input received. Could you please type something?")))

(defun gptel-aibo--handle-post-insert (fsm)
  "Handle post-insert operations for FSM.

Add text property ''gptai to the response."
  (let* ((info (gptel-fsm-info fsm))
         (working-context (plist-get info :context))
         (working-buffer (plist-get working-context :working-buffer))
         (start-marker (plist-get info :position))
         (tracking-marker (or (plist-get info :tracking-marker)
                              start-marker))
         ;; start-marker may have been moved if :buffer was read-only
         (gptel-buffer (marker-buffer start-marker)))
    (unless (eq start-marker tracking-marker)
      (with-current-buffer gptel-buffer
        (add-text-properties start-marker tracking-marker
                             `(gptai ,working-buffer))))))

(defun gptel-aibo--insert-response (response info)
  "Insert the LLM RESPONSE into the gptel buffer.

INFO is a plist containing information relevant to this buffer.
See `gptel--url-get-response' for details."
  (let* ((gptel-buffer (plist-get info :buffer))
         (working-context (plist-get info :context))
         (working-buffer (plist-get working-context :working-buffer))
         (start-marker (plist-get info :position))
         (tracking-marker (plist-get info :tracking-marker)))
    (cond
     ((stringp response)                ;Response text
      (with-current-buffer gptel-buffer
        (when-let* ((transformer (plist-get info :transformer)))
          (setq response (funcall transformer response)))
        (when tracking-marker           ;separate from previous response
          (setq response (concat "\n\n" response)))
        (save-excursion
          (add-text-properties
           0 (length response) `(gptel response
                                 gptai ,working-buffer
                                 front-sticky (gptel gptai))
           response)
          (with-current-buffer (marker-buffer start-marker)
            (goto-char (or tracking-marker start-marker))
            ;; (run-hooks 'gptel-pre-response-hook)
            (unless (or (bobp) (plist-get info :in-place)
                        tracking-marker)
              (insert "\n\n")
              (when gptel-mode
                (insert (gptel-response-prefix-string)))
              (move-marker start-marker (point)))
            (insert response)
            (plist-put info :tracking-marker
                       (setq tracking-marker (point-marker)))
            ;; for uniformity with streaming responses
            (set-marker-insertion-type tracking-marker t)))))
     ((consp response)                  ;tool call or tool result?
      (gptel--display-tool-calls response info)))))

(defun gptel-aibo--stream-insert-response (response info)
  "Insert streaming RESPONSE from an LLM into the gptel buffer.

INFO is a mutable plist containing information relevant to this buffer.
See `gptel--url-get-response' for details."
  (let* ((working-context (plist-get info :context))
         (working-buffer (plist-get working-context :working-buffer)))
    (cond
     ((stringp response)
      (let ((start-marker (plist-get info :position))
            (tracking-marker (plist-get info :tracking-marker))
            (transformer (plist-get info :transformer)))
        (with-current-buffer (marker-buffer start-marker)
          (save-excursion
            (unless tracking-marker
              (goto-char start-marker)
              (unless (or (bobp) (plist-get info :in-place))
                (insert "\n\n")
                (when gptel-mode
                  ;; Put prefix before AI response.
                  (insert (gptel-response-prefix-string)))
                (move-marker start-marker (point)))
              (setq tracking-marker (set-marker (make-marker) (point)))
              (set-marker-insertion-type tracking-marker t)
              (plist-put info :tracking-marker tracking-marker))

            (when transformer
              (setq response (funcall transformer response)))

            (add-text-properties
             0 (length response) `(gptel response
                                   gptai ,working-buffer
                                   front-sticky (gptel gptai))
             response)
            (goto-char tracking-marker)
            ;; (run-hooks 'gptel-pre-stream-hook)
            (insert response)
            (run-hooks 'gptel-post-stream-hook)))))
     ((consp response)
      (gptel--display-tool-calls response info)))))

;;;###autoload
(defun gptel-aibo-complete-at-point ()
  "Complete text at point using LLM suggestions.

The response is inserted as an overlay with these keybindings:
- TAB or RET: Accept and move to the end of the overlay.
- Any other key: Reject and execute its normal action."
  (interactive)
  (let ((gptel--system-message gptel-aibo--system-role)
        (prompt (concat (gptel-aibo-context-info) gptel-aibo--complete-message)))
    (message "Requesting LLM suggestions...")
    ;; (message prompt)
    (gptel-request prompt
      :position (point)
      :callback #'gptel-aibo--insert-completion)))

(defun gptel-aibo--insert-completion (response info)
  "Insert the LLM RESPONSE into the calling buffer.

INFO is a plist containing information relevant to this buffer.
See `gptel--url-get-response' for details."
  (cond
   ((stringp response)
    (message "LLM response received")
    (let ((marker (plist-get info :position)))
      (with-current-buffer (marker-buffer marker)
        (when (= (point) (marker-position marker))
          (save-excursion
            (let* ((beg (point))
                   (_ (insert response))
                   (end (point))
                   (ov (make-overlay beg end))
                   (map (make-sparse-keymap)))
              (overlay-put ov 'face 'gptel-aibo-completion-face)
              (overlay-put ov 'keymap map)

              (define-key map (kbd "TAB")
                          (lambda ()
                            (interactive)
                            (goto-char (overlay-end ov))
                            (delete-overlay ov)))
              (define-key map (kbd "<tab>")
                          (lambda ()
                            (interactive)
                            (goto-char (overlay-end ov))
                            (delete-overlay ov)))
              (define-key map (kbd "RET")
                          (lambda ()
                            (interactive)
                            (goto-char (overlay-end ov))
                            (delete-overlay ov)))
              (define-key map (kbd "<return>")
                          (lambda ()
                            (interactive)
                            (goto-char (overlay-end ov))
                            (delete-overlay ov)))
              (define-key map [t]
                          (lambda ()
                            (interactive)
                            (delete-region beg end)
                            (delete-overlay ov)
                            (let ((cmd (key-binding (this-command-keys-vector))))
                              (when cmd
                                (call-interactively cmd)))))))))))
   (t
    (message "The LLM did not respond as requested."))))

(defvar gptel-aibo-complete-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-i i") 'gptel-aibo-complete-at-point)
    map)
  "Keymap used for `gptel-aibo-complete-mode`.")

;;;###autoload
(define-minor-mode gptel-aibo-complete-mode
  "Minor mode `gptel-aibo' llm completions."
  :lighter " GPTel-Aibo-Complete"
  :keymap gptel-aibo-complete-mode-map)

(provide 'gptel-aibo)
;;; gptel-aibo.el ends here
