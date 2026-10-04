;;; hitl-ui.el --- Tabulated list buffer and indicator for HITL -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Keywords: tools, convenience, agent, ai
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tabulated list UI and modeline lighter for `hitl.el'.
;; Provides `hitl-list' (`*hitl-questions*') for interactive management
;; and `hitl-indicator-mode' for modeline status tracking.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function hitl-get "hitl")
(declare-function hitl-list-all "hitl")
(declare-function hitl-list-pending "hitl")
(declare-function hitl-cancel "hitl")
(declare-function hitl-prompt "hitl-prompt")
(declare-function hitl-question-id "hitl")
(declare-function hitl-question-prompt "hitl")
(declare-function hitl-question-kind "hitl")
(declare-function hitl-question-status "hitl")
(declare-function hitl-question-target "hitl")

(defgroup hitl-ui nil
  "User interface components for HITL."
  :group 'hitl)

(defface hitl-pending-face
  '((t :foreground "orange" :weight bold))
  "Face for pending HITL questions.")

(defface hitl-answered-face
  '((t :foreground "forestgreen"))
  "Face for answered HITL questions.")

(defface hitl-cancelled-face
  '((t :foreground "gray50" :slant italic))
  "Face for cancelled HITL questions.")

(defface hitl-expired-face
  '((t :foreground "firebrick" :slant italic))
  "Face for expired HITL questions.")

;;; Tabulated List Buffer (*hitl-questions*)

(defvar hitl-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'hitl-ui-answer-at-point)
    (define-key map (kbd "c") #'hitl-ui-cancel-at-point)
    (define-key map (kbd "d") #'hitl-ui-cancel-at-point)
    (define-key map (kbd "g") #'hitl-ui-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `hitl-list-mode'.")

(define-derived-mode hitl-list-mode tabulated-list-mode "HITL"
  "Major mode for browsing and responding to HITL questions."
  (setq tabulated-list-format
        [("ID" 16 t)
         ("Status" 12 t)
         ("Kind" 14 t)
         ("Target" 20 t)
         ("Prompt" 40 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun hitl-ui-refresh ()
  "Refresh the `*hitl-questions*' tabulated list buffer."
  (interactive)
  (let ((buf (get-buffer-create "*hitl-questions*")))
    (with-current-buffer buf
      (hitl-list-mode)
      (setq tabulated-list-entries
            (mapcar
             (lambda (q)
               (let* ((qid (hitl-question-id q))
                      (status (hitl-question-status q))
                      (kind (symbol-name (hitl-question-kind q)))
                      (target (or (hitl-question-target q) "-"))
                      (target-str (if (bufferp target) (buffer-name target) (format "%s" target)))
                      (prompt (hitl-question-prompt q))
                      (status-face (pcase status
                                     ('pending 'hitl-pending-face)
                                     ('answered 'hitl-answered-face)
                                     ('expired 'hitl-expired-face)
                                     (_ 'hitl-cancelled-face)))
                      (status-str (propertize (symbol-name status) 'face status-face)))
                 (list qid (vector qid status-str kind target-str prompt))))
             (hitl-list-all)))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

;;;###autoload
(defun hitl-list ()
  "Display the list of all HITL questions."
  (interactive)
  (hitl-ui-refresh))

(defalias 'hitl-list-questions #'hitl-list)

(defun hitl-ui-answer-at-point ()
  "Answer the question at point in the `*hitl-questions*' buffer."
  (interactive)
  (let ((qid (tabulated-list-get-id)))
    (when qid
      (hitl-prompt qid)
      (hitl-ui-refresh))))

(defun hitl-ui-cancel-at-point ()
  "Cancel the question at point in the `*hitl-questions*' buffer."
  (interactive)
  (let ((qid (tabulated-list-get-id)))
    (when qid
      (hitl-cancel qid "Cancelled from UI")
      (hitl-ui-refresh))))

;;; Modeline Indicator

(defvar hitl--indicator-string ""
  "Modeline string indicating pending HITL questions.")

(defun hitl--update-indicator (&rest _args)
  "Update `hitl--indicator-string' based on pending question count."
  (let ((count (length (hitl-list-pending))))
    (setq hitl--indicator-string
          (if (> count 0)
              (propertize (format " HITL[%d]" count)
                          'face 'hitl-pending-face
                          'help-echo "Click to view pending HITL questions"
                          'mouse-face 'mode-line-highlight
                          'local-map (let ((map (make-sparse-keymap)))
                                       (define-key map [mode-line mouse-1] #'hitl-list)
                                       map))
            ""))
    (force-mode-line-update t)))

;;;###autoload
(define-minor-mode hitl-indicator-mode
  "Global minor mode to show pending HITL question count in the mode line."
  :global t
  :lighter hitl--indicator-string
  (if hitl-indicator-mode
      (progn
        (add-hook 'hitl-on-question-created-functions #'hitl--update-indicator)
        (add-hook 'hitl-on-question-answered-functions #'hitl--update-indicator)
        (add-hook 'hitl-on-question-cancelled-functions #'hitl--update-indicator)
        (add-hook 'hitl-on-question-expired-functions #'hitl--update-indicator)
        (hitl--update-indicator))
    (remove-hook 'hitl-on-question-created-functions #'hitl--update-indicator)
    (remove-hook 'hitl-on-question-answered-functions #'hitl--update-indicator)
    (remove-hook 'hitl-on-question-cancelled-functions #'hitl--update-indicator)
    (remove-hook 'hitl-on-question-expired-functions #'hitl--update-indicator)
    (setq hitl--indicator-string "")
    (force-mode-line-update t)))

(provide 'hitl-ui)
;;; hitl-ui.el ends here
