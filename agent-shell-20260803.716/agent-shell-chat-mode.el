;;; agent-shell-chat-mode.el --- Chat-style labels for agent-shell. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

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
;; `agent-shell-chat-mode' relabels the shell so it reads like a chat:
;; each submitted user turn is boxed `Me' and each response is boxed with
;; the agent's name.  Labels are overlays, so the buffer text is untouched
;; (a `display' overlay replaces the visible comint prompt for `Me', and a
;; `before-string' overlay renders the agent label at the invisible
;; `<shell-maker-end-of-prompt>' marker).
;;
;; The live prompt awaiting input shows `Me' too, so you can type straight
;; into the shell.  When `agent-shell-prompt-bar-mode' is enabled, input
;; flows through that bar instead, so the live prompt is hidden.
;;
;; Toggle it with `M-x agent-shell-chat-mode'.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'map)
(require 'seq)
(eval-when-compile (require 'subr-x))

(defvar agent-shell--state)
(defvar agent-shell-section-functions)
;; Soft reference: `agent-shell-prompt-bar-mode' may be unbound when the
;; prompt bar is not loaded.  Read it with `bound-and-true-p'.
(defvar agent-shell-prompt-bar-mode)

;; Forward-declared: used before the `define-minor-mode' at the end.
(defvar agent-shell-chat-mode)

;;; Faces

(defface agent-shell-chat-me-label
  '((t :inherit (bold font-lock-keyword-face) :inverse-video t :box t))
  "Face for the user (\"Me\") chat label.
`:inverse-video' fills the badge with the foreground color (text inverts
to the background); `:box' t adds a border in the foreground color."
  :group 'agent-shell)

(defface agent-shell-chat-agent-label
  '((t :inherit (bold font-lock-function-name-face) :inverse-video t :box t))
  "Face for the agent chat label.
`:inverse-video' fills the badge with the foreground color (text inverts
to the background); `:box' t adds a border in the foreground color."
  :group 'agent-shell)

;;; State

(defvar-local agent-shell-chat--labeled nil
  "Non-nil once chat labels have been applied to this shell buffer.")

;;; Labels

(defun agent-shell-chat--label (text face)
  "Return TEXT padded and propertized with FACE, as a chat label.

FACE carries the box (see `agent-shell-chat-me-label').

For example, (agent-shell-chat--label \"Me\" \\='agent-shell-chat-me-label)
returns \" Me \" in that face."
  (propertize (format " %s " text) 'face face))

(defun agent-shell-chat--agent-name ()
  "Return the attached agent's display name for the response label.

For example, with a mode-line name of \"Claude\" returns \"Claude\";
with none available, returns \"Agent\"."
  (or (map-nested-elt agent-shell--state '(:agent-config :mode-line-name))
      "Agent"))

(defun agent-shell-chat--prompt-face-p (value)
  "Return non-nil when a `font-lock-face' VALUE marks a shell prompt.
The prompt run carries `comint-highlight-prompt', possibly repeated.

For example, both \\='comint-highlight-prompt and
\\='(comint-highlight-prompt comint-highlight-prompt) return non-nil,
while \\='default returns nil."
  (or (eq value 'comint-highlight-prompt)
      (and (listp value) (memq 'comint-highlight-prompt value))))

(defun agent-shell-chat--overlay-in (beg end category)
  "Return an existing label overlay of CATEGORY between BEG and END, or nil."
  (seq-find (lambda (overlay) (eq (overlay-get overlay 'category) category))
            (overlays-in beg (max end (1+ beg)))))

(defun agent-shell-chat--label-prompts ()
  "Overlay each prompt run in the current buffer.

A prompt with non-empty input after it (a submitted turn) shows a `Me'
label.  The live prompt awaiting input has empty input: it also shows
`Me' so it can be typed into, unless `agent-shell-prompt-bar-mode' is on,
in which case input flows through the bar and the empty prompt is hidden.

Keys off the input, not the `<shell-maker-end-of-prompt>' marker (which
only appears once the response starts), so `Me' shows the instant a
prompt is submitted.

For a submitted prompt the overlay also swallows the input's leading
blank lines, so the label sits exactly one line above its text no matter
how much padding the shell inserted.  Updates the overlay in place, so a
prompt flips between hidden and `Me' as soon as its input, or the bar,
changes."
  (save-excursion
    (let ((pos (point-min)))
      (while (< pos (point-max))
        (let ((next (or (next-single-property-change pos 'font-lock-face) (point-max))))
          (when (agent-shell-chat--prompt-face-p
                 (get-text-property pos 'font-lock-face))
            (let* ((input-end (or (save-excursion
                                    (goto-char next)
                                    (when (re-search-forward
                                           "<shell-maker-end-of-prompt>" nil t)
                                      (match-beginning 0)))
                                  (point-max)))
                   (blank (string-blank-p (buffer-substring-no-properties next input-end)))
                   (end (if blank next
                          (save-excursion (goto-char next)
                                          (skip-chars-forward " \t\n")
                                          (point))))
                   (display (if (and blank (bound-and-true-p agent-shell-prompt-bar-mode))
                                ""
                              (concat (agent-shell-chat--label
                                       "Me" 'agent-shell-chat-me-label)
                                      "\n\n ")))
                   (overlay (agent-shell-chat--overlay-in
                             pos next 'agent-shell-chat-me)))
              (if overlay
                  (progn
                    (unless (and (= (overlay-start overlay) pos)
                                 (= (overlay-end overlay) end))
                      (move-overlay overlay pos end))
                    (unless (equal (overlay-get overlay 'display) display)
                      (overlay-put overlay 'display display)))
                (setq overlay (make-overlay pos end))
                (overlay-put overlay 'category 'agent-shell-chat-me)
                (overlay-put overlay 'evaporate t)
                (overlay-put overlay 'display display))))
          (setq pos next))))))

(defun agent-shell-chat--label-responses ()
  "Overlay the agent label before every response in the current buffer.
Anchored on the invisible `<shell-maker-end-of-prompt>' marker, whose
region is stable; the `before-string' renders even though the marker
text itself stays invisible.  Idempotent."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<shell-maker-end-of-prompt>" nil t)
      (unless (agent-shell-chat--overlay-in
               (match-beginning 0) (match-end 0) 'agent-shell-chat-agent)
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put overlay 'category 'agent-shell-chat-agent)
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'before-string
                       (concat "\n" (agent-shell-chat--label
                                     (agent-shell-chat--agent-name)
                                     'agent-shell-chat-agent-label)
                               "\n ")))))))

(defun agent-shell-chat--relabel ()
  "Apply the `Me' and agent labels to the current buffer (idempotent).
Scans the whole buffer; cheap in practice since it walks property
changes and skips already-labeled runs, but could be scoped to the
active turn if it ever shows on very long conversations."
  (agent-shell-chat--label-prompts)
  (agent-shell-chat--label-responses))

(defun agent-shell-chat--relabel-all ()
  "Relabel every labeled shell buffer.
Used after `agent-shell-prompt-bar-mode' toggles, so the live prompt
flips between hidden and `Me' immediately across all shells."
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'agent-shell-chat--labeled buffer)
      (with-current-buffer buffer
        (agent-shell-chat--relabel)))))

(defun agent-shell-chat--relabel-current (&rest _)
  "Relabel the current buffer if it carries chat labels.

Added to both `agent-shell-section-functions' (mid-stream; its range
argument is ignored) and `shell-maker-finish-output-hook' (which runs in
the shell buffer after a turn, error, init, or `clear' brings the prompt
back)."
  (when agent-shell-chat--labeled
    (agent-shell-chat--relabel)))

(defun agent-shell-chat--enable-labels (buffer)
  "Turn on chat labels for shell BUFFER and backfill existing turns.

Live updates then come from `agent-shell-section-functions' (mid-stream)
and `shell-maker-finish-output-hook' (turn completion, clear, init)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless agent-shell-chat--labeled
        (setq-local agent-shell-chat--labeled t)
        (agent-shell-chat--relabel)))))

(defun agent-shell-chat--on-shell-init ()
  "Enable chat labels for a newly initialized shell buffer.
Added to `agent-shell-mode-hook', so shells created while the mode is on
are labeled too."
  (agent-shell-chat--enable-labels (current-buffer)))

(defun agent-shell-chat--label-all-shells ()
  "Enable chat labels for every existing `agent-shell' buffer."
  (dolist (buffer (buffer-list))
    (when (provided-mode-derived-p
           (buffer-local-value 'major-mode buffer) 'agent-shell-mode)
      (agent-shell-chat--enable-labels buffer))))

(defun agent-shell-chat--unlabel-all ()
  "Remove chat labels from every buffer that carries them."
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'agent-shell-chat--labeled buffer)
      (with-current-buffer buffer
        (remove-overlays (point-min) (point-max)
                         'category 'agent-shell-chat-me)
        (remove-overlays (point-min) (point-max)
                         'category 'agent-shell-chat-agent)
        (kill-local-variable 'agent-shell-chat--labeled)))))

;;; Mode

;;;###autoload
(define-minor-mode agent-shell-chat-mode
  "Toggle chat-style `Me'/agent labels in every `agent-shell' buffer.

Each submitted turn is boxed `Me' and each response the agent's name.
The live prompt shows `Me' so you can type into the shell; when
`agent-shell-prompt-bar-mode' is on it is hidden, since input flows
through the bar instead."
  :global t
  :lighter nil
  :group 'agent-shell
  (if agent-shell-chat-mode
      (progn
        (add-hook 'agent-shell-section-functions #'agent-shell-chat--relabel-current)
        (add-hook 'shell-maker-finish-output-hook #'agent-shell-chat--relabel-current)
        (add-hook 'agent-shell-mode-hook #'agent-shell-chat--on-shell-init)
        (agent-shell-chat--label-all-shells))
    (remove-hook 'agent-shell-section-functions #'agent-shell-chat--relabel-current)
    (remove-hook 'shell-maker-finish-output-hook #'agent-shell-chat--relabel-current)
    (remove-hook 'agent-shell-mode-hook #'agent-shell-chat--on-shell-init)
    (agent-shell-chat--unlabel-all)))

(provide 'agent-shell-chat-mode)

;;; agent-shell-chat-mode.el ends here
