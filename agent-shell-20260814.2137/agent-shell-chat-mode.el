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

(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")

(defvar agent-shell--state)
;; Soft reference: `agent-shell-prompt-bar-mode' may be unbound when the
;; prompt bar is not loaded.  Read it with `bound-and-true-p'.
(defvar agent-shell-prompt-bar-mode)

;; Forward-declared: used before the `define-minor-mode' at the end.
(defvar agent-shell-chat-mode)

;;; Customization

(defcustom agent-shell-chat-mode-enabled t
  "Whether a new agent shell enables `agent-shell-chat-mode' by default.
When non-nil, starting a shell turns the (global) chat mode on, so that
shell and any others render as a chat.  Toggling the mode off by hand is
overridden the next time a shell starts."
  :type 'boolean
  :group 'agent-shell)

;;; Constants

(defconst agent-shell-chat--prompt "❯ "
  "Prompt marker shown on the live shell prompt while it awaits input.
Cleared the instant the prompt is submitted, since the overlay then
renders the submitted turn instead.")

(defconst agent-shell-chat--body-indent "  "
  "Indent that lines the prompt input up with the response body.
Mirrors the two-column base `line-prefix' the response carries (see
`agent-shell-ui--indent-text'); hiding the comint prompt would otherwise
drop the input flush to column 0.")

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

(defvar-local agent-shell-chat--subscription nil
  "Event subscription token keeping chat labels in sync, or nil.")

(defvar-local agent-shell-chat--relabel-timer nil
  "Pending coalesced relabel timer for this buffer, or nil.")

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
A live prompt carries `comint-highlight-prompt'; a restored or echoed
prompt carries `agent-shell-prompt' (which inherits it).  Either may be
repeated.

For example, \\='comint-highlight-prompt, \\='agent-shell-prompt, and
\\='(comint-highlight-prompt comint-highlight-prompt) all return non-nil,
while \\='default returns nil."
  (let ((faces (if (listp value) value (list value))))
    (or (memq 'comint-highlight-prompt faces)
        (memq 'agent-shell-prompt faces))))

(defun agent-shell-chat--extends-bg-p (face)
  "Return non-nil when FACE paints an `:extend' background past end of line.
FACE is a `face' text-property value (a face symbol or list of them).  A
code block's padding carries such a face (`agent-shell-markdown-source-block'),
so this marks whitespace the prompt overlay must not swallow."
  (seq-some (lambda (f)
              (and (facep f) (eq (face-attribute f :extend nil t) t)))
            (if (proper-list-p face) face (list face))))

(defun agent-shell-chat--overlay-in (beg end category)
  "Return an existing label overlay of CATEGORY between BEG and END, or nil."
  (seq-find (lambda (overlay) (eq (overlay-get overlay 'category) category))
            (overlays-in beg (max end (1+ beg)))))

(defun agent-shell-chat--upsert-overlay (category anchor-beg anchor-end beg end props)
  "Ensure a CATEGORY overlay spans BEG..END carrying PROPS.

PROPS is an alist of overlay property to value.  Reuses an existing
CATEGORY overlay overlapping ANCHOR-BEG..ANCHOR-END (moving it when the
span changed), otherwise creates one."
  (let* ((existing (seq-filter (lambda (overlay)
                                 (eq (overlay-get overlay 'category) category))
                               (overlays-in anchor-beg (max anchor-end (1+ anchor-beg)))))
         (overlay (or (car existing)
                      (let ((created (make-overlay beg end)))
                        (overlay-put created 'category category)
                        (overlay-put created 'evaporate t)
                        created))))
    ;; Delete stray duplicates: relabels re-create an overlay whenever its
    ;; span has drifted outside the search range, so more than one can pile up.
    (dolist (extra (cdr existing))
      (delete-overlay extra))
    (unless (and (= (overlay-start overlay) beg) (= (overlay-end overlay) end))
      (move-overlay overlay beg end))
    (map-do (lambda (property value)
              (unless (equal (overlay-get overlay property) value)
                (overlay-put overlay property value)))
            props)))

(defun agent-shell-chat--prompt-runs ()
  "Return each prompt-face run in the current buffer as a list of (BEG . END).
In buffer order.  Consecutive prompts (e.g. an empty submission leaves a
stale prompt above the fresh one) are separate entries, since input or
whitespace separates them."
  (save-excursion
    (goto-char (point-min))
    (let ((runs '())
          (pos (point-min)))
      (while (< pos (point-max))
        (let ((run-end (or (next-single-property-change pos 'font-lock-face)
                           (point-max))))
          (when (agent-shell-chat--prompt-face-p
                 (get-text-property pos 'font-lock-face))
            (push (cons pos run-end) runs))
          (setq pos run-end)))
      (nreverse runs))))

(defun agent-shell-chat--label-prompts ()
  "Overlay each prompt run in the current buffer.

Every prompt run shows a `Me' label.  A submitted turn's input follows
as buffer text; an empty submission (RET on an empty prompt reprints a
fresh prompt below it) shows a bare `Me'.  The last prompt is the live
one and also shows the prompt marker so it can be typed into, unless
`agent-shell-prompt-bar-mode' is on (then input flows through the bar and
the live prompt is hidden).

Input is bounded by the next prompt, not just the
`<shell-maker-end-of-prompt>' marker (which only appears once a response
starts): `Me' shows the instant a prompt is submitted, and an empty
prompt does not claim the fresh prompt below as its input.

The overlay swallows the blank lines around the prompt so the label is
padded by exactly one blank line on each side.  Updates in place."
  (save-excursion
    (let ((runs (agent-shell-chat--prompt-runs))
          (prev-end nil))
      (while runs
        (let* ((pos (caar runs))
               (run-end (cdar runs))
               (next-pos (caadr runs))
               (limit (or next-pos (point-max)))
               (marker-pos (save-excursion
                             (goto-char run-end)
                             (when (re-search-forward
                                    "<shell-maker-end-of-prompt>" limit t)
                               (match-beginning 0))))
               (input-end (or marker-pos limit))
               (blank (string-blank-p
                       (buffer-substring-no-properties run-end input-end)))
               ;; The live prompt is the last one with no response yet: no
               ;; prompt and no end-of-prompt marker follow it.  (A submitted
               ;; turn awaiting its reprinted prompt is last but has a marker.)
               (live (and (null next-pos) (null marker-pos)))
               (raw-start (save-excursion
                            (goto-char pos)
                            (skip-chars-backward " \t\n")
                            ;; Do not swallow into the previous prompt run (an
                            ;; empty submission leaves a stale one right above).
                            (when (and prev-end (< (point) prev-end))
                              (goto-char prev-end))
                            ;; Leave a code block's tinted padding (an `:extend'
                            ;; background) to the panel: stepping back out of it
                            ;; keeps the overlay off those newlines, so the
                            ;; panel keeps its padding and cannot bleed across
                            ;; the label.
                            (while (and (< (point) pos)
                                        (agent-shell-chat--extends-bg-p
                                         (get-text-property (point) 'face)))
                              (forward-char 1))
                            (point)))
               ;; Classify what precedes the label: drives the leading pad and
               ;; whether a line terminator must be kept.
               (stacked (and prev-end (= raw-start prev-end)))
               (after-marker (and (> raw-start (point-min))
                                  (get-text-property (1- raw-start) 'shell-maker--marker)))
               (after-panel (and (> raw-start (point-min))
                                 (agent-shell-chat--extends-bg-p
                                  (get-text-property (1- raw-start) 'face))))
               ;; After response body (the normal case) keep that content's
               ;; line terminator visible: hiding it would merge the last
               ;; output line into the label for line motion (e.g.
               ;; `end-of-visual-line').  The other cases already leave a
               ;; visible terminator (a stale prompt, marker, or panel newline).
               (keep-term (and (not stacked) (not after-marker) (not after-panel)
                               (< raw-start pos) (eq (char-after raw-start) ?\n)))
               (start (if keep-term (1+ raw-start) raw-start))
               ;; The live prompt keeps its in-progress input (never swallow
               ;; it); a submitted turn swallows the input's leading blank lines.
               (end (if (or blank live) run-end
                      (save-excursion (goto-char run-end)
                                      (skip-chars-forward " \t\n")
                                      (point))))
               (me-label (agent-shell-chat--label
                          "Me" 'agent-shell-chat-me-label))
               ;; Face the padding and marker `default' so they do not inherit
               ;; the covered text's face: a display string's unfaced chars
               ;; take the face of the text they replace, and after a code
               ;; block that is the tinted source-block background.
               (pad (propertize "\n\n" 'face 'default))
               ;; Leading blank lines before the label.  `pad' (two newlines)
               ;; renders one blank line when `start' is mid-line after content.
               ;; Several cases need fewer, to keep it at exactly one:
               (lead (cond
                      ;; Directly stacked on the previous label (empty
                      ;; submissions in a row): its trailing `pad' already
                      ;; supplies the one blank line, so add none.
                      (stacked "")
                      ;; Directly after an (empty) agent response: the agent
                      ;; label's trailing pad supplies the one blank line.
                      (after-marker "")
                      ;; After a code block panel: its tinted padding already
                      ;; separates the label, so one newline keeps exactly one.
                      (after-panel (propertize "\n" 'face 'default))
                      ;; Terminator kept visible: it ends the content line, so
                      ;; one newline adds the single blank line.
                      (keep-term (propertize "\n" 'face 'default))
                      (t pad)))
               (before (cond
                        ;; Bar on hides the live prompt (input via the bar).
                        ((and live (bound-and-true-p agent-shell-prompt-bar-mode))
                         "")
                        ;; Live prompt (the active input line): `Me' and the
                        ;; prompt marker, whether or not text has been typed
                        ;; yet.  Keying this off `blank' would drop the marker
                        ;; the instant the user starts typing.
                        (live
                         (concat lead me-label pad
                                 (propertize
                                  (concat agent-shell-chat--body-indent
                                          agent-shell-chat--prompt)
                                  'face 'default)))
                        ;; An empty submission (blank but not the live prompt)
                        ;; is not labeled: only the live prompt shows an empty
                        ;; `Me'.
                        (blank "")
                        ;; A submitted turn: `Me', its input following as
                        ;; buffer text.
                        (t (concat lead me-label pad)))))
          (agent-shell-chat--upsert-overlay
           'agent-shell-chat-me pos run-end start end
           ;; Render the label as a `before-string' (like the agent label) and
           ;; hide the covered prompt with a `display' of \"\".  A `display'
           ;; string is backed by the covered buffer positions, so vertical
           ;; motion (`next-line'/`previous-line') lands the cursor on the
           ;; label; a `before-string' is not, so the cursor skips it.  Empty
           ;; `line-prefix'/`wrap-prefix' drop any tinted gutter inherited from
           ;; the covered text.
           (list (cons 'before-string before)
                 (cons 'display "")
                 (cons 'line-prefix "")
                 (cons 'wrap-prefix "")))
          ;; Indent a submitted turn's input so it aligns with the response
          ;; body.  The live prompt (input flows after the marker) and empty
          ;; prompts have no input to indent.
          (if (or blank live)
              (when-let* ((stale (agent-shell-chat--overlay-in
                                  end (1+ end) 'agent-shell-chat-me-input)))
                (delete-overlay stale))
            ;; End at the input's last real character, not `input-end': the
            ;; agent label's `before-string' renders in the trailing newline
            ;; before the marker, and would inherit this `line-prefix'.
            (let ((input-last (save-excursion (goto-char input-end)
                                              (skip-chars-backward " \t\n")
                                              (point))))
              (agent-shell-chat--upsert-overlay
               'agent-shell-chat-me-input end input-last end input-last
               (list (cons 'line-prefix agent-shell-chat--body-indent)
                     (cons 'wrap-prefix agent-shell-chat--body-indent)))))
          (setq prev-end run-end)
          (setq runs (cdr runs)))))))

(defun agent-shell-chat--label-responses ()
  "Overlay the agent label before every response in the current buffer.

Anchored on the invisible `<shell-maker-end-of-prompt>' marker.  Hides
the marker and the extra blank lines around it with a `display' of \"\",
and renders the label via a `before-string' padded by one blank line on
each side.  Keeps the input's line terminator visible: hiding that
newline would merge the input line into the response for line motion
\(e.g. `end-of-visual-line').  Updates in place; idempotent."
  (save-excursion
    (goto-char (point-min))
    (let ((label (agent-shell-chat--label
                  (agent-shell-chat--agent-name)
                  'agent-shell-chat-agent-label)))
      (while (re-search-forward "<shell-maker-end-of-prompt>" nil t)
        (let* ((mbeg (match-beginning 0))
               (mend (match-end 0))
               (end (save-excursion
                      (goto-char mend)
                      ;; Swallow the response's leading blank lines, but stop
                      ;; at a code block panel's tinted top padding (an
                      ;; `:extend' background) so the panel keeps its internal
                      ;; padding rather than having it hidden by `display'.
                      (while (and (< (point) (point-max))
                                  (memq (char-after) '(?\s ?\t ?\n))
                                  (not (agent-shell-chat--extends-bg-p
                                        (get-text-property (point) 'face))))
                        (forward-char 1))
                      ;; Do not swallow the whitespace before a following prompt
                      ;; (an empty response): it belongs to that prompt's
                      ;; spacing, and swallowing it would overlap the `Me'
                      ;; overlay.  Keep the label anchored at the marker.
                      (if (agent-shell-chat--prompt-face-p
                           (get-text-property (point) 'font-lock-face))
                          mend
                        (point))))
               ;; Start just past the input's line terminator: the first
               ;; newline after the input, whether it precedes the marker
               ;; (live turns: \"input\\n<marker>\") or follows it (restored
               ;; turns: \"input<marker>\\n\").  That newline stays visible
               ;; while extra blank lines are hidden; hiding it would merge
               ;; the input line into the response for line motion (e.g.
               ;; `end-of-visual-line').  Bounded by `end' so the search stops
               ;; before the response body.  The marker hides itself.
               (start (save-excursion
                        (goto-char mbeg)
                        (skip-chars-backward " \t\n")
                        (if (re-search-forward "\n" end t) (point) mbeg)))
               ;; With the terminator kept, one leading newline pads the label;
               ;; without one keep two.
               (before (concat (if (and (> start (point-min))
                                        (eq (char-before start) ?\n))
                                   "\n"
                                 "\n\n")
                               label "\n\n"))
               ;; A completed turn with no response text (a tool-only turn, or
               ;; a restored empty turn) is not labeled: another marker or the
               ;; next prompt follows the marker with only whitespace between.
               ;; A marker at end of buffer is the active, just-submitted turn
               ;; (its output has not streamed yet), so it IS labeled.
               (response-empty (save-excursion
                                 (goto-char mend)
                                 (skip-chars-forward " \t\n")
                                 (or (get-text-property (point) 'shell-maker--marker)
                                     (agent-shell-chat--prompt-face-p
                                      (get-text-property (point) 'font-lock-face))))))
          (if response-empty
              ;; Drop any label from a turn that no longer has a response.
              (dolist (overlay (overlays-in mbeg (max end (1+ mbeg))))
                (when (eq (overlay-get overlay 'category) 'agent-shell-chat-agent)
                  (delete-overlay overlay)))
            ;; Anchor the reuse search on the whole span, not just the marker:
            ;; a restored turn's overlay starts past the marker (keeping the
            ;; terminator visible), so a marker-only anchor would miss it and
            ;; pile up duplicates.
            (agent-shell-chat--upsert-overlay
             'agent-shell-chat-agent mbeg end start end
             (list (cons 'before-string before) (cons 'display "")))))))))

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

(defun agent-shell-chat--relabel-buffer (buffer)
  "Relabel BUFFER, clearing its pending relabel timer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq agent-shell-chat--relabel-timer nil)
      (agent-shell-chat--relabel))))

(defun agent-shell-chat--schedule-relabel (&rest _)
  "Schedule a coalesced, deferred relabel of the current buffer.

Deferred so the triggering change's own text properties (e.g. the prompt
face shell-maker applies after inserting) are in place; coalesced so a
burst yields a single relabel.  Runs from the event subscription (which
covers submissions, streaming, turn completion and `session-restored')
and from `shell-maker-finish-output-hook' (which covers `clear')."
  (when (and agent-shell-chat--labeled
             (not agent-shell-chat--relabel-timer))
    (setq agent-shell-chat--relabel-timer
          (run-at-time 0 nil #'agent-shell-chat--relabel-buffer (current-buffer)))))

(defun agent-shell-chat--enable ()
  "Turn on chat labels in the current buffer and keep them in sync.

Backfills existing turns, subscribes to shell events so a coalesced
relabel tracks submissions, streaming responses, turn completion and
reloads (`session-restored'), and adds a buffer-local
`shell-maker-finish-output-hook' so `clear' and the other internal
commands (which reprint the prompt with no `agent-shell' event) relabel
too."
  (unless agent-shell-chat--labeled
    (setq-local agent-shell-chat--labeled t)
    (agent-shell-chat--relabel)
    (setq-local agent-shell-chat--subscription
                (agent-shell-subscribe-to
                 :shell-buffer (current-buffer)
                 :on-event #'agent-shell-chat--schedule-relabel))
    (add-hook 'shell-maker-finish-output-hook
              #'agent-shell-chat--schedule-relabel nil t)))

(defun agent-shell-chat--disable ()
  "Remove chat labels, subscription, timer and hook from the current buffer."
  (remove-hook 'shell-maker-finish-output-hook
               #'agent-shell-chat--schedule-relabel t)
  (when agent-shell-chat--subscription
    (agent-shell-unsubscribe :subscription agent-shell-chat--subscription))
  (when (timerp agent-shell-chat--relabel-timer)
    (cancel-timer agent-shell-chat--relabel-timer))
  (remove-overlays (point-min) (point-max) 'category 'agent-shell-chat-me)
  (remove-overlays (point-min) (point-max) 'category 'agent-shell-chat-me-input)
  (remove-overlays (point-min) (point-max) 'category 'agent-shell-chat-agent)
  (kill-local-variable 'agent-shell-chat--subscription)
  (kill-local-variable 'agent-shell-chat--relabel-timer)
  (kill-local-variable 'agent-shell-chat--labeled))

;;; Mode

;;;###autoload
(define-minor-mode agent-shell-chat-mode
  "Toggle chat-style `Me'/agent labels in the current `agent-shell' buffer.

Each submitted turn is boxed `Me' and each response the agent's name.
The live prompt shows `Me' so you can type into the shell; when
`agent-shell-prompt-bar-mode' is on it is hidden, since input flows
through the bar instead.

Enable it for new shells by default with `agent-shell-chat-mode-enabled'."
  :lighter nil
  :group 'agent-shell
  (cond
   ((not agent-shell-chat-mode)
    (agent-shell-chat--disable))
   ((derived-mode-p 'agent-shell-mode)
    (agent-shell-chat--enable))
   (t
    ;; Undo the toggle before erroring so the mode does not read as on.
    (setq agent-shell-chat-mode nil)
    (user-error "Not in an `agent-shell' buffer"))))

(provide 'agent-shell-chat-mode)

;;; agent-shell-chat-mode.el ends here
