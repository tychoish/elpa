;;; cmdr-ops.el --- Reusable interactive operations -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/cmdr.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Autoloaded higher-level commands.  Loading installs no hooks, advice or modes.
;;
;; Entry points are unprefixed on purpose; each name was checked against
;; Emacs and common packages.  The kill operations keep Emacs's own vetoes
;; (`kill-buffer-query-functions') and the save prompts for modified buffers.

;;; Code:

(require 'seq)
(require 'subr-x)

(declare-function org-metaup "org")
(declare-function org-metadown "org")
(declare-function annotated-completing-read-directory "annotated-completing-read")
(declare-function kill-buffer-ask "files")
(defvar package-user-dir)
(defvar package-directory-list)

;;; Paths

(defun cmdr--minimize-path (path)
  "Return PATH with the home directory abbreviated to `~'."
  (abbreviate-file-name path))

;;; Window resizing

;;;###autoload
(defun increase-window-up ()
  "Grow the selected window by one line."
  (interactive)
  (enlarge-window 1 nil))

;;;###autoload
(defun increase-window-down ()
  "Shrink the selected window by one line."
  (interactive)
  (enlarge-window -1 nil))

;;;###autoload
(defun increase-window-left ()
  "Grow the selected window by one column."
  (interactive)
  (enlarge-window 1 t))

;;;###autoload
(defun increase-window-right ()
  "Shrink the selected window by one column."
  (interactive)
  (enlarge-window -1 t))

;;;###autoload
(defun pin-buffer-to-window-toggle ()
  "Toggle whether the selected window is dedicated to its buffer.
Most useful for keeping chat buffers under control."
  (interactive)
  (let ((window (selected-window))
        (pinned (not (window-dedicated-p (selected-window)))))
    (set-window-dedicated-p window pinned)
    (message (if pinned "pinned %s to window" "unpinned %s from window")
             (buffer-name))))

;;; Opacity

(defun cmdr--frame-opacity-modify (&optional dec)
  "Change the selected frame's opacity by 5%, or reduce it when DEC is non-nil.
The result stays within 20 and 100; otherwise nothing changes."
  (let* ((raw (frame-parameter nil 'alpha))
         (active (if (consp raw) (car raw) raw))
         (current (cond
                   ((null active) 100)
                   ((and (floatp active) (<= active 1.0)) (round (* active 100)))
                   ((numberp active) (round active))
                   (t 100)))
         (step 5)
         (next (if dec (- current step) (+ current step))))
    (when (and (>= next 20) (<= next 100))
      (modify-frame-parameters nil (list (cons 'alpha next))))))

;;;###autoload
(defun frame-opacity-increase ()
  "Make the selected frame more opaque."
  (interactive)
  (cmdr--frame-opacity-modify))

;;;###autoload
(defun frame-opacity-decrease ()
  "Make the selected frame more transparent."
  (interactive)
  (cmdr--frame-opacity-modify t))

;;;###autoload
(defun frame-opacity-reset ()
  "Reset the selected frame's opacity to 95."
  (interactive)
  (modify-frame-parameters nil '((alpha . 95))))

(defvar-keymap frame-opacity-repeat-map
  :doc "Repeat map for the frame opacity commands."
  :repeat t
  "=" #'frame-opacity-increase
  "-" #'frame-opacity-decrease
  "0" #'frame-opacity-reset)

;;; Text movement

(defun cmdr--move-text-internal (arg)
  "Move the active region, or the current line, ARG lines down."
  (cond
   ((and mark-active transient-mark-mode)
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

;;;###autoload
(defun move-text-down (arg)
  "Move region (`transient-mark-mode' active) or current line ARG lines down.
In `org-mode', defers to `org-metadown' instead, which moves the
subtree at point rather than just the current line."
  (interactive "*p")
  (if (derived-mode-p 'org-mode)
      (org-metadown arg)
    (cmdr--move-text-internal arg)))

;;;###autoload
(defun move-text-up (arg)
  "Move region (`transient-mark-mode' active) or current line ARG lines up.
In `org-mode', defers to `org-metaup' instead, which moves the subtree
at point rather than just the current line."
  (interactive "*p")
  (if (derived-mode-p 'org-mode)
      (org-metaup arg)
    (cmdr--move-text-internal (- arg))))

;;; Wrapping

(defun cmdr--show-wrapping-mode ()
  "Report the wrapping mode of the current buffer."
  (message "wrapping mode `%s' for %s <%s>"
           (if auto-fill-function "hard" "soft")
           major-mode
           (buffer-name)))

;;;###autoload
(defun turn-on-soft-wrap ()
  "Switch the current buffer from hard (auto-fill) to soft (visual line) wrapping."
  (interactive)
  (let ((was-hard-wrapping auto-fill-function))
    (auto-fill-mode -1)
    (visual-line-mode 1)
    (when was-hard-wrapping
      (cmdr--show-wrapping-mode))))

;;;###autoload
(defun turn-off-soft-wrap ()
  "Switch the current buffer from soft (visual line) to hard (auto-fill) wrapping."
  (interactive)
  (let ((was-soft-wrapping (not auto-fill-function)))
    (visual-line-mode -1)
    (auto-fill-mode 1)
    (when was-soft-wrapping
      (cmdr--show-wrapping-mode))))

;;;###autoload
(defalias 'turn-on-hard-wrap #'turn-off-soft-wrap)

;;;###autoload
(defalias 'turn-off-hard-wrap #'turn-on-soft-wrap)

;;;###autoload
(defun toggle-word-wrap (&optional arg)
  "Toggle between hard and soft wrapping.  ARG is rejected as ambiguous."
  (interactive)
  (when arg
    (user-error "Ambiguous argument to `toggle-word-wrap'"))
  (if auto-fill-function
      (turn-on-soft-wrap)
    (turn-on-hard-wrap)))

;;; Text and line helpers

;;;###autoload
(defun unfill-region (begin end)
  "Remove all line breaks between BEGIN and END except paragraph breaks.
Indented text (quotes, code) and lines starting with an asterisk (lists)
are left intact."
  (interactive "r")
  (replace-regexp-in-region "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" begin end))

;;;###autoload
(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines between BEG and END."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

;;;###autoload
(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

;;;###autoload
(defun set-tab-width (num-spaces)
  "Set `tab-width' to NUM-SPACES in the current buffer."
  (interactive "nTab width: ")
  (setq-local tab-width num-spaces))

;;;###autoload
(defun text-scale-reset ()
  "Reset the text scale of the current buffer."
  (interactive)
  (text-scale-set 0))

;;;###autoload
(defun insert-current-date ()
  "Insert today's date as YYYY-MM-DD."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;;###autoload
(defun jump-to-elisp-help ()
  "Search documentation for the symbol at point."
  (interactive)
  (apropos-documentation (symbol-name (intern-soft (thing-at-point 'symbol)))))

;;;###autoload
(defun toggle-local-whitespace-cleanup ()
  "Toggle whitespace cleanup on save for the current buffer."
  (interactive)
  (if (setq-local show-trailing-whitespace (not show-trailing-whitespace))
      (progn
        (add-hook 'before-save-hook #'whitespace-cleanup nil t)
        (message "whitespace-cleanup: enabled"))
    (remove-hook 'before-save-hook #'whitespace-cleanup t)
    (message "whitespace-cleanup: disabled")))

;;; Buffer operations

;;;###autoload
(defun save-all-buffers ()
  "Save all unsaved buffers without prompting."
  (interactive)
  (save-some-buffers t t))

;;;###autoload
(defun buffer-line-count (&optional buf)
  "Return the number of lines in BUF (name or buffer), default the current buffer."
  (car (buffer-line-statistics buf)))

;;;###autoload
(defun buffer-directory (buf)
  "Return the directory of the file BUF visits, or nil when BUF is not a buffer."
  (when (bufferp buf)
    (with-current-buffer buf
      (let ((file-name (buffer-file-name buf)))
        (cond ((null file-name) nil)
              ((file-directory-p file-name) file-name)
              ((file-regular-p file-name) (file-name-directory file-name))
              (t default-directory))))))

;;;###autoload
(defun buffers-matching-path (regexp &optional internal-too)
  "Return buffers whose file name matches REGEXP.
Ignore names starting with a space unless INTERNAL-TOO is non-nil."
  (seq-filter (lambda (buf)
                (when-let* ((name (buffer-file-name buf)))
                  (and (not (string-equal name ""))
                       (or internal-too (/= (aref name 0) ?\s))
                       (string-match regexp name))))
              (buffer-list)))

;;;###autoload
(defun buffers-matching-mode (mode)
  "Return buffers whose major mode is MODE."
  (seq-filter (lambda (buf) (with-current-buffer buf (eq major-mode mode)))
              (buffer-list)))

;;;###autoload
(defun kill-eldoc-and-help-buffers ()
  "Kill all eldoc and help buffers."
  (interactive)
  (kill-matching-buffers "\\*Help\\*\\|\\*eldoc.*\\*" nil t))

;;;###autoload
(defun kill-buffers-in-directory (&optional directory)
  "Kill all file buffers under DIRECTORY, prompting for it when nil.
Buffers vetoed by `kill-buffer-query-functions' stay alive.  Return the
killed file names, or message a summary when called interactively."
  (interactive)
  (unless directory
    (require 'annotated-completing-read)
    (setq directory (annotated-completing-read-directory)))
  (let ((killed (thread-last
                  (buffer-list)
                  (seq-filter #'buffer-file-name)
                  ;; Compare remote identification first so a buffer on another
                  ;; host never triggers a connection attempt.
                  (seq-filter (lambda (buf)
                                (and (equal (file-remote-p (buffer-file-name buf))
                                            (file-remote-p directory))
                                     (file-in-directory-p (buffer-file-name buf)
                                                          directory))))
                  (seq-map (lambda (buf) (cons (buffer-file-name buf) (kill-buffer buf))))
                  (seq-filter #'cdr)
                  (seq-map (lambda (c) (cmdr--minimize-path (car c)))))))
    (if (called-interactively-p 'any)
        (message "killed %d buffers in subdirectory %s: '%S'"
                 (length killed)
                 (cmdr--minimize-path directory)
                 (string-join killed ", "))
      killed)))

;;;###autoload
(defalias 'kill-buffers-matching-name #'kill-matching-buffers)

;;;###autoload
(defun kill-buffers-matching-path (regexp &optional internal-too no-ask)
  "Kill buffers visiting a path that matches REGEXP.
Ignore names starting with a space unless INTERNAL-TOO is non-nil.  Ask
before killing each buffer unless NO-ASK is non-nil."
  (interactive "sKill buffers visiting a path matching this regular expression: \n")
  (let ((killed (thread-last
                  (buffers-matching-path regexp internal-too)
                  (seq-map (lambda (buf)
                             (cons (buffer-file-name buf)
                                   (funcall (if no-ask #'kill-buffer #'kill-buffer-ask) buf))))
                  (seq-filter #'cdr)
                  (seq-map #'car))))
    (if (called-interactively-p 'any)
        (message "killed %d buffers matching '%S'" (length killed) (string-join killed ", "))
      killed)))

;;;###autoload
(defun force-kill-buffers-matching-path (regexp)
  "Kill buffers visiting a path matching REGEXP without asking."
  (interactive "sKill buffers visiting a path matching this regular expression: \n")
  (kill-buffers-matching-path regexp t t))

;;;###autoload
(defun kill-all-reference-and-source-buffers ()
  "Kill all buffers for files in external (upstream) sources.
These are likely opened by jump-to-definition."
  (interactive)
  (require 'package)
  (let ((killed (thread-last
                  '("/usr/share/emacs/.*" "/usr/lib/go/.*" ".*/src/emacs.*/src/.*")
                  (append (cons package-user-dir package-directory-list))
                  (seq-mapcat #'force-kill-buffers-matching-path)
                  (seq-filter #'stringp)
                  (seq-map #'cmdr--minimize-path))))
    (if (called-interactively-p 'any)
        (message "killed %s reference/source buffers [%s]" (length killed) (string-join killed ", "))
      killed)))

;;;###autoload
(defun kill-buffers-matching-mode (mode)
  "Kill all buffers whose major mode is MODE.  Return the number killed."
  (interactive
   (list (intern
          (completing-read
           "mode: "
           obarray
           (lambda (symbol) (string-suffix-p "-mode" (symbol-name symbol)))
           t nil nil major-mode))))
  (let* ((buffers (buffers-matching-mode mode))
         (count (length buffers)))
    (message "killing all buffers (%d) with mode \"%s\"" count mode)
    (seq-do #'kill-buffer buffers)
    count))

;;;###autoload
(defun kill-buffers-visiting-missing-files ()
  "Kill buffers visiting files that no longer exist on disk.
Prompts before killing each buffer.  Return the killed file paths when
called non-interactively."
  (interactive)
  (let ((killed (thread-last
                  (buffer-list)
                  (seq-filter (lambda (buf)
                                (when-let* ((file (buffer-file-name buf)))
                                  (not (file-exists-p file)))))
                  (seq-map (lambda (buf)
                             (cons (buffer-file-name buf) (kill-buffer-ask buf))))
                  (seq-filter #'cdr)
                  (seq-map #'car))))
    (if (called-interactively-p 'any)
        (message "killed %d buffers visiting missing files%s"
                 (length killed)
                 (if killed
                     (format ": %s" (string-join (seq-map #'cmdr--minimize-path killed) ", "))
                   ""))
      killed)))

;;;###autoload
(defun kill-buffer-and-delete-file (&optional buffer)
  "Kill BUFFER (default the current buffer), then delete the file it visits.
Signal a `user-error' when BUFFER is not visiting a file.  The buffer is
killed before the file is deleted, so declining Emacs's \"buffer
modified; kill anyway?\" prompt, or its \"save and then kill\" choice,
leaves the file on disk.  Never prompts when called non-interactively."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (file (buffer-file-name buffer)))
    (unless file
      (user-error "Buffer `%s' is not visiting a file" (buffer-name buffer)))
    (if (called-interactively-p 'any)
        (unless (kill-buffer buffer)
          (user-error "Aborted: `%s' was not killed" (buffer-name buffer)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))
    (delete-file file)
    (when (called-interactively-p 'any)
      (message "Deleted %s" (cmdr--minimize-path file)))))

;;; Kill ring cleaning

(defvar clean-kill-ring-filters '(string-blank-p)
  "Predicates; a `kill-ring' entry satisfying any of them is removed.")

(defvar clean-kill-ring-prevent-duplicates t
  "When non-nil, `clean-kill-ring-clean' also removes duplicate entries.")

(defun clean-kill-ring-filter-catch-p (string)
  "Return t if STRING satisfies at least one of `clean-kill-ring-filters'."
  (let ((s (substring-no-properties string)))
    (and (seq-some (lambda (filter) (funcall filter s))
                   clean-kill-ring-filters)
         t)))

;;;###autoload
(defun clean-kill-ring-clean (&optional remove-dups)
  "Remove `kill-ring' members that satisfy one of `clean-kill-ring-filters'.
If REMOVE-DUPS or `clean-kill-ring-prevent-duplicates' is non-nil, or if
called interactively, also remove duplicate items."
  (interactive (list t))
  (let ((cleaned (seq-remove #'clean-kill-ring-filter-catch-p kill-ring)))
    (setq kill-ring
          (if (or remove-dups clean-kill-ring-prevent-duplicates)
              (delete-dups cleaned)
            cleaned))))

;;; Font, theme and display operations

(declare-function annotated-completing-read-font-name "annotated-completing-read")

;;;###autoload
(defun disable-all-themes ()
  "Disable all currently active custom themes."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

;;;###autoload
(defun cmdr-set-frame-font-interactive (orig-fn font &optional keep-size frames inhibit-customize)
  "Prompt for font interactively using `annotated-completing-read-font-name'.
Advice wrapper for `set-frame-font'."
  (interactive
   (let* ((current (frame-parameter nil 'font))
          (font (if (fboundp 'annotated-completing-read-font-name)
                    (annotated-completing-read-font-name :prompt "Font name: ")
                  (completing-read (format-prompt "Font name" current)
                                   (when (fboundp 'font-family-list) (font-family-list))
                                   nil nil nil nil current))))
     (list font current-prefix-arg nil)))
  (funcall orig-fn font keep-size frames inhibit-customize))

;;;###autoload
(defun set-up-frame-font (font-face-name size)
  "Set font to FONT-FACE-NAME and SIZE using `set-frame-font'."
  (interactive
   (list (if (fboundp 'annotated-completing-read-font-name)
             (annotated-completing-read-font-name)
           (read-string "Font name: "))
         (read-number "Size: " 13)))
  (let ((font (format "%s-%s" font-face-name size))
        (cell (assoc 'font default-frame-alist)))
    (if cell
        (setcdr cell font)
      (add-to-list 'default-frame-alist (cons 'font font)))
    (when (display-graphic-p)
      (set-frame-font font nil t))
    (alist-get 'font default-frame-alist)))

;;;###autoload
(defun ensure-frame-font (font-face-name size)
  "Ensure default frame font is set, applying FONT-FACE-NAME and SIZE if unset."
  (unless (alist-get 'font default-frame-alist)
    (let ((font (format "%s-%s" font-face-name size)))
      (add-to-list 'default-frame-alist (cons 'font font))
      (when (display-graphic-p)
        (set-frame-font font nil t)))))

;;;###autoload
(defun ensure-frame-opacity (&optional opacity frame)
  "Ensure frame opacity is OPACITY on FRAME.
Default OPACITY is 95, FRAME defaults to the current.  If OPACITY is
nil, 1 (as from minor mode activation), or t, use the existing `alpha'
from `default-frame-alist' (if > 20) or default to 95.  Also ensures
`default-frame-alist' includes `alpha'."
  (let* ((def-alpha (alist-get 'alpha default-frame-alist))
         (usable-def (and (numberp def-alpha) (> def-alpha 20) def-alpha))
         (target (cond
                  ((or (null opacity) (eq opacity t) (equal opacity 1))
                   (or usable-def 95))
                  ((and (numberp opacity) (<= opacity 1.0) (> opacity 0.0))
                   (round (* opacity 100)))
                  ((numberp opacity)
                   (round opacity))
                  (t (or usable-def 95))))
         (alpha (max 20 (min 100 target))))
    (if (assq 'alpha default-frame-alist)
        (setcdr (assq 'alpha default-frame-alist) alpha)
      (push (cons 'alpha alpha) default-frame-alist))
    (when (display-graphic-p frame)
      (set-frame-parameter frame 'alpha alpha))))

;;;###autoload
(defun frame-unset-background-for-tty (frame)
  "Unset default face background and foreground on TTY frame FRAME."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)
    (set-face-foreground 'default "unspecified-fg" frame)))

;;;###autoload
(defun current-frame-unset-background-for-tty ()
  "Reset the background on the current frame, but only if it is a TTY frame."
  (interactive)
  (frame-unset-background-for-tty (selected-frame)))

;;;###autoload
(defun contextual-menubar (&optional frame)
  "Display menubar in FRAME if graphical, hide if TTY.
FRAME defaults to the selected frame."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))

(provide 'cmdr-ops)
;;; cmdr-ops.el ends here
