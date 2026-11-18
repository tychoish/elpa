;;; jsonl-view.el --- General-purpose line-oriented JSON viewer -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, files
;; URL: https://github.com/tychoish/logx

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; General-purpose interactive line-oriented JSON (JSON Lines / NDJSON) viewer.
;; Provides zero domain-specific logging assumptions, functioning cleanly over
;; arbitrary JSON Lines files, process streams, and structured datasets.
;;
;; Features:
;; - Interactive inspection for arbitrary `.jsonl' and `.ndjson' files.
;; - In-place collapsible disclosure widgets (`TAB' / `S-TAB') for nested structures.
;; - Dedicated auxiliary side-window Info Inspector buffer (`*jsonl-info*') bound to `i'.
;; - Optional Tree-sitter accelerated AST delimiter concealment.
;; - Zero-copy invisibility filtering via `buffer-invisibility-spec'.
;; - Pluggable line projection and formatting via `jsonl-view-formatter-function'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'json)
(require 'button)

(defgroup jsonl-view nil
  "General-purpose line-oriented JSON viewer."
  :group 'tools
  :prefix "jsonl-view-")

;;; ----------------------------------------------------------------------------
;;; Customization and Semantic Faces
;;; ----------------------------------------------------------------------------

(defcustom jsonl-view-truncate-length 60
  "Maximum string length before inline truncation with disclosure button."
  :type 'integer
  :group 'jsonl-view)

(defcustom jsonl-view-info-buffer-name "*jsonl-info*"
  "Name of the auxiliary pretty-printed JSON inspector buffer."
  :type 'string
  :group 'jsonl-view)

(defvar-local jsonl-view-formatter-function #'jsonl-view-default-formatter
  "Function called to render a parsed JSON record into propertized buffer text.
Accepts (RECORD &optional PREV-RECORD) and returns a propertized string.")

(defface jsonl-view-key
  '((default :inherit font-lock-variable-name-face))
  "Face for JSON property keys."
  :group 'jsonl-view)

(defface jsonl-view-string
  '((default :inherit font-lock-string-face))
  "Face for JSON string values."
  :group 'jsonl-view)

(defface jsonl-view-number
  '((default :inherit font-lock-constant-face))
  "Face for JSON numeric and boolean constants."
  :group 'jsonl-view)

(defface jsonl-view-disclosure-button
  '((default :inherit button))
  "Face for inline disclosure expansion widgets."
  :group 'jsonl-view)

;;; ----------------------------------------------------------------------------
;;; Buffer-Local State
;;; ----------------------------------------------------------------------------

(defvar-local jsonl-view-follow-mode nil
  "Non-nil if the viewer buffer automatically scrolls to the end on new lines.")

(defvar-local jsonl-view-info-follow-mode nil
  "Non-nil if the auxiliary info window auto-updates on cursor movement.")

(defvar-local jsonl-view-filter-predicate nil
  "Active filter predicate function receiving parsed record, or nil if unfiltered.")

;;; ----------------------------------------------------------------------------
;;; Line Parsing and Generic Formatting
;;; ----------------------------------------------------------------------------

;;;###autoload
(defun jsonl-view-parse-line (line-str)
  "Parse LINE-STR into a Lisp plist using native C JSON.
Returns nil if LINE-STR is blank or unparseable."
  (let ((trimmed (string-trim line-str)))
    (unless (string-empty-p trimmed)
      (condition-case nil
          (json-parse-string trimmed :object-type 'plist)
        (error nil)))))

;;;###autoload
(defun jsonl-view-default-formatter (record &optional _prev-record)
  "Render generic JSON RECORD plist into a one-line summary string."
  (with-temp-buffer
    (let ((tail record)
          (first t))
      (while tail
        (let* ((k (car tail))
               (v (cadr tail))
               (k-name (if (keywordp k) (substring (symbol-name k) 1) (format "%s" k)))
               (v-str (format "%s" v)))
          (unless first (insert "  "))
          (setq first nil)
          (insert (propertize (format "%s:" k-name) 'face 'jsonl-view-key) " ")
          (if (or (consp v) (hash-table-p v))
              (insert-text-button "▶"
                                  'action #'jsonl-view--toggle-disclosure-action
                                  'jsonl-value v
                                  'jsonl-expanded nil
                                  'face 'jsonl-view-disclosure-button
                                  'help-echo "mouse-1, RET: expand payload"
                                  'follow-link t)
            (if (> (length v-str) jsonl-view-truncate-length)
                (progn
                  (insert (propertize (substring v-str 0 jsonl-view-truncate-length)
                                      'face 'jsonl-view-string))
                  (insert " ")
                  (insert-text-button "…"
                                      'action #'jsonl-view--toggle-disclosure-action
                                      'jsonl-value v
                                      'jsonl-expanded nil
                                      'face 'jsonl-view-disclosure-button
                                      'help-echo "mouse-1, RET: expand full text"
                                      'follow-link t))
              (insert (propertize v-str 'face (if (numberp v) 'jsonl-view-number 'jsonl-view-string))))))
        (setq tail (cddr tail)))
      (insert "\n")
      (propertize (buffer-string)
                  'jsonl-record record))))

;;; ----------------------------------------------------------------------------
;;; In-Place Disclosure Engine
;;; ----------------------------------------------------------------------------

(defun jsonl-view--toggle-disclosure-action (btn)
  "Toggle disclosure state for disclosure button BTN."
  (let* ((expanded (button-get btn 'jsonl-expanded))
         (full-val (button-get btn 'jsonl-value))
         (inhibit-read-only t))
    (save-excursion
      (goto-char (button-start btn))
      (if expanded
          (progn
            (let ((beg (button-start btn))
                  (end (button-end btn)))
              (delete-region beg end)
              (insert-text-button (if (stringp full-val) "…" "▶")
                                  'action #'jsonl-view--toggle-disclosure-action
                                  'jsonl-value full-val
                                  'jsonl-expanded nil
                                  'face 'jsonl-view-disclosure-button
                                  'help-echo "mouse-1, RET: expand payload"
                                  'follow-link t))
            (goto-char (line-end-position))
            (when (get-text-property (point) 'jsonl-expansion)
              (let ((exp-end (or (next-single-property-change (point) 'jsonl-expansion) (point-max))))
                (delete-region (point) exp-end))))
        (let ((beg (button-start btn))
              (end (button-end btn)))
          (delete-region beg end)
          (insert-text-button "▼"
                              'action #'jsonl-view--toggle-disclosure-action
                              'jsonl-value full-val
                              'jsonl-expanded t
                              'face 'jsonl-view-disclosure-button
                              'help-echo "mouse-1, RET: collapse payload"
                              'follow-link t))
        (goto-char (line-end-position))
        (let ((formatted-json (condition-case nil
                                  (json-serialize full-val)
                                (error (format "%S" full-val)))))
          (insert (propertize (format "\n    %s" formatted-json)
                              'jsonl-expansion t
                              'face 'font-lock-doc-face)))))))

;;;###autoload
(defun jsonl-view-toggle-disclosure ()
  "Toggle payload disclosure expansion on current record."
  (interactive)
  (let ((btn (button-at (point))))
    (if (and btn (button-get btn 'jsonl-value))
        (jsonl-view--toggle-disclosure-action btn)
      (save-excursion
        (beginning-of-line)
        (let ((found nil)
              (eol (line-end-position)))
          (while (and (< (point) eol) (not found))
            (let ((b (button-at (point))))
              (if (and b (button-get b 'jsonl-value))
                  (setq found b)
                (forward-char 1))))
          (if found
              (jsonl-view--toggle-disclosure-action found)
            (message "No expandable disclosure on current line.")))))))

;;;###autoload
(defun jsonl-view-collapse-all ()
  "Collapse all expanded disclosure blocks across the buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (btn (next-button (point-min))))
    (save-excursion
      (while btn
        (let ((next (next-button (button-end btn))))
          (when (button-get btn 'jsonl-expanded)
            (jsonl-view--toggle-disclosure-action btn))
          (setq btn next)))))
  (message "Collapsed all disclosures."))

;;; ----------------------------------------------------------------------------
;;; Generic Invisibility Filtering
;;; ----------------------------------------------------------------------------

;;;###autoload
(defun jsonl-view-filter (predicate-or-regex)
  "Filter buffer records using PREDICATE-OR-REGEX function or string."
  (interactive "sFilter regex: ")
  (let ((pred (if (stringp predicate-or-regex)
                  (lambda (rec)
                    (string-match-p predicate-or-regex
                                    (condition-case nil (json-serialize rec) (error (format "%S" rec)))))
                predicate-or-regex)))
    (setq jsonl-view-filter-predicate pred)
    (jsonl-view--apply-filters)
    (message "Applied filter.")))

;;;###autoload
(defun jsonl-view-clear-filters ()
  "Clear active filter predicate, restoring all lines."
  (interactive)
  (setq jsonl-view-filter-predicate nil)
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max) '(invisible nil)))
  (message "Cleared all filters."))

(defun jsonl-view--apply-filters ()
  "Apply `jsonl-view-filter-predicate' via buffer-invisibility-spec."
  (let ((inhibit-read-only t)
        (parent-hidden nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((pos (point))
               (rec (get-text-property pos 'jsonl-record))
               (is-expansion (get-text-property pos 'jsonl-expansion))
               (eol (line-end-position))
               (line-end (min (1+ eol) (point-max))))
          (cond
           (is-expansion
            (if parent-hidden
                (put-text-property pos line-end 'invisible 'jsonl-filter-hidden)
              (remove-text-properties pos line-end '(invisible nil))))
           (rec
            (let ((hide (and jsonl-view-filter-predicate
                             (not (funcall jsonl-view-filter-predicate rec)))))
              (setq parent-hidden hide)
              (if hide
                  (put-text-property pos line-end 'invisible 'jsonl-filter-hidden)
                (remove-text-properties pos line-end '(invisible nil)))))
           (t
            (setq parent-hidden nil))))
        (forward-line 1)))))

;;; ----------------------------------------------------------------------------
;;; Dedicated Auxiliary Info Inspector
;;; ----------------------------------------------------------------------------

(defun jsonl-view--update-info-buffer (record)
  "Render RECORD in pretty-printed JSON inside `jsonl-view-info-buffer-name'."
  (let ((buf (get-buffer-create jsonl-view-info-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if record
            (let ((formatted (condition-case nil
                                 (json-serialize record)
                               (error (format "%S" record)))))
              (insert formatted "\n")
              (ignore-errors
                (if (fboundp 'json-ts-mode)
                    (json-ts-mode)
                  (when (fboundp 'json-mode) (json-mode))))
              (ignore-errors (json-pretty-print-buffer)))
          (insert "No JSON record under point.\n"))
        (goto-char (point-min))))))

;;;###autoload
(defun jsonl-view-toggle-info ()
  "Toggle auxiliary side window pretty-printing record at point."
  (interactive)
  (let ((existing (get-buffer-window jsonl-view-info-buffer-name)))
    (if existing
        (delete-window existing)
      (let ((rec (get-text-property (point) 'jsonl-record)))
        (jsonl-view--update-info-buffer rec)
        (display-buffer (get-buffer jsonl-view-info-buffer-name)
                        '((display-buffer-in-side-window)
                          (side . right)
                          (window-width . 0.4)))))))

(defun jsonl-view--info-post-command ()
  "Update info inspector window on point change if follow mode is active."
  (when (and jsonl-view-info-follow-mode
             (get-buffer-window jsonl-view-info-buffer-name))
    (let ((rec (get-text-property (point) 'jsonl-record)))
      (jsonl-view--update-info-buffer rec))))

;;; ----------------------------------------------------------------------------
;;; Export and Follow Mode
;;; ----------------------------------------------------------------------------

;;;###autoload
(defun jsonl-view-copy-as-json ()
  "Copy pretty-printed JSON of the record under point to kill-ring."
  (interactive)
  (let ((rec (get-text-property (point) 'jsonl-record)))
    (if rec
        (let ((json-str (condition-case nil
                            (json-serialize rec)
                          (error (format "%S" rec)))))
          (kill-new json-str)
          (message "Copied record JSON to kill-ring."))
      (message "No JSON record at point."))))

;;;###autoload
(defun jsonl-view-toggle-follow ()
  "Toggle auto-scrolling tail follow mode."
  (interactive)
  (setq jsonl-view-follow-mode (not jsonl-view-follow-mode))
  (message "Auto-scroll follow mode: %s" (if jsonl-view-follow-mode "ON" "OFF")))

;;; ----------------------------------------------------------------------------
;;; Major Mode Definition and Keymap
;;; ----------------------------------------------------------------------------

(defvar jsonl-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB")       #'jsonl-view-toggle-disclosure)
    (define-key map (kbd "<tab>")     #'jsonl-view-toggle-disclosure)
    (define-key map (kbd "<backtab>") #'jsonl-view-collapse-all)
    (define-key map (kbd "S-TAB")     #'jsonl-view-collapse-all)
    (define-key map (kbd "i")         #'jsonl-view-toggle-info)
    (define-key map (kbd "w")         #'jsonl-view-copy-as-json)
    (define-key map (kbd "F")         #'jsonl-view-toggle-follow)
    (define-key map (kbd "/")         #'jsonl-view-filter)
    (define-key map (kbd "C")         #'jsonl-view-clear-filters)
    (define-key map (kbd "q")         #'quit-window)
    map)
  "Keymap for `jsonl-view-mode'.")

(defun jsonl-view--on-kill-buffer ()
  "Clean up auxiliary info inspector window when viewer buffer is killed."
  (let ((win (get-buffer-window jsonl-view-info-buffer-name)))
    (when (window-live-p win)
      (delete-window win))))

;;;###autoload
(define-derived-mode jsonl-view-mode special-mode "JSONL-View"
  "Major mode for viewing line-delimited JSON data streams."
  :group 'jsonl-view
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t)
  (add-to-invisibility-spec '(jsonl-filter-hidden . t))
  (add-hook 'post-command-hook #'jsonl-view--info-post-command nil t)
  (add-hook 'kill-buffer-hook #'jsonl-view--on-kill-buffer nil t))

;;;###autoload
(defun jsonl-view-file (file)
  "Open FILE in `jsonl-view-mode' with rendered line-oriented records."
  (interactive "fOpen JSON Lines file: ")
  (let ((buf (generate-new-buffer (format "*jsonl: %s*" (file-name-nondirectory file)))))
    (with-current-buffer buf
      (jsonl-view-mode)
      (let ((inhibit-read-only t)
            (prev-rec nil))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                   (parsed (jsonl-view-parse-line line)))
              (when parsed
                (let ((rendered (funcall jsonl-view-formatter-function parsed prev-rec)))
                  (with-current-buffer buf
                    (insert rendered)))
                (setq prev-rec parsed)))
            (forward-line 1))))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jsonl\\'" . jsonl-view-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ndjson\\'" . jsonl-view-mode))

(provide 'jsonl-view)
;;; jsonl-view.el ends here
