;;; orgx-fill.el --- Fill policy for headings, blocks and other no-fill regions -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Keywords: org, text, convenience

;;; Commentary:
;; Decides where `fill-paragraph' must not act, and fills whole buffers
;; with that policy applied.
;;
;; Org structure is covered here: headings, keyword lines, drawers,
;; planning and clock lines, and source/example/export/fixed-width
;; blocks.  Markdown headings, code blocks and YAML front matter are
;; covered too (folded `>' scalars in front matter are filled), using
;; markdown-mode and tree-sitter only when a markdown buffer is involved.
;; Other modes plug in through two lists:
;;
;; - `orgx-fill-suppress-functions': predicates of no arguments; any
;;   non-nil result means "do not fill at point".
;; - `orgx-fill-handlers': functions of no arguments that fill a special
;;   region themselves (see `orgx-fill-with-prefix') and return non-nil
;;   when they did.  Handlers run before the suppress predicates.
;;
;; `orgx-fill-guard-mode' installs the policy as `:around' advice on
;; `orgx-fill-advised-functions'.  Loading this file installs nothing.

;;; Code:

(require 'org-element)
(require 'seq)
(require 'subr-x)

(defgroup orgx-fill nil
  "Fill policy for headings, blocks and other no-fill regions."
  :group 'orgx
  :prefix "orgx-fill-")

(defcustom orgx-fill-org-element-types
  '(headline keyword property-drawer node-property drawer planning clock
    babel-call src-block example-block export-block fixed-width verse-block
    comment-block latex-environment table table-row quote-block special-block
    center-block dynamic-block)
  "Org element types that `fill-paragraph' must leave untouched.
Greater blocks (quote, special, center, dynamic) are listed so that
their opening line is skipped on its own and `orgx-fill-buffer' then
descends into their contents instead of skipping the whole block."
  :type '(repeat symbol)
  :group 'orgx-fill)

(defcustom orgx-fill-column 72
  "Default fill column for `orgx-fill-buffer'."
  :type 'integer
  :group 'orgx-fill)

(defun orgx-fill-org-suppressed-p ()
  "Return non-nil when point is in an Org element that must not be filled."
  (and (derived-mode-p 'org-mode)
       (memq (org-element-type (org-element-at-point))
             orgx-fill-org-element-types)))

(defvar orgx-fill-suppress-functions
  '(orgx-fill-org-suppressed-p orgx-fill-markdown-suppressed-p)
  "Predicates of no arguments; any non-nil result suppresses filling at point.")

(defvar orgx-fill-handlers '(orgx-fill-markdown-fill-folded-scalar)
  "Functions of no arguments that fill a special region at point.
Each returns non-nil when it handled filling.")

(defvar orgx-fill-advised-functions '(fill-paragraph markdown-fill-paragraph)
  "Functions `orgx-fill-guard-mode' wraps with the fill policy.")

;;; Markdown

;; Soft dependency: markdown-mode and the tree-sitter yaml grammar are only
;; touched after `derived-mode-p' confirms a markdown buffer, which implies
;; markdown-mode is loaded.  Without the yaml grammar, front matter is left
;; unfilled.

(declare-function markdown-code-block-at-point-p "markdown-mode")
(declare-function markdown-heading-at-point "markdown-mode")
(declare-function markdown-text-property-at-point "markdown-mode")
(declare-function treesit-language-available-p "treesit")
(declare-function treesit-parser-create "treesit")
(declare-function treesit-parser-set-included-ranges "treesit")
(declare-function treesit-parser-root-node "treesit")
(declare-function treesit-node-descendant-for-range "treesit")
(declare-function treesit-parent-until "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")

(defun orgx-fill-markdown-frontmatter-p ()
  "Return non-nil when point is in a markdown YAML front matter block."
  (syntax-propertize (line-end-position))
  (seq-some #'markdown-text-property-at-point
            '(markdown-yaml-metadata-section
              markdown-yaml-metadata-begin
              markdown-yaml-metadata-end)))

(defun orgx-fill-yaml-folded-scalar-bounds ()
  "Return (BEG END INDENT) of the folded (`>') scalar body at point.
Uses the tree-sitter yaml grammar over the front matter section; return
nil when point is not in a folded scalar or the grammar is missing."
  (when-let* ((section (markdown-text-property-at-point 'markdown-yaml-metadata-section))
              ((treesit-language-available-p 'yaml))
              (parser (treesit-parser-create 'yaml nil nil 'front-matter))
              (node (progn
                      (treesit-parser-set-included-ranges
                       parser (list (cons (car section) (cadr section))))
                      (treesit-parent-until
                       (treesit-node-descendant-for-range
                        (treesit-parser-root-node parser) (point) (point) t)
                       (lambda (n) (equal (treesit-node-type n) "block_scalar"))
                       t)))
              ((eq (char-after (treesit-node-start node)) ?>)))
    (save-excursion
      (goto-char (treesit-node-start node))
      (forward-line 1)
      (list (point) (treesit-node-end node) (current-indentation)))))

(defun orgx-fill-markdown-suppressed-p ()
  "Return non-nil on markdown headings, code blocks and front matter.
Folded YAML scalars in front matter are not suppressed; see
`orgx-fill-markdown-fill-folded-scalar'."
  (and (derived-mode-p 'markdown-mode)
       (if (orgx-fill-markdown-frontmatter-p)
           (not (orgx-fill-yaml-folded-scalar-bounds))
         (or (markdown-code-block-at-point-p)
             (markdown-heading-at-point)))))

(defun orgx-fill-markdown-fill-folded-scalar ()
  "Fill the folded YAML scalar at point in markdown front matter.
Return non-nil when point was in one."
  (when-let* (((derived-mode-p 'markdown-mode))
              ((orgx-fill-markdown-frontmatter-p))
              (bounds (orgx-fill-yaml-folded-scalar-bounds)))
    (apply #'orgx-fill-with-prefix bounds)
    t))

(defun orgx-fill-suppressed-p ()
  "Return non-nil when any of `orgx-fill-suppress-functions' says to skip."
  (seq-some #'funcall orgx-fill-suppress-functions))

(defun orgx-fill-handled-p ()
  "Return non-nil when one of `orgx-fill-handlers' filled at point."
  (seq-some #'funcall orgx-fill-handlers))

(defun orgx-fill-with-prefix (beg end indent)
  "Fill the text between BEG and END, indenting continuation lines by INDENT.
Paragraphs are separated by blank lines, independent of the major mode's
own paragraph rules."
  (let ((fill-prefix (make-string indent ?\s))
        (fill-forward-paragraph-function #'forward-paragraph))
    (fill-region beg end)))

(defun orgx-fill-paragraph-at-point ()
  "Fill at point under the policy; return non-nil unless suppressed."
  (cond
   ((orgx-fill-handled-p) t)
   ((orgx-fill-suppressed-p) nil)
   (t (fill-paragraph) t)))

(defun orgx-fill-buffer (&optional column)
  "Fill every paragraph in the current buffer to COLUMN under the policy.
COLUMN defaults to `orgx-fill-column'.  Point is preserved.  Return
non-nil when the buffer changed."
  (let ((fill-column (or column orgx-fill-column))
        (before (buffer-hash)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((start (point)))
          (cond
           ((looking-at-p "[ \t]*$")
            (forward-line 1))
           ((or (orgx-fill-handled-p) (orgx-fill-suppressed-p))
            (forward-line 1))
           (t
            (fill-paragraph)
            (forward-paragraph)
            (unless (bolp)
              (forward-line 1))))
          (when (<= (point) start)
            (goto-char start)
            (forward-line 1)))))
    (not (equal before (buffer-hash)))))

(defun orgx-fill--ad-guard (fn &rest args)
  "Around advice for FN: apply the fill policy before filling with ARGS."
  (or (orgx-fill-handled-p)
      (orgx-fill-suppressed-p)
      (apply fn args)))

;;;###autoload
(define-minor-mode orgx-fill-guard-mode
  "Apply the orgx fill policy to `orgx-fill-advised-functions'."
  :global t
  :group 'orgx-fill
  (seq-do (lambda (fn)
            (if orgx-fill-guard-mode
                (advice-add fn :around #'orgx-fill--ad-guard)
              (advice-remove fn #'orgx-fill--ad-guard)))
          orgx-fill-advised-functions))

(provide 'orgx-fill)
;;; orgx-fill.el ends here
