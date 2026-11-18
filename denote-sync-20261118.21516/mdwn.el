;;; mdwn.el --- Convert Markdown to Org using tree-sitter -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.7"))
;; Keywords: outlines, text, wp
;; URL: https://github.com/tychoish/denote-sync

;;; Commentary:
;; `mdwn-to-org' converts a Markdown string to Org markup by walking the
;; parse tree produced by the `markdown' and `markdown-inline' tree-sitter
;; grammars.  The walk builds an `org-element' tree that
;; `org-element-interpret-data' renders, so Org's own syntax rules (escaping,
;; list numbering, table layout) apply.  There is no external-process or regexp fallback: when a
;; grammar is missing it signals a `user-error', or installs it first when
;; `mdwn-install-grammars' is non-nil.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'treesit)
(require 'org-element)

(defgroup mdwn nil
  "Markdown to Org conversion."
  :group 'text
  :prefix "mdwn-")

(defcustom mdwn-install-grammars nil
  "When non-nil, install missing tree-sitter grammars on demand.
Installation needs network access, git and a C compiler."
  :type 'boolean
  :group 'mdwn)

(defconst mdwn--grammar-sources
  '((markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
              "split_parser" "tree-sitter-markdown/src")
    (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                     "split_parser" "tree-sitter-markdown-inline/src"))
  "`treesit-language-source-alist' entries for the Markdown grammars.")

(defun mdwn--ensure-grammars ()
  "Make sure the Markdown grammars are usable, or signal a `user-error'."
  (unless (and (fboundp 'treesit-available-p) (treesit-available-p))
    (user-error "mdwn: this Emacs was built without tree-sitter support"))
  (dolist (entry mdwn--grammar-sources)
    (let ((lang (car entry)))
      (unless (treesit-language-available-p lang)
        (if mdwn-install-grammars
            (let ((treesit-language-source-alist
                   (append mdwn--grammar-sources treesit-language-source-alist)))
              (treesit-install-language-grammar lang))
          (user-error "mdwn: tree-sitter grammar `%s' is not installed; run `M-x treesit-install-language-grammar' or set `mdwn-install-grammars'"
                      lang))
        (unless (treesit-language-available-p lang)
          (user-error "mdwn: installing the `%s' grammar failed" lang))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inline content

(defun mdwn--text (node)
  "Return NODE's source text."
  (treesit-node-text node t))

(defun mdwn--child-text (node type)
  "Return the text of NODE's first child of TYPE, or nil."
  (when-let* ((c (seq-find (lambda (n) (equal (treesit-node-type n) type))
                           (treesit-node-children node))))
    (mdwn--text c)))

(defun mdwn--el (type props &rest children)
  "Create an Org element or object of TYPE with PROPS and CHILDREN."
  (apply #'org-element-create type props children))

(defun mdwn--inline-range (node start end)
  "Return the Org objects for the inline region START..END of NODE."
  (let ((pos start) (out nil))
    (dolist (child (treesit-node-children node))
      (let ((cs (treesit-node-start child)) (ce (treesit-node-end child)))
        (when (and (>= cs start) (<= ce end) (>= cs pos))
          (push (buffer-substring-no-properties pos cs) out)
          (push (mdwn--inline-node child) out)
          (setq pos ce))))
    (push (buffer-substring-no-properties pos end) out)
    (delq nil (nreverse out))))

(defun mdwn--delimited (node type)
  "Render emphasis-like NODE as an Org object of TYPE."
  (let* ((kids (treesit-node-children node))
         (delims (seq-filter (lambda (n) (member (treesit-node-type n)
                                                 '("emphasis_delimiter" "code_span_delimiter")))
                             kids))
         (half (/ (length delims) 2))
         (start (treesit-node-end (nth (1- half) delims)))
         (end (treesit-node-start (nth half (reverse (reverse delims))))))
    (apply #'mdwn--el type nil (mdwn--inline-range node start end))))

(defun mdwn--link-destination (node)
  "Return NODE's link destination without angle brackets."
  (let ((d (or (mdwn--child-text node "link_destination") "")))
    (if (string-match "\\`<\\(.*\\)>\\'" d) (match-string 1 d) d)))

(defun mdwn--link (destination &rest description)
  "Return an Org link to DESTINATION with DESCRIPTION objects."
  (let ((type (if (string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\):\\(.*\\)\\'" destination)
                  (cons (match-string 1 destination) (match-string 2 destination))
                (cons "fuzzy" destination))))
    (apply #'mdwn--el 'link (list :type (car type) :path (cdr type) :raw-link destination)
           description)))

(defun mdwn--inline-node (node)
  "Render the inline NODE as Org objects."
  (pcase (treesit-node-type node)
    ("strong_emphasis" (mdwn--delimited node 'bold))
    ("emphasis" (mdwn--delimited node 'italic))
    ("strikethrough"
     ;; the grammar nests `~~x~~' as strikethrough-in-strikethrough, so trim by text
     (let* ((txt (mdwn--text node))
            (lead (- (length txt) (length (string-trim-left txt "~+"))))
            (trail (- (length txt) (length (string-trim-right txt "~+")))))
       (apply #'mdwn--el 'strike-through nil
              (mdwn--inline-range node (+ (treesit-node-start node) lead)
                                  (- (treesit-node-end node) trail)))))
    ("code_span"
     (let ((delims (seq-filter (lambda (n) (equal (treesit-node-type n) "code_span_delimiter"))
                               (treesit-node-children node))))
       (mdwn--el 'code (list :value (string-trim (buffer-substring-no-properties
                                                  (treesit-node-end (car delims))
                                                  (treesit-node-start (car (last delims)))))))))
    ("inline_link"
     (let ((text (when-let* ((lt (seq-find (lambda (n) (equal (treesit-node-type n) "link_text"))
                                           (treesit-node-children node))))
                   (mdwn--inline-range lt (treesit-node-start lt) (treesit-node-end lt)))))
       (apply #'mdwn--link (mdwn--link-destination node) text)))
    ("image" (mdwn--link (mdwn--link-destination node)))
    ("uri_autolink" (mdwn--link (string-trim (mdwn--text node) "<" ">")))
    ("backslash_escape" (substring (mdwn--text node) 1))
    ("hard_line_break" "\n")
    (_ (mdwn--text node))))

(defun mdwn--inline (text)
  "Convert inline Markdown TEXT to a list of Org objects."
  (if (string-empty-p text)
      nil
    (with-temp-buffer
      (insert text)
      (let ((root (treesit-parser-root-node (treesit-parser-create 'markdown-inline))))
        (mdwn--inline-range root (point-min) (point-max))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blocks

(defun mdwn--children (node)
  "Return NODE's block children as Org elements, blank-line separated."
  (let ((els (mapcan #'mdwn--block (treesit-node-children node t))))
    (seq-do-indexed (lambda (el i)
                      (org-element-put-property el :post-blank (if (< i (1- (length els))) 1 0)))
                    els)
    els))

(defun mdwn--paragraph (node)
  "Render paragraph NODE."
  (apply #'mdwn--el 'paragraph nil
         (mdwn--inline (string-trim (mapconcat #'mdwn--text
                                               (seq-filter (lambda (n) (equal (treesit-node-type n) "inline"))
                                                           (treesit-node-children node))
                                               "\n")))))

(defun mdwn--heading-level (node)
  "Return the heading level of atx or setext heading NODE."
  (let ((marker (seq-find (lambda (n) (string-match-p "\\`\\(atx_h[1-6]_marker\\|setext_h[12]_underline\\)\\'"
                                                      (treesit-node-type n)))
                          (treesit-node-children node))))
    (pcase (treesit-node-type marker)
      ((rx "atx_h" (let n digit)) (string-to-number n))
      ((rx "setext_h" (let n digit)) (string-to-number n)))))

(defun mdwn--list-item (node ordered first)
  "Render list item NODE; ORDERED lists number from the marker when FIRST."
  (let* ((kids (treesit-node-children node t))
         (marker (car (treesit-node-children node)))
         (start (and ordered first (string-to-number (mdwn--text marker))))
         (task (seq-find (lambda (n) (string-match-p "task_list_marker" (treesit-node-type n))) kids))
         (checkbox (when task
                     (if (string-match-p "unchecked" (treesit-node-type task)) 'off 'on)))
         (blocks (mapcan (lambda (n)
                           (unless (string-match-p "task_list_marker" (treesit-node-type n))
                             (mdwn--block n)))
                         kids)))
    (dolist (el blocks) (org-element-put-property el :post-blank 0))
    (apply #'mdwn--el 'item
           (list :bullet (if ordered "1. " "- ") :pre-blank 0 :checkbox checkbox
                 :counter (and start (/= start 1) start))
           blocks)))

(defun mdwn--list (node)
  "Render list NODE."
  (let* ((items (seq-filter (lambda (c) (equal (treesit-node-type c) "list_item"))
                            (treesit-node-children node t)))
         (ordered (and items
                       (string-match-p "dot\\|parenthesis"
                                       (treesit-node-type (car (treesit-node-children (car items))))))))
    (apply #'mdwn--el 'plain-list (list :type (if ordered 'ordered 'unordered))
           (seq-map-indexed (lambda (item i) (mdwn--list-item item ordered (zerop i))) items))))

(defun mdwn--code-fence (node)
  "Render fenced code block NODE."
  (let ((lang (or (mdwn--child-text (or (seq-find (lambda (n) (equal (treesit-node-type n) "info_string"))
                                                  (treesit-node-children node))
                                        node)
                                    "language")
                  ""))
        (content (or (mdwn--child-text node "code_fence_content") "")))
    (mdwn--el 'src-block (list :language (unless (string-empty-p lang) lang)
                               :value (concat (string-trim-right content "\n+") "\n")))))

(defun mdwn--table-row (row)
  "Render pipe-table ROW."
  (apply #'mdwn--el 'table-row (list :type 'standard)
         (mapcar (lambda (c)
                   (apply #'mdwn--el 'table-cell nil (mdwn--inline (string-trim (mdwn--text c)))))
                 (seq-filter (lambda (n) (equal (treesit-node-type n) "pipe_table_cell"))
                             (treesit-node-children row)))))

(defun mdwn--table (node)
  "Render pipe-table NODE."
  (apply #'mdwn--el 'table (list :type 'org)
         (mapcar (lambda (row)
                   (if (equal (treesit-node-type row) "pipe_table_delimiter_row")
                       (mdwn--el 'table-row (list :type 'rule))
                     (mdwn--table-row row)))
                 (treesit-node-children node t))))

(defun mdwn--block (node)
  "Return the Org elements (a list, possibly empty) for block NODE."
  (pcase (treesit-node-type node)
    ((or "document" "section") (mdwn--children node))
    ("paragraph" (list (mdwn--paragraph node)))
    ((or "atx_heading" "setext_heading")
     (let ((content (or (treesit-node-child-by-field-name node "heading_content") node)))
       (list (mdwn--el 'headline
                 (list :level (mdwn--heading-level node)
                       :title (mdwn--inline
                               (string-trim (mdwn--text
                                             (or (seq-find (lambda (n) (equal (treesit-node-type n) "inline"))
                                                           (treesit-node-children content))
                                                 content)))))))))
    ("list" (list (mdwn--list node)))
    ("block_quote" (list (apply #'mdwn--el 'quote-block nil (mdwn--children node))))
    ("fenced_code_block" (list (mdwn--code-fence node)))
    ("indented_code_block"
     (list (mdwn--el 'example-block
                     (list :value (concat (string-trim-right (mdwn--text node) "\n+") "\n")))))
    ("pipe_table" (list (mdwn--table node)))
    ("thematic_break" (list (mdwn--el 'horizontal-rule nil)))
    ("html_block"
     (list (mdwn--el 'export-block
                     (list :type "html" :value (concat (string-trim (mdwn--text node)) "\n")))))
    ((or "link_reference_definition" "block_continuation" "block_quote_marker") nil)
    ((rx bos (or "list_marker" "task_list_marker")) nil)
    (_ (list (mdwn--el 'paragraph nil (mdwn--text node))))))

;;;###autoload
(defun mdwn-to-org (markdown)
  "Convert the Markdown string MARKDOWN to Org markup."
  (mdwn--ensure-grammars)
  (let ((tree (with-temp-buffer
                (insert markdown "\n")
                (let ((root (treesit-parser-root-node (treesit-parser-create 'markdown))))
                  (apply #'mdwn--el 'org-data nil (mdwn--children root)))))
        (org-src-preserve-indentation t)
        (org-edit-src-content-indentation 0))
    (string-trim (substring-no-properties (org-element-interpret-data tree)))))

(provide 'mdwn)
;;; mdwn.el ends here
