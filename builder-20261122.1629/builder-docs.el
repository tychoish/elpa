;;; builder-docs.el --- Validate and generate Elisp package documentation -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Keywords: tools, lisp, docs

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Documentation helpers for Emacs Lisp packages, exposed as the
;; `doc_validate' and `doc_generate_api' MCP tools by `builder-mcp':
;;
;; - `builder-docs-validate': check every Org link in README.org and
;;   docs/*.org (file targets, line numbers, :CUSTOM_ID: and heading
;;   anchors, internal anchors).
;; - `builder-docs-generate-api': scaffold or regenerate docs/api.org
;;   with `org-docsgen' (an optional dependency).
;;
;; Validation scans text with regexps instead of entering `org-mode', so it
;; runs no user org hooks and costs milliseconds per file.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function org-docsgen-regenerate-file "org-docsgen")

(defconst builder-docs--link-regexp
  "\\[\\[\\(\\(?:[^][\\]\\|\\\\.\\)+?\\)\\]\\(?:\\[[^][]*\\]\\)?\\]"
  "Match a bracket Org link; group 1 is the link target.")

(defun builder-docs--content (file table)
  "Return the contents of FILE, reading it once into hash TABLE."
  (or (gethash file table)
      (when (file-exists-p file)
        (puthash file (with-temp-buffer (insert-file-contents file) (buffer-string))
                 table))))

(defun builder-docs--custom-id-regexp (id)
  "Return a regexp matching a :CUSTOM_ID: property line for ID."
  (concat "^[ \t]*:CUSTOM_ID:[ \t]+" (regexp-quote id) "[ \t]*$"))

(defun builder-docs--error (file line message)
  "Return an error plist for FILE:LINE with MESSAGE."
  (list :file file :line line :message message))

(defun builder-docs--check-link (file dir raw line table)
  "Return an error plist for link RAW at FILE:LINE, or nil if it is valid.
DIR is FILE's directory and TABLE caches file contents.  The current
buffer holds FILE's text."
  (cond
   ;; Internal anchor: a custom id or a heading.
   ((string-prefix-p "#" raw)
    (let* ((id (substring raw 1))
           (heading (concat "^\\*+[ \t]+"
                            (mapconcat #'regexp-quote (split-string id "-") "[ -]+"))))
      (unless (save-excursion
                (goto-char (point-min))
                (or (re-search-forward (builder-docs--custom-id-regexp id) nil t)
                    (progn (goto-char (point-min))
                           (re-search-forward heading nil t))))
        (builder-docs--error
         file line (format "Broken internal anchor '%s' (no matching :CUSTOM_ID: or heading)"
                           raw)))))
   ;; File link, optionally with ::LINE, ::#ID or ::*HEADING.
   ((or (string-prefix-p "file:" raw)
        (not (string-match-p "\\`[a-zA-Z0-9+.-]+:" raw)))
    (let* ((parts (split-string (string-remove-prefix "file:" raw) "::"))
           (target (expand-file-name (car parts) dir))
           (anchor (cadr parts)))
      (cond
       ((not (file-exists-p target))
        (builder-docs--error
         file line (format "Target file does not exist: %s (from link '%s')" target raw)))
       ((and anchor (not (string-empty-p anchor)))
        (let ((content (builder-docs--content target table)))
          (cond
           ((string-match-p "\\`[0-9]+\\'" anchor)
            (let ((n (string-to-number anchor))
                  (total (with-temp-buffer (insert content) (count-lines (point-min) (point-max)))))
              (when (or (< n 1) (> n total))
                (builder-docs--error
                 file line (format "Line %d out of range (1-%d) in %s" n total target)))))
           ((string-prefix-p "#" anchor)
            (unless (string-match-p (builder-docs--custom-id-regexp (substring anchor 1)) content)
              (builder-docs--error
               file line (format "Custom ID '%s' not found in %s" anchor target))))
           ((string-prefix-p "*" anchor)
            (unless (string-match-p
                     (concat "^\\*+[ \t]+" (regexp-quote (substring anchor 1))) content)
              (builder-docs--error
               file line (format "Heading '%s' not found in %s" (substring anchor 1) target))))))))))))

(defun builder-docs--validate-file (file table)
  "Return error plists for the Org links in FILE; TABLE caches file contents."
  (let ((dir (file-name-directory (expand-file-name file)))
        errors)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward builder-docs--link-regexp nil t)
        (let ((raw (match-string 1))
              (line (line-number-at-pos (match-beginning 0))))
          (save-match-data
            (when-let* ((err (builder-docs--check-link file dir raw line table)))
              (push err errors))))))
    (nreverse errors)))

;;;###autoload
(defun builder-docs-validate (directory)
  "Validate Org links in DIRECTORY and DIRECTORY/docs.
Returns a plist: :status (\"ok\" or \"failed\"), :directory, :count and
:errors (vector of plists with :file :line :message)."
  (let* ((dir (file-name-as-directory (expand-file-name directory)))
         (docs (expand-file-name "docs" dir))
         (files (append (directory-files dir t "\\`[^.#].*\\.org\\'")
                        (and (file-directory-p docs)
                             (directory-files docs t "\\`[^.#].*\\.org\\'"))))
         (table (make-hash-table :test #'equal))
         (errors (mapcan (lambda (f) (builder-docs--validate-file f table)) files)))
    (list :status (if errors "failed" "ok")
          :directory dir
          :files (length files)
          :count (length errors)
          :errors (vconcat errors))))

;;;; API reference

(defun builder-docs--package-files (dir)
  "Return the sorted package .el file names in DIR, without tests or scratch files."
  (sort (seq-remove
         (lambda (f)
           (or (string-match-p "-tests?\\.el\\'" f)
               (string-match-p "\\`\\(?:test-\\|scratch\\)" f)
               (string-equal f "test.el")))
         (directory-files dir nil "\\`[^.#].*\\.el\\'"))
        #'string<))

(defun builder-docs--package-name (dir)
  "Infer the primary package name from DIR or its .el files."
  (let ((name (file-name-nondirectory (directory-file-name dir))))
    (if (file-exists-p (expand-file-name (concat name ".el") dir))
        name
      (when-let* ((first (car (builder-docs--package-files dir))))
        (file-name-sans-extension first)))))

(defun builder-docs--api-source (name files)
  "Return docs/api.org text for package NAME covering FILES."
  (concat
   (format "#+TITLE: %s API Reference\n#+OPTIONS: toc:3 num:nil\n\n" name)
   (format "API documentation for the entire %s package, generated from docstrings.\n\n" name)
   "* API Reference\n"
   "#+BEGIN_SRC emacs-lisp :results output raw :exports none\n"
   "  (require 'org-docsgen)\n"
   "  (let ((base (expand-file-name \"..\" default-directory)))\n"
   "    (org-docsgen-run\n     :el-files\n     (delq nil\n           (mapcar (lambda (f) (expand-file-name f base))\n                   '(\n"
   (mapconcat (lambda (f) (format "                     \"%s\"\n" f)) files "")
   "                     )))\n"
   "     :scope 'exported\n     :include-kinds '(variables customs)\n"
   (format "     :namespace \"%s\"\n" name)
   "     :section-style 'triple-semi\n     :link-style 'org\n     :group-by '(\n"
   (mapconcat (lambda (f) (format "                 \"%s\"\n" (file-name-sans-extension f))) files "")
   "                 )))\n#+END_SRC\n"))

;;;###autoload
(defun builder-docs-generate-api (directory &optional dry-run)
  "Scaffold or regenerate docs/api.org for the package in DIRECTORY.
Scaffolds when the file is missing, regenerates otherwise.  With DRY-RUN
only report which action would run.  Signals a `user-error' when
org-docsgen is not installed.  Returns a plist with :action (\"scaffold\"
or \"regenerate\"), :file and :dry_run."
  (let* ((dir (file-name-as-directory (expand-file-name directory)))
         (api (expand-file-name "docs/api.org" dir))
         (action (if (file-exists-p api) "regenerate" "scaffold")))
    (unless (or dry-run (require 'org-docsgen nil t))
      (user-error "Org-docsgen is not installed"))
    (unless dry-run
      (when (equal action "scaffold")
        (make-directory (file-name-directory api) t)
        (with-temp-file api
          (insert (builder-docs--api-source (builder-docs--package-name dir)
                                            (builder-docs--package-files dir)))))
      (let ((inhibit-message t))
        (org-docsgen-regenerate-file api)))
    (list :action action :file api :dry_run (if dry-run t :json-false))))

(provide 'builder-docs)
;;; builder-docs.el ends here
