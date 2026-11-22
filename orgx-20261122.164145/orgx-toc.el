;;; orgx-toc.el --- Update Org tables of contents non-interactively -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Batch-friendly wrappers over `toc-org' for refreshing the `:TOC:'
;; headings of package documentation: `orgx-toc-update-file' for one Org
;; file and `orgx-toc-update-directory' for a package's README.org and
;; docs/*.org.  Only files that actually carry a `:TOC:' heading are
;; touched.

;;; Code:

(require 'subr-x)

(declare-function toc-org-insert-toc "toc-org" (&optional depth))

;;;###autoload
(defun orgx-toc-update-file (file)
  "Refresh the `toc-org' table of contents in FILE and save it."
  (interactive "fOrg file to update TOC: ")
  (require 'toc-org)
  (with-current-buffer (find-file-noselect file)
    (toc-org-insert-toc)
    (save-buffer)
    (message "Updated TOC in %s" file)))

(defun orgx-toc--has-toc-p (file)
  "Return non-nil when FILE contains a `:TOC:' heading."
  (with-temp-buffer
    (insert-file-contents file)
    (re-search-forward "^\\*+[ \t]+.*:TOC.*:" nil t)))

;;;###autoload
(defun orgx-toc-update-directory (dir)
  "Update tables of contents in README.org and docs/*.org under DIR.
Return the list of files updated."
  (interactive "DDirectory of package: ")
  (let* ((root (file-name-as-directory (expand-file-name dir)))
         (readme (expand-file-name "README.org" root))
         (docs (expand-file-name "docs" root))
         (files (append (and (file-exists-p readme) (list readme))
                        (and (file-directory-p docs)
                             (directory-files docs t "\\`[^.#].*\\.org\\'"))))
         updated)
    (dolist (f files)
      (when (orgx-toc--has-toc-p f)
        (orgx-toc-update-file f)
        (push f updated)))
    (nreverse updated)))

(provide 'orgx-toc)
;;; orgx-toc.el ends here
