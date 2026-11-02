;;; builder-rust.el --- Rust project and workspace discovery for builder -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (f "0.20.0") (xtdlib "0.1.0"))
;; Keywords: tools, rust, cargo

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Discovery and metadata primitives for Rust projects and Cargo workspaces.
;; Used by `builder-tasks' to populate first-class compilation candidates.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'f)
(require 'xtd-f)
(require 'xtd-project)
(require 'builder-core)

(defgroup builder-rust nil
  "Rust and Cargo development tools for builder."
  :group 'builder
  :prefix "builder-rust-")

(defcustom builder-rust-cargo-binary "cargo"
  "Path to the cargo executable."
  :type 'string
  :group 'builder-rust)

;;; Project and workspace discovery

(defun builder-rust-root (&optional dir)
  "Find the directory containing the root Cargo.toml for DIR.
Defaults to `default-directory'."
  (let ((search-dir (or dir default-directory)))
    (locate-dominating-file search-dir "Cargo.toml")))

(defun builder-rust-workspace-p (&optional dir)
  "Return non-nil if the Rust project for DIR is a Cargo workspace."
  (when-let* ((root (builder-rust-root dir))
              (manifest (f-join root "Cargo.toml")))
    (when (f-exists-p manifest)
      (with-temp-buffer
        (insert-file-contents manifest)
        (goto-char (point-min))
        (re-search-forward "^\\[workspace\\]" nil t)))))

(defun builder-rust-packages (&optional dir)
  "Return a list of package names in the Rust workspace or project for DIR."
  (when-let* ((root (builder-rust-root dir)))
    (let (packages)
      (dolist (cargo-file (directory-files-recursively root "\\`Cargo\\.toml\\'"))
        (with-temp-buffer
          (insert-file-contents cargo-file)
          (goto-char (point-min))
          (when (re-search-forward "^\\[package\\]" nil t)
            (when (re-search-forward "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\"" nil t)
              (push (match-string 1) packages)))))
      (nreverse (delete-dups packages)))))

(defun builder-rust-binaries (&optional dir)
  "Return a list of binary names in the Rust project for DIR."
  (when-let* ((root (builder-rust-root dir)))
    (let (binaries)
      ;; 1. Check [[bin]] in Cargo.toml files
      (dolist (cargo-file (directory-files-recursively root "\\`Cargo\\.toml\\'"))
        (with-temp-buffer
          (insert-file-contents cargo-file)
          (goto-char (point-min))
          (while (re-search-forward "^\\[\\[bin\\]\\]" nil t)
            (when (re-search-forward "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\"" nil t)
              (push (match-string 1) binaries)))))
      ;; 2. Check src/bin/*.rs and src/main.rs
      (dolist (rs-file (directory-files-recursively root "\\`main\\.rs\\'"))
        (let* ((src-dir (f-parent rs-file))
               (crate-dir (f-parent src-dir))
               (manifest (f-join crate-dir "Cargo.toml")))
          (when (f-exists-p manifest)
            (with-temp-buffer
              (insert-file-contents manifest)
              (goto-char (point-min))
              (when (re-search-forward "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\"" nil t)
                (push (match-string 1) binaries))))))
      (nreverse (delete-dups binaries)))))

(provide 'builder-rust)
;;; builder-rust.el ends here
