;;; elpaish-website.el --- Static GitHub Pages catalog generation for ELPAish -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maintenance, tools, elpa, package

;;; Commentary:
;; Renders the static HTML pages published alongside the package archives:
;; a per-track package catalog (`elpaish-generate-github-index') and the
;; top-level landing page (`elpaish-generate-top-index').  HTML is built as
;; `dom' sexps and printed with `dom-print'; CSS is built as sexps via
;; `elpaish-css' and rendered to a single inline `<style>' block.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'elpaish)
(require 'elpaish-css)
(require 'elpaish-keyring)
(require 'htmlize nil t)
(defconst elpaish-website--fonts-preamble
  '((link ((rel . "preconnect") (href . "https://fonts.googleapis.com")))
    (link ((rel . "preconnect") (href . "https://fonts.gstatic.com") (crossorigin . "")))
    (link ((rel . "stylesheet")
           (href . "https://fonts.googleapis.com/css2?family=Source+Code+Pro:wght@400;600;700&family=Source+Sans+3:wght@400;600;700&display=swap"))))
  "Shared Google Fonts `<link>' tags for `Source Sans 3' and `Source Code Pro'.")

(defcustom elpaish-website-intro-text
  "ELPAish provides continuously built, cryptographically signed Emacs Lisp package archives directly from upstream Git repositories. Archives are organized across three delivery streams: development snapshots, tagged stable releases, and pre-release staging candidates."
  "Optional introductory text rendered on the landing page and catalog headers."
  :type '(choice (const :tag "None" nil) string)
  :group 'elpaish)

(defcustom elpaish-website-repo-url "https://github.com/tychoish/elpaish"
  "Upstream repository URL for ELPAish itself."
  :type '(choice (const :tag "None" nil) string)
  :group 'elpaish)

(defcustom elpaish-website-license-info '("GPL-3.0-or-later" . "https://github.com/tychoish/elpaish/blob/main/LICENSE")
  "Cons pair of (LICENSE-NAME . LICENSE-URL) for the package repository."
  :type '(choice (const :tag "None" nil)
                 (cons (string :tag "License Name") (string :tag "License URL")))
  :group 'elpaish)

(defcustom elpaish-website-extra-sections nil
  "Alist of (HEADING . CONTENT) additional sections to append to the landing page."
  :type '(alist :key-type string :value-type string)
  :group 'elpaish)

(defconst elpaish-website--stream-labels
  '((snapshot . "snapshot")
    (stable . "stable")
    (staging . "staging")
    (elpaish . "snapshot")
    (elpaish-stable . "stable")
    (elpaish-staging . "staging"))
  "Human-readable label for each package archive release stream, used in page titles.")

(defun elpaish-website--stream-label (stream)
  "Return the human-readable catalog page label for STREAM."
  (or (alist-get (elpaish-canonical-stream stream) elpaish-website--stream-labels)
      (symbol-name stream)))

(defun elpaish-website--stream-url (stream)
  "Return the published catalog URL for STREAM under `elpaish-base-url'."
  (format "%s/%s/" (string-remove-suffix "/" elpaish-base-url) (elpaish-canonical-stream stream)))

(defun elpaish-website--navbar (&optional relative-prefix)
  "Return a `<nav class=\"navbar\">' dom node with Home, About, and icon links.
RELATIVE-PREFIX is prepended to relative links (e.g. \"../\" for stream pages)."
  (let ((pfx (or relative-prefix "")))
    `(nav ((class . "navbar"))
          (a ((href . ,(format "%sindex.html" pfx))) "Home")
          (a ((href . ,(format "%sabout.html" pfx))) "About")
          ,@(when elpaish-website-repo-url
              `((a ((href . ,elpaish-website-repo-url)
                    (class . "nav-icon")
                    (title . "GitHub Repository")
                    (aria-label . "GitHub Repository")
                    (target . "_blank")
                    (rel . "noopener"))
                   (i ((class . "nf nf-fa-github")) ""))))
          ,@(when elpaish-website-license-info
              `((a ((href . ,(cdr elpaish-website-license-info))
                    (title . ,(format "License: %s" (car elpaish-website-license-info)))
                    (target . "_blank")
                    (rel . "noopener"))
                   ,(format "License: %s" (car elpaish-website-license-info))))))))
;;; Per-track package catalog

(defun elpaish-website--icon-cell (recipe artifact target-dir)
  "Return dom nodes for RECIPE's repository link, doc link, signature, and checksum badge."
  (let ((repo-url (elpaish-recipe-url recipe))
        (doc-url (elpaish-recipe-doc recipe))
        (sig-file (expand-file-name (concat artifact ".sig") target-dir))
        (sha-file (expand-file-name (concat artifact ".sha256") target-dir)))
    (delq nil
          (list
           (when repo-url
             `(a ((href . ,repo-url) (class . "icon-link") (title . "Repository")
                  (aria-label . "Repository") (target . "_blank") (rel . "noopener"))
                 (i ((class . "nf nf-fa-github")) "")))
           (when doc-url
             `(a ((href . ,doc-url) (class . "icon-link icon-doc") (title . "Documentation")
                  (aria-label . "Documentation") (target . "_blank") (rel . "noopener"))
                 (i ((class . "nf nf-fa-book")) "")))
           (if (file-exists-p sig-file)
               `(a ((href . ,(concat artifact ".sig")) (class . "icon-link") (title . "GPG signature")
                    (aria-label . "GPG signature"))
                   (i ((class . "nf nf-fa-key")) ""))
             `(i ((class . "nf nf-fa-key icon-disabled") (title . "No signature available")) ""))
           (if (file-exists-p sha-file)
               `(a ((href . ,(concat artifact ".sha256")) (class . "icon-link icon-checksum")
                    (title . "SHA256 checksum file") (aria-label . "SHA256 checksum file"))
                   (i ((class . "nf nf-fa-check")) ""))
             `(i ((class . "nf nf-fa-check icon-disabled") (title . "No checksum available")) ""))))))
(defun elpaish-website--file-sha256 (file-path)
  "Compute and return the hex SHA256 checksum for FILE-PATH if it exists."
  (when (and file-path (file-exists-p file-path))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file-path)
      (secure-hash 'sha256 (current-buffer)))))

(defun elpaish-website--format-reqs (reqs)
  "Format dependency requirements REQS into a readable string without lisp sexps."
  (if (null reqs)
      "None"
    (mapconcat
     (lambda (req)
       (let ((dep-name (if (consp req) (car req) req))
             (dep-ver (when (consp req) (cadr req))))
         (if (and dep-ver (not (equal dep-ver '(0))) (not (equal dep-ver "0")))
             (let ((ver-str (if (listp dep-ver) (mapconcat #'number-to-string dep-ver ".") (format "%s" dep-ver))))
               (format "%s (>= %s)" dep-name ver-str))
           (format "%s" dep-name))))
     reqs ", ")))

(defun elpaish-website--package-rows (recipe stream target-dir)
  "Return list of HTML `tr' dom nodes (main row and full-width detail row) for RECIPE on STREAM."
  (unless (elpaish-recipe-disabled-for-stream-p recipe stream)
    (when-let* ((ver-str (elpaish-recipe-version-for-stream recipe stream)))
      (let* ((name (elpaish-recipe-name recipe))
             (summary (or (elpaish-recipe-summary recipe) "No description"))
             (is-tar (eq (elpaish-recipe-built-type recipe) 'tar))
             (artifact (format "%s-%s.%s" name ver-str (if is-tar "tar" "el")))
             (artifact-file (expand-file-name artifact target-dir))
             (sig-file (expand-file-name (concat artifact ".sig") target-dir))
             (sig-exists (file-exists-p sig-file))
             (sha256-hash (elpaish-website--file-sha256 artifact-file))
             (reqs (elpaish-recipe-requires recipe))
             (keywords (elpaish-recipe-keywords recipe))
             (url (elpaish-recipe-url recipe))
             (doc (elpaish-recipe-doc recipe)))
        (let ((toggle-id (format "toggle-%s" name)))
          (list
           `(tr ((class . "pkg-row"))
                (td ((class . "pkg-name-cell"))
                    (input ((type . "checkbox") (id . ,toggle-id) (class . "pkg-toggle-input")))
                    (label ((for . ,toggle-id) (class . "pkg-toggle-label") (title . "Click to view package details"))
                           (span ((class . "pkg-disclosure-icon")) "▶")
                           (b nil ,name)))
                (td ((class . "pkg-version-cell")) (a ((href . ,artifact)) ,ver-str))
                (td ((class . "pkg-desc-cell")) ,summary)
                (td ((class . "pkg-icons-cell")) ,@(elpaish-website--icon-cell recipe artifact target-dir)))
           `(tr ((class . "pkg-detail-row"))
                (td ((colspan . "4") (class . "pkg-detail-cell"))
                    (div ((class . "pkg-detail-box"))
                         (div ((class . "detail-list"))
                              (div ((class . "detail-field"))
                                   (strong nil "Dependencies: ")
                                   ,(elpaish-website--format-reqs reqs))
                              ,@(when keywords
                                  `((div ((class . "detail-field"))
                                         (strong nil "Keywords: ")
                                         ,(mapconcat #'identity keywords ", "))))
                              ,@(when url
                                  `((div ((class . "detail-field"))
                                         (strong nil "Upstream: ")
                                         (a ((href . ,url) (target . "_blank") (rel . "noopener")) ,url))))
                              ,@(when doc
                                  `((div ((class . "detail-field"))
                                         (strong nil "Documentation: ")
                                         (a ((href . ,doc) (target . "_blank") (rel . "noopener")) ,doc))))
                              (div ((class . "detail-field"))
                                   (strong nil "Package Binary: ")
                                   (a ((href . ,artifact)) ,artifact))
                              (div ((class . "detail-field"))
                                   (strong nil "GPG Signature: ")
                                   ,(if sig-exists
                                        `(a ((href . ,(concat artifact ".sig"))) ,(concat artifact ".sig"))
                                      "Not signed"))
                              ,@(when sha256-hash
                                  `((div ((class . "detail-field"))
                                         (strong nil "SHA256 Checksum: ")
                                         (code ((class . "detail-hash")) ,sha256-hash))))))))))))))

(defalias 'elpaish-website--package-row 'elpaish-website--package-rows)

(defun elpaish-website--catalog-table (rows)
  "Return the packages `<table>' dom node for ROWS, or a placeholder if empty."
  (if (null rows)
      '(p nil "No packages published in this release stream.")
    `(div ((class . "table-wrapper"))
          (table ((id . "pkg-table"))
                 (tr nil
                     (th ((class . "pkg-name-cell")) "Package")
                     (th ((class . "pkg-version-cell")) "Version")
                     (th ((class . "pkg-desc-cell")) "Description")
                     (th ((class . "pkg-icons-cell")) "Links"))
                 ,@rows))))

;;;###autoload
(cl-defun elpaish-generate-stream-index (&optional stream output-dir title)
  "Generate static `index.html' package catalog for STREAM in OUTPUT-DIR.
TITLE overrides the default page title derived from STREAM."
  (let* ((effective-stream (elpaish-canonical-stream (or stream elpaish-release-mode)))
         (target-dir (or output-dir (elpaish-stream-dir effective-stream)))
         (page-title (or title (format "ELPAish Repository — (%s)"
                                        (elpaish-website--stream-label effective-stream))))
         (sorted-recipes (sort (hash-table-values elpaish-registry)
                               (lambda (a b) (string< (elpaish-recipe-name a) (elpaish-recipe-name b)))))
         (rows (apply #'append
                      (delq nil
                            (mapcar (lambda (recipe)
                                      (elpaish-website--package-rows recipe effective-stream target-dir))
                                    sorted-recipes)))))
    (make-directory target-dir t)
    (with-temp-file (expand-file-name "index.html" target-dir)
      (insert "<!DOCTYPE html>\n")
      (dom-print
       `(html nil
              (head nil
                    (meta ((charset . "utf-8")))
                    (meta ((name . "viewport") (content . "width=device-width, initial-scale=1")))
                    (title nil ,page-title)
                    ,@elpaish-website--fonts-preamble
                    (link ((rel . "stylesheet") (href . "https://www.nerdfonts.com/assets/css/webfont.css")))
                    (style nil ,(elpaish-css-render (elpaish-css-stream-index-stylesheet))))
              (body nil
                    (h1 nil ,page-title)
                    ,(elpaish-website--navbar "../")
                    (p nil "To use this release stream, add it to " (code nil "package-archives") " in your " (code nil "init.el") ":")
                    (pre nil (code nil ,(elpaish-website--stream-config-snippet effective-stream)))
                    ,(elpaish-website--catalog-table rows)))))
    (expand-file-name "index.html" target-dir)))

(defalias 'elpaish-generate-github-index 'elpaish-generate-stream-index
  "Compatibility alias for `elpaish-generate-stream-index'.")
;;; Top-level landing page

(defun elpaish-website--stream-card (path heading description button-label)
  "Return a `<div class=\"card\">' dom node linking to PATH.
HEADING, DESCRIPTION, and BUTTON-LABEL are strings."
  `(div ((class . "card"))
        (h2 nil (a ((href . ,path)) ,heading))
        (p nil ,description)
        (a ((class . "btn") (href . ,path)) ,button-label)))

(defun elpaish-website--stream-config-snippet (stream)
  "Return the `package-archives' registration snippet for STREAM."
  (let* ((canon (elpaish-canonical-stream stream))
         (archive-name (pcase canon
                         ('snapshot "elpaish-snapshot")
                         ('stable "elpaish")
                         ('staging "elpaish-staging")
                         (_ (format "elpaish-%s" canon))))
         (url (elpaish-website--stream-url canon)))
    (format "(add-to-list 'package-archives '(\"%s\" . \"%s\") t)" archive-name url)))

(defconst elpaish-website--stream-cards
  '(("snapshot/" "Snapshot"
     "Development builds with date-based versioning."
     "Browse Snapshot Packages")
    ("stable/" "Stable"
     "Official release builds with semver tags. Packages without version tags are omitted."
     "Browse Stable Packages")
    ("staging/" "Staging"
     "Pre-releases and git describe-derived versioning for integration tests."
     "Browse Staging Packages"))
  "PATH, HEADING, DESCRIPTION, and BUTTON-LABEL for each stream's landing card.")

(defun elpaish-website--package-archives-snippet ()
  "Return the `package-archives' configuration snippet shown on the landing page."
  (format (concat ";; Development snapshot stream:\n"
                   "(add-to-list 'package-archives '(\"elpaish\" . \"%s\") t)\n\n"
                   ";; Production stable release stream:\n"
                   "(add-to-list 'package-archives '(\"elpaish-stable\" . \"%s\") t)\n\n"
                   ";; Staging pre-release stream:\n"
                   "(add-to-list 'package-archives '(\"elpaish-staging\" . \"%s\") t)")
          (elpaish-website--stream-url 'snapshot)
          (elpaish-website--stream-url 'stable)
          (elpaish-website--stream-url 'staging)))
(defun elpaish-website--keyring-import-snippet ()
  "Return the GPG keyring import snippet shown on the landing page."
  (let ((pub-key-url (format "%s/elpaish.pub.asc" (string-remove-suffix "/" elpaish-base-url))))
    (format (concat "# Download and import armored public key into GPG keyring:\n"
                     "curl -sSL %s | gpg --import\n\n"
                     "# Or download key file directly:\n"
                     "curl -O %s\n"
                     "gpg --import < elpaish.pub.asc")
            pub-key-url pub-key-url)))

(defun elpaish-website--package-keyring-snippet ()
  "Return the `package-import-keyring' snippet shown on the landing page.
Distinct from `elpaish-website--keyring-import-snippet': `package-install'
checks signatures against Emacs's own `package-gnupghome-dir' keyring, not
the user's regular GPG keyring, so the key needs to be imported there too
for `package-check-signature' to actually verify anything."
  (let ((binary-key-url (format "%s/elpaish-keyring.gpg" (string-remove-suffix "/" elpaish-base-url))))
    (format (concat ";; Download the binary keyring and import it into `package.el's own keyring,\n"
                     ";; then enable signature checking:\n"
                     "(url-copy-file \"%s\" \"elpaish-keyring.gpg\" t)\n"
                     "(package-import-keyring \"elpaish-keyring.gpg\")\n"
                     "(setq package-check-signature t)")
            binary-key-url)))

;;;###autoload
(defun elpaish-generate-about-page (&optional output-dir)
  "Generate static `about.html' page in OUTPUT-DIR."
  (let ((target-dir (or output-dir elpaish-output-dir)))
    (make-directory target-dir t)
    (with-temp-file (expand-file-name "about.html" target-dir)
      (insert "<!DOCTYPE html>\n")
      (dom-print
       `(html nil
              (head nil
                    (meta ((charset . "utf-8")))
                    (meta ((name . "viewport") (content . "width=device-width, initial-scale=1")))
                    (title nil "About ELPAish Package Archives")
                    ,@elpaish-website--fonts-preamble
                    (link ((rel . "stylesheet") (href . "https://www.nerdfonts.com/assets/css/webfont.css")))
                    (style nil ,(elpaish-css-render (elpaish-css-top-index-stylesheet))))
              (body nil
                    (h1 nil "About ELPAish")
                    ,(elpaish-website--navbar "")
                    (h2 nil "Overview & Architecture")
                    (p nil "ELPAish is a toolkit for building (and a prototype application of) an Emacs Lisp package archive/repository. It manages recipe discovery, stream packaging, GPG subkey cryptographic signing, and static website publication without external build dependencies.")
                    (h2 nil "Release Streams")
                    (ul nil
                        (li nil (strong nil "Snapshot: ") "Automated builds from the default branch head with date-based versioning (" (code nil "YYYYMMDD.HHMMSS") ").")
                        (li nil (strong nil "Stable: ") "Official releases derived strictly from clean semver Git tags (" (code nil "vX.Y.Z") "). Packages without version tags are omitted.")
                        (li nil (strong nil "Staging: ") "Pre-releases and git describe-derived versioning for integration tests."))
                    (h2 nil "Security & Verification")
                    (p nil "Every package archive artifact and catalog index is signed with an isolated GPG subkey. Consumers can verify package integrity and authenticity using Emacs's built-in " (code nil "package.el") " keyring verification.")
                    ,@(when elpaish-website-license-info
                        `((h2 nil "License")
                          (p nil "This repository and its packages are distributed under "
                             (a ((href . ,(cdr elpaish-website-license-info)) (target . "_blank") (rel . "noopener"))
                                ,(car elpaish-website-license-info)) ".")))
                    (div ((class . "meta-footer"))
                         (p nil "Generated by ELPAish — "
                            (a ((href . "index.html")) "Home")))))))
    (expand-file-name "about.html" target-dir)))

;;;###autoload
(defun elpaish-generate-top-index (&optional output-dir)
  "Generate top-level static `index.html' landing page and about page in OUTPUT-DIR."
  (let ((target-dir (or output-dir elpaish-output-dir)))
    (make-directory target-dir t)
    (elpaish-generate-about-page target-dir)
    (with-temp-file (expand-file-name "index.html" target-dir)
      (insert "<!DOCTYPE html>\n")
      (dom-print
       `(html nil
              (head nil
                    (meta ((charset . "utf-8")))
                    (meta ((name . "viewport") (content . "width=device-width, initial-scale=1")))
                    (title nil "ELPAish: Emacs Lisp Package Archives")
                    ,@elpaish-website--fonts-preamble
                    (link ((rel . "stylesheet") (href . "https://www.nerdfonts.com/assets/css/webfont.css")))
                    (style nil ,(elpaish-css-render (elpaish-css-top-index-stylesheet))))
              (body nil
                    (h1 nil "ELPAish Emacs Package Archives")
                    ,(elpaish-website--navbar "")
                    ,@(when elpaish-website-intro-text
                        `((p nil ,elpaish-website-intro-text)))
                    (div ((class . "stream-grid"))
                         ,@(mapcar (lambda (card) (apply #'elpaish-website--stream-card card))
                                   elpaish-website--stream-cards))
                    (p nil "Add your preferred release stream to " (code nil "package-archives") " in your "
                       (code nil "init.el") ":")
                    (pre nil (code nil ,(elpaish-website--package-archives-snippet)))
                    (h2 nil "GPG Keyring Verification")
                    (p nil "Packages and index files are GPG signed. Download and import the public key into your GPG keyring:")
                    (pre nil (code nil ,(elpaish-website--keyring-import-snippet)))
                    (p nil "To have "
                       (code nil "package-install")
                       " verify signatures, import the public key into the "
                       (code nil "package.el")
                       " keyring and enable "
                       (code nil "package-check-signature")
                       ":")
                    (pre nil (code nil ,(elpaish-website--package-keyring-snippet)))
                    (h2 nil "Keyring and Certificate Assets")
                    (ul nil
                        (li nil (a ((href . "elpaish-keyring.gpg")) "elpaish-keyring.gpg") " — Binary public keyring")
                        (li nil (a ((href . "elpaish.pub.asc")) "elpaish.pub.asc") " — Armored ASCII public key")
                        (li nil (a ((href . "elpaish.rev.asc")) "elpaish.rev.asc") " — Published revocation certificates (if any)"))
                    ,@(when elpaish-website-extra-sections
                        (seq-mapcat (lambda (sec)
                                      `((h2 nil ,(car sec))
                                        (p nil ,(cdr sec))))
                                    elpaish-website-extra-sections))
                    (div ((class . "meta-footer"))
                         (p nil "ELPAish package repository — "
                            (a ((href . "about.html")) "About")
                            ,@(when elpaish-website-repo-url
                                `(" | " (a ((href . ,elpaish-website-repo-url) (target . "_blank") (rel . "noopener")) "Source")))
                            ,@(when elpaish-website-license-info
                                `(" | " (a ((href . ,(cdr elpaish-website-license-info)) (target . "_blank") (rel . "noopener"))
                                           ,(format "License: %s" (car elpaish-website-license-info)))))))))))
    (expand-file-name "index.html" target-dir)))

;;;###autoload
(defun elpaish-build-website (&optional output-directory-path)
  "Regenerate all static HTML pages (landing page, about page, and stream catalogs).
Does not rebuild package archives or re-derive git versions.
OUTPUT-DIRECTORY-PATH defaults to `elpaish-output-dir'."
  (interactive)
  (let ((target-root (or output-directory-path elpaish-output-dir)))
    (make-directory target-root t)
    (dolist (stream elpaish-streams)
      (let ((stream-dir (elpaish-stream-dir stream target-root)))
        (make-directory stream-dir t)
        (elpaish-generate-stream-index stream stream-dir)))
    (elpaish-generate-top-index target-root)
    (message "ELPAish website successfully generated at %s" target-root)))

(provide 'elpaish-website)
;;; elpaish-website.el ends here
