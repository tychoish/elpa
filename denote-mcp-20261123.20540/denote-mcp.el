;;; denote-mcp.el --- Denote MCP service integration for mcpkit -*- lexical-binding: t; -*-

;; Author: Sam Kleinman <sam@tychoish.com>
;; Maintainer: Sam Kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (mcpkit "0.1.0") (denote "4.0.0") (denote-sequence "0.3.0") (denote-org "0.3.0") (denote-convert "0.1.0") (denote-sync "0.2.0"))
;; Homepage: https://github.com/tychoish/denote-mcp
;; Keywords: tools, mcp, denote, notes

;;; Commentary:
;;
;; Provides Model Context Protocol (MCP) tools for interacting with the Denote
;; note-taking and knowledge-base system via mcpkit.el.
;;
;; Exposes discovery, reading, note creation, frontmatter syncing, Folgezettel
;; sequence hierarchy management, linking, dynamic block insertion, backdating,
;; and plan lifecycle tracking.
;;
;; Tools are registered at top-level on the `denote' service upon loading.
;; Services are started only when explicitly requested via `mcpkit-start-service'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'org)
(require 'mcpkit)
(require 'mcpkit-org)
(require 'denote)
(require 'denote-sequence)
(require 'denote-org)

(defvar mdwn-install-grammars)
(defvar denote-convert-after-conversion-functions)
(declare-function denote-mcp-notes-create "denote-mcp-notes" (args))
(declare-function mdwn-to-org "mdwn" (markdown))
(declare-function denote-convert-file-type "denote-convert" (file new-file-type))
(declare-function denote-convert--front-matter-end "denote-convert" (file-type))
(declare-function denote-sync--body-without-front-matter "denote-sync" (file))

;;; Directory & Execution Environment

(defgroup denote-mcp nil
  "Denote MCP service integration for mcpkit."
  :group 'mcpkit
  :prefix "denote-mcp-")

(defcustom denote-mcp-executed-keyword "x"
  "Filetag applied to mark a plan note as executed/implemented."
  :type 'string
  :group 'denote-mcp)

(defun denote-mcp-get-directory ()
  "Return the primary denote directory as an absolute directory path."
  (cond
   ((and (boundp 'denote-directory) (listp denote-directory))
    (file-name-as-directory (expand-file-name (car denote-directory))))
   ((and (boundp 'denote-directory) (stringp denote-directory))
    (file-name-as-directory (expand-file-name denote-directory)))
   ((fboundp 'denote-directory)
    (file-name-as-directory (expand-file-name (denote-directory))))
   (t (file-name-as-directory (expand-file-name "~/denote/")))))

(defmacro denote-mcp-silently (&rest body)
  "Execute BODY with all interactive confirmation prompts neutralized.
Suppresses y-or-n-p prompts, local variables queries, stale-buffer revert
prompts, large-file warnings, and UTF-8 encoding prompts."
  `(cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
             ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
     (let ((enable-local-variables nil)
           (revert-without-query '(".*"))
           (large-file-warning-threshold nil)
           (coding-system-for-read 'utf-8)
           (coding-system-for-write 'utf-8)
           (denote-save-buffers t))
       ,@body)))

;;; Note Index (cached, filename-derived)

(defcustom denote-mcp-sequence-scheme nil
  "Folgezettel sequence scheme the sequence tools bind, or nil.
When nil the tools use the ambient `denote-sequence-scheme'; only
`denote_seq_reparent' falls back to `alphanumeric'."
  :type '(choice (const :tag "Ambient denote-sequence-scheme" nil)
                 (const alphanumeric) (const numeric))
  :group 'denote-mcp)

(defmacro denote-mcp-mutating (&rest body)
  "Run BODY like `denote-mcp-silently', then drop the cached note index."
  (declare (indent 0))
  `(unwind-protect (denote-mcp-silently ,@body)
     (denote-mcp-invalidate-cache)))

(cl-defstruct (denote-mcp--note (:constructor denote-mcp--note-create))
  "Filename-derived facts about one note."
  path id seq slug keywords ext)

(defvar denote-mcp--cache nil
  "Cached note index as (KEY . NOTES), see `denote-mcp--notes'.")

(defun denote-mcp-invalidate-cache ()
  "Forget the cached note index."
  (setq denote-mcp--cache nil))

(defconst denote-mcp--filename-rx
  (rx bos (group (= 8 digit) "T" (= 6 digit))
      (? "==" (group (+ (not (in "-_.")))))
      (? "--" (group (* (not (in "_.")))))
      (? "__" (group (* (not (in "."))))) "." (group (+ nonl)) eos)
  "Match a standard denote file name: ID, signature, slug, keywords, extension.")

(defun denote-mcp--note-from-path (file)
  "Return a `denote-mcp--note' for FILE parsed from its name, or nil."
  (let ((base (file-name-nondirectory file)))
    (if (string-match denote-mcp--filename-rx base)
        ;; Read every group before `split-string' clobbers the match data.
        (let ((id (match-string 1 base)) (seq (match-string 2 base))
              (slug (match-string 3 base)) (kw (match-string 4 base))
              (ext (match-string 5 base)))
          (denote-mcp--note-create
           :path file :id id :seq seq :slug slug
           :keywords (when kw (split-string kw "_" t))
           :ext ext))
      (when-let* ((id (denote-retrieve-filename-identifier base)))
        (denote-mcp--note-create
         :path file :id id :seq (denote-retrieve-filename-signature base)
         :slug (denote-retrieve-filename-title base)
         :keywords (denote-retrieve-filename-keywords-as-list base)
         :ext (file-name-extension base))))))

(defun denote-mcp--dir-key (dir)
  "Return a key that changes when notes are added, removed or renamed in DIR.
It holds the modification times of DIR and its immediate subdirectories."
  (let ((key (list (cons dir (file-attribute-modification-time (file-attributes dir))))))
    (dolist (entry (directory-files-and-attributes dir t "\\`[^.]" t))
      (when (eq (file-attribute-type (cdr entry)) t)
        (push (cons (car entry) (file-attribute-modification-time (cdr entry))) key)))
    key))

(defun denote-mcp--notes (&optional regexp)
  "Return `denote-mcp--note' structs for all notes, optionally matching REGEXP.
REGEXP is matched against the path relative to the denote directory.  The
full index is cached and revalidated by directory modification times, so
repeated calls avoid rescanning the tree."
  (let* ((dir (denote-mcp-get-directory))
         (key (denote-mcp--dir-key dir))
         (notes (if (equal key (car denote-mcp--cache))
                    (cdr denote-mcp--cache)
                  (let* ((denote-directory dir)
                         (files (if (fboundp 'denote-directory-files)
                                    (denote-directory-files nil nil t)
                                  (directory-files-recursively
                                   dir "\\`[0-9]\\{8\\}T[0-9]\\{6\\}.*\\.\\(org\\|md\\|txt\\)\\'")))
                         (built (delq nil (mapcar #'denote-mcp--note-from-path files))))
                    (setq denote-mcp--cache (cons key built))
                    built))))
    (if regexp
        (let ((skip (length dir)))
          (seq-filter (lambda (n)
                        (let ((path (denote-mcp--note-path n)))
                          ;; Cheaper than `file-relative-name' for the common case.
                          (string-match-p regexp (if (string-prefix-p dir path)
                                                     (substring path skip)
                                                   (file-relative-name path dir)))))
                      notes))
      notes)))

(defun denote-mcp--all-note-files (&optional regexp)
  "Return list of all regular denote note files matching optional REGEXP."
  (mapcar #'denote-mcp--note-path (denote-mcp--notes regexp)))

(defun denote-mcp--sequences ()
  "Return all sequence signatures of the notes, as a list of strings."
  (delq nil (mapcar #'denote-mcp--note-seq (denote-mcp--notes))))

(defmacro denote-mcp--with-scheme (&rest body)
  "Run BODY with `denote-sequence-scheme' set to `denote-mcp-sequence-scheme'."
  (declare (indent 0))
  `(let ((denote-sequence-scheme (or denote-mcp-sequence-scheme denote-sequence-scheme)))
     ,@body))

(defun denote-mcp--insert-head (file &optional bytes)
  "Insert the first BYTES (default 2048) of FILE into the current buffer.
File name handlers, coding detection and change hooks are bypassed: this is
several times faster than a plain `insert-file-contents' when reading many
files, and notes are UTF-8."
  (let ((file-name-handler-alist nil)
        (coding-system-for-read 'utf-8-unix)
        (inhibit-modification-hooks t))
    (insert-file-contents file nil 0 (or bytes 2048))))

(defun denote-mcp--head (file &optional bytes)
  "Return the first BYTES (default 2048) of FILE as a string."
  (with-temp-buffer
    (denote-mcp--insert-head file bytes)
    (buffer-string)))

(defun denote-mcp--title (file &optional fallback)
  "Return FILE's title from its front matter, else FALLBACK or its slug.
Reads only the head of the file."
  (or (let ((head (denote-mcp--head file 1024))
            (case-fold-search t))
        (cond
         ((string-match "^#\\+title:[ \t]*\\(.+?\\)[ \t]*$" head)
          (match-string 1 head))
         ((string-match "^title:[ \t]*\"?\\([^\"\n]+?\\)\"?[ \t]*$" head)
          (match-string 1 head))))
      fallback
      (when-let* ((n (denote-mcp--note-from-path file))) (denote-mcp--note-slug n))))

(defun denote-mcp--summary (note)
  "Return the common result plist describing NOTE (a `denote-mcp--note')."
  (let ((file (denote-mcp--note-path note)))
    (list :id (denote-mcp--note-id note)
          :sequence (denote-mcp--note-seq note)
          :title (denote-mcp--title file (denote-mcp--note-slug note))
          :keywords (or (denote-mcp--note-keywords note) [])
          :path file)))

(defmacro denote-mcp--with-visited (file &rest body)
  "Run BODY in a buffer visiting FILE, killing the buffer afterwards if new."
  (declare (indent 1))
  `(let* ((--f ,file)
          (--existing (get-file-buffer --f))
          ;; `find-file-noselect' also reverts an unmodified stale buffer.
          (--buf (find-file-noselect --f)))
     (unwind-protect (with-current-buffer --buf ,@body)
       (when (and (not --existing) (buffer-live-p --buf)
                  (not (buffer-modified-p --buf)))
         (kill-buffer --buf)))))

;;; Helper Functions

(defun denote-mcp--parent-sequence (seq)
  "Return the implied parent of sequence SEQ, or nil for a root sequence.
Components are split by `denote-sequence-split', so multi-digit parts such as
\"3a10\" are handled."
  (when-let* ((parts (ignore-errors (denote-sequence-split seq)))
              ((> (length parts) 1)))
    (denote-sequence-join (butlast parts) denote-sequence-scheme)))

(defun denote-mcp--find-file (file-or-slug)
  "Resolve FILE-OR-SLUG to an absolute file path in denote directory.
If FILE-OR-SLUG exists on disk, return it.  Otherwise searches the denote
directory and subdirectories for matching slug, identifier, or signature,
filtering out backup, lock, and autosave files."
  (cond
   ((and (stringp file-or-slug) (file-exists-p file-or-slug))
    (expand-file-name file-or-slug))
   ((and (stringp file-or-slug) (not (string-empty-p file-or-slug)))
    (let* ((pattern (regexp-quote file-or-slug))
           (files (denote-mcp--all-note-files pattern)))
      (seq-find (lambda (f)
                  (let ((base (file-name-nondirectory f)))
                    (and (file-regular-p f)
                         (not (backup-file-name-p f))
                         (not (string-prefix-p ".#" base))
                         (not (string-suffix-p "#" base)))))
                files)))
   (t nil)))

(defun denote-mcp--parse-filename (file)
  "Parse denote FILE into a plist :id, :seq, :title, :keywords, :ext."
  (when (stringp file)
    (when-let* ((n (denote-mcp--note-from-path file)))
      (list :id (denote-mcp--note-id n)
            :seq (denote-mcp--note-seq n)
            :title (denote-mcp--note-slug n)
            :keywords (or (denote-mcp--note-keywords n) [])
            :ext (denote-mcp--note-ext n)))))

(defun denote-mcp--get-metadata (file)
  "Extract structured frontmatter and file metadata from FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (let* ((get-org (lambda (key)
                      (goto-char (point-min))
                      (if (re-search-forward (concat "^#\\+" key ":[ \t]*\\(.*\\)$") nil t)
                          (string-trim (match-string 1)) "")))
           (title (funcall get-org "title"))
           (date (funcall get-org "date"))
           (tags (funcall get-org "filetags"))
           (id (funcall get-org "identifier"))
           (sig (funcall get-org "signature"))
           (status (funcall get-org "execution_status"))
           (ext (file-name-extension file)))
      ;; Fallback to filename components if frontmatter is empty
      (let ((parsed (denote-mcp--parse-filename file)))
        (when (string-empty-p id)
          (setq id (or (plist-get parsed :id) "")))
        (when (string-empty-p sig)
          (setq sig (or (plist-get parsed :seq) "")))
        (when (string-empty-p title)
          (setq title (or (plist-get parsed :title) (file-name-base file))))
        (list :path file
              :id id
              :signature (if (string-empty-p sig) nil sig)
              :title title
              :date (if (string-empty-p date) nil date)
              :filetags (if (string-empty-p tags)
                            (or (plist-get parsed :keywords) [])
                          (split-string (string-trim tags ":" ":") ":" t))
              :execution_status (if (string-empty-p status) nil status)
              :format ext)))))

(defun denote-mcp--set-sequence (file seq)
  "Assign sequence SEQ to FILE by updating #+signature: and renaming via denote.
If SEQ is nil or empty, removes any existing signature."
  (let ((path (denote-mcp--find-file file)))
    (unless (and path (file-exists-p path))
      (error "File not found: %s" file))
    (denote-mcp-mutating
     (denote-mcp--with-visited path
       ;; Remove existing signature lines
       (goto-char (point-min))
       (while (re-search-forward "^#\\+signature:.*\\n?" nil t)
         (delete-region (match-beginning 0) (match-end 0)))
       ;; Insert new signature if non-empty
       (when (and seq (not (string-empty-p seq)))
         (goto-char (point-min))
         (if (re-search-forward "^#\\+identifier:.*$" nil t)
             (progn (end-of-line) (insert "\n#+signature: " seq))
           (goto-char (point-min))
           (while (and (not (eobp)) (looking-at "^#\\+"))
             (forward-line 1))
           (beginning-of-line)
           (insert "#+signature: " seq "\n")))
       (save-buffer)
       (denote-rename-file-using-front-matter path)
       (buffer-file-name)))))

(defvar denote-file-has-backlinks-function)

(defun denote-mcp--set-identifier (file new-id)
  "Change FILE's #+identifier: to NEW-ID (YYYYMMDDTHHMMSS) and rename.
The caller rewrites links afterwards, so denote's backlink guard is disabled."
  (let ((path (denote-mcp--find-file file)))
    (unless (and path (file-exists-p path))
      (error "File not found: %s" file))
    (unless (string-match-p "\\`[0-9]\\{8\\}T[0-9]\\{6\\}\\'" new-id)
      (user-error "Invalid identifier format (expected YYYYMMDDTHHMMSS): %s" new-id))
    (let* ((time (encode-time (parse-time-string
                               (format "%s-%s-%sT%s:%s:%s"
                                       (substring new-id 0 4) (substring new-id 4 6) (substring new-id 6 8)
                                       (substring new-id 9 11) (substring new-id 11 13) (substring new-id 13 15)))))
           (date-str (format-time-string "[%Y-%m-%d %a %H:%M]" time)))
      (denote-mcp-mutating
       ;; `denote--file-has-backlinks-p' guarded older denote; newer denote
       ;; reads `denote-file-has-backlinks-function'.
       (let ((denote-file-has-backlinks-function #'ignore))
         (cl-letf (((symbol-function 'denote--file-has-backlinks-p) (lambda (&rest _) nil)))
           (denote-mcp--with-visited path
             (goto-char (point-min))
             (if (re-search-forward "^#\\+identifier:.*$" nil t)
                 (replace-match (concat "#+identifier: " new-id))
               (error "No #+identifier: found in %s" path))
             (goto-char (point-min))
             (when (re-search-forward "^#\\+date:.*$" nil t)
               (replace-match (concat "#+date:       " date-str)))
             (save-buffer)
             (denote-rename-file-using-front-matter path)
             (buffer-file-name))))))))

(defun denote-mcp--files-containing (string files)
  "Return the members of FILES whose contents include the fixed STRING.
One `grep -F' process is used when available; otherwise FILES are read into a
single reused buffer."
  (if-let* ((grep (and files (executable-find "grep"))))
      (with-temp-buffer
        (let ((default-directory (denote-mcp-get-directory)))
          ;; Chunk the arguments to stay below ARG_MAX on huge trees.
          (dolist (chunk (seq-partition files 400))
            (apply #'process-file grep nil t nil "-lFZ" "-e" string "--" chunk)))
        (split-string (buffer-string) "\0" t))
    (with-temp-buffer
      (let ((file-name-handler-alist nil)
            (coding-system-for-read 'utf-8-unix)
            (inhibit-modification-hooks t))
        (seq-filter (lambda (f)
                      (erase-buffer)
                      (insert-file-contents f)
                      (goto-char (point-min))
                      (search-forward string nil t))
                    files)))))

(defun denote-mcp--git-grep-files (string dir)
  "Return absolute paths of files tracked by the git repo around DIR with STRING."
  (when-let* ((root (locate-dominating-file dir ".git"))
              (default-directory root))
    (with-temp-buffer
      (when (zerop (process-file "git" nil '(t nil) nil "grep" "-lFz" "-e" string))
        (mapcar (lambda (f) (expand-file-name f root))
                (split-string (buffer-string) "\0" t))))))

(defun denote-mcp--replace-links (old-id new-id &optional files)
  "Replace occurrences of OLD-ID with NEW-ID across denote files.
FILES limits the search to those paths.  Only files that actually contain
OLD-ID are visited.  Return the number of
replacements."
  (let* ((candidates (or files (denote-mcp--all-note-files)))
         (target-files (denote-mcp--files-containing
                        old-id (seq-filter #'file-exists-p candidates)))
         (count 0))
    (denote-mcp-mutating
     (dolist (path target-files)
       (denote-mcp--with-visited path
         (goto-char (point-min))
         (while (search-forward old-id nil t)
           (replace-match new-id t t)
           (cl-incf count))
         (when (buffer-modified-p)
           (when (derived-mode-p 'org-mode)
             (ignore-errors (org-element-cache-reset)))
           (save-buffer)))))
    count))

;;; Frontmatter synchronization

(defun denote-mcp--md-frontmatter-bounds ()
  "Return (START . END) of the YAML front matter of the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "---[ \t]*\n")
      (let ((start (match-end 0)))
        (goto-char start)
        (when (re-search-forward "^---" nil t)
          (cons start (line-beginning-position)))))))

(defun denote-mcp--md-get (key bounds)
  "Return KEY's value within BOUNDS, accepting `key: v' and `#+key: v' forms."
  (save-excursion
    (goto-char (car bounds))
    (when (re-search-forward (format "^#?\\+?%s:[ \t]*\"?\\([^\"\n]*\\)\"?" key) (cdr bounds) t)
      (let ((v (string-trim (match-string 1))))
        (unless (string-empty-p v) v)))))

(defun denote-mcp--org-get (key)
  "Return the value of #+KEY: in the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:[ \t]*\\(.*\\)$" key) nil t)
      (let ((v (string-trim (match-string 1))))
        (unless (string-empty-p v) v)))))

(defun denote-mcp--frontmatter-in-sync-p (note)
  "Return non-nil if the front matter in the current buffer matches NOTE's name.
The buffer holds the head of NOTE's file."
  (let ((want-id (denote-mcp--note-id note))
        (want-sig (denote-mcp--note-seq note)))
    (goto-char (point-min))
    (pcase (denote-mcp--note-ext note)
      ("org" (and (equal (denote-mcp--org-get "identifier") want-id)
                  (equal (denote-mcp--org-get "signature") want-sig)))
      ;; Markdown notes carry either fenced YAML or bare #+key: lines.
      ("md" (if-let* ((b (denote-mcp--md-frontmatter-bounds)))
                (progn
                  (goto-char (car b))
                  (and (not (re-search-forward "^#\\+[a-zA-Z]+:" (cdr b) t))
                       (equal (denote-mcp--md-get "identifier" b) want-id)
                       (equal (denote-mcp--md-get "signature" b) want-sig)))
              (and (equal (denote-mcp--org-get "identifier") want-id)
                   (equal (denote-mcp--org-get "signature") want-sig))))
      (_ t))))

(defun denote-mcp--sync-org-buffer (base want-id want-sig)
  "Fix identifier and signature in the current Org buffer, return change strings.
BASE is the file name for messages; WANT-ID and WANT-SIG come from it."
  (let (changes)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+identifier:[ \t]*\\(.*\\)$" nil t)
        (let ((cur (string-trim (match-string 1))))
          (unless (equal cur want-id)
            (replace-match (concat "#+identifier: " want-id))
            (push (format "%s: identifier %s -> %s" base cur want-id) changes)))
      (goto-char (point-min))
      (insert "#+identifier: " want-id "\n")
      (push (format "%s: inserted identifier %s" base want-id) changes))
    (goto-char (point-min))
    (let ((has-sig (re-search-forward "^#\\+signature:[ \t]*\\(.*\\)$" nil t)))
      (cond
       (want-sig
        (if has-sig
            (let ((cur (string-trim (match-string 1))))
              (unless (equal cur want-sig)
                (replace-match (concat "#+signature:  " want-sig))
                (push (format "%s: signature %s -> %s" base cur want-sig) changes)))
          (goto-char (point-min))
          (if (re-search-forward "^#\\+identifier:.*$" nil t)
              (progn (end-of-line) (insert "\n#+signature:  " want-sig))
            (insert "#+signature:  " want-sig "\n"))
          (push (format "%s: inserted signature %s" base want-sig) changes)))
       ((and (not want-sig) has-sig)
        (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))
        (push (format "%s: removed obsolete signature" base) changes))))
    changes))

(defun denote-mcp--md-set (key value bounds)
  "Set KEY to VALUE inside BOUNDS of the current Markdown buffer."
  (save-excursion
    (goto-char (car bounds))
    (if (re-search-forward (format "^#?\\+?%s:[ \t]*.*$" key) (cdr bounds) t)
        (progn (delete-region (line-beginning-position) (line-end-position))
               (insert (format "%s: \"%s\"" key value)))
      (goto-char (cdr bounds))
      (insert (format "%s: \"%s\"\n" key value)))))

(defun denote-mcp--sync-md-buffer (base want-id want-sig)
  "Fix identifier and signature in the current Markdown YAML buffer.
BASE, WANT-ID and WANT-SIG are as in `denote-mcp--sync-org-buffer'.
Also converts `#+key: v' lines inside the front matter to `key: \"v\"'.
Return change strings."
  (let (changes)
    (when-let* ((b (denote-mcp--md-frontmatter-bounds)))
      (save-excursion
        (goto-char (car b))
        (let ((end (copy-marker (cdr b))))
          (while (re-search-forward "^#\\+\\([a-zA-Z]+\\):[ \t]*\\(.*\\)$" end t)
            (let ((k (downcase (match-string 1))) (v (string-trim (match-string 2))))
              (delete-region (line-beginning-position) (line-end-position))
              (insert (format "%s: \"%s\"" k v))
              (unless changes
                (push (format "%s: repaired mixed #+key: syntax in YAML front matter" base)
                      changes))))
          (set-marker end nil)))
      (setq b (denote-mcp--md-frontmatter-bounds))
      (let ((cur-id (denote-mcp--md-get "identifier" b))
            (cur-sig (denote-mcp--md-get "signature" b)))
        (unless (equal cur-id want-id)
          (denote-mcp--md-set "identifier" want-id b)
          (push (format "%s: identifier %s -> %s" base (or cur-id "nil") want-id) changes))
        (setq b (denote-mcp--md-frontmatter-bounds))
        (cond
         (want-sig
          (unless (equal cur-sig want-sig)
            (denote-mcp--md-set "signature" want-sig b)
            (push (format "%s: signature %s -> %s" base (or cur-sig "nil") want-sig) changes)))
         (cur-sig
          (save-excursion
            (goto-char (car b))
            (when (re-search-forward "^#?\\+?signature:.*\n?" (cdr b) t)
              (delete-region (match-beginning 0) (match-end 0))))
          (push (format "%s: removed obsolete signature" base) changes)))))
    changes))

(defun denote-mcp--sync-note (note scratch)
  "Make NOTE's front matter match its file name; return change strings.
SCRATCH is a reusable buffer; notes whose first 4 KiB already match are
skipped without being visited."
  (let* ((f (denote-mcp--note-path note))
         (ext (denote-mcp--note-ext note))
         (base (file-name-nondirectory f)))
    (when (and (member ext '("org" "md"))
               (denote-mcp--note-id note)
               (not (with-current-buffer scratch
                      (erase-buffer)
                      (denote-mcp--insert-head f 4096)
                      (denote-mcp--frontmatter-in-sync-p note))))
      (let ((want-id (denote-mcp--note-id note))
            (want-sig (denote-mcp--note-seq note))
            changes)
        (denote-mcp--with-visited f
          (setq changes (if (and (equal ext "md") (denote-mcp--md-frontmatter-bounds))
                            (denote-mcp--sync-md-buffer base want-id want-sig)
                          (denote-mcp--sync-org-buffer base want-id want-sig)))
          (when changes
            (when (derived-mode-p 'org-mode)
              (ignore-errors (org-element-cache-reset)))
            (save-buffer)))
        (nreverse changes)))))

(defun denote-mcp--sync-frontmatter (&optional file)
  "Sync identifier and signature front matter to match file names.
If FILE is given, sync only FILE; otherwise sync every Org and Markdown note.
Return a list of change descriptions."
  (let ((notes (if file
                   (when-let* ((path (denote-mcp--find-file file))
                               (n (denote-mcp--note-from-path path)))
                     (list n))
                 (denote-mcp--notes)))
        changes)
    (with-temp-buffer
      (let ((scratch (current-buffer)))
        (denote-mcp-silently
         (dolist (n notes)
           (setq changes (nconc changes (denote-mcp--sync-note n scratch)))))))
    ;; Only contents change here, never names, so the note index stays valid.
    changes))

;;; Service Definition & Top-Level Tool Registration

(defvar denote-mcp-service
  (or (mcpkit-get-service 'denote)
      (mcpkit-define-service 'denote
        :description "Denote Note-Taking and Knowledge Base Management"))
  "The Denote `mcpkit-service' instance.")

;; 1. denote_find (also backs denote_find_by_slug and denote_find_most_recent)
(cl-defun denote-mcp--find-notes (&key query tags sequence prefix sort limit)
  "Return up to LIMIT notes matching all given criteria.
QUERY is a regexp on the relative file name, TAGS a list that must all be
present, SEQUENCE an exact signature, PREFIX a signature prefix.  SORT is
`recent' for newest identifier first."
  (let* ((notes (denote-mcp--notes query))
         (hits (seq-filter
                (lambda (n)
                  (let ((seq (denote-mcp--note-seq n)))
                    (and (or (null sequence) (equal seq sequence))
                         (or (null prefix) (and seq (string-prefix-p prefix seq)))
                         (or (null tags)
                             (let ((kw (denote-mcp--note-keywords n)))
                               (seq-every-p (lambda (tg) (member tg kw)) tags))))))
                notes)))
    (when (eq sort 'recent)
      (setq hits (sort (copy-sequence hits)
                       (lambda (a b) (string> (file-name-nondirectory (denote-mcp--note-path a))
                                              (file-name-nondirectory (denote-mcp--note-path b)))))))
    (if limit (seq-take hits limit) hits)))

(mcpkit-register-tool 'denote_find 'denote
  :description "Find notes by any combination of: regex over the file name (title, keywords, identifier, sequence), required tags, exact sequence, sequence prefix. Optionally newest first. Returns id, sequence, title, keywords, path."
  :input-schema '(:type "object"
                  :properties (:query (:type "string" :description "Regex over the file name (optional)")
                               :tags (:type "array" :items (:type "string") :description "Tags that must all be present")
                               :sequence (:type "string" :description "Exact sequence signature")
                               :sequence_prefix (:type "string" :description "Sequence prefix (subtree)")
                               :sort (:type "string" :enum ["name" "recent"] :description "recent = newest identifier first")
                               :max_results (:type "integer" :description "Max results (default 50)"))
                  :required [])
  (mapcar #'denote-mcp--summary
          (denote-mcp--find-notes
           :query (plist-get args :query)
           :tags (seq-into (plist-get args :tags) 'list)
           :sequence (plist-get args :sequence)
           :prefix (plist-get args :sequence_prefix)
           :sort (and (equal (plist-get args :sort) "recent") 'recent)
           :limit (or (plist-get args :max_results) 50))))

;; 2. denote_find_by_slug
(mcpkit-register-tool 'denote_find_by_slug 'denote
  :description "Look up a note by title slug or identifier fragment."
  :input-schema '(:type "object"
                  :properties (:slug (:type "string" :description "Unique title slug or identifier fragment"))
                  :required ["slug"])
  (let* ((slug (plist-get args :slug))
         (file (denote-mcp--find-file slug))
         (note (and file (denote-mcp--note-from-path file))))
    (if (not note)
        (list :found :json-false :slug slug)
      (append (list :found t) (denote-mcp--summary note)))))

;; 3. denote_find_most_recent
(mcpkit-register-tool 'denote_find_most_recent 'denote
  :description "Find the most recently authored note matching all specified tags (e.g. [\"agent\", \"plan\"]). Same as denote_find with sort=recent, max_results=1."
  :input-schema '(:type "object"
                  :properties (:tags (:type "array" :items (:type "string") :description "List of required tags")))
  (if-let* ((note (car (denote-mcp--find-notes
                        :tags (seq-into (plist-get args :tags) 'list)
                        :sort 'recent :limit 1))))
      (append (list :found t) (denote-mcp--summary note))
    (list :found :json-false)))

;; 4. denote_get_metadata
(mcpkit-register-tool 'denote_get_metadata 'denote
  :description "Retrieve structured metadata (id, sequence, title, tags, date, execution_status) for a note."
  :input-schema '(:type "object"
                  :properties (:file_or_slug (:type "string" :description "File path or slug fragment"))
                  :required ["file_or_slug"])
  (let* ((target (plist-get args :file_or_slug))
         (file (denote-mcp--find-file target)))
    (unless file
      (error "Note not found for: %s" target))
    (denote-mcp--get-metadata file)))

;; 5. denote_read_note
(mcpkit-register-tool 'denote_read_note 'denote
  :description "Safely read the text content of a denote note with UTF-8 encoding."
  :input-schema '(:type "object"
                  :properties (:file_or_slug (:type "string" :description "File path or slug fragment")
                               :max_bytes (:type "integer" :description "Optional max bytes to read")
                               :include_metadata (:type "boolean" :description "Also return the get_metadata result (saves a round trip)"))
                  :required ["file_or_slug"])
  (let* ((target (plist-get args :file_or_slug))
         (file (denote-mcp--find-file target))
         (max-b (plist-get args :max_bytes)))
    (unless file
      (error "Note not found for: %s" target))
    (let ((result
           (denote-mcp-silently
            (with-temp-buffer
              (insert-file-contents file nil 0 max-b)
              (list :path file
                    :content (buffer-string)
                    :truncated (if (and max-b (> (file-attribute-size (file-attributes file)) max-b))
                                   t :json-false))))))
      (if (eq t (plist-get args :include_metadata))
          (append result (list :metadata (denote-mcp--get-metadata file)))
        result))))

;; 6. denote_create_note
(mcpkit-register-tool 'denote_create_note 'denote
  :description "Create a new denote note with optional Folgezettel sequence and initial body."
  :input-schema '(:type "object"
                  :properties (:title (:type "string" :description "Title of the note")
                               :keywords (:type "array" :items (:type "string") :description "List of tag strings")
                               :sequence (:type "string" :description "Optional Folgezettel sequence (e.g. '3a1')")
                               :content (:type "string" :description "Initial note content body")
                               :file_type (:type "string" :enum ["org" "markdown" "text"] :description "File type (default org)")
                               :date (:type "string" :description "Optional date string YYYY-MM-DD")
                               :type (:type "string" :enum ["plan" "report" "tracker"]
                                            :description "Typed agent note: sets tags and template, and captures a standing TODO to agentic.org for plans and trackers")
                               :sequence_or_parent (:type "string" :description "Typed notes only: parent signature (next child) or an unused exact signature"))
                  :required ["title"])
  (if (plist-get args :type)
      (denote-mcp-notes-create args)
  (let* ((title (plist-get args :title))
         (keywords (seq-into (plist-get args :keywords) 'list))
         (seq (plist-get args :sequence))
         (content (plist-get args :content))
         (file-type (intern (or (plist-get args :file_type) "org")))
         (date-str (plist-get args :date))
         (date (when date-str (date-to-time (concat date-str " 00:00:00"))))
         (denote-dir (denote-mcp-get-directory)))
    (denote-mcp-mutating
     (let ((file (save-window-excursion
                   (denote title keywords file-type denote-dir date nil
                           (and seq (not (string-empty-p seq)) seq)))))
       (unless file (error "denote returned nil for %s" title))
       ;; Save initial content
       (with-current-buffer (find-file-noselect file)
         (when content
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (insert content))
         (basic-save-buffer))
       (let ((meta (denote-mcp--get-metadata file)))
         (list :path file
               :id (plist-get meta :id)
               :sequence (plist-get meta :signature)
               :title (plist-get meta :title)
               :keywords (plist-get meta :filetags))))))))

;; 7. denote_append_body
(mcpkit-register-tool 'denote_append_body 'denote
  :description "Append text to a note, optionally under an Org heading or custom ID."
  :input-schema '(:type "object"
                  :properties (:file_or_slug (:type "string" :description "File path or slug fragment")
                               :content (:type "string" :description "Text content to append")
                               :target_heading (:type "string" :description "Target Org heading")
                               :custom_id (:type "string" :description "Target Org CUSTOM_ID")
                               :headline_title (:type "string" :description "Add content under a new child heading with this title")
                               :todo_state (:type "string" :description "TODO keyword for the new child heading")
                               :priority (:type "string" :description "Priority for the new child heading")
                               :properties (:type "object" :description "Properties for the new child heading"))
                  :required ["file_or_slug" "content"])
  (let* ((target (plist-get args :file_or_slug))
         (content (plist-get args :content))
         (heading (plist-get args :target_heading))
         (custom-id (plist-get args :custom_id))
         (file (denote-mcp--find-file target)))
    (unless file (error "Note not found for: %s" target))
    (if (or heading custom-id (plist-get args :headline_title))
        (progn
          (unless (string-equal (downcase (or (file-name-extension file) "")) "org")
            (user-error "Targeted append requires an Org note: %s" file))
          (mcpkit-org-append-entry file content :target-heading heading :custom-id custom-id
                                   :headline-title (plist-get args :headline_title)
                                   :todo-state (plist-get args :todo_state)
                                   :priority (plist-get args :priority)
                                   :properties (plist-get args :properties))
          (list :path file :bytes_appended (length content)))
      (denote-mcp-silently
       (with-current-buffer (find-file-noselect file)
         (goto-char (point-max))
         (unless (bolp) (insert "\n"))
         (insert content)
         (when (derived-mode-p 'org-mode)
           (ignore-errors (org-element-cache-reset)))
         (basic-save-buffer)
         (list :path file :bytes_appended (length content)))))))

;; 8. denote_rename (and its keyword-only alias denote_retag)
(defun denote-mcp--list (value)
  "Return VALUE (list, vector or nil) as a list."
  (seq-into (or value nil) 'list))

(cl-defun denote-mcp--update-note (file &key title set add remove)
  "Change FILE's TITLE and/or keywords, return a result plist.
SET replaces the keyword list first; ADD and REMOVE then adjust it.  Nothing
is touched when the result equals the current state (:changed nil)."
  (let* ((old (denote-retrieve-filename-keywords-as-list file))
         (kws (denote-keywords-sort
               (seq-remove (lambda (k) (member k remove))
                           (seq-uniq (append (if set set old) add)))))
         (keywords-changed (not (equal kws (denote-keywords-sort (copy-sequence old)))))
         (old-title (and title (denote-mcp--title file)))
         (title-changed (and title (not (equal title old-title)))))
    (if (not (or keywords-changed title-changed))
        (list :old_path file :new_path file :path file
              :title (or old-title (denote-mcp--title file)) :keywords (or old []) :changed :json-false)
      (denote-mcp-mutating
       (let ((new-path (denote-rename-file file
                                           (if title-changed title 'keep-current)
                                           (if keywords-changed kws 'keep-current)
                                           'keep-current 'keep-current 'keep-current)))
         (list :old_path file :new_path new-path :path new-path
               :title (denote-mcp--title new-path)
               :keywords (or (denote-retrieve-filename-keywords-as-list new-path) [])
               :changed t))))))

(defun denote-mcp--update-note-args (args &optional keyword-only)
  "Apply the rename/retag ARGS plist, honoring a `files' batch.
With KEYWORD-ONLY, ARGS use `set' instead of `new_keywords' and no title."
  (let* ((targets (or (denote-mcp--list (plist-get args :files))
                      (list (plist-get args :file_or_slug))))
         (run (lambda (target)
                (let ((file (denote-mcp--find-file target)))
                  (unless file (error "Note not found for: %s" target))
                  (denote-mcp--update-note
                   file
                   :title (and (not keyword-only) (plist-get args :new_title))
                   :set (denote-mcp--list (if keyword-only (plist-get args :set)
                                            (plist-get args :new_keywords)))
                   :add (denote-mcp--list (plist-get args :add_keywords))
                   :remove (denote-mcp--list (plist-get args :remove_keywords)))))))
    (if (plist-get args :files)
        (list :results (vconcat (mapcar run targets)))
      (funcall run (car targets)))))

(mcpkit-register-tool 'denote_rename 'denote
  :description "Update a note's title and/or keywords, rewriting frontmatter and renaming on disk. new_keywords replaces the list; add_keywords/remove_keywords adjust it incrementally. Idempotent (changed=false when nothing differs). Pass files[] to batch keyword edits."
  :input-schema '(:type "object"
                  :properties (:file_or_slug (:type "string" :description "File path or slug fragment")
                               :files (:type "array" :items (:type "string") :description "Batch: several files or slugs instead of file_or_slug")
                               :new_title (:type "string" :description "New title string (or omit to keep)")
                               :new_keywords (:type "array" :items (:type "string") :description "Replace the keywords list")
                               :add_keywords (:type "array" :items (:type "string") :description "Keywords to add")
                               :remove_keywords (:type "array" :items (:type "string") :description "Keywords to remove"))
                  :required [])
  (denote-mcp--update-note-args args))

(mcpkit-register-tool 'denote_retag 'denote
  :description "Add, remove or replace keywords (filetags) of one note or a batch (files[]); updates frontmatter and filename. Idempotent. Keyword-only form of denote_rename."
  :input-schema '(:type "object"
                  :properties (:file_or_slug (:type "string" :description "File path or slug fragment")
                               :files (:type "array" :items (:type "string") :description "Batch: several files or slugs")
                               :add (:type "array" :items (:type "string") :description "Keywords to add")
                               :remove (:type "array" :items (:type "string") :description "Keywords to remove")
                               :set (:type "array" :items (:type "string") :description "Replace the keyword list first"))
                  :required [])
  (denote-mcp--update-note-args
   (list :file_or_slug (plist-get args :file_or_slug) :files (plist-get args :files)
         :set (plist-get args :set)
         :add_keywords (plist-get args :add) :remove_keywords (plist-get args :remove))
   t))

;; Convert Markdown notes to Org, including their body.
(mcpkit-register-tool 'denote_convert_to_org 'denote
  :description "Convert a Denote Markdown YAML note and its body to Org, preserving its identifier and signature. Requires denote-convert and denote-sync with Markdown tree-sitter grammars."
  :input-schema '(:type "object"
                  :properties (:file_or_slug (:type "string" :description "Denote Markdown note path, identifier, or slug"))
                  :required ["file_or_slug"])
  (let* ((target (plist-get args :file_or_slug))
         (file (denote-mcp--find-file target)))
    (unless file (error "Note not found for: %s" target))
    (unless (eq (denote-filetype-heuristics file) 'markdown-yaml)
      (error "Expected a Denote Markdown YAML note: %s" file))
    (unless (denote-retrieve-filename-identifier file)
      (error "Note has no Denote identifier: %s" file))
    (let ((visiting (get-file-buffer file))
          (destination (concat (file-name-sans-extension file) ".org")))
      (when (and visiting (buffer-modified-p visiting))
        (error "Save the modified note before conversion: %s" file))
      (when (file-exists-p destination)
        (error "Destination already exists: %s" destination))
      (require 'denote-convert)
      (require 'denote-sync)
      (require 'mdwn)
      (denote-mcp-silently
       (let* ((source-hash (with-temp-buffer
                           (insert-file-contents-literally file)
                           (secure-hash 'sha256 (current-buffer))))
            (_validated
             (with-temp-buffer
               (insert-file-contents file)
               (goto-char (point-min))
               (unless (looking-at "^---[ \t]*$")
                 (error "Missing YAML front matter: %s" file))
               (forward-line 1)
               (let (seen)
                 (while (and (not (eobp)) (not (looking-at "^---[ \t]*$")))
                   (unless (looking-at "^\\(title\\|date\\|tags\\|identifier\\|signature\\):[^\n]*$")
                     (error "Unsupported YAML front matter; conversion would lose metadata: %s" file))
                   (let ((key (match-string 1)))
                     (when (member key seen) (error "Duplicate YAML field: %s" key))
                     (push key seen))
                   (forward-line 1)))
               (when (eobp) (error "Unterminated YAML front matter: %s" file))))
            (body (denote-sync--body-without-front-matter file))
              (mdwn-install-grammars nil)
              (converted (mdwn-to-org body))
              (temp-dir (make-temp-file
                         (expand-file-name ".denote-convert-" (file-name-directory file)) t))
              (temp-source (expand-file-name (file-name-nondirectory file) temp-dir))
              (temp-target (expand-file-name (file-name-nondirectory destination) temp-dir)))
         (unwind-protect
             (progn
               (copy-file file temp-source)
               (let ((denote-convert-after-conversion-functions nil))
                 (denote-convert-file-type temp-source 'org))
               (unless (file-exists-p temp-target)
                 (error "Converted note has an unexpected filename"))
               (with-current-buffer (find-file-noselect temp-target)
                 (unwind-protect
                     (progn
                       (goto-char (point-min))
                       (delete-region (denote-convert--front-matter-end 'org) (point-max))
                       (goto-char (point-max))
                       (insert "\n" converted "\n")
                       (save-buffer))
                   (kill-buffer (current-buffer))))
               (unless (equal source-hash
                              (with-temp-buffer
                                (insert-file-contents-literally file)
                                (secure-hash 'sha256 (current-buffer))))
                 (error "Source changed during conversion: %s" file))
               ;; Do not replace an existing target, even if it appeared during conversion.
               (rename-file temp-target destination nil)
               (condition-case err
                   (delete-file file)
                 (error (delete-file destination) (signal (car err) (cdr err))))
               (when (buffer-live-p visiting)
                 (with-current-buffer visiting
                   (set-visited-file-name destination t t)
                   (revert-buffer t t)))
               (list :old_path file :new_path destination
                     :identifier (denote-retrieve-filename-identifier destination)
                     :file_type "org"))
           (delete-directory temp-dir t)))))))

;; 9. denote_mark_executed
(defun denote-mcp--mark-executed (file)
  "Mark FILE executed: add the executed keyword, drop `done', set status."
  (let* ((kw denote-mcp-executed-keyword)
         (raw (denote-retrieve-filename-keywords-as-list file)))
    (if (and (member kw raw) (not (member "done" raw)))
        (list :path file :status "already_marked")
      (let* ((clean (seq-remove (lambda (k) (member k '("done" "x"))) raw))
             (new-kws (cons kw clean)))
        (denote-mcp-mutating
         (denote-mcp--with-visited file
           (goto-char (point-min))
           (when (re-search-forward "^#\\+filetags:.*" nil t)
             (replace-match (format "#+filetags:   :%s:" (mapconcat #'identity new-kws ":"))))
           (goto-char (point-min))
           (when (re-search-forward "^#\\+execution_status:[ \t]*.*$" nil t)
             (replace-match "#+execution_status: complete"))
           (when (derived-mode-p 'org-mode)
             (ignore-errors (org-element-cache-reset)))
           (save-buffer))
         (let ((new-path (denote-rename-file file 'keep-current new-kws 'keep-current 'keep-current 'keep-current)))
           (list :path new-path :status "marked" :keywords new-kws)))))))

(mcpkit-register-tool 'denote_mark_executed 'denote
  :description "Mark plan notes as executed: applies 'x' filetag, cleans 'done', and updates execution status. Pass files[] to mark several in one call."
  :input-schema '(:type "object"
                  :properties (:file_or_slug (:type "string" :description "File path or slug fragment")
                               :files (:type "array" :items (:type "string") :description "Batch: several files or slugs"))
                  :required [])
  (let ((run (lambda (target)
               (let ((file (denote-mcp--find-file target)))
                 (unless file (error "Note not found for: %s" target))
                 (denote-mcp--mark-executed file)))))
    (if (plist-get args :files)
        (list :results (vconcat (mapcar run (denote-mcp--list (plist-get args :files)))))
      (funcall run (plist-get args :file_or_slug)))))

;; 10. denote_sync_frontmatter
(mcpkit-register-tool 'denote_sync_frontmatter 'denote
  :description "Synchronize note frontmatter (#+identifier:, #+signature:) to match filenames."
  :input-schema '(:type "object"
                  :properties (:file_or_slug (:type "string" :description "Optional single note to sync")))
  (let* ((target (plist-get args :file_or_slug))
         (changes (denote-mcp--sync-frontmatter target)))
    (list :changes_count (length changes)
          :changes (vconcat changes))))

;; 11. denote_seq_get_next
(mcpkit-register-tool 'denote_seq_get_next 'denote
  :description "Calculate the next available sequence (child or sibling for target_sequence or file_or_slug, or root parent sequence if omitted)."
  :input-schema '(:type "object"
                  :properties (:target_sequence (:type "string" :description "Optional parent or sibling sequence string, e.g. '3a1'")
                               :file_or_slug (:type "string" :description "Optional note file path or slug fragment to resolve sequence from")
                               :type (:type "string" :enum ["parent" "child" "sibling"] :description "Optional sequence relation type"))
                  :required [])
  (let* ((target (plist-get args :file_or_slug))
         (seq (or (plist-get args :target_sequence)
                  (when (and target (not (string-empty-p target)))
                    (when-let* ((file (denote-mcp--find-file target)))
                      (denote-mcp--note-seq (denote-mcp--note-from-path file))))))
         (type-str (plist-get args :type))
         (rel-type (when type-str (intern type-str)))
         (all (denote-mcp--sequences)))
    (denote-mcp--with-scheme
      (if (and seq (not (string-empty-p seq)))
          (let* ((has-children
                  (cl-some (lambda (s) (and (not (string= s seq)) (string-prefix-p seq s)))
                           (denote-sequence-get-all-sequences-with-prefix seq all)))
                 (next-seq (cond
                            ((eq rel-type 'child) (denote-sequence-get-new 'child seq all))
                            ((eq rel-type 'sibling) (denote-sequence-get-new 'sibling seq all))
                            (has-children (denote-sequence-get-new 'child seq all))
                            ((< (denote-sequence-depth seq) 3)
                             (denote-sequence-get-new 'child seq all))
                            (t (denote-sequence-get-new 'sibling seq all))))
                 (rel (if (string-prefix-p seq next-seq) "child" "sibling")))
            (list :target_sequence seq
                  :next_sequence next-seq
                  :relation rel))
        (if (memq rel-type '(child sibling))
            (error "Cannot calculate %s sequence without a target_sequence or file_or_slug" rel-type)
          (list :target_sequence ""
                :next_sequence (denote-sequence-get-new 'parent nil all)
                :relation "parent"))))))

;; 12. denote_seq_set
(mcpkit-register-tool 'denote_seq_set 'denote
  :description "Assign sequence signature to a note and rename file to match."
  :input-schema '(:type "object"
                  :properties (:file_or_slug (:type "string" :description "File path or slug fragment")
                               :sequence (:type "string" :description "New sequence signature (e.g. '3a1b')"))
                  :required ["file_or_slug" "sequence"])
  (let* ((target (plist-get args :file_or_slug))
         (seq (plist-get args :sequence))
         (new-path (denote-mcp--set-sequence target seq)))
    (list :path new-path :sequence seq)))

;; 13. denote_seq_graft
(mcpkit-register-tool 'denote_seq_graft 'denote
  :description "Batch reassign sequences safely ordered in reverse depth to avoid slot collisions."
  :input-schema '(:type "object"
                  :properties (:pairs (:type "array"
                                       :items (:type "object"
                                               :properties (:slug (:type "string")
                                                            :new_sequence (:type "string"))
                                               :required ["slug" "new_sequence"])))
                  :required ["pairs"])
  (let* ((pairs (seq-into (plist-get args :pairs) 'list))
         (resolved
          (cl-loop for pair in pairs
                   for slug = (plist-get pair :slug)
                   for new-seq = (plist-get pair :new_sequence)
                   for file = (denote-mcp--find-file slug)
                   for cur-seq = (when file
                                   (plist-get (denote-mcp--parse-filename file) :seq))
                   collect (list :file file :cur_seq cur-seq :new_seq new-seq :slug slug)))
         (sorted (sort resolved
                       (lambda (a b)
                         (string> (or (plist-get a :cur_seq) "")
                                  (or (plist-get b :cur_seq) "")))))
         (results nil))
    (dolist (entry sorted)
      (let ((f (plist-get entry :file))
            (nseq (plist-get entry :new_seq))
            (slug (plist-get entry :slug)))
        (if f
            (let ((new-file (denote-mcp--set-sequence f nseq)))
              (push (list :slug slug :new_path new-file :sequence nseq) results))
          (push (list :slug slug :error "Slug not found") results))))
    (list :modified (vconcat (nreverse results)))))

;; 14. denote_seq_reparent
(mcpkit-register-tool 'denote_seq_reparent 'denote
  :description "Reparent a note (or subtree) under a new parent note."
  :input-schema '(:type "object"
                  :properties (:source_slug (:type "string" :description "Source note slug")
                               :new_parent_slug (:type "string" :description "New parent note slug")
                               :recursive (:type "boolean" :description "Whether to reparent child notes recursively"))
                  :required ["source_slug" "new_parent_slug"])
  (let* ((src (plist-get args :source_slug))
         (parent (plist-get args :new_parent_slug))
         (recursive (plist-get args :recursive))
         (src-file (denote-mcp--find-file src))
         (parent-file (denote-mcp--find-file parent)))
    (unless src-file (error "Source note not found: %s" src))
    (unless parent-file (error "Parent note not found: %s" parent))
    (denote-mcp-mutating
     (let ((denote-sequence-scheme (or denote-mcp-sequence-scheme 'alphanumeric)))
       (denote-sequence-reparent src-file parent-file recursive)
       (let* ((parent-seq (plist-get (denote-mcp--parse-filename parent-file) :seq))
              (src-id (plist-get (denote-mcp--parse-filename src-file) :id))
              (new-file (or (denote-mcp--find-file src-id) src-file)))
         (list :status (if recursive "reparented_recursive" "reparented")
               :source new-file
               :new_parent parent-file
               :parent_sequence parent-seq))))))

;; 15. denote_seq_tree
(mcpkit-register-tool 'denote_seq_tree 'denote
  :description "List all notes organized in Folgezettel sequence hierarchy. Set titles=false to skip reading titles from the files (faster)."
  :input-schema '(:type "object"
                  :properties (:root_sequence (:type "string" :description "Optional root sequence prefix")
                               :titles (:type "boolean" :description "Read titles from the files (default true)"))
                  :required [])
  (let* ((root (plist-get args :root_sequence))
         (titles (not (eq :json-false (plist-get args :titles))))
         (notes (seq-filter
                 (lambda (n)
                   (and (member (denote-mcp--note-ext n) '("org" "md"))
                        (when-let* ((seq (denote-mcp--note-seq n)))
                          (or (null root) (string-prefix-p root seq)))))
                 (denote-mcp--notes)))
         (sorted (sort (copy-sequence notes)
                       (lambda (a b) (string< (denote-mcp--note-seq a) (denote-mcp--note-seq b)))))
         (rows (mapcar
                (lambda (n)
                  (list :sequence (denote-mcp--note-seq n)
                        :id (denote-mcp--note-id n)
                        :title (if titles
                                   (denote-mcp--title (denote-mcp--note-path n) (denote-mcp--note-slug n))
                                 (denote-mcp--note-slug n))
                        :path (denote-mcp--note-path n)))
                sorted)))
    (list :count (length rows) :notes (vconcat rows))))

;; 16. denote_verify
(mcpkit-register-tool 'denote_verify 'denote
  :description "Validate sequence integrity, check for missing parent stubs, and report all sequences. Also repairs frontmatter that disagrees with file names unless fix_frontmatter is false."
  :input-schema '(:type "object"
                  :properties (:quiet (:type "boolean" :description "If true, skip full sequence list")
                               :fix_frontmatter (:type "boolean" :description "Sync frontmatter to file names (default true)"))
                  :required [])
  (let* ((changes (unless (eq :json-false (plist-get args :fix_frontmatter))
                    (denote-mcp--sync-frontmatter)))
         (seqs (sort (denote-mcp--sequences) #'string<))
         (known (let ((h (make-hash-table :test #'equal)))
                  (dolist (s seqs h) (puthash s t h))))
         (missing nil))
    ;; Check for implied parent gaps
    (denote-mcp--with-scheme
      (dolist (s seqs)
        (when (> (length s) 1)
          (let ((parent-seq (denote-mcp--parent-sequence s)))
            (when (and parent-seq (not (gethash parent-seq known)))
              (push (format "Missing parent sequence: %s (implied by %s)" parent-seq s) missing))))))
    (list :valid (if missing :json-false t)
          :sequenced_count (length seqs)
          :missing_parents (vconcat (nreverse (delete-dups missing)))
          :frontmatter_changes (vconcat changes)
          :sequences (if (plist-get args :quiet) [] (vconcat seqs)))))

;; 17. denote_link_string
(mcpkit-register-tool 'denote_link_string 'denote
  :description "Construct a canonical [[denote:ID][Title]] link without sequence signatures."
  :input-schema '(:type "object"
                  :properties (:target_slug (:type "string" :description "Target note slug or identifier")
                               :link_text (:type "string" :description "Optional override link display text"))
                  :required ["target_slug"])
  (let* ((target (plist-get args :target_slug))
         (file (denote-mcp--find-file target)))
    (unless file (error "Target note not found: %s" target))
    (let* ((parsed (denote-mcp--parse-filename file))
           (id (plist-get parsed :id))
           (title (or (plist-get args :link_text)
                      (denote-retrieve-title-value file (denote-filetype-heuristics file))
                      (plist-get parsed :title))))
      (list :link (format "[[denote:%s][%s]]" id title)
            :id id
            :title title))))

;; 18. denote_insert_dblock
(mcpkit-register-tool 'denote_insert_dblock 'denote
  :description "Insert and update an Org dynamic block (denote-backlinks, denote-links, denote-sequence) in a note."
  :input-schema '(:type "object"
                  :properties (:file_or_slug (:type "string" :description "Target note slug or file")
                               :block_type (:type "string" :description "Block type: denote-backlinks, denote-links, denote-sequence")
                               :params (:type "object" :description "Parameters plist or key-value map"))
                  :required ["file_or_slug" "block_type"])
  (let* ((target (plist-get args :file_or_slug))
         (btype (plist-get args :block_type))
         (params (plist-get args :params))
         (file (denote-mcp--find-file target)))
    (unless file (error "Target note not found: %s" target))
    (denote-mcp-silently
     (with-current-buffer (find-file-noselect file)
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       (insert "\n#+BEGIN: " btype)
       (when (listp params)
         (cl-loop for (k v) on params by #'cddr do
                  (insert " " (if (keywordp k) (substring (symbol-name k) 1) (symbol-name k)) " "
                          (cond ((stringp v) (concat "\"" v "\""))
                                ((null v) "nil")
                                (t (format "%s" v))))))
       (insert "\n#+END:\n")
       (re-search-backward "^#\\+BEGIN:" nil t)
       (org-dblock-update)
       (save-buffer)
       (list :path file :block_type btype :updated t)))))

;; 19. denote_redate
(mcpkit-register-tool 'denote_redate 'denote
  :description "Backdate a note's identifier and rewrite links to it in one call. scope=notes (default) rewrites inside the denote directory; scope=repo also rewrites every git-tracked file in the enclosing repository."
  :input-schema '(:type "object"
                  :properties (:file_or_slug (:type "string" :description "Target note slug or file")
                               :new_identifier (:type "string" :description "New identifier YYYYMMDDTHHMMSS")
                               :scope (:type "string" :enum ["notes" "repo"] :description "Where to rewrite links (default notes)"))
                  :required ["file_or_slug" "new_identifier"])
  (let* ((target (plist-get args :file_or_slug))
         (new-id (plist-get args :new_identifier))
         (file (denote-mcp--find-file target)))
    (unless file (error "Target note not found: %s" target))
    (let* ((parsed (denote-mcp--parse-filename file))
           (old-id (plist-get parsed :id))
           (new-path (denote-mcp--set-identifier file new-id))
           (links-fixed 0))
      (when (and new-path old-id (not (string= old-id new-id)))
        (setq links-fixed
              (denote-mcp--replace-links
               old-id new-id
               (if (equal (plist-get args :scope) "repo")
                   (delete-dups
                    (append (denote-mcp--all-note-files)
                            (denote-mcp--git-grep-files old-id (denote-mcp-get-directory))))
                 nil))))
      (list :new_path new-path
            :old_identifier old-id
            :new_identifier new-id
            :links_updated links-fixed))))

;; 20. denote_suggest_parent
(defconst denote-mcp--category-body-threshold 600
  "Body size in characters under which a note with children counts as a category.")

(defconst denote-mcp--stop-words
  '("a" "about" "above" "after" "again" "against" "all" "an" "and" "any" "are"
    "as" "at" "be" "because" "been" "before" "being" "below" "between" "both"
    "but" "by" "can" "did" "do" "does" "doing" "don" "down" "during" "each"
    "few" "for" "from" "further" "had" "has" "have" "having" "here" "how"
    "if" "in" "into" "is" "it" "its" "just" "more" "most" "no" "nor" "not"
    "now" "of" "off" "on" "once" "only" "or" "other" "our" "out" "over" "own"
    "s" "same" "should" "so" "some" "such" "t" "than" "that" "the" "their"
    "theirs" "them" "then" "there" "these" "they" "this" "those" "through"
    "to" "too" "under" "until" "up" "very" "was" "we" "were" "what" "when"
    "where" "which" "while" "who" "whom" "why" "will" "with")
  "Common stop words filtered out during title-word matching.")

(defun denote-mcp--title-words (string)
  "Return a list of distinctive words extracted from STRING (title or slug).
Lowercases and splits on non-alphanumeric characters, filtering out
common stop words and single-character words."
  (when (and string (stringp string))
    (let* ((raw-words (split-string (downcase string) "[^[:alnum:]]+" t))
           (words (seq-filter (lambda (w)
                                (and (> (length w) 1)
                                     (not (member w denote-mcp--stop-words))))
                              raw-words)))
      (or words (seq-filter (lambda (w) (> (length w) 1)) raw-words)))))

(defun denote-mcp--body-size (file)
  "Return the size of FILE past its leading #+ front matter block."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (looking-at "^#\\+") (forward-line 1))
    (- (point-max) (point))))

(defun denote-mcp-suggest-parent (tags &optional top-n title)
  "Rank sequence nodes as candidate parents for a new note tagged TAGS.
Score is overlap with the node's own tags plus half the overlap with the tags
of all its descendants; ties go to the node with more descendants.  Return up
to TOP-N (default 5) plists.

When TAGS overlap is zero across all candidates (e.g. no tag matches or tags
are empty) and TITLE is non-nil, falls back to scoring by word overlap between
TITLE and each node's title (and half-weighted descendant titles).

The index is built from file names only.  Descendants are found in one pass
over the sorted sequences (a node's subtree is the contiguous run after it),
and files are read only for the winners."
  (let* ((tagset (mapcar #'downcase tags))
         (nodes (seq-filter (lambda (n) (and (denote-mcp--note-seq n)
                                             (equal (denote-mcp--note-ext n) "org")))
                            (denote-mcp--notes)))
         (vec (vconcat (sort (copy-sequence nodes)
                             (lambda (a b) (string< (denote-mcp--note-seq a)
                                                    (denote-mcp--note-seq b))))))
         (n (length vec))
         (scored nil)
         (fallback-used nil))
    (when tagset
      (dotimes (i n)
        (let* ((node (aref vec i))
               (seq (denote-mcp--note-seq node))
               (j (1+ i))
               (child-tags nil)
               (count 0)
               (sample nil))
          (while (and (< j n) (string-prefix-p seq (denote-mcp--note-seq (aref vec j))))
            (let ((child (aref vec j)))
              (unless (equal (denote-mcp--note-seq child) seq)
                (cl-incf count)
                (when (< (length sample) 3) (push child sample))
                (dolist (k (denote-mcp--note-keywords child)) (cl-pushnew k child-tags :test #'equal))))
            (cl-incf j))
          (let ((overlap (+ (length (cl-intersection tagset (denote-mcp--note-keywords node) :test #'equal))
                            (* 0.5 (length (cl-intersection tagset child-tags :test #'equal))))))
            (when (> overlap 0)
              (push (list :node node :overlap overlap :count count :sample (nreverse sample)) scored))))))
    ;; Title-word fallback: activate only when tag overlap is zero across all candidates
    (when (and (null scored) title (stringp title))
      (let ((query-words (denote-mcp--title-words title)))
        (when query-words
          (setq fallback-used t)
          (dotimes (i n)
            (let* ((node (aref vec i))
                   (seq (denote-mcp--note-seq node))
                   (j (1+ i))
                   (child-words nil)
                   (count 0)
                   (sample nil)
                   (node-words (denote-mcp--title-words (denote-mcp--note-slug node))))
              (while (and (< j n) (string-prefix-p seq (denote-mcp--note-seq (aref vec j))))
                (let ((child (aref vec j)))
                  (unless (equal (denote-mcp--note-seq child) seq)
                    (cl-incf count)
                    (when (< (length sample) 3) (push child sample))
                    (dolist (w (denote-mcp--title-words (denote-mcp--note-slug child)))
                      (cl-pushnew w child-words :test #'equal))))
                (cl-incf j))
              (let ((overlap (+ (length (cl-intersection query-words node-words :test #'equal))
                                (* 0.5 (length (cl-intersection query-words child-words :test #'equal))))))
                (when (> overlap 0)
                  (push (list :node node :overlap overlap :count count :sample (nreverse sample)) scored))))))))
    (let ((top (seq-take
                (sort scored
                      (lambda (a b)
                        (or (> (plist-get a :overlap) (plist-get b :overlap))
                            (and (= (plist-get a :overlap) (plist-get b :overlap))
                                 (> (plist-get a :count) (plist-get b :count))))))
                (or top-n 5)))
          (all (denote-mcp--sequences)))
      (denote-mcp--with-scheme
        (mapcar
         (lambda (r)
           (let* ((node (plist-get r :node))
                  (file (denote-mcp--note-path node)))
             (list :sequence (denote-mcp--note-seq node)
                   :title (denote-mcp--title file (denote-mcp--note-slug node))
                   :path file
                   :overlap (plist-get r :overlap)
                   :fallback (if fallback-used t :json-false)
                   :category (if (and (> (plist-get r :count) 0)
                                      (< (denote-mcp--body-size file) denote-mcp--category-body-threshold))
                                 t :json-false)
                   :child_count (plist-get r :count)
                   :sample_children (vconcat (mapcar (lambda (c) (denote-mcp--title (denote-mcp--note-path c) (denote-mcp--note-slug c)))
                                                     (plist-get r :sample)))
                   :next_sequence (denote-sequence-get-new 'child (denote-mcp--note-seq node) all))))
         top)))))

(mcpkit-register-tool 'denote_suggest_parent 'denote
  :description "Rank existing sequence nodes as candidate parents for a new note by tag overlap (own tags plus half-weighted descendant tags). When tag overlap across all candidates is zero and title is provided, falls back to title word overlap. Each result has next_sequence ready for denote_seq_set or denote_create_note."
  :input-schema '(:type "object"
                  :properties (:tags (:type "array" :items (:type "string") :description "Tags of the new note")
                               :top_n (:type "integer" :description "Max candidates (default 5)")
                               :title (:type "string" :description "Optional title of the new note for fallback matching when tag overlap is zero"))
                  :required ["tags"])
  (denote-mcp-suggest-parent (denote-mcp--list (plist-get args :tags))
                             (plist-get args :top_n)
                             (plist-get args :title)))

;; 21. denote_find_authorship
(defun denote-mcp-find-authorship (phrase &optional pathspecs show)
  "Return the earliest commit, across all refs, that introduced PHRASE.
PATHSPECS limit the search; SHOW adds the matching diff lines.  The search
runs in the git repository containing the denote directory."
  (let* ((dir (denote-mcp-get-directory))
         (default-directory (or (locate-dominating-file dir ".git") dir))
         (specs (or pathspecs '("."))))
    (unless (executable-find "git") (error "Git is not available"))
    (let ((commit
           (with-temp-buffer
             (apply #'process-file "git" nil '(t nil) nil
                    "log" "--all" "--reverse" "-S" phrase "--format=%H" "--" specs)
             (goto-char (point-min))
             (buffer-substring (point) (line-end-position)))))
      (if (string-empty-p commit)
          (list :found :json-false :phrase phrase)
        (let ((info (with-temp-buffer
                      (process-file "git" nil t nil "log" "-1" "--date=iso-strict"
                                    "--format=%ad%x00%s" commit)
                      (split-string (string-trim-right (buffer-string)) "\0"))))
          (append
           (list :found t :commit commit :date (car info) :subject (cadr info))
           (when show
             (list :excerpt
                   (with-temp-buffer
                     (apply #'process-file "git" nil t nil "show" commit "--" specs)
                     (let (lines)
                       (goto-char (point-min))
                       (while (not (eobp))
                         (let ((line (buffer-substring (point) (line-end-position))))
                           (when (string-search phrase line) (push line lines)))
                         (forward-line 1))
                       (mapconcat #'identity (seq-take (nreverse lines) 20) "\n")))))))))))

(mcpkit-register-tool 'denote_find_authorship 'denote
  :description "Find the earliest commit (all refs, all files) that introduced a phrase, to recover a note's real authorship date after migrations stamped it with the migration date. Verify with show=true: reused prose can give false matches."
  :input-schema '(:type "object"
                  :properties (:phrase (:type "string" :description "Unique phrase from the text (fixed string)")
                               :pathspecs (:type "array" :items (:type "string") :description "Optional git pathspecs")
                               :show (:type "boolean" :description "Include the matching diff lines"))
                  :required ["phrase"])
  (denote-mcp-find-authorship (plist-get args :phrase)
                              (denote-mcp--list (plist-get args :pathspecs))
                              (eq t (plist-get args :show))))

;;;###autoload
(defun denote-mcp-register ()
  "Ensure `denote-mcp-service' is registered in `mcpkit-registry' and return it.
Kept for backward compatibility."
  (interactive)
  (unless (mcpkit-get-service 'denote)
    (mcpkit-register-service denote-mcp-service))
  denote-mcp-service)

(provide 'denote-mcp)

;; Plan, report and tracker tools register on the same service.
(require 'denote-mcp-notes)

;;; denote-mcp.el ends here
