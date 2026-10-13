;;; denote-sync-silo.el --- Local silo/directory backend for denote-sync -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (denote "3.0.0") (denote-sequence "0.3.0") (annotated-completing-read "0.1.0"))
;; Keywords: convenience, files, tools
;; URL: https://github.com/tychoish/denote-sync

;;; Commentary:
;; A `denote-sync' backend whose "remote" is another Denote directory (a
;; silo).  Notes are exported with optional file-type translation
;; (org <-> markdown), `denote-sequence' signature remapping, and
;; whole-tree export with intra-tree `denote:' link rewriting.
;;
;; Source notes track their target with `sync-silo-id', `sync-silo-dir',
;; `sync-silo-sync-hash' and `sync-silo-edited' front matter.
;; Target-side operations run with `denote-directory' dynamically bound to
;; the target silo (see `denote-sync-silo-with-target').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'denote)
(require 'denote-sequence)
(require 'denote-sync)
(require 'annotated-completing-read)
(require 'mdwn)

(declare-function denote-sync--org-to-markdown "denote-sync")
(declare-function denote-sync--export-markdown "denote-sync")
(declare-function denote-sync--dangling-links "denote-sync")
(defvar denote-silo-directories)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom variables

(defgroup denote-sync-silo nil
  "Local Denote silo backend for denote-sync."
  :group 'denote-sync
  :prefix "denote-sync-silo-")

(defcustom denote-sync-silo-default-directory nil
  "Target silo directory used when a note carries no `sync-silo-dir'."
  :type '(choice (const nil) directory)
  :group 'denote-sync-silo)

(defcustom denote-sync-silo-default-format 'same
  "Default target file type: `same' or a `denote-file-types' symbol."
  :type '(choice (const same) (const org) (const markdown-yaml)
                 (const markdown-toml) (const text))
  :group 'denote-sync-silo)

(defcustom denote-sync-silo-allow-overlap nil
  "When non-nil, allow a target directory that overlaps the source directory.
Off by default: sharing one tree between two id spaces can create duplicate
identifiers.  Bind or set it to override the check."
  :type 'boolean
  :group 'denote-sync-silo)

(defcustom denote-sync-silo-create-dir-locals nil
  "When non-nil, write a `.dir-locals.el' into newly created target silos."
  :type 'boolean
  :group 'denote-sync-silo)

(defcustom denote-sync-silo-format-translators
  '(((org . markdown-yaml) . denote-sync-silo--org-to-markdown)
    ((org . markdown-toml) . denote-sync-silo--org-to-markdown)
    ((org . org) . denote-sync-silo--org-passthrough)
    ((markdown-yaml . org) . denote-sync-silo--markdown-to-org)
    ((markdown-toml . org) . denote-sync-silo--markdown-to-org))
  "Alist mapping (FROM-TYPE . TO-TYPE) to translation functions.
Each function is called with (CONTENT FILE), where CONTENT is the
engine's exported Markdown body and FILE the source note, and returns the
target body.  Pairs without an entry pass CONTENT through unchanged."
  :type '(alist :key-type (cons symbol symbol) :value-type function)
  :group 'denote-sync-silo)

(defvar denote-sync-silo-rules nil
  "List of tree sync rules, each a plist.  See `denote-sync-silo-add-rule'.")

(defvar denote-sync-silo--plan nil
  "Hash table of source id -> plist (:id :signature :format :rewrite-links).
Bound by `denote-sync-silo-export-tree' so create/update know the planned
target identifier and signature of each note in the tree.")

(defvar denote-sync-silo--directory nil
  "Dynamically bound target directory, overriding per-note lookup.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context scoping

(defmacro denote-sync-silo-with-target (target-dir &rest body)
  "Execute BODY with `denote-directory' bound to TARGET-DIR."
  (declare (indent 1))
  (let ((d (make-symbol "dir")))
    `(let* ((,d (file-name-as-directory (expand-file-name ,target-dir)))
            (denote-directory ,d)
            (default-directory ,d))
       ,@body)))

(defun denote-sync-silo--strip-quotes (value)
  "Return VALUE without surrounding quotes, or nil when empty."
  (when (and value (not (string-empty-p value)) (not (equal value "\"\"")))
    (string-trim value "\"" "\"")))

(defun denote-sync-silo--directory-for-id (id)
  "Return the target directory for remote ID."
  (or denote-sync-silo--directory
      (when-let* ((file (denote-sync--find-tracked-file id 'silo)))
        (denote-sync-silo--strip-quotes (denote-sync-frontmatter-get file (denote-sync--prop-key "silo" "dir"))))
      denote-sync-silo-default-directory
      (user-error "No silo directory known for note %s" id)))

(defun denote-sync-silo--ensure-directory (dir)
  "Create DIR (and optionally its `.dir-locals.el') if it does not exist."
  (unless (file-directory-p dir)
    (make-directory dir t)
    (when denote-sync-silo-create-dir-locals
      (with-temp-file (expand-file-name ".dir-locals.el" dir)
        (insert (format "((nil . ((denote-directory . %S))))\n"
                        (file-name-as-directory (expand-file-name dir))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disjoint directories

(defun denote-sync-silo--check-disjoint (dir)
  "Signal a `user-error' unless DIR is separate from the source Denote directories.
Syncing a tree into itself or a subdirectory is a renumber or reparent,
which `denote-tree' handles; here it would mix two id spaces.  Skipped
when `denote-sync-silo-allow-overlap' is non-nil."
  (let ((target (file-name-as-directory (expand-file-name dir))))
    (dolist (src (unless denote-sync-silo-allow-overlap (denote-directories)))
      (when (or (file-in-directory-p target src) (file-in-directory-p src target))
        (user-error "Silo directory %s overlaps source directory %s" target src)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time, front matter splitting, file writing

(defun denote-sync-silo--mtime (path)
  "Return PATH's modification time as a sortable UTC string."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%6NZ"
                      (file-attribute-modification-time (file-attributes path))
                      t))

(defun denote-sync-silo--split-front-matter (text type &optional source)
  "Split TEXT of file TYPE into (FRONT-MATTER . BODY).
Signal a `user-error' naming SOURCE when the front matter is malformed."
  (let ((bad (lambda () (user-error "Malformed %s front matter in %s" type (or source "note")))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (pcase type
        ((or 'markdown-yaml 'markdown-toml)
         (let ((delim (if (eq type 'markdown-yaml) "^---[ \t]*$" "^\\+\\+\\+[ \t]*$")))
           (unless (and (looking-at delim)
                        (progn (forward-line 1) (re-search-forward delim nil t)))
             (funcall bad))
           (forward-line 1)))
        ('org (unless (looking-at "^#\\+") (funcall bad))
              (while (looking-at "^#\\+") (forward-line 1)))
        ('text (unless (re-search-forward "^-\\{20,\\}$" nil t) (funcall bad))
               (forward-line 1))
        (_ (funcall bad)))
      (skip-chars-forward "\n")
      (cons (buffer-substring-no-properties (point-min) (point))
            (string-trim (buffer-substring-no-properties (point) (point-max)))))))

(defun denote-sync-silo--read-note (path)
  "Return (TYPE FRONT-MATTER . BODY) for the target note at PATH."
  (let* ((type (denote-filetype-heuristics path))
         (text (if-let* ((buf (find-buffer-visiting path)))
                   (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max)))
                 (with-temp-buffer (insert-file-contents path) (buffer-string))))
         (split (denote-sync-silo--split-front-matter text type path)))
    (cons type split)))

(defun denote-sync-silo--write-file (path text)
  "Write TEXT to PATH, through the visiting buffer if there is one.
Refuses to clobber a visiting buffer with unsaved edits."
  (if-let* ((buf (find-buffer-visiting path)))
      (with-current-buffer buf
        (when (buffer-modified-p)
          (user-error "Target note %s has unsaved edits; save or revert it first" path))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text))
        (save-buffer))
    (with-temp-file path (insert text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Format translation

(defconst denote-sync-silo--denote-id-regexp "denote:\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)"
  "Regexp matching a `denote:' identifier reference; group 1 is the id.")

(defun denote-sync-silo--render (file backend)
  "Return (CONTENT . DANGLING) for pushing FILE to the silo.
An Org source defers rendering: the target is usually Org and takes the raw
body, so the Markdown export runs only when a Markdown target needs it.
Other sources are already Markdown and render as the engine does."
  (if (eq (denote-filetype-heuristics file) 'org)
      (cons nil (denote-sync--dangling-links file backend))
    (denote-sync--export-markdown file backend)))

(defun denote-sync-silo--org-to-markdown (content file)
  "Return CONTENT, or FILE's Markdown export when rendering was deferred."
  (or content
      (car (denote-sync--export-markdown file (denote-sync-get-backend 'silo)))))

(defun denote-sync-silo--replace-denote-ids (text fn)
  "Replace each `denote:ID' in TEXT with `denote:' plus (FN ID) when non-nil."
  (replace-regexp-in-string
   denote-sync-silo--denote-id-regexp
   (lambda (m)
     ;; FN does its own matching (path lookups); keep it from clobbering the
     ;; match data `replace-regexp-in-string' replaces with afterwards.
     (let ((new (save-match-data
                  (funcall fn (progn (string-match denote-sync-silo--denote-id-regexp m)
                                     (match-string 1 m))))))
       (if new (concat "denote:" new) m)))
   text t t))

(defun denote-sync-silo--org-passthrough (_content file)
  "Return FILE's raw Org body with tracked `denote:' links mapped to target ids."
  (denote-sync-silo--replace-denote-ids
   (denote-sync--body-without-front-matter file)
   (lambda (id)
     (when-let* ((path (denote-get-path-by-id id)))
       (denote-sync--get-id path (denote-sync-get-backend 'silo))))))

(defun denote-sync-silo--markdown-to-org (content &optional _file)
  "Translate Markdown CONTENT to Org with `mdwn-to-org'."
  (mdwn-to-org content))

(defun denote-sync-silo--target-body (file content target-type &optional raw-p)
  "Translate CONTENT (exported from FILE) for TARGET-TYPE.
With RAW-P, ignore CONTENT's link rewriting and use FILE's unrewritten
body (Markdown-converted when FILE is Org)."
  (let* ((src-type (denote-filetype-heuristics file))
         (content (if raw-p
                      (let ((raw (denote-sync--body-without-front-matter file)))
                        (if (eq src-type 'org) (denote-sync--org-to-markdown raw) raw))
                    content))
         (fn (alist-get (cons src-type target-type) denote-sync-silo-format-translators
                        nil nil #'equal)))
    (cond
     ((and raw-p (eq src-type 'org) (eq target-type 'org))
      (denote-sync--body-without-front-matter file))
     (fn (funcall fn content file))
     (t content))))

(defun denote-sync-silo--canonical-body (text type)
  "Return target body TEXT of file TYPE in the engine's Markdown form."
  (if (eq type 'org)
      (condition-case nil (denote-sync--org-to-markdown text) (error text))
    text))

(defun denote-sync-silo--format-family (type)
  "Return TYPE's family for pull compatibility."
  (if (memq type '(markdown-yaml markdown-toml)) 'markdown type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequence remapping

(defun denote-sync-silo--last-type (seq)
  "Return :digit or :letter for the last character of SEQ."
  (if (string-match-p "[0-9]\\'" seq) :digit :letter))

(defun denote-sync-silo--component-number (component)
  "Return COMPONENT (digits or letters) as an integer."
  (if (string-match-p "\\`[0-9]+\\'" component)
      (string-to-number component)
    (string-to-number (denote-sequence--alpha-to-number component))))

(defun denote-sync-silo--alternation-rewrite (components old-last new-last)
  "Rewrite COMPONENTS so they alternate correctly after a root ending in NEW-LAST.
OLD-LAST and NEW-LAST are :digit or :letter; return COMPONENTS unchanged
when they agree."
  (if (eq old-last new-last)
      components
    (let ((type (if (eq new-last :digit) :letter :digit)))
      (mapcar (lambda (c)
                (let ((n (denote-sync-silo--component-number c)))
                  (prog1 (if (eq type :digit)
                             (number-to-string n)
                           (denote-sequence--number-to-alpha (number-to-string n)))
                    (setq type (if (eq type :digit) :letter :digit)))))
              components))))

(defun denote-sync-silo--in-scheme (seq scheme)
  "Return SEQ converted to SCHEME."
  (if (eq (cdr (denote-sequence-and-scheme-p seq)) scheme)
      seq
    (denote-sequence-make-conversion seq scheme)))

(defun denote-sync-silo--map-sequence (seq &rest opts)
  "Map source signature SEQ to its target signature, or nil to strip it.
OPTS is a plist: `:strip' drops the signature; `:source-prefix' and
`:target-prefix' rebase SEQ's subtree (fixing alphanumeric alternation);
`:scheme' converts the result to that `denote-sequence-schemes' scheme."
  (let ((sp (plist-get opts :source-prefix))
        (tp (plist-get opts :target-prefix))
        (target-scheme (plist-get opts :scheme)))
    (unless (or (plist-get opts :strip) (null seq) (string-empty-p seq))
      (let* ((scheme (let ((own (cdr (denote-sequence-and-scheme-p seq))))
                       ;; "1" alone is ambiguous; letters anywhere imply alphanumeric
                       (if (and (eq own 'numeric)
                                (seq-some (lambda (s) (and s (string-match-p "[a-z]" s)))
                                          (list sp tp)))
                           'alphanumeric
                         own)))
             (comps (denote-sequence-split seq)))
        (when (and sp tp)
          (let* ((sp (denote-sync-silo--in-scheme sp scheme))
                 (tp (denote-sync-silo--in-scheme tp scheme))
                 (pcomps (denote-sequence-split sp))
                 (rem (nthcdr (length pcomps) comps)))
            (unless (equal pcomps (seq-take comps (length pcomps)))
              (user-error "Signature %s is not under prefix %s" seq sp))
            (when (eq scheme 'alphanumeric)
              (setq rem (denote-sync-silo--alternation-rewrite
                         rem (denote-sync-silo--last-type sp) (denote-sync-silo--last-type tp))))
            (setq comps (append (denote-sequence-split tp) rem))))
        (let ((joined (denote-sequence-join comps scheme)))
          (if (and target-scheme (not (eq target-scheme scheme)))
              (denote-sequence-make-conversion joined target-scheme)
            joined))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tag mapping

(defun denote-sync-silo--map-keywords (keywords opts)
  "Return source KEYWORDS transformed by the tag options in plist OPTS.
`:tags' replaces them (a list, or a function of the keyword list);
`:remove-tags' is then subtracted and `:add-tags' appended.  Result is
sorted and duplicate-free."
  (let* ((tags (plist-get opts :tags))
         (kw (cond ((functionp tags) (funcall tags keywords))
                   ((plist-member opts :tags) tags)
                   (t keywords))))
    (setq kw (seq-difference kw (plist-get opts :remove-tags)))
    (sort (delete-dups (append kw (plist-get opts :add-tags))) #'string<)))

(defun denote-sync-silo--tag-opts (plist)
  "Return the tag options (`:tags', `:add-tags', `:remove-tags') in PLIST."
  (cl-loop for key in '(:tags :add-tags :remove-tags)
           when (plist-member plist key)
           append (list key (plist-get plist key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Target note operations

(defun denote-sync-silo--fresh-id (dir &optional avoid)
  "Return an identifier unused in DIR and not in the AVOID list."
  (let ((time (current-time)) id)
    (while (progn (setq id (format-time-string "%Y%m%dT%H%M%S" time))
                  (or (member id avoid)
                      (denote-sync-silo-with-target dir (denote-get-path-by-id id))))
      (setq time (time-add time 1)))
    id))

(defun denote-sync-silo--choose-id (file dir)
  "Return the target id for FILE in DIR: its own id unless that collides."
  (let ((id (denote-retrieve-filename-identifier file)))
    (if (denote-sync-silo-with-target dir (denote-get-path-by-id id))
        (denote-sync-silo--fresh-id dir)
      id)))

(defun denote-sync-silo--source-date (file)
  "Return FILE's creation time from its identifier."
  (or (ignore-errors (date-to-time (denote-retrieve-filename-identifier file)))
      (current-time)))

(defun denote-sync-silo--write-note (dir id signature file type body keywords &optional existing)
  "Write FILE's note as ID with SIGNATURE, KEYWORDS and BODY of TYPE into DIR.
When EXISTING is non-nil the note is updated (and renamed if needed).
Return the target path."
  (let* ((title (denote-sync--export-title file))
         (path (denote-format-file-name dir id keywords title
                                        (denote--file-extension type) (or signature "")))
         (text (concat (denote--format-front-matter
                        title (denote-sync-silo--source-date file) keywords id
                        (or signature "") type)
                       body "\n")))
    (denote-sync-silo--ensure-directory dir)
    (when (and (file-exists-p path) (not (and existing (file-equal-p existing path))))
      (user-error "Target path %s already exists" path))
    (when (and existing (not (file-equal-p existing path)))
      (if-let* ((buf (find-buffer-visiting existing)))
          (with-current-buffer buf
            (rename-file existing path)
            (set-visited-file-name path nil t))
        (rename-file existing path)))
    (denote-sync-silo--write-file path text)
    path))

(defun denote-sync-silo--resolve-type (file requested)
  "Return the concrete target file type for FILE given REQUESTED."
  (if (or (null requested) (eq requested 'same))
      (denote-filetype-heuristics file)
    requested))

(defun denote-sync-silo--note-title (path)
  "Return the title of the target note at PATH."
  (denote-retrieve-title-or-filename path (denote-filetype-heuristics path)))

(defun denote-sync-silo--result (id path)
  "Build the backend result plist for target note ID at PATH."
  (list :id id :url path :edited-time (denote-sync-silo--mtime path)))

(defun denote-sync-silo--create (file parent content)
  "Create a target note for FILE under PARENT (a plist) with CONTENT."
  (let* ((dir (or (plist-get parent :directory)
                  denote-sync-silo-default-directory
                  (user-error "No target directory in parent")))
         (_ (denote-sync-silo--check-disjoint dir))
         (src-id (denote-retrieve-filename-identifier file))
         (planned (and denote-sync-silo--plan (gethash src-id denote-sync-silo--plan)))
         (id (or (plist-get planned :id) (denote-sync-silo--choose-id file dir)))
         (type (denote-sync-silo--resolve-type
                file (or (plist-get planned :format) (plist-get parent :target-format)
                         denote-sync-silo-default-format)))
         (signature (if planned
                        (plist-get planned :signature)
                      (apply #'denote-sync-silo--map-sequence
                             (denote-retrieve-filename-signature file)
                             (list :strip (plist-get parent :strip-signature)
                                   :source-prefix (plist-get parent :source-prefix)
                                   :target-prefix (plist-get parent :target-prefix)
                                   :scheme (plist-get parent :scheme)))))
         (keywords (if planned
                       (plist-get planned :keywords)
                     (denote-sync-silo--map-keywords
                      (denote-extract-keywords-from-path file)
                      (denote-sync-silo--tag-opts parent))))
         (body (denote-sync-silo--target-body
                file content type (eq (plist-get parent :rewrite-links) 'none)))
         (path (denote-sync-silo--write-note dir id signature file type body keywords)))
    (denote-sync-frontmatter-set file (denote-sync--prop-key "silo" "dir") (expand-file-name dir))
    (denote-sync-silo--result id path)))

(defun denote-sync-silo--update (file id content _force)
  "Update (or, when planned, create) target note ID for FILE with CONTENT."
  (let* ((dir (or (denote-sync-silo--strip-quotes (denote-sync-frontmatter-get file (denote-sync--prop-key "silo" "dir")))
                  (denote-sync-silo--directory-for-id id)))
         (planned (and denote-sync-silo--plan
                       (gethash (denote-retrieve-filename-identifier file) denote-sync-silo--plan)))
         (existing (denote-sync-silo-with-target dir (denote-get-path-by-id id))))
    (unless (or existing planned)
      (user-error "Target note %s not found in %s; clear sync-silo-id to re-create it" id dir))
    (let* ((type (cond
                  (planned (denote-sync-silo--resolve-type file (plist-get planned :format)))
                  (existing (denote-filetype-heuristics existing))))
           (signature (if planned
                          (plist-get planned :signature)
                        (denote-retrieve-filename-signature existing)))
           ;; without a plan the target's own tags are kept
           (keywords (if planned
                         (plist-get planned :keywords)
                       (denote-extract-keywords-from-path existing)))
           (body (denote-sync-silo--target-body
                  file content type (eq (plist-get planned :rewrite-links) 'none)))
           (path (denote-sync-silo--write-note dir id signature file type body keywords existing)))
      (denote-sync-silo--result id path))))

(defun denote-sync-silo--fetch-remote (id &optional fetch-body-p)
  "Fetch target note ID's metadata and, with FETCH-BODY-P, canonical body."
  (let* ((dir (denote-sync-silo--directory-for-id id))
         (path (denote-sync-silo-with-target dir (denote-get-path-by-id id))))
    (if (not path)
        (list :id id :missing t)
      (pcase-let ((`(,type ,_front . ,body) (denote-sync-silo--read-note path)))
        (list :id id
              :name (denote-sync-silo--note-title path)
              :edited-time (denote-sync-silo--mtime path)
              :body (when fetch-body-p
                      (denote-sync-silo-with-target dir
                        (denote-sync-silo--canonical-body body type))))))))

(defun denote-sync-silo--link-url (id)
  "Return the `denote:' link URL for target ID."
  (concat "denote:" id))

(defun denote-sync-silo--extract-id (id-or-url)
  "Return the bare identifier in ID-OR-URL."
  (let ((s (string-trim id-or-url)))
    (if (string-match denote-sync-silo--denote-id-regexp s) (match-string 1 s) s)))

(defun denote-sync-silo--reverse-links (body)
  "Map target `denote:' ids in BODY back to the source notes' ids."
  (denote-sync-silo--replace-denote-ids
   body
   (lambda (tid)
     (when-let* ((src (denote-sync--find-tracked-file tid 'silo)))
       (denote-retrieve-filename-identifier src)))))

(defun denote-sync-silo--refresh (file id)
  "Refresh FILE from target note ID; requires matching file types."
  (let* ((dir (denote-sync-silo--directory-for-id id))
         (path (or (denote-sync-silo-with-target dir (denote-get-path-by-id id))
                   (user-error "Target note %s not found in %s" id dir)))
         (src-type (denote-filetype-heuristics file)))
    (pcase-let ((`(,type ,_front . ,body) (denote-sync-silo--read-note path)))
      (unless (eq (denote-sync-silo--format-family src-type)
                  (denote-sync-silo--format-family type))
        (user-error "Pull needs matching formats (source %s, target %s)" src-type type))
      (denote-sync--write-body file (denote-sync-silo--reverse-links body))
      (denote-sync--record-synced-content
       file (denote-sync-get-backend 'silo) id
       (car (denote-sync--export-body file (denote-sync-get-backend 'silo)))
       (denote-sync-silo--mtime path)))))

(defun denote-sync-silo--import (id)
  "Import target note ID as a new source note in the target's format family.
The target's signature is kept unless the source directory already uses it."
  (let* ((dir (or denote-sync-silo--directory
                  denote-sync-silo-default-directory
                  (read-directory-name "Silo directory: ")))
         (_ (denote-sync-silo--check-disjoint dir))
         (path (or (denote-sync-silo-with-target dir (denote-get-path-by-id id))
                   (user-error "Note %s not found in %s" id dir)))
         (backend (denote-sync-get-backend 'silo)))
    (pcase-let ((`(,type ,_front . ,body) (denote-sync-silo--read-note path)))
      (let* ((signature (denote-retrieve-filename-signature path))
             (signature (unless (and signature (denote-sequence-get-path signature)) signature))
             (created (denote (denote-sync-silo--note-title path)
                              (denote-extract-keywords-from-path path)
                              (if (eq type 'org) 'org 'markdown-yaml)
                              nil nil nil signature))
             (new-file (if (stringp created) created (buffer-file-name))))
        (denote-sync--write-body new-file (denote-sync-silo--reverse-links body))
        (denote-sync-frontmatter-set new-file (denote-sync--prop-key "silo" "id") id)
        (denote-sync-frontmatter-set new-file (denote-sync--prop-key "silo" "dir") (expand-file-name dir))
        (denote-sync--record-synced-content
         new-file backend id (car (denote-sync--export-body new-file backend))
         (denote-sync-silo--mtime path))
        new-file))))

;; The target is local, so the async protocol is the synchronous one.

(defun denote-sync-silo--async-fetch-remote (id callback &optional fetch-body-p)
  "Call CALLBACK with (ERROR RESULT) for `denote-sync-silo--fetch-remote'."
  (pcase (condition-case err
             (cons nil (denote-sync-silo--fetch-remote id fetch-body-p))
           (error (cons (error-message-string err) nil)))
    (`(,e . ,r) (funcall callback e r))))

(defun denote-sync-silo--async-push (file id content callback)
  "Call CALLBACK with (ERROR RESULT) after updating target ID from FILE."
  (pcase (condition-case err
             (cons nil (denote-sync-silo--update file id content nil))
           (error (cons (error-message-string err) nil)))
    (`(,e . ,r) (funcall callback e r))))

(defun denote-sync-silo--async-refresh (file id callback)
  "Call CALLBACK with ERROR (or nil) after refreshing FILE from ID."
  (funcall callback
           (condition-case err
               (progn (denote-sync-silo--refresh file id) nil)
             (error (error-message-string err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parents, rules, and interactive commands

(defun denote-sync-silo--format-parent (parent)
  "Format PARENT plist for display."
  (format "%s (%s)%s" (plist-get parent :directory)
          (or (plist-get parent :target-format) denote-sync-silo-default-format)
          (if (denote-sync-silo--tag-opts parent) " [tags]" "")))

(defun denote-sync-silo--read-tag-list (prompt)
  "Read a comma-separated tag list with PROMPT."
  (split-string (read-string prompt) "[, ]+" t))

(defun denote-sync-silo--candidate-directories ()
  "Return known silo directories from `denote-silo' and the parent registry."
  (delete-dups
   (delq nil
         (append (when (boundp 'denote-silo-directories) (copy-sequence denote-silo-directories))
                 (mapcar (lambda (e) (plist-get (caddr e) :directory))
                         (seq-filter (lambda (e) (eq (cadr e) 'silo))
                                     denote-sync-parent-registry))))))

(defun denote-sync-silo--read-parent ()
  "Prompt for a silo directory and target format; return a parent plist."
  (let* ((dirs (denote-sync-silo--candidate-directories))
         (dir (annotated-completing-read
               (mapcar (lambda (d) (cons d (unless (file-directory-p d) "[new]"))) dirs)
               :prompt "Silo directory: "))
         (fmt (completing-read "Target format: "
                               '("same" "org" "markdown-yaml" "markdown-toml" "text")
                               nil t nil nil "same")))
    (append (list :directory (expand-file-name dir) :target-format (intern fmt))
            (when-let* ((add (denote-sync-silo--read-tag-list "Add tags (comma separated, optional): ")))
              (list :add-tags add))
            (when-let* ((rm (denote-sync-silo--read-tag-list "Remove tags (comma separated, optional): ")))
              (list :remove-tags rm)))))

;;;###autoload
(cl-defun denote-sync-silo-add-rule (&key name source-prefix target-directory
                                          target-prefix target-parent target-format
                                          scheme strip-signature (rewrite-links t)
                                          (tags nil tags-p) add-tags remove-tags)
  "Register a tree sync rule NAME and a matching parent registry entry.
SOURCE-PREFIX is the root signature to export; TARGET-DIRECTORY the silo.
Rebase to TARGET-PREFIX, or allocate a new child of TARGET-PARENT in the
silo on first export.  TARGET-FORMAT is `same' or a Denote file type,
SCHEME converts the sequence scheme, STRIP-SIGNATURE drops signatures, and
REWRITE-LINKS (default t) rewrites links to exported notes.  Target tags
derive from each source note's: TAGS replaces them (a list, or a function
from the source tag list to the new list), REMOVE-TAGS are dropped, then
ADD-TAGS appended."
  (unless (and name source-prefix target-directory)
    (user-error "A rule needs :name, :source-prefix and :target-directory"))
  (let ((rule (list :name name :source-prefix source-prefix
                    :directory (expand-file-name target-directory)
                    :target-prefix target-prefix :target-parent target-parent
                    :target-format (or target-format 'same) :scheme scheme
                    :strip-signature strip-signature
                    :rewrite-links (if rewrite-links t 'none)
                    :add-tags add-tags :remove-tags remove-tags)))
    (when tags-p (setq rule (plist-put rule :tags tags)))
    (setq denote-sync-silo-rules
          (cons rule (seq-remove (lambda (r) (equal (plist-get r :name) name))
                                 denote-sync-silo-rules)))
    (setq denote-sync-parent-registry
          (cons (cons name (list 'silo rule))
                (assoc-delete-all name (copy-sequence denote-sync-parent-registry))))
    rule))

(defun denote-sync-silo--find-rule (name)
  "Return the rule named NAME or signal an error."
  (or (seq-find (lambda (r) (equal (plist-get r :name) name)) denote-sync-silo-rules)
      (user-error "No silo rule named %s" name)))

(defun denote-sync-silo--target-prefix (rule files)
  "Return RULE's target prefix, given source tree FILES (root first)."
  (let* ((dir (plist-get rule :directory))
         (root (car files))
         (src-root-id (and root (denote-sync--get-id root (denote-sync-get-backend 'silo))))
         (existing (and src-root-id
                        (denote-sync-silo-with-target dir (denote-get-path-by-id src-root-id)))))
    (when (and (plist-get rule :target-parent) (not existing)
               (seq-some (lambda (f)
                           (when-let* ((tid (denote-sync--get-id f (denote-sync-get-backend 'silo))))
                             (denote-sync-silo-with-target dir (denote-get-path-by-id tid))))
                         (cdr files)))
      (user-error "Root note of tree %s is missing from %s but descendants exist; restore it or clear sync-silo-id"
                  (plist-get rule :source-prefix) dir))
    (cond
     ((plist-get rule :target-prefix))
     (existing (denote-retrieve-filename-signature existing))
     ((plist-get rule :target-parent)
      (let ((parent (plist-get rule :target-parent)))
        (denote-sync-silo-with-target dir
          (if (denote-sequence-get-path parent)
              (denote-sequence--get-new-child parent)
            (denote-sequence-join
             (append (denote-sequence-split parent)
                     (list (denote-sequence--get-start parent)))
             (cdr (denote-sequence-and-scheme-p parent)))))))
     (t (plist-get rule :source-prefix)))))

(defun denote-sync-silo--plan-tree (rule)
  "Return a list of (FILE . PLIST) for RULE's tree, parents before children.
Each PLIST has :id, :signature, :keywords, :format and :rewrite-links."
  (let* ((prefix (plist-get rule :source-prefix))
         (dir (plist-get rule :directory))
         (files (sort (copy-sequence (denote-sequence-get-all-files-with-prefix prefix))
                      (lambda (a b)
                        (let ((sa (denote-retrieve-filename-signature a))
                              (sb (denote-retrieve-filename-signature b)))
                          (or (< (denote-sequence-depth sa) (denote-sequence-depth sb))
                              (and (= (denote-sequence-depth sa) (denote-sequence-depth sb))
                                   (string< sa sb)))))))
         (backend (denote-sync-get-backend 'silo))
         (tprefix (unless (plist-get rule :strip-signature)
                    (denote-sync-silo--target-prefix rule files)))
         (used nil))
    (unless files (user-error "No notes under signature prefix %s" prefix))
    (mapcar
     (lambda (file)
       (let* ((tracked (denote-sync--get-id file backend))
              (id (or tracked
                      (let ((own (denote-retrieve-filename-identifier file)))
                        (if (or (member own used)
                                (denote-sync-silo-with-target dir (denote-get-path-by-id own)))
                            (denote-sync-silo--fresh-id dir used)
                          own)))))
         (push id used)
         (cons file
               (list :id id
                     :signature (denote-sync-silo--map-sequence
                                 (denote-retrieve-filename-signature file)
                                 :strip (plist-get rule :strip-signature)
                                 :source-prefix prefix :target-prefix tprefix
                                 :scheme (plist-get rule :scheme))
                     :keywords (denote-sync-silo--map-keywords
                                (denote-extract-keywords-from-path file)
                                (denote-sync-silo--tag-opts rule))
                     :format (plist-get rule :target-format)
                     :rewrite-links (plist-get rule :rewrite-links)))))
     files)))

;; File types a target note may use.
(defconst denote-sync-silo--target-types '(same org markdown-yaml markdown-toml text))

(defun denote-sync-silo--needs-move-p (file entry dir)
  "Return non-nil if FILE's existing target note differs from ENTRY's plan.
That is, its signature, tags or file type no longer matches, so it must be
rewritten even though the body is unchanged."
  (when-let* ((existing (denote-sync-silo-with-target dir
                          (denote-get-path-by-id (plist-get entry :id)))))
    (or (not (equal (or (denote-retrieve-filename-signature existing) "")
                    (or (plist-get entry :signature) "")))
        (not (equal (sort (copy-sequence (denote-extract-keywords-from-path existing)) #'string<)
                    (plist-get entry :keywords)))
        (not (eq (denote-filetype-heuristics existing)
                 (denote-sync-silo--resolve-type file (plist-get entry :format)))))))

(defun denote-sync-silo--preflight (rule plan)
  "Validate PLAN (from `denote-sync-silo--plan-tree') for RULE before any write.
Collect every problem and signal a single `user-error' listing them."
  (let ((dir (plist-get rule :directory)) problems)
    (cl-flet ((bad (fmt &rest args) (push (apply #'format fmt args) problems)))
      (condition-case err (denote-sync-silo--check-disjoint dir)
        (user-error (bad "%s" (cadr err))))
      (unless (memq (plist-get rule :target-format) denote-sync-silo--target-types)
        (bad "Unknown target format %s" (plist-get rule :target-format)))
      (let (signatures needs-mdwn)
        (pcase-dolist (`(,file . ,entry) plan)
          (let ((src-type (denote-filetype-heuristics file)))
            (condition-case err
                (denote-sync-silo--split-front-matter
                 (with-temp-buffer (insert-file-contents file) (buffer-string)) src-type file)
              (user-error (bad "%s" (cadr err))))
            (when-let* ((sig (plist-get entry :signature)))
              (if (member sig signatures)
                  (bad "Duplicate target signature %s (%s)" sig (file-name-nondirectory file))
                (push sig signatures)))
            (when (and (not (eq src-type 'org))
                       (eq (denote-sync-silo--resolve-type file (plist-get rule :target-format)) 'org))
              (setq needs-mdwn t))
            (when-let* ((existing (denote-sync-silo-with-target dir
                                    (denote-get-path-by-id (plist-get entry :id)))))
              (condition-case err
                  (denote-sync-silo--read-note existing)
                (user-error (bad "%s" (cadr err))))
              (when-let* ((buf (find-buffer-visiting existing))
                          ((buffer-modified-p buf)))
                (bad "Target note %s has unsaved edits" existing)))))
        (when needs-mdwn
          (condition-case err (mdwn--ensure-grammars)
            (user-error (bad "%s" (cadr err)))))))
    (when problems
      (user-error "Cannot export tree %s:\n  %s" (plist-get rule :name)
                  (string-join (nreverse problems) "\n  ")))))

;;;###autoload
(defun denote-sync-silo-export-tree (rule-name)
  "Export and synchronize the tree described by rule RULE-NAME.
Plans every target id and signature first so `denote:' links between
notes in the tree are rewritten in a single write pass.  A preflight
check runs before anything is written, and source notes seeded for a
push that fails are returned to the untracked state."
  (interactive
   (list (annotated-completing-read
          (mapcar (lambda (r) (cons (plist-get r :name)
                                    (format "[%s -> %s]" (plist-get r :source-prefix)
                                            (plist-get r :directory))))
                  denote-sync-silo-rules)
          :prompt "Tree rule: ")))
  (let* ((rule (denote-sync-silo--find-rule rule-name))
         (dir (plist-get rule :directory))
         (plan (denote-sync-silo--plan-tree rule))
         (table (make-hash-table :test #'equal))
         (backend (denote-sync-get-backend 'silo))
         seeded done)
    (denote-sync-silo--preflight rule plan)
    (denote-sync-silo--ensure-directory dir)
    (condition-case err
        (progn
          (pcase-dolist (`(,file . ,entry) plan)
            (puthash (denote-retrieve-filename-identifier file) entry table)
            (unless (denote-sync--get-id file backend)
              (push file seeded)
              (denote-sync-frontmatter-set file (denote-sync--prop-key "silo" "id") (plist-get entry :id)))
            (denote-sync-frontmatter-set file (denote-sync--prop-key "silo" "dir") dir))
          (let ((denote-sync-silo--plan table))
            (dolist (item plan)
              ;; an unchanged body skips the engine's update, so moves are forced,
              ;; but never over target-side edits
              (let ((force (and (denote-sync--get-sync-hash (car item) backend)
                                (denote-sync-silo--needs-move-p (car item) (cdr item) dir)
                                (memq (denote-sync--sync-state (car item) backend)
                                      '(unchanged local-only)))))
                (denote-sync-push (car item) nil (and force t) 'silo))
              (push (car item) done))))
      (error
       (dolist (file (seq-difference seeded done))
         (denote-sync-frontmatter-set file (denote-sync--prop-key "silo" "id") "")
         (denote-sync-frontmatter-set file (denote-sync--prop-key "silo" "dir") ""))
       (signal (car err) (cdr err))))
    (message "Exported %d note(s) to %s" (length plan) dir)
    (mapcar (lambda (item) (cons (car item) (plist-get (cdr item) :id))) plan)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backend registration

(denote-sync-register-backend
 (denote-sync-backend-create
  :name 'silo
  :frontmatter-prefix "silo"
  :create-fn #'denote-sync-silo--create
  :update-fn #'denote-sync-silo--update
  :fetch-remote-fn #'denote-sync-silo--fetch-remote
  :link-url-fn #'denote-sync-silo--link-url
  :extract-id-fn #'denote-sync-silo--extract-id
  :async-fetch-remote-fn #'denote-sync-silo--async-fetch-remote
  :async-push-fn #'denote-sync-silo--async-push
  :async-refresh-fn #'denote-sync-silo--async-refresh
  :refresh-fn #'denote-sync-silo--refresh
  :import-fn #'denote-sync-silo--import
  :read-parent-fn #'denote-sync-silo--read-parent
  :format-parent-fn #'denote-sync-silo--format-parent
  :parse-parent-fn nil
  :render-fn #'denote-sync-silo--render))

(provide 'denote-sync-silo)
;;; denote-sync-silo.el ends here
