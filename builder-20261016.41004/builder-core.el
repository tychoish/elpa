;;; builder-core.el --- Core candidate registry and compilation driver -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (f "0.20.0") (xtdlib "0.1.0") (annotated-completing-read "0.1.0"))
;; Keywords: tools, compilation

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Core candidate registry data structures, caching, minibuffer selection,
;; and compilation driver for builder.

;;; Code:

(require 'compile)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'cl-lib)
(require 'f)
(require 'xtd-f)
(require 'xtd-macro)
(require 'xtd-dash)
(require 'xtd-project)
(require 'xtd-s)
(require 'annotated-completing-read)

(declare-function alert "alert" (message &rest args))
(defvar sprite-instance-id (or (bound-and-true-p sprite-instance-id) (system-name)))

(defgroup builder nil
  "Compilation buffer and command runner."
  :group 'tools
  :prefix "builder-")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal buffer helpers (self-contained, no external config coupling)

(defun builder--buffer-line-count (&optional buf)
  "Return number of lines in BUF (defaults to current buffer)."
  (with-current-buffer (or buf (current-buffer))
    (count-lines (point-min) (point-max))))

(defun builder--buffer-directory (&optional buf)
  "Return default directory or buffer file directory of BUF."
  (when-let* ((b (or buf (current-buffer))))
    (with-current-buffer b
      (if-let* ((file (buffer-file-name b)))
          (if (file-directory-p file) file (file-name-directory file))
        default-directory))))


;;;###autoload
(defun builder ()
  "Run compile operation selecting compile buffer and commands."
  (interactive)
  (builder-compile-project))

(cl-defun builder--compile-post-hook
    (buffer compile-result-message started-at &optional &key process-name program args send-when
     (alert-threshold 60)
     (notification-threshold 120))
  (let* ((end-at (current-time))
	 (duration (time-subtract end-at started-at))
	 (compile-result-message (string-trim compile-result-message))
	 (msg (format "build %s in %.06fs -- %s"
		      compile-result-message
		      (float-time duration)
		      (with-current-buffer buffer
		        (string-trim (car compilation-arguments))))))

    (when (or send-when
	      (> alert-threshold (float-time (time-since (current-idle-time))))
	      (and (> (float-time duration) notification-threshold) process-name program args))

      (apply #'async-start-process
	     (pa "emacs-process-name" :is process-name)
	     (pa "program" :is program)
	     (pa "on-finish" :is (lambda (out) (message "INFO: notify process for %s completed [%s] with %s" (buffer-name buffer) out compile-result-message)))
	     (append args (-strings (format "<%s> %s -- %s" sprite-instance-id (buffer-name buffer) msg)))))

    (when (or send-when
	      (> (/ alert-threshold 2) (float-time (time-since (current-idle-time))))
	      (> alert-threshold (ffloor (float-time duration))))
      (alert
       msg
       :title (format "%s:%s:%s" sprite-instance-id (buffer-name buffer) compile-result-message)
       :buffer buffer))

    (with-current-buffer buffer
      (save-excursion
	(setq buffer-read-only nil)
	(goto-char (point-min))
	(unless (eq (point-min) (re-search-forward "\\(^Compilation.*\n$\\|\n{2,}\\)"))
	  (replace-match ""))
	(goto-char (point-max))
	(compilation-insert-annotation
	 (format "\n--- %s completed in %.06fs at %s\n\n"
		 compile-result-message (float-time duration)
		 (format-time-string "%Y-%m-%d %H:%M:%S" end-at)))
	 (setq buffer-read-only t)))))

(defun builder-reset-finish-hooks ()
  (interactive)
  (seq-do (lambda (it) (with-current-buffer it
		 (setq-local compilation-finish-functions nil)))
	     (mode-buffers 'compilation-mode)))

(defun builder--buffer-for-command (command)
  "Return the name of a project compilation buffer that last ran COMMAND, or nil."
  (when-let* ((buf (seq-find (lambda (it) (with-current-buffer it
				     (string-equal (string-trim (or (car compilation-arguments) ""))
						    (string-trim command))))
			 (mode-buffers-for-project :mode 'compilation-mode))))
    (buffer-name buf)))

(defun builder--buffer-ran-command-p (buffer command)
  "Return non-nil if BUFFER last ran COMMAND."
  (with-current-buffer buffer
    (string-equal (string-trim (or (car compilation-arguments) ""))
		  (string-trim command))))

;;;###autoload
(defun builder-compile-project (&optional name command)
  (let* ((project-root-directory (approximate-project-root))
	 (start-at (current-time))
	 ;; Step 1: select and confirm command
	 (cc-result (builder--read-command command))
	 (candidate (car cc-result))
	 (compile-command (read-from-minibuffer "edit command => " (builder-candidate-command candidate)))
	 (op-default-directory (or (builder-candidate-directory candidate) default-directory))
	 ;; Step 2: select buffer, pre-seeding the one that last ran this command
	 (prior-buf-name (builder--buffer-for-command compile-command))
	 (compilation-buffer-candidates (builder--project-compilation-buffers :name name))
	 (op-name (annotated-completing-read
		   compilation-buffer-candidates
		   :prompt "compilation buffer => "
		   :require-match nil
		   :initial-input prior-buf-name))
	 (compile-buf (get-buffer op-name)))

    (save-some-buffers
     t
     (lambda () (let ((filename (buffer-file-name)))
		  (and filename (file-in-directory-p filename project-root-directory)))))

    ;; Step 3: recompile if buffer already ran this command; prefix arg forces new compilation
    (let ((active-buf
	   (if (and compile-buf
		    (not current-prefix-arg)
		    (builder--buffer-ran-command-p compile-buf compile-command))
	       (progn (with-current-buffer compile-buf (recompile)) compile-buf)
	     (let ((default-directory op-default-directory))
	       (compilation-start
		compile-command
		'compilation-mode
		(compile-buffer-name op-name))))))

      (with-current-buffer active-buf
	(add-one-shot-hook :name (format "%s notification hook" op-name)
	 :hook 'compilation-finish-functions
	 :local t
	 :make-unique t
	 :args (compilation-buffer msg)
	 :form (builder--compile-post-hook
		compilation-buffer
		msg
		start-at
		:process-name "sardis-notify"
		:program "sardis"
		:args '("notify" "send")
		:send-when (or current-prefix-arg
			       (not (get-buffer-window active-buf t))
			       (< 10 (float-time (time-since (current-idle-time)))))))

	(when-let* ((hook (builder-candidate-hook candidate)))
	  (add-one-shot-hook :name (format "post-%s-hook-operation" op-name)
	   :hook 'compilation-finish-functions
	   :make-unique t
	   :local t
	   :args (compilation-buffer msg)
	   :form (funcall hook compilation-buffer msg)))

	(let ((op-window (get-buffer-window (current-buffer) (selected-frame))))
	  (if op-window (select-window op-window)
	    (switch-to-buffer-other-window (current-buffer))))))))

;;;###autoload
(defun builder-change-directory ()
  "Change the directory for the current compilation buffer."
  (interactive)
  (unless (derived-mode-p 'compilation-mode)
    (user-error "operation is only applicable for COMPILATION-MODE buffers"))
  (let ((directory (or (annotated-completing-read-directory
                        :candidates (seq-uniq
                                     (append (list compilation-directory default-directory)
                                             (annotated-completing-read--directory-default-candidates)
                                             (f-directories (approximate-project-root))))
                        :prompt "in directory =>> ")
                       compilation-directory
                       default-directory)))
    (setq-local default-directory directory
                compilation-directory directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; builder-compile-project implementation

(defun builder--candidate-priority (candidates name)
  "Return the priority of candidate NAME in CANDIDATES, defaulting to 2."
  (or (when-let* ((c (map-elt candidates name)))
	(builder-candidate-priority c))
      2))

(defun builder--make-sort-fn (candidates history-key)
  "Return a display-sort-function for `annotated-completing-read'.
Candidates that appear in HISTORY-KEY's history list (stored in
`annotated-completing-read-history') sort first by recency (lowest index =
most recent).  Candidates not in history are then ordered by structural
priority from CANDIDATES, with key length as a final tiebreaker."
  (lambda (items)
    (let* ((hist (map-elt annotated-completing-read-history history-key))
           (hist-rank (lambda (name)
                        (or (cl-position name hist :test #'equal)
                            most-positive-fixnum))))
      (sort (copy-sequence items)
        (lambda (a b)
          (let ((ha (funcall hist-rank a))
                (hb (funcall hist-rank b)))
            (cond
             ((< ha hb) t)
             ((> ha hb) nil)
             (t (let ((pa (builder--candidate-priority candidates a))
                      (pb (builder--candidate-priority candidates b)))
                  (or (< pa pb)
                      (and (= pa pb) (< (length a) (length b)))))))))))))

(defun builder--read-command (&optional command table)
  (let* ((candidates (or table
			 (builder--get-candidates default-directory command)))
         (selection
          (annotated-completing-read
           (map-into
            (seq-map (lambda (name)
                       (let ((cand (map-elt candidates name)))
                         (cons name (cons (builder-candidate-annotation cand) cand))))
                     (map-keys candidates))
            'hash-table)
           :prompt "compile command => "
           :require-match nil
           :history 'compile-history
           :sort-fn (builder--make-sort-fn candidates 'compile-history)))
	 (candidate (if (builder-candidate-p selection)
			selection
		      (let ((new-candidate (make-builder-candidate
					     :command selection
					     :directory default-directory
					     :annotation "user input")))
			(builder--add-candidate candidates new-candidate)
			new-candidate))))
    (cons candidate candidates)))

(cl-defun builder--project-compilation-buffers (&optional &key name (project (approximate-project-name)))
  "Return a hash table of candidate compilation buffer names for PROJECT."
  (let ((buffer-table (map-into
                       (seq-map (lambda (it)
                                  (with-current-buffer it
                                    (cons (buffer-name it)
                                          (format "reuse buffer: project=%s errors=%s lines=%s command='%s'"
                                                  project
                                                  compilation-num-errors-found
                                                  (builder--buffer-line-count it)
                                                  (string-trim (or (car compilation-arguments) ""))))))
                                (mode-buffers-for-project :mode 'compilation-mode))
                       '(hash-table :test equal)))
        (default-names (list "push" "gen" "buf" "benchmark" "check" "compile" "run" "lint" "test" "build")))
    (when name
      (cl-pushnew name default-names :test #'equal))

    (dolist (it default-names)
      (let ((name (format "*%s-%s*" project it)))
        (if (map-contains-key buffer-table name)
            (setf (map-elt buffer-table (generate-new-buffer-name name))
                  (format "new %s buffer for project %s" name project))
          (setf (map-elt buffer-table name)
                (format "create default compilation buffer for %s" project)))))

    buffer-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compilation candidate struct and table operations

(cl-defstruct (builder-candidate
               (:constructor nil)
               (:constructor make-builder-candidate
			     (&key command
				   (name (if (> (length command) 32) (concat (substring command 0 29) "...") command))
				   (directory default-directory)
				   (annotation (if (not (string-equal command name))
						   (format "'%s' in %s" command (f-abbrev directory))
						 command))
				   hook
				   notification-threshold
				   (priority 2))))
  "Structure for compilation candidates."
  (name
   "build"
   :documentation "name of command, default to using the command"
   :type string)
  (command
   "test"
   :documentation "shell command to run in a compile buffer."
   :type string)
  (directory
   default-directory
   :documentation "directory in which to run the command."
   :type string)
  (annotation
   "compilation command"
   :documentation "description of command, used for annotations."
   :type string)
  (hook
   nil
   :documentation "a function to run when the compilation completes"
   :type function)
  (notification-threshold
   30
   :documentation "if a command runs longer than this number of seconds, send a notification when the compile completes."
   :type integer)
  (priority
   2
   :documentation "display sort priority: 0=current-file, 1=rerun, 2=project, 3=history (lower = higher priority)"
   :type integer))

(defun builder--add-candidate (table candidate)
  (setf (map-elt table (builder-candidate-name candidate)) candidate))

(defun builder--add-candidates (table candidates)
  (seq-do (lambda (it) (builder--add-candidate table it))
	   (seq-filter #'builder-candidate-p candidates)))

(defvar builder-candidate-functions nil
  "List of functions that populate a table of possible compilation commands.
Each function is called with four arguments:
  PROJECT-ROOT-DIRECTORY — absolute path to the project root.
  PROJECT-NAME           — short name of the project (directory basename).
  DIRECTORIES            — list of directories derived from open project
                           buffers; this is what drives language-specific
                           discovery (e.g. finding go.mod or Cargo.toml files
                           near files the user actually has open).
  TABLE                  — a hash table of `builder-candidate' objects;
                           functions should call `builder--add-candidates' to
                           populate it and return t.")

(defun builder-add-candidates (fn)
  "Register FN as a candidate generator on `builder-candidate-functions'.
FN is called with (ROOT NAME DIRS TABLE): ROOT is the project root path, NAME
is the project name, DIRS is the set of open-buffer directories that drives
language-specific discovery, and TABLE is a hash table of `builder-candidate'
objects.  FN should call `builder--add-candidates' to populate TABLE and
return t.

This is the plain-function alternative to `builder-register-candidates'.
Use it when you have a plain `defun' and do not need predicate filtering,
named-symbol generation, or mode-hook scoping."
  (add-hook 'builder-candidate-functions fn))

(cl-defmacro builder-register-candidates (&key name (predicate t) (hooks nil) pipeline)
  "Define a named candidate generator and register it on `builder-candidate-functions'.
NAME is used to derive the generated function symbol
`builder-candidates-for-NAME'.  PREDICATE gates execution (default t).
PIPELINE is an expression that evaluates to a list of `builder-candidate'
objects; it runs inside the generated function with PROJECT-ROOT-DIRECTORY,
PROJECT-NAME, DIRECTORIES, and OPERATION-TABLE bound.

When HOOKS is nil the generator is registered globally via
`builder-add-candidates'.  When HOOKS is a list of hook symbols the generator
is registered buffer-locally when any of those hooks fires, so candidates only
appear for buffers of the relevant major mode.

For simple cases that do not need predicate filtering or mode scoping, prefer
the plain-function API: write a `defun' with the four-argument protocol and
call `builder-add-candidates'."
  (let ((symbol-name (intern (format "builder-candidates-for-%s" name)))
	(hook-registering-function-name (intern (format "builder-candidate-registrar-for-%s" name))))
    `(progn
       (defun ,symbol-name (project-root-directory project-name directories operation-table)
	 ,(format "Candidate generator for `%s'; see `builder-register-candidates'." name)
	 (ignore project-root-directory project-name directories operation-table)
	 (when ,predicate
	   (builder--add-candidates
	    operation-table
	    ,pipeline))
	 t)

       ,(if (eql 0 (length hooks))
	    `(builder-add-candidates #',symbol-name)
	  `(let ((hooks ,hooks))
	     (defun ,hook-registering-function-name ()
	       (add-hook 'builder-candidate-functions #',symbol-name 0 t))

	     (seq-do (lambda (it) (add-hook it ',hook-registering-function-name)) hooks))))))

(defvar-local builder--cached-candidates nil)

(defvar builder--cache-bypass nil
  "Disables the compilation candidate cache (per-buffer).")

(create-toggle-functions builder--cache-bypass)

(defun builder--get-candidates (&optional directory command)
  "Generate candidate compilation commands based on mode and directory structure."
  (when current-prefix-arg
    (builder--clear-candidate-cache))

  (unless (or builder--cache-bypass
	      builder--cached-candidates)
    (let* ((project-root-directory (approximate-project-root))
	   (project-name (file-name-nondirectory project-root-directory))
	   (default-directory (or directory default-directory))
           (directories (seq-uniq
			    (append
			     (annotated-completing-read--directory-parents default-directory project-root-directory)
			     (seq-map #'f-full
				      (seq-filter #'file-directory-p
						 (seq-keep #'builder--buffer-directory (approximate-project-buffers)))))))
	   (operation-table (make-hash-table :test #'equal)))

      (when command
	(builder--add-candidate
	 operation-table
	 (make-builder-candidate
	  :name (if (> (length command) 32) (concat (substring command 0 29) "...") command)
	  :command command
	  :annotation "runtime suggested candidate"
	  :directory (or directory project-root-directory default-directory))))

      (run-hook-with-args
       'builder-candidate-functions
       project-root-directory
       project-name
       directories
       operation-table)

      (setq-local builder--cached-candidates operation-table)))

  builder--cached-candidates)

(cl-defun builder--candidate-cache-p (&optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (when builder--cached-candidates t)))

(defun builder-clear-all-caches ()
  (interactive)
  (seq-do #'builder--clear-candidate-cache
	   (seq-filter #'builder--candidate-cache-p (buffer-list))))

(defun builder--after-load-alist-options ()
  "Return (NAME KEY ANNOTATION) tuples for `after-load-alist' entries.
NAME is a display string for KEY (usually a feature symbol, sometimes
a load-history regexp cons); ANNOTATION shows how many forms are
queued for it."
  (seq-map (lambda (entry)
             (let* ((key (car entry))
                    (count (length (cdr entry))))
               (list (format "%S" key)
                     key
                     (format "%d pending form%s" count (if (= count 1) "" "s")))))
          after-load-alist))

;;;###autoload
(defun builder-remove-eval-after-load ()
  "Remove all `with-eval-after-load' forms queued for a feature or file.

Picks the target via `annotated-completing-read' from the entries
currently registered in `after-load-alist', annotated with how many
forms are pending for each, then drops every form queued for it via

  (setq after-load-alist (assoc-delete-all KEY after-load-alist))

Useful for clearing stale after-load forms -- for example ones
referencing a function that was renamed or removed -- without
restarting Emacs."
  (interactive)
  (let* ((options (builder--after-load-alist-options))
         (table (seq-map (lambda (e) (cons (car e) (nth 2 e))) options))
         (choice (annotated-completing-read
                  table
                  :prompt "remove eval-after-load for => "
                  :require-match t))
         (key (nth 1 (assoc choice options))))
    (setq after-load-alist (assoc-delete-all key after-load-alist))
    (message "Removed `with-eval-after-load' entries for %S" key)))

(cl-defun builder-clear-cache (&optional (buffer (current-buffer)))
  (interactive)
  (if current-prefix-arg
      (builder-clear-all-caches)
    (builder--clear-candidate-cache buffer)))

(cl-defun builder--clear-candidate-cache (&optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (setq-local builder--cached-candidates nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(provide 'builder-core)
;;; builder-core.el ends here
