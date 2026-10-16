;;; builder-tasks.el --- Candidate generators and language task discovery -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (f "0.20.0") (xtdlib "0.1.0") (eglot-test-at-point "0.1.0"))
;; Keywords: tools, compilation

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Declarative candidate generators for build, test, and lint operations across
;; Go, Python, Rust, Org-Babel, Makefiles, Justfiles, Markdown, and Emacs Lisp.

;;; Code:

(require 'compile)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'cl-lib)
(require 'f)
(require 'xtd-f)
(require 'xtd-macro)
(require 'xtd-project)
(require 'xtd-s)
(require 'eglot-test-at-point)
(require 'builder-core)
(require 'builder-elisp)
(require 'builder-isolate-elisp)

(f-directories-containing-file-with-extension-function "go")
(f-directories-containing-file-with-extension-function "py")
(f-directories-containing-file-with-extension-function "rs")
(f-directories-containing-file-with-extension-function "el")

(f-directories-containing-file-with-extension-function "md")
(f-directories-containing-file-with-extension-function "txt")
(f-directories-containing-file-with-extension-function "rst")
(f-directories-containing-file-with-extension-function "org")
(f-directories-containing-file-with-extension-function "mdwn")

(f-directories-containing-file-function "Cargo.toml")
(f-directories-containing-file-function "go.mod")
(f-directories-containing-file-function "justfile")
(f-directories-containing-file-function "makefile" "Makefile" "GNUmakefile")
(f-directories-containing-file-function "pyproject.toml")


;; candidate discovery/generator registration

(builder-register-candidates
 :name "current-buffer-text-file"
 :predicate (when (and (buffer-file-name)
		       (derived-mode-p '(org-mode markdown-mode rst-mode)))
	      (save-buffer) t)
 :pipeline (let ((filename (buffer-file-name)))
	     (list (make-builder-candidate
		  :directory default-directory
		  :command (format "vale --output=line %s" filename)
		  :name (format "vale check %s" (f-collapse-homedir filename))
		  :annotation "find errors in the current file"
		  :priority 0
		  :hook (lambda (buffer msg) (builder--vale-insert-statistics buffer msg :filename filename)))
		 (make-builder-candidate
		  :directory default-directory
		  :command (format "vale --ls-metrics %s" filename)
		  :name (format "vale report %s" (f-collapse-homedir filename))
		  :annotation "report statistics about the current file"
		  :priority 0))))

(builder-register-candidates
 :name "minibuffer-shell-commands"
 :pipeline (seq-map
		 (lambda (it)
		   (let ((command (car it))
			 (source (cdr it))
			 (annotation-tag (if (string-equal default-directory project-root-directory) "project root" "working directory")))
		     (make-builder-candidate
		      :name (if (> (length command) 32) (concat (substring command 0 29) "...") command)
		      :command command
		      :directory project-root-directory
		      :annotation (format "operation from `%s' in the %s (%s)" source annotation-tag default-directory)
		      :priority 3)))
		 (append (seq-map (lambda (it) (cons it "minibuffer-history")) (minibuffer-default-add-shell-commands))
			(seq-map (lambda (it) (cons it "shell-command-history")) shell-command-history))))

(builder-register-candidates
 :name "project-compilation-buffer-commands"
 :pipeline (seq-mapcat
		 (lambda (it)
		   (let ((command (car it))
			 (buf (cdr it)))
		     (seq-map (lambda (it)
				(let ((operation-directory it))
				  (make-builder-candidate
				   :name (format "rerun from %s [%s]" buf (f-visually-compress-to-five operation-directory))
				   :command command
				   :directory operation-directory
				   :annotation (format "command '%s' from %s in %s" command buf operation-directory)
				   :priority 1)))
			      (seq-filter #'identity
					 (seq-uniq
					  (list project-root-directory (builder--buffer-directory buf) default-directory))))))
		 (seq-keep
		  (lambda (it) (with-current-buffer it
				 (when-let* ((command (s-trimmed-or-nil (car compilation-arguments))))
				   (cons command it))))
		  (mode-buffers-for-project :mode 'compilation-mode))))

(builder-register-candidates
 :name "makefiles"
 :pipeline (seq-mapcat
		 (lambda (it)
		   (let* ((directory (alist-get 'directory it))
			  (target (alist-get 'target it))
			  (is-current-directory (f-equal-p default-directory directory))
			  (is-project-root (f-equal-p project-root-directory directory))
			  (display-directory (s-shortest
					     (string-remove-suffix project-root-directory directory)
					     (if is-project-root
						 (concat "<" project-name ">")
					       (f-collapse-homedir directory))))
			  (command-template (if is-current-directory
					       (format "make %%s%s" target)
					     (format "make %%s-C %s %s" directory target)))
			  (name-template (if is-current-directory
					    command-template
					  (string-replace directory display-directory command-template)))
			  (annotation-template (cond
					       (is-project-root (format "build target %s in project root (%s)" target display-directory))
					       (is-current-directory (format "build target %s in current directory (%s)" target display-directory))
					       (t (format "build target %s in %s" target display-directory)))))
		     (seq-map (lambda (it)
				(let ((flag (car it))
				      (annotation-suffix (cdr it)))
				  (make-builder-candidate
				   :name (format name-template flag)
				   :command (format command-template flag)
				   :directory directory
				   :annotation (s-join-with-space annotation-template annotation-suffix))))
			      '((""	  "")
				("-k "	  ", continuing on error")
				("-B "	  ", unconditionally")
				("-k -B " ", unconditionally while continuing on error")))))
		 (seq-mapcat
		  (lambda (it)
		    (let* ((default-directory it)
			   (makefile-report (process-lines "bash" "-c" "make --dry-run --print-data-base | grep -E '^[a-zA-Z0-9_-]+:' | sed 's/:.*//'")))
		      (unless (string-prefix-p "make: ***" (car makefile-report))
			(seq-filter (lambda (it) (and it (alist-get 'directory it) (alist-get 'target it)))
				    (seq-map (lambda (it)
					      (let ((target it))
						`((target . ,target)
						  (directory . ,default-directory))))
					    makefile-report)))))
		  (f-directories-containing-file-makefile directories))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go -- test, coverage, and tools

(defun builder-go-project-root (&optional directory)
  "Return the Go module or workspace root for DIRECTORY.
DIRECTORY defaults to `default-directory`.
Looks for `go.work` first, then `go.mod`."
  (let ((dir (or directory default-directory)))
    (or (locate-dominating-file dir "go.work")
        (locate-dominating-file dir "go.mod"))))

(defun builder-go-coverage--ensure-git-exclude (root)
  "Ensure coverage/ is listed in .git/info/exclude if .git exists under ROOT."
  (let* ((git-dir (expand-file-name ".git" root)))
    (when (file-directory-p git-dir)
      (let* ((info-dir (expand-file-name "info" git-dir))
             (exclude-file (expand-file-name "exclude" info-dir)))
        (unless (file-directory-p info-dir)
          (make-directory info-dir t))
        (let ((contents (if (file-exists-p exclude-file)
                            (with-temp-buffer
                              (insert-file-contents exclude-file)
                              (buffer-string))
                          "")))
          (unless (string-match-p "\\(^\\|\n\\)coverage/?\\($\\|\n\\)" contents)
            (with-temp-file exclude-file
              (insert contents)
              (unless (or (string-empty-p contents) (string-suffix-p "\n" contents))
                (insert "\n"))
              (insert "coverage/\n"))))))))

(defun builder-go-coverage--refresh-buffers (root)
  "Refresh `cov-mode` overlays in open Go buffers under ROOT."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'go-mode 'go-ts-mode)
                 (buffer-file-name)
                 (file-in-directory-p (buffer-file-name) root))
        (if (bound-and-true-p cov-mode)
            (when (fboundp 'cov-update)
              (cov-update))
          (when (fboundp 'cov-turn-on)
            (cov-turn-on)))))))

;;;###autoload
(defun builder-go-coverage-and-convert (&optional target directory)
  "Run `go test` with coverage and convert the profile to lcov.info via `gcov2lcov`.
TARGET specifies the package pattern to test (defaults to \"./...\").
DIRECTORY is the directory from which to run tests (defaults to
`default-directory`).
With a prefix argument, prompt for TARGET among common scopes.

Coverage files are placed in <project-root>/coverage/:
- cover.out (Go test coverage profile)
- lcov.info (lcov-format report for `cov-mode`)

Signals `user-error` if not in a Go module/project or if `go` or
`gcov2lcov` is not found in PATH."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Coverage target: "
                            '("./..." "." "project root (./...)")
                            nil t nil nil "./..."))
         nil))
  (let* ((root (builder-go-project-root directory)))
    (unless root
      (user-error "Not in a Go module or project: no go.mod or go.work found"))
    (unless (executable-find "go")
      (user-error "Could not find 'go' executable in PATH"))
    (unless (executable-find "gcov2lcov")
      (user-error "gcov2lcov executable not found. Install it with: go install github.com/jandelgado/gcov2lcov@latest"))
    (let* ((root-dir (file-name-as-directory (expand-file-name root)))
           (effective-dir (cond
                           ((equal target "project root (./...)") root-dir)
                           (directory (file-name-as-directory (expand-file-name directory)))
                           (t default-directory)))
           (effective-target (if (or (null target) (equal target "project root (./...)"))
                                 "./..."
                               target))
           (coverage-dir (expand-file-name "coverage" root-dir))
           (cover-out (expand-file-name "cover.out" coverage-dir))
           (lcov-info (expand-file-name "lcov.info" coverage-dir))
           (proj-name (file-name-nondirectory (directory-file-name root-dir)))
           (buf-name (format "*%s-go-coverage*" proj-name))
           (cmd (format "go test -coverprofile=%s %s && gcov2lcov -infile %s -outfile %s"
                        (shell-quote-argument cover-out)
                        (shell-quote-argument effective-target)
                        (shell-quote-argument cover-out)
                        (shell-quote-argument lcov-info))))
      (make-directory coverage-dir t)
      (builder-go-coverage--ensure-git-exclude root-dir)
      (let ((default-directory effective-dir))
        (let ((comp-buf (compilation-start
                         cmd
                         'compilation-mode
                         (compile-buffer-name buf-name))))
          (with-current-buffer comp-buf
            (add-hook 'compilation-finish-functions
                      (lambda (_buffer msg)
                        (when (string-match-p "finished" msg)
                          (message "Go coverage converted to %s" lcov-info)
                          (builder-go-coverage--refresh-buffers root-dir)))
                      nil t))
          comp-buf)))))

(builder-register-candidates
 :name "go-packages"
 :pipeline (seq-mapcat
		 (lambda (it)
		   (let* ((prefix (concat "." (f-path-separator)))
			  (directory it)
			  (operation-directory (cond
				((or (f-equal-p directory project-root-directory) (f-directory-contains-go-mod-file directory)) "./")
				((string-prefix-p prefix directory) directory)
				((string-prefix-p (f-path-separator) directory) directory)
				(t (concat prefix directory))))
			  (operation-directory-tree (f-join operation-directory "..."))
			  (operation-directory-tree (if (string-equal "..." operation-directory-tree) "./..." operation-directory-tree))
			  (short-path (f-collapse-homedir operation-directory))
			  (package-path (string-replace project-root-directory "" operation-directory))
			  (path-for-project-tag (cond ((f-equal-p operation-directory project-root-directory) "")
						      ((equal short-path package-path) "")
						      (t package-path)))
			  (proj-path-for-name (s-join-with-space (format "<%s>/%s" project-name
									 (f-visually-compress-to-five path-for-project-tag)))))
		     (append
		      (list (make-builder-candidate
			   :name (s-join-with-space "go build" (f-join proj-path-for-name "..."))
			   :command (s-join-with-space "go build" operation-directory-tree)
			   :directory directory
			   :annotation (s-join-with-space "build in" project-name "at" short-path "recursively"))
			  (make-builder-candidate
			   :name (s-join-with-space "go build +tests" (f-join proj-path-for-name "..."))
			   :command (s-join-with-space "go test -run=NOOP" operation-directory-tree)
			   :directory directory
			   :annotation (s-join-with-space "build in (including tests) for" project-name "at" short-path "recursively"))
			  (make-builder-candidate
			   :name (s-join-with-space "go test +race +coverage +gaps" proj-path-for-name)
			   :directory directory
			   :command (s-join-with-space
				     "go test -coverprofile=coverage.out -race" operation-directory ";"
				     "go tool cover -html=coverage.out -o=coverage.html;"
				     (s-join-with-pipe
				      "go tool cover -func=coverage.out"
				      "sed -r \"$(go list -f='s%{{.ImportPath}}%{{.Dir}}%')\""
				      "grep -v '100.0%'"
				      "column -t;"))
			   :annotation (s-join-with-space "collect and report coverage data for" short-path))
			  (make-builder-candidate
			   :name (s-join-with-space "go test +cov2lcov" proj-path-for-name)
			   :directory directory
			   :command (format "go test -coverprofile=%s %s && gcov2lcov -infile %s -outfile %s"
					    (shell-quote-argument (expand-file-name "coverage/cover.out" project-root-directory))
					    (shell-quote-argument operation-directory)
					    (shell-quote-argument (expand-file-name "coverage/cover.out" project-root-directory))
					    (shell-quote-argument (expand-file-name "coverage/lcov.info" project-root-directory)))
			   :hook (lambda (_buf msg)
				   (when (string-match-p "finished" msg)
				     (builder-go-coverage--refresh-buffers project-root-directory)))
			   :annotation (s-join-with-space "collect coverage and generate lcov.info for" short-path)))
		      (seq-mapcat
		       (lambda (it)
			 (let ((command-prefix (car it))
			       (annotation-prefix (cadr it)))
			   (seq-map
			    (lambda (it)
			      (let* ((timeout-spec it)
				     (is-default (equal timeout-spec ""))
				     (timeout-arg (unless is-default (concat "--timeout=" timeout-spec)))
				     (timeout-name (if is-default "no timeout" (s-join-with-space "a" timeout-spec "timeout"))))
				(make-builder-candidate
				 :name (s-join-with-space command-prefix timeout-spec (f-join proj-path-for-name "..."))
				 :command (s-join-with-space command-prefix timeout-arg operation-directory-tree)
				 :directory directory
				 :annotation (format "run go test in %s recursively with %s and %s" short-path annotation-prefix timeout-name))))
			    '("10s" "30s" "1m"))))
		       '(("go test -cover"  "the code coverage collector")
			 ("go test -race"   "the race detector")
			 ("go test"         "default options")))
		      (seq-map
		       (lambda (it)
			 (make-builder-candidate
			  :name (format "lint %s %s" it proj-path-for-name)
			  :command (format "golangci-lint run --enable-only=%s %s" it operation-directory)
			  :directory directory
			  :annotation (format "run (only) the %s linter in %s" it short-path)))
		       '("revive" "makezero" "exhaustruct")))))
		 (seq-uniq
		  (seq-filter #'identity
			      (append
			       (f-directories-containing-file-with-extension-go directories)
			       (f-directories-containing-file-go-mod project-root-directory))))))
(builder-register-candidates
 :name "go-files"
 :pipeline (when-let* ((filename (if (and (buffer-file-name) (derived-mode-p '(go-mode go-ts-mode)))
				     (buffer-file-name)
				   (thing-at-point 'filename)))
		       (exists (file-exists-p filename))
		       (is-golang (string-equal (file-name-extension filename) "go"))
		       (short-filename (f-collapse-homedir filename))
		       (basename (file-name-nondirectory filename)))
	     (list (make-builder-candidate
		  :name (format "gofumpt +extra %s" basename)
		  :directory (f-dirname filename)
		  :command (format "gofumpt -extra -w %s" filename)
		  :annotation (format "run gofumpt with non-extra constraints on %s" short-filename)
		  :priority 0)
		 (make-builder-candidate
		  :name (format "gofumpt %s" basename)
		  :directory (f-dirname filename)
		  :command (format "gofumpt -w %s" filename)
		  :annotation (format "run gofumpt with standard gofmt constraints on %s" short-filename)
		  :priority 0)
		 (make-builder-candidate
		  :name (format "gci %s <import sort>" basename)
		  :directory (f-dirname filename)
		  :command (format "golangci-lint fmt --enable gci %s" filename)
		  :annotation (format "sort imports with gci for %s" short-filename)
		  :priority 0)
		 (make-builder-candidate
		  :name (format "meta fmt %s" basename)
		  :directory (f-dirname filename)
		  :command (format "golangci-lint fmt %s" filename)
		  :annotation (format "run meta formatter on %s" short-filename)
		  :priority 0))))

(builder-register-candidates
 :name "go-test-file"
 :pipeline
 (when-let* ((filename  (buffer-file-name))
             (_         (derived-mode-p '(go-mode go-ts-mode)))
             (_         (string-suffix-p "_test.go" filename))
             (directory (f-dirname filename))
             (basename  (file-name-nondirectory filename))
             (short-path (f-collapse-homedir directory))
             (test-names (eglot-test-at-point--go-names-in-buffer)))
   (seq-mapcat
    (lambda (it)
      (let* ((test-name (car it))
             (is-bench  (eq (cdr it) 'benchmark))
             (run-flags (if is-bench
                            (format "-bench=^%s$ -run=^$" test-name)
                          (format "-run=^%s$" test-name)))
             (type-name (if is-bench "benchmark" "test")))
        (seq-mapcat
         (lambda (it)
           (let ((race-flag  (car it))
                 (race-label (cadr it)))
             (seq-map
              (lambda (it)
                (let ((timeout-flag (format "-timeout=%s" it))
                      (timeout-label it))
                  (make-builder-candidate
                   :name       (s-join-with-space "go test -v" race-label timeout-label test-name basename)
                   :command    (s-join-with-space "go test -v" race-flag timeout-flag run-flags ".")
                   :directory  directory
                   :annotation (format "run %s %s in %s" type-name test-name short-path)
                   :priority   0)))
              '("10s" "30s" "1m"))))
         '(("" "")
           ("-race" "+race")))))
    test-names)))

(builder-register-candidates
 :name "go-modules"
 :pipeline (let ((go-mod-directories (f-directories-containing-file-go-mod directories)))
	     (seq-mapcat
	      (lambda (it)
		(let ((filename (file-name-nondirectory it))
		      (directory it))
		  (seq-filter
		   #'identity
		   (seq-map
		    (lambda (it)
		      (let ((name (if (car it) (car it) (cadr it)))
			    (command (cadr it))
			    (annotation (caddr it)))
			(make-builder-candidate
			 :name name
			 :command command
			 :directory directory
			 :annotation (s-join-with-space annotation filename))))
		    (append
		     '(("lint run"         "golangci-lint run"        "run `golangci-lint' in package")
		       ("lint fix"         "golangci-lint run --fix"  "run `golangci-lint' and fix trivial errors in package")
		       (nil                "go mod tidy"              "run `go mod tidy' in package")
		       (nil                "go doc -all"              "go doc for entire package")
		       ("go doc -outline"  "go doc --"                "go doc outline for package")
		       		       ("<pkgs> | go test +cov2lcov"
			    "go test -coverprofile=coverage/cover.out ./... && gcov2lcov -infile coverage/cover.out -outfile coverage/lcov.info"
			    "generate lcov coverage for all submodules of")
("<pkgs> | xargs go test +race +coverage"
			    "go list -f '{{ if (or .TestGoFiles .XTestGoFiles) }}{{ .ImportPath }}{{ end }}' ./... | xargs --verbose go test -race -cover"
			    "run all tests (with the race detector) for all submodules of")
		       ("<pkgs> | xargs go build +test ./..."
			    "go list -f '{{ if (or .TestGoFiles .XTestGoFiles) }}{{ .ImportPath }}{{ end }}' ./... | xargs --verbose go build"
			   "build all tests in all packages and sub-packages for"))
		     (unless (eql 1 (length go-mod-directories))
		       '(("<mod> | xargs go test -race"
			       "find . -name 'go.mod' | xargs --verbose -I{} bash -c 'pushd $(dirname {}); go test -race -cover ./...'"
			      "run tests for all modules and submodules in")
			 ("<mod> | xargs go build ./..."
			       "find . -name 'go.mod' | xargs --verbose -I{} bash -c 'pushd $(dirname {}); go build ./...'"
			      "run builds for all modules and submodules in"))))))))
	      go-mod-directories)))
(builder-register-candidates
 :name "py-projects"
 :pipeline (seq-mapcat
	     (lambda (it)
	       (list
		(make-builder-candidate
		 :command "ruff check"
		 :directory it
		 :annotation (format "run `ruff check' in package %s" it))
		(make-builder-candidate
		 :name "ruff check fix"
		 :command "ruff check --fix --show-fixes"
		 :directory it
		 :annotation (format "run `ruff check' and automatically fix lint violations in package %s" it))
		(make-builder-candidate
		 :name "ruff check fix +unsafe"
		 :command "ruff check --fix --unsafe-fixes --show-fixes"
		 :directory it
		 :annotation (format "render all unsafe fixes (which may modify the intent of the code) in package %s" it))
		(make-builder-candidate
		 :name "ruff format"
		 :command "ruff format --respect-gitignore"
		 :directory it
		 :annotation (format "run `ruff format' in package %s" it))
		(make-builder-candidate
		 :command "ruff analyze"
		 :directory it
		 :annotation (format "run `ruff analyze' in package %s" it))))
	     (f-directories-containing-file-pyproject-toml directories)))

(builder-register-candidates
 :name "python"
 :pipeline (seq-mapcat
	     (lambda (it)
	       (list
		(make-builder-candidate
		 :command (format "black %s" it)
		 :directory it
		 :annotation (format "run `black' in the directory %s" it))
		(make-builder-candidate
		 :command (format "isort %s" it)
		 :directory it
		 :annotation (format "run `isort' in the directory %s" it))
		(make-builder-candidate
		 :command (format "isort --force-single-line-imports %s" it)
		 :directory it
		 :annotation (format "run `isort' in the directory %s (single line imports)" it))))
	     (f-directories-containing-file-with-extension-py directories)))

(builder-register-candidates
 :name "rust-project"
 :pipeline (append
	     (seq-mapcat
	      (lambda (it)
		(let ((annotation-directory (f-collapse-homedir it))
		      (directory it))
		  (seq-map
		   (lambda (it)
		     (if (f-equal-p project-root-directory directory)
			 (make-builder-candidate
			  :directory directory
			  :command (format "cargo %s" it)
			  :annotation (format "run cargo target '%s' in project root (%s)" it annotation-directory))
		       (make-builder-candidate
			:directory directory
			:name (format "cargo %s <%s>" it (file-name-sans-extension (file-name-nondirectory directory)))
			:command (format "cd %s; cargo %s" directory it)
			:annotation (format "run cargo target '%s' in package (%s)" it annotation-directory))))
		   '("build" "check" "test" "fix" "fmt" "clippy" "clippy --fix"))))
	      (f-directories-containing-file-cargo-toml directories))
	     (when (f-directories-containing-file-cargo-toml project-root-directory)
	       (let ((annotation-directory (f-collapse-homedir project-root-directory)))
		 (seq-mapcat
		  (lambda (it)
		    (list
		     (make-builder-candidate
		      :directory project-root-directory
		      :command (concat "cargo build --" it)
		      :annotation (format "build %s in project root (%s)" it annotation-directory))
		     (make-builder-candidate
		      :directory project-root-directory
		      :command (concat "cargo check --" it)
		      :annotation (format "run static analysis %s in project root (%s)" it annotation-directory))))
		  '("libs" "bins" "examples" "tests" "benches" "all-targets"))))))

(builder-register-candidates
 :name "justfile"
 :pipeline (seq-mapcat
	     (lambda (it)
	       (let ((directory it)
		     (annotation-directory (f-collapse-homedir it)))
		 (seq-map
		  (lambda (it)
		    (make-builder-candidate
		     :directory directory
		     :command (format "just %s" it)
		     :annotation (format "exec justfile target %s in %s" it annotation-directory)))
		  (when-let* ((default-directory directory)
			      (output (shell-command-to-string "just --summary"))
			      (candidates (split-string output)))
		    candidates))))
	     (f-directories-containing-file-justfile directories)))
(defun builder--vale-insert-statistics (buffer _message &key filename)
  (interactive)
  (let* ((table (json-parse-string (shell-command-to-string (format "vale ls-metrics --output=line %s" filename)))))
    (with-current-buffer buffer
      (insert (format "\nstatistics for %s" (propertize (f-collapse-homedir filename) 'face 'italic)))
      (map-do (lambda (key value) (insert (format "\n%s:%s" (propertize key 'face 'bold) value)))
	      table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs-lisp -- static analysis and linting targets

(defun builder--el-files-in-directory (directory)
  "Return a list of .el files in DIRECTORY, excluding byte-compiled .elc files."
  (seq-filter (lambda (it) (string-equal (file-name-extension it) "el")) (f-entries directory #'file-regular-p)))

(builder-register-candidates
 :name "emacs-lisp-file"
 :pipeline
 (when-let* ((filename (buffer-file-name))
             (_ (derived-mode-p 'emacs-lisp-mode))
             (short-name (f-collapse-homedir filename))
             (basename (file-name-nondirectory filename))
             (directory (f-dirname filename))
             (lisp-dir (f-join user-emacs-directory "lisp")))
   (list (make-builder-candidate
        :name (format "byte-compile %s" basename)
        :command (format "emacs --batch -L %s -f package-initialize -f batch-byte-compile %s" lisp-dir filename)
        :directory directory
        :annotation (format "byte-compile %s" short-name))
       (make-builder-candidate
        :name (format "relint %s" basename)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'relint)\" -f relint-batch %s" lisp-dir filename)
        :directory directory
        :annotation (format "check regexps in %s" short-name))
       (make-builder-candidate
        :name (format "package-lint %s" basename)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'package-lint)\" -f package-lint-batch-and-exit %s" lisp-dir filename)
        :directory directory
        :annotation (format "lint package metadata in %s" short-name))
       (make-builder-candidate
        :name (format "elisp-lint %s" basename)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'elisp-lint)\" -f elisp-lint-files-batch %s" lisp-dir filename)
        :directory directory
        :annotation (format "run elisp-lint on %s" short-name))
       (make-builder-candidate
        :name (format "elsa %s" basename)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'elsa)\" -f elsa-run %s" lisp-dir filename)
        :directory directory
        :annotation (format "run elsa type analysis on %s" short-name)))))

(builder-register-candidates
 :name "emacs-lisp-project"
 :pipeline
 (when-let* ((_ (derived-mode-p 'emacs-lisp-mode))
             (el-files (builder--el-files-in-directory project-root-directory))
             (_ el-files)
             (file-args (string-join el-files " "))
             (lisp-dir (f-join user-emacs-directory "lisp")))
   (list (make-builder-candidate
        :name (format "byte-compile <%s>" project-name)
        :command (format "emacs --batch -L %s -f package-initialize -f batch-byte-compile %s" lisp-dir file-args)
        :directory project-root-directory
        :annotation (format "byte-compile all .el files in %s" project-name))
       (make-builder-candidate
        :name (format "relint <%s>" project-name)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'relint)\" -f relint-batch %s" lisp-dir file-args)
        :directory project-root-directory
        :annotation (format "check regexps across all .el files in %s" project-name))
       (make-builder-candidate
        :name (format "package-lint <%s>" project-name)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'package-lint)\" -f package-lint-batch-and-exit %s" lisp-dir file-args)
        :directory project-root-directory
        :annotation (format "lint package metadata across all .el files in %s" project-name))
       (make-builder-candidate
        :name (format "elisp-lint <%s>" project-name)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'elisp-lint)\" -f elisp-lint-files-batch %s" lisp-dir file-args)
        :directory project-root-directory
        :annotation (format "run elisp-lint across all .el files in %s" project-name))
       (make-builder-candidate
        :name (format "elsa <%s>" project-name)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'elsa)\" -f elsa-run %s" lisp-dir file-args)
        :directory project-root-directory
        :annotation (format "run elsa type analysis across all .el files in %s" project-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-babel -- execute source blocks in org files

(defun builder--org-babel-select-file ()
  "Prompt for an org file to execute.
Candidates are open org-mode buffers first, then `org-agenda-files'.
Falls back to `read-file-name' when the candidate list is empty."
  (let* ((open-org (thread-last (buffer-list)
                     (seq-filter (lambda (b)
                                   (with-current-buffer b
                                     (and (derived-mode-p 'org-mode)
                                          (buffer-file-name)))))
                     (seq-map #'buffer-file-name)))
         (agenda (ignore-errors
                   (seq-filter #'file-exists-p (org-agenda-files))))
         (candidates (seq-uniq (append open-org agenda)))
         (choice (if candidates
                     (completing-read "Execute org file: " candidates)
                   (read-file-name "Execute org file: " default-directory
                                   nil t nil
                                   (lambda (f)
                                     (or (file-directory-p f)
                                         (string-suffix-p ".org" f)))))))
    (if (file-name-absolute-p choice)
        choice
      (expand-file-name choice default-directory))))

;;;###autoload
(defun builder-org-babel-execute-file (&optional file)
  "Execute all Babel source blocks in FILE without confirmation prompts.
When FILE is nil and the current buffer is `org-mode', execute it directly.
Otherwise prompt via `builder--org-babel-select-file' over open org buffers
and `org-agenda-files', falling back to `read-file-name'.
Buffers already visiting the file before this call are left open afterwards."
  (interactive)
  (let* ((target (or file
                     (if (derived-mode-p 'org-mode)
                         (buffer-file-name)
                       (builder--org-babel-select-file))))
         (already-open (find-buffer-visiting target))
         (buf (or already-open (find-file-noselect target))))
    (with-current-buffer buf
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-execute-buffer)))
    (unless already-open
      (kill-buffer buf))))

;;;###autoload
(defun builder-org-babel-execute-directory (directory)
  "Execute all Babel source blocks in every .org file under DIRECTORY.
Calls `builder-org-babel-execute-file' on each file found by
`directory-files-recursively'."
  (interactive (list (read-directory-name "Execute org files in: ")))
  (seq-do #'builder-org-babel-execute-file
          (directory-files-recursively directory "\\.org\\'")))

(builder-register-candidates
 :name "org-babel-file"
 :pipeline
 (when-let* ((filename (buffer-file-name))
             (_ (derived-mode-p 'org-mode))
             (short-name (f-collapse-homedir filename))
             (basename (file-name-nondirectory filename))
             (directory (f-dirname filename)))
   (list (make-builder-candidate
        :name (format "org-babel-execute %s (emacsclient)" basename)
        :command (format "emacsclient --eval '(builder-org-babel-execute-file \"%s\")'" filename)
        :directory directory
        :annotation (format "execute all babel blocks in %s via emacsclient" short-name)
        :priority 0)
       (make-builder-candidate
        :name (format "org-babel-execute %s (fresh emacs)" basename)
        :command (format "emacs --org-exec %s" (shell-quote-argument filename))
        :directory directory
        :annotation (format "execute all babel blocks in %s in a fresh Emacs" short-name)
        :priority 0))))

(builder-register-candidates
 :name "org-babel-directory"
 :pipeline
 (when-let* ((_ (derived-mode-p 'org-mode))
             (org-files (seq-filter (lambda (f) (string-suffix-p ".org" f))
                                    (f-files project-root-directory nil t)))
             (_ org-files))
   (list (make-builder-candidate
        :name (format "org-babel-execute <%s> (emacsclient)" project-name)
        :command (format "emacsclient --eval '(builder-org-babel-execute-directory \"%s\")'"
                         project-root-directory)
        :directory project-root-directory
        :annotation (format "execute all org babel blocks in %s via emacsclient" project-name))
       (make-builder-candidate
        :name (format "org-babel-execute <%s> (fresh emacs)" project-name)
        :command (format "emacs --org-exec-dir %s"
                         (shell-quote-argument project-root-directory))
        :directory project-root-directory
        :annotation (format "execute all org files in %s in a fresh Emacs" project-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(builder-register-candidates
 :name "emacs-lisp-package"
 :pipeline
 (when-let* ((_ (derived-mode-p 'emacs-lisp-mode))
             (_ (builder-elisp-package-p project-root-directory))
             (display-name (builder-elisp-package--name project-root-directory))
             (lisp-dir (f-join user-emacs-directory "lisp")))
   (seq-map
    (lambda (it)
      (let ((op (car it))
	    (fn (cadr it))
	    (desc (caddr it)))
	(make-builder-candidate
	 :name (format "%s-<%s>" op display-name)
	 :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'builder)\" -f %s" lisp-dir fn)
	 :directory project-root-directory
	 :annotation (format "%s elisp package <%s>" desc display-name))))
    '(("check-package"   "elpaish-run-checks" "run all checks (lint/test/compile) for")
      ("test-package"    "builder-elisp-package-test"    "run ert tests for")
      ("compile-package" "builder-elisp-package-compile" "byte-compile sources of")
      ("lint-package"    "builder-elisp-package-lint"    "run linters (byte-compile/checkdoc/package-lint) for")
      ("build-package"   "builder-elisp-package-build"   "build installable .tar via package-build for")
      ("clean-package"   "builder-elisp-package-clean"   "remove build artifacts of")))))

(builder-register-candidates
 :name "emacs-lisp-test-files"
 :pipeline
 (when-let* ((_ (derived-mode-p 'emacs-lisp-mode))
             (test-files (builder-elisp-package--test-files project-root-directory))
             (lisp-dir (f-join user-emacs-directory "lisp")))
   (seq-map
    (lambda (file)
      (let ((rel-file (f-relative file project-root-directory)))
        (make-builder-candidate
         :name (format "ert-test-file <%s>" rel-file)
         :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'builder)\" --eval \"(progn (add-to-list 'load-path \\\"%s\\\") (when (file-directory-p \\\"%s\\\") (add-to-list 'load-path \\\"%s\\\")) (load \\\"%s\\\") (ert-run-tests-batch-and-exit))\""
                          lisp-dir
                          project-root-directory
                          (expand-file-name "test" project-root-directory)
                          (expand-file-name "test" project-root-directory)
                          file)
         :directory project-root-directory
         :annotation (format "run ERT tests in %s" rel-file))))
    test-files)))

(builder-register-candidates
 :name "emacs-lisp-project-test"
 :pipeline
 (when-let* ((_ (derived-mode-p 'emacs-lisp-mode))
             (_ (not (builder-elisp-package-p project-root-directory)))
             (test-files (builder-elisp-package--test-files project-root-directory))
             (lisp-dir (f-join user-emacs-directory "lisp")))
   (list (make-builder-candidate
          :name (format "test-project-ert-all-<%s>" project-name)
          :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'builder)\" -f builder-elisp-package-test" lisp-dir)
          :directory project-root-directory
          :annotation (format "run all ERT tests for project <%s>" project-name)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs-lisp -- Cask project operations

(builder-register-candidates
 :name "emacs-lisp-cask"
 :pipeline
 (when (file-regular-p (f-join project-root-directory "Cask"))
   (list (make-builder-candidate
        :name (format "cask-install <%s>" project-name)
        :command "cask install"
        :directory project-root-directory
        :annotation (format "install Cask dependencies for %s" project-name))
       (make-builder-candidate
        :name (format "cask-test <%s>" project-name)
        :command "cask exec ert-runner"
        :directory project-root-directory
        :annotation (format "run ert tests via ert-runner for %s" project-name))
       (make-builder-candidate
        :name (format "cask-build <%s>" project-name)
        :command "cask build"
        :directory project-root-directory
        :annotation (format "byte-compile sources via Cask for %s" project-name))
       (make-builder-candidate
        :name (format "cask-clean <%s>" project-name)
        :command "cask clean-elc"
        :directory project-root-directory
        :annotation (format "remove compiled .elc files via Cask for %s" project-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(builder-register-candidates
 :name "emacs-lisp-file-isolated-check"
 :pipeline
 (when-let* ((filename (buffer-file-name))
             (_ (derived-mode-p 'emacs-lisp-mode))
             (short-name (f-collapse-homedir filename))
             (basename (file-name-nondirectory filename))
             (directory (f-dirname filename)))
   (list (make-builder-candidate
        :name (format "isolated-byte-compile %s" basename)
        :command (format "emacsclient --eval '(builder-emacs-conf-byte-compile-and-delete-artifact \"%s\")'" filename)
        :directory directory
        :annotation (format "byte-compile %s in a clean subprocess" short-name))
       (make-builder-candidate
        :name (format "isolated-load-check %s" basename)
        :command (format "emacsclient --eval '(builder-emacs-conf-load-check \"%s\")'" filename)
        :directory directory
        :annotation (format "load %s in a clean subprocess" short-name)))))

(builder-register-candidates
 :name "emacs-lisp-package-isolated-test"
 :pipeline
 (when-let* ((_ (derived-mode-p 'emacs-lisp-mode))
             (_ (builder-elisp-package-p project-root-directory))
             (display-name (builder-elisp-package--name project-root-directory)))
   (list (make-builder-candidate
        :name (format "test-package-isolated-<%s>" display-name)
        :command (format "emacsclient --eval '(builder-emacs-conf-elisp-package-test-isolated \"%s\")'" project-root-directory)
        :directory project-root-directory
        :annotation (format "run ert tests for elisp package <%s> in an isolated subprocess" display-name)))))
(builder-register-candidates
 :name "emacs-conf-ci-tasks"
 :pipeline
 (when-let* ((root project-root-directory)
             (name project-name)
             (_ (or (string-equal name "dot-emacs")
                    (string-equal root (expand-file-name user-emacs-directory)))))
   (list (make-builder-candidate
        :name "ci-run-tests"
        :command "emacsclient --eval '(builder-emacs-conf-run-ci-tests-isolated)'"
        :directory root
        :priority 0
        :annotation "run full CI ERT test suite in an isolated subprocess (never the live daemon)")
       (make-builder-candidate
        :name "ci-clean-elc"
        :command (format "emacsclient --eval '(builder-emacs-conf-clean-elc %S)'" root)
        :directory root
        :priority 0
        :annotation "delete all .elc files matching CI cleanup step")
       (make-builder-candidate
        :name "ci-docker-tests"
        :command "emacsclient --eval '(builder-run-ci-tests-docker)'"
        :directory root
        :priority 0
        :annotation "run CI test suite in silex/emacs:30.2-ci docker container"))))

(builder-register-candidates
 :name "elpaish-build-tasks"
 :pipeline
 (when-let* ((root project-root-directory)
	     (_ (string-equal "elpaish" (file-name-nondirectory root))))
   (list (make-builder-candidate
        :name "build-elpa-all"
        :command "emacsclient --eval '(progn (require (quote elpaish)) (elpaish-build-all))'"
        :directory root
        :annotation "build all ELPAish package release streams (snapshot, stable, staging)")
       (make-builder-candidate
        :name "build-elpa-website"
        :command "emacsclient --eval '(progn (require (quote elpaish-website)) (elpaish-build-website))'"
        :directory root
        :annotation "rebuild ELPAish static website only (catalogs and landing page)")
       (make-builder-candidate
        :name "build-elpa-single"
        :command "emacsclient --eval '(progn (require (quote elpaish)) (call-interactively (function elpaish-build-single)))'"
        :directory root
        :annotation "interactively build a single ELPAish package")
       (make-builder-candidate
        :name "build-elpa-snapshot"
        :command "emacsclient --eval '(progn (require (quote elpaish)) (elpaish-build-all (quote snapshot)))'"
        :directory root
        :annotation "build elpaish development snapshot track (date versions)")
       (make-builder-candidate
        :name "build-elpa-stable"
        :command "emacsclient --eval '(progn (require (quote elpaish)) (elpaish-build-all (quote stable)))'"
        :directory root
        :annotation "build elpaish stable release track (clean semver tags only)")
       (make-builder-candidate
        :name "build-elpa-staging"
        :command "emacsclient --eval '(progn (require (quote elpaish)) (elpaish-build-all (quote staging)))'"
        :directory root
        :annotation "build elpaish staging pre-release & git describe track")
       (make-builder-candidate
        :name "elpa-preflight"
        :command "emacsclient --eval '(progn (require (quote elpaish)) (elpaish-status-preflight-all))'"
        :directory root
        :annotation "run preflight quality validation across all registered recipes")
       (make-builder-candidate
        :name "elpa-rotate-keys"
        :command "emacsclient --eval '(progn (require (quote elpaish)) (call-interactively (function elpaish-rotate-keys)))'"
        :directory root
        :annotation "rotate GPG signing subkey and sync with GitHub secrets")
       (make-builder-candidate
        :name "elpa-serve-local"
        :command "emacsclient --eval '(progn (require (quote elpaish)) (elpaish-serve-local))'"
        :directory root
        :annotation "start local preview HTTP server for public/ repository")
       (make-builder-candidate
        :name "elpaish-menu"
        :command "emacsclient --eval '(progn (require (quote elpaish)) (elpaish-menu))'"
        :directory root
        :annotation "open ELPAish transient dispatch menu")
       (make-builder-candidate
        :name "elpaish-status"
        :command "emacsclient --eval '(progn (require (quote elpaish)) (elpaish-status))'"
        :directory root
        :annotation "open ELPAish status and package track management buffer"))))



(provide 'builder-tasks)
;;; builder-tasks.el ends here
