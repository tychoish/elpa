;;; builder.el --- Interactive compilation buffer and task runner -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (f "0.20.0") (xtdlib "0.1.0") (annotated-completing-read "0.1.0") (eglot-test-at-point "0.1.0") (mcpkit "0.1.0"))
;; Keywords: tools, compilation, convenience
;; URL: https://github.com/tychoish/builder

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Provides interactive compilation buffer selection and project-aware
;; command discovery for build, test, and lint workflows.  Candidates are
;; registered via `builder-register-candidates' and dispatched via `builder',
;; the primary interactive entry point.
;;
;; Key features:
;; - Smart project-aware candidate discovery across Go, Python, Rust,
;;   Makefiles, Justfiles, Markdown, and Emacs Lisp.
;; - Global minor mode `builder-mode' managing compilation error regexps,
;;   ANSI colorization, background notifications, and shortcuts.
;; - Extensible Emacs Lisp package workflows with dynamic path resolvers,
;;   dependency analysis, isolated subprocess checks, and live in-session
;;   package reloading.
;; - Model Context Protocol (MCP) service for AI agents and external tools
;;   via `mcpkit' (`builder-mcp').

;;; Code:

(require 'builder-core)
(require 'builder-mode)
(require 'builder-elisp)
(require 'builder-isolate-elisp)
(require 'builder-tasks)
(require 'builder-mcp)

;;;###autoload (autoload 'builder "builder-core" "Primary interactive entry point for `builder'." t)
;;;###autoload (autoload 'builder-compile-project "builder-core" "Run interactive compilation for project." t)
;;;###autoload (autoload 'builder-change-directory "builder-core" "Change compilation directory." t)
;;;###autoload (autoload 'builder-compile "builder-mode" "Run interactive compile operation." t)
;;;###autoload (autoload 'builder-mode "builder-mode" "Global minor mode for compilation enhancements." t)
;;;###autoload (autoload 'builder-elisp-package-reload "builder-elisp" "Reload an elisp package into current session." t)
;;;###autoload (autoload 'builder-elisp-package-test "builder-elisp" "Run tests for elisp package." t)
;;;###autoload (autoload 'builder-elisp-package-compile "builder-elisp" "Compile elisp package files." t)
;;;###autoload (autoload 'builder-run-ert-tests-with-prefix "builder-elisp" "Run ERT tests matching prefix." t)
;;;###autoload (autoload 'builder-run-ert-tests-with-glob "builder-elisp" "Run ERT tests matching glob pattern." t)
;;;###autoload (autoload 'builder-isolated-check "builder-isolate-elisp" "Run isolated subprocess or daemon check." nil)
;;;###autoload (autoload 'builder-mcp-start "builder-mcp" "Start or merge the builder MCP service into mcpkit." t)

(provide 'builder)
;;; builder.el ends here
