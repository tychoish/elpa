;;; xtdlib.el --- E(x)tended s(t)an(d)ard Emacs Lisp Library -*- lexical-binding: t -*-

;; Author: sam kleinman (tychoish)
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4") (f "0.20") (s "1.12"))
;; Keywords: utility, library, elisp.
;; URL: https://github.com/tychoish/xtdlib.el

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions and additions for f.el and s.el, plus ht.el- and
;; dash.el-style list/table helpers, project-context wrappers, and
;; utility macros for hooks, timers, and annotations to improve code
;; ergonomics and clarity.
;;
;; This file is a pure umbrella entry point: it requires (and
;; re-exports, via `provide') the split-out modules below and defines
;; nothing of its own. Callers that only need one piece (e.g. just the
;; utility macros) can `require' the specific module directly instead
;; of pulling in all of `xtdlib':
;;
;; - `xtd-s'       -- string helpers built on `s.el'
;; - `xtd-f'       -- file/path helpers built on `f.el' (depends on `xtd-s')
;; - `xtd-ht'      -- hash-table accessor macros in the style of `ht.el',
;;                    built on `map.el' (no real `ht' dependency)
;; - `xtd-dash'    -- list helpers in the style of `dash.el' (no dependency on the others)
;; - `xtd-macro'   -- general-purpose macros: hooks, timers, toggles, keymaps (depends on `xtd-s')
;; - `xtd-project'   -- project-context wrappers over `projectile'/`project.el' (depends on `xtd-s')
;; None of the modules above require `xtdlib' itself, so there is no
;; require cycle between them or with this file. `f' and `s' are listed
;; in `Package-Requires' as the package's overall transitive
;; dependencies (needed by `xtd-f' and `xtd-s' respectively), but this
;; file does not `require' them itself -- it has no code of its own
;; that would need them.

;;; Code:

(require 'xtd-s)
(require 'xtd-dash)
(require 'xtd-f)
(require 'xtd-ht)
(require 'xtd-macro)
(require 'xtd-project)
(provide 'xtdlib)
;;; xtdlib.el ends here
