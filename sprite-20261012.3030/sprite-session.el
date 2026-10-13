;;; sprite-session.el --- Map agent-driver session ids to sprite bindings -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/tychoish/sprite
;; Keywords: tools, daemon, processes

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; sprite-session.el is a small, driver-agnostic registry mapping an
;; opaque agent-driver "session id" (a string -- could be an
;; `agent-shell' buffer name, an `asq' queue item id, a `gptel' buffer
;; name, or anything else) to plain sprite-identifying data: a sprite
;; id, a port, and a worktree path.  Callers supply these three values
;; directly; this module makes no assumption about where they came
;; from and has no dependency on any particular sprite-spawning
;; package.
;;
;; This module does not know or validate which driver a session id
;; belongs to, does not spawn or kill sprites itself, and does not
;; require `agent-shell-queue' or `gptel' -- callers pass a free-form
;; `driver' symbol/string when binding a session, and this file simply
;; keeps the association around and lets it be looked up, filtered by
;; driver, and later removed.
;;
;; Unbinding a session never kills the underlying sprite -- lifecycle
;; and reaping of the underlying sprite/daemon is entirely the
;; caller's responsibility.  This file only owns the
;; session-id-to-sprite bookkeeping.

;;; Code:

;;;; Registry

(defvar sprite-session-registry (make-hash-table :test #'equal)
  "Maps agent-driver session-id strings to sprite binding plists.
Each value is a plist of the form:
  (:sprite-id STRING :port INTEGER :worktree STRING :driver SYMBOL-OR-STRING)
STRING/INTEGER values are supplied directly by the caller of
`sprite-session-bind'; `driver' is a free-form label the caller
supplies and this file neither validates nor interprets.")

;;;; Bind / unbind / lookup

;;;###autoload
(defun sprite-session-bind (session-id sprite-id port worktree &optional driver)
  "Bind SESSION-ID to SPRITE-ID, PORT, and WORKTREE.
SPRITE-ID is a string, PORT is an integer, and WORKTREE is a string
path; the caller is responsible for supplying values that identify a
live sprite.  DRIVER is an optional free-form symbol or string
identifying which agent-driver owns SESSION-ID (e.g. `agent-shell',
`asq', `gptel'); this module does not validate or enumerate driver
values.

Rebinding SESSION-ID to the same SPRITE-ID as an existing binding is
idempotent (a no-op re-bind). Rebinding SESSION-ID to a *different*
sprite without first calling `sprite-session-unbind' signals a
`user-error'."
  (let ((existing (gethash session-id sprite-session-registry)))
    (when (and existing (not (equal (plist-get existing :sprite-id) sprite-id)))
      (user-error "sprite-session-bind: session %s already bound to sprite %s; unbind first"
                  session-id (plist-get existing :sprite-id)))
    (puthash session-id
             (list :sprite-id sprite-id
                   :port port
                   :worktree worktree
                   :driver driver)
             sprite-session-registry)))

;;;###autoload
(defun sprite-session-unbind (session-id)
  "Remove the sprite binding for SESSION-ID, if any.
Does not stop, kill, or otherwise touch the underlying sprite daemon;
this only removes the session-to-sprite bookkeeping. Returns non-nil
if a binding existed and was removed."
  (when (gethash session-id sprite-session-registry)
    (remhash session-id sprite-session-registry)
    t))

;;;###autoload
(defun sprite-session-lookup (session-id)
  "Return the binding plist for SESSION-ID, or nil if unbound."
  (gethash session-id sprite-session-registry))

;;;###autoload
(defun sprite-session-sprites-for-driver (driver)
  "Return an alist of (SESSION-ID . PLIST) for all sessions bound under DRIVER."
  (let (result)
    (maphash (lambda (session-id plist)
               (when (equal (plist-get plist :driver) driver)
                 (push (cons session-id plist) result)))
             sprite-session-registry)
    (nreverse result)))

(provide 'sprite-session)
;;; sprite-session.el ends here
