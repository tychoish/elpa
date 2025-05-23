;;; gptel-fn-complete-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from gptel-fn-complete.el

(autoload 'gptel-fn-complete-mark-function "gptel-fn-complete" "\
Put mark at end of this function, point at beginning.

If STEPS is negative, mark `- arg - 1` extra functions backward.
The behavior for when STEPS is positive is not currently well-defined.

(fn &optional STEPS)" t)
(autoload 'gptel-fn-complete "gptel-fn-complete" "\
Complete function at point using an LLM.

Either the function at point or the current region will be used for
context, along with any other context that has already been added to
gptel.

If DRY-RUN is non-nil, construct and return the full query data as usual,
but do not send the request.

(fn &optional DRY-RUN)" t)
(autoload 'gptel-fn-complete-send "gptel-fn-complete" "\
Complete region using an LLM.

If SINGLE-FUNCTION-P is non-nil, encourage the LLM to return a single
function.

If DRY-RUN is non-nil, construct and return the full query data as usual,
but do not send the request.

(fn SINGLE-FUNCTION-P &optional DRY-RUN)")
(register-definition-prefixes "gptel-fn-complete" '("gptel-fn-complete-"))

;;; End of scraped data

(provide 'gptel-fn-complete-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; gptel-fn-complete-autoloads.el ends here
