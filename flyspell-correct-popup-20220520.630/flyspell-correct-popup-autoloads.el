;;; flyspell-correct-popup-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flyspell-correct-popup" "flyspell-correct-popup.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flyspell-correct-popup.el

(autoload 'flyspell-correct-popup "flyspell-correct-popup" "\
Run `popup-menu*' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return a selected word to use as a replacement or a tuple
of (command, word) to be used by `flyspell-do-correct'.

\(fn CANDIDATES WORD)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flyspell-correct-popup-autoloads.el ends here
