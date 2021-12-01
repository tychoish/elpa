;;; revbufs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "revbufs" "revbufs.el" (0 0 0 0))
;;; Generated autoloads from revbufs.el

(autoload 'revbufs "revbufs" "\
Examines all open buffers and reverts any that are out of date.
If there are orphans or out of date buffers, this operation opens
the `*revbufs*' buffer reporting the outcome." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "revbufs" '("revbufs-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; revbufs-autoloads.el ends here
