;;; helm-slime-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-slime" "helm-slime.el" (0 0 0 0))
;;; Generated autoloads from helm-slime.el

(autoload 'helm-slime-complete "helm-slime" "\
Select a symbol from the SLIME completion systems." t nil)

(autoload 'helm-slime-list-connections "helm-slime" "\
Yet another `slime-list-connections' with `helm'." t nil)

(autoload 'helm-slime-apropos "helm-slime" "\
Yet another `slime-apropos' with `helm'." t nil)

(autoload 'helm-slime-repl-history "helm-slime" "\
Select an input from the SLIME repl's history and insert it." t nil)

(autoload 'helm-slime-mode "helm-slime" "\
Use Helm for SLIME xref selections.
Note that the local minor mode has a global effect, thus making
`global-helm-slime-mode' and `helm-slime-mode' equivalent.

If called interactively, enable Helm-Slime mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-helm-slime-mode 'globalized-minor-mode t)

(defvar global-helm-slime-mode nil "\
Non-nil if Global Helm-Slime mode is enabled.
See the `global-helm-slime-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-helm-slime-mode'.")

(custom-autoload 'global-helm-slime-mode "helm-slime" nil)

(autoload 'global-helm-slime-mode "helm-slime" "\
Toggle Helm-Slime mode in all buffers.
With prefix ARG, enable Global Helm-Slime mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Helm-Slime mode is enabled in all buffers where
`helm-slime-mode' would do it.
See `helm-slime-mode' for more information on Helm-Slime mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-slime" '("helm-slime-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-slime-autoloads.el ends here
