;;; cmdr-defaults.el --- Optional general-purpose cmdr bindings -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/cmdr.el

;;; Commentary:
;; Opt-in general editing, display and navigation bindings, as a spec.  Loading
;; this library installs nothing; add `defaults' to `cmdr-opt-in-specs' and run
;; `cmdr-spec-discover', or call `(cmdr-spec-install \='defaults)'.

;;; Code:

(require 'cmdr-spec)

(cmdr-register-spec
  :name defaults
  :doc "General editing, display, help, buffer and navigation bindings."
  :opt-in t
  :prefixes (("C-c t" . "core")
             ("C-c f" . "display")
             ("C-c f o" . "opacity")
             ("C-c k" . "kill")
             ("C-c w" . "web-browser")
             ("C-c h" . "docs")
             ("C-x C-b" . "buffer-control"))
  :bindings (("C-c t m" . cmdr-menu)
             ("C-c t ," . cmdr-dispatch)
             ("C-x ." . cmdr-menu)
             ("C-x ," . cmdr-dispatch)
             ("C-c f =" . text-scale-increase)
             ("C-c f -" . text-scale-decrease)
             ("C-c f 0" . text-scale-reset)
             ("C-c f h" . auto-fill-mode)
             ("C-c f s" . visual-line-mode)
             ("C-c f o =" . frame-opacity-increase)
             ("C-c f o -" . frame-opacity-decrease)
             ("C-c f o 0" . frame-opacity-reset)
             ("C-c k s" . backward-kill-sentence)
             ("C-c k p" . backward-kill-paragraph)
             ("C-c k f" . backward-kill-sexp)
             ("C-c k d" . delete-region)
             ("C-c k w" . delete-trailing-whitespace)
             ("C-c w d" . browse-url-generic)
             ("C-c w e" . browse-url)
             ("C-c w f" . browse-url-firefox)
             ("C-c w c" . browse-url-chrome)
             ("C-c w g" . eww-search-words)
             ("C-c h h" . help)
             ("C-c h v" . describe-variable)
             ("C-c h j" . jump-to-elisp-help)
             ("C-c h e" . eldoc)
             ("C-c h b" . eldoc-doc-buffer)
             ("C-c h c" . hyperspec-lookup)
             ("C-x C-b d" . kill-buffers-in-directory)
             ("C-x C-b <SPC>" . revert-buffer-quick)
             ("C-x C-b m" . kill-buffers-matching-mode)
             ("C-x C-b h" . bury-buffer)
             ("C-x C-b b" . switch-to-buffer)
             ("C-x C-b n" . switch-to-buffer-other-window)
             ("C-x C-b k" . kill-this-buffer)
             ("C-c C-w" . whitespace-cleanup)
             ("M-." . xref-find-definitions)
             ("M-h" . windmove-left)
             ("M-j" . windmove-down)
             ("M-k" . windmove-up)
             ("M-l" . windmove-right)
             ("S-<left>" . windmove-left)
             ("S-<down>" . windmove-down)
             ("S-<up>" . windmove-up)
             ("S-<right>" . windmove-right)
             ("C-x l" . goto-line)
             ("C-x f" . find-file)
             ("C-x h" . mark-whole-buffer)
             ("C-x m" . execute-extended-command)
             ("C-x C-m" . execute-extended-command)
             ("C-x C-f" . find-file)
             ("C-x C-x" . exchange-point-and-mark)
             ("C-x C-r" . recentf)
             ("C-x C-n" . count-words)
             ("C-c i" . indent-region)
             ("C-c c" . comment-region)
             ("C-c u w" . upcase-word)
             ("C-c u t" . upcase-initials-region)
             ("C-c u r" . upcase-region)
             ("C-c C-f" . set-fill-column)
             ("C-c C-p" . set-mark-command)
             ("C-c C-r" . rename-buffer)
             ("C-z" . undo)
             ("C-w" . kill-region)
             ("C-h" . backward-kill-word)
             ("C-<backspace>" . backward-kill-word)
             ("M-<SPC>" . set-mark-command)
             ("M-X" . execute-extended-command-for-buffer)
             ("s-c" . clipboard-kill-ring-save)
             ("s-v" . clipboard-yank)
             ("s-x" . clipboard-kill-region)
             ("s-h" . increase-window-left)
             ("s-j" . increase-window-down)
             ("s-k" . increase-window-up)
             ("s-l" . increase-window-right)
             ("s-<left>" . increase-window-left)
             ("s-<down>" . increase-window-down)
             ("s-<up>" . increase-window-up)
             ("s-<right>" . increase-window-right)
             ("M-<up>" . move-text-up)
             ("M-<down>" . move-text-down)
             ("<mouse-2>" . clipboard-yank)))

(provide 'cmdr-defaults)
;;; cmdr-defaults.el ends here
