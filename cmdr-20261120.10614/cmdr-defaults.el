;;; cmdr-defaults.el --- Optional general-purpose cmdr bindings -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/cmdr.el

;;; Commentary:
;; Opt-in general editing, display, window, browser, documentation, and buffer
;; management bindings as specs.  Loading this library installs nothing; add
;; desired specs (e.g., `defaults', `windows', `display', `browser', `docs',
;; `buffers') to `cmdr-opt-in-specs' and run `cmdr-spec-discover', or call
;; `(cmdr-spec-install \='defaults)'.

;;; Code:

(require 'cmdr-spec)

(cmdr-register-spec
  :name defaults
  :doc "General editing, kill-ring, and core dispatch bindings."
  :opt-in t
  :prefixes (("C-c t" . "core")
             ("C-c k" . "kill"))
  :bindings (("C-c t m" . cmdr-menu)
             ("C-c t ," . cmdr-dispatch)
             ("C-x ." . cmdr-menu)
             ("C-x ," . cmdr-dispatch)
             ("C-c k s" . backward-kill-sentence)
             ("C-c k p" . backward-kill-paragraph)
             ("C-c k f" . backward-kill-sexp)
             ("C-c k d" . delete-region)
             ("C-c k w" . delete-trailing-whitespace)
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
             ("C-c C-w" . whitespace-cleanup)
             ("C-z" . undo)
             ("C-w" . kill-region)
             ("C-h" . backward-kill-word)
             ("C-<backspace>" . backward-kill-word)
             ("M-<SPC>" . set-mark-command)
             ("M-X" . execute-extended-command-for-buffer)
             ("s-c" . clipboard-kill-ring-save)
             ("s-v" . clipboard-yank)
             ("s-x" . clipboard-kill-region)
             ("M-<up>" . move-text-up)
             ("M-<down>" . move-text-down)
             ("<mouse-2>" . clipboard-yank))
  :keymap-bindings
  ((isearch isearch-mode-map
    ("C-o" . isearch-occur))
   (comint comint-mode-map
    ("M-n" . comint-next-input)
    ("M-p" . comint-previous-input)
    ("<down>" . comint-next-matching-input-from-input)
    ("<up>" . comint-previous-matching-input-from-input))
   (dired dired-mode-map
    ("w" . wdired-change-to-wdired-mode))))

(cmdr-register-spec
  :name windows
  :doc "Window navigation and sizing bindings (windmove and increase-window)."
  :opt-in t
  :bindings (("M-h" . windmove-left)
             ("M-j" . windmove-down)
             ("M-k" . windmove-up)
             ("M-l" . windmove-right)
             ("S-<left>" . windmove-left)
             ("S-<down>" . windmove-down)
             ("S-<up>" . windmove-up)
             ("S-<right>" . windmove-right)
             ("s-h" . increase-window-left)
             ("s-j" . increase-window-down)
             ("s-k" . increase-window-up)
             ("s-l" . increase-window-right)
             ("s-<left>" . increase-window-left)
             ("s-<down>" . increase-window-down)
             ("s-<up>" . increase-window-up)
             ("s-<right>" . increase-window-right)))

(cmdr-register-spec
  :name display
  :doc "Display scaling, opacity, and text wrapping bindings."
  :opt-in t
  :prefixes (("C-c f" . "display")
             ("C-c f o" . "opacity"))
  :bindings (("C-c f =" . text-scale-increase)
             ("C-c f -" . text-scale-decrease)
             ("C-c f 0" . text-scale-reset)
             ("C-c f h" . auto-fill-mode)
             ("C-c f s" . visual-line-mode)
             ("C-c f o =" . frame-opacity-increase)
             ("C-c f o -" . frame-opacity-decrease)
             ("C-c f o 0" . frame-opacity-reset)))

(cmdr-register-spec
  :name browser
  :doc "Web browser and URL viewing bindings."
  :opt-in t
  :prefixes (("C-c w" . "web-browser"))
  :bindings (("C-c w d" . browse-url-generic)
             ("C-c w e" . browse-url)
             ("C-c w f" . browse-url-firefox)
             ("C-c w c" . browse-url-chrome)
             ("C-c w g" . eww-search-words)))

(cmdr-register-spec
  :name docs
  :doc "Documentation, eldoc, and help bindings."
  :opt-in t
  :prefixes (("C-c h" . "docs"))
  :bindings (("C-c h h" . help)
             ("C-c h v" . describe-variable)
             ("C-c h j" . jump-to-elisp-help)
             ("C-c h e" . eldoc)
             ("C-c h b" . eldoc-doc-buffer)
             ("C-c h c" . hyperspec-lookup)))

(cmdr-register-spec
  :name buffers
  :doc "Buffer switching, killing, and management bindings."
  :opt-in t
  :prefixes (("C-x C-b" . "buffer-control"))
  :bindings (("C-x C-b d" . kill-buffers-in-directory)
             ("C-x C-b <SPC>" . revert-buffer-quick)
             ("C-x C-b m" . kill-buffers-matching-mode)
             ("C-x C-b h" . bury-buffer)
             ("C-x C-b b" . switch-to-buffer)
             ("C-x C-b n" . switch-to-buffer-other-window)
             ("C-x C-b k" . kill-this-buffer)))

(provide 'cmdr-defaults)
;;; cmdr-defaults.el ends here
