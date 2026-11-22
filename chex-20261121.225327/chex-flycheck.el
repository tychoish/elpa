;;; chex-flycheck.el --- Deferred Flycheck setup for Chex -*- lexical-binding: t; -*-

;;; Commentary:
;; Checker and keymap initialization performed when Flycheck is first used.

;;; Code:

(require 'seq)
(require 'chex)
(require 'flycheck)
(require 'flycheck-aspell)

(declare-function consult-flycheck "consult-flycheck")

;;;###autoload
(defun chex--initialize-flycheck ()
  "Initialize Chex checkers and Flycheck bindings."
  (flycheck-aspell-define-checker "org" "Org" ("--add-filter" "url") (org-mode))
  (flycheck-aspell-define-checker "rst" "reStructuredText" ("--add-filter" "url") (rst-mode))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
  (keymap-set flycheck-command-map ";" #'consult-flycheck)
  (keymap-set flycheck-command-map "m" #'consult-flycheck)
  (seq-do (lambda (checker) (add-to-list 'flycheck-checkers checker))
          chex-aspell-checkers)
  (remove-hook 'flycheck-mode-hook #'chex--initialize-flycheck))

(provide 'chex-flycheck)
;;; chex-flycheck.el ends here
