;;; orgx-global.el --- Lightweight Orgx global mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Global bindings and deferred Orgx setup.

;;; Code:

(declare-function orgx-setup "orgx")
(declare-function orgx--setup-babel-once "orgx")
(declare-function orgx--setup-modules-once "orgx")
(declare-function orgx-mode-turn-on "orgx")
(declare-function orgx-agenda-minor-mode-turn-on "orgx")
(declare-function orgx--disable-advice "orgx")
(declare-function orgx-global--revert-settings "orgx")
(declare-function orgx-gist-export-private "orgx")
(declare-function orgx-gist-export-public "orgx")
(declare-function orgx-capture "orgx")
(declare-function orgx-capture-shell "orgx-capture-shell")
(declare-function orgx-capture-shell-toggle "orgx-capture-shell")
(declare-function orgx-capture-shell-transient "orgx-capture-shell")
(declare-function orgx-capture-shell-execute-command "orgx-capture-shell")
(declare-function orgx-archive-done-tasks-to-archive-sibling "orgx")
(declare-function orgx-archive-done-tasks-to-archive-file "orgx")
(declare-function orgx-mark-done-and-archive "orgx")
(declare-function orgx-bulk-todo-state "orgx-bulk")
(declare-function orgx-bulk-done "orgx-bulk")
(declare-function orgx-bulk-cancel "orgx-bulk")
(declare-function orgx-bulk-resume "orgx-bulk")
(declare-function orgx-agenda-view "orgx")
(declare-function orgx-insert-date "orgx")
(declare-function orgx-agenda-files-open "orgx")
(declare-function orgx-agenda-files-reload "orgx")
(declare-function orgx-agenda-untagged-in-file "orgx")
(declare-function orgx-agenda-for-file "orgx")
(declare-function org-agenda "org-agenda")
(declare-function org-capture "org-capture")
(declare-function consult-org-heading "consult")
(declare-function agent-shell-queue-org-refile-from-heading "agent-shell-queue")
(declare-function org-capture-goto-last-stored "org-capture")
(declare-function org-capture-refile "org-capture")
(declare-function org-cycle-force-archived "org-cycle")
(declare-function org-archive-set-tag "org-archive")
(declare-function org-archive-to-archive-sibling "org-archive")
(declare-function org-store-link "ol")
(declare-function org-id-store-link "org-id")
(declare-function org-insert-link "ol")
(declare-function org-narrow-to-subtree "org")
(declare-function org-insert-property-drawer "org")
(declare-function org-save-all-org-buffers "org")
(declare-function org-set-tags-command "org")
(declare-function org-refile "org-refile")
(autoload 'orgx-gist-export-private "orgx" nil t)
(autoload 'orgx-gist-export-public "orgx" nil t)
(autoload 'orgx-capture-shell-transient "orgx-capture-shell" nil t)
(autoload 'orgx-capture-shell-execute-command "orgx-capture-shell" nil t)
(autoload 'org-annotate-file "org-annotate-file" nil t)
(autoload 'org-cycle-force-archived "org-cycle" nil t)
(autoload 'org-archive-set-tag "org-archive" nil t)
(autoload 'org-archive-to-archive-sibling "org-archive" nil t)
(autoload 'org-capture-goto-last-stored "org-capture" nil t)
(autoload 'org-capture-refile "org-capture" nil t)
(autoload 'org-id-store-link "org-id" nil t)
(autoload 'org-insert-property-drawer "org" nil t)
(autoload 'org-narrow-to-subtree "org" nil t)
(autoload 'org-save-all-org-buffers "org" nil t)
(autoload 'org-set-tags-command "org" nil t)
(declare-function orgx--setup-capture-once "orgx")
(declare-function orgx--setup-agenda-once "orgx")
(defvar orgx--setup-done)
(defvar orgx-global-mode)
(defvar orgx-global--initialized nil)

(defvar-keymap orgx-gist-map
  :name "orgx-gist"
  :doc "Gist export commands under C-c o g (orgx-mode)."
  "p" #'orgx-gist-export-private
  "g" #'orgx-gist-export-public)

;;; Keymaps and minor modes

(defvar-keymap orgx-capture-map
  :name "orgx-capture"
  :doc "Capture commands under C-c o c (orgx-mode).")

(keymap-set orgx-capture-map "c" #'orgx-capture)
(keymap-set orgx-capture-map "m" #'org-capture)
(keymap-set orgx-capture-map "p" #'org-capture-goto-last-stored)
(keymap-set orgx-capture-map "l" #'org-capture-goto-last-stored)
(keymap-set orgx-capture-map "t" #'orgx-capture-shell-toggle)
(keymap-set orgx-capture-map "r" #'org-capture-refile)
(keymap-set orgx-capture-map "w" #'org-capture-refile)
(keymap-set orgx-capture-map "s" #'orgx-capture-shell)
(keymap-set orgx-capture-map "x" #'orgx-capture-shell-transient)

(defvar-keymap orgx-archive-map
  :name "orgx-archive"
  :doc "Archive commands under C-c o f (orgx-mode).")

(keymap-set orgx-archive-map "a" (cons "done→sibling" #'orgx-archive-done-tasks-to-archive-sibling))
(keymap-set orgx-archive-map "e" #'org-cycle-force-archived)
(keymap-set orgx-archive-map "f" (cons "done→file" #'orgx-archive-done-tasks-to-archive-file))
(keymap-set orgx-archive-map "t" #'org-archive-set-tag)
(keymap-set orgx-archive-map "s" (cons "current→sibling" #'org-archive-to-archive-sibling))
(keymap-set orgx-archive-map "d" (cons "done+archive" #'orgx-mark-done-and-archive))

(defvar-keymap orgx-bulk-map
  :name "orgx-bulk"
  :doc "Bulk operation commands under C-c o b (orgx-mode)."
  "b" #'orgx-bulk-todo-state
  "d" #'orgx-bulk-done
  "c" #'orgx-bulk-cancel
  "r" #'orgx-bulk-resume)

(defvar-keymap orgx-link-map
  :name "org-link"
  :doc "Org link commands under C-c o l (orgx-mode)."
  "s" #'org-store-link
  "a" #'org-id-store-link
  "i" #'org-insert-link
  "n" #'org-annotate-file)

(defvar-keymap orgx-command-map
  :name "org"
  :doc "Command keymap for orgx commands mounted under C-c o.")

(keymap-set orgx-command-map "4" #'org-agenda)
(keymap-set orgx-command-map "a" #'orgx-agenda-view)
(keymap-set orgx-command-map "b" (cons "bulk" orgx-bulk-map))
(keymap-set orgx-command-map "c" orgx-capture-map)
(keymap-set orgx-command-map "d" #'orgx-insert-date)
(keymap-set orgx-command-map "f" orgx-archive-map)
(keymap-set orgx-command-map "g" orgx-gist-map)
(keymap-set orgx-command-map "h" #'consult-org-heading)
(keymap-set orgx-command-map "j" #'orgx-capture)
(keymap-set orgx-command-map "k" #'org-capture)
(keymap-set orgx-command-map "l" orgx-link-map)
(keymap-set orgx-command-map "m" #'orgx-capture-shell-transient)
(keymap-set orgx-command-map "n" #'org-narrow-to-subtree)
(keymap-set orgx-command-map "o" #'orgx-agenda-files-open)
(keymap-set orgx-command-map "p" #'org-insert-property-drawer)
(keymap-set orgx-command-map "q" #'agent-shell-queue-org-refile-from-heading)
(keymap-set orgx-command-map "r" #'orgx-agenda-files-reload)
(keymap-set orgx-command-map "s" #'org-save-all-org-buffers)
(keymap-set orgx-command-map "t" #'org-set-tags-command)
(keymap-set orgx-command-map "u" (cons "untagged-in-file" #'orgx-agenda-untagged-in-file))
(keymap-set orgx-command-map "w" #'org-refile)
(keymap-set orgx-command-map "x" #'orgx-capture-shell-execute-command)
(keymap-set orgx-command-map "/" #'orgx-agenda-for-file)

(defvar-keymap orgx-mode-map
  :doc "Buffer-local minor mode keymap activating commands under C-c o."
  "C-c o" orgx-command-map)

(defvar-keymap orgx-global-mode-map
  :doc "Global minor mode keymap activating commands under C-c o."
  "C-c o" orgx-command-map)

(defun orgx-global--ensure-setup (&rest _)
  "Initialize Orgx when an Org buffer, agenda, or capture is used."
  (unless orgx-global--initialized
    (require 'orgx)
    (unless orgx--setup-done
      (orgx-setup))
    (setq orgx-global--initialized t)))

(defcustom orgx-global-warm-delay 2
  "Idle seconds before `orgx-global-mode' registers capture and agenda setup.
Nil disables the warm-up, leaving setup to the first Org buffer, capture or
agenda."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'orgx)

(defvar orgx-global--warm-timer nil
  "Pending idle timer for `orgx-global--warm'.")

(defun orgx-global--warm ()
  "Register capture templates and agenda commands without waiting for first use.
Other consumers (capture selectors, queues, MCP tools) read
`org-capture-templates' without going through `org-capture'."
  (setq orgx-global--warm-timer nil)
  (when orgx-global-mode
    (orgx-global--ensure-setup)
    (orgx--setup-capture-once)
    (orgx--setup-agenda-once)))

(defun orgx-global--org-mode-setup ()
  "Initialize Orgx for the first Org buffer."
  (unless orgx-global--initialized
    (orgx-global--ensure-setup)
    (orgx--setup-modules-once)
    (orgx--setup-babel-once)
    (orgx-mode-turn-on)))

(defun orgx-global--before-capture (&rest _)
  "Prepare the first capture when it triggers Orgx initialization."
  (unless orgx-global--initialized
    (orgx-global--ensure-setup)
    (orgx--setup-capture-once))
  (advice-remove 'org-capture #'orgx-global--before-capture)
  (advice-remove 'orgx-capture #'orgx-global--before-capture))

(defun orgx-global--before-agenda (&rest _)
  "Prepare the first agenda when it triggers Orgx initialization."
  (unless orgx-global--initialized
    (orgx-global--ensure-setup)
    (orgx--setup-modules-once)
    (orgx--setup-agenda-once))
  (advice-remove 'org-agenda #'orgx-global--before-agenda))

;;;###autoload
(define-minor-mode orgx-global-mode
  "Enable Orgx bindings, deferring integration until Org is used."
  :global t :keymap orgx-global-mode-map :group 'orgx
  (if orgx-global-mode
      (progn
        (add-hook 'org-mode-hook #'orgx-global--org-mode-setup)
        (advice-add 'org-agenda :before #'orgx-global--before-agenda)
        (advice-add 'org-capture :before #'orgx-global--before-capture)
        (advice-add 'orgx-capture :before #'orgx-global--before-capture)
        (when (and (featurep 'org)
                   (or (featurep 'orgx) (not load-in-progress)))
          (orgx-global--ensure-setup))
        (when (and orgx-global-warm-delay (not orgx-global--warm-timer))
          (setq orgx-global--warm-timer
                (run-with-idle-timer orgx-global-warm-delay nil #'orgx-global--warm))))
    (when (timerp orgx-global--warm-timer)
      (cancel-timer orgx-global--warm-timer))
    (setq orgx-global--warm-timer nil)
    (setq orgx-global--initialized nil)
    (remove-hook 'org-mode-hook #'orgx-global--org-mode-setup)
    (advice-remove 'org-agenda #'orgx-global--before-agenda)
    (advice-remove 'org-capture #'orgx-global--before-capture)
    (advice-remove 'orgx-capture #'orgx-global--before-capture)
    (when (featurep 'orgx)
      (orgx--disable-advice)
      (orgx-global--revert-settings)
      (remove-hook 'org-mode-hook #'orgx-mode-turn-on)
      (remove-hook 'org-mode-hook #'orgx--setup-modules-once)
      (advice-remove 'org-agenda #'orgx--setup-modules-once)
      (remove-hook 'org-mode-hook #'orgx--setup-babel-once)
      (advice-remove 'org-babel-execute-src-block #'orgx--setup-babel-once)
      (remove-hook 'org-capture-mode-hook #'orgx-mode-turn-on)
      (remove-hook 'org-agenda-mode-hook #'orgx-agenda-minor-mode-turn-on)
      (setq orgx--setup-done nil))))

;;;###autoload
(defalias 'orgx-global-minor-mode #'orgx-global-mode)

(provide 'orgx-global)
;;; orgx-global.el ends here
