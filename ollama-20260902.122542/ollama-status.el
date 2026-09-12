;;; ollama-status.el --- Status view for ollama.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jiale.liu

;; Author: jiale.liu <im@liujiale.me>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (ollama "0.1") (ollama-api "0.1") (ollama-utils "0.1"))
;; Keywords: ollama, ai, models, dashboard
;; URL: https://github.com/nailuoGG/ollama.el

;;; Commentary:
;; Dashboard interface for Ollama models.
;; Displays models in a tabulated list with information like size, modified date, etc.
;; Provides keybindings for common operations on models.

;;; Code:

(require 'ollama-api)
(require 'ollama-utils)
(require 'tabulated-list)
(require 'subr-x)

(declare-function ollama-run-model "ollama" (model-name))
(declare-function ollama-pull-model "ollama" (model-name))
(declare-function ollama-delete-model "ollama" (model-name))
(declare-function ollama-copy-model "ollama" (source destination))
(declare-function ollama-show-model "ollama" (model-name))
(declare-function evil-define-key "evil-core" (state keymap key def &rest bindings))

(defgroup ollama-status nil
  "Ollama status view."
  :group 'ollama)

(defvar ollama-status-buffer-name "*Ollama Status*"
  "Name of the buffer used for Ollama status.")

(defvar ollama-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ollama-show-model-info)
    (define-key map (kbd "u")   #'ollama-status-refresh)
    (define-key map (kbd "g")   #'ollama-status-refresh)
    (define-key map (kbd "r")   #'ollama-run-model-at-point)
    (define-key map (kbd "p")   #'ollama-pull-model)
    (define-key map (kbd "d")   #'ollama-delete-model-at-point)
    (define-key map (kbd "c")   #'ollama-copy-model)
    (define-key map (kbd "i")   #'ollama-show-model-info)
    (define-key map (kbd "s")   #'tabulated-list-sort)
    (define-key map (kbd "q")   #'quit-window)
    (with-eval-after-load 'evil
      (evil-define-key 'normal map
        "u" #'ollama-status-refresh
        "g" #'ollama-status-refresh
        "r" #'ollama-run-model-at-point
        "p" #'ollama-pull-model
        "d" #'ollama-delete-model-at-point
        "c" #'ollama-copy-model
        "i" #'ollama-show-model-info
        "s" #'tabulated-list-sort
        "q" #'quit-window))
    map)
  "Keymap for `ollama-status-mode'.")

(define-derived-mode ollama-status-mode tabulated-list-mode "Ollama Status"
  "Major mode for Ollama status view.

\\{ollama-status-mode-map}"
  (setq tabulated-list-format
        [("Name" 40 t)
         ("Size" 15 ollama--sort-size)
         ("Modified" 20 ollama--sort-modified)
         ("Format" 10 t)
         ("Params" 10 t)])
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  (setq-local revert-buffer-function 'ollama-status-refresh)
  (setq-local evil-read-only-exempt-commands
              '(ollama-status-refresh
                ollama-run-model-at-point
                ollama-pull-model
                ollama-delete-model-at-point
                ollama-copy-model
                ollama-show-model-info
                ollama-sort-models))
  (hl-line-mode 1)
  (use-local-map ollama-status-mode-map))

(defvar ollama-status--models nil
  "List of models in the current status view.")

(defun ollama-status-refresh (&optional callback)
  "Refresh the models list from the server.
If CALLBACK is provided, call it after refresh with success status."
  (interactive)
  (if (not (ollama-server-reachable-p))
      (progn
        (when (get-buffer ollama-status-buffer-name)
          (with-current-buffer (get-buffer-create ollama-status-buffer-name)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "Ollama server is not running or unreachable at %s\n\n" ollama-api-url))
              (insert "To start the server:\n")
              (insert "  • Run M-x ollama-start-server\n")
              (insert "  • Or run: sudo systemctl start ollama\n\n")
              (insert "Press 'g' or 'u' to retry once the server is active.\n"))))
        (message "Ollama server is not running at %s (run M-x ollama-start-server)" ollama-api-url)
        (when callback
          (funcall callback nil)))
    (message "Refreshing Ollama models...")
    (ollama--api-request "/api/tags"
                         "GET"
                         nil
                         (lambda (data)
                           (let ((models (alist-get 'models data)))
                             (setq ollama-status--models models)
                             (ollama--setup-model-buffer
                              ollama-status-buffer-name
                              'ollama-status-mode
                              models)
                             (message "Refreshed %d Ollama models" (length models))
                             (when callback
                               (funcall callback t))))
                         :error (lambda (err)
                                  (message "Failed to refresh Ollama models: %s" err)
                                  (when (get-buffer ollama-status-buffer-name)
                                    (with-current-buffer (get-buffer-create ollama-status-buffer-name)
                                      (let ((inhibit-read-only t))
                                        (erase-buffer)
                                        (insert (format "Error: %s\n\n" err))
                                        (insert "Press 'u' or 'g' to retry\n"))))
                                  (when callback
                                    (funcall callback nil))))))

(defun ollama-list-models ()
  "Display the list of models in a buffer."
  (interactive)
  (ollama-status))

(defun ollama-sort-models ()
  "Sort the models by the column at point."
  (interactive)
  (call-interactively 'tabulated-list-sort))

(defun ollama-status--get-model-at-point ()
  "Get the model at point."
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      (aref entry 0))))

(defun ollama-run-model-at-point ()
  "Run or preload the model at point into memory."
  (interactive)
  (let ((model-name (ollama-status--get-model-at-point)))
    (if (not model-name)
        (user-error "No model at point")
      (require 'ollama)
      (ollama-run-model model-name))))

(defun ollama-delete-model-at-point ()
  "Delete the model at point in the Ollama status buffer."
  (interactive)
  (let ((model-name (ollama-status--get-model-at-point)))
    (if (not model-name)
        (user-error "No model at point. Please position cursor on a model first")
      (when (yes-or-no-p (format "Delete model %s? " model-name))
        (message "Deleting model %s..." model-name)
        (condition-case err
            (progn
              (require 'ollama)
              (ollama-delete-model model-name)
              (run-at-time 1.5 nil
                           (lambda ()
                             (ollama-status-refresh))))
          (error
           (message "Error deleting model: %s" (error-message-string err))))))))

(defun ollama-show-model-info ()
  "Show detailed information about the model at point."
  (interactive)
  (let ((model-name (ollama-status--get-model-at-point)))
    (if (not model-name)
        (user-error "No model at point. Please position cursor on a model first")
      (require 'ollama)
      (ollama-show-model model-name))))

;;;###autoload
(defun ollama-status ()
  "Open the Ollama status dashboard.
Offers to start the Ollama server if it is currently inactive."
  (interactive)
  (if (not (ollama-server-reachable-p))
      (if (y-or-n-p (format "Ollama server is not running at %s. Start it now? " ollama-api-url))
          (ollama-start-server
           (lambda (started)
             (if started
                 (ollama-status-refresh
                  (lambda (success)
                    (when success
                      (pop-to-buffer (get-buffer-create ollama-status-buffer-name)))))
               (message "Ollama server could not be started"))))
        (with-current-buffer (get-buffer-create ollama-status-buffer-name)
          (ollama-status-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Ollama server is not running at %s\n\n" ollama-api-url))
            (insert "To start the server:\n")
            (insert "  • Run M-x ollama-start-server\n")
            (insert "  • Or run: sudo systemctl start ollama\n\n")
            (insert "Press 'g' or 'u' to refresh once the server is active.\n"))
          (pop-to-buffer (current-buffer))))
    (ollama-status-refresh
     (lambda (success)
       (when success
         (pop-to-buffer (get-buffer-create ollama-status-buffer-name)))))))

(provide 'ollama-status)
;;; ollama-status.el ends here
