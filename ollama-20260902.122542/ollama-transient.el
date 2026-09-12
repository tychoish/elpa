;;; ollama-transient.el --- Transient menu for ollama.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jiale.liu

;; Author: jiale.liu <im@liujiale.me>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (transient "0.3.0") (ollama "0.1") (ollama-status "0.1"))
;; Keywords: ollama, ai, models, transient
;; URL: https://github.com/nailuoGG/ollama.el

;;; Commentary:
;; This package provides a transient menu interface for ollama.el commands.
;; Avoids uppercase letters in menu bindings in all cases.

;;; Code:

(require 'transient)
(require 'ollama)
(require 'ollama-status)

;;;###autoload
(transient-define-prefix ollama-transient-menu ()
  "Ollama model management interface.

Note: Model operations are asynchronous. If an operation fails,
check the server status with 'k' or start the daemon with 'st'."
  :info-manual "(ollama.el) Model Management"
  ["Model Operations"
   ("l" "List models" ollama-list-models)
   ("r" "Run/start model" ollama-run-model)
   ("p" "Pull model" ollama-pull-model)
   ("d" "Delete model" ollama-delete-model)
   ("c" "Copy model" ollama-copy-model)
   ("i" "Show model info" ollama-show-model)]
  ["Status & Server"
   ("s"  "Show status dashboard" ollama-status)
   ("g"  "Refresh models" ollama-status-refresh)
   ("k"  "Check server status" ollama-check-server)
   ("st" "Start server daemon" ollama-start-server)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun ollama-transient-setup ()
  "Setup and display the Ollama transient menu."
  (interactive)
  (condition-case err
      (ollama-transient-menu)
    (error
     (message "Error starting Ollama transient interface: %s"
              (error-message-string err)))))

(provide 'ollama-transient)
;;; ollama-transient.el ends here
