;;; ollama.el --- Emacs front-end for Ollama -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jiale.liu

;; Author: jiale.liu <im@liujiale.me>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: ollama, ai, models
;; URL: https://github.com/nailuoGG/ollama.el

;;; Commentary:
;; An Emacs front-end package for managing Ollama models.
;; Provides commands for pulling, deleting, copying, and running local models,
;; as well as inspecting model details and managing the server lifecycle.

;;; Code:

(require 'ollama-api)
(require 'ollama-utils)
(require 'subr-x)

(defgroup ollama nil
  "Ollama model management."
  :group 'tools)

(defun ollama--ensure-server (on-ready)
  "Ensure the Ollama server is running before executing ON-READY.
Prompts the user to start the server if it is inactive."
  (if (ollama-server-reachable-p)
      (funcall on-ready)
    (if (y-or-n-p (format "Ollama server is not running at %s. Start it now? " ollama-api-url))
        (ollama-start-server
         (lambda (started)
           (if started
               (funcall on-ready)
             (user-error "Failed to start Ollama server"))))
      (user-error "Ollama server is not running at %s" ollama-api-url))))

;;;###autoload
(defun ollama-pull-model (model-name)
  "Pull MODEL-NAME from Ollama repository.
If called interactively, prompt for model name."
  (interactive
   (list (read-string "Model name to pull: ")))
  (if (or (null model-name) (string-empty-p model-name))
      (user-error "Model name cannot be empty")
    (ollama--ensure-server
     (lambda ()
       (message "Pulling model: %s (this may take a while)..." model-name)
       (ollama--api-request "/api/pull"
                            "POST"
                            `((name . ,model-name))
                            (lambda (_data)
                              (message "Model %s pulled successfully" model-name))
                            :error (lambda (err)
                                     (message "Failed to pull model %s: %s" model-name err)))))))

;;;###autoload
(defun ollama-delete-model (model-name)
  "Delete MODEL-NAME from Ollama.
If called interactively, prompt for model name with completion."
  (interactive
   (list (ollama-select-model)))
  (if (or (null model-name) (string-empty-p model-name))
      (user-error "Model name cannot be empty")
    (ollama--ensure-server
     (lambda ()
       (message "Deleting model: %s..." model-name)
       (condition-case err
           (ollama--api-request "/api/delete"
                                "DELETE"
                                `((model . ,model-name))
                                (lambda (_data)
                                  (message "Model %s deleted successfully" model-name))
                                :error (lambda (err)
                                         (message "Failed to delete model %s: %s" model-name err)))
         (error
          (message "Error in delete request: %s" (error-message-string err))))))))

;;;###autoload
(defun ollama-select-model (&optional callback)
  "Select an Ollama model from local models using completing-read.
If CALLBACK is provided, call it with the selected model name."
  (interactive)
  (if (not (ollama-server-reachable-p))
      (if (y-or-n-p (format "Ollama server is not running at %s. Start it now? " ollama-api-url))
          (ollama-start-server
           (lambda (started)
             (if started
                 (ollama-select-model callback)
               (user-error "Ollama server not started"))))
        (user-error "Cannot select model: Ollama server is not running at %s" ollama-api-url))
    (message "Fetching available models...")
    (ollama--get-local-models
     (lambda (model-data)
       (condition-case err
           (let* ((models (mapcar (lambda (model)
                                    (alist-get 'name model))
                                  (or model-data '()))))
             (if (null models)
                 (progn
                   (message "No models available. Use M-x ollama-pull-model to download a model")
                   (when callback
                     (funcall callback nil)))
               (if (called-interactively-p 'any)
                   (let ((selected (completing-read "Select model: " models)))
                     (if callback
                         (funcall callback selected)
                       selected))
                 (if callback
                     (funcall callback (car models))
                   (message "Warning: ollama-select-model called non-interactively without callback")
                   nil))))
         (error
          (message "Error selecting model: %s" (error-message-string err))
          (when callback
            (funcall callback nil))
          nil)))))
  nil)

;;;###autoload
(defun ollama-show-model (model-name)
  "Show information about MODEL-NAME.
If called interactively, prompt for model name with completion."
  (interactive
   (list (ollama-select-model)))
  (if (or (null model-name) (string-empty-p model-name))
      (user-error "Model name cannot be empty")
    (ollama--ensure-server
     (lambda ()
       (message "Fetching information for model: %s..." model-name)
       (condition-case err
           (ollama--api-request "/api/show"
                                "POST"
                                `((model . ,model-name))
                                (lambda (data)
                                  (with-current-buffer (get-buffer-create "*Ollama Model Info*")
                                    (let ((inhibit-read-only t))
                                      (erase-buffer)
                                      (emacs-lisp-mode)
                                      (insert ";; Model information for: " model-name "\n\n")
                                      (insert (pp-to-string data))
                                      (goto-char (point-min))
                                      (font-lock-ensure)
                                      (setq buffer-read-only t)
                                      (pop-to-buffer (current-buffer))
                                      (message "Showing information for model: %s" model-name))))
                                :error (lambda (err)
                                         (message "Failed to get information for model %s: %s" model-name err)))
         (error
          (message "Error in show model request: %s" (error-message-string err))))))))

;;;###autoload
(defun ollama-copy-model (source destination)
  "Copy SOURCE model to DESTINATION.
If called interactively, prompt for source and destination model names with completion."
  (interactive
   (list (ollama-select-model)
         (read-string "Destination model name: ")))
  (if (or (null source) (string-empty-p source))
      (user-error "Source model cannot be empty")
    (if (or (null destination) (string-empty-p destination))
        (user-error "Destination model name cannot be empty")
      (ollama--ensure-server
       (lambda ()
         (message "Copying model %s to %s..." source destination)
         (ollama--api-request "/api/copy"
                              "POST"
                              `((source . ,source)
                                (destination . ,destination))
                              (lambda (_data)
                                (message "Successfully copied model %s to %s" source destination))
                              :error (lambda (err)
                                       (message "Failed to copy model %s to %s: %s"
                                                source destination err))))))))

;;;###autoload
(defun ollama-check-server ()
  "Check if the Ollama server is running and offer to start it if inactive."
  (interactive)
  (message "Checking Ollama server status...")
  (if (ollama-server-reachable-p)
      (message "Ollama server is running at %s" ollama-api-url)
    (if (y-or-n-p (format "Ollama server is not running at %s. Start it now? " ollama-api-url))
        (ollama-start-server)
      (message "Ollama server is inactive at %s" ollama-api-url))))

;;;###autoload
(defun ollama-run-model (model-name)
  "Preload and start MODEL-NAME in the local Ollama server."
  (interactive
   (list (ollama-select-model)))
  (if (or (null model-name) (string-empty-p model-name))
      (user-error "Model name cannot be empty")
    (ollama--ensure-server
     (lambda ()
       (message "Starting/loading model %s into memory..." model-name)
       (ollama--api-request "/api/generate"
                            "POST"
                            `((model . ,model-name)
                              (prompt . "")
                              (keep_alive . "10m"))
                            (lambda (_data)
                              (message "Model %s is active and loaded in memory" model-name))
                            :error (lambda (err)
                                     (message "Failed to start model %s: %s" model-name err)))))))

(provide 'ollama)
;;; ollama.el ends here
