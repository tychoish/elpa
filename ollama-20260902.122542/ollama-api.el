;;; ollama-api.el --- API communication for ollama.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 jiale.liu

;; Author: jiale.liu <im@liujiale.me>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))
;; Keywords: ollama, ai, api, http
;; URL: https://github.com/nailuoGG/ollama.el

;;; Commentary:
;; Low-level API communication layer for Ollama.
;; Handles HTTP requests, JSON parsing, error handling, server status checks,
;; and daemon process management.
;; Uses the `request' library for HTTP communication.

;;; Code:

(require 'request)
(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'url-parse)

(defgroup ollama-api nil
  "Ollama API settings."
  :group 'ollama)

(defcustom ollama-api-url "http://localhost:11434"
  "Base URL for Ollama API."
  :type 'string
  :group 'ollama-api)

(defcustom ollama-api-timeout 30
  "Default timeout in seconds for API requests."
  :type 'integer
  :group 'ollama-api)

;;;###autoload
(defun ollama-server-reachable-p (&optional url timeout)
  "Return non-nil if the Ollama server at URL is reachable.
URL defaults to `ollama-api-url'. TIMEOUT is in seconds (default 1)."
  (condition-case nil
      (let* ((target (or url ollama-api-url))
             (parsed (url-generic-parse-url target))
             (host (or (url-host parsed) "localhost"))
             (port (or (url-port parsed) 11434))
             (proc (open-network-stream "ollama-probe" nil host port :timeout (or timeout 1))))
        (when proc
          (delete-process proc)
          t))
    (error nil)))

;;;###autoload
(defun ollama-start-server (&optional callback)
  "Attempt to start the Ollama server daemon.
Checks for systemd user unit, systemd system unit, or the `ollama' CLI binary.
If CALLBACK is provided, call it with t if started, nil otherwise."
  (interactive)
  (if (ollama-server-reachable-p)
      (progn
        (message "Ollama server is already running at %s" ollama-api-url)
        (when callback (funcall callback t)))
    (message "Starting Ollama server daemon...")
    (let ((proc nil))
      (cond
       ;; 1. Systemd user service
       ((and (executable-find "systemctl")
             (eq 0 (call-process "systemctl" nil nil nil "--user" "cat" "ollama.service")))
        (setq proc (make-process :name "ollama-start"
                                 :buffer "*ollama-start*"
                                 :command '("systemctl" "--user" "start" "ollama.service"))))
       ;; 2. Systemd system service (via sudo if passwordless, or direct)
       ((and (executable-find "systemctl")
             (eq 0 (call-process "systemctl" nil nil nil "cat" "ollama.service")))
        (setq proc (make-process :name "ollama-start"
                                 :buffer "*ollama-start*"
                                 :command '("sudo" "systemctl" "start" "ollama.service"))))
       ;; 3. CLI fallback
       ((executable-find "ollama")
        (setq proc (start-process "ollama-server" "*ollama-server*" "ollama" "serve"))))
      (if (not proc)
          (progn
            (message "Unable to find a method to start Ollama server (no systemd service or ollama executable)")
            (when callback (funcall callback nil)))
        ;; Poll server reachability for up to 5 seconds
        (let ((attempts 0)
              (max-attempts 16)
              (timer nil))
          (setq timer
                (run-at-time
                 0.3 0.3
                 (lambda ()
                   (setq attempts (1+ attempts))
                   (cond
                    ((ollama-server-reachable-p)
                     (cancel-timer timer)
                     (message "Ollama server daemon started and listening at %s" ollama-api-url)
                     (when callback (funcall callback t)))
                    ((>= attempts max-attempts)
                     (cancel-timer timer)
                     (message "Failed to connect to Ollama server at %s after launch attempt" ollama-api-url)
                     (when callback (funcall callback nil))))))))))))

(defun ollama--api-request (endpoint &optional method data callback &rest args)
  "Make an API request to the Ollama server.
ENDPOINT is the API endpoint (e.g. \"/api/generate\").
METHOD is the HTTP method (GET, POST, etc.), defaults to POST if DATA is provided, GET otherwise.
DATA is the data to send (will be JSON encoded).
CALLBACK is called with the parsed response.
ARGS contains optional parameters like :error callback."
  (let* ((url (concat ollama-api-url endpoint))
         (method (or method (if data "POST" "GET")))
         (headers '(("Content-Type" . "application/json")))
         (error-callback (plist-get args :error)))

    ;; Fail fast if server is unreachable, avoiding noisy curl crashes
    (if (not (ollama-server-reachable-p))
        (let ((msg (format "Ollama server is not running or unreachable at %s" ollama-api-url)))
          (if error-callback
              (funcall error-callback msg)
            (message "%s" msg))
          nil)
      (condition-case err
          (request
           url
           :type method
           :headers headers
           :data (when data
                   (condition-case json-err
                       (json-encode data)
                     (error
                      (let ((msg (format "JSON encoding error: %s" (error-message-string json-err))))
                        (if error-callback
                            (funcall error-callback msg)
                          (message "%s" msg))
                        nil))))
           :parser (lambda ()
                     (let ((raw-response (buffer-string)))
                       (cond
                        ;; For DELETE requests, allow empty responses
                        ((and (string= method "DELETE") (string-empty-p (string-trim raw-response)))
                         '((success . t)))
                        ;; Ignore curl metadata or empty buffer when request failed
                        ((or (string-prefix-p "(:num-redirects" (string-trim raw-response))
                             (string-prefix-p "(error" (string-trim raw-response))
                             (string-empty-p (string-trim raw-response)))
                         nil)
                        ;; Parse JSON
                        (t
                         (condition-case json-err
                             (json-read)
                           (json-error
                            (if (string= method "DELETE")
                                '((success . t))
                              (unless (or (string-prefix-p "(:num-redirects" (string-trim raw-response))
                                          (string-empty-p (string-trim raw-response)))
                                (message "JSON parsing error: %s. Raw response: %s"
                                         (error-message-string json-err)
                                         raw-response))
                              nil))
                           (error
                            (message "Error parsing response: %s" (error-message-string json-err))
                            nil))))))
           :timeout ollama-api-timeout
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (cond
                        ((and (string= method "DELETE") (null data))
                         (when callback
                           (funcall callback '((success . t)))))
                        ((null data)
                         (let ((msg "API returned invalid JSON response"))
                           (if error-callback
                               (funcall error-callback msg)
                             (message "%s" msg))))
                        (t
                         (when callback
                           (funcall callback data))))))
           :error (cl-function
                   (lambda (&key error-thrown response &allow-other-keys)
                     (let* ((status-code (when (and response (request-response-p response))
                                           (request-response-status-code response)))
                            (err-raw (if (consp error-thrown) (cdr error-thrown) error-thrown))
                            (err-str (string-trim (format "%s" (or err-raw "connection failed"))))
                            (msg (format "Ollama API Error: %s (HTTP %s)"
                                         err-str
                                         (or status-code "server unreachable"))))
                       (if error-callback
                           (funcall error-callback msg)
                         (message "%s" msg))))))
        (error
         (let ((msg (format "Request error: %s" (error-message-string err))))
           (if error-callback
               (funcall error-callback msg)
             (message "%s" msg))))))))

(defun ollama--check-server (&optional callback)
  "Check if the Ollama server is running.
If CALLBACK is provided, call it with t if server is running, nil otherwise."
  (if (ollama-server-reachable-p)
      (when callback (funcall callback t))
    (message "Ollama server not running or unreachable at %s" ollama-api-url)
    (when callback (funcall callback nil))))

(defun ollama--get-local-models (&optional callback)
  "Get list of local models from Ollama server.
If CALLBACK is provided, call it with the list of models."
  (ollama--check-server
   (lambda (server-running)
     (if (not server-running)
         (progn
           (message "Ollama server is not running or unreachable at %s" ollama-api-url)
           (when callback
             (funcall callback nil)))
       (ollama--api-request "/api/tags"
                            "GET"
                            nil
                            (lambda (data)
                              (when callback
                                (let ((models (alist-get 'models data)))
                                  (funcall callback models))))
                            :error (lambda (err)
                                     (message "Failed to get local models: %s" err)
                                     (when callback
                                       (funcall callback nil))))))))

(provide 'ollama-api)
;;; ollama-api.el ends here
