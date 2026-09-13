;;; ollama-tailnet.el --- Ollama Tailnet Orchestration and Gptel Integration -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.0") (annotated-completing-read "0.1.0") (transient "0.4.0"))
;; Keywords: hypermedia, tools, ai, tailscale, systemd, gptel
;; URL: https://github.com/tychoish/ollama-tailnet

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Systemd configuration and gptel backend orchestration for deploying
;; and querying Ollama LLM services hosting Gemma 4 and Mistral models
;; across a Tailscale tailnet.
;;
;; Quickstart:
;;
;;   (require 'ollama-tailnet)
;;
;;   ;; Register tailnet hosts with explicit models and default model alias
;;   (ollama-tailnet-register-host 'derrida
;;     :host "derrida.tailnet-name.ts.net:11434"
;;     :default-model "gemma4:27b"
;;     :models '("gemma4:27b" "gemma4:32b" "mistral:latest"))
;;
;;   (ollama-tailnet-register-host 'laptop
;;     :host "127.0.0.1:11434"
;;     :default-model "gemma4:2b"
;;     :models '("gemma4:2b" "gemma4:e4b" "mistral:7b"))
;;
;;   ;; Or use default laptop preset configuration
;;   (ollama-tailnet-setup-laptop-presets)
;;
;;   ;; Set gptel default backend to derrida's default model alias
;;   (ollama-tailnet-set-gptel-backend 'derrida/default)
;;
;; Interactive commands:
;;   `M-x ollama-tailnet-status'            - View status of all tailnet Ollama nodes
;;   `M-x ollama-tailnet-pull-model'        - Download/pull models to a target host
;;   `M-x ollama-tailnet-service-restart'   - Restart systemd ollama.service on a host
;;   `M-x ollama-tailnet-service-status'    - Inspect systemd ollama.service status
;;   `M-x ollama-tailnet-set-gptel-backend' - Set gptel backend to a host-model spec
;;   `M-x ollama-tailnet-models'            - Tabulated dashboard of models across hosts
;;   `M-x ollama-tailnet-search-model'      - Search library and pull to a target host

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'ollama-tailnet-vars)
(require 'ollama-tailnet-gptel)
(require 'ollama-tailnet-control)
(require 'ollama-tailnet-models)

;;;###autoload
(defun ollama-tailnet-bind-hud-keys ()
  "Bind `ollama-tailnet' commands to `hud-robot-ollama-tailnet-map' if available."
  (when (boundp 'hud-robot-ollama-tailnet-map)
    (keymap-set hud-robot-ollama-tailnet-map "s" #'ollama-tailnet-status)
    (keymap-set hud-robot-ollama-tailnet-map "d" #'ollama-tailnet-discover-hosts)
    (keymap-set hud-robot-ollama-tailnet-map "p" #'ollama-tailnet-pull-model)
    (keymap-set hud-robot-ollama-tailnet-map "r" #'ollama-tailnet-service-restart)
    (keymap-set hud-robot-ollama-tailnet-map "t" #'ollama-tailnet-service-status)
    (keymap-set hud-robot-ollama-tailnet-map "b" #'ollama-tailnet-set-gptel-backend)
    (keymap-set hud-robot-ollama-tailnet-map "l" #'ollama-tailnet-models)
    (keymap-set hud-robot-ollama-tailnet-map "S" #'ollama-tailnet-search-model)))

;; Bind HUD keys at load time if map exists
(ollama-tailnet-bind-hud-keys)

(provide 'ollama-tailnet)
;;; ollama-tailnet.el ends here
