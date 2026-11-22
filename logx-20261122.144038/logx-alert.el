;;; logx-alert.el --- Alert.el integration sink for logx -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, logging
;; URL: https://github.com/tychoish/logx

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Provides optional `alert.el' frontend integration for `logx'.
;; Emits alert notifications directly into the `logx' structured logging pipeline.

;;; Code:

(require 'logx)

(defvar alert-styles)
(declare-function alert-define-style "alert" (name &rest args))

;;;###autoload
(defun logx-alert-sink (info)
  "Route `alert' notification INFO plist into `logx' structured logging."
  (let* ((sev (or (plist-get info :severity) 'normal))
         (level (pcase sev
                  ('emergency 'fatal)
                  ('urgent    'alert)
                  ('high      'critical)
                  ('moderate  'warn)
                  ('low       'debug)
                  (_          'info)))
         (cat (or (plist-get info :category) 'alert))
         (title (plist-get info :title))
         (msg (plist-get info :message))
         (combined-msg (cond
                        ((and title msg) (format "[%s] %s" title msg))
                        (title (format "[%s]" title))
                        (msg (format "%s" msg))
                        (t "Alert notification"))))
    (logx--emit-event level (intern (format "%s" cat)) combined-msg nil info)))

;;;###autoload
(defun logx-alert-register ()
  "Register `logx' style with `alert' if alert is loaded."
  (when (fboundp 'alert-define-style)
    (alert-define-style 'logx
                        :title "Logx Structured Logger"
                        :notifier #'logx-alert-sink)))

;;;###autoload
(defun logx-alert-deregister ()
  "Deregister `logx' style from `alert'."
  (when (boundp 'alert-styles)
    (setq alert-styles (assoc-delete-all 'logx alert-styles))))

(defun logx-alert--after-load (&rest _)
  "Register `logx' style once `alert' is loaded."
  (when (featurep 'alert)
    (logx-alert-register)
    (remove-hook 'after-load-functions #'logx-alert--after-load)))

;;;###autoload
(define-minor-mode logx-alert-mode
  "Global minor mode to integrate `logx' structured logging with `alert'."
  :global t
  :group 'logx
  (if logx-alert-mode
      (if (featurep 'alert)
          (logx-alert-register)
        (add-hook 'after-load-functions #'logx-alert--after-load))
    (remove-hook 'after-load-functions #'logx-alert--after-load)
    (logx-alert-deregister)))

(provide 'logx-alert)
;;; logx-alert.el ends here
