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

(with-eval-after-load 'alert
  (logx-alert-register))

(provide 'logx-alert)
;;; logx-alert.el ends here
