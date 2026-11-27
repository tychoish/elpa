;;; mcpkit-menu.el --- Transient menu for managing mcpkit services -*- lexical-binding: t; -*-

;;; Commentary:
;; `mcpkit-menu' groups the commands for inspecting, starting and stopping
;; mcpkit services and the shared MCP server.

;;; Code:

(require 'transient)
(require 'mcpkit)
(require 'mcpkit-services)

(declare-function mcpkit-stats-view-logs "mcpkit-stats")

(defun mcpkit-menu--description ()
  "Return the `mcpkit-menu' heading, including the running port."
  (if-let* ((port (mcpkit-active-port)))
      (format "MCPKit: listening on port %d" port)
    "MCPKit: not running"))

;;;###autoload (autoload 'mcpkit-menu "mcpkit-menu" nil t)
(transient-define-prefix mcpkit-menu ()
  "Manage mcpkit services and the shared MCP server."
  [:description mcpkit-menu--description
   ["Services"
    ("l" "list services" mcpkit-list-services)
    ("s" "start service" mcpkit-start-service)
    ("k" "stop service" mcpkit-stop-service)]
   ["Server"
    ("i" "init (instance port)" mcpkit-init-services)
    ("a" "start all" mcpkit-start-all-services)
    ("r" "restart all" mcpkit-restart-all-services)
    ("h" "halt all" mcpkit-stop-all-services)]
   ["Inspect"
    ("p" "show endpoint" mcpkit-show-endpoint)
    ("t" "tool stats/logs" mcpkit-stats-view-logs)]])

(provide 'mcpkit-menu)
;;; mcpkit-menu.el ends here
