# agent-shell-notifications

Desktop notifications for [agent-shell](https://github.com/xenodium/agent-shell) events.

- Notifies you when the agent needs attention or a turn has completed
- Immediate notification when the shell is not visible; configurable idle delay when it is
- Dismisses automatically when you interact with the shell
- Supports multiple concurrent agent-shell buffers
- Clicking a notification switches directly to the relevant shell buffer
- Highly configurable — control when, how, how long, and via what backend notifications are sent

## Requirements

- [agent-shell](https://github.com/xenodium/agent-shell)
- A notification backend (see [Backends](#backends))

## Installation

```elisp
(use-package agent-shell-notifications
  :straight (agent-shell-notifications
             :type git
             :host github
             :repo "zackattackz/agent-shell-notifications")

  :hook
  ;; Enable notifications in each agent-shell buffer
  (agent-shell-mode . agent-shell-notifications-mode)
  ;; If using agent-shell-viewport, also add:
  ;; (agent-shell-viewport-edit-mode . agent-shell-notifications-viewport-edit-mode)
  ;; (agent-shell-viewport-view-mode . agent-shell-notifications-viewport-view-mode)

  :config
  ;; Notification display timeout in seconds (0 = never expire (the default), -1 = backend default)
  ;; (setq agent-shell-notifications-timeout 5)

  ;; Seconds to wait before notifying when the shell is already visible (default: 10)
  ;; (setq agent-shell-notifications-idle-timeout 30)

  ;; Advanced filtering: suppress notifications during certain hours
  ;; (add-hook 'agent-shell-notifications-inhibit-functions
  ;;           (lambda (_type _event)
  ;;             (let ((hour (decoded-time-hour (decode-time))))
  ;;               (and (>= hour 9) (< hour 17)))))

  ;; Use the knockknock backend instead of the default libnotify
  ;; (setq agent-shell-notifications-provider 'agent-shell-notifications-knockknock)
  )
```

## Backends

### libnotify (default)

Uses Emacs' built-in [`notifications.el`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Notifications.html) via D-Bus. Works out of the box on Linux with a
running notification daemon. (Tested with KDE Plasma 6)

### knockknock (experimental)

Displays notifications as an in-Emacs overlay using the
[knockknock](https://github.com/konrad1977/knockknock) package. (Requires [PR #4](https://github.com/konrad1977/knockknock/pull/4)) Useful if you prefer
notifications inside Emacs or are on a system where D-Bus is unavailable.

> **Note:** This backend is experimental. It is included primarily as a working example of
> a custom backend. The author does not use knockknock personally, so it may have rough
> edges. Bug reports and improvements are welcome.

Requires `knockknock` to be installed. Enable it with:

```elisp
(setq agent-shell-notifications-provider 'agent-shell-notifications-knockknock)
```

## Configuration

### Backend

**`agent-shell-notifications-provider`** — symbol naming the backend feature to load.
Defaults to `agent-shell-notifications-libnotify`. Set to `nil` to skip auto-loading and
configure `agent-shell-notifications-send-function` and
`agent-shell-notifications-close-function` manually.

### When to notify

**`agent-shell-notifications-inhibit-functions`** — list of functions `(type event) → bool`
called before sending any notification. If any returns non-nil, the notification is
suppressed entirely (no immediate send, no idle timer scheduled). By default includes a
function that suppresses `turn-complete` notifications when the turn was cancelled by the
user. Use `add-hook` / `remove-hook` to extend or clear it.

Prefer `agent-shell-notifications-immediate-notifications` and
`agent-shell-notifications-idle-notifications` for filtering by event type — both accept a
predicate function. Reserve this hook for advanced filtering.

**`agent-shell-notifications-immediate-notifications`** — Controls which events trigger an
immediate notification when the shell buffer is not visible. Accepts:
- `t` — notify for all events (default)
- `nil` — disable immediate notifications entirely
- a list of event type symbols, e.g. `'(permission-request)` — notify only for those types
- a predicate function `(type event) → bool`

**`agent-shell-notifications-idle-notifications`** — Controls which events trigger a
notification when the shell buffer *is* visible, after an idle delay. Same value forms as
above. Defaults to `t`.

**`agent-shell-notifications-idle-timeout`** — How long to wait (in seconds) before
notifying when the shell is already visible. The use case is if you are in the shell buffer
but don't respond because you are doing something else. (Definitely not because you are on your phone)
Accepts:
- a number — used for all event types (default: `10`)
- an alist of `(TYPE . SECONDS)` pairs, with an optional `(default . SECONDS)` fallback
- a function `(type event) → number`

### Notification appearance

**`agent-shell-notifications-timeout`** — how long notifications are displayed, in seconds.
`0` means never expire; `-1` means use the backend's default. Accepts the same
forms as `agent-shell-notifications-idle-timeout` (number, alist, or function).

**`agent-shell-notifications-format-function`** — function `(type event) → plist` that
builds the notification content. The returned plist should contain at least `:title`; `:body`
is optional. Replace this to fully customize notification text.

### Hooks

**`agent-shell-notifications-shell-visible-function`** — function `(shell-buffer) → bool`
used to decide whether the shell is currently visible. The default checks whether the shell
buffer (or its viewport) is in a focused frame. Override this to integrate with custom
window management.

**`agent-shell-notifications-switch-to-shell-function`** — function `(shell-buffer)` called
when a notification action is invoked. The default raises the frame containing the shell (or
viewport) and selects its window. Override this to control how navigation works.

## Custom backends

A backend is an Elisp file that sets variables and provides a feature:

```elisp
(setq agent-shell-notifications-send-function   #'my-backend--send)
(setq agent-shell-notifications-close-function  #'my-backend--close)
;; Optional: convert the timeout unit for your backend
(setq agent-shell-notifications-transform-timeout-function #'my-backend--transform-timeout)
;; Optional: remap the standard plist keys for your backend
(setq agent-shell-notifications-transform-function #'my-backend--transform)

(provide 'agent-shell-notifications-my-backend)
```

**`agent-shell-notifications-send-function`** — called with a notification plist, should
display the notification and return an ID (or `nil` if your backend has no concept of IDs).

**`agent-shell-notifications-close-function`** — called with the ID previously returned by
the send function. Should dismiss the notification.

**`agent-shell-notifications-transform-timeout-function`** — optional. Called with the
resolved timeout in seconds before the notification plist is assembled. Should return the
timeout in whatever unit your backend expects. Defaults to `identity` (seconds pass through
unchanged). For example, the libnotify backend sets this to `(lambda (secs) (* 1000 secs))`
to convert to milliseconds, while the knockknock backend leaves it as `identity` since it
uses seconds natively.

**`agent-shell-notifications-transform-function`** — optional. Called with the standard notification
plist before it reaches your send function. Use it to rename or reformat keys for your
backend. Defaults to `identity`.

### Standard notification plist

| Key | Type | Description |
|---|---|---|
| `:title` | string | Notification title |
| `:body` | string or nil | Notification body text |
| `:app-icon` | string or nil | Path to icon file |
| `:timeout` | number | Display duration in seconds (`0` = never expire, `-1` = backend default) |
| `:actions` | list | Action labels, e.g. `'("default" "Switch to shell")` |
| `:on-action` | function | Called with `(id key)` when an action is invoked |

To load your backend, set `agent-shell-notifications-provider` to the feature symbol and
call `agent-shell-notifications-set-provider`:

```elisp
(setq agent-shell-notifications-provider 'agent-shell-notifications-my-backend)
(agent-shell-notifications-set-provider agent-shell-notifications-provider)
```

## TODO

- [ ] macOS backend — could be implemented using `terminal-notifier` or `osascript`
- [ ] Windows backend — could be implemented using `toast` or PowerShell

Contributions for either are welcome.

## Special thanks

[Alvaro Ramirez](https://xenodium.com) for creating agent-shell and the original
[agent-shell-knockknock](https://github.com/xenodium/agent-shell-knockknock) that
inspired this package. If agent-shell has been useful to you, please consider
[supporting its development](https://github.com/sponsors/xenodium).
