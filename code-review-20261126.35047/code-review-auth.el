;;; code-review-auth.el --- GitHub CLI authentication for code-review -*- lexical-binding: t; -*-

;;; Commentary:
;; Resolves GitHub tokens with `gh auth token -h HOST -u USER', so no
;; Personal Access Token or auth-source entry is needed.  The account is
;; always named explicitly (buffer-local `code-review-gh-account', else
;; `code-review-gh-default-account') and the global state of the `gh'
;; CLI is never modified: there is no `gh auth switch'.

;;; Code:

(require 'eieio)
(require 'map)
(require 'subr-x)

(defvar code-review-db--pullreq-id)
(declare-function code-review-db-get-pullreq "code-review-db")

(defgroup code-review-auth nil
  "Authentication for code-review."
  :group 'code-review)

(defcustom code-review-auth-login-marker
  'code-review
  "Symbol for looking up the github, gitlab and bitbucket login
data in auth-sources (e.g. ~/.authinfo.gpg). By default, it is
set to \\='code-review, but if you have already configured the
logins for magit forge, you can use these by setting this option
to \\='forge."
  :group 'code-review-auth
  :type 'symbol)

(defcustom code-review-auth-backend 'gh
  "Authentication backend for GitHub operations in `code-review'.
Possible values:
- `gh': resolve tokens with the GitHub CLI (`gh auth token').
- `auth-source': use `code-review-auth-login-marker' and `auth-sources'.
- A function called with (HOST ACCOUNT) that returns a token string or
  an auth-source symbol."
  :group 'code-review-auth
  :type '(choice (const :tag "GitHub CLI (gh)" gh)
                 (const :tag "Emacs auth-source" auth-source)
                 (function :tag "Custom function")))

(defcustom code-review-gh-default-account nil
  "Account name used when `code-review-gh-account' is unset.
When nil you are prompted to choose an account; `gh''s active account
is never used implicitly."
  :group 'code-review-auth
  :type '(choice (const :tag "Prompt for an account" nil)
                 (string :tag "Username")))

(defvar-local code-review-gh-account nil
  "Buffer-local GitHub account name for requests made from this buffer.
Overrides `code-review-gh-default-account'.")

(defvar code-review-gh-token-cache (make-hash-table :test #'equal)
  "Resolved tokens keyed by (HOST . ACCOUNT).")

(defvar code-review-gh--pr-accounts (make-hash-table :test #'equal)
  "Account names keyed by \"OWNER/REPO#NUMBER\".
Requests run from timers and callbacks outside the PR buffer, so the
account is kept here (outside the persisted PR object) instead of
relying on the buffer-local `code-review-gh-account'.")

(defun code-review-gh--pr-key (pr)
  "Return the account-table key for PR."
  (format "%s/%s#%s" (oref pr owner) (oref pr repo) (oref pr number)))

(defun code-review-gh-pr-account (pr)
  "Return the account registered for PR, or nil."
  (map-elt code-review-gh--pr-accounts (code-review-gh--pr-key pr)))

(defun code-review-gh-register-pr-account (pr &optional account)
  "Associate ACCOUNT with PR; default to the buffer's, then the default account.
Does nothing when there is no account to register."
  (when-let* ((name (or account code-review-gh-account code-review-gh-default-account)))
    (setf (map-elt code-review-gh--pr-accounts (code-review-gh--pr-key pr)) name)))

(defun code-review-auth-clear-cache ()
  "Forget all cached tokens."
  (interactive)
  (clrhash code-review-gh-token-cache))

(defun code-review-gh--normalize-host (host)
  "Return the bare `gh' hostname for API HOST (nil means github.com)."
  (let ((name (car (string-split
                    (replace-regexp-in-string "\\`https?://" "" (or host "github.com"))
                    "/"))))
    (if (string-equal name "api.github.com")
        "github.com"
      name)))

(defun code-review-gh--call (&rest args)
  "Run `gh' with ARGS, returning trimmed stdout or signaling `user-error'."
  (unless (executable-find "gh")
    (user-error "code-review: `gh' CLI not found on PATH"))
  (with-temp-buffer
    (let ((code (apply #'call-process "gh" nil (list (current-buffer) nil) nil args)))
      (unless (zerop code)
        (user-error "code-review: `gh %s' failed" (string-join args " ")))
      (string-trim (buffer-string)))))

(defun code-review-gh--account-name (account)
  "Return ACCOUNT, else the buffer's or default account; error when none."
  (let ((user (or account code-review-gh-account code-review-gh-default-account)))
    (unless (and (stringp user) (not (string-empty-p user)))
      (user-error "code-review: no GitHub account set; use `code-review-gh-select-account' or set `code-review-gh-default-account'"))
    user))

(defun code-review-gh-auth-token (&optional host account)
  "Return the token for HOST and ACCOUNT via `gh auth token -u ACCOUNT'.
ACCOUNT defaults to `code-review-gh-account', then
`code-review-gh-default-account'.  There is no implicit account: with
none configured this signals a `user-error' rather than letting `gh'
use its active account."
  (let* ((h (code-review-gh--normalize-host host))
         (user (code-review-gh--account-name account)))
    (let ((key (cons h user)))
      (or (map-elt code-review-gh-token-cache key)
          (let ((token (code-review-gh--call "auth" "token" "-h" h "-u" user)))
            (when (string-empty-p token)
              (user-error "code-review: empty token from `gh auth token' for %s" h))
            (setf (map-elt code-review-gh-token-cache key) token))))))

(defun code-review-auth-evict-token (token)
  "Drop every cached entry whose token is TOKEN."
  (seq-do (lambda (key)
            (when (equal (map-elt code-review-gh-token-cache key) token)
              (map-delete code-review-gh-token-cache key)))
          (map-keys code-review-gh-token-cache)))

(defun code-review-gh-ensure-account ()
  "Return the buffer's or default account, prompting once when interactive.
Returns nil without prompting in batch or when `gh' is not the backend."
  (or code-review-gh-account
      code-review-gh-default-account
      (when (and (eq code-review-auth-backend 'gh)
                 (not noninteractive))
        (call-interactively #'code-review-gh-select-account)
        code-review-gh-account)))

(defun code-review-gh-accounts (&optional host)
  "Return the logged-in `gh' account names for HOST."
  (let* ((status (json-parse-string
                  (code-review-gh--call "auth" "status" "--json" "hosts")))
         (hosts (map-elt status "hosts")))
    (seq-map (lambda (account) (map-elt account "login"))
             (map-elt hosts (code-review-gh--normalize-host host)))))

;;;###autoload
(defun code-review-gh-select-account (account)
  "Use ACCOUNT for requests from the current buffer.
Only this buffer's variable changes; the `gh' CLI state is untouched."
  (interactive
   (list (completing-read "GitHub account: " (code-review-gh-accounts) nil t)))
  (setq-local code-review-gh-account account)
  (when code-review-db--pullreq-id
    (code-review-gh-register-pr-account (code-review-db-get-pullreq) account))
  (message "code-review: using GitHub account %s in this buffer" account))

(defun code-review-auth-username (&optional account)
  "Return the `:username' value for ghub requests made with ACCOUNT.
ACCOUNT is a name or a PR object, as for `code-review-auth-resolve'.
ghub looks up `git config github.user' whenever `:username' is nil,
even when `:auth' is a token string, so with the `gh' backend the
account name is passed along.  Other backends return nil."
  (when (eieio-object-p account)
    (setq account (ignore-errors (code-review-gh-pr-account account))))
  (when (eq code-review-auth-backend 'gh)
    (code-review-gh--account-name account)))

(defun code-review-auth-resolve (forge &optional host account)
  "Return the `:auth' value for ghub requests to FORGE (a symbol) on HOST.
Only `github' uses the GitHub CLI; other forges and the `auth-source'
backend yield `code-review-auth-login-marker'.  ACCOUNT, a name or a
PR object (whose registered account is used), overrides the buffer's
account."
  (when (eieio-object-p account)
    (setq account (ignore-errors (code-review-gh-pr-account account))))
  (cond
   ((functionp code-review-auth-backend)
    (funcall code-review-auth-backend host account))
   ((and (eq code-review-auth-backend 'gh)
         (eq forge 'github))
    (code-review-gh-auth-token host account))
   (t code-review-auth-login-marker)))

(provide 'code-review-auth)
;;; code-review-auth.el ends here
