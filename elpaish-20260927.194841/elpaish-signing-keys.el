;;; elpaish-signing-keys.el --- GPG signing key ceremony and verification -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maint, tools, elpa, package, security

;;; Commentary:
;; Verification and generation helpers for the GPG signing key an ELPAish
;; archive (this one, or any deployment built on this package) uses to sign
;; packages in CI. Works standalone — nothing here requires `elpaish.el' to
;; be loaded, though it will use `elpaish-gpg-key' when it is.
;;
;; Verification (`elpaish-verify-signing-key') confirms:
;;   1. A sign-capable Ed25519 subkey exists under the given primary key.
;;   2. That subkey is not expired.
;;   3. The subkey can produce a detached signature with NO passphrase
;;      supplied, loopback pinentry mode — i.e. exactly the code path
;;      `elpaish--sign-with-gpg-cli' uses in headless CI when
;;      `ELPAISH_GPG_PASSPHRASE' is unset. This is the only way to know
;;      whether the subkey will actually work in CI (a passphrase-protected
;;      subkey usually fails silently there instead of prompting).
;;
;; Verification and the passphrase-less-sign check use the built-in `epg'
;; library (`epg-list-keys', `epg-sign-string') instead of hand-parsed
;; `--with-colons' output — both are first-class, well-supported epg
;; operations. Key GENERATION and MODIFICATION (`--quick-generate-key',
;; `--quick-add-key', `--passwd') deliberately do NOT go through epg: epg's
;; high-level API has no wrapper for the modern `--quick-*' family or
;; `--passwd', and forcing them through its private `epg--start' plumbing
;; was tested and found unreliable (it can hang: epg only auto-answers
;; GET_LINE/GET_BOOL/GET_HIDDEN prompts when `epg-context-operation' is
;; `edit-key', and neither `--quick-add-key' nor `--passwd' go through
;; `--edit-key''s menu). Those stay on plain `call-process'/
;; `call-process-region', which has been verified to work reliably.
;;
;; Generation covers the ceremony's key-material steps: primary certification
;; key, a signing subkey created WITHOUT a passphrase (batch mode can't
;; answer a passphrase prompt, and `--gen-revoke' plainly refuses to run in
;; batch mode at all, so we lean on GnuPG's own auto-generated
;; openpgp-revocs.d/<FPR>.rev instead of trying to script `--gen-revoke'),
;; and exporting all the artifacts a maintainer needs.
;;
;; `elpaish-set-key-passphrase' is the general add/change/remove-protection
;; command; `elpaish-add-signing-subkey' is really just "add a subkey, then
;; make sure it ends up with no protection" and shares the same fallback.
;;
;; GnuPG can only accept ONE static passphrase value per invocation (its
;; `--passphrase'/`--passphrase-fd' flags answer every passphrase query in
;; that invocation with the same string — verified empirically: supplying
;; a real unlock passphrase to `--quick-add-key' also protects the newly
;; created subkey with that same passphrase). So: a "fast path" attempt
;; with one value (empty for "no protection", or the desired new value)
;; works whenever there is nothing ELSE to unlock first. When a key already
;; has a DIFFERENT existing passphrase, unlocking it and setting a new/no
;; passphrase on the result are two distinct prompts that need two distinct
;; answers — GnuPG only supports that via real, separate pinentry dialogs.
;; The fallback routes those through Emacs's own minibuffer using the
;; `pinentry' package (GNU ELPA) acting as gpg-agent's pinentry program.
;;
;; Usage (from a shell — always via an explicit --eval, not bare --load;
;; `command-line-args-left' during --load processing is unreliable to key
;; off of when other switches follow it):
;;   emacs --batch --load elpaish-signing-keys.el \
;;     --eval '(kill-emacs (if (elpaish-verify-signing-key) 0 1))'         ; auto-detect key
;;   emacs --batch --load elpaish-signing-keys.el \
;;     --eval '(kill-emacs (if (elpaish-verify-signing-key "<KEY-ID>") 0 1))'
;;
;; Usage (from a live Emacs, e.g. `M-x eval-expression' or `M-:'; the
;; interactive commands below can also be run via `M-x'):
;;   (elpaish-verify-signing-key "<KEY-ID-OR-FPR>")
;;   (elpaish-add-signing-subkey "<MASTER-FPR>")           ; add a fresh, passphrase-less subkey
;;                                                          ; to an EXISTING master key — this is
;;                                                          ; the remediation for a subkey that
;;                                                          ; failed the passphrase-less check
;;   (elpaish-set-key-passphrase "<KEY-ID>")               ; add/change/remove protection on
;;                                                          ; any key (empty new passphrase
;;                                                          ; removes protection)
;;   (elpaish-run-key-ceremony "Name <email>")             ; full ceremony from scratch

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'epg)

(declare-function pinentry-start "pinentry")
(defgroup elpaish-signing-keys nil
  "GPG signing key verification and generation for ELPAish archives."
  :group 'external)

(defcustom elpaish-signing-key-uid-hint "ELPAish"
  "Substring to look for in a secret key's UID when auto-detecting the
signing key. Only consulted as a last resort by
`elpaish-verify-signing-key''s auto-detection — the `elpaish-gpg-key'
customization variable (when the `elpaish' package is loaded and it is
set) and the ELPAISH_SIGNING_KEY / ELPAISH_GPG_KEY environment variables
are checked first. Override this if your key's UID doesn't happen to
contain the word \"ELPAish\"."
  :type 'string
  :group 'elpaish-signing-keys)

(defcustom elpaish-key-output-dir "~/garen/emacs/keys/elpaish/"
  "Directory where generated ELPAish key material is written."
  :type 'directory
  :group 'elpaish-signing-keys)

(defcustom elpaish-signing-key-default-expiry "1y"
  "Default expiry period passed to `--quick-add-key' when generating a
signing subkey (see `elpaish-add-signing-subkey')."
  :type 'string
  :group 'elpaish-signing-keys)

(defconst elpaish-signing-keys--ed25519-algorithm-id 22
  "GnuPG public-key algorithm ID for EdDSA (Ed25519), per RFC 4880bis.")

;;; Verification (epg-based)

(defun elpaish-verify-signing-key--report (label ok detail)
  "Print a PASS/FAIL line for LABEL/OK/DETAIL and return OK unchanged."
  (message "[%s] %s%s" (if ok "PASS" "FAIL") label
           (if detail (format " — %s" detail) ""))
  ok)

(defun elpaish-verify-signing-key--find-key (key-id)
  "Return the `epg-key' for KEY-ID in the secret keyring, or nil."
  (car (epg-list-keys (epg-make-context 'OpenPGP) key-id t)))

(defun elpaish-verify-signing-key--auto-detect ()
  "Return a configured or auto-detected signing key ID/fingerprint.
Prefers, in order: the `elpaish-gpg-key' customization variable (when the
`elpaish' package is loaded and it is set), the ELPAISH_SIGNING_KEY or
ELPAISH_GPG_KEY environment variables (the same ones `elpaish.el' itself
checks), and finally a substring search over secret key UIDs for
`elpaish-signing-key-uid-hint'."
  (or (and (boundp 'elpaish-gpg-key) elpaish-gpg-key)
      (getenv "ELPAISH_SIGNING_KEY")
      (getenv "ELPAISH_GPG_KEY")
      (let ((match (seq-find
                    (lambda (key)
                      (seq-some (lambda (uid)
                                  (string-match-p (regexp-quote elpaish-signing-key-uid-hint)
                                                   (epg-user-id-string uid)))
                                (epg-key-user-id-list key)))
                    (epg-list-keys (epg-make-context 'OpenPGP) nil t))))
        (and match (epg-sub-key-fingerprint (car (epg-key-sub-key-list match)))))))

(defun elpaish-verify-signing-key--find-sign-subkey (key)
  "Return the first sign-capable `epg-sub-key' in KEY, or nil."
  (seq-find (lambda (sub) (memq 'sign (epg-sub-key-capability sub)))
            (epg-key-sub-key-list key)))

(defun elpaish-verify-signing-key--subkey-expired-p (sub)
  "Return non-nil if `epg-sub-key' SUB has a past expiration time."
  (let ((expiry (epg-sub-key-expiration-time sub)))
    (and expiry (not (equal expiry 0)) (time-less-p expiry (current-time)))))

(defun elpaish-verify-signing-key--ed25519-p (sub)
  "Return non-nil if `epg-sub-key' SUB uses the EdDSA (Ed25519) algorithm."
  (eql (epg-sub-key-algorithm sub) elpaish-signing-keys--ed25519-algorithm-id))

(defun elpaish-verify-signing-key--passphraseless-sign-p (key-id)
  "Attempt a real detached signature with KEY-ID, no passphrase, via epg.
Mirrors `elpaish--sign-with-gpg-cli' exactly (see lisp/elpaish.el): loopback
pinentry mode with a passphrase callback that always answers with an empty
string, matching what CI does when `ELPAISH_GPG_PASSPHRASE' is unset. A pass
here means CI will actually be able to sign with this key today."
  (let ((context (epg-make-context 'OpenPGP)))
    (setf (epg-context-pinentry-mode context) 'loopback)
    (epg-context-set-passphrase-callback context (lambda (&rest _) ""))
    (setf (epg-context-signers context) (epg-list-keys context key-id t))
    (condition-case nil
        (progn (epg-sign-string context "elpaish signing verification payload\n" 'detached)
               t)
      (error nil))))

;;;###autoload
(cl-defun elpaish-verify-signing-key (&optional key-id)
  "Verify the ELPAish CI signing subkey for KEY-ID (or auto-detect it).
Prints a pass/fail report for each check and returns non-nil overall."
  (interactive)
  (let ((resolved-key (or key-id (elpaish-verify-signing-key--auto-detect))))
    (if (not resolved-key)
        (elpaish-verify-signing-key--report
         "key found" nil "no configured or auto-detected signing key, and none supplied")
      (let* ((key (elpaish-verify-signing-key--find-key resolved-key))
             (subkey (and key (elpaish-verify-signing-key--find-sign-subkey key)))
             (results
              (list
               (elpaish-verify-signing-key--report "key found" (not (null key)) resolved-key)
               (elpaish-verify-signing-key--report
                "sign-capable subkey present" (not (null subkey))
                (and subkey (epg-sub-key-id subkey))))))
        (when subkey
          (setq results
                (append
                 results
                 (list
                  (elpaish-verify-signing-key--report
                   "subkey uses Ed25519" (elpaish-verify-signing-key--ed25519-p subkey) nil)
                  (elpaish-verify-signing-key--report
                   "subkey not expired"
                   (not (elpaish-verify-signing-key--subkey-expired-p subkey))
                   (let ((expiry (epg-sub-key-expiration-time subkey)))
                     (if (and expiry (not (equal expiry 0)))
                         (format-time-string "expires %Y-%m-%d" expiry)
                       "no expiration set")))
                  (elpaish-verify-signing-key--report
                   "signs with NO passphrase (the CI path)"
                   (elpaish-verify-signing-key--passphraseless-sign-p resolved-key)
                   "if this fails, either regenerate the subkey with no passphrase, or configure gpg-agent.conf's allow-loopback-pinentry and set ELPAISH_GPG_PASSPHRASE")))))
        (seq-every-p #'identity results)))))

;;; Generation and modification helpers (raw gpg CLI — see Commentary)

(defun elpaish-generate--gnupg-homedir ()
  "Return the active GnuPG home directory via `gpgconf'."
  (with-temp-buffer
    (call-process "gpgconf" nil t nil "--list-dirs" "homedir")
    (string-trim (buffer-string))))

(defun elpaish-generate--gpg-with-passphrase (args passphrase)
  "Run gpg ARGS in batch/loopback mode, piping PASSPHRASE via stdin.
Return a cons (EXIT-CODE . OUTPUT-STRING), OUTPUT-STRING including the
`--status-fd 1' machine-readable status lines. GnuPG reuses PASSPHRASE for
every passphrase query in this one invocation — there is no way to supply
different values per query in a single command (verified: `--quick-add-key'
given a real unlock passphrase also protects the new subkey with that same
passphrase). Pass an empty string when there is nothing existing to unlock,
for a genuinely unprotected result."
  (with-temp-buffer
    (insert passphrase "\n")
    (let ((exit-code
           (apply #'call-process-region (point-min) (point-max) "gpg" t t nil
                  "--batch" "--status-fd" "1" "--pinentry-mode" "loopback"
                  "--passphrase-fd" "0" args)))
      (cons exit-code (buffer-string)))))

(defun elpaish-generate--extract-created-fpr (output key-created-type)
  "Extract the fingerprint from a KEY_CREATED status line in OUTPUT.
KEY-CREATED-TYPE is \"P\" for a new primary key or \"S\" for a new subkey."
  (when (string-match
         (format "\\[GNUPG:\\] KEY_CREATED %s \\([0-9A-F]+\\)" key-created-type) output)
    (match-string 1 output)))

(defun elpaish-generate--ensure-emacs-pinentry ()
  "Make gpg-agent route pinentry prompts through this Emacs session.
Requires the `pinentry' package (GNU ELPA) and `allow-emacs-pinentry' in
gpg-agent.conf (appended here if missing, followed by a `gpgconf --reload').
Needed only when a key already has a passphrase different from the one
being supplied: GnuPG then needs two DIFFERENT passphrases in one
invocation (the real one to unlock, and a new/empty one for the result),
and its own `--passphrase'/`--passphrase-fd' flags cannot supply two
distinct values in a single invocation — real, distinct pinentry dialogs
can."
  (unless (require 'pinentry nil t)
    (user-error "Install the `pinentry' package first: M-x package-install RET pinentry"))
  (let* ((homedir (elpaish-generate--gnupg-homedir))
         (conf-file (expand-file-name "gpg-agent.conf" homedir)))
    (make-directory homedir t)
    (unless (and (file-exists-p conf-file)
                 (with-temp-buffer
                   (insert-file-contents conf-file)
                   (goto-char (point-min))
                   (re-search-forward "^allow-emacs-pinentry" nil t)))
      (with-temp-buffer
        (when (file-exists-p conf-file) (insert-file-contents conf-file))
        (goto-char (point-max))
        (unless (bobp) (insert "\n"))
        (insert "allow-emacs-pinentry\n")
        (write-region (point-min) (point-max) conf-file))
      (call-process "gpgconf" nil nil nil "--reload" "gpg-agent"))
    (pinentry-start)))

;;;###autoload
(cl-defun elpaish-generate-master-key (identity &optional passphrase)
  "Generate an offline Ed25519 certification-only [C] primary key for IDENTITY.
IDENTITY is a UID string, e.g. \"ELPAish Package Signing <elpa@example.com>\".
Returns the new primary key's fingerprint.

The master key SHOULD be passphrase-protected (unlike the signing subkey —
see `elpaish-add-signing-subkey'). When PASSPHRASE is omitted and this is
called interactively, you are prompted for one with confirmation via
`read-passwd'; pass an explicit empty string only if you deliberately want
an unprotected master."
  (interactive
   (list (read-string "Primary key identity (Name <email>): ")
         (read-passwd "Master key passphrase (confirm): " t)))
  (let* ((effective-passphrase (or passphrase (read-passwd "Master key passphrase (confirm): " t)))
         (result (elpaish-generate--gpg-with-passphrase
                  (list "--quick-generate-key" identity "ed25519" "cert" "0")
                  effective-passphrase))
         (fpr (elpaish-generate--extract-created-fpr (cdr result) "P")))
    (if fpr
        (message "Generated master key %s for %s" fpr identity)
      (message "Master key generation did not report a fingerprint — check *Messages* / gpg output"))
    fpr))

;;;###autoload
(defun elpaish-add-signing-subkey (master-fpr &optional expiry)
  "Add a passphrase-less Ed25519 signing subkey [S] to MASTER-FPR.
EXPIRY defaults to `elpaish-signing-key-default-expiry'. Returns the new
subkey's fingerprint.

Tries the fully headless path first: an empty passphrase in loopback mode,
which GnuPG stores as a genuinely unprotected key when MASTER-FPR has no
existing protection to unlock (verified against a scratch keyring) — no
prompts at all in that case. If MASTER-FPR already has a passphrase, that
fast path fails, so this falls back to routing the two DISTINCT prompts
\(master unlock, then the new subkey's own — leave that one empty) through
Emacs's minibuffer via `elpaish-generate--ensure-emacs-pinentry'."
  (interactive "sMaster key fingerprint: ")
  (let* ((result (elpaish-generate--gpg-with-passphrase
                  (list "--quick-add-key" master-fpr "ed25519" "sign" (or expiry elpaish-signing-key-default-expiry)) ""))
         (fpr (elpaish-generate--extract-created-fpr (cdr result) "S")))
    (or fpr
        (progn
          (message "Master key is passphrase-protected — falling back to interactive prompts.")
          (elpaish-generate--ensure-emacs-pinentry)
          (message "You will be asked for the MASTER key's passphrase once, then for the NEW subkey's passphrase — leave that second one EMPTY and confirm.")
          (let* ((output (with-temp-buffer
                            (call-process "gpg" nil t nil "--batch" "--status-fd" "1"
                                          "--quick-add-key" master-fpr "ed25519" "sign"
                                          (or expiry elpaish-signing-key-default-expiry))
                            (buffer-string)))
                 (new-fpr (elpaish-generate--extract-created-fpr output "S")))
            (if new-fpr
                (message "Added signing subkey %s to %s" new-fpr master-fpr)
              (message "Subkey generation did not report a fingerprint — check *Messages* / gpg output"))
            new-fpr)))))

;;;###autoload
(defun elpaish-set-key-passphrase (key-id &optional new-passphrase)
  "Add, change, or remove password protection on KEY-ID.
Interactively prompts for the new passphrase — leave it empty to remove
protection entirely.

Tries the fully headless path first: this only works when KEY-ID currently
has no protection, or already has exactly NEW-PASSPHRASE. If KEY-ID has a
DIFFERENT existing passphrase, GnuPG needs that real one to unlock and the
desired NEW-PASSPHRASE for the result — two distinct values it cannot both
take from a single `--passphrase-fd' — so this falls back to routing the
CURRENT-passphrase prompt and the NEW-passphrase prompt(s) through Emacs's
minibuffer via `elpaish-generate--ensure-emacs-pinentry'."
  (interactive
   (list (read-string "Key fingerprint or ID: ")
         (read-passwd "New passphrase (leave empty to remove protection): " t)))
  (let* ((effective-new (or new-passphrase ""))
         (result (elpaish-generate--gpg-with-passphrase (list "--passwd" key-id) effective-new)))
    (if (zerop (car result))
        (message "Passphrase %s for %s"
                 (if (string-empty-p effective-new) "removed" "set") key-id)
      (message "Key already has a different passphrase — falling back to interactive prompts.")
      (elpaish-generate--ensure-emacs-pinentry)
      (message "You will be asked for the CURRENT passphrase, then the NEW one (twice, to confirm) — %s."
                (if (string-empty-p effective-new)
                    "leave the new one EMPTY to remove protection"
                  "enter the new passphrase you just chose"))
      (if (zerop (call-process "gpg" nil nil nil "--batch" "--passwd" key-id))
          (message "Passphrase updated for %s" key-id)
        (message "Passphrase change failed — check *Messages* / gpg output")))))

;;;###autoload
(defun elpaish-copy-revocation-cert (master-fpr &optional output-dir)
  "Copy MASTER-FPR's auto-generated revocation cert into OUTPUT-DIR.
GnuPG writes one automatically at key-creation time under
homedir/openpgp-revocs.d/<FPR>.rev — `--gen-revoke' cannot run in batch mode
at all (confirmed: \"gpg: can't do this in batch mode\"), so this reuses that
file instead of trying to script the interactive command."
  (interactive "sMaster key fingerprint: ")
  (let* ((target-dir (expand-file-name (or output-dir elpaish-key-output-dir)))
         (source-file (expand-file-name
                       (format "openpgp-revocs.d/%s.rev" master-fpr)
                       (elpaish-generate--gnupg-homedir)))
         (dest-file (expand-file-name "elpaish.rev.asc" target-dir)))
    (make-directory target-dir t)
    (set-file-modes target-dir #o700)
    (if (file-exists-p source-file)
        (progn
          (copy-file source-file dest-file t)
          (message "Copied revocation cert to %s" dest-file)
          dest-file)
      (message "No auto-generated revocation cert found at %s" source-file))))

;;;###autoload
(defun elpaish-export-key-material (master-fpr &optional output-dir)
  "Export all ceremony artifacts for MASTER-FPR into OUTPUT-DIR.
Writes master.key.asc (full secret backup — keep offline), signing-<year>.key
\(secret subkeys only — this is what goes to `ELPAISH_SIGNING_KEY'),
elpaish-keyring.gpg (binary public keyring), and elpaish.pub.asc (armored
public key)."
  (interactive "sMaster key fingerprint: ")
  (let* ((target-dir (expand-file-name (or output-dir elpaish-key-output-dir)))
         (signing-file (expand-file-name
                        (format "signing-%s.key" (format-time-string "%Y"))
                        target-dir)))
    (make-directory target-dir t)
    (set-file-modes target-dir #o700)
    (call-process "gpg" nil nil nil "--batch" "--yes" "--armor" "--output"
                  (expand-file-name "master.key.asc" target-dir)
                  "--export-secret-keys" master-fpr)
    (call-process "gpg" nil nil nil "--batch" "--yes" "--armor" "--output"
                  signing-file "--export-secret-subkeys" master-fpr)
    (call-process "gpg" nil nil nil "--batch" "--yes" "--output"
                  (expand-file-name "elpaish-keyring.gpg" target-dir)
                  "--export" master-fpr)
    (call-process "gpg" nil nil nil "--batch" "--yes" "--armor" "--output"
                  (expand-file-name "elpaish.pub.asc" target-dir)
                  "--export" master-fpr)
    (seq-do (lambda (f) (set-file-modes f #o600))
            (list (expand-file-name "master.key.asc" target-dir) signing-file))
    (message "Exported key material for %s to %s" master-fpr target-dir)
    target-dir))

;;;###autoload
(defun elpaish-run-key-ceremony (identity &optional output-dir expiry)
  "Run the full ELPAish key ceremony for IDENTITY end to end.
Generates the master key (prompts for its passphrase interactively — keep
one), adds a passphrase-less EXPIRY (default `elpaish-signing-key-default-expiry')
signing subkey, copies
the auto-generated revocation cert, exports all artifacts to OUTPUT-DIR
\(default `elpaish-key-output-dir'), and runs `elpaish-verify-signing-key'.
This mints real key material — confirm you mean to run it."
  (interactive "sPrimary key identity (Name <email>): ")
  (if (not (y-or-n-p (format "Generate a new ELPAish master key for %S? " identity)))
      (message "Aborted.")
    (when-let* ((master-fpr (elpaish-generate-master-key identity)))
      (when-let* ((subkey-fpr (elpaish-add-signing-subkey master-fpr expiry)))
        (elpaish-copy-revocation-cert master-fpr output-dir)
        (elpaish-export-key-material master-fpr output-dir)
        (message "Ceremony complete for %s (master %s, subkey %s). Verifying..."
                 identity master-fpr subkey-fpr)
        (elpaish-verify-signing-key master-fpr)))))

(provide 'elpaish-signing-keys)

;; Local Variables:
;; package-lint-main-file: "pkg/elpaish.el"
;; End:
;;; elpaish-signing-keys.el ends here
