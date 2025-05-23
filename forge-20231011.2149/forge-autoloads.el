;;; forge-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from forge.el

(defvar forge-add-default-bindings t "\
Whether to add Forge's bindings to various Magit keymaps.

If you want to disable this, then you must set this to nil before
`magit' is loaded.  If you do it before `forge' but after `magit'
is loaded, then `magit-mode-map' ends up being modified anyway.

If this is nil, then `forge-toggle-display-in-status-buffer' can
no longer do its job.  It might be better to set the global value
of `forge-display-in-status-buffer' to nil instead.  That way you
can still display topics on demand in the status buffer.")
(with-eval-after-load 'magit-mode (when forge-add-default-bindings (keymap-set magit-mode-map "'" #'forge-dispatch) (keymap-set magit-mode-map "N" #'forge-dispatch) (keymap-set magit-mode-map "<remap> <magit-browse-thing>" #'forge-browse) (keymap-set magit-mode-map "<remap> <magit-copy-thing>" #'forge-copy-url-at-point-as-kill)))
(with-eval-after-load 'git-commit (when forge-add-default-bindings (keymap-set git-commit-mode-map "C-c C-v" #'forge-visit-topic)))
(register-definition-prefixes "forge" '("forge-"))


;;; Generated autoloads from forge-bitbucket.el

(register-definition-prefixes "forge-bitbucket" '("forge-bitbucket-repository"))


;;; Generated autoloads from forge-commands.el

 (autoload 'forge-dispatch "forge-commands" nil t)
(autoload 'forge-pull "forge-commands" "\
Pull topics from the forge repository.

With a prefix argument and if the repository has not been fetched
before, then read a date from the user and limit pulled topics to
those that have been updated since then.

If pulling is too slow, then also consider setting the Git variable
`forge.omitExpensive' to `true'.

(fn &optional REPO UNTIL)" t)
(autoload 'forge-pull-notifications "forge-commands" "\
Fetch notifications for all repositories from the current forge." t)
(autoload 'forge-pull-topic "forge-commands" "\
Read a TOPIC and pull data about it from its forge.

(fn TOPIC)" t)
(autoload 'forge-browse-issues "forge-commands" "\
Visit the current repository's issues using a browser." t)
(autoload 'forge-browse-pullreqs "forge-commands" "\
Visit the current repository's pull-requests using a browser." t)
(autoload 'forge-browse-topic "forge-commands" "\
Read a TOPIC and visit it using a browser.
By default only offer open topics but with a prefix argument
also offer closed topics.

(fn TOPIC)" t)
(autoload 'forge-browse-issue "forge-commands" "\
Read an ISSUE and visit it using a browser.
By default only offer open issues but with a prefix argument
also offer closed issues.

(fn ISSUE)" t)
(autoload 'forge-browse-pullreq "forge-commands" "\
Read a PULL-REQUEST and visit it using a browser.
By default only offer open pull-requests but with a prefix
argument also offer closed pull-requests.

(fn PULL-REQUEST)" t)
(autoload 'forge-browse-commit "forge-commands" "\
Read a COMMIT and visit it using a browser.

(fn COMMIT)" t)
(autoload 'forge-browse-branch "forge-commands" "\
Read a BRANCH and visit it using a browser.

(fn BRANCH)" t)
(autoload 'forge-browse-remote "forge-commands" "\
Read a REMOTE and visit it using a browser.

(fn REMOTE)" t)
(autoload 'forge-browse-repository "forge-commands" "\
Read a REPOSITORY and visit it using a browser.

(fn REPOSITORY)" t)
(autoload 'forge-browse-this-topic "forge-commands" "\
Visit the topic at point using a browser." t)
(autoload 'forge-browse-this-repository "forge-commands" "\
Visit the repository at point using a browser." t)
(autoload 'forge-copy-url-at-point-as-kill "forge-commands" "\
Copy the url of the thing at point." t)
(autoload 'forge-browse "forge-commands" "\
Visit the thing at point using a browser." t)
(autoload 'forge-visit-topic "forge-commands" "\
Read a TOPIC and visit it.
By default only offer open topics for completion;
with a prefix argument also closed topics.

(fn TOPIC)" t)
(autoload 'forge-visit-issue "forge-commands" "\
Read an ISSUE and visit it.
By default only offer open topics for completion;
with a prefix argument also closed topics.

(fn ISSUE)" t)
(autoload 'forge-visit-pullreq "forge-commands" "\
Read a PULL-REQUEST and visit it.
By default only offer open topics for completion;
with a prefix argument also closed topics.

(fn PULL-REQUEST)" t)
(autoload 'forge-visit-this-topic "forge-commands" "\
Visit the topic at point." t)
(autoload 'forge-visit-this-repository "forge-commands" "\
Visit the repository at point." t)
(autoload 'forge-branch-pullreq "forge-commands" "\
Create and configure a new branch from a pull-request.
Please see the manual for more information.

(fn PULLREQ)" t)
(autoload 'forge-checkout-pullreq "forge-commands" "\
Create, configure and checkout a new branch from a pull-request.
Please see the manual for more information.

(fn PULLREQ)" t)
(autoload 'forge-checkout-worktree "forge-commands" "\
Create, configure and checkout a new worktree from a pull-request.
This is like `forge-checkout-pullreq', except that it also
creates a new worktree. Please see the manual for more
information.

(fn PATH PULLREQ)" t)
(autoload 'forge-fork "forge-commands" "\
Fork the current repository to FORK and add it as a REMOTE.
If the fork already exists, then that isn't an error; the remote
is added anyway.  Currently this only supports Github and Gitlab.

(fn FORK REMOTE)" t)
(autoload 'forge-merge "forge-commands" "\
Merge the current pull-request using METHOD using the forge's API.

If there is no current pull-request or with a prefix argument,
then read pull-request PULLREQ to visit instead.

Use of this command is discouraged.  Unless the remote repository
is configured to disallow that, you should instead merge locally
and then push the target branch.  Forges detect that you have
done that and respond by automatically marking the pull-request
as merged.

(fn PULLREQ METHOD)" t)
(autoload 'forge-rename-default-branch "forge-commands" "\
Rename the default branch to NEWNAME.
Change the name on the upstream remote and locally, and update
the upstream remotes of local branches accordingly." t)
 (autoload 'forge-add-pullreq-refspec "forge-commands" nil t)
 (autoload 'forge-add-repository "forge-commands" nil t)
(autoload 'forge-add-user-repositories "forge-commands" "\
Add all of USER's repositories from HOST to the database.
This may take a while.  Only Github is supported at the moment.

(fn HOST USER)" t)
(autoload 'forge-add-organization-repositories "forge-commands" "\
Add all of ORGANIZATION's repositories from HOST to the database.
This may take a while.  Only Github is supported at the moment.

(fn HOST ORGANIZATION)" t)
(autoload 'forge-remove-repository "forge-commands" "\
Remove a repository from the database.

(fn HOST OWNER NAME)" t)
(autoload 'forge-remove-topic-locally "forge-commands" "\
Remove a topic from the local database only.
Due to how the supported APIs work, it would be too expensive to
automatically remove topics from the local database that were
removed from the forge.  The purpose of this command is to allow
you to manually clean up the local database.

(fn TOPIC)" t)
(autoload 'forge-reset-database "forge-commands" "\
Move the current database file to the trash.
This is useful after the database scheme has changed, which will
happen a few times while the forge functionality is still under
heavy development." t)
(register-definition-prefixes "forge-commands" '("forge-"))


;;; Generated autoloads from forge-core.el

(register-definition-prefixes "forge-core" '("forge-"))


;;; Generated autoloads from forge-db.el

(register-definition-prefixes "forge-db" '("forge-"))


;;; Generated autoloads from forge-gitea.el

(register-definition-prefixes "forge-gitea" '("forge-gitea-repository"))


;;; Generated autoloads from forge-github.el

(register-definition-prefixes "forge-github" '("forge-"))


;;; Generated autoloads from forge-gitlab.el

(register-definition-prefixes "forge-gitlab" '("forge-gitlab-repository"))


;;; Generated autoloads from forge-gogs.el

(register-definition-prefixes "forge-gogs" '("forge-gogs-repository"))


;;; Generated autoloads from forge-issue.el

(register-definition-prefixes "forge-issue" '("forge-"))


;;; Generated autoloads from forge-list.el

(autoload 'forge-list-topics "forge-list" "\
List topics of the current repository in a separate buffer.

(fn ID)" t)
(autoload 'forge-list-issues "forge-list" "\
List issues of the current repository in a separate buffer.

(fn ID)" t)
(autoload 'forge-list-labeled-issues "forge-list" "\
List issues of the current repository that have LABEL.
List them in a separate buffer.

(fn ID LABEL)" t)
(autoload 'forge-list-assigned-issues "forge-list" "\
List issues of the current repository that are assigned to you.
List them in a separate buffer.

(fn ID)" t)
(autoload 'forge-list-owned-issues "forge-list" "\
List open issues from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now." t)
(autoload 'forge-list-pullreqs "forge-list" "\
List pull-requests of the current repository in a separate buffer.

(fn ID)" t)
(autoload 'forge-list-labeled-pullreqs "forge-list" "\
List pull-requests of the current repository that have LABEL.
List them in a separate buffer.

(fn ID LABEL)" t)
(autoload 'forge-list-assigned-pullreqs "forge-list" "\
List pull-requests of the current repository that are assigned to you.
List them in a separate buffer.

(fn ID)" t)
(autoload 'forge-list-requested-reviews "forge-list" "\
List pull-requests of the current repository that are awaiting your review.
List them in a separate buffer.

(fn ID)" t)
(autoload 'forge-list-owned-pullreqs "forge-list" "\
List open pull-requests from all your Github repositories.
Options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now." t)
(autoload 'forge-list-authored-pullreqs "forge-list" "\
List open pull-requests of the current repository that are authored by you.
List them in a separate buffer.

(fn ID)" t)
(autoload 'forge-list-authored-issues "forge-list" "\
List open issues from the current repository that are authored by you.
List them in a separate buffer.

(fn ID)" t)
(autoload 'forge-list-notifications "forge-list" "\
List notifications." t)
(autoload 'forge-list-repositories "forge-list" "\
List known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database." t)
(autoload 'forge-list-owned-repositories "forge-list" "\
List your own known repositories in a separate buffer.
Here \"known\" means that an entry exists in the local database
and options `forge-owned-accounts' and `forge-owned-ignored'
controls which repositories are considered to be owned by you.
Only Github is supported for now." t)
(register-definition-prefixes "forge-list" '("forge-"))


;;; Generated autoloads from forge-notify.el

(register-definition-prefixes "forge-notify" '("forge-"))


;;; Generated autoloads from forge-post.el

(register-definition-prefixes "forge-post" '("forge-"))


;;; Generated autoloads from forge-pullreq.el

(register-definition-prefixes "forge-pullreq" '("forge-"))


;;; Generated autoloads from forge-repo.el

(register-definition-prefixes "forge-repo" '("forge-"))


;;; Generated autoloads from forge-revnote.el

(register-definition-prefixes "forge-revnote" '("forge-revnote"))


;;; Generated autoloads from forge-semi.el

(register-definition-prefixes "forge-semi" '("forge-"))


;;; Generated autoloads from forge-topic.el

(register-definition-prefixes "forge-topic" '("forge-"))

;;; End of scraped data

(provide 'forge-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; forge-autoloads.el ends here
