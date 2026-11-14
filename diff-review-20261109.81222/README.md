# diff-review

Review a git diff in Emacs the way a pull request page does. A review is a
small JSON file: open it and you get the counts and the changed files as a
tree; pick a file and you get the real file in its own major mode, so `xref`,
the language server and every editing command work while you review. Overlays
add what a diff shows: removed lines inline above where they were, a tint on
added lines, and a focus mode that folds the unchanged lines. Mark files
viewed, leave comment threads on lines, approve or request changes. The file
is plain JSON, so an agent can read and write the same review with `jq`.

[![Watch the demo](https://img.youtube.com/vi/ewVE_kd48m0/maxresdefault.jpg)](https://www.youtube.com/watch?v=ewVE_kd48m0)

## Install

```elisp
(use-package diff-review
  :vc (:url "https://github.com/xiaoxinghu/diff-review" :rev :newest))
```

Needs Emacs 29.1 and `magit-section`. The `:vc` keyword is Emacs 30's; on
29, `M-x package-vc-install` with the URL.

## Use

`M-x diff-review-branch` reviews a branch: pick one that another worktree has
checked out, or type any, and the review of its commits against the base
opens. The file is `review/<branch>.json` under the `.git` every worktree
shares, so main's clone and the branch's worktree open the same review and git
never commits it. The review follows the branch: after a commit, `g` rescans,
viewed marks clear for the files that changed, and threads follow their lines
or read outdated.

`M-x diff-review-find-file` visits a review file anywhere, creating it when it
does not exist. Such a review is of the worktree's uncommitted changes against
the base. From Lisp, `(diff-review-find-file FILE ROOT BASE HEAD)` reviews
HEAD against BASE at ROOT, for any commit range.

The base is `diff-review-default-base`: by default what `origin/HEAD` points
at, else the first of `init.defaultBranch`, `main` and `master` that exists.
The file records the base it got, so changing the setting leaves existing
reviews alone.

The files of a branch review are real files too. One that a worktree has
checked out at that commit opens there; commits no worktree has are checked
out read-only under `review/worktrees/<hash>` in the shared `.git`, a
throwaway worktree the review removes when it ends.

In the overview and the sidebar:

| Key       | Command                                  |
|-----------|------------------------------------------|
| `RET`     | show the file at point, or fold a folder |
| `n` `p`   | next, previous file                      |
| `u`       | next unviewed file                       |
| `v`       | toggle the viewed mark                   |
| `g`       | rescan the diff                          |
| `t`       | show or hide the sidebar                 |
| `A`       | approve the head                         |
| `C`       | request changes to the head              |
| `C-c C-c` | the file as its JSON, and back           |
| `q`       | end the review                           |

In a file, the commands sit under one prefix, `C-c _` by default, so the
major mode keeps its own keys. With `repeat-mode` the prefix is typed once:
`C-c _ n n v` walks two hunks and marks the file viewed. `M-g n` and `M-g p`
walk the hunks too, with no prefix.

| Key                 | Command                                             |
|---------------------|-----------------------------------------------------|
| `C-c _ n` `C-c _ p` | next, previous hunk, across files                   |
| `C-c _ v`           | mark viewed and show the next unviewed              |
| `C-c _ z`           | fold the unchanged lines                            |
| `C-c _ d`           | show or hide the removed lines                      |
| `C-c _ c`           | comment on the region's lines, or the line at point |
| `C-c _ r`           | reply to the thread at point                        |
| `C-c _ x`           | resolve the thread at point, or reopen it           |
| `C-c _ D`           | delete the thread at point                          |
| `C-c _ o`           | open every thread, or only the one at point again   |
| `C-c _ N` `C-c _ P` | next, previous thread, across files                 |
| `C-c _ t`           | show or hide the sidebar                            |
| `C-c _ l`           | back to the overview                                |
| `C-c _ q`           | end the review                                      |

`C-c _ C-h` lists the file commands, `C-h m` the whole mode, and `?` does the
same in the overview and the sidebar. A file's header line ends with hints
for the keys you have bound.

Every worktree's branch is a candidate pull request, and magit-status can
list them: the branch, its commits ahead of the base and, once it has a
review, the viewed count, open threads and verdict, `RET` opening the review.

`M-x diff-review-magit-commits` reviews the commits selected in any magit
buffer, the oldest included, or the commit at point. Its file is
`review/OLD..NEW.json` under the common git directory, named by their
abbreviated hashes, so the same selection reopens with its marks from any
worktree; a branch's own review stays `diff-review-branch`'s.

Both are opt-in, behind one switch. It loads no magit, so it can go
anywhere in your init; waiting for magit keeps diff-review itself out of
startup too, since the mode's autoload loads the package:

```elisp
(with-eval-after-load 'magit (diff-review-magit-mode))
```

The section joins magit-status, and `d v`, "Review" in magit's diff menu,
reviews the selection. `M-x diff-review-magit-mode` again takes both out.
For one half alone:

```elisp
(with-eval-after-load 'magit
  (add-hook 'magit-status-sections-hook #'diff-review-magit-insert-worktrees t)
  (transient-append-suffix 'magit-diff 'magit-stash-show
    '("v" "Review" diff-review-magit-commits)))
```

## Customize

Every setting has a conventional default; these are the ones worth knowing:

```elisp
(use-package diff-review
  :vc (:url "https://github.com/xiaoxinghu/diff-review" :rev :newest)
  :custom
  (diff-review-command-prefix "C-c r") ; the file commands' prefix; nil binds none
  (diff-review-sidebar-side 'right)    ; or 'left
  (diff-review-sidebar-width 40)       ; columns a review starts with; drag to resize
  (diff-review-sidebar nil)            ; start with the sidebar hidden; `t' shows it
  (diff-review-context 3)              ; unchanged lines kept around a hunk in focus mode
  (diff-review-inline-removed t)       ; removed lines inline; `C-c _ d' toggles per review
  (diff-review-fringe 'left)           ; hunk bars in the left or right fringe, or nil
  (diff-review-author "xiaoxing")      ; the name on your comments; nil is git's user.name
  :hook
  ((diff-review-mode diff-review-sidebar-mode) . hl-line-mode)
  :init
  (with-eval-after-load 'magit        ; the worktrees in magit-status, and `d v'
    (diff-review-magit-mode)))
```

Everything drawn has a face in `M-x customize-group diff-review`. Added and
removed lines take the backgrounds of `diff-added` and `diff-removed` unless
`diff-review-added` and `diff-review-removed` set their own, and
`diff-review-tint-added` or `diff-review-tint-removed` turn a tint off,
leaving the fringe bar. A thread is `diff-review-comment`, its lines while
open `diff-review-comment-range`. `diff-review-file-header` can leave the
buffer's header line alone or name a function that makes one, and
`diff-review-file-header-keys` drops the key hints.

## Evil

With Evil, the overview and the sidebar start in normal state. Normal state
owns `n`, `p`, `g` and `TAB`, so there `gj` `gk` walk the files, `gr`
rescans and `TAB` folds a folder; `RET`, `u`, `v`, `t`, `A`, `C` and `q` are
as above. In a file only the prefix keys are bound. Set `diff-review-evil-keys`
to nil before loading to leave Evil untouched.

For vim keys in a file:

```elisp
(with-eval-after-load 'evil
  (evil-define-minor-mode-key 'normal 'diff-review-file-mode
    "]c" #'diff-review-next-hunk
    "[c" #'diff-review-previous-hunk
    "zv" #'diff-review-viewed-and-next
    "zf" #'diff-review-toggle-focus
    "zd" #'diff-review-toggle-inline-removed
    "gc" #'diff-review-comment
    "gl" #'diff-review-overview
    "gq" #'diff-review-quit))
```

It shadows `gq` (`evil-fill-and-move`), and `zf` `zd` `gl` `gc` where
evil-lion, vimish-fold, origami or evil-commentary bind them; pick others if
you use those.

## The review file

The file is for agents as much as for Emacs, so it is plain. Keys Emacs does
not know are kept as they are, so an agent can add a severity or a confidence
to a comment.

```json
{"format": "diff-review/1",
 "root": "/path/to/worktree/",
 "base": "main",
 "head": "feature/login",
 "viewed": {"src/a.ts": "<blob hash>"},
 "comments": [
   {"id": "k3f9a1",
    "path": "src/a.ts",
    "lines": [12, 15],
    "blob": "<blob hash of src/a.ts when this was written>",
    "author": "xiaoxing",
    "at": "2026-11-06T12:40:11+13:00",
    "body": "This retries forever when the socket is gone.",
    "replies": [
      {"author": "claude", "at": "2026-11-06T12:58:02+13:00", "body": "Capped at 5 in 3b1c0e2."}],
    "resolved": true},
   {"id": "77c02d", "author": "review", "at": "2026-11-06T13:01:40+13:00",
    "body": "Overall: correct, one nit above."}],
 "verdict": {"state": "changes", "head": "<full hash of the tip>",
             "by": "claude", "at": "2026-11-07T10:02:00+13:00"}}
```

- `head` is a branch the review follows, or a commit for a fixed range;
  without it the review is of the worktree.
- `viewed` maps a path to the blob hash it was viewed at, so the mark clears
  when the file changes.
- A comment is a thread: the first message carries `path` and `lines`, first
  and last inclusive, counted in the content `blob` names, so the thread
  follows its lines through `git diff` of the two blobs and reads outdated
  when a change landed inside them. `replies` is a flat list of `{author, at,
  body}`. A comment with no `path` is on the review as a whole; a `path`
  outside the diff joins the tree as an unchanged file.
- `verdict` is the reviewer's last word on one commit: `changes` or
  `approved`, stale once the branch moves past `head`. It says whose turn it
  is: the reviewer's with no verdict or a stale one, the author's after
  `changes`, the merge's once `approved`.

Emacs rereads the file before each of its own writes and whenever it changes
on disk, so writing beside an open review loses nothing. To comment as an
agent, append one object; the blob is `git hash-object -w <path>` for a
worktree review, `git rev-parse <head>:<path>` for a branch or commit:

```sh
f=$(git rev-parse --git-common-dir)/review/feature.json
jq --arg blob "$(git rev-parse feature:src/a.ts)" --arg at "$(date -Iseconds)" \
  '.comments += [{id: "a1b2c3", path: "src/a.ts", lines: [12, 15], blob: $blob,
                  author: "claude", at: $at, body: "Retries forever.", replies: []}]' \
  "$f" > "$f.new" && mv "$f.new" "$f"
```

To reply, push to the thread's `replies`; to mark it fixed, set `resolved`.
To give a verdict, and to read the turn:

```sh
jq --arg head "$(git rev-parse feature)" --arg at "$(date -Iseconds)" \
  '.verdict = {state: "changes", head: $head, by: "claude", at: $at}' \
  "$f" > "$f.new" && mv "$f.new" "$f"

jq -r --arg tip "$(git rev-parse feature)" \
  'if .verdict.head != $tip then "reviewer"
   elif .verdict.state == "changes" then "author"
   elif .verdict.state == "approved" then "merge" else "reviewer" end' "$f"
```

## Check

`make check` byte-compiles without warnings, runs checkdoc and the ERT tests.
The first run fetches magit-section and magit from MELPA into `.deps/`; it
reads no Emacs config. `EMACS=...` picks the Emacs.
