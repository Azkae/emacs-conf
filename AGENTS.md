# AGENTS.md

Personal Emacs configuration. Packages are declared with `use-package` +
[straight.el](https://github.com/radian-software/straight.el).

## Layout

- `init.el` — bootstraps straight, then `require`s each `conf-*.el` module in order.
- `conf-*.el` — one module per topic (editing, completion, magit, python, …).
- `lock-versions.el` — pinned package revisions.
- `custom.el` — `custom-set-variables` output; generated, don't hand-edit.
- `min.el` — minimal config for bug repros: `emacs -Q -l ~/emacs-conf/min.el`.

## Reading package source

Third-party package sources are checked out locally under
`~/.emacs.d/straight/repos/`. Read them there instead of guessing at an API or
fetching from the web.

## Conventions

- Each `conf-*.el` starts with a `;;; Commentary:` header and ends with
  `(provide 'conf-<name>)`; keep lexical binding on.
- Add a new module as `conf-<topic>.el` plus a `require` in `init.el`.
