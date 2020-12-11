## Basic configs

This config includes:

* [`Helm`](https://github.com/emacs-helm/helm) with [helm-ag](https://github.com/syohex/emacs-helm-ag) for fast grep
* [`Company-mode`](https://github.com/company-mode/company-mode) with [company-irony](https://github.com/Sarcasm/company-irony) for c/c++ completion
* [`Elpy`](https://github.com/jorgenschaefer/elpy)
* [`Multiple cursors`](https://github.com/magnars/multiple-cursors.el)
* [`Projectile`](https://github.com/bbatsov/projectile)

## Keybindings

Aside from basic emacs keybindings here is a list of keybind defined in this config.

Keybinding | Description
-----------|-------------------------------------------------
<kbd>C-f</kbd> | select/mark current line
<kbd>C-d</kbd> | duplicate current line
<kbd>C-q</kbd> | kill buffer
<kbd>M-[UP-DOWN]</kbd> | scroll buffer
<kbd>M-S-[arrow]</kbd> | jump to window to the left/right/up/down

### Helm

Keybinding | Description
-----------|-------------------------------------------------
<kbd>C-x C-f</kbd> | `helm-find-files`: view and open files
<kbd>C-p</kbd> | `helm-buffer-list`: select/create buffer
<kbd>M-f</kbd> | `helm-occur`: search in current buffer
<kbd>M-R</kbd> | `helm-ag`: select a directory and do a recursive ag search

While in `helm-find-files` (<kbd>C-x C-f</kbd>):

Keybinding | Description
-----------|-------------------------------------------------
<kbd>M-R</kbd> | start `helm-ag` in selected directory
<kbd>C-p</kbd> | use `find` command in selected directory to search file

While in helm-mode, use <kbd>tab</kbd> to quickly see the candidate in another buffer.

### Projectile

Keybinding | Description
-----------|-------------------------------------------------
<kbd>C-c p f</kbd> | list all the file in the project
<kbd>C-c p F</kbd> | list all files from all opened project
<kbd>C-c p s s</kbd> | runs `helm-ag` (recursive grep) at the root of the project
<kbd>C-c p a</kbd> | switch between files with the same name but different extensions (for example .c/.h)
<kbd>C-c p r</kbd> | runs `query-replace` on all files in the project
<kbd>C-c p i</kbd> | regenerate project file cache (if many file path changed)

Theses are standard projectile keybindings, see the complete list [here](https://github.com/bbatsov/projectile#interactive-commands)

### Multiple Cursors

Keybinding | Description
-----------|-------------------------------------------------
<kbd>C-j</kbd> | add a new cursor to the next point that match the selected region.
<kbd>M-j</kbd> | add a new cursor to the next point that match the selected symbol.

### Virtualenv

In `.dir-locals.el`:
```
((python-mode
  (pyvenv-activate . "/path/to/venv")
  (flycheck-checker . python-flake8)))
```
