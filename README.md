# Emacs config

This is my personnal Emacs configuration.
Using [`Straight`](https://github.com/radian-software/straight.el) to declare packages.

## Install

Install emacs:
```
brew install libtool cmake

brew tap d12frosted/emacs-plus
brew install emacs-plus --with-native-comp
./install.sh
# Put the following in your ~/.emacs:
# (load-file "$(pwd)/init.el")
```

Run
```
M-x nerd-icons-install-fonts
M-x treesit-auto-install-all
```

MacOS:
```
System Settings -> Keyboard shortcuts -> Input sources:
Disable all ^Space to fix ctrl-space
```

## Keybindings

Aside from basic emacs keybindings here is a list of keybind defined in this config.

Keybinding | Description
-----------|-------------------------------------------------
<kbd>C-f</kbd> | select/mark current line
<kbd>C-d</kbd> | duplicate current line
<kbd>C-q</kbd> | kill buffer
<kbd>M-[UP-DOWN]</kbd> | scroll buffer
<kbd>C-M-[arrow]</kbd> | move the window to the left/right/up/down
<kbd>M-q-[arrow]</kbd> | jump to window to the left/right/up/down

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

### Notes:
For *Iterm*:

In `Profiles` -> `Advanced` -> `Sementic History`:
Set to `Run command`, with value: `$PATH_TO/emacsclient -n \2 \1`

On emacs version 28 and lower, apply helm patch for OSX:
```
cd ~/.emacs.d/straight/repos/helm
git apply ~/emacs-conf/helm.patch
```

To fix lldb bug on MacOS Sonoma
```
rm ~/.emacs.d/debug-adapters/codelldb/extension/lldb/bin/debugserver
```

### How to reproduce bugs:
`emacs -Q -l ~/.emacs.d/straight/repos/straight.el/bootstrap.el`

```
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(global-set-key (kbd "C-f") "\C-a\C-a\C-@\C-e")
(global-set-key (kbd "C-q") 'kill-this-buffer)


(when (string-equal system-type "darwin")
  (setq mac-command-modifier 'meta
  mac-option-modifier nil))

(use-package ...)
```

TODO:
- try without fork on helm-ag: https://github.com/Azkae/emacs-helm-ag/compare/master...emacsorphanage:helm-ag:master
  they added finished status on modeline

- try consult & https://github.com/mhayashi1120/Emacs-wgrep to replace helm & helm-ag?
  see embark: https://github.com/oantolin/embark
  > If you use the grepping commands from the Consult package, consult-grep, consult-git-grep or consult-ripgrep, then you should install the embark-consult package, which adds support for exporting a list of grep results to an honest grep-mode buffer, on which you can even use wgrep if you wish.
  ^ wgrep also works with helm

Vertico / Consult - Big TODO:
- M-p for previous history is not working
- Fix copy-face not working at launch
- Show unsaved file differently in consult-buffers (see how vertico-multi colorize activated command modes)

Not sure that need fixing, vertico default might be better, we just need a way so embark-act on the current file?
- Fix find-file file sorting?
- Fix find-file not selecting current file by default?

Embark TODO:
- Magit
- VTerm
