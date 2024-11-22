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

### Notes:
For *Iterm*:

In `Profiles` -> `Advanced` -> `Sementic History`:
Set to `Run command`, with value: `$PATH_TO/emacsclient -n \2 \1`

### How to reproduce bugs:
`emacs -Q -l ~/.emacs.d/straight/repos/straight.el/bootstrap.el`

```
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(when (string-equal system-type "darwin")
  (setq mac-command-modifier 'meta
  mac-option-modifier nil))

(use-package ...)
```

TODO Vertico / Consult:
- M-p for previous history is not working
- Show unsaved file differently in consult-buffers (see how vertico-multi colorize activated command modes)
- Fix "text in read" only on vertico-repeat

