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

Make sure to install all versions of Fira Mono (Medium, Regular & Bold)

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

TODO:
- M-p for previous history is not working
- Show unsaved file differently in consult-buffers (see how vertico-multi colorize activated command modes)
- add https://github.com/karthink/consult-dir
- N should search in reverse always? but n should then always search forward..
- Fix M-B with M-R session showing nil
- Fix git timemachine keybind with meow
- Fix timemachine highlighting (use font locking?)
