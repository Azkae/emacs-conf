# Emacs config

This is my personnal Emacs configuration.
Using [`Straight`](https://github.com/radian-software/straight.el) to declare packages.

## Install

Install emacs (MacOS):
```
brew install libtool cmake pass pinentry-mac

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
`emacs -Q -l ~/emacs-conf/min.el`

TODO:
- meow: explore N for reverse search
- vertico-repeat with project-switch-project
- disallow recursive M-o
- change the region color to blue
