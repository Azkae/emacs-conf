#!/bin/bash

set -e

mkdir -p ~/.emacs.d/straight/versions

lock_file=~/.emacs.d/straight/versions/default.el
if [ ! -f ~/.emacs.d/straight/versions/default.el ]; then
    echo "Setting up lock file.."
    ln -s `pwd`/lock-versions.el ~/.emacs.d/straight/versions/default.el
    echo
fi

echo "Put the following in your ~/.emacs:"
echo "(load-file \"$(pwd)/init.el\")"
