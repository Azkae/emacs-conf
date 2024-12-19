#!/bin/bash

set -e

mkdir -p ~/.emacs.d/straight/versions
ln -fs `pwd`/lock-versions.el ~/.emacs.d/straight/versions/default.el

line="(load-file \"$(pwd)/init.el\")"

if ! grep -qF "$line" ~/.emacs; then
    echo "$line" >> ~/.emacs
    echo "Added config to ~/.emacs"
else
    echo "Config already setup in ~/.emacs, skipping"
fi
