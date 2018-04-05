#!/bin/bash

set -e

mkdir -p ~/.emacs.d/straight/versions
ln -fs `pwd`/lock-versions.el ~/.emacs.d/straight/versions/default.el

echo "Put the following in your ~/.emacs:"
echo "(load-file \"$(pwd)/init.el\")"
