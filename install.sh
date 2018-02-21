#!/bin/bash

set -xe

mkdir -p ~/.emacs.d/straight/versions
ln -s `pwd`/lock-versions.el ~/.emacs.d/straight/versions/default.el
