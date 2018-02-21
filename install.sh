#!/bin/bash

set -xe

mkdir -p ~/.emacs.d/straight/versions
ln -s lock-versions.el ~/.emacs.d/straight/versions/default.el
