#!/bin/bash

set -e

mkdir -p ~/.emacs.d/straight/versions
ln -fs `pwd`/lock-versions.el ~/.emacs.d/straight/versions/default.el

write-to-file-maybe() {
    local line="$1"
    local path="$2"

    if ! grep -qF "$line" "$path"; then
        echo "$line" >> "$path"
        echo "Added config to $path"
    else
        echo "Config already present in $path, skipping"
    fi
}

write-to-file-maybe "(load-file \"$(pwd)/init.el\")" ~/.emacs

write-to-file-maybe "pinentry-program $(which pinentry-mac)" \
                    ~/.gnupg/gpg-agent.conf
