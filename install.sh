#!/bin/sh

# create parent directories
find ntd -type d | sed -e 's!^ntd/*!!' | xargs -I '{}' mkdir -pv ~/'{}'

# symlink
find ntd -type f | sed -e 's!^ntd/*!!' |
(
    while read fn; do
        if [ -e "$HOME/$fn" ]; then
            echo "$fn exists, won't replace"
        else
            echo ln -sv "$(pwd)/ntd/$fn"  "$HOME/$fn"
        fi
    done
)
