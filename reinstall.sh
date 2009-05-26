#!/bin/bash

## File: relink.sh
## Author: Neil T. Dantam
##
## This script will fix up the hard links that may have been misplaced.
## It will make files in ~ point to ./ntd

relink() {
    relname=$1
    basename=`echo $relname | sed -e 's!^ntd/*!!'`
    link_cnt=`ls -l $relname | cut -d ' ' -f 2`
    if [ $link_cnt = 1 ]; then
        ln -iv  $relname ~/$basename
    fi
}

for f in `find ntd -type f`; do
    relink $f
done
