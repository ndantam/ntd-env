## .env.sh - Environment setup script
## by Neil Dantam
##
## This file hereby release into the public domain
## NO WARRANTY EXPRESSED OR IMPLIED

if [ `uname` = Linux ]; then
    alias ls="ls -F --color=auto"
    alias sshfs="sshfs -o readdir_ino,workaround=rename"
fi

if [ `hostname` = daneel ]; then
    alias kermit-sparky="kermit -l /dev/ttyUSB0 -b 115200 -8"
    export TEXINPUTS=:$HOME/src/ntd-latex:$TEXINPUTS
    alias cu-sparky="cu -lttyUSB0 --parity=none -s115200 --nostop"
    alias openarena="(unset LIBGL_ALWAYS_INDIRECT & openarena); xrandr --output DVI-0 --right-of DVI-1"
fi

export LD_LIBRARY_PATH=~/lib:/usr/local/lib:$LD_LIBRARY_PATH

