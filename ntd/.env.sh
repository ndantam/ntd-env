## .env.sh - Environment setup script
## by Neil Dantam
##
## This file hereby release into the public domain
## NO WARRANTY EXPRESSED OR IMPLIED

if [ `uname` = Linux ]; then
    alias ls="ls -F --color=auto"
    alias sshfs="sshfs -o readdir_ino,workaround=rename"
fi


export LD_LIBRARY_PATH=~/lib:/usr/local/lib:$LD_LIBRARY_PATH

