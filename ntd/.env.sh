## .env.sh - Environment setup script
## by Neil Dantam
##
## This file hereby release into the public domain
## NO WARRANTY EXPRESSED OR IMPLIED

if [ `uname` = Linux ]; then
    alias ls="ls -F --color=auto"
    alias ec=emacsclient
    alias sshfs="sshfs -o readdir_ino,workaround=rename,reconnect,TCPKeepAlive=yes,ServerAliveInterval=60"
fi

if [ `hostname` = daneel  ]; then
    alias kermit-sparky="kermit -l /dev/ttyS0 -b 115200 -8"
    export TEXINPUTS=:$HOME/src/ntd-latex:$TEXINPUTS
    alias cu-sparky="cu -lttyS0 --parity=none -s115200 --nostop"
    alias openarena="(unset LIBGL_ALWAYS_INDIRECT & openarena); xrandr --output DVI-0 --right-of DVI-1"
fi

## LL WS env vars
if [ `hostname` = olivaw ]; then
    export ROS_ROOT=~/src/ros
    export ROS_PACKAGE_PATH=~/src/ros-pkg:~/src:~/src/indoor-packbot/software/python:
    export PATH=$ROS_ROOT/bin:$PATH
    export ROS_MASTER_URI=http://localhost:11311
    export PYTHONPATH=$PYTHONPATH:$ROS_ROOT/core/roslib/src:$ROS_ROOT/core/rospy/src
    export OCTAVE_PATH=$OCTAVE_PATH:$ROS_ROOT/core/experimental/rosoct/octave
fi

# Setup tmp
if [ -f ~/tmp/.ntd-tmp-flag ]; then
    true
else
    # I should somehow randomize this...
    TMPNAM=ntd-tmpdir
    mkdir /tmp/$TMPNAM
    ln -s /tmp/$TMPNAM ~/tmp
    touch ~/tmp/.ntd-tmp-flag
fi

export LD_LIBRARY_PATH=~/lib:/usr/local/lib:$LD_LIBRARY_PATH
