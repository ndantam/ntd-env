## .env.sh - Environment setup script
## by Neil Dantam
##
## This file hereby release into the public domain
## NO WARRANTY EXPRESSED OR IMPLIED

export EDITOR=vim

export CDPATH="$CDPATH:$HOME/src:$HOME/class"

if [ `uname` = Linux ]; then
    alias ls="ls -F --color=auto"
    alias ec=emacsclient
    alias sshfs="sshfs -o readdir_ino,workaround=rename,reconnect,TCPKeepAlive=yes,ServerAliveInterval=60"
  # limit virtual memory to 1GB because linux sucks (and I sometimes write memory leaks)
    ulimit -v 1024000
    alias ecdisp='emacsclient -e "(make-frame-on-display \"$DISPLAY\")"'
    alias lp-duplex='lp -o sides=two-sided-long-edge'
fi


alias rscp="rsync --inplace --partial --progress --times"

function make-common-dist {
    make clean && make && make deb && pushdeb $(ls *.deb | sort | tail -n 1) && make dist
}

if [ `hostname` = "daneel"  ]; then
  #export DOXPATH=~/mnt/prism/public_html/dox
    export DOXRSYNCSSH=acme:public_html/docs
    export DISTSCPPATH=acme:tarballs
    alias kermit-sparky="kermit -l /dev/ttyS0 -b 115200 -8"
    export TEXINPUTS=:$HOME/src/ntd-latex:$TEXINPUTS
    alias cu-sparky="cu -lttyS0 --parity=none -s115200 --nostop"
    alias cu-sparky="cu -lttyS0 --parity=none -s9600 --nostop"
    alias openarena="(unset LIBGL_ALWAYS_INDIRECT & openarena); xrandr --output DVI-0 --right-of DVI-1"
    alias openarena="(unset LIBGL_ALWAYS_INDIRECT & openarena); xrandr --output DVI-0 --right-of DVI-1"
    alias mount-cc="sshfs gaia: ~/mnt/cc"
    alias mount-acme="sshfs acme: ~/mnt/prism"
    alias mount-ccwww="sshfs gaia:/net/www/grads/n/ndantam3 ~/www-cc"
    alias mount-virjay="sshfs virjay: ~/mnt/virjay"
  #alias mount-humanoids="sshfs thebrain:/home/humanoids ~/mnt/humanoids"
    alias mount-humaniods="sudo mount -t cifs -o username=ntd,acl,uid=ntd,gid=ntd //thebrain/humanoids /mnt/humanoids"
    alias mount-brain="sshfs thebrain: ~/mnt/thebrain"
    export PATH=$PATH:~/src/other/depot_tools
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
    if [ ! -d /tmp/$TMPNAM ]; then
        mkdir /tmp/$TMPNAM
    fi
    ln -s /tmp/$TMPNAM ~/tmp
    touch ~/tmp/.ntd-tmp-flag
fi

function ntd_ros_load_shell {
    case $SHELL in
        /bin/zsh)
source $ROS_ROOT/tools/rosbash/roszsh
;;
esac
}

if [ `hostname` = "SuperSloth" -o `hostname` = "babel"  ]; then
    export ROS_ROOT=~/src/ros
    export ROS_PACKAGE_PATH=~/src/ros-pkg:~/src/indoor-packbot
    export ROS_MASTER_URI=http://192.168.1.2:11311/
    export ROS_IP=192.168.1.2
    export PATH=$PATH:$ROS_ROOT/bin
    export PYTHONPATH=$PYTHONPATH:$ROS_ROOT/core/roslib/src
    export OCTAVE_PATH=$OCTAVE_PATH:$ROS_ROOT/core/experimental/rosoct
    ntd_ros_load_shell
fi

if [ `hostname` = "babel" ]; then
    alias wifion="sudo ifdown eth0 && sudo ifup wlan0"
    alias wifioff="sudo ifdown wlan0 && sudo ifup eth0"
fi
export LD_LIBRARY_PATH=~/lib:/usr/local/lib:$LD_LIBRARY_PATH


if [ `hostname` = "vasilia" ]; then
    if [ -f "/mnt/scratch-ntd/.ntd-thebrain" ] ; then ; else
        sshfs thebrain:/scratch/ntd /mnt/scratch-ntd
    fi
fi
PATH=~/bin:$PATH
