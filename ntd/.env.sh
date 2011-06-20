## .env.sh - Environment setup script
## by Neil Dantam
##
## This file hereby release into the public domain
## NO WARRANTY EXPRESSED OR IMPLIED

export EDITOR=vim

export CDPATH="$CDPATH:$HOME:$HOME/src"
if [ `uname` = Linux ]; then
    if [ "$TERM" = dumb -o "$EMACS" = t ] ; then
        alias ls="ls -F"
    else
        alias ls="ls -F --color=auto"
    fi
    alias clbuild="~/src/clbuild/clbuild"
    alias ec=emacsclient
    alias sshfs="sshfs -o readdir_ino,workaround=rename,reconnect,TCPKeepAlive=yes,ServerAliveInterval=60"
  # limit virtual memory to 1GB because linux sucks (and I sometimes write memory leaks)
    ulimit -m 1024000
    alias ecdisp='emacsclient -e "(make-frame-on-display \"$DISPLAY\")"'
    alias lp-duplex='lp -o sides=two-sided-long-edge'
    alias cu-thebrain="cu -lttyS0 --parity=none -s38400 --nostop"
    alias cu-packbot="cu -lttyS0 --parity=none -s115200 --nostop"
    alias mount-cc="sshfs gaia: ~/mnt/cc"
    alias mount-acme="sshfs acme: ~/mnt/prism"
    alias mount-ccwww="sshfs gaia:/net/www/grads/n/ndantam3 ~/www-cc"
    alias mount-virjay="sshfs virjay: ~/mnt/virjay"
    alias mount-brain="sshfs thebrain: ~/mnt/thebrain"
    alias mount-humanoids-ssh="sshfs ntd@thebrain.cc.gt.atl.ga.us:/home/humanoids ~/mnt/humanoids"
    alias clbuild="~/src/clbuild/clbuild"
    alias mount-daneel="sshfs daneel: ~/mnt/daneel"
    alias KILL="kill -9"
    alias sshsock="ssh -v -D1080 daneel"
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
  #alias cu-sparky="cu -lttyS0 --parity=none -s9600 --nostop"
    alias openarena="(unset LIBGL_ALWAYS_INDIRECT & openarena); xrandr --output DVI-0 --right-of DVI-1"
    alias openarena="(unset LIBGL_ALWAYS_INDIRECT & openarena); xrandr --output DVI-0 --right-of DVI-1"
    alias mount-humaniods="sudo mount -t cifs -o username=ntd,acl,uid=ntd,gid=ntd //thebrain/humanoids /mnt/humanoids"
    export PATH=$PATH:~/src/other/depot_tools
fi

if [ `hostname` = "chetter"  ]; then
    alias mount-humaniods="sudo mount -t cifs -o username=ntd,acl,uid=ntd,gid=ntd //thebrain/humanoids /mnt/humanoids"
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



# iRobot
if [ `hostname` = "IRBT-2914" ]; then
    ulimit -s 2048
    aware_env() {
        export AWAREPM_LOCAL_CACHE=~/src/irobot/cache
        export AWAREPM_REMOTE_CACHES="http://prodfiles.hq.irobot.com/software-releases/Aware2|http://prodfiles.hq.irobot.com/software-releases/Research|http://prodfiles.hq.irobot.com/software-releases/PackBot"
        alias awarepm=/opt/awarepm_280/aware-build/awarepm.py
        source /opt/irobot/aware-build/aware2Shell.sh > /dev/null
        ST_FLAG=${ST_FLAG}"(AWR)"
    }

    ros_env() {
        export ROS_ROOT=~/src/ros/ros
        export PATH=$ROS_ROOT/bin:$PATH
        export PYTHONPATH=$ROS_ROOT/core/roslib/src:$PYTHONPATH
        if [ ! "$ROS_MASTER_URI" ]; then
            export ROS_MASTER_URI=http://localhost:11311
        fi
        export ROS_PACKAGE_PATH=~/src/ros/stacks:~/src/research/projects/ros_pkg:~/src/ros/ros_experimental/tags/boxturtle
        source $ROS_ROOT/tools/rosbash/roszsh
        ST_FLAG=${ST_FLAG}"(ROS)"
    }
fi


if [ `hostname` = "leela"  ]; then
    fanlevel() {
        echo level $1 | sudo tee /proc/acpi/ibm/fan
    }
    alias ff32="schroot -p iceweasel -- -P 32 -no-remote"
    alias fanlow='fanlevel 2'
    alias fanmed='fanlevel 4'
    alias fanmax='fanlevel 7'
    alias fanauto='fanlevel auto'
    alias fandis='fanlevel disengaged'
    export DOXRSYNCSSH=acme:public_html/docs
fi
PATH=~/bin:$PATH

hmake() {
    pushd $HUMROOT/$1
    shift
    for i in $@; do make $i; done
    popd
}
