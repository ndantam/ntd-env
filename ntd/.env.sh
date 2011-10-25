## .env.sh - Environment setup script
## by Neil Dantam
##
## This file hereby release into the public domain
## NO WARRANTY EXPRESSED OR IMPLIED

##########
## VARS ##
##########

export EDITOR=vim
export DOXRSYNCSSH="thebrain:~humanoids/public_html/doc"
export DEBEMAIL="ntd@gatech.edu"
export DEBFULLNAME="Neil Dantam"
export CDPATH="$CDPATH:$HOME:$HOME/src"
export PATH=~/bin:$PATH
export LD_LIBRARY_PATH=~/lib:/usr/local/lib:$LD_LIBRARY_PATH

#############
## ALIASES ##
#############
alias rscp="rsync --recursive --partial --perms --progress --times"
alias ec=emacsclient
alias lp-duplex='lp -o sides=two-sided-long-edge'
alias cu-thebrain="cu -lttyS0 --parity=none -s38400 --nostop"
alias cu-packbot="cu -lttyS0 --parity=none -s115200 --nostop"
alias mount-cc="sshfs killerbee4: ~/mnt/cc"
alias mount-acme="sshfs acme: ~/mnt/prism"
alias mount-ccwww="sshfs killerbee4:/net/www/grads/n/ndantam3 ~/www-cc"
alias mount-virjay="sshfs virjay: ~/mnt/virjay"
alias mount-brain="sshfs thebrain: ~/mnt/thebrain"
alias mount-humanoids-ssh="sshfs ntd@thebrain.cc.gt.atl.ga.us:/home/humanoids ~/mnt/humanoids"
alias mount-daneel="sshfs daneel: ~/mnt/daneel"
alias sshfs="sshfs -o readdir_ino,workaround=rename,reconnect,TCPKeepAlive=yes,ServerAliveInterval=60"



## Linux specific
if [ `uname` = Linux ]; then
    if [ "$TERM" = dumb -o "$EMACS" = t ] ; then
        alias ls="ls -F"
    else
        alias ls="ls -F --color=auto"
    fi
    # limit virtual memory to 1GB because linux sucks
    # (and I sometimes write memory leaks)
    ulimit -m 1024000
fi

## BSD
if [ `uname` = FreeBSD ]; then
    if [ "$TERM" = dumb -o "$EMACS" = t ] ; then
        alias ls="ls -F"
    else
        alias ls="ls -FG"
    fi
fi

###############
## FUNCTIONS ##
###############



ecdisp() {
    emacsclient -e '(shell-command  "xauth merge ~/.Xauthority")' && \
    emacsclient -e "(make-frame-on-display  \"$DISPLAY\")"
}

webcp () {
    pushd ~/www
    rsync --links --progress --recursive --times \
        index.html lisp.html android.html web img \
        killerbee3:www
    popd
}

rdebi () {
    scp "$2" "$1":/tmp/ && ssh $1 "sudo dpkg -i /tmp/$2; rm /tmp/$2"
}

make_common_dist () {
    make clean && make && make deb && pushdeb $(ls *.deb | sort | tail -n 1) && make dist
}


hmake() {
    pushd $HUMROOT/$1
    shift
    for i in $@; do make $i; done
    popd
}


pdfcat() {
    gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=- $@
}

gitrcp () {
    RDIR=`echo "$1" | sed -e 's/^\([^:]\+:\)//'`
    RHOST=`echo "$1" | sed -e 's/\(:.*\)$//'`
    RGRP=$2
    if [ -d .git ] ; then
	if [ -n "$RHOST" ] && [ -n "$RDIR" ] && [ -n "$RGRP" ] ; then
	    rsync -r .git/ "$RHOST:$RDIR" && \
		ssh "$RHOST" \
		"cd $RDIR && chmod -R g+w . && find . -type d -exec chmod g+s '{}' ';' && chgrp -R $RGRP . && git config core.bare true"
	else
	    echo "Usage: gitrcp REMOTE-HOST:REMOTE-DIR.git REMOTE-GROUP"
	fi
    else
	echo "No .git here"
    fi
}

bglisp () {
    if [ ! -d ~/.bglisp ] ; then
	mkdir ~/.bglisp  || return 1
    fi
    detachtty                                        \
	--pid-file ~/.bglisp/pid                     \
	--dribble-file ~/.bglisp/dribble             \
	~/.bglisp/socket                              \
	/usr/bin/env sbcl  --eval '(require :swank)' \
	--eval '(swank:create-server :dont-close t)' \
	|| return 1
}

##############
## PER-HOST ##
##############

## My
if [ "$HOST" = "leela"  ]; then
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

if [ "$HOST" = "leela-susan"  ]; then
    alias vi=vim
    alias pkg_add="sudo pkg_add -rv"
fi


## GT
if [ "$HOST" = "daneel"  ]; then
    export DISTSCPPATH=acme:tarballs
    alias kermit-sparky="kermit -l /dev/ttyS0 -b 115200 -8"
    export TEXINPUTS=:$HOME/src/ntd-latex:$TEXINPUTS
    # alias cu-sparky="cu -lttyS0 --parity=none -s9600 --nostop"
    alias openarena="(unset LIBGL_ALWAYS_INDIRECT & openarena); xrandr --output DVI-0 --right-of DVI-1"
    alias openarena="(unset LIBGL_ALWAYS_INDIRECT & openarena); xrandr --output DVI-0 --right-of DVI-1"
    alias mount-humaniods="sudo mount -t cifs -o username=ntd,acl,uid=ntd,gid=ntd //thebrain/humanoids /mnt/humanoids"
    export PATH=$PATH:~/src/other/depot_tools
fi

if [ "$HOST" = "chetter"  ]; then
    alias mount-humaniods="sudo mount -t cifs -o username=ntd,acl,uid=ntd,gid=ntd //thebrain/humanoids /mnt/humanoids"
fi

## LL
if [ "$HOST" = olivaw ]; then
    export ROS_ROOT=~/src/ros
    export ROS_PACKAGE_PATH=~/src/ros-pkg:~/src:~/src/indoor-packbot/software/python:
    export PATH=$ROS_ROOT/bin:$PATH
    export ROS_MASTER_URI=http://localhost:11311
    export PYTHONPATH=$PYTHONPATH:$ROS_ROOT/core/roslib/src:$ROS_ROOT/core/rospy/src
    export OCTAVE_PATH=$OCTAVE_PATH:$ROS_ROOT/core/experimental/rosoct/octave
fi

if [ "$HOST" = "SuperSloth" ] || [ "$HOST" = "babel"  ]; then
    export ROS_ROOT=~/src/ros
    export ROS_PACKAGE_PATH=~/src/ros-pkg:~/src/indoor-packbot
    export ROS_MASTER_URI=http://192.168.1.2:11311/
    export ROS_IP=192.168.1.2
    export PATH=$PATH:$ROS_ROOT/bin
    export PYTHONPATH=$PYTHONPATH:$ROS_ROOT/core/roslib/src
    export OCTAVE_PATH=$OCTAVE_PATH:$ROS_ROOT/core/experimental/rosoct
    ntd_ros_load_shell
fi

if [ "$HOST" = "babel" ]; then
    alias wifion="sudo ifdown eth0 && sudo ifup wlan0"
    alias wifioff="sudo ifdown wlan0 && sudo ifup eth0"
fi

# iRobot
if [ "$HOST" = "IRBT-2914" ]; then
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


#######
# ROS #
#######

ntd_ros_load_shell () {
    case $SHELL in
        /bin/zsh)
	    source $ROS_ROOT/tools/rosbash/roszsh
	    ;;
    esac
}

if [ -d "$HOME/ros/pkg" ]; then
    ROS_PACKAGE_PATH="$HOME/ros/pkg:$ROS_PACKAGE_PATH"
fi
