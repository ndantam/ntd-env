## .env.sh - Environment setup script
## by Neil Dantam
##
## This file hereby release into the public domain
## NO WARRANTY EXPRESSED OR IMPLIED

##########
## VARS ##
##########

export EDITOR=vim
export DEBEMAIL="ndantam@mines.edu"
export DEBFULLNAME="Neil Dantam"
export CDPATH="$CDPATH:$HOME:$HOME/git"
export PATH=~/bin:~/.local/bin:$PATH
export LD_LIBRARY_PATH=~/lib:/usr/local/lib:$LD_LIBRARY_PATH
export NPROC=`nproc`

# PYTHONPATH
if sh -c "ls -d /usr/local/lib/python* 2> /dev/null  > /dev/null"; then
    PYTHONPATH_TMP=$(sh -c "ls -d /usr/local/lib/python*" | sort -Vr | head -n 1)
    if [ -n ${PYTHON_PATH_TMP} ] ; then
        export PYTHONPATH="${PYTHONPATH}:${PYTHONPATH_TMP}/site-packages"
    fi
    unset PYTHONPATH_TMP
fi

#############
## ALIASES ##
#############
alias rscp="rsync --recursive --partial --perms --progress --times --links"
alias lp-duplex='lp -o sides=two-sided-long-edge'
alias sshfs="sshfs -o readdir_ino,workaround=rename,reconnect,TCPKeepAlive=yes,ServerAliveInterval=60"

alias ll="ls -lh"
alias la="ls -a"
alias lal="ls -alh"

#alias npr="vlc -I dummy 'https://stream.houstonpublicmedia.org/news-aac-128.m3u'"
alias npr="vlc -I dummy 'http://livestream.cprnetwork.org/pls/live_newsinfo_aac.pls'"


if [ "$NPROC" -ge 2 ]; then
    # Use as many threads as CPU cores
    export XZ_OPT="--threads=0"

    alias xz="xz --threads=0"
    alias txz="tar -I 'xz -T 0'"

    if which lbzip2 > /dev/null; then
        alias bzip2="lbzip2"
        alias bunzip2="lbuzip2"
        alias tbz2="tar -I 'lbzip2'"
    else
        alias tbz2="tar -j"
    fi

else
    alias txz="tar -J"
    alias tbz2="tar -j"
fi

## Linux specific
if [ `uname` = Linux ]; then
    if [ "$TERM" = dumb -o "$EMACS" = t ] ; then
        alias ls="ls -F"
    else
        alias ls="ls -F --color=auto"
    fi
    # limit RSS to 4GB because linux sucks
    # (and I sometimes write memory leaks)
    ulimit -m 4096000

    # Explicitly resize serial consoles
    case "$TTY" in
        /dev/ttyS*)
            resize
            ;;
    esac
fi

## BSD
if [ `uname` = FreeBSD ]; then
    if [ "$TERM" = dumb -o "$EMACS" = t ] ; then
        alias ls="ls -F"
    else
        alias ls="ls -FG"
    fi
fi

fix_pa_volume () {
    for sink in ` pactl list sinks short | sed -e 's/[[:alnum:]]\+[[:blank:]]\+\([[:graph:]]\+\).*/\1/'`; do
        pactl set-sink-volume $sink '100%'
    done
}

########
## X11 #
########

if [ -n "$DISPLAY" ] ; then
    # TODO: compute height

    x11_height() {
        echo $((1200-75))
    }
    x11_width () {
        echo 1920
    }
    x11_xoff () {
        echo 1920
    }
fi

####################
## Local Packages ##
####################

MY_LOCAL=''

if [ -d "$HOME/local" -o -L "$HOME/local" ]; then
    MY_LOCAL="$HOME/local"
fi

if [ -n "$MY_LOCAL" ]; then
    local_config () {
        echo "--prefix=$MY_LOCAL"  "CPPFLAGS=-I$MY_LOCAL/include" "LDFLAGS=-L$MY_LOCAL/lib"
    }
    export PATH="$MY_LOCAL/bin:$PATH"
    export LD_LIBRARY_PATH="$MY_LOCAL/lib:$LD_LIBRARY_PATH"
fi

###############
## FUNCTIONS ##
###############

start_emacs () {
    ## Emacs, no slave to X sessions, gets its own xauthority
    # XAUTHORITY=/tmp/xauth.emacs:$USER@$HOST emacs --daemon
    systemctl --user start emacs.service
}

stop_emacs () {
    systemctl --user stop emacs.service
}

pdfcat() {
    gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=- $@
}
pdfcompress() {
    F=`tempfile`
    gs \
        -dCompatibilityLevel=1.7 \
        -dDetectDuplicateImages=true \
        -dNOPAUSE \
        -dBATCH \
        -dQUIET \
        -dCompressFonts=true \
        -dPrinted=false \
        -dPDFSETTINGS=/prepress \
        -sDEVICE=pdfwrite \
        -sOutputFile="$F" "$1"
    if [ `stat --format="%s" "$1"` -gt `stat --format="%s" "$F"` ]; then \
        mv "$F" "$1"
    else
        rm "$F"
    fi
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

cl_core() {
    mkdir -p "$HOME/.cache/common-lisp/"
    sbcl --script <<EOF
;; Load userinit
(load "$HOME/.sbclrc")
;; Load common libraries
(ql:quickload :sb-posix)
(ql:quickload :swank)
(ql:quickload :cffi)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :sb-sprof)
(ql:quickload :clpython)
(sb-ext:save-lisp-and-die "$HOME/.cache/common-lisp/sbcl.core"
                          :executable t
                          :compression t
                          :save-runtime-options nil)

EOF
}

cl_run() {
    "$HOME/.cache/common-lisp/sbcl.core" \
        --dynamic-space-size 8GB \
        --control-stack-size 32MB \
        --eval '(asdf:clear-source-registry)' \
        $@

}

# cl-run() {
#     lisp-local \
#         --eval '(asdf:clear-source-registry)' \
#         --eval '(sb-ext:enable-debugger)' \
#         --eval '(swank:swank-require :swank-arglists)' \
#         --eval '(swank:create-server :dont-close t)' \
#         $@

#         #--eval '(setq swank:*communication-style* :fd-handler)' \
# }

cpugov () {
    i=0
    while [ $i -lt `grep "^processor" /proc/cpuinfo | wc -l` ]; do
        sudo cpufreq-set -c $i -g $1 || return
        i=$(($i+1))
    done
}

# input-file, output-file, video-bitrate (kbps), audio-bitrate (kbps)

trans264() {
    ffmpeg -i $1 -pass 1 -vcodec libx264 -b:v $((1024 * $3)) -an -f rawvideo -y /dev/null
    ffmpeg -i $1 -pass 2 -vcodec libx264 -b:v $((1024 * $3)) -b:a $((1024 * $4)) -y $2
}

trans264n() {
    avconv -i $1 -pass 1 -codec:v libx264 -b:v ${3}k -an -f rawvideo -y /dev/null
    avconv -i $1 -pass 2 -codec:v libx264 -b:v ${3}k -an -y $2
}

trans264mn() {
    for i in 1 2; do
        mencoder $1 \
            -ovc x264 -x264encopts bitrate=$3:pass=$i \
            -nosound \
            -o $2
    done;
}

# input-file, output-file, video-bitrate (kbps), audio-quality (1-10)
transvp8_1() {
    avconv -i $1 -pass 1 -codec:v libvpx -b:v ${3}k -an -f rawvideo -y /dev/null
}
transvp8_2() {
    avconv -i $1 -pass 2 -codec:v libvpx -b:v ${3}k -acodec libvorbis -qscale:a $4 -y $2
}

transvp8_2n() {
    avconv -i $1 -pass 2 -codec:v libvpx -b:v ${3}k -an -y $2
}

transvp8() {
    transvp8_1 $@
    transvp8_2 $@
}

transvp8n() {
    transvp8_1 $@
    transvp8_2n $@
}

transtheoran() {
    avconv -i $1 -pass 1 -codec:v libtheora -b:v ${3}k -an -f rawvideo -y /dev/null
    avconv -i $1 -pass 2 -codec:v libtheora -b:v ${3}k -an  -y $2
}


# cloudbk() {
#     if [ "$HOST" != farnsworth ] ; then
#         >&2 echo "Refusing to copy from current host"
#         return 1;
#     fi
#     rclone sync --progress --dry-run \
#            "$HOME/pro" \
#            --filter='- teaching/pl/video/*/out/**' \
#            --filter='- teaching/pl/video/*/rec/**' \
#            --filter='- teaching/pl/video/*/clip/**' \
#            --filter='+ teaching/pl/video/basics/**' \
#            --filter='- *' \
#            onedrive:pro
# }


#################
## Compilation ##
#################

if [ -d /usr/lib/ccache/ ] ; then
    # Add ccache to path
    PATH="/usr/lib/ccache:$PATH"

    # Store cache in /tmp
    #export CCACHE_DIR="/tmp/ntd-cache/ccache/cache"
    #export CCACHE_TEMPDIR="/tmp/ntd-cache/ccache/tmp"
    #export CCACHE_COMPRESS="yes"

    if [ -d "${HOME}/scratch/.ccache" ]; then
        export CCACHE_DIR="${HOME}/scratch/.ccache"
    elif [ -d "${HOME}/cache/.ccache" ]; then
        export CCACHE_DIR="${HOME}/ccache/.ccache"
    elif [ -d "${HOME}/git/.ccache" ]; then
        export CCACHE_DIR="${HOME}/git/.ccache"
    else
        export CCACHE_DIR="${HOME}/.cache/.ccache"
    fi

    ## hardlinks break make, automake, etc. Do not hardlink!
    #export CCACHE_HARDLINK="yes"
    export CCACHE_NOHARDLINK="yes"
    export CCACHE_BASEDIR="${HOME}/git"

    alias debuild="debuild  --prepend-path=/usr/lib/ccache"

    # Create cache directories
    if [ ! -d "$CCACHE_DIR" ]; then
        mkdir -p "$CCACHE_DIR"
        # Limit cache size
        ccache -M 1G
    fi
fi

#if [ "$HOST" = "daneel"  ]; then
    #export DISTCC_HOSTS="donovan talos jander vasilia donovan localhost"
    #export CCACHE_PREFIX="distcc"
#fi

###########
## EMSDK ##
###########

if [ -d /usr/local/emsdk_portable/ ] ; then
    source /usr/local/emsdk_portable/emsdk_env.sh > /dev/null
fi

##############
## PER-HOST ##
##############

# if [ "$HOST" = "leela-susan"  ]; then
#     alias vi=vim
#     alias pkg_add="sudo pkg_add -rv"
# fi
if [ `nproc` -le 2 ]; then
    alias make="make -j $((2*`nproc`))"
else
    alias make="make -j $((3*`nproc`/2))"
fi

leela_dock () {
    xrandr --output VGA-1 --off
    xrandr --output LVDS-1 --auto \
           --output DP-3 --auto --right-of LVDS-1 \
           --output DP-2 --auto --right-of DP-3

    setxkbmap -option "ctrl:nocaps"
    xmodmap ~/.xmodmap
}

if [ "$HOST" = "apollo"  ]; then
    alias make="make -j 12"
fi

leela_undock () {
    xrandr --output DP-2 --off \
           --output DP-3 --off \
           --output HDMI-1 --off
    xrandr --output LVDS-1 --auto
}

## GT
if [ "$HOST" = "daneel"  ]; then
    export TEXINPUTS=:$HOME/src/ntd-latex:$TEXINPUTS
    # alias cu-sparky="cu -lttyS0 --parity=none -s9600 --nostop"
    alias openarena="(unset LIBGL_ALWAYS_INDIRECT & openarena); xrandr --output DVI-0 --right-of DVI-1"
    alias openarena="(unset LIBGL_ALWAYS_INDIRECT & openarena); xrandr --output DVI-0 --right-of DVI-1"
    alias make="make -j 5"
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

# RICE

#######
# ROS #
#######

ros_src_file() {
    if test -d $1; then
        case $SHELL in
            *zsh)
                source $1/setup.zsh
                ;;
            *bash)
                source $1/setup.bash
                ;;
            *)
                source $1/setup.sh
                ;;
        esac
    fi
}

ros_env() {

    if test "x$ROS_ROOT" = x; then
        if test -d /opt/ros/lunar; then
            export ROS_ROOT=/opt/ros/lunar
        elif test -d /opt/ros/kinetic; then
            export ROS_ROOT=/opt/ros/kinetic
        elif test -d /opt/ros/indigo; then
            export ROS_ROOT=/opt/ros/indigo
        else
            echo "No ROS_ROOT"
        fi
    fi
    export PATH=$ROS_ROOT/bin:$PATH
    export PYTHONPATH=$ROS_ROOT/core/roslib/src:$PYTHONPATH
    if [ ! "$ROS_MASTER_URI" ]; then
        export ROS_MASTER_URI=http://localhost:11311
    fi

    ros_src_file "$ROS_ROOT"
    ros_src_file "$HOME/ros_ws/devel"

    if [ -d "$HOME/ros_ws/src" ]; then
        ROS_PACKAGE_PATH="$ROS_PACKAGE_PATH:$HOME/ros_ws/src"
    fi

    if test -z "$ROS_DISTRO"; then
        ROS_DISTRO=`basename "$ROS_ROOT"`
    fi
    ST_FLAG=${ST_FLAG}"(ROS-$ROS_DISTRO)"
    #ST_FLAG=${ST_FLAG}"(ROS)"
}


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

#######
# ACE #
#######

if [ -d /usr/local/ace ] ; then
    export ACE_ROOT=/usr/local/ace
fi

if [ -n "$ACE_ROOT" ] ; then
    if [ -d "$ACE_ROOT/TAO" ] ; then
        export TAO_ROOT="$ACE_ROOT/TAO"
    fi

    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH$ACE_ROOT/lib:"
fi
