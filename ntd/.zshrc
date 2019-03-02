##  ~/.zshrc
## by Neil T. Dantam
##
## This work hereby released into the public domain
## NO WARRANTY EXPRESSED OR IMPLIED


## Print the fancy status line
if [ -f /chroot-name ]; then
    ST_FLAG="[`cat /chroot-name`]"
else
    ST_FLAG=''
fi

function precmd {
    case "$TERM" in
        xterm*|linux|eterm-color|screen)
            local BLACK="%{\033[0;30m%}"
            local RED="%{\033[0;31m%}"
            local LIGHT_RED="%{\033[1;31m%}"
            local PURPLE="%{\033[0;35m%}"
            local LIGHT_PURPLE="%{\033[1;35m%}"
            local BROWN="%{\033[0;33%}"
            local WHITE="%{\033[1;37%}"
            local DARK_GRAY="%{\033[1;30m%}"
            local LIGHT_GRAY="%{\033[0;37m%}"
            local CYAN="%{\033[0;36m%}"
            local LIGHT_CYAN="%{\033[1;36m%}"
            local BLUE="%{\033[0;34m%}"
            local LIGHT_BLUE="%{\033[1;34m%}"
            local GREEN="%{\033[0;32m%}"
            local LIGHT_GREEN="%{\033[1;32m%}"
            local YELLOW="%{\033[1;33m%}"

            local NO_COLOR="%{\033[0m%}"
            local CLEAR_LINE='%{\033[K%}'

            local BLACK_BACK="%{\033[40m%}"
            local RED_BACK="%{\033[41m%}"
            local GREEN_BACK="%{\033[42m%}"
            local ORANGE_BACK="%{\033[43m%}"
            local BLUE_BACK="%{\033[44m%}"
            local PURPLE_BACK="%{\033[45m%}"
            local CYAN_BACK="%{\033[46m%}"
            local GRAY_BACK="%{\033[47m%}"

            local NO_COLOR_BACK="%{\033[49m%}"


            local UNDERLINE="%{\033[38m%}"
            local BOLD="%{\033[1m%}"

            local CLEAR_LINE='%{\033[K%}'
            local TOP_LEFT='%{\033[H%}'
            ;;
    esac


    if [ $HOST = "1x" -o $HOST = "leela" ]; then
        local battery="`acpi -b | sed -e 's/.*\(..\)%.*/\1/'`"
        if [  "${battery}" -gt 60   ]; then
            local battery="[${LIGHT_GREEN}${battery}%%${NO_COLOR}]"
        elif [  "${battery}" -eq 00  ]; then
            local battery="[${LIGHT_GREEN}${battery}%%${NO_COLOR}]"
        elif [ "${battery}" -gt 30 ]; then
            local battery="[${YELLOW}${battery}%%${NO_COLOR}]"
        else
            local battery="[${RED}${battery}%%${NO_COLOR}]"
        fi
    fi

    if [ $HOST = "krang" ]; then
        local battery="[`bms -V`]"
    fi

    case $HOST in
        # Robots
        krang|calvin|*hubo*)
            local HOSTCOLOR=${LIGHT_RED}
            ;;
        leela)
            local HOSTCOLOR=${LIGHT_PURPLE}
            ;;
        1x)
            local HOSTCOLOR=${LIGHT_GRAY}
            ;;
        farnsworth)
            local HOSTCOLOR=${LIGHT_CYAN}
            ;;
        *)
            local HOSTCOLOR=${GREEN}
            ;;
    esac


    # Print titlebar in xterms
    case "$TERM" in
        xterm*)
            print -Pn "\e]0;%n@%m://%~\a"
            ;;
    esac

    local ST_HOST=${(%):-%B-(%b${LIGHT_BLUE}%n${NO_COLOR}@${HOSTCOLOR}%m${NO_COLOR}${LIGHT_PURPLE}${ST_FLAG}${NO_COLOR}${battery}:${LIGHT_GRAY}%y${CYAN}//${NO_COLOR}${YELLOW}%~${NO_COLOR}%B)-%b}
    echo ${ST_HOST};
    #local ST_HOST=${(%):-%B-(%b%n@%m:%y//%~%B)-%b}
    #local ST_RET=${(%):-%?}
    #PR_TITLEBAR=''
}

#print titlebar in xterm
function preexec {
    case "$TERM" in
        xterm*)
        print -Pn "\e]2;%n@%m: $1\a"
        ;;
    esac
}

## Set PS1
case $TERM in
    dumb*)
        PS1="$(print '%?,%!%# ')"
        #PS1="$(print '%n@%m://%~ %# ')"
        ;;
    xterm-*|linux|eterm-color*)
        PS1="$(print '%(?..%{\e[0;31m%})%?%{\e[0m%},%{\e[1;32m%}%!%#%{\e[0m%} ')"
        #PS1="$(print '%{\e[1;32m%}%!%#%{\e[0m%} ')"
        ;;
    tramp)
        unsetopt zle && PS1='$ '
        ;;
    *)
        PS1="$(print '%!%# ')"
        ;;
esac




if [ `hostname` = vasilia -o `hostname` = kelden ] ; then
    export HUMROOT=~/mnt/daneel-src/humanoids/src/common
else
    export HUMROOT=$HOME/src/humanoids/src/common
fi

function hls {
    find $HUMROOT -maxdepth 3 \( \( -name 3rdparty \) -prune \) -o -name Makefile
}

function hcomplete {
    reply=(`hls | egrep "$1[^/]*/(trunk/)?Makefile\$" | sed -e 's@.*/\([^/]\+\)/trunk/Makefile@\1@' -e 's@.*/\([^/]\+\)/Makefile@\1@'`)
    #reply=(`ls $HUMROOT/**/Makefile | egrep "$1[^/]*/(trunk/)?Makefile\$" | sed -e 's@.*/\([^/]\+\)/trunk/Makefile@\1@' -e 's@.*/\([^/]\+\)/Makefile@\1@'`)
}

## Inspired by roscd
function hcd {
    cd  $(dirname $(hls | egrep "/$1/(trunk/)?Makefile$" | sort -r | head -n 1))
}

compctl -K hcomplete hcd

function zgitls {
    #find -L ~/git -name .git -prune -print | sed -e 's!/.git$!!'
    find -L ~/git -type d -exec test -e  '{}'/.git ';' -prune -print
    #'ls' -d ~/git/**/.git |  sed -e 's!/.git$!!'
}

function zgitcomplete {
    reply=(`zgitls | egrep "/$1[^/]*$" | sed -e 's@.*/\([^/]\+\)/\?$@\1@'`)
}

function gcd {
    DIR=$(zgitls | grep "/$1\$" | sort)
    if [ -n "$DIR" ] ; then
        if [ `echo $DIR | wc -l` = 1 ]; then
            cd "$DIR"
        else
            echo Ambiguous repo:
            echo "$DIR" | sed -e 's/^/> /'
            return 1
        fi
    else
        echo "no such project: $1"
        return 1
    fi
}

compctl -K zgitcomplete gcd


# set function path
fpath=($HOME/.zsh.d $fpath)

## enable autocomplete
autoload -U compinit #promptinit
compinit

#ignore hosts file for completion
zstyle '*' hosts $hosts
#promptinit;

## set keymapt
bindkey -v

## get rid of prompt \r
unsetopt promptcr

## emacs bindings
bindkey -e

[[ $EMACS = t ]] && unsetopt zle

## enable correction
setopt correct

## enable history
HISTSIZE=4096
HISTFILE=~/.zsh_history
SAVEHIST=4096

# These options seem to help ZSH play nice on NFS
if [ ! -e /chroot-name ]; then
    setopt APPEND_HISTORY
    setopt SHARE_HISTORY
    setopt HIST_FCNTL_LOCK
fi

## ROS SETUP  ##
if [ -e "$HOME/ros/setup.zsh" ]; then
    source "$HOME/ros/setup.zsh"
elif [ -e "$HOME/src/ros/setup.zsh" ]; then
    source "$HOME/src/ros/setup.zsh"
elif [ -e /opt/ros/electric/setup.zsh ]; then
    source /opt/ros/electric/setup.zsh
fi

## load environment
source ~/.env.sh
