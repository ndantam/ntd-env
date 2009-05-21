##  ~/.zshrc
## by Neil T. Dantam
##
## This work hereby released into the public domain
## NO WARRANTY EXPRESSED OR IMPLIED

function precmd {
    local BLACK="%{\033[0;30%}"
    local RED="%{\033[0;31%}"
    local LIGHT_RED="%{\033[1;31%}"
    local PURPLE="%{\033[0;35%}"
    local LIGHT_PURPLE="%{\033[1;35%}"
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


    if [ `hostname` = "hesh" ]; then
        local battery=`acpi -b | sed -e 's/.*\(..\)%,.*/\1/'`
    fi
    case $TERM in
        dumb*)
;;
xterm*)
print -Pn "\e]0;%n@%m://%~\a"
if [ `hostname` = "hesh" ]; then
    local ST_HOST=${(%):-%B-(%b${LIGHT_BLUE}%n${NO_COLOR}@${GREEN}%m${NO_COLOR}["${battery}%%"]:${LIGHT_GRAY}%y${CYAN}//${NO_COLOR}${YELLOW}%~${NO_COLOR}%B)-%b}
else
    local ST_HOST=${(%):-%B-(%b${LIGHT_BLUE}%n${NO_COLOR}@${GREEN}%m${NO_COLOR}:${LIGHT_GRAY}%y${CYAN}//${NO_COLOR}${YELLOW}%~${NO_COLOR}%B)-%b}
fi
local ST_RET=${(%):-%?}
echo ${ST_HOST};
;;
*)
local ST_HOST=${(%):-%B-(%b%n@%m:%y//%~%B)-%b}
local ST_RET=${(%):-%?}
PR_TITLEBAR=''
echo ${ST_HOST};
;;
esac
###


}

function preexec {
    case $TERM in
        dumb*)
;;
xterm*)
print -Pn "\e]2;%n@%m: $1\a"
;;
*)
PR_TITLEBAR=''
;;
esac
}

#local GREEN="%{\033[0;32m%}"
#local LIGHT_GREEN="%{\033[1;32m%}"
#local NO_COLOR="%{\033[0m%}"
case $TERM in
    dumb*)
    PS1="$(print '%n@%m://%~ %# ')"
    ;;
    xterm*)
                #PS1="$(print '%{\e[1;32m%}%!%#%{\e[0m%} ')"
    PS1="$(print '%?,%{\e[1;32m%}%!%#%{\e[0m%} ')"
    ;;
    *)
    PS1="$(print '%!%# ')"
    ;;
esac



## enable autocomplete
autoload -U compinit #promptinit
compinit
#promptinit;

## set keymapt
bindkey -v

## load environment
source ~/.env.sh

## get rid of prompt \r
unsetopt promptcr

## emacs bindings
bindkey -e

## enable history
HISTSIZE=1000
HISTFILE=~/.zsh_history
SAVEHIST=500
