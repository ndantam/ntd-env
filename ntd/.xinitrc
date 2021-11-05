#!/bin/sh


###########
# Screens #
###########

if [ -x "${HOME}/bin/xrandr-$(hostname)" ]; then
    "${HOME}/bin/xrandr-$(hostname)"
fi


############
# Keyboard #
############

setxkbmap -option "ctrl:nocaps"
#setxkbmap -option "compose:sclk"
xmodmap ~/.xmodmap

# Make touchpad scroll properly
#synclient "VertEdgeScroll=1"


#########
# Mouse #
#########

ntd_xinitrc_dev() {
    xinput --list --name-only | grep -i "$1"
}

# Logitech M570
NAME=$(ntd_xinitrc_dev "logitech m570")
if [ -n "${NAME}" ]; then
    # echo Found "${NAME}"
    xinput set-button-map "${NAME}" 1 3 2 4 5 6 7 8 9
fi

# Kensington Slimblade
ntd_xinitrc_kes_left() {
    # 'Kensington Kensington Slimblade Trackball'
    # - SW: 1 (left Click)
    # - SE: 3 (Right Click)
    # - NW: 2 (Middle Click)
    # - NE: 8 (Back)
    KES_SW=1
    KES_SE=3
    KES_NW=2
    KES_NE=8

    # Name changes in different models
    NAME=$(ntd_xinitrc_dev slimblade)

    # Right Hand
    # xinput set-button-map \
    #        'Kensington Kensington Slimblade Trackball' \
    #        1 8 3 4 5 6 7 2 9
           # 1 2 3 4 5 6 7 8 9

    # Left Hand
    xinput set-button-map "${NAME}" 3 2 1 4 5 6 7 8 9
}


case "$(hostname)" in
    bender)
        ntd_xinitrc_kes_left
        ;;
    farnsworth)
        ntd_xinitrc_kes_left

        # Anker
        xinput set-button-map \
               'USB Optical Mouse' \
               1 3 2 4 5 6 7 8 9
        ;;
esac


#########
# Other #
#########

# xinput set-button-map \
    #        'Logitech Rechargeable Touchpad T650' \
    #        1 3 2 4 5 6 7 8 9

## T40
# T40_LB=1
# T40_LS=8
# T40_RS=9
# T40_RB=3
# T40_SU=4
# T40_SD=5
# T40_SC=2

# T40_NAME='MOSART Semi. 2.4G INPUT DEVICE Mouse'
# xinput set-button-map "${T40_NAME}" \
    #        $T40_LB \
    #        $T40_RB \
    #        $T40_SC \
    #        $T40_SU \
    #        $T40_SD \
    #        6 \
    #        7 \
    #        $T40_LS \
    #        $T40_RS \

# Reverse mouse direction
# xinput set-prop "$T40_NAME" "Coordinate Transformation Matrix" -1 0 0 0 -1 0 0 0
