#!/bin/sh
#
# Source: https://github.com/xmonad/xmonad-testing/blob/master/build-scripts/build-with-stack.sh
#

# cd "$HOME"/.config/xmobarrc && stack install || exit
cd "$XMONAD_CONFIG_DIR" && stack build --reconfigure || exit

# Create a hard link at the requested destination, replacing any existing one.
ln -f -T "$(stack exec -- which xmonadrc)" "$HOME/.local/bin/xmonadrc"
#ln -f -T "$(stack exec -- which xmonad)" "$HOME/.local/bin/xmonad"


