#!/bin/bash

export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/opt/X11/lib/pkgconfig"

LIBRARY_PATH="/opt/X11/lib:$LIBRARY_PATH" \
CPPFLAGS="-I/opt/X11/include" \
cabal install X11 --flags="-use_xft"

LIBRARY_PATH="/opt/X11/lib:$LIBRARY_PATH" \
cabal install X11-xft xmonad xmobar xmonad-contrib-gpl \
      --flags="with_uft8 with_xft" \
      --extra-include-dirs="/opt/X11/include/freetype2"

# The following specific versions have been tested with Haskell Platform 7.8.4
# and XQuartz 2.7.7. You might or might not need pkg-config

# LIBRARY_PATH="/opt/X11/lib:$LIBRARY_PATH" \
# CPPFLAGS="-I/opt/X11/include" \
# cabal install X11-1.6.1.2 --flags="-use_xft"

# LIBRARY_PATH="/opt/X11/lib:$LIBRARY_PATH" \
# cabal install \
#             X11-xft-0.3.1 \
#             xmonad-0.11.1 \
#             xmobar-0.23.1 \
#             xmonad-contrib-0.11.3 \
#       --flags="with_uft8 with_xft" \
#       --extra-include-dirs="/opt/X11/include/freetype2"
