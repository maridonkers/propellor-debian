{-# LANGUAGE QuasiQuotes #-}

module Bashprofile (bashprofileMdo) where

import Quasiquote (config)

bashprofileMdo :: String
bashprofileMdo =
  [config|# This file is under control of Propellor.

# Include secrets
if [ -r ~/.bashrc_secrets ]
then
    . ~/.bashrc_secrets 
fi

# https://askubuntu.com/questions/432217/prevent-duplicate-entries-in-path
[[ ":$PATH:" =~ ":~/bin:" ]] || PATH="$PATH:~/bin"
[[ ":$PATH:" =~ ":~/.local/bin" ]] || PATH="~/.local/bin:$PATH"
[[ ":$PATH:" =~ ":~/.cargo/bin" ]] || PATH="$PATH:~/.cargo/bin"
[[ ":$PATH:" =~ ":~/.cabal/bin" ]] || PATH="$PATH:~/.cabal/bin"
[[ ":$PATH:" =~ ":~/go/bin" ]] || PATH="$PATH:~/go/bin"
[[ ":$PATH:" =~ ":~/.nix-profile/bin" ]] || PATH="$PATH:~/.nix-profile/bin"

export EDITOR="vi"
export QT_LOGGING_RULES="*=false"
export FREETYPE_PROPERTIES="truetype:interpreter-version=38"

# Workaround for Java VM to make it think xmonad is a parenting WM
# https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Using_SetWMName
export _JAVA_AWT_WM_NONREPARENTING=1

# Guix configuration
export GUIX_PROFILE="/home/mdo/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

export DISPLAY=":0"
# export TERM="xterm-256color"              # getting proper colors

# TODO What was this meant to do? Why switch off screen?
# Determine if there's an HDMI monitor and switch off screen off if there is.
# xrandr --listactivemonitors --verbose|grep '^HDMI-[0-9] connected ' > /dev/null
# if [ $? -eq 0 ]
# then
#   xset s off -dpms
# fi

# Rust
# https://internals.rust-lang.org/t/cargo-sparse-protocol-feedback-thread/18234
export CARGO_REGISTRIES_CRATES_IO_PROTOCOL="sparse"

export LYNX_LSS=$HOME/lynx.lss

export LESSOPEN="| highlight --out-format=xterm256 --style=clarity %s"
export LESS=' -R '

# Security CAM.
export SC_IPADDRESS="192.168.1.4"

export COWPATH="${HOME}/lib/cowfiles.nixos:${HOME}/lib/cowfiles"
#~/bin/randomcowsay

if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi
|]
