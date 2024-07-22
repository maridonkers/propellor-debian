{-# LANGUAGE QuasiQuotes #-}

-- Nftables.hs
module Xwindows (xInitrc, xModmap, xResources) where

import Quasiquote (config)

xResources :: String
xResources =
  [config|! This file is under control of Propellor.

! Reload this file via the following command:
! xrdb ~/.Xresources
!

! Allow xterm to report the TERM variable correctly.
! Do not set the TERM variable from your ~/.bashrc or ~/.bash_profile or similar file.
! The terminal itself should report the correct TERM to the system so that the proper terminfo file will be used.
! Two usable terminfo names are xterm and xterm-256color
XTerm.termName: xterm-256color

! Emacs.font: DejaVuSansMono-14
! Xft.hinting: 1
! Xft.dpi: 96

! Default size of xterm.
xterm*geometry: 94x28

!Xcursor*theme: aero-large-drop
!Xcursor*size: 48
!Xft.dpi: 96

! http://www.futurile.net/2016/06/14/xterm-setup-and-truetype-font-configuration/
xterm*VT100.Translations: #override \
    Ctrl <Key> minus: smaller-vt-font() \n\
    Ctrl <Key> plus: larger-vt-font() \n\
    Ctrl <Key> 0: set-vt-font(d)

! (set-default-font “DejaVuSansMono-11”)
! (set-default-font “Hack-12”)

Xft.dpi: 96
Xft.antialias: true
Xft.hinting: true
Xft.rgba: rgb
Xft.autohint: true
Xft.hintstyle: hintfull
Xft.lcdfilter: lcdfilter

XTerm*renderFont: true
! XTerm*faceName: xft:Mononoki Nerd Font, \
!                 xft:JoyPixels:size=10, \
!                 xft:Monospace:style=Medium:size=10
! XTerm*faceName: xft:Mononoki Nerd Font
! XTerm*faceName: xft:Jetbrains Mono

XTerm*faceName: xft:JiraCode
XTerm*faceSize: 12
XTerm*utf8: 2
XTerm*locale: true

! TODO
! https://askubuntu.com/questions/161652/how-to-change-the-default-font-size-of-xterm
xterm*font:     *-fixed-*-*-*-18-*

XTerm.vt100.translations: #override \n\
  Ctrl <Key> j: smaller-vt-font() \n\
  Ctrl <Key> k: larger-vt-font()

! Every shell is a login shell by default (for inclusion of all necessary environment variables)
XTerm*loginshell: true

! I like a LOT of scrollback...
XTerm*savelines: 16384

! double-click to select whole URLs :D
XTerm*charClass: 33:48,36-47:48,58-59:48,61:48,63-64:48,95:48,126:48

! XTerm*foreground: #bbc5ff
! XTerm*background: #282a36
! XTerm*cursorColor: #bbc5ff

! Dracula Xresources palette
*.foreground: #F8F8F2
*.background: #282A36
*.color0:     #000000
*.color8:     #4D4D4D
*.color1:     #FF5555
*.color9:     #FF6E67
*.color2:     #50FA7B
*.color10:    #5AF78E
*.color3:     #F1FA8C
*.color11:    #F4F99D
*.color4:     #BD93F9
*.color12:    #CAA9FA
*.color5:     #FF79C6
*.color13:    #FF92D0
*.color6:     #8BE9FD
*.color14:    #9AEDFE
*.color7:     #BFBFBF
*.color15:    #E6E6E6
|]

xModmap :: String
xModmap =
  [config|! This file is under control of Propellor.

! Remove shift lock functionality
clear Lock

! Shift + Caps_Lock is compose key
keycode 66 = Caps_Lock Multi_key Caps_Lock
|]

xInitrc :: String
xInitrc =
  [config|# This file is under control of Propellor.

#setxkbmap -rules evdev -model evdev -layout us -variant altgr-intl

if [ -r ~/.Xmodmap ]
then
   xmodmap ~/.Xmodmap
fi

if [ -r ~/.Xresources ]
then
   xrdb -merge ~/.Xresources
fi

exec /etc/alternatives/x-window-manager
|]
