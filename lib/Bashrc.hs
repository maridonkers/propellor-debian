
{-# LANGUAGE QuasiQuotes #-}

module Bashrc (bashrcMdo, bashrcRoot) where

import Quasiquote (config)

bashrcMdo :: String
bashrcMdo =
  [config|# This file is under control of Propellor.

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Include secrets
if [ -r ~/.bashrc_secrets ]
then
    . ~/.bashrc_secrets 
fi

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# https://askubuntu.com/questions/432217/prevent-duplicate-entries-in-path
[[ ":$PATH:" =~ ":~/bin:" ]] || PATH="$PATH:~/bin"
[[ ":$PATH:" =~ ":~/.local/bin" ]] || PATH="~/.local/bin:$PATH"
[[ ":$PATH:" =~ ":~/.cargo/bin" ]] || PATH="$PATH:~/.cargo/bin"
[[ ":$PATH:" =~ ":~/.cabal/bin" ]] || PATH="$PATH:~/.cabal/bin"
[[ ":$PATH:" =~ ":~/go/bin" ]] || PATH="$PATH:~/go/bin"
[[ ":$PATH:" =~ ":~/.npm-global/bin" ]] || PATH="$PATH:~/.npm-global/bin"

#[[ ":$PATH:" =~ ":~/.nix-profile/bin" ]] || PATH="$PATH:~/.nix-profile/bin"

export EDITOR="vi"
export QT_LOGGING_RULES="*=false"
export FREETYPE_PROPERTIES="truetype:interpreter-version=38"

# Workaround for Java VM to make it think xmonad is a parenting WM
# https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Using_SetWMName
export _JAVA_AWT_WM_NONREPARENTING=1

# Guix configuration
export GUIX_PROFILE="/home/mdo/.guix-profile"
# TODO Commented out the source command below because of redundancy in PATH
# . "$GUIX_PROFILE/etc/profile"

# https://guix.gnu.org/manual/en/html_node/Invoking-guix-gc.html
alias guix-garbage-collect="guix package --delete-generations && guix gc --collect-garbage"
alias guix-list-installed="guix package --list-installed"
alias guix-update="guix pull && guix package -u"

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

# IHP
export IHP_TELEMETRY_DISABLED=1
export IHP_BROWSER=firefox
export IHP_EDITOR="emacs"
eval "$(direnv hook bash)"

# https://www.atlassian.com/git/tutorials/dotfiles
alias config='git --git-dir=/home/mdo/.cfg/ --work-tree=/home/mdo'

alias b="batcat -n"
alias v="nvim -R"
alias md="mdcat -p"
alias lcat="less -EX"
alias bl="LESSOPEN='' less"  
# alias www="librewolf --new-window"
alias www="falkon --new-window"
alias http-server="python -m http.server"

alias ls="ls --color=never"
alias l="exa"
alias ll="exa -l"
alias lls="exa -ls modified"

alias lsb="lsblk -o PATH,SIZE,FSTYPE,MOUNTPOINTS,UUID"

alias tma="tmux attach-session -t"
alias tmn="tmux new-session -s"
alias tml="tmux list-sessions"

alias nl="trans -s nl"
alias en="ennl"
alias fr="frnl"
alias es="esnl"
alias pt="ptnl"
alias it="itnl"
alias nlen="trans -s nl -t en"
alias ennl="trans -s en -t nl"
alias esnl="trans -s es -t nl"
alias nles="trans -s nl -t es"
alias esen="trans -s es -t en"
alias enes="trans -s en -t es"
alias frnl="trans -s fr -t nl"
alias nlfr="trans -s nl -t fr"
alias pten="trans -s pt -t en"
alias ptnl="trans -s pt -t nl"
alias itnl="trans -s it -t nl"
alias fren="trans -s fr -t en"

alias xdg-list-paths="XDG_UTILS_DEBUG_LEVEL=2 xdg-mime query default text/html"

export LYNX_LSS=$HOME/lynx.lss

# EMH emh: displays help for command line e-mail commands
alias emh="grep '^# EMH ' ~/.bashrc | sed -e 's/^# EMH //'"
# EMH emo: gets new e-mails via mbsync (beware: not added to notmuch; use emn command to add)
alias emo="mbsync -a"
# EMH emg: gets new e-mails via mbsync and adds them to notmuch
alias emg="mbsync -a && notmuch new"

# EMH emn: adds new e-mails to notmuch
alias emn="notmuch new"
# EMH emf: searches e-mail for search term; see man notmuch-search-terms
alias emf="notmuch search"
# EMH emlt: list e-mails for today
alias emlt="notmuch search date:today"
# EMH emly: list e-mails from yesterday to today
alias emly="notmuch search date:yesterday..today"
# EMH emld: list e-mails from # days to today
function emld() { # get list of ID's in e-mail
  notmuch search date:$1d..today
}
# EMH emlw: list e-mails for week to today
alias emlw="notmuch search date:week..today"
# EMH emlm: list e-mails for month to today
alias emlm="notmuch search date:month..today"
# EMH ems: show e-mail content, including textual HTML dump for the entire thread (if applicable)
alias ems="notmuch show --include-html --entire-thread=true"
# EMH emsl: list mime IDs
function emsl() { # get list of ID's in e-mail
  ems $1 | grep '{ ID:'
}
# EMH emsi: show image content by ID in feh image viewer
function emsi() { # show image by ID in e-mail
  notmuch show --part=$1 $2 | feh -
}
# EMH emsp: show PDF content by ID in zathura PDF viewer
function emsp() { # show PDF by ID in e-mail
  notmuch show --part=$1 $2 | zathura -
}
# EMH emsm: show mime content by ID (likely clutters terminal, hence store in file).
function emsm() { # show mime part by ID in e-mail
  notmuch show --part=$1 $2
}
# EMH emsa: show attachment by ID and type (likely clutters terminal, hence store in file).
# EMH       attachment{ ID: 5, Filename: Brief voor - ambassade def2.docx, Content-id: ...
# EMH       verify type of attachment by ID and type in e-mail; e.g.: emsa 5 def2.docx thread:0000000000005dea
function emsa() { # show attachment by ID and type in e-mail; e.g.: emsa 5 docx thread:0000000000005dea
  notmuch show --part=$1 attachment:$2 and $3
}
# EMH emsav: verify attachment type by ID and type (reported is file type).
# EMH        attachment{ ID: 5, Filename: Brief voor - ambassade def2.docx, Content-id: ...
# EMH        verify type of attachment by ID and type in e-mail; e.g.: emsav 5 def2.docx thread:0000000000005dea
function emsav() {
  notmuch show --part=$1 attachment:$2 and $3 | file -
}
# EMH emsww: show HTML content by ID in lynx command line webbrowser
function emsww() {
  notmuch show --part=$1 $2 | lynx -stdin
}
# EMH emsw: show HTML content (ID determined automatically) in lynx command line webbrowser
function emsw() {
  IDX=`emsl $1 | grep 'Content-type: text/html' | sed -e "s/^.*ID: \([0-9]\+\),.*$/\1/"`
  if [ "$IDX" != "" ]
  then
    notmuch show --part=$IDX $1 | lynx -stdin
  else
    echo "NO HTML CONTENT"
  fi
}

alias ps="ps -ww"
alias scl="screen -ls"
alias scs="screen -S"
alias scz="screen -ls | grep '(Detached)'"
alias scr="screen -DR"

alias speedtest="speedtest -p"

alias lsblks="lsblk -o +FSSIZE,FSAVAIL,PTTYPE,HOTPLUG,UUID"

alias eip4="dig +short myip.opendns.com @resolver1.opendns.com"

alias security="w3m https://www.debian.org/security/"

function mpv-dvd() {
    mpv dvd://$1 --dvd-device="$2"
}

alias sophia="ssh -i ~/.ssh/id_rsa-sophia mdo@192.168.1.43"
alias styssh="ssh -i ~/.ssh/id_ed25519 -p 2022 192.168.1.154"
alias tarssh="ssh -i ~/.ssh/id_rsa-tartarus -p 2022 u0_a44@192.168.1.205"

alias dcdf="tarssh '(cd ~/storage/movies/DroidDashcam/Temporary; df -h .)'"
alias dcdu="tarssh '(cd ~/storage/movies/DroidDashcam/Temporary; du -h .)'"
alias dcls="tarssh '(cd ~/storage/movies/DroidDashcam/Temporary; exa -l)'"
alias dccpa="tarssh '(cd ~/storage/movies/DroidDashcam/Temporary; tar cf - *)' | tar xvf -"
function dccp() {
	tarssh "(cd ~/storage/movies/DroidDashcam/Temporary; tar cf - $1)" | tar xvf -
}

alias nixsearch="nix search nixpkgs"
alias nix-env="PAGER= nix-env"

alias es-dw="mpv https://www.youtube.com/watch?v=Io5mt83nCcU &"
alias es-euronews="mpv https://www.youtube.com/watch?v=O9mOtdZ-nSk &"
alias es-france24="mpv https://www.youtube.com/live/Y-IlMeCCtIg &"
alias es-rtve="mpv https://www.youtube.com/watch?v=y3-l3m4MeU8 &"

alias fr-euronews="mpv https://www.youtube.com/watch?v=NiRIbKwAejk &"
alias fr-france24="mpv https://www.youtube.com/live/l8PMl7tUDIE &"
alias fr-franceinfo="mpv https://www.youtube.com/watch?v=Z-Nwo-ypKtM &"

alias en-dw="mpv https://www.youtube.com/watch?v=tZT2MCYu6Zw &"
alias en-euronews="mpv https://www.youtube.com/watch?v=pykpO5kQJ98 &"
alias en-france24="mpv https://www.youtube.com/watch?v=Ap-UM1O9RBU &"

export LESSOPEN="| highlight --out-format=xterm256 --style=clarity %s"
export LESS=' -R '

# Security CAM.
export SC_IPADDRESS="192.168.1.4"

export COWPATH="${HOME}/lib/cowfiles.nixos:${HOME}/lib/cowfiles"
#~/bin/randomcowsay

# https://gitlab.com/dwt1/dotfiles/-/blob/master/.bashrc
if [[ ${EUID} == 0 ]] ; then
    PS1='\[\033[01;31m\][\h\[\033[01;36m\] \W\[\033[01;31m\]]\$\[\033[00m\] '
else
    PS1='\[\033[01;32m\][\u@\h\[\033[01;37m\] \W\[\033[01;32m\]]\$\[\033[00m\] '
fi
|]

bashrcRoot :: String
bashrcRoot =
  [config|# This file is under control of Propellor.

# ~/.bashrc: executed by bash(1) for non-login shells.

[[ ":$PATH:" =~ ":/snap/bin:" ]] || PATH="$PATH:/snap/bin"

# Note: PS1 and umask are already set in /etc/profile. You should not
# need this unless you want different defaults for root.
# PS1='${debian_chroot:+($debian_chroot)}\h:\w\$ '
# umask 022

# You may uncomment the following lines if you want `ls' to be colorized:
# export LS_OPTIONS='--color=auto'
# eval "$(dircolors)"
# alias ls='ls $LS_OPTIONS'
# alias ll='ls $LS_OPTIONS -l'
# alias l='ls $LS_OPTIONS -lA'
#
# Some more alias to avoid making mistakes:
# alias rm='rm -i'
# alias cp='cp -i'
# alias mv='mv -i'

alias ls="ls --color=never"
alias l="exa"
alias ll="exa -l"
alias lls="exa -ls modified"

alias lsb="lsblk -o PATH,SIZE,FSTYPE,MOUNTPOINTS,UUID"

alias tma="tmux attach-session -t"
alias tmn="tmux new-session -s"
alias tml="tmux list-sessions"
|]
