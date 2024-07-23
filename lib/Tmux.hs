{-# LANGUAGE QuasiQuotes #-}
-- Tmux.hs
module Tmux (tmuxMdo) where

import Quasiquote (config)

tmuxMdo :: String
tmuxMdo =
  [config|# This file is under control of Propellor.

# To reload config file use:
#   tmux source-file ~/.tmux.conf
#
# (Also see tmux section in /etc/nixos/configuration.nix)
#

set -g mouse on
set -g history-limit 9999
set -g default-terminal "screen-256color"

# Scrolling as in xterm (apparently doesn't work)
#bind -n S-PageUp send-keys C-x \; send-keys PageUp
#bind -n S-Pagedown send-keys Pagedown

# https://gist.github.com/william8th/faf23d311fc842be698a1d80737d9631
# Set new panes to open in current directory
# bind c new-window -c "#{pane_current_path}"
# bind '"' split-window -c "#{pane_current_path}"
# bind % split-window -h -c "#{pane_current_path}"

# https://www.seanh.cc/2020/12/30/how-to-make-tmux's-windows-behave-like-browser-tabs/#:~:text=Key%20bindings&text=conf%20file%20to%20get%20browser,and%20C%2DS%2DTab%20in%20tmux.
set -g base-index 1       # Start numbering windows at 1, not 0.
set -g pane-base-index 1  # Start numbering panes at 1, not 0.
bind -n C-t new-window
bind -n C-PgDn next-window
bind -n C-PgUp previous-window
bind -n C-S-Left swap-window -t -1\; select-window -t -1
bind -n C-S-Right swap-window -t +1\; select-window -t +1
bind -n M-1 select-window -t 1
bind -n M-2 select-window -t 2
bind -n M-3 select-window -t 3
bind -n M-4 select-window -t 4
bind -n M-5 select-window -t 5
bind -n M-6 select-window -t 6
bind -n M-7 select-window -t 7
bind -n M-8 select-window -t 8
bind -n M-9 select-window -t:$
bind -n C-M-w kill-window
bind -n C-M-q confirm -p "Kill this tmux session?" kill-session
bind -n F11 resize-pane -Z

set -g status-style "bg=default"
set -g window-status-current-style "bg=default,reverse"
set -g window-status-separator ''  # No spaces between windows in the status bar.
set -g window-status-format "#{?window_start_flag,, }#I:#W#{?window_flags,#F, } "
set -g window-status-current-format "#{?window_start_flag,, }#I:#W#{?window_flags,#F, } "
|]
