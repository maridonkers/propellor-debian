{-# LANGUAGE QuasiQuotes #-}

module Muttrc (muttrcMdo) where

import Quasiquote (config)

muttrcMdo :: String
muttrcMdo =
  [config|# This file is under control of Propellor.

# http://www.mutt.org/doc/manual/
# https://www.techrepublic.com/article/10-helpful-tips-for-mutt-e-mail-client-power-users/
# https://roidelapluie.be/blog/2015/05/09/mutt/
# https://superuser.com/questions/1107704/merge-multiple-email-accounts-in-single-view-in-mutt
# https://www.mankier.com/1/notmuch-mutt#Integration_with_Mutt
# https://anarc.at/blog/2021-11-21-mbsync-vs-offlineimap/
# https://isync.sourceforge.io/mbsync.html
# https://linuxconfig.org/how-to-install-configure-and-use-mutt-with-a-gmail-account-on-linux
# http://www.mutt.org/doc/manual/#patterns-modifier
# https://unix.stackexchange.com/questions/63869/how-to-bind-controlarrowkey-in-mutt
#

set browser_abbreviate_mailboxes=yes

# Folder hooks
folder-hook 'account.org.photonsphere.contact' 'source ~/.mutt/account.org.photonsphere.contact'
folder-hook 'account.nl.donkersautomatisering.info' 'source ~/.mutt/account.nl.donkersautomatisering.info'
folder-hook 'account.com.donkersphotography.contact' 'source ~/.mutt/account.com.donkersphotography.contact'
# folder-hook 'account.com.gmail.a.h.m.donkers' 'source ~/.mutt/account.com.gmail.a.h.m.donkers'
# folder-hook 'account.com.gmail.mari.donkers' 'source ~/.mutt/account.com.gmail.mari.donkers'
# folder-hook 'account.com.gmail.donkersautomatisering' 'source ~/.mutt/account.com.gmail.donkersautomatisering'

# Default account
source ~/.mutt/account.org.photonsphere.contact

# Switch accounts
macro index <f1> '<sync-mailbox><enter-command>source ~/.mutt/account.org.photonsphere.contact<enter><change-folder>!<enter>'
macro index <f2> '<sync-mailbox><enter-command>source ~/.mutt/account.nl.donkersautomatisering.info<enter><change-folder>!<enter>'
macro index <f3> '<sync-mailbox><enter-command>source ~/.mutt/account.com.donkersphotography.contact<enter><change-folder>!<enter>'
# macro index <f4> '<sync-mailbox><enter-command>source ~/.mutt/account.com.gmail.a.h.m.donkers<enter><change-folder>!<enter>'
#macro index <f5> '<sync-mailbox><enter-command>source ~/.mutt/account.com.gmail.mari.donkers<enter><change-folder>!<enter>'
#macro index <f6> '<sync-mailbox><enter-command>source ~/.mutt/account.com.gmail.donkersautomatisering<enter><change-folder>!<enter>'

# HTML e-mail format
auto_view text/html # view HTML automatically
alternative_order text/html text/enriched text/plain

# View and optionally follow URLs in a message (using CTRL+b)
# https://manpages.ubuntu.com/manpages/kinetic/en/man1/urlscan.1.html
macro index,pager \cb "<pipe-message> urlscan<Enter>" "call urlscan to extract URLs out of a message"
macro attach,compose \cb "<pipe-entry> urlscan<Enter>" "call urlscan to extract URLs out of a message"

# Key binding for list in view
bind attach <return> view-mailcap
# Key binding for alternative 'including deleted' navigation via Shift+Up and Shift+Down
bind index \e[1\;2A previous-entry
bind index \e[1\;2B next-entry

# Sorting
set sort=threads
# set sort = "reverse-date-received"
# set sort = "date-received"

# Various defaults
set ssl_force_tls = yes
set abort_nosubject = no
set mail_check = 60
set timeout = 10

# No line wrap markers (+ character)
set markers=no

# Coloring
# https://unix.stackexchange.com/questions/264814/adjusting-colors-for-mutts-status-line-in-multi-account-setup
# https://groups.google.com/g/comp.mail.mutt/c/NUpldK-eLo8

# Index stuff
#color indicator color229 color238
color indicator color236 color188
color tree color109 color237
color index white black ~A
color index cyan black ~N # New
color index yellow black ~F # Flagged
color index white black ~O # Old
color index brightgreen black ~P # From me
color index red black ~D # Deleted

# set index_format="%4C %Z %{%b %d} %-15.15L (%?l?%4l&%4c?) %s"
# set index_format="%4C %Z %{%b %d} %-15.15L (%4l) %s"
set index_format="%4C %Z %{%Y-%m-%d %H:%M:%S} %-15.15L (%?l?%4l&%4c?) %s"

# Header stuff
color hdrdefault yellow black # headers white on black
color header brightgreen black ^From: # sender’s name in green
color quoted cyan black # quoted text in blue
color signature red black # signature in rde

# Scrolling
set menu_scroll = yes

# Headers
set edit_headers=yes

# Integration of notmuch via notmuch-mutt
macro index <F12> \
    "<enter-command>set my_old_pipe_decode=\$pipe_decode my_old_wait_key=\$wait_key nopipe_decode nowait_key<enter>\
    <pipe-message>mbsync -a && notmuch new<enter>\
    <enter-command>set pipe_decode=\$my_old_pipe_decode wait_key=\$my_old_wait_key<enter>" \
          "notmuch: remove message from inbox"
|]
