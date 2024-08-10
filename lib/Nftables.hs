{-# LANGUAGE QuasiQuotes #-}

module Nftables (nftRules) where

import Quasiquote (config)

nftRules :: String
nftRules =
  [config|#!/usr/sbin/nft -f

table firewall {
  chain incoming {
    type filter hook input priority 0; policy drop;
    # established/related connections
    ct state established,related accept
    # loopback interface
    iifname lo accept
    # icmp
    icmp type echo-request accept
    # open tcp ports: sshd (22), etc.
    tcp dport { ssh, 1234, 3333, 8080, 8096 } accept
  }
}

table ip6 firewall {
  chain incoming {
    type filter hook input priority 0; policy drop;
    # established/related connections
    ct state established,related accept
    # invalid connections
    ct state invalid drop
    # loopback interface
    iifname lo accept
    # icmp
    # routers may also want: mld-listener-query, nd-router-solicit
    icmpv6 type { echo-request, nd-neighbor-solicit } accept
    # open tcp ports: sshd (22), etc.
    tcp dport { ssh, 1234, 3333, 8080, 8096 } accept
  }
}
|]
