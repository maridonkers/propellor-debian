-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.
import Data.List
import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.File as File
-- import qualified Propellor.Property.Firewall as Firewall
import qualified Propellor.Property.Fstab as Fstab
import qualified Propellor.Property.Group as Group
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.User as User

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts =
  [ sapientia
  ]

-- Host to configure.
sapientia :: Host
sapientia =
  host "sapientia" $
    props
      -- Debian OS
      -- & osDebian Unstable X86_64
      & osDebian (Stable "bookworm") X86_64
      & Apt.stdSourcesList -- `onChange` Apt.upgrade
      & File.fileProperty "Add non-free-firmware" fAptSources "/etc/apt/sources.list"
      & Apt.update
      & Apt.upgrade
      -- & Apt.unattendedUpgrades -- TODO Is this useful?
      -- File systems
      {- TODO enable on actual target
            & "/etc/crypttab"
              `File.hasContent` [ "cr-home UUID=75236c0e-cad4-43a7-986c-a5f82f68cf65 none luks"
                                ]
            & Fstab.mounted
              "ext4"
              "UUID=8148bec1-bb21-4202-bf99-8ad3c33d8c32"
              "/"
              (Fstab.MountOpts ["x-initrd.mount"]) -- mempty
            & Fstab.mounted
              "ext4"
              "UUID=6e2a0881-0a29-43e6-a8ed-44e1fa8909e6"
              "/boot"
              (Fstab.MountOpts ["defaults"]) -- mempty
            & Fstab.mounted
              "btrfs"
              "/dev/mapper/cr-home"
              "/home"
              (Fstab.MountOpts ["noatime,space_cache"]) -- mempty
            & Fstab.swap "UUID=493ad088-5b50-4aae-95b4-381a52292946"
      -}
      -- Install base packages (what remains with nix?)
      & Apt.installed
        [ "intel-microcode",
          "firmware-linux-free",
          "firmware-linux-nonfree",
          "firmware-misc-nonfree",
          "etckeeper",
          "nftables",
          "rsync",
          "ssh",
          "gpg",
          "tmux",
          "vim-nox", --TODO
          "xinit", --TODO
          "xterm",
          "alacritty", --TODO
          "i3",
          "nix-bin",
          "fonts-hack-ttf",
          "ormolu",
          "docker.io",
          -- "android-studio",
          -- "cabal-install",
          -- "cabal2nix",
          -- "ghc",
          -- "pkgs-unstable.pkg-config",
          -- "stack",
          -- "vivaldi",
          -- "vivaldi-ffmpeg-codecs",
          -- "wapm-cli",
          -- "wine",
          -- "winetricks",
          "aegisub",
          "android-file-transfer",
          "apktool",
          -- "appimage-run",
          "arandr",
          "aria2",
          "ark",
          "ascii",
          -- "aspellDicts.en",
          -- "aspellDicts.en-computers",
          -- "aspellDicts.en-science",
          -- "aspellDicts.nl",
          -- "aws-sam-cli",
          -- "awscli2",
          -- "bandwhich",
          -- "banner",
          "bat",
          "beep",
          -- "binutils-unwrapped",
          -- "bottom",
          -- "boxes",
          -- "brave",
          "brotli",
          "btrfs-heatmap",
          "btrfs-progs",
          -- "cachix",
          "calibre",
          -- "castnow",
          -- "cbonsai",
          "ccache",
          -- "cdrkit",
          "chromium",
          "cifs-utils",
          -- "cmatrix",
          -- "compsize",
          "cowsay",
          -- "cpdump",
          "cryptsetup",
          "darcs",
          -- "dbmate",
          -- "ddrescue",
          "dict",
          -- "dig",
          -- "digikam",
          -- "dillo",
          "direnv",
          "dmidecode",
          "docker",
          "docker-compose",
          "dos2unix",
          "duperemove",
          "dvdbackup",
          "e2fsprogs",
          "ecl",
          "entr",
          "exa",
          "exif",
          "exiv2",
          -- "exliveMinimal",
          -- "eza",
          "fd-find",
          "feh",
          "ffmpeg",
          "figlet",
          "file",
          "filezilla",
          -- "fortune",
          -- "freecad",
          -- "freetube",
          -- "freetype",
          "fzf",
          -- "gambit",
          -- "gcc_multi",
          "genisoimage",
          -- "gerbil",
          "gimp",
          -- "git", -- already installed
          "git-crypt",
          "git-lfs",
          -- "gitAndTools.gitRemoteGcrypt",
          -- "gitAndTools.tig",
          -- "gnumake",
          "gnupg",
          -- "go",
          "graphviz",
          "handbrake",
          "hashcat",
          -- "hashcat-utils",
          "hcxtools",
          -- "hddtemp",
          -- "hdparm",
          -- "heimdall-gui",
          "highlight",
          "hledger",
          "hledger-ui",
          "hlint",
          -- "html-tidy",
          "htop",
          "thunderbird",
          -- "hydra-check",
          "imagemagick",
          -- "inetutils",
          "iotop",
          -- "ipfs",
          -- "irccloud",
          "isync",
          -- "jellycli",
          -- "jellyfin",
          -- "jellyfin-media-player",
          -- "jp2a",
          "jq",
          -- "jujutsu",
          -- "just",
          -- "kate",
          -- "kcalc",
          -- "kdenlive",
          -- "kdiff3",
          "keepassxc",
          -- "killall",
          -- "kismet",
          -- "koreader",
          -- "lazygit",
          "ledger",
          "lftp",
          -- "librecad",
          "libreoffice",
          -- "librewolf",
          -- "libstemmer",
          -- "lm_sensors",
          "lshw",
          "lsof",
          "lsscsi",
          "lynis",
          "lynx",
          -- "mdcat",
          "mercurial",
          -- "metasploit",
          -- "microcodeIntel",
          "mkvtoolnix",
          "mpack",
          "mpv",
          -- "mpvScripts.quality-menu",
          -- "mpvScripts.sponsorblock",
          -- "musikcube",
          -- "mutt",
          "neofetch",
          -- "neovim",
          -- "neovim-qt",
          -- "nix-index",
          -- "nix-prefetch-scripts",
          "nmap",
          -- "nomacs",
          "notmuch",
          -- "nyxt",
          "offlineimap",
          -- "okei",
          -- "okular",
          -- "ollama",
          -- "ookla-speedtest",
          -- "opencascade-occt",
          -- "openh264",
          -- "openscad",
          "openssl",
          "openvpn",
          -- "opera",
          -- "or-browser-bundle-bin",
          -- "oterm",
          "p7zip",
          "pandoc",
          -- "paperwork",
          "par",
          "pass",
          -- "pavucontrol",
          "pciutils",
          "pcmanfm",
          -- "pcre",
          "emacs",
          -- "racket",
          -- "plantuml-c4",
          "plantuml",
          -- "pmutils",
          "poppler-utils",
          -- "procs",
          "psensor",
          "psmisc",
          -- "pstree",
          "pv",
          -- "python3", -- already installed
          -- "python310Packages.ipython ",
          -- "ranger",
          "translate-shell",
          "rawtherapee",
          -- "rclone",
          "tree",
          "rename",
          "restic",
          "ripgrep",
          "rmlint",
          -- "rustup",
          -- "sabnzbd",
          "sabnzbdplus",
          "safecopy",
          "screen",
          "scrot",
          "silversearcher-ag",
          -- "skim",
          "slrn",
          "smartmontools",
          "smem",
          "smemstat",
          -- "snapper",
          "socat",
          -- "sourceHighlight",
          "sqlite3",
          "sshfs",
          -- "subdl",
          -- "subtitleeditor",
          -- "sutils",
          "sysstat",
          -- "ums",
          "unzip",
          "urlscan",
          "usbutils",
          "vim",
          "virt-manager",
          "vlc",
          "vym",
          -- "wasmer",
          "wcalc",
          -- "weather",
          "wf-recorder",
          "wget",
          -- "wirelesstools",
          "wmctrl",
          "wpasupplicant",
          -- "xclip",
          "xdotool",
          -- "xlockmore",
          -- "xmobar",
          "x11-apps",
          "x11-utils",
          -- "xorg.xdpyinfo",
          -- "xorg.xev",
          -- "xorg.xeyes",
          -- "xorg.xhost",
          -- "xorg.xinit",
          -- "xorg.xkill",
          -- "xorg.xmessage",
          -- "xorg.xmodmap",
          -- "xorg.xwininfo",
          "xsane",
          "xsel",
          -- "yara",
          "yt-dlp",
          "zathura"
          -- "zellij"
        ]
      -- Users and groups
      & User.hasSomePassword (User "root")
      & User.accountFor (User "mdo")
      & User.hasSomePassword (User "mdo")
      & User.accountFor (User "csp")
      & User.hasSomePassword (User "csp")
      -- Sudo
      & Sudo.enabledFor (User "mdo")
      -- Nix
      & Group.hasUser (Group "docker") (User "mdo")
      & Group.hasUser (Group "libvirt") (User "mdo")
      & Group.hasUser (Group "kvm") (User "mdo")
      & Group.hasUser (Group "nix-users") (User "mdo")
      & File.dirExists "/home/mdo/.config"
      -- TODO i3/config and i3status/config ?
      {-
      & File.dirExists "/home/mdo/.config/i3"
      & File.dirExists "/home/mdo/.config/i3status"
      & "/home/mdo/.config/i3/config"
        `File.hasContent` lines i3Config
      & "/home/mdo/.config/i3status/config"
        `File.hasContent` lines i3StatusConfig
      -}
      & File.dirExists "/home/mdo/.config/nix"
      & "/home/mdo/.config/nix/nix.conf"
        `File.containsLines` [ "extra-experimental-features = nix-command",
                               "extra-experimental-features = flakes"
                             ]
      & File.ownerGroup "/home/mdo/.config/nix/nix.conf" (User "mdo") (Group "mdo")
      & Ssh.passwordAuthentication False
      & Ssh.setSshdConfig "PermitRootLogin" "prohibit-password"
      & File.fileProperty "Add sshd config Match block" fSshdMatch "/etc/ssh/sshd_config"
      -- Public key
      & Ssh.authorizedKey (User "mdo") "ecdsa-sha2-nistp521 AAAAE2VjZHNhLXNoYTItbmlzdHA1MjEAAAAIbmlzdHA1MjEAAACFBAFmvV41MBn9RoSWkUFnID+XafA7KqOf2wQhQnET1evIdjo8AIaSV5tjZ0strLZ6NjWayOU1JgjFCXfRJn+qq12vqgGgOF0i/9+R7GXnHMAoSktQiWvKwEFXuxTKqWv9g/tjrqGuxWNIDrYP+VD83k8qfseaLIWvkxWUQD4Tp6V7eRbVCA== u0_a75@localhost"
      -- Firewall
      & "/etc/nftables.conf"
        `File.hasContent` lines nftRules
      -- Timezone
      & "/etc/timezone"
        `File.hasContent` ["Europe/Amsterdam"]
      -- Systemd
      & Systemd.installed
      & Apt.serviceInstalledRunning "ntp"
      & Systemd.enabled "nftables"
      & Apt.serviceInstalledRunning "nftables"
      -- TODO What is this for?
      & Cron.runPropellor (Cron.Times "30 * * * *")
  where
    fAptSources :: [File.Line] -> [File.Line]
    fAptSources = map f
      where
        replaceAll [] _ _ = []
        replaceAll input from to =
          if from `isPrefixOf` input
            then to ++ replaceAll (drop (length from) input) from to
            else head input : replaceAll (tail input) from to
        f l = replaceAll l "non-free" "non-free non-free-firmware"

    fSshdMatch :: [File.Line] -> [File.Line]
    fSshdMatch inputLines =
      if alreadyPresent propellorMark inputLines
        then inputLines
        else findAndAdd "PermitRootLogin" addedLines inputLines
      where
        addedLines =
          [ propellorMark,
            "Match host 127.0.0.1",
            "    PasswordAuthentication yes",
            "    PermitRootLogin yes",
            "Match all"
          ]

        propellorMark = "# [Propellor match localhost root login with password]"
        alreadyPresent :: String -> [File.Line] -> Bool
        alreadyPresent _ [] = False
        alreadyPresent mark lns = any (\l -> mark `isInfixOf` l) lns

        -- TODO Commented out lines when an actual line is present.
        findAndAdd :: String -> [File.Line] -> [File.Line] -> [File.Line]
        findAndAdd _ _ [] = []
        findAndAdd keyword addLines (l : ls)
          | keyword `isInfixOf` l = l : addLines ++ ls
          | otherwise = l : findAndAdd keyword addLines ls

    {-
     -- TODO Read this from a configuration file
    i3Config =
      "TODO"

    -- TODO Read this from a configuration file
    i3StatusConfig =
      "TODO"
    -}

    -- TODO Read this from a configuration file
    nftRules =
      "#!/usr/sbin/nft -f\n\n\
      \table firewall {\n\
      \  chain incoming {\n\
      \    type filter hook input priority 0; policy drop;\n\
      \    # established/related connections\n\
      \    ct state established,related accept\n\
      \    # loopback interface\n\
      \    iifname lo accept\n\
      \    # icmp\n\
      \    icmp type echo-request accept\n\
      \    # open tcp ports: sshd (22), etc.\n\
      \    tcp dport { ssh, 1234, 3333, 8080, 8096 } accept\n\
      \  }\n\
      \}\n\n\
      \table ip6 firewall {\n\
      \  chain incoming {\n\
      \    type filter hook input priority 0; policy drop;\n\
      \    # established/related connections\n\
      \    ct state established,related accept\n\
      \    # invalid connections\n\
      \    ct state invalid drop\n\
      \    # loopback interface\n\
      \    iifname lo accept\n\
      \    # icmp\n\
      \    # routers may also want: mld-listener-query, nd-router-solicit\n\
      \    icmpv6 type { echo-request, nd-neighbor-solicit } accept\n\
      \    # open tcp ports: sshd (22), etc.\n\
      \    tcp dport { ssh, 1234, 3333, 8080, 8096 } accept\n\
      \  }\n\
      \}"
