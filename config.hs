-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

-- import qualified Propellor.Property.Cron as Cron
-- import qualified Propellor.Property.Firewall as Firewall
-- import qualified Propellor.PrivData as PrivData

import Bashrc (bashrcMdo, bashrcRoot)
import Data.List
import I3 (i3Config, i3StatusConfig)
import Nftables (nftRules)
import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Fstab as Fstab
import qualified Propellor.Property.Group as Group
import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.Laptop as Laptop
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.User as User
import Tmux (tmuxMdo)
import Xmonad (xmobarRc0, xmobarRc1, xmonadMdo)
import Xwindows (xInitrc, xModmap, xResources)

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
  host "sapientia.home" $
    props
      -- Debian OS
      -- & osDebian Unstable X86_64
      & osDebian (Stable "bookworm") X86_64
      & Laptop.powertopAutoTuneOnBoot -- TODO What does this do?
      -- & Laptop.trimSSD -- TODO don't have SSDs
      -- & Apt.stdSourcesList -- `onChange` Apt.upgrade
      & Grub.cmdline_Linux_default "i915.enable_psr=1" -- TODO What does this do?
        ! Grub.cmdline_Linux_default "quiet splash" -- TODO Does this work?
      & Systemd.persistentJournal -- TODO What does this do?
      -- LibreWolf from their repository - https://librewolf.net/installation/debian
      & Apt.installed ["lsb-release", "apt-transport-https", "ca-certificates", "wget"]
      & File.checkOverwrite File.PreserveExisting "/usr/share/keyrings/librewolf.gpg" fLibrewolf
      & "/etc/apt/sources.list.d/librewolf.sources"
        `File.hasContent` [ "Types: deb",
                            "URIs: https://deb.librewolf.net",
                            "Suites: bookworm",
                            "Components: main",
                            "Architectures: amd64",
                            "Signed-By: /usr/share/keyrings/librewolf.gpg"
                          ]
      -- Configure standard sources; update & upgrade
      & Apt.stdSourcesList
        `onChange` File.fileProperty "Add non-free-firmware" fAptSources "/etc/apt/sources.list"
      -- & Apt.unattendedUpgrades -- TODO Is this useful?
      & Apt.update
      & Apt.upgrade
      -- File systems for data partitions
      & "/etc/crypttab"
        `File.hasContent` ["cr-home UUID=75236c0e-cad4-43a7-986c-a5f82f68cf65 none luks"]
      & Fstab.mounted
        "btrfs"
        "/dev/mapper/cr-home"
        "/home"
        (Fstab.MountOpts ["noatime,space_cache"]) -- mempty
        -- Install base packages
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
          -- "guix", -- TODO fails with libssl3 dependency error
          "tmux",
          "vim-nox",
          "pulseaudio",
          "xinit", --TODO
          "xterm",
          "i3",
          "fonts-hack",
          "ormolu",
          "docker.io",
          "build-essential",
          -- "android-studio",
          -- "cabal-install",
          -- "ghc",
          -- "pkgs-unstable.pkg-config",
          -- "stack",
          -- "vivaldi", -- TODO where to get for Debian?
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
          -- "brave", -- TODO where to get for Debian?
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
          "clamav",
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
          -- "dillo", -- TODO where to get for Debian?
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
          "freecad",
          -- "freetube", -- TODO where to get for Debian?
          -- "freetype",
          "fzf",
          -- "gambit",
          -- "gcc_multi",
          "genisoimage",
          -- "gerbil",
          "gimp",
          "git-crypt",
          "git-lfs",
          "git-remote-gcrypt",
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
          "heimdall-flash",
          "heimdall-flash-frontend",
          "highlight",
          "hledger",
          "hledger-ui",
          "hlint",
          "tidy",
          "htop",
          "hugo",
          "thunderbird",
          "imagemagick",
          -- "inetutils",
          "iotop",
          -- "ipfs",
          -- "irccloud",
          "isync",
          -- "jellycli", -- TODO where to get Jellyfin for Debian?
          -- "jellyfin",
          -- "jellyfin-media-player",
          "jp2a",
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
          "librewolf",
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
          "mutt",
          "neofetch",
          -- "neovim",
          -- "neovim-qt",
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
          "plantuml", -- "plantuml-c4"
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
          "tig",
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
          -- "wirelesstools",
          "wmctrl",
          "wpasupplicant",
          -- "xclip",
          "xdotool",
          -- "xmobar",
          "x11-apps",
          "x11-utils",
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
      & Group.hasUser (Group "docker") (User "mdo")
      & Group.hasUser (Group "libvirt") (User "mdo")
      & Group.hasUser (Group "kvm") (User "mdo")
      & Group.hasUser (Group "adm") (User "mdo")
      & Group.hasUser (Group "nix-users") (User "mdo")
      -- Sudo
      & Sudo.enabledFor (User "mdo")
      -- Secrets (to be included from ~/.bashrc files)
      & "/home/mdo/.bashrc_secrets"
        `File.hasPrivContentExposed` (Context "sapientia.mdo.bashrc.secrets")
      -- Configuration files
      & File.dirExists "/root"
      & "/root/.bashrc"
        `File.hasContent` lines bashrcRoot
      & File.dirExists "/home/mdo"
      & "/home/mdo/.bashrc"
        `File.hasContent` lines bashrcMdo
      & "/home/mdo/.tmux.conf"
        `File.hasContent` lines tmuxMdo
      & "/home/mdo/.Xresources"
        `File.hasContent` lines xResources
      & "/home/mdo/.Xmodmap"
        `File.hasContent` lines xModmap
      & "/home/mdo/.xinitrc"
        `File.hasContent` lines xInitrc
      & File.dirExists "/home/mdo/.config"
      & File.dirExists "/home/mdo/.config/i3"
      & "/home/mdo/.config/i3/config"
        `File.hasContent` lines i3Config
      & File.dirExists "/home/mdo/.config/i3status"
      & "/home/mdo/.config/i3status/config"
        `File.hasContent` lines i3StatusConfig
      & File.dirExists "/home/mdo/.xmonad"
      & "/home/mdo/.xmonad/xmonad.hs"
        `File.hasContent` lines xmonadMdo
      & File.dirExists "/home/mdo/.config/xmobar"
      & "/home/mdo/.config/xmobar/xmobarrc0"
        `File.hasContent` lines xmobarRc0
      & "/home/mdo/.config/xmobar/xmobarrc1"
        `File.hasContent` lines xmobarRc1
      & File.dirExists "/home/mdo/.config/nix"
      & "/home/mdo/.config/nix/nix.conf"
        `File.containsLines` [ "extra-experimental-features = nix-command",
                               "extra-experimental-features = flakes"
                             ]
      & File.ownerGroup "/home/mdo/.config/nix/nix.conf" (User "mdo") (Group "mdo")
      -- Musikcube from downloaded archive
      & check
        (not <$> Apt.isInstalled "musikcube")
        ( cmdProperty "wget" ["https://github.com/clangen/musikcube/releases/download/3.0.4/musikcube_3.0.4_linux_x86_64.deb", "-O", "/root/musikcube_3.0.4_linux_x86_64.deb"]
            `assume` MadeChange
            `describe` "Musikcube archive downloaded"
        )
      & check
        (not <$> Apt.isInstalled "musikcube")
        ( cmdProperty "dpkg" ["-i", "/root/musikcube_3.0.4_linux_x86_64.deb"]
            `assume` MadeChange
            `describe` "Musikcube installed"
        )
      -- Timezone
      & "/etc/timezone"
        `File.hasContent` ["Europe/Amsterdam"]
      -- Systemd
      & Systemd.installed
      & Apt.serviceInstalledRunning "ntp"
      -- Firewall
      & Systemd.enabled "nftables"
      & Apt.serviceInstalledRunning "nftables"
      & "/etc/nftables.conf"
        `File.hasContent` lines nftRules
        `onChange` Systemd.restarted "nftables"
      -- SSH
      & Systemd.enabled "ssh"
      & Apt.serviceInstalledRunning "ssh"
      & Ssh.passwordAuthentication False
      & Ssh.setSshdConfig "PermitRootLogin" "prohibit-password"
      & File.fileProperty "Add sshd config Match block" fSshdMatch "/etc/ssh/sshd_config"
        `onChange` Systemd.restarted "ssh"
      -- Public key
      & Ssh.authorizedKey (User "mdo") "ecdsa-sha2-nistp521 AAAAE2VjZHNhLXNoYTItbmlzdHA1MjEAAAAIbmlzdHA1MjEAAACFBAFmvV41MBn9RoSWkUFnID+XafA7KqOf2wQhQnET1evIdjo8AIaSV5tjZ0strLZ6NjWayOU1JgjFCXfRJn+qq12vqgGgOF0i/9+R7GXnHMAoSktQiWvKwEFXuxTKqWv9g/tjrqGuxWNIDrYP+VD83k8qfseaLIWvkxWUQD4Tp6V7eRbVCA== u0_a75@localhost"
  where
    -- TODO What is this for exactly?
    -- & Cron.runPropellor (Cron.Times "30 * * * *")

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
        else inputLines <> addedLines
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

    fLibrewolf :: FilePath -> Property UnixLike
    fLibrewolf p =
      scriptProperty ["wget https://deb.librewolf.net/keyring.gpg -O- | gpg --dearmor -o " <> p]
        `assume` MadeChange
        `describe` "Librewolf repository key downloaded and saved"
