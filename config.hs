-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Bashrc (bashrcMdo, bashrcRoot)
import Data.List
import I3 (i3Config, i3StatusConfig)
import Muttrc (muttrcMdo)
import Nftables (nftRules)
import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Fstab as Fstab
import qualified Propellor.Property.Group as Group
-- import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.Laptop as Laptop
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.User as User
import Text.Regex (Regex, mkRegex, subRegex)
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
      -- & Grub.cmdline_Linux_default "i915.enable_psr=1" -- TODO What does this do?
      -- ! Grub.cmdline_Linux "quiet splash" -- TODO Does this work?
      & Systemd.persistentJournal -- TODO What does this do?
      & Apt.installed ["lsb-release", "apt-transport-https", "ca-certificates", "wget"]
      -- Librewolf webbrowser from their repository - https://librewolf.net/debian-migration/
      -- One time manual command may be required: apt-get update --allow-releaseinfo-change
      & Apt.installed ["extrepo"]
      & File.checkOverwrite File.PreserveExisting "/etc/apt/sources.list.d/extrepo_librewolf.sources" fLibrewolf
      {-
      -- Opera webbrowser from their repository - https://deb.opera.com/manual.html
      -- (remove redundant sources.list that is installed by the opera package)
      & File.notPresent "/etc/apt/sources.list.d/opera-stable.list"
      & File.checkOverwrite File.PreserveExisting "/usr/share/keyrings/opera-browser.gpg" fOpera
      & "/etc/apt/sources.list.d/opera-archive.list"
        `File.hasContent` [ "deb [signed-by=/usr/share/keyrings/opera-browser.gpg] https://deb.opera.com/opera-stable/ stable non-free"
                          ]
      -- It may have been recreated... (use extreme prejudice, just to be safe)
      & File.notPresent "/etc/apt/sources.list.d/opera-stable.list"
      -}
      -- Vivaldi webbrowser from their repository - https://itsfoss.com/install-vivaldi-ubuntu-linux/
      & File.checkOverwrite File.PreserveExisting "/usr/share/keyrings/vivaldi-browser.gpg" fVivaldi
      & "/etc/apt/sources.list.d/vivaldi-archive.list"
        `File.hasContent` [ "deb [signed-by=/usr/share/keyrings/vivaldi-browser.gpg arch=amd64] https://repo.vivaldi.com/archive/deb/ stable main"
                          ]
      -- Brave from their repository - https://brave.com/linux/
      & File.checkOverwrite File.PreserveExisting "/usr/share/keyrings/brave-browser-archive-keyring.gpg" fBrave
      & "/etc/apt/sources.list.d/brave-browser-release.list"
        `File.hasContent` [ "deb [signed-by=/usr/share/keyrings/brave-browser-archive-keyring.gpg] https://brave-browser-apt-release.s3.brave.com/ stable main"
                          ]
      -- Jellycli pre-built binary from GitHub
      -- https://github.com/tryffel/jellycli
      & File.checkOverwrite File.PreserveExisting "/usr/local/bin/jellycli" fJellycli
      -- Jellyfin from their repository
      -- https://linuxcapable.com/how-to-install-jellyfin-media-server-on-debian-linux/
      & File.checkOverwrite File.PreserveExisting "/usr/share/keyrings/jellyfin.gpg" fJellyfin
      & "/etc/apt/sources.list.d/jellyfin.sources"
        `File.hasContent` [ "Types: deb",
                            "URIs: https://repo.jellyfin.org/debian",
                            "Suites: bookworm",
                            "Components: main",
                            "Architectures: amd64",
                            "Signed-By: /usr/share/keyrings/jellyfin.gpg"
                          ]
      -- NodeJS from their installer
      -- https://github.com/nodesource/distributions
      & File.checkOverwrite File.PreserveExisting "/etc/apt/sources.list.d/nodesource.list" fNodeJS
      -- Configure standard sources; update & upgrade
      & Apt.stdSourcesList
      -- No longer needed with latest Propellor version (directly from git master)
      -- `onChange` File.fileProperty "Add non-free-firmware" fAptSources "/etc/apt/sources.list"
      -- & Apt.unattendedUpgrades -- TODO Is this useful?
      & Apt.cacheCleaned
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

      -- Remove `user_readenv=1` from the `session required` line in /etc/pam.d/sshd
      -- `session    required     pam_env.so user_readenv=1 envfile=/etc/default/locale`
      -- See: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=1018106
      & File.fileProperty "Remove user_readenv=1" fPamSshd "/etc/pam.d/sshd"
      -- Docker
      & Docker.installed
      -- Install base packages
      & Apt.installed
        [ "adb",
          "android-file-transfer",
          -- "android-sdk",
          -- "android-sdk-platform-tools-common",
          "apktool",
          "apt-listbugs",
          "apt-listchanges",
          "arandr",
          "aria2",
          "ark",
          "ascii",
          "awscli",
          "bat",
          "beep",
          "brave-browser",
          "brotli",
          "btrfsmaintenance",
          "btrfs-heatmap",
          "btrfs-progs",
          "build-essential",
          -- "cabal-install", -- TODO
          "calibre",
          "ccache",
          "chromium",
          -- "chromium-codecs-ffmpeg-extra", -- TODO suggested but not available?
          "cifs-utils",
          "clamav",
          "cowsay",
          "cryptsetup",
          "darcs",
          "ddrescueview",
          "ddrutility",
          "default-jdk",
          "dict",
          "direnv",
          "dmidecode",
          "docker-compose",
          "dos2unix",
          "duperemove",
          "dvdbackup",
          "e2fsprogs",
          "ecl",
          "emacs",
          "entr",
          "etckeeper",
          "exa",
          "exif",
          "exiv2",
          "falkon",
          "fd-find",
          "feh",
          "ffmpeg",
          "figlet",
          "file",
          "filezilla",
          "firmware-linux-free",
          "firmware-linux-nonfree",
          "firmware-misc-nonfree",
          "fonts-hack",
          "fonts-noto-hinted",
          "fortune-mod",
          "fortunes",
          "freecad",
          "fzf",
          "gddrescue",
          "genisoimage",
          -- "ghc", -- TODO
          "gimp",
          "git-crypt",
          "git-lfs",
          "git-remote-gcrypt",
          "gnupg",
          "gpg",
          "graphviz",
          -- "gwenview",
          -- "guix",
          "handbrake-cli",
          "hashcat",
          "hcxtools",
          "heimdall-flash",
          "heimdall-flash-frontend",
          "highlight",
          "hledger",
          "hledger-ui",
          "hlint",
          "horst",
          "hplip",
          "htop",
          "hugo",
          -- "i3",
          "igal2",
          "imagemagick",
          "intel-microcode",
          "iotop",
          "isync",
          "jami",
          "jellyfin",
          "jp2a",
          "jq",
          -- "kdenlive",
          -- "kdiff3",
          "keepassxc",
          "ldraw-parts",
          "ledger",
          "leocad",
          "lftp",
          "libghc-xmonad-contrib-dev",
          "libpq-dev",
          "librecad",
          "libreoffice",
          "librewolf",
          "libsdl2-dev",
          "lksctp-tools",
          "lshw",
          "lsof",
          "lsscsi",
          "lynis",
          "lynx",
          "mercurial",
          "mkvtoolnix",
          "mpack",
          "mpv",
          "mutt",
          "myrescue",
          "ncal",
          "needrestart",
          "needrestart-session",
          "neofetch",
          "net-tools",
          "network-manager",
          "nftables",
          "nmap",
          "nodejs",
          "notmuch",
          "offlineimap",
          "okular",
          "openscad",
          "openssl",
          "openvpn",
          -- "opera-stable",
          "ormolu",
          "p7zip",
          "pandoc",
          "par",
          "pass",
          "pciutils",
          "pcmanfm",
          "plantuml", -- "plantuml-c4"
          "poppler-utils",
          "psensor",
          "psmisc",
          "pulseaudio",
          "pulsemixer",
          "pv",
          "python3-pip",
          "qnapi",
          "rawtherapee",
          "rename",
          "restic",
          "rfkill",
          "ripgrep",
          "rmlint",
          "rsync",
          "safecopy",
          "screen",
          "scrot",
          "sdkmanager",
          "silversearcher-ag",
          "slrn",
          "smartmontools",
          "smem",
          "smemstat",
          "snapd", -- see further below for installation of snap packages
          "socat",
          "speedtest-cli",
          "sqlite3",
          "ssh",
          "sshfs",
          "subdownloader",
          "subliminal",
          "subtitlecomposer",
          "suckless-tools",
          "sweethome3d",
          "sweethome3d-furniture",
          "sweethome3d-furniture-editor",
          "sweethome3d-furniture-nonfree",
          "sysstat",
          "sysvbanner",
          "tcpdump",
          "testdisk",
          "thunderbird",
          "tidy",
          "tig",
          "tmux",
          "torbrowser-launcher",
          "translate-shell",
          "tree",
          "unzip",
          "urlscan",
          "urlview",
          "usbutils",
          "vim",
          "vim-nox",
          "virt-manager",
          "vivaldi-stable",
          "vlc",
          "vym",
          "wcalc",
          "wf-recorder",
          "wmctrl",
          "wpasupplicant",
          "x11-apps",
          "x11-utils",
          "xdotool",
          "xinit", --TODO
          "xinput",
          "xmobar",
          "xmonad",
          "xsane",
          "xsel",
          "xterm",
          "ytfzf",
          "zathura"
          -- "appimage-run",
          -- "aspellDicts.en",
          -- "aspellDicts.en-computers",
          -- "aspellDicts.en-science",
          -- "aspellDicts.nl",
          -- "bandwhich",
          -- "binutils-unwrapped",
          -- "bottom",
          -- "boxes",
          -- "cbonsai",
          -- "cmatrix",
          -- "compsize",
          -- "cpdump",
          -- "dbmate",
          -- "digikam",
          -- "exliveMinimal",
          -- "fortune",
          -- "freetype",
          -- "gambit",
          -- "gcc_multi",
          -- "gerbil",
          -- "hashcat-utils",
          -- "hdparm",
          -- "inetutils",
          -- "ipfs",
          -- "irccloud",
          -- "jujutsu",
          -- "just",
          -- "kate",
          -- "kdenlive",
          -- "kdiff3",
          -- "killall",
          -- "kismet",
          -- "lazygit",
          -- "librecad",
          -- "libstemmer",
          -- "lm_sensors",
          -- "mdcat",
          -- "metasploit",
          -- "neovim",
          -- "neovim-qt",
          -- "nomacs",
          -- "nyxt",
          -- "okei",
          -- "okular",
          -- "ollama",
          -- "ookla-speedtest",
          -- "opencascade-occt",
          -- "openh264",
          -- "oterm",
          -- "paperwork",
          -- "pavucontrol",
          -- "pcre",
          -- "pmutils",
          -- "procs",
          -- "pstree",
          -- "racket",
          -- "ranger",
          -- "rclone",
          -- "rustup",
          -- "skim",
          -- "snapper",
          -- "sourceHighlight",
          -- "subdl",
          -- "subtitleeditor",
          -- "sutils",
          -- "ums",
          -- "wapm-cli",
          -- "wasmer",
          -- "weather",
          -- "wine",
          -- "winetricks",
          -- "wirelesstools",
          -- "xclip",
          -- "yara",
          -- "zellij"
        ]
      -- Backports packages
      & Apt.backportInstalled
        [ "sabnzbdplus",
          "yt-dlp"
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
      -- Use with lix — https://git.lix.systems/lix-project/lix
      -- & Group.hasUser (Group "nix-users") (User "mdo")
      -- Sudo'ers
      & Sudo.enabledFor (User "mdo")
      -- Secrets (to be included from ~/.bashrc files)
      --   ./propellor --list-fields
      --   ./propellor --set 'PrivFile "/home/mdo/.bashrc_secrets"' 'sapientia.mdo.bashrc.secrets' < source-for-bashrc_secrets
      --   ./propellor --dump 'PrivFile "/home/mdo/.bashrc_secrets"' 'sapientia.mdo.bashrc.secrets'
      --   ./propellor --edit 'PrivFile "/home/mdo/.bashrc_secrets"' 'sapientia.mdo.bashrc.secrets'
      & "/home/mdo/.bashrc_secrets"
      `File.hasPrivContentExposed` (Context "sapientia.mdo.bashrc.secrets")
        -- Configuration files
        & File.dirExists "/root"
        & "/root/.bashrc"
          `File.hasContent` lines bashrcRoot
        & File.dirExists "/home/mdo"
        & "/home/mdo/.bashrc"
          `File.hasContent` lines bashrcMdo
        & File.dirExists "/home/mdo/.mutt"
        & "/home/mdo/.mutt/account.org.photonsphere.contact"
      `File.hasPrivContentExposed` (Context "sapientia.mdo.muttrc.account.org.photonsphere.contact.secrets")
        & "/home/mdo/.mutt/account.nl.donkersautomatisering.info"
      `File.hasPrivContentExposed` (Context "sapientia.mdo.muttrc.account.nl.donkersautomatisering.info.secrets")
        & "/home/mdo/.mutt/account.com.donkersphotography.contact"
      `File.hasPrivContentExposed` (Context "sapientia.mdo.muttrc.account.com.donkersphotography.contact.secrets")
        & "/home/mdo/.mutt/muttrc"
          `File.hasContent` lines muttrcMdo
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
        {-
        & File.dirExists "/home/mdo/.config/nix"
        & "/home/mdo/.config/nix/nix.conf"
        `File.containsLines` [ "extra-experimental-features = nix-command",
                               "extra-experimental-features = flakes"
                             ]
        & File.ownerGroup "/home/mdo/.config/nix/nix.conf" (User "mdo") (Group "mdo")
        -}
        -- Musikcube from downloaded archive
        -- TODO Get latest release as documented here: https://docs.github.com/en/repositories/releasing-projects-on-github/linking-to-releases
        -- TODO Determine asset name for latest release by reading HTML or perhaps get tag? Also checking if already installed does not really suffice (compare installed version against potential newer version?)
        -- https://hackage.haskell.org/package/req-3.2.0/docs/Network-HTTP-Req.html
        -- Julia from their installer
        -- https://docs.julialang.org/en/v1.11/manual/installation/
        & File.mode "/home/mdo/.bashrc" 700 -- or FileWriteMode ?
        & File.checkOverwrite File.PreserveExisting "/home/mdo/.juliaup/bin/julia" (fJulia "mdo")
        & File.mode "/home/mdo/.bashrc" 500 -- or FileWriteMode ?
        & check
          (not <$> Apt.isInstalled "musikcube")
          -- Via: https://github.com/clangen/musikcube/releases/latest/musikcube_3.0.4_linux_x86_64.deb"
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
        -- KOReader from downloaded archive latest working for Debian
        -- https://github-wiki-see.page/m/koreader/koreader/wiki/Installation-on-desktop-Linux
        & check
          (not <$> Apt.isInstalled "koreader")
          ( cmdProperty "wget" ["https://github.com/koreader/koreader/releases/download/v2024.04/koreader-2024.04-amd64.deb", "-O", "/root/koreader-2024.04-amd64.deb"]
              `assume` MadeChange
              `describe` "KOReader archive downloaded"
          )
        & check
          (not <$> Apt.isInstalled "koreader")
          ( cmdProperty
              "dpkg"
              ["-i", "/root/koreader-2024.04-amd64.deb"]
              `assume` MadeChange
              `describe` "KOReader installed"
          )
        -- https://unix.stackexchange.com/questions/651673/install-guixsd-using-an-existing-linux-system
        & File.checkOverwrite File.PreserveExisting "/usr/local/bin/guix" fGuix -- TODO
        & "/home/mdo/.config/guix/channels.scm"
        `File.containsLines` [ "(cons* (channel",
                               "        (name 'nonguix)",
                               "        (url \"https://gitlab.com/nonguix/nonguix\")",
                               "        ;; Enable signature verification:",
                               "        (introduction",
                               "         (make-channel-introduction",
                               "          \"897c1a470da759236cc11798f4e0a5f7d4d59fbc\"",
                               "          (openpgp-fingerprint",
                               "           \"2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5\"))))",
                               "       %default-channels)"
                             ]
        -- Install Haskell Stack -- https://docs.haskellstack.org/en/stable/install_and_upgrade/
        -- TODO Install ghcup instead? (installs non-root?) https://www.haskell.org/ghcup/
        -- These commands should be run as non-root/non-admin user:
        -- curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
        -- Forget it: install ghcup (and stack via it) manually
        -- & File.checkOverwrite File.PreserveExisting "/usr/local/bin/stack" fHaskellStack
        -- & File.checkOverwrite File.PreserveExisting "~/.ghcup" (fGhcup "mdo") -- TODO
        -- Install snap packages
        & propertyList
          "Install snap packages"
          ( toProps $
              map
                ( \p ->
                    cmdProperty "snap" ["install", p]
                      `assume` MadeChange
                      `describe` (p <> " installed")
                )
                -- Put snap packages to install here.
                [ "freetube",
                  "whatsdesk"
                ]
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

-- & Cron.runPropellor (Cron.Times "30 * * * *") -- TODO What is this for exactly?

-- No longer needed with latest Propellor version (directly from git master)
{-
fAptSources :: [File.Line] -> [File.Line]
fAptSources = map f
  where
    replaceAll [] _ _ = []
    replaceAll input from to =
      if from `isPrefixOf` input
        then to ++ replaceAll (drop (length from) input) from to
        else head input : replaceAll (tail input) from to
    f l = replaceAll l " non-free" " non-free non-free-firmware"
-}

fPamSshd :: [File.Line] -> [File.Line]
fPamSshd = map (replaceAll (mkRegex "^(session\\s+required\\s+pam_env.so)\\s+user_readenv=1") "\\1")
  where
    replaceAll :: Regex -> String -> File.Line -> File.Line
    replaceAll regex replacement source = subRegex regex source replacement

fSshdMatch :: [File.Line] -> [File.Line]
fSshdMatch inputLines =
  if alreadyPresent propellorMark inputLines
    then inputLines
    else inputLines <> addedLines
  where
    propellorMark = "# [Propellor match localhost root login with password]"
    addedLines =
      [ propellorMark,
        "Match host 127.0.0.1",
        "    PasswordAuthentication yes",
        "    PermitRootLogin yes",
        "Match all"
      ]
    alreadyPresent :: String -> [File.Line] -> Bool
    alreadyPresent _ [] = False
    alreadyPresent mark lns = any (\l -> mark `isInfixOf` l) lns

fLibrewolf :: FilePath -> Property UnixLike
fLibrewolf _ =
  scriptProperty ["extrepo enable librewolf"]
    `assume` MadeChange
    `describe` "Librewolf repository enabled with extrepo"

{-
fOpera :: FilePath -> Property UnixLike
fOpera p =
  scriptProperty ["wget https://deb.opera.com/archive.key -qO- | gpg --dearmor -o " <> p]
    `assume` MadeChange
    `describe` "Opera repository key downloaded and saved"
-}

fVivaldi :: FilePath -> Property UnixLike
fVivaldi p =
  scriptProperty ["wget https://repo.vivaldi.com/archive/linux_signing_key.pub -O- | gpg --dearmor -o " <> p]
    `assume` MadeChange
    `describe` "Vivaldi repository key downloaded and saved"

fBrave :: FilePath -> Property UnixLike
fBrave p =
  scriptProperty ["wget https://brave-browser-apt-release.s3.brave.com/brave-browser-archive-keyring.gpg -O- | gpg --dearmor -o " <> p]
    `assume` MadeChange
    `describe` "Brave repository key downloaded and saved"

-- TODO Yes, this needs to be manually changed when a new version is published...
fJellycli :: FilePath -> Property UnixLike
fJellycli p =
  scriptProperty ["wget https://github.com/tryffel/jellycli/releases/download/v0.9.1/jellycli_0.9.1_Linux_x86_64 -O " <> p]
    `assume` MadeChange
    `describe` "Jellycli binary downloaded and saved"

fJellyfin :: FilePath -> Property UnixLike
fJellyfin p =
  scriptProperty ["wget https://repo.jellyfin.org/debian/jellyfin_team.gpg.key -O- | gpg --dearmor -o " <> p]
    `assume` MadeChange
    `describe` "Jellyfin repository key downloaded and saved"

fNodeJS :: FilePath -> Property UnixLike
fNodeJS _ =
  scriptProperty ["curl -sSL https://deb.nodesource.com/setup_22.x | sh"]
    `assume` MadeChange
    `describe` "NodeJS downloaded and installed"

fJulia :: String -> FilePath -> Property UnixLike
fJulia user _ =
  userScriptProperty (User user) ["curl -fsSL https://install.julialang.org | sh -s -- --yes"]
    `assume` MadeChange
    `describe` ("User: " <> user <> " -- Julia downloaded and installed")

fGuix :: FilePath -> Property UnixLike
fGuix _ =
  scriptProperty ["curl -sSL https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh -o ~/guix-install.sh && chmod u+x ~/guix-install.sh && ~/guix-install.sh"]
    `assume` MadeChange
    `describe` "Guix downloaded and installed"

{-
fGhcup :: FilePath -> String -> Property UnixLike
fGhcup user _ =
  userScriptProperty (User user) ["curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"]
    `assume` MadeChange
    `describe` "ghcup downloaded and installed"
-}

{-
fHaskellStack :: FilePath -> Property UnixLike
fHaskellStack _ =
  scriptProperty ["curl -sSL https://get.haskellstack.org/ | sh"]
    `assume` MadeChange
    `describe` "Haskell stack downloaded and installed"
-}
