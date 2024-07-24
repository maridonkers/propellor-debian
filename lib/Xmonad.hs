{-# LANGUAGE QuasiQuotes #-}

-- Xmonad.hs
module Xmonad (xmonadMdo, xmobarRc0, xmobarRc1) where

import Quasiquote (config)

xmonadMdo :: String
xmonadMdo =
  [config|-- This file is under control of Propellor.

--------------------------------------------------------------------------------
-- | xmonad.hs
--
-- This configuration file for xmonad is adapted from DT's dotfiles.
-- See: https://gitlab.com/dwt1/dotfiles
--
-- hlint xmonad.hs
--------------------------------------------------------------------------------
-- Examples:
-- https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips
-- https://wiki.haskell.org/Xmonad/Config_archive/adamvo%27s_xmonad.hs
-- https://wiki.haskell.org/Xmonad/Config_archive/dmwit%27s_xmonad.hs
-- https://gitlab.com/dwt1/dotfiles  (xmonad and xmobar configuration)
-- https://xiangji.me/2018/11/19/my-xmonad-configuration/
-- https://github.com/xmonad/xmonad/issues/245
--
-- To display key strokes use the xev program.
--
{-# OPTIONS_GHC -Wall -fwarn-unused-imports #-}

import qualified GHC.IO.Encoding                    as GIO
import           System.Exit
import           System.IO                          (hPutStrLn)

-- import           Data.Char                          (isSpace)
-- import Data.Typeable
import qualified Data.Map                           as M

import           Text.Printf

-- import qualified Debug.Trace                        as D
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.GridSelect
-- import           XMonad.Actions.Minimize
import           XMonad.Actions.MouseResize
import           XMonad.Actions.NoBorders
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-OnScreen.html
--import XMonad.Actions.OnScreen
import           XMonad.Actions.RotSlaves           (rotAllDown, rotSlavesDown)
import           XMonad.Actions.SwapWorkspaces
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog            (PP (..), dynamicLogWithPP,
                                                     shorten, wrap, xmobarColor,
                                                     xmobarPP)
-- import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import           XMonad.Hooks.ManageDocks           (ToggleStruts (..),
                                                     avoidStruts, docks,
                                                     manageDocks)
import           XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.ScreenCorners
-- import XMonad.Hooks.SetWMName
import           XMonad.Layout.BinarySpacePartition
-- import qualified XMonad.Layout.BoringWindows as BW
import           XMonad.Layout.Grid                 (Grid (..))
import           XMonad.Layout.IndependentScreens
-- import XMonad.Layout.LayoutModifier (ModifiedLayout)
-- import           XMonad.Layout
-- import           XMonad.Layout.Minimize
import           XMonad.Hooks.RefocusLast           (--isFloat,
                                                     refocusLastLayoutHook,
                                                     -- refocusLastWhen
                                                    )
import           XMonad.Layout.NoBorders            (hasBorder, noBorders,
                                                     smartBorders)
import           XMonad.Layout.ResizableTile        (ResizableTall (..))
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ToggleLayouts        (ToggleLayout (..),
                                                     toggleLayouts)
import           XMonad.Layout.TrackFloating
import           XMonad.Layout.MultiColumns
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.WindowArranger       (windowArrange)
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Input
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet                    as W
import           XMonad.Util.EZConfig
-- import           XMonad.Util.PositionStore
import           XMonad.Util.Run                    (spawnPipe)
import           XMonad.Util.SpawnOnce

------------------------------------------------------------------------
-- DEFINITIONS
------------------------------------------------------------------------

myTerminal :: String
-- myTerminal = "termonad"
-- myTerminal = "xterm -e \"exec tmux\""
myTerminal = "xterm"

myFloatingTerminal :: String
myFloatingTerminal = "xterm -title \"floatterm\""

{-
-- https://github.com/SimSaladin/configs/blob/646a363ed2f47db190e41a4ed58808687f92e0dd/.xmonad/xmonad.hs
-- | Float current according to saved position
myFloatCurrent :: X ()
myFloatCurrent = withFocused $ \window -> withWindowSet $ \ws -> do
    ps <- getPosStore
    let sr@(Rectangle _srX _srY srW srH) = screenRect . W.screenDetail $ W.current ws
    case posStoreQuery ps window sr of
        Just (Rectangle x y w h) -> do
            let r' = W.RationalRect (fromIntegral x / fromIntegral srW)
                                    (fromIntegral y / fromIntegral srH)
                                    (fromIntegral w / fromIntegral srW)
                                    (fromIntegral h / fromIntegral srH)
            io $ writeFile "/tmp/xm" (show r')
            windows $ W.float window r'
        Nothing  -> return ()

-- | Save float position of the window
saveFloatPosition :: Window -> X ()
saveFloatPosition window = do
    sr <- withWindowSet $ return . screenRect . W.screenDetail . W.current
    (_, rect) <- floatLocation window
    modifyPosStore $ \ps -> posStoreInsert ps window (scaleRationalRect sr rect) sr
-}

-- https://www.reddit.com/r/xmonad/comments/hm2tg0/how_to_toggle_floating_state_on_a_window/
toggleFloat :: Window -> X()
toggleFloat w = do
  -- (_, rr) <- floatLocation w
  let rr = W.RationalRect 0 0 (1/2) (1/2)
  windows (\s -> if M.member w (W.floating s)
                 then W.sink w s
                 else W.float w rr s)

mySpacing :: Integer
mySpacing = 2

myBorderWidth :: Dimension
myBorderWidth = 3

myFocusedBorderColor :: String
myFocusedBorderColor = "#ff0000"

myEditor :: String
myEditor = "bin/ec"

myScreendump :: String
myScreendump = "scrot"

myFloatingGHCI :: String
myFloatingGHCI = "xterm -title \"floatterm\" -e \"exec ghci\""

myFloatingPython :: String
myFloatingPython = "xterm -title \"floatterm\" -e \"exec python\""

myFileManager :: String
myFileManager = "pcmanfm"

myLibreOffice :: String
myLibreOffice = "libreoffice"

myMusicPlayer :: String
myMusicPlayer = "xterm -title \"musikcube\" -e \"exec musikcube\""

myKeepassXc :: String
myKeepassXc = "keepassxc"

myThunderbird :: String
myThunderbird = "thunderbird"

myFirefox :: String
myFirefox = "firefox"

myOpera :: String
myOpera = "opera"

myNyxt :: String
myNyxt = "nyxt"

myChromium :: String
myChromium = "chromium"

myLibreWolf :: String
myLibreWolf = "librewolf"

myBrave :: String
myBrave = "brave"

myVivaldi :: String
myVivaldi = "vivaldi"

myYoutubeBrowser :: String
myYoutubeBrowser = "librewolf --new-window https://youtube.com"

myFilezilla :: String
myFilezilla = "filezilla"

myTorBrowser :: String
myTorBrowser = "tor-browser"

myPtEuronews :: String
myPtEuronews = "mpv https://www.youtube.com/watch?v=fLtn2L7OdeI&pp=ygUYZXVyb25ld3MgbGl2ZSBwb3J0dWd1ZXNl &"

myEsDw :: String
myEsDw = "mpv https://www.youtube.com/watch?v=tsStUN73_6I &"
myEsRtve :: String
myEsRtve = "mpv https://www.youtube.com/watch?v=mzdfGCdNSHQ &"
myEsFrance24 :: String
myEsFrance24 = "mpv https://www.youtube.com/live/Y-IlMeCCtIg &"
myEsEuronews :: String
myEsEuronews = "mpv https://www.youtube.com/watch?v=O9mOtdZ-nSk&pp=ygUVZXVyb25ld3MgbGl2ZSBzcGFuaXNo &"

myFrFrance24 :: String
myFrFrance24 = "mpv https://www.youtube.com/live/l8PMl7tUDIE &"
myFrFranceinfo :: String
myFrFranceinfo = "mpv https://www.youtube.com/watch?v=Z-Nwo-ypKtM &"
myFrEuronews :: String
myFrEuronews = "mpv https://www.youtube.com/watch?v=NiRIbKwAejk &"

myEnFrance24 :: String
myEnFrance24 = "mpv https://www.youtube.com/watch?v=h3MuIUNCCzI&pp=ygUPZnJhbmNlIDI0IGxpdmUg &"
myEnEuronews :: String
myEnEuronews = "mpv https://www.youtube.com/watch?v=pykpO5kQJ98&pp=ygUVZXVyb25ld3MgbGl2ZSBzcGFuaXNo &"
myEnDw :: String
myEnDw = "mpv https://www.youtube.com/watch?v=pqabxBKzZ6M&pp=ygUIZHcgbGl2ZSA%3D &"

-- myRedshiftOn :: String
-- myRedshiftOn = "redshift"

-- myRedshiftOff :: String
-- myRedshiftOff = "redshift ; redshift -x"

myScreensaver :: String
myScreensaver = "slock"

myScreenBlank :: String
myScreenBlank = "sleep 1; xset dpms force off"

myCursorToggle :: String
myCursorToggle = "~/bin/togglecursor"

-- logCommand :: String
-- logCommand = "echo \"" ++ (show (typeOf defaults)) ++ "\" > /tmp/XMONAD.txt"

runItOnce :: String -> X ()
runItOnce cmd = spawn $ "~/bin/runonce " ++ cmd

-- killItOnce :: String -> X ()
-- killItOnce cmd = spawn $ "~/bin/killonce " ++ cmd

------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
-- Workspaces are clickable meaning that the mouse can be used to switch
-- workspaces. This requires xdotool. You need to use UnsafeStdInReader instead
-- of simply StdInReader in xmobar config so you can pass actions to it.

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myFirstWorkspace :: Integer
myFirstWorkspace = 1

myLastWorkspace :: Integer
myLastWorkspace = 9

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape
               $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
  where
        clickable l = [ "<action=xdotool key super+" ++ show n ++
                        ">" ++ "<fn=2>" ++ ws ++ "</fn>"
                        ++ "</action>" |
                      (i,ws) <- zip [myFirstWorkspace .. myLastWorkspace] l,
                      let n = i ]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length .
  W.integrate' . W.stack . W.workspace .
  W.current . windowset

webPrompt :: String -> X ()
webPrompt _ = do
    str <- inputPrompt cfg "Path|URL"
    case str of
        Just s  -> spawn $ printf "librewolf --new-window \"%s\"" s
        Nothing -> pure ()
  where
    cfg = myXPConfig { defaultText = "" }

mpvPrompt :: String -> X ()
mpvPrompt _ = do
    str <- inputPrompt cfg "Path|URL"
    case str of
        Just s  -> spawn $ printf "mpv \"%s\"" s
        Nothing -> pure ()
  where
    cfg = myXPConfig { defaultText = "" }

mpvYTPrompt :: String -> X ()
mpvYTPrompt _ = do
    str <- inputPrompt cfg "Path|URL"
    case str of
        Just s  -> spawn $ printf "yt-dlp \"%s\" -o - | mpv -" s
        Nothing -> pure ()
  where
    cfg = myXPConfig { defaultText = "" }

------------------------------------------------------------------------
-- KEY BINDINGS
------------------------------------------------------------------------
-- Add some extra key bindings; M1 is Alt key.
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Util-EZConfig.html
keysAdditional :: [(String, X ())]
keysAdditional =
      [ ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess))
      , ("M-p", shellPrompt myXPConfig)
      , ("M-<Home>", sendMessage (Toggle "Full"))
      , ("M-<Space>", sendMessage (Toggle "Full") >> sendMessage ToggleStruts)
      , ("M-<Esc>", withFocused toggleFloat)
      , ("M-d", spawn myScreendump)
      , ("M-m", goToSelected def) --defaultGSConfig)
      -- , ("M-w", withFocused minimizeWindow)
      -- , ("M-S-m", withLastMinimized maximizeWindowAndFocus)
      , ("M-<Backspace>", kill)
      , ("M-b", withFocused toggleBorder)
      , ("M-c", spawn myCursorToggle)
      , ("M-C-<Return>", spawn myFloatingTerminal)
      , ("M-e", spawn myEditor)
      , ("M-w", spawn myLibreWolf)
      , ("M-u", webPrompt "librewolf")
      , ("M-v", mpvPrompt "mpv")
      , ("M-y", mpvYTPrompt "mpv")
      , ("M-/ b", spawn myBrave)
      , ("M-/ c", spawn myChromium)
      , ("M-/ e", spawn myEditor)
      , ("M-/ f", spawn myFileManager)
      , ("M-/ h", spawn myFloatingGHCI)
      , ("M-/ i", spawn myFloatingPython)
      , ("M-/ k", spawn myKeepassXc)
      , ("M-/ l", spawn myLibreOffice)
      , ("M-/ t", spawn myThunderbird)
      , ("M-/ m", spawn myMusicPlayer)
      , ("M-/ o", spawn myTorBrowser)
      , ("M-/ v p e", spawn myPtEuronews)
      , ("M-/ v e 2", spawn myEsFrance24)
      , ("M-/ v e d", spawn myEsDw)
      , ("M-/ v e e", spawn myEsEuronews)
      , ("M-/ v e r", spawn myEsRtve)
      , ("M-/ v f 2", spawn myFrFrance24)
      , ("M-/ v f e", spawn myFrEuronews)
      , ("M-/ v f f", spawn myFrFranceinfo)
      , ("M-/ v u 2", spawn myEnFrance24)
      , ("M-/ v u d", spawn myEnDw)
      , ("M-/ v u e", spawn myEnEuronews)
      , ("M-/ w", spawn myLibreWolf)
      , ("M-/ y", spawn myYoutubeBrowser)
      , ("M-/ z", spawn myFilezilla)
      -- , ("M-/ [", spawn "xrandr --output LVDS-1 --primary --auto --mode 1366x768 --pos 1920x312 --rotate normal --output VGA-1 --auto --mode 1440x900 --pos 0x0 --rotate normal --output HDMI-1 --off --output DP-1 --off")
      , ("M-/ [", spawn "xrandr --output LVDS-1 --primary --auto --mode 1366x768 --pos 1920x312 --rotate normal --output VGA-1 --off --output HDMI-1 --auto --mode 1920x1080 --pos 0x0 --rotate normal --output DP-1 --off")
      , ("M-/ ]", spawn "xrandr --output LVDS-1 --primary --auto --output HDMI-1 --off --output VGA-1 --off --output DP-1 --off")
      , ("M-i", spawn "pactl set-card-profile 0 output:hdmi-stereo")
      , ("M-a", spawn "pactl set-card-profile 0 output:analog-stereo")
      , ("M-S-<Left>", sendMessage Shrink)
      , ("M-S-<Right>", sendMessage Expand)
      , ("M-S-<Up>", windows W.swapDown)
      , ("M-S-<Down>", windows W.swapUp)
      , ("M-<Left>", windows W.focusUp)
      , ("M-<Right>", windows W.focusDown)
      , ("M-z", windows W.focusUp)
      , ("M-x", windows W.focusDown)
      , ("M-C-<Down>", windows W.swapDown >> windows W.focusUp)
      , ("M-C-<Up>", windows W.swapUp >> windows W.focusDown)
      , ("M-<Up>", rotSlavesDown)
      , ("M-<Down>", rotAllDown)
      , ("M-=", toggleWS)
      , ("M-<Tab>", nextWS)
      , ("M-S-<Tab>", prevWS)
      , ("M-<Page_Down>", nextWS)
      , ("M-<Page_Up>", prevWS)
      , ("M-C-<Tab>", shiftToNext >> nextWS)
      , ("M-C-S-<Tab>", shiftToPrev >> prevWS)
      -- , ("M-M1-<Left>", prevWS)
      -- , ("M-M1-<Right>", nextWS)
      , ("M-C-M1-<Left>", shiftToPrev >> prevWS)
      , ("M-C-M1-<Right>", shiftToNext >> nextWS)
      , ("M-C-<Left>", prevScreen)
      , ("M-C-<Right>", nextScreen)
      -- , ("M-S-C-<Left>", shiftPrevScreen)
      -- , ("M-S-C-<Right>", shiftNextScreen)
      -- , ("M-S-C-<Up>", swapPrevScreen)
      -- , ("M-S-C-<Down>", swapNextScreen)
      , ("M-`", sendMessage NextLayout)
      -- , ("M-S-`", setLayout $ layoutHook conf)
      -- , ("M-C-u", sendMessage Arrange)
      -- , ("M-C-d", sendMessage DeArrange)
      -- , ("M-r", runItOnce myRedshiftOn)
      -- , ("M-S-r", killItOnce myRedshiftOff)
      -- , ("M-l", spawn logCommand)
      -- , ("M-0", spawn "xscreensaver-command -lock")
      -- , ("M-C-0", spawn "xscreensaver-command -lock & systemctl suspend")
      , ("M-<Pause>", spawn myScreenBlank)
      , ("M-S-<Pause>", spawn "systemctl suspend -i")
      , ("M-C-S-<Pause>", spawn "systemctl hibernate")
      , ("M-<Scroll_lock>", spawn myScreensaver)
      , ("M-S-<Scroll_lock>", spawn "myScreensaver & systemctl suspend -i")
      , ("M-C-S-<Scroll_lock>", spawn "myScreensaver & systemctl hibernate")
      -- https://www.mankier.com/1/pactl
      , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
      , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
      , ("M-<KP_Subtract>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
      , ("M-<KP_Add>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
      -- , ("M-<KP_Divide>", spawn "pacmd suspend 1")
      , ("M-<KP_Divide>", spawn "pactl suspend-sink @DEFAULT_SINK@ true")
      -- , ("M-<KP_Multiply>", spawn "pacmd suspend 0")
      , ("M-<KP_Multiply>", spawn "pactl suspend-sink @DEFAULT_SINK@ false")
      , ("M-<KP_Enter>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
      , ("<XF86AudioMute>", spawn "amixer set Master toggle")
      , ("<XF86AudioPlay>", spawn "clementine -t")
      , ("<XF86AudioPrev>", spawn "clementine -r")
      , ("<XF86AudioNext>", spawn "clementine -f")
      -- https://github.com/benweitzman/BinarySpacePartition
      -- , ("M-M1-<Left>", sendMessage $ ExpandTowards L)
      , ("M-M1-C-<Left>", sendMessage $ ExpandTowards L)
      -- , ("M-M1-<Right>", sendMessage $ ShrinkFrom L)
      , ("M-M1-C-<Right>", sendMessage $ ShrinkFrom L)
      -- , ("M-M1-<Up>", sendMessage $ ExpandTowards U)
      , ("M-M1-C-<Up>", sendMessage $ ExpandTowards U)
      -- , ("M-M1-<Down>", sendMessage $ ShrinkFrom U)
      , ("M-M1-C-<Down>", sendMessage $ ShrinkFrom U)
      -- , ("M-M1-C-<Left>", sendMessage $ ShrinkFrom R)
      , ("M-M1-<Left>", sendMessage $ ShrinkFrom R)
      -- , ("M-M1-C-<Right>", sendMessage $ ExpandTowards R)
      , ("M-M1-<Right>", sendMessage $ ExpandTowards R)
      -- , ("M-M1-C-<Up>", sendMessage $ ShrinkFrom D)
      , ("M-M1-<Up>", sendMessage $ ShrinkFrom D)
      -- , ("M-M1-C-<Down>", sendMessage $ ExpandTowards D)
      , ("M-M1-<Down>", sendMessage $ ExpandTowards D)
      , ("M-s", sendMessage $ Swap)
      , ("M-r", sendMessage $ Rotate)
      , ("M-[", sendMessage $ SplitShift Prev)
      , ("M-]", sendMessage $ SplitShift Next)
      ]

      -- Appending swap workspace keybindings (Mod+Control+# swaps with current WS).
      ++ [("M-C-" ++ k, windows $ swapWithCurrent w)
           | (w, k) <- zip myWorkspaces (map show [myFirstWorkspace .. myLastWorkspace])]

myStartupHook :: X ()
myStartupHook = do
  -- addScreenCorners [(SCUpperLeft, goToSelected def)] --defaultGSConfig)]
  spawnOnce "xsetroot -solid black"
  -- runItOnce myRedshiftOn
  runItOnce "emacs --daemon"

--------------------------------------------------------------------------------
main :: IO ()
main = do
  GIO.setFileSystemEncoding GIO.char8
  -- D.trace "DO YOU SEE THIS?" undefined
  numberOfScreens <- countScreens
  if numberOfScreens > (1 :: Integer)
   then do
    -- spawn "xrandr --output LVDS-1 --primary --auto  --output HDMI-1 --auto --left-of LVDS-1"
    -- My monitor struggles more and more to come out of HDMI black screen after resume
    -- spawn "xrandr --output LVDS-1 --primary --auto --mode 1366x768 --pos 1920x312 --rotate normal --output VGA-1 --auto --mode 1440x900 --pos 0x0 --rotate normal --output HDMI-1 --off --output DP-1 --off"
    -- spawn "xrandr --output LVDS-1 --primary --mode 1366x768 --pos 1920x312 --rotate normal --output VGA-1 --off --output HDMI-1 --mode 1920x1080 --pos 0x0 --rotate normal --output DP-1 --off"
    -- Use VGA output instead of HDMI for video output (switch off HDMI completely)
    spawn "xrandr --output LVDS-1 --primary --mode 1366x768 --pos 1920x312 --rotate normal --output VGA-1 --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-1 --off --output DP-1 --off"
    -- My monitor struggels more and more to come out of HDMI black screen after resume
    -- spawn "pactl set-card-profile 0 output:hdmi-stereo"
    -- Use analog stereo audio output
    spawn "pactl set-card-profile 0 output:analog-stereo"
   else do
    spawn "xrandr --output LVDS-1 --primary --auto"
    spawn "pactl set-card-profile 0 output:analog-stereo"

  -- TODO -p position parameter for both instances.
  -- Launching instances of xmobar on their monitors. TODO check if only one monitor.
  xmproc0 <- spawnPipe "xmobar -b -p \"xpos=0, width=1920, height=24\" -x 1 /home/mdo/.config/xmobar/xmobarrc0"
  -- xmproc1 <- spawnPipe "xmobar -b -p \"xpos=1920 , ypos=744, width=1366, height=24\" -x 0 /home/mdo/.config/xmobar/xmobarrc1"
  xmproc1 <- spawnPipe "xmobar -b -p \"xpos=1920, width=1366, height=24\" -x 0 /home/mdo/.config/xmobar/xmobarrc1"

  xmonad $ docks def {
    terminal = myTerminal,
    borderWidth = myBorderWidth,
    focusedBorderColor = myFocusedBorderColor,
    modMask = mod4Mask, -- Use the "Win" key for the mod key
    workspaces = myWorkspaces,
    manageHook = myManageHook <+> manageHook desktopConfig <+> manageDocks,
    layoutHook = desktopLayoutModifiers myLayouts, -- $ screenCornerLayoutHook myLayouts,
    logHook = dynamicLogWithPP xmobarPP
        { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x  -- >> hPutStrLn xmproc2 x
          , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
          , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
          , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
          , ppHiddenNoWindows = xmobarColor "#b3afc2" ""        -- Hidden workspaces (no windows)
          , ppTitle = xmobarColor "#ffffff" "" . shorten 50     -- Title of active window in xmobar
          , ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
          , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
          , ppExtras  = [windowCount]                           -- # of windows current workspace
          , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        },
    startupHook = myStartupHook
    }
    -- Remove some keybindings (e.g. Emacs s-l has lots of lsp bindings).
    `removeKeys` [ (mod4Mask , xK_l)
                 , (mod4Mask , xK_h)
                 ]
    `additionalKeysP` keysAdditional
  where
    -- | Customize layouts.
    --
    -- Use the 'M-<Esc>' key binding defined above to toggle between the
    -- current layout and a full screen layout. Use 'M-f' key binding for
    -- a full screen layout with xmobar visible at the top.
    myLayouts = -- minimize . BW.boringWindows $
      refocusLastLayoutHook . trackFloating
      $ avoidStruts
      $ mouseResize
      $ windowArrange
      $ toggleLayouts (noBorders Full) others
      where
        others = smartBorders
                 $ spacingRaw True
                              (Border 0 mySpacing mySpacing mySpacing)
                              True
                              (Border mySpacing mySpacing mySpacing mySpacing)
                              True
                 $ ResizableTall 1 (1.5/100) (6/10) []
                     ||| Mirror (ResizableTall 1 (1.5/100) (6/10) [])
                     ||| ThreeCol 1 (3/100) (1/2)
                     ||| multiCol [1] 1 0.01 (-0.5)
                     ||| emptyBSP
                     ||| Grid
                     ||| noBorders simpleTabbed

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
myXPConfig :: XPConfig
myXPConfig = def
  { position = Top
  , alwaysHighlight = True
  , promptBorderWidth = 0
  , font = "xft:monospace:size=9"
  }

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created. The list given to
-- @composeOne@ is processed from top to bottom. The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
myManageHook :: ManageHook
myManageHook = composeOne
  [ className =? "mpv" -?> doFloat <+> hasBorder False
    , className =? "cvlc" -?> doFloat <+> hasBorder False
    , className =? "vlc" -?> doFloat <+> hasBorder False
    , className =? "opera" -?> hasBorder True
    -- , title =? "Clementine" -?> doFloat <+> hasBorder False
    -- , title =? "ghci" -?> doFloat
    -- , title =? "python" -?> doFloat
    , title =? "floatterm" -?> doFloat
    , className =? "Pinentry" -?> doFloat
    , className =? "Pavucontrol"  -?> doFloat
    -- , className =? "Clementine" -?> doFloat
    , className =? "Pcmanfm" -?> doFloat
    , isDialog -?> doCenterFloat

    -- Move transient windows to their parent:
    , transience
  ]
|]

xmobarRc0 :: String
xmobarRc0 =
  [config|-- This file is under control of Propellor.

-- http://projects.haskell.org/xmobar/

Config { font = "Noto Mono 10"
       , bgColor = "#292d3e"
       , fgColor = "#FFB86C"
       -- Via -p parameter to xmobar.
       -- , position = Static { xpos = 0 , ypos = 1056, width = 1920, height = 24 }
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       -- RefreshRate is in 1/10th of seconds.
       , commands = [ 
                      -- Time and date
                      Run Date "%a, %b %d %Y (%H:%M)" "date" 300
                      -- Network up and down
                    , Run Network "ens5" ["-t", "<rx>kb <tx>kb"] 10
                      -- Cpu usage in percent
                    , Run MultiCpu ["-t", "<total0>% <total1>% <total2>% <total3>%","-H","50","--high","red"] 10
                      -- Ram used number and percent
                    , Run Memory ["-t", "<used>M (<usedratio>%)"] 10
                      -- Disk space free
                    , Run DiskU [("/home", "home: <free>"),
                                 ("/", "/: <free>"),
                                 ("/boot", "boot: <free>")
                                ] [] 600
                      -- Disk read/write speed
                    , Run DiskIO [("sda", "sda:<read> <write>"), ("sdb", "sdb:<read> <write>")] [] 10
                      -- Runs a standard shell command 'uname -r' to get kernel version
                    -- , Run Com "uname" ["-r"] "" 36000
                      -- Prints out the left side items such as workspaces, layout, etc.
                      -- The workspaces are 'clickable' in my configs.
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       -- , template = " <fc=#666666>|</fc> %UnsafeStdinReader% }{ <fc=#FFB86C>%multicpu% </fc><fc=#666666>| </fc><fc=#FF5555>%memory% </fc><fc=#666666>| </fc><fc=#82AAFF>%disku% </fc><fc=#666666>| </fc><fc=#b3afc2>%uname% </fc><fc=#82AAFF>| </fc><fc=#8BE9FD>%date%</fc> "
       , template = " <fc=#666666>|</fc> %UnsafeStdinReader% }{ <fc=#FFB86C>%multicpu% </fc><fc=#666666>| </fc><fc=#FF5555>%memory% </fc><fc=#666666>| </fc><fc=#82AAFF>%disku% </fc><fc=#666666>| </fc><fc=#8BE9FD>%date%</fc> "
       }
|]

xmobarRc1 :: String
xmobarRc1 =
  [config|-- This file is under control of Propellor.

-- http://projects.haskell.org/xmobar/

Config { font = "Noto Mono 10"
       , bgColor = "#292d3e"
       , fgColor = "#FFB86C"
       -- Via -p parameter to xmobar.
       -- , position = Static { xpos = 0 , ypos = 744, width = 1366, height = 24 }
       -- , position = Static { xpos = 1920 , ypos = 744, width = 1366, height = 24 }
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       -- RefreshRate is in 1/10th of seconds.
       , commands = [ 
                      -- Time and date
                      Run Date "%a, %b %d %Y (%H:%M)" "date" 300
                      -- Network up and down
                    , Run Network "ens5" ["-t", "<rx>kb <tx>kb"] 10
                      -- Cpu usage in percent
                    , Run MultiCpu ["-t", "<total0>% <total1>% <total2>% <total3>%","-H","50","--high","red"] 10
                      -- Ram used number and percent
                    , Run Memory ["-t", "<used>M (<usedratio>%)"] 10
                      -- Disk space free
                    , Run DiskU [("/", "/: <free>"),
                                 -- ("/boot", "boot: <free>"),
                                 ("/home", "home: <free>")
                                ] [] 600
                    -- , Run DiskIO [("sda", "sda:<read> <write>"), ("sdb", "sdb:<read> <write>")] [] 10
                      -- Runs a standard shell command 'uname -r' to get kernel version
                    , Run Com "uname" ["-r"] "" 36000
                      -- Prints out the left side items such as workspaces, layout, etc.
                      -- The workspaces are 'clickable' in my configs.
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <fc=#666666>|</fc> %UnsafeStdinReader% }{ <fc=#666666>| </fc><fc=#FFB86C>%multicpu% </fc><fc=#666666>| </fc><fc=#FF5555>%memory% </fc><fc=#666666>| </fc><fc=#82AAFF>%disku% </fc><fc=#666666>| </fc><fc=#b3afc2>%uname% </fc><fc=#82AAFF>| </fc><fc=#8BE9FD>%date%</fc> "
       }
|]
