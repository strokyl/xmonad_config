{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Config.Azerty
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.ManageDocks
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Util.Cursor
import Graphics.X11.ExtraTypes.XF86
import XMonad.Prompt.ConfirmPrompt
import System.Exit
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive

setLum :: Show a => Num a => a -> X ()
setLum perCent = spawn $ "xbacklight -inc " ++ show perCent

lumStep :: Int
lumStep = 10 -- in percent

setVolume :: (Show a, Num a, Ord a) => a -> X ()
setVolume perCent = spawn $ "amixer -D pulse sset Master " ++ show (abs perCent) ++ "%" ++ signe
  where signe = if perCent >= 0 then "+" else "-"

toggleVolume :: X ()
toggleVolume = spawn "amixer -D pulse set Master toggle"

volumStep :: Int
volumStep = 7 -- in percent

dbusAudio action program = spawn $ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2." ++ program ++ " /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player." ++ action

audioProgram = ["vlc", "spotify"]

myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
  [((modm, button2), const $ spawn "toggle_scroll")]

myKeys conf@XConfig {XMonad.modMask = modm}= M.fromList $
    [((modm, xK_r),                      spawn "rofi -show run"),
     ((modm, xK_w),                      spawn "rofi -show window"),
     ((modm .|. shiftMask, xK_e),        spawn "emacsclient -c"),
     ((modm, xK_f),                      spawn "firefox"),
     ((controlMask .|. mod1Mask, xK_l),  spawn "i3lock-wrapper"),
     ((modm, xK_g),                      moveTo Next EmptyWS),
     ((modm .|. shiftMask, xK_Tab),      moveTo Prev HiddenWS),
     ((modm .|. shiftMask, xK_q),        confirmPrompt def "Exit XMonad" $ io exitSuccess),
     ((modm .|. mod1Mask, xK_space),     spawn "toggle_touchpad"),

     ((0, xF86XK_AudioLowerVolume),      setVolume $ -volumStep),
     ((0, xF86XK_AudioRaiseVolume),      setVolume volumStep),
     ((0, xF86XK_AudioMute),             toggleVolume),
     ((0, xF86XK_AudioPlay),             foldl1 (>>) $ map (dbusAudio "PlayPause") audioProgram),
     ((0, xF86XK_AudioNext),             foldl1 (>>) $ map (dbusAudio "Next") audioProgram),
     ((0, xF86XK_AudioPrev),             foldl1 (>>) $ map (dbusAudio "Previous") audioProgram),

     ((0, xF86XK_MonBrightnessUp),       setLum lumStep),
     ((0, xF86XK_MonBrightnessDown),     setLum (-lumStep)),

     ((modm, xK_Tab),                    moveTo Next HiddenNonEmptyWS)]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modm, key), windows $ f i)
      | (i, key) <- zip (XMonad.workspaces conf) keyOnetoNightInAzerty
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (W.greedyView, mod1Mask)]]

  ++

    -- mod-{a,z,e} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{a,z,e} %! Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_z, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

keyOnetoNightInAzerty = [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0]

myPP = def
  {
    ppCurrent = xmobarColor color "". ("*"++),
    ppVisible = xmobarColor color "",
    ppSort = getSortByXineramaRule
  }
  where
    color = "green"

myXmobar :: LayoutClass l Window
       => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myXmobar = statusBar "xmobar" myPP toggleStrutsKey

myConfig = desktopConfig
  {
    terminal    = "urxvtc",

    logHook     = updatePointer (0.25, 0.25) (0, 0) >> dynamicLogXinerama <+>
                  fadeInactiveLogHook 0.7 <+>
                  logHook desktopConfig,

    modMask     = mod4Mask,

    keys = \c -> myKeys c `M.union`
                 azertyKeys c `M.union`
                 keys desktopConfig c,

    mouseBindings  = \c -> myMouseBindings c `M.union`
                           mouseBindings desktopConfig c,

    borderWidth = 3,

    manageHook =  (isFullscreen --> doFullFloat) <+>
                  manageHook desktopConfig,

    startupHook = setDefaultCursor xC_left_ptr >> setWMName "LG3D" <+>
                  startupHook desktopConfig,

    handleEventHook = XMonad.Hooks.EwmhDesktops.fullscreenEventHook <+>
                      handleEventHook desktopConfig,

    layoutHook = smartBorders $
                 noBorders $
                 layoutHook desktopConfig
  }

main = xmonad =<< myXmobar (fullscreenSupport $ ewmh myConfig)
