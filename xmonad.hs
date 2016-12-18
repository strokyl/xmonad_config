{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Config.Azerty
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.ManageDocks
import XMonad.Util.WorkspaceCompare

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [((modm, xK_r),               spawn "rofi -show run"),
     ((modm .|. shiftMask, xK_e), spawn "emacs"),
     ((modm, xK_f),               spawn "firefox")]

    ++
    -- mod-{a,z,e} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{a,z,e} %! Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_z, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

myPP = defaultPP
  {
    ppCurrent = xmobarColor color "". ("*"++),
    ppVisible = xmobarColor color "",
    ppSort = getSortByXineramaRule
  }
  where
    color = "green"

myXmobar :: LayoutClass l Window
       => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myXmobar conf = statusBar "xmobar" myPP toggleStrutsKey conf

main = xmonad =<< myXmobar desktopConfig
    {
        terminal    = "urxvtc",
        logHook     = dynamicLogXinerama,
        modMask     = mod4Mask,
        keys = \c -> azertyKeys c `M.union` myKeys c `M.union` keys desktopConfig c,
        borderWidth = 3
    }
