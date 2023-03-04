-- |
module Main where

import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.Hooks.ManageDocks as Docks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Accordion
import XMonad.Layout.DwmStyle
import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed
import XMonad.Layout.Combo
import XMonad.Layout.Master
import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import qualified XMonad.StackSet as S
import XMonad.Util.SpawnOnce

------------------------------------------------------------------------------
myTerminal      = "termonad"
myNavigator     = "firefox-devedition"
myEditor        = "emacs"
myBorderWidth   = 5
myModMask       = mod4Mask
myWorkspaces    = map show [1..9]
myNormalBorderColor  = "#333333"
myFocusedBorderColor = "#FCB3FC"
myFocusFollowsMouse = True

------------------------------------------------------------------------------
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask              , xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask              , xK_d     ), spawn "rofi -show drun")
    , ((modMask              , xK_w     ), spawn "rofi -show window")
    , ((modMask              , xK_f     ), sendMessage $ Toggle FULL)
    , ((modMask              , xK_v     ), sendMessage $ Toggle MIRROR)
    , ((modMask              , xK_BackSpace), withFocused (sendMessage . maximizeRestore))
    , ((modMask              , xK_F1    ), spawn $ (XMonad.terminal conf) ++ " -name mutt -e mutt")
    , ((modMask              , xK_F2    ), spawn $ XMonad.terminal conf)
    , ((modMask              , xK_F3    ), spawn $ (XMonad.terminal conf) ++ " -name irssi -e irssi")
    , ((modMask              , xK_F4    ), spawn myNavigator)
    , ((modMask              , xK_F5    ), spawn myEditor)
    , ((modMask              , xK_F6    ), spawn $ (XMonad.terminal conf) ++ " -name slrn -e slrn")
    , ((modMask .|. shiftMask, xK_q     ), kill)
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask              , xK_v     ), sendMessage $ Toggle MIRROR)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask,               xK_n     ), refresh)
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((mod1Mask,              xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modMask .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move Docks.R)
    , ((modMask .|. controlMask .|. shiftMask, xK_Left ), sendMessage $ Move Docks.L)
    , ((modMask .|. controlMask .|. shiftMask, xK_Up   ), sendMessage $ Move Docks.U)
    , ((modMask .|. controlMask .|. shiftMask, xK_Down ), sendMessage $ Move Docks.D)
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_semicolon), sendMessage (IncMasterN (-1)))
    , ((modMask              , xK_b     ), sendMessage Docks.ToggleStruts)
    , ((modMask              , xK_q     ), restart "xmonad" True)
    , ((modMask              , xK_k     ), spawn "i3lock")
    , ((modMask              , xK_p     ), spawn "flameshot gui")
    , ((0                    , xK_Print), spawn "scrot -u ~/Pictures/Screenshot_%Y%m%d_%H%M%S.png")
    , ((0                    , xF86XK_MonBrightnessUp), spawn "light -A 5")
    , ((0                    , xF86XK_MonBrightnessDown), spawn "light -U 5")
    , ((0                    , xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0                    , xF86XK_AudioPrev), spawn "playerctl previous")
    , ((0                    , xF86XK_AudioNext), spawn "playerctl next")
    , ((0                    , xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")
    , ((0                    , xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -5%")
    , ((0                    , xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +5%")
    ]
    ++
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(S.greedyView, 0), (S.shift, shiftMask)]]    
    ++
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_z, xK_x] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

------------------------------------------------------------------------------
myLayout =
  id
  . Docks.avoidStruts
  . addSpacing
  . smartBorders
  . mkToggle (NOBORDERS ?? FULL ?? EOT)
  . mkToggle (single MIRROR)
  $ layouts
  where
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

    addSpacing = spacingRaw False (Border 8 8 8 8) True (Border 8 8 8 8) True
    bared = noFrillsDeco shrinkText def

    layouts = tiled ||| accordion ||| fullNoBorders ||| tab

    accordion = bared $ renamed [Replace "2/3"] (mastered (1/100) (2/3) Accordion)
    fullNoBorders = noBorders Full
    tab = tabbed shrinkText myTabConfig
    tiled = maximize (Tall nmaster delta ratio)

    myTabConfig =
      def
        { inactiveBorderColor = "#BFBFBF"
        , activeTextColor     = "#FFFFFF"
        }

------------------------------------------------------------------------------
myManageHook = Docks.manageDocks <+> composeAll
    [ className =? "Gimp" --> doFloat
    ]

------------------------------------------------------------------------
myStartupHook = setWMName "dad's xmonad"
              >> spawnOnce "autorandr hub-monitor-only"
              >> spawnHere "feh --bg-fill ~/Downloads/IMG_1765.jpeg"

------------------------------------------------------------------------
main :: IO ()
main = do
  xmonad . ewmh $
    def {
      terminal           = myTerminal,
      focusFollowsMouse  = myFocusFollowsMouse,
      borderWidth        = myBorderWidth,
      modMask            = myModMask,
      workspaces         = myWorkspaces,
      normalBorderColor  = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      keys               = myKeys,
      mouseBindings      = myMouseBindings,
      layoutHook         = myLayout,
      manageHook         = myManageHook,
      startupHook        = myStartupHook
      }
