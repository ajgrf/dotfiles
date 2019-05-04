{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Werror=unused-imports #-}

import           Control.Monad                  ( when
                                                , join
                                                )
import           Data.List                      ( isSuffixOf )
import           Data.Map                       ( fromList
                                                , union
                                                )
import           Data.Maybe                     ( isJust
                                                , maybeToList
                                                )
import           System.Environment             ( lookupEnv )
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Minimize
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.Plane
import           XMonad.Actions.Warp
import           XMonad.Actions.WindowGo
import           XMonad.Actions.WithAll
import           XMonad.Config.Mate
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.Place
import           XMonad.Hooks.PositionStoreHooks
import           XMonad.Hooks.ScreenCorners
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.WallpaperSetter
import           XMonad.Hooks.WorkspaceHistory
import           XMonad.Layout.BorderResize
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.ButtonDecoration
import           XMonad.Layout.Decoration
import           XMonad.Layout.DraggingVisualizer
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.MultiDishes
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.PositionStoreFloat
import           XMonad.Layout.ShowWName
import           XMonad.Layout.Spacing
import           XMonad.Layout.WindowSwitcherDecoration
import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.Man
import           XMonad.Prompt.Pass
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window
import qualified XMonad.StackSet               as W
import           XMonad.Util.Image
import           XMonad.Util.Run
import           XMonad.Util.Scratchpad

main = do
  workmanEnv <- lookupEnv "WORKMAN"
  xmonad $ withMyUrgencyHook mateConfig
    { normalBorderColor  = inactiveBorderColor adwaitaTheme
    , focusedBorderColor = activeBorderColor adwaitaTheme
    , terminal           = "xterm"
    , layoutHook         = myLayoutHook
    , manageHook         = myWindowRules
                           <+> scratchpadManageHook (W.RationalRect 0.25 0.85 0.5 0.15)
                           <+> placeHook (smart (0.5, 0.5))
                           <+> positionStoreManageHook (Just adwaitaTheme)
                           <+> doF avoidMaster
                           <+> manageHook mateConfig
    , handleEventHook    = fullscreenEventHook
                           <+> minimizeEventHook
                           <+> hintsEventHook
                           <+> positionStoreEventHook
                           <+> screenCornerEventHook
                           <+> dynamicTitle myDynamicRules
                           <+> handleEventHook mateConfig
    , workspaces         = myWorkspaces
    , modMask            = mod1Mask
    , keys               = myKeys (isJust workmanEnv) <+> keys mateConfig
    , borderWidth        = 1
    , startupHook        = addScreenCorner SCUpperLeft (sendMessage NextLayout)
                           <+> startupHook mateConfig
                           >>  addEWMHFullscreen
    , logHook            = workspaceHistoryHook
                           <+> wallpaperSetter defWallpaperConf
                                 { wallpaperBaseDir = "/home/ajgrf/pics/backgrounds"
                                 , wallpapers = WallpaperList
                                                  [ (ws, WallpaperFix (show i ++ ".jpg"))
                                                  | (i, ws) <- zip [1 ..] myWorkspaces
                                                  ]
                                 }
                           <+> logHook mateConfig
    }

myWorkspaces =
  [ "1:Main"
  , "2:Alt"
  , "3:Comm"
  , "4:Media"
  , "5:Misc"
  , "6:Games"
  , "7:VM"
  , "8:Logs"
  , "NSP"
  ]

-- See xmonad/xmonad-contrib#280 for smartBorders bug with
-- multi-head/fullscreen setups.
myLayoutHook =
  ( showWName
    . avoidStruts
    . smartBorders
    . boringWindows
    . minimize
    . screenCornerLayoutHook
    . mkToggle (single HIDE)
    )
    layouts
 where
  layouts =
    ( onWorkspace "1:Main" (mouseTiled ||| float)
      . onWorkspace "7:VM"   Full
      . onWorkspace "8:Logs" dishes
      )
      mouseFriendly
  dishes =
    ( buttonDeco shrinkText adwaitaThemeWithButtons
      . maximizeWithPadding 0
      . smartSpacing 2
      )
      (MultiDishes 1 2 (1 / 5))
  float =
    ( buttonDeco shrinkText adwaitaThemeWithButtons
      . maximizeWithPadding 0
      . borderResize
      . layoutHints
      )
      positionStoreFloat
  mouseTiled =
    ( windowSwitcherDecorationWithButtons shrinkText adwaitaThemeWithButtons
      . draggingVisualizer
      . maximizeWithPadding 0
      . smartSpacing 2
      . layoutHintsWithPlacement (0.5, 0.5)
      )
      mouseResizableTile { masterFrac = 11 / 20, fracIncrement = 1 / 20 }
  mouseFriendly = float ||| mouseTiled
  smartSpacing x = spacingRaw True (Border x x x x) True (Border x x x x) True
  showWName = showWName' SWNC
    { swn_font    = "xft:Cantarell:bold:size=11"
    , swn_bgcolor = "#353535"
    , swn_color   = "#eeeeec"
    , swn_fade    = 1
    }

myKeys isWorkman conf@(XConfig { XMonad.modMask = modMask }) =
  union (planeKeys (controlMask .|. mod1Mask) (Lines 3) Linear)
    $  fromList
    $  [ ((modMask, xK_j)               , focusDown)
       , ((modMask, xK_k)               , focusUp)
       , ((modMask .|. shiftMask, xK_h) , sendMessage ShrinkSlave)
       , ((modMask .|. shiftMask, xK_l) , sendMessage ExpandSlave)
       , ((modMask, xK_m)               , withFocused minimizeWindow)
       , ((mod4Mask, xK_d)              , sendMessage $ Toggle HIDE)
       , ((mod4Mask, xK_m)              , withAll minimizeWindow)
       , ((mod4Mask .|. shiftMask, xK_m), withAll maximizeWindow)
       , ( (modMask .|. shiftMask, xK_m)
         , withLastMinimized maximizeWindowAndFocus
         )
       , ((mod4Mask, xK_Up), withFocused (sendMessage . maximizeRestore))
       , ((modMask, xK_slash), banishScreen LowerRight)
       , ((modMask, xK_p), shellPrompt adwaitaXPConfig)
       , ((modMask .|. shiftMask, xK_p), passTypePrompt adwaitaXPConfig)
       , ((modMask, xK_g), windowPrompt adwaitaXPConfig Goto allWindows)
       , ((modMask, xK_F1), manPrompt adwaitaXPConfig)
       , ((mod1Mask, xK_F2), shellPrompt adwaitaXPConfig)
       , ((mod1Mask, xK_F4), kill)
       , ((0, xK_F1), runOrRaiseNext "firefox" (className =? "Firefox-esr"))
       , ((0, xK_F2), runOrRaiseNext "emacs" (className =? "Emacs"))
       , ((0, xK_F3), scratchpadSpawnAction conf)
       , ( (0, xK_F4)
         , raiseMaybe (runInTerm "-name ncmpc" "ncmpc") (appName =? "ncmpc")
         )
       , ((0, xK_F5)                   , planeMove (Lines 3) Linear ToLeft)
       , ((0, xK_F6)                   , planeMove (Lines 3) Linear ToRight)
       , ((modMask, xK_Tab)            , toggleWS)
       , ((modMask, xK_0)              , focusUrgent)
       , ((modMask .|. shiftMask, xK_0), killAllOtherCopies)
       , ((modMask .|. shiftMask .|. controlMask, xK_0), windows copyToAll)
       ]
    ++ [ ((modMask .|. shiftMask .|. controlMask, k), windows $ copy i)
       | (i, k) <- zip (workspaces conf) [xK_1 ..]
       ]
    ++ [ ((m .|. modMask, key), f horizontalScreenOrderer sc)
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
       , (f  , m ) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
       ]
    ++ if isWorkman then workmanKeys else []
 where
  workmanKeys =
    [ ((modMask, xK_k)              , refresh)
      , ((modMask, xK_n)              , focusDown)
      , ((modMask, xK_e)              , focusUp)
      , ((modMask .|. shiftMask, xK_n), windows W.swapDown)
      , ((modMask .|. shiftMask, xK_e), windows W.swapUp)
      , ((modMask, xK_y)              , sendMessage Shrink)
      , ((modMask, xK_o)              , sendMessage Expand)
      , ((modMask .|. shiftMask, xK_y), sendMessage ShrinkSlave)
      , ((modMask .|. shiftMask, xK_o), sendMessage ExpandSlave)
      ]
      ++ [ ((m .|. modMask, key), f horizontalScreenOrderer sc)
         | (key, sc) <- zip [xK_d, xK_r, xK_w] [0 ..]
         , (f  , m ) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
         ]

myWindowRules = composeAll
  [ className =? "Org.gnome.Maps" --> doFloat
  , appName =? "ncmpc" --> doShift "4:Media"
  , className =? "mpv" --> doShift "4:Media"
  , className =? "Gnome-mpv" --> (doShift "4:Media" <+> doFloat)
  , className =? "Sol" --> doShift "6:Games"
  , className =? "Gnome-mines" --> (doShift "6:Games" <+> doFloat)
  , className =? "Gnome-mahjongg" --> (doShift "6:Games" <+> doFloat)
  , className =? "Gnome-sudoku" --> (doShift "6:Games" <+> doFloat)
  , className =? "Gnome-boxes" --> doShift "7:VM"
  , className =? "Mate-system-log" --> doShift "8:Logs"
  , className =? "Mate-system-monitor" --> doShift "8:Logs"
  , appName =? "backup" --> doShift "8:Logs"
  ]

myDynamicRules =
  composeAll [isSuffixOf " (Private Browsing)" <$> title --> doShift "NSP"]

withMyUrgencyHook = withUrgencyHookC
  (   borderUrgencyHook (urgentBorderColor adwaitaTheme)
  <+> spawnUrgencyHook "notify-urgency "
  )
  urgencyConfig { suppressWhen = Focused }

adwaitaTheme = Theme
  { activeColor         = "#dfdcd8"
  , inactiveColor       = "#f6f5f4"
  , urgentColor         = "#3584e4"
  , activeBorderColor   = "#bfb8b1"
  , inactiveBorderColor = "#cdc7c2"
  , urgentBorderColor   = "#1658a7"
  , activeTextColor     = "#2e3436"
  , inactiveTextColor   = "#929595"
  , urgentTextColor     = "#ffffff"
  , fontName            = "xft:Cantarell:bold:size=11"
  , decoWidth           = 400
  , decoHeight          = 35
  , windowTitleAddons   = []
  , windowTitleIcons    = []
  }

adwaitaThemeWithButtons = adwaitaTheme
  { windowTitleIcons = [ (minimizeIcon, CenterRight 44)
                       , (maximizeIcon, CenterRight 25)
                       , (closeIcon   , CenterRight 6)
                       ]
  }

minimizeIcon = map
  (map ((==) 1))
  [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ]

maximizeIcon = map
  (map ((==) 1))
  [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ]

closeIcon = map
  (map ((==) 1))
  [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ]

adwaitaXPConfig = def { fgColor         = "#eeeeec"
                      , bgColor         = "#353535"
                      , fgHLight        = "#ffffff"
                      , bgHLight        = "#15539e"
                      , borderColor     = "#1c1c1c"
                      , font            = "xft:Cantarell:size=11"
                      , position        = CenteredAt 0.15 0.3
                      , height          = 27
                      , maxComplRows    = Just 15
                      , searchPredicate = fuzzyMatch
                      }

-- Avoid the master window, but otherwise manage new windows normally.
-- https://git.joeyh.name/index.cgi/joey/home-plus.git/tree/.xmonad/xmonad.hs.general
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
  W.Stack t [] (r : rs) -> W.Stack t [r] rs
  _                     -> c

-- Implement "show desktop" feature by toggling an empty layout
-- https://superuser.com/a/1082273

data EmptyLayout a = EmptyLayout deriving (Show, Read)

instance LayoutClass EmptyLayout a where
    doLayout a b _ = emptyLayout a b
    description _ = "Empty Layout"

data HIDE = HIDE deriving (Read, Show, Eq, Typeable)
instance Transformer HIDE Window where
    transform _ x k = k (EmptyLayout) (\(EmptyLayout) -> x)

-- Advertise fullscreen support in startupHook.
-- https://github.com/xmonad/xmonad-contrib/issues/183#issuecomment-307407822

addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \dpy -> do
  r               <- asks theRoot
  a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
  a               <- getAtom "ATOM"
  liftIO $ do
    sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
    when (fromIntegral x `notElem` sup) $ changeProperty32 dpy
                                                           r
                                                           a_NET_SUPPORTED
                                                           a
                                                           propModeAppend
                                                           [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  wms <- getAtom "_NET_WM_STATE"
  wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  mapM_ addNETSupported [wms, wfs]
