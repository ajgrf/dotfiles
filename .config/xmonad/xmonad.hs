{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Werror=unused-imports #-}

import           Control.Monad                  ( when
                                                , join
                                                )
import           Data.Map                       ( fromList
                                                , union
                                                )
import           Data.Maybe                     ( isJust
                                                , maybeToList
                                                )
import           System.Environment             ( lookupEnv )
import           XMonad
import           XMonad.Actions.GroupNavigation
import           XMonad.Actions.Minimize
import           XMonad.Actions.Plane
import           XMonad.Actions.Warp
import           XMonad.Actions.WithAll
import           XMonad.Config.Mate
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.Place
import           XMonad.Hooks.PositionStoreHooks
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.WallpaperSetter
import           XMonad.Layout.BorderResize
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.ButtonDecoration
import           XMonad.Layout.Decoration
import           XMonad.Layout.DraggingVisualizer
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PositionStoreFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.WindowSwitcherDecoration
import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet               as W
import           XMonad.Util.Image

main = do
  workmanEnv <- lookupEnv "WORKMAN"
  xmonad $ withMyUrgencyHook mateConfig
    { normalBorderColor  = inactiveBorderColor adwaitaTheme
    , focusedBorderColor = activeBorderColor adwaitaTheme
    , terminal           = "xterm"
    , layoutHook         = myLayoutHook
    , manageHook         = myWindowRules
                           <+> placeHook (smart (0.5, 0.5))
                           <+> positionStoreManageHook (Just adwaitaTheme)
                           <+> doF avoidMaster
                           <+> manageHook mateConfig
    , handleEventHook    = fullscreenEventHook
                           <+> minimizeEventHook
                           <+> hintsEventHook
                           <+> positionStoreEventHook
                           <+> handleEventHook mateConfig
    , workspaces         = myWorkspaces
    , modMask            = mod1Mask
    , keys               = myKeys (isJust workmanEnv) <+> keys mateConfig
    , borderWidth        = 1
    , startupHook        = startupHook mateConfig >> addEWMHFullscreen
    , logHook            = wallpaperSetter defWallpaperConf
                               { wallpaperBaseDir = "/home/ajgrf/pics/backgrounds"
                               , wallpapers = WallpaperList
                                                [ (ws, WallpaperFix (show i ++ ".jpg"))
                                                | (i, ws) <- zip [1 ..] myWorkspaces
                                                ]
                               }
                             <+> logHook mateConfig
    }

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- See xmonad/xmonad-contrib#280 for smartBorders bug with
-- multi-head/fullscreen setups.
myLayoutHook
  = (avoidStruts . smartBorders . boringWindows . minimize . mkToggle
      (single HIDE)
    )
    (tiled ||| float ||| Full)
 where
  float =
    ( buttonDeco shrinkText adwaitaThemeWithButtons
      . maximizeWithPadding 0
      . borderResize
      . layoutHints
      )
      positionStoreFloat
  tiled =
    ( windowSwitcherDecorationWithButtons shrinkText adwaitaThemeWithButtons
      . draggingVisualizer
      . maximizeWithPadding 0
      . spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True
      . layoutHintsWithPlacement (0.5, 0.5)
      )
      mouseResizableTile { masterFrac = 11 / 20, fracIncrement = 1 / 20 }

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
       , ((modMask, xK_p)    , shellPrompt adwaitaXPConfig)
       , ((mod1Mask, xK_F2)  , shellPrompt adwaitaXPConfig)
       , ((mod1Mask, xK_F4)  , kill)
       , ( (modMask, xK_Tab)
         , nextMatch Forward (return True) <+> withFocused maximizeWindow
         )
       , ( (modMask .|. shiftMask, xK_Tab)
         , nextMatch Backward (return True) <+> withFocused maximizeWindow
         )
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
      ++ [ ( (m .|. modMask, key)
           , screenWorkspace sc >>= flip whenJust (windows . f)
           )
         | (key, sc) <- zip [xK_d, xK_r, xK_w] [0 ..]
         , (f  , m ) <- [(W.view, 0), (W.shift, shiftMask)]
         ]

myWindowRules = composeAll []

withMyUrgencyHook = withUrgencyHookC
  (borderUrgencyHook (urgentBorderColor adwaitaTheme))
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
  { windowTitleIcons = [ (minimizeIcon, CenterRight 48)
                       , (maximizeIcon, CenterRight 29)
                       , (closeIcon   , CenterRight 10)
                       ]
  }

minimizeIcon = map
  (map ((==) 1))
  [ [0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0]
  , [1, 1, 1, 1, 1, 1, 1, 1]
  , [1, 1, 1, 1, 1, 1, 1, 1]
  ]

maximizeIcon = map
  (map ((==) 1))
  [ [1, 1, 1, 1, 1, 1, 1, 1]
  , [1, 1, 1, 1, 1, 1, 1, 1]
  , [1, 1, 0, 0, 0, 0, 1, 1]
  , [1, 1, 0, 0, 0, 0, 1, 1]
  , [1, 1, 0, 0, 0, 0, 1, 1]
  , [1, 1, 0, 0, 0, 0, 1, 1]
  , [1, 1, 1, 1, 1, 1, 1, 1]
  , [1, 1, 1, 1, 1, 1, 1, 1]
  ]

closeIcon = map
  (map ((==) 1))
  [ [1, 1, 0, 0, 0, 0, 1, 1]
  , [1, 1, 1, 0, 0, 1, 1, 1]
  , [0, 1, 1, 1, 1, 1, 1, 0]
  , [0, 0, 1, 1, 1, 1, 0, 0]
  , [0, 0, 1, 1, 1, 1, 0, 0]
  , [0, 1, 1, 1, 1, 1, 1, 0]
  , [1, 1, 1, 0, 0, 1, 1, 1]
  , [1, 1, 0, 0, 0, 0, 1, 1]
  ]

adwaitaXPConfig = def { fgColor           = "#2e3436"
                      , bgColor           = "#f6f5f4"
                      , fgHLight          = "#f6f5f4"
                      , bgHLight          = "#3584e4"
                      , font              = "xft:Cantarell:size=11"
                      , promptBorderWidth = 0
                      , position          = Bottom
                      , height            = 27
                      , searchPredicate   = fuzzyMatch
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
