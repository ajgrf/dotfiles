{-# OPTIONS_GHC -Werror -Wunused-imports #-}

import           Control.Monad                  ( when
                                                , join
                                                )
import           Data.Maybe                     ( isJust
                                                , maybeToList
                                                )
import           System.Environment             ( lookupEnv )
import           XMonad
import           XMonad.Actions.Warp            ( banishScreen
                                                , Corner(LowerRight)
                                                )
import           XMonad.Config.Mate             ( mateConfig )
import           XMonad.Hooks.EwmhDesktops      ( fullscreenEventHook )
import           XMonad.Hooks.ManageDocks       ( avoidStruts )
import           XMonad.Hooks.Place             ( placeHook
                                                , smart
                                                )
import           XMonad.Layout.Decoration
import           XMonad.Layout.DraggingVisualizer
                                                ( draggingVisualizer )
import           XMonad.Layout.LayoutHints      ( hintsEventHook
                                                , layoutHintsWithPlacement
                                                )
import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Layout.MouseResizableTile
                                                ( mouseResizableTile
                                                , masterFrac
                                                , fracIncrement
                                                , MRTMessage
                                                  ( ExpandSlave
                                                  , ShrinkSlave
                                                  )
                                                )
import           XMonad.Layout.SimplestFloat    ( simplestFloat )
import           XMonad.Layout.Spacing          ( spacingRaw
                                                , Border(Border)
                                                )
import           XMonad.Layout.WindowSwitcherDecoration
                                                ( windowSwitcherDecorationWithButtons
                                                )
import qualified Data.Map                      as M
import qualified XMonad.StackSet               as W
import           XMonad.Util.Font               ( Align(AlignRightOffset) )


main = do
  workmanEnv <- lookupEnv "WORKMAN"
  xmonad $ mateConfig
    { normalBorderColor  = inactiveBorderColor adwaitaTheme
    , focusedBorderColor = activeBorderColor adwaitaTheme
    , terminal           = "xterm"
    , layoutHook         = myLayoutHook
    , manageHook = placeHook (smart (0.5, 0.5)) <+> manageHook mateConfig
    , handleEventHook    = fullscreenEventHook <+> hintsEventHook
    , keys               = myKeys (isJust workmanEnv) <+> keys mateConfig
    , borderWidth        = 1
    , startupHook        = startupHook mateConfig >> addEWMHFullscreen
    }

myLayoutHook = tiled ||| simplestFloat ||| Full
 where
  tiled =
    ( windowSwitcherDecorationWithButtons shrinkText adwaitaTheme
      . draggingVisualizer
      . avoidStruts
      -- See xmonad/xmonad-contrib#280 for smartBorders bug with
      -- multi-head/fullscreen setups.
      . smartBorders
      . spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True
      . layoutHintsWithPlacement (0.5, 0.5)
      )
      mouseResizableTile { masterFrac = 11 / 20, fracIncrement = 1 / 20 }

myKeys isWorkman conf@(XConfig { XMonad.modMask = modMask }) =
  M.fromList
    $  [ ((modMask .|. shiftMask, xK_h), sendMessage ShrinkSlave)
       , ((modMask .|. shiftMask, xK_l), sendMessage ExpandSlave)
       , ((modMask, xK_slash)          , banishScreen LowerRight)
       ]
    ++ if isWorkman then workmanKeys else []
 where
  workmanKeys =
    [ ((modMask, xK_k)              , refresh)
      , ((modMask, xK_n)              , windows W.focusDown)
      , ((modMask, xK_e)              , windows W.focusUp)
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

adwaitaTheme = Theme
  { activeColor         = "#dfdcd8"
  , inactiveColor       = "#f6f5f4"
  , urgentColor         = "#f6f5f4"
  , activeBorderColor   = "#bfb8b1"
  , inactiveBorderColor = "#cdc7c2"
  , urgentBorderColor   = "#4a90d9"
  , activeTextColor     = "#2e3436"
  , inactiveTextColor   = "#929595"
  , urgentTextColor     = "#4a90d9"
  , fontName            = "xft:Cantarell:bold:size=11"
  , decoWidth           = 7680
  , decoHeight          = 35
  , windowTitleAddons   = [("×", AlignRightOffset 10)]
  , windowTitleIcons    = []
  }

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
