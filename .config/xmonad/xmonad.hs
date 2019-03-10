import           Control.Monad                  ( when
                                                , join
                                                )
import           Data.Maybe                     ( isJust
                                                , maybeToList
                                                )
import           System.Environment             ( lookupEnv )
import           System.Exit                    ( exitWith
                                                , ExitCode(ExitSuccess)
                                                )
import           XMonad
import           XMonad.Actions.Warp            ( banishScreen
                                                , Corner(LowerRight)
                                                )
import           XMonad.Config.Mate             ( mateConfig )
import           XMonad.Hooks.EwmhDesktops      ( fullscreenEventHook )
import           XMonad.Hooks.ManageDocks       ( avoidStruts )
import           XMonad.Layout.LayoutHints      ( layoutHintsWithPlacement )
import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Layout.Spacing          ( spacingRaw
                                                , Border(Border)
                                                )
import qualified Data.Map                      as M
import qualified XMonad.StackSet               as W

main = do
  workmanEnv <- lookupEnv "WORKMAN"
  xmonad $ mateConfig
    { normalBorderColor  = "#b6b6b3"
    , focusedBorderColor = "#4a90d9"
    , terminal           = "xterm"
    , layoutHook         = myLayoutHook
    , handleEventHook    = fullscreenEventHook
    , keys               = myKeys (isJust workmanEnv) <+> keys mateConfig
    , borderWidth        = 2
    , startupHook        = startupHook mateConfig >> addEWMHFullscreen
    }

myLayoutHook =
  -- See xmonad/xmonad-contrib#280 for smartBorders bug with
  -- multi-head/fullscreen setups.
  (avoidStruts . smartBorders . smartSpacing 4 . layoutHintsWithPlacement
      (0.5, 0.5)
    )
    layouts
 where
  smartSpacing x = spacingRaw True (Border x x x x) True (Border x x x x) True
  layouts = tiled ||| Full
  tiled   = Tall nmaster delta ratio
  nmaster = 1
  delta   = 1 / 20
  ratio   = 11 / 20

myKeys isWorkman conf@(XConfig { XMonad.modMask = modMask }) =
  M.fromList $ [((modMask, xK_slash), banishScreen LowerRight)] ++ if isWorkman
    then workmanKeys
    else []
 where
  workmanKeys =
    [ ((modMask, xK_k)              , refresh)
      , ((modMask, xK_n)              , windows W.focusDown)
      , ((modMask, xK_e)              , windows W.focusUp)
      , ((modMask .|. shiftMask, xK_n), windows W.swapDown)
      , ((modMask .|. shiftMask, xK_e), windows W.swapUp)
      , ((modMask, xK_y)              , sendMessage Shrink)
      , ((modMask, xK_o)              , sendMessage Expand)
      ]
      ++ [ ( (m .|. modMask, key)
           , screenWorkspace sc >>= flip whenJust (windows . f)
           )
         | (key, sc) <- zip [xK_d, xK_r, xK_w] [0 ..]
         , (f  , m ) <- [(W.view, 0), (W.shift, shiftMask)]
         ]

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
