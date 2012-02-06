import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Submap
import XMonad.Actions.Warp
import XMonad.Actions.WindowGo
import XMonad.Config.Gnome
-- import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.Reflect

main = xmonad $ gnomeConfig
	{ normalBorderColor  = "#93a1a1"
	, focusedBorderColor = "#cb4b16"
	, borderWidth = 1
	, terminal = "urxvt"
	, modMask = mod4Mask
	, layoutHook = onWorkspace "5" (avoidStruts $ withIM (1%5) (And (ClassName "Pidgin") (Role "buddy_list")) $ Grid) $ smartBorders $ layoutHook gnomeConfig
	, manageHook = manageHook gnomeConfig <+> composeAll
		[ isFullscreen --> doFullFloat
		, className =? "Iceweasel" --> doShift "1"
		, className =? "URxvt"     --> doShift "2"
		, className =? "Anki"      --> doShift "3"
		, className =? "Icedove"   --> doShift "4"
		, className =? "Pidgin"    --> doShift "5"
		]
	}
	`additionalKeysP`
		[ ("M-n", windows W.focusDown)
		, ("M-e", windows W.focusUp)
		, ("M-S-n", windows W.swapDown)
		, ("M-S-e", windows W.swapUp)
		, ("M-i", sendMessage Expand)
		, ("M-k", refresh)
		, ("M-f",   screenWorkspace 1 >>= flip whenJust (windows . W.view))  -- focus second screen
		, ("M-S-f", screenWorkspace 1 >>= flip whenJust (windows . W.shift)) -- move to second screen
		, ("M-r", banishScreen LowerLeft)
		, ("M-<Tab>", toggleWS)
		, ("M-s", submap . M.fromList $
			[ ((0, xK_w), spawn "firefox") 
			, ((0, xK_a), runOrRaise "anki" (className =? "Anki"))
			, ((0, xK_m), spawn "icedove")
			, ((0, xK_p), spawn "pidgin")
			, ((0, xK_q), spawn "quodlibet --toggle-window || quodlibet")
			])
		]
	`removeKeysP`
		["M-j", "M-k", "M-S-j", "M-S-k", "M-l", "M-S-r", "M-S-p"]

