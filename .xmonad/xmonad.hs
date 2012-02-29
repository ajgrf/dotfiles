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
	{ normalBorderColor  = "#c5c5c5"
	, focusedBorderColor = "#de935f"
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
		[ ("M-r", banishScreen LowerLeft)
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
		["M-S-p"]

