  -- Base
import XMonad
import System.IO
import System.Exit (exitSuccess)

  -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.MouseResize
import XMonad.Actions.WithAll (sinkAll, killAll)

  -- Data
import Data.Maybe (fromJust)
import qualified Data.Map as M

  -- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

  -- Layouts
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.LayoutScreens
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.Renamed
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed

  -- Layouts Modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

  -- Utilities 
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W

  -- Variables
myTerminal :: String
myTerminal = "alacritty"

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 2

myFont :: String
myFont = "xft:JetBrains Mono:size=8:regular:antialias=true:hinting=true"

myNormalBorderColor :: String
myNormalBorderColor = "#5e81ac"


myFocusedBorderColor :: String
myFocusedBorderColor = "#88c0d0"

altMask :: KeyMask
altMask = mod1Mask

  -- Mouse support
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False

  -- Applications
myBrowser :: String
myBrowser = "firefox"

myEditor :: String
myEditor = myTerminal ++ " -e nvim"

myRofi :: String
myRofi = "rofi -modi window,drun,ssh -theme gruvbox -icon-theme \"Papirus\" -show-icons -show drun"

  -- Autostart
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom &"

  -- Workspaces
myWorkspaces = [" dev ", " www ", " sys ", " doc ", " vbox ", " chat ", " mus ", " vid ", " gfx "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

  -- XMobar
myBar :: String
myBar = "xmobar"

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myPP = xmobarPP 
            { ppCurrent = xmobarColor "#8fbcbb" "" . wrap "[ " " ]"         -- Current workspace
              , ppVisible = xmobarColor "#4c566a" "" . clickable              -- Visible but not current workspace
              , ppHidden = xmobarColor "#88c0d0" "" . wrap "" "*" . clickable -- Hidden workspaces
              , ppHiddenNoWindows = xmobarColor "#88c0d0" ""  . clickable     -- Hidden workspaces (no windows)
              -- , ppTitle = xmobarColor "#5e81ac" "" . shorten 60               -- Title of active window
              , ppSep =  "<fc=#81a1c1> <fn=1>|</fn> </fc>"                    -- Separator character
              , ppUrgent = xmobarColor "#ebcb8b" "" . wrap "!" "!"            -- Urgent workspace
              , ppExtras  = [windowCount]                                     -- # of windows current workspace
              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]     
            }

  -- Hooks
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
    [ className =? "confirm"         --> doCenterFloat
    , className =? "file_progress"   --> doCenterFloat
    , className =? "dialog"          --> doCenterFloat
    , className =? "download"        --> doCenterFloat
    , className =? "error"           --> doCenterFloat
    , className =? "Gimp"            --> doCenterFloat
    , resource  =? "desktop_window"  --> doIgnore
    , resource  =? "kdesktop"        --> doIgnore
    , title     =? "Mozilla Firefox" --> doShift ( myWorkspaces !! 0 )
    , className =? "neovim"          --> doShift ( myWorkspaces !! 1 )
    , className =? "discord"         --> doShift ( myWorkspaces !! 3 )
    , className =? "vlc"             --> doShift ( myWorkspaces !! 4 )
    , className =? "Gimp"            --> doShift ( myWorkspaces !! 6 )
    , isFullscreen --> doFullFloat
                                 ]

  -- Key bindings
myKeys :: [(String, X ())]
myKeys =
        [ ("M-C-r", spawn "xmonad --recompile")         -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")           -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                     -- Quits xmonad
	, ("M1-<Space>", spawn myRofi)			-- Spawn Rofi
	-- Actions
	, ("M-c", kill1) -- Kill the currently focused client
	-- Layout
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
	, ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
	, ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile
	-- Gaps
	, ("C-M1-j", decWindowSpacing 4)         -- Decrease window spacing
        , ("C-M1-k", incWindowSpacing 4)         -- Increase window spacing
        , ("C-M1-h", decScreenSpacing 4)         -- Decrease screen spacing
        , ("C-M1-l", incScreenSpacing 4)         -- Increase screen spacing
	-- Windows navigation
	, ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
	-- Volume and brightness
	, ("<XF86AudioPlay>", spawn "mocp --play")
	, ("<XF86AudioPrev>", spawn "mocp --previous")
        , ("<XF86AudioNext>", spawn "mocp --next")
        , ("<XF86AudioMute>", spawn "amixer set Master toggle")
	, ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
	, ("<Print>", spawn "flameshot")
        ]

  -- Layout
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ smartBorders
           $ windowNavigation
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 10 simplestFloat
tabs     = renamed [Replace "tabs"]

myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (EOT) myDefaultLayout
             where
               myDefaultLayout =    withBorder myBorderWidth tall
                                ||| floats

  -- Xmonad main config
main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc"
    xmonad $ def { 
	terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
	layoutHook  = gaps [(L,10), (R,10), (U,20), (D,20)] $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $ smartBorders $ myLayoutHook,
	startupHook = myStartupHook
        } `additionalKeysP` myKeys
