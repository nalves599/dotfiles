  -- Base
import XMonad
import System.IO
import System.Exit (exitSuccess)

  -- Actions
import XMonad.Actions.MouseResize

  -- Data
import Data.Maybe (fromJust)
import qualified Data.Map as M

  -- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

  -- Layouts
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

  -- Layouts Modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

  -- Utilities 
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce

  -- Variables
myTerminal :: String
myTerminal = "alacritty"

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 2

myFont :: String
myFont = "xft:JetBrains Mono:size=8:regular:antialias=true:hinting=true"

myNormColor :: String
myNormColor = "#282c34"

altMask :: KeyMask
altMask = mod1Mask

  -- Autostart
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nitrogen --restore &"
  
  -- Grid
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 10 simplestFloat
tabs     = renamed [Replace "tabs"]
           $ tabbed shrinkText myTabTheme

myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (EOT) myDefaultLayout
             where
               myDefaultLayout =    withBorder myBorderWidth tall
                                ||| floats
                                ||| noBorders tabs

myWorkspaces = [" dev ", " www ", " sys ", " doc ", " vbox ", " chat ", " mus ", " vid ", " gfx "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

  -- Start Keys
myKeys :: [(String, X ())]
myKeys =
        [ ("M-C-r", spawn "xmonad --recompile")         -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")           -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                     -- Quits xmonad
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        ]


main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar $HOME/.xmobarrc"
    xmonad $ def 
        { terminal        = myTerminal
        , layoutHook      = myLayoutHook
        , workspaces      = myWorkspaces
        , borderWidth     = myBorderWidth
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"
                        , ppVisible = xmobarColor "#98be65" "" . clickable
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"                    -- Separator character
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]                    -- order of things in xmobar
                        }
        , modMask = myModMask
        } `additionalKeysP` myKeys
