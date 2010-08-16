-- imports {{{
import XMonad hiding ( (|||) ) -- don't import the ||| operator, it comes in layoutcombinators
import XMonad.ManageHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
 --hopefully making matlab run
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Cursor
import XMonad.Util.Run
import XMonad.Util.Dmenu
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows
import XMonad.Util.WorkspaceCompare
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.HintedTile
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.LayoutHints
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Cross
import XMonad.Layout.Named
import XMonad.Layout.Maximize
import XMonad.Layout.Simplest
import XMonad.Layout hiding ( (|||) )
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Core
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.Plane
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Process
import System.IO
import Data.List
import Data.Maybe ( catMaybes, isJust )

-- }}}

-- hooks {{{
myManageHook = (composeAll . concat $
    [ [className =? c --> doF (W.shift "web")  | c <- myWebs]
    , [className =? c --> doF (W.shift "img")  | c <- myImgs]
    , [className =? c --> doF (W.shift "IM")   | c <- myIms]
    , [className =? c --> doF (W.shift "fm")   | c <- myFms]
    , [className =? c --> doF (W.shift "laut") | c <- myLauts]
    , [className =? c --> doF (W.swapDown)     | c <- mySwapDowns]
    , [className =? c --> doFloat              | c <- myFloats]
    ]) <+> mySpecialHooks
    where
       myWebs      = ["Firefox"]
       myImgs      = ["Gimp","Gimp-2.6"]
       myIms       = ["Pidgin","Skype"]
       myFms       = ["pcmanfm","Krusader","Dolphin"]
       myLauts     = ["Amarok"]
       mySwapDowns = ["Skype","Pidgin"]
       myFloats    = ["Truecrypt","."]

       mySpecialHooks = composeAll
            [ (role =? "gimp-toolbox" <||> role =? "gimp-image-window" <||> role =? "gimp-dock") --> (ask >>= doF . W.sink)
            , name =? "Copying" --> doCenterFloat -- Krusader copy dialog
            , isFullscreen --> doFullFloat -- fullscreen flash and stuff
            , transience' -- focus parent windows of transient ones
            ]
        
       role = stringProperty "WM_WINDOW_ROLE"
       name = stringProperty "WM_NAME"


--myManageHook = composeAll --composeAll applies every hook, instead of composeOne which will stop after the first match
--    [ (role =? "gimp-toolbox" <||> role =? "gimp-image-window" <||> role =? "gimp-dock") --> (ask >>= doF . W.sink)
--    , name =? "Copying" --> doCenterFloat -- Krusader copy dialog
--	, className =? "Gimp" --> doF (W.shift "img")
--	, className =? "Gimp-2.6" --> doF (W.shift "img") -- sometimes gimp has "Gimp-2.6" as classname
--  , className =? "Pidgin" --> doF (W.shift "IM")
--  , className =? "Pidgin" --> doF (W.swapDown)
--  , className =? "Skype" --> doF (W.shift "IM")
--  , className =? "Skype" --> doF (W.swapDown)
--  , className =? "Firefox" --> doF (W.shift "web")
--  , className =? "." --> doFloat -- eclipse splash
--    , (className =? "truecrypt" <||> className =? "Truecrypt") --> doFloat -- Truecrypt password dialog
--    , (className =? "pcmanfm" <||> className =? "Pcmanfm") --> doF (W.shift "fm")
--    , (className =? "amarok" <||> className =? "Amarok") --> doF (W.shift "laut")
--    , isFullscreen --> doFullFloat -- fullscreen flash and stuff
--    , transience' -- focus parent windows of transient ones
--	]
--    where
--        role = stringProperty "WM_WINDOW_ROLE"
--        name = stringProperty "WM_NAME"

newManageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig

data ThinkLightUrgencyHook = ThinkLightUrgencyHook
                    { blinks :: Int -- ^ number of times to blink the thinklight 
                    }
     deriving (Read, Show)

instance UrgencyHook ThinkLightUrgencyHook where
    urgencyHook ThinkLightUrgencyHook { blinks = d } w = do
    spawn ("thinkalert " ++ show d)
    return () 
    
myUrgencyHook = withUrgencyHook ThinkLightUrgencyHook
    { blinks = 2 } 

myLayoutHook = onWorkspace "IM" (named "myIM" imlayout) $
               onWorkspace "img" (named "myImg" gimp) $
               named "myTall" (myLayoutMods tiled) |||
               named "myMirrorTall"  (myLayoutMods $ Mirror tiled) |||
               named "myMagnifyTall" (myLayoutMods $ magnify tiled) |||
               named "myFull" (myLayoutMods $ noBorders Full) |||
               named "myCross" (myLayoutMods simpleCross) |||
               named "multimedia" ( noBorders Simplest )
    where 
      tiled = ResizableTall nmaster delta ratio []
      -- imlayout makes pidgin and skype occupy 175px at either side of the screen and puts a regular tiled layout in the middle
      -- using myLayoutMods on the gimp or IM layout does more damage than it helps, so just avoidStruts here
      -- also can't just use "tiled" layout on IM because a single window wouldn't have borders
      imlayout = avoidStruts $ withIM (0.137) (Role "buddy_list") $ reflectHoriz $ ((withIM (0.159) (ClassName "Skype") (reflectHoriz tiled )))
      gimp = avoidStruts $ withIM (0.15) (Role "gimp-toolbox") $
             reflectHoriz $ withIM (0.2) (Role "gimp-dock") (ResizableTall 3 delta ratio [])
      nmaster = 1
      delta = 3/100
      ratio = 1/2
      magnify = magnifiercz (1.2)
      myLayoutMods x = maximize $ avoidStruts $ layoutHintsToCenter $ smartBorders x

myLogHook = ewmhDesktopsLogHook >> setWMName "LG3D"

myStartupHook = setWMName "LG3D" >> setDefaultCursor xC_left_ptr
--}}}

-- defines {{{

mrtDraggerOffset :: Position
mrtDraggerOffset = 1

mrtDraggerSize :: Dimension
mrtDraggerSize = 2

myWorkspaces = ["main", "web", "fm", "IM", "img", "laut"] ++ map show [7 .. 8 :: Int]

escapeColor :: String -> String
escapeColor str = "'" ++ str ++ "'"

myModMask = mod4Mask
myFont = "'-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*'"

myFgColor = "#59bbe8"
myBgColor = "#0d0d0d"
myFontColor = "#cccccc"

myFocusedBorderColor = myFgColor
myNormalBorderColor = myBgColor 

myPanelHeight = "16"
myPanelY = "0"
myTerminal = "urxvt"

myMainPanelWidth = "490"
myConkyPanelWidth = "680"
myTrayerWidth = "110"
myTrayerMargin = "1170" --mainpanel + conkypanel

myTrayerCmd  = "trayer "
            ++ " --edge top "
            ++ " --align left "
            ++ " --margin " ++ myTrayerMargin 
            ++ " --widthtype pixel "
            ++ " --height " ++ myPanelHeight
            ++ " --width " ++ myTrayerWidth
            ++ " --transparent true "
            ++ " --alpha 0 "
            ++ " --tint 0x1A1A1A "

myDzenFlags  = " -bg " ++ escapeColor myBgColor
            ++ " -fg " ++ escapeColor myFontColor
            ++ " -e 'onstart=lower' "
            ++ " -h " ++ myPanelHeight
            ++ " -fn " ++ myFont
            ++ " -sa c "
            ++ " -y " ++ myPanelY
            ++ " -xs 0 "

statusBarCmd  = "dzen2 "
             ++ myDzenFlags
             ++ " -w " ++ myMainPanelWidth
             ++ " -ta l " 

secondBarCmd  = "conky -c ~/.xmonad/conkyrc | dzen2 " 
             ++ myDzenFlags
             ++ " -w " ++ myConkyPanelWidth
             ++ " -x " ++ myMainPanelWidth
             ++ " -ta r "

myXcompmgrCmd = "xcompmgr"

myDmenuString  = "dmenu_run "
              ++ " -fn " ++ myFont
              ++ " -nb " ++ escapeColor myBgColor
              ++ " -nf " ++ escapeColor myFontColor
              ++ " -sb " ++ escapeColor myFgColor
              ++ " -sf " ++ escapeColor myBgColor
              ++ " -i " -- match case-insensitively
-- }}}

-- The prompt config {{{
myPromptConfig :: XPConfig
myPromptConfig = defaultXPConfig
    { position          = Top
    , promptBorderWidth = 0
    , font              = myFont 
    , height            = 16
    , bgColor           = myBgColor
    , fgColor           = myFgColor
    , bgHLight          = myFgColor
    , fgHLight          = myBgColor
    }
-- }}}

-- the keys config {{{
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((myModMask, xK_p), spawn myDmenuString)
    , ((myModMask, xK_b), sendMessage ToggleStruts)
    , ((myModMask, xK_y), sendMessage MirrorShrink) --resizableTall keys
    , ((myModMask, xK_a), sendMessage MirrorExpand)
    , ((myModMask .|. shiftMask, xK_l), spawn ("xlock"))
    , ((myModMask .|. shiftMask, xK_x), spawn ("alock -auth pam"))
    , ((myModMask .|. shiftMask, xK_e), spawn ("dolphin"))
    , ((myModMask .|. shiftMask, xK_f), spawn ("firefox"))
    , ((myModMask .|. shiftMask, xK_a), spawn ("amarok"))
    , ((myModMask .|. shiftMask, xK_g), spawn ("gimp"))
    , ((myModMask .|. shiftMask, xK_p), spawn ("pidgin"))
    , ((myModMask .|. shiftMask, xK_w), spawn (myTerminal ++ " -e wicd-curses"))
    , ((myModMask, xK_q), spawn ("killall dzen2 ; killall conky ; killall trayer ; killall xcompmgr ; xmonad --recompile && xmonad --restart"))
    , ((myModMask, xK_F1), (sendMessage $ JumpToLayout "myTall"))
    , ((myModMask, xK_F2), (sendMessage $ JumpToLayout "myMirrorTall"))
    , ((myModMask, xK_F3), (sendMessage $ JumpToLayout "myMagnifyTall"))
    , ((myModMask, xK_F4), (sendMessage $ JumpToLayout "myFull"))
    , ((myModMask, xK_F5), (sendMessage $ JumpToLayout "myCross"))
    , ((myModMask, xK_F8), (sendMessage $ JumpToLayout "multimedia"))
    , ((myModMask, xK_F11), spawn "killall xcompmgr")
    , ((myModMask, xK_F12), spawn ("killall xcompmgr;" ++ myXcompmgrCmd))
    , ((myModMask, xK_u), withFocused (sendMessage . maximizeRestore))
    , ((myModMask, xK_g), withFocused toggleBorder)
    , ((myModMask, xK_o), shellPrompt myPromptConfig) 
    , ((myModMask, xK_Tab), goToSelected defaultGSConfig) 
    ]
-- }}}
   
-- the PP config {{{

imagePath = "/home/corrupt/.xmonad/images/"

ppCurrentColor = dzenColor "#1a1a1a" myFocusedBorderColor 
ppVisibleColor = dzenColor myFocusedBorderColor ""
ppHiddenColor  = dzenColor myFontColor ""
ppHiddenNWColor = dzenColor myFontColor ""
ppLayoutColor = dzenColor myFontColor ""
ppTitleColor = dzenColor myFontColor ""
ppUrgentColor = dzenColor "#1a1a1a" myFontColor

myPP = dzenPP
    { ppCurrent         = ppCurrentColor . \a -> image "window-active" ++ a ++ setFgColor ++ image "vspace" 
    , ppVisible         = ppVisibleColor . wrapClickable
    , ppHidden          = ppHiddenColor . (\a -> setFgColor ++ image "window" ++ setTextColor ++ a) . wrapClickable
    , ppHiddenNoWindows = ppHiddenNWColor . (\wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId) . wrapClickable
    , ppUrgent          = ppUrgentColor . (\a -> image "window-active" ++ a ++ setTextColor ++ image "vspace") . dzenStrip 
    , ppLayout          = ppLayoutColor . wrapLayoutSwitch .
                          (\ x -> fill (case x of
                              "myTall"        -> "Tall"        ++ setFgColor ++ imagePad "tall"
                              "myMirrorTall"  -> "MirrorTall"  ++ setFgColor ++ imagePad "mtall"
                              "myFull"        -> "Full"        ++ setFgColor ++ imagePad "full"
                              "myCross"       -> "Cross"       ++ setFgColor ++ imagePad "cross"
                              "myMagnifyTall" -> "MagnifyTall" ++ setFgColor ++ imagePad "magnify"
                              "myIM"          -> "IM"          ++ setFgColor ++ imagePad "im"
                              "myImg"         -> "Gimp Grid"   ++ setFgColor ++ imagePad "gimp"
                              _               -> pad x) 4)
    , ppSep             = " | "
    , ppWsSep           = " "
    , ppTitle           = ppTitleColor . dzenEscape
    }
    where
      setFgColor = "^fg(" ++ myFgColor ++ ")"
      setTextColor = "^fg(" ++ myFontColor ++ ")"
      setBgColor = "^fg(" ++ myBgColor ++ ")"
      fill :: String -> Int -> String
      fill h i = "^p(" ++ show i ++ ")" ++ h ++ "^p(" ++ show i ++ ")"
      image :: String -> String
      image img = "^i(" ++ imagePath ++ img ++ ".xbm)"
      imagePad :: String -> String
      imagePad img = " " ++ (image img)
      currentWsIndex w = case (elemIndex w myWorkspaces) of -- needs to be modified should I decide to use DynamicWorkspaces one day
                                Nothing -> "1"
                                Just n -> show (n+1)
      wrapClickable content = "^ca(1,xdotool key super+" ++  (currentWsIndex content) ++ ")" ++ content ++ "^ca()"
      wrapLayoutSwitch content = "^ca(1,xdotool key super+space)" ++ content ++ "^ca()"

-- }}}

main = do
     secondBar <- spawnPipe secondBarCmd
     din <- spawnPipe statusBarCmd
     spawn myTrayerCmd
     spawn myXcompmgrCmd 
     xmonad $ myUrgencyHook
            $ ewmh defaultConfig
        { manageHook = newManageHook 
		, layoutHook = myLayoutHook
        , startupHook = myStartupHook
		, focusedBorderColor = myFocusedBorderColor
		, normalBorderColor = myNormalBorderColor
        , borderWidth = 2
		, workspaces = myWorkspaces
		, modMask = myModMask
        , keys = \c -> myKeys c `M.union` keys defaultConfig c
        , logHook = myLogHook >> (dynamicLogWithPP $ myPP
                { ppOutput = hPutStrLn din
                })
		, terminal = myTerminal
	 }

-- vim: fdm=marker ts=4 sw=4 sts=4 et:
