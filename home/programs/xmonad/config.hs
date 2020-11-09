import           Data.Monoid
import           Graphics.X11.ExtraTypes.XF86
import           System.Exit
import           System.IO                             ( hPutStr
                                                       , hClose
                                                       )
import           System.Taffybar.Support.PagerHints    ( pagerHints )
import           XMonad
import           XMonad.Actions.CycleWS                ( Direction1D(..)
                                                       , WSType(..)
                                                       , findWorkspace
                                                       )
import           XMonad.Actions.DynamicProjects        ( Project(..)
                                                       , dynamicProjects
                                                       , switchProjectPrompt
                                                       )
import           XMonad.Actions.DynamicWorkspaces      ( removeWorkspace )
import           XMonad.Actions.FloatKeys              ( keysAbsResizeWindow
                                                       , keysResizeWindow
                                                       )
import           XMonad.Actions.RotSlaves              ( rotSlavesUp )
import           XMonad.Hooks.EwmhDesktops             ( ewmh
                                                       , ewmhDesktopsEventHook
                                                       , fullscreenEventHook
                                                       )
import           XMonad.Hooks.FadeInactive             ( fadeInactiveLogHook )
import           XMonad.Hooks.InsertPosition           ( Focus(Newer)
                                                       , Position(Below)
                                                       , insertPosition
                                                       )
import           XMonad.Hooks.ManageDocks              ( Direction2D(..)
                                                       , ToggleStruts(..)
                                                       , avoidStruts
                                                       , docks
                                                       )
import           XMonad.Hooks.ManageHelpers            ( (-?>)
                                                       , isDialog
                                                       , isFullscreen
                                                       , isInProperty
                                                       , doCenterFloat
                                                       , doFullFloat
                                                       )
import           XMonad.Layout.Gaps                    ( gaps )
import           XMonad.Layout.MultiToggle             ( Toggle(..)
                                                       , mkToggle
                                                       , single
                                                       )
import           XMonad.Layout.MultiToggle.Instances   ( StdTransformers(NBFULL) )
import           XMonad.Layout.NoBorders               ( smartBorders )
import           XMonad.Layout.PerWorkspace            ( onWorkspace )
import           XMonad.Layout.Spacing                 ( spacing )
import           XMonad.Prompt                         ( XPConfig(..)
                                                       , amberXPConfig
                                                       , XPPosition(CenteredAt)
                                                       )
import           XMonad.Util.EZConfig                  ( mkNamedKeymap )
import           XMonad.Util.NamedActions              ( (^++^)
                                                       , NamedAction (..)
                                                       , addDescrKeys'
                                                       , addName
                                                       , showKm
                                                       , subtitle
                                                       )
import           XMonad.Util.NamedScratchpad           ( NamedScratchpad(..)
                                                       , customFloating
                                                       , defaultFloating
                                                       , namedScratchpadAction
                                                       , namedScratchpadManageHook
                                                       )
import           XMonad.Util.Run                       ( spawnPipe )
import           XMonad.Util.SpawnOnce                 ( spawnOnce )
import           XMonad.Util.WorkspaceCompare          ( getSortByIndex )

import qualified Data.Map                              as M
import qualified XMonad.StackSet                       as W

main :: IO ()
main = xmonad . docks . ewmh . pagerHints . dynProjects . keybindings $ def
  { terminal           = myTerminal
  , focusFollowsMouse  = False
  , clickJustFocuses   = False
  , borderWidth        = 3
  , modMask            = myModMask
  , workspaces         = myWS
  , normalBorderColor  = "#dddddd" -- light gray (default)
  , focusedBorderColor = "#1681f2" -- blue
  , mouseBindings      = myMouseBindings
  , layoutHook         = myLayout
  , manageHook         = myManageHook
  , handleEventHook    = myEventHook
  , logHook            = myLogHook
  , startupHook        = myStartupHook
  }
 where
  myModMask   = mod4Mask -- super as the mod key
  dynProjects = dynamicProjects projects
  keybindings = addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
myStartupHook = startupHook def

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

myTerminal   = "alacritty"
appLauncher  = "rofi -modi drun,ssh,window -show drun -show-icons"
screenLocker = "betterlockscreen -l dim"

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe "zenity --text-info --font=terminus"
  hPutStr h (unlines $ showKm x)
  hClose h

myKeys conf@XConfig {XMonad.modMask = modm} =
  keySet "Audio"
    [ key "Mute"          (0, xF86XK_AudioMute              ) $ spawn "amixer -q set Master toggle"
    , key "Lower volume"  (0, xF86XK_AudioLowerVolume       ) $ spawn "amixer -q set Master 5%-"
    , key "Raise volume"  (0, xF86XK_AudioRaiseVolume       ) $ spawn "amixer -q set Master 5%+"
    , key "Play / Pause"  (0, xF86XK_AudioPlay              ) $ spawn "playerctl play-pause"
    , key "Stop"          (0, xF86XK_AudioStop              ) $ spawn "playerctl stop"
    , key "Previous"      (0, xF86XK_AudioPrev              ) $ spawn "playerctl previous"
    , key "Next"          (0, xF86XK_AudioNext              ) $ spawn "playerctl next"
    ] ^++^
  keySet "Launchers"
    [ key "Terminal"      (modm .|. shiftMask  , xK_Return  ) $ spawn (XMonad.terminal conf)
    , key "Apps (Rofi)"   (modm                , xK_p       ) $ spawn appLauncher
    , key "Lock screen"   (modm .|. controlMask, xK_l       ) $ spawn screenLocker
    ] ^++^
  keySet "Layouts"
    [ key "Next"          (modm              , xK_space     ) $ sendMessage NextLayout
    , key "Reset"         (modm .|. shiftMask, xK_space     ) $ setLayout (XMonad.layoutHook conf)
    , key "Fullscreen"    (modm              , xK_f         ) $ sendMessage (Toggle NBFULL)
    ] ^++^
  keySet "Projects"
    [ key "Switch prompt" (modm              , xK_o         ) $ switchProjectPrompt projectsTheme
    ] ^++^
  keySet "Scratchpads"
    [ key "Files"           (modm .|. controlMask,  xK_f    ) $ runScratchpadApp nautilus
    , key "Screen recorder" (modm .|. controlMask,  xK_r    ) $ runScratchpadApp scr
    , key "Spotify"         (modm .|. controlMask,  xK_s    ) $ runScratchpadApp spotify
    , key "ytop"            (modm .|. controlMask,  xK_y    ) $ runScratchpadApp ytop
    ] ^++^
  keySet "Screens" switchScreen ^++^
  keySet "System"
    [ key "Toggle status bar gap" (modm              , xK_b ) $ sendMessage ToggleStruts
    , key "Logout (quit XMonad)"  (modm .|. shiftMask, xK_q ) $ io exitSuccess
    , key "Restart XMonad"        (modm              , xK_q ) $ spawn "xmonad --recompile; xmonad --restart"
    ] ^++^
  keySet "Windows"
    [ key "Close focused"  (modm              , xK_BackSpace) kill
    , key "Refresh size"   (modm              , xK_n        ) refresh
    , key "Focus next"     (modm              , xK_j        ) $ windows W.focusDown
    , key "Focus previous" (modm              , xK_k        ) $ windows W.focusUp
    , key "Focus master"   (modm              , xK_m        ) $ windows W.focusMaster
    , key "Swap master"    (modm              , xK_Return   ) $ windows W.swapMaster
    , key "Swap next"      (modm .|. shiftMask, xK_j        ) $ windows W.swapDown
    , key "Swap previous"  (modm .|. shiftMask, xK_k        ) $ windows W.swapUp
    , key "Shrink master"  (modm              , xK_h        ) $ sendMessage Shrink
    , key "Expand master"  (modm              , xK_l        ) $ sendMessage Expand
    , key "Switch to tile" (modm              , xK_t        ) $ withFocused (windows . W.sink)
    , key "Rotate slaves"  (modm .|. shiftMask, xK_Tab      ) rotSlavesUp
    , key "Decrease size"  (modm              , xK_d        ) $ withFocused (keysResizeWindow (-10,-10) (1,1))
    , key "Increase size"  (modm              , xK_s        ) $ withFocused (keysResizeWindow (10,10) (1,1))
    , key "Decr  abs size" (modm .|. shiftMask, xK_d        ) $ withFocused (keysAbsResizeWindow (-10,-10) (1024,752))
    , key "Incr  abs size" (modm .|. shiftMask, xK_s        ) $ withFocused (keysAbsResizeWindow (10,10) (1024,752))
    ] ^++^
  keySet "Workspaces"
    [ key "Next"          (modm              , xK_period    ) nextWS'
    , key "Previous"      (modm              , xK_comma     ) prevWS'
    , key "Remove"        (modm .|. shiftMask, xK_BackSpace ) removeWorkspace
    ] ++ switchWsById
 where
  keySet s ks = subtitle s : ks
  key n k a = (k, addName n a)
  action m = if m == shiftMask then "Move to " else "Switch to "
  -- mod-[1..9]: Switch to workspace N | mod-shift-[1..9]: Move client to workspace N
  switchWsById =
    [ key (action m <> show i) (m .|. modm, k) (windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  switchScreen =
    [ key (action m <> show sc) (m .|. modm, k) (screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m)  <- [(W.view, 0), (W.shift, shiftMask)]]

----------- Cycle through workspaces one by one but filtering out NSP (scratchpads) -----------

nextWS' = switchWS Next
prevWS' = switchWS Prev

switchWS dir =
  findWorkspace filterOutNSP dir AnyWS 1 >>= windows . W.view

filterOutNSP =
  let g f xs = filter (\(W.Workspace t _ _) -> t /= "NSP") (f xs)
  in  g <$> getSortByIndex

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts . smartBorders . fullScreenToggle . comWs . devWs $ (tiled ||| Mirror tiled ||| full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = gapSpaced 10 $ Tall nmaster delta ratio
     full    = gapSpaced 5 Full

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

     -- Gaps bewteen windows
     myGaps gap  = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
     gapSpaced g = spacing g . myGaps g

     -- Per workspace layout
     comWs = onWorkspace "com" full
     devWs = onWorkspace "dev" (Mirror tiled)

     -- Fullscreen
     fullScreenToggle = mkToggle (single NBFULL)

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--

type AppTitle     = String
type AppClassName = String
type AppCommand   = String

data App = App
  { appTitle :: AppTitle
  , appClassName :: AppClassName
  , appCommand :: AppCommand
  } deriving Show

gimp     = App "gimp"     "Gimp"                 "gimp"
nautilus = App "files"    "Org.gnome.Nautilus"   "nautilus"
pavuctrl = App "pactl"    "Pavucontrol"          "pavucontrol"
scr      = App "scr"      "SimpleScreenRecorder" "simplescreenrecorder"
spotify  = App "spotify"  "Spotify"              "spotify -force-device-scale-factor=2.0 %U"
ytop     = App "ytop"     "ytop"                 "alacritty -t ytop -e ytop"

myManageHook = manageApps <+> manageScratchpads
 where
  isBrowserDialog     = isDialog <&&> className =? "Brave-browser"
  isFileChooserDialog = isRole =? "GtkFileChooserDialog"
  isPopup             = isRole =? "pop-up"
  isSplash            = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
  isRole              = stringProperty "WM_WINDOW_ROLE"
  tileBelow           = insertPosition Below Newer
  manageScratchpads = namedScratchpadManageHook scratchpads
  manageApps = composeAll
    [ className =? appClassName gimp     --> doFloat
    , className =? appClassName spotify  --> doFullFloat
    , className =? appClassName nautilus --> doCenterFloat
    , className =? appClassName pavuctrl --> doCenterFloat
    , className =? appClassName scr      --> doCenterFloat
    , title     =? appTitle ytop         --> doFullFloat
    , className =? "Vlc"                 --> doFullFloat
    , className =? "Zenity"              --> doFullFloat
    , appName   =? "eog"                 --> doCenterFloat
    , resource  =? "desktop_window"      --> doIgnore
    , resource  =? "kdesktop"            --> doIgnore
    -- dialogs
    , isBrowserDialog     --> doCenterFloat
    , isFileChooserDialog --> doCenterFloat
    , isDialog            --> doCenterFloat
    , isPopup             --> doCenterFloat
    , isSplash            --> doCenterFloat
    -- misc
    , isFullscreen --> doFullFloat
    , pure True    --> tileBelow
    ]

scratchpadApp :: Query String -> App -> NamedScratchpad
scratchpadApp query (App t cn cmd) = NS t cmd (query =? cn) defaultFloating

runScratchpadApp (App t _ _) = namedScratchpadAction scratchpads t

scratchpads =
  let byTitle = scratchpadApp title     <$> [ ytop ]
      byClass = scratchpadApp className <$> [ nautilus, scr, spotify ]
  in  byTitle <> byClass

------------------------------------------------------------------------
-- Dynamic Projects
--
projects :: [Project]
projects =
  [ Project { projectName      = "web"
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn "brave"
            }
  , Project { projectName      = "oss"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do sequence_ (replicate 2 $ spawn myTerminal)
                                           spawn $ myTerminal <> " -e home-manager edit"
            }
  , Project { projectName      = "dev"
            , projectDirectory = "~/workspace/cr/app"
            , projectStartHook = Just . sequence_ . replicate 3 $ spawn myTerminal
            }
  , Project { projectName      = "com"
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn "telegram-desktop"
            }
  , Project { projectName      = "sys"
            , projectDirectory = "~/"
            , projectStartHook = Just . spawn $ myTerminal <> " -e sudo su"
            }
  ]

myWS = projectName <$> projects

projectsTheme :: XPConfig
projectsTheme = amberXPConfig
  { bgHLight = "#002b36"
  , font     = "xft:Bitstream Vera Sans Mono:size=8:antialias=true"
  , height   = 50
  , position = CenteredAt 0.5 0.5
  }

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = ewmhDesktopsEventHook <> fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = fadeInactiveLogHook 0.8
