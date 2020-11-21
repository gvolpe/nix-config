import           Data.Foldable                         ( traverse_ )
import           Data.Monoid
import           Graphics.X11.ExtraTypes.XF86
import           System.Exit
import           System.IO                             ( hPutStr
                                                       , hClose
                                                       )
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
import           XMonad.Actions.SpawnOn                ( manageSpawn
                                                       , spawnOn
                                                       )
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
                                                       , docksEventHook
                                                       )
import           XMonad.Hooks.ManageHelpers            ( (-?>)
                                                       , isDialog
                                                       , isFullscreen
                                                       , isInProperty
                                                       , doCenterFloat
                                                       , doFullFloat
                                                       )
import           XMonad.Hooks.UrgencyHook              ( UrgencyHook(..)
                                                       , withUrgencyHook
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
import           XMonad.Layout.ThreeColumns            ( ThreeCol(..) )
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
import           XMonad.Util.Run                       ( safeSpawn
                                                       , spawnPipe
                                                       )
import           XMonad.Util.SpawnOnce                 ( spawnOnce )
import           XMonad.Util.WorkspaceCompare          ( getSortByIndex )

import qualified Control.Exception                     as E
import qualified Data.Map                              as M
import qualified XMonad.StackSet                       as W
import qualified XMonad.Util.NamedWindows              as W

-- Imports for Polybar --
import qualified Codec.Binary.UTF8.String              as UTF8
import qualified DBus                                  as D
import qualified DBus.Client                           as D
import           XMonad.Hooks.DynamicLog

main :: IO ()
main = mkDbusClient >>= main'

main' :: D.Client -> IO ()
main' dbus = xmonad . docks . ewmh . dynProjects . keybindings . urgencyHook $ def
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
  , logHook            = myPolybarLogHook dbus
  , startupHook        = myStartupHook
  }
 where
  myModMask   = mod4Mask -- super as the mod key
  dynProjects = dynamicProjects projects
  keybindings = addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
  urgencyHook = withUrgencyHook LibNotifyUrgencyHook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
myStartupHook = startupHook def

-- original idea: https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- W.getName w
    maybeIdx <- W.findTag w <$> gets windowset
    traverse_ (\i -> safeSpawn "notify-send" [show name, "workspace " ++ i]) maybeIdx

------------------------------------------------------------------------
-- Polybar settings (needs DBus client).
--
mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = (D.signal opath iname mname)
      body   = [D.toVariant $ UTF8.decodeString str]
  in  D.emit dbus $ signal { D.signalBody = body }

polybarHook :: D.Client -> PP
polybarHook dbus =
  let wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
                  | otherwise  = mempty
      blue   = "#2E9AFE"
      gray   = "#7F7F7F"
      orange = "#ea4300"
      purple = "#9058c7"
      red    = "#722222"
  in  def { ppOutput          = dbusOutput dbus
          , ppCurrent         = wrapper blue
          , ppVisible         = wrapper gray
          , ppUrgent          = wrapper orange
          , ppHidden          = wrapper gray
          , ppHiddenNoWindows = wrapper red
          , ppTitle           = shorten 100 . wrapper purple
          }

myPolybarLogHook dbus = myLogHook <+> dynamicLogWithPP (polybarHook dbus)

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

myTerminal   = "alacritty"
appLauncher  = "rofi -modi drun,ssh,window -show drun -show-icons"
screenLocker = "betterlockscreen -l dim"
playerctl c  = "playerctl --player=spotify,%any " <> c

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" . io $
  E.bracket (spawnPipe $ getAppCommand zenity) hClose (\h -> hPutStr h (unlines $ showKm x))

myKeys conf@XConfig {XMonad.modMask = modm} =
  keySet "Applications"
    [ key "Slack"         (modm                , xK_F2      ) $ spawnOn comWs "slack"
    ] ^++^
  keySet "Audio"
    [ key "Mute"          (0, xF86XK_AudioMute              ) $ spawn "amixer -q set Master toggle"
    , key "Lower volume"  (0, xF86XK_AudioLowerVolume       ) $ spawn "amixer -q set Master 5%-"
    , key "Raise volume"  (0, xF86XK_AudioRaiseVolume       ) $ spawn "amixer -q set Master 5%+"
    , key "Play / Pause"  (0, xF86XK_AudioPlay              ) $ spawn $ playerctl "play-pause"
    , key "Stop"          (0, xF86XK_AudioStop              ) $ spawn $ playerctl "stop"
    , key "Previous"      (0, xF86XK_AudioPrev              ) $ spawn $ playerctl "previous"
    , key "Next"          (0, xF86XK_AudioNext              ) $ spawn $ playerctl "next"
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
  keySet "Polybar"
    [ key "Hide"          (modm              , xK_minus     ) $ spawn "polybar-msg cmd hide &"
    , key "Show"          (modm              , xK_equal     ) $ spawn "polybar-msg cmd show &"
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
myLayout =
  avoidStruts
    . smartBorders
    . fullScreenToggle
    . comLayout $ (tiled ||| Mirror tiled ||| column3 ||| full)
   where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = gapSpaced 10 $ Tall nmaster delta ratio
     full    = gapSpaced 5 Full
     column3 = gapSpaced 10 $ ThreeColMid 1 (3/100) (1/2)

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
     comLayout = onWorkspace comWs full

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

type AppName      = String
type AppTitle     = String
type AppClassName = String
type AppCommand   = String

data App
  = ClassApp AppClassName AppCommand
  | TitleApp AppTitle AppCommand
  | NameApp AppName AppCommand
  deriving Show

eog      = NameApp  "eog"                  "eog"
gimp     = ClassApp "Gimp"                 "gimp"
nautilus = ClassApp "Org.gnome.Nautilus"   "nautilus"
pavuctrl = ClassApp "Pavucontrol"          "pavucontrol"
scr      = ClassApp "SimpleScreenRecorder" "simplescreenrecorder"
spotify  = ClassApp "Spotify"              "spotify -force-device-scale-factor=2.0 %U"
vlc      = ClassApp "Vlc"                  "vlc"
ytop     = TitleApp "ytop"                 "alacritty -t ytop -e ytop"
zenity   = ClassApp "Zenity"               "zenity --text-info --font=terminus"

myManageHook = manageApps <+> manageSpawn <+> manageScratchpads
 where
  isBrowserDialog     = isDialog <&&> className =? "Brave-browser"
  isFileChooserDialog = isRole =? "GtkFileChooserDialog"
  isPopup             = isRole =? "pop-up"
  isSplash            = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
  isRole              = stringProperty "WM_WINDOW_ROLE"
  tileBelow           = insertPosition Below Newer
  manageScratchpads = namedScratchpadManageHook scratchpads
  manageApps = composeAll
    [ isInstance gimp                                 --> doFloat
    , isInstance spotify                              --> doFullFloat
    , (
        isInstance eog <||> isInstance nautilus <||>
        isInstance pavuctrl <||> isInstance scr
      )                                               --> doCenterFloat
    , (
        isInstance vlc <||> isInstance ytop <||>
        isInstance zenity
      )                                               --> doFullFloat
    , resource  =? "desktop_window"                   --> doIgnore
    , resource  =? "kdesktop"                         --> doIgnore
    , (
        isBrowserDialog <||> isFileChooserDialog <||>
        isDialog <||> isPopup <||> isSplash
      )                                               --> doCenterFloat
    , isFullscreen                                    --> doFullFloat
    , pure True                                       --> tileBelow
    ]

isInstance (ClassApp c _) = className =? c
isInstance (TitleApp t _) = title =? t
isInstance (NameApp n _)  = appName =? n

getNameCommand (ClassApp n c) = (n, c)
getNameCommand (TitleApp n c) = (n, c)
getNameCommand (NameApp  n c) = (n, c)

getAppName    = fst . getNameCommand
getAppCommand = snd . getNameCommand

scratchpadApp :: App -> NamedScratchpad
scratchpadApp app = NS (getAppName app) (getAppCommand app) (isInstance app) defaultFloating

runScratchpadApp = namedScratchpadAction scratchpads . getAppName

scratchpads = scratchpadApp <$> [ nautilus, scr, spotify, ytop ]

------------------------------------------------------------------------
-- Workspaces
--
webWs = "web"
ossWs = "oss"
devWs = "dev"
comWs = "com"
sysWs = "sys"
etcWs = "etc"

myWS :: [WorkspaceId]
myWS = [webWs, ossWs, devWs, comWs, sysWs, etcWs]

------------------------------------------------------------------------
-- Dynamic Projects
--
projects :: [Project]
projects =
  [ Project { projectName      = webWs
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn "brave"
            }
  , Project { projectName      = ossWs
            , projectDirectory = "~/"
            , projectStartHook = Just $ do sequence_ (replicate 2 $ spawn myTerminal)
                                           spawn $ myTerminal <> " -e home-manager edit"
            }
  , Project { projectName      = devWs
            , projectDirectory = "~/workspace/cr/app"
            , projectStartHook = Just . sequence_ . replicate 3 $ spawn myTerminal
            }
  , Project { projectName      = comWs
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawn "telegram-desktop"
            }
  , Project { projectName      = sysWs
            , projectDirectory = "/etc/nixos/"
            , projectStartHook = Just . spawn $ myTerminal <> " -e sudo su"
            }
  , Project { projectName      = etcWs
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  ]

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
myEventHook = docksEventHook <+> ewmhDesktopsEventHook <+> fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = fadeInactiveLogHook 0.9
