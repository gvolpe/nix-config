import           Data.Monoid
import           Graphics.X11.ExtraTypes.XF86
import           System.Exit
import           System.Taffybar.Support.PagerHints    ( pagerHints )
import           XMonad
import           XMonad.Hooks.EwmhDesktops             ( ewmh )
import           XMonad.Hooks.ManageDocks              ( Direction2D(..)
                                                       , ToggleStruts(..)
                                                       , avoidStruts
                                                       , docks
                                                       )
import           XMonad.Hooks.ManageHelpers            ( doCenterFloat
                                                       , doFullFloat
                                                       )
import           XMonad.Layout.Gaps                    ( gaps )
import           XMonad.Layout.Spacing                 ( spacing )
import           XMonad.Util.NamedScratchpad           ( NamedScratchpad(..)
                                                       , customFloating
                                                       , defaultFloating
                                                       , namedScratchpadAction
                                                       , namedScratchpadManageHook
                                                       )
import           XMonad.Util.Run                       ( spawnPipe )
import           XMonad.Util.SpawnOnce                 ( spawnOnce )

import qualified XMonad.StackSet                       as W
import qualified Data.Map                              as M

------------------------------------------------------------------------

main :: IO ()
main = xmonad . docks . ewmh . pagerHints $ def
  { terminal           = "terminator"
  , focusFollowsMouse  = False
  , clickJustFocuses   = False
  , borderWidth        = 3
  , modMask            = mod4Mask -- super as the mod key
  , workspaces         = ["web", "oss", "dev", "chat", "etc"]
  , normalBorderColor  = "#dddddd" -- light gray (default)
  , focusedBorderColor = "#1681f2" -- blue

  -- key bindings
  , keys               = myKeys
  , mouseBindings      = myMouseBindings

  -- hooks, layouts
  , layoutHook         = myLayout
  , manageHook         = myManageHook
  , handleEventHook    = myEventHook
  , logHook            = myLogHook
  , startupHook        = myStartupHook
  }

taffybarExec = "taffybar-linux-x86_64.taffybar-wrapped"

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
myStartupHook = do
  spawnOnceIf "status-notifier-watcher"
  spawnOnce "nitrogen --restore &"
  spawn $ "pidof " <> taffybarExec <> " && killall -q " <> taffybarExec
  spawn "taffybar &"
  spawnOnce "nm-applet --sm-disable --indicator &"
  --spawnPipe "xmobar -x 0 /home/gvolpe/.config/xmobar/config"

spawnOnceIf p =
  spawnOnce $ "if [ -z $(pidof " <> p <> ") ] ; then " <> p <> " & fi"

appLauncher  = "rofi -modi drun,ssh,window -show drun -show-icons"
screenLocker = "betterlockscreen -l dim"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn appLauncher)

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- lock screen
    , ((modm .|. controlMask, xK_l   ), spawn screenLocker)

    -- Mute volume
    , ((0, xF86XK_AudioMute          ), spawn "amixer -q set Master toggle")

    -- Decrease volume
    , ((0, xF86XK_AudioLowerVolume   ), spawn "amixer -q set Master 5%-")

    -- Increase volume
    , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer -q set Master 5%+")

    -- Play / Pause
    , ((0, xF86XK_AudioPlay          ), spawn "playerctl play-pause")

    -- Stop
    , ((0, xF86XK_AudioStop          ), spawn "playerctl stop")

    -- Previous song
    , ((0, xF86XK_AudioPrev          ), spawn "playerctl previous")

    -- Next song
    , ((0, xF86XK_AudioNext          ), spawn "playerctl next")

    {----------------- Scratchpads ---------------------}

    -- run spotify (or show if already running)
    , ((modm .|. controlMask,  xK_s  ), runScratchpadApp spotify)

    -- run neofetch (or show if already running)
    , ((modm .|. controlMask,  xK_n  ), runScratchpadApp neofetch)

    -- run nautilus (or show if already running)
    , ((modm .|. controlMask,  xK_f  ), runScratchpadApp nautilus)

    -- run ytop (or show if already running)
    , ((modm .|. controlMask,  xK_y  ), runScratchpadApp ytop)

    {----------------- Windows ---------------------}

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

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
myLayout = avoidStruts (tiled ||| Mirror tiled ||| full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled    = gapSpaced 10 $ Tall nmaster delta ratio
     full     = gapSpaced 5 $ Full

     -- The default number of windows in the master pane
     nmaster  = 1

     -- Default proportion of screen occupied by master pane
     ratio    = 1/2

     -- Percent of screen to increment by when resizing panes
     delta    = 3/100

     -- Gaps bewteen windows
     myGaps gap  = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
     gapSpaced g = spacing g . myGaps g

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

gimp     = App "gimp"     "Gimp"               "gimp"
nautilus = App "files"    "Org.gnome.Nautilus" "nautilus"
neofetch = App "neofetch" "neofetch"           "alacritty -t neofetch -e neofetch"
pavuctrl = App "pactl"    "Pavucontrol"        "pavucontrol"
spotify  = App "spotify"  "Spotify"            "spotify -force-device-scale-factor=2.0 %U"
ytop     = App "ytop"     "ytop"               "alacritty -t ytop -e ytop"

manageApps = composeAll
  [ className =? appClassName gimp     --> doFloat
  , className =? appClassName spotify  --> doFullFloat
  , className =? appClassName nautilus --> doCenterFloat
  , className =? appClassName pavuctrl --> doCenterFloat
  , title     =? appTitle ytop         --> doFullFloat
  , title     =? appTitle neofetch     --> doCenterFloat
  , resource  =? "desktop_window"      --> doIgnore
  , resource  =? "kdesktop"            --> doIgnore
  ]

myManageHook = manageApps <+> namedScratchpadManageHook scratchpads

scratchpadApp :: Query String -> App -> NamedScratchpad
scratchpadApp query (App t cn cmd) = NS t cmd (query =? cn) defaultFloating

runScratchpadApp (App t _ _) = namedScratchpadAction scratchpads t

scratchpads =
  let byTitle = scratchpadApp title     <$> [ neofetch, ytop ]
      byClass = scratchpadApp className <$> [ nautilus, spotify ]
  in  byTitle <> byClass

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()
