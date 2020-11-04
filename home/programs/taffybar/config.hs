{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Functor                   ( void )
import           System.Taffybar
import           System.Taffybar.Context        ( TaffybarConfig(..) )
import           System.Taffybar.Hooks
import           System.Taffybar.Information.CPU2
import           System.Taffybar.Information.Memory
import           System.Taffybar.SimpleConfig
import           System.Taffybar.Util           ( runCommandFromPath )
import           System.Taffybar.Widget
import           System.Taffybar.Widget.Generic.PollingGraph

main :: IO ()
main = dyreTaffybar . appendHook notifySystemD $ myConfig

transparent, yellow1, yellow2, green1, green2, taffyBlue
  :: (Double, Double, Double, Double)
transparent = (0.0, 0.0, 0.0, 0.0)
yellow1 = (0.9453125, 0.63671875, 0.2109375, 1.0)
yellow2 = (0.9921875, 0.796875, 0.32421875, 1.0)
green1 = (0, 1, 0, 1)
green2 = (1, 0, 1, 0.5)
taffyBlue = (0.129, 0.588, 0.953, 1)

myGraphConfig :: GraphConfig
myGraphConfig = defaultGraphConfig { graphPadding         = 0
                                   , graphBorderWidth     = 0
                                   , graphWidth           = 75
                                   , graphBackgroundColor = transparent
                                   }

netCfg :: GraphConfig
netCfg = myGraphConfig { graphDataColors = [yellow1, yellow2]
                       , graphLabel      = Just "net"
                       }

memCfg :: GraphConfig
memCfg = myGraphConfig { graphDataColors = [taffyBlue]
                       , graphLabel      = Just "mem"
                       }

cpuCfg :: GraphConfig
cpuCfg = myGraphConfig { graphDataColors = [green1, green2]
                       , graphLabel      = Just "cpu"
                       }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = getCPULoad "cpu"

notifySystemD = void $ runCommandFromPath ["systemd-notify", "--ready"]

myConfig :: TaffybarConfig
myConfig =
  let myWorkspacesConfig = defaultWorkspacesConfig
        { minIcons        = 1
        , widgetGap       = 0
        , showWorkspaceFn = hideEmpty
        }
      workspaces = workspacesNew myWorkspacesConfig
      bat        = textBatteryNew "$percentage$%"
      cpu        = pollingGraphNew cpuCfg 1 cpuCallback
      mem        = pollingGraphNew memCfg 1 memCallback
      net        = networkGraphNew netCfg Nothing
      netmon     = networkMonitorNew defaultNetFormat Nothing
      clock      = textClockNewWith defaultClockConfig
        { clockUpdateStrategy = ConstantInterval 1.0
        , clockFormatString   = "%a %b %d %I:%M:%S %p"
        }
      layout     = layoutNew defaultLayoutConfig
      windowsW   = windowsNew defaultWindowsConfig
      -- See https://github.com/taffybar/gtk-sni-tray#statusnotifierwatcher
      -- for a better way to set up the sni tray
      tray       = sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
      myConfig   = defaultSimpleTaffyConfig
        { startWidgets  = workspaces : map (>>= buildContentsBox) [layout, windowsW]
        , endWidgets    = map (>>= buildContentsBox)
                            [bat, batteryIconNew, clock, tray, cpu, mem, net, netmon, mpris2New]
        , barPosition   = Top
        , barPadding    = 10
        , barHeight     = 50
        , widgetSpacing = 1
        }
  in  withBatteryRefresh . withLogServer . withToggleServer . toTaffyConfig $ myConfig
