{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Taffybar
import           System.Taffybar.Context        ( TaffybarConfig(..) )
import           System.Taffybar.Hooks
import           System.Taffybar.Information.CPU
import           System.Taffybar.Information.Memory
import           System.Taffybar.SimpleConfig
import           System.Taffybar.Widget
import           System.Taffybar.Widget.Generic.PollingGraph

main :: IO ()
main = dyreTaffybar exampleTaffybarConfig

transparent, yellow1, yellow2, green1, green2, taffyBlue
  :: (Double, Double, Double, Double)
transparent = (0.0, 0.0, 0.0, 0.0)
yellow1 = (0.9453125, 0.63671875, 0.2109375, 1.0)
yellow2 = (0.9921875, 0.796875, 0.32421875, 1.0)
green1 = (0, 1, 0, 1)
green2 = (1, 0, 1, 0.5)
taffyBlue = (0.129, 0.588, 0.953, 1)

myGraphConfig, netCfg, memCfg, cpuCfg :: GraphConfig
myGraphConfig = defaultGraphConfig { graphPadding         = 0
                                   , graphBorderWidth     = 0
                                   , graphWidth           = 75
                                   , graphBackgroundColor = transparent
                                   }

netCfg = myGraphConfig { graphDataColors = [yellow1, yellow2]
                       , graphLabel      = Just "net"
                       }

memCfg =
  myGraphConfig { graphDataColors = [taffyBlue], graphLabel = Just "mem" }

cpuCfg =
  myGraphConfig { graphDataColors = [green1, green2], graphLabel = Just "cpu" }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

exampleTaffybarConfig :: TaffybarConfig
exampleTaffybarConfig =
  let myWorkspacesConfig = defaultWorkspacesConfig
        { minIcons        = 1
        , widgetGap       = 0
        , showWorkspaceFn = hideEmpty
        }
      workspaces = workspacesNew myWorkspacesConfig
      cpu        = pollingGraphNew cpuCfg 0.5 cpuCallback
      mem        = pollingGraphNew memCfg 1 memCallback
      net        = networkGraphNew netCfg Nothing
      clock      = textClockNewWith defaultClockConfig
      layout     = layoutNew defaultLayoutConfig
      windowsW   = windowsNew defaultWindowsConfig
      -- See https://github.com/taffybar/gtk-sni-tray#statusnotifierwatcher
      -- for a better way to set up the sni tray
      tray       = sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
      myConfig   = defaultSimpleTaffyConfig
        { startWidgets  = workspaces
                            : map (>>= buildContentsBox) [layout, windowsW]
        , endWidgets    = map
                            (>>= buildContentsBox)
                            [batteryIconNew, clock, tray, cpu, mem, net, mpris2New]
        , barPosition   = Top
        , barPadding    = 10
        , barHeight     = 50
        , widgetSpacing = 0
        }
  in  withBatteryRefresh $ withLogServer $ withToggleServer $ toTaffyConfig
        myConfig
