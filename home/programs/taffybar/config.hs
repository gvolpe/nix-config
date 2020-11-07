{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import           Control.Exception                           ( SomeException
                                                             , try
                                                             )
import           Data.Functor                                ( (<&>)
                                                             , void
                                                             )
import           Data.Text                                   ( Text )
import qualified Data.Text                                   as T
import           GI.Gtk                                      ( Widget
                                                             , toWidget
                                                             , widgetShowAll
                                                             )
import           System.Environment.XDG.BaseDir              ( getUserConfigFile )
import           System.Taffybar
import           System.Taffybar.Context                     ( TaffybarConfig
                                                             , TaffyIO
                                                             )
import           System.Taffybar.Hooks
import           System.Taffybar.Information.CPU2
import           System.Taffybar.Information.Memory
import           System.Taffybar.SimpleConfig
import           System.Taffybar.Util                        ( runCommandFromPath )
import           System.Taffybar.Widget
import           System.Taffybar.Widget.Decorators           ( buildPadBox )
import           System.Taffybar.Widget.Generic.PollingGraph
import           System.Taffybar.Widget.Generic.PollingLabel
import           System.Taffybar.Widget.Util

main :: IO ()
main = do
  css <- getUserConfigFile "taffybar" "taffybar.css"
  dyreTaffybar
    . appendHook notifySystemD
    . appendHook (void $ getHost False)
    $ myConfig css

transparent, yellow1, yellow2, green1, green2, taffyBlue
  :: (Double, Double, Double, Double)
transparent = (0.0, 0.0, 0.0, 0.0)
yellow1     = (0.9453125, 0.63671875, 0.2109375, 1.0)
yellow2     = (0.9921875, 0.796875, 0.32421875, 1.0)
green1      = (0, 1, 0, 1)
green2      = (1, 0, 1, 0.5)
taffyBlue   = (0.129, 0.588, 0.953, 1)

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
memCallback = return . memoryUsedRatio <$> parseMeminfo

cpuCallback :: IO [Double]
cpuCallback = getCPULoad "cpu"

notifySystemD = void $ runCommandFromPath ["systemd-notify", "--ready"]

myConfig :: FilePath -> TaffybarConfig
myConfig myCss =
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
      clock      = textClockNew Nothing "📅 %b %_d 🕐 %_H:%M" 1.0
      layout     = layoutNew defaultLayoutConfig
      windowsW   = windowsNew defaultWindowsConfig
      tray       = sniTrayNew
      taffyCfg   = defaultSimpleTaffyConfig
        { startWidgets  = workspaces : map (>>= buildContentsBox) [layout, windowsW]
        , centerWidgets = map (>>= buildContentsBox) [mpris2New]
        , endWidgets    = map (>>= buildContentsBox)
                            [bat, batteryIconNew, tray, volumeNew, clock, cpu, mem, net, netmon]
        , barPosition   = Top
        , barPadding    = 10
        , barHeight     = 50
        , widgetSpacing = 1
        , cssPath       = return myCss
        }
  in  withBatteryRefresh . withLogServer . withToggleServer . toTaffyConfig $ taffyCfg

------------- Volume status ---------------

volumeNew :: TaffyIO Widget
volumeNew = do
  label <- pollingLabelNew 1 tryGetVolume
  widgetShowAll label
  toWidget label

data AudioStatus = AudioOn | AudioOff deriving (Eq, Show)

-- could use the alsa-mixer package but that requires a fork of taffybar to add the package; not today.
parseVolume :: IO (Int, AudioStatus)
parseVolume = do
  runCommandFromPath ["amixer", "get", "Master"] >>= \case
    Left _  -> return (0, AudioOff)
    Right s ->
      let raw    = takeWhile (/= '\n') $ dropWhile (/= '[') s
          volume = read $ takeWhile (/= '%') $ drop 1 (takeWhile (/= ' ') raw)
          status = case takeWhile (/= ']') $ drop 2 (dropWhile (/= ' ') raw) of
                     "on" -> AudioOn
                     _    -> AudioOff
      in return (volume, status)

volIcon :: Int -> Text
volIcon x | x == 0 = "🔇"
          | x < 30 = "🔈"
          | x < 60 = "🔉"
          | True   = "🔊"

tryGetVolume :: IO Text
tryGetVolume = (try getVolume :: IO (Either SomeException Text)) <&> \case
  Left _  -> "Error"
  Right x -> x

getVolume :: IO Text
getVolume = parseVolume <&> \case
  (_, AudioOff) -> volIcon 0 <> " Mute"
  (v, AudioOn)  -> volIcon v <> T.pack (" " <> show v <> "%")
