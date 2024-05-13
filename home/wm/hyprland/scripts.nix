{ lib, writeShellScriptBin }:

let
  monitorsConf = "$XDG_CONFIG_HOME/hypr/monitors.conf";

  monitorAdded = writeShellScriptBin "monitor-added" ''
    hyprctl dispatch moveworkspacetomonitor 1 HDMI-A-1
    hyprctl dispatch moveworkspacetomonitor 2 HDMI-A-1
    hyprctl dispatch moveworkspacetomonitor 3 HDMI-A-1
    hyprctl dispatch moveworkspacetomonitor 4 HDMI-A-1
    hyprctl dispatch moveworkspacetomonitor 5 HDMI-A-1
    ${lib.exe monitorConnected}
  '';

  monitorConnected = writeShellScriptBin "monitor-connected" ''
    hyprctl dispatch dpms off eDP-1
    echo "monitor=HDMI-A-1,2560x1440@59.95,0x0,1" > ${monitorsConf}
    echo "monitor=eDP-1,disable" >> ${monitorsConf}
  '';

  monitorRemoved = writeShellScriptBin "monitor-removed" ''
    hyprctl dispatch dpms on eDP-1
    echo "monitor=eDP-1,2880x1800@90,0x0,2" > ${monitorsConf}
  '';
in
{
  inherit monitorAdded monitorRemoved;

  wsNix = writeShellScriptBin "ws-nix" ''
    footclient -D ~/workspace/nix-config -E fish -C 'neofetch' &
    footclient -D ~/workspace/nix-config -E fish -C 'nitch' &
  '';

  monitorInit = writeShellScriptBin "monitor-init" ''
    monitors=$(hyprctl monitors)
    if [[ $monitors == *"HDMI-A-1"* ]]; then
      ${lib.exe monitorConnected}
    else
      ${lib.exe monitorRemoved}
    fi
  '';
}
