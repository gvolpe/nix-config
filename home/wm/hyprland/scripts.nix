{ lib, writeShellScriptBin }:

let
  intMonitor = "eDP-1";
  extMonitor = "DP-3";

  monitorsConf = "$XDG_CONFIG_HOME/hypr/monitors.conf";

  monitorAdded = writeShellScriptBin "monitor-added" ''
    hyprctl --batch "\
      dispatch moveworkspacetomonitor 1 ${extMonitor};\
      dispatch moveworkspacetomonitor 2 ${extMonitor};\
      dispatch moveworkspacetomonitor 3 ${extMonitor};\
      dispatch moveworkspacetomonitor 4 ${extMonitor};\
      dispatch moveworkspacetomonitor 5 ${extMonitor};\
      dispatch moveworkspacetomonitor 6 ${extMonitor}"
    ${lib.exe monitorConnected}
  '';

  monitorConnected = writeShellScriptBin "monitor-connected" ''
    hyprctl dispatch dpms off ${intMonitor}
    echo "monitor=${extMonitor},2560x1440@59.95,0x0,1" > ${monitorsConf}
    echo "monitor=${intMonitor},disable" >> ${monitorsConf}
  '';

  monitorRemoved = writeShellScriptBin "monitor-removed" ''
    hyprctl dispatch dpms on ${intMonitor}
    echo "monitor=${intMonitor},2880x1800@90,0x0,2" > ${monitorsConf}
  '';
in
{
  inherit extMonitor monitorAdded monitorRemoved;

  wsNix = writeShellScriptBin "ws-nix" ''
    footclient -D ~/workspace/nix-config -E fish -C 'hyfetch' &
    footclient -D ~/workspace/nix-config -E fish -C 'nitch' &
  '';

  monitorInit = writeShellScriptBin "monitor-init" ''
    monitors=$(hyprctl monitors)
    if [[ $monitors == *"${extMonitor}"* ]]; then
      ${lib.exe monitorConnected}
    else
      ${lib.exe monitorRemoved}
    fi
  '';
}
