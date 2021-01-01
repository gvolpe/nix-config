{ config, pkgs, ...}:

let
  xrandr = "${pkgs.xorg.xrandr}/bin/xrandr";
  bls    = "${pkgs.betterlockscreen}/bin/betterlockscreen";
in
  pkgs.writeShellScriptBin "hms" ''
    monitors=$(${xrandr} --listmonitors)
    lockRes=$(${bls} version)

    if [[ $monitors == *"HDMI"* ]]; then
      echo "Switching to default HM config for HDMI display"
      home-manager -f ${config.xdg.configHome}/nixpkgs/display/hdmi.nix switch
      if [[ $lockRes != *"3840x2160"* ]]; then
        echo "Setting up lockscreen resolution for HDMI display"
        ${bls} -u ${config.home.homeDirectory}/Pictures/nixos.png -r 3840x2160
      fi
    elif [[ $monitors == *"eDP"* ]]; then
      echo "Switching to HM config for eDP or eDP-1 (laptop display)"
      home-manager -f ${config.xdg.configHome}/nixpkgs/display/edp.nix switch
      if [[ $lockRes != *"1920x1080"* ]]; then
        echo "Setting up lockscreen resolution for laptop display"
        ${bls} -u ${config.home.homeDirectory}/Pictures/nixos.png -r 1920x1080
      fi
    else
      echo "Could not detect monitor: $monitors"
      exit 1
    fi
  ''
