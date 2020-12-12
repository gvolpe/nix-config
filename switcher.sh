#! /usr/bin/env sh

# Shows the output of every command
set +x

monitors=$(xrandr --listmonitors)

if [[ $monitors == *"HDMI-1"* ]]; then
  echo "Switching to default HM config for HDMI-1"
  home-manager switch
elif [[ $monitors == *"eDP"* ]]; then
  echo "Switching to HM config for eDP or eDP-1 (laptop display)"
  home-manager -f $XDG_CONFIG_HOME/nixpkgs/display/edp.nix switch
else
  exit 1
fi
