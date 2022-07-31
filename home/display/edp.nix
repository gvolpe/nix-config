# Configuration for the eDP display of the Tongfang laptop (default: HDMI-1)
{ config, lib, pkgs, stdenv, nur, ... }:

let
  wm = import ../programs/xmonad/default.nix { inherit config pkgs lib; };
in
{
  imports = [
    ../home.nix
    wm
  ];

  programs =
    {
      firefoxie = {
        enable = true;
        addons = nur.repos.rycee.firefox-addons;
      };
      megasync.enable = true;
      polybar.enable = true;
      spotify.enable = true;
      termie.enable = true;
    };
}
