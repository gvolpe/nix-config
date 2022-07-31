# Configuration for the eDP display of the Tongfang laptop (default: HDMI-1)
{ config, lib, pkgs, stdenv, nur, ... }:

{
  imports = [ ../home.nix ];

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
      xmonad.enable = true;
    };
}
