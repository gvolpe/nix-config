# Configuration for the HDMI-1 display monitor
{ config, lib, pkgs, stdenv, nur, ... }:

let
  hidpi = true;
in
{
  imports = [ ../home.nix ];

  programs = {
    firefoxie = {
      enable = true;
      addons = nur.repos.rycee.firefox-addons;
      inherit hidpi;
    };

    megasync = {
      enable = true;
      inherit hidpi;
    };

    polybar = {
      enable = true;
      inherit hidpi;
    };

    spotify = {
      enable = true;
      inherit hidpi;
    };

    termie = {
      enable = true;
      inherit hidpi;
    };

    xmonad = {
      enable = true;
      inherit hidpi;
    };
  };
}
