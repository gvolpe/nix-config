# Configuration for the HDMI-1 display monitor
{ config, lib, pkgs, stdenv, nur, ... }:

let
  hidpi = true;
  hdmiExtra = ''
    ${pkgs.xorg.xrandr}/bin/xrandr --output HDMI-A-0 --mode 3840x2160 --rate 30.00
  '';

  wm = import ../programs/xmonad/default.nix { inherit config pkgs lib; };
in
{
  imports = [
    ../home.nix
    wm
  ];

  xsession.initExtra = wm.xsession.initExtra + hdmiExtra;

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
  };
}
