# Configuration for the HDMI-1 display monitor
{ config, lib, pkgs, stdenv, nur, ... }:

let
  hdmiBar = pkgs.callPackage ../services/polybar/bar.nix { };

  statusBar = import ../services/polybar/default.nix {
    inherit config pkgs;
    mainBar = hdmiBar;
    openCalendar = "${pkgs.xfce.orage}/bin/orage";
  };

  hdmiExtra = ''
    ${pkgs.xorg.xrandr}/bin/xrandr --output HDMI-A-0 --mode 3840x2160 --rate 30.00
  '';

  wm = import ../programs/xmonad/default.nix { inherit config pkgs lib; };
in
{
  imports = [
    ../home.nix
    statusBar
    wm
  ];

  xsession.initExtra = wm.xsession.initExtra + hdmiExtra;

  programs = {
    firefoxie = {
      enable = true;
      addons = nur.repos.rycee.firefox-addons;
      hidpi = true;
    };

    megasync = {
      enable = true;
      hidpi = true;
    };

    spotify = {
      enable = true;
      hidpi = true;
    };

    termie = {
      enable = true;
      hidpi = true;
    };
  };
}
