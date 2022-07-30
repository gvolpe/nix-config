# Configuration for the eDP display of the Tongfang laptop (default: HDMI-1)
{ config, lib, pkgs, stdenv, nur, ... }:

let
  laptopBar = pkgs.callPackage ../services/polybar/bar.nix {
    font0 = 10;
    font1 = 12;
    font2 = 24;
    font3 = 18;
    font4 = 5;
    font5 = 10;
  };

  statusBar = import ../services/polybar/default.nix {
    inherit config pkgs;
    mainBar = laptopBar;
    openCalendar = "${pkgs.xfce.orage}/bin/orage";
  };

  terminal = import ../programs/alacritty/default.nix { fontSize = 8; inherit pkgs; };

  wm = import ../programs/xmonad/default.nix { inherit config pkgs lib; };
in
{
  imports = [
    ../home.nix
    statusBar
    terminal
    wm
  ];

  programs =
    {
      firefoxie = {
        enable = true;
        addons = nur.repos.rycee.firefox-addons;
      };
      megasync.enable = true;
      spotify.enable = true;
    };
}
