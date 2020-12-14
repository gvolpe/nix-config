# Configuration for the eDP display of the Tongfang laptop (default: HDMI-1)
{ config, lib, pkgs, stdenv, ... }:

let
  base = pkgs.callPackage ../home.nix { inherit config lib pkgs stdenv; };

  laptopBar = pkgs.callPackage ../services/polybar/bar.nix {
    font0 = 10;
    font1 = 12;
    font2 = 24;
    font3 = 18;
    font4 = 5;
  };

  myspotify = import ../programs/spotify/default.nix { opts = ""; inherit pkgs; };

  statusBar = import ../services/polybar/default.nix {
    inherit config pkgs;
    mainBar = laptopBar;
    openCalendar = "";
  };

  terminal = import ../programs/alacritty/default.nix { fontSize = 8; inherit pkgs; };
in
{
  imports = [
    ../home.nix
    statusBar
    terminal
  ];

  home.packages = base.home.packages ++ [ myspotify ];
}
