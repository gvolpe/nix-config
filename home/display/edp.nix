# Configuration for the eDP display of the Tongfang laptop (default: HDMI-1)
{ config, lib, pkgs, stdenv, nur, ... }:

let
  hdmiOn = false;

  base = pkgs.callPackage ../home.nix { inherit config lib pkgs stdenv; };

  browser = pkgs.callPackage ../programs/browsers/firefox.nix {
    inherit config pkgs nur hdmiOn;
  };

  laptopBar = pkgs.callPackage ../services/polybar/bar.nix {
    font0 = 10;
    font1 = 12;
    font2 = 24;
    font3 = 18;
    font4 = 5;
    font5 = 10;
  };

  megasync = import ../programs/megasync/default.nix {
    inherit pkgs hdmiOn;
  };

  spotify = import ../programs/spotify/default.nix {
    inherit pkgs hdmiOn;
  };

  statusBar = import ../services/polybar/default.nix {
    inherit config pkgs;
    mainBar = laptopBar;
    openCalendar = "${pkgs.xfce.orage}/bin/orage";
    #openCalendar = "";
  };

  terminal = import ../programs/alacritty/default.nix { fontSize = 8; inherit pkgs; };

  wm = import ../programs/xmonad/default.nix {
    inherit config pkgs lib hdmiOn megasync;
  };
in
{
  imports = [
    ../home.nix
    statusBar
    terminal
    wm
  ];

  programs.firefox = browser.programs.firefox;

  home.packages = base.home.packages ++ [ megasync spotify ];
}
