{ config, pkgs, ... }:

{
  home.packages = [ pkgs.swaybg pkgs.waypaper ];
  xdg.configFile."waypaper/config.ini".source =
    config.dotfiles.make ./config.ini;
}
