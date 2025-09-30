{ config, pkgs, ... }:

let
  filePath = "${config.dotfiles.path}/programs/waypaper/config.ini";

  configSrc =
    if !config.dotfiles.mutable then ./config.ini
    else config.lib.file.mkOutOfStoreSymlink filePath;
in
{
  home.packages = [ pkgs.swaybg pkgs.waypaper ];
  xdg.configFile."waypaper/config.ini".source = configSrc;
}
