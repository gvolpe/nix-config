{ pkgs, ... }:

{
  home.packages = [ pkgs.hyprpaper ];
  xdg.configFile."hypr/hyprpaper.conf".text = ''
    preload=${./hyprland.png}
    wallpaper=,${./hyprland.png}
    ipc=off
  '';
}
