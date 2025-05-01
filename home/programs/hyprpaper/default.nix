{ pkgs, ... }:

{
  home.packages = [ pkgs.hyprpaper ];
  xdg.configFile."hypr/hyprpaper.conf".text = ''
    preload=${./nixos.png}
    wallpaper=,${./nixos.png}
    ipc=off
  '';
}
