{ lib, ... }:
{
  # status bar for hyprland/wayland
  programs.waybar = {
    enable = true;
    settings = [ (lib.importJSON ./config.json) ];
    style = builtins.readFile ./style.css;
    systemd.enable = true;
  };
}
