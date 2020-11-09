{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.alacritty}/bin/alacritty";
    theme = ./theme.rafi;
  };
}
