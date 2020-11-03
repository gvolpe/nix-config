{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.terminator}/bin/terminator";
    theme = ./theme.rafi;
  };
}
