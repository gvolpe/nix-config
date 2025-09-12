{ lib, pkgs, ... }:

{
  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        font = "JetBrainsMono Nerd Font:size 10";
        icon-theme = "BeautyLine";
        layer = "overlay";
        terminal = lib.exe pkgs.kitty;
      };
      colors = {
        background = "0e0730aa";
        prompt = "c8b2d7fa";
        text =   "ebf9fcaa";
      };
    };
  };
}
