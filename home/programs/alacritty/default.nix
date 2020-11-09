{ pkgs, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {
      background_opacity = 0.8;
      colors = {
        primary = {
          background = "#040404";
          foreground = "#c5c8c6";
        };
      };
      font = {
        normal = {
          family = "JetBrainsMono Nerd Font";
          style = "Medium";
        };
        size = 10;
      };
      shell.program = "${pkgs.fish}/bin/fish";
      window = {
        decorations = "full";
      };
    };
  };
}
