{ pkgs, specialArgs, ... }:

let
  fontSize = if specialArgs.hidpi then 10 else 8;
in
{
  programs.alacritty = {
    enable = true;
    settings = {
      bell = {
        animation = "EaseOutExpo";
        duration = 5;
        color = "#ffffff";
      };
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
        size = fontSize;
      };
      key_bindings = [
        { key = 53; mods = "Shift"; mode = "Vi"; action = "SearchBackward"; }
      ];
      selection.save_to_clipboard = true;
      shell.program = "${pkgs.fish}/bin/fish";
      window = {
        decorations = "full";
        opacity = 0.8;
        padding = {
          x = 5;
          y = 5;
        };
      };
    };
  };
}
