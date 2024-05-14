{ pkgs, specialArgs, ... }:

let
  inherit (specialArgs) hidpi;
  fontSize = if hidpi then "14" else "10";
in
{
  # lightweight wayland terminal emulator
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        shell = "${pkgs.fish}/bin/fish";
        font = "JetBrainsMono Nerdfont:size=${fontSize}";
        pad = "12x12";
        dpi-aware = "yes";
        selection-target = "both";
      };
      colors = {
        alpha = 0.5;
      };
    };
  };
}
