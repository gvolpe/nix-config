{ pkgs, specialArgs, ... }:

let
  inherit (specialArgs) hidpi;
  fontSize = if hidpi then "12" else "8";
in
{
  # lightweight wayland terminal emulator
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        shell = "${pkgs.fish}/bin/fish";
        font = "JetBrainsMono Nerdfont:size=${fontSize}:line-height:16px";
        pad = "12x12";
        dpi-aware = "yes";
        selection-target = "both";
      };
    };
  };
}
