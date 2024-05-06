{ pkgs, ... }:

{
  # lightweight wayland terminal emulator
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        shell = "${pkgs.fish}/bin/fish";
        font = "JetBrainsMono Nerdfont:size=8:line-height:16px";
        pad = "12x12";
        dpi-aware = "yes";
        selection-target = "both";
      };
    };
  };
}
