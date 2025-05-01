{ config, pkgs, ... }:

{
  # lightweight wayland terminal emulator
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        shell = "${pkgs.fish}/bin/fish";
        font = "JetBrainsMono Nerdfont:size=${config.programs.foot.fontsize}";
        pad = "12x12";
        dpi-aware = "yes";
        selection-target = "both";
      };
      colors = {
        alpha = 0.75;
      };
    };
  };
}
