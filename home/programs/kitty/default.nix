{ config, lib, ... }:

let
  filePath = "${config.dotfiles.path}/programs/kitty/kitty.conf";
in
{
  programs.kitty = {
    enable = true;
    shellIntegration.enableFishIntegration = true;
    themeFile = "OneDark";
    font = {
      name = "JetBrainsMono Nerdfont";
      size = config.programs.kitty.fontsize;
    };
    extraConfig = lib.mkIf config.dotfiles.mutable ''
      # Mutable config included below
      include ${filePath}
    '';
  };
}
