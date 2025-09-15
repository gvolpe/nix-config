{ config, ... }:

{
  programs.kitty = {
    enable = true;
    shellIntegration.enableFishIntegration = true;
    themeFile = "OneDark";
    font = {
      name = "JetBrainsMono Nerdfont";
      size = config.programs.kitty.fontsize;
    };
    extraConfig = ''
      copy_on_select yes
      background_opacity 0.80
    '';
  };
}
