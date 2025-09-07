{ ... }:

{
  programs.kitty = {
    enable = true;
    #font = {
    #name = "JetBrainsMono Nerd Font";
    #size = config.programs.alacritty.fontsize;
    #};
    shellIntegration.enableFishIntegration = true;
    themeFile = "OneDark";
    extraConfig = ''
      copy_on_select yes
    '';
  };
}
