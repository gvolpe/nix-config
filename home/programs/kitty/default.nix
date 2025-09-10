{ ... }:

{
  programs.kitty = {
    enable = true;
    shellIntegration.enableFishIntegration = true;
    themeFile = "OneDark";
    extraConfig = ''
      copy_on_select yes
      background_opacity 0.80
    '';
  };
}
