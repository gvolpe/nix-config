{ pkgs, specialArgs, ... }:

{
  programs.signal = {
    enable = true;

    settings = {
      window = {
        maximized = false;
        autoHideMenuBar = false;
        fullscreen = false;
        width = 1015;
        height = 515;
        x = 6;
        y = 32;
      };
      spell-check = false;
      theme-setting = "dark";
    };
  };
}
