{
  services.xserver = {
    enable = true;
    layout = "us";
    libinput.enable = true;
      
    displayManager.defaultSession = "none+xmonad";

    windowManager = {
      xmonad.enable = true;
      xmonad.enableContribAndExtras = true;
    };
  };
}
