{
  services = {
    gnome3.gnome-keyring.enable = true;

    xserver = {
      enable = true;
      layout = "us";
      libinput.enable = true;
      
      displayManager.defaultSession = "none+xmonad";

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
  };
}
