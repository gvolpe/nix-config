{ config, lib, pkgs, ... }:

{
  services = {
    gnome3.gnome-keyring.enable = true;
    upower.enable = true;

    dbus = {
      enable = true;
      socketActivated = true;
      packages = [ pkgs.gnome3.dconf ];
    };

    xserver = {
      enable = true;
      startDbusSession = true;

      extraLayouts.us-custom = {
        description = "US layout with custom hyper keys";
        languages   = [ "eng" ];
        symbolsFile = ./us-custom.xkb;
      };

      layout = "us-custom";

      libinput = {
        enable = true;
        disableWhileTyping = true;
      };
      
      serverLayoutSection = ''
        Option "StandbyTime" "0"
        Option "SuspendTime" "0"
        Option "OffTime"     "0"
      '';

      displayManager = {
        defaultSession = "none+xmonad";
      };

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };

      xkbOptions = "shift:both_capslock, caps:ctrl_modifier";
    };
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  systemd.services.upower.enable = true;
}
