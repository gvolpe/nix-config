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
      layout = "us";
      startDbusSession = true;

      libinput = {
        enable = true;
        disableWhileTyping = true;
      };
      
      serverLayoutSection = ''
        Option "StandbyTime" "0"
        Option "SuspendTime" "0"
        Option "OffTime"     "0"
      '';

      xkbOptions = "shift:both_capslock, caps:ctrl_modifier";

      displayManager.defaultSession = "none+xmonad";

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
  };

  systemd.services.upower.enable = true;
}
