{ pkgs, ... }:

{
  programs.gnupg.agent.pinentryPackage = pkgs.pinentry-gnome3;

  # Enable the X11 windowing system.
  services = {
    # Gnome config
    dbus.packages = [ pkgs.dconf ];
    udev.packages = [ pkgs.gnome.gnome-settings-daemon ];

    # GUI interface
    xserver = {
      enable = true;
      layout = "us";

      # Enable touchpad support.
      libinput.enable = true;

      # Enable the Gnome3 desktop manager
      displayManager.gdm.enable = true;
      displayManager.gdm.wayland = false; # screen-sharing is broken
      desktopManager.gnome.enable = true;
    };
  };
}
