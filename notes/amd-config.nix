{ config, lib, pkgs, ... }:

{
  boot = {
    initrd.kernelModules = [ "amdgpu" ];
    kernelPackages = pkgs.linuxPackages_5_8;
  };

  networking = {
    networkmanager = {
      enable   = true;
      packages = [ pkgs.networkmanager_openvpn ];
    };
    useDHCP = false;
    wireless.enable = false;
  };

  environment.systemPackages = with pkgs; [
    chromium wget gparted
  ];

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services = {
    dbus.packages = [ pkgs.gnome3.dconf ];
    udev.packages = [ pkgs.gnome3.gnome-settings-daemon ];
    openssh.enable = true;

    xserver = {
      enable = true;
      layout = "us";
      libinput.enable = true;
      videoDrivers = [ "amdgpu" ];

      displayManager.gdm.enable    = true;
      displayManager.gdm.wayland   = true;
      desktopManager.gnome3.enable = true;
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.gvolpe = {
    isNormalUser = true;
    extraGroups  = [ "docker" "networkmanager" "wheel" ]; # wheel for ‘sudo’.
    shell        = pkgs.fish;
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}
