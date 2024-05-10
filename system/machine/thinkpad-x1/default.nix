{ pkgs, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../wm/hyprland.nix
    ];

  # Bootloader.
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  # Enable networking
  networking.hostName = "thinkpad-x1";

  # intel webcam workaround (a bit buggy, does not work with latest linux kernels)
  # needs: kernelPackages = pkgs.linuxPackages_6_6;
  hardware.ipu6 = {
    enable = false;
    platform = "ipu6ep";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
