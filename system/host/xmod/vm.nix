{ pkgs, lib, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ../../wm/niri-light.nix
    ];

  networking =
    {
      hostName = "niri-vm";
      wireless.enable = lib.mkForce false;
    };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  system.stateVersion = "23.05";
}
