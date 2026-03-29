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

  users.users.gvolpe = {
    initialPassword = lib.mkForce "test";
    extraGroups = lib.mkForce [ "libvirtd" "networkmanager" "wheel" ];
  };

  # wayland compositors require 3d acceleration
  virtualisation.vmVariant = {
    hardware.graphics = {
      enable = true;
      extraPackages = [ pkgs.mesa ];
    };

    services = {
      qemuGuest.enable = true;
      spice-autorandr.enable = true;
      spice-vdagentd.enable = true; # enable copy and paste between host and guest
    };

    virtualisation = {
      graphics = true;
      qemu.options = [
        "-device virtio-vga-gl"
        "-display gtk,gl=on"
        "-device virtio-keyboard-pci"
        "-usb"
        "-device usb-tablet"
      ];
    };
  };

  # overrides for system/configuration.nix
  virtualisation.docker.enable = lib.mkForce false;
  hardware.sane.enable = lib.mkForce false;

  services = {
    avahi.enable = lib.mkForce false;
    gvfs.enable = lib.mkForce false;
    pcscd.enable = lib.mkForce false;
    printing.enable = lib.mkForce false;
    udev.packages = lib.mkForce [ ];
  };

  boot = {
    initrd.kernelModules = [ "virtio_gpu" ];
    kernelModules = [ "virtio_gpu" ];
    kernelPackages = pkgs.linuxPackages_latest;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  system.stateVersion = "23.05";
}
