{ pkgs, lib, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;

  services.globalprotectvpn.enable = true;

  environment.systemPackages = with pkgs; [
    gnome-console
    globalprotect-openconnect
  ];

  services.gnome = {
    core-apps.enable = false;
    core-developer-tools.enable = false;
    games.enable = false;
  };

  networking =
    {
      hostName = "gnome-vm";
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
      diskSize = 51200; # 50 gbs
      graphics = true;

      sharedDirectories = {
        sxm_workspace = {
          source = "/home/gvolpe/workspace/sxm";
          target = "/mnt/workspace";
        };
      };

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

  system.activationScripts.diff = lib.mkForce "";

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
