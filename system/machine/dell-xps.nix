{ config, pkgs, ... }:

{
  # Use the GRUB 2 boot loader.
  boot = {
    kernelPackages = pkgs.linuxPackages_5_8;
    loader = {
      grub = {
        enable  = true;
        device = "/dev/nvme0n1"; # or "nodev" for efi only
        version = 2;
      };
    };
  };

  networking.interfaces.wlp2s0.useDHCP = true;

  fileSystems."/data" = { 
    device = "/dev/nvme0n1p3";
    fsType = "ext4";
  };
}
