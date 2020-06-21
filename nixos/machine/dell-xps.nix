{ config, pkgs, ... }:

# Configuration specic to the machine #
{
  boot.loader.grub.device = "/dev/nvme0n1"; # or "nodev" for efi only
  networking.interfaces.wlp2s0.useDHCP = true;
}
