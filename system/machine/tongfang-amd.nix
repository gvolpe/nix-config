{ config, pkgs, ... }:

{
  boot = {
    kernelPackages = pkgs.linuxPackages_5_8;

    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
   
    initrd.kernelModules = [ "amdgpu" ];
  };

  services.xserver.videoDrivers = [ "amdgpu" ];

  networking.interfaces.eno1.useDHCP = true;
  networking.interfaces.wlp1s0.useDHCP = true;

  fileSystems."/data" = { 
    device = "/dev/nvme0n1p3";
    fsType = "ext4";
  };
}
