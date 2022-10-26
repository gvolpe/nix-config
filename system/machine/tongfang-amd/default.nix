{ config, pkgs, ... }:

{
  imports = [
    # Hardware scan
    ./hardware-configuration.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    # Use the systemd-boot EFI boot loader.
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };

    initrd.kernelModules = [ "amdgpu" ];
  };

  networking = {
    hostName = "tongfang-amd";
    interfaces = {
      eno1.useDHCP = true;
      wlp1s0.useDHCP = true;
    };
  };

  fileSystems."/data" = {
    device = "/dev/nvme0n1p3";
    fsType = "ext4";
  };

  services.sysprof.enable = true; 

  services.xserver = {
    videoDrivers = [ "amdgpu" ];

    xrandrHeads = [
      {
        output = "HDMI-A-0";
        primary = true;
        monitorConfig = ''
          Modeline "3840x2160_30.00"  338.75  3840 4080 4488 5136  2160 2163 2168 2200 -hsync +vsync
          Option "PreferredMode" "3840x2160_30.00"
          Option "Position" "0 0"
        '';
      }
      {
        output = "eDP";
        primary = false;
        monitorConfig = ''
          Option "PreferredMode" "1920x1080"
          Option "Position" "0 0"
        '';
      }
    ];

    resolutions = [
      { x = 2048; y = 1152; }
      { x = 1920; y = 1080; }
      { x = 2560; y = 1440; }
      { x = 3072; y = 1728; }
      { x = 3840; y = 2160; }
    ];
  };

}
