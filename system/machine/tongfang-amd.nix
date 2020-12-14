{ config, pkgs, ... }:

{
  boot = {
    kernelPackages = pkgs.linuxPackages_5_8;

    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
   
    initrd.kernelModules = [ "amdgpu" ];
  };

  networking = {
    hostName = "tongfang-amd-ryzen-7";
    interfaces = {
      eno1.useDHCP = true;
      wlp1s0.useDHCP = true;
    };
  };  

  fileSystems."/data" = { 
    device = "/dev/nvme0n1p3";
    fsType = "ext4";
  };

  services.xserver = {
    videoDrivers = [ "amdgpu" ];

    xrandrHeads = [
      { output = "HDMI-A-0";
        primary = true;
        monitorConfig = ''
          Option "PreferredMode" "3840x2160"
          Option "TargetRefresh" "30"
          Option "Position" "0 0"
        '';
      }
      { output = "eDP";
        monitorConfig = ''
          Option "PreferredMode" "1920x1980"
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
