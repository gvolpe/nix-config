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

  networking = {
    hostName = "dell-xps-15-9560";
    interfaces.wlp2s0.useDHCP = true;
  };

  fileSystems."/data" = {
    device = "/dev/nvme0n1p3";
    fsType = "ext4";
  };

  services.xserver = {
    xrandrHeads = [
      { output = "HDMI-1";
        primary = true;
        monitorConfig = ''
          Option "PreferredMode" "3840x2160"
          Option "Position" "0 0"
        '';
      }
      { output = "eDP-1";
        monitorConfig = ''
          Option "PreferredMode" "3840x2160"
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
