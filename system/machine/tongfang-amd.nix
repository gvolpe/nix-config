{ config, pkgs, ... }:

{
  boot = {
    kernelPackages = pkgs.linuxPackages_5_11;

    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
   
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

  services.xserver = {
    videoDrivers = [ "amdgpu" ];

    xrandrHeads = [
      { output = "HDMI-A-0";
        primary = true;
        monitorConfig = ''
          Modeline "3840x2160_30.00"  338.75  3840 4080 4488 5136  2160 2163 2168 2200 -hsync +vsync
          Option "PreferredMode" "3840x2160_30.00"
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

  nixpkgs.config.packageOverrides = p: rec {
    mesa = p.mesa.overrideAttrs (attrs: {
      version = "21.1.0-devel-2021-02-13";

      src = p.fetchFromGitLab {
        domain = "gitlab.freedesktop.org";
        owner  = "mesa";
        repo   = "mesa";
        rev    = "628ce5472ca45f6e92856a0314905fc578300f4f"; 
        sha256 = "0njmarq3yzqznpqg12r6iwbm3fldi3jkqsmlwp89mg2vchfl2i9g";
      };

      mesonFlags = attrs.mesonFlags ++ [ 
        "-Dosmesa=true" 
        "-Dmicrosoft-clc=disabled" 
      ];
    });
  };

}
