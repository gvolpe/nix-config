{ pkgs, ... }:

{
  imports = [
    # Hardware scan
    ./hardware-configuration.nix
    ../../wm/gnome.nix
  ];

  # Use the GRUB 2 boot loader.
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader = {
      grub = {
        enable = true;
        device = "/dev/nvme0n1"; # or "nodev" for efi only
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
      {
        output = "HDMI-1";
        primary = true;
        monitorConfig = ''
          Option "PreferredMode" "3840x2160"
          Option "Position" "0 0"
        '';
      }
      {
        output = "eDP-1";
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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.03"; # Did you read the comment?
}
