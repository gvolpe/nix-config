{ pkgs, ... }:

let
  netdev = "enp132s0";
in
{
  imports = [
    ./hardware-configuration.nix
    ../../wm/niri.nix
  ];

  # bootloader.
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  environment.systemPackages = [ pkgs.wayvnc ];

  hardware = {
    # graphics card (AMD Radeon RX 7800 XT)
    graphics = {
      enable = true;
      enable32Bit = true;
    };

    # gamepad's udev rules from https://github.com/ValveSoftware/steam-devices
    steam-hardware.enable = true;
  };

  # gaming
  programs.steam = {
    enable = true;
    extraCompatPackages = [ pkgs.proton-ge-bin ];
  };

  programs.gamemode.enable = true;

  # tailscale
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "server";
    extraSetFlags = [ "--advertise-exit-node" ];
  };
  # https://tailscale.com/kb/1320/performance-best-practices#linux-optimizations-for-subnet-routers-and-exit-nodes
  systemd.services = {
    tailscale-udp-gro-forwarding = {
      description = "tailscale udp exit node optimization";
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.ethtool}/bin/ethtool -K ${netdev} rx-udp-gro-forwarding on rx-gro-list off";
      };
      requires = [ "tailscaled.service" ];
      after = [ "tailscaled.service" ];
    };
  };

  # hostname
  networking.hostName = "aorus";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11"; # Did you read the comment?
}
