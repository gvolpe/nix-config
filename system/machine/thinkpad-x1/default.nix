{ pkgs, lib, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot = {
    # ipu6 (webcam support) does not yet work with newer kernel versions
    kernelPackages = pkgs.linuxPackages_6_6;

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  # Enable networking
  networking.hostName = "thinkpad-x1";

  programs = {
    dconf.enable = true;
    hyprland.enable = true;
  };

  # tty service config
  systemd.services.greetd.serviceConfig = {
    Type = "idle";
    StandardInput = "tty";
    StandardOutput = "tty";
    StandardError = "journal";
    TTYReset = true;
    TTYVHangup = true;
    TTYVTDisallocate = true;
  };

  # intel webcam workaround
  hardware.ipu6 = {
    enable = true;
    platform = "ipu6ep";
  };

  hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
      };
    };
  };

  # nice but buggy: https://github.com/rharish101/ReGreet/issues/45
  programs.regreet = {
    enable = false;
    settings = (lib.importTOML ./regreet.toml) // {
      background = {
        path = ../imgs/hyprland.png;
      };
    };
  };

  services = {
    # Bluetooth manager
    blueman.enable = true;

    # GTK theme config
    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };

    # User's credentials manager
    gnome.gnome-keyring.enable = true;

    # Init session with hyprland
    greetd = {
      enable = true;
      settings = rec {
        regreet_session = {
          command = "${lib.exe pkgs.cage} -s -- ${lib.exe pkgs.greetd.regreet}";
          user = "greeter";
        };
        tuigreet_session = {
          command = "${lib.exe pkgs.greetd.tuigreet} --time --remember --cmd ${pkgs.hyprland}/bin/Hyprland";
          user = "greeter";
        };
        default_session = tuigreet_session;
      };
    };

    # Prerequisite for screensharing
    pipewire.wireplumber.enable = true;

    # Allows Hyprland to run without root privileges
    seatd.enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
