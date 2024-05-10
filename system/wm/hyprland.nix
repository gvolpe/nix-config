{ pkgs, lib, ... }:

{
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
        tuigreet_session =
          let
            session = "${pkgs.hyprland}/bin/Hyprland";
            tuigreet = "${lib.exe pkgs.greetd.tuigreet}";
          in
          {
            command = "${tuigreet} --time --remember --cmd ${session}";
            user = "greeter";
          };
        default_session = tuigreet_session;
      };
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # Prerequisite for screensharing
      wireplumber.enable = true;
    };

    # Allows Hyprland to run without root privileges
    seatd.enable = true;
  };
}
