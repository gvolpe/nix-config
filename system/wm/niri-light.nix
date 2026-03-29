{ pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    wl-clipboard
    wayland-utils
    libsecret
    cage
    gamescope
    xwayland-satellite #-unstable
  ];

  programs = {
    dconf.enable = true;
    niri = {
      enable = true;
      package = pkgs.niri-unstable;
    };
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

  services = {
    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };

    # User's credentials manager
    gnome.gnome-keyring.enable = true;

    # Init session with niri
    greetd = {
      enable = true;
      settings = rec {
        tuigreet_session =
          let
            session = "${pkgs.niri-unstable}/bin/niri-session";
            tuigreet = "${lib.exe pkgs.tuigreet}";
          in
          {
            command = "${tuigreet} --time --remember --cmd ${session}";
            user = "greeter";
          };
        default_session = tuigreet_session;
      };
    };

    # Allows niri to run without root privileges
    seatd.enable = true;
  };
}
