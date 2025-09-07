let
  more = {
    services = {
      flameshot = {
        enable = false;
        settings = {
          General = {
            showStartupLaunchMessage = false;
          };
        };
      };

      gnome-keyring = {
        enable = false;
        components = [ "pkcs11" "secrets" "ssh" ];
      };
    };
  };
in
[
  # TODO: applies to hyprland but not to niri
  #../services/dunst
  ../services/gpg-agent
  ../services/udiskie
  more
]
