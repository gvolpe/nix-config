let
  more = {
    services = {
      flameshot = {
        enable = true;
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
  ../services/dunst
  ../services/gpg-agent
  ../services/udiskie
  more
]
