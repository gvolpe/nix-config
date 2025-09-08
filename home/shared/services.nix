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
  ../services/gpg-agent
  ../services/udiskie
  more
]
