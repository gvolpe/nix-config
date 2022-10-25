let
  scripts = { config, lib, pkgs, ... }:
    let
      gen-ssh-key = pkgs.callPackage ./gen-ssh-key.nix { inherit pkgs; };
      hms = pkgs.callPackage ./switcher.nix { inherit config pkgs; };
      kls = pkgs.callPackage ./keyboard-layout-switch.nix { inherit pkgs; };
      szp = pkgs.callPackage ./show-zombie-parents.nix { inherit pkgs; };
      profiles = "/nix/var/nix/profiles/per-user/${config.home.username}/home-manager-*-link"; 
    in
    {
      home = {
        activation.changesReport = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          # Disable nvd if there are less than 2 hm profiles.
          if [ $(ls -d1v ${profiles} 2>/dev/null | wc -l) -lt 2 ]; then
            echo "Skipping changes report..."
          else
            $DRY_RUN_CMD ${pkgs.nvd}/bin/nvd diff $(ls -d1v ${profiles} | tail -2)
          fi
        '';

        packages =
          [
            gen-ssh-key # generate ssh key and add it to the system
            kls         # switch keyboard layout
            szp         # show zombie parents
            # hms       # custom home-manager switcher that considers the current DISPLAY
          ];
      };
    };
in [ scripts ]
