{ config, pkgs, ... }:

let
  profiles = "/nix/var/nix/profiles/per-user/${config.home.username}/home-manager-*-link";
in
pkgs.writeShellScriptBin "hm-changes-report" ''
  # Disable nvd if there are less than 2 hm profiles.
  if [ $(ls -d1v ${profiles} 2>/dev/null | wc -l) -lt 2 ]; then
    echo "Skipping changes report..."
  else
    ${pkgs.nvd}/bin/nvd diff $(ls -d1v ${profiles} | tail -2)
  fi
''
