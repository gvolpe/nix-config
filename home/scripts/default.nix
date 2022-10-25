let
  scripts = { config, lib, pkgs, ... }:
    let
      gen-ssh-key = pkgs.callPackage ./gen-ssh-key.nix { inherit pkgs; };
      hcr = pkgs.callPackage ./changes-report.nix { inherit config pkgs; };
      kls = pkgs.callPackage ./keyboard-layout-switch.nix { inherit pkgs; };
      szp = pkgs.callPackage ./show-zombie-parents.nix { inherit pkgs; };
    in
    {
      home.packages = [
        hcr # home-manager changes report between generations
        gen-ssh-key # generate ssh key and add it to the system
        kls # switch keyboard layout
        szp # show zombie parents
      ];
    };
in
[ scripts ]
