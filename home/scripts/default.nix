let
  scripts = { config, lib, pkgs, ... }:
    let
      gsk = pkgs.callPackage ./gen-ssh-key.nix { };
      kls = pkgs.callPackage ./keyboard-layout-switch.nix { };
      szp = pkgs.callPackage ./show-zombie-parents.nix { };
    in
    {
      home.packages = [
        gsk # generate ssh key and add it to the system
        kls # switch keyboard layout
        szp # show zombie parents
      ] ++ (pkgs.sxm.scripts or [ ]);
    };
in
[ scripts ]
