{ config, pkgs, ... }:

let
  gen-ssh-key = pkgs.callPackage ./gen-ssh-key.nix { inherit pkgs; };
  hms = pkgs.callPackage ./switcher.nix { inherit config pkgs; };
in
[
  gen-ssh-key # generate ssh key and add it to the system
  hms         # custom home-manager switcher that considers the current DISPLAY
]
