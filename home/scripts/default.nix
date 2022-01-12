{ config, pkgs, ... }:

let
  gen-ssh-key = pkgs.callPackage ./gen-ssh-key.nix { inherit pkgs; };
  hms = pkgs.callPackage ./switcher.nix { inherit config pkgs; };
  kls = pkgs.callPackage ./keyboard-layout-switch.nix { inherit pkgs; };
in
[
  gen-ssh-key # generate ssh key and add it to the system
  kls         # switch keyboard layout
  # hms       # custom home-manager switcher that considers the current DISPLAY
]
