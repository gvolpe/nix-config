{ pkgs, ... }:

{
  flags-world-color = pkgs.callPackage ./flags-world-color.nix { };
  icomoon-feather = pkgs.callPackage ./icomoon-feather.nix { };
}
