{ pkgs }:

{
  system = pkgs.lib.recurseIntoAttrs (
    pkgs.nixos [ ./configuration.nix ]
  );
}
