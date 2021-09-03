{ pkgs }:

{
  system = pkgs.lib.recurseIntoAttrs (
    pkgs.nixos [ ./configuration.nix ]
  );

  recurseForDerivations = true;
}
