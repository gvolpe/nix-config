{ inputs, system, ... }:

let
  nixosSystem = inputs.nixpkgs.lib.nixosSystem;
in
{
  dell-xps = nixosSystem {
    inherit system;
    specialArgs = { inherit inputs; };
    modules = [
      ../system/machine/dell-xps
      ../system/configuration.nix
    ];
  };

  tongfang-amd = nixosSystem {
    inherit system;
    specialArgs = { inherit inputs; };
    modules = [
      ../system/machine/tongfang-amd
      ../system/configuration.nix
    ];
  };
}
