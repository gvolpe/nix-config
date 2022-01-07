{ lib, inputs, system, ... }:

{
  dell-xps = lib.nixosSystem {
    inherit system;
    specialArgs = { inherit inputs; };
    modules = [
      ../system/machine/dell-xps.nix
      ../system/configuration.nix
    ];
  };

  tongfang-amd = lib.nixosSystem {
    inherit system;
    specialArgs = { inherit inputs; };
    modules = [
      ../system/machine/tongfang-amd.nix
      ../system/configuration.nix
    ];
  };
}
