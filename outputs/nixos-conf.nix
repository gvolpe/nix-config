{ inputs, system, ... }:

let
  inherit (inputs.nixpkgs.lib) nixosSystem;

  libx = import ../lib { inherit (inputs.nixpkgs) lib; };

  lib = inputs.nixpkgs.lib.extend (_: _: {
    inherit (libx) secretManager;
  });

  sxmOverlay =
    if (builtins.hasAttr "sxm-flake" inputs)
    then inputs.sxm-flake.overlays.default
    else (f: p: { });

  pkgs = import inputs.nixpkgs {
    inherit system;
    config = {
      allowUnfree = true;
      permittedInsecurePackages = [
        "xrdp-0.9.9"
      ];
    };
    overlays = [ sxmOverlay ];
  };
in
{
  dell-xps = nixosSystem {
    inherit lib pkgs system;
    specialArgs = { inherit inputs; };
    modules = [
      ../system/machine/dell-xps
      ../system/configuration.nix
    ];
  };

  tongfang-amd = nixosSystem {
    inherit lib pkgs system;
    specialArgs = { inherit inputs; };
    modules = [
      ../system/machine/tongfang-amd
      ../system/configuration.nix
    ];
  };
}
