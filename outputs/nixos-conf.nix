{ inputs, system, pkgs, ... }:

with inputs;

let
  inherit (nixpkgs.lib) nixosSystem;
  inherit (pkgs) lib;

  tongfangModules = [
    ../system/machine/tongfang-amd
    ../system/configuration.nix
  ];

  edpHomeModules = [
    home-manager.nixosModules.home-manager
    (import ./home-module.nix {
      inherit inputs system;
      extraSpecialArgs = pkgs.xargs { hidpi = false; };
    })
  ];

  isoModules = [
    "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
    # disable networking.wireless from the iso minimal conf as we use networkmanager
    { networking.wireless.enable = false; }
  ];

  vmUser = [{
    users.users.gvolpe.initialPassword = "test";
  }];
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

  thinkpad = nixosSystem {
    inherit lib pkgs system;
    specialArgs = { inherit inputs; };
    modules = [
      ../system/machine/thinkpad-x1
      ../system/configuration.nix
    ];
  };

  tongfang-amd = nixosSystem {
    inherit lib pkgs system;
    specialArgs = { inherit inputs; };
    modules = tongfangModules;
  };

  edp-tongfang-amd = nixosSystem {
    inherit lib pkgs system;
    specialArgs = { inherit inputs; };
    modules = tongfangModules ++ edpHomeModules ++ isoModules ++ vmUser;
  };
}
