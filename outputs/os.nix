{ extraSystemConfig, inputs, system, pkgs, ... }:

with inputs;

let
  inherit (nixpkgs.lib) nixosSystem;
  inherit (pkgs) lib;

  tongfangModules = [
    ../system/machine/tongfang-amd
    ../system/configuration.nix
    extraSystemConfig
  ];
in
{
  dell-xps = nixosSystem {
    inherit lib pkgs system;
    specialArgs = { inherit inputs; };
    modules = [
      ../system/machine/dell-xps
      ../system/configuration.nix
      extraSystemConfig
    ];
  };

  thinkpad = nixosSystem {
    inherit lib pkgs system;
    specialArgs = { inherit inputs; };
    modules = [
      ../system/machine/thinkpad-x1
      ../system/configuration.nix
      extraSystemConfig
    ];
  };

  tongfang-amd = nixosSystem {
    inherit lib pkgs system;
    specialArgs = { inherit inputs; };
    modules = tongfangModules;
  };

  # FIXME: zfs-kernel-2.2.3-6.8.9 is marked as broken
  #edp-tongfang-amd = nixosSystem {
  #inherit lib pkgs system;
  #specialArgs = { inherit inputs; };
  #modules = tongfangModules ++ [
  ## edp modules
  #home-manager.nixosModules.home-manager
  #(import ./mod.nix {
  #inherit inputs system;
  #extraSpecialArgs = pkgs.xargs { hidpi = false; };
  #})
  ## iso image modules
  #"${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
  ## disable networking.wireless from the iso minimal conf as we use networkmanager
  #{ networking.wireless.enable = false; }
  ## vm user and password
  #{ users.users.gvolpe.initialPassword = "test"; }
  #];
  #};
}
