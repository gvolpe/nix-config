{ extraSystemConfig, inputs, system, pkgs, ... }:

with inputs;

let
  inherit (nixpkgs.lib) nixosSystem;
  inherit (pkgs) lib;

  tongfangModules = [
    ../system/modules/globalprotectvpn.nix
    ../system/machine/tongfang-amd
    ../system/configuration.nix
    ../system/virtualisation.nix
    extraSystemConfig
  ];
in
{
  dell-xps = nixosSystem {
    inherit lib pkgs system;
    specialArgs = { inherit inputs; };
    modules = [
      ../system/modules/globalprotectvpn.nix
      ../system/machine/dell-xps
      ../system/configuration.nix
      ../system/virtualisation.nix
      extraSystemConfig
    ];
  };

  thinkpad = nixosSystem {
    inherit lib pkgs system;
    specialArgs = { inherit inputs; };
    modules = [
      ../system/modules/globalprotectvpn.nix
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

  edp-tongfang-amd = nixosSystem {
    inherit lib pkgs system;
    specialArgs = { inherit inputs; };
    modules = tongfangModules ++ [
      home-manager.nixosModules.home-manager
      (import ./mod.nix {
        inherit inputs system;
        extraSpecialArgs = pkgs.xargs { hidpi = false; };
      })
      # FIXME: zfs-kernel-2.2.3-6.8.9 is marked as broken
      # iso image modules
      #"${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
      # disable networking.wireless from the iso minimal conf as we use networkmanager
      #{ networking.wireless.enable = false; }
    ];
  };
}
