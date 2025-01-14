{ extraSystemConfig, inputs, system, pkgs, ... }:

let
  inherit (inputs.nixpkgs.lib) nixosSystem;
  inherit (pkgs) lib;

  hosts = [ "dell-xps" "thinkpad-x1" "tongfang-amd" "xmod" ];

  modules' = [
    ../system/modules/globalprotectvpn.nix
    ../system/configuration.nix
    ../system/virtualisation.nix
    extraSystemConfig
    { nix.registry.nixpkgs.flake = inputs.nixpkgs; }
  ];

  make = host: {
    ${host} = nixosSystem {
      inherit lib pkgs system;
      specialArgs = { inherit inputs; };
      modules = modules' ++ [ ../system/machine/${host} ];
    };
  };
in
lib.mergeAttrsList (map make hosts)
