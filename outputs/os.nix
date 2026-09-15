{ extraSystemConfig ? { }, inputs, pkgs, ... }:

let
  inherit (pkgs) lib;
  inherit (pkgs.stdenv.hostPlatform) system;
  inherit (inputs.nixpkgs.lib) nixosSystem;

  hosts = [ "aorus" "dell-xps" "live" "thinkpad-x1" "tongfang-amd" "xmod" ];

  modules' = [
    ../system/configuration.nix
    ../system/virtualisation.nix
    extraSystemConfig
    { nix.registry.nixpkgs.flake = inputs.nixpkgs; }
  ];

  make = host: {
    ${host} = nixosSystem {
      inherit lib pkgs system;
      modules = modules' ++ [ ../system/host/${host} ];
      specialArgs = { inherit inputs; };
    };
  };
in
lib.mergeAttrsList (map make hosts)
