{ pkgs, lib, specialArgs, ... }:

let
  inherit (specialArgs) inputs;
  inherit (pkgs.stdenv.hostPlatform) system;
in
{
  imports = [
    ../tongfang-amd
    inputs.home-manager.nixosModules.home-manager
    # iso image modules
    # FIXME: Package ‘zfs-kernel-2.3.0-6.13.5’ is marked as broken
    #"${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
    # disable networking.wireless from the iso minimal conf as we use networkmanager
    { networking.wireless.enable = lib.mkForce false; }
    { networking.hostName = lib.mkForce "xmod-amd"; }
    # home manager settings
    {
      home-manager = {
        extraSpecialArgs = pkgs.xargs;
        useGlobalPkgs = true;

        sharedModules = [
          inputs.neovim-flake.homeManagerModules.${system}.default
          inputs.nix-index.homeManagerModules.${system}.default
          ({ nix.registry.nixpkgs.flake = inputs.nixpkgs; })
        ];

        users.gvolpe = import ../../../home/wm/xmonad;
      };
    }
  ];
}
