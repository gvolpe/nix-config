{ pkgs, specialArgs, ... }:

let
  inherit (specialArgs) inputs;
  inherit (pkgs.stdenv.hostPlatform) system;
in
{
  imports = [
    ./vm.nix
    inputs.home-manager.nixosModules.home-manager
    # iso image modules
    "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
    # home manager settings
    {
      home-manager = {
        extraSpecialArgs = pkgs.xargs;
        useGlobalPkgs = true;

        sharedModules = [
          inputs.neovim-flake.homeManagerModules.${system}.default
          inputs.nix-index.homeManagerModules.${system}.default
          inputs.vicinae.homeManagerModules.default
          ({ nix.registry.nixpkgs.flake = inputs.nixpkgs; })
          { hidpi = false; dotfiles.mutable = false; }
        ];

        users.gvolpe = import ../../../home/wm/niri/vm.nix;
      };
    }
  ];
}
