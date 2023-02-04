{ inputs, system, ... }:

with inputs;

let
  pkgs = import nixpkgs {
    inherit system;

    config.allowUnfree = true;

    overlays = [
      neovim-flake.overlays.${system}.default
    ];
  };
in
{
  metals = pkgs.callPackage ../home/programs/neovim-ide/metals.nix { };
  metals-updater = pkgs.callPackage ../home/programs/neovim-ide/update-metals.nix { };
}
