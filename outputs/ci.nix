{ pkgs, ... }:

{
  metals = pkgs.callPackage ../home/programs/neovim-ide/metals.nix { };
  metals-updater = pkgs.callPackage ../home/programs/neovim-ide/update-metals.nix { };
}
