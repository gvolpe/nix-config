{ inputs, system, pkgs, ... }:

with inputs;

let
  imports = [
    neovim-flake.nixosModules.${system}.hm
    ../home/home.nix
  ];

  mkHome = { hidpi ? false }: (
    home-manager.lib.homeManagerConfiguration {
      inherit pkgs;

      extraSpecialArgs = {
        inherit gh-md-toc hidpi;
        inherit (rycee-nurpkgs.lib.${system}) buildFirefoxXpiAddon;
        addons = pkgs.nur.repos.rycee.firefox-addons;
      };

      modules = [{ inherit imports; }];
    }
  );
in
{
  gvolpe-edp = mkHome { hidpi = false; };
  gvolpe-hdmi = mkHome { hidpi = true; };
}
