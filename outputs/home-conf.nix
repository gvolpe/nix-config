{ inputs, system, pkgs, extraArgs, ... }:

with inputs;

let
  imports = [
    neovim-flake.nixosModules.${system}.hm
    ../home/home.nix
  ];

  mkHome = { hidpi ? false }: (
    home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = extraArgs { inherit hidpi; };
      modules = [{ inherit imports; }];
    }
  );
in
{
  gvolpe-edp = mkHome { hidpi = false; };
  gvolpe-hdmi = mkHome { hidpi = true; };
}
