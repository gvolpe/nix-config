{ extraPkgs, inputs, system, pkgs, ... }:

with inputs;

let
  imports = [
    neovim-flake.nixosModules.${system}.hm
    ../home/home.nix
    ({ home.packages = extraPkgs; })
  ];

  mkHome = { hidpi }: (
    home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = pkgs.xargs { inherit hidpi; };
      modules = [{ inherit imports; }];
    }
  );
in
{
  gvolpe-edp = mkHome { hidpi = false; };
  gvolpe-hdmi = mkHome { hidpi = true; };
}
