{ extraHomeConfig, inputs, system, pkgs, ... }:

let
  modules' = [
    inputs.neovim-flake.homeManagerModules.${system}.default
    inputs.nix-index.homeManagerModules.${system}.default
    { nix.registry.nixpkgs.flake = inputs.nixpkgs; }
    extraHomeConfig
  ];

  mkHome = { hidpi, mut ? false, mods ? [ ] }:
    inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = pkgs.xargs;
      modules = modules' ++ mods ++ [
        { inherit hidpi; dotfiles.mutable = mut; }
      ];
    };

  mkXmonadHome = { hidpi }: mkHome {
    inherit hidpi;
    mods = [ ../home/wm/xmonad/home.nix ];
  };

  mkHyprlandHome = { hidpi, mut ? false }: mkHome {
    inherit hidpi mut;
    mods = [
      inputs.hypr-binds-flake.homeManagerModules.${system}.default
      ../home/wm/hyprland/home.nix
    ];
  };
in
{
  hyprland-edp = mkHyprlandHome { hidpi = false; };
  hyprland-hdmi = mkHyprlandHome { hidpi = true; };
  hyprland-hdmi-mutable = mkHyprlandHome { hidpi = true; mut = true; };
  xmonad-edp = mkXmonadHome { hidpi = false; };
  xmonad-hdmi = mkXmonadHome { hidpi = true; };
}
