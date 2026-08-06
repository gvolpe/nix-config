{ extraHomeConfig, inputs, pkgs, system, ... }:

let
  modules' = [
    inputs.dots.homeModules.default
    inputs.neovim-flake.homeModules.default
    inputs.nix-index.homeModules.default
    { nix.registry.nixpkgs.flake = inputs.nixpkgs; }
    extraHomeConfig
  ];

  mkHome = { hidpi, mods ? [ ], mut ? false }:
    inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = pkgs.xargs;
      modules = modules' ++ mods ++ [
        (import ../home/dotfiles.nix { mutable = mut; })
        { inherit hidpi; }
      ];
    };

  mkXmonadHome = { hidpi }: mkHome {
    inherit hidpi;
    mods = [ ../home/wm/xmonad ];
  };

  mkHyprlandHome = { hidpi, mut ? false }: mkHome {
    inherit hidpi mut;
    mods = [
      inputs.hypr-binds-flake.homeManagerModules.${system}.default
      ../home/wm/hyprland
    ];
  };

  mkNiriHome = { hidpi, mut ? false }: mkHome {
    inherit hidpi mut;
    mods = [
      inputs.sunix.homeModules.default
      ../home/wm/niri
    ];
  };
in
{
  hyprland-laptop = mkHyprlandHome { hidpi = false; };
  hyprland-desktop = mkHyprlandHome { hidpi = true; };
  niri-laptop = mkNiriHome { hidpi = false; mut = true; };
  niri-desktop = mkNiriHome { hidpi = true; mut = true; };
  xmonad-laptop= mkXmonadHome { hidpi = false; };
  xmonad-desktop = mkXmonadHome { hidpi = true; };
}
