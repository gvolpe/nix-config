{ extraHomeConfig ? { }, inputs, pkgs, ... }:

let
  inherit (pkgs.stdenv.hostPlatform) system;

  modules' = [
    inputs.agenix.homeManagerModules.default
    inputs.dots.homeModules.default
    inputs.neovim-flake.homeModules.default
    inputs.nix-index.homeModules.default
    (import ../home/secrets)
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
  hyprland-desktop = mkHyprlandHome { hidpi = true; };
  hyprland-laptop = mkHyprlandHome { hidpi = false; };
  niri = mkNiriHome { hidpi = true; mut = false; };
  niri-desktop = mkNiriHome { hidpi = true; mut = true; };
  niri-laptop = mkNiriHome { hidpi = false; mut = true; };
  xmonad-desktop = mkXmonadHome { hidpi = true; };
  xmonad-laptop = mkXmonadHome { hidpi = false; };
}
