{ extraHomeConfig ? { }, inputs, pkgs, ... }:

let
  modules' = [
    inputs.agenix.homeManagerModules.default
    inputs.dots.homeModules.default
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

  mkNiriHome = { hidpi, mut ? false }: mkHome {
    inherit hidpi mut;
    mods = [
      inputs.sunix.homeModules.default
      ../home/wm/niri
    ];
  };
in
{
  niri = mkNiriHome { hidpi = true; mut = false; };
  niri-desktop = mkNiriHome { hidpi = true; mut = true; };
  niri-laptop = mkNiriHome { hidpi = false; mut = true; };
}
