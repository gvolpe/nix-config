{ extraPkgs, extraImports, inputs, system, pkgs, ... }:

with inputs;

let
  sharedImports = [
    neovim-flake.homeManagerModules.${system}.default
    nix-index.homeManagerModules.${system}.default
    ({ home.packages = extraPkgs; })
    ({ nix.registry.nixpkgs.flake = inputs.nixpkgs; })
    extraImports
  ];

  mkXmonadHome = { hidpi }:
    let
      imports = sharedImports ++ [
        ../home/wm/xmonad/home.nix
        ({ inherit hidpi; })
      ];
    in
    (
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = pkgs.xargs;
        modules = [{ inherit imports; }];
      }
    );

  mkHyprlandHome = { hidpi, mutableDotFiles ? false }:
    let
      imports = sharedImports ++ [
        inputs.hypr-binds-flake.homeManagerModules.${system}.default
        ../home/wm/hyprland/home.nix
        ({ inherit hidpi; dotfiles.mutable = mutableDotFiles; })
      ];
    in
    (
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = pkgs.xargs;
        modules = [{ inherit imports; }];
      }
    );
in
{
  hyprland-edp = mkHyprlandHome { hidpi = false; };
  hyprland-hdmi = mkHyprlandHome { hidpi = true; };
  hyprland-hdmi-mutable = mkHyprlandHome { hidpi = true; mutableDotFiles = true; };
  xmonad-edp = mkXmonadHome { hidpi = false; };
  xmonad-hdmi = mkXmonadHome { hidpi = true; };
}
