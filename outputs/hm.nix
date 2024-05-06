{ extraPkgs, inputs, system, pkgs, ... }:

with inputs;

let
  sharedImports = [
    neovim-flake.homeManagerModules.${system}.default
    ({ home.packages = extraPkgs; })
  ];

  mkXmonadHome = { hidpi }:
    let
      imports = sharedImports ++ [ ../home/wm/xmonad/home.nix ];
    in
    (
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = pkgs.xargs { inherit hidpi; };
        modules = [{ inherit imports; }];
      }
    );

  mkHyprlandHome = { hidpi }:
    let
      imports = sharedImports ++ [ ../home/wm/hyprland/home.nix ];
    in
    (
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = pkgs.xargs { inherit hidpi; };
        modules = [{ inherit imports; }];
      }
    );
in
{
  hyprland-edp = mkHyprlandHome { hidpi = false; };
  hyprland-hdmi = mkHyprlandHome { hidpi = true; };
  xmonad-edp = mkXmonadHome { hidpi = false; };
  xmonad-hdmi = mkXmonadHome { hidpi = true; };
}
