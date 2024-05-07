{ extraPkgs, inputs, system, pkgs, ... }:

with inputs;

let
  sharedImports = [
    neovim-flake.homeManagerModules.${system}.default
    ({ home.packages = extraPkgs; })
  ];

  hyprlandDpiSettings = { hidpi }: {
    programs.browser.settings.dpi = if hidpi then "0" else "1.7";
  };

  xmonadDpiSettings = { hidpi }: {
    programs.browser.settings.dpi = if hidpi then "-1.0" else "0.7";
  };

  mkXmonadHome = { hidpi }:
    let
      imports = sharedImports ++ [
        ../home/wm/xmonad/home.nix
        (xmonadDpiSettings { inherit hidpi; })
      ];
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
      imports = sharedImports ++ [
        ../home/wm/hyprland/home.nix
        (hyprlandDpiSettings { inherit hidpi; })
      ];
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
