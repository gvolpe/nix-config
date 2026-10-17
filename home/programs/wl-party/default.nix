{ config, pkgs, ... }:

let
  inherit (config.dotfiles) make;

  effects = [ "fireworks" "nixos-confetti" "nixos-rain" ];
in
{
  home.packages = [ pkgs.wl-party ];

  xdg.configFile = builtins.listToAttrs (map
    (fx: {
      name = "wl-party/effects/${fx}.rhai";
      value.source = make (./. + "/${fx}.rhai");
    })
    effects);
}
