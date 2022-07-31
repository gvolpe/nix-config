{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.termie;
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.programs.termie = {
    enable = mkEnableOption "Alacritty terminal with HiDPI settings.";

    hidpi = mkOption {
      type = types.bool;
      default = false;
      description = "Set proper HiDPI resolution.";
    };
  };

  config = mkIf cfg.enable (
    import ../programs/alacritty/default.nix { fontSize = (if cfg.hidpi then 10 else 8); inherit pkgs; }
  );
}
