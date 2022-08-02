{ config, lib, pkgs, specialArgs, ... }:

with lib;

let
  cfg = config.programs.termie;
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.programs.termie = {
    enable = mkEnableOption "Alacritty terminal with HiDPI settings.";
  };

  config = mkIf cfg.enable (
    import ../programs/alacritty/default.nix {
      inherit pkgs;
      fontSize = (if specialArgs.hidpi then 10 else 8);
    }
  );
}
