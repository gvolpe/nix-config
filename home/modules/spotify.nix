{ config, lib, pkgs, specialArgs, ... }:

with lib;

let
  cfg = config.programs.spotify;

  hidpiPackage = pkgs.spotify.override { deviceScaleFactor = 1.4; };
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.programs.spotify = {
    enable = mkEnableOption "Play music from the Spotify music service.";

    package = mkOption {
      type = types.package;
      default = if specialArgs.hidpi then hidpiPackage else pkgs.spotify;
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
  };
}
