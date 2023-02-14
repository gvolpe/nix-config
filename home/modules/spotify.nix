{ config, lib, pkgs, specialArgs, ... }:

with lib;

let
  cfg = config.programs.spotify;

  package = pkgs.spotify;

  hidpiPackage = pkgs.spotify.override { deviceScaleFactor = 1.4; };
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.programs.spotify = {
    enable = mkEnableOption "Play music from the Spotify music service.";
  };

  config = mkIf cfg.enable {
    home.packages = [
      (if specialArgs.hidpi then hidpiPackage else package)
    ];
  };
}
