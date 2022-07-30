{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.spotify;

  package = pkgs.spotify;

  hidpiPackage = pkgs.symlinkJoin
    {
      name = "spotify";
      paths = [ pkgs.spotify ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/spotify --add-flags "-force-device-scale-factor=1.4"
      '';
    };
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.programs.spotify = {
    enable = mkEnableOption "Play music from the Spotify music service.";

    hidpi = mkOption {
      type = types.bool;
      default = false;
      description = "Set proper HiDPI resolution.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      (if cfg.hidpi then hidpiPackage else package)
    ];
  };
}
