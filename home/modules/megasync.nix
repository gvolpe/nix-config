{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.megasync;

  hidpiPackage = pkgs.symlinkJoin
    {
      name = "megasync";
      paths = [ pkgs.megasync ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/megasync --prefix QT_SCALE_FACTOR : ${cfg.scaleFactor}
      '';
    };
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.programs.megasync = {
    enable = mkEnableOption "Syncing tool for Mega.nz";

    package = mkOption {
      type = types.package;
      default = if cfg.scale then hidpiPackage else pkgs.megasync;
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
  };
}
