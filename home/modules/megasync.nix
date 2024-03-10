{ config, lib, pkgs, specialArgs, ... }:

with lib;

let
  cfg = config.programs.megasync;

  hidpiPackage = pkgs.symlinkJoin
    {
      name = "megasync";
      paths = [ pkgs.megasync ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/megasync --prefix QT_SCALE_FACTOR : 1
      '';
    };
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.programs.megasync = {
    enable = mkEnableOption "Syncing tool for Mega.nz";

    package = mkOption {
      type = types.package;
      default = if specialArgs.hidpi then hidpiPackage else pkgs.megasync;
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
  };
}
