{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.megasync;

  package = pkgs.megasync;

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
