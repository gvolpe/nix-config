{ config, lib, pkgs, specialArgs, ... }:

with lib;

let
  cfg = config.programs.megasync;

  # See https://github.com/NixOS/nixpkgs/pull/224183
  package = pkgs.megasync.override {
    ffmpeg = pkgs.ffmpeg_4;
  };

  hidpiPackage = pkgs.symlinkJoin
    {
      name = "megasync";
      paths = [ package ];
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
  };

  config = mkIf cfg.enable {
    home.packages = [
      (if specialArgs.hidpi then hidpiPackage else package)
    ];
  };
}
