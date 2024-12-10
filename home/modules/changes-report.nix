{ config, lib, pkgs, ... }:

{
  options.home.changes-report.enable = lib.mkEnableOption "changesReport";

  config = lib.mkIf config.home.changes-report.enable {
    home.activation.changesReport = lib.hm.dag.entryAnywhere ''
      ${pkgs.nvd}/bin/nvd diff $oldGenPath $newGenPath
    '';
  };
}
