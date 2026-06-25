{ config, lib, pkgs, ... }:

{
  options.home.changes-report.enable = lib.mkEnableOption "changesReport";

  config = lib.mkIf config.home.changes-report.enable {
    home.activation.changesReport = lib.hm.dag.entryAnywhere ''
      if [ -n "$oldGenPath" ] && [ -n "$newGenPath" ]; then
          ${pkgs.nvd}/bin/nvd diff $oldGenPath $newGenPath
      else
          echo "Unable to process diff:"
          echo "  - oldGenPath: $oldGenPath"
          echo "  - newGenPath: $newGenPath"
      fi
    '';
  };
}
