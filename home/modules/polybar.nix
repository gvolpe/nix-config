{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.polybar;

  hdmiBar = pkgs.callPackage ../services/polybar/bar.nix { };

  laptopBar = pkgs.callPackage ../services/polybar/bar.nix {
    font0 = 10;
    font1 = 12;
    font2 = 24;
    font3 = 18;
    font4 = 5;
    font5 = 10;
  };
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.programs.polybar = {
    enable = mkEnableOption "Polybar with HiDPI settings.";

    hidpi = mkOption {
      type = types.bool;
      default = false;
      description = "Set proper HiDPI resolution.";
    };
  };

  config = mkIf cfg.enable (
    import ../services/polybar/default.nix {
      inherit config pkgs;
      mainBar = if cfg.hidpi then hdmiBar else laptopBar;
      openCalendar = "${pkgs.xfce.orage}/bin/orage";
    }
  );
}
