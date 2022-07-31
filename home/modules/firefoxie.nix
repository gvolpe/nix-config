{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.firefoxie;
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.programs.firefoxie = {
    enable = mkEnableOption "Firefox browser with HiDPI settings.";

    addons = mkOption {
      type = types.attrsOf types.package;
      example = "nur.repos.rycee.firefox-addons;";
      description = "NUR packages addons.";
    };

    hidpi = mkOption {
      type = types.bool;
      default = false;
      description = "Set proper HiDPI resolution.";
    };
  };

  config = mkIf cfg.enable (
    import ../programs/browsers/firefox.nix {
      inherit (cfg) addons hidpi; inherit pkgs;
    }
  );
}
