{ config, lib, pkgs, specialArgs, ... }:

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
  };

  config = mkIf cfg.enable (
    import ../programs/browsers/firefox.nix {
      inherit (cfg) addons;
      inherit (specialArgs) hidpi;
      inherit pkgs;
    }
  );
}
