{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.signal;

  jsonType = types.attrsOf types.anything;
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.programs.signal = {
    enable = mkEnableOption "Privacy-focused messaging client";

    package = mkOption {
      type = types.package;
      default = pkgs.signal-desktop;
      example = literalExpression "pkgs.signal-desktop";
      description = "The signal-desktop package to use";
    };

    settings = mkOption {
      type = jsonType;
      default = { };
      example = literalExpression ''
        {
          window = {
            maximized = false;
            autoHideMenuBar = false;
            fullscreen = false;
            width = 1015;
            height = 515;
            x = 6;
            y = 32;
          };
          spell-check = false;
          theme-setting = "dark";
        }
      '';
      description = "Attribute set of signal preferences (converted to JSON file).";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."Signal/ephemeral.json" = mkIf (cfg.settings != { }) {
      text = generators.toJSON { } cfg.settings;
    };
  };
}

