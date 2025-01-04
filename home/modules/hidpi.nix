{ config, lib, ... }:

with lib;

{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options = {
    hidpi = lib.mkEnableOption "HiDPI displays";

    programs = {
      browser.settings.dpi = mkOption {
        type = types.str;
        default = "0";
      };

      foot.fontSize = mkOption {
        type = types.str;
        default = if config.hidpi then "14" else "10";
      };

      signal.scaleFactor = mkOption {
        type = types.str;
        default = if config.hidpi then "2" else "1.5";
      };
    };

    services = {
      hypridle.dpms = mkOption {
        type = types.attrs;
        default = if config.hidpi then { } else
        {
          timeout = 1200;
          on-timeout = "hyprctl dispatch dpms off";
          on-resume = "hyprctl dispatch dpms on";
        };
      };
    };
  };
}
