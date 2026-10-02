{ config, lib, ... }:

with lib;

let
  niri = config.xdg.portal;
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options = {
    hidpi = lib.mkEnableOption "HiDPI displays";

    programs = {
      browser.settings.dpi = mkOption {
        type = types.str;
        default =
          if niri.enable then (if config.hidpi then "0" else "1.7")
          else "0";
      };

      kitty.fontsize = mkOption {
        type = types.int;
        default = if config.hidpi then 14 else 10;
      };
    };

    services = {
      waybar = {
        fontsize = mkOption {
          type = types.int;
          default = if config.hidpi then 20 else 16;
          description = "Waybar main font size";
        };
        window.maxlen = mkOption {
          type = types.int;
          default = if config.hidpi then 70 else 35;
          description = "Waybar window title max length";
        };
      };
    };
  };
}
