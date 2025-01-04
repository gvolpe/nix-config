{ config, lib, ... }:

with lib;

let
  inherit (config.wayland.windowManager) hyprland;
  inherit (config.xsession.windowManager) xmonad;
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options = {
    hidpi = lib.mkEnableOption "HiDPI displays";

    programs = {
      alacritty.fontSize = mkOption {
        type = types.int;
        default = if config.hidpi then 10 else 8;
      };

      browser.settings.dpi = mkOption {
        type = types.str;
        default =
          if hyprland.enable then (if config.hidpi then "0" else "1.7")
          else if xmonad.enable then (if config.hidpi then "-1.0" else "0.7")
          else "0";
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
