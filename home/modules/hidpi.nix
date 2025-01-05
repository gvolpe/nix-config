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
      alacritty.fontsize = mkOption {
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

      foot.fontsize = mkOption {
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

      polybar.fontsizes = {
        font0 = mkOption {
          type = types.int;
          default = if config.hidpi then 16 else 10;
          description = "Iosevka Nerd font size (text font)";
        };
        font1 = mkOption {
          type = types.int;
          default = if config.hidpi then 18 else 12;
          description = "Icomoon-feather font size (icon font)";
        };
        font2 = mkOption {
          type = types.int;
          default = if config.hidpi then 40 else 24;
          description = "Iosevka Nerd font size (Powerline Glyphs)";
        };
        font3 = mkOption {
          type = types.int;
          default = if config.hidpi then 28 else 18;
          description = "Iosevka Nerd font size (larger font size for bar fill icons)";
        };
        font4 = mkOption {
          type = types.int;
          default = if config.hidpi then 7 else 5;
          description = "Iosevka Nerd font size (smaller font size for shorter spaces)";
        };
        font5 = mkOption {
          type = types.int;
          default = if config.hidpi then 16 else 10;
          description = "FlagsWorldColor font size (keyboard layout icons)";
        };
      };
    };
  };
}
