{ config, lib, ... }:

with lib;

let
  cfg = config.hidpi;
  niri = config.xdg.portal;
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.hidpi = {
    enable = lib.mkEnableOption "HiDPI displays";

    browser.settings.dpi = mkOption {
      type = types.str;
      default =
        if niri.enable then (if cfg.enable then "0" else "1.7")
        else "0";
    };

    kitty.fontsize = mkOption {
      type = types.int;
      default = if cfg.enable then 14 else 10;
    };

    niri = {
      output = mkOption {
        type = types.str;
        default = if cfg.enable then ''include "hdmi.kdl"'' else ''include "edp.kdl"'';
        description = "The niri output file to include";
      };
    };

    swaylock = {
      ringsize = mkOption {
        type = types.int;
        default = if cfg.enable then 100 else 70;
        description = "Swaylock-plugin ring size";
      };
      ringthickness = mkOption {
        type = types.int;
        default = if cfg.enable then 7 else 5;
        description = "Swaylock-plugin ring thickness";
      };
    };

    waybar = {
      fontsize = mkOption {
        type = types.int;
        default = if cfg.enable then 20 else 16;
        description = "Waybar main font size";
      };
      outputs = mkOption {
        type = types.listOf types.str;
        default = if cfg.enable then [ "DP-1" "DP-3" "HDMI-1" "Virtual-1" ] else [ "eDP-1" ];
        description = "The outputs on which Waybar is displayed";
      };
      window.maxlen = mkOption {
        type = types.int;
        default = if cfg.enable then 70 else 35;
        description = "Waybar window title max length";
      };
    };
  };
}
