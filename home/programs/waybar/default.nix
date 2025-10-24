{ config, lib, ... }:

let
  filePath = "${config.dotfiles.path}/programs/waybar/style.css";

  styleImport =
    if !config.dotfiles.mutable then ''@import url("${./style.css}";''
    else ''@import url("${filePath}");'';

  output = if config.hidpi then [ "DP-3" ] else [ "eDP-1" ];
  include = [ "${./niri.json}" ];
in
{
  # status bar for niri/wayland
  programs.waybar = {
    enable = true;
    style = ''
      * {
        font-size: ${toString config.services.waybar.fontsize}px;
        font-family: monospace;
      }

      ${styleImport}
    '';
    settings = [
      {
        inherit output include;
        position = "top";
        modules-left = [
          "niri/workspaces"
          "custom/right-arrow-dark"
        ];
        modules-center = [
          "custom/left-arrow-dark"
          "clock#2"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "tray"
          "custom/right-arrow-dark"
          "custom/right-arrow-light"
          "clock#3"
          "custom/right-arrow-dark"
        ];
        modules-right = lib.optionals config.hidpi
          ([
            "custom/left-arrow-dark"
            "memory"
            "custom/left-arrow-light"
            "custom/left-arrow-dark"
            "cpu"
            "custom/left-arrow-light"
            "custom/left-arrow-dark"
            "disk"
            "custom/left-arrow-light"
          ]) ++ [
          "custom/left-arrow-dark"
          "pulseaudio"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "custom/notification"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "battery"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
        ];
      }
      {
        inherit output include;
        position = "bottom";
        modules-left = [
          "custom/right-arrow-dark"
          "custom/right-arrow-light"
          "custom/spotify"
          "custom/mpris"
          "custom/right-arrow-dark"
        ];
        modules-center = [
          "custom/left-arrow-dark"
          "niri/window"
          "custom/right-arrow-dark"
        ];
        modules-right = [
          "custom/left-arrow-dark"
          "niri/language"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "network"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "custom/audio_idle_inhibitor"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "group/group-power"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
        ];

        "niri/window" = {
          "format" = "{}";
          "rewrite" = {
            "(.*) — Mozilla Firefox" = "  $1";
            "^.*Github.*" = "  Github";
            "~/(.*)" = "   [~/$1]";
            "vim (.*)" = "   [$1]";
            "(.*)fish" = " 󰈺 [~/$1]";
            "(.*) Slack Grid Workspaces - Slack" = "   [$1]";
            "(.*)Meeting" = "   Meeting $1";
          };
          "max-length" = config.services.waybar.window.maxlen;
          "separate-outputs" = true;
        };
      }
    ];
    systemd.enable = true;
  };
}
