{ pkgs, ... }:

let
  colors = import ../../themes/colors.nix;
in
{
  services.dunst = {
    enable = true;
    iconTheme = {
      name = "BeautyLine";
      package = pkgs.beauty-line-icon-theme;
      size = "16x16";
    };
    settings = with colors.scheme.helios; {
      global = {
        monitor = 0;
        geometry = "600x50-50+65";
        shrink = "yes";
        transparency = 10;
        padding = 16;
        horizontal_padding = 16;
        frame_width = 3;
        frame_color = "${base00}";
        separator_color = "frame";
        font = "JetBrainsMono Nerd Font 10";
        line_height = 4;
        idle_threshold = 120;
        markup = "full";
        format = ''<b>%s</b>\n%b'';
        alignment = "left";
        vertical_alignment = "center";
        icon_position = "left";
        word_wrap = "yes";
        ignore_newline = "no";
        show_indicators = "yes";
        sort = true;
        stack_duplicates = true;
        startup_notification = false;
        hide_duplicate_count = true;
      };
      urgency_low = {
        background = "#${base00}";
        foreground = "#${base05}";
        timeout = 4;
      };
      urgency_normal = {
        background = "#${base00}";
        foreground = "#${base05}";
        timeout = 4;
      };
      urgency_critical = {
        background = "#${base00}";
        foreground = "#${base05}";
        frame_color = "#${base08}";
        timeout = 10;
      };
    };
  };
}
