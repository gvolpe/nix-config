{ pkgs, ... }:

let
  colors = import ../../themes/colors.nix;

  # Version 1.9.1 has a serious issue: https://github.com/dunst-project/dunst/issues/1102
  dunst190 = pkgs.dunst.overrideAttrs (old: rec {
    version = "1.9.0";

    src = pkgs.fetchFromGitHub {
      owner = "dunst-project";
      repo = "dunst";
      rev = "v${version}";
      sha256 = "sha256-fRPhu+kpwLPvdzIpXSjXFzQTfv4xewOMv/1ZqLJw3dk=";
    };

    postInstall = ''
      wrapProgram $out/bin/dunst \
        --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE"
    '';
  });
in
{
  services.dunst = {
    enable = true;
    package = dunst190;
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
        background = "#${base0A}";
        foreground = "#${base0D}";
        frame_color = "#${base08}";
        timeout = 10;
      };
    };
  };
}
