{ config, pkgs, ... }:

let
  source = fetchTarball {
    name   = "base16";
    url    = "https://github.com/atpotts/base16-nix/archive/4f192af.tar.gz";
    sha256 = "1yf59vpd1i8lb2ml7ha8v6i4mv1b0xwss8ngzw08s39j838gyx6h";
  };
in
{
  imports = [ "${source}/base16.nix" ];

  # More schemes at: https://github.com/atpotts/base16-nix/blob/master/schemes.json
  themes.base16 = {
    enable  = true;
    scheme  = "helios";
    variant = "helios";
  };

  services.dunst = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome3.adwaita-icon-theme;
      size = "16x16";
    };
    settings = with config.lib.base16.theme; {
      global = {
        monitor = 0;
        geometry = "600x50-50+65";
        shrink = "yes";
        transparency = 10;
        padding = 16;
        horizontal_padding = 16;
        frame_width = 3;
        frame_color = "${base00-hex}";
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
        background = "#${base00-hex}";
        forefround = "#${base05-hex}";
        timeout = 4;
      };
      urgency_normal = {
        background = "#${base00-hex}";
        forefround = "#${base05-hex}";
        timeout = 4;
      };
      urgency_critical = {
        background = "#${base0A-hex}";
        forefrond = "#${base0D-hex}";
        frame_color = "#${base08-hex}";
        timeout = 10;
      };
    };
  };
}
