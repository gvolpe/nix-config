{ ... }:

let
  colors = import ../../themes/colors.nix;
in
{
  services.mako = {
    enable = true;

    settings = with colors.scheme.helios; {
      actions = false;
      anchor = "top-right";
      border-radius = 0;
      default-timeout = 4;
      font = "JetBrainsMono Nerd Font 10";
      height = 100;
      icons = true;
      ignore-timeout = false;
      layer = "top";
      margin = 10;
      markup = true;
      width = 300;

      "urgency=critical" = {
        background-color = "#${base00}";
        text-color = "#${base05}";
        border-color = "#${base08}";
        border-radius = 1;
        default-timeout = 10;
      };
    };
  };
}
