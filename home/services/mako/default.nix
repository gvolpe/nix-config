{ ... }:

let
  colors = import ../../themes/colors.nix;
in
{
  services.mako = {
    enable = true;

    settings = with colors.scheme.helios; {
      "actionable=false" = {
        anchor = "top-left";
      };
      actions = false;
      anchor = "top-right";
      background-color = "#${base00}";
      border-color = "#${base00}";
      border-radius = 0;
      text-color = "#${base05}";
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
        border-color = "#${base08}";
        default-timeout = 10;
      };
    };
  };
}
