{ pkgs, ... }:

# command-line system information
{
  home.packages = with pkgs; [ hyfetch neofetch ];
  xdg.configFile."hyfetch.json".source = ./hyfetch.json;
  xdg.configFile."neofetch/config.conf".source = ./electric.conf;
}
