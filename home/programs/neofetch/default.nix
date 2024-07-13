{ pkgs, ... }:

# command-line system information
{
  home.packages = with pkgs; [ hyfetch neofetch ];
  xdg.configFile."neofetch/config.conf".source = ./electric.conf;
}
