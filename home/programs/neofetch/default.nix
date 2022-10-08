{ pkgs, ... }:

# command-line system information
{
  home.packages = [ pkgs.neofetch ];
  xdg.configFile."neofetch/config.conf".source = ./neofetch.conf;
}
