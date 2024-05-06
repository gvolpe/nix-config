{ pkgs, ... }:

{
  # scratchpads
  home.packages = [ pkgs.pyprland ];
  xdg.configFile."hypr/pyprland.toml".source = ./pyprland.toml;
}
