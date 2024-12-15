{ pkgs, config, specialArgs, ... }:

let
  path = "/home/gvolpe/workspace/nix-config/home/programs/neofetch/electric.conf";
  configSrc =
    if specialArgs.mutableDotFiles
    then config.lib.file.mkOutOfStoreSymlink path
    else ./electric.conf;
in
{
  home.packages = with pkgs; [ hyfetch neofetch ];
  xdg.configFile."hyfetch.json".source = ./hyfetch.json;
  xdg.configFile."neofetch/config.conf".source = configSrc;
}
