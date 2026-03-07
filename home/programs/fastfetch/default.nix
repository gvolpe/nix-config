{ pkgs, config, ... }:

let
  filePath = "${config.dotfiles.path}/programs/fastfetch/config.jsonc";
  configSrc =
    if !config.dotfiles.mutable then ./config.jsonc
    else config.lib.file.mkOutOfStoreSymlink filePath;
in
{
  home.packages = [ pkgs.fastfetch ];
  xdg.configFile."fastfetch/config.jsonc".source = configSrc;
}
