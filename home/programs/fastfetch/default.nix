{ config, pkgs, ... }:

{
  home.packages = [ pkgs.fastfetch ];

  xdg.configFile."fastfetch/config.jsonc".source =
    config.dotfiles.make ./config.jsonc "programs/fastfetch";
}
