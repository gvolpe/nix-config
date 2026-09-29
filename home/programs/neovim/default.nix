{ config, pkgs, ... }:

{
  home.packages = [
    (
      if config.dotfiles.mutable
      then pkgs.neovim-dev
      else pkgs.neovim
    )
  ];
}
