{ config, lib, pkgs, ... }:

{
  wrappers.fastfetch = lib.mkWrapper {
    basePackage = pkgs.fastfetch;
    prependFlags = [
      "--config"
      (config.dotfiles.make ./config.jsonc)
    ];
  };
}
