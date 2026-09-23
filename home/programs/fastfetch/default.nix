{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.make ./config.jsonc;
in
{
  wrappers.fastfetch = lib.mkWrapper {
    basePackage = pkgs.fastfetch;
    prependFlags = [ "--config" cfg ];
  };

  # this is not required, but leaving it here for demo purposes
  xdg.configFile."fastfetch/config.jsonc".source = cfg;
}
