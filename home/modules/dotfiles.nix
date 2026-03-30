{ config, lib, ... }:

{
  options = {
    dotfiles = {
      mutable = lib.mkEnableOption "mutable dotfiles";

      path = lib.mkOption {
        type = lib.types.path;
        apply = toString;
        default = "${config.home.homeDirectory}/workspace/nix-config/home";
        example = "${config.home.homeDirectory}/.dotfiles";
        description = "Location of the dotfiles working copy";
      };
    };
  };
}
