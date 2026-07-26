{ config, lib, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  cfg = config.dotfiles;
in
{
  options = {
    dotfiles = {
      make = lib.mkOption {
        default = filepath: subpath:
          if !cfg.mutable then filepath
          else mkOutOfStoreSymlink "${cfg.path}/${subpath}/${builtins.baseNameOf filepath}";
        description = "Make an immutable or mutable config file";
      };

      mutable = lib.mkEnableOption "mutable dotfiles";

      path = lib.mkOption {
        apply = toString;
        default = "${config.home.homeDirectory}/workspace/nix-config/home";
        description = "Location of the dotfiles working copy";
        example = "${config.home.homeDirectory}/.dotfiles";
        type = lib.types.path;
      };
    };
  };
}
