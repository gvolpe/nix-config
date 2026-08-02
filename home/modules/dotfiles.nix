{ config, lib, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  cfg = config.dotfiles;
  homeSourceRoot = ./..;
in
{
  options = {
    dotfiles = {
      /**
      Correctly link a configuration file in an immutable or mutable fashion,
      depending on the value of ${config.dotfiles.mutable}.

      When `dotfiles.mutable = true`, it returns a symlink derivation. Otherwise,
      it returns the direct Nix path literal (which HM copies to the store during activation).

      # Inputs

      `filepath`: the immutable configuration file path to link.

      # Type

      ```
      dotfiles.make :: Path -> (Path | Derivation)
      ```

      # Usage
      :::{.example}

      ```nix
      { config, pkgs, ... }:

      {
        xdg.configFile."fastfetch/config.jsonc".source =
          config.dotfiles.make ./config.jsonc;
      }
      ```

      :::
      */
      make = lib.mkOption {
        default = filepath:
          let
            relpath = lib.removePrefix "./" (lib.path.removePrefix homeSourceRoot filepath);
          in if !cfg.mutable then filepath
          else mkOutOfStoreSymlink "${cfg.path}/${relpath}";
        description = "Make an immutable or mutable config file";
      };

      mutable = lib.mkEnableOption "mutable dotfiles";

      path = lib.mkOption {
        apply = toString;
        default = "${config.home.homeDirectory}/workspace/nix-config/home";
        description = "Location of the mutable dotfiles home tree";
        example = "${config.home.homeDirectory}/.dotfiles/home";
        type = lib.types.path;
      };
    };
  };
}
