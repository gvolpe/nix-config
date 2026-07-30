{ config, lib, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  cfg = config.dotfiles;
  sourcePath = ./..;
in
{
  options = {
    dotfiles = {
      /**
      Correctly link a configuration file in an immutable or mutable fashion,
      depending on the value of ${config.dotfiles.mutable}.

      # Inputs

      `filepath`: the immutable configuration file path to link.
      `rootpath`: the source directory containing the configuration file.

      # Type

      ```
      dotfiles.make :: Path -> Path -> (Path | Derivation)
      ```

      # Usage
      :::{.example}

      When `dotfiles.mutable = true`, it returns a symlink derivation:

      ```console
      nix-repl> :b homeConfigurations.niri-hdmi.config.dotfiles.make ./home/programs/fastfetch/config.jsonc ./home/programs/fastfetch

      This derivation produced the following outputs:
        out -> /nix/store/p62qfcfschb9g8fcgah4xvdk88b89mmx-hm_config.jsonc

      $ readlink -f /nix/store/p62qfcfschb9g8fcgah4xvdk88b89mmx-hm_config.jsonc
      /home/gvolpe/workspace/nix-config/home/programs/fastfetch/config.jsonc
      ```

      When `dotfiles.mutable = false`, it returns the direct Nix path literal (which HM copies to the store during activation):

      ```console
      nix-repl> homeConfigurations.niri-hdmi.config.dotfiles.make ./home/programs/fastfetch/config.jsonc ./home/programs/fastfetch
      /home/gvolpe/workspace/nix-config/home/programs/fastfetch/config.jsonc

      nix-repl> cf = homeConfigurations.niri-hdmi.config.dotfiles.make ./home/programs/fastfetch/config.jsonc ./home/programs/fastfetch
      nix-repl> "${cf}"
      "/nix/store/n62mxdkhz8a16za5pm2k5n21blv28lkd-config.jsonc"

      readlink -f /nix/store/n62mxdkhz8a16za5pm2k5n21blv28lkd-config.jsonc
      /nix/store/n62mxdkhz8a16za5pm2k5n21blv28lkd-config.jsonc
      ```

      :::
      */
      make = lib.mkOption {
        default = filepath: rootpath:
          let
            rootRelpath = lib.path.removePrefix sourcePath rootpath;
            fileRelpath = lib.path.removePrefix rootpath filepath;
          in if !cfg.mutable then filepath
          else mkOutOfStoreSymlink "${cfg.path}/${rootRelpath}/${fileRelpath}";
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
