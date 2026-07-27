{ config, lib, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  cfg = config.dotfiles;
in
{
  options = {
    dotfiles = {
      /**
      Correctly link a configuration file in an immutable or mutable fashion,
      depending on the value of ${config.dotfiles.mutable}.

      # Inputs

      `filepath`: the immutable configuration file path to link.
      `rootpath`: the directory where the mutable file would be linked.

      # Type

      ```
      dotfiles.make :: Path -> Path -> (Path | Derivation)
      ```

      # Usage
      :::{.example}

      When `dotfiles.mutable = true`, it returns a symlink derivation:

      ```console
      nix-repl> :b homeConfigurations.niri-hdmi.config.dotfiles.make ./LICENSE ./.

      This derivation produced the following outputs:
        out -> /nix/store/p62qfcfschb9g8fcgah4xvdk88b89mmx-hm_LICENSE

      $ readlink -f /nix/store/p62qfcfschb9g8fcgah4xvdk88b89mmx-hm_LICENSE
      /home/gvolpe/workspace/nix-config/home/LICENSE
      ```

      When `dotfiles.mutable = false`, it returns the direct Nix path literal (which HM copies to the store during activation):

      ```console
      nix-repl> homeConfigurations.niri-hdmi.config.dotfiles.make ./LICENSE ./.
      /home/gvolpe/workspace/nix-config/LICENSE

      nix-repl> cf = homeConfigurations.niri-hdmi.config.dotfiles.make ./LICENSE ./.
      nix-repl> "${cf}"
      "/nix/store/n62mxdkhz8a16za5pm2k5n21blv28lkd-LICENSE"

      readlink -f /nix/store/n62mxdkhz8a16za5pm2k5n21blv28lkd-LICENSE
      /nix/store/n62mxdkhz8a16za5pm2k5n21blv28lkd-LICENSE
      ```

      :::
      */
      make = lib.mkOption {
        default = filepath: rootpath:
          let relpath = lib.path.removePrefix rootpath filepath;
          in if !cfg.mutable then filepath
          else mkOutOfStoreSymlink "${cfg.path}/${relpath}";
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
