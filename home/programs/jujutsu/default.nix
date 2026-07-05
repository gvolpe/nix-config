{ config, ... }:

let
  cfg = config.programs.git;
in
{
  programs.jujutsu = {
    enable = true;

    settings = {
      inherit (cfg.settings) user;

      aliases = {
        l = {
          definition = [ "log" "-r" "(main..@):: | (main..@)-" ];
          doc = "Log pending changes";
        };
      };

      git = {
        auto-local-bookmark = true;
        immutable-heads = "none";
      };

      revset-aliases = {
        "trunk()" = "main@origin";
        "immutable_heads()" = "empty()";
      };

      signing = {
        inherit (cfg.signing) key;
        behavior = "own";
        backend = "gpg";
      };
    };
  };
}
