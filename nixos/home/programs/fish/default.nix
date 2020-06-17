{ config, pkgs, ... }:

let
  fishFunctions = builtins.readFile ./functions.nix;
  fishPlugins   = builtins.readFile ./plugins.nix;

  fzfConfig = ''
    # FZF config
    set -x FZF_DEFAULT_OPTS "--preview='bat {} --color=always'"
    set -x SKIM_DEFAULT_COMMAND "rg --files || fd || find ."
  '';
in
{
  programs.fish = {
    enable = true;
    extraConfig = fzfConfig;
    functions = fishFunctions;
    plugins = fishPlugins;
    shellAliases = {
      cat = bat;
      du = "ncdu --color dark -rr -x";
      ls = exa;
      ll = "ls -a";
      ".." = "cd ..";
    };
  };
}
