{ config, pkgs, ... }:

let
  fishFunctions = pkgs.callPackage ./functions.nix {};
  fishPlugins   = pkgs.callPackage ./plugins.nix {};

  fzfConfig = ''
    set -x FZF_DEFAULT_OPTS "--preview='bat {} --color=always'" \n
    set -x SKIM_DEFAULT_COMMAND "rg --files || fd || find ."
  '';
in
{
  programs.fish = {
    enable = true;
    #functions = fishFunctions;
    plugins = fishPlugins;
    promptInit = "";
    shellAliases = {
      cat  = "bat";
      du   = "ncdu --color dark -rr -x";
      ls   = "exa";
      ll   = "ls -a";
      ".." = "cd ..";
    };
    shellInit = fzfConfig;
  };
}
