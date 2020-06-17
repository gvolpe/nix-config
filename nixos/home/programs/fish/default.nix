{ config, pkgs, lib, ... }:

let
  fishPlugins = pkgs.callPackage ./plugins.nix {};
  fzfConfig = ''
    set -x FZF_DEFAULT_OPTS "--preview='bat {} --color=always'" \n
    set -x SKIM_DEFAULT_COMMAND "rg --files || fd || find ."
  '';
in
{
  programs.fish = {
    enable = true;
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

  # Hard-coded path where the theme is sourced. Is there a better way to do it?
  home.activation.fish = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    ln -sf /nix/store/wi32bbh5jng7cm701c2zkmj0vab2l2fn-source/fish_prompt.fish ~/.config/fish/functions/
  '';
}
