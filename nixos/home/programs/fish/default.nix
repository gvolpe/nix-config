{ config, pkgs, lib, ... }:

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
    #functions = fishFunctions; # not working yet...
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

  xdg.configFile."fish/functions/__bobthefish_prompt_nix.fish".source = ./functions/__bobthefish_prompt_nix.fish;

  # Hard-coded path where the theme is sourced. Is there a better way to do it?
  home.activation.fish = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    ln -sf /nix/store/jnz46wgfh3k7mxmiwfjgxy6gzb2gd45z-source/fish_prompt.fish ~/.config/fish/functions/
  '';
}
