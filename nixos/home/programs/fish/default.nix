{ config, pkgs, lib, ... }:

let
  fishPlugins = pkgs.callPackage ./plugins.nix {};
  fzfConfig = ''
    set -x FZF_DEFAULT_OPTS "--preview='bat {} --color=always'" \n
    set -x SKIM_DEFAULT_COMMAND "rg --files || fd || find ."
  '';
  themeConfig = ''
    set -g theme_display_date no
    set -g theme_nerd_fonts yes
    set -g theme_display_git_master_branch no
    set -g theme_nerd_fonts yes
    set -g theme_newline_cursor yes
    set -g theme_color_scheme solarized
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
    shellInit = fzfConfig + themeConfig;
  };

  # Hard-coded path where the theme is sourced. Is there a better way to do it?
  # Find it by grepping the nix store: rg 'gvolpe/theme-bobthefish' /nix/store/
  home.activation.fish = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    ln -sf /nix/store/fkp03ghyl8kgbfyvwaxijibpvkzfnj7f-source/fish_prompt.fish ~/.config/fish/functions/
  '';
}
