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
  hooks = ''
    eval (direnv hook fish)
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
    shellInit = fzfConfig + themeConfig + hooks;
  };

  # Hack to get the prompt of the sourced theme. Is there a better way to do this?
  home.activation.fish = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    prompt=$(find /nix/store -maxdepth 2 -name 'fish_prompt.fish' | tail -1)
    mkdir -p ~/.config/fish/functions/
    ln -sf $prompt ~/.config/fish/functions/
  '';
}
