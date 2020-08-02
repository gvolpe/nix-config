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
    plugins = [ fishPlugins.theme ];
    promptInit = ''
      eval (direnv hook fish)
      any-nix-shell fish --info-right | source
    '';
    shellAliases = {
      cat  = "bat";
      du   = "ncdu --color dark -rr -x";
      ls   = "exa";
      ll   = "ls -a";
      ".." = "cd ..";
      ping = "prettyping";
    };
    shellInit = fzfConfig + themeConfig + "set fish_greeting";
  };

  xdg.configFile."fish/functions/fish_prompt.fish".text = fishPlugins.prompt;
}
