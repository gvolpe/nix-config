{ config, pkgs, ... }:

let
  tmuxConfig = "";
in
{
  programs.tmux = {
    enable = true;
    extraConfig = tmuxConfig;
    keyMode = "vi";
    plugins = with pkgs; [
      tmuxPlugins.cpu
      tmuxPlugins.nordTmux
      {
        plugin = tmuxPlugins.resurrect;
        extraConfig = "set -g @resurrect-strategy-nvim 'session'";
      }
      {
        plugin = tmuxPlugins.continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '60' # minutes
        '';
      }
    ];
    shortcut = "a";
  };
}
