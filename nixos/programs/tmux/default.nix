{ config, pkgs, ... }:

let
  plugins = pkgs.tmuxPlugins // pkgs.callPackage ./custom-plugins.nix {};
in
{
  programs.tmux = {
    enable = true;
    extraConfig = "";
    keyMode = "vi";
    plugins = with plugins; [
      cpu
      nord-tmux
      {
        plugin = resurrect;
        extraConfig = "set -g @resurrect-strategy-nvim 'session'";
      }
      {
        plugin = continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '60' # minutes
        '';
      }
    ];
    shortcut = "a";
  };
}
