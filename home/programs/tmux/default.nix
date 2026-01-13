{ pkgs, specialArgs, ... }:

let
  nord = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "nord";
    src = specialArgs.nord-tmux;
    version = specialArgs.nord-tmux.rev;
  };
in
{
  programs.tmux = {
    enable = true;
    aggressiveResize = true;
    baseIndex = 1;
    extraConfig = ''
      # automatically renumber tmux windows
      set -g renumber-windows on

      # Activity Monitoring
      setw -g monitor-activity off
      set -g visual-activity off
    '';
    escapeTime = 0;
    keyMode = "vi";
    plugins = with pkgs.tmuxPlugins; [
      cpu
      nord # theme
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
    shortcut = "b";
    terminal = "screen-256color";
  };
}
