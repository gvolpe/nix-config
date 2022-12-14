{ pkgs, ... }:

let
  buildTmuxPlugin = pkgs.tmuxPlugins.mkTmuxPlugin;
in
{
  nord = buildTmuxPlugin {
    pluginName = "nord";
    version = "v0.3.0";
    src = builtins.fetchTarball {
      name   = "Nord-Tmux-2020-08-25";
      url    = "https://github.com/arcticicestudio/nord-tmux/archive/4e2dc2a5065f5e8e67366700f803c733682e8f8c.tar.gz";
      sha256 = "0l97cqbnq31f769jak31ffb7bkf8rrg72w3vd0g3fjpq0717864a";
    };
  };
}
