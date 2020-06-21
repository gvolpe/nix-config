{ config, pkgs, ... }:

let
  buildTmuxPlugin = pkgs.tmuxPlugins.mkDerivation;
in
{
  nord = buildTmuxPlugin {
    pluginName = "nord";
    version = "v0.3.0";
    src = builtins.fetchTarball {
      name   = "Nord-Tmux-v0.3.0";
      url    = "https://github.com/arcticicestudio/nord-tmux/archive/v0.3.0.tar.gz";
      sha256 = "14xhh49izvjw4ycwq5gx4if7a0bcnvgsf3irywc3qps6jjcf5ymk";
    };
  };
}
