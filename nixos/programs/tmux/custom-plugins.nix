{ config, pkgs, ... }:

{
  nord-tmux = pkgs.stdenv.mkDerivation {
    name = "nord-tmux";
    src = builtins.fetchTarball {
      name   = "Nord-Tmux-v0.3.0";
      url    = "https://github.com/arcticicestudio/nord-tmux/archive/v0.3.0.tar.gz";
      sha256 = "14xhh49izvjw4ycwq5gx4if7a0bcnvgsf3irywc3qps6jjcf5ymk";
    };
  };
}
