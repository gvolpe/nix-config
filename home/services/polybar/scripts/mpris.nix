{ pkgs, ...}:

let
  pctl = "${pkgs.playerctl}/bin/playerctl";
in
  pkgs.writeShellScriptBin "mpris" ''
    echo $(${pctl} --player=spotify,%any metadata --format '{{ artist }} - {{ title }}')
  ''
