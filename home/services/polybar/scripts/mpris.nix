{ pkgs, ...}:

let
  pctl = "${pkgs.playerctl}/bin/playerctl";
in
  pkgs.writeShellScriptBin "mpris" ''
    echo $(${pctl} metadata --format '{{ artist }} - {{ title }}')
  ''
