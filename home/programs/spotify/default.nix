{ opts, pkgs, ...}:

let
  spotify = "${pkgs.spotify}/bin/spotify";
in
  pkgs.writeShellScriptBin "myspotify" ''${spotify} ${opts}''
