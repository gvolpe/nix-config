{ pkgs, ... }:

let
  firefox = "${pkgs.firefox-beta-bin}/bin/firefox";
in
  pkgs.writeShellScriptBin "work-browser" ''${firefox} -p "demo" -new-tab $1''
