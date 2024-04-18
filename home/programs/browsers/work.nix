{ lib, pkgs, ... }:

pkgs.writeShellScriptBin "work-browser" ''
  ${lib.exe pkgs.firefox-beta-bin} -p "sxm" -new-tab $1
''
