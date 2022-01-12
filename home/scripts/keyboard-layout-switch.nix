{ pkgs, ...}:

let
  xkbmap = "${pkgs.xorg.setxkbmap}/bin/setxkbmap";
  rg     = "${pkgs.ripgrep}/bin/rg";
in
  pkgs.writeShellScriptBin "kls" ''
    layout=$(${xkbmap} -query | ${rg} layout)

    if [[ $layout == *"us-custom"* ]]; then
      ${xkbmap} -layout es
    elif [[ $layout == *"es"* ]]; then
      ${xkbmap} -layout pl
    else
      ${xkbmap} -layout us-custom
    fi
  ''
