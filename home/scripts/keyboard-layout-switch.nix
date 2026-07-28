{ writeShellScriptBin, ripgrep, setxkbmap, ... }:

let
  xkbmap = "${setxkbmap}/bin/setxkbmap";
  rg = "${ripgrep}/bin/rg";
in
writeShellScriptBin "kls" ''
  layout=$(${xkbmap} -query | ${rg} layout)

  if [[ $layout == *"us"* ]]; then
    ${xkbmap} -layout es
  elif [[ $layout == *"es"* ]]; then
    ${xkbmap} -layout pl
  else
    ${xkbmap} -layout us
  fi
''
