{ lib, jq, wofi, writeShellScriptBin }:

let
  cmdcolor= "cyan";
  modmasks = {
    "0" = "";
    "1" = "SHIFT+";
    "4" = "CTRL+";
    "5" = "SHIFT+CTRL+";
    "8" = "ALT+";
    "64" = "SUPER+";
    "65" = "SUPER+SHIFT+";
    "68" = "SUPER+CTRL+";
  };
  keycodes = {
    "59" = "Comma";
    "60" = "Dot";
  };
in
writeShellScriptBin "hypr-binds-help" ''
  hyprctl binds -j |
    ${lib.exe jq} -r '
      map({mod:.modmask|tostring,key:.key,code:.keycode|tostring,desc:.description,dp:.dispatcher,arg:.arg,sub:.submap}) |
      map(.mod |= ${builtins.toJSON modmasks} [.]) |
      map(.code |= ${builtins.toJSON keycodes} [.]) |
      sort_by(.mod) | .[] |
      select(.sub == "") |
      "<b>\(.mod)\(if .key == "" then .code else .key end)</b> <i>\(.desc)</i> <span color=\"${cmdcolor}\">\(.dp) \(.arg)</span>"
    ' |
    ${lib.exe wofi} --dmenu -m -i -p "Hypr binds" |
    # extract the command (dispatcher + arg)
    sed -n 's/.*<span color=\"${cmdcolor}\">\(.*\)<\/span>.*/\1/p' |
    # add double quotes to the string so it can be piped to hyprctl dispatch
    sed -e 's/^/"/g' -e 's/$/"/g' |
    xargs -n1 hyprctl dispatch
''
