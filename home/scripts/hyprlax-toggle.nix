{ coreutils, gawk, hyprlax, procps, writeShellScriptBin }:

writeShellScriptBin "hyprlax-toggle" ''
  set -euo pipefail

  wallpaper="$HOME/Pictures/wallpapers/4k-sci-fi/921318.jpg"
  user_name="$(${coreutils}/bin/id -un)"

  mapfile -t pids < <(
    ${procps}/bin/ps -ww -eo pid=,user=,args= |
      ${gawk}/bin/awk -v user="$user_name" -v wallpaper="$wallpaper" '
        $2 == user && $3 ~ /(^|\/)hyprlax$/ && index($0, wallpaper) { print $1 }
      '
  )

  if (( ''${#pids[@]} > 0 )); then
    kill "''${pids[@]}"
    exit 0
  fi

  exec ${hyprlax}/bin/hyprlax "$wallpaper"
''
