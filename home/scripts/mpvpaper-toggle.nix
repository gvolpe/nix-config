{ coreutils, gawk, mpvpaper, procps, writeShellScriptBin }:

writeShellScriptBin "mpvpaper-toggle" ''
  set -euo pipefail

  wallpaper="$HOME/Pictures/wallpapers/hollow-knight/glowing-grove.mp4"
  user_name="$(${coreutils}/bin/id -un)"

  mapfile -t pids < <(
    ${procps}/bin/ps -ww -eo pid=,user=,args= |
      ${gawk}/bin/awk -v user="$user_name" -v wallpaper="$wallpaper" '
        $2 == user && $3 ~ /(^|\/)mpvpaper$/ && index($0, wallpaper) { print $1 }
      '
  )

  if (( ''${#pids[@]} > 0 )); then
    kill "''${pids[@]}"
    exit 0
  fi

  exec ${mpvpaper}/bin/mpvpaper -vp -o 'no-audio loop' ALL "$wallpaper"
''
