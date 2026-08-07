{ coreutils
, gawk
, hyprlax
, mpvpaper
, procps
, writeShellScriptBin
}:

let
  killWallpapers = ''
    user_name="$(${coreutils}/bin/id -un)"
    current_backend_running=false

    mapfile -t pids < <(
      ${procps}/bin/ps -ww -eo pid=,user=,args= |
        ${gawk}/bin/awk -v user="$user_name" -v backend="$backend" '
          $2 == user && $3 ~ /(^|\/)(hyprlax|mpvpaper)$/ {
            name = $3
            sub(/^.*\//, "", name)
            print $1, name == backend
          }
        '
    )

    if (( ''${#pids[@]} > 0 )); then
      kill_pids=()
      for process in "''${pids[@]}"; do
        read -r pid is_current_backend <<< "$process"
        kill_pids+=("$pid")
        if [[ "$is_current_backend" == "1" ]]; then
          current_backend_running=true
        fi
      done

      kill "''${kill_pids[@]}" 2>/dev/null || true

      if $current_backend_running; then
        exit 0
      fi
    fi
  '';

  hyprlax-toggle = writeShellScriptBin "hyprlax-toggle" ''
    set -euo pipefail
    backend="hyprlax"
    wallpaper="$HOME/Pictures/wallpapers/4k-sci-fi/921318.jpg"
    ${killWallpapers}
    exec ${hyprlax}/bin/hyprlax "$wallpaper"
  '';

  mpvpaper-toggle = writeShellScriptBin "mpvpaper-toggle" ''
    set -euo pipefail
    backend="mpvpaper"
    wallpaper="$HOME/Pictures/wallpapers/hollow-knight/glowing-grove.mp4"
    ${killWallpapers}
    exec ${mpvpaper}/bin/mpvpaper -vp -o 'no-audio loop' ALL "$wallpaper"
  '';
in
{
  inherit hyprlax-toggle mpvpaper-toggle;
}
