{ pkgs, ... }:

let
  pctl = "${pkgs.playerctl}/bin/playerctl";
  zsc = "${pkgs.zscroll}/bin/zscroll";

  format = ''
    {{ artist }} - {{ title }} [{{ duration(mpris:length) }}]
  '';

  metadata = pkgs.writeShellScriptBin "spotify_metadata" ''
    meta=$(echo -n $(${pctl} --player=spotify metadata --format '${format}' 2>/dev/null))
    if [[ $meta == "" ]]; then
      echo "Click to open Spotify"
    else
      echo $meta
    fi
  '';

  status = pkgs.writeShellScriptBin "spotify_status" ''
    state=$(echo -n $(${pctl} --player=spotify status 2>/dev/null))
    if [[ $state == "" ]]; then
      echo "Click to open Spotify"
    else
      echo $state
    fi
  '';
in
# Credits to https://github.com/PrayagS/polybar-spotify for the scrolling idea
pkgs.writeShellScriptBin "mpris" ''
  ${zsc} -l 40 \
         --delay 0.3 \
         --scroll-padding "  " \
         --match-command "${status}/bin/spotify_status" \
         --match-text "Playing" "--scroll 1" \
         --match-text "Paused" "--scroll 0" \
         --update-check true "${metadata}/bin/spotify_metadata" &

  wait
''
