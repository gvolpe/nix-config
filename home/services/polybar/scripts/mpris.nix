{ pkgs, ... }:

let
  pctl = "${pkgs.playerctl}/bin/playerctl";
  zsc = "${pkgs.zscroll}/bin/zscroll";

  format = ''
    {{ artist }} - {{ title }} (from ‟{{ album }}”) - Duration: {{ duration(mpris:length) }}
  '';

  metadata = pkgs.writeShellScriptBin "spotify_metadata" ''
    ${pctl} --player=spotify,%any metadata --format '${format}'
  '';

  status = pkgs.writeShellScriptBin "spotify_status" ''
    ${pctl} --player=spotify status
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
