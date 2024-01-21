{ pkgs, ... }:

let
  pctl = "${pkgs.playerctl}/bin/playerctl";
  zsc = "${pkgs.zscroll}/bin/zscroll";

  format = ''
    {{ artist }} - {{ title }} (from ‟{{ album }}”) - Duration: {{ duration(mpris:length) }}
  '';

  status = pkgs.writeShellScriptBin "spotify_status" ''
    ${pctl} --player=spotify,%any metadata --format '${format}'
  '';
in
# Credits to https://github.com/PrayagS/polybar-spotify for the scrolling idea
pkgs.writeShellScriptBin "mpris" ''
  ${zsc} -l 40 \
         --delay 0.3 \
         --scroll-padding "  " \
         --match-command "${status}/bin/spotify_status" \
         --update-check true "${status}/bin/spotify_status" &

  wait
''
