{ grim, jq, niri, satty, slurp, writeShellApplication }:

{
  # screenshot tooling script: https://github.com/gabm/satty?tab=readme-ov-file#wlroots-based-compositors-sway-hyprland-wayfire-river
  satty = writeShellApplication {
    name = "satty-shot";
    runtimeInputs = [ grim satty slurp ];
    text = ''
      FILENAME=~/Pictures/Screenshots/satty-$(date '+%Y%m%d-%H:%M:%S').png
      grim -g "$(slurp -o -r -c '#ff0000ff')" -t ppm - | \
        satty --filename - --fullscreen --output-filename "$FILENAME"
    '';
  };

  # When a niri session starts, move the selected scratchpad windows that have been spawned
  # to a specific XY location in the display and resize them accordingly.
  niri-scratchpad-init = writeShellApplication {
    name = "niri-scratchpad-init";
    runtimeInputs = [ jq niri ];
    text = ''
      windows=$(niri msg -j windows)
      nemo_window=$(echo "$windows" | jq '.[] | select(.app_id == "nemo")' | jq .id)
      spotify_window=$(echo "$windows" | jq '.[] | select(.app_id == "spotify")' | jq .id)

      niri msg action move-window-to-floating --id "$nemo_window"
      niri msg action set-window-width --id "$nemo_window" 1157
      niri msg action set-window-height --id "$nemo_window" 736
      niri msg action move-floating-window --id "$nemo_window" -x 664 -y 364

      niri msg action move-window-to-floating --id "$spotify_window"
      niri msg action set-window-width --id "$spotify_window" 1596
      niri msg action set-window-height --id "$spotify_window" 1240
      niri msg action move-floating-window --id "$spotify_window" -x 443 -y 93
    '';
  };
}
