{ lib, grim, satty, slurp, writeShellScriptBin }:

{
  # screenshot tooling script: https://github.com/gabm/satty?tab=readme-ov-file#wlroots-based-compositors-sway-hyprland-wayfire-river-
  satty = writeShellScriptBin "satty-shot" ''
    ${lib.exe grim} -g "$(${lib.exe slurp} -o -r -c '#ff0000ff')" -t ppm - | ${lib.exe satty} --filename - --fullscreen --output-filename ~/Pictures/Screenshots/satty-$(date '+%Y%m%d-%H:%M:%S').png
  '';

  # idea adapted from https://github.com/YaLTeR/niri/discussions/329
  scratchpad = writeShellScriptBin "scratchpad-sh" ''
    SCRATCH_WIN_NAME=$1

    workId=$(niri msg -j workspaces | jq '.[] | select(.is_focused == true)' | jq .idx)
    id=$(niri msg -j windows | jq ".[] | select(.app_id == \"$SCRATCH_WIN_NAME\")" | jq .id)

    if [[ -z $id ]]; then
      notify-send -u critical 'Scratchpad Error' "There is no window id for: $SCRATCH_WIN_NAME" -t 5000
      exit 1
    fi

    is_win_focused=$(niri msg -j windows | jq ".[] | select(.app_id == \"$SCRATCH_WIN_NAME\")" | jq .is_focused)

    if [[ $is_win_focused == "false" ]]; then
      niri msg action move-window-to-workspace --window-id $id $workId
      niri msg action move-window-to-floating --id $id
      niri msg action focus-window --id $id
    else
      niri msg action move-window-to-workspace --window-id $id "scratch" --focus=false
      niri msg action move-window-to-tiling --id $id
      niri msg action focus-workspace $workId
    fi
  '';
}
