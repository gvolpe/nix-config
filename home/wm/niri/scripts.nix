{ lib, grim, satty, slurp, writeShellScriptBin }:

{
  # screenshot tooling script: https://github.com/gabm/satty?tab=readme-ov-file#wlroots-based-compositors-sway-hyprland-wayfire-river
  satty = writeShellScriptBin "satty-shot" ''
    FILENAME=~/Pictures/Screenshots/satty-$(date '+%Y%m%d-%H:%M:%S').png
    ${lib.exe grim} -g "$(${lib.exe slurp} -o -r -c '#ff0000ff')" -t ppm - | \
      ${lib.exe satty} --filename - --fullscreen --output-filename $FILENAME
  '';

  # idea adapted from https://github.com/YaLTeR/niri/discussions/329
  scratchpad = writeShellScriptBin "scratchpad-sh" ''
    SCRATCH_WIN_NAME=$1

    windows=$(niri msg -j windows | jq ".[] | select(.app_id == \"$SCRATCH_WIN_NAME\")")

    win_id=$(echo $windows | jq .id)

    if [[ -z $win_id ]]; then
      notify-send -u critical 'Scratchpad Error' "There is no window id for: $SCRATCH_WIN_NAME" -t 5000
      exit 1
    fi

    moveWindowToScratchpad() {
      niri msg action move-window-to-workspace --window-id $win_id "scratch" --focus=false
      niri msg action move-window-to-tiling --id $win_id
    }

    is_win_focused=$(echo $windows | jq .is_focused)

    if [[ $is_win_focused == "false" ]]; then
      work_id=$(niri msg -j workspaces | jq '.[] | select(.is_focused == true)' | jq .idx)
      win_work_id=$(echo $windows | jq .workspace_id)

      if [[ $win_work_id == $work_id ]]; then
        moveWindowToScratchpad
      else
        niri msg action move-window-to-workspace --window-id $win_id $work_id
        niri msg action move-window-to-floating --id $win_id
        niri msg action focus-window --id $win_id
      fi
    else
      moveWindowToScratchpad
    fi
  '';
}
