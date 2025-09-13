{ lib, grim, satty, slurp, writeShellScriptBin }:

{
  # screenshot tooling script: https://github.com/gabm/satty?tab=readme-ov-file#wlroots-based-compositors-sway-hyprland-wayfire-river
  satty = writeShellScriptBin "satty-shot" ''
    FILENAME=~/Pictures/Screenshots/satty-$(date '+%Y%m%d-%H:%M:%S').png
    ${lib.exe grim} -g "$(${lib.exe slurp} -o -r -c '#ff0000ff')" -t ppm - | \
      ${lib.exe satty} --filename - --fullscreen --output-filename $FILENAME
  '';
}
