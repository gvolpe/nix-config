{ grim, satty, slurp, writeShellApplication, ... }:

# screenshot tooling script: https://github.com/gabm/satty?tab=readme-ov-file#wlroots-based-compositors-sway-hyprland-wayfire-river
writeShellApplication {
  name = "satty-shot";
  runtimeInputs = [ grim satty slurp ];
  text = ''
    FILENAME=~/Pictures/Screenshots/satty-$(date '+%Y%m%d-%H:%M:%S').png
    grim -g "$(slurp -o -r -c '#ff0000ff')" -t ppm - | \
      satty --filename - --fullscreen --output-filename "$FILENAME"
  '';
}
