self: super:

{
  hyprland = super.hyprland.overrideAttrs (old: {
    # see: https://github.com/hyprwm/Hyprland/issues/9518
    version = "0.46.2";
    src = super.sources.hyprland;
  });
}
