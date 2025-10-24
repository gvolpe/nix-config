self: super:

{
  pyprland = super.pyprland.overrideAttrs (old: {
    version = "2.3.8";
    src = super.sources.pyprland;
  });
}
