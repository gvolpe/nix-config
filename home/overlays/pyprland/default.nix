final: prev:

{
  pyprland = prev.pyprland.overrideAttrs (old: {
    version = "2.3.8";
    src = prev.sources.pyprland;
  });
}
