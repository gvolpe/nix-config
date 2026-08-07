# Overlays with expiration

Blogpost: https://jezenthomas.com/2026/07/nix-overrides-that-expire-themselves/

```nix
final: prev:

let
  inherit (prev) lib;
  version = "2.3.8";
  noOverride = lib.versionAtLeast prev.pyprland.version version;

  finalPackage =
    if noOverride then
      prev.pyprland
    else
      prev.pyprland.overrideAttrs (old: {
        inherit version;
        src = prev.sources.pyprland;
      });

  warning = ''
    pyprland >= ${version} is now in nixpkgs, the override can be removed.
  '';
in
{
  pyprland = lib.warnIf noOverride warning finalPackage;
}
```

Building the Hyprland desktop with this overlay for `pyprland` gives this warning:

```console
$ nix build .#homeConfigurations.hyprland-desktop.activationPackage -L
evaluation warning: pyprland >= 2.3.8 is now in nixpkgs, the override can be removed.
```
