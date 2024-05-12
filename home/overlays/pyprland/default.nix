self: super:

let
  version = "2.2.20";
in
{
  pyprland = super.pyprland.overrideAttrs (old: {
    inherit version;

    src = super.fetchFromGitHub {
      owner = "hyprland-community";
      repo = "pyprland";
      rev = version;
      hash = "sha256-/eJ3emWI2l9jYgD7RX6tGUy6wHHQ25qS6Xd1x1uWZ1w=";
    };

    # workaround for this issue: https://github.com/hyprland-community/pyprland/issues/96
    prePatch = (old.prePatch or "") + ''
      substituteInPlace pyprland/plugins/pyprland.py \
        --replace "await self.notify_error(\"Error: 'hyprctl version': incorrect JSON data\")" ""
    '';
  });
}
