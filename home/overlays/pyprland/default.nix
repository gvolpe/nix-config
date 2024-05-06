self: super:

let
  #version = "2.2.16";
  version = "3d58a8d5edff6ee2734618466a8ad4c3a95ec3f9";
in
{
  pyprland = super.pyprland.overrideAttrs (old: {
    inherit version;

    src = super.fetchFromGitHub {
      owner = "hyprland-community";
      repo = "pyprland";
      rev = version;
      #hash = "sha256-zT+ixOM+by13iM78CHkQqTS9LCLFspHNyEjd7P2psUE=";
      hash = "sha256-MEdZaRLh71HDRl+jZHjbHsTcocBamjJhsXkgfplIzYA=";
    };

    # workaround for this issue: https://github.com/hyprland-community/pyprland/issues/96
    prePatch = (old.prePatch or "") + ''
      substituteInPlace pyprland/plugins/pyprland.py \
        --replace "await self.notify_error(\"Error: 'hyprctl version': incorrect JSON data\")" ""
    '';
  });
}
