self: super:

let
  # downgrading because Zoom sucks -_-
  # see: https://github.com/NixOS/nixpkgs/issues/322970
  version = "6.0.2.4680";
in
{
  # pipewire-zoom = pipewire v1.0.7
  zoom-us-old = (super.zoom-us.override { pipewire = super.pipewire-zoom; }).overrideAttrs (old: {
    inherit version;

    src = super.fetchurl {
      url = "https://zoom.us/client/${version}/zoom_x86_64.pkg.tar.xz";
      hash = "sha256-027oAblhH8EJWRXKIEs9upNvjsSFkA0wxK1t8m8nwj8=";
    };
  });

  zoom-latest = super.zoom-us.overrideAttrs (old: rec {
    version = "v6.4.5.1046";

    src = super.fetchurl {
      url = "https://github.com/gvolpe/zoom-test/releases/download/${version}/zoom_withlog_6.4.5.1046_x86_64.pkg.tar.xz";
      hash = "sha256-VGsW8X6QiZR2qnmE7XcQCdO8sCfRiAwKhE8UCPWtLl0=";
    };
  });
}
