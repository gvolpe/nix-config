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
    version = "6.3.11.7212";

    src = super.fetchurl {
      url = "https://zoom.us/client/${version}/zoom_x86_64.pkg.tar.xz";
      hash = "sha256-wSXb2v2qXoLXctmjOZpL0SiOP8+ySwpTDpJmPrfQQco=";
    };
  });
}
