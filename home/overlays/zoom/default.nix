self: super:

let
  # see: https://github.com/NixOS/nixpkgs/issues/322970
  version = "v6.4.5.1046";
in
{
  zoom-latest = super.zoom-us.overrideAttrs (old: {
    src = super.fetchurl {
      url = "https://github.com/gvolpe/zoom-test/releases/download/${version}/zoom_withlog_6.4.5.1046_x86_64.pkg.tar.xz";
      hash = "sha256-VGsW8X6QiZR2qnmE7XcQCdO8sCfRiAwKhE8UCPWtLl0=";
    };
  });
}
