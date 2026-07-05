{ lib
, stdenv
, rustPlatform
, sources
, pkg-config
, glib
, gtk4
, gtk4-layer-shell
, gdk-pixbuf
, graphene
, cairo
, wrapGAppsHook4
}:

let
  toml = fromTOML (builtins.readFile "${sources.waycal}/Cargo.toml");
in
rustPlatform.buildRustPackage {
  pname = "waycal";
  version = toml.package.version;
  src = sources.waycal;

  cargoHash = "sha256-zOOG8vF0d3+X85O6bu0Y5XKNZSjcufKMHXQmZ54jCXw=";

  doCheck = !stdenv.hostPlatform.isDarwin;

  nativeBuildInputs = [
    pkg-config
    wrapGAppsHook4
  ];

  buildInputs = [
    glib
    gtk4
    gtk4-layer-shell
    gdk-pixbuf
    graphene
    cairo
  ];

  meta = with lib; {
    description = "A tiny Waybar calendar popup for wlr-layer-shell compositors";
    homepage = "https://github.com/forrestknight/waycal";
    license = licenses.mit;
    maintainers = with maintainers; [ gvolpe ];
    mainProgram = "waycal";
  };
}
