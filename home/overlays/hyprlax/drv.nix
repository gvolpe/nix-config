{ lib
, stdenv
, pkg-config
, wayland
, wayland-protocols
, wayland-scanner
, mesa
, libGL
, xargs
}:

stdenv.mkDerivation {
  name = "hyprlax";
  version = "v2.2.0";

  src = xargs.hyprlax-src;

  nativeBuildInputs = [
    pkg-config
    wayland-scanner
  ];

  buildInputs = [
    wayland
    wayland-protocols
    mesa
    libGL
  ];

  installFlags = [ "PREFIX=$(out)" ];

  meta = {
    description = "Smooth parallax wallpaper animations for Hyprland";
    homepage = "https://github.com/sandwichfarm/hyprlax";
    license = lib.licenses.mit;
    maintainers = [ ];
    platforms = lib.platforms.linux;
  };
}
