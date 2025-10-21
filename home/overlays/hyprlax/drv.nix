{ lib
, stdenv
, pkg-config
, fetchFromGitHub
, wayland
, wayland-protocols
, wayland-scanner
, mesa
, libGL
}:

let
  version = "2.1.2";
in
stdenv.mkDerivation {
  name = "hyprlax";

  src = fetchFromGitHub {
    owner = "sandwichfarm";
    repo = "hyprlax";
    tag = "v${version}";
    hash = "sha256-j4+sS7zpoMPTRy2fmCpOISjs+7RKykvDma1wmoCkSBQ=";
  };

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
