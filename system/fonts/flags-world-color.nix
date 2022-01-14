{ stdenv, lib }:

stdenv.mkDerivation {
  name = "flags-world-color-0.1";
  src = ./FlagsWorldColor.ttf;

  phases = ["installPhase"];

  installPhase = ''
    install -D $src $out/share/fonts/truetype/FlagsWorldColor.ttf
  '';
}
