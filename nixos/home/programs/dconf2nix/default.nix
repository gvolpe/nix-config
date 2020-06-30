{ stdenv, ... }:

let
  version = "v0.0.1";
in
  stdenv.mkDerivation {
    name = "dconf2nix";

    src = builtins.fetchurl {
      url    = "https://github.com/gvolpe/dconf2nix/releases/download/${version}/dconf2nix-linux-x86-64";
      sha256 = "00icz1nslj6a7cxfx2fpcpvrg6sczcnqfy1pr1585477l2n6d0sn";
    };

    phases = ["installPhase" "patchPhase"];

    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/dconf2nix
      chmod +x $out/bin/dconf2nix
    '';
  }
