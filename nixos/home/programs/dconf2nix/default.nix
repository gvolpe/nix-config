{ stdenv, ... }:

let
  version = "v0.0.2";
in
  stdenv.mkDerivation {
    name = "dconf2nix-${version}";

    src = builtins.fetchurl {
      url    = "https://github.com/gvolpe/dconf2nix/releases/download/${version}/dconf2nix-linux-x86-64";
      sha256 = "135xl48aiqlbbcn2q95aj50p2bzfyk87h4jzbnk81qaak5043krp";
    };

    phases = ["installPhase" "patchPhase"];

    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/dconf2nix
      chmod +x $out/bin/dconf2nix
    '';
  }
