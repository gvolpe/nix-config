{ stdenv }:

stdenv.mkDerivation rec {
  name    = "dconf2nix-${version}";
  version = "v0.0.5";

  src = builtins.fetchurl {
    url    = "https://github.com/gvolpe/dconf2nix/releases/download/${version}/dconf2nix-linux-x86-64";
    sha256 = "1hidf1vwsi8hmmf3vv4n1dvqs5wyk9xhly7bdckxqkqsq2gb44dg";
  };

  phases = ["installPhase" "patchPhase"];

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/dconf2nix
    chmod +x $out/bin/dconf2nix
  '';
}
