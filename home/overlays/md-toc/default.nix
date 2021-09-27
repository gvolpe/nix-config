self: super:

{
  md-toc = self.stdenv.mkDerivation {
    name = "gh-md-toc-24-05-2020";

    src = builtins.fetchurl {
      url    = "https://raw.githubusercontent.com/ekalinin/github-markdown-toc/488f310064b16c1eb9c17862cc5844189ee65955/gh-md-toc";
      sha256 = "1253n0qw3xgikl7gcdicg3vmc3wzz6122bmhmffj1irrachq89fi";
    };

    phases = ["installPhase" "patchPhase"];

    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/md-toc
      chmod +x $out/bin/md-toc
    '';
  };
}
