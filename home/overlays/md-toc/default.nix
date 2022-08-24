{ gh-md-toc }:

self: super:

{
  md-toc = self.stdenv.mkDerivation {
    name = "gh-md-toc-24-08-2022";

    src = gh-md-toc;

    phases = [ "installPhase" "patchPhase" ];

    installPhase = ''
      mkdir -p $out/bin
      cp $src/gh-md-toc $out/bin/md-toc
      chmod +x $out/bin/md-toc
    '';
  };
}
