self: super:

{
  # generate ToC in markdown files
  md-toc = super.stdenv.mkDerivation {
    name = "gh-md-toc-24-08-2022";

    src = super.sources.gh-md-toc;

    phases = [ "installPhase" "patchPhase" ];

    installPhase = ''
      mkdir -p $out/bin
      cp $src/gh-md-toc $out/bin/md-toc
      chmod +x $out/bin/md-toc
    '';
  };
}
