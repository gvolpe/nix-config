{ pkgs, specialArgs, ... }:

let
  md-toc = pkgs.stdenv.mkDerivation
    {
      name = "gh-md-toc-24-08-2022";

      src = specialArgs.gh-md-toc;

      phases = [ "installPhase" "patchPhase" ];

      installPhase = ''
        mkdir -p $out/bin
        cp $src/gh-md-toc $out/bin/md-toc
        chmod +x $out/bin/md-toc
      '';
    };
in
{
  # generate ToC in markdown files
  home.packages = [ md-toc ];
}
