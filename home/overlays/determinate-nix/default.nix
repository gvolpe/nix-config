final: prev:

{
  # determinate nix binary as a wrapper to use flake schemas and test other features
  dnix = prev.symlinkJoin {
    name = "dnix";
    nativeBuildInputs = [ prev.makeWrapper ];
    paths = [ prev.sources.determinate-nix.packages.${prev.stdenv.system}.default ];
    postBuild = ''
      mv $out/bin/nix $out/bin/dnix
    '';
  };
}
