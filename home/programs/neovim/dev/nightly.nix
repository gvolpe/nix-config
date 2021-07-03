{ pkgs }:

pkgs.neovim-unwrapped.overrideAttrs (
  old: {
    name    = "neovim-5.0.0";
    version = "v0.5.0";

    src = pkgs.fetchFromGitHub {
      owner  = "neovim";
      repo   = "neovim";
      rev    = "a5ac2f45ff84a688a09479f357a9909d5b914294";
      sha256 = "0lgbf90sbachdag1zm9pmnlbn35964l3khs27qy4462qzpqyi9fi";
    };

    buildInputs = old.buildInputs ++ [ pkgs.tree-sitter ];
  }
)
