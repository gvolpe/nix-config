{ pkgs }:

pkgs.neovim-unwrapped.overrideAttrs (
  old: {
    name    = "neovim-nightly-2020-12-12";
    version = "v0.5.0-dev+941-g9c56d8e5f";

    src = pkgs.fetchFromGitHub {
      owner  = "neovim";
      repo   = "neovim";
      rev    = "9c56d8e5f792cf770ebe88d9488549b4106b06a9";
      sha256 = "0yhh9yix6jzcp73skavqhhgnkr0mwx0wkw2nd7cqvvy8xb2r59c7";
    };

    buildInputs = old.buildInputs ++ [ pkgs.tree-sitter ];
  }
)
