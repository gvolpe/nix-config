{ pkgs }:

pkgs.neovim-unwrapped.overrideAttrs (
  old: {
    name    = "neovim-nightly-2021-01-11";
    version = "v0.5.0-dev+1016-g0af5a56e4";

    src = pkgs.fetchFromGitHub {
      owner  = "neovim";
      repo   = "neovim";
      rev    = "0af5a56e47b543c8497eaa71ca8ff6900059d062";
      sha256 = "019dl8w0l2b0bk26irfgnl6dal1pisdwn1qy4b36hvij2y5mad3s";
    };

    buildInputs = old.buildInputs ++ [ pkgs.tree-sitter ];
  }
)
