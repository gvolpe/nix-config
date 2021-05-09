{ pkgs }:

pkgs.neovim-unwrapped.overrideAttrs (
  old: {
    name    = "neovim-nightly-2021-05-08";
    version = "v0.5.0-dev+1309-g516551319";

    src = pkgs.fetchFromGitHub {
      owner  = "neovim";
      repo   = "neovim";
      rev    = "5165513198dbbe7415f1b8c51225ae8f822628ec";
      sha256 = "0qi9fn2g28g92p6y3gw8mryb41cmxz1sr5m54hl2zb7mq8na4vpg";
    };

    buildInputs = old.buildInputs ++ [ pkgs.tree-sitter ];
  }
)
