{ pkgs }:

pkgs.neovim-unwrapped.overrideAttrs (
  old: {
    # newer versions break coc.nvim (there's a fix in master but idk how to override it)
    name    = "neovim-nightly-2021-05-07";
    version = "v0.5.0-dev+1309-g516551319";

    src = pkgs.fetchFromGitHub {
      owner  = "neovim";
      repo   = "neovim";
      rev    = "4be0e92db01a502863ac4bb26dd0fee16d833145";
      sha256 = "1h3sinb52xi4m94dv3dwlzjyr8mzz8v0ndgrkzgi3c1nwmf6b0f2";
    };

    buildInputs = old.buildInputs ++ [ pkgs.tree-sitter ];
  }
)
