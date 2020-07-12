{ pkgs }:

# nightly release from Jul 12
pkgs.neovim-unwrapped.overrideAttrs (
  old: {
    version = "v0.5.0-593-g1ca67a73c";
    src = pkgs.fetchFromGitHub {
      owner  = "neovim";
      repo   = "neovim";
      rev    = "1ca67a73c0ba680eb8328e68bea31f839855dd29";
      sha256 = "1568d5xmpnndxfclcjc05i4dp5480fp7qq0jqajz9n0xlrcfsmn2";
    };
  }
)
