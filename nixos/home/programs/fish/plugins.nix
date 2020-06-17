{ config, pkgs, ... }:

[
  {
    name = "oh-my-fish";
    src = pkgs.fetchFromGitHub {
      owner  = "oh-my-fish";
      repo   = "oh-my-fish";
      rev    = "4ff9981ac2082e2c06f37ce5c8ac344ee51605ad";
      sha256 = "0r3fc29r816l3fjih6p5fzib25b962y1r3yq33fh3jkl54svmimz";
    };
  }

  {
    name = "theme-bobthefish";
    src = pkgs.fetchFromGitHub {
      owner  = "gvolpe";
      repo   = "theme-bobthefish";
      rev    = "bcd43e5f8818a62ce48f33b4aa380fe1e67bfaf5";
      sha256 = "0sfa96sm1hvfim2zqbxq2b81dvpxz0bihmgd8gycmwfp3wk3331s";
    };
  }
]
