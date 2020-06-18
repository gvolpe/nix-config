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
      rev    = "d3df6f516e351e12fbb455bcf310640695cb2662";
      sha256 = "1nvazfyz7vk37nska7yw94kknv8jbsqwzz3ybwbnhsnf98xlir0r";
    };
  }
]
