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
      owner  = "oh-my-fish";
      repo   = "theme-bobthefish";
      rev    = "a2ad38aa051aaed25ae3bd6129986e7f27d42d7b";
      sha256 = "1fssb5bqd2d7856gsylf93d28n3rw4rlqkhbg120j5ng27c7v7lq";
    };
  }
]
