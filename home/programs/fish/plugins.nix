{ pkgs }:

let
  bobthefish = {
    name = "theme-bobthefish";
    src  = pkgs.fetchFromGitHub {
      owner  = "gvolpe";
      repo   = "theme-bobthefish";
      rev    = "d3df6f516e351e12fbb455bcf310640695cb2662";
      sha256 = "1nvazfyz7vk37nska7yw94kknv8jbsqwzz3ybwbnhsnf98xlir0r";
    };
  };
in
{
  theme  = bobthefish;
  prompt = builtins.readFile "${bobthefish.src.out}/fish_prompt.fish";
}
