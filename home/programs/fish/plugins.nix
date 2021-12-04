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

  keytool-completions = {
    name = "keytool-completions";
    src  = pkgs.fetchFromGitHub {
      owner  = "ckipp01";
      repo   = "keytool-fish-completions";
      rev    = "dcb24bae7b8437e1e1210b00b7172841a26d6573";
      sha256 = "0581z6mzi6wjfqm4hcbl9w2haq3zfa5p1jgql5y7q2jwsn1lyzvr";
    };
  };
in
{
  completions = {
    keytool = builtins.readFile "${keytool-completions.src.out}/completions/keytool.fish";
  };

  theme   = bobthefish;
  prompt  = builtins.readFile "${bobthefish.src.out}/fish_prompt.fish";
}
