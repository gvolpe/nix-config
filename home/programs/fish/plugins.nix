{ lib, pkgs }:

let
  bobthefish = {
    name = "theme-bobthefish";
    src = pkgs.fish-bobthefish-theme;
  };

  keytool-completions = {
    name = "keytool-completions";
    src = pkgs.fish-keytool-completions;
  };
in
{
  completions = {
    keytool = lib.readFile "${keytool-completions.src}/completions/keytool.fish";
  };

  theme = bobthefish;
  prompt = lib.readFile "${bobthefish.src}/fish_prompt.fish";
}
