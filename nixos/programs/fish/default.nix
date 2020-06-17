{ config, pkgs, ... }:

let
  fishConfig = "";
in
{
  programs.fish = {
    enable = true;
    extraConfig = fishConfig;
    # functions
    # plugins
    shellAliases = {
      ll = "ls -a";
      ".." = "cd ..";
    };
  };
}
