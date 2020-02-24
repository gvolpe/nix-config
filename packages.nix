let
  pkgs = import <nixpkgs> {};
  fixed = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz") {};
in
(
  with fixed; [
    brave # web browser
    cachix # nix caching
    git # source version control
    neovim # best text editor ever
    openjdk8 # Java development kit
    sbt # Scala build tool    
  ]
) ++ (
  with pkgs; [
    fd # "find" for files
    fzf # fuzzy find tool
    glow # markdown viewer
    niv # nix package manager
    nix # nix commands
    ripgrep # fast grep
  ]
) ++ (
  with pkgs.gitAndTools; [
    diff-so-fancy # git diff with colors
    tig # diff and comit view
  ]
) ++ (
  with pkgs.haskellPackages; [
    brittany # code formatter
    hoogle # documentation
  ]
) 
