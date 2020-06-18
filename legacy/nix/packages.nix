let
  pkgs = import <nixpkgs> {};
  fixed = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") {};
in
(
  with pkgs.haskellPackages; [
    brittany # code formatter
    cabal2nix # convert cabal projects to nix
    cabal-install # package manager
    ghc # compiler
    ghcide # haskell IDE
    hoogle # documentation
  ]
)
