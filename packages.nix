let
  pkgs = import <nixpkgs> {};
  fixed = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz") {};
in
(
  with fixed; [
    brave # web browser
    cachix # nix caching
    git # source version control
    kdiff3 # git diff
    neovim # best text editor ever
    nodejs # required by coc.nvim (autocompletion plugin)
    openjdk8 # Java development kit
    python3 # for vim plugins
    sbt # Scala build tool
  ]
) ++ (
  with pkgs; [
    bloop # Scala build server
    fd # "find" for files
    fzf # fuzzy find tool
    glow # markdown viewer
    metals # Scala LSP server
    niv # nix package manager
    nix # nix commands
    ripgrep # fast grep
  ]
) ++ (
  with pkgs.gitAndTools; [
    diff-so-fancy # git diff with colors
    tig # diff and commit view
  ]
) ++ (
  with pkgs.haskellPackages; [
    brittany # code formatter
    hoogle # documentation
  ]
) ++ (
  with pkgs.python3Packages; [
    pynvim # for vim plugins that require python
  ]
)
