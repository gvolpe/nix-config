let
  pkgs = import <nixpkgs> {};
  fixed = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") {};
in
(
  with fixed; [
    nodejs # required by coc.nvim (autocompletion plugin)
    openjdk8 # Java development kit
    python3 # for vim plugins
  ]
) ++ (
  with pkgs; [
    bat # a better `cat`
    brave # web browser
    bloop # Scala build server
    cachix # nix caching
    direnv # load and unload environments on demand
    exa # a better `ls`
    fd # "find" for files
    fzf # fuzzy find tool
    git # source version control
    gitui # terminal gui for git
    glow # markdown viewer
    htop # interactive processes viewer
    ncdu # disk space info (a better du)
    neovim # best text editor ever
    ngrok # secure tunnels to localhost
    niv # nix package manager
    nix # nix commands
    nix-direnv # nix shell replacement (integrated with direnv)
    prettyping # a nicer ping
    ripgrep # fast grep
    sbt # scala build tool
    spotify # music source
    tldr # summary of a man page
    tmux # terminal multiplexer and sessions
    tree # display files in a tree view
  ]
) ++ (
  with pkgs.gitAndTools; [
    diff-so-fancy # git diff with colors
    tig # diff and commit view
  ]
) ++ (
  with pkgs.haskellPackages; [
    brittany # code formatter
    cabal2nix # convert cabal projects to nix
    cabal-install # package manager
    ghc # compiler
    ghcide # haskell IDE
    hoogle # documentation
  ]
) ++ (
  with pkgs.python3Packages; [
    pynvim # for vim plugins that require python
  ]
) ++ (
  with pkgs.nodePackages; [
    node2nix # to convert node packages
  ]
)
