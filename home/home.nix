{ config, lib, pkgs, stdenv, ... }:

let
  dconf2nix = pkgs.callPackage ./programs/dconf2nix/default.nix {};
  gnomePkgs = (pkgs.callPackage ./programs/gnome/default.nix {}).gnomePkgs;
  nix-doc   = pkgs.callPackage ./programs/nix-doc/default.nix {};

  defaultPkgs = with pkgs; [
    any-nix-shell  # fish support for nix shell
    asciinema      # record the terminal
    cachix         # nix caching
    dconf2nix      # dconf (gnome) files to nix converter
    docker-compose # docker manager
    exa            # a better `ls`
    fd             # "find" for files
    hyperfine      # command-line benchmarking tool
    insomnia       # rest client with graphql support
    k9s            # k8s pods manager
    ncdu           # disk space info (a better du)
    nix-doc        # nix documentation search tool
    prettyping     # a nicer ping
    ripgrep        # fast grep
    rnix-lsp       # nix lsp server
    slack          # messaging client
    spotify        # music source
    tdesktop       # telegram messaging client
    terminator     # great terminal multiplexer
    tldr           # summary of a man page
    tree           # display files in a tree view
    xclip          # clipboard support (also for neovim)
    ytop           # alternative to top and htop

    # fixes the `ar` error required by cabal
    binutils-unwrapped
  ];

  gitPkgs = with pkgs.gitAndTools; [
    diff-so-fancy # git diff with colors
    tig           # diff and commit view
  ];

  haskellPkgs = with pkgs.haskellPackages; [
    brittany      # code formatter
    cabal2nix     # convert cabal projects to nix
    cabal-install # package manager
    ghc           # compiler
    ghcide        # haskell IDE
    hoogle        # documentation
    nix-tree      # visualize nix dependencies
  ];

in
{
  programs.home-manager.enable = true;

  nixpkgs.overlays = [
    (import ./overlays/vim-plugins.nix)
  ];

  imports = [
    ./programs/git/default.nix
    ./programs/gnome/dconf.nix
    ./programs/fish/default.nix
    ./programs/neovim/default.nix
    ./programs/sbt/default.nix
    ./programs/terminator/default.nix
    ./programs/tmux/default.nix
  ];

  xdg.enable = true;

  home = {
    username      = "gvolpe";
    homeDirectory = "/home/gvolpe";
    stateVersion  = "20.09";

    packages = defaultPkgs ++ gitPkgs ++ gnomePkgs ++ haskellPkgs;

    sessionVariables = {
      EDITOR = "nvim";
    };
  };

  # notifications about home-manager news
  news.display = "silent";

  programs = {

    bat = {
      enable = true;
    };

    chromium = {
      enable = true;
      extensions = [
        "kklailfgofogmmdlhgmjgenehkjoioip" # google meet grid view
        "aapbdbdomjkkjkaonfhkkikfgjllcleb" # google translate
        "hdokiejnpimakedhajhdlcegeplioahd" # lastpass password manager
        "hkgfoiooedgoejojocmhlaklaeopbecg" # picture-in-picture
        "dbepggeogbaibhgnhhndojpepiihcmeb" # vimium
      ];
    };

    direnv = {
      enable = true;
      enableFishIntegration = true;
      enableNixDirenvIntegration = true;
    };

    fzf = {
      enable = true;
      enableFishIntegration = true;
    };

    gnome-terminal = {
      enable = false;
    };

    gpg = {
      enable = true;
    };

    htop = {
      enable = true;
      sortDescending = true;
      sortKey = "PERCENT_CPU";
    };

    jq = {
      enable = true;
    };

    ssh = {
      enable = true;
    };

  };

  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800;
      enableSshSupport = true;
    };
  };

}
