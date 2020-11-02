{ config, lib, pkgs, stdenv, ... }:

let
  gnomePkgs = (pkgs.callPackage ./programs/gnome/default.nix {}).gnomePkgs;
  unstable  = import (import ./unstable.nix) {};

  unstablePkgs = with unstable; [
    betterlockscreen # fast lockscreen based on i3lock
    brave            # private web browser based on chromium
    compton          # composite manager for X11
    dmenu            # application launcher
    killall          # kill processes by name
    manix            # documentation searcher for nix
    mupdf            # pdf viewer with vim-like keybindings
    taffybar         # awesome status bar
  ];

  defaultPkgs = with pkgs; [
    any-nix-shell  # fish support for nix shell
    asciinema      # record the terminal
    cachix         # nix caching
    calibre        # e-book reader
    dconf2nix      # dconf (gnome) files to nix converter
    docker-compose # docker manager
    dive           # explore docker layers
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
    hub           # github command-line client
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

  xmonadPkgs = with pkgs; [
    haskellPackages.libmpd # music player daemon
    haskellPackages.xmobar # status bar
    networkmanager_dmenu   # networkmanager on dmenu
    networkmanagerapplet   # networkmanager applet
    nitrogen               # wallpaper manager
    xorg.xrandr            # display manager (X Resize and Rotate protocol)
  ];

in
{
  programs.home-manager.enable = true;

  nixpkgs.overlays = [
    (import ./overlays/dconf2nix.nix)
    (import ./overlays/manix.nix)
    (import ./overlays/taffybar.nix)
    (import ./overlays/vim-plugins.nix)
  ];

  imports = [
    ./programs/browsers/brave.nix
    ./programs/git/default.nix
    ./programs/fish/default.nix
    ./programs/neovim/default.nix
    ./programs/networkmanager/default.nix
    ./programs/taffybar/default.nix
    ./programs/terminator/default.nix
    ./programs/xmonad/default.nix
    ./programs/xmobar/default.nix
  ];

  xdg.enable = true;

  home = {
    username      = "gvolpe";
    homeDirectory = "/home/gvolpe";
    stateVersion  = "20.09";

    packages = defaultPkgs ++ gitPkgs ++ gnomePkgs ++ haskellPkgs ++ xmonadPkgs ++ unstablePkgs;

    sessionVariables = {
      EDITOR = "nvim";
    };
  };

  # notifications about home-manager news
  news.display = "silent";

  # temporarily disable it until there is a fix upstream
  manual = {
    json.enable = false;
    html.enable = false;
    manpages.enable = false;
  };

  programs = {

    bat = {
      enable = true;
    };

    broot = {
      enable = true;
      enableFishIntegration = true;
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
    flameshot.enable = true;

    gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800;
      enableSshSupport = true;
    };

    screen-locker = {
      enable = true;
      inactiveInterval = 60;
      lockCmd = "${pkgs.betterlockscreen}/bin/betterlockscreen -l dim";
    };

    taffybar.enable = true;
  };

}
