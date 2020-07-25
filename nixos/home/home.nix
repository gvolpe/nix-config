{ config, lib, pkgs, stdenv, ... }:

let
  customGnome3Ext = pkgs.callPackage ./programs/gnome/extensions.nix {};

  dconf2nix = pkgs.callPackage ./programs/dconf2nix/default.nix {};

  defaultPkgs = with pkgs; [
    asciinema      # record the terminal
    cachix         # nix caching
    dconf2nix      # dconf (gnome) files to nix converter
    docker-compose # docker manager
    exa            # a better `ls`
    fd             # "find" for files
    k9s            # k8s pods manager
    ncdu           # disk space info (a better du)
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

    # fixes the `ar` error required by cabal
    binutils-unwrapped
  ];

  gitPkgs = with pkgs; [
    gitAndTools.diff-so-fancy # git diff with colors
    gitAndTools.tig           # diff and commit view
  ];

  gnomePkgs = with pkgs.gnome3; [
    # gnome3 apps
    eog    # image viewer
    evince # pdf reader

    # desktop look & feel
    customGnome3Ext.dash-to-dock
    customGnome3Ext.timepp
    customGnome3Ext.topicons-plus
    pkgs.gnomeExtensions.clipboard-indicator
    pkgs.gnomeExtensions.sound-output-device-chooser
    gnome-tweak-tool
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

  #nixpkgs.overlays = [
    #(import ./overlays/node-coc-metals.nix)
  #];

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

  home.packages = defaultPkgs ++ gitPkgs ++ gnomePkgs ++ haskellPkgs;

  home = {
    username = "gvolpe";
    homeDirectory = "/home/gvolpe";
    stateVersion = "20.09";
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
        #"nckgahadagoaajjgafhacjanaoiihapd" # google hangouts
        "kklailfgofogmmdlhgmjgenehkjoioip" # google meet grid view
        "aapbdbdomjkkjkaonfhkkikfgjllcleb" # google translate
        "hdokiejnpimakedhajhdlcegeplioahd" # lastpass password manager
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

    #obs-studio = {
    #enable = true;
    #plugins = [];
    #};

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
