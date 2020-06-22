{ config, pkgs, stdenv, lib, ... }:

let
  customGnome3Ext = pkgs.callPackage ./programs/gnome/extensions.nix {};

  defaultPkgs = with pkgs; [
    cachix         # nix caching
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

    # desktop look & feel
    gnome3.gnome-tweak-tool
    customGnome3Ext.dash-to-dock
    customGnome3Ext.timepp
    customGnome3Ext.topicons-plus
  ];

  gitPkgs = with pkgs; [
    gitAndTools.diff-so-fancy # git diff with colors
    gitAndTools.tig           # diff and commit view
  ];

  haskellPkgs = with pkgs.haskellPackages; [
    brittany      # code formatter
    cabal2nix     # convert cabal projects to nix
    cabal-install # package manager
    ghc           # compiler
    ghcide        # haskell IDE
    hoogle        # documentation
  ];
in
{
  programs.home-manager.enable = true;

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

  home.packages = defaultPkgs ++ gitPkgs ++ haskellPkgs;

  home = {
    username = "gvolpe";
    homeDirectory = "/home/gvolpe";
    stateVersion = "20.09";

    # Set user's profile picture for gnome3 (TODO: find a better way to do it as root)
    activation = {
      userPicture = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        sudo cp ~/.config/nixpkgs/gvolpe.png /var/lib/AccountsService/icons/gvolpe
        sudo echo "Icon=/var/lib/AccountsService/icons/gvolpe" >> /var/lib/AccountsService/users/gvolpe
      '';
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
        "dbepggeogbaibhgnhhndojpepiihcmeb" # vimium
      ];
    };

    direnv = {
      enable = true;
      enableFishIntegration = true;
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
