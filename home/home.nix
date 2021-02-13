{ config, lib, pkgs, stdenv, ... }:

let
  hms = pkgs.callPackage ./switcher.nix { inherit config pkgs; };

  defaultPkgs = with pkgs; [
    act                  # run github actions locally
    any-nix-shell        # fish support for nix shell
    asciinema            # record the terminal
    audacious            # simple music player
    betterlockscreen     # fast lockscreen based on i3lock
    bottom               # alternative to htop & ytop
    cachix               # nix caching
    calibre              # e-book reader
    dconf2nix            # dconf (gnome) files to nix converter
    dmenu                # application launcher
    docker-compose       # docker manager
    dive                 # explore docker layers
    element-desktop      # a feature-rich client for Matrix.org
    exa                  # a better `ls`
    fd                   # "find" for files
    gimp                 # gnu image manipulation program
    hms                  # custom home-manager switcher
    hyperfine            # command-line benchmarking tool
    insomnia             # rest client with graphql support
    jitsi-meet-electron  # open source video calls and chat
    k9s                  # k8s pods manager
    killall              # kill processes by name
    libreoffice          # office suite
    libnotify            # notify-send command
    ncdu                 # disk space info (a better du)
    neofetch             # command-line system information
    ngrok-2              # secure tunneling to localhost
    nix-doc              # nix documentation search tool
    nyancat              # the famous rainbow cat!
    manix                # documentation searcher for nix
    mupdf                # pdf viewer with vim-like keybindings
    pavucontrol          # pulseaudio volume control
    paprefs              # pulseaudio preferences
    pasystray            # pulseaudio systray
    playerctl            # music player controller
    prettyping           # a nicer ping
    pulsemixer           # pulseaudio mixer
    ripgrep              # fast grep
    rnix-lsp             # nix lsp server
    signal-desktop       # signal messaging client
    simplescreenrecorder # self-explanatory
    slack                # messaging client
    spotify              # music source
    tdesktop             # telegram messaging client
    terminator           # great terminal multiplexer
    tldr                 # summary of a man page
    tree                 # display files in a tree view
    vlc                  # media player
    xclip                # clipboard support (also for neovim)
    yad                  # yet another dialog - fork of zenity

    # fixes the `ar` error required by cabal
    binutils-unwrapped
  ];

  gitPkgs = with pkgs.gitAndTools; [
    diff-so-fancy # git diff with colors
    git-crypt     # git files encryption
    hub           # github command-line client
    tig           # diff and commit view
  ];

  gnomePkgs = with pkgs.gnome3; [
    eog            # image viewer
    evince         # pdf reader
    gnome-calendar # calendar
    nautilus       # file manager
    # themes
    adwaita-icon-theme
    pkgs.material-design-icons
  ];

  haskellPkgs = with pkgs.haskellPackages; [
    brittany                # code formatter
    cabal2nix               # convert cabal projects to nix
    cabal-install           # package manager
    ghc                     # compiler
    haskell-language-server # haskell IDE (ships with ghcide)
    hoogle                  # documentation
    nix-tree                # visualize nix dependencies
  ];

  polybarPkgs = with pkgs; [
    font-awesome-ttf      # awesome fonts
    material-design-icons # fonts with glyphs
  ];

  xmonadPkgs = with pkgs; [
    haskellPackages.libmpd # music player daemon
    haskellPackages.xmobar # status bar
    networkmanager_dmenu   # networkmanager on dmenu
    networkmanagerapplet   # networkmanager applet
    nitrogen               # wallpaper manager
    xcape                  # keymaps modifier
    xorg.xkbcomp           # keymaps modifier
    xorg.xmodmap           # keymaps modifier
    xorg.xrandr            # display manager (X Resize and Rotate protocol)
  ];

in
{
  programs.home-manager.enable = true;

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = p: {
      # temporary hack for fish until there's a fix upstream
      fish-foreign-env = pkgs.fishPlugins.foreign-env;
      nur = import (import pinned/nur.nix) { inherit pkgs; };
    };
  };

  nixpkgs.overlays = [];

  imports = (import ./programs) ++ (import ./services);

  xdg.enable = true;

  home = {
    username      = "gvolpe";
    homeDirectory = "/home/gvolpe";
    stateVersion  = "20.09";

    packages = defaultPkgs ++ gitPkgs ++ gnomePkgs ++ haskellPkgs ++ polybarPkgs ++ xmonadPkgs;

    sessionVariables = {
      DISPLAY = ":0";
      EDITOR = "nvim";
    };
  };

  manual = {
    json.enable = false;
    html.enable = false;
    manpages.enable = false;
  };

  # notifications about home-manager news
  news.display = "silent";

  gtk = {
    enable = true;
    iconTheme = {
      name = "Adwaita-dark";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
    theme = {
      name = "Adwaita-dark";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
  };

  programs = {
    bat.enable = true;

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

    gpg.enable = true;

    htop = {
      enable = true;
      sortDescending = true;
      sortKey = "PERCENT_CPU";
    };

    jq.enable = true;
    ssh.enable = true;
  };

  services = {
    flameshot.enable = true;
  };

}
