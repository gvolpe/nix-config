{ config, lib, pkgs, stdenv, ... }:

let
  # workaround to open a URL in a new tab in the specific firefox profile
  work-browser = pkgs.callPackage ./programs/browsers/work.nix {};

  defaultPkgs = with pkgs; [
    any-nix-shell        # fish support for nix shell
    arandr               # simple GUI for xrandr
    asciinema            # record the terminal
    audacious            # simple music player
    bitwarden-cli        # command-line client for the password manager
    bottom               # alternative to htop & ytop
    cachix               # nix caching
    calibre              # e-book reader
    dconf2nix            # dconf (gnome) files to nix converter
    discord              # chat client for dev stuff
    dmenu                # application launcher
    docker-compose       # docker manager
    dive                 # explore docker layers
    duf                  # disk usage/free utility
    element-desktop      # a feature-rich client for Matrix.org
    exa                  # a better `ls`
    fd                   # "find" for files
    gimp                 # gnu image manipulation program
    gnomecast            # chromecast local files
    hyperfine            # command-line benchmarking tool
    insomnia             # rest client with graphql support
    jitsi-meet-electron  # open source video calls and chat
    k9s                  # k8s pods manager
    killall              # kill processes by name
    libreoffice          # office suite
    libnotify            # notify-send command
    multilockscreen      # fast lockscreen based on i3lock
    ncdu                 # disk space info (a better du)
    neofetch             # command-line system information
    ngrok-2              # secure tunneling to localhost
    nix-doc              # nix documentation search tool
    nix-index            # files database for nixpkgs
    nixos-generators     # nix tool to generate isos
    nyancat              # the famous rainbow cat!
    manix                # documentation searcher for nix
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
    tex2nix              # texlive expressions for documents
    tldr                 # summary of a man page
    tree                 # display files in a tree view
    vlc                  # media player
    xclip                # clipboard support (also for neovim)
    yad                  # yet another dialog - fork of zenity

    # work stuff
    work-browser

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

  scripts = pkgs.callPackage ./scripts/default.nix { inherit config pkgs; };

  xmonadPkgs = with pkgs; [
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
      nur = import (import pinned/nur.nix) { inherit pkgs; };
    };
  };

  nixpkgs.overlays = [
    (import ./overlays/beauty-line)
    (import ./overlays/discord)
    (import ./overlays/tex2nix)
  ];

  imports = (import ./programs) ++ (import ./services) ++ [(import ./themes)];

  xdg.enable = true;

  home = {
    username      = "gvolpe";
    homeDirectory = "/home/gvolpe";
    stateVersion  = "21.03";

    packages = defaultPkgs ++ gitPkgs ++ gnomePkgs ++ haskellPkgs ++ polybarPkgs ++ scripts ++ xmonadPkgs;

    sessionVariables = {
      DISPLAY = ":0";
      EDITOR = "nvim";
    };
  };

  # restart services on change
  systemd.user.startServices = "sd-switch";

  # notifications about home-manager news
  news.display = "silent";

  programs = {
    autojump = {
      enable = true;
      enableFishIntegration = true;
    };

    bat.enable = true;

    broot = {
      enable = true;
      enableFishIntegration = true;
    };

    direnv = {
      enable = true;
      enableFishIntegration = true;
      nix-direnv.enable = true;
    };

    fzf = {
      enable = true;
      enableFishIntegration = true;
    };

    gpg.enable = true;

    htop = {
      enable = true;
      settings = {
        sort_direction = true;
        sort_key = "PERCENT_CPU";
      };
    };

    jq.enable = true;

    obs-studio = {
      enable = true;
      plugins = [];
    };

    ssh.enable = true;
  };

  services = {
    flameshot.enable = true;
  };

}
