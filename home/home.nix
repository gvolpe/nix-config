{ config, lib, pkgs, stdenv, ... }:

let
  username = "gvolpe";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";

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
    cobang               # qr-code scanner
    dconf2nix            # dconf (gnome) files to nix converter
    dig                  # dns command-line tool
    docker-compose       # docker manager
    dive                 # explore docker layers
    drawio               # diagram design
    duf                  # disk usage/free utility
    exa                  # a better `ls`
    fd                   # "find" for files
    gimp                 # gnu image manipulation program
    glow                 # terminal markdown viewer
    gnomecast            # chromecast local files
    hyperfine            # command-line benchmarking tool
    insomnia             # rest client with graphql support
    jitsi-meet-electron  # open source video calls and chat
    jmtpfs               # mount mtp devices
    killall              # kill processes by name
    libreoffice          # office suite
    libnotify            # notify-send command
    lnav                 # log file navigator on the terminal
    multilockscreen      # fast lockscreen based on i3lock
    ncdu                 # disk space info (a better du)
    ngrok                # secure tunneling to localhost
    nix-index            # locate packages containing certain nixpkgs
    nyancat              # the famous rainbow cat!
    pavucontrol          # pulseaudio volume control
    paprefs              # pulseaudio preferences
    pasystray            # pulseaudio systray
    pgcli                # modern postgres client
    playerctl            # music player controller
    prettyping           # a nicer ping
    protonvpn-gui        # official proton vpn client
    pulsemixer           # pulseaudio mixer
    rage                 # encryption tool for secrets management
    ranger               # terminal file explorer
    ripgrep              # fast grep
    simple-scan          # scanner gui
    simplescreenrecorder # screen recorder gui
    skypeforlinux        # messaging client
    slack                # messaging client
    tdesktop             # telegram messaging client
    tex2nix              # texlive expressions for documents
    tldr                 # summary of a man page
    tree                 # display files in a tree view
    vlc                  # media player
    xdg-utils            # xdg-open replaced with mimeo (see overlay)
    xsel                 # clipboard support (also for neovim)
    zoom-us              # message client

    # haskell packages
    haskellPackages.nix-tree # visualize nix dependencies

    # work stuff
    work-browser

    # fixes the `ar` error required by cabal
    binutils-unwrapped
  ];

  gnomePkgs = with pkgs.gnome; [
    eog      # image viewer
    evince   # pdf reader
    #nautilus # file manager

    # file manager overlay
    pkgs.nautilus-gtk3
    #pkgs.nautilus-bin
    #pkgs.nautilus-patched
  ];
in
{
  programs.home-manager.enable = true;

  imports = builtins.concatMap import [
    ./modules
    ./age
    ./programs
    ./scripts
    ./services
    ./themes
  ];

  xdg = {
    inherit configHome;
    enable = true;
  };

  home = {
    inherit username homeDirectory;
    stateVersion = "21.03";

    packages = defaultPkgs ++ gnomePkgs;

    sessionVariables = {
      DISPLAY = ":0";
      EDITOR = "nvim";
    };
  };

  # restart services on change
  systemd.user.startServices = "sd-switch";

  # notifications about home-manager news
  news.display = "silent";
}
