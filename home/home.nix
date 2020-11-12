{ config, lib, pkgs, stdenv, ... }:

let
  unstable  = import (import ./unstable.nix) {};

  unstablePkgs = with unstable; [
    betterlockscreen     # fast lockscreen based on i3lock
    brave                # private web browser based on chromium
    compton              # composite manager for X11
    dmenu                # application launcher
    killall              # kill processes by name
    libnotify            # notify-send command
    manix                # documentation searcher for nix
    mupdf                # pdf viewer with vim-like keybindings
    neofetch             # command-line system information
    nyancat              # the famous rainbow cat!
    pavucontrol          # pulseaudio volume control
    paprefs              # pulseaudio preferences
    pasystray            # pulseaudio systray
    playerctl            # music player controller
    pulsemixer           # pulseaudio mixer
    simplescreenrecorder # self-explanatory
    vlc                  # media player

    # status bar packages
    taffybar                             # awesome status bar
    haskellPackages.gtk-sni-tray         # gtk-sni-tray-standalone
    haskellPackages.status-notifier-item # status-notifier-watcher for taffybar
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

  gnomePkgs = with pkgs.gnome3; [
    eog      # image viewer
    evince   # pdf reader
    nautilus # file manager
    zenity   # display dialogs
    # themes
    adwaita-icon-theme
    pkgs.hicolor-icon-theme
    pkgs.material-design-icons
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

  nixpkgs.overlays = [
    (import ./overlays/dconf2nix.nix)
    (import ./overlays/manix.nix)
    (import ./overlays/taffybar-deps.nix)
    (import ./overlays/taffybar-wrapper.nix)
    (import ./overlays/vim-plugins.nix)
  ];

  imports = [
    ./programs/alacritty/default.nix
    ./programs/browsers/brave.nix
    ./programs/git/default.nix
    ./programs/fish/default.nix
    ./programs/neovim/default.nix
    ./programs/rofi/default.nix
    ./programs/taffybar/default.nix
    ./programs/terminator/default.nix
    ./programs/xmonad/default.nix
    ./programs/xmobar/default.nix
    ./services/dunst/default.nix
    ./services/networkmanager/default.nix
    ./services/polybar/default.nix
    ./services/screenlocker/default.nix
  ];

  xdg.enable = true;

  home = {
    username      = "gvolpe";
    homeDirectory = "/home/gvolpe";
    stateVersion  = "20.09";

    packages = defaultPkgs ++ gitPkgs ++ gnomePkgs ++ haskellPkgs ++ polybarPkgs ++ xmonadPkgs ++ unstablePkgs;

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

    gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800;
      enableSshSupport = true;
    };

    mpd = {
      enable = true;
      extraConfig = ''
        audio_output {
          type            "alsa"
          name            "USB-Audio - Scarlett 2i4 USB"
          device          "hw:1"
        }

        audio_output {
          type            "pulse"
          name            "nixos"
        }
      '';
    };

    mpdris2.enable = true;
  };

}
