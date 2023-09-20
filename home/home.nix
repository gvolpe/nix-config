{ pkgs, ... }:

let
  username = "ali";
  homeDirectory = "/home/${username}";
  configHome = "${homeDirectory}/.config";

  defaultPkgs = with pkgs; [
    #gnomecast            # chromecast local files
    alacritty            # fast terminal emulator
    any-nix-shell        # fish support for nix shell
    arandr               # simple GUI for xrandr
    asciinema            # record the terminal
    audacious            # simple music player
    audacity             # sound recording software
    bitwarden-cli        # command-line client for the password manager
    bottom               # alternative to htop & ytop
    calibre              # e-book reader
    cobang               # qr-code scanner
    cowsay               # cowsay fortune teller with random images
    dconf2nix            # dconf (gnome) files to nix converter
    dig                  # dns command-line tool
    dive                 # explore docker layers
    docker-compose       # docker manager
    drawio               # diagram design
    duf                  # disk usage/free utility
    exa                  # a better `ls`
    fd                   # "find" for files
    gimp                 # gnu image manipulation program
    glow                 # terminal markdown viewer
    haskellPackages.nix-tree # visualize nix dependencies
    hyperfine            # command-line benchmarking tool
    insomnia             # rest client with graphql support
    jitsi-meet-electron  # open source video calls and chat
    jmtpfs               # mount mtp devices
    killall              # kill processes by name
    libnotify            # notify-send command
    libreoffice          # office suite
    lnav                 # log file navigator on the terminal
    mpv                  # fantastic media player
    multilockscreen      # fast lockscreen based on i3lock
    ncdu                 # disk space info (a better du)
    nitch                # minimal system information fetch
    nix-index            # locate packages containing certain nixpkgs
    nix-output-monitor   # nom: monitor nix commands
    nyancat              # the famous rainbow cat!
    ouch                 # painless compression and decompression for your terminal
    paprefs              # pulseaudio preferences
    pasystray            # pulseaudio systray
    pavucontrol          # pulseaudio volume control
    pgcli                # modern postgres client
    playerctl            # music player controller
    prettyping           # a nicer ping
    protonvpn-gui        # official proton vpn client
    pulsemixer           # pulseaudio mixer
    rage                 # encryption tool for secrets management
    ranger               # terminal file explorer
    ripgrep              # fast grep
    screenkey            # shows keypresses on screen
    simple-scan          # scanner gui
    simplescreenrecorder # screen recorder gui
    skypeforlinux        # messaging client
    slack                # messaging client
    tdesktop             # telegram messaging client
    tldr                 # summary of a man page
    tmux                 # terminal multiplexer
    tree                 # display files in a tree view
    vlc                  # media player
    xsel                 # clipboard support (also for neovim)
    zoom-us              # message client
    firefox              # web browser
    keybase-gui          # keybase messenger
  ];

  gnomePkgs = with pkgs.gnome; [
    eog      # image viewer
    evince   # pdf reader
    gnome-disk-utility
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
