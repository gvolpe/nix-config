{ pkgs, config, ... }:

let
  extra = ''
    set +x
    ${pkgs.util-linux}/bin/setterm -blank 0 -powersave off -powerdown 0
    ${pkgs.xorg.xset}/bin/xset s off
    ${pkgs.xcape}/bin/xcape -e "Hyper_L=Tab;Hyper_R=backslash"
    ${pkgs.xorg.setxkbmap}/bin/setxkbmap -option ctrl:nocaps
    ${pkgs.autorandr}/bin/autorandr --change
  '';

  mega = config.programs.megasync;

  polybarOpts = ''
    ${pkgs.nitrogen}/bin/nitrogen --restore &
    ${pkgs.pasystray}/bin/pasystray &
    ${pkgs.blueman}/bin/blueman-applet &
    ${pkgs.networkmanagerapplet}/bin/nm-applet --sm-disable --indicator &
    ${pkgs.bat-lvl}/bin/battery-level-check &
    ${if mega.enable then "${mega.package}/bin/megasync &" else ""}
  '';

  xmonadPkgs = with pkgs; [
    arandr # simple GUI for xrandr
    asciinema # record the terminal
    bitwarden-cli # command-line client for the password manager
    # FIXME: calibre is broken
    #calibre # e-book reader
    #cobang               # qr-code scanner
    cowsay # cowsay fortune teller with random images
    dive # explore docker layers
    drawio # diagram design
    #gnomecast            # chromecast local files
    libnotify # notify-send command
    multilockscreen # fast lockscreen based on i3lock
    ouch # painless compression and decompression for your terminal
    pavucontrol # pulseaudio volume control
    paprefs # pulseaudio preferences
    pasystray # pulseaudio systray
    pgcli # modern postgres client
    playerctl # music player controller
    prettyping # a nicer ping
    protonvpn-gui # official proton vpn client
    pulsemixer # pulseaudio mixer
    rage # encryption tool for secrets management
    screenkey # shows keypresses on screen (only for x11)
    simple-scan # scanner gui
    simplescreenrecorder # screen recorder gui
    tldr # summary of a man page

    # haskell packages
    haskellPackages.nix-tree # visualize nix dependencies

    # xmonad
    dialog # Dialog boxes on the terminal (to show key bindings)
    networkmanager_dmenu # networkmanager on dmenu
    networkmanagerapplet # networkmanager applet
    nitrogen # wallpaper manager
    xcape # keymaps modifier
    xorg.xkbcomp # keymaps modifier
    xorg.xmodmap # keymaps modifier
    xorg.xrandr # display manager (X Resize and Rotate protocol)
  ];

  gnomePkgs = with pkgs; [
    eog # image viewer
    evince # pdf reader
    gnome-disk-utility
    nautilus # file manager
  ];
in
{
  programs.home-manager.enable = true;
  programs.megasync.enable = true;

  imports = [
    ../../shared
    ../../programs/alacritty
    ../../programs/autorandr
    ../../programs/orage
    ../../programs/rofi
    ../../programs/statix
    ../../services/dunst
    ../../services/networkmanager
    ../../services/picom
    ../../services/polybar
    ../../services/screenlocker
  ];

  home = {
    stateVersion = "21.03";
    packages = xmonadPkgs ++ gnomePkgs;
  };

  xsession = {
    enable = true;

    initExtra = extra + polybarOpts;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [
        hp.dbus
        hp.monad-logger
      ];
      config = ./config.hs;
    };
  };

  xresources.properties = {
    "Xft.dpi" = 180;
    "Xft.autohint" = 0;
    "Xft.hintstyle" = "hintfull";
    "Xft.hinting" = 1;
    "Xft.antialias" = 1;
    "Xft.rgba" = "rgb";
    "Xcursor*theme" = "Vanilla-DMZ-AA";
    "Xcursor*size" = 24;
  };
}
