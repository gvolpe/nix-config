{ pkgs, lib, ... }:

let
  nerdFonts = with (pkgs.nerd-fonts); [
    jetbrains-mono
    iosevka
  ];

  fontPkgs = with pkgs; [
    font-awesome # awesome fonts
    material-design-icons # fonts with glyphs
  ] ++ nerdFonts;

  audioPkgs = with pkgs; [
    paprefs # pulseaudio preferences
    pasystray # pulseaudio systray
    pavucontrol # pulseaudio volume control
    playerctl # music player controller
    pulsemixer # pulseaudio mixer
    reaper # digital audio workstation (daw)
  ];

  scripts = pkgs.callPackage ../hyprland/scripts.nix { };

  packages = with pkgs; [
    alacritty # terminal
    fuzzel # app launcher
    swaybg # wallpapers
    networkmanagerapplet # network manager systray app

    brightnessctl # control laptop display brightness
    loupe # image viewer
    grim # screenshots
    grimblast # screenshot program from hyprland
    kooha # screencast recorder
    libnotify # notifications
    nemo # file manager
    nix-search # faster nix search client
    unzip # uncompress files
    wl-clipboard # clipboard support
    zip # compress files
  ] ++ fontPkgs ++ audioPkgs ++ [ scripts.satty ];
in
{
  programs.swaylock.enable = true; # Super+Alt+L in the default setting (screen locker)

  services = {
    swayidle.enable = true; # idle management daemon
    polkit-gnome.enable = true; # polkit
  };

  imports = [
    ../../shared
    ../../programs/kitty
    ../../programs/waybar
    ../../services/mako
  ];

  home = {
    inherit packages;
    stateVersion = "23.05";

    sessionVariables = {
      NIXOS_OZONE_WL = 1;
      SHELL = "${lib.exe pkgs.fish}";
      MOZ_ENABLE_WAYLAND = 1;
      XDG_CURRENT_DESKTOP = "niri";
      XDG_SESSION_DESKTOP = "Wayland";
      XDG_SESSION_TYPE = "wayland";
      GDK_BACKEND = "wayland,x11";
      QT_QPA_PLATFORM = "wayland;xcb";
    };
  };

  fonts.fontconfig.enable = true;

  # e.g. for slack, etc
  xdg.configFile."electron-flags.conf".text = ''
    --enable-features=UseOzonePlatform
    --ozone-platform=wayland
  '';

  xdg.portal = {
    enable = true;
    config = {
      common = {
        default = [ "gtk" "gnome" ];
      };
      niri = {
        default = [ "gtk" "gnome" ];
      };
    };
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-gnome
    ];
    xdgOpenUsePortal = true;
  };
}
