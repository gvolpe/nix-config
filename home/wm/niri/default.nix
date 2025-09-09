{ config, lib, pkgs, ... }:

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

  scripts =
    let s = pkgs.callPackage ./scripts.nix { };
    in [ s.satty s.scratchpad ];

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
  ] ++ fontPkgs ++ audioPkgs ++ scripts;

  filePath = "${config.dotfiles.path}/wm/niri/config.kdl";

  configSrc =
    if !config.dotfiles.mutable then ./config.kdl
    else config.lib.file.mkOutOfStoreSymlink filePath;
in
{
  xdg.configFile."niri/config.kdl".source = configSrc;

  services.polkit-gnome.enable = true;

  imports = [
    ../../shared
    ../../programs/kitty
    ../../programs/waybar
    ../../services/dunst
    ../../services/swayidle
  ];

  home = {
    inherit packages;
    stateVersion = "23.05";

    sessionVariables = {
      NIXOS_OZONE_WL = 1;
      SHELL = "${lib.exe pkgs.fish}";
      MOZ_ENABLE_WAYLAND = 1;
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
