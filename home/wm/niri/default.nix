{ config, lib, pkgs, ... }:

let
  inherit (config) dotfiles;

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
    pear-desktop # youtube music with mpris support
    playerctl # music player controller
    pulsemixer # pulseaudio mixer
    reaper # digital audio workstation (daw)
  ];

  videoPkgs = with pkgs.video-scripts; [
    compression # compress video
    recording # record video
    trimming # trim video
    extractFrame # extract video frame as image
  ];

  packages = with pkgs; [
    brightnessctl # control laptop display brightness
    diskonaut # disk space manager tui
    dnix # determinate nix binary with flake schemas support
    loupe # image viewer
    grim # screenshots
    grimblast # screenshot program from hyprland
    handy # speech to text
    hyprlax # dynamic wallpapers
    kooha # screencast recorder
    libnotify # notifications
    nemo # file manager
    networkmanagerapplet # network manager systray app
    nfsm # niri fullscreen manager daemon
    nfsm-cli # niri fullscreen manager client
    niri-scratchpad # niri scratchpad support
    nsticky # niri sticky windows support
    satty-shot # screenshots
    snitch # inspect network connections
    wl-clipboard # clipboard support
    wooz # zoom / magnifier utility
  ] ++ fontPkgs ++ audioPkgs ++ videoPkgs;

  genConfigFile = name:
    dotfiles.make ./config/${name}.kdl ./config;

  includeConfig =
    lib.lists.forEach
      [ "animations" "binds" "edp" "hdmi" "input" "layers" "layout" "misc" "windows" "workspaces" ]
      (n: { xdg.configFile."niri/config/${n}.kdl".source = genConfigFile n; });
in
{
  fonts.fontconfig.enable = true;

  home = {
    inherit packages;
    sessionVariables = {
      ELECTRON_OZONE_PLATFORM_HINT = "auto";
      MOZ_ENABLE_WAYLAND = 1;
      NIXOS_OZONE_WL = 1;
      SHELL = "${lib.exe pkgs.fish}";
    };
    stateVersion = "23.05";
  };

  imports = includeConfig ++ [
    ../../shared
    ../../programs/atuin
    ../../programs/cava
    ../../programs/dankcalendar
    ../../programs/fuzzel
    ../../programs/gram
    ../../programs/helium
    ../../programs/jujutsu
    ../../programs/kitty
    ../../programs/waybar
    ../../programs/waypaper
    ../../programs/wlogout
    ../../programs/yazi
    ../../services/swayidle
    ../../services/swaync
    ../../services/vicinae
  ];

  services.polkit-gnome.enable = true;

  software.defaults.enable = true;

  # e.g. for slack, etc
  xdg.configFile."electron-flags.conf".text = ''
    --enable-features=UseOzonePlatform
    --ozone-platform=wayland
  '';

  xdg.configFile."niri/config.kdl".source = dotfiles.make ./config.kdl ./.;

  xdg.configFile."niri/config/output.kdl".text =
    if config.hidpi then ''include "hdmi.kdl"'' else ''include "edp.kdl"'';

  xdg.portal = {
    config = {
      common = {
        default = [ "gtk" "gnome" ];
      };
      niri = {
        default = [ "gtk" "gnome" ];
      };
    };
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-gnome
    ];
    xdgOpenUsePortal = true;
  };
}
