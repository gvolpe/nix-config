{ config, lib, pkgs, ... }:

let
  inherit (config.lib.file) mkOutOfStoreSymlink;

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

  videoPkgs = with pkgs.video-scripts; [
    compression # compress video
    recording # record video
    trimming # trim video
    extractFrame # extract video frame as image
  ];

  packages = with pkgs; [
    brightnessctl # control laptop display brightness
    loupe # image viewer
    grim # screenshots
    grimblast # screenshot program from hyprland
    kooha # screencast recorder
    libnotify # notifications
    nemo # file manager
    networkmanagerapplet # network manager systray app
    nfsm # niri fullscreen manager daemon
    nfsm-cli # niri fullscreen manager client
    niri-scratchpad # niri scratchpad support
    nsticky # niri sticky windows support
    satty-shot # screenshots
    wl-clipboard # clipboard support
  ] ++ fontPkgs ++ audioPkgs ++ videoPkgs;

  configPath = "${config.dotfiles.path}/wm/niri";

  configSrc =
    if !config.dotfiles.mutable then ./config.kdl
    else mkOutOfStoreSymlink "${configPath}/config.kdl";

  genConfigFileName = name:
    if !config.dotfiles.mutable then ./config/${name}.kdl
    else mkOutOfStoreSymlink "${configPath}/config/${name}.kdl";

  includeConfig =
    lib.lists.forEach
      [ "binds" "input" "layers" "layout" "misc" "output" "windows" "workspaces" ]
      (n: { xdg.configFile."niri/config/${n}.kdl".source = genConfigFileName n; });
in
{
  xdg.configFile."niri/config.kdl".source = configSrc;

  services.polkit-gnome.enable = true;

  imports = includeConfig ++ [
    ../../shared
    ../../programs/cava
    ../../programs/fuzzel
    ../../programs/kitty
    ../../programs/waybar
    ../../programs/waypaper
    ../../programs/wlogout
    ../../programs/yazi
    ../../services/swayidle
    ../../services/swaync
  ];

  home = {
    inherit packages;
    stateVersion = "23.05";

    sessionVariables = {
      NIXOS_OZONE_WL = 1;
      SHELL = "${lib.exe pkgs.fish}";
      MOZ_ENABLE_WAYLAND = 1;
      ELECTRON_OZONE_PLATFORM_HINT = "auto";
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
