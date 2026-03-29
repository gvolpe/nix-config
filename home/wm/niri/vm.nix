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

  packs = with pkgs; [
    libnotify # notifications
    nemo # file manager
    networkmanagerapplet # network manager systray app
    nfsm # niri fullscreen manager daemon
    nfsm-cli # niri fullscreen manager client
    snitch # inspect network connections
    wl-clipboard # clipboard support
    any-nix-shell # fish support for nix shell
    bottom # alternative to htop & ytop
    dig # dns command-line tool
    duf # disk usage/free utility
    eza # a better `ls`
    fd # "find" for files
    killall # kill processes by name
    ncdu # disk space info (a better du)
    nix-output-monitor # nom: monitor nix commands
    nix-search # faster nix search client
    ripgrep # fast grep
    socat # multipurpose relay (SOcket CAT)
    tree # display files in a tree view
    unzip # uncompress files
    xsel # clipboard support (also for neovim)
    zip # compress files
  ] ++ fontPkgs ++ (pkgs.sxm.scripts or [ ]);

  configPath = "${config.dotfiles.path}/wm/niri";

  configSrc =
    if !config.dotfiles.mutable then ./config.kdl
    else mkOutOfStoreSymlink "${configPath}/config.kdl";

  genConfigFileName = name:
    if !config.dotfiles.mutable then ./config/${name}.kdl
    else mkOutOfStoreSymlink "${configPath}/config/${name}.kdl";

  includeConfig =
    lib.lists.forEach
      [ "animations" "binds" "edp" "hdmi" "input" "layers" "layout" "misc" "windows" "workspaces" ]
      (n: { xdg.configFile."niri/config/${n}.kdl".source = genConfigFileName n; });
in
{
  xdg.configFile."niri/config.kdl".source = configSrc;
  xdg.configFile."niri/config/output.kdl".text =
    if config.hidpi then ''include "hdmi.kdl"'' else ''include "edp.kdl"'';

  services.polkit-gnome.enable = true;

  imports = includeConfig ++ [
    ../../shared
    ../../programs/fuzzel
    ../../programs/kitty
    ../../programs/waybar
    ../../programs/waypaper
    ../../programs/wlogout
    ../../programs/yazi
    ../../services/swaync
    ../../services/vicinae
  ];

  home = {
    # override packages from shared (too many for a light vm)
    packages = lib.mkForce packs;
    stateVersion = "23.05";

    sessionVariables = {
      NIXOS_OZONE_WL = 1;
      SHELL = "${lib.exe pkgs.fish}";
      MOZ_ENABLE_WAYLAND = 1;
      ELECTRON_OZONE_PLATFORM_HINT = "auto";
    };
  };

  fonts.fontconfig.enable = true;

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
