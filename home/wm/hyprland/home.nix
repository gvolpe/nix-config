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

  packages = with pkgs; [
    brightnessctl # control laptop display brightness
    loupe # image viewer
    grim # screenshots
    grimblast # screenshot program from hyprland
    libnotify # notifications
    nemo # file manager
    nix-search # faster nix search client
    unzip # compressed files
    wl-clipboard # clipboard support
    wofi # app launcher
    kdePackages.xwaylandvideobridge # screensharing bridge
  ] ++ fontPkgs ++ audioPkgs;

  wpctl = "${pkgs.wireplumber}/bin/wpctl";

  scripts = pkgs.callPackage ./scripts.nix { };

  workspaceConf = { monitor }: ''
    workspace=1,persistent:true,monitor:${monitor}
    workspace=2,persistent:true,on-created-empty:${lib.exe scripts.wsNix},monitor:${monitor}
    workspace=3,persistent:true,monitor:${monitor}
    workspace=4,persistent:true,monitor:${monitor}
    workspace=5,persistent:true,on-created-empty:firefox-beta -p 'sxm',monitor:${monitor}
    workspace=6,persistent:true,on-created-empty:footclient -e btm,monitor:${monitor}
  '';
in
{
  imports = [
    ../../shared
    ../../programs/foot
    ../../programs/hyprlock
    ../../programs/hyprpaper
    ../../programs/pyprland
    ../../programs/waybar
    ../../services/hypridle
  ];

  home = {
    inherit packages;
    stateVersion = "23.05";

    sessionVariables = {
      NIXOS_OZONE_WL = 1;
      SHELL = "${lib.exe pkgs.fish}";
      MOZ_ENABLE_WAYLAND = 1;
      XDG_CURRENT_DESKTOP = "Hyprland";
      XDG_SESSION_DESKTOP = "Hyprland";
      XDG_SESSION_TYPE = "wayland";
      GDK_BACKEND = "wayland,x11";
      QT_QPA_PLATFORM = "wayland;xcb";
    };
  };

  fonts.fontconfig.enable = true;

  programs.hypr-binds.enable = true;

  # e.g. for slack, signal, etc
  xdg.configFile."electron-flags.conf".text = ''
    --enable-features=UseOzonePlatform
    --ozone-platform=wayland
  '';

  xdg.portal = {
    enable = true;
    config = {
      common = {
        default = [ "hyprland" ];
      };
      hyprland = {
        default = [ "gtk" "hyprland" ];
      };
    };
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland
    ];
    xdgOpenUsePortal = true;
  };

  wayland.windowManager.hyprland = {
    enable = true;
    extraConfig = (builtins.readFile ./hyprland.conf) + ''
      bindd=,F1,Show keybindings,exec,hypr-binds
      bindd=SUPER,P,App launcher,exec,${lib.exe pkgs.wofi} --show run --style=${./wofi.css} --term=footclient --prompt=Run
      bindd=SUPERSHIFT,A,Take screenshot,exec,${lib.exe scripts.satty}
      bindd=SUPERCTRL,L,Lock system,exec,${lib.exe pkgs.hyprlock}
      # audio volume bindings
      binddel=,XF86AudioRaiseVolume,Raise volume 󰝝 ,exec,${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%+
      binddel=,XF86AudioLowerVolume,Lower volume 󰝞 ,exec,${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%-
      binddl=,XF86AudioMute,Toggle mute 󰝟 ,exec,${wpctl} set-mute @DEFAULT_AUDIO_SINK@ toggle

      ${workspaceConf { monitor = "${scripts.extMonitor}"; }}

      exec-once=${lib.exe scripts.monitorInit}
      exec-once=${lib.exe pkgs.hyprland-monitor-attached} ${lib.exe scripts.monitorAdded} ${lib.exe scripts.monitorRemoved}
      exec-once=${lib.exe pkgs.hyprpaper}
      exec-once=${pkgs.pyprland}/bin/pypr
      exec-once=${pkgs.blueman}/bin/blueman-applet
      exec-once=${pkgs.networkmanagerapplet}/bin/nm-applet --sm-disable --indicator
      exec-once=${lib.exe pkgs.pasystray}
    '';
    plugins = [ ];
    systemd = {
      enable = true;
      variables = [ "--all" ];
    };
    xwayland.enable = true;
  };
}
