{ pkgs, lib, ... }:

let
  fontPkgs = with pkgs; [
    font-awesome # awesome fonts
    material-design-icons # fonts with glyphs
    (nerdfonts.override {
      fonts = [
        "JetBrainsMono"
        "Iosevka"
      ];
    })
  ];

  audioPkgs = with pkgs; [
    paprefs # pulseaudio preferences
    pasystray # pulseaudio systray
    pavucontrol # pulseaudio volume control
    playerctl # music player controller
    pulsemixer # pulseaudio mixer
  ];

  packages = with pkgs; [
    brightnessctl # control laptop display brightness
    cinnamon.nemo # file manager
    loupe # image viewer
    grim # screenshots
    grimblast # screenshot program from hyprland
    libnotify # notifications
    wl-clipboard # clipboard support
    wofi # app launcher
    xwaylandvideobridge # screensharing bridge
  ] ++ fontPkgs ++ audioPkgs;

  gblast = lib.exe pkgs.grimblast;
  wpctl = "${pkgs.wireplumber}/bin/wpctl";

  wsNixScript = pkgs.writeShellScriptBin "ws-nix" ''
    footclient -D ~/workspace/nix-config -E fish -C 'neofetch' &
    footclient -D ~/workspace/nix-config -E fish -C 'nitch' &
  '';

  monitorAddedScript = pkgs.writeShellScriptBin "monitor-added" ''
    hyprctl dispatch moveworkspacetomonitor 1 HDMI-A-1
    hyprctl dispatch moveworkspacetomonitor 2 HDMI-A-1
    hyprctl dispatch moveworkspacetomonitor 3 HDMI-A-1
    hyprctl dispatch moveworkspacetomonitor 4 HDMI-A-1
    hyprctl dispatch moveworkspacetomonitor 5 HDMI-A-1
    echo "monitor=HDMI-A-1,3840x2160@59.99700,0x0,2" > ~/.config/hypr/monitors.conf
    echo "monitor=eDP-1,2880x1800@90,1920x0,2,mirror,HDMI-A-1" >> ~/.config/hypr/monitors.conf
  '';

  monitorRemovedScript = pkgs.writeShellScriptBin "monitor-removed" ''
    echo "monitor=eDP-1,2880x1800@90,0x0,2" > ~/.config/hypr/monitors.conf
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
      bind=SUPER,P,exec,${lib.exe pkgs.wofi} --show run --style=${./wofi.css} --term=footclient --prompt=Run
      bind=SUPER,A,exec,${gblast} save area
      bind=SUPER,S,exec,${gblast} save screen
      bind=SUPERCTRL,L,exec,${lib.exe pkgs.hyprlock}
      # audio volume bindings
      bindel=,XF86AudioRaiseVolume,exec,${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%+
      bindel=,XF86AudioLowerVolume,exec,${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%-
      bindl=,XF86AudioMute,exec,${wpctl} set-mute @DEFAULT_AUDIO_SINK@ toggle

      workspace=2,persistent:true,on-created-empty:${lib.exe wsNixScript}

      exec-once=${lib.exe pkgs.hypr-monitor-attached} ${lib.exe monitorAddedScript} ${lib.exe monitorRemovedScript}
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
