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
    cinnamon.nemo # file manager
    loupe # image viewer
    grim # screenshots
    grimblast # screenshot program from hyprland
    libnotify # notifications
    swaybg # background image
    wlsunset # day/night gamma adjustments
    wl-clipboard # clipboard support
    wofi # app launcher
    xwaylandvideobridge # screensharing bridge
  ] ++ fontPkgs ++ audioPkgs;

  wsNixScript = pkgs.writeShellScriptBin "ws-nix" ''
    footclient -D ~/workspace/nixos-hyprland -E fish -C 'neofetch' &
    footclient -D ~/workspace/nixos-hyprland -E fish -C 'nitch' &
  '';
in
{
  imports = [
    ../../shared
    ../../programs/foot
    ../../programs/pyprland
    ../../programs/waybar
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
      bind=SUPER,A,exec,${lib.exe pkgs.grimblast} save area
      bind=SUPER,S,exec,${lib.exe pkgs.grimblast} save screen

      workspace=2,persistent:true,on-created-empty:${lib.exe wsNixScript},monitor:HDMI-A-1

      exec-once=${lib.exe pkgs.swaybg} -i ${./hyprland.png}
      exec-once=${lib.exe pkgs.wlsunset} -l 54.5 -L 18.5
      exec-once=${pkgs.pyprland}/bin/pypr
      exec-once=${pkgs.blueman}/bin/blueman-applet &
      exec-once=${pkgs.networkmanagerapplet}/bin/nm-applet --sm-disable --indicator &
      exec-once=${lib.exe pkgs.pasystray} &
    '';
    plugins = [ ];
    systemd = {
      enable = true;
      variables = [ "--all" ];
    };
    xwayland.enable = true;
  };
}
