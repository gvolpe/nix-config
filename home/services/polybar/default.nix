{ config, pkgs, ... }:

let
  openCalendar = "${pkgs.gnome3.gnome-calendar}/bin/gnome-calendar";
  openGithub   = "${pkgs.xdg_utils}/bin/xdg-open https\\://github.com/notifications";

  mypolybar = pkgs.polybar.override {
    alsaSupport   = true;
    githubSupport = true;
    mpdSupport    = true;
    pulseSupport  = true;
  };

  myenv = [
    ''"DISPLAY=:0"''
    ''"PATH=${mypolybar}/bin:/run/wrappers/bin:/run/current-system/sw/bin"''
  ];

  # theme adapted from: https://github.com/adi1090x/polybar-themes#-polybar-5
  bars   = builtins.readFile ./bars.ini;
  colors = builtins.readFile ./colors.ini;
  mods1  = builtins.readFile ./modules.ini;
  mods2  = builtins.readFile ./user_modules.ini;

  bluetoothScript = pkgs.callPackage ./scripts/bluetooth.nix {};
  monitorScript   = pkgs.callPackage ./scripts/monitor.nix {};
  mprisScript     = pkgs.callPackage ./scripts/mpris.nix {};

  bctl = ''
    [module/bctl]
    type = custom/script
    exec = ${bluetoothScript}/bin/bluetooth-ctl
    tail = true
    click-left = ${bluetoothScript}/bin/bluetooth-ctl --toggle &
  '';

  cal = ''
    [module/clickable-date]
    inherit = module/date
    label = %{A1:${openCalendar}:}%time%%{A}
  '';

  github = ''
    [module/clickable-github]
    inherit = module/github
    token = ''${file:${config.xdg.configHome}/polybar/github-notifications-token}
    label = %{A1:${openGithub}:}  %notifications%%{A}
  '';

  mpris = ''
    [module/mpris]
    type = custom/script

    exec = ${mprisScript}/bin/mpris
    tail = true

    label-maxlen = 60

    interval = 2
    format =   <label>
    format-padding = 2
  '';

  xmonad = ''
    [module/xmonad]
    type = custom/script
    exec = ${pkgs.xmonad-log}/bin/xmonad-log

    tail = true
  '';

  customMods = bctl + cal + github + mpris + xmonad;
in
{
  xdg.configFile."polybar/github-notifications-token".source = ../../secrets/github-notifications-token;

  services.polybar = {
    enable = true;
    package = mypolybar;
    config = ./config.ini;
    extraConfig = bars + colors + mods1 + mods2 + customMods;
    script = ''
      export MONITOR=$(${monitorScript}/bin/monitor)
      echo "Running polybar on $MONITOR"
      polybar top 2>${config.xdg.configHome}/polybar/logs/top.log &
      polybar bottom 2>${config.xdg.configHome}/polybar/logs/bottom.log &
    '';
  };

  systemd.user.services.polybar.Service.Environment = pkgs.lib.mkForce myenv;
}
