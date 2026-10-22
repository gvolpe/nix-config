{ config, lib, pkgs, ... }:

let
  colors = {
    navy = "15236E";
    sky-blue = "7EBAE4";
    light-purple = "C084FC";
    blue = "348FEB";
    transparent = "00000000";
    translucent-black = "00000088";
    white = "FFFFFF";
  };

  lock-background = pkgs.writeShellScript "syncrain-lock-background" ''
    export WAYLAND_DISPLAY=swaylock-plugin
    export GDK_BACKEND=wayland
    exec ${lib.exe pkgs.syncrain} \
      --theme nixos \
      --logo "${./nixos.png}" \
      --logo-cycle 0 \
      --logo-size 0.5 \
      --drift 0 \
      --host native \
      --layer background \
      --pause-under never
  '';

  lockcmd-bin = pkgs.writeShellScriptBin "swaylock-cmd" ''
    # Keep repeated idle, lid and manual lock requests on one locker.
    exec 9>"$XDG_RUNTIME_DIR/swaylock-cmd.lock"
    ${pkgs.util-linux}/bin/flock --nonblock 9 || exit 0

    # Return only after the compositor confirms the lock, including before sleep.
    exec ${pkgs.swaylock-plugin}/bin/swaylock-plugin \
      --config /dev/null \
      --daemonize \
      --command ${lib.escapeShellArg (toString lock-background)} \
      --indicator-radius ${toString config.hidpi.swaylock.ringsize} \
      --indicator-thickness ${toString config.hidpi.swaylock.ringthickness} \
      --ring-color ${colors.navy} \
      --ring-clear-color ${colors.sky-blue} \
      --ring-wrong-color ${colors.light-purple} \
      --bs-hl-color ${colors.light-purple} \
      --key-hl-color ${colors.blue} \
      --line-color ${colors.transparent} \
      --line-clear-color ${colors.transparent} \
      --line-wrong-color ${colors.transparent} \
      --inside-color ${colors.translucent-black} \
      --inside-clear-color ${colors.translucent-black} \
      --inside-wrong-color ${colors.translucent-black} \
      --text-color ${colors.white} \
      --text-clear-color ${colors.white} \
      --text-caps-lock-color ${colors.white} \
      --text-ver-color ${colors.white} \
      --text-wrong-color ${colors.white} \
      --layout-text-color ${colors.white} \
      --separator-color ${colors.transparent}
  '';
in
{
  home.packages = [
    lockcmd-bin
    pkgs.sway-audio-idle-inhibit
  ];

  services.swayidle = {
    enable = true;

    events = {
      "before-sleep" = lib.exe lockcmd-bin;
      "lock" = lib.exe lockcmd-bin;
    };

    timeouts = [
      { command = lib.exe lockcmd-bin; timeout = 600; }
    ];
  };
}
