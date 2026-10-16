{ lib, pkgs, ... }:

let
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
      --indicator-radius 100 \
      --indicator-thickness 7 \
      --ring-color 15236E \
      --ring-clear-color 7EBAE4 \
      --ring-wrong-color C084FC \
      --key-hl-color 348FEB \
      --line-color 00000000 \
      --line-clear-color 00000000 \
      --line-wrong-color 00000000 \
      --inside-color 00000088 \
      --inside-clear-color 00000088 \
      --inside-wrong-color 00000088 \
      --text-color FFFFFF \
      --text-clear-color FFFFFF \
      --text-caps-lock-color FFFFFF \
      --text-ver-color FFFFFF \
      --text-wrong-color FFFFFF \
      --layout-text-color FFFFFF \
      --separator-color 00000000
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
