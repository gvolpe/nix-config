{ lib, pkgs, ... }:

let
  lockcmd-bin = pkgs.writeShellScriptBin "swaylock-cmd" ''
    ${pkgs.swaylock-effects}/bin/swaylock \
    	--screenshots \
    	--clock \
    	--indicator \
    	--indicator-radius 100 \
    	--indicator-thickness 7 \
    	--effect-blur 7x5 \
    	--effect-vignette 0.5:0.5 \
    	--ring-color 15236E \
    	--key-hl-color 348FEB \
    	--line-color 00000000 \
    	--inside-color 00000088 \
    	--separator-color 00000000 \
    	--grace 3 \
    	--fade-in 0.2
  '';
in
{
  # Super+Alt+L in the default setting (screen locker)
  home.packages = [ lockcmd-bin ];

  services.swayidle = {
    enable = true;

    events = [
      { event = "before-sleep"; command = lib.exe lockcmd-bin; }
      { event = "lock"; command = "lock"; }
    ];

    timeouts = [
      { timeout = 600; command = lib.exe lockcmd-bin; }
    ];
  };
}
