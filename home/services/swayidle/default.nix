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
  home.packages = [
    lockcmd-bin
    pkgs.sway-audio-idle-inhibit
  ];

  services.swayidle = {
    enable = true;

    events = {
      "before-sleep" = lib.exe lockcmd-bin;
      "lock" = "lock";
    };

    timeouts = [
      { timeout = 600; command = lib.exe lockcmd-bin; }
    ];
  };
}
