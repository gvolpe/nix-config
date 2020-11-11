{ config, pkgs, ... }:

# TODO: Missing reproducible installation of required font
let
  mypolybar = pkgs.polybar.override {
    alsaSupport  = true;
    mpdSupport   = true;
    pulseSupport = true;
  };

  bars   = builtins.readFile ./bars.ini;
  colors = builtins.readFile ./colors.ini;
  mods1  = builtins.readFile ./modules.ini;
  mods2  = builtins.readFile ./user_modules.ini;

  xmonad = ''
    [module/xmonad]
    type = custom/script
    exec = ${pkgs.xmonad-log}/bin/xmonad-log

    tail = true
  '';
in
{
  services.polybar = {
    enable = true;
    package = mypolybar;
    config = ./config.ini;
    extraConfig = bars + colors + mods1 + mods2 + xmonad;
    script = ''
      polybar top &
      sleep 1
      polybar bottom &
    '';
  };
}
