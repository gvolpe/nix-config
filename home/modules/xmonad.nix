{ config, lib, pkgs, specialArgs, ... }:

with lib;

let
  cfg = config.programs.xmonad;

  hdmiOps = ''
    ${pkgs.xorg.xrandr}/bin/xrandr --output HDMI-A-0 --mode 3840x2160 --rate 30.00
  '';
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.programs.xmonad = {
    enable = mkEnableOption "XMonad with HiDPI settings.";
  };

  config = mkIf cfg.enable (
    import ../programs/xmonad/default.nix {
      inherit config pkgs lib;
      hdmiExtra = if specialArgs.hidpi then hdmiOps else "";
    }
  );
}
