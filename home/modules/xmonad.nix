{ config, lib, pkgs, ... }:

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

    hidpi = mkOption {
      type = types.bool;
      default = false;
      description = "Set proper HiDPI resolution.";
    };
  };

  config = mkIf cfg.enable (
    import ../programs/xmonad/default.nix {
      inherit config pkgs lib;
      hdmiExtra = if cfg.hidpi then hdmiOps else "";
    }
  );
}
