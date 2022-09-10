{ config, lib, pkgs, specialArgs, ... }:

with lib;

let
  cfg = config.programs.signal;

  scaleFactor = if specialArgs.hidpi then "2" else "1.5";

  signal = pkgs.signal-desktop.overrideAttrs (old: {
    preFixup = old.preFixup + ''
      substituteInPlace $out/share/applications/signal-desktop.desktop \
        --replace "--no-sandbox" "--use-tray-icon --force-device-scale-factor=${scaleFactor}"
    '';
  });

  finalPackage = pkgs.symlinkJoin
    {
      name = "signal-desktop";
      paths = [ signal ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/signal-desktop \
          --add-flags "--use-tray-icon" \
          --add-flags "--force-device-scale-factor=${scaleFactor}"
      '';
    };

  jsonType = types.attrsOf types.anything;
in
{
  meta.maintainers = [ hm.maintainers.gvolpe ];

  options.programs.signal = {
    enable = mkEnableOption "Privacy-focused messaging client";

    settings = mkOption {
      type = jsonType;
      default = { };
      example = literalExpression ''
        {
          window = {
            maximized = false;
            autoHideMenuBar = false;
            fullscreen = false;
            width = 1015;
            height = 515;
            x = 6;
            y = 32;
          };
          spell-check = false;
          theme-setting = "dark";
        }
      '';
      description = "Attribute set of signal preferences (converted to JSON file).";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ finalPackage ];

    xdg.configFile."Signal/ephemeral.json" = mkIf (cfg.settings != { }) {
      text = generators.toJSON { } cfg.settings;
    };
  };
}

