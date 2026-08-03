{ config, ... }:

let
  inherit (config.lib.vicinae) mkRayCastExtension;

  gifSearch = mkRayCastExtension {
    name = "gif-search";
    rev = "365c9557780eb21293979aed3de9e06c05fab51f";
    sha256 = "sha256-/59ZaKe6gUkemauakgSvwkb76kN3aciKHgAh2yYk6jI=";
  };

  jwtDecoder = mkRayCastExtension {
    name = "jwt-decoder";
    rev = "365c9557780eb21293979aed3de9e06c05fab51f";
    sha256 = "sha256-/dHuBYGcN/uJWKHdjCLByP9GCk+UoxefuWhT/RPWWzA=";
  };
in
{
  programs.vicinae = {
    enable = true;
    extensions = [ gifSearch jwtDecoder ];
    systemd = {
      autoStart = true;
      enable = true;
    };
    useLayerShell = true;
  };

  xdg.configFile."vicinae/settings.json".source =
    config.dotfiles.make ./settings.json;
}
