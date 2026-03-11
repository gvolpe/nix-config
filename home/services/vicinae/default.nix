{ pkgs, config, ... }:

let
  filePath = "${config.dotfiles.path}/services/vicinae/settings.json";
  configSrc =
    if !config.dotfiles.mutable then ./settings.json
    else config.lib.file.mkOutOfStoreSymlink filePath;

  gifSearch = pkgs.mkRayCastExtension {
    name = "gif-search";
    sha256 = "sha256-/59ZaKe6gUkemauakgSvwkb76kN3aciKHgAh2yYk6jI=";
    rev = "365c9557780eb21293979aed3de9e06c05fab51f";
  };

  jwtDecoder = pkgs.mkRayCastExtension {
    name = "jwt-decoder";
    sha256 = "sha256-/dHuBYGcN/uJWKHdjCLByP9GCk+UoxefuWhT/RPWWzA=";
    rev = "365c9557780eb21293979aed3de9e06c05fab51f";
  };
in
{
  home.packages = [ pkgs.vicinae ];

  services.vicinae = {
    enable = true;
    extensions = [ gifSearch jwtDecoder ];
    systemd = {
      enable = true;
      autoStart = true;
      environment = {
        USE_LAYER_SHELL = 1;
      };
    };
  };

  xdg.configFile."vicinae/settings.json".source = configSrc;
}
